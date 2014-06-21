
/*
 A rather brute-forceish kernel module for the TI AM3359 eCAP which
 allows use of the input capture feature. 
 
 Targetted at Linux 3.2.x because anything newer that properly supports device
 trees doesn't actually interrupt and I have no idea why. I've got a DT version
 laying around somewhere. Let me know if you want to debug the interrupt issue.

 BUGS:
 - Current code only initializes and exposes eCAP0 (hw has two more)
 - Doesn't work on any kernel newer than 3.2; only really tested on TI EZSDK kernels

 Copyleft Brandon Matthews <thenewwazoo@optimaltour.us> GPLv2
*/

#include <linux/kernel.h>
#include <linux/module.h>
#include <linux/interrupt.h>
#include <linux/platform_device.h>
#include <linux/spinlock.h>
#include <linux/clk.h>
#include <linux/pm_runtime.h>
#include <linux/err.h>
#include <linux/sysfs.h>
#include <asm/io.h>
#include <asm/page.h>

#include "bbb_mmap.h"
#include "ecap_defn.h"
#include "pwmss_defn.h"

struct ecap_data {
    void __iomem * soc_control_mmio_base;
    int soc_control_conf_reg_offset;
    void __iomem * cm_per_mmio_base;
    int cm_per_clk_reg_offset;

    struct PWMSS_regs* pwmss_regs;
    struct eCAP_regs* ecap_regs; // struct overlay of ecap registers

    unsigned int pin;       // input pin, passed to GPIO support
    unsigned int mux_mode;  // mux mode for the pin
    unsigned int trigger;   // the ecap_evt_cap value for muxing the capture trigger
    struct clk *clk;

    unsigned long int num_intr; // number of interrupts since probe
    unsigned short int int_flags; // ECFLG storage

    unsigned long flags;
    spinlock_t ecap_lock;
};

static irqreturn_t ecap_interrupt(int irq, void* dev_id);

int ecap_capture_probe( struct platform_device* pdev )
{
    int ret = -1;
    uint16_t val;
    struct resource* res;
    int ecapirq;
    char con_id[255] = "epwmss";
    irq_handler_t ecap_intr_handler; /* callback for IRQ request */

    struct ecap_data* pdata;

    printk(KERN_DEBUG "Probing platform device [%s%d]\n", pdev->name, pdev->id);

    pdata = (struct ecap_data*) &(pdev->dev.platform_data);

    /*
    sprintf(con_id, "%s%d_%s", con_id, pdev->id, "fck");
    pdata->clk = clk_get(&pdev->dev, con_id);
    if (IS_ERR(pdata->clk)) { printk(KERN_ERR "Could not get device clock."); ret = PTR_ERR(pdata->clk); goto fail_clk_get; }
printk(KERN_DEBUG "got clock ok.");
*/

/*    pm_runtime_enable(&pdev->dev);
printk(KERN_DEBUG "did pm_runtime_enable.");
*/

    res = platform_get_resource_byname(pdev, IORESOURCE_MEM, "soc_control_mmio");
    if (!res) { printk(KERN_ERR "Could not find IORESOURCE_MEM soc_control_mmio"); ret = -EINVAL; goto fail_getres; }
    pdata->soc_control_mmio_base = ioremap(res->start, res->end - res->start);
    if (IS_ERR(pdata->soc_control_mmio_base)) { printk(KERN_ERR "Failed to map soc_control_mmio"); ret = -ENXIO; goto fail_remap; }
printk(KERN_DEBUG "got soc_control_mmio.\n");

    res = platform_get_resource_byname(pdev, IORESOURCE_MEM, "soc_conf_reg_offset");
    if (!res) { printk(KERN_ERR "Could not get soc_conf_reg_offset resource"); ret = -EINVAL; goto fail_getres; }
    pdata->soc_control_conf_reg_offset = res->start;
printk(KERN_DEBUG "got soc control conf offset 0x%x.\n", pdata->soc_control_conf_reg_offset);

    res = platform_get_resource_byname(pdev, IORESOURCE_MEM, "cm_per_mmio");
    if (!res) { printk(KERN_ERR "Could not find IORESOURCE_MEM cm_per_mmio"); ret = -EINVAL; goto fail_getres; }
    pdata->cm_per_mmio_base = ioremap(res->start, PAGE_SIZE);
    if (IS_ERR(pdata->cm_per_mmio_base)) { printk(KERN_ERR "Failed to map cm_per_mmio"); ret = -ENXIO; goto fail_remap; }
printk(KERN_DEBUG "got cm_per_mmio.\n");

    res = platform_get_resource_byname(pdev, IORESOURCE_MEM, "cm_per_pwmss_clkctrl_reg_offset");
    if (!res) { printk(KERN_ERR "Could not get cm_per_pwmss_clkctrl_reg_offset resource"); ret = -EINVAL; goto fail_getres; }
    pdata->cm_per_clk_reg_offset = res->start;
printk(KERN_DEBUG "got cm_per clkctrl offset 0x%x.\n", pdata->cm_per_clk_reg_offset);

    res = platform_get_resource_byname(pdev, IORESOURCE_MEM, "ecap_regs_mmio");
    if (!res) { printk(KERN_ERR "Could not find IORESOURCE_MEM ecap_regs_mmio"); ret = -EINVAL; goto fail_getres; }
    pdata->ecap_regs = (struct eCAP_regs*)ioremap(res->start, PAGE_SIZE);
    if (IS_ERR(pdata->ecap_regs)) { printk(KERN_ERR "Could not map ecap_regs"); ret = -ENXIO; goto fail_remap; }
printk(KERN_DEBUG "got ecap_regs.\n");

    res = platform_get_resource_byname(pdev, IORESOURCE_MEM, "pwmss_regs_mmio");
    if (!res) { printk(KERN_ERR "Could not find IORESOURCE_MEM pwmss_regs_mmio"); ret = -EINVAL; goto fail_getres; }
    pdata->pwmss_regs = (struct PWMSS_regs*)ioremap(res->start, PAGE_SIZE);
    if (IS_ERR(pdata->pwmss_regs)) { printk(KERN_ERR "Could not map pwmss_regs"); ret = -ENXIO; goto fail_remap; }
printk(KERN_DEBUG "got pwmss_regs.\n");

    ecapirq = platform_get_irq_byname(pdev, "ecap_irq");
    if (ecapirq < 0) { printk(KERN_ERR "Could not get IRQ"); goto fail_irq; }
printk(KERN_DEBUG "got irq %d.\n", ecapirq);

    spin_lock_init(&pdata->ecap_lock);
    spin_lock_irqsave(&(pdata->ecap_lock), pdata->flags);

    ecap_intr_handler = &ecap_interrupt;
    sprintf(con_id, "%s%d", pdev->name, pdev->id);
    //ret = request_irq( ecapirq, ecap_intr_handler, (IRQF_TRIGGER_NONE), con_id, pdev);
    ret = request_irq( ecapirq, 
                       ecap_intr_handler, 
                       (IRQF_TRIGGER_MASK), 
                       "ecap-capture0", 
                       pdev);
    if (ret) { printk(KERN_ERR "Failed to acquire interrupt.\n"); ret = -EINVAL; goto fail_intr; }
printk(KERN_DEBUG "registered irq\n");

    val = readl(pdata->cm_per_mmio_base + pdata->cm_per_clk_reg_offset);
    val |= (MODULEMODE_ENABLE << MODULEMODE);
    writel(val, pdata->cm_per_mmio_base + pdata->cm_per_clk_reg_offset);
printk(KERN_DEBUG "enabled module in cm_per.\n");

   // mux the pin (this is basically the only way in Linux 3.2)
    writel( (CONTROL_CONF_RXACTIVE | CONTROL_CONF_MUXMODE(pdata->mux_mode) ), 
            pdata->soc_control_mmio_base + pdata->soc_control_conf_reg_offset
            );
printk(KERN_DEBUG "mux'd pin\n");

    pdata->ecap_regs->ECEINT = 0x0;
    pdata->ecap_regs->ECCTL1 = (EC_RISING << CAP1POL)   | (EC_RISING << CAP2POL)   | (EC_RISING << CAP3POL)   | (EC_RISING << CAP4POL)   |
                               (EC_ABS_MODE << CTRRST1) | (EC_ABS_MODE << CTRRST2) | (EC_ABS_MODE << CTRRST3) | (EC_ABS_MODE << CTRRST4) |
                               (EC_ENABLE << CAPLDEN)   | (EC_DIV1 << PRESCALE);
    pdata->ecap_regs->ECCTL2 = (EC_CAP_MODE << CAP_APWM)   | (EC_CONTINUOUS << CONT_ONESHT) | (EC_EVENT4 << STOP_WRAP) |
                               (EC_SYNCO_DIS << SYNCO_SEL) | (EC_DISABLE << SYNCI_EN);
    pdata->ecap_regs->ECCLR = BIT(CTR_EQ_CMP) | BIT(CTR_EQ_PRD) | BIT(CTROVF) | BIT(CEVT4) | BIT(CEVT3) | BIT(CEVT2) | BIT(CEVT1) | BIT(INT);
    pdata->ecap_regs->ECEINT = BIT(CTROVF) | BIT(CEVT4) | BIT(CEVT3) | BIT(CEVT2) | BIT(CEVT1);
    pdata->ecap_regs->ECCTL2 |= (EC_RUN << TSCTRSTOP);
printk(KERN_DEBUG "set up the various ecap registers\n");

    val = (IDLEMODE_SMART << SYSCONFIG_IDLEMODE);
    pdata->pwmss_regs->SYSCONFIG = val;
    val = pdata->pwmss_regs->CLKCONFIG;
    val |= (CLKCONFIG_CLK_EN << eCAPCLK_EN);
    pdata->pwmss_regs->CLKCONFIG = val;
    val = pdata->pwmss_regs->CLKSTATUS;
    if ( !(val & (1<<eCAP_CLK_EN_ACK)) ) { printk(KERN_ERR "Failed to start eCAP counter. Bailing.\n"); ret = -ENODEV; goto fail_counter; }
printk(KERN_DEBUG "clock started; \n");

    spin_unlock_irqrestore(&pdata->ecap_lock, pdata->flags);

printk(KERN_DEBUG "Done probing.\n");

    return 0;

fail_clk_get:
fail_getres:
fail_remap:
fail_intr:
fail_irq:
fail_counter:
    printk(KERN_ERR "Failed to probe for %s%d\n", pdev->name, pdev->id);
    return ret;
}

int ecap_capture_remove( struct platform_device* pdev )
{
    printk(KERN_DEBUG "Removing device %s\n", pdev->name);
    return 0;
}

static struct platform_driver ecap_capture_driver = {
    .probe      = ecap_capture_probe,
    .remove     = ecap_capture_remove,
    .driver     = {
        .name   = "ecap-capture",
        .owner  = THIS_MODULE,
    },
};

static struct resource ecap0_resources[] = {
    [0] = {
        .start  = CM_PER_MMIO_BASE,
        .end    = CM_PER_MMIO_END,
        .flags  = IORESOURCE_MEM,
        .name   = "cm_per_mmio",
    },
// this entry is because I don't have a CM_PER struct defined, like with PWMSS and ECAP
// and it's the wrong resource type because IORESOURCE_REG doesn't exist in kernel 3.2  :-|
    [1] = { 
        .start  = CM_PER_EPWMSS0_CLKCTRL_OFFSET,
        .end    = CM_PER_EPWMSS0_CLKCTRL_OFFSET,
        .flags  = IORESOURCE_MEM,
        .name   = "cm_per_pwmss_clkctrl_reg_offset",
    },
    [2] = {
        .start  = PWMSS0_MMIO_BASE,
        .end    = PWMSS0_MMIO_END,
        .flags  = IORESOURCE_MEM,
        .name   = "pwmss_regs_mmio",
    },
    [3] = {
        .start  = ECAP0_REGS_BASE, 
        .end    = ECAP0_REGS_END,
        .flags  = IORESOURCE_MEM,
        .name   = "ecap_regs_mmio",
    },
    [4] = {
        .start  = ECAP0_INT,
        .end    = ECAP0_INT,
        .flags  = IORESOURCE_IRQ,
        .name   = "ecap_irq",
    },
    [5] = {
        .start  = SOC_CONTROL_REGS,
        .end    = SOC_CONTROL_REGS + 0x20000,
        .flags  = IORESOURCE_MEM,
        .name   = "soc_control_mmio",
    },
    [6] = {
        .start  = CONTROL_CONF_ECAP0_IN_PWM0_OUT_OFFSET,
        .end    = CONTROL_CONF_ECAP0_IN_PWM0_OUT_OFFSET,
        .flags  = IORESOURCE_MEM,
        .name   = "soc_conf_reg_offset",
    },
};

// attributes:
//  interrupt - read: num_interrupts (blocks), write: enable/disable?
//  cap_reg   - read: reg[...] (noblock)
//  clock     - read: tsctr (noblock)

// TODO: Implement a feature by which captures can be "saved up" and missed ones recovered. 
//  In the interrupt, logical-OR the ECFLG value with the currently-stored value.
//     pdata->intr_flags |= pdata->ecap_regs->ECFLG;
//  Then, emit intr_flags in the capture SHOW function.
//  Create a capture STORE function that clears pdata->intr_flags bits based on the input. This
//   way, I can tell the module about which interrupts I've recieved without losing new ones.

ssize_t capture_show(struct device *dev, struct device_attribute *attr, char *buf) {
    int ret;
    struct platform_device* pdev = container_of(dev, struct platform_device, dev);
    struct ecap_data* pdata = (struct ecap_data*) &(pdev->dev.platform_data);
    ret = snprintf( buf, PAGE_SIZE, "%08x", pdata->int_flags );
    printk(KERN_DEBUG "showed capture\n");
    return ret;
}
static DEVICE_ATTR(capture, 0444, capture_show, NULL);

ssize_t num_intr_show(struct device *dev, struct device_attribute *attr, char *buf) {
    int ret;
    struct platform_device* pdev = container_of(dev, struct platform_device, dev);
    struct ecap_data* pdata = (struct ecap_data*) &(pdev->dev.platform_data);
    ret = snprintf( buf, PAGE_SIZE, "%ld", pdata->num_intr );
    return ret;
}
static DEVICE_ATTR(num_intr, 0444, num_intr_show, NULL);

ssize_t cap_reg_show(struct device *dev, struct device_attribute *attr, char *buf) {
    int ret;
    struct platform_device* pdev = container_of(dev, struct platform_device, dev);
    struct ecap_data* pdata = (struct ecap_data*) &(pdev->dev.platform_data);
    ret = snprintf( buf, 
                    PAGE_SIZE, 
                    "%08x,%08x,%08x,%08x", 
                    pdata->ecap_regs->CAP4,
                    pdata->ecap_regs->CAP3,
                    pdata->ecap_regs->CAP2,
                    pdata->ecap_regs->CAP1 );
    printk(KERN_DEBUG "wrote %d for intr\n", ret);
    return ret;
}
static DEVICE_ATTR(cap_reg, 0444, cap_reg_show, NULL);

ssize_t clock_show(struct device *dev, struct device_attribute *attr, char *buf) {
    int ret;
    struct platform_device* pdev = container_of(dev, struct platform_device, dev);
    struct ecap_data* pdata = (struct ecap_data*) &(pdev->dev.platform_data);
    ret = snprintf( buf, PAGE_SIZE, "%08x", pdata->ecap_regs->TSCTR );
    return ret;
}
static DEVICE_ATTR(clock, 0444, clock_show, NULL);

static struct attribute* ecap_attrs[] = {
    &dev_attr_num_intr.attr,
    &dev_attr_cap_reg.attr,
    &dev_attr_clock.attr,
    &dev_attr_capture.attr,
    NULL,
};

static struct attribute_group ecap_attr_group = {
    .attrs = ecap_attrs,
};

static const struct attribute_group* ecap_attr_groups[] = {
    &ecap_attr_group,
    NULL,
};

static struct ecap_data ecap0_pdata = {
    .pin            = 0x10, // the input pin...
                            // uh, supposedly. not used because we have to munge regs as above
    .mux_mode       = 0x00, // the mux mode for the pin
    .trigger        = 0x00, // the ecap_evt_cap value for muxing capture trigger
};

static struct platform_device ecap0_device = {
    .name   = "ecap-capture",
    .id     = 0,
    .num_resources  = ARRAY_SIZE(ecap0_resources),
    .resource       = ecap0_resources,
    .dev    = {
        .platform_data = &ecap0_pdata,
        .groups = ecap_attr_groups,
    },
};

// Seriously, I cannot wait to port to 3.12 and use device trees.

static struct platform_device *ecap_devices[] __initdata = {
    &ecap0_device,
//    &ecap1_device,
//    &ecap2_device,
    NULL
};

static int __init ecap_capture_init(void)
{
    int ret = -1;

    printk(KERN_INFO "ecap platform driver module loading");

    /* I ... guess we don't need to do this. Keeping for posterity.
    control_mmio_base = ioremap(SOC_CONTROL_REGS, 0x20000);
    if (!control_mmio_base) { printk(KERN_ERR "Failed to remap control reg addr\n"); ret = -EADDRNOTAVAIL; goto fail_remap; }
    val = readl(control_mmio_base + CONTROL_CONF_PWMSS_CTRL);
    val |= (TBCLKEN_ENABLE << PWMSS0_TBCLKEN); // | (TBCLKEN_ENABLE << PWMSS1_TBCLKEN) | (TBCLKEN_ENABLE << PWMSS2_TBCLKEN);
    writel(val, control_mmio_base + CONTROL_CONF_PWMSS_CTRL);
    */

    ret = platform_driver_register( &ecap_capture_driver );
    if (ret) { printk(KERN_ERR "error registering platform driver: %d", ret); goto fail_register; }

    ret = platform_device_register( &ecap0_device );
    //ret = platform_add_devices( ecap_devices, ARRAY_SIZE(ecap_devices) );
    if (ret) { printk(KERN_ERR "error adding device(s): %d", ret); goto fail_add; }

printk(KERN_DEBUG "\n");
    return 0;

fail_add:
    platform_driver_unregister( &ecap_capture_driver );
fail_register:
    return ret;
}

void __exit ecap_capture_exit(void)
{
    printk(KERN_INFO "ecap platform driver module unloading");

    platform_driver_unregister( &ecap_capture_driver );
}

module_init(ecap_capture_init);
module_exit(ecap_capture_exit);

static irqreturn_t ecap_interrupt(int irq, void *dev_id)
{
    printk(KERN_DEBUG "ecap interrupted\n");
    struct platform_device* pdev = dev_id;
    struct ecap_data* pdata = (struct ecap_data*) &(pdev->dev.platform_data);

    spin_lock_irqsave(&pdata->ecap_lock, pdata->flags);
    pdata->int_flags = pdata->ecap_regs->ECFLG;
    pdata->ecap_regs->ECCLR = 0xFF;
    pdata->num_intr++;
    spin_unlock_irqrestore(&pdata->ecap_lock, pdata->flags);
    sysfs_notify(&pdev->dev.kobj, NULL, "capture");

    return IRQ_HANDLED;
}

MODULE_LICENSE("GPL");
MODULE_AUTHOR("Brandon Matthews <bmatt@optimaltour.us>");
MODULE_DESCRIPTION("A basic ecap platform device module.");

