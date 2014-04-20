#define CONTROL_CONF_PULLUDDISABLE     0x00000008
#define CONTROL_CONF_PULLUPSEL         0x00000010
#define CONTROL_CONF_RXACTIVE          0x00000020
#define CONTROL_CONF_SLOWSLEW          0x00000040
#define CONTROL_CONF_MUXMODE(n)        (n)

#define SOC_CONTROL_REGS                        (0x44E10000)
#define CONTROL_IOPAD_OFFSET                    (0x800)
#define CONTROL_CONF_ECAP0_IN_PWM0_OUT_OFFSET   (0x964)
#define CONTROL_CONF_PWMSS_CTRL                 (0x664)
#define CONTROL_CONF_ECAP_EVT_CAP               (0xFD4)

/* CONTROL_CONF_PWMSS_CTRL bit fields */
#define PWMSS0_TBCLKEN                  0
#define PWMSS1_TBCLKEN                  1
#define PWMSS2_TBCLKEN                  2

/* CONTROL_CONF_PWMSS_CTRL bit values */
#define TBCLKEN_DISABLE                 0
#define TBCLKEN_ENABLE                  1

/* CONTROL_CONF_ECAP_EVT_CAP bit fields */
#define ECAP0_EVTCAP                    0x00
#define ECAP1_EVTCAP                    0x08
#define ECAP2_EVTCAP                    0x16

#define PWMSS0_MMIO_BASE    (0x48300000)
#define PWMSS0_MMIO_END     (0x483000FF)
#define ECAP0_REGS_BASE     (0x48300100)
#define ECAP0_REGS_OFFSET   (0x100) /* offset from PWMSS_MMIO_BASE */
#define ECAP0_REGS_END      (0x4830017F)

/* Clock control stuff */
#define CM_PER_MMIO_BASE                (0x44E00000)
#define CM_PER_MMIO_END                 (0x44E03FFF)
#define CM_PER_EPWMSS0_CLKCTRL_OFFSET   (0xD4)
#define CM_PER_L4LS_CLKSTCTRL_OFFSET    (0x00)
#define CM_PER_L4LS_CLKCTRL_OFFSET      (0x60)

#define IDLEST      16
#define MODULEMODE  0

#define IDLEST_FUNC     0
#define IDLEST_TRANS    1
#define IDLEST_IDLE     2
#define IDLEST_DISABLE  3

#define MODULEMODE_DISABLED 0
#define MODULEMODE_ENABLE   2
