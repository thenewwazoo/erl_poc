#ifdef MODULE
#include <linux/types.h>
#else
#include <stdint.h>
#endif

struct PWMSS_regs {
    volatile uint32_t IDVER;
    volatile uint32_t SYSCONFIG;
    volatile uint32_t CLKCONFIG;
    volatile uint32_t CLKSTATUS;
};

/* IDVER fields */
#define IDVER_Y_MINOR 0
#define IDVER_CUSTOM  6
#define IDVER_X_MAJOR 8
#define IDVER_R_RTL   11
#define IDVER_FUNC    16
#define IDVER_SCHEME  30

/* SYSCONFIG fields */
#define SYSCONFIG_SOFTRESET   0
#define SYSCONFIG_FREEEMU     1
#define SYSCONFIG_IDLEMODE    2
#define SYSCONFIG_STANDBYMODE 4

/* SYSCONFIG bit values */
#define SOFTRESET_RESET     1
#define FREEMU_EMU_SENS     0
#define FREEMU_EMU_NOSENS   1
#define IDLEMODE_FORCE      0
#define IDLEMODE_NOIDLE     1
#define IDLEMODE_SMART      2
#define STANDBYMODE_FORCE   0
#define STANDBYMODE_NOSTBY  1
#define STANDBYMODE_SMART   2

/* CLKCONFIG fields */
#define eCAPCLK_EN      0
#define eCAPCLKSTOP_REQ 1
#define eQEPCLK_EN      4
#define eQEPCLKSTOP_REQ 5
#define ePWMCLK_EN      8
#define ePWMCLKSTOP_REQ 9

/* CLKCONFIG bit values */
#define CLKCONFIG_CLK_EN    1
#define CLKCONFIG_STOP_REQ  1

/* CLKSTATUS fields */
#define eCAP_CLK_EN_ACK     0
#define eCAP_CLKSTOP_ACK    1
#define eQEP_CLK_EN_ACK     4
#define eQEP_CLKSTOP_ACK    5
#define ePWM_CLK_EN_ACK     8
#define ePWM_CLKSTOP_ACK    9
