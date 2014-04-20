#ifdef MODULE
#include <linux/types.h>
#else
#include <stdint.h>
#endif

#define ECAP0_INT   31

// TI ECAP register offset defines
struct eCAP_regs { 
    volatile uint32_t TSCTR; 
    volatile uint32_t CTRPHS; 
    volatile uint32_t CAP1; 
    volatile uint32_t CAP2; 
    volatile uint32_t CAP3; 
    volatile uint32_t CAP4; 
    volatile uint16_t RSVD0[8]; 
    volatile uint16_t ECCTL1; 
    volatile uint16_t ECCTL2; 
    volatile uint16_t ECEINT; 
    volatile uint16_t ECFLG; 
    volatile uint16_t ECCLR; 
    volatile uint16_t ECFRC; 
    volatile uint16_t RSVD1[20]; 
    volatile uint32_t REVID; 
};

// TI ECAP field defines
//  defined in terms of bit shifts

// Capture control register 1 bit definitions */                                     
// ECCTL1
#define	 CAP1POL      0    //  Capture Event 1 Polarity select 
#define	 CTRRST1      1    //  Counter Reset on Capture Event 1 
#define	 CAP2POL      2    //  Capture Event 2 Polarity select 
#define	 CTRRST2      3    //  Counter Reset on Capture Event 2 
#define	 CAP3POL      4    //  Capture Event 3 Polarity select 
#define	 CTRRST3      5    //  Counter Reset on Capture Event 3 
#define	 CAP4POL      6    //  Capture Event 4 Polarity select 
#define	 CTRRST4      7    //  Counter Reset on Capture Event 4 
#define	 CAPLDEN      8    //  Enable Loading CAP1-4 regs on a Cap Event 
#define	 PRESCALE     9    //  Event Filter prescale select 
#define	 FREE_SOFT   14    //  Emulation mode 

// Capture control register 2 bit definitions */                                     
// ECCTL2
#define  CONT_ONESHT 0  // Continuous or one-shot 
#define  STOP_WRAP   1  // Stop value for one-shot, Wrap for continuous 
#define  REARM       3  // One-shot re-arm 
#define  TSCTRSTOP   4  // TSCNT counter stop 
#define  SYNCI_EN    5  // Counter sync-in select 
#define  SYNCO_SEL   6  // Sync-out mode 
#define  SWSYNC      8  // SW forced counter sync 
#define  CAP_APWM    9  // CAP/APWM operating mode select 
#define  APWMPOL     10 // APWM output polarity select 

// ECAP interrupt enable register bit definitions
// ECEINT
#define  CEVT1      1 //     Capture Event 1 Interrupt Enable 
#define  CEVT2      2 //     Capture Event 2 Interrupt Enable 
#define  CEVT3      3 //     Capture Event 3 Interrupt Enable 
#define  CEVT4      4 //     Capture Event 4 Interrupt Enable          
#define  CTROVF     5 //     Counter Overflow Interrupt Enable 
#define  CTR_EQ_PRD 6 //     Period Equal Interrupt Enable 
#define  CTR_EQ_CMP 7 //     Compare Equal Interrupt Enable

// ECAP interrupt flag register bit definitions */                                     
// ECFLG_BITS
#define  INT        0 //     Global Flag 
#define  CEVT1      1 //     Capture Event 1 Interrupt Flag 
#define  CEVT2      2 //     Capture Event 2 Interrupt Flag 
#define  CEVT3      3 //     Capture Event 3 Interrupt Flag 
#define  CEVT4      4 //     Capture Event 4 Interrupt Flag          
#define  CTROVF     5 //     Counter Overflow Interrupt Flag 
#define  CTR_EQ_PRD 6 //     Period Equal Interrupt Flag 
#define  CTR_EQ_CMP 7 //     Compare Equal Interrupt Flag 



// TI ECAP bit defines

// ECCTL1 ( ECAP Control Reg 1)
//==========================
// CAPxPOL bits
#define    EC_RISING              0x0
#define    EC_FALLING             0x1
// CTRRSTx bits
#define    EC_ABS_MODE            0x0
#define    EC_DELTA_MODE          0x1
// PRESCALE bits
#define    EC_BYPASS              0x0
#define    EC_DIV1                0x0
#define    EC_DIV2                0x1
#define    EC_DIV4                0x2
#define    EC_DIV6                0x3
#define    EC_DIV8                0x4
#define    EC_DIV10               0x5
// ECCTL2 ( ECAP Control Reg 2)
//==========================
// CONT/ONESHOT bit
#define    EC_CONTINUOUS          0x0
#define    EC_ONESHOT             0x1
// STOPVALUE bit
#define    EC_EVENT1              0x0
#define    EC_EVENT2              0x1
#define    EC_EVENT3              0x2
#define    EC_EVENT4              0x3
// RE-ARM bit
#define    EC_ARM                 0x1
// TSCTRSTOP bit
#define    EC_FREEZE              0x0
#define    EC_RUN                 0x1
// SYNCO_SEL bit
#define    EC_SYNCIN              0x0
#define    EC_CTR_PRD             0x1
#define    EC_SYNCO_DIS           0x2
// CAP/APWM mode bit
#define    EC_CAP_MODE            0x0
#define    EC_APWM_MODE           0x1
// APWMPOL bit
#define    EC_ACTV_HI             0x0
#define    EC_ACTV_LO             0x1
// Generic
#define    EC_DISABLE             0x0
#define    EC_ENABLE              0x1
#define    EC_FORCE               0x1

// ECEINT
//==========================
#define  EC_CEVT1_EN      1 //     Capture Event 1 Interrupt Enable 
#define  EC_CEVT2_EN      1 //     Capture Event 2 Interrupt Enable 
#define  EC_CEVT3_EN      1 //     Capture Event 3 Interrupt Enable 
#define  EC_CEVT4_EN      1 //     Capture Event 4 Interrupt Enable          
#define  EC_CTROVF_EN     1 //     Counter Overflow Interrupt Enable 
#define  EC_CTR_EQ_PRD_EN 1 //     Period Equal Interrupt Enable 
#define  EC_CTR_EQ_CMP_EN 1 //     Compare Equal Interrupt Enable


