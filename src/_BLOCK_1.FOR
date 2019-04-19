*=== GRAFM.dsp _BLOCK_1.FOR ======================================================*
!----------------------------------------
      BLOCK DATA M1
!----------------------------------------
      IMPLICIT REAL*8 (A-H,O-Z)
      INCLUDE '_PARAM.FOR'
      INCLUDE '_COMON.FOR'
      DATA MS/-23/
COM12............./COM12/BDR(NP),BDZ(NP),REX(NP),IGM(NP),MS
      DATA BDR(01),BDZ(01),IGM(01),REX(01)/ 0.0,    300.0, 2,	   0.0/
      DATA BDR(02),BDZ(02),IGM(02),REX(02)/20.0,    300.0,-6,	   0.0/
      DATA BDR(03),BDZ(03),IGM(03),REX(03)/20.0,     82.5,-6,	   0.0/
      DATA BDR(04),BDZ(04),IGM(04),REX(04)/14.0,     82.5,-6,	   0.0/
      DATA BDR(05),BDZ(05),IGM(05),REX(05)/14.0,     77.5,-6,	   0.0/
      DATA BDR(06),BDZ(06),IGM(06),REX(06)/18.5,     65.5,-6,	   0.0/
      DATA BDR(07),BDZ(07),IGM(07),REX(07)/23.0,     65.5,-6,	   0.0/
      DATA BDR(08),BDZ(08),IGM(08),REX(08)/27.0,     70.0,-6,	   0.0/
      DATA BDR(09),BDZ(09),IGM(09),REX(09)/27.0,     75.0,-6,	   0.0/
      DATA BDR(10),BDZ(10),IGM(10),REX(10)/75.0,     75.0, 1,	   0.0/
      DATA BDR(11),BDZ(11),IGM(11),REX(11)/75.0,     60.5,-1,	   0.0/
      DATA BDR(12),BDZ(12),IGM(12),REX(12)/39.444,   60.5,-1,	   0.0/
      DATA BDR(13),BDZ(13),IGM(13),REX(13)/35.0,     55.5,-1,	   0.0/
      DATA BDR(14),BDZ(14),IGM(14),REX(14)/36.884,   52.7,-1,	   20./
      DATA BDR(15),BDZ(15),IGM(15),REX(15)/30.015,   38.4,-1,	   0.0/
      DATA BDR(16),BDZ(16),IGM(16),REX(16)/20.0,     35.5,-1,	   0.0/
      DATA BDR(17),BDZ(17),IGM(17),REX(17)/18.0,     30.5,-1,	   0.0/
      DATA BDR(18),BDZ(18),IGM(18),REX(18)/18.0,     25.0,-1,	   0.0/
      DATA BDR(19),BDZ(19),IGM(19),REX(19)/75.0,     25.0, 1,	   0.0/
      DATA BDR(20),BDZ(20),IGM(20),REX(20)/75.0,     13.0,-7,	   0.0/
      DATA BDR(21),BDZ(21),IGM(21),REX(21)/19.4984,  13.0,-7,	   0.0/
      DATA BDR(22),BDZ(22),IGM(22),REX(22)/19.4984,   7.0,-7,	   31./
      DATA BDR(23),BDZ(23),IGM(23),REX(23)/ 0.0,       .1, 4,	   0.0/
      DATA BDR(24),BDZ(24),IGM(24),REX(24)/ 0.0,       .0, 0,	   0.0/
      DATA BDR(25),BDZ(25),IGM(25),REX(25)/ 0.0,       .0, 0,	   0.0/
      DATA BDR(26),BDZ(26),IGM(26),REX(26)/ 0.0,       .0, 0,	   0.0/
      DATA BDR(27),BDZ(27),IGM(27),REX(27)/ 0.0,       .0, 0,	   0.0/
      DATA BDR(28),BDZ(28),IGM(28),REX(28)/ 0.0,       .0, 0,	   0.0/
      DATA BDR(29),BDZ(29),IGM(29),REX(29)/ 0.0,       .0, 0,	   0.0/
      DATA BDR(30),BDZ(30),IGM(30),REX(30)/ 0.0,      0.0, 0,	   0.0/
COM13............./COM13/ROK(NO),XOK(NO),YOK(NO)                 
      DATA ROK(01),XOK(01),YOK(01)/20.0,    38.5,     16.9/
      DATA ROK(02),XOK(02),YOK(02)/0.0,      0.0,      0.0/
      DATA ROK(03),XOK(03),YOK(03)/0.0,      0.0,      0.0/
      DATA ROK(04),XOK(04),YOK(04)/0.0,      0.0,      0.0/
      DATA ROK(05),XOK(05),YOK(05)/0.0,      0.0,      0.0/
      DATA ROK(06),XOK(06),YOK(06)/0.0,      0.0,      0.0/
      DATA ROK(07),XOK(07),YOK(07)/0.0,      0.0,      0.0/
      DATA ROK(08),XOK(08),YOK(08)/0.0,      0.0,      0.0/
      DATA ROK(09),XOK(09),YOK(09)/0.0,      0.0,      0.0/
      DATA ROK(10),XOK(10),YOK(10)/ 0.0,     0.0,      0.0/
      DATA NITM/50/
      DATA ITMA/9999/
      DATA ITMI/50/
      DATA MTWR/.TRUE./
      DATA RELE/178D-9/
      DATA ABSE/1D-9/
      DATA W   /1.8/
      DATA FGUN/'INTG   '/
      DATA NUMB/0/
!!!08-04-18 DATA NUMB/22/
      DATA POT
     & /0.,0.,0.,0.,0.,1.,1.,0.,0.,0.,0.,9*0./

      END !BLOCK DATA M1