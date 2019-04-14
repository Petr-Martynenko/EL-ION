*=== GRAFM.dsp _BLOCK_2.FOR ======================================================*
!----------------------------------------
      BLOCK DATA M2
!----------------------------------------
!
      IMPLICIT REAL*8 (A-H,O-Z)
      INCLUDE '_PARAM.FOR'
      INCLUDE '_COMON.FOR'

      DATA NK /0/
      DATA MI /4/
      DATA MZ /1/
      DATA NAME /'FORCE   '/
      DATA S/1D-1/
      DATA C/12D-2,15D-2,25D-2,35D-2,45D-2,55D-2,65D-2,75D-2,85D-2,9D-1/
      DATA NITER /0/
      DATA EPS /0.01/
      DATA UVOMAX /0.005/
      DATA GAMAS /2D-1,14*75D-2/
!      DATA GAMAS 
! /2D-1,0.75,0.75,0.75,0.75,0.75,0.75,0.75,0.75,0.75, 0.75,0.75,0.75,0.75,0.75/
      DATA DR /1.7D0/
      DATA IZZ /80/
      DATA AKAT /0.1235,0.0540,0.01/
      DATA BKAT /0.1490,0.0675,0.02/
      DATA DKAT /0.0165,0.3440,0.10/
      DATA ZKAT /0.1255,0.500,0.10/
      DATA TKAT /-33.00,+80.00,0.00/
      DATA WKAT /+142.0,2556.0,500./
!      DATA AKAT /0.1235,0.0540,0.01/
!      DATA BKAT /0.1490,0.0675,0.02/
!      DATA DKAT /0.0165,0.3440,0.10/
!      DATA ZKAT /0.1255,0.5050,0.10/
!      DATA TKAT /-33.00,+80.00,0.00/
!      DATA WKAT /+142.0,2556.0,500./
      DATA FG /150000./
      DATA Z0 /2D-3/
      DATA Y /0.,0.,0.,0.,0./
      DATA ISTART /0/
      DATA FO /0./
      DATA VO /0./
      DATA ZNAK /-1./
      DATA ZSCHIF /65./
      DATA STEP /0.5/
      DATA MAG /.FALSE./
      DATA MAGSEG /3/
      DATA MAGORD /6/

      END !BLOCK DATA M2
