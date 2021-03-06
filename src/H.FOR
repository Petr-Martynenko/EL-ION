CC    H.FOR MS Compaq Visual FORTRAN 6.6.0
CC
CC    11.10.89
***
* n-�� ����� ���������� ���������� ���� � ������� ���������� ���� �� ���
!----------------------------------------
      SUBROUTINE MAGFD
!----------------------------------------
      IMPLICIT REAL*8 (A-H,O-Z)

      PARAMETER (KK = 3)
      INCLUDE '_PARAM.FOR'
      INCLUDE '_COMON.FOR'

C     Main program variables

      REAL*8 BAXES, R1, R2, P1, P2, Z1, S1, I1,XUZ
!�������-�������� "BAXES" ���������� �������� ��������� �������� �� ��� 
      BAXES(R1, R2, P1, P2)=
     & P1*DLOG((R2+DSQRT(R2*R2+P1*P1))/(R1+DSQRT(R1*R1+P1*P1)))
     &-P2*DLOG((R2+DSQRT(R2*R2+P2*P2))/(R1+DSQRT(R1*R1+P2*P2)))
*������� �������� ���������� � ��������	  
*  BN   = ���������� ��������� ��������, Wb/m
*
      BN = SQRT(2.*FG/DEL)/Z0
CC    PRINT -- ������ �������� ����������	  
      PRINT '(//T3,''AMU0'',T18,''FG'',T28,''Z0'',
     + T38,''BN'',T46,''MAGSEG'',T56,''MAGORD''/
     + 1X,E10.3,F10.0,2F10.3,2I10)'
     + ,AMU0, FG, Z0, BN, MAGSEG, MAGORD
      DO 10 I= -6, IZMAX
	      BZA(I)=0.0
 10    CONTINUE

      IF (MAGSEG) 20,40,110
*��������� ��������������� ������� ������� ��� �������� ���������� ����
*  R1   = ��������� ������ �������,m
*  R2   = ������� ������ �������,m
*  Z1   = ���������� ������ �������,m 
*  S1   = ��������� �������,�
*  I1   = ��� �  ����� , �
*  N1   = ���-�� ������ , ���� N1= � , ��
*  I1 - ��������� ���� � ���������� ������� ������� , �/(m**2)
*
 110  CONTINUE
      DO 18 I=1,KK
	      R1  =AKAT(I)
	      R2  =BKAT(I)
	      Z1  =ZKAT(I)
	      S1  =DKAT(I)
	      I1  =TKAT(I)
	      N1  =WKAT(I)
	      CON=PI * AMU0 * I1 * N1 / S1 / (R2-R1)
C! ������ ���������� ��������������� ������� �������
      PRINT '(T8,''R1,m'',T18,''R2,m'',T28,''Z1,m'',
     & T38,''S1,m'',T46,''I1,A'',T58,''N1'',T68,''CON''/
     & 5F10.4,I10,F10.3)',
     & R1, R2, Z1, S1, I1, N1,  CON
C! ���������� ������� ������� �������� ��������� ��������
      DO 15 IND=-6, IZMAX
	      XUZ=Z0*(IND+XU(0))-Z1
	      BZA(IND)=BZA(IND)+CON*BAXES(R1, R2, XUZ+S1, XUZ-S1)
 15   CONTINUE
 18   CONTINUE
      GOTO 31
 20   CONTINUE
      GOTO 31
 40   CONTINUE
 31   MAG=.TRUE.
C! ������ ������� ������� �������� ��������� ��������, ��
      PRINT '(//100(1H*))'
      PRINT '(6(F7.3,1H:,G14.7,2H ;))'
     &  ,(Z0*(I+XU(0)),BZA(I),I=-6, IZMAX)
      DO 1000 I= -6, IZMAX
	      BZA(I)=BZA(I)/ BN
 1000 CONTINUE

      END SUBROUTINE MAGFD
!----------------------------------------
      SUBROUTINE MAGCAT 
!----------------------------------------
      IMPLICIT REAL*8 (A-H,O-Z)
      INCLUDE '_PARAM.FOR'
      INCLUDE '_COMON.FOR'

C     Main program variables
	  
      REAL*8 Z    !���������� �-�� �����,���� 
      REAL*8 R    !���������� �-�� �����,����.
      REAL*8 PCI  !������� ���������� ���,����. 
      REAL*8 Bx2  !���������� ���������� ��, ����. 
      REAL*8 By2   !���������� ���������� ��, ����.
      LOGICAL PrintFile
      DATA PrintFile /.TRUE./!TRUE - ���� ����� �� �� CONSOLE

      DO 10 J=1,IZZ
       Z= UX(J)
       IF(NAMCAT.EQ. 'CATOD1' )Z=0. 5*( Z+UX(J-1) )
       R= VY(J)
       IF(NAMCAT.EQ.'CATOD1')R=0.5*(R+VY(J-1))
*
*REAL*8 Z - inp    !���������� �-�� �����,����.
*REAL*8 R - inp    !���������� �-�� �����,����.
*REAL*8 PCI- out   !������� ���������� ���,����. 
*REAL*8 Bx2- out   !���������� ���������� ��, ����. 
*REAL*8 By2- out   !���������� ���������� ��, ����.
    
       CALL ForMag ( PCI, Z, R, Bx2, By2)
       PCIMG(J)=       PCI
 10    CONTINUE
C *** PRINT ***     �������� ��� �� ������ ***
      IF ( PrintFile ) THEN
      PRINT '('' �������� ��� �� ������,Wb/m'')'
      PRINT '(10E14.7,2H ;))'
     &  ,(PCIMG(J)*Z0*BN,J= 1, IZZ)
      END IF
C ***********************************************
      DO 20 J= 1, IZZ
 	     R=VY(J)
	     IF (NAMCAT .EQ. 'CATOD1') R=0.5*(R+VY(J-1))
	     PCIMG(J)=PCIMG(J)*R 
 20    CONTINUE
      PRINT '('' �������� ���������� ������ �� ������,Wb'')'
      PRINT '(10E14.7,2H ;))'
     &  ,(PI2*PCIMG(J)*BN*Z0*Z0,J= 1, IZZ)

      END !MAGCAT
!----------------------------------------
CC    FORMAG.for - For MS FORTRAN 5.1
CC
CC    ���� ����� �������� �� 30-01-92
CC
CC Program FORMAG.FOR ����������� ���, ��������� ��������� ��������,
CC ����������� ������� ��������������� ������� �������
CC � ����� � ��������� ������������
CC �������� ������� ��������� �������� 1.E-7 TL
      SUBROUTINE FORMAG (X,Y1,Y2,C1,C2)
*      REAL*8 X   - out  ! ������� ���������� ���, TL*�
*      REAL*8 Y1  - inp  ! ���������� ���������� �����, �
*      REAL*8 Y2  - inp  ! ���������� ���������� �����, �
*      REAL*8 C1  - out  ! ���������� ��������� ��, TL
*      REAL*8 C2  - out  ! ���������� ��������� ��, TL
!----------------------------------------
      IMPLICIT REAL*8 (A-H,O-Z)
      INCLUDE '_PARAM.FOR'
      COMMON/COMAA/ZPL,ZML,R,RHO,PHI,SUM0,SUM1,SUM2
      INCLUDE '_COMON.FOR'

C     Main program variables
CC    CHIP --	������������� ������ FUNCT3, ����������� ��������������� ���������
CC    CHIP=.TRUE.���������� ���������������� ��������� ��� ��������� ��������� ��������	  
CC    CHIP=0--���������� ���������������� ��������� � ��� ������� ���������� ���	  
      LOGICAL CHIP
      REAL*8 MINONE,MIDONE,MAXONE,HALFON,STEPON
      REAL*8 MINTWO,MIDTWO,MAXTWO,HALFTW,STEPTW
      DATA EPSABS /1.D-5/
      AO=0.0 
      BZ=0.0  
      BR=0.0 

      CSQR=SQRT(1. / 12.)
      CHIP=.TRUE.  
*  ������������ ������������ ���������
      ZQ=Y1*Z0 ! ���������� ���������� �����, �
      RQ=Y2*Z0 ! ���������� ���������� �����, � 
*��������� ��������������� ������� ������� ��� �������� ���������� ����
*  R1   = ��������� ������ �������,�
*  R2   = ������� ������ �������,�
*  Z1   = ���������� ������ �������,� 
*  S1   = ��������� �������,�
*  I1   = ��� �  ����� , �
*  N1   = ���-�� ������ , ���� N1= � , ��
*  I1 - ��������� ���� � ���������� ������� ������� , �/(�**2)
*
      DO 30 ind3=1,MAGSEG
	      R1  =AKAT(ind3)
	      R2  =BKAT(ind3)
	      CON = AMU0 * TKAT(ind3)
	      IF (WKAT(ind3) .NE. 0) 
     &        CON = CON * 2 *  WKAT(ind3) / DKAT(ind3) / (R2-R1)
C! ������� � ������� ���������,��������� � ������� ���������
	      Z  =ZQ-ZKAT(ind3)
	      R  =RQ
	      ZPL=Z+DKAT(ind3)
	      ZML=Z-DKAT(ind3)
*  ����� ���������
      NMBDIV=1 
      BZOLD=0.0  
      BROLD=0.0 
 2    NMBDIV=NMBDIV * 2 
*  �-�� ������ 3-� ���������� ���������� �� ������������
*  �-��� ������ ������� �������
      SUM0=0.0 
      SUM1=0.0  
      SUM2=0.0
*  ����� ��������� �� ������� ������� ���������
      NMBTWO=NMBDIV  
      NMBONE= (R2-R1) / (R2+R1) * 2 * NMBDIV / PI
	IF (NMBONE .LE. 1) NMBONE= 2
      MINTWO=0.0  
	STEPON= (R2-R1) / NMBONE
	STEPTW= (PI-0.) / NMBTWO
*  ������ ���� ���������� �� PHI
      DO 20 ind2=1,NMBTWO
      MAXTWO=MINTWO+STEPTW
      MIDTWO=(MAXTWO+MINTWO) / 2
      HALFTW=STEPTW / 2 * CSQR

      MINONE=R1  
*  ������ ���� ���������� �� RHO
      DO 10 ind1=1,NMBONE
      MAXONE=MINONE+STEPON
      MIDONE=(MAXONE+MINONE) / 2
      HALFON=STEPON * CSQR

      RHO=MIDONE+HALFON
      PHI=MIDTWO+HALFTW
      CALL FUNCT3 (CHIP)

      RHO=MIDONE-HALFON
      CALL FUNCT3 (CHIP)

      PHI=MIDTWO-HALFTW
      CALL FUNCT3 (CHIP)

      RHO=MIDONE+HALFON
      CALL FUNCT3 (CHIP)

      MINONE=MAXONE  
 10   CONTINUE

      MINTWO=MAXTWO  
 20   CONTINUE

      BZNEW=BZOLD  
      BRNEW=BROLD 
      BROLD=CON * SUM1 * (STEPON * STEPTW * 0.5* 0.5) 
      BZOLD=CON * SUM2 * (STEPON * STEPTW * 0.5* 0.5) 
	IF (.NOT.CHIP) GOTO 25
!11:56 11.11.2016 
      IF (NMBDIV .GE. 2**5) GOTO 25
!11:56 11.11.2016 
      IF (DABS(BROLD-BRNEW) .GE. EPSABS) GOTO 2
      IF (DABS(BZOLD-BZNEW) .GE. EPSABS) GOTO 2
 25   CONTINUE
      CHIP=.NOT.CHIP  
	IF (.NOT.CHIP) GOTO 2
      AO=AO+CON * SUM0 *(STEPON*STEPTW*0.5*0.5)! ������� ���������� ���, TL*� 
      BR=BR+BROLD 			       ! ���������� ��������� ��, TL
      BZ=BZ+BZOLD                              ! ���������� ��������� ��, TL
 30   CONTINUE
*  ������������ ��������� ������� ��, ���
      X =AO/BN/Z0
      C2=BR/BN 
      C1=BZ/BN  
      RETURN
      END !FORMAG
!----------------------------------------
CC    FUNCT3.for - For MS FORTRAN 5.1
CC
CC ���������� ���������������� ��������� ����������� ����������
CC ��������� ����������� ���, ��������� ��������� ��������, ����������� 
CC ������� ��������������� ������� �������� ����� � ��������� ������������
CC �������� ������� ��������� �������� 1.E-7 TL
      SUBROUTINE FUNCT3 (CHIP)
!----------------------------------------
      IMPLICIT REAL*8 (A-H,O-Z)
CC    CHIP=1--���������� ���������������� ��������� ��� ��������� ��������� ��������	  
CC    CHIP=0--���������� ���������������� ��������� ��� ������� ���������� ���	  
      LOGICAL CHIP

      COMMON/COMAA/ZPL,ZML,R,RHO,PHI,SUM0,SUM1,SUM2

      RP=RHO*COS(PHI)
      GAMA2=R * R +RHO * RHO -2. * R * RP 
         ZPG=SQRT(ZPL * ZPL + GAMA2)
         ZMG=SQRT(ZML * ZML + GAMA2)
      X1=ZPL+SQRT(ZPL * ZPL +GAMA2) 
      X2=ZML+SQRT(ZML * ZML +GAMA2) 

      FUN0=LOG(X1/X2)
      SUM0=SUM0  +FUN0 * RP 
      FUN1=1./ZMG-1./ZPG
      SUM1=SUM1  +FUN1 * RP  
      FUN2= (R-RP) *(1./(ZPG * X1) -1./(ZMG * X2))
	IF (CHIP) RETURN
	IF (R .NE. 0) FUN2= FUN0/R  +FUN2

      SUM2=SUM2  +FUN2 * RP 
      END !FUNCT3
