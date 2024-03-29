C!    H.FOR MS FORTRAN 5.1
C!
C!    11.10.89
!----------------------------------------
      SUBROUTINE MAGFD
!----------------------------------------
!***
!* n-�� ����� ���������� ���������� ���� � ������� ���������� ���� �� ���
!**gb
      IMPLICIT REAL*8 (A-H,O-Z)

      PARAMETER (KK = 3)
      INCLUDE '_PARAM.FOR'
      INCLUDE '_COMMON.FOR'

!     Main program variables

      REAL*8 BAXES, R1, R2, P1, P2, Z1, S1, I1,XUZ
!�������-�������� "BAXES" ���������� �������� ��������� �������� �� ���
      BAXES(R1, R2, P1, P2)=
     & P1*DLOG((R2+DSQRT(R2*R2+P1*P1))/(R1+DSQRT(R1*R1+P1*P1)))
     +-P2*DLOG((R2+DSQRT(R2*R2+P2*P2))/(R1+DSQRT(R1*R1+P2*P2)))
	  
*
*������� �������� ���������� � ��������	  
*  FG   = ���������� �� �����, V
*  AMU0 = ��������� ������������� �������: 4*PI, ��/m
*  BN   = ���������� ��������� ��������, Wb/m/m
*  Z0   = ���������� �������� ��������, m/���.���
*  DEL  = ��������� ������ ��������� � �����, ��/��
*
      BN = SQRT(2.*FG/DEL)/Z0
!    PRINT -- ������ �������� ����������	  
      WRITE (9,'(T3,''AMU0'',T18,''FG'',T28,''Z0'',
     + T38,''BN'',T46,''MAGSEG'',T56,''MAGORD''/
     + 1X,E10.3,F10.0,2F10.3,2I10)')
     + AMU0, FG, Z0, BN, MAGSEG, MAGORD
      DO 10 I= -6, IZMAX
	      BZA(I)=0D0
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
      DO 18 I=1,MAGSEG
	      R1  =AKAT(I)
	      R2  =BKAT(I)
	      Z1  =ZKAT(I)
	      S1  =DKAT(I)
	      I1  =TKAT(I)
	      N1  =WKAT(I)
C! ������ ���������� ��������������� ������� �������
	      CON =PI * AMU0 * I1 * N1 / S1 / (R2-R1)
      WRITE (9,'(T8,''R1,m'',T18,''R2,m'',T28,''Z1,m'',
     + T38,''S1,m'',T46,''I1,A'',T58,''N1'',T68,''CON''/
     + 5F10.4,I10,F10.3)')
     + R1, R2, Z1, S1, I1, N1,CON
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
      WRITE (9,'(80(1H*))')
      WRITE (9,'(3(F7.3,1H:,G14.7,2H ;))')
     +  (Z0*(I+XU(0)),BZA(I),I=-6, IZMAX)
      DO I= -6, IZMAX
	      BZA(I)=BZA(I)/ BN
                     END DO
                               PAUSE
      END !MAGFD
!----------------------------------------
      SUBROUTINE MAGCAT 
!----------------------------------------
      IMPLICIT REAL*8 (A-H,O-Z)
      INCLUDE '_PARAM.FOR'
      INCLUDE '_COMMON.FOR'


C     Main program variables
	  
      REAL*8 Z    !���������� �-�� �����,���� 
      REAL*8 R    !���������� �-�� �����,����.
      REAL*8 PCI  !������� ���������� ���,����. 
C!      REAL*8 Bx2  !���������� ���������� ��,����.
C!      REAL*8 By2  !���������� ���������� ��,����.
      LOGICAL PrintFile
      DATA PrintFile /.TRUE./!TRUE - ���� ����� �� �� CONSOLE

      DO 20 J=1,IZZ
       Z= UX(J)
       IF(NAMCAT.EQ. 'CATOD1' )Z=0. 5*( Z+UX(J-1) )
       R= VY(J)
       IF(NAMCAT.EQ. 'CATOD1' )R=0.5*(R+VY(J-1))
*
*      REAL*8 PCI - out  ! ������� ���������� ���, ����.
*      REAL*8 Z   - inp  ! ���������� ���������� ����� .���.��.
*      REAL*8 R   - inp  ! ���������� ���������� ����� .���.��.
*      REAL*8 BZ  - out  ! ���������� ��������� ��,����.
*      REAL*8 BR  - out  ! ���������� ��������� ��,����.
    
	     CALL  FORMAG (PCI,Z,R,BZ,BR)
       PCIMG(J)=       PCI
 20    CONTINUE
C! *** PRINT ***     �������� ��� �� ������ ***
      IF ( PrintFile ) THEN
      WRITE (9, '('' �������� ��� �� ������,Wb/m'')')
      WRITE (9, '(5E14.7,2H ;))')(PCIMG(J)*Z0*BN,J= 1, IZZ)
      END IF
C! ***********************************************
      DO 30 J= 1, IZZ
 	     R=VY(J)
	     IF (NAMCAT .EQ. 'CATOD1') R=0.5*(R+VY(J-1))
	     PCIMG(J)=PCIMG(J)*R 
 30    CONTINUE
      WRITE (9,'('' �������� ���������� ������ �� ������,Wb'')')
      WRITE (9,'(3E14.7,2H ;))')
     +  (PI*2*PCIMG(J)*BN*Z0*Z0,J= 1, IZZ)

      END !MAGCAT
!----------------------------------------
!
! Program GRA4WIN.FOR ������ �������� ���������� ����
!
      INCLUDE  'FGRAPH.FI'
      SUBROUTINE GRAF4WIN
      IMPLICIT REAL*8 (A-H,O-Z)
      INCLUDE  'FGRAPH.FD'
      INCLUDE '_PARAM.FOR'
      INCLUDE '_COMMON.FOR'
	  
      REAL*8 X1,Y1,X2,Y2,X
      REAL*8 XMAX,YMAX,XMIN,YMIN,XL,YL
      INTEGER*2      i, status
      INTEGER*4      oldcolor
      RECORD /wxycoord/ wxy
      RECORD / videoconfig / vc
      LOGICAL*2   finvert / .TRUE. /
      DATA NK4 /4/ !����� ������ ��������������
      DATA NK2 /2/

      IF (MI .EQ. 0) RETURN
!
!     Find graphics mode.
!
      IF( setvideomode( $MAXRESMODE ) .EQ. 0 ) 
     &    STOP 'Error:  cannot set graphics mode'
      CALL getvideoconfig( vc )
      CALL SETTEXTWINDOW( INT2(1), INT2(2), INT2(40), INT2(40))

      WRITE (9, *)
     &'�������� ��������� GRAF4WIN ************************************'
      WRITE (9,'('' mastab'',E10.3)')S
      IF (MAG) THEN 
	     BMA=BZA(IZMAX)
	     BMI=BZA(IZMAX)
	     DO J=-6,IZMAX
           IF (BMI .GT. BZA(J))BMI=BZA(J)
      END DO
	     DO J=-6,IZMAX
           IF (BMA .LT. BZA(J))BMA=BZA(J)
      END DO
	     BMA=BMA*BN
	     BMI=BMI*BN
	     BUY=(BMA-BMI)/2.
	     WRITE (9, *) ' BMI=',BMI,' TL       BMA=',BMA,' TL'
      ENDIF
! ����������� �������� �������

      XMAX=FLOAT(IZL)
      YMAX=FLOAT(IRL)
      XMIN=0.0
      YMIN=0.0
      XL=XMAX*S
      YL=YMAX*S
! ����������� �������

      status = SETWINDOW( finvert , XMIN,YMIN, XMAX,YMAX)

! ���������� ���� X,Y

      oldcolor = 3
      status = SETCOLOR( oldcolor ) ! full green for axes
!
!	    Draw vertical and horizontal lines. Vertical line style
!	    is anything other than horizontal style. Since lines
!	    are overdrawn with several line styles, this has the
!	    effect of combining colors and styles.
!
!	    status = #F00F ! X * 256.0
CALL setlinestyle( status)  
      DO i = 0, IZL
         x=FLOAT(i)
         CALL MOVETO_W( x, YMIN, wxy )
         status = LINETO_W( x, YMAX )
      END DO
      DO i = 0, IRL
         x=FLOAT(i)
         CALL MOVETO_W( XMIN, x, wxy )
         status   = LINETO_W(  XMAX, x )
      END DO

! ���������� ������� �������

      status = SETCOLOR( $MAGENTA )
      IO=0
      DO I = 1, ABS(MS)
      I1=I-1
      IF (I.EQ.1) I1=ABS(MS)
      R1=BDR(I1)
      Z1=BDZ(I1)
      N=IEX(I1)
      R2=BDR(I)
      Z2=BDZ(I)
      IF (I.EQ. 1) CALL MOVETO_W( Z2, R2, wxy )
      IF (MI.LT.0) THEN
! ���������� ��������� ����� ������� �������
	     X1=Z1+S
	     Y1=R1+S
	     X2=Z1-S
	     Y2=R1-S
	     status = ELLIPSE_W( $GFILLINTERIOR, X1, Y1, X2, Y2)
!	    Returns:  a nonzero value if the ellipse is drawn successfully,
!	    or 0 if not.
	   IF(status.EQ.0)STOP'Error:the ellipse is drawn successfully'  
      ELSE
! ���������� ������� �������
        IF (N .NE. 0) THEN 
         IO=IO+1
!
!	����������� ��������� ����� ���������� X0,Y0
!
        IF(N .EQ. 1)CALL CORCR(Z1,R1,Z2,R2,X0,Y0, ROK(IO))
        IF(N .EQ.-1)CALL CORCR(Z1,R1,Z2,R2,X0,Y0,-ROK(IO))
         X1=X0+ROK(IO)
         Y1=Y0+ROK(IO)
         X2=X0-ROK(IO)
         Y2=Y0-ROK(IO)
        IF(N .EQ.-1)status = ARC_W( X1, Y1, X2, Y2, Z2, R2, Z1, R1 )
        IF(N .EQ. 1)status = ARC_W( X1, Y1, X2, Y2, Z1, R1, Z2, R2 )
         CALL MOVETO_W( Z2, R2, wxy )
        ELSE
         status = LINETO_W( Z2, R2)
        END IF
      END IF
      END DO

!
! ����������  ���������������
!
      IF (ABS(MI) .EQ. 2) GOTO 100
      NUMB=0
      EQLN=3  ! ������.���������� �������� ��������� ���������
      EQST=3  ! ���������� ������� ���������� ���� ��������������
      OPEN (UNIT=4,
     &FILE='F4',STATUS='UNKNOWN',ACCESS='SEQUENTIAL',FORM='UNFORMATTED')
      DO 50 J=IZZ,1,-1
      NUMB=NUMB+1
	     REWIND (NK4)
             IF (NUMB.LE.NO.AND.(MOD(J,M4).EQ.0 .OR. J.EQ.1)) THEN
	     Z=UX(J)
	     IF (NAMCAT .EQ. 'CATOD1') Z=0.5*(Z+UX(J-1))
	     R=VY(J)
	     IF (NAMCAT .EQ. 'CATOD1') R=0.5*(R+VY(J-1))
	     EQUIPR=R
	     FF=PCIMG(J)
C! *** CALCULATES EQUIPOTENTIAL LINE ***
C!*  REAL*4 Z - inp  ! ���������� ���������� �����, ����.
C!*  INTEGER ND - inp  ! 
	     CALL EQUIPM(FLOAT(Z),1)
!
! ���������� ��������������, ����� �������� �� ���������� ������
!
      REWIND (NK4)
 81   READ (NK4, END=85) FF,MR,(PX(I),PY(I), I=1,MR)
	     FF=2*PI*FF*BN*Z0*Z0
      WRITE (9, *)' FF=',REAL(FF),' Wb     ',MR,'  points FOR LINE�=',J

!      DO 82 I=1,IZZ
!       IF ( FF .EQ. PCIMG(I)) NUMB=I
! 82   CONTINUE
      oldcolor = NUMB
      status = SETCOLOR( oldcolor)
	     X1=PX(1)
	     Y1=PY(1)
             CALL MOVETO_W( X1,Y1, wxy )
      DO 83 I=2,MR
	     X1=PX(I)
	     Y1=PY(I)
             status = LINETO_W(X1,Y1)
 83   CONTINUE
      GOTO  81
      END IF
 85   CONTINUE
 50   CONTINUE
!
! ���������� ���������� ������
!
      IF (ABS(MI) .EQ. 2) GO TO 100
      NUMB=0
!
! ���������� ������� ���������� ������
!
      REWIND (NK2)
      DO 70 I=1,IZZ
	   IF (MOD(I,M4).EQ.0 .OR. I.EQ.1) THEN
	     READ (NK2, END=71) MTM,(PX(J),PY(J),J=1,MTM)
	     IF (MTM.GT.1) THEN
      NUMB=NUMB+1
      oldcolor = NUMB
      status = SETCOLOR( oldcolor)
	     Z1=PX(1)
	     R1=PY(1)
	     X1=Z1+S
	     Y1=R1+S
	     X2=Z1-S
	     Y2=R1-S
	     status = ELLIPSE_W( $GFILLINTERIOR, X1, Y1, X2, Y2)
!	    Returns:  a nonzero value if the ellipse is drawn successfully,
!	    or 0 if not.
	     IF(status.EQ.0) STOP 'Error:  cannot set graphics mode'

             CALL MOVETO_W( Z1, R1, wxy )
             DO j=2,MTM
		Z1=PX(j)
		R1=PY(j)
                status = LINETO_W( Z1, R1)
             END DO
             END IF
	   END IF
 70   CONTINUE
 71   CONTINUE
 100  status = RECTANGLE_W( $GBORDER, XMIN,YMIN, XMAX,YMAX)
      PAUSE
      END !GRAF4WIN

!----------------------------------------
      SUBROUTINE DSPROM( IEQQ, EQB, AO, RHO, ZETA, C1, C2, *)
C *** INCREMENTS EQUIPOTENTIAL LINE ***
*  INTEGER IEQQ - inp  ! ������.���������� �������� ��������� ���������  
*  LOGICAL EQB  - inp  !
*  REAL*8 RHO   - inp  ! ���������� ���������� ����� .���.��.
*  REAL*8 ZETA  - inp  ! ���������� ���������� ����� .���.��.
!----------------------------------------
      IMPLICIT REAL*8 (A-H,O-Z)
      INCLUDE '_PARAM.FOR'
      INCLUDE '_COMMON.FOR'
C!    EQB = .TRUE.--����������
C!    EQB=0--
      LOGICAL EQB

      EQB = .TRUE.
      DS = 1.0
C! *** ��������� �������� ����� �������� ***
      L=0
C! *** ���� ��������� ��������� ���������� �������������� ***
 10   CONTINUE
      IF (RHO .LT. 1.E-10) GOTO 40
C! *** ������ �������� ���� ***
      IF (DS .LE. 0.0001) RETURN
C! *** ���������� ���������� �������� ***
      IF (L .GT. IEQQ) RETURN
C!*      REAL*8 AO  - out  ! ������� ���������� ���, ����.
C!*      REAL*8 ZETA- inp  ! ���������� ���������� ����� .���.��.
C!*      REAL*8 RHO - inp  ! ���������� ���������� ����� .���.��.
C!*      REAL*8 BZ  - out  ! ���������� ��������� ��,����.
C!*      REAL*8 BR  - out  ! ���������� ��������� ��,����.
      CALL  FORMAG (AO,ZETA,RHO,C1,C2)
      DS = C1*C1+C2*C2
      IF (DS .LT. 1.E-6) DS = 1.0
      DELTA = FF/RHO-AO
!C *** �������� ����������� �������� ������� � ��������� ***
      IF (DABS(DELTA) .LE. 0.0001) RETURN
      DS = DELTA / DS
!C *** ���������� ���� �� ����������� ***
      RHO=RHO+C1*DS
      ZETA=ZETA-C2*DS
      IF (DABS(C2) .LE. DABS(C1)) GOTO 20
      DS = C2*DS
      GOTO 30 
 20   DS = C1*DS
!C *** ���������� �������� ����� ***
 30   L=L+1
       GOTO 10 
 40   RETURN 1
      END !DSPROM
!----------------------------------------
      SUBROUTINE EQUIPM(FZ,ND)
C! *** CALCULATES EQUIPOTENTIAL LINE ***
C!*  INTEGER ND - inp  ! 
C!*  REAL*4 FZ - inp  ! ���������� ���������� , ����.
C!----------------------------------------
      IMPLICIT REAL*8 (A-H,O-Z)
      INCLUDE '_PARAM.FOR'
      INCLUDE '_COMMON.FOR'
      REAL*4 FZ

      ND = 1
C! *** SS DETERMINES SIGN (I.E., DIRECTION) OF EQUIPOTENTIAL LINE ***
      DO 50 LLL=1,2
       SS = 1.0
	   IF (LLL .EQ. 2) SS = -1.0
       DELT = 1./EQST
       RHO=EQUIPR  !��������� ���������� ���������� EQUIPOTENTIAL LINE, ���.��.
       ZETA=FZ	   !��������� ���������� ���������� EQUIPOTENTIAL LINE, ���.��.
*      REAL*8 AO  - out  ! ������� ���������� ���, ����.
*      REAL*8 ZETA- inp  ! ���������� ���������� ����� .���.��.
*      REAL*8 RHO - inp  ! ���������� ���������� ����� .���.��.
*      REAL*8 BZ  - out  ! ���������� ��������� ��,����.
*      REAL*8 BR  - out  ! ���������� ��������� ��,����.
	   CALL  FORMAG (AO,ZETA,RHO,BZ,BR)

	   TERM3=DABS(BZ)+DABS(BR)
	   IF (TERM3 .LT. 1.E-10) RETURN
       DELT1 = ND / DSQRT(BZ*BZ+BR*BR)
       RPROB=RHO +BR*DELT1*SS
       ZPROB=ZETA+BZ*DELT1*SS
C ***********************************************
      IF ( RPROB .GE. YV(IRL).OR. ZPROB .LE. XU(0)
     + .OR. ZPROB .GE. XU(IZL)) GOTO 50
C ***********************************************
       CALL  LISTM (SS,RHO,ZETA)
 50   CONTINUE
      END !EQUIPM
!----------------------------------------
CC    FORMAG.FOR
CC
CC    ���� ����� �������� �� 19-04-91
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
      INCLUDE '_COMMON.FOR'
      COMMON/COM A/ZPL,ZML,R,RHO,PHI,SUM0,SUM1,SUM2

C     Main program variables
CC    CHIP --	������������� ������ �-���, ����������� ��������������� ���������
CC    CHIP=1--���������� ���������������� ��������� ��� ��������� ��������� ��������	  
CC    CHIP=0--���������� ���������������� ��������� ��� ������� ���������� ���	  
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
     1        CON = CON * WKAT(ind3) / (2 * DKAT(ind3)) / (R2-R1)
C! ������� � ������� ���������,��������� � ������� ���������
	      Z  =ZQ-ZKAT(ind3)
	      R  =RQ
	      ZPL=Z+DKAT(ind3)
	      ZML=Z-DKAT(ind3)
*  ����� ���������
      NMBDIV=1 
      BZOLD=0.D0  
      BROLD=0.D0 
 2    NMBDIV=NMBDIV * 2 
*  �-�� ������ 3-� ���������� ���������� �� ������������
*  �-��� ������ ������� �������
      SUM0=0.D0 
      SUM1=0.D0  
      SUM2=0.D0
*  ����� ��������� �� ������� ������� ���������
      NMBTWO=NMBDIV  
      NMBONE= (R2-R1) / (R2+R1) * 2 * NMBDIV / PI
	IF (NMBONE .LE. 1) NMBONE= 2
      MINTWO=0.D0  
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
      BROLD=CON * SUM1 * (STEPON * STEPTW * 0.5) 
      BZOLD=CON * SUM2 * (STEPON * STEPTW * 0.5)  
	IF (.NOT.CHIP) GOTO 25
      IF (DABS(BROLD-BRNEW) .GE. EPSABS) GOTO 2
      IF (DABS(BZOLD-BZNEW) .GE. EPSABS) GOTO 2
 25   CONTINUE
      CHIP=.NOT.CHIP  
	IF (.NOT.CHIP) GOTO 2
      AO=AO+CON * SUM0 *(STEPON * STEPTW * 0.5)! ������� ���������� ���, TL*� 
      BR=BR+BROLD 			       ! ���������� ��������� ��, TL
      BZ=BZ+BZOLD                    ! ���������� ��������� ��, TL
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
      SUBROUTINE FUNCT3 (CHIP)
!----------------------------------------
      IMPLICIT REAL*8 (A-H,O-Z)
CC    CHIP=1--���������� ���������������� ��������� ��� ��������� ��������� ��������	  
CC    CHIP=0--���������� ���������������� ��������� ��� ������� ���������� ���	  
      LOGICAL CHIP

      COMMON/COM A/ZPL,ZML,R,RHO,PHI,SUM0,SUM1,SUM2

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
!----------------------------------------
      SUBROUTINE LISTM(SS,RHO,ZETA)
! *** LISTS AND DRAWS EQUIPOTENTIAL LINE ***
*  REAL*8 RHO   - inp  ! ���������� ���������� ����� .���.��.
*  REAL*8 ZETA  - inp  ! ���������� ���������� ����� .���.��.
!----------------------------------------
      IMPLICIT REAL*8 (A-H,O-Z)
      INCLUDE '_PARAM.FOR'
      INCLUDE '_COMMON.FOR'

       J=1
       ZSTART=ZETA
 20    CALL DSPROM( EQLN, .TRUE., AO, RHO, ZETA, C1, C2, *30)
       IF ( ZETA .GT.XU(IZL).OR. ZETA .LT.XU(0)) GOTO 25
       IF (  RHO .GT.YV(IRL).OR.  RHO .LT.YV(0)) GOTO 25
       PX(J) = ZETA
       PY(J) = RHO
       J=J+1
       TERM3=DABS(C1)+DABS(C2)
       IF (TERM3 .LT. 1.E-10) GOTO 27
       IF ( J .GT. MT ) GOTO 30
       DELT1 = DELT/ DSQRT(C1*C1+C2*C2)
       RHO=RHO+C2*DELT1*SS
       ZETA=ZETA+C1*DELT1*SS
       GOTO 20 
 25   CONTINUE
       GOTO 30 
 27   CONTINUE
! *** ���� ���������� �������������� �������� ***
 30   J=J-1
       IF ( J .LE. 1 ) GOTO 40
       WRITE (4) FF,J,(PX(I),PY(I), I=1,J)
 40   CONTINUE
	   RHO=EQUIPR
	   ZETA=ZSTART
      END !LISTM
