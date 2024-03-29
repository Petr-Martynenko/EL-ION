!CC    J.FOR MS FORTRAN 5.1
!CC
!CC    20.11.89
!----------------------------------------
      SUBROUTINE INTGR
!----------------------------------------
      IMPLICIT REAL*8 (A-H,O-Z)
      INCLUDE '_PARAM.FOR'
      INCLUDE '_COMMON.FOR'

!     Main program variables

      REAL*8 UVX(N4)
!
      DO 1 I=1,IDIM
       FP(I)=0D0
 1    CONTINUE
!
      IF ( ISTART .EQ. 0 ) THEN
       IF ( NITER .GE. NITM ) REWIND 5
       IF ( NITER .GE. NITM ) WRITE (5) UX(0)*Z0,VY(0)*Z0
	   PERVO=0D0
	   Q1=1D0
      END IF
      IF ( ISTART .EQ. 1 ) THEN
	   CALL FLUN56
	   REWIND 6
	   READ (6) UX(0),VY(0)
	   UX(0)=UX(0)/Z0
	   VY(0)=VY(0)/Z0
	   Q1=0.
      END IF
!
      IF ( NITER .EQ. 1 )       WRITE(*,'(/81(1H*))')
      IF (.NOT. MAG ) WRITE(*,'(T3,''N   Z-KOOP.	R-KOOP.'',
     & T25, ''Z-SPEED   R-SPEED   l-SPEED  CHARGE'')' )

      IF ( MAG ) WRITE(*,'(T3,''N   Z-KOOP.	R-KOOP.'',T25,
     &''Z-SPEED  R-SPEED  L-SPEED   F-SPEED	T-SPEED	CHARGE'')')
!
!	GAMA=�������������� ���������
!
      IF ( NITER .LE. 30 ) THEN
         GAMA=GAMAS(NITER)
      ELSE
         GAMA=GAMAS(30)
      END IF
!
!       ������� ��������� �������
!
      DO 10 I=1,IZZ
!
      IF ( ISTART .EQ. 1 ) THEN
	   READ (6) Y,PCIMG(I),UX(I),VY(I)
	   Y(1)=Y(1)/Z0
	   Y(2)=Y(2)/Z0
	   UX(I)=UX(I)/Z0
	   VY(I)=VY(I)/Z0
	   PCIMG(I)=PCIMG(I)/Z0
	   AL0=DSQRT((UX(I-1)-UX(I))**2+(VY(I-1)-VY(I))**2)
	   L=1
	   Q1=0D0
      END IF
!
      IF ( ISTART .EQ. 0 ) THEN
      IF ( NAMCAT .EQ. 'CATOD1' ) THEN
      CALL DISTN( UX(I-1), VY(I-1), UX(I), VY(I), Y(1), Y(2), DR)
		AL0=DSQRT((UX(I-1)-UX(I))**2+(VY(I-1)-VY(I))**2)
      END IF
!
      IF ( NAMCAT .EQ. 'CATOD2' ) THEN
	  FO = FHO + (2 * I - 1) * DFO
	  Y(1)=UX(I)-DR * DCOS (FO)
	  Y(2)=VY(I)-DR * DSIN (FO)
      END IF
!
!	����������� ���������� � ����� ������
!
      CALL FORCE(POTX(I),Y(1),Y(2),C1,C2)
      IF (IGMA .EQ. 0 .OR. POTX(I).LE.0.) GOTO 10
      VO=SQRT(ABS(POT(-IGM(NUMB))-POTX(I)))
!
!	��������� ���� ��������
!
      UVO=VO*VO*VO
      UVX(I)=1.D+10
      UVO=UVOX(I)+GAMA*(UVO-UVOX(I))
      FOX=UVOX(I)*EPS
      FOX=DABS(FOX)
      IF (DABS(UVO-UVOX(I)) .GE. FOX) Q1=0D0
      IF (UVO .GT. UVOMAX) UVO=UVOMAX
      IF (UVOX(I) .NE. 0.0) UVX(I)=UVO/UVOX(I)
      UVOX(I)=UVO
!
!	����������� ��������� � ����� ��������
!
		Y(1)=UX(I)
		Y(2)=VY(I)
		IF ( NAMCAT .EQ. 'CATOD1' ) THEN
		   Y(1)=.5*(Y(1)+UX(I-1))
		   Y(2)=.5*(Y(2)+VY(I-1))
		END IF
		Y(3)=0D0
		Y(4)=0D0
		Y(5)=PJA*UVO*AL0*Y(2)
		PERVO=PERVO+PMA*AL1*Y(5)
		L=0
      END IF
      IGMA=1
      CALL INTEGR(I,L)
 9    FORMAT (' PER=',F6.4,'(MICROAMPS/V**1.5) FG=',I7,' (V)  I=',
     & G8.3,'(AMPS) NITER=',I4,' G=',F4.3)
 10   CONTINUE
      IF ( ISTART .EQ. 0 ) THEN
        WRITE(*,9)  PERVO,INT4(FG),PERVO*FG**1.5*1.E-6,NITER,GAMA
           END IF
      WRITE(*,'(T3,''START POINT POTENTIALS  NORMALIZED FOR FG='',
     & I7,'' (V)'')')INT4(FG)
      IF ( NITER .EQ. 1 ) THEN
        DO I=1,IZZ
	  FO = FHO + (2 * I - 1) * DFO
            WRITE (*,'(F10.3,F10.6)') FO, POTX(I)
              END DO    
                END IF
      IF ( NITER .NE. 1 ) THEN
          WRITE(*,
     & '(T3,''RATIO OF CURRENT DENCITY FOR FG='',I7,'' (V)'')')INT4(FG)
          WRITE(*,'(8F10.3)') (UVX(I),I=1,IZZ)
!
!       ������� ��������� ���� �� ������
!
      UVMA=1.D-7
      DO 20 I=1,IZZ
      UVX(I)=0D0
      IF (POTX(I) .GE. 0.0)
     & UVX(I)=UVOX(I)*PJA !*PMA*FG**1.5*1.E-10/Z0**2
      UVMA=AMAX1(UVMA,UVOX(I))
 20   CONTINUE  
          WRITE(*,'(T3,
     & ''CURRENT DENCITY  NORMALIZED FOR FG='',I7,'' (V)'')')INT4(FG)
          WRITE(*,'(8F10.5)') (UVX(I),I=1,IZZ)
      END IF
      IF (Q1 .LT. 0.5 .OR. NITER .GE. NITM ) THEN
      PRINT '(/81(1H*))'
      PAUSE
      END IF
!*=== End of subroutine INTGR =========================================*
      END
!----------------------------------------
!
! ���������� �������������� ��������� ��������
! ���������������� ������  ���������� ����
!
      SUBROUTINE INTEGR(INZ,L)
!*  INTEGER INZ - inp  ! ����� ������� 
!*  INTEGER L   - inp  ! �������� �������� 
!    L=1--�������������� �������	  
!    L=0--����������� ������������ ����	  
!----------------------------------------
      IMPLICIT REAL*8 (A-H,O-Z)
      INCLUDE '_PARAM.FOR'
      INCLUDE '_COMMON.FOR'

      REAL*8  Y0(4),YI(5),CAL1(3),CAL2(4),DER(4)      !
      LOGICAL IFL
      DATA COND3 /.3333333333333/
      VFE=0.0
      RTE=0.0
      IFL=.FALSE.
      IF ( NITER .GE. NITM ) IFL=.TRUE.
      ZZ= XU(0) + ZSCHIF
!    L=0--����������� ������������ ����	  
 1    IF (L .EQ. 0) THEN
	   DT=3.*DR/VO
	   IF (NAMCAT .EQ. 'CATOD1') CALL CATOD1(INZ,YI)
	   IF (NAMCAT .EQ. 'CATOD2') CALL CATOD2(INZ,YI)
	   L=L+1
	   DO 5 J=1,4
	   Y(J)=YI(J)
 5	   CONTINUE
	   IF(MAG)THEN
C *************
           Z=Y(1)
	   R=Y(2)
	   R2=R*R
*      REAL*8 AO  - out  ! ������� ���������� ���, TL*�
*      REAL*8 Z   - inp  ! ���������� ���������� ����� .�
*      REAL*8 R   - inp  ! ���������� ���������� ����� .�
*      REAL*8 BZ  - out  ! ���������� ��������� ��, TL
*      REAL*8 BR  - out  ! ���������� ��������� ��, TL
	   CALL FORMAG (AO,Z,R,BZ,BR)
	   TETA=(R*AO-PCIMG(INZ))/R2*ZNAK
	   RTE=R*TETA
	   VS2=VO+RTE*RTE
           PRINT 4, INZ, Y(1), Y(2), Y(3), Y(4), VO, RTE, VO, Y(5)*DT
	   END IF
      IF (.NOT. MAG )
     &	 PRINT 4, INZ, Y(1), Y(2), Y(3), Y(4), VO, Y(5)*DT
      ELSE
!    L=1--�������������� �������	  
! ***********************************************
	   VS2=Y(3)*Y(3)+Y(4)*Y(4)
!    ���������� �������� ���� �������������� ���������	  
! ***********************************************
	   DT=STEP/ DSQRT(VS2)
	   IF( Y(2)  .LT. 1. ) DT=DT/10.
	   CAL1(1)=.5*DT
	   CAL1(2)=.5*DT
	   CAL1(3)=   DT
	   CAL2(1)=COND3*CAL1(1)
	   CAL2(2)=COND3*CAL1(3)
	   CAL2(3)=COND3*CAL1(3)
	   CAL2(4)=COND3*CAL1(1)
	   
	   DO 10 J=1,4
	   YI(J)=Y(J)
	   Y0(J)=Y(J)
 10    CONTINUE
	   DO 30 J=1,4
	   IGMA=INZ
	   CALL FORCE(F0,YI(1),YI(2), C1,C2)
	   C1=.5*C1
	   C2=.5*C2
	   IF (IGMA .EQ. 0) GOTO 45
	   IF(MAG)THEN
! *************
           Z=YI(1)
	   R=YI(2)
	   R2=R*R
*      REAL*8 AO  - out  ! ������� ���������� ���, TL*�
*      REAL*8 Y1  - inp  ! ���������� ���������� ����� .�
*      REAL*8 Y2  - inp  ! ���������� ���������� ����� .�
*      REAL*8 BZ  - out  ! ���������� ��������� ��, TL
*      REAL*8 BR  - out  ! ���������� ��������� ��, TL
	   CALL FORMAG (AO,Z,R,BZ,BR)
	   TETA=(R*AO-PCIMG(INZ))/R2*ZNAK
	   RTE=R*TETA
	   C1=C1+RTE* BR
	   C2=C2-RTE*(BZ-TETA*ZNAK)
	   END IF
! ***********************************************
	   DER(1)=YI(3)
	   DER(2)=YI(4)
	   DER(3)=C1*ZNAK
	   DER(4)=C2*ZNAK
	   IF(J.GE.4) GOTO 31
	   DO 20 I=1,4
	   YI(I)=Y(I)+DER(I)*CAL1(J)
	   Y0(I)=Y0(I)+DER(I)*CAL2(J)
 20    CONTINUE
 30    CONTINUE
 31    DO 40 I=1,4
	    YI(I)=Y0(I)+DER(I)*CAL2(4)
 40    CONTINUE
! ***********************************************
	   VS2=YI(3)*YI(3)+YI(4)*YI(4)
	   VO=DSQRT(VS2)
	   VS2=VO*VO+RTE*RTE
      END IF
! *************
 45   DT=DT               !!!/3.
      YM1=(YI(3)+Y(3))*DT*.5
      YM2=(YI(4)+Y(4))*DT*.5
      YI(5)=Y(5)*DT
      DO 50 I=1,1        !!!3
       Z=Y(1)+YM1* (2*I-1)
       R=Y(2)+YM2*(2*I-1)
       IF(R.LT.0.)R=-R
! **** �������� ������ ��������� �� ������� ������������� ������� ***
       IF ( Z .GE.XU(IZL).OR. Z .LT.XU(0)
     + .OR. R .GE.YV(IRL).OR. R .LT.YV(0)) GOTO 51
       Z1=Z-INT(Z)
       R1=R-INT(R)
       IZT=Z-XU(0)
       IRT=R-YV(0)
       IF(R .LT. 1.)R=0.5
       YRT=YI(5)/R*ZNAK
! *************
!    ���������� ������� ��������� ������	  
! ***********************************************
       IND0=IZT+IZLIM1*IRT+1
       IND1=IND0+IZLIM1
       IND2=IND0+1
       IND3=IND0+IZLIM1+1
       FP(IND0)=FP(IND0)+YRT*(1.-Z1)*(1.-R1)
       FP(IND1)=FP(IND1)+YRT*(1.-Z1)*    R1
       FP(IND2)=FP(IND2)+YRT*    Z1 *(1.-R1)
       FP(IND3)=FP(IND3)+YRT*    Z1 *    R1
 50   CONTINUE
 51   CONTINUE
      Y(1)=YI(1)
      Y(2)=YI(2)
      Y(3)=YI(3)
      Y(4)=YI(4)
! *************
!    ��������� ���������� �� ��� ���������	  
! ***********************************************
      IF (Y(2) .LT. 0.) THEN
       Y(2)=-Y(2)
       Y(4)=-Y(4)
      END IF
! *************
!    ����������� ��������� � ������� ZZ	  
! ***********************************************
      IF (IFL) THEN
	   IF(Y(1) .LT. ZZ)THEN
	   RZ=Y(2)
	   VZ=Y(3)
	   VR=Y(4)
	   VFE=RTE
	   IF( Y(3) .NE. 0.) THEN
	   VRZ= VR / VZ
	   RZ= RZ*(ZZ-Y(1))*VRZ
	   END IF
	   ELSE
	    IFL=.FALSE.
	   END IF
      END IF
	   IF (IGMA .EQ. 0) GOTO 60
      IF( Y(1).LE.(XU(IZL)-STEP)) GOTO 1
! *************
!    ����������� ��������� � �������� �������	  
! *************    X=XU(IZL)
      IF( Y(3) .NE. 0.) THEN
           R=Y(2) + (XU(IZL)-Y(1))*Y(4)/Y(3)
                IF(R .LT. 0.)R=-R
                   Y(1)=XU(IZL)
                         END IF
                             Y(2)=R

 60   IF ( NITER .GE. NITM ) THEN
        IF (IFL) THEN
        ZZ=Y(1)
        RZ=Y(2)
        VZ=Y(3)
        VR=Y(4)
        ENDIF
! *************
!   ����� �������� ��������� � ������������� �������	  
! ***********************************************
      WRITE(5)ZZ*Z0,RZ*Z0,VZ,VR,Y(5),PCIMG(INZ)*Z0,UX(INZ)*Z0,VY(INZ)*Z0
      IF ( MAG )
     &	 PRINT 4, INZ, ZZ, RZ, VZ,VR, VO, RTE,  SQRT(VS2),Y(5)*DT
      IF (.NOT. MAG )
     &	 PRINT 4,INZ,ZZ,RZ,VZ,VR,VO,Y(5)*DT
      END IF
 4     FORMAT (I3,2F10.4,3F9.4,3G10.4,2I4)
!*=== End of subroutine  INTEGR =========================================*
      END
