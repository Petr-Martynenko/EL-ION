!CC    G.FOR MS FORTRAN 5.1
!CC   ���������� ���������� ���������������� ������ � �������� �����
!CC   ���������� �������� ��������� ����������
!CC    Created on 11.10.89
!----------------------------------------
      SUBROUTINE INTEGF(INZ,L)
!*                                                                      *
!*    Input variables:                                                  *
!*                                                                      *
!*          INZ=  ����� ����������                                      *
!*          L = PARAMETER                                               *
!*                                                                      *
!*    Output variables:                                                 *
!*                                                                      *
!----------------------------------------
      IMPLICIT REAL*8 (A-H,O-Z)
      INCLUDE '_PARAM.FOR'
      INCLUDE '_COMMON.FOR'

      REAL*8 Z, R
      REAL*8 Y0(4),YI(5),CAL1(3),CAL2(4),DER(4)      !
      DATA COND3 /.3333333333333/
      DATA NK2 /2/ !����� ������ ����������
      RTE=0.0
      VFE=0.0
      MTM=0
      DELEE=0.0
      EE=POT(-IGM(NUMB))
!    L=0--����������� ������������ ����	  
 1    IF (L .EQ. 0) THEN
	   DT=3.*DR/VO
	   IF (NAMCAT .EQ. 'CATOD1') CALL CATOD1(INZ,Y)
	   IF (NAMCAT .EQ. 'CATOD2') CALL CATOD2(INZ,Y)
	   L=L+1
	   IF(MTM .LT. MT)THEN
		MTM=MTM+1
        	     PX(MTM)=Y(1)
        		 PY(MTM)=Y(2)
                	   END IF
	   IF(MAG)THEN
! ***********************************************
	    Z=Y(1)
	     R=Y(2)
	      R2=R*R
!*
!*REAL*8 Z - inp    !���������� �-�� �����,����.
!*REAL*8 R - inp    !���������� �-�� �����,����.
!*REAL*8 PCI- out   !������� ���������� ���,����. 
!*REAL*8 Bx2- out   !���������� ���������� ��, ����. 
!*REAL*8 By2- out   !���������� ���������� ��, ����.
    
	   CALL FORMAG (AO,Z,R,BZ,BR)
	   TETA=(R*AO-PCIMG(INZ))/R2*ZNAK
	     RTE=Y(2)*TETA
	         VS2=VO*VO+RTE*RTE
   	            WRITE (9,'(I3,7F10.4,F7.5)')
     &	   INZ,Y(1),Y(2),Y(3),Y(4),VO,RTE, DSQRT(VS2),Y(5)*DT
	                 END IF
      IF (.NOT. MAG ) WRITE (9,'(I3,6F10.4,F8.5)')
     &	  INZ, Y(1), Y(2), Y(3), Y(4), VO,POTX(INZ)
     &     ,PJA*VO**3*PMA*FG**1.5*1.E-10/Z0**2  !(AMPS/CM**2)
      ELSE
!    L=1--�������������� �������	  
! ***********************************************
	   VS2=Y(3)*Y(3)+Y(4)*Y(4)
	   VO=DSQRT(VS2)
!    ���������� �������� ���� �������������� ���������	  
! ***********************************************
	   DT=STEP/ VO
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
	   CALL FORCE(FO,YI(1),YI(2), C1,C2)
	   IF (IGMA .EQ. 0) GOTO 60
	   C1=C1*0.5
	   C2=C2*0.5
	   IF(MAG)THEN
! *************
           Z=YI(1)
	   R=YI(2)
	   R2=R*R
*      REAL*8 AO  - out  ! ������� ���������� ���, TL*�
*      REAL*8 Z   - inp  ! ���������� ���������� ����� .�
*      REAL*8 R   - inp  ! ���������� ���������� ����� .�
*      REAL*8 BZ  - out  ! ���������� ��������� ��, TL
*      REAL*8 BR  - out  ! ���������� ��������� ��, TL
	   CALL FORMAG (AO,Z,R,BZ,BR)
	   TETA=(R*AO-PCIMG(INZ))/R2*ZNAK
	   RTE=R*TETA
	   C1=C1+RTE* BR
	   C2=C2-RTE*(BZ-TETA*ZNAK)
	   END IF
! ***********************************************
	   IF(MTM .LT. MT .AND. J .EQ. 1)THEN
		MTM=MTM+1
		PX(MTM)=YI(1)
		PY(MTM)=YI(2)
                DELEN=ABS(VS2-FO*ZNAK-EE)
                DELEE=DELEN
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
! ***********************************************
 31    CONTINUE
       DO 40 I=1,4
	    Y(I)=Y0(I)+DER(I)*CAL2(4)
 40    CONTINUE
! ***********************************************
	   VS2=Y(3)*Y(3)+Y(4)*Y(4)
	   VO=DSQRT(VS2)
	   VS2=VO*VO+RTE*RTE
      END IF
! *************
!    ��������� ���������� �� ��� ���������	  
! ***********************************************
      IF(Y(2).LT.0.)THEN
        Y(2)=-Y(2)
        Y(4)=-Y(4)
	   IF(MTM .LT. MT .AND. Y(4) .NE. 0.)THEN
		MTM=MTM+1
!    ���������� ����������� ����������� ��� ���������	  
		PX(MTM)= Y(1)-Y(2)*Y(3)/Y(4)
		PY(MTM)=0.
	   END IF
      END IF
      IF( Y(1).LE.(XU(IZL)-STEP)) GOTO 1
! *************
!    ����������� ��������� � �������� �������	  
! *************    X=XU(IZL)
	   IF(MTM.LT. MT .AND. Y(3) .NE. 0.)THEN
              R=Y(2) + (XU(IZL)-Y(1))*Y(4)/Y(3)
                IF(R .LT. 0.)R=-R
                   Y(1)=XU(IZL)
                     Y(2)=R
		MTM=MTM+1
		PX(MTM)=XU(IZL)
		PY(MTM)=Y(2)
	   END IF

 60   IF(MTM .GT. 0)WRITE (NK2) MTM,(PX(J),PY(J),J=1,MTM)
      IF ( MAG )
     &	 PRINT '(I3,7F10.4,F7.5)',
     &    INZ, Y(1), Y(2), Y(3), Y(4),VO, RTE, DSQRT(VS2),Y(5)*DT
      IF (.NOT. MAG )THEN
       WRITE (9,80) INZ, Y(1), Y(2), Y(3), Y(4), VO,FO
     	     PRINT 80, INZ, Y(1), Y(2), Y(3), Y(4), VO,FO
                   END IF
 80    FORMAT (I3,6F10.4,F8.5)
      RETURN
!*=== End of subroutine INTEGF =========================================*
      END !SUBROUTINE INTEGF
!----------------------------------------
      SUBROUTINE INTGF
!----------------------------------------
      IMPLICIT REAL*8 (A-H,O-Z)
      INCLUDE '_PARAM.FOR'
      INCLUDE '_COMMON.FOR'

      REAL*8 UVX(N4),UVXX(N4)
      DATA NK2 /2/ !����� ������ ����������
!
      IF ( ISTART .EQ. 1 ) THEN
	   CALL FLUN56
	   REWIND 6
	   READ (6) UX(0),VY(0)
	   UX(0)=UX(0)/Z0
	   VY(0)=VY(0)/Z0
      END IF
!
      IF ( ISTART .EQ. 0 ) THEN
       IF ( NITER .GE. NITM ) REWIND 5
       IF ( NITER .GE. NITM ) WRITE (5) UX(0)*Z0,VY(0)*Z0
      END IF
      IF ( NITER .EQ. NITM ) PRINT '(81(1H*))'
      IF (.NOT. MAG ) WRITE(9,'(T3,''N   Z-KOOP.	R-KOOP.'',
     & T25, ''Z-SPEED'',
     & 3X,''R-SPEED'',3X,''l-SPEED'',5X,''POT_X'',3X,''DENCITY'')')
      IF ( MAG ) WRITE(9,'(T3,''N   Z-KOOP.	R-KOOP.'',T25,
     &''Z-SPEED'',T35,''R-SPEED   l-SPEED   F-SPEED'',T65,
     &''TOTAL-SP TETA'',3X,''DENCITY'')')
      REWIND (NK2)
      PERV=0.0
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
!     ����������� ���������� � ����� ������
!
      CALL FORCE(POTX(I),Y(1),Y(2),C1,C2)
      IF (IGMA .EQ. 0 .OR. POTX(I).LE.0.) GOTO 10
      VO=SQRT(ABS(POT(-IGM(NUMB))-POTX(I)))
!
!     ����������� ��������� ���� ��������
!
      UVO=VO*VO*VO
      UVXX(I)=UVO
	PERV=PERV+PMA*AL1*PJA*UVO*AL0*VY(I)
		L=0
      END IF
      IF (MOD(I,M4).EQ.0 .OR. I.EQ.1) THEN
		IGMA=1
		CALL INTEGF(I,L)
      END IF
 9    FORMAT (' PER=',G8.3,'(MICROAMPS/V**1.5) FG=',I7,' (V)  I=',
     + G8.3,'(AMPS) NIT=',I3)
 10   CONTINUE
      WRITE(9,9) PERV,INT4(FG),PERV*FG**1.5*1.E-6,NITER
      UVMA=1.D-7
      UVMAVER=1.D-7
      DO 20 I=1,IZZ
      UVX(I)=0.0
      IF (UVOX(I) .GE. 0.0)
     + UVX(I)=UVXX(I)*PJA*PMA*FG**1.5*1.E-10/Z0**2  !(AMPS/CM**2)
      UVMA=AMAX1(UVMA,UVX(I))
      UVMAVER=UVMAVER+ UVX(I)
      USQUARE=USQUARE+UVX(I)*UVX(I)
 20   CONTINUE
      UVMAVER=UVMAVER/IZZ
      USQUARE=1.D-7
      DO 30 I=1,IZZ
      USQUARE=USQUARE+(UVX(I)-UVMAVER)*(UVX(I)-UVMAVER)
 30   CONTINUE
      USQUARE=SQRT(USQUARE/IZZ)
      WRITE(9,'(T3,''AVG_CURRENT DENCITY='',G8.3,
     & ''(AMPS/CM**2) FOR FG='',
     & I7,''(V)'')') UVMAVER,INT4(FG)
      WRITE(9,'(T3,''AVG_SQUARECURRENT DENCITY='',G8.3,
     & ''(AMPS/CM**2) FOR FG='',
     & I7,''(V)'')') USQUARE,INT4(FG)
      IF (Q1 .LT. 0.5 .OR. NITER .GE. NITM ) THEN
      PRINT '(/81(1H*))'
      PAUSE
      END IF
!*=== End of subroutine INTGF =========================================*
      END ! SUBROUTINE INTGF
