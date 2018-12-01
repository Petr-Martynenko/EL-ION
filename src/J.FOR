CC    J.FOR Compaq Visual FORTRAN 6.6.0
CC
CC    20.11.89
!----------------------------------------
      SUBROUTINE INTGR
!----------------------------------------
      IMPLICIT REAL*8 (A-H,O-Z)
      INCLUDE '_PARAM.FOR'
      INCLUDE '_COMON.FOR'

C     Main program variables
      INTEGER     NOUT/0/
      REAL*8      UVX(N4),Y1,Y2,C1,C2
      OPEN  (UNIT=NOUT,FILE='CHECKERR.TXT',POSITION='APPEND')
!
      IF ( ISTART .EQ. 1 ) THEN
	   CALL FLUN56
	   REWIND 6
	   READ (6) UX(0),VY(0)
	   UX(0)=UX(0)/Z0
	   VY(0)=VY(0)/Z0
	   Q1=0.
      END IF
!
      IF ( ISTART .EQ. 0 ) THEN
       IF ( NITER .GE. NITM ) REWIND 5
       IF ( NITER .GE. NITM ) WRITE (5) UX(0)*Z0,VY(0)*Z0
	   PERVO=0.
	   Q1=1.
      END IF
      IF ( NITER .EQ. 1 ) PRINT '(141(1H*))' ! выдача на дисплей
      IF (.NOT. MAG ) PRINT '(T3,''N   Z-KOOP.'',T17,''R-KOOP.'', 	  
     &  T28,''Z-SPEED   R-SPEED   l-SPEED'',T58,''CHARGE'')'
      IF ( MAG ) PRINT'(T3,''N   Z-KOOP.'',T17,''R-KOOP.'',T27,  
     &''Z-SPEED   R-SPEED   L-SPEED   F-SPEED'',
     &	T66,''TOTAL-SP CHARGE'')'
!
!	GAMA=корректирующий множитель
!
      IF ( NITER .LE. 15 ) GAMA=GAMAS(NITER)
      IF ( NITER .GE. 15 ) GAMA=GAMAS(15)
!
!       задание начальных условий
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
	   AL0=SQRT((UX(I-1)-UX(I))**2+(VY(I-1)-VY(I))**2)
	   L=1
	   Q1=0.
      END IF
!
      IF ( ISTART .EQ. 0 ) THEN
      IF ( NAMCAT .EQ. 'CATOD1' ) THEN
      CALL DISTN( UX(I-1), VY(I-1), UX(I), VY(I), Y(1), Y(2), DR)
		AL0=SQRT((UX(I-1)-UX(I))**2+(VY(I-1)-VY(I))**2)
      END IF
!
      IF ( NAMCAT .EQ. 'CATOD2' ) THEN
	  FO = FHO + (2 * I - 1) * DFO
	  Y1=UX(I)-DR*DCOS(FO)
	  Y2=VY(I)-DR*DSIN(FO)
      END IF
!
!	определение потенциала в точке старта
!
      CALL FORCE(POTX(I),Y1,Y2,C1,C2)
      IF (IGMA .EQ. 0) GOTO 10
      IF ((POT(-IGM(NUMB))-POTX(I))*ZNAK.GE.0.) GOTO 10
      VO=DSQRT(DABS(POT(-IGM(NUMB))-POTX(I)))
!
!	коррекция тока инжекции
!
      UVO=VO*VO*VO
      UVX(I)=1.D+10
      UVO=UVOX(I)+GAMA*(UVO-UVOX(I))
      FOX=UVOX(I)*EPS
      FOX=DABS(FOX)
      IF (DABS(UVO-UVOX(I)) .GE. FOX) Q1=0.
      IF (UVO .GT. UVOMAX) UVO=UVOMAX
      IF (UVOX(I) .NE. 0.0) UVX(I)=UVO/UVOX(I)
      UVOX(I)=UVO
!
!	определение координат в точке инжекции
!
		Y(1)=UX(I)
		Y(2)=VY(I)
		IF ( NAMCAT .EQ. 'CATOD1' ) THEN
		   Y(1)=.5*(Y(1)+UX(I-1))
		   Y(2)=.5*(Y(2)+VY(I-1))
		END IF
		Y(3)=0.
		Y(4)=0.
		Y(5)=PJA*UVO*AL0*Y(2)
		PERVO=PERVO+PMA*AL1*Y(5)
		L=0
      END IF
      IGMA=1
      CALL INTEGR(I,L)
 9    FORMAT (' PER=',F6.4,'(MICROAMPS/V**1.5) FG=',I7,'(V) I=',G8.3,
     + '(AMPS) NIT=',I3,' GAMA=',F5.3)
 10   CONTINUE
      PRINT '(T3,''RATIO OF START POINT POTENTIALS'')'
      PRINT '(14F10.6)', (POTX(I),I=1,IZZ)
      PRINT
     & '(T3,''RATIO OF CURRENT DENCITY FOR FG='',I7,''(V)'')',INT4(FG)
      PRINT '(14F10.3)', (UVX(I),I=1,IZZ)
!
!       задание
!
      DO 20 I=1,IZZ
	 UVX(I)=0.0
	 IF ( POTX(I) .GE.0. ) UVX(I)=UVOX(I)*PJA !*PMA*FG**1.5*1.E-10/Z0**2
 20   CONTINUE
      PRINT '(T3,
     & ''CURRENT DENCITY  NORMALIZED FOR FG='',I7,''(V)'')', INT4(FG)
      PRINT '(14F10.6)', (UVX(I),I=1,IZZ)
      IF ( ISTART .EQ. 0 ) THEN
      PRINT 9, PERVO,INT4(FG),PERVO*FG**1.5*1.E-6,NITER,GAMA
      write (NOUT,9) PERVO,INT4(FG),PERVO*FG**1.5*1.E-6,NITER,GAMA
      END IF
      IF (Q1 .LT. 0.5 .OR. NITER .GE. NITM ) THEN
	 PRINT '(141(1H*))'
      END IF      
      CLOSE (UNIT=NOUT, STATUS = 'KEEP')
      RETURN
*= End of subroutine INTGR =========================================*
      END SUBROUTINE INTGR
!----------------------------------------
CC
CC реализация интегрирования уравнений движения
CC нерелятивистских частиц без магнитного поля
CC
      SUBROUTINE INTEGR(INZ,L)
*  INTEGER INZ - inp  ! номер частицы 
*  INTEGER L   - inp  ! параметр инжекции 
CC    L=1--интегрирование методом	  
CC    L=0--прохождение прикатодного слоя	  
!----------------------------------------
      IMPLICIT REAL*8 (A-H,O-Z)
      INCLUDE '_PARAM.FOR'
      INCLUDE '_COMON.FOR'

C     Main program variables

      REAL*8 Z   !продольная к-та точки,норм 
      REAL*8 R   !поперечная к-та точки,норм.
      REAL*8 AO  !угловая компонента ВМП,норм. 
      REAL*8 BZ  !продольная компонента МИ,норм.
      REAL*8 BR  !поперечная компонента МИ,норм.
      REAL*8 TETA_N  ! угловая скорость вращения ,норм.
      REAL*8 YI(5),Y0(4),RTE
      REAL*8 CAL1(3),CAL2(4),DER(4),COND3
      REAL*8 Z1,R1
      REAL*8 VRZ      !отношение поперечной скорости к продольной
      LOGICAL IFL
      DATA COND3 /.3333333333333/
      VFE=0.0
      RTE=0.0
      IFL=.FALSE.
      IF ( NITER .GE. NITM ) IFL=.TRUE.
      ZZ= XU(0) + ZSCHIF
CC    L=0--прохождение прикатодного слоя	  
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
*      REAL*8 AO  - out  ! угловая компонента ВМП,норм.
*      REAL*8 Y(1) - inp  ! продольная к-та точки,норм.
*      REAL*8 Y(2) - inp  ! поперечная к-та точки,норм.
*      REAL*8 BZ  - out  ! продольная компонент МИ, норм.
*      REAL*8 BR  - out  ! поперечная компонент МИ, норм.
*      REAL*8 PCIMG- inp  ! поток МИ, норм./2*PI
	   TETA=TETA_N (AO,Y(1),Y(2),PCIMG(INZ),BZ,BR)*ZNAK
	   RTE=Y(2)*TETA
	   END IF
C ***********************************************
	   VS2=VO*VO+RTE*RTE
!	   IF ( NITER .GE. NITM ) THEN
	   IF (.NOT. MAG ) PRINT 80,
     &	   INZ, Y(1), Y(2), Y(3), Y(4), SQRT(VS2), Y(5)*DT
	   IF ( MAG ) PRINT 80,
     &	   INZ, Y(1), Y(2), Y(3), Y(4),VO, RTE,  SQRT(VS2),Y(5)*DT
!	   END IF      
      ELSE
CC    L=1--интегрирование методом	  
C ***********************************************
	   VS2=Y(3)*Y(3)+Y(4)*Y(4)
CC    определяем величину шага интегрирования уравнений	  
C ***********************************************
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
!18:13 24.04.2017	   IF (IGMA .EQ. 0) GOTO 45
	   IF (IGMA .EQ. 0) GOTO 60
	   C1=.5*C1
	   C2=.5*C2
	   IF(MAG)THEN
C *************
           Z=YI(1)
	   R=YI(2)
	   R2=R*R
*      REAL*8 AO  - out  ! угловая компонента ВМП, TL*м
*      REAL*8 Y1  - inp  ! продольная координата точки .м
*      REAL*8 Y2  - inp  ! поперечная координата точки .м
*      REAL*8 BZ  - out  ! продольная компонент МИ, TL
*      REAL*8 BR  - out  ! поперечная компонент МИ, TL
	   CALL FORMAG (AO,Z,R,BZ,BR)
	   TETA=(R*AO-PCIMG(INZ))/R2*ZNAK
	   RTE=R*TETA
	   C1=C1+RTE* BR
	   C2=C2-RTE*(BZ-TETA*ZNAK)
	   END IF
C ***********************************************
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
C ***********************************************
 31    CONTINUE
	    DO 40 I=1,4
	    YI(I)=Y0(I)+DER(I)*CAL2(4)
 40    CONTINUE
	   VS2=YI(3)*YI(3)+YI(4)*YI(4)
	   VO=DSQRT(VS2)
	   VS2=VO*VO+RTE*RTE
      END IF
C *************
 45   DT=DT/3.
      YM1=(YI(3)+Y(3))*DT*.5
      YM2=(YI(4)+Y(4))*DT*.5
      YI(5)=Y(5)*DT
      DO 50 I=1,3
       Z=Y(1)+YM1* (2*I-1)
       R=Y(2)+YM2*(2*I-1)
       IF(R.LT.0.)R=-R
C **** проверка выхода координат за границы прямоугольной области ***
       IF ( Z .GE.XU(IZL1-1).OR. Z .LT.XU(0)
     & .OR. R .GE.YV(IRL1-1).OR. R .LT.YV(0)) GOTO 51
       Z1=Z-INT(Z)
       R1=R-INT(R)
       IZT=Z-XU(0)
       IRT=R-YV(0)
       IF(R .LT. 1.)R=0.5
       YRT=YI(5)/R*ZNAK
C *************
CC    заполнение матрицы обьемного заряда	  
C ***********************************************
       IND0=IZT+IZL1*IRT+1
       IND1=IND0+IZL1
       IND2=IND0+1
       IND3=IND0+IZL1+1
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
C *************
CC    отражение траектории от оси симметрии	  
C ***********************************************
      IF (Y(2) .LT. 0.) THEN
       Y(2)=-Y(2)
       Y(4)=-Y(4)
      END IF
C *************
CC    определение координат в сечении ZZ	  
C ***********************************************
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
!18:48 24.04.2017	   IF (IGMA .EQ. 0) GOTO 60
      IF( Y(1).LE.(XU(IZL1-1)-STEP)) GOTO 1
C *************
CC    определение координат в выходном сечении	  
C *************    X=XU(IZL1-1)
      VRZ=0.
      IF( Y(3) .NE. 0.)VRZ= Y(4)/Y(3)
      R=Y(2) + (XU(IZL1-1)-Y(1))*VRZ
        IF(R .LT. 0.)VRZ=-VRZ
        IF(R .LT. 0.)R=-R
	IF( Y(3) .NE. 0.) Y(1)=XU(IZL1-1)
        Y(2)=R

 60   IF ( NITER .GE. NITM ) THEN
        IF (IFL) THEN
        ZZ=Y(1)
        RZ=Y(2)
        VZ=Y(3)
        VR=Y(4)
        ENDIF
C *************
CC   вывод значений координат в промежуточном сечении	  
C ***********************************************
      WRITE(5)ZZ*Z0,RZ*Z0,VZ,VR,Y(5),PCIMG(INZ)*Z0,UX(INZ)*Z0,VY(INZ)*Z0
	PRINT 4,INZ,ZZ,RZ,VZ,VR,SQRT(VS2),Y(5)*DT
      END IF
 4    FORMAT (I3,4F10.4,2E10.4,2I4)
	   IF (.NOT. MAG ) PRINT 80,
     &    INZ, Y(1), Y(2), Y(3), Y(4), SQRT(VS2), Y(5)*DT
	   IF ( MAG ) PRINT 80,
     &    INZ, Y(1), Y(2), Y(3), Y(4),VO, RTE,  SQRT(VS2),Y(5)*DT
 80    FORMAT (I3,7F10.4,F7.5)
*= End of subroutine INTEGR =========================================*
      END SUBROUTINE INTEGR
