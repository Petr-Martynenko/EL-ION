CC    B.FOR MS Compaq Visual FORTRAN 6.6.0
CC
CC    есть текст программ от 05-10-91
CC
!----------------------------------------
      SUBROUTINE B
!----------------------------------------
      IMPLICIT REAL*8 (A-H,O-Z)
      INCLUDE '_PARAM.FOR'
      INCLUDE '_COMON.FOR'
	  
C     Main program variables

      CHARACTER*1 PT,ASTER,NULL,ONE,CAARY(120)
      CHARACTER*10 OneTen

      IF ( NK .EQ. 0 )  THEN
!     задано нулевое число точек контура:
       PRINT '('' задано нулевое число точек контура NK=0'')'
!
       CALL FDATA1
       CALL WDATA
      ELSE
         CALL RDATA
      ENDIF
!
      DO I = 1,NK
         IR(I)=IR(I)+1
         IZ(I)=IZ(I)+1
      END DO
CC
CC INTEGER IFD() - out
CC INTEGER IZ() - inp
CC INTEGER IR() - inp
CC INTEGER IZL1, IRL1, NK - inp
CC
CC Program BITA формирует двумерный массив описатель области
CC
         CALL BITA( IFD, IZ, IR, IZL1, IRL1, NK)
      DO I = 1,NK
         IR(I)=IR(I)-1
         IZ(I)=IZ(I)-1
      END DO
CC
CC    тестовая программа построения массива описателя области(MOO)
CC
CC    есть текст программ от 02-06-91
CC
C ***** выдача на экран терминала (MOO)
      ONE='1'
      NULL='0'
      ASTER='*'
	  OneTen='0123456789'
C ***** 
      ISETS=1+(IZL1-1)/160
      ISET1=IZL1/10
      DO 130 I=1,ISETS
      LB=120*I-120
	  UB=MIN0(LB+119,IZL1-1)
	  NB=UB-LB+1
	  WRITE (*,'(''****MOO***MOO***MOO***MOO***MOO***MOO***MOO'')')
	  WRITE (*,'(14X,12(I2,8X))') (I2,I2=1,ISET1)
	  WRITE (*,'(5X,12(A10))') (OneTen,I2=1,ISET1)
	  WRITE (*,'(4X,1Hг,120(A1))') ('=',I2=1,NB)
CC
CC вывод на печать массива-описателя области (МОО)
CC
      DO 120 I2=1,IRL1
	  IRN=IRL1-I2
      DO 110 I3=1,NB
         PT=ONE
         IF (IFD(LB+I3+IRN*IZL1) .NE. 10)PT=NULL
         IF (IFD(LB+I3+IRN*IZL1) .EQ. 10)PT=ASTER
 110     CAARY(I3)=PT
 120  WRITE (*,'(I4,1HI,120(A1))') IRN, (CAARY(I3), I3=1,NB)
	  WRITE (*,'(4X,1HL,120(A1))') ('=',I2=1,NB)
	  WRITE (*,'(5X,12(A10))') (OneTen,I2=1,ISET1)
 130  WRITE (*,'(14X,12(I2,8X))') (I2,I2=1,ISET1)
*=== End of subroutine B =========================================*
      END 
!----------------------------------------
      SUBROUTINE FDATA1
!----------------------------------------
      IMPLICIT REAL*8 (A-H,O-Z)
      INCLUDE '_PARAM.FOR'
      INCLUDE '_COMON.FOR'
	  
C     Main program variables

      INTEGER     NOUT/0/
      REAL*8 CC(3)     !
      REAL*8 P1(2)     !
      REAL*8 P(2)      !
      REAL*8 DLN,DLT
      REAL ANGL1,ANGL2,ANGLE,ANGLN,ANGLT,ANGLER,DELTAX,DELTAY
      LOGICAL MPRT,FDL,IRCOK
      INTEGER I,J,NPS(6)
      DATA NPS/1,-1,0,0,1,-1/
!функция-оператор "FDL" определяет положение точки (XT, YT)
!по отношению к дуге окружности (XCN, YCN, RCO)
!    .TRUE. точка справа по направлению обхода, т.е вне контура
!    .FALSE. точка слева по направлению обхода, т.е. внутри контура
      FDL(XT, YT, XCN, YCN, RCO,ZNA_K)=
     & (DSQRT((XT-XCN)**2+(YT-YCN)**2)-RCO)*ZNA_K .GT. 0.

      OPEN  (UNIT=NOUT,FILE='CHECKERR.TXT',POSITION='APPEND')
      IF ( MS .GT. 0 )  THEN
       MPRT=.FALSE.
      ELSEIF ( MS .EQ. 0 )  THEN
!     задано нулевое число узлов контура:
       PRINT '(//''задано нулевое число узлов контура'')'
       STOP
      ELSE
       MPRT=.TRUE.
       MS=-MS
      ENDIF
!
	write (NOUT,'(T6,
     &  ''N  R-KOOP.  Z-KOOP.***IGM   ***IEX        NK    ANGLE'')')
      PRINT '(''**** GEOMETRY ****'')'
      PRINT
     &'(''     N  R-KOOP.  Z-KOOP.***IGM   ***IEX        NK    ANGLE'')'
      NK=0
      I=0
      IOK=0
      NKT=1
 11   I=I+1
      I1=I+1
	  IF (I.EQ.MS) I1=1
	  R1=BDR(I)
	  R2=BDR(I1)
	  Z1=BDZ(I)
	  Z2=BDZ(I1)
	  RZ=(R2-R1)*(R2-R1)+(Z2-Z1)*(Z2-Z1)
	  N=IGM(I)
!----------------------------------------
!   отработка соседних точек
!----------------------------------------
      IF (RZ.LE.2.) GOTO 505
!----------------------------------------
!   отработка дуги окружности
!----------------------------------------
	  IF (REX(I).NE. 0 ) GOTO 510
      DELTAX=Z2-Z1
      DELTAY=R2-R1
      ANGLE=ANGLER(DELTAX,DELTAY)
!--- I ---
      IF (ANGLE.GE.315. .OR. ANGLE.LE. 45.) THEN
	 TNG=(R2-R1)/(Z2-Z1)
	 IZS=Z1
	 IZF=Z2+0.4999
	 IF(IZF.GT.IZL1-1)IZF=IZL1-1
!проверка первой гр.точки сегмента контура на совпадение с последней
      IF(NK.GT.0)THEN
        IF(IZS.EQ.IZ(NK))THEN
        IF(IPO(NK).LT.0.AND.N.LT.0)IZS=IZS+1
	      ELSE
	IF(IPO(NK).LT.0.AND.N.GT.0)IZS=IZS+1
	ENDIF
      ENDIF
         DO 100 J=IZS,IZF
            ZK=FLOAT(J)
            R=R1+(ZK-Z1)*TNG
            IRT=R+0.9999
            IF (J .NE. IZS .AND. IRT .NE. IR(NK)) THEN
			   NK=NK+1
               IF (ANGLE.LE. 45.) THEN
			   IR(NK)=IR(NK-1)
			   IZ(NK)=J
			   ELSE
			   IR(NK)=IRT
			   IZ(NK)=IZ(NK-1)
			   END IF
               IPO(NK)=N
               DELZ(NK)=0.
               DELR(NK)=0.
			END IF
			NK=NK+1
			IR(NK)=IRT
			IZ(NK)=J
			DELR(NK)=R-IR(NK)
			IF (TNG .EQ. 0.) THEN
			   DELZ(NK)=0.
			   IF (DELR(NK) .NE. 0.) DELZ(NK)=2.
			ELSE
			   RK=FLOAT(IR(NK))
			   Z=Z1+(RK-R1)/TNG
			   DELZ(NK)=Z-IZ(NK)
			END IF
			IPO(NK)=N
 100    CONTINUE
      ELSE
     +    IF (ANGLE.GT. 45. .AND. ANGLE.LT.135.) THEN
C --- II---
      TNG=(Z2-Z1)/(R2-R1)
      IRS=R1+0.99999
      IRF=R2
      IF (FLOAT(IRF).EQ.R2.AND.N.GT.0.AND.IGM(I1).LT.0) IRF=IRF-1
!проверка первой гр.точки сегмента контура на совпадение с последней
      IF(NK.GT.0)THEN
        IF(IRS.EQ.IR(NK))THEN
        IF(IPO(NK).LT.0.AND.N.LT.0)IRS=IRS+1
        ENDIF
      ENDIF
      DO 200 J=IRS,IRF
      RK=FLOAT(J)
      IF ( RK.EQ.R2 ) THEN
      Z=Z2
      ELSE
      Z=Z1+(RK-R1)*TNG
      ENDIF
      IZT=Z
      IF(J.NE.IRS.AND.IZT.NE.IZ(NK))THEN
      NK=NK+1
      IF(ANGLE.GT.90.)THEN
      IZ(NK)=IZ(NK-1)
      IR(NK)=J
      ELSE
      IZ(NK)=IZT
      IR(NK)=IR(NK-1)
      ENDIF
      IPO(NK)=N
      DELR(NK)=0.
      DELZ(NK)=0.
      ENDIF
      NK=NK+1
      IZ(NK)=IZT
      IR(NK)=J
      DELZ(NK)=Z-IZ(NK)
      IF (TNG.EQ.0.0) THEN
      DELR(NK)=0.0
      IF (N.GT.0.AND.J.NE.IRS.AND.J.NE.IRF) DELR(NK)=2.0
      IF (DELZ(NK).NE.0.0) DELR(NK)=2.0
      ELSE
      ZK=FLOAT(IZ(NK ))
      R=R1+(ZK-Z1)/TNG
      DELR(NK)=R-IR(NK)
      ENDIF
      IPO(NK)=N
!16:32 24.04.2017      IF (N.GT.0.AND.IR(NK).EQ.IRS.AND.IZ(NK).EQ.IZL1-1) IPO(NK)=6
C
 200  CONTINUE
C --- III---
      ELSE
     +    IF (ANGLE.GE.135. .AND. ANGLE.LE.225.) THEN
      TNG=(R2-R1)/(Z2-Z1)
      IZS=Z1
      IZF=Z2+0.9999
      IF(IZF.GT.IZL1-1)IZF=IZL1-1
!проверка первой гр.точки сегмента контура на совпадение с последней
      IF(NK.GT.0)THEN
        IF(IZS.EQ.IZ(NK))THEN
	    IF(IPO(NK).LT.0.AND.N.LT.0)IZS=IZS-1
	ELSE
	    IF(IPO(NK).LT.0.AND.INT(R1).NE.IR(NK))IZS=IZS+1
	ENDIF
      ENDIF
      DO 300 J=IZS,IZF,-1
      ZK=FLOAT(J)
      R=R1+(ZK-Z1)*TNG
      IRT=R+0.0001
      IF(J.NE.IZS.AND.IRT.NE.IR(NK))THEN
      NK=NK+1
      IF(ANGLE.GT.180.) THEN
      IR(NK)=IR(NK-1)
      IZ(NK)=J
      ELSE
      IR(NK)=IRT
      IZ(NK)=IZ(NK-1)
      ENDIF
      IPO(NK)=N
      DELR(NK)=0.
      DELZ(NK)=0.
      ENDIF
      NK=NK+1
      IR(NK)=IRT
      IZ(NK)=J
      DELR(NK)=R-IR(NK)
      IF (TNG.EQ.0.0) THEN
      DELZ(NK)=0.0
      IF (N.GT.0.AND.J.NE.IZS.AND.J.NE.IZF) DELZ(NK)=2.0
      IF (DELR(NK).NE.0.0) DELZ(NK)=2.
      ELSE
      RK=FLOAT(IR(NK))
      Z=Z1+(RK-R1)/TNG
      DELZ(NK)=Z-IZ(NK)
      ENDIF
      IPO(NK)=N
      IF(J.EQ.IZS.AND.FLOAT(IZ(NK)).GT.Z1) DELR(NK)=2.0
 300  CONTINUE
      ELSE
C --- IV ---
      TNG=(Z2-Z1)/(R2-R1)
      IRS=R1
      IRF=R2 !+0.9999
!проверка первой гр.точки сегмента контура на совпадение с последней
      IF(NK.GT.0)THEN
        IF(IRS.EQ.IR(NK))THEN
        IF(IPO(NK).LT.0.AND.N.LT.0)IRS=IRS-1
        ENDIF
      ENDIF
      DO 400 J=IRS,IRF,-1
      RK=FLOAT(J)
      Z=Z1+(RK-R1)*TNG
      IZT=Z+0.9999
      IF(J.NE.IRS.AND.IZT.NE.IZ(NK))THEN
      NK=NK+1
      IF(ANGLE.GT.270.)THEN
      IZ(NK)=IZ(NK-1)
      IR(NK)=J
      ELSE
      IZ(NK)=IZT
      IR(NK)=IR(NK-1)
      ENDIF
      IPO(NK)=N
      DELR(NK)=0.
      DELZ(NK)=0.
      ENDIF
      NK=NK+1
      IZ(NK)=IZT
      IR(NK)=J
      DELZ(NK)=Z-IZ(NK)
      IF (TNG.EQ.0.0) THEN
      DELR(NK)=0.0
      IF (N.GT.0 .AND. J.NE.IRS .AND. J.NE.IRF) DELR(NK)=2.0
      IF (DELZ(NK).NE.0.0) DELR(NK)=2.0
      ELSE
      ZK=FLOAT(IZ(NK))
      R=R1+(ZK-Z1)/TNG
      DELR(NK)=R-IR(NK)
      ENDIF

      IPO(NK)=N

 400  CONTINUE

      END IF
      GOTO 540 

 505  NK=NK+1
      IZ(NK)=Z1
      IR(NK)=R1
      IPO(NK)=N
      DELZ(NK)=0.
      DELR(NK)=0.
      GOTO 540 
!----------------------------------------------
!   заполнение точек контура на дуге окружности
!----------------------------------------------	  
 510  IOK=IOK+1
! only --- II ---  дуга окружности во 2-ой четверти
      IRCOK=.FALSE.
      IZF=Z2+0.9999
      IRF=R2
	write (NOUT,'('' НОМЕР УЗЛА ГРАНИЦЫ:'',I2,'' ДУГА ОКРУЖНОСТИ  N:''
     &          ,I2,'' НОМЕР ГРАНИЧНОЙ ТОЧКИ:'',I3)') I,IOK,NK
CC
CC определение параметров окружности (XCN, YCN, RCO)
CC 
CC(1)=XCN
CC(2)=YCN
CC(3)=RCOK
      CC(3)=REX(I)
      CALL CORCR( Z1, R1, Z2, R2, CC(1), CC(2), CC(3))
CC
CC определение углов(ГРАДУСЫ) наклона радиуса
CC в начальной и конечной точках дуги окружности к горизонту
CC
      DELTAX=Z1-CC(1)
      DELTAY=R1-CC(2)
      ANGL1=ANGLER(DELTAX,DELTAY)
      DELTAX=Z2-CC(1)
      DELTAY=R2-CC(2)
      ANGL2=ANGLER(DELTAX,DELTAY)
CC
CC определение направления обхода по дуге окружности
CC	ZNA_K=+1. против часовой стрелки
CC	ZNA_K=-1. по часовой стрелке
CC
      ZNA_K=REX(I)/ABS(REX(I))
CC(3)=CC(3)*ZNA_K
CC
CC определение координат регулярного узла
CC 
 515  IF ( NK .NE. 0 )  THEN
       ZT=DFLOAT(IZ(NK))
       RT=DFLOAT(IR(NK))
      ELSE
       IZN=Z1+0.5
       IRN=R1+0.5
       ZT=DFLOAT(IZN)
       RT=DFLOAT(IRN)
      ENDIF
!	  
      DELTAX=ZT-CC(1)
      DELTAY=RT-CC(2)
      ANGLE=ANGLER(DELTAX,DELTAY)
      IF (ANGLE .EQ. 0. .AND. ZNA_K .EQ. -1.) ANGLE=360.
      IF (ANGLE .LT. ANGL2 .AND. ZNA_K .EQ. -1.) ANGLE=360.+ANGLE
      DLN=2.
!
      DO 535 NTK=1,4
      IF ( IRCOK )  THEN
       IZT=ZT+NPS(NTK+2)
       IRT=RT+NPS(NTK)
      ELSE
       IZT=ZT
       IRT=RT
       IRCOK=.TRUE.
      ENDIF
       IF (IRT .LT. 0) GOTO 535
       IF (IRT .GT. IRL1-1) GOTO 535
       IF (IZT .EQ. IZ(NK-1) .AND. IRT .EQ.IR(NK-1)) GOTO 535
	   P1(1)=DFLOAT(IZT)
	   P1(2)=DFLOAT(IRT)
CC
CC проверка положения точки по отношению к дуге окружности
CC 
       IF (FDL(P1(1),P1(2),CC(1),CC(2),CC(3),ZNA_K)) GOTO 535
!	  
       DELTAX=P1(1)-CC(1)
       DELTAY=P1(2)-CC(2)
       ANGLT=ANGLER(DELTAX,DELTAY)
       IF ((ANGLT-ANGLE)*ZNA_K .GE. 0.) THEN
!
	   CALL PCOWN(P1,CC,P,DLT)
         IF ( DLT .LE. DLN )  THEN
         IZN=IZT
         IRN=IRT
	   DLN=DLT
	   ANGLN=ANGLT
         ENDIF
       ENDIF
 535  CONTINUE
!      IF (IZN .EQ. IZ(NK) .AND. IRN .EQ.IR(NK)) GOTO 540
	  NK=NK+1
	  write (NOUT,'( 7F9.4,'' № ГР.ТОЧКИ:'',I3)')
     &  ANGLE, DFLOAT(IZT), DFLOAT(IRT), CC(1), CC(2), CC(3), ANGL1,NK
CC
CC определение координат регулярного узла
CC 
      IZ(NK)=IZN
      IR(NK)=IRN
      IPO(NK)=N
      ANGLE=ANGLN
!	 
      ARGX=DFLOAT(IZN) 
      ARGY=DFLOAT(IRN) 
	  IF ( ARGX .GT. CC(1) )  THEN
       MJ=1
      ELSE
       MJ=-1
      ENDIF
	  IF ( ARGY .GT. CC(2) )  THEN
       MK=1
      ELSE
       MK=-1
      ENDIF
CC
CC определение приращений координат регулярного узла
CC 
	  DELZ(NK)=FUNXY(ARGY,CC(2),CC(1),CC(3),MJ)   
	  IF (MJ.NE. 0) DELZ(NK)=DELZ(NK)-IZN	  
	  DELR(NK)=FUNXY(ARGX,CC(1),CC(2),CC(3),MK)  	  
	  IF (MK.NE. 0) DELR(NK)=DELR(NK)-IRN
CC
CC определение продвижения по дуге окружности
CC 
      IF (IZN .EQ. IZF .AND. IRN .EQ.IRF) GOTO 540
CC
CC если R2=0 (конец дуги лежит на оси), то происходит 
CC зацикливание, т.к.ANGLE=ANGL2, а предыдущее условие
CC не выполняется
      IF ((ANGL2-ANGLE)*ZNA_K .GE. 0.) GOTO 515
!
 540  CONTINUE
	write (NOUT,'( I6,F9.3,F9.3,I6,F9.3,I10,F9.3)')
     &	I,BDR(I),BDZ(I),IGM(I),REX(I),NK,ANGLE
      PRINT '( I6,F9.3,F9.3,I6,F9.3,I10,F9.3)'
     +   ,I,BDR(I),BDZ(I),IGM(I),REX(I),NK,ANGLE
	IF ( NK .GT. NS ) STOP 540
      IF( MPRT ) write (NOUT,'(1(4I5,2F9.4,1X,1HI))')
     &	(K,IPO(K),IR(K),IZ(K),DELR(K),DELZ(K),K=NKT,NK)

	  NKT=NK+1
	  IF (I.NE.MS) GOTO 11
CC
CC  упорядочивание массива граничных точек контура
CC    первый проход
	  I=0
	  J=0
 550  I=I+1
	  IF (I.GT.NK) GOTO 560
	  I1=I+1
	  J=J+1
	  IF (I .EQ. NK) I1=1
       IZ1=IZ(I)
       IR1=IR(I)
       IP1=IPO(I)
      DZ1=DELZ(I)
      DR1=DELR(I)
       IZ2=IZ(I1)
       IR2=IR(I1)
       IP2=IPO(I1)
      DZ2=DELZ(I1)
      DR2=DELR(I1)
      IZ(J)=IZ1
      IR(J)=IR1
      IPO(J)=IP1
      DELZ(J)=DZ1
      DELR(J)=DR1
      IF (IZ1 .EQ. IZ2 .AND. IR1 .EQ. IR2) THEN
         IF (IP1 .EQ. IP2) THEN
            IPO(J)=IP2
            DELZ(J)=DZ2
            DELR(J)=DR2
         ENDIF
         IF (IP1 .GT. 0 .AND. IP2 .LE. 0) THEN
             IPO(J)=IP2
            DELZ(J)=DZ2
            DELR(J)=DR2
        ENDIF
         IF (IP1 .GT. 0 .AND. IP2 .GT. 0) THEN
 	        IF ( IZ1 .EQ. 0 )  THEN
               IPO(J)=5
               IF ( IP1 .EQ. 7 ) IPO(J)=7
           ELSE
               IPO(J)=6
               IF ( IP2 .EQ. 7 ) IPO(J)=7
            ENDIF
        ENDIF
	  I=I+1
      ENDIF
	  GOTO 550
 560	NK=J
!    второй проход
	  I=0
	  J=0
 561  I=I+1
	  IF (I.GT.NK) GOTO 565
	  I1=I+1
	  J=J+1
	  IF (I .EQ. NK) I1=1
       IZ1=IZ(I)
       IR1=IR(I)
       IP1=IPO(I)
      DZ1=DELZ(I)
      DR1=DELR(I)
       IZ2=IZ(I1)
       IR2=IR(I1)
       IP2=IPO(I1)
      DZ2=DELZ(I1)
      DR2=DELR(I1)
      IZ(J)=IZ1
      IR(J)=IR1
      IPO(J)=IP1
      DELZ(J)=DZ1
      DELR(J)=DR1
      IF (IZ1 .NE. IZ2 .AND. IR1 .NE. IR2) THEN
	     J=J+1
 	     I=I+1
         DO 562 IND1=J,NK
            IND2=NK-IND1+J
            IZ(IND2+1)=IZ(IND2)
            IR(IND2+1)=IR(IND2)
            IPO(IND2+1)=IPO(IND2)
            DELZ(IND2+1)=DELZ(IND2)
            DELR(IND2+1)=DELR(IND2)
 562    CONTINUE
 	     NK=NK+1
           IZ21=IZ2-IZ1
           IR21=IR2-IR1
           IPO(J)=IP1
           DELZ(J)=0.0
           DELR(J)=0.0
 	        IF ( IZ21*IR21 .EQ. 1 )  THEN
               IZ(J)=IZ2
               IR(J)=IR1
            ELSE
               IZ(J)=IZ1
               IR(J)=IR2
            ENDIF
      ENDIF
      IF (N.GT.0.AND.IR(NK).EQ.0.AND.IZ(NK).EQ.IZL1-1) IPO(NK)=6
	  GOTO 561
 565	NK=J
!
      IF( MPRT ) write (NOUT, 800)
      IF( MPRT ) write (NOUT,'(4(4I5,2F9.4,1X,1HI))')
     &	(K,IPO(K),IR(K),IZ(K),DELR(K),DELZ(K),K=1,NK)
      IF( MPRT ) write (NOUT, 800)
      CLOSE (UNIT=NOUT, STATUS = 'KEEP')
      RETURN
 800  FORMAT('    N       R-K. Z-K.    DR-K.    DZ-K.')
 900  STOP 900
*=== End of subroutine FDATA1 =========================================*
      END
!----------------------------------------
      SUBROUTINE PCOWN(P1,CC,P,D)
!----------------------------------------
      IMPLICIT NONE
      REAL*8 CC(3)     !
      REAL*8 P1(2)     !
      REAL*8 P(2)      !
      REAL*8 D         !
      REAL*8 DELTAX,DELTAY,DELABS
CC
      DELTAX=P1(1)-CC(1) 
      DELTAY=P1(2)-CC(2) 
      DELABS= DSQRT(DELTAX**2+DELTAY**2)
        IF (DELABS.EQ.CC(3)) THEN
         P(1)=P1(1)
         P(2)=P1(2)
         D =0.
        ELSE
CC
CC находит точку P на заданной окружности CC,
CC ближайшую к точке P1 и определяет 
CC расстояние D между ними
         P(1)=CC(1)+DELTAX*CC(3) /DELABS
         P(2)=CC(2)+DELTAY*CC(3) /DELABS
         D = ABS(DELABS-CC(3))
        END IF

      RETURN
      END !PCOWN
!----------------------------------------
      FUNCTION FUNXY(ARG,X0,Y0,RH,MKJ)
!----------------------------------------
CC
CC FUNCTION FUNXY определяет по одной известной координате
CC точки на дуге окружности ее вторую координату (известны
CC координаты центра окружности ее радиус)
      IMPLICIT REAL*8 (A-H,O-Z)
      DEL=ARG-X0
      IF (DABS(DEL) .LE. RH) THEN
       IF (DEL .EQ. 0.) THEN
       FUNXY=Y0+MKJ*RH
       ELSE
       FUNXY=Y0+MKJ*DSQRT(RH*RH-DEL*DEL)
       END IF
      ELSE
CC
CC значение MKJ=0 сигнализирует о том, что программа
CC не может выдать результат, к-рый приравнивается
CC фиктивному значению  MKJ=0
       MKJ=0
       FUNXY=2.
      END IF
      END !FUNCTION FUNXY
CC
CC    есть текст программ от 11-10-89
CC
!----------------------------------------
      SUBROUTINE RDATA
!----------------------------------------
      IMPLICIT REAL*8 (A-H,O-Z)
      INCLUDE '_PARAM.FOR'
      INCLUDE '_COMON.FOR'
	  
C     Main program variables

      LOGICAL MPRT

      IF ( NK .GT. 0 )  THEN
       MPRT=.FALSE.
      ELSEIF ( NK .EQ. 0 )  THEN
!     задано нулевое число точек контура:
       PRINT '(/T3,''задано нулевое число точек контура'')'
       STOP
      ELSE
       MPRT=.TRUE.
       NK=-NK
      ENDIF
!
      DO 562 I=1,NK
	READ (11,*)	IPO(I),IR(I),IZ(I),DELR(I),DELZ(I)
 562  CONTINUE
!
      IF( MPRT )
     + PRINT '(''     N  R-KOOP.  Z-KOOP. DR-.    DZ-K.'')'	  
      IF( MPRT ) PRINT '(2(4I5,2F9.4,1X,1HI))'
     +  ,(K,IPO(K),IR(K),IZ(K),DELR(K),DELZ(K),K=1,NK)
      RETURN
*=== End of subroutine RDATA =========================================*
      END
CC
CC    есть текст программ от 11-10-89
CC
!----------------------------------------
      SUBROUTINE WDATA
!----------------------------------------
      IMPLICIT REAL*8 (A-H,O-Z)
      INCLUDE '_PARAM.FOR'
      INCLUDE '_COMON.FOR'
	  
      PRINT '(/'' число точек граничного контура:'',I5/)',NK
	WRITE (11,*) ' число точек граничного контура:'
      DO 562 I=1,NK
	WRITE (11,*) IZ(I),IR(I),DELR(I),DELZ(I),IPO(I)
 562    CONTINUE
      RETURN
*=== End of subroutine WDATA =========================================*
      END
