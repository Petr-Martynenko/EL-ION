*=== GRAFM.dsp E.FOR ======================================================*
CC    Compaq Visual FORTRAN 6.6.0
CC
CC    11.10.89
!----------------------------------------
      SUBROUTINE E
!----------------------------------------
      IMPLICIT REAL*8 (A-H,O-Z)
!DEC$ ATTRIBUTES C,ALIAS:"__setcolor" ::  setcolor

      INCLUDE '_PARAM.FOR'
      INCLUDE '_COMON.FOR'

C     Main program variables
	  
      REAL*8      aMAX/1D0/,aMIN/-1D0/  
	INTEGER*2   j2,J
      INTEGER     NOUT/0/
      REAL        XMAX,YMAX,XMIN,YMIN,XL,YL
      REAL        XF,YF
      REAL        BMA,BMI,BUY
      REAL        Z2,R2,RF,XOF,YOF
      LOGICAL     LYP / .TRUE. /
	CHARACTER*6 CHRNIT
      DATA    NK2 /2/ !канал записи траекторий трубок тока
      DATA    NK4 /4/ !канал записи эквипотенциалей
      OPEN   (UNIT=NOUT,FILE='CHECKERR.TXT',POSITION='APPEND')
!----------------------------------------------
 1    WRITE (*,*) 'INSERT ПРИЗНАК РИСУНКА (DEFAULT:',MI,')'
      READ  (*,*, ERR=1, END=2) MI
 2    WRITE (*,*) 'MI=',MI
      IF (MI .EQ. 0) GOTO 11111
      CALL IBCD   (NITER,CHRNIT)
      IF (MI .GE. 0) THEN 
 3    WRITE (*,*) 'INSERT 10 NUMB.POTENTIALS (DEFAULT:',C,')'
      READ  (*,*, ERR=3, END=4) C
 4    WRITE (*,*) 'C=',C
      DO 5 J=1,NO
      IF (C(J).NE.0.) LYP = .FALSE.
 5    CONTINUE
      IF (LYP) THEN
CALL LEVFUN( F, IZL1, IRL1, C, 10) 
C...построения массива уровней с равным шагом между экстремумами.
      FF = aMIN
      DO 6 J=1,NO
      C(J) = FF+(aMAX-aMIN)/(NO+1)
      FF = C(J)
 6    CONTINUE
      END IF
      write (NOUT,'('' МАССИВ УРОВНЕЙ ПОТЕНЦИАЛА''/
     + 10(I3,E10.3))') (I,C(I),I=1,10)
      REWIND (NK4)
      CALL PLOTS
      ENDIF
      write (NOUT,'('' МАСШТАБНЫЙ МНОЖИТЕЛЬ РАВЕН'',E10.3)') S
C! Определение к-т правого верхнего узла области
      XMAX=XU(IZL1-1)
      YMAX=YV(IRL1-1)
C! Определение к-т левого нижнего узла области
      XMIN=XU(0)
      YMIN=YV(0)
C! Определение продольного и поперечного размера страницы
      XL=(XMAX-XMIN)*S
      YL=(YMAX-YMIN)*S
      CALL grinit
      CALL gf_setnp(1250, 600)
C! Определение ОБЛАСТИ,ПРЕДЕЛОВ,СТРАНИЦЫ,ОСЕЙ X,Y
      CALL page (XL+2.0,YL+8.0, 'PAGE GRAFOR', 11, 1)
      CALL setBGR (63)
      CALL setPEN (0)
CALL page (25.6, 19.0, 'PAGE GRAFOR', 11, 1)
      CALL setPEN (7)
      CALL region (2.3, 1.3, XL, YL, 'MEDIAN PLANE', 12, 1)
	CALL SYMBOL (3.,0.0,0.4,'NITER='//CHRNIT, -12,0.)
      CALL LIMITS (XMIN*Z0, XMAX*Z0, YMIN*Z0, YMAX*Z0)
      CALL axes   ('X,m', 3, 0.0, 1, 'Y,m', 3, 0.05, 1, 00)
      CALL LIMITS (XMIN, XMAX, YMIN, YMAX)
      CALL setPEN (0)
      CALL BAR   (XMIN+.5,YMIN+.4,YMAX-YMIN,XMAX-XMIN-.5,YMAX-YMIN,2,20)
      CALL setPEN (7)
CALL SYMBOL_RUS(2.3,YL,0.777,'эквипотенциали', 14,0.)
!
! Построение контура ОБЛАСТИ
!
      CALL setPEN (7)
      CALL MOVE(2.3, 2.3, 0)
      DO 20 I = 1, ABS(MS)
      IM1=I-1
      IF (I.EQ.1) IM1=ABS(MS)
      R2=BDR(I)
      Z2=BDZ(I)
      CALL TMF (Z2,R2,XF,YF)
      IF (I.EQ. 1) CALL MOVE(XF,YF,0)
      IF (MI.LT.0) THEN
!
! Построение граничных точек контура ОБЛАСТИ
!
         CALL MOVE(XF,YF,0)
         CALL MARKER(2)
      ELSE
!
! Построение контура ОБЛАСТИ
!
      IF (REX(IM1) .NE. 0.) THEN
      N=REX(IM1)/DABS(REX(IM1))
!
!	определение 
!
	 CALL CORCR(BDZ(IM1),BDR(IM1),BDZ(I),BDR(I),XCN,YCN,REX(IM1))
       Z2=XCN
       R2=YCN
	 CALL TMF (Z2,R2,XOF,YOF)
	 RF = SQRT((XF-XOF)**2+(YF-YOF)**2)
	 IF (N .EQ. -1) RF = -RF
	 CALL ARCIB( RF, XF, YF, 0)
      ELSE
         CALL MOVE(XF,YF,1)
      END IF
      END IF
 20   CONTINUE
!
! Построение эквипотенциалей
!
      IF (ABS(MI) .EQ. 1) GOTO 100
      REWIND (NK4)
 40   READ   (NK4, END=55) FF,MTM,(PX(J),PY(J), J=1,MTM)
	J=0
      DO 50 I=1,NO
	 J=J+1
       IF (J .EQ. 16) J=1
       IF (FF .EQ. C(I)) j2 = setcolor (J)
 50   CONTINUE
	     Z2=PX(1)
	     R2=PY(1)
	     CALL TMF (Z2,R2,XF,YF)
	     CALL MOVE(XF,YF,0)
      DO 53 I=2,MTM
	     Z2=PX(I)
	     R2=PY(I)
	     CALL TMF (Z2,R2,XF,YF)
	     CALL MOVE(XF,YF,1)
 53   CONTINUE
      GO TO 40
!
! Построение траекторий частиц
!
 55   IF (ABS(MI) .EQ. 2) GO TO 100
      CALL setPEN (8)
!
! Считывание массива траекторий частиц
!
      REWIND (NK2)
      DO 70 I=1,IZZ
	   IF (MOD(I,MZ).EQ.0 .OR. I.EQ.1) THEN
	     READ (NK2, END=71) MTM,(PX(J),PY(J),J=1,MTM)
	     IF (MTM.GT.1) CALL LINEO(PX,PY,MTM)
	   END IF
 70   CONTINUE
 71   CONTINUE
      IF (ABS(MI).EQ.3) GOTO 100
!
! Построение распределения потенциала на оси
!
      CALL setPEN (7)
      CALL REGION (2.3, YL+3.9, XL, 3.8, 'AXES POTENTIAL', 14, 1)
      CALL LIMITS (XMIN*Z0, XMAX*Z0, .0, FG*1.2)
      CALL axes   (''C, 1, 0.0, 4, 'F', 1, FG, 5, 00)
      CALL setpen (8)
      Z2=XU(2)*Z0
      R2=F(3)*FG
      CALL TMF (Z2,R2,XF,YF)
      CALL MOVE(XF,YF,0)
      DO 80 I=3,IZL1
	     Z2=XU(I-1)*Z0
	     R2=F(I)*FG
	     CALL TMF (Z2,R2,XF,YF)
      CALL FATLIN(XF,YF,.8)
 80   CONTINUE
      IF (ABS(MI).EQ.4) GOTO 100
!
! Построение распределения потенциала на оси
!
      IF (MAG) THEN 
	   BMA=BZA(IZMAX)
	   BMI=BZA(IZMAX)
	   DO 81 J=-6,IZMAX
           IF (BMI .GT. BZA(J))BMI=BZA(J)
 81	   CONTINUE
	   DO 82 J=-6,IZMAX
           IF (BMA .LT. BZA(J))BMA=BZA(J)
 82	   CONTINUE
	   BMA=BMA*BN
	   BMI=BMI*BN
	   BUY=(BMA-BMI)
	   write (NOUT,*)' BMI=',BMI,' TL       BMA=',BMA,' TL'
      CALL LIMITS (XMIN*Z0, XMAX*Z0,BMI, BMA)
CALL axes (''C, 0, 0.0, 0, ''C, 0, 0.0, 0, 0)
      CALL setPEN (1)
      Z2=XU(0)*Z0
      R2=SNGL(BZA(0))*BN
      CALL TMF (Z2,R2,XF,YF)
      CALL MOVE(XF,YF,0)
      DO 90 I=1,IZL1
	     Z2=XU(I-1)*Z0
	     R2=SNGL(BZA(I-1))*BN
	     CALL TMF (Z2,R2,XF,YF)
	     CALL MOVE(XF,YF,1)
 90    CONTINUE
      ENDIF
 100  CALL ENDPG(0)
11111 write (NOUT,*)	'AXES POTENTIAL DISTRIBUTION'
      write (NOUT,'(12G12.3)')(XU(I-1),F(I),I=1,IZL1)
      CLOSE (UNIT=NOUT, STATUS = 'KEEP')
*=== End of subroutine E =========================================*
      END
!----------------------------------------
      SUBROUTINE PLOTS
C *** CALLS FOR EQUIPOTENTIAL LINES ***
!----------------------------------------
      IMPLICIT REAL*8 (A-H,O-Z)

      INCLUDE '_PARAM.FOR'
      INCLUDE '_COMON.FOR'
!/MM/
CC    S-масштаб рисунка, см/сет.ед.
CC    MI=0--график не строится  
CC    MI<0--эквипотенциали не строятся  
	     EQUIPR=7D0
	     EQLN=3 ! макс.количество итераций уточнения координат
	     EQST=3 ! количество делений начального шага эквипотенциали			   
	     DO 10 I=1,NO
	     FF=C(I)
	     CALL EQUIP
 10   CONTINUE
      RETURN
C *** *** END PLOTS
      END
!----------------------------------------
      SUBROUTINE DSPROC( IEQQ, EQB, RHO, ZETA, C1, C2, *)
C *** INCREMENTS EQUIPOTENTIAL LINE ***
*  INTEGER IEQQ - inp  ! макс.кол-во итераций уточнения координат  
*  LOGICAL EQB  - inp  !
*  REAL*8 ZETA  - inp  ! продольная координата , норм.
!----------------------------------------
      IMPLICIT REAL*8 (A-H,O-Z)
CC    EQB=.TRUE.--определяются приращения координат эквипотенциали	  
CC    EQB=.FALSE.--определяется только приращение продольной координаты	  
      INTEGER IEQQ
      LOGICAL EQB
	  
      INCLUDE '_PARAM.FOR'
      INCLUDE '_COMON.FOR'
	  
      DS = 1.0
C *** обнуление счетчика числа итераций ***
      L=0
C *** цикл уточнения координат приращения эквипотенциали ***
 10   CONTINUE
      IF (RHO .LT. 1.E-10) GOTO 40
C *** слабая вариация шага ***
      IF (DABS(DS) .LE. 0.01) RETURN
C *** исчерпание количества итераций ***
      IF (L .GT. IEQQ) RETURN
C ****    CALL  FORCE (AO,ZETA,RHO,C1,C2)
      CALL  FORCE (X,ZETA,RHO,C1,C2)
      IF (IGMA .EQ. 0) GOTO 40 
      DS = C1*C1+C2*C2
C *** проверка приближения значения функции к заданному ***
      IF (DS .LT. 0.001) DS = 1.0
      DS = (FF-X) / DS
C *** выполнение шага по координатам ***
      IF (EQB) RHO=RHO+C2*DS
             ZETA=ZETA+C1*DS
      IF (DABS(C2) .LE. DABS(C1)) GOTO 20
      DS = C2*DS
      GOTO 30 
 20   DS = C1*DS
C *** увеличение счетчика шагов ***
 30   L=L+1
      GOTO 10 
 40   CONTINUE
C *** WRITE (*,*) ' RHO =',RHO,' ZETA =',ZETA,' DS =',DS,' X =',X
      RETURN 1
C *** *** END DSPROC
      END
!----------------------------------------
      SUBROUTINE EQUIP
C *** CALCULATES EQUIPOTENTIAL LINE ***
!----------------------------------------
      IMPLICIT REAL*8 (A-H,O-Z)
      INCLUDE '_PARAM.FOR'
      INCLUDE '_COMON.FOR'

C *** SS DETERMINES SIGN (I.E., DIRECTION) OF EQUIPOTENTIAL LINE ***
      DO 50 LLL=1,2
       SS = 1.0
	 IF (LLL .EQ. 2) SS = -1.0
       DSSI2 = 0.
       DSSI1 = 0.
       DELT = 1./EQST
	 RHO=EQUIPR
       R = IDINT(RHO)
C *** обнуление координаты Z ***
 10    Z=0
C *** проверка значений координаты Z ***
 20    IF (Z .GE. IZL1-1) GOTO 50
       IF (R .GE. IRL1-1) GOTO 25
       DS = F(R*IZL1+Z+1)-FF
       DSSI2 = DSIGN (1D0,DS)
	 IF (DSSI1+DSSI2 .EQ. 0.) GOTO 30
C *** перебор значений координатных узлов оси Z ***
 25    Z=Z+1
       GOTO 40
C *** координата Z ячейки начала эквипотенциали найдена ***
 30    ZETA=Z
       DSSI2 = 0.
       DSSI1 = 0.
       CALL DSPROC( 20, .FALSE., RHO, ZETA, C1, C2, *40)
       I1=IDINT(ZETA)+1
       I2=Z+1
       Z=MAX0 (I2,I1)
       CALL  LISTL (SS,RHO,ZETA)
 40    DSSI1 = DSSI2
       GOTO 20
 50   CONTINUE
C *** *** END EQUIP
      END
!----------------------------------------
      SUBROUTINE LISTL(SS,RHO,ZETA)
C *** LISTS AND DRAWS EQUIPOTENTIAL LINE ***
!----------------------------------------
      IMPLICIT REAL*8 (A-H,O-Z)
      INCLUDE '_PARAM.FOR'
      INCLUDE '_COMON.FOR'

C     Main common blocks

      INTEGER     NOUT/0/
      DATA NK4 /4/ !канал записи эквипотенциали

      J=1
 20   CALL DSPROC( EQLN, .TRUE., RHO, ZETA, C1, C2, *30)
      PX(J) = ZETA
      PY(J) = RHO
      J=J+1
 25   TERM3=DABS(C1)+DABS(C2)
      IF (TERM3 .LT. 1.E-10) GOTO 30 
      IF ( J .GT. MT ) GOTO 30
      DELT1 = DELT/ DSQRT(C1*C1+C2*C2)
      RHO=RHO-C1*DELT1*SS
      ZETA=ZETA+C2*DELT1*SS
      GOTO 20 
C *** цикл построения эквипотенциали завершен ***
 30   J=J-1
      IF ( J .LE. 1 ) GOTO 40
     	IF(SS.GE.0.)write (NOUT,100) ZETA,FF,J
      WRITE (NK4) FF,J,(PX(I),PY(I), I=1,J)
 40   CONTINUE
	RHO=EQUIPR
      RETURN
 100  FORMAT
     *(' EQUIPOTENTIAL LINE AT Z=',F8.3,' WITH POTENTIAL V=',F8.3,
     * ' Number points of LINE NK=',I4)
C *** *** END LISTL *** *** 
      END
