!CC    I.for - For MS FORTRAN 5.1
!CC
!CC    11.10.89
!----------------------------------------
      SUBROUTINE IOAT (FPARAM)
! FPARAM=.TRUE.  вывод массива объемного заряда	  CHARGE?.TXT
! FPARAM=.FALSE. вывод потенциала 	          POTENT?.TXT
!----------------------------------------
      IMPLICIT REAL*8 (A-H,O-Z)
      INCLUDE '_PARAM.FOR'
      INCLUDE '_COMMON.FOR'

      LOGICAL	Fexist
      CHARACTER*128 fname/'mtra.TXT'C/
      LOGICAL	FPARAM
      DATA    NK2 /2/ !канал записи траекторий трубок тока
!---
      NVAR = 0
      Fexist=.TRUE.
      DO WHILE (Fexist)
           NVAR = NVAR + 1
         IF (FPARAM)  THEN 
	   fname='CHARGE'//CHAR(64+NVAR)//'.TXT'
         ELSE
	   fname='POTENT'//CHAR(64+NVAR)//'.TXT'
         ENDIF
!---	   INQUIRE about file's existence:
	   INQUIRE (FILE = fname, EXIST = Fexist)
	   IF(.NOT.Fexist)OPEN (UNIT=14, FILE = fname)
      END DO  
!--- вывод массива объемного заряда	  CHARGE?.TXT
      IF (FPARAM)  THEN 
	WRITE (14,'(''EL&ION VISUALISATION CHRG OUTPUT'')')  
	WRITE (14,'(''TITLE=EL&ION VISUALISATION №'',I3.3)') NITER 
	WRITE (14,'(''XLIM=0,150'')')
	WRITE (14,'(''YLIM=-75,75'')')
	WRITE (14,'(''XLABEL=AXIS-Z'')')
	WRITE (14,'(''YLABEL=AXIS-R'')')
	WRITE (14,'(''MASK=0'')')
!
! Построение поля объемного заряда
!
	DO IZT = 0, IZL1-1
           Z2=XU(IZT)
           DO IRT = -(IRL1-1), -1  !LOOP_BOT: 
           R2=-YV(-IRT)
	     FF = FP(-IRT*IZL1+IZT+1)
	     WRITE (14,*) HFIX(Z2),HFIX(R2),FF  
		 END DO  !LOOP_BOT 
           DO IRT = 0, (IRL1-1)  !LOOP_TOP: 
           R2=YV(IRT)
	     FF = FP(IRT*IZL1+IZT+1) ! вывод потенциала
	     WRITE (14,*) HFIX(Z2),HFIX(R2),FF  
		 END DO  !LOOP_TOP 
	END DO 
!---  вывод потенциала 	          POTENT?.TXT
      ELSE
	WRITE (14,'(''EL&ION VISUALISATION POTENTIAL OUTPUT'')')
	WRITE (14,'(''TITLE=EL&ION VISUALISATION №'',I3.3)') NITER 
	WRITE (14,'(''XLIM=0,150'')')
	WRITE (14,'(''YLIM=-75,75'')')
	WRITE (14,'(''XLABEL=AXIS-Z'')')
	WRITE (14,'(''YLABEL=AXIS-R'')')
	WRITE (14,'(''MASK=0'')')
!
! Построение эквипотенциалей
!
	DO IZT = 0, IZL1-1
           Z2=XU(IZT)
           DO IRT = -(IRL1-1), -1  !LOOP_BOT: 
             R2=-YV(-IRT)
	       FF = F(-IRT*IZL1+IZT+1)
	         WRITE (14,*) HFIX(Z2),HFIX(R2),FF  
		   END DO  !LOOP_BOT 
           DO IRT = 0, (IRL1-1)  !LOOP_TOP: 
             R2=YV(IRT)
	       FF = F(IRT*IZL1+IZT+1) ! вывод потенциала
	         WRITE (14,*) HFIX(Z2),HFIX(R2),FF  
		   END DO  !LOOP_TOP 
	END DO 
      ENDIF
!
! Построение граничных точек контура ОБЛАСТИ
!
	WRITE (14,'(''LINE-XY'')')
	WRITE (14,'(''DUBLICATE-Y'')')
      DO K=1,NK
	  WRITE (14,*) IZ(K),IR(K),REAL(DELR(K)),REAL(DELZ(K))     !,IPO(I)
		 END DO 

      IF (MAG)  THEN 
!
! Построение эквипотенциалей
!
	     EQUIPR=0.1 !
	     EQLN=3 ! макс.количество итераций уточнения координат
	     EQST=3 ! количество делений начального шага эквипотенциали			   
      DO 55 K=1,NO
	     FF=C(K)
	     CALL EQUIP
	     IF(MTM.GT.1)   THEN
!
! Если при считывании эквипотенциалей, то нет записи
!
!	WRITE (14,'(''EQUIPOTENTIAL LINE №'',I3.3)') K 
	WRITE (14,'(''LINE-XY'')')
      IF (K .GE. 1)  THEN
	WRITE (14,'(''COLOR=BLACK'')')
	WRITE (14,'(''LINEWIDTH=0.5'')')
	WRITE (14,'(''ALPHA=0.5'')')
      ENDIF
	       DO J=1,MTM
	         WRITE (14,'(2F8.3)') REAL(PX(J)),REAL(PY(J))
		   END DO 
		     ENDIF
 55   CONTINUE
      ENDIF
!
! Считывание массива траекторий частиц
!
      REWIND (NK2)
      DO K=1,N4  !LOOP_NK2: 
	     READ (NK2, END=77777) MTM,(PX(J),PY(J),J=1,MTM)
	     IF(MTM.GT.1)   THEN
!
! Если при считывании массива траекторий частиц ошибка, то нет записи
!
!	WRITE (14,'(''TRаекторий LINE №'',I3.3)') K 
	WRITE (14,'(''LINE-XY'')')
	WRITE (14,'(''COLOR=BLACK'')')
	WRITE (14,'(''GAMA=  .05'')')
	WRITE (14,'(''LINEWIDTH=0.5'')')
	WRITE (14,'(''ALPHA=0.5'')')
	       DO J=1,MTM
	         WRITE (14,*) REAL(PX(J)),REAL(PY(J))
		   END DO 
	     ENDIF
	END DO   !LOOP_NK2
77777 CONTINUE
      CLOSE (UNIT=14,  STATUS='KEEP')
!* End of subroutine IOAT =========================================*
      END  
!----------------------------------------
      SUBROUTINE DSPROC( IEQQ, EQB, RHO, ZETA, C1, C2, *)
C *** INCREMENTS EQUIPOTENTIAL LINE ***
*  INTEGER IEQQ - inp  ! макс.кол-во итераций уточнения координат  
*  LOGICAL EQB  - inp  !
*  REAL*8 ZETA  - inp  ! продольная координата , норм.
!----------------------------------------
      IMPLICIT REAL*8 (A-H,O-Z)
      INCLUDE '_PARAM.FOR'
      INCLUDE '_COMMON.FOR'
***********************************************
CC    EQB=.TRUE.--определяются приращения координат эквипотенциали	  
CC    EQB=.FALSE.--определяется только приращение продольной координаты	  
      INTEGER IEQQ
      LOGICAL EQB
	  
      DS = 1.0
C *** обнуление счетчика числа итераций ***
      L=0
C *** цикл уточнения координат приращения эквипотенциали ***
 10   CONTINUE
      IF (RHO .LT. 1.E-10) GOTO 40
C *** слабая вариация шага ***
      IF (DABS(DS) .LE. 0.001) GOTO 60
C *** исчерпание количества итераций ***
      IF (L .GT. IEQQ) RETURN
!*      REAL*8 ZETA- inp  ! продольная координата точки .сет.ед.
!*      REAL*8 RHO - inp  ! поперечная координата точки .сет.ед.
!*      REAL*8 C1  - out  ! продольная компонент ..,норм.
!*      REAL*8 C2  - out  ! поперечная компонент .....,норм.
	    CALL  FORCE (X,ZETA,RHO,C1,C2)
      IF (IGMA .EQ. 0) GOTO 40 
      DS = C1*C1+C2*C2
C *** проверка приближения значения функции к заданному ***
      IF (DS .LT. 0.001) DS = 1.0
      DS = (FF-X) / DS
C *** выполнение шага по координатам ***
      IF (EQB) RHO=RHO+C2*DS
             ZETA=ZETA+C1*DS
C *** уточнения приращения шага ***
      IF (DABS(C2) .LE. DABS(C1)) GOTO 20
      DS = C2*DS
      GOTO 30 
 20   DS = C1*DS
C *** увеличение счетчика шагов ***
 30   L=L+1
      GOTO 10 
 40   CONTINUE
      RETURN 1
 60   CONTINUE
      RETURN
C *** *** END DSPROC
      END
!----------------------------------------
      SUBROUTINE EQUIP
C *** CALCULATES EQUIPOTENTIAL LINE ***
!----------------------------------------
      IMPLICIT REAL*8 (A-H,O-Z)
      INCLUDE '_PARAM.FOR'
      INCLUDE '_COMMON.FOR'

***********************************************

      INTEGER*4 R
      INTEGER*4 Z

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
        Z=0
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
! *** LISTS AND DRAWS EQUIPOTENTIAL LINE ***
!*  REAL*8 RHO   - inp  ! поперечная координата точки .сет.ед.
!*  REAL*8 ZETA  - inp  ! продольная координата точки .сет.ед.
!----------------------------------------
      IMPLICIT REAL*8 (A-H,O-Z)
      INCLUDE '_PARAM.FOR'
      INCLUDE '_COMMON.FOR'

      DATA NK4 /4/ !канал записи эквипотенциали
***********************************************
 10   FORMAT
     &('EQUIPOTENTIAL LINE AT Z=',F8.3,' WITH POTENTIAL V=',F8.3,
     & 'RHO =',F8.3,' ZETA =',F8.3,' NUM.POINT of LINE=',I4.0)
      J=1
      ZSTART=ZETA
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
      MTM=J
!17:16 05.12.2016
      IF ( J .LE. 1 ) GOTO 40
      WRITE(9,10) ZSTART,FF,RHO,ZETA,J
      WRITE (NK4) FF,J,(PX(I),PY(I), I=1,J)
 40   CONTINUE
	RHO=EQUIPR
	RETURN
C *** *** END LISTL *** *** 
      END
!----------------------------------------
      SUBROUTINE PLOTS
C *** CALLS FOR EQUIPOTENTIAL LINES ***
!----------------------------------------
      IMPLICIT REAL*8 (A-H,O-Z)
      INCLUDE '_PARAM.FOR'
      INCLUDE '_COMMON.FOR'
	     EQUIPR=0.1
	     EQLN=3 ! макс.количество итераций уточнения координат
	     EQST=3 ! количество делений начального шага эквипотенциали			   
	     DO 10 I=1,NO
	     FF=C(I)
	     CALL EQUIP
 10   CONTINUE
      RETURN
! *** *** END PLOTS
      END
!----------------------------------------
      SUBROUTINE REDZAP
!----------------------------------------
!
      IMPLICIT REAL*8 (A-H,O-Z)
      INCLUDE '_PARAM.FOR'
      INCLUDE '_COMMON.FOR'

C     Main program variables

      CHARACTER*6 RDZP

      WRITE (*,*)'INSERT CONDITION OF RUN'
      WRITE (*,*)'RED - TO READ BOUNDARY CONDITION'
      WRITE (*,*)'ZAP - TO WRITE BOUNDARY CONDITION'
      READ  (*,'(A3)')	RDZP
      WRITE (*,'(A3)') 'RDZP=',RDZP
      REWIND 4
	  IF (RDZP .EQ. 'RED') THEN
      IZLS=1
      DO 10 J=1,40,2
      IND1=J*IZL1+IZLS
      F(IND1)=0.5*(F(IND1+IZL1)+F(IND1-IZL1))
 10   CONTINUE
 1111	FORMAT(F6.3)
 2222	FORMAT(11E11.4)
	  WRITE (*,1111) Z0*(IZLS-1+XU(0))
	  WRITE (*,2222) (F(I*IZL1+IZLS)*FG,I=0,20)
	  END IF

      IF (RDZP .EQ. 'ZAP') THEN
	IZLS=ZSCHIF+1
	IZLF=IZL1
	WRITE (4) (F(I*IZL1+IZLS),I=0,20)
	WRITE (*,1111) Z0*(IZLS-1+XU(0))
	WRITE (*,2222) (F(I*IZL1+IZLS)*FG,I=0,20)
	WRITE (*,1111) Z0*(IZLF-1+XU(0))
	WRITE (*,2222) (F(I*IZL1+IZLF)*FG,I=0,20)
      END IF
      END !REDZAP
