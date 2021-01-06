!CC    I.for - For MS FORTRAN 5.1
!CC
!CC    11.10.89
!----------------------------------------
      SUBROUTINE IOAT (FPARAM)
!----------------------------------------
      IMPLICIT REAL*8 (A-H,O-Z)
      INCLUDE '_PARAM.FOR'
      INCLUDE '_COMMON.FOR'

      REAL FF  
      CHARACTER*6 CHRNIT
      CHARACTER*128 fname/'mtra.TXT'C/
      LOGICAL	Fexist,	FPARAM
      DATA    NK2 /2/ !канал записи траекторий трубок тока
!---
      CALL IBCD   (NITER,CHRNIT)
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
!---
      IF(FPARAM)WRITE (14,'(''EL&ION VISUALISATION POTENTIAL OUTPUT'')')
      IF(.NOT.FPARAM)WRITE (14,'(''EL&ION VISUALISATION CHRG OUTPUT'')')  
	WRITE (14,*) 'TITLE=EL&ION VISUALISATION №'//CHRNIT  
	WRITE (14,*) 'XLIM=0,300' 
	WRITE (14,*) 'YLIM=-75,75' 
	WRITE (14,*) 'XLABEL=AXIS-Z'
	WRITE (14,*) 'YLABEL=AXIS-R'
!
! Построение эквипотенциалей
!
	WRITE (14,*) 'CONTOUR-XYZ'
	WRITE (14,*) 'MASK=0'
      DO IZT = 0, IZL1-1
           Z2=XU(IZT)
           DO IRT = -(IRL1-1), -1  !LOOP_BOT: 
           R2=-YV(-IRT)
      IF (FPARAM)  THEN 
	     FF = FP(-IRT*IZL1+IZT+1)
      ELSE
	     FF = F(-IRT*IZL1+IZT+1)
      ENDIF
	     WRITE (14,*) HFIX(Z2),HFIX(R2),FF  
		 END DO  !LOOP_BOT 
           DO IRT = 0, (IRL1-1)  !LOOP_TOP: 
           R2=YV(IRT)
      IF (FPARAM)  THEN 
	     FF = FP(IRT*IZL1+IZT+1) ! вывод потенциала
      ELSE
	     FF = F(IRT*IZL1+IZT+1)! вывод объемного заряда
      ENDIF
	     WRITE (14,*) HFIX(Z2),HFIX(R2),FF  
		 END DO  !LOOP_TOP 
	END DO 
! Построение граничных точек контура ОБЛАСТИ
!
	WRITE (14,*) ' LINE-XY'
	WRITE (14,*) ' DUBLICATE-Y' 
      DO K=1,NK
	  WRITE (14,*) HFIX(IZ(K)),HFIX(IR(K)),REAL(DELR(K)),REAL(DELZ(K))     !,IPO(I)
		 END DO 
!
! Считывание массива траекторий частиц
!
      REWIND (NK2)
      DO K=1,N4  !LOOP_NK2: 
	     CALL IBCD   (K,CHRNIT)
	     READ (NK2, END=77777) MTM,(PX(J),PY(J),J=1,MTM)
!
! Если при считывании массива траекторий частиц ошибка, то нет записи
!
	     WRITE (14,'(A)') 'Output trajectories №'//CHRNIT  
	     WRITE (14,*) 'LINE-XY' 
	     WRITE (14,*) 'LINEWIDTH=0.3' 
	     WRITE (14,*) 'ALPHA=0.3' 
	     WRITE (14,*) 'COLOR=green' 
	     WRITE (14,*) 'DUBLICATE-Y' 
	     IF(MTM.GT.1)   THEN
	       DO J=1,MTM
	         WRITE (14,*) PX(J),PY(J)
		   END DO 
                     ENDIF
	END DO   !LOOP_NK2
77777 CONTINUE
      CLOSE (UNIT=14,  STATUS='KEEP')
!* End of subroutine IOAT =========================================*
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
