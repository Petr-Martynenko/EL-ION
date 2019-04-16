*=== GRAFM.dsp IOAT.FOR ======================================================*
CC    Compaq Visual FORTRAN 6.6.0
CC
CC    11.10.89
!----------------------------------------
      SUBROUTINE IOAT
!----------------------------------------
      IMPLICIT REAL*8 (A-H,O-Z)

      INCLUDE '_PARAM.FOR'
      INCLUDE '_COMON.FOR'
	CHARACTER*6 CHRNIT
      CHARACTER*128 fname/'mtra.DAT'C/
      LOGICAL	Fexist
      DATA    NK2 /2/ !канал записи траекторий трубок тока
      DATA    NK4 /4/ !канал записи эквипотенциалей
!---
      CALL IBCD   (NITER,CHRNIT)
!---
      NVAR = 0
      Fexist=.TRUE.
      DO WHILE (Fexist)
	   NVAR = NVAR+1
	   fname='mtra'//CHAR(64+NVAR)//'.DAT'
!---	   INQUIRE about file's existence:
	   INQUIRE (FILE = fname, EXIST = Fexist)
	   IF(.NOT.Fexist) OPEN (UNIT=14, FILE = fname, STATUS='REPLACE')
      END DO
!---
	WRITE (14,'(''EL&ION VISUALISATION OUTPUT'')') 
	WRITE (14,*) 'TITLE=EL&ION VISUALISATION'//CHRNIT 
	WRITE (14,*) 'XLIM=0,300' 
	WRITE (14,*) 'YLIM=-50,50' 
	WRITE (14,*) 'XLABEL=AXIS-X'
	WRITE (14,*) 'YLABEL=AXIS-Y'
!
! Построение эквипотенциалей
!
	WRITE (14,*) 'CONTOUR-XYZ'
	WRITE (14,*) 'MASK=0'
      DO IZT = 1, IZL1
           Z2=XU(IZT-1)
		 DO IRT = 1, IRL1
           R2=YV(IRT-1)
	     FF = F((IRT-1)*IZL1+IZT)
	     WRITE (14,*) Z2,R2,FF
		 END DO 
	END DO 
!
! Построение граничных точек контура ОБЛАСТИ
!
	WRITE (14,*) ' LINE-XY'
	WRITE (14,*) ' DUBLICATE-Y' 
      DO K=1,NK
	  WRITE (14,*) IZ(K),IR(K),DELR(K),DELZ(K)     !,IPO(I)
		 END DO 
!
! Считывание массива траекторий частиц
!
      REWIND (NK2)
      DO K=1,IZZ
	     CALL IBCD   (K,CHRNIT)
	     WRITE (14,*) 'Output trajectories №'//CHRNIT 
	     WRITE (14,*) 'LINE-XY' 
	     WRITE (14,*) 'LINEWIDTH=0.5' 
      IF (ZNAK .GT. 0.) WRITE (14,*) 'COLOR=black' 
      IF (ZNAK .LT. 0.) WRITE (14,*) 'COLOR=green' 
	     WRITE (14,*) 'DUBLICATE-Y' 
	     READ (NK2, END=77777) MTM,(PX(J),PY(J),J=1,MTM)
	     IF(MTM.GT.1) CALL LINEO(PX,PY,MTM)
	     DO J=1,MTM
	     WRITE (14,*) PX(J),PY(J)
		 END DO 
	END DO 
77777 CONTINUE
*=== End of subroutine IOAT =========================================*
      END