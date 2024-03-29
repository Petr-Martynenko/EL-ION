!CC    I.for - For MS FORTRAN 5.1
!CC
!CC    11.10.89
!----------------------------------------
      SUBROUTINE IOAT (FPARAM)
! FPARAM=.TRUE.  ����� ������� ��������� ������	  CHARGE?.TXT
! FPARAM=.FALSE. ����� ���������� 	          POTENT?.TXT
!----------------------------------------
      IMPLICIT REAL*8 (A-H,O-Z)
      INCLUDE '_PARAM.FOR'
      INCLUDE '_COMMON.FOR'

      CHARACTER*128 fname/'mtra.TXT'C/
      LOGICAL	Fexist,	FPARAM
      DATA    NK2 /2/ !����� ������ ���������� ������ ����
!---
	   OPEN (UNIT=14, ACCESS='APPEND', FILE = fname)
!---	   INQUIRE about file's existence:
	   INQUIRE (FILE = fname, EXIST = Fexist)
	   IF (.NOT.Fexist) return
!--- ����� ������� ��������� ������	  CHARGE?.TXT
      IF(FPARAM)  THEN
	WRITE (14,'(''EL&ION VISUALISATION CHRG OUTPUT'')')
      ELSE
	WRITE (14,'(''EL&ION VISUALISATION POTENT OUTPUT'')')
      ENDIF
	WRITE (14,'(''TITLE=EL&ION VISUALISATION �'',I3.3)') NITER 
	WRITE (14,'(''XLIM=0,150'')')
	WRITE (14,'(''YLIM=0,105'')')
	WRITE (14,'(''XLABEL=AXIS-Z'')')
	WRITE (14,'(''YLABEL=AXIS-R'')')
	WRITE (14,'(''CONTOUR-XYZ'')')
	WRITE (14,'(''MASK=0'')')
!--- ����� �������
      DO IZT = 0, IZLIM1-1
           Z2=XU(IZT)
           DO IRT = -(IRLIM1-1), -1  !LOOP_BOT: 
           R2=-YV(-IRT)
      IF (FPARAM)  THEN 
	     FF = FP(-IRT*IZLIM1+IZT+1)
      ELSE
	     FF = F(-IRT*IZLIM1+IZT+1)
      ENDIF
	     WRITE (14,*) HFIX(Z2),HFIX(R2),REAL(FF)  
		 END DO  !LOOP_BOT 
           DO IRT = 0, (IRLIM1-1)  !LOOP_TOP: 
           R2=YV(IRT)
      IF (FPARAM)  THEN 
	     FF = FP(IRT*IZLIM1+IZT+1) ! ����� ����������
      ELSE
	     FF = F(IRT*IZLIM1+IZT+1)! ����� ��������� ������
      ENDIF
	     WRITE (14,*) HFIX(Z2),HFIX(R2),REAL(FF)  
		 END DO  !LOOP_TOP 
	END DO 
!
! ���������� ��������� ����� ������� �������
!
	WRITE (14,'(''LINE-XY'')')
	WRITE (14,'(''DUBLICATE-Y'')')
      DO K=1,NK
	  WRITE (14,*) IZ(K),IR(K),REAL(DELR(K)),REAL(DELZ(K))     !,IPO(I)
		 END DO 

      IF (FPARAM)  THEN 
!
! ���������� ���������������
!
	     EQUIPR=0.1 !
	     EQLN=3 ! ����.���������� �������� ��������� ���������
	     EQST=3 ! ���������� ������� ���������� ���� ��������������			   
      DO 55 K=1,NO
	     FF=C(K)
	     CALL EQUIP
	     IF(MTM.GT.1)   THEN
!
! ���� ��� ���������� ��������������� ������, �� ��� ������
!
!	WRITE (14,'(''EQUIPOTENTIAL LINE �'',I3.3)') K 
	WRITE (14,'(''LINE-XY'')')
	WRITE (14,'(''COLOR=BLACK'')')
	WRITE (14,'(''LINEWIDTH=0.6'')')
	WRITE (14,'(''ALPHA=0.5'')')
	       DO J=1,MTM
	         WRITE (14,'(2F8.3)') REAL(PX(J)),REAL(PY(J))
		   END DO 
      ENDIF
 55   CONTINUE
      ENDIF
!
! ���������� ������� ���������� ������
!
      REWIND (NK2)
      DO K=1,N4  !LOOP_NK2: 
	     READ (NK2, END=77777) MTM,(PX(J),PY(J),J=1,MTM)
	     IF(MTM.GT.1)   THEN
!
! ���� ��� ���������� ������� ���������� ������ ������, �� ��� ������
!
	WRITE (14,'(''Output trajectories �'',I3.3)') K  
	WRITE (14,'(''LINE-XY'')')
	WRITE (14,'(''COLOR=WHITE'')')
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
!*** INCREMENTS EQUIPOTENTIAL LINE ***
!*  INTEGER IEQQ - inp  ! ����.���-�� �������� ��������� ���������  
!*  LOGICAL EQB  - inp  !
!*  REAL*8 RHO   - inp  ! ���������� ���������� ����� .���.��.
!*  REAL*8 ZETA  - inp  ! ���������� ���������� ����� .���.��.
!*  REAL*8 C1    - out  ! ���������� ��������� ..,����.
!*  REAL*8 C2    - out  ! ���������� ��������� .....,����.
!----------------------------------------
      IMPLICIT REAL*8 (A-H,O-Z)
      INCLUDE '_PARAM.FOR'
      INCLUDE '_COMMON.FOR'
***********************************************
!    EQB=.TRUE. --������������ ���������� ��������� ��������������	  
!    EQB=.FALSE.--������������ ������ ���������� ���������� ����������	  
      INTEGER IEQQ
      LOGICAL EQB
	  
      DS = 1.0
C *** ��������� �������� ����� �������� ***
      L=0
C *** ���� ��������� ��������� ���������� �������������� ***
 10   CONTINUE
      IF (RHO .LT. 1.E-10) GOTO 40
C *** ������ �������� ���� ***
      IF (DABS(DS) .LE. 0.001) GOTO 60
C *** ���������� ���������� �������� ***
      IF (L .GT. IEQQ) RETURN
!*      REAL*8 ZETA- inp  ! ���������� ���������� ����� .���.��.
!*      REAL*8 RHO - inp  ! ���������� ���������� ����� .���.��.
!*      REAL*8 C1  - out  ! ���������� ��������� ..,����.
!*      REAL*8 C2  - out  ! ���������� ��������� .....,����.
	    CALL  FORCE (X,ZETA,RHO,C1,C2)
      IF (IGMA .EQ. 0) GOTO 40 
      DS = C1*C1+C2*C2
C *** �������� ����������� �������� ������� � ��������� ***
      IF (DS .LT. 0.001) DS = 1.0
      DS = (FF-X) / DS
C *** ���������� ���� �� ����������� ***
      IF (EQB) RHO=RHO+C2*DS
             ZETA=ZETA+C1*DS
C *** ��������� ���������� ���� ***
      IF (DABS(C2) .LE. DABS(C1)) GOTO 20
      DS = C2*DS
      GOTO 30 
 20   DS = C1*DS
C *** ���������� �������� ����� ***
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
C *** ��������� ���������� Z ***
        Z=0
C *** �������� �������� ���������� Z ***
 20    IF (Z .GE. IZLIM1-1) GOTO 50
       IF (R .GE. IRLIM1-1) GOTO 25
       DS = F(R*IZLIM1+Z+1)-FF
       DSSI2 = DSIGN (1D0,DS)
	 IF (DSSI1+DSSI2 .EQ. 0.) GOTO 30
C *** ������� �������� ������������ ����� ��� Z ***
 25    Z=Z+1
       GOTO 40
C *** ���������� Z ������ ������ �������������� ������� ***
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
!*  REAL*8 RHO   - inp  ! ���������� ���������� ����� .���.��.
!*  REAL*8 ZETA  - inp  ! ���������� ���������� ����� .���.��.
!----------------------------------------
      IMPLICIT REAL*8 (A-H,O-Z)
      INCLUDE '_PARAM.FOR'
      INCLUDE '_COMMON.FOR'

      DATA NK4 /4/ !����� ������ ��������������
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
C *** ���� ���������� �������������� �������� ***
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
	     EQLN=3 ! ����.���������� �������� ��������� ���������
	     EQST=3 ! ���������� ������� ���������� ���� ��������������			   
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
      IND1=J*IZLIM1+IZLS
      F(IND1)=0.5*(F(I*IZLIM1)+F(IND1-IZLIM1))
 10   CONTINUE
 1111	FORMAT(F6.3)
 2222	FORMAT(11E11.4)
	  WRITE (*,1111) Z0*(IZLS-1+XU(0))
	  WRITE (*,2222) (F(I*IZLIM1+IZLS)*FG,I=0,20)
	  END IF

      IF (RDZP .EQ. 'ZAP') THEN
	IZLS=ZSCHIF+1
	IZLF=IZLIM1
	WRITE (4) (F(I*IZLIM1+IZLS),I=0,20)
	WRITE (*,1111) Z0*(IZLS-1+XU(0))
	WRITE (*,2222) (F(I*IZLIM1+IZLS)*FG,I=0,20)
	WRITE (*,1111) Z0*(IZLF-1+XU(0))
	WRITE (*,2222) (F(I*IZLIM1+IZLF)*FG,I=0,20)
      END IF
      END !REDZAP
