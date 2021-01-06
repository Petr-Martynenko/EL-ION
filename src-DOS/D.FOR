!CC    D.for - For MS FORTRAN 5.1
!CC
!CC    11.10.89
!----------------------------------------
      SUBROUTINE FPOTE
!----------------------------------------
      IMPLICIT REAL*8 (A-H,O-Z)
      INCLUDE '_PARAM.FOR'
      INCLUDE '_COMMON.FOR'

!C     Main program variables

      REAL*8 AP
      REAL*8 PP
      REAL*8 UVX(N4)
      REAL*8 PTX(N4)
      LOGICAL BL
      DATA NK1 /1/ !канал записи потенциала
      DATA NK3 /3/ !канал записи обьемного заряда
!CC
!CC    печать основных параметров
!CC
      IF(NITER .EQ. 0) THEN 
      WRITE (*,'(T6,''Potential of contour electrodes='')')
      WRITE(*,'(10(I6,F8.2))')(I,POT(I),I=1,NJMAX)
      ENDIF
      IDIM1=IRL1*IZL1 !к-во узлов сетки потенциала и матрицы плотности
      IF( IDIM1 .GT. IDIM ) GO TO 900
!----------------------------------------------
      IF (ZNAK .LT. 0.) WRITE (NK3)(FP(I),I=1,IDIM1)
      BL=MTWR
 15   NITER=NITER+1
      PERVO=0.
      Q1=1.
      CALL TIMER (AP)
      ITER=0	  
!**************************************************
 230  ITER=ITER+1
      MTWR=.TRUE.
!CC
!CC    метод последовательной верхней релаксации
!CC
      CALL POISON
      IF (ITER. LT. ITMI ) GOTO 230
      IF (ITER. LT. ITMA ) THEN 
      IF (.NOT. MTWR) GO TO 230
      ENDIF
      CALL TIMER (PP)
!
!	GAMA=корректирующий множитель
!
      IF ( NITER .LE. 15)GAMA=GAMAS(NITER)
      IF ( NITER .GE. 15)GAMA=GAMAS(15)
       WRITE
     + (*,'(T3,''NITER='',I4,T15,''ITER='',I4,T25,''GAMA='',F6.3,
     + T42,''TIME='',F7.2)') NITER,ITER, GAMA,PP-AP
! ***********************************************
      IF (ISTART .EQ. 0  .AND. NITER .GT. 1)THEN
      DO 250 I=1,IZZ
      UVX(I)=1.D+10
      PTX(I)=1.D+10
      IF ( NAMCAT .EQ. 'CATOD1' )
     + CALL DISTN( U(I-1), V(I-1), U(I), V(I), Y(1), Y(2), DR)
!
      IF ( NAMCAT .EQ. 'CATOD2' ) THEN
	  FO = FHO + (2 * I - 1) * DFO
	  Y(1)=U(I)-DR* COS (FO)
	  Y(2)=V(I)-DR* SIN (FO)
      END IF
!
!	определение потенциала в точке старта
!
      CALL FORCE(POTT,Y(1),Y(2),C1,C2)
      IF (IGMA .EQ. 0 ) GOTO 250
      IF ( POTT.LE.0.) GOTO 250
      VO=SQRT(DABS(POT(-IGM(NUMB))-POTT))
!CC
!CC коррекция тока инжекции
!CC
      UVO=VO*VO*VO
      IF (UVOX(I) .NE. 0.0) UVX(I)=UVO/UVOX(I)
      IF (POTX(I) .NE. 0.0) PTX(I)=POTT/POTX(I)
 250  CONTINUE
      WRITE(*,'(T3,''RATIO OF CURRENT DENCITY'')')
      WRITE(*,'(8F10.2)') (UVX(I),I=1,IZZ)
      WRITE(*,'(T3,''RATIO OF POTENTIAL'')')
      WRITE(*,'(8F10.2)') (PTX(I),I=1,IZZ)
      END IF
! ***********************************************
      REWIND NK1
      WRITE (NK1) NITER,(F(I),I=1,IDIM1)
      IF (NUMB .EQ. 0) RETURN

      IF (ZNAK .GT. 0.) THEN 
!----------------------------------------------
      DO I=1,IDIM1
         FP(I)=0.
      END DO
      ELSE
!----------------------------------------------
          REWIND NK3
	  WRITE (NK3)(FP(I),I=1,IDIM1)
      ENDIF
!----------------------------------------------
      MTWR=BL
      RETURN
 900  STOP 900
      END !SUBROUTINE FPOTE
!----------------------------------------
      SUBROUTINE POISON
!----------------------------------------
CC
CC     расчет сетки потенциала методом верхней релаксации
CC
      IMPLICIT REAL*8 (A-H,O-Z)
      INCLUDE '_PARAM.FOR'
      INCLUDE '_COMMON.FOR'

C     Main program variables

      REAL*8 AP
      DATA EPS /1.D-5/
      CALL TIMER (AP) 
!**************************************************
CC     задание потенциала в нерегулярных узлах
       DO 205 I=2,NK
       N  = IPO(I)
       DER = DELR(I)
       DEZ = DELZ(I)
       IRT=   IR(I)
       IZT=   IZ(I)
       IND=IRT*IZL1+IZT+1
        Q=FP(IND)
        U1=F(IND-1)
        U2=F(IND-IZL1)
        U3=F(IND+1)
 
       IF(IRT .EQ. IRL1-1) U4=F(IND)
       IF(IRT .NE. IRL1-1) U4=F(IND+IZL1)

       IF(N .LE. 0) THEN
        IF( ABS(DER).LT. EPS .AND. ABS(DEZ).LT. EPS) GOTO 80
        AL01=-1.
        AL02=-1.
        AL03=1.
        AL04=1.
        IF( ABS(DER) .GT. 1. )GO TO 60
        AL02=DER
        U2=POT(-N)
        IF(DER.LT.0.)GO TO 60
        AL02=-1.
        U2=F(IND-IZL1)
        AL04=DER
        U4=POT(-N)
 60   IF(  ABS(DEZ) .GT. 1.  )GO TO 70
        AL01=DEZ
        U1=POT(-N)
        IF(DEZ.LT. 0.)GO TO 70
        AL01=-1.
        U1=F(IND-1)
        AL03=DEZ
        U3=POT(-N)
 70     AL31=AL03-AL01
        AL42=AL04-AL02
        AL24=AL04*AL02
        AL13=AL03*AL01
        AL12=AL13*AL24

       IF(IRT .NE. 0)
     + U0=-AL12* (-.5*Q+ (U3*AL01-U1*AL03)/
     + (AL31*AL13 ) + ((1. +AL04*. 5/IRT)*U4*AL02- (1. +
     + AL02*. 5/IRT)*U2*AL04)/(AL42*AL24) )/(AL13+AL24)
       IF(IRT .EQ. 0)
     + U0=-AL12*(-.5*Q+(U3*AL01-U1*AL03)/
     +(AL31*AL13)+4.*U4/(AL04*AL42) )/(AL13+AL13+AL24)
       GO TO 200 
 80   U0=POT(-N)
       GO TO 200
       ELSE
       GO TO (101,102,103,104,105,106,107) ,N 
!
! *101*****************     * * *
!                             *
 101   U0=.25*(U1+U3+2.*U2-Q)
       GO TO 200
!                             *
! *102*****************     * *
!                             *
 102   U0=. 25* ((1. +.5/IRT)*U4+ (1. -.5/IRT)*U2+2.*U1-Q)
       GO TO 200
!                             *
! *103***************         * *
!                             *
 103   U0=0.25*( (1. +.5/IRT)*U4+(1. -.5/IRT)*U2+2.*U3-Q)
       GO TO 200
!                             *
! *104***************       * * *
!
 104   U0=1./6.*(U1+ U3+4.*U4-Q)
       GO TO 200
!                             *
! *105********************    * *
!	
 105   U0=1./6.*(2.*U3+4.*U4-Q)
       GO TO 200
!                             *
! *106***************       * *
!	
 106   U0=1./6.*(2.*U1+4.*U4-Q)
       GO TO 200
!
! *107****************************
!
 107   U0=F(IND)
       END IF
 200	F(IND)=U0
 205	CONTINUE	
CC
CC     вычисление потенциала в регулярных узлах
CC     использовать линейную норму вектора погрешности
CC
      XPR=0.
      DO 220 IBEG=1,2
      DO 210 I=IBEG,IRL1*IZL1,2
      IF (IFD(I) .NE. 0) GOTO 210
      J=(I-1)/IZL1
      SUM=F(I)
      F(I) = .25*(F(I+1)+F(I-1)+(1. + .5/J)*F(I+IZL1)+
     +(1.-.5/J)*F(I-IZL1)-FP(I))
      DELF=F(I)-SUM
      XPR=DMAX1 (XPR, DABS (DELF) )
      F(I)=DELF*W+SUM
 210  CONTINUE
 220  CONTINUE
      IF (XPR .GE. RELE) MTWR=.FALSE.

      RETURN
      END !SUBROUTINE POISON
!----------------------------------------
      SUBROUTINE TIMER (TT)
!----------------------------------------
      REAL*8 TT
      INTEGER*2 IH, IM, IS, I100
	  CALL GETTIM(IH, IM, IS, I100)
      TT=3600.*IH+ 60. * IM + IS +0.01 * I100

      END !SUBROUTINE TIMER
