!CC    A.for - For MS FORTRAN 5.1
!CC
!CC    есть текст программ от 11-10-89
!CC
!----------------------------------------
      SUBROUTINE CATOD1(INZ,YI)
!****  NAMCAT='CATOD1'--инжекция с заданной поверхности катода        ***	  
!*                                                                      *
!*    Input variables:                                                  *
!*                                                                      *
!*  INTEGER INZ  - inp  ! индекс точки эмиссии 
!*                                                                      *
!*    Output variables:                                                 *
!*                                                                      *
!*  REAL*8 YI(5) - out  ! значение координат точки эмиссии, норм.
!*
!*----------------------------------------------------------------------*
!
!     программа готовит координаты инжекции трубок тока
!
      IMPLICIT REAL*8 (A-H,O-Z)
      INCLUDE '_PARAM.FOR'
      INCLUDE '_COMMON.FOR' 
      REAL*8  YI(5)
		Y(1)=UX(INZ)
		Y(2)=VY(INZ)
		IF ( NAMCAT .EQ. 'CATOD1' ) THEN
		   Y(1)=.5*(Y(1)+UX(INZ-1))
		   Y(2)=.5*(Y(2)+VY(INZ-1))
		END IF
		Y(3)=0.
		Y(4)=0.
		Y(5)=0.
      YI(1)=UX(INZ)- (DR*ZNAK)*DCOS(FO)
      YI(2)=VY(INZ)- (DR*ZNAK)*DSIN(FO)
      YI(3)=(-VO)*DCOS(FO)
      YI(4)=(-VO)*DSIN(FO)  
!*=== End of SUBROUTINE CATOD1 =========================================*
      END	  
!----------------------------------------
      SUBROUTINE CATOD2(INZ,YI)
!****  NAMCAT='CATOD2'--инжекция со сферического катода               ***
!*                                                                      *
!*    Input variables:                                                  *
!*                                                                      *
!*  INTEGER INZ  - inp  ! индекс точки эмиссии 
!*                                                                      *
!*    Output variables:                                                 *
!*                                                                      *
!*  REAL*8 YI(5) - out  ! значение координат точки эмиссии, норм.
!*
!*----------------------------------------------------------------------*
      IMPLICIT REAL*8 (A-H,O-Z)
      INCLUDE '_PARAM.FOR'
      INCLUDE '_COMMON.FOR' 	  
      REAL*8  YI(5)
      YI(1)=UX(INZ) - DR* DCOS (FO)
      YI(2)=VY(INZ) - DR* DSIN (FO)
      YI(3)=(-VO)* DCOS (FO)
      YI(4)=(-VO)* DSIN (FO)
*=== End of subroutine CATOD2 =========================================*
      END
!----------------------------------------
      SUBROUTINE DISTN( Z1, R1, Z2, R2, ZET, RHO, DIST)
!----------------------------------------
      IMPLICIT REAL*8 (A-H,O-Z)
		ABSDZ = DABS(Z2-Z1) 
		ABSDR = DABS(R2-R1) 

      IF ( ABSDZ .LT. 0.001 )  THEN
       IDZ=0
      ELSE
       IDZ=(Z2-Z1)/ABSDZ
      ENDIF
	  
      IF ( ABSDR .LT. 0.001 )  THEN
       IDR=0
      ELSE
       IDR=(R2-R1)/ABSDR
      ENDIF

	  IF ( IDZ .EQ. 0 )  THEN     
        ZET=Z1- DIST* IDR
        RHO=(R2+ R1) * 0.5
      ELSE
        A1=(R2-R1)/(Z2-Z1)
        Z3=(Z2+ Z1) * 0.5
	    R3=(R2+ R1) * 0.5
		COSIN = 1. /  DSQRT(1. + A1* A1)
		SINUS = COSIN * A1
        ZET=Z3- DIST* IDZ *SINUS
        RHO=R3+ DIST* IDZ *COSIN
      ENDIF
*=== End of SUBROUTINE DISTN =========================================*
      END
!----------------------------------------
      SUBROUTINE FKMAG(I,FK)
**** calculate finite difenition ***
*  INTEGER IZ   - inp  ! индекс ближайшего сет .узла 
*  REAL*8 FK(6) - out  ! значение производных МИ, норм.
!----------------------------------------
      IMPLICIT REAL*8 (A-H,O-Z)
      INCLUDE '_PARAM.FOR'
      INCLUDE '_COMMON.FOR' 
      REAL*8 FK(6)

! **** calculate vector potential at catod ***
      FK(1)=(BZA(I+1)-BZA(I-1))*.5 
      FK(2)=(BZA(I+2)-2.*BZA(I)+BZA(I-2))*.25 
      IF (MAGORD .LT. 4) GOTO 99
      FK(3)=(BZA(I+3)-6.*FK(1)-BZA(I-3))*.5 
      FK(4)=(BZA(I+4)-16.*FK(2)-2.*BZA(I)+BZA(I-4))*.0625 
      IF (MAGORD .LT. 6) GOTO 99
      FK(5)=(BZA(I+5)-40.*FK(3)-10.*FK(1)-BZA(I-5))*.03125 
      FK(6)=(BZA(I+6)-96.*FK(4)-36.*FK(2)-2.*BZA(I)+BZA(I-6))*.015625 
 99   CONTINUE
*=== End of  SUBROUTINE FKMAG =========================================*
      END	 
!----------------------------------------
      SUBROUTINE FLUN56
!----------------------------------------
      REAL A(8)
      REWIND 5
      REWIND 6
	      READ (5) A(1),A(2)
	      WRITE(6) A(1),A(2)
      DO 10 I=1,40
	      READ (5) A
	      WRITE(6) A
 10    CONTINUE
*=== End of subroutine  FLUN56 =========================================*
      END
!----------------------------------------
      SUBROUTINE FORCE (X,Y1,Y2,C1,C2)
*                                                                      *
*    Input variables:                                                  *
*                                                                      *
*      REAL*8 Y1  - inp  ! продольная координата точки.
*      REAL*8 Y2  - inp  ! поперечная координата точки.
*                                                                      *
*    Output variables:                                                 *
*                                                                      *
!----------------------------------------
      IMPLICIT REAL*8 (A-H,O-Z)
      INCLUDE '_PARAM.FOR'
      INCLUDE '_COMMON.FOR' 

!**** проверка выхода координат за границы прямоугольной области ***
! ***********************************************
      IF ( DABS(Y2) .GE. YV(IRLIM1-1).OR. Y1 .LT. XU(0)
     + .OR. Y1 .GE. XU(IZLIM1-1)) GOTO 1111
! ***********************************************
       R=Y2
      IF(Y2 .LT. 0.)R=-R
       IZT=Y1-XU(0)
       IRT=R-YV(0)
       IND0=IZT+IZLIM1*IRT+1
       IND1=IND0+IZLIM1
       IND2=IND0+1
       IND3=IND0+IZLIM1+1
      IF (IFD(IND0) .EQ. 10 .AND. IFD(IND1) .EQ. 10 
     +.AND. IFD(IND2) .EQ. 10 .AND. IFD(IND3) .EQ. 10) GOTO 1111
      X1=Y1-XU(0)-IZT
      X2=R -YV(0)-IRT
! ***********************************************
	  A0=F(IND0)
	  A1=F(IND1)-A0
	  A2=F(IND2)-A0
	  A3=F(IND3)-A0-A1-A2
	  C1=A2+A3*X2
	  C2=A1+A3*X1
      X=A0+A2*X1+A1*X2
      IGMA=1
      RETURN
! ***********************************************
 1000 CONTINUE
      DZ=Y1-XU(0)-IZT
      DR=R -YV(0)-IRT
	U0=F(IND0)
	UE=F(IND0+1)
	UW=F(IND0-1)
	UN=F(IND0+IZLIM1)
 1111 IGMA=0
      RETURN
!*=== End of subroutine  FORCE =========================================*
      END
!----------------------------------------
      SUBROUTINE FORCM (AO,Y1,Y2,BZ,BR)
*                                                                      *
*    Input variables:                                                  *
*                                                                      *
*      REAL*8 Y1  - inp  ! продольная координата точки.
*      REAL*8 Y2  - inp  ! поперечная координата точки.
*                                                                      *
*    Output variables:                                                 *
*                                                                      *
*      REAL*8 AO  - out  ! угловая компонента ВМП, 
*      REAL*8 BZ  - out  ! продольная компонент МИ,
*      REAL*8 BR  - out  ! поперечная компонент МИ,
!----------------------------------------
      IMPLICIT REAL*8 (A-H,O-Z)
      INCLUDE '_PARAM.FOR'
      INCLUDE '_COMMON.FOR' 
      REAL*8 A(6)

! ***********************************************
      IF ( DABS(Y2) .GE. YV(IRLIM1-1).OR. Y1 .LT. XU(0)
     + .OR. Y1 .GE. XU(IZLIM1-1)) GOTO 1111
! ***********************************************
      R=Y2
      R2=R*R
      IZT=IDINT(Y1-XU(0)+0.5)
      X1=Y1-XU(0)-DFLOAT(IZT)
! **** calculate finite difenition ***
!*  INTEGER IZT - inp  ! индекс ближайшего сет .узла 
!*  REAL*8 A(6)   - out  ! значение производных МИ, норм.
       CALL FKMAG(IZT,A)
	B0=BZA(IZT)+X1*(A(1)+X1/2.*(A(2)+X1/3.*(A(3)+X1/4.*(A(4)+
     +X1/5.*(A(5)+X1/6.*A(6))))))
	    B1=A(1)+X1*(A(2)+X1/2.*(A(3)+X1/3.*(A(4)+X1/4.*(A(5)+
     + X1/5.*A(6)))))
	    B2=A(2)+X1*(A(3)+X1/2.*(A(4)+X1/3.*(A(5)+X1/4.*A(6))))
	    B3=A(3)+X1*(A(4)+X1/2.*(A(5)+X1/3.*A(6)))
	    B4=A(4)+X1*(A(5)+X1/2.* A(6))
	    B5=A(5)+X1* A(6)
      AOL=B2-R2/24.*(B4-R2/48.*A(6)) 
      BDBL=B2-R2/16.*(B4-R2/36.*A(6)) 
      BSLOPE=B1-R2/8.*(B3-R2/24.*B5) 
! ***********************************************
      BR=-0.5*R*BSLOPE 
      BZ=B0-R2/4.*BDBL 
      AO=0.5*R*(B0-R2/8.*AOL) 
      IGMA=1
      RETURN
 1111 IGMA=0
      RETURN
!*=== End of subroutine FORCM =========================================*
      END
