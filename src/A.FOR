CC    A.FOR  Compaq Visual FORTRAN 6.6.0
CC
CC    есть текст программ от 11-10-89
CC
!----------------------------------------
      FUNCTION TETA_N (AO,Y1,Y2,PCIMG,BZ,BR)
!----------------------------------------
      IMPLICIT NONE
CC
CC FUNCTION TETA_N определяет по заданным координатам
CC компоненты осесимметричного магнитного поля (известны
CC координаты и параметры магнитных катушек)
CC и угловую скорость вращения
      REAL*8 TETA_N  ! угловая скорость вращения
      REAL*8 AO
      REAL*8 Y1
      REAL*8 Y2
      REAL*8 BZ
      REAL*8 BR
      REAL*8 PCIMG    !
!----------------------------------------
*      REAL*8 AO  - out  ! угловая компонента ВМП,норм.
*      REAL*8 Y1  - inp  ! продольная к-та точки,норм.
*      REAL*8 Y2  - inp  ! поперечная к-та точки,норм.
*      REAL*8 BZ  - out  ! продольная компонент МИ, норм.
*      REAL*8 BR  - out  ! поперечная компонент МИ, норм.
	   CALL FORMAG (AO,Y1,Y2,BZ,BR)
CC
CC значение TETA_N не учитывает знак заряда,т.е. + и - 
CC вращаются с одной угловой скоростью
CC
	   TETA_N=(Y2*AO-PCIMG)/(Y2*Y2)
C *** END FUNCTION TETA_N ***
      END
!----------------------------------------
      SUBROUTINE CATOD1(INZ,YI)
****  NAMCAT='CATOD1'--инжекция с катода *** 
*  INTEGER INZ  - inp  ! индекс точки эмиссии 
*  REAL*8 YI(5) - out  ! значение координат точки эмиссии, норм.
!----------------------------------------
CC
CC     программа готовит координаты инжекции трубок тока
CC     с заданной поверхности
CC
      IMPLICIT REAL*8 (A-H,O-Z)
      INCLUDE '_PARAM.FOR'
      INCLUDE '_COMON.FOR'

C     Main program variables

      REAL*8 YI(5)
      YI(1)=UX(INZ)- (DR*ZNAK)* DCOS (FO)
      YI(2)=VY(INZ)- (DR*ZNAK)* DSIN (FO)
      YI(3)=(-VO)* DCOS (FO)
      YI(4)=(-VO)* DSIN (FO)
      END !CATOD1	  
!----------------------------------------
      SUBROUTINE CATOD2(INZ,YI)
*  INTEGER INZ  - inp  ! индекс точки эмиссии 
*  REAL*8 YI(5) - out  ! значение координат точки эмиссии, норм.
!----------------------------------------
CC     программа готовит координаты инжекции трубок тока
CC     со сферического катода
CC
      IMPLICIT REAL*8 (A-H,O-Z)
      INCLUDE '_PARAM.FOR'
      INCLUDE '_COMON.FOR'
	  
CC     Main program variables

      REAL*8  YI(5)
      YI(1)=UX(INZ) - DR* DCOS (FO)
      YI(2)=VY(INZ) - DR* DSIN (FO)
      YI(3)=(-VO)* DCOS (FO)
      YI(4)=(-VO)* DSIN (FO)
      END !CATOD2	  
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
      END !DISTN	  
!----------------------------------------
      SUBROUTINE FKMAG(I,FK)
C **** calculate finite difenition ***
*  INTEGER IZ   - inp  ! индекс ближайшего сет .узла 
*  REAL*8 FK(6) - out  ! значение производных МИ, норм.
!----------------------------------------
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 FK(6)
      INCLUDE '_PARAM.FOR'
      INCLUDE '_COMON.FOR'

C **** calculate vector potential at catod ***
      FK(1)=(BZA(I+1)-BZA(I-1))*.5
      FK(2)=(BZA(I+2)-2.*BZA(I)+BZA(I-2))*.25
      IF (MAGORD .LT. 4) GOTO 99
      FK(3)=(BZA(I+3)-6.*FK(1)-BZA(I-3))*.5
      FK(4)=(BZA(I+4)-16.*FK(2)-2.*BZA(I)+BZA(I-4))*.0625
      IF (MAGORD .LT. 6) GOTO 99
      FK(5)=(BZA(I+5)-40.*FK(3)-10.*FK(1)-BZA(I-5))*.03125
      FK(6)=(BZA(I+6)-96.*FK(4)-36.*FK(2)-2.*BZA(I)+BZA(I-6))*.015625
 99   CONTINUE
      END !FKMAG	 
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
      END !FLUN56
!----------------------------------------
      SUBROUTINE FORCE (X,Y1,Y2,C1,C2)
!----------------------------------------
      IMPLICIT REAL*8 (A-H,O-Z)
      INCLUDE '_PARAM.FOR'
      INCLUDE '_COMON.FOR'
C **** проверка выхода координат за границы прямоугольной области ***
C ***********************************************
      IF ( DABS(Y2) .GE. YV(IRL1-1).OR. Y1 .LT. XU(0)
     + .OR. Y1 .GE. XU(IZL1-1)) GOTO 1111
C ***********************************************
       R=Y2
      IF(Y2 .LT. 0.)R=-R
       IZT=Y1-XU(0)
       IRT=R-YV(0)
       IND0=IZT+IZL1*IRT+1
       IND1=IND0+IZL1
       IND2=IND0+1
       IND3=IND0+IZL1+1
      IF (IFD(IND0) .EQ. 10 .AND. IFD(IND1) .EQ. 10 
     +.AND. IFD(IND2) .EQ. 10 .AND. IFD(IND3) .EQ. 10) GOTO 1111
      X1=Y1-XU(0)-IZT
      X2=R -YV(0)-IRT
      IND=IZT+IZL1*IRT+1
C ***********************************************
	  A0=F(IND0)
	  A1=F(IND1)-A0
	  A2=F(IND2)-A0
	  A3=F(IND3)-A0-A1-A2
	  C1=A2+A3*X2
	  C2=A1+A3*X1
      X=A0+A2*X1+A1*X2
      IGMA=1
      RETURN
C ***********************************************
 1000 CONTINUE
      DZ=Y1-XU(0)-IZT
      DR=R -YV(0)-IRT
	U0=F(IND)
	UE=F(IND+1)
	UW=F(IND-1)
	UN=F(IND+IZL1)
	  C1=(A2+A3*X2)*.5
	  C2=(A1+A3*X1)*.5
      X=A0+A2*X1+A1*X2
 1111 IGMA=0
      RETURN
      END !FORCE
!----------------------------------------
      SUBROUTINE FORCM (AO,Y1,Y2,BZ,BR)
*      REAL*8 AO  - out  ! угловая компонента ВМП, 
*      REAL*8 Y1  - inp  ! продольная координата точки.
*      REAL*8 Y2  - inp  ! поперечная координата точки.
*      REAL*8 BZ  - out  ! продольная компонент МИ,
*      REAL*8 BR  - out  ! поперечная компонент МИ,
!----------------------------------------
      IMPLICIT REAL*8 (A-H,O-Z)
      INCLUDE '_PARAM.FOR'
      INCLUDE '_COMON.FOR'
C **** проверка выхода координат за границы прямоугольной области ***
C ***********************************************
      IF ( DABS(Y2) .GE. YV(IRL1-1).OR. Y1 .LT. XU(0)
     + .OR. Y1 .GE. XU(IZL1-1)) GOTO 1111
C ***********************************************
	   Z=Y1
	   R=Y2
*      REAL*8 AO  - out  ! угловая компонента ВМП, TL*м
*      REAL*8 Y1  - inp  ! продольная координата точки .м
*      REAL*8 Y2  - inp  ! поперечная координата точки .м
*      REAL*8 BZ  - out  ! продольная компонент МИ, TL
*      REAL*8 BR  - out  ! поперечная компонент МИ, TL
	   CALL FORMAG (AO,Z,R,BZ,BR)
      IGMA=1
      RETURN
 1111 IGMA=0
      RETURN
      END !FORCM
!Sets the current language, country, or codepage
      Subroutine RusConsole
! Build as QuickWin or Standard Graphics
      use dflib
! Set the x & y pixels to 800X600 and fontname
      TYPE (windowconfig) wc
      LOGICAL             statusmode /.FALSE./
      wc%numxpixels  = 1200
      wc%numypixels  = 9999
      wc%extendfontname = "Courier"C
      wc%title="ТЕКСТОВОЕ ОКНО ВЫВОДА"C
      wc%fontsize = QWIN$EXTENDFONT
      wc%mode = QWIN$SCROLLDOWN
      do while(SETWINDOWCONFIG(wc) .EQ. statusmode)
      end do
!
      End Subroutine RusConsole 
      function ru_dos(string)
      character(*), intent(in) :: string
      character (len(string)) :: ru_dos, ru_temp
      integer dos_win_code, dif
      ru_temp=string
      l=len_trim(ru_temp)
      do i=1,l
        dos_win_code=iachar(ru_temp(i:i))
        dif=0
        select case (dos_win_code)
        case (#C0:#EF)
            dif=#40
        case (#F0:#FF)
            dif=#10
        case (#A8)
            dif=-#48
        case (#B8)
            dif=-#39
        case (#B9)
            dif=-#43
        end select
        if (dif/=0) ru_temp(i:i)=char(dos_win_code-dif)
      end do
      ru_dos=ru_temp
      end function ru_dos