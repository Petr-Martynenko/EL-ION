!CC    C.FOR MS FORTRAN 5.1
!CC
!CC    11.10.89
!----------------------------------------
      SUBROUTINE START
!----------------------------------------
!CC
!CC     программа готовит начальные координаты
!CC     инжекции трубок тока с заданной поверхности
!CC     печатает значени€ параметров задачи
!CC
      IMPLICIT REAL*8 (A-H,O-Z)
      INCLUDE '_PARAM.FOR'
      INCLUDE '_COMMON.FOR'
 

      REAL*8 X0     !продольна€ координата
      REAL*8 Y0     !поперечна€ координата
      REAL*8 DELTAX,DELTAY
      double precision xs(N4+1), ys(N4+1)
      double precision arg, Bse(N4+1), Cse(N4+1), Dse(N4+1), seval  

      WRITE (*,'(T6,''Number points of contour NK='',I3)') NK
      WRITE (*,'(/T3,''FGUN'',T17,''DR'',T27,''MI'',T37,''MZ'',T45,
     + ''NAME'',T56,''NUMB'',T66,''ISTART''/
     + 1X,A6,F10.5,2I10,4X,A6,2I10/
     +'' C-ARRAY of ELECTRIC POTENTIAL''/10F8.2)')
     + FGUN, DR, MI, M4, NAME, NUMB, ISTART, C
      IF (ZNAK.GT.0.) THEN
         PMA = 1.E+6*EPS0*SQRT(2.*DEL) 
!*  BN   = нормировка магнитной индукции, Wb/m/m
	 BN = SQRT(2.*FG/DEL)/Z0
      ELSEIF (ZNAK.LT.0.) THEN
         PMA = 1.E+6*EPS0*SQRT(2.*DIO/40.)!40-атомна€ масса аргона
         BN = SQRT(2.*FG/DIO/40.)/Z0
      ELSE
         STOP '*******ZNAK=0.'
      END IF
	  PJA= 4./9./DR/DR
	  BETA2=2.*DEL/VC0*FG/VC0
	  BETA1=0.25*BETA2
	  NAMCAT='CATOD1'
	  IF (NUMB.LE.0) GOTO 50
	  IOK=0
      DO 10 I=1,ABS(MS)
	   IF (IEX(I) .NE. 0) IOK=IOK+1  
 	   IF (I .EQ. NUMB) THEN
       R1=BDR(I)
       Z1=BDZ(I)
       R2=BDR(I+1)
       Z2=BDZ(I+1)
!
!	определение координат цетра окружности X0,Y0
!
	   CALL CORCR(Z1,R1,Z2,R2,X0,Y0,ROK(IOK))
	   RH = DABS(ROK(IOK))
!	   X0=XOK(IOK)
!	   Y0=YOK(IOK)
!
! определение углов наклона радиуса в начальной
! и конечной точках дуги окружности
!
	   DELTAX=Z1-X0
	   DELTAY=R1-Y0
	   FHO = DATAN2(DELTAY,DELTAX)
	   DELTAX=Z2-X0
	   DELTAY=R2-Y0
	   FHF = DATAN2(DELTAY,DELTAX)
	   GOTO 20
       END IF
 10   CONTINUE
 20   CONTINUE
      DFO = (FHF - FHO) / 2. / IZZ
	  AL0 = RH*DFO*2.
      DO 30 I=1,IZZ
	  FO = FHO + (2 * I - 1) * DFO
	  UX(I)=X0+ RH * DCOS (FO)
	  VY(I)=Y0+ RH * DSIN (FO)
 30   CONTINUE	  
!   ****  NAMCAT='CATOD2'--инжекци€ со сферического катода
	  NAMCAT='CATOD2'
 50   AL1=PI*2.	
      NUMB = ABS(NUMB) 
      WRITE 
     + (*,'(T8,''PJA'',T18,''PMA'',T26,''BETA1'',T36,''BETA2'',T48,
     + ''FG'',T55,''ZSCHIF'',T67,''STEP'',T75,''NAMCAT'',/2F10.3,2F10.5,
     + 3F10.3,A10)') PJA, PMA, BETA1, BETA2, FG, ZSCHIF, STEP, NAMCAT
 	   IF (NAMCAT .EQ. 'CATOD1') IBEG=0
 	   IF (NAMCAT .EQ. 'CATOD2') IBEG=1
      WRITE (*, '(T3,''продольна€ координата точек эмиссии''/(8F10.3))')
     & (UX(I), I=IBEG,IZZ)
      WRITE (*, '(T3,''поперечна€ координата точек эмиссии''/(8F10.3))')
     & (VY(I), I=IBEG,IZZ)
!   ****  NAMCAT='CATOD2'--инжекци€ со сферического катода
      IF (NAMCAT .EQ. 'CATOD2') THEN
      WRITE (*, '(T3,''площадь сегмента эмиссии''/(8F10.3))')
     & (AL1*AL0*VY(I),
     & I=IBEG,IZZ)
      WRITE (*, '(T3,''длина окружности сегмента эмиссии''/(8F10.3))')
     & (AL1*VY(I),
     & I=IBEG,IZZ)
      WRITE (*, '(T2,''угол наклона'',
     &  T16,''координата точек и площадь сегмента эмиссии'')')
	  IOK=0
      SQI = 0D0 	   
      DO 777 I=0,IZZ
 	   IF (I .EQ. 0) THEN
	     FO  = FHO + 2 * I * DFO
	     UXI = X0+ RH * DCOS (FO)
	     VYI = Y0+ RH * DSIN (FO)
             WRITE (*,'(I4,3F10.4)') I, FO, UXI, VYI
       ELSEIF (I .NE. 0) THEN
	     FO  = FHO + (2 * I - 1) * DFO
	     UXI = X0+ RH * DCOS (FO)
	     VYI = Y0+ RH * DSIN (FO)
 	     SQI = SQI + AL1*AL0*(Y0+ RH * DSIN (FO)) 	   
             WRITE (*,'(I4,3F10.4,F10.3)')
     &   I, FO, UXI, VYI, AL1*AL0*(Y0+ RH * DSIN (FO))
	     FO  = FHO + 2 * I * DFO
	     UXI = X0+ RH * DCOS (FO)
	     VYI = Y0+ RH * DSIN (FO)
             WRITE (*,'(I4,3F10.4)') I, FO, UXI, VYI
       ELSE
         STOP '*******I<0'
           END IF
	  IOK=IOK+1
	  ys(IOK)=VYI
	  xs(IOK)=FO  
 777  CONTINUE	  
	call dspline (IOK, xs, ys, bse, cse, dse)    
	write (*,FMT='('' The spline interpolation Npoint='',I3)')N4
	DO I=1,N4
        arg=FHO+(FHF - FHO)*(I-1)/(N4-1)
        write (*,*) arg,seval(IOK, arg, xs, ys, bse, cse, dse),I
	endDO    
      END IF
      write (*,'(T8,''X0'',T18,''Y0'',T24,''RADIUS'',T38,''FHO'',T48,
     + ''FHF'',T58,''DFO'',T68,''AL0'',T75,''SQUARE''/8F10.3)')
     + X0, Y0, RH, FHO, FHF, DFO, AL0, SQI
      write (*, '(T2,''Ёмиссионна€ площадь катода (кв.м) SQUARE=''
     &   F10.4)') SQI*Z0*Z0
      PAUSE
!*=== End of subroutine  START =========================================*
      END !SUBROUTINE START
!----------------------------------------
      SUBROUTINE CORCR( Z1, R1, Z2, R2, ZCR, RCR, RADIUS)
!*                                        *
!*     определение параметров окружности: *
!*                                        *
!*                                        *
!*    Input variables:                    *
!*                                        *
!*          Z1 = координата начальной точк*
!*          R1 =                          *
!*          Z2 = координата конечной точки*
!*          R2 =                          *
!*          RADIUS = радиус окружности    *
!*  учитывает направлени€ обхода по дуге окружности*
!
!	RADIUS=+1. против часовой стрелки
!	RADIUS=-1. по часовой стрелке
!
!*                                        *
!*    Output variables:                   *
!*                                        *
!*          ZCR =                         *
!*          RCR =                         *
!----------------------------------------  
      IMPLICIT REAL*8 (A-H,O-Z)
		DL2 = (Z2-Z1)**2+(R2-R1)**2
		DLE = DSQRT(DL2)
		COSIN = (Z2-Z1) / DLE
		SINUS = (R2-R1) / DLE
		DL = DLE / 2
		H = DSQRT(RADIUS**2-DL**2)
		ZCR = (Z2+ Z1) / 2 - H * SINUS * RADIUS / DABS(RADIUS)
		RCR = (R2+ R1) / 2 + H * COSIN * RADIUS / DABS(RADIUS)		
!*=== End of SUBROUTINE CORCR =========================================*
      END !SUBROUTINE CORCR
!****************************************************************************
!
!  SUBPROGRAM: dspline   {?????????? ???????????????? ??????}
!
!  PURPOSE:  {?????????? ????????????? ???????}.
!
!****************************************************************************

      subroutine dspline (n, x, y, b, c, d)
      integer n
      double precision x(n), y(n), b(n), c(n), d(n)
!
!  the coefficients b(i), c(i), and d(i), i=1,2,...,n are computed
!  for a cubic interpolating spline
!
!    s(x) = y(i) + b(i)*(x-x(i)) + c(i)*(x-x(i))**2 + d(i)*(x-x(i))**3
!
!    for  x(i) .le. x .le. x(i+1)
!
!  input..
!
!    n = the number of data points or knots (n.ge.2)
!    x = the abscissas of the knots in strictly increasing order
!    y = the ordinates of the knots
!
!  output..
!
!    b, c, d  = arrays of spline coefficients as defined above.
!
!  using  p  to denote differentiation,
!
!    y(i) = s(x(i))
!    b(i) = sp(x(i))
!    c(i) = spp(x(i))/2
!    d(i) = sppp(x(i))/6  (derivative from the right)
!
!  the accompanying function subprogram  seval  can be used
!  to evaluate the spline.
!

      integer nm1, ib, i
      double precision t

      nm1 = n-1
      if ( n .lt. 2 ) return
      if ( n .lt. 3 ) go to 50
!
!  set up tridiagonal system
!
!  b = diagonal, d = offdiagonal, c = right hand side.
!
      d(1) = x(2) - x(1)
      c(2) = (y(2) - y(1))/d(1)
      do 10 i = 2, nm1
         d(i) = x(i+1) - x(i)
         b(i) = 2.*(d(i-1) + d(i))
         c(i+1) = (y(i+1) - y(i))/d(i)
         c(i) = c(i+1) - c(i)
   10 continue
!
!  end conditions.  third derivatives at  x(1)  and  x(n)
!  obtained from divided differences
!
      b(1) = -d(1)
      b(n) = -d(n-1)
      c(1) = 0.
      c(n) = 0.
      if ( n .eq. 3 ) go to 15
      c(1) = c(3)/(x(4)-x(2)) - c(2)/(x(3)-x(1))
      c(n) = c(n-1)/(x(n)-x(n-2)) - c(n-2)/(x(n-1)-x(n-3))
      c(1) = c(1)*d(1)**2/(x(4)-x(1))
      c(n) = -c(n)*d(n-1)**2/(x(n)-x(n-3))
!
!  forward elimination
!
   15 do 20 i = 2, n
         t = d(i-1)/b(i-1)
         b(i) = b(i) - t*d(i-1)
         c(i) = c(i) - t*c(i-1)
   20 continue
!
!  back substitution
!
      c(n) = c(n)/b(n)
      do 30 ib = 1, nm1
         i = n-ib
         c(i) = (c(i) - d(i)*c(i+1))/b(i)
   30 continue
!
!  c(i) is now the sigma(i) of the text
!
!  compute polynomial coefficients
!
      b(n) = (y(n) - y(nm1))/d(nm1) + d(nm1)*(c(nm1) + 2.*c(n))
      do 40 i = 1, nm1
         b(i) = (y(i+1) - y(i))/d(i) - d(i)*(c(i+1) + 2.*c(i))
         d(i) = (c(i+1) - c(i))/d(i)
         c(i) = 3.*c(i)
   40 continue
      c(n) = 3.*c(n)
      d(n) = d(n-1)
      return

   50 b(1) = (y(2)-y(1))/(x(2)-x(1))
      c(1) = 0.
      d(1) = 0.
      b(2) = b(1)
      c(2) = 0.
      d(2) = 0.
      return
      end
!****************************************************************************
!
!  function: seval
!
!  PURPOSE:  {?????????? ???????}.
!
!****************************************************************************

      double precision function seval(n, u, x, y, b, c, d)
      integer n
      double precision  u, x(n), y(n), b(n), c(n), d(n)
!
!  this subroutine evaluates the cubic spline function
!
!    seval = y(i) + b(i)*(u-x(i)) + c(i)*(u-x(i))**2 + d(i)*(u-x(i))**3
!
!    where  x(i) .lt. u .lt. x(i+1), using horner's rule
!
!  if  u .lt. x(1) then  i = 1  is used.
!  if  u .ge. x(n) then  i = n  is used.
!
!  input..
!
!    n = the number of data points
!    u = the abscissa at which the spline is to be evaluated
!    x,y = the arrays of data abscissas and ordinates
!    b,c,d = arrays of spline coefficients computed by spline
!
!  if  u  is not in the same interval as the previous call, then a
!  binary search is performed to determine the proper interval.
!
      integer i, j, k
      double precision dx
      data i/1/
      if ( i .ge. n ) i = 1
      if ( u .lt. x(i) ) go to 10
      if ( u .le. x(i+1) ) go to 30
!
!  binary search
!
   10 i = 1
      j = n+1
   20 k = (i+j)/2
      if ( u .lt. x(k) ) j = k
      if ( u .ge. x(k) ) i = k
      if ( j .gt. i+1 ) go to 20
!
!  evaluate spline
!
   30 dx = u - x(i)
      seval = y(i) + dx*(b(i) + dx*(c(i) + dx*d(i)))
      return
      end	  
