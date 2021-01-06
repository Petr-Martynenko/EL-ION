!CC    C.FOR MS FORTRAN 5.1
!CC
!CC    11.10.89
!----------------------------------------
      SUBROUTINE START
!----------------------------------------
!CC
!CC     программа готовит начальные координаты
!CC     инжекции трубок тока с заданной поверхности
!CC     печатает значения параметров задачи
!CC
      IMPLICIT REAL*8 (A-H,O-Z)
      INCLUDE '_PARAM.FOR'
      INCLUDE '_COMMON.FOR'
 

      REAL*8 X0     !продольная координата
      REAL*8 Y0     !поперечная координата
      REAL*8 DELTAX,DELTAY

      WRITE (*,'(T6,''Number points of contour NK='',I3)') NK
      WRITE (*,'(/T3,''FGUN'',T18,''DR'',T28,''MI'',
     + T38,''MZ'',T46,''NAME'',T56,''NUMB'',T66,''ISTART''/
     + 1X,A6,F10.5,2I10,4X,A6,2I10/
     +'' C-ARRAY of ELECTRIC POTENTIAL''/10F8.2)')
     + FGUN, DR, MI, M4, NAME, NUMB, ISTART, C
      IF (ZNAK.GT.0.) THEN
         PMA = 1.E+6*EPS0*SQRT(2.*DEL) 
!*  BN   = нормировка магнитной индукции, Wb/m/m
	 BN = SQRT(2.*FG/DEL)/Z0
      ELSEIF (ZNAK.LT.0.) THEN
         PMA = 1.E+6*EPS0*SQRT(2.*DIO/40.)!40-атомная масса аргона
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
       N=IEX(I)
       R2=BDR(I+1)
       Z2=BDZ(I+1)
!
!	определение координат цетра окружности X0,Y0
!
	   CALL CORCR(Z1,R1,Z2,R2,XOK(IOK),YOK(IOK),ROK(IOK))
	   RH = DABS(ROK(IOK))
	   X0=XOK(IOK)
	   Y0=YOK(IOK)
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
	  U(I)=X0+ RH * DCOS (FO)
	  V(I)=Y0+ RH * DSIN (FO)
 30   CONTINUE	  
	  NAMCAT='CATOD2'
 50   AL1=PI*2.	
      NUMB = ABS(NUMB) 
      WRITE 
     + (*,'(T8,''PJA'',T18,''PMA'',T26,''BETA1'',T36,''BETA2'',T49,
     + ''FG'',T55,''ZSCHIF'',T67,''STEP'',T75,''NAMCAT'',/2F10.3,2F10.5,
     + 3F10.3,A10)') PJA, PMA, BETA1, BETA2, FG, ZSCHIF, STEP, NAMCAT
 	   IF (NAMCAT .EQ. 'CATOD1') IBEG=0
 	   IF (NAMCAT .EQ. 'CATOD2') IBEG=1
      WRITE (*, '(T3,''продольная координата точек эмиссии''/(8F10.3))')
     & (U(I), I=IBEG,IZZ)
      WRITE (*, '(T3,''поперечная координата точек эмиссии''/(8F10.3))')
     & (V(I), I=IBEG,IZZ)
      IF (NAMCAT .EQ. 'CATOD2') THEN
      WRITE (*, '(T3,''площадь сегмента эмиссии''/(8F10.3))')
     & (AL1*AL0*V(I),
     & I=IBEG,IZZ)
      WRITE (*, '(T3,
     & ''угол наклона, координата точек и площадь сегмента эмиссии'')')
      DO 777 I=0,IZZ
 	   IF (I .EQ. 0) THEN
	     FO = FHO + 2 * I * DFO
	     UXI = X0+ RH * DCOS (FO)
	     VYI = Y0+ RH * DSIN (FO)
             WRITE (*,'(I4,3F10.3)') I, FO, UXI, VYI
           END IF
 	   IF (I .NE. 0) THEN 
	     FO = FHO + (2 * I - 1) * DFO
	     UXI = X0+ RH * DCOS (FO)
	     VYI = Y0+ RH * DSIN (FO)
 	     SQI = AL1*AL0*(Y0+ RH * DSIN (FO)) 	   
             WRITE (*,'(I4,4F10.3)') I, FO, UXI, VYI, SQI
	     FO = FHO + 2 * I * DFO
	     UXI = X0+ RH * DCOS (FO)
	     VYI = Y0+ RH * DSIN (FO)
             WRITE (*,'(I4,3F10.3)') I, FO, UXI, VYI
           END IF
 777  CONTINUE	  
      END IF
      WRITE (*,'(T8,''X0'',T18,''Y0'',T28,''RH'',T38,''FHO'',
     + T48,''FHF'',T58,''DFO'',T68,''AL0'',T75,''SQUARE''/8F10.3)')
     + X0, Y0, RH, FHO, FHF, DFO, AL0,
     & AL1*RH*Y0*(FHF - FHO) -AL1*RH*RH*(DCOS(FHF)-DCOS(FHO))
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
!*  учитывает положение центра окружности *
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
