CC    C.FOR Compaq Visual FORTRAN 6.6.0
CC
CC    11.10.89
!----------------------------------------
      SUBROUTINE START
!----------------------------------------
CC
CC     программа готовит начальные координаты
CC     инжекции трубок тока с заданной поверхности
CC     печатает значения параметров задачи
CC
      IMPLICIT REAL*8 (A-H,O-Z)
      INCLUDE '_PARAM.FOR'
      INCLUDE '_COMON.FOR'

C     Main program variables

      INTEGER     NOUT/0/
      REAL*8 ANGL1,ANGL2,DELTAX,DELTAY
      REAL*8 X0    !продольная координата XCN
      REAL*8 Y0    !поперечная координата YCN
      REAL*8 UXI
      REAL*8 VYI
	  
      OPEN  (UNIT=NOUT,FILE='CHECKERR.TXT',POSITION='APPEND')
      write (NOUT,'(/T3,''Number points of contour NK='',I3)') NK
      write (NOUT,'(/T3,''FGUN'',T18,''DR'',T28,''MI'',
     & T38,''MZ'',T46,''NAME'',T56,''NUMB'',T66,''ISTART''/
     & 1X,A6,F10.5,2I10,4X,A6,2I10/
     &'' C-ARRAY of ELECTRIC POTENTIAL''/10F8.2)')
     & FGUN, DR, MI, MZ, NAME, NUMB, ISTART, C
      write (NOUT,
     & '(/T3,''NORMA of ELECTRIC POTENTIAL ='',F11.3,'' (V)'')') FG
      IF (ZNAK.GT.0.) THEN
         PMA = 1.E+6*EPS0*DSQRT(2.*DEL) 
*  BN   = нормировка магнитной индукции, Wb/m/m
	 BN = SQRT(2.*FG/DEL)/Z0
      ELSEIF (ZNAK.LT.0.) THEN
         PMA = 1.E+6*EPS0*DSQRT(2.*DIO/40.)     !ионы К+
!         PMA = 1.E+6*EPS0*DSQRT(2.*DIO/28.)     !молекулярный азот
         BN = SQRT(2.*FG/DIO/28.)/Z0
      ELSE
         STOP '*******ZNAK=0.'
      END IF
	  PJA= 4./9./DR/DR
	  BETA2=2.*DEL/VC0*FG/VC0
	  BETA1=0.25*BETA2
	  NAMCAT='CATOD1'
      write (NOUT,
     &'(T8,''PJA'',T18,''PMA'',T26,''BETA1'',T36,''BETA2'',T49,
     & ''FG'',T56,''ZSCHIF'',T67,''STEP'',T75,''NAMCAT''
     &/4F10.3,F11.2,F9.3,F10.3,A10)')
     & PJA, PMA, BETA1, BETA2, FG, ZSCHIF, STEP, NAMCAT
	IF (NUMB.LE.0) GOTO 50000
      DO 10 I=1,ABS(MS)
 	 IF (I .EQ. NUMB) THEN
	   R1=BDR(I)
	   Z1=BDZ(I)
	   R2=BDR(I+1)
	   Z2=BDZ(I+1)
CC
CC определение  координат цетра окружности (XCN, YCN, RCO)
CC 
	   CALL CORCR(Z1,R1,Z2,R2,X0,Y0,REX(I))
CC
CC определение углов наклона радиуса в начальной
CC и конечной точках дуги окружности
CC
	   DELTAX=Z1-X0
	   DELTAY=R1-Y0
	   ANGL1=DATAN2(DELTAY,DELTAX)
	   IF (DELTAY.LE.0) ANGL1= ANGL1+ PI + PI
	   DELTAX=Z2-X0
	   DELTAY=R2-Y0
	   ANGL2=DATAN2(DELTAY,DELTAX)
	   IF (DELTAY.LE.0) ANGL2= ANGL2+ PI + PI
CC
CC определение направления обхода по дуге окружности
CC
	   RH = DABS(REX(I))
	   FHO = ANGL1
	   FHF = ANGL2
         DFO = (FHF - FHO) / 2. / IZZ
	   AL0 = RH*DFO*2.
       END IF
 10   CONTINUE
CC
CC программа готовит начальные координаты инжекции трубок тока
CC
      DO 30 I=1,IZZ
	  FO = FHO + (2 * I - 1) * DFO
	  UX(I)=X0+ RH * DCOS (FO)
	  VY(I)=Y0+ RH * DSIN (FO)
 30   CONTINUE	  
	  NAMCAT='CATOD2'
50000   AL1=PI*2.	
      NUMB = ABS(NUMB) 
!	IF (NAMCAT .EQ. 'CATOD1') READ (10,*) (UX(I),VY(I),I=0,N4)
      write (NOUT,'(T8,''X0'',T18,''Y0'',T28,''RH'',T38,''FHO'',
     & T48,''FHF'',T58,''DFO'',T68,''AL0'',T75,''SQUARE''/8F10.3)')
     & X0, Y0, RH, FHO, FHF, DFO, AL0,
     & AL1*RH*Y0*(FHF - FHO) -AL1*RH*RH*(DCOS(FHF)-DCOS(FHO))
 	   IF (NAMCAT .EQ. 'CATOD1') IBEG=0
 	   IF (NAMCAT .EQ. 'CATOD2') IBEG=1
      write (NOUT,
     &'(T3,''продольная координата точек эмиссии на сетке''/(12F10.3))')
     & (UX(I), I=IBEG,IZZ)
      write (NOUT,
     &'(T3,''поперечная координата точек эмиссии на сетке''/(12F10.3))')
     & (VY(I), I=IBEG,IZZ)
      IF (NAMCAT .EQ. 'CATOD2') THEN
      write (NOUT,
     &'(T3,''площадь сегмента эмиссии на сетке''/(12F10.3))')
     & (AL1*AL0*VY(I),
     & I=IBEG,IZZ)
      write (NOUT,'(T3,''Продольная и поперечная координата''
     & ,T38,''граничной точки и площадь сегмента эмиссии на сетке'')')
      DO 77777 I=0,IZZ, MZ
	  FO = FHO + 2 * I * DFO
	  UXI = X0+ RH * DCOS (FO)
	  VYI = Y0+ RH * DSIN (FO)
 	   IF (I .EQ. 0) write (NOUT,'(I3,2F10.3)') I, UXI, VYI
	  FO = FHO + (2 * I - 1) * DFO
 	   IF (I .NE. 0) SQI = AL1*AL0*(Y0+ RH * DSIN (FO)) 	   
 	   IF (I .NE. 0) write (NOUT,'(I3,3F10.3)') I, UXI, VYI, SQI
77777 CONTINUE	  
      END IF
88888 CLOSE (UNIT=NOUT, STATUS = 'KEEP')
      RETURN
*=== End of subroutine START =========================================*
      END
!----------------------------------------
      SUBROUTINE CORCR( Z1, R1, Z2, R2, ZCR, RCR, RADIUS)
*                                                                      *
*    Input variables:                                                  *
*	   RADIUS = радиус окружности				                       *
*                                                                      *
*                                                                      *
*    Output variables:                                                 *
*                                                                      *
*          ZCR =                                                       *
*          RCR =                                                       *
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
*=== End of subroutine CORCR =========================================*
      END
