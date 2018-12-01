CC    K.FOR Compaq Visual FORTRAN 6.6.0
CC
CC    24.07.2018
!----------------------------------------
      SUBROUTINE INTGR1
!----------------------------------------
      IMPLICIT REAL*8 (A-H,O-Z)
      INCLUDE '_PARAM.FOR'
      INCLUDE '_COMON.FOR'

C     Main program variables
      INTEGER     NOUT/0/
      REAL*8      UVX(N4),Y1,Y2,C1,C2
      OPEN  (UNIT=NOUT,FILE='CHECKERR.TXT',POSITION='APPEND')
!
      IF ( ISTART .EQ. 1 ) THEN
	   CALL FLUN56
	   REWIND 6
	   READ (6) UX(0),VY(0)
	   UX(0)=UX(0)/Z0
	   VY(0)=VY(0)/Z0
	   Q1=0.
      END IF
!
      IF ( ISTART .EQ. 0 ) THEN
       IF ( NITER .GE. NITM ) REWIND 5
       IF ( NITER .GE. NITM ) WRITE (5) UX(0)*Z0,VY(0)*Z0
	   PERVO=0.
	   Q1=1.
      END IF
      IF ( NITER .EQ. 1 ) PRINT '(141(1H*))' ! выдача на дисплей
      IF (.NOT. MAG ) PRINT '(T3,''N   Z-KOOP.'',T17,''R-KOOP.'', 	  
     &  T28,''Z-SPEED   R-SPEED   l-SPEED'',T58,''CHARGE'')'
      IF ( MAG ) PRINT'(T3,''N   Z-KOOP.'',T17,''R-KOOP.'',T27,  
     &''Z-SPEED   R-SPEED   L-SPEED   F-SPEED'',
     &	T66,''TOTAL-SP CHARGE'')'
!
!	GAMA=корректирующий множитель
!
      GAMA=1D0
!
!       задание начальных условий
!
      DO 10 I=1,IZZ
!
      IF ( ISTART .EQ. 1 ) THEN
	   READ (6) Y,PCIMG(I),UX(I),VY(I)
	   Y(1)=Y(1)/Z0
	   Y(2)=Y(2)/Z0
	   UX(I)=UX(I)/Z0
	   VY(I)=VY(I)/Z0
	   PCIMG(I)=PCIMG(I)/Z0
	   AL0=SQRT((UX(I-1)-UX(I))**2+(VY(I-1)-VY(I))**2)
	   L=1
	   Q1=0.
      END IF
!
      IF ( ISTART .EQ. 0 ) THEN
      IF ( NAMCAT .EQ. 'CATOD1' ) THEN
      CALL DISTN( UX(I-1), VY(I-1), UX(I), VY(I), Y(1), Y(2), DR)
		AL0=SQRT((UX(I-1)-UX(I))**2+(VY(I-1)-VY(I))**2)
      END IF
!
      IF ( NAMCAT .EQ. 'CATOD2' ) THEN
	  FO = FHO + (2 * I - 1) * DFO
	  Y1=UX(I)-DR*DCOS(FO)
	  Y2=VY(I)-DR*DSIN(FO)
      END IF
!
!	определение потенциала в точке старта
!
      CALL FORCE(POTX(I),Y1,Y2,C1,C2)
      IF (IGMA .EQ. 0) GOTO 10
      IF ((POT(-IGM(NUMB))-POTX(I))*ZNAK.GE.0.) GOTO 10
      VO=DSQRT(DABS(POT(-IGM(NUMB))-POTX(I)))
!
!	коррекция тока инжекции
!
      UVO=VO*VO*VO
      UVX(I)=1.D+10
      FOX=UVOX(I)*EPS
      FOX=DABS(FOX)
      IF (DABS(UVO-UVOX(I)) .GE. FOX) Q1=0.
      IF (UVO .GT. UVOMAX) UVO=UVOMAX
      IF (UVOX(I) .NE. 0.0) UVX(I)=UVO/UVOX(I)
      UVOX(I)=UVO
!
!	определение координат в точке инжекции
!
		Y(1)=UX(I)
		Y(2)=VY(I)
		IF ( NAMCAT .EQ. 'CATOD1' ) THEN
		   Y(1)=.5*(Y(1)+UX(I-1))
		   Y(2)=.5*(Y(2)+VY(I-1))
		END IF
		Y(3)=0.
		Y(4)=0.
		Y(5)=PJA*UVO*AL0*Y(2)
		PERVO=PERVO+PMA*AL1*Y(5)
		L=0
      END IF
      IGMA=1
      CALL INTEGR(I,L)
 9    FORMAT (' PER=',F6.4,'(MICROAMPS/V**1.5) FG=',I7,'(V) I=',G8.3,
     + '(AMPS) NIT=',I3,' GAMA=',F5.3)
 10   CONTINUE
      PRINT '(T3,''RATIO OF START POINT POTENTIALS'')'
      PRINT '(14F10.6)', (POTX(I),I=1,IZZ)
      PRINT
     & '(T3,''RATIO OF CURRENT DENCITY FOR FG='',I7,''(V)'')',INT4(FG)
      PRINT '(14F10.3)', (UVX(I),I=1,IZZ)
!
!       задание
!
      DO 20 I=1,IZZ
	 UVX(I)=0.0
	 IF ( POTX(I) .GE.0. ) UVX(I)=UVOX(I)*PJA !*PMA*FG**1.5*1.E-10/Z0**2
 20   CONTINUE
      PRINT '(T3,
     & ''CURRENT DENCITY  NORMALIZED FOR FG='',I7,''(V)'')', INT4(FG)
      PRINT '(14F10.6)', (UVX(I),I=1,IZZ)
      IF ( ISTART .EQ. 0 ) THEN
      PRINT 9, PERVO,INT4(FG),PERVO*FG**1.5*1.E-6,NITER,GAMA
      write (NOUT,9) PERVO,INT4(FG),PERVO*FG**1.5*1.E-6,NITER,GAMA
      END IF
      IF (Q1 .LT. 0.5 .OR. NITER .GE. NITM ) THEN
	 PRINT '(141(1H*))'
      END IF      
      CLOSE (UNIT=NOUT, STATUS = 'KEEP')
      RETURN
*= End of subroutine INTGR1 =========================================*
      END SUBROUTINE INTGR1