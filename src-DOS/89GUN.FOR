CC    89GUN.for - For MS FORTRAN 5.1
CC
CC    есть текст программ от 11-10-89
CC
CC Program 89GUN.FOR резервирует память для общих блоков и организует
CC загрузку пр-м по плану, указанному во вводимых данных, и открывает
CC логические каналы №№ 1 - 6

!----------------------------------------
      BLOCK DATA M1
!----------------------------------------
      IMPLICIT REAL*8 (A-H,O-Z)
      INCLUDE '_PARAM.FOR'  
      INCLUDE '_COMMON.FOR'
      DATA MS/20/
!     считывание геометрии контура
      DATA BDR(01),BDZ(01),IGM(01),IEX(01)/ 0.0,    150.0, 2,  0/
      DATA BDR(02),BDZ(02),IGM(02),IEX(02)/20.0,    150.0,-6,  0/
      DATA BDR(03),BDZ(03),IGM(03),IEX(03)/20.0,     64.5,-6,  0/
      DATA BDR(04),BDZ(04),IGM(04),IEX(04)/14.0,     64.5,-6,  0/
      DATA BDR(05),BDZ(05),IGM(05),IEX(05)/14.0,     60.0,-6,  0/
      DATA BDR(06),BDZ(06),IGM(06),IEX(06)/18.5,     50.5,-6,  0/
      DATA BDR(07),BDZ(07),IGM(07),IEX(07)/23.0,     50.5,-6,  0/
      DATA BDR(08),BDZ(08),IGM(08),IEX(08)/27.0,     55.0,-6,  0/
      DATA BDR(09),BDZ(09),IGM(09),IEX(09)/27.0,     60.0,-6,  0/
      DATA BDR(10),BDZ(10),IGM(10),IEX(10)/106.0,     60.0, 1,  0/
      DATA BDR(11),BDZ(11),IGM(11),IEX(11)/106.0,     45.5,-1,  0/
      DATA BDR(12),BDZ(12),IGM(12),IEX(12)/39.444,   45.5,-1,  0/
      DATA BDR(13),BDZ(13),IGM(13),IEX(13)/35.0,     40.5,-1,  0/
      DATA BDR(14),BDZ(14),IGM(14),IEX(14)/36.884,   37.7,-1, 1/
      DATA BDR(15),BDZ(15),IGM(15),IEX(15)/30.015,   23.4,-1,  0/
      DATA BDR(16),BDZ(16),IGM(16),IEX(16)/20.0,     20.5,-1,  0/
      DATA BDR(17),BDZ(17),IGM(17),IEX(17)/17.2,     15.5,-1,  0/
      DATA BDR(18),BDZ(18),IGM(18),IEX(18)/17.2,      0.0,-1,  0/
      DATA BDR(19),BDZ(19),IGM(19),IEX(19)/16.0,      0.0, 3,  0/
      DATA BDR(20),BDZ(20),IGM(20),IEX(20)/ 0.0,      0.0, 4,  0/
      DATA ROK(01),XOK(01),YOK(01)/20.0,    38.5,     16.9/
      DATA NITM/13/
      DATA ITMA/2000/
      DATA ITMI/50/
      DATA MTWR/.TRUE./
      DATA RELE/1.E-5/
      DATA ABSE/1.E-9/
      DATA W   /1.9/
      DATA FGUN/'INTG '/
      DATA NUMB/14/
      DATA POT
     & /0.,0.,0.,0.,0.,1.,0.,0.,0.,0.,0.,9*0./

      END !BLOCK DATA M1
!----------------------------------------
      BLOCK DATA M2
!----------------------------------------
!
      IMPLICIT REAL*8 (A-H,O-Z)
      INCLUDE '_PARAM.FOR'  
      INCLUDE '_COMMON.FOR'


      DATA NK /300/
      DATA MI /4/
      DATA M4 /1/
      DATA NAME /'FORCE '/
      DATA S /0.2/
      DATA C /.05,.15,.25,.35,.45,.55,.65,.75,.85,.95/
      DATA NITER /0/
      DATA EPS /0.01/
      DATA UVOMAX /0.005/
      DATA GAMAS !коэффициент релаксации обьемного заряда
     &	 /0.025,0.035,0.045,0.055,0.065,
     &	  0.075,0.100,0.100,0.200,0.200,
     &	  0.250,0.275,0.300,0.325,0.375/
      DATA DR /1.7/
      DATA IZZ /120/
!внутрений радиус катушки,м
      DATA AKAT(01),AKAT(02),AKAT(03)/0.1235,0.0540,0.01/  
!внешний радиус катушки,м
      DATA BKAT(01),BKAT(02),BKAT(03)/0.1490,0.0675,0.02/
!полудлина соленоида,м
      DATA DKAT(01),DKAT(02),DKAT(03)/0.0165,0.3440,0.10/  
!коорд. ц.соленоида в лаб.системе координат,м
      DATA ZKAT(01),ZKAT(02),ZKAT(03)/0.1255,0.5050,0.10/  
!ток в  витке , А
      DATA TKAT(01),TKAT(02),TKAT(03)/-35.00,+80.00,0.00/
!кол-во витков , если N1= О , плотность тока в сечении катушки,А/(m**2) 
      DATA WKAT(01),WKAT(02),WKAT(03)/+142.0,2556.0,500./  
      DATA FG /50000./
      DATA Z0 /0.002/
      DATA Y /0.,0.,0.,0.,0./
      DATA ISTART /0/
      DATA FO /0./
      DATA VO /0./
      DATA ZNAK /1./
      DATA ZSCHIF /135./
      DATA NAMCAT /'CATOD1'/
      DATA STEP /0.5/
      DATA MAG /.FALSE./
      DATA MAGSEG /3/
      DATA MAGORD /6/

      END !BLOCK DATA M2

!----------------------------------------
