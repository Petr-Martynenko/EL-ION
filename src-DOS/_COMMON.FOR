C     Main common blocks
COM01............./COM01/IPO(NS)
      INTEGER      IPO
      COMMON/COM01/IPO(NS)
COM02............./COM02/IR(NS)
      INTEGER      IR     ! массив координат граничных точек области
      COMMON/COM02/IR(NS)
COM03............./COM03/IZ(NS)
      INTEGER      IZ     ! массив координат граничных точек области
      COMMON/COM03/IZ(NS)
COM04............./COM04/DELR(NS)
      REAL*8         DELR
      COMMON/COM04/DELR(NS)
COM05............./COM05/DELZ(NS)
      REAL*8         DELZ
      COMMON/COM05/DELZ(NS)
COM06............./COM06/IFD(IDIM)
      INTEGER      IFD     ! массив-описатель формы области
      COMMON/COM06/IFD(IDIM)
COM07............./COM07/F(IDIM)
      REAL*8         F
      COMMON/COM07/F(IDIM)
COM08............./COM08/FP(IDIM)
      REAL*8         FP
      COMMON/COM08/FP(IDIM)
COM09............./COM09/IZL1,IRL1,NK,MTM
      INTEGER      IZL1
      INTEGER      IRL1
      INTEGER      NK
      INTEGER      MTM
      COMMON/COM09/IZL1,IRL1,NK,MTM
COM10............./COM10/XU(0:IZL)
      REAL*8         XU     !продольная координата сеточных узлов
      COMMON/COM10/XU(0:IZL)
COM11............./COM11/YV(0:IRL)
      REAL*8         YV     !поперечная координата сеточных узлов
      COMMON/COM11/YV(0:IRL) 
COM12............./COM12/MS,BDR(NP),BDZ(NP),IGM(NP),IEX(NP)
      INTEGER      MS       !к-во узлов контура
      REAL*8       BDR
      REAL*8       BDZ
      INTEGER      IGM      !признак граничного условия
      INTEGER      IEX      !определение направления обхода по дуге окружности
!
! определение направления обхода по дуге окружности
!	IEX=+1. против часовой стрелки
!	IEX=-1. по часовой стрелке
!
      COMMON/COM12/MS,BDR(NP),BDZ(NP),IGM(NP),IEX(NP)
COM13............./COM13/ROK(NP),XOK(NP),YOK(NP)                 
      REAL*8         ROK
      REAL*8         XOK
      REAL*8         YOK
      COMMON/COM13/ROK(NO),XOK(NO),YOK(NO)
COM14............./COM14/MI, M4, NAME, S, C(NO)
      INTEGER      MI
!    MI=0--график не строится  
!    MI<0--эквипотенциали не строятся  
      INTEGER      M4
      CHARACTER*6 NAME
      REAL*8         S     !масштаб рисунка, см/сет.ед.
      REAL*8         C     !
      COMMON/COM14/MI, M4, NAME, S, C(10)
COM15............./COM15/IGMA                 
      INTEGER      IGMA
      COMMON/COM15/IGMA
COM16............./COM16/NITER, EPS, UVOMAX, GAMAS(15)
      INTEGER      NITER   !номер итерации
      REAL*8       EPS     !точность расчета
      REAL*8       UVOMAX  !MAX эмитирующая способность
      REAL*8       GAMAS   !массив корректирующих множителей
      COMMON/COM16/NITER, EPS, UVOMAX, GAMAS(15)
COM17............./COM17/PX(MT)                 
      REAL         PX	   !массив продольных координата
      COMMON/COM17/PX(MT)
COM18............./COM18/PY(MT)
      REAL         PY    !массив поперечных координата
      COMMON/COM18/PY(MT)
COM19............./COM19/NITM,ITMA,ITMI,MTWR,RELE,ABSE,W,FGUN,NUMB,POT(NJMAX)
      INTEGER      NITM      ! к-во итераций потенциала до записи
      INTEGER      ITMA	     ! MAX к-во итераций расчетов потенциала
      INTEGER      ITMI	     ! MIN к-во итераций расчетов потенциала
      LOGICAL      MTWR	     ! параметр печати
      REAL*8       RELE
      REAL*8       ABSE
      REAL*8       W
      CHARACTER*6  FGUN
CC    считывание начальных условий инжекции трубок тока
CC    NUMB<0--задан массив координат точек эмиссии
CC    NUMB>0--номер поверхности инжекции трубок тока  
CC    NUMB=0--расчет вакуумного потенциала  
      INTEGER      NUMB	   !номер поверхности инжекции трубок тока
      REAL*8       POT
      COMMON/COM19/NITM,ITMA,ITMI,MTWR,RELE,ABSE,W,FGUN,NUMB,POT(NJMAX)
COM20............./COM20/U(0:N4),V(0:N4),DR,BETA1,BETA2,IZZ
      REAL*8       U     !продольная координата точки эмиссии
      REAL*8       V     !поперечная координата точки эмиссии
      REAL*8       DR
      REAL*8       BETA1
      REAL*8       BETA2
      INTEGER      IZZ   !к-во разбиений пов-сти эмиссии
      COMMON/COM20/U(0:N4),V(0:N4),DR,BETA1,BETA2,IZZ
COM21............./COM21/AKAT(3),BKAT(3),DKAT(3),TKAT(3),WKAT(3),ZKAT(3)
      REAL*8         AKAT !внутрений радиус катушки,м  
      REAL*8         BKAT !внешний радиус катушки,м 
      REAL*8         DKAT !полудлина соленоида,м 
      REAL*8         TKAT !ток в  витке , А 
      REAL*8         WKAT !кол-во витков,если N1= О,плотность тока в сечении катушки,А/(m**2) 
      REAL*8         ZKAT !коорд. ц.соленоида в лаб.системе координат,м 
      COMMON/COM21/AKAT(3),BKAT(3),DKAT(3),TKAT(3),WKAT(3),ZKAT(3)
COM22............./COM22/PERVO,POTX(N4),Q1,UVOX(N4)
      REAL*8         PERVO
      REAL*8         POTX !потенциал в точке старта трубок тока
      REAL*8         Q1
      REAL*8         UVOX
      COMMON/COM22/PERVO,POTX(N4),Q1,UVOX(N4)
COM23............./COM23/PJA,PMA,AL0,AL1,DFO,FHO
      REAL*8         PJA
      REAL*8         PMA
      REAL*8         AL0
      REAL*8         AL1
      REAL*8         DFO !угловая величина сегмента инжекции в радианах
      REAL*8         FHO !начальный угол поверхности инжекции в радианах
      COMMON/COM23/PJA,PMA,AL0,AL1,DFO,FHO
COM24............./COM24/BZA(-6:IZMAX)
      REAL*8       BZA     !осевое распределение магнитного поля
      COMMON/COM24/BZA(-6:IZMAX)
COM25............./COM25/PCIMG(N4)
      REAL*8         PCIMG
      COMMON/COM25/PCIMG(N4)
COM26............./COM26/FG,Z0,BN
      REAL*8         FG    ! напряжение на пушке, V
      REAL*8         Z0    ! нормировка линейных размеров задачи, м/сет.ед.
      REAL*8         BN    ! нормировка магнитной индукции, Wb/m/m
      COMMON/COM26/FG,Z0,BN
COM27............./COM27/Y(5),ISTART,FO,VO,ZNAK,ZSCHIF,NAMCAT,STEP
      REAL*8         Y	  
!    ISTART=0--инжекция частиц с поверхности эмиссии  
!    ISTART=1--считывание начальных условий для старта
      INTEGER      ISTART !инжекция частиц с поверхности эмисси
      REAL*8       FO     !	  
      REAL*8       VO     !скорость в точке старта	  
      REAL*8       ZNAK	  
!    ZNAK=+1.--для электронов	  
!    ZNAK=-1.--для ионов	  
      REAL*8       ZSCHIF	  
!    NAMCAT='CATOD2'--инжекция со сферического катода	  
!    NAMCAT='CATOD1'--инжекция с заданной поверхности	  
      CHARACTER*6 NAMCAT
      REAL*8       STEP	  
      COMMON/COM27/Y(5),ISTART,FO,VO,ZNAK,ZSCHIF,NAMCAT,STEP
COM28............./COM28/MAG, MAGSEG, MAGORD
CC    MAGSEG=0--полиномиальная аппроксимация  магнитного поля	  
CC    MAGSEG= --количество соленоидов	  
CC    MAG=.TRUE. --определено осевое распределение магнитного поля	  
CC    MAG=.FALSE.--задача без магнитного поля	    
      LOGICAL      MAG !=.TRUE.--определено осевое распределение магнитного поля	  
      INTEGER      MAGSEG
      INTEGER      MAGORD
      COMMON/COM28/MAG, MAGSEG, MAGORD
