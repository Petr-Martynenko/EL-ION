*=== GRAFM.dsp _COMMON.FOR ======================================================*
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
      REAL*8        DELR
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
      REAL*8        FP
      COMMON/COM08/FP(IDIM)
COM09............./COM09/IZL1,IRL1,NK,MTM
      INTEGER      IZL1      !к-во продольных сеточных узлов
      INTEGER      IRL1      !к-во поперечных сеточных узлов
      INTEGER      NK        !к-во граничных контура
      INTEGER      MTM
      COMMON/COM09/IZL1,IRL1,NK,MTM
COM10............./COM10/XU(0:IZL)
      REAL*4         XU     !продольная координата сеточных узлов
      COMMON/COM10/XU(0:IZL)
COM11............./COM11/YV(0:IRL)
      REAL*4        YV     !поперечная координата сеточных узлов
      COMMON/COM11/YV(0:IRL) 
COM12............./COM12/BDR(NP),BDZ(NP),REX(NP),IGM(NP),MS
      REAL*8        BDR
      REAL*8        BDZ
      REAL*8        REX     !радиус с направлением обхода по дуге окружности
      INTEGER      IGM      !признак граничного условия
      INTEGER      MS       !к-во узлов контура
      COMMON/COM12/BDR(NP),BDZ(NP),REX(NP),IGM(NP),MS
COM13............./COM13/ROK(NO),XOK(NO),YOK(NO)                 
      REAL*8        ROK
      REAL*8        XOK
      REAL*8        YOK
      COMMON/COM13/ROK(NO),XOK(NO),YOK(NO)
COM14............./COM14/S, C(10), NAME, MI, MZ
      REAL*8       S     !масштаб рисунка, см/сет.ед.
      REAL*8       C     !массив значений уровней;
      CHARACTER*8  NAME
      INTEGER      MI     !MI=0--график не строится;
      INTEGER      MZ
      COMMON/COM14/S, C(10), NAME, MI, MZ
COM15............./COM15/IGMA                 
      INTEGER      IGMA
      COMMON/COM15/IGMA
COM16............./COM16/EPS, UVOMAX, GAMAS(15), NITER
      REAL*8       EPS     !точность расчета
      REAL*8       UVOMAX  !MAX эмитирующая способность
      REAL*8       GAMAS   !массив корректирующих множителей
      INTEGER      NITER   !номер шага приближения поетнциала+траектории
      COMMON/COM16/EPS, UVOMAX, GAMAS(15), NITER
COM17............./COM17/PX(MT)                 
      REAL         PX	     !массив продольных координата
      COMMON/COM17/PX(MT)
COM18............./COM18/PY(MT)
      REAL         PY      !массив поперечных координата
      COMMON/COM18/PY(MT)
COM19............./COM19/POT(NJMAX),NITM,ITMA,ITMI,MTWR,RELE,ABSE,W,FGUN,NUMB
      REAL*8       POT
      INTEGER      NITM      ! к-во итераций потенциала до записи
      INTEGER      ITMA	   ! MAX к-во итераций расчетов потенциала
      INTEGER      ITMI	   ! MIN к-во итераций расчетов потенциала
CC    NAMCAT=.TRUE.--
CC    NAMCAT=.FALSE. -- ошибка расчета потенциала больше заданной точности расчета
      LOGICAL      MTWR	   ! параметр печати и расчета потенциала
      REAL*8       RELE
      REAL*8       ABSE
      REAL*8       W    ! параметр расчета потенциала методом верхней релаксации
      CHARACTER*8  FGUN
CC    NUMB=0--расчет вакуумного потенциала  
      INTEGER      NUMB	   !номер поверхности инжекции трубок тока
      COMMON/COM19/POT(NJMAX),NITM,ITMA,ITMI,MTWR,RELE,ABSE,W,FGUN,NUMB
COM20............./COM20/UX(0:N4),VY(0:N4),DR,BETA1,BETA2,IZZ
      REAL*4       UX    !продольная координата точки эмиссии
      REAL*4       VY    !поперечная координата точки эмиссии
      REAL*8       DR
      REAL*8       BETA1
      REAL*8       BETA2
      INTEGER      IZZ   !к-во разбиений пов-сти эмиссии
      COMMON/COM20/UX(0:N4),VY(0:N4),DR,BETA1,BETA2,IZZ
COM21............./COM21/AKAT(3),BKAT(3),DKAT(3),TKAT(3),WKAT(3),ZKAT(3)
      REAL         AKAT
      REAL         BKAT
      REAL         DKAT
      REAL         TKAT
      REAL         WKAT
      REAL         ZKAT
      COMMON/COM21/AKAT(3),BKAT(3),DKAT(3),TKAT(3),WKAT(3),ZKAT(3)
COM22............./COM22/PERVO,POTX(N4),Q1,UVOX(N4)
      REAL*8       PERVO ! значение первеанса
      REAL*8       POTX  ! потенциал в точке старта трубок тока
      REAL*8       Q1
      REAL*8       UVOX
      COMMON/COM22/PERVO,POTX(N4),Q1,UVOX(N4)
COM23............./COM23/PJA,PMA,AL0,AL1,DFO,FHO
      REAL*8        PJA !нормировка сеточной плотности тока
      REAL*8        PMA !нормировка физической плотности тока
      REAL*8        AL0
      REAL*8        AL1
      REAL*8        DFO !угловая величина сегмента инжекции
      REAL*8        FHO !начальный угол поверхности инжекции
      COMMON/COM23/PJA,PMA,AL0,AL1,DFO,FHO
COM24............./COM24/BZA(-6:IZMAX)
      REAL*8       BZA     !осевое распределение магнитного поля
      COMMON/COM24/BZA(-6:IZMAX)
COM25............./COM25/PCIMG(N4)
      REAL*8         PCIMG
      COMMON/COM25/PCIMG(N4)
COM26............./COM26/FG,Z0,BN
      REAL         FG    ! напряжение на пушке, V
      REAL         Z0    ! нормировка линейных размеров задачи, м/сет.ед.
      REAL         BN    ! нормировка магнитной индукции, Wb/m/m
      COMMON/COM26/FG,Z0,BN
COM27............./COM27/Y(5),FO,VO,ZNAK,ZSCHIF,STEP,NAMCAT,ISTART
      REAL*8        Y	  
      REAL*8        FO !	  
      REAL*8        VO !скорость в точке старта	  
CC    ZNAK=+1--инжекция электронов	  
CC    ZNAK=-A--инжекция ионов с атомным номером A	  
      REAL*8        ZNAK	  
      REAL*8        ZSCHIF	  
      REAL*8        STEP	  
CC    NAMCAT='CATOD1'--инжекция со сферического катода	  
CC    NAMCAT='CATOD2'--инжекция с заданной поверхности	  
      CHARACTER*8 NAMCAT
CC    ISTART=0--инжекция частиц с поверхности эмиссии  
CC    ISTART=1--считывание начальных условий для старта
      INTEGER      ISTART !инжекция частиц с поверхности эмисси
      COMMON/COM27/Y(5),FO,VO,ZNAK,ZSCHIF,STEP,NAMCAT,ISTART
COM28............./COM28/MAG, MAGSEG, MAGORD
CC    MAGSEG=0--полиномиальная аппроксимация  магнитного поля	  
CC    MAGSEG= --количество соленоидов	  
      LOGICAL      MAG !=.TRUE.--определено осевое распределение магнитного поля	  
      INTEGER      MAGSEG
      INTEGER      MAGORD
      COMMON/COM28/MAG, MAGSEG, MAGORD
COM29............./ECOM/FF,DELT,EQUIPR,EQLN,EQST
      REAL*8        FF  
      REAL*8        DELT !	  
      REAL*8        EQUIPR
      INTEGER      EQLN ! макс.количество итераций уточнения координат
      INTEGER      EQST ! количество делений начального шага эквипотенциал
      COMMON/ECOM/FF,DELT,EQUIPR,EQLN,EQST
