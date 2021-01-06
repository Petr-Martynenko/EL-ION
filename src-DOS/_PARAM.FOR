      INTEGER IRL 
      PARAMETER (IRL = 106) ! MAX число поперечных €чеек сетки
      INTEGER IZL 
      PARAMETER (IZL = 150) ! MAX число продольных €чеек сетки
      INTEGER IDIM 
      PARAMETER (IDIM = 16384) ! размерность массивов потенциалов
      INTEGER NS 
      PARAMETER (NS = 1024)  ! MAX число √–јЌ»„Ќџ’ “ќ„≈  контура
      INTEGER NP 
      PARAMETER (NP = 30)    ! MAX число узлов контура число дуг окружностей
      INTEGER NO 
      PARAMETER (NO = 10)    ! MAX к-во эквипотенциалей
      INTEGER NJMAX 
      PARAMETER (NJMAX = 20) ! MAX число значений потенциалов электродов
      INTEGER N4 
      PARAMETER (N4  = 120)  ! MAX число трубок тока
      INTEGER MT 
      PARAMETER (MT = 2048)  ! MAX число точек кривой
!           REAL XU0
      PARAMETER (XU0 = 0.)
!           REAL YV0
      PARAMETER (YV0 = 0.)
      INTEGER IZMAX 
      PARAMETER (IZMAX = IZL+6)
      PARAMETER (PI = 3.14159265358)
      PARAMETER (VC0 = 2.997925E+8 ) !скорость света м/сек
      PARAMETER (DEL = 1.7589E+11)    !отношение зар€да эл-на к массе, кл/к√
      PARAMETER (EPS0 = 8.854304E-12) !ф/м
      PARAMETER (DIO = 0.957E+8)  !отношение зар€да пр-на к массе
      PARAMETER (AMU0 = 1.E-7) !магнитна€ прониаемость вакуума
