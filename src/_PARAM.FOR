*=== GRAFM.dsp _PARAM.FOR ======================================================*
      INTEGER IRL            ! MAX число поперечных ячеек сетки 
      PARAMETER (IRL = 75)
      INTEGER IZL            ! MAX число продольных ячеек сетки 
      PARAMETER (IZL = 300)
      INTEGER IDIM           ! MAX число узлов двумерной сетки
!старый вариант      PARAMETER (IDIM = 9966)
      PARAMETER (IDIM = (IRL +1)*(IZL+1))
      INTEGER NS             ! MAX граничных точек 
      PARAMETER (NS  = 3*(IZL+IRL)+1)
      INTEGER NP             ! MAX число узлов контура 
      PARAMETER (NP = 30)
      INTEGER NO             ! MAX число дуг окружностей 
      PARAMETER (NO = 10)
      INTEGER NJMAX          ! MAX число значений потенциалов электродов 
      PARAMETER (NJMAX = 20)
      INTEGER N4             ! MAX число трубок тока 
      PARAMETER (N4  = 80)
      INTEGER MT             ! MAX число точек кривой 
      PARAMETER (MT = 1500)
      INTEGER IZMAX 
      PARAMETER (IZMAX = IZL+6)
	REAL XU0
      PARAMETER (XU0 = 0.)
      REAL YV0
      PARAMETER (YV0 = 0.)
      REAL*8 PI, PI2
      PARAMETER (PI = 3.14159265358, PI2 = PI + PI)
      REAL*8 VC0              !отношение заряда эл-на к массе, кл/кГ
      PARAMETER (VC0 = 2.997925D+8)
      REAL*8 DEL              !отношение заряда эл-на к массе, кл/кГ
      PARAMETER (DEL = 1.7589D+11)
      REAL*8 DIO              !отношение заряда протона к массе, кл/кГ
      PARAMETER (DIO = 0.957D+8)
      REAL*8 EPS0
      PARAMETER (EPS0 = 8.854304D-12)
      REAL*8 AMU0
      PARAMETER (AMU0 = 1D-7)
