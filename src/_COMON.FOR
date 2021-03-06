*=== GRAFM.dsp _COMMON.FOR ======================================================*
C     Main common blocks
COM01............./COM01/IPO(NS)
      INTEGER      IPO
      COMMON/COM01/IPO(NS)
COM02............./COM02/IR(NS)
      INTEGER      IR     ! ������ ��������� ��������� ����� �������
      COMMON/COM02/IR(NS)
COM03............./COM03/IZ(NS)
      INTEGER      IZ     ! ������ ��������� ��������� ����� �������
      COMMON/COM03/IZ(NS)
COM04............./COM04/DELR(NS)
      REAL*8        DELR
      COMMON/COM04/DELR(NS)
COM05............./COM05/DELZ(NS)
      REAL*8         DELZ
      COMMON/COM05/DELZ(NS)
COM06............./COM06/IFD(IDIM)
      INTEGER      IFD     ! ������-��������� ����� �������
      COMMON/COM06/IFD(IDIM)
COM07............./COM07/F(IDIM)
      REAL*8         F
      COMMON/COM07/F(IDIM)
COM08............./COM08/FP(IDIM)
      REAL*8        FP
      COMMON/COM08/FP(IDIM)
COM09............./COM09/IZL1,IRL1,NK,MTM
      INTEGER      IZL1      !�-�� ���������� �������� �����
      INTEGER      IRL1      !�-�� ���������� �������� �����
      INTEGER      NK        !�-�� ��������� �������
      INTEGER      MTM
      COMMON/COM09/IZL1,IRL1,NK,MTM
COM10............./COM10/XU(0:IZL)
      REAL*4         XU     !���������� ���������� �������� �����
      COMMON/COM10/XU(0:IZL)
COM11............./COM11/YV(0:IRL)
      REAL*4        YV     !���������� ���������� �������� �����
      COMMON/COM11/YV(0:IRL) 
COM12............./COM12/BDR(NP),BDZ(NP),REX(NP),IGM(NP),MS
      REAL*8        BDR
      REAL*8        BDZ
      REAL*8        REX     !������ � ������������ ������ �� ���� ����������
      INTEGER      IGM      !������� ���������� �������
      INTEGER      MS       !�-�� ����� �������
      COMMON/COM12/BDR(NP),BDZ(NP),REX(NP),IGM(NP),MS
COM13............./COM13/ROK(NO),XOK(NO),YOK(NO)                 
      REAL*8        ROK
      REAL*8        XOK
      REAL*8        YOK
      COMMON/COM13/ROK(NO),XOK(NO),YOK(NO)
COM14............./COM14/S, C(10), NAME, MI, MZ
      REAL*8       S     !������� �������, ��/���.��.
      REAL*8       C     !������ �������� �������;
      CHARACTER*8  NAME
      INTEGER      MI     !MI=0--������ �� ��������;
      INTEGER      MZ
      COMMON/COM14/S, C(10), NAME, MI, MZ
COM15............./COM15/IGMA                 
      INTEGER      IGMA
      COMMON/COM15/IGMA
COM16............./COM16/EPS, UVOMAX, GAMAS(15), NITER
      REAL*8       EPS     !�������� �������
      REAL*8       UVOMAX  !MAX ����������� �����������
      REAL*8       GAMAS   !������ �������������� ����������
      INTEGER      NITER   !����� ���� ����������� ����������+����������
      COMMON/COM16/EPS, UVOMAX, GAMAS(15), NITER
COM17............./COM17/PX(MT)                 
      REAL         PX	     !������ ���������� ����������
      COMMON/COM17/PX(MT)
COM18............./COM18/PY(MT)
      REAL         PY      !������ ���������� ����������
      COMMON/COM18/PY(MT)
COM19............./COM19/POT(NJMAX),NITM,ITMA,ITMI,MTWR,RELE,ABSE,W,FGUN,NUMB
      REAL*8       POT
      INTEGER      NITM      ! �-�� �������� ���������� �� ������
      INTEGER      ITMA	   ! MAX �-�� �������� �������� ����������
      INTEGER      ITMI	   ! MIN �-�� �������� �������� ����������
CC    NAMCAT=.TRUE.--
CC    NAMCAT=.FALSE. -- ������ ������� ���������� ������ �������� �������� �������
      LOGICAL      MTWR	   ! �������� ������ � ������� ����������
      REAL*8       RELE
      REAL*8       ABSE
      REAL*8       W    ! �������� ������� ���������� ������� ������� ����������
      CHARACTER*8  FGUN
CC    NUMB=0--������ ���������� ����������  
      INTEGER      NUMB	   !����� ����������� �������� ������ ����
      COMMON/COM19/POT(NJMAX),NITM,ITMA,ITMI,MTWR,RELE,ABSE,W,FGUN,NUMB
COM20............./COM20/UX(0:N4),VY(0:N4),DR,BETA1,BETA2,IZZ
      REAL*4       UX    !���������� ���������� ����� �������
      REAL*4       VY    !���������� ���������� ����� �������
      REAL*8       DR
      REAL*8       BETA1
      REAL*8       BETA2
      INTEGER      IZZ   !�-�� ��������� ���-��� �������
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
      REAL*8       PERVO ! �������� ���������
      REAL*8       POTX  ! ��������� � ����� ������ ������ ����
      REAL*8       Q1
      REAL*8       UVOX
      COMMON/COM22/PERVO,POTX(N4),Q1,UVOX(N4)
COM23............./COM23/PJA,PMA,AL0,AL1,DFO,FHO
      REAL*8        PJA !���������� �������� ��������� ����
      REAL*8        PMA !���������� ���������� ��������� ����
      REAL*8        AL0
      REAL*8        AL1
      REAL*8        DFO !������� �������� �������� ��������
      REAL*8        FHO !��������� ���� ����������� ��������
      COMMON/COM23/PJA,PMA,AL0,AL1,DFO,FHO
COM24............./COM24/BZA(-6:IZMAX)
      REAL*8       BZA     !������ ������������� ���������� ����
      COMMON/COM24/BZA(-6:IZMAX)
COM25............./COM25/PCIMG(N4)
      REAL*8         PCIMG
      COMMON/COM25/PCIMG(N4)
COM26............./COM26/FG,Z0,BN
      REAL         FG    ! ���������� �� �����, V
      REAL         Z0    ! ���������� �������� �������� ������, �/���.��.
      REAL         BN    ! ���������� ��������� ��������, Wb/m/m
      COMMON/COM26/FG,Z0,BN
COM27............./COM27/Y(5),FO,VO,ZNAK,ZSCHIF,STEP,NAMCAT,ISTART
      REAL*8        Y	  
      REAL*8        FO !	  
      REAL*8        VO !�������� � ����� ������	  
CC    ZNAK=+1--�������� ����������	  
CC    ZNAK=-A--�������� ����� � ������� ������� A	  
      REAL*8        ZNAK	  
      REAL*8        ZSCHIF	  
      REAL*8        STEP	  
CC    NAMCAT='CATOD1'--�������� �� ������������ ������	  
CC    NAMCAT='CATOD2'--�������� � �������� �����������	  
      CHARACTER*8 NAMCAT
CC    ISTART=0--�������� ������ � ����������� �������  
CC    ISTART=1--���������� ��������� ������� ��� ������
      INTEGER      ISTART !�������� ������ � ����������� ������
      COMMON/COM27/Y(5),FO,VO,ZNAK,ZSCHIF,STEP,NAMCAT,ISTART
COM28............./COM28/MAG, MAGSEG, MAGORD
CC    MAGSEG=0--�������������� �������������  ���������� ����	  
CC    MAGSEG= --���������� ����������	  
      LOGICAL      MAG !=.TRUE.--���������� ������ ������������� ���������� ����	  
      INTEGER      MAGSEG
      INTEGER      MAGORD
      COMMON/COM28/MAG, MAGSEG, MAGORD
COM29............./ECOM/FF,DELT,EQUIPR,EQLN,EQST
      REAL*8        FF  
      REAL*8        DELT !	  
      REAL*8        EQUIPR
      INTEGER      EQLN ! ����.���������� �������� ��������� ���������
      INTEGER      EQST ! ���������� ������� ���������� ���� �������������
      COMMON/ECOM/FF,DELT,EQUIPR,EQLN,EQST
