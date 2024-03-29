!=== 89GUN common blocks=======================================*
!$_ECOM
      INTEGER*4 EQST ! ���������� ������� ���������� ���� ��������������
      INTEGER*4 EQLN ! ����.���������� �������� ��������� ���������
      COMMON/ECOM/FF,DELT,EQUIPR,EQLN,EQST
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
      REAL*8         DELR
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
      REAL*8         FP
      COMMON/COM08/FP(IDIM)
COM09............./COM09/IZLIM1,IRLIM1,NK,MTM
      INTEGER      IZLIM1
      INTEGER      IRLIM1
      INTEGER      NK
      INTEGER      MTM
      COMMON/COM09/IZLIM1,IRLIM1,NK,MTM
COM10............./COM10/XU(0:IZL)
      REAL*8         XU     !���������� ���������� �������� �����
      COMMON/COM10/XU(0:IZL)
COM11............./COM11/YV(0:IRL)
      REAL*8         YV     !���������� ���������� �������� �����
      COMMON/COM11/YV(0:IRL) 
COM12............./COM12/MS,BDR(NP),BDZ(NP),IGM(NP),IEX(NP)
      INTEGER      MS       !�-�� ����� �������
      REAL*8       BDR
      REAL*8       BDZ
      INTEGER      IGM      !������� ���������� �������
      INTEGER      IEX      !����������� ����������� ������ �� ���� ����������
!
! ����������� ����������� ������ �� ���� ����������
!	IEX=+1. ������ ������� �������
!	IEX=-1. �� ������� �������
!
      COMMON/COM12/MS,BDR(NP),BDZ(NP),IGM(NP),IEX(NP)
COM13............./COM13/ROK(NP),XOK(NP),YOK(NP)                 
      REAL*8         ROK
      REAL*8         XOK
      REAL*8         YOK
      COMMON/COM13/ROK(NO),XOK(NO),YOK(NO)
COM14............./COM14/MI, M4, NAME, S, C(NO)
      INTEGER      MI
!    MI=0--������ �� ��������  
!    MI<0--�������������� �� ��������  
      INTEGER      M4
      CHARACTER*6 NAME
      REAL*8         S     !������� �������, ��/���.��.
      REAL*8         C     !
      COMMON/COM14/MI, M4, NAME, S, C(NO)
COM15............./COM15/IGMA                 
      INTEGER      IGMA
      COMMON/COM15/IGMA
COM16............./COM16/NITER, EPS, UVOMAX, GAMAS(30)
      INTEGER      NITER   !����� ��������
      REAL*8       EPS     !�������� �������
      REAL*8       UVOMAX  !MAX ����������� �����������
      REAL*8       GAMAS   !������ �������������� ����������
      COMMON/COM16/NITER, EPS, UVOMAX, GAMAS(30)
COM17............./COM17/PX(MT)                 
      REAL         PX	   !������ ���������� ����������
      COMMON/COM17/PX(MT)
COM18............./COM18/PY(MT)
      REAL         PY    !������ ���������� ����������
      COMMON/COM18/PY(MT)
COM19............./COM19/NITM,ITMA,ITMI,MTWR,RELE,ABSE,W,FGUN,NUMB,POT(NJMAX)
      INTEGER      NITM      ! �-�� �������� ���������� �� ������
      INTEGER      ITMA	     ! MAX �-�� �������� �������� ����������
      INTEGER      ITMI	     ! MIN �-�� �������� �������� ����������
      LOGICAL      MTWR	     ! �������� ������
      REAL*8       RELE
      REAL*8       ABSE
      REAL*8       W
      CHARACTER*6  FGUN
CC    ���������� ��������� ������� �������� ������ ����
CC    NUMB<0--����� ������ ��������� ����� �������
CC    NUMB>0--����� ����������� �������� ������ ����  
CC    NUMB=0--������ ���������� ����������  
      INTEGER      NUMB	   !����� ����������� �������� ������ ����
      REAL*8       POT
      COMMON/COM19/NITM,ITMA,ITMI,MTWR,RELE,ABSE,W,FGUN,NUMB,POT(NJMAX)
COM20............./UVW/UX(0:N4),VY(0:N4),DR,BETA1,BETA2,IZZ
      REAL*8       UX     !���������� ���������� ����� �������
      REAL*8       VY     !���������� ���������� ����� �������
      REAL*8       DR
      REAL*8       BETA1
      REAL*8       BETA2
      INTEGER      IZZ !�-�� ��������� ���-��� �������
      COMMON/COM20/UX(0:N4),VY(0:N4),DR,BETA1,BETA2,IZZ
COM21............./COM21/AKAT(3),BKAT(3),DKAT(3),TKAT(3),WKAT(3),ZKAT(3)
      REAL*8         AKAT !��������� ������ �������,�  
      REAL*8         BKAT !������� ������ �������,� 
      REAL*8         DKAT !��������� ���������,� 
      REAL*8         TKAT !��� �  ����� , � 
      REAL*8         WKAT !���-�� ������,���� N1= �,��������� ���� � ������� �������,�/(m**2) 
      REAL*8         ZKAT !�����. �.��������� � ���.������� ���������,� 
      COMMON/COM21/AKAT(3),BKAT(3),DKAT(3),TKAT(3),WKAT(3),ZKAT(3)
COM22............./COM22/PERVO,POTX(N4),Q1,UVOX(N4)
      REAL*8         PERVO
      REAL*8         POTX !��������� � ����� ������ ������ ����
      REAL*8         Q1
      REAL*8         UVOX
      COMMON/COM22/PERVO,POTX(N4),Q1,UVOX(N4)
COM23............./COM23/PJA,PMA,AL0,AL1,DFO,FHO
      REAL*8         PJA
      REAL*8         PMA
      REAL*8         AL0
      REAL*8         AL1
      REAL*8         DFO !������� �������� �������� �������� � ��������
      REAL*8         FHO !��������� ���� ����������� �������� � ��������
      COMMON/COM23/PJA,PMA,AL0,AL1,DFO,FHO
COM24............./COM24/BZA(-6:IZMAX)
      REAL*8       BZA     !������ ������������� ���������� ����
      COMMON/COM24/BZA(-6:IZMAX)
COM25............./COM25/PCIMG(N4)
      REAL*8         PCIMG
      COMMON/COM25/PCIMG(N4)
COM26............./COM26/FG,Z0,BN
      REAL*8         FG    ! ���������� �� �����, V
      REAL*8         Z0    ! ���������� �������� �������� ������, �/���.��.
      REAL*8         BN    ! ���������� ��������� ��������, Wb/m/m
      COMMON/COM26/FG,Z0,BN
COM27............./COM27/Y(5),ISTART,FO,VO,ZNAK,ZSCHIF,NAMCAT,STEP
      REAL*8         Y	  
!    ISTART=0--�������� ������ � ����������� �������  
!    ISTART=1--���������� ��������� ������� ��� ������
      INTEGER      ISTART !�������� ������ � ����������� ������
      REAL*8       FO     !	  
      REAL*8       VO     !�������� � ����� ������	  
      REAL*8       ZNAK	  
!    ZNAK=+1.--��� ����������	  
!    ZNAK=-1.--��� �����	  
      REAL*8       ZSCHIF	  
!    NAMCAT='CATOD2'--�������� �� ������������ ������	  
!    NAMCAT='CATOD1'--�������� � �������� �����������	  
      CHARACTER*6 NAMCAT
      REAL*8       STEP	  
      COMMON/COM27/Y(5),ISTART,FO,VO,ZNAK,ZSCHIF,NAMCAT,STEP
COM28............./COM28/MAG, MAGSEG, MAGORD
CC    MAGSEG=0--�������������� �������������  ���������� ����	  
CC    MAGSEG= --���������� ����������	  
CC    MAG=.TRUE. --���������� ������ ������������� ���������� ����	  
CC    MAG=.FALSE.--������ ��� ���������� ����	    
      LOGICAL      MAG !=.TRUE.--���������� ������ ������������� ���������� ����	  
      INTEGER      MAGSEG
      INTEGER      MAGORD
      COMMON/COM28/MAG, MAGSEG, MAGORD

