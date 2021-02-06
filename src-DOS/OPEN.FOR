CC    OPEN.for - For MS FORTRAN 5.1
CC
CC    есть текст программ от 11-10-89
CC
CC Program OPEN.FOR резервирует память для общих блоков и организует
CC загрузку пр-м по плану, указанному во вводимых данных, и открывает
CC логические каналы №№ 1 - 6
	INTERFACE TO SUBROUTINE
     +             clrscr[FAR,C,ALIAS:"__clearscreen"](area)
	INTEGER*2 area
	END
      PROGRAM OPEN
      IMPLICIT REAL*8 (A-H,O-Z)
      INCLUDE '_PARAM.FOR'  
      INCLUDE '_COMMON.FOR'  


C     Main program variables

      CHARACTER*1 ch1
      CHARACTER*6 NAMEA

      CALL clrscr(0)
      OPEN (UNIT=1,  !канал записи массива потенциала
     +FILE='F1',STATUS='UNKNOWN',ACCESS='SEQUENTIAL',FORM='UNFORMATTED')
      OPEN (UNIT=2,  !канал записи траекторий трубок тока
     +FILE='F2',STATUS='UNKNOWN',ACCESS='SEQUENTIAL',FORM='UNFORMATTED')
      OPEN (UNIT=3,  !канал записи массива поля объемного заряда
     +FILE='F3',STATUS='UNKNOWN',ACCESS='SEQUENTIAL',FORM='UNFORMATTED')
      OPEN (UNIT=4,
     +FILE='F4',STATUS='UNKNOWN',ACCESS='SEQUENTIAL',FORM='UNFORMATTED')
      OPEN (UNIT=5,
     +FILE='F5',STATUS='UNKNOWN',ACCESS='SEQUENTIAL',FORM='UNFORMATTED')
      OPEN (UNIT=6,
     +FILE='F6',STATUS='UNKNOWN',ACCESS='SEQUENTIAL',FORM='UNFORMATTED')
      IRL1=IRL+1
      IZL1=IZL+1                                         
      WRITE (*,*)'Работает программа OPEN89.FOR'
      WRITE (*,*)	'IDIM=', IZL1*IRL1
      IF( IZL1*IRL1 .GT. IDIM ) GO TO 40
       MTM=MT
        NK=NS
      WRITE (*,'(T63,''*******''/T3,''IZL='',I3,T13,
     &''IRL='',I3,T23,''MT='',I4,T33,''NS='',I4,T43,''XU0='',E10.3)')
     & IZL,IRL,MT,NS,XU0
      WRITE (*,'(T6,''NITM'',T16,''ITMA'',T26,''ITMI''
     &,T36,''MTWR'',T43,''RELE'',T53,''ABSE'',T63,''W''/
     & 3I10,L10,3E10.3)')
     & NITM,ITMA,ITMI,MTWR,RELE,ABSE,W
      WRITE (*,'(T3,''EPS'',T18,''NT'',T23,''UVOMAX''/
     & E10.3,I10,E10.3,//,'' GAMAS-ARRAY''/15F8.3/)')
     & EPS, NK,UVOMAX, GAMAS
      DO 13 I=1,IDIM
         F(I)=0.
 13   CONTINUE
      DO 14 I=1,IDIM
         FP(I)=0.
 14   CONTINUE
      DO 15 I = 0, IZL
        XU(I) =FLOAT(I)+XU0
 15   CONTINUE
      DO 16 I= 0, IRL
        YV(I)=FLOAT(I)+YV0
 16   CONTINUE
      DO 17 I= -6, IZMAX
        BZA(I)= 0.
 17   CONTINUE
      DO WHILE ( .TRUE. )
           ! read and print :
 1    CALL clrscr($GCLEARSCREEN)
	WRITE (*,*)'                                        A:FDATA1'
	WRITE (*,*)'                                        R:START'
	WRITE (*,*)'                                        D:FPOTE'
	WRITE (*,*)'                                        E:GRAFO'
	WRITE (*,*)'                                        F:READL1'
	WRITE (*,*)'                                        G:INTGF'
	WRITE (*,*)'                                        H:MAGFD'
	WRITE (*,*)'                                        I:CHARGE'
	WRITE (*,*)'                                        J:INTGR'
	WRITE (*,*)'                                        L:POTENT'  
	WRITE (*,*)'                                        Z:STOP'
	WRITE (*,'(A\)') ' INSERT NAME OF PROGRAM(A-Z)'  
	READ (*,'(A)',END=90) NAMEA
	ch1=NAMEA(1:1)
      SELECT CASE (ch1)
      CASE ('A', 'a')
	   WRITE (*, *)
     &'Работает программа FDATA **************************************'
	CALL FDATA1
      CASE ('R', 'r')
	   WRITE (*, *)
     &'Работает программа START **************************************'
	CALL START
      CASE ('D', 'd')
	   WRITE (*, *)
     &'Работает программа FPOTE **************************************'
	CALL FPOTE
C	CALL POTLST
      CASE ('E', 'e')
	   WRITE (*, *)
     &'Работает программа GRAFO **************************************'
	CALL GRAFO
      CASE ('F', 'f')
	   WRITE (*, *)
     &'Работает программа READL1**************************************'
	CALL READL1
      CASE ('G', 'g')
!	   WRITE (9, *)
!     &'Работает программа INTGF **************************************'
      CALL INTGF
      CASE ('H', 'h')
	   WRITE (*, *)
     &'Работает программа MAGFD **************************************'
	CALL MAGFD
	CALL MAGCAT
CALL GRAFM2
      CASE ('I', 'i')
	   WRITE (*, *)
     &'Работает программа REDZAP**************************************'  
! вывод массива объемного заряда	  CHARGE?.TXT
        call IOAT (.TRUE.)
!      call REDZAP 
      CASE ('J', 'j')
!	   WRITE (9, *)
!     &'Работает программа INTGR **************************************'
CC    интегрирование уравнений движения	  
CC    заполнение массива обьемного заряда	  
	CALL INTGR
      CASE ('L', 'l')
	   WRITE (*, *)
     &'Работает программа  IOAT **************************************'
        call IOAT (.FALSE.)  
      CASE ('Z', 'z')
	 GOTO 30
      CASE DEFAULT
	 WRITE (*, *) "Command not recognized; please use H for help"
      END SELECT
      END DO  
 30   CONTINUE
         IF(MAG)call GRAF4WIN
      CLOSE (UNIT=1,STATUS='KEEP')
      CLOSE (UNIT=2,STATUS='DELETE')
      CLOSE (UNIT=4,STATUS='DELETE')
      CLOSE (UNIT=5,STATUS='DELETE')
      CLOSE (UNIT=6,STATUS='DELETE')
         IF (MAG) close (UNIT=12,STATUS='DELETE')
      STOP  
 40   STOP 40
 90   STOP 90
      END !PROGRAM OPEN
