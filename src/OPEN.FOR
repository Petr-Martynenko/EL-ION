CC    OPEN.FOR Compaq Visual FORTRAN 6.6.0
CC
CC    ���� ����� �������� �� 09-06-88;30-06-2019
CC
CC Program OPEN.FOR ����������� ������ ��� ����� ������ � ����������
CC �������� ��-� �� �����, ���������� �� �������� ������
CC ��������� ����������� ���, ��������� ��������� ��������, ����������� 
CC ������� ��������������� ������� ������ � ����� � ��������� ������������
CC �������� ������� ��������� �������� 1.E-7 TL
CC
!MS$OBJCOMMENT LIB: "C:\WORK\Grafor.lib"
      PROGRAM OPEN
      USE MSIMSL
      IMPLICIT REAL*8 (A-H,O-Z)
!----------------------------------------

      INCLUDE '_PARAM.FOR'
      INCLUDE '_COMON.FOR'

C                                 Declare variables
      integer     IDAY, MONTH, IYEAR
      integer     IHOUR, MINUTE, ISEC
      integer     NOUT, iocheck
      CHARACTER*1 cmdchar1
      CHARACTER*45 NMFLA                   ! ����� ������ 9
      CHARACTER*45 NMFLB /'EL&ION.dat'/    ! ����� ����� 10
      CHARACTER*45 NMFLC /'11.TXT'/        ! ����� ������ 11
      DATA         NMFLA /'CHECKERR.TXT'/
      INCLUDE     'Namelist.FOR'
      namelist /Q11/ AKAT,BKAT,DKAT,TKAT,WKAT,ZKAT
!     iocheck	     An integer variable that returns zero if no error
!		     occurs, -1 if end-of-file is encountered, or the
!		     error number if an error occurs.
!     Get the name of a file
      OPEN (UNIT=10,   FILE='', IOSTAT=iocheck)
!     INQUIRE about file's existence:
      INQUIRE (UNIT=10,NAME = NMFLB, IOSTAT=iocheck)
00100 READ (10,input0, ERR=100, END=50000)
00120 OPEN (UNIT=11,   FILE=NMFLC, IOSTAT=iocheck)
	CALL RusConsole
C
C	The AMACH function and UMACH subroutine are declared in the IMSL module
C
C      UMACH (-2, NUNIT)                       Write to the FILE='CHECKERR'
C      UMACH (+3, NUNIT)                       Write to the console
C                                 Set Parameter NOUT
	CALL UMACH (-2, NOUT)
      OPEN (UNIT=NOUT,FILE=NMFLA, IOSTAT=iocheck)
      write (NOUT,22222)
22222 FORMAT (/T3,'�������� ������   GRAFM.dsp'/
     &	    /T3,'�������� ��������� OPEN.FOR'/)
C                                 Get today's date
      CALL TDATE (IDAY, MONTH, IYEAR)
C                                 Get time of day
      CALL TIMDY (IHOUR, MINUTE, ISEC)
C                                 
      write (NOUT,'(" IDAY   = ",I2.2,1H-,I2.2,1H-,I4.4)')
     & IDAY, MONTH, IYEAR
      write (NOUT,'(" TIME   = ",I2.2,1H:,I2.2,1H:,I2.2)')
     & IHOUR, MINUTE, ISEC
      write (NOUT,'(A,F10.0)') ' CPUtime= ', CPSEC ()
      write (NOUT,input0)
      write (NOUT,'("��� ����� ����� ����� 10= ",A)') NMFLB
      write (NOUT,'(/80(1H*))')
      write (NOUT,
     &'(T63,''���������� ���������='',F10.4/T3,''IZL='',I3,T13,
     &''IRL='',I3,T23,''IZZ='',I4,T33,''NS='',I4,T43,''XU0='',E10.3)')
     & Z0,IZL,IRL,IZZ,NS,XU0
      write (NOUT,'(T13,''�������� ������'',
     &'' ���������������� ������� ���������� ������� ����������='',
     & F10.4)') W
      write (NOUT,'(T6,''NITM'',T16,''ITMA'',T26,''ITMI''
     & ,T36,''MTWR'',T43,''RELE'',T53,''ABSE'',T63,''W''/
     & 3I10,L10,1P,3E10.3)')
     & NITM,ITMA,ITMI,MTWR,RELE,ABSE,W
      write (NOUT,'(T3,''EPS'',T18,''NT'',T23,''UVOMAX''/
     & G10.3,I10,G10.3,/,'' GAMAS-ARRAY''/15F8.3/)')
     & EPS, NK,UVOMAX, GAMAS
      write (NOUT,'(T6,''Potential of contour electrodes='')')
      write (NOUT,'(5(I6,F8.2))')(I,POT(I),I=1,NJMAX)
      write (NOUT,'(//100(1H*))')
      CLOSE (UNIT=NOUT, STATUS = 'KEEP')
!---execution-part
      IRL1=IRL+1
      IZL1=IZL+1
      IDIM1=IRL1*IZL1 !�-�� ����� ����� ����������
      F=0.
      FP=0.
      DO IN = 0, IZL
      XU(IN)=FLOAT(IN)+XU0
         END DO
      DO IN = 0, IRL
      YV(IN)=FLOAT(IN)+YV0
         END DO
      OPEN (UNIT=1,  !����� ������ ������� ����������
     +FILE='F1',STATUS='UNKNOWN',ACCESS='SEQUENTIAL',FORM='UNFORMATTED')
      OPEN (UNIT=2,  !����� ������ ���������� ������ ����
     +FILE='F2',STATUS='UNKNOWN',ACCESS='SEQUENTIAL',FORM='UNFORMATTED')
      OPEN (UNIT=3,  !����� ������ ������� ���� ��������� ������
     +FILE='F3',STATUS='UNKNOWN',ACCESS='SEQUENTIAL',FORM='UNFORMATTED')
      OPEN (UNIT=4,  !����� ������ ��������������� 
     +FILE='F4',STATUS='UNKNOWN',ACCESS='SEQUENTIAL',FORM='UNFORMATTED')
      OPEN (UNIT=5,
     +FILE='F5',STATUS='UNKNOWN',ACCESS='SEQUENTIAL',FORM='UNFORMATTED')
      OPEN (UNIT=6,
     +FILE='F6',STATUS='UNKNOWN',ACCESS='SEQUENTIAL',FORM='UNFORMATTED')
!----------------------------------------------
      LOOP_ANSWER: DO
00940 READ (10,'(A)', ERR=940, END=50000) cmdchar1
      GET_ANSWER: SELECT CASE (cmdchar1)
        CASE ('A', 'a')
	   WRITE (*, *)' FDATA **************************************'
          CALL B
        CASE ('R', 'r')
	   WRITE (*, *)' START **************************************'
      REWIND (UNIT=3)
      WRITE  (UNIT=3)(FP(IN),IN=1,IDIM1)
      ENDFILE(UNIT=3)
33333 READ (10,input0, ERR=33333, END=50000)
          CALL START
        CASE ('D', 'd')
	   WRITE (*, *)' FPOTE **************************************'
          CALL FPOTE
        CASE ('E', 'e')
	   WRITE (*, *)' GRAFO **************************************'
          CALL E
        CASE ('F', 'f')
	   WRITE (*, *)' READL1**************************************'
CALL READL1
        CASE ('G', 'g')
	   WRITE (*, *)' INTGF **************************************'
          CALL INTGF
        CASE ('H', 'h')
	   WRITE (*, *)' MAGFD **************************************'
44444 READ (10,Q11, ERR=44444, END=50000)
          CALL MAGFD
          CALL MAGCAT
          CALL GRAF4WIN
        CASE ('I', 'i')
	   WRITE (*, *)' IOAT **************************************'
          CALL IOAT (.TRUE.)
        CASE ('J', 'j')
	   WRITE (*, *)' INTGR **************************************'
CC    �������������� ��������� ��������	  
CC    ���������� ������� ��������� ������	  
          CALL INTGR
        CASE ('K', 'k')
	   WRITE (*, *)' INTGR1 **************************************'
CC    �������������� ��������� ��������	  
CC    ���������� ������� ��������� ������ ��� ���������	  
          CALL INTGR1
        CASE ('L', 'l')
	   WRITE (*, *)' IOAT **************************************'
          CALL IOAT (.FALSE.)
        CASE ('Z', 'z')
	   GOTO 50000
        CASE DEFAULT
         WRITE (*, *) 'Command not recognized; please use H for help'
      END SELECT GET_ANSWER
      END DO LOOP_ANSWER

50000 OPEN  (UNIT=NOUT,FILE='CHECKERR.TXT',POSITION='APPEND',
     & IOSTAT=iocheck)
      write (NOUT,'(A,F10.2)') ' CPUtime= ', CPSEC ()  
      write (NOUT,'(A,I10.2)') ' NOUT   = ', NOUT
C     Close and discard file:
      IF    (NOUT == 6)
     &CLOSE (UNIT=NOUT,STATUS='DELETE')
      CLOSE (UNIT=01,  STATUS='KEEP')
      CLOSE (UNIT=02,  STATUS='DELETE')
      CLOSE (UNIT=03,  STATUS='KEEP')
      CLOSE (UNIT=04,  STATUS='DELETE')
      CLOSE (UNIT=05,  STATUS='DELETE')
      CLOSE (UNIT=06,  STATUS='DELETE')
      CLOSE (UNIT=10,  STATUS='KEEP')
      CLOSE (UNIT=11,  STATUS='KEEP')
99999 END PROGRAM OPEN

      INCLUDE '_BLOCK_0.FOR'

      INCLUDE '_BLOCK_1.FOR'

      INCLUDE '_BLOCK_2.FOR'
!
      LOGICAL(4) FUNCTION INITIALSETTINGS( )
      use msflib
      integer*4 i4
      TYPE (qwinfo) qwi
! Set window frame size.
      qwi%x = 0
      qwi%y = 0
      qwi%w = 1000
      qwi%h = 1000     ! window height
      qwi%type = QWIN$SET
      i4 = SetWSizeQQ( QWIN$FRAMEWINDOW, qwi )
      INITIALSETTINGS= .true.
      END FUNCTION INITIALSETTINGS