      subroutine gf_setnp (MXX, MXY)
C	  subroutine set     23/08/73
C...subroutine	set	 13/01/19
      use msflib
      INTEGER(4) status
C..координата правого угла области
      integer(2) gfR,gfYP2,gfXP2
	common  /gfgg/ gfR(2),gfYP2,gfXP2
      TYPE (QWINFO) winfo
! Set window frame position and size by SETWSIZEQQ.
      winfo%x = 700 ! x-coordinate for upper left corner.
      winfo%y = 0   ! y-coordinate for upper left corner.
! размеры окна приложения MXX х MXY пикселей
      winfo%h = MXY+50 ! Window height.
      winfo%w = MXX+50 ! Window width.
      winfo%type =QWIN$SET
! Set current size of child window associated with unit ?.
      status     = SetWSizeQQ( QWIN$FRAMEWINDOW, winfo)
! Child window size is winfo%w by  winfo%h
! размеры окна приложения 800х600 пикселей
            gfXP2 = MXX
            gfYP2 = MXY
	return
        end

      SUBROUTINE begin (nt2x, nt2y, name,n)
      use msflib
      IMPLICIT  NONE
C..координата правого угла области
      integer(2) gfR,gfYP2,gfXP2
	common  /gfgg/ gfR(2),gfYP2,gfXP2
C  инициализация устройств и явное задание размера страницы                               Declare variables
      TYPE (windowconfig) wc
      LOGICAL status
      LOGICAL QOPEN
      character name(1)
      integer n
      integer(2) nt2x, nt2y,IUNIT
      nt2x = gfXP2
      nt2y = gfYP2
      gfR(1) = gfXP2-1
      gfR(2) = gfYP2-1
! Set window frame position and size by SETWSIZEQQ.
      wc%numxpixels  = nt2x		   ! Number of pixels on x-axis.
      wc%numypixels  = nt2y		   ! Number of pixels on y-axis.
      wc%extendfontname = "Courier"C
      wc%numcolors   = 256_2         ! Number of color indexes
      wc%numtextcols = -1		       ! Number of text columns available.
      wc%numtextrows = -1		       ! Number of text rows available.
      wc%title=""C                   ! Window title.
      wc%fontsize = QWIN$EXTENDFONT  ! Size of default font.
      wc%extendfontname = "Courier"C
      do IUNIT = 1,n
      wc%title(IUNIT:IUNIT)=name(IUNIT)
      end do
      do IUNIT=99,10,-1
        INQUIRE (UNIT=IUNIT,  OPENED=QOPEN)
        IF (.NOT.QOPEN) THEN
           OPEN(UNIT=IUNIT,FILE='USER',ERR=99999)
           EXIT
        END IF
      end do
      STATUS = .FALSE.
      do while(.NOT.STATUS)
      STATUS = SETWINDOWCONFIG(wc)
      end do
      STATUS = GETWINDOWCONFIG(wc)
      call sleepqq(100)
99999 end	subroutine begin

      subroutine setpen (ncolor)
C	  subroutine set     23/08/73
C...subroutine	set	 13/01/19
      use msflib
      INTEGER*4 IRGB, RGB16(0:255)
      INTEGER*4 ncolor
      DATA RGB16(0:255)/
     &  #000000,#0000FF,#00AAFF,#00FFFF,#00FF00,#FF9300,#FF0000,#FF00FF,
     1  #888888,#000088,#008888,#00FFAA,#93FF00,#AA9300,#FF0093,#FFFFFF,
	2  #000000,#111111,#222222,#333333,#444444,#555555,#666666,#777777,
     3  #888888,#999999,#AAAAAA,#BBBBBB,#CCCCCC,#DDDDDD,#EEEEEE,#FFFFFF,
	4  #FF0000,#FF1111,#FF2222,#FF3333,#FF4444,#FF5555,#FF6666,#FF7777,
	5  #FF8888,#FF9999,#FFAAAA,#FFBBBB,#FFCCCC,#FFDDDD,#FFEEEE,#FFFFFF,
     6  #00FF00,#11FF11,#22FF22,#33FF33,#44FF44,#55FF55,#66FF66,#77FF77,
	7  #88FF88,#99FF99,#AAFFAA,#BBFFBB,#CCFFCC,#DDFFDD,#EEFFEE,#FFFFFF,
     8  #FFFF00,#FFFF11,#FFFF22,#FFFF33,#FFFF44,#FFFF55,#FFFF66,#FFFF77,
     9  #FFFF88,#FFFF99,#FFFFAA,#FFFFBB,#FFFFCC,#FFFFDD,#FFFFEE,#FFFFFF,
     &  #0000FF,#1111FF,#2222FF,#3333FF,#4444FF,#5555FF,#6666FF,#7777FF,
     1  #8888FF,#9999FF,#AAAAFF,#BBBBFF,#CCCCFF,#DDDDFF,#EEEEFF,#FFFFFF,
	2  #00FFFF,#11FFFF,#22FFFF,#33FFFF,#44FFFF,#55FFFF,#66FFFF,#77FFFF,
     3  #88FFFF,#99FFFF,#AAFFFF,#BBFFFF,#CCFFFF,#DDFFFF,#EEFFFF,#FFFFFF,
	4  #682440,#FF0480,#FF04C0,#FF0500,#FF0540,#FF0580,#FF05C0,#FF0000,
	5  #702640,#FF0680,#FF06C0,#FF0700,#FF0740,#FF0780,#FF07C0,#FF0000,
     6  #782840,#FF0880,#FF08C0,#FF0900,#FF0940,#FF0980,#FF09C0,#FF1000,
	7  #803040,#FF1080,#FF10C0,#FF1100,#FF1140,#FF1180,#FF11C0,#FF1000,
	8  #883240,#FF1280,#FF12C0,#FF1300,#FF1340,#FF1380,#FF13C0,#FF1000,
	9  #903440,#FF1480,#FF14C0,#FF1500,#FF1540,#FF1580,#FF15C0,#FF1000,
     &  #983640,#FF1680,#FF16C0,#FF1700,#FF1740,#FF1780,#FF17C0,#FF1000,
	1  #A03840,#FF1880,#FF18C0,#FF1900,#FF1940,#FF1980,#FF19C0,#FF2000,
	2  #A84040,#FF0080,#FF00C0,#FF0100,#FF0140,#FF0180,#FF01C0,#FF0200,
     3  #B04240,#FF0280,#FF02C0,#FF0300,#FF0340,#FF0380,#FF03C0,#FF0400,
	4  #B84440,#FF0480,#FF04C0,#FF0500,#FF0540,#FF0580,#FF05C0,#FF0600,
	5  #C04640,#FF0680,#FF06C0,#FF0700,#FF0740,#FF0780,#FF07C0,#FF0800,
     6  #C84840,#FF0880,#FF08C0,#FF0900,#FF0940,#FF0980,#FF09C0,#FF1000,
	7  #D05040,#FF1080,#FF10C0,#FF1100,#FF1140,#FF1180,#FF11C0,#FF1200,
	8  #D85240,#FF1280,#FF12C0,#FF1300,#FF1340,#FF1380,#FF13C0,#FF1400,
	9  #E04440,#FF1480,#FF14C0,#FF1500,#FF1540,#FF1580,#FF15C0,#FF1600,
     &  #E85640,#FF1680,#FF16C0,#FF1700,#FF1740,#FF1780,#FF17C0,#FF1800,
	1  #F05840,#FF1880,#FF18C0,#FF1900,#FF1940,#FF1980,#FF19C0,#FF2000/
C-----------------------------------------------------------------------
C General parameters:
C
C   ncol : номер цвета (N от 0 до 255)
C...Following data statement provides unique colors on all tested adapters
C
           IRGB = SETCOLORRGB(RGB16(ncolor))
	end subroutine setpen

      subroutine setBGR (ncol)
      use msflib
      INTEGER*4 RGB16(0:255)
      INTEGER*4 ncol
C...Following data statement provides unique colors on all tested adapters
      DATA RGB16(0:255)/
     &  #000000,#FFFFFF,#0000FF,#00FF00,#FF0000,#FFFF00,#FF00FF,#00FFFF,
     1  #005FFF,#00FFAA,#AAFF00,#FF9300,#FF0093,#5F00FF,#555555,#AAAAAA,
	2  #000000,#111111,#222222,#333333,#444444,#555555,#666666,#777777,
     3  #888888,#999999,#AAAAAA,#BBBBBB,#CCCCCC,#DDDDDD,#EEEEEE,#FFFFFF,
	4  #FF0000,#FF1111,#FF2222,#FF3333,#FF4444,#FF5555,#FF6666,#FF7777,
	5  #FF8888,#FF9999,#FFAAAA,#FFBBBB,#FFCCCC,#FFDDDD,#FFEEEE,#FFFFFF,
     6  #00FF00,#11FF11,#22FF22,#33FF33,#44FF44,#55FF55,#66FF66,#77FF77,
	7  #88FF88,#99FF99,#AAFFAA,#BBFFBB,#CCFFCC,#DDFFDD,#EEFFEE,#FFFFFF,
     8  #FFFF00,#FFFF11,#FFFF22,#FFFF33,#FFFF44,#FFFF55,#FFFF66,#FFFF77,
	9  #401440,#FF1480,#FF14C0,#FF1500,#FF1540,#FF1580,#FF15C0,#FF1000,
     &  #480640,#FF1680,#FF16C0,#FF1700,#FF1740,#FF1780,#FF17C0,#FF1000,
     1  #500840,#FF1880,#FF18C0,#FF1900,#FF1940,#FF1980,#FF19C0,#FF2000,
	2  #582040,#FF0080,#FF00C0,#FF0100,#FF0140,#FF0180,#FF01C0,#FF0000,
     3  #602240,#FF0280,#FF02C0,#FF0300,#FF0340,#FF0380,#FF03C0,#FF0000,
	4  #682440,#FF0480,#FF04C0,#FF0500,#FF0540,#FF0580,#FF05C0,#FF0000,
	5  #702640,#FF0680,#FF06C0,#FF0700,#FF0740,#FF0780,#FF07C0,#FF0000,
     6  #782840,#FF0880,#FF08C0,#FF0900,#FF0940,#FF0980,#FF09C0,#FF1000,
	7  #803040,#FF1080,#FF10C0,#FF1100,#FF1140,#FF1180,#FF11C0,#FF1000,
	8  #883240,#FF1280,#FF12C0,#FF1300,#FF1340,#FF1380,#FF13C0,#FF1000,
	9  #903440,#FF1480,#FF14C0,#FF1500,#FF1540,#FF1580,#FF15C0,#FF1000,
     &  #983640,#FF1680,#FF16C0,#FF1700,#FF1740,#FF1780,#FF17C0,#FF1000,
	1  #A03840,#FF1880,#FF18C0,#FF1900,#FF1940,#FF1980,#FF19C0,#FF2000,
	2  #A84040,#FF0080,#FF00C0,#FF0100,#FF0140,#FF0180,#FF01C0,#FF0200,
     3  #B04240,#FF0280,#FF02C0,#FF0300,#FF0340,#FF0380,#FF03C0,#FF0400,
	4  #B84440,#FF0480,#FF04C0,#FF0500,#FF0540,#FF0580,#FF05C0,#FF0600,
	5  #C04640,#FF0680,#FF06C0,#FF0700,#FF0740,#FF0780,#FF07C0,#FF0800,
     6  #C84840,#FF0880,#FF08C0,#FF0900,#FF0940,#FF0980,#FF09C0,#FF1000,
	7  #D05040,#FF1080,#FF10C0,#FF1100,#FF1140,#FF1180,#FF11C0,#FF1200,
	8  #D85240,#FF1280,#FF12C0,#FF1300,#FF1340,#FF1380,#FF13C0,#FF1400,
	9  #E04440,#FF1480,#FF14C0,#FF1500,#FF1540,#FF1580,#FF15C0,#FF1600,
     &  #E85640,#FF1680,#FF16C0,#FF1700,#FF1740,#FF1780,#FF17C0,#FF1800,
	1  #F05840,#FF1880,#FF18C0,#FF1900,#FF1940,#FF1980,#FF19C0,#FF2000/
C-----------------------------------------------------------------------
C General parameters:
C
C   ncol : номер цвета (N от 0 до 255)
C...Sets the current background color to the given Red-Green-Blue(RGB) value. 
C
      if(setBKcolorRGB(RGB16(ncol)) >= 0)call ClearScreen($GCLEARSCREEN)     
!W      I4state = setBKcolorRGB(ncolor) 
	return
	end subroutine setBGR

      subroutine plot (nx, ny, k)
!...subroutine    plot      01/02/74
      use msflib
      IMPLICIT  NONE
!
!    выполняет три функции:
!    присвоение текущему положению пера указанных координат,
!    движение в указанную точку без рисования,
!    движение в указанную точку из текущего положения с рисованием
!
!    но теперь выполняет функции посредника,
!    обращаясь к соответствующим входам
!    компилирующей программы нижнего уровня,
!    которая генерирует приказы хэндлеру устройства
!
! В физической СК координаты точки –
! целочисленные положительные значения, начиная с 0.	Позиция текущей
! точки, в которой выполнялся графический вывод, сохраняется в
! переменной структурного типа (xycoord).
! Начальной точкой физической СК является точка с координатами (0,0),
! расположенная в верхнем левом углу экрана. Координата х возрастает
! слева направо, координата у – сверху вниз.
      TYPE (xycoord) XY
      integer nx !координата в пикселах
      integer ny !координата в пикселах
      integer k  !управляет перемещением пера в точку nx, ny.
!*Если k==2, перемещение происходит с опущеным пером (отрисовка линии),
!*               иначе - с поднятым (изменение позиции).
!*              
      integer(2) I2X,I2Y,I2STAT
      integer(2) gfR,gfYP2,gfXP2
	common  /gfgg/ gfR(2),gfYP2,gfXP2
         I2X   =          INT2(nx)
         I2Y   =(gfR(2) - INT2(ny))
      if(I2Y .GE. gfYP2) I2Y = gfR(2)                                                          
!*      if(I2Y .LT. 0)     I2Y = 0                                                          
!*      if(I2X .GE. gfXP2) I2X = gfR(1)                                                          
      if (k == 2) THEN
         I2STAT = LINETO(I2X, I2Y)
	  else
         CALL     MOVETO(I2X, I2Y, XY)
      end if
      return
      end
!---
$FREEFORM
! Copyright (C) 2012 Intel Corporation. All Rights Reserved. 
!
! The source code contained or described herein and all documents related to the source code 
! ("Material") are owned by Intel Corporation or its suppliers or licensors. Title to the 
! Material remains with Intel Corporation or its suppliers and licensors.  The Material is 
! protected by worldwide copyright laws and treaty provisions. No part of the Material may be 
! used, copied, reproduced, modified, published, uploaded, posted, transmitted, distributed, 
! or disclosed in any way except as expressly provided in the license provided with the 
! Materials.  No license under any patent, copyright, trade secret or other intellectual 
! property right is granted to or conferred upon you by disclosure or delivery of the 
! Materials, either expressly, by implication, inducement, estoppel or otherwise, except as 
! expressly provided in the license provided with the Materials.
!
! Example of calling the Win32 API routine GetOpenFileName
! This can be used from any application type, including Console
!
! GetSaveFileName is very similar.
!
Subroutine fileopen (file_spec)
use comdlg32
implicit none

! Declare structure used to pass and receive attributes
!Когда GetOpenFileName возвращает значение, эта структура содержит информацию о файле, 
!выбранном пользователем.
!
type(T_OPENFILENAME) ofn

! Declare filter specification.  This is a concatenation of
! pairs of null-terminated strings.  The first string in each pair
! is the file type name, the second is a semicolon-separated list
! of file types for the given name.  The list ends with a trailing
! null-terminated empty string.
!
character(*),parameter :: filter_spec = &
  "Data Files"C//"*.dat"C// &
  "Text Files"C//"*.txt"C// &
  "Fortran Files"C//"*.f90;*.f"C//""C

! Declare string variable to return the file specification.
! Initialize with an initial filespec, if any - null string
! otherwise
!
character(512) :: file_spec
integer status,ilen
ofn%lStructSize = SIZEOF(ofn)
ofn%hwndOwner = NULL
ofn%hInstance = NULL  ! For Win32 applications, you
                      ! can set this to the appropriate
                      ! hInstance
                      !
ofn%lpstrFilter = loc(filter_spec)
ofn%lpstrCustomFilter = NULL
ofn%nMaxCustFilter = 0
ofn%nFilterIndex = 1 ! Specifies initial filter value
ofn%lpstrFile = loc(file_spec)
ofn%nMaxFile = sizeof(file_spec)
ofn%nMaxFileTitle = 0
ofn%lpstrInitialDir = NULL  ! Use Windows default directory
ofn%lpstrTitle = loc(""C)
ofn%Flags = OFN_PATHMUSTEXIST
ofn%lpstrDefExt = loc("dat"C)
ofn%lpfnHook = NULL
ofn%lpTemplateName = NULL

! Call GetOpenFileName and check status
! Функция GetOpenFileName создает  диалоговое окно Открыть (Open),
! которое позволяет пользователю определить открываемый диск,
! каталог и имя файла или имена ряда файлов.
!
status = GetOpenFileName(ofn)
! Если пользователь прервал работу или закрыл диалоговое окно Открыть (Open)
! или произошла ошибка, возвращаемое значение - ноль.
if (status .eq. 0) then
  type *,'No file name specified'
else
  !Если пользователь задает имя файла и щелкает по кнопке ОК, возвращаемое значение не нуль.
  ! Get length of file_spec by looking for trailing NUL
  ilen = INDEX(file_spec,CHAR(0))
  type *,'Filespec is ',file_spec(1:ilen-1)
  ! Example of how to see if user said "Read Only"
  !  
  if (IAND(ofn%flags,OFN_READONLY) /= 0) &
    type *,'Readonly was requested'
end if
end Subroutine fileopen
