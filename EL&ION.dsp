# Microsoft Developer Studio Project File - Name="EL&ION" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) QuickWin Application" 0x0107

CFG=EL&ION - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "EL&ION.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "EL&ION.mak" CFG="EL&ION - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "EL&ION - Win32 Release" (based on "Win32 (x86) QuickWin Application")
!MESSAGE "EL&ION - Win32 Debug" (based on "Win32 (x86) QuickWin Application")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
F90=df.exe
MTL=midl.exe
RSC=rc.exe

!IF  "$(CFG)" == "EL&ION - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Release"
# PROP Intermediate_Dir "Release"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE F90 /compile_only /libs:qwin /nologo /warn:nofileopt
# ADD F90 /compile_only /fpscomp:filesfromcmd /libs:qwin /nologo /optimize:0 /warn:nofileopt
# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MBCS" /YX /FD /c
# ADD CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MBCS" /YX /FD /c
# ADD BASE MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x419 /d "NDEBUG"
# ADD RSC /l 0x419 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib /nologo /entry:"WinMainCRTStartup" /subsystem:windows /machine:I386 /nodefaultlib:"dfconsol.lib"
# ADD LINK32 kernel32.lib /nologo /entry:"WinMainCRTStartup" /subsystem:windows /machine:I386 /nodefaultlib:"dfconsol.lib" /out:"EL&ION.exe"

!ELSEIF  "$(CFG)" == "EL&ION - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "Debug"
# PROP Intermediate_Dir "Debug"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE F90 /check:bounds /compile_only /dbglibs /debug:full /libs:qwin /nologo /traceback /warn:argument_checking /warn:nofileopt
# ADD F90 /check:bounds /compile_only /dbglibs /debug:full /libs:qwin /nologo /traceback /warn:argument_checking /warn:nofileopt
# ADD BASE CPP /nologo /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MBCS" /YX /FD /GZ /c
# ADD CPP /nologo /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MBCS" /YX /FD /GZ /c
# ADD BASE MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x419 /d "_DEBUG"
# ADD RSC /l 0x419 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib /nologo /entry:"WinMainCRTStartup" /subsystem:windows /debug /machine:I386 /nodefaultlib:"dfconsol.lib" /pdbtype:sept
# ADD LINK32 C:\WORK\GRAFOR65 kernel32.lib /nologo /entry:"WinMainCRTStartup" /subsystem:windows /incremental:no /debug /machine:I386 /nodefaultlib:"dfconsol.lib" /nodefaultlib /pdbtype:sept

!ENDIF 

# Begin Target

# Name "EL&ION - Win32 Release"
# Name "EL&ION - Win32 Debug"
# Begin Group "Source Files"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;idl;hpj;bat;f90;for;f;fpp"
# Begin Source File

SOURCE=.\src\A.FOR
DEP_F90_A_FOR=\
	".\src\_COMON.FOR"\
	".\src\_PARAM.FOR"\
	
# End Source File
# Begin Source File

SOURCE=.\src\B.FOR
DEP_F90_B_FOR=\
	".\src\_COMON.FOR"\
	".\src\_PARAM.FOR"\
	
# End Source File
# Begin Source File

SOURCE=.\src\C.FOR
DEP_F90_C_FOR=\
	".\src\_COMON.FOR"\
	".\src\_PARAM.FOR"\
	
# End Source File
# Begin Source File

SOURCE=.\src\D.FOR
DEP_F90_D_FOR=\
	".\src\_COMON.FOR"\
	".\src\_PARAM.FOR"\
	
# End Source File
# Begin Source File

SOURCE=.\src\E.FOR
DEP_F90_E_FOR=\
	".\src\_COMON.FOR"\
	".\src\_PARAM.FOR"\
	
# End Source File
# Begin Source File

SOURCE=.\src\F.for
DEP_F90_F_FOR=\
	".\src\_COMON.FOR"\
	".\src\_PARAM.FOR"\
	
# End Source File
# Begin Source File

SOURCE=.\src\G.FOR
DEP_F90_G_FOR=\
	".\src\_COMON.FOR"\
	".\src\_PARAM.FOR"\
	
# End Source File
# Begin Source File

SOURCE="..\..\..\MSDEV 6.0\My Projects 6.0\Grafor4.20\Grafor\low\grinit.f"
# End Source File
# Begin Source File

SOURCE=.\src\H.FOR
DEP_F90_H_FOR=\
	".\src\_COMON.FOR"\
	".\src\_PARAM.FOR"\
	
# End Source File
# Begin Source File

SOURCE=.\src\I.FOR
DEP_F90_I_FOR=\
	".\src\_COMON.FOR"\
	".\src\_PARAM.FOR"\
	
# End Source File
# Begin Source File

SOURCE=.\src\J.FOR
DEP_F90_J_FOR=\
	".\src\_COMON.FOR"\
	".\src\_PARAM.FOR"\
	
# End Source File
# Begin Source File

SOURCE=.\src\K.FOR
DEP_F90_K_FOR=\
	".\src\_COMON.FOR"\
	".\src\_PARAM.FOR"\
	
# End Source File
# Begin Source File

SOURCE=.\src\OPEN.FOR
DEP_F90_OPEN_=\
	".\src\_BLOCK_0.FOR"\
	".\src\_BLOCK_1.FOR"\
	".\src\_BLOCK_2.FOR"\
	".\src\_COMON.FOR"\
	".\src\_PARAM.FOR"\
	".\src\Namelist.for"\
	{$(INCLUDE)}"MSIMSL.mod"\
	
# End Source File
# End Group
# End Target
# End Project
