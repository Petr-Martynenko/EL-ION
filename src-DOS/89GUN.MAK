PROJ = 89GUN
PROJFILE = 89GUN.mak
DEBUG = 0

PWBRMAKE  = pwbrmake
NMAKEBSC1  = set
NMAKEBSC2  = nmake
CC  = cl
CFLAGS_G  = /W2 /BATCH
CFLAGS_D  = /Gi$(PROJ).mdt /Zi /Od
CFLAGS_R  = /Ot /Oi /Ol /Oe /Og /Gs
FOR  = fl
FORFLAGS_G  = /AH /G2
FORFLAGS_R  = /Ot /FPi
FORFLAGS_D  = /Zi /Od /4Yb
MAPFILE_D  = NUL
MAPFILE_R  = $(PROJ).map
LFLAGS_G  =   /BATCH /SE:150
LFLAGS_D  = /CO /FAR /PACKC
LFLAGS_R  = /M /EXE /FAR /PACKC
LINKER  = link
ILINK  = ilink
LRF  = echo > NUL
LLIBS_G  = GRAFLIB+GRAF1+GRAF2+GRAF3

OBJS_EXT  = DRDISP.OBJ
OBJS  = 89GUN.obj OPEN.obj J.obj I.obj H.obj G.obj F.obj E.obj D.obj C.obj\
        B.obj A.obj $(OBJS_EXT)
SBRS  = 89GUN.sbr OPEN.sbr J.sbr I.sbr H.sbr G.sbr F.sbr E.sbr D.sbr C.sbr\
        B.sbr A.sbr

all: $(PROJ).exe

.SUFFIXES:
.SUFFIXES: .obj .sbr .for

89GUN.obj : 89GUN.FOR _PARAM.FOR _COMMON.FOR

89GUN.sbr : 89GUN.FOR _PARAM.FOR _COMMON.FOR

OPEN.obj : OPEN.FOR _PARAM.FOR _COMMON.FOR

OPEN.sbr : OPEN.FOR _PARAM.FOR _COMMON.FOR

J.obj : J.FOR _PARAM.FOR _COMMON.FOR

J.sbr : J.FOR _PARAM.FOR _COMMON.FOR

I.obj : I.FOR _PARAM.FOR _COMMON.FOR

I.sbr : I.FOR _PARAM.FOR _COMMON.FOR

H.obj : H.FOR _PARAM.FOR _COMMON.FOR

H.sbr : H.FOR _PARAM.FOR _COMMON.FOR

G.obj : G.FOR _PARAM.FOR _COMMON.FOR

G.sbr : G.FOR _PARAM.FOR _COMMON.FOR

F.obj : F.FOR _PARAM.FOR _COMMON.FOR

F.sbr : F.FOR _PARAM.FOR _COMMON.FOR

E.obj : E.FOR C:\FORT\INCLUDE\FGRAPH.FI C:\FORT\INCLUDE\FGRAPH.FD _PARAM.FOR\
        _COMMON.FOR

E.sbr : E.FOR C:\FORT\INCLUDE\FGRAPH.FI C:\FORT\INCLUDE\FGRAPH.FD _PARAM.FOR\
        _COMMON.FOR

D.obj : D.FOR _PARAM.FOR _COMMON.FOR

D.sbr : D.FOR _PARAM.FOR _COMMON.FOR

C.obj : C.FOR _PARAM.FOR _COMMON.FOR

C.sbr : C.FOR _PARAM.FOR _COMMON.FOR

B.obj : B.FOR _PARAM.FOR _COMMON.FOR

B.sbr : B.FOR _PARAM.FOR _COMMON.FOR

A.obj : A.FOR _PARAM.FOR _COMMON.FOR

A.sbr : A.FOR _PARAM.FOR _COMMON.FOR


$(PROJ).bsc : $(SBRS)
        $(PWBRMAKE) @<<
$(BRFLAGS) $(SBRS)
<<

$(PROJ).exe : $(OBJS)
!IF $(DEBUG)
        $(LRF) @<<$(PROJ).lrf
$(RT_OBJS: = +^
) $(OBJS: = +^
)
$@
$(MAPFILE_D)
$(LLIBS_G: = +^
) +
$(LLIBS_D: = +^
) +
$(LIBS: = +^
)
$(DEF_FILE) $(LFLAGS_G) $(LFLAGS_D);
<<
!ELSE
        $(LRF) @<<$(PROJ).lrf
$(RT_OBJS: = +^
) $(OBJS: = +^
)
$@
$(MAPFILE_R)
$(LLIBS_G: = +^
) +
$(LLIBS_R: = +^
) +
$(LIBS: = +^
)
$(DEF_FILE) $(LFLAGS_G) $(LFLAGS_R);
<<
!ENDIF
        $(LINKER) @$(PROJ).lrf


.for.obj :
!IF $(DEBUG)
        $(FOR) /c $(FORFLAGS_G) $(FORFLAGS_D) /Fo$@ $<
!ELSE
        $(FOR) /c $(FORFLAGS_G) $(FORFLAGS_R) /Fo$@ $<
!ENDIF

.for.sbr :
!IF $(DEBUG)
        $(FOR) /Zs $(FORFLAGS_G) $(FORFLAGS_D) /FR$@ $<
!ELSE
        $(FOR) /Zs $(FORFLAGS_G) $(FORFLAGS_R) /FR$@ $<
!ENDIF


run: $(PROJ).exe
        $(PROJ).exe $(RUNFLAGS)

debug: $(PROJ).exe
        CV $(CVFLAGS) $(PROJ).exe $(RUNFLAGS)
