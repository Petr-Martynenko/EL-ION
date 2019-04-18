CC    F.FOR Compaq Visual FORTRAN 6.6.0
CC
CC    åñòü òåêñò ïðîãðàìì îò 03-01-91
CC
CC Ïðîåêò GRAF4WIN.FOR ðåçåðâèðóåò ïàìÿòü äëÿ îáùèõ áëîêîâ
CC îðãàíèçóåò ïîñòðîåíèå ñèëîâûõ ëèíèé ìàãíèòíîãî ïîëÿ
CC ïðîãðàììû îïðåäåëåíèÿ ÂÌÏ, êîìïîíåíò ìàãíèòíîé èíäóêöèè, ñîçäàâàåìûõ 
CC íàáîðîì îñåñèììåòðè÷íûõ òîêîâûõ êàòóøå â òî÷êå ñ çàäàííûìè êîîðäèíàòàìè
CC òî÷íîñòü ðàñ÷åòà ìàãíèòíîé èíäóêöèè 1.E-7 TL
!----------------------------------------
      SUBROUTINE DSPROM( IEQQ, EQB, AO, RHO, ZETA, C1, C2, *)
C *** INCREMENTS EQUIPOTENTIAL LINE ***
*  INTEGER IEQQ - inp  ! ìàêñèì.êîëè÷åñòâî èòåðàöèé óòî÷íåíèÿ êîîðäèíàò  
*  LOGICAL EQB  - inp  !
*  REAL*8 ZETA  - inp  ! ïðîäîëüíàÿ êîîðäèíàòà , íîðì.
!----------------------------------------
      IMPLICIT REAL*8 (A-H,O-Z)
CC    EQB=1--îïðåäåëåíî	  
CC    EQB=0--	  
      LOGICAL EQB
	  
      INTEGER*4 EQLN,EQST
      COMMON/ECOM/FF,DELT,EQUIPR,EQLN,EQST

C     Main program variables
	  
      DS = 1.0
C *** îáíóëåíèå ñ÷åò÷èêà ÷èñëà èòåðàöèé ***
      L=0
C *** öèêë óòî÷íåíèÿ êîîðäèíàò ïðèðàùåíèÿ ýêâèïîòåíöèàëè ***
 10   CONTINUE
      IF (RHO .LT. 1.E-10) GOTO 40
C *** ñëàáàÿ âàðèàöèÿ øàãà ***
      IF (DS .LE. 0.0001) RETURN
C *** èñ÷åðïàíèå êîëè÷åñòâà èòåðàöèé ***
      IF (L .GT. IEQQ) RETURN
C *** calculate ***
*      REAL*8 AO  - out  ! óãëîâàÿ êîìïîíåíòà ÂÌÏ, íîðì.
*      REAL*8 Y1  - inp  ! ïðîäîëüíàÿ êîîðäèíàòà òî÷êè .ñåò.åä.
*      REAL*8 Y2  - inp  ! ïîïåðå÷íàÿ êîîðäèíàòà òî÷êè .ñåò.åä.
*      REAL*8 BZ  - out  ! ïðîäîëüíàÿ êîìïîíåíò ÌÈ,íîðì.
*      REAL*8 BR  - out  ! ïîïåðå÷íàÿ êîìïîíåíò ÌÈ,íîðì.
      CALL  FORMAG (AO,ZETA,RHO,C1,C2)
C **** CALL  FORCM (AO,ZETA,RHO,C1,C2)
      DS = C1*C1+C2*C2
      IF (DS .LT. 0.0001) DS = 1.0
       DEL = FF/RHO-AO
C *** ïðîâåðêà ïðèáëèæåíèÿ çíà÷åíèÿ ôóíêöèè ê çàäàííîìó ***
      IF (DABS(DEL) .LE. 0.0001) RETURN
      DS = DEL / DS
C *** âûïîëíåíèå øàãà ïî êîîðäèíàòàì ***
      RHO=RHO+C1*DS
      ZETA=ZETA-C2*DS
      IF (DABS(C2) .LE. DABS(C1)) GOTO 20
      DS = C2*DS
      GOTO 30 
 20   DS = C1*DS
C *** óâåëè÷åíèå ñ÷åò÷èêà øàãîâ ***
 30   L=L+1
       GOTO 10 
 40   RETURN 1
C *** *** END DSPROM *** *** 
      END
!----------------------------------------
      SUBROUTINE EQUIPM(FZ,ND)
C *** CALCULATES EQUIPOTENTIAL LINE ***
*  INTEGER ND - inp ! 
*  REAL*4 FZ - inp  ! ïðîäîëüíàÿ êîîðäèíàòà , ñåò.åä.
!----------------------------------------
      IMPLICIT REAL*8 (A-H,O-Z)

      INCLUDE '_PARAM.FOR'
      INCLUDE '_COMON.FOR'

      REAL*8 FZ
      REAL*8 AO  ! óãëîâàÿ êîìïîíåíòà ÂÌÏ, íîðì.
      REAL*8 ZETA ! ïðîäîëüíàÿ êîîðäèíàòà òî÷êè, ñåò.åä.
      REAL*8 RHO  ! ïîïåðå÷íàÿ êîîðäèíàòà òî÷êè, ñåò.åä.
      REAL*8 BZ  ! ïðîäîëüíàÿ êîìïîíåíò ÌÈ,íîðì.
      REAL*8 BR  ! ïîïåðå÷íàÿ êîìïîíåíò ÌÈ,íîðì.

C *** SS DETERMINES SIGN (I.E., DIRECTION) OF EQUIPOTENTIAL LINE ***
      DO 50 LLL=1,2
        SS = 1.0
	   IF (LLL .EQ. 2) SS = -1.0
         DELT = 1./EQST
	   RHO=EQUIPR  !íà÷àëüíàÿ ïîïåðå÷íàÿ êîîðäèíàòà EQUIPOTENTIAL LINE, ñåò.åä.
	   ZETA=FZ	   !íà÷àëüíàÿ ïðîäîëüíàÿ êîîðäèíàòà EQUIPOTENTIAL LINE, ñåò.åä.
C *** calculate ***
*      REAL*8 AO  - out  ! óãëîâàÿ êîìïîíåíòà ÂÌÏ, íîðì.
*      REAL*8 ZETA- inp  ! ïðîäîëüíàÿ êîîðäèíàòà òî÷êè .ñåò.åä.
*      REAL*8 RHO - inp  ! ïîïåðå÷íàÿ êîîðäèíàòà òî÷êè .ñåò.åä.
*      REAL*8 BZ  - out  ! ïðîäîëüíàÿ êîìïîíåíò ÌÈ,íîðì.
*      REAL*8 BR  - out  ! ïîïåðå÷íàÿ êîìïîíåíò ÌÈ,íîðì.
	   CALL  FORMAG (AO,ZETA,RHO,BZ,BR)
C *** CALL FORCM (AO,ZETA,RHO,C1,C2)
	   TERM3=DABS(BZ)+DABS(BR)
	   IF (TERM3 .LT. 1.E-10) RETURN
       DELT1 = ND / DSQRT(BZ*BZ+BR*BR)
       RPROB=RHO +BR*DELT1*SS
       ZPROB=ZETA+BZ*DELT1*SS
C ***********************************************
      IF ( RPROB .GE. YV(IRL).OR. ZPROB .LT. XU(0)
     + .OR. ZPROB .GE. XU(IZL)) GOTO 50
C ***********************************************
       CALL  LISTM (SS,RHO,ZETA)
 50   CONTINUE
C *** *** END EQUIPM *** *** 
      END
!----------------------------------------
CC
CC Program GRA4WIN.FOR ñòðîèò èçîëèíèè ìàãíèòíîãî ïîëÿ
CC
      SUBROUTINE GRAF4WIN
      IMPLICIT REAL*8 (A-H,O-Z)
!----------------------------------------

      INCLUDE '_PARAM.FOR'
      INCLUDE '_COMON.FOR'

C     Main program variables
	  
      INTEGER     NOUT/0/
      REAL XMAX,YMAX,XMIN,YMIN,XL,YL
      REAL BMA,BMI,BUY
      REAL Z,R,XF,YF,Z2,R2,RF,XOF,YOF
      DATA NK4 /4/ !êàíàë çàïèñè ýêâèïîòåíöèàëè
      namelist/Q11/ AKAT,BKAT,DKAT,TKAT,WKAT,ZKAT

      OPEN  (UNIT=NOUT,FILE='CHECKERR.TXT',POSITION='APPEND')
      IF (MAG) THEN 
	     BMA=BZA(IZMAX)
	     BMI=0.0
	     DO 1 J=-6,IZMAX
           IF (BMI .GT. BZA(J))BMI=BZA(J)
 1         CONTINUE
	     DO 2 J=-6,IZMAX
           IF (BMA .LT. BZA(J))BMA=BZA(J)
 2         CONTINUE
	     BMA=BMA*BN
	     BMI=BMI*BN
	     EQLN=10 ! ìàêñèì.êîëè÷åñòâî èòåðàöèé óòî÷íåíèÿ êîîðäèíàò
	     EQST=4  ! êîëè÷åñòâî äåëåíèé íà÷àëüíîãî øàãà ýêâèïîòåíöèàëè
	     BUY=(BMA-BMI)/2.
	     write (NOUT,*)' BMI=',BMI,' TL       BMA=',BMA,' TL'
      ENDIF

C! Îïðåäåëåíèå ÎÁËÀÑÒÈ,ÏÐÅÄÅËÎÂ,ÑÒÐÀÍÈÖÛ,ÎÑÅÉ X,Y
      XMAX=XU(IZL)
      YMAX=YV(IRL)
      XMIN=XU(0)
      YMIN=YV(0)
      XL=(XMAX-XMIN)*S
      YL=(YMAX-YMIN)*S
      CALL gf_setnp(1200, 600)
      CALL page (30.0, 18.0, 'MEDIAN PLANE', 12, 1)
      call setpen (7)

      IF (MAG) THEN 
      XMIN=XMIN*Z0
      XMAX=XMAX*Z0
      CALL REGION
     + (2.3,YL+2.9,XL,4.0,'AXES MAGNETIC FIELD DISTRIBUTION',32,1)
      CALL limits (XMIN, XMAX, BMI, BMA)
C! call xaxis (XU(0), 0,0, (XMAX-XMIN)/5., 1, 0, 1)
C! call Yaxis (0.0, 0,0, BUY, 2, 0, 1)
      call axes ('X,m', 3, 0.0, 1, 'BM,TL', 5, BUY, 1, 11)
      call setpen (5)
      Z=XU(0)*Z0
      R=SNGL(BZA(0))*BN
      call tmf (Z,R,XF,YF)
      CALL MOVE(XF,YF,0)
      DO 7 I=1,IZL
	     Z=XU(I)*Z0
	     R=SNGL(BZA(I))*BN
	     call tmf (Z,R,XF,YF)
	     CALL FATLIN(XF,YF,1)
 7    CONTINUE
      ENDIF

C! Îïðåäåëåíèå ÎÁËÀÑÒÈ,ÏÐÅÄÅËÎÂ,ÑÒÐÀÍÈÖÛ,ÎÑÅÉ X,Y
      XMAX=XU(IZL)
      YMAX=YV(IRL)
      XMIN=XU(0)
      YMIN=YV(0)
      XL=(XMAX-XMIN)*S
      YL=(YMAX-YMIN)*S

      CALL REGION (2.3, 1.4, XL, YL, 'IZOLIN', 6, 1)
      CALL LIMITS (XMIN, XMAX, YMIN, YMAX)
C! Ïîñòðîåíèå êîíòóðà ÎÁËÀÑÒÈ
      CALL MOVE(2.3, 1.0, 0)
      DO 20 I = 1, ABS(MS)
      IM1=I-1
      IF (I.EQ.1) IM1=ABS(MS)
      R2=BDR(I)
      Z2=BDZ(I)
      CALL TMF (Z2,R2,XF,YF)
      IF (I.EQ. 1) CALL MOVE(XF,YF,0)
      IF (MI.LT.0) THEN
!
! Ïîñòðîåíèå ãðàíè÷íûõ òî÷åê êîíòóðà ÎÁËÀÑÒÈ
!
         CALL MOVE(XF,YF,0)
         CALL MARKER(2)
      ELSE
!
! Ïîñòðîåíèå êîíòóðà ÎÁËÀÑÒÈ
!
      IF (REX(IM1) .NE. 0.) THEN
      N=REX(IM1)/DABS(REX(IM1))
!
!	îïðåäåëåíèå 
!
	 CALL CORCR(BDZ(IM1),BDR(IM1),BDZ(I),BDR(I),XCN,YCN,REX(IM1))
       Z2=XCN
       R2=YCN
	 CALL TMF (Z2,R2,XOF,YOF)
	 RF = SQRT((XF-XOF)**2+(YF-YOF)**2)
	 IF (N .EQ. -1) RF = -RF
	 CALL ARCIB( RF, XF, YF, 0)
      ELSE
         CALL MOVE(XF,YF,1)
      END IF
      END IF
 20   CONTINUE

	DO 50 J=IZZ,1,-1
      IF (MOD(J,MZ).EQ.0 .OR. J.EQ.1) THEN
	     REWIND (NK4)
       Z= UX(J)
       IF(NAMCAT.EQ. 'CATOD1' )Z=0. 5*( Z+UX(J-1) )
       R= VY(J)
       IF(NAMCAT.EQ.'CATOD1')R=0.5*(R+VY(J-1))
	     EQUIPR=R
	     FF=PCIMG(J)
		 !*BN*Z0*Z0
C *** CALCULATES EQUIPOTENTIAL LINE ***
*  REAL*4 Z - inp  ! ïðîäîëüíàÿ êîîðäèíàòà òî÷êè, íîðì.
*  INTEGER ND - inp  ! 
	     CALL EQUIPM(Z,1)
! Ïîñòðîåíèå ýêâèïîòåíöèàëè, ìîæåò ñîñòîÿòü èç íåñêîëüêèõ ÷àñòåé
           REWIND (NK4)
 81        READ (NK4, END=85) FF,MR,(PX(I),PY(I), I=1,MR)
	     write (NOUT,*)' FF=',FF,' Wb=TL*m*m',MR,' points  LINE=',J
	     CALL setpen (NUMB)
	     Z=PX(1)
	     R=PY(1)
	     CALL tmf (Z,R,XF,YF)
	     CALL move(XF,YF,0)
      DO 83 I=2,MR
	     Z=PX(I)
	     R=PY(I)
	     CALL tmf (Z,R,XF,YF)
	     CALL move(XF,YF,1)
 83   CONTINUE
      END IF
      GOTO  81
 85   CONTINUE
 50   CONTINUE
      XMAX=XMAX*Z0
      YMAX=YMAX*Z0
      XMIN=XMIN*Z0
      YMIN=YMIN*Z0
      CALL limits (XMIN, XMAX, YMIN, YMAX)
      CALL setpen (7)
      CALL axes ('X,m', 3, 0.0, 1,'Y,m', 3, 0.05, 1, 00)
      CALL ENDPG (0)

      write (NOUT,Q11)
      write (NOUT,'(''AKAT(3)= âíóòðåíèé ðàäèóñ êàòóøêè,m'')')
      write (NOUT,'(''BKAT(3)= âíåøíèé ðàäèóñ êàòóøêè,m'')')
      write (NOUT,'(''DKAT(3)= ïîëóäëèíà êàòóøêè,m'')')
      write (NOUT,'(''AKAT(3)= âíóòðåíèé ðàäèóñ êàòóøêè,m'')')
      write (NOUT,'(''TKAT(3)= òîê â  âèòêå , À'')')
      write (NOUT,'(''WKAT(3)= êîë-âî âèòêîâ'')')
      write (NOUT,'(''ZKAT(3)= êîîðäèíàòà öåíòðà êàòóøêè,m'')')
      write (NOUT,'('' mastab'',E10.3,'' BMA='',E10.3,''Òë'')')S,BMA
      write (NOUT,'('' BMA='',E11.5,'' BMI='',E11.5)')BMA,BMI
      write (NOUT,'(''îñåâîå ðàñïðåäåëåíèå ìàãíèòíîé èíäóêöèè, Òë'')')
      write (NOUT,'(F7.3,1H:,G14.7,2H ;)')
     &  (Z0*(I+XU(0)),BZA(I)*BN,I=-6, IZMAX)

      PRINT '(100(1H*))'
      PRINT '('' MEANS OF FLUX LINES AT CATHOD,[Wb=TL*m*m]''
     + /(10G12.3))',C
88888 CLOSE (UNIT=NOUT, STATUS = 'KEEP')
      RETURN
C *** *** END  GRAF4WIN *** *** 
      END
!----------------------------------------
      SUBROUTINE LISTM(SS,RHO,ZETA)
C *** LISTS AND DRAWS EQUIPOTENTIAL LINE ***
!----------------------------------------
      IMPLICIT REAL*8 (A-H,O-Z)

      INCLUDE '_PARAM.FOR'
      INCLUDE '_COMON.FOR'

      DATA NK4 /4/ !êàíàë çàïèñè ýêâèïîòåíöèàëè

 10   FORMAT
     *(' EQUIPOTENTIAL LINE AT Z=',F8.3,' WITH POTENTIAL V=',F8.3,
     * ' NUmbER points of LINE NK=',I3)
       J=1
       ZSTART=ZETA
 20    CALL DSPROM( EQLN, .TRUE., AO, RHO, ZETA, C1, C2, *30)
       IF ( ZETA .GT.XU(IZL).OR. ZETA .LT.XU(0)) GOTO 25
       IF (  RHO .GT.YV(IRL).OR.  RHO .LT.YV(0)) GOTO 25
       PX(J) = ZETA
       PY(J) = RHO
       J=J+1
       TERM3=DABS(C1)+DABS(C2)
       IF (TERM3 .LT. 1.E-10) GOTO 27
       IF ( J .GT. MT ) GOTO 30
       DELT1 = DELT/ DSQRT(C1*C1+C2*C2)
       RHO=RHO+C2*DELT1*SS
       ZETA=ZETA+C1*DELT1*SS
       GOTO 20 
 25   CONTINUE
       GOTO 30 
 27   CONTINUE
C *** öèêë ïîñòðîåíèÿ ýêâèïîòåíöèàëè çàâåðøåí ***
 30   J=J-1
       IF ( J .LE. 1 ) GOTO 40
      WRITE (NK4) FF,J,(PX(I),PY(I), I=1,J)
!     	WRITE(9,10) ZSTART,FF,J
 40   CONTINUE
      RHO=EQUIPR
	ZETA=ZSTART
C *** *** END  LISTM *** *** 
      END
