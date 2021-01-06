!CC    E.for - For MS FORTRAN 5.1
!CC
!CC    11.10.89
      INCLUDE  'FGRAPH.FI'
!----------------------------------------
      SUBROUTINE GRAFO
!----------------------------------------
!
      IMPLICIT REAL*8 (A-H,O-Z)
      INCLUDE  'FGRAPH.FD'
      INCLUDE '_PARAM.FOR'
      INCLUDE '_COMMON.FOR'


      REAL*8 X1,Y1,X2,Y2,X
      REAL*8 XMAX,YMAX,XMIN,YMIN,XL,YL
      INTEGER*2      i, status, ncolor
      INTEGER*4      in4, oldcolor
      RECORD /wxycoord/ wxy
      RECORD / videoconfig / vc

      LOGICAL*2   finvert / .TRUE. /
      DATA NK1 /1/ !канал записи потенциала
      DATA NK2 /2/ !канал записи траекторий
      DATA oldcolor / 2 /    ! full grean


      IF (MI .EQ. 0) RETURN
!
!     Find graphics mode.
!
      IF( setvideomode( $MAXRESMODE ) .EQ. 0 ) 
     +    STOP 'Error:  cannot set graphics mode'
      CALL getvideoconfig( vc )
            ! Turn off currsor
            iDummy = displaycursor($GCURSOROFF)

      CALL SETTEXTWINDOW( INT2(1), INT2(2), INT2(40), INT2(40))
      status = SETCOLOR( oldcolor ) ! full grean
      WRITE (9,'('' mastab'',E10.3)')S
! Определение ПРЕДЕЛОВ ОБЛАСТИ

      XMAX=FLOAT(IZL)
      YMAX=FLOAT(IRL)
      XMIN=0.0
      YMIN=0.0
      XL=XMAX*S
      YL=YMAX*S
! Определение ОБЛАСТИ

      status = SETWINDOW( finvert , XMIN,YMIN, XMAX,YMAX)

! Построение ОСЕЙ X,Y

      oldcolor = 3
      status = SETCOLOR( oldcolor ) ! full green for axes
!
!	    Draw vertical and horizontal lines. Vertical line style
!	    is anything other than horizontal style. Since lines
!	    are overdrawn with several line styles, this has the
!	    effect of combining colors and styles.
!
!	    status = #F00F ! X * 256.0
CALL setlinestyle( status)  
      DO i = 0, IZL
         x=FLOAT(i)
         CALL MOVETO_W( x, YMIN, wxy )
         status = LINETO_W( x, YMAX )
      END DO
      DO i = 0, IRL
         x=FLOAT(i)
         CALL MOVETO_W( XMIN, x, wxy )
         status   = LINETO_W(  XMAX, x )
      END DO

! Построение контура ОБЛАСТИ

      status = SETCOLOR( $MAGENTA )
      IO=0
      DO I = 1, ABS(MS)
      I1=I-1
      IF (I.EQ.1) I1=ABS(MS)
      R1=BDR(I1)
      Z1=BDZ(I1)
      N=IEX(I1)
      R2=BDR(I)
      Z2=BDZ(I)
      IF (I.EQ. 1) CALL MOVETO_W( Z2, R2, wxy )
      IF (MI.LT.0) THEN
! Построение граничных точек контура ОБЛАСТИ
	     X1=Z1+S
	     Y1=R1+S
	     X2=Z1-S
	     Y2=R1-S
	     status = ELLIPSE_W( $GFILLINTERIOR, X1, Y1, X2, Y2)
!	    Returns:  a nonzero value if the ellipse is drawn successfully,
!	    or 0 if not.
	   IF(status.EQ.0)STOP'Error:the ellipse is drawn successfully'  
      ELSE
! Построение контура ОБЛАСТИ
        IF (N .NE. 0) THEN 
         IO=IO+1
         X1=XOK(IO)+ROK(IO)
         Y1=YOK(IO)+ROK(IO)
         X2=XOK(IO)-ROK(IO)
         Y2=YOK(IO)-ROK(IO)
        IF(N .EQ.-1)status = ARC_W( X1, Y1, X2, Y2, Z2, R2, Z1, R1)
        IF(N .EQ. 1)status = ARC_W( X1, Y1, X2, Y2, Z1, R1, Z2, R2 )
         CALL MOVETO_W( Z2, R2, wxy )
        ELSE
         status = LINETO_W( Z2, R2)
        END IF
      END IF
      END DO
!
! Построение эквипотенциалей поля 
!
      IF (ABS(MI) .EQ. 1) GOTO 100
      DO i = 0, IRL
         R1=FLOAT(i)
         DO j = 0, IZL
	     Z1=FLOAT(j)
	     IF (IFD(j+1+i*IZL1) .EQ. 10)CYCLE
	     in4 = F(j+1+i*IZL1) * vc.numcolors
	     ncolor = MOD( in4, vc.numcolors )
	     status = SETCOLOR( ncolor )
	     X1=Z1+0.5
	     Y1=R1+0.5
	     X2=Z1-0.5
	     Y2=R1-0.5
	     status = rectangle_w( $GFILLINTERIOR, X1, Y1, X2, Y2)
         END DO
      END DO
!----------------------------------------------
      REWIND (NK1)
!      IDIM1=IRL1*IZL1
!к-во узлов сетки потенциала и матрицы плотности
!      READ (NK1) NITER,(F(I),I=1,IDIM1)
!
! Построение траекторий частиц
!
      IF (ABS(MI) .EQ. 2) GOTO 100
      status = SETCOLOR( $RED)
!
! Считывание массива траекторий частиц
!
       REWIND (NK2)
       DO I=1,IZZ
	   IF (MOD(I,M4).EQ.0 .OR. I.EQ.1) THEN
	     READ (NK2, END=85) MTM,(PX(J),PY(J),J=1,MTM)
	     IF (MTM.GT.1) THEN
	     Z1=PX(1)
	     R1=PY(1)
	     X1=Z1+S
	     Y1=R1+S
	     X2=Z1-S
	     Y2=R1-S
	     status = ELLIPSE_W( $GFILLINTERIOR, X1, Y1, X2, Y2)
!	    Returns:  a nonzero value if the ellipse is drawn successfully,
!	    or 0 if not.
	     IF(status.EQ.0) STOP 'Error:  cannot set graphics mode'

             CALL MOVETO_W( Z1, R1, wxy )
             DO j=2,MTM
		Z1=PX(j)
		R1=PY(j)
                status = LINETO_W( Z1, R1)
             END DO
             END IF
	   END IF
       END DO
 85    CONTINUE
!
! Построение распределения потенциала на оси
!
      IF (ABS(MI).EQ.3) GOTO 100
!     Draw a rectangle around the graph.
!
 100  status = RECTANGLE_W( $GBORDER, XMIN,YMIN, XMAX,YMAX)
      PAUSE
!
! Построение распределения потенциала на оси
!
      WRITE (9,*)	'AXES POTENTIAL DISTRIBUTION'
      WRITE (9,'(10G12.3)')(XU(I-1),F(I),I=1,IZL1)
      RETURN
      END !SUBROUTINE GRAFO
