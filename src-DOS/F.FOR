!CC    F.For MS FORTRAN 5.1
!CC
!CC    11.10.89
!----------------------------------------
      SUBROUTINE READL1
!----------------------------------------
!
      IMPLICIT REAL*8 (A-H,O-Z)
      INCLUDE '_PARAM.FOR'
      INCLUDE '_COMMON.FOR'

      WRITE (*,*)  'INSERT THE NUMBER OF THE RECORD'
      READ    (*,*)    NUMBRC
      WRITE (*,*) 'NUMBRC=',NUMBRC
      REWIND 1
      READ(1) NITER,(F(J),J=1,IRL1*IZL1),UVOX
      WRITE (*,*)  'NITER=',NITER
      END
