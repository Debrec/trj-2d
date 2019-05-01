SUBROUTINE trj
  USE trj_main_mod
  IMPLICIT none
  INTEGER I

  CALL trj_init

  DO I=1,Ntime-1
     CALL trj1(I)
  END DO

  OPEN(13,FILE='input/temp_trj.dat')
  DO I=1,Npart
     WRITE(13,*) Long(I,2),  Lat(I,2)
  END DO
  CLOSE(13)
  OPEN(13,FILE='output/temp_trj_3d.dat')
  DO I=1,Npart
     WRITE(13,*) Long(I,2),  Lat(I,2), P(I,2)
  END DO
  CLOSE(13)

  CALL trj_finish

END SUBROUTINE trj
