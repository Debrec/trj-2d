!----------------------------------------------------------------------
!----------------------------------------------------------------------
! Septiembre , 2019
! Modulos Para el calculo de trayectorias
! Autor : Hernán H. G. Braile
! Email : debrec28@gmail.com
!----------------------------------------------------------------------
!----------------------------------------------------------------------
!----------------------------------------------------------------------
!               Modulos del programa trayectoria
!               Incluye:
!                    --> parametros
!                    --> particle
!                    --> thsurf
!
!----------------------------------------------------------------------

!//////////////////////////////////////////////////////////////////////

!----------------------------------------------------------------------
!           Rt = radio medio de la tierra en metros (m)
!           Pi = el numero Pi
!           DegToR= pasa de grados a radianes
!----------------------------------------------------------------------
MODULE parametros
  IMPLICIT none
  REAL Rt,DegToR,Pi

  parameter (Rt=6.371229E+06)
  parameter (Pi=3.14159265358979)
  parameter (DegToR=0.01745329251994)

END MODULE parametros
!------------------------------------------------------------------------

!////////////////////////////////////////////////////////////////////////

!------------------------------------------------------------------------
!      PROPIEDADES DE LAS PARTI!ULAS
!
!      U_part,V_part velocidades en (m/s)
!      lat_part, long_part latitud y longitud en grados
!      Alt_part Altura en (m)
!      P_part Presion (mb)
!      T_part Temperatura (k)
!      Theta_part Temperatura potencial en (k)
!
!------------------------------------------------------------------------
!------------------------------------------------------------------------
MODULE particle
  IMPLICIT none
  REAL, SAVE :: U_part,V_part
  REAL, SAVE :: lat_part , long_part, Alt_part
  REAL, SAVE :: xstr_part,ystr_part
  REAL, SAVE :: P_part, T_part, Theta_part,pv_part,o3_part

  PUBLIC :: update

  !----------------------------------------------------------------------
CONTAINS
  !----------------------------------------------------------------------

  !----------------------------------------------------------------------
  !     lat = latitude en grados
  !     long = longitud en grados
  !     z = altura geometrica en metros
  !     U , V velocidades zonales y meridional
  !     ti,tf tiempos iniciales y finales
  !----------------------------------------------------------------------
  SUBROUTINE trayect(z,ti,tf)

    USE parametros
    !-----------------------------------------------------------------------
    IMPLICIT none
    REAL z,ti,tf

    !-----------------------------------------------------------------------
    EXTERNAL derivs,ode,derivstr,derivstrN
    REAL latf,longf,xstrf,ystrf,rlong,rlat,pf
    INTEGER Nok,Nbad,i

    IF((lat_part.GT.(-75)).AND.(lat_part.LT.75)) THEN
			 CALL ode(long_part,lat_part,longf,latf,ti,tf,derivs)
			 lat_part=latf
       long_part=longf
	     CALL ang_to_str(long_part*DegToR,lat_part*DegToR,&
            xstr_part,ystr_part)
    ELSE IF(lat_part.LE.(-75)) THEN
       CALL ang_to_str(long_part*DegToR,lat_part*DegToR,xstr_part,ystr_part)
       CALL ode(xstr_part,ystr_part,xstrf,ystrf,ti,tf,derivstr)
       xstr_part=xstrf
       ystr_part=ystrf
       CALL str_to_ang(rlong,rlat,xstr_part,ystr_part)
       long_part=rlong/DegToR
       lat_part=rlat/DegToR
    ELSE IF(lat_part.GE.75) THEN
       CALL ang_to_strN(long_part*DegToR,lat_part*DegToR,xstr_part,ystr_part)
       CALL ode(xstr_part,ystr_part,xstrf,ystrf,ti,tf,derivstrN)
       xstr_part=xstrf
       ystr_part=ystrf
       CALL str_to_angN(rlong,rlat,xstr_part,ystr_part)
       long_part=rlong/DegToR
       lat_part=rlat/DegToR
    END IF

  END SUBROUTINE trayect
  !----------------------------------------------------------------------
  !----------------------------------------------------------------------
  SUBROUTINE rangcheck(olong,olat)
    IMPLICIT none
    REAL olong, olat

    olat=lat_part
    olong=long_part
    DO WHILE((olat.GT.90).OR.(olat.LT.(-90)))

       IF(olat.GT.90) THEN
          olat=180.-olat
          olong=180.+olong
       END IF
       IF(olat.LT.(-90)) THEN
          olat=-180.-olat
          olong=180.+olong
       END IF

    END DO

    DO WHILE((olong.GT.360).OR.(olong.LT.0))
       IF(olong.GT.360)olong=olong-360.
       IF(olong.LT.0)olong=olong+360.
    END DO

  END SUBROUTINE rangcheck

  SUBROUTINE Latcheck2()
    IMPLICIT none

  END SUBROUTINE Latcheck2

  SUBROUTINE update()
    use interpolacion, ONLY : intp2d
    use winds, ONLY : long_surf,lat_surf,U_surf, V_surf,P_surf, T_surf, nx, ny, &
                      th_surf
    IMPLICIT none
    REAL olat,olong
    INTEGER intpm
    !EXTERNAL interp2

    CALL rangcheck(olong,olat)

    Theta_part=th_surf

    intpm=2

    IF(intpm.EQ.1) THEN
  !     CALL interp2P(olong,olat,U_part,long_surf(1:(nx+1),1),&
  !          lat_surf(1,1:ny),U_surf,(nx+1),ny)

  !     CALL interp2P(olong,olat,V_part,long_surf(1:(nx+1),1),&
  !          lat_surf(1,1:ny),V_surf,(nx+1),ny)

  !     CALL interp2P(olong,olat,T_part,long_surf(1:(nx+1),1),&
  !          lat_surf(1,1:ny),T_surf,(nx+1),ny)

  !     CALL interp2P(olong,olat,P_part,long_surf(1:(nx+1),1),&
  !          lat_surf(1,1:ny),P_surf,(nx+1),ny)

       ! CALL interp2P(olong,olat,Alt_part,long_surf(1:nx,1),&
       !      lat_surf(1,1:ny),Alt_surf,nx,ny)
       !
       ! CALL interp2P(olong,olat,pv_part,long_surf(1:nx,1),&
       !      lat_surf(1,1:ny),pv_surf,nx,ny)
       !
       ! CALL interp2P(olong,olat,o3_part,long_surf(1:nx,1),&
       !      lat_surf(1,1:ny),o3_surf,nx,ny)

    ELSE IF (intpm.EQ.2) THEN
       U_part = intp2d(olong,olat,long_surf(1:(nx+1),1),&
            lat_surf(1,1:ny),U_surf,(nx+1),ny)

       V_part = intp2d(olong,olat,long_surf(1:(nx+1),1),&
            lat_surf(1,1:ny),V_surf,(nx+1),ny)

       T_part = intp2d(olong,olat,long_surf(1:(nx+1),1),&
            lat_surf(1,1:ny),T_surf,(nx+1),ny)

       P_part = intp2d(olong,olat,long_surf(1:(nx+1),1),&
            lat_surf(1,1:ny),P_surf,(nx+1),ny)

       ! Alt_part = intp2d(olong,olat,long_surf(1:nx,1),&
       !      lat_surf(1,1:ny),Alt_surf,nx,ny)
       !
       ! pv_part = intp2d(olong,olat,long_surf(1:nx,1),&
       !      lat_surf(1,1:ny),pv_surf,nx,ny)
       !
       ! o3_part = intp2d(olong,olat,long_surf(1:nx,1),&
       !      lat_surf(1,1:ny),o3_surf,nx,ny)
    ELSE
      WRITE(*,*) 'Método de interpolación desconocido'
      STOP
    END IF

  END SUBROUTINE update

END MODULE particle
!----------------------------------------------------------------------

!//////////////////////////////////////////////////////////////////////

!----------------------------------------------------------------------
