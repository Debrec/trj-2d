!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
! Enero 2015
! Este modulo es parte del modelo trj.
! Subrutinas auxiliares para obtener las derivadas temporales usadas en odeint
! dx/dt y dy/dt, Coordenadas locales curvilineas.
! Autor : Hernán H. G. Braile
! Email : hbraile@gmail.com
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!        y(1) = x
!        y(2) = y
!-----------------------------------------------------------------------
      SUBROUTINE derivs(time,y,Dydt)
!-----------------------------------------------------------------------
      USE particle, ONLY : U_part,V_part,lat_part,long_part,latcheck2
      USE thsurf, ONLY : update
      USE parametros, ONLY : DegToR, Rt

!-----------------------------------------------------------------------
!     Argumetos
      IMPLICIT none
      REAL Dydt(2) , time , y(2)
!-----------------------------------------------------------------------
      lat_part=y(2)
      long_part=y(1)
      CALL Latcheck2()
      CALL update()
      Dydt(1)=U_part/((Rt)*COS(lat_part*DegToR)*DegToR)
      Dydt(2)=V_part/((Rt)*DegToR)

      END SUBROUTINE derivs
!-----------------------------------------------------------------------
