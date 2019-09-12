!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
! Este modulo es parte del modelo trj.
! Subrutinas auxiliares para obtener las derivadas temporales usadas en
! odeint
! dx/dt y dy/dt, Coordenadas locales curvilineas.
! Autor : Hernán H. G. Braile
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!----------------------------------------------------------------------
!        y(1) = x
!        y(2) = y
!----------------------------------------------------------------------
      SUBROUTINE derivstr(time,y,Dydt)
!----------------------------------------------------------------------
      USE particle, ONLY : U_part,V_part,lat_part,long_part,latcheck2,&
                           xstr_part,ystr_part, update
      USE parametros, ONLY : DegToR, Rt

!----------------------------------------------------------------------
!     Argumetos
      IMPLICIT none
      REAL Dydt(2) , time , y(2)
      REAL Ustr,Vstr,rlong,rlat
!----------------------------------------------------------------------
      CALL str_to_ang(rlong,rlat,y(1),y(2))
      long_part=rlong/DegToR
      lat_part=rlat/DegToR

      CALL Latcheck2()
      CALL update()

      CALL strwinds(rlong,rlat,U_part,V_part,&
               Ustr,Vstr)
      Dydt(1)=Ustr
      Dydt(2)=Vstr

      END SUBROUTINE derivstr
!----------------------------------------------------------------------

!----------------------------------------------------------------------
!        y(1) = x
!        y(2) = y
!----------------------------------------------------------------------
      SUBROUTINE derivstrN(time,y,Dydt)
!----------------------------------------------------------------------
      USE particle, ONLY : U_part,V_part,lat_part,long_part,latcheck2,&
                           xstr_part,ystr_part, update
      USE parametros, ONLY : DegToR, Rt

!----------------------------------------------------------------------
!     Argumetos
      IMPLICIT none
      REAL Dydt(2) , time , y(2)
      REAL Ustr,Vstr,rlong,rlat
!----------------------------------------------------------------------
      CALL str_to_angN(rlong,rlat,y(1),y(2))
      long_part=rlong/DegToR
      lat_part=rlat/DegToR

      CALL Latcheck2()
      CALL update()

      CALL strwindsN(rlong,rlat,U_part,V_part,&
               Ustr,Vstr)
      Dydt(1)=Ustr
      Dydt(2)=Vstr

      END SUBROUTINE derivstrN
!----------------------------------------------------------------------
