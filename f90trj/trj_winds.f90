!----------------------------------------------------------------------
!----------------------------------------------------------------------
! Febrero , 2015
! Guarda archivos de viento y trazadores, lee campos de meteorologicos de
! archivos del ECMWF
! Autor : Hernán H. G. Braile
! Email : hbraile@gmail.com
!----------------------------------------------------------------------
!----------------------------------------------------------------------
MODULE winds

  !----------------------------------------------------------------------
  IMPLICIT none
  SAVE
  REAL, ALLOCATABLE :: U(:,:,:,:),V(:,:,:,:)
  REAL, ALLOCATABLE :: PV(:,:,:,:),O3(:,:,:,:)
  REAL, ALLOCATABLE :: T(:,:,:,:), P(:),lon(:),lat(:),time(:)

  REAL, ALLOCATABLE :: Us(:,:,:),Vs(:,:,:)
  REAL, ALLOCATABLE :: PVs(:,:,:),O3s(:,:,:)
  REAL, ALLOCATABLE :: Ts(:,:,:), Ps(:,:,:)

  INTEGER :: nx, ny, nz, nt

  CHARACTER*40, PUBLIC :: filein,filetrc


  PUBLIC readd, read_curr, updatew, winds_finish

  !----------------------------------------------------------------------
CONTAINS
  !----------------------------------------------------------------------

  !read data from netcdf file, currently, only U, V y T
  SUBROUTINE readd
    USE trj_netcdf
    IMPLICIT none

    INTEGER I,J,k,l,nro(4)

    WRITE(6,*) 'Reading data for file: ',filein
    !Read data lengs and store it in nro array.
    CALL dim_ecmwf_nc_file(filein, &
         'longitude',nro(1))
    CALL dim_ecmwf_nc_file(filein, &
         'latitude',nro(2))
!    CALL dim_ecmwf_nc_file(filein, &
!              'level',nro(3))
    CALL dim_ecmwf_nc_file(filein, &
         'level',nro(3))
    CALL dim_ecmwf_nc_file(filein, &
         'time',nro(4))

    nt=nro(4); nx=nro(1); ny=nro(2); nz=nro(3)

    !Allocate space for matrix data
    ALLOCATE(U(nx,ny,nz,2),V(nx,ny,nz,2))
    ALLOCATE(PV(nx,ny,nz,2),T(nx,ny,nz,2),O3(nx,ny,nz,2))
    ALLOCATE(lon(nx),lat(ny),P(nz))
    ALLOCATE(time(nro(4)))

    ALLOCATE(Us(nx,ny,2),Vs(nx,ny,2))
    ALLOCATE(PVs(nx,ny,2),Ts(nx,ny,2))
    ALLOCATE(O3s(nx,ny,2),Ps(nx,ny,2))

    !Read dimensions data
    CALL read_dim_ecmwf_nc_file(filein, &
         'longitude',lon,nx)
    CALL read_dim_ecmwf_nc_file(filein, &
         'latitude',lat,ny)
!    CALL read_dim_ecmwf_nc_file(filein, &
!         'level',P,nz)
    CALL read_dim_ecmwf_nc_file(filein, &
          'level',P,nz)
    CALL read_dim_ecmwf_nc_file(filein, &
         'time',time,nt)

  END SUBROUTINE readd

  SUBROUTINE read_curr(itime)
    USE trj_netcdf
    IMPLICIT none
    INTEGER, INTENT(IN) :: itime
    INTEGER :: nro(4),it1,it2
    CHARACTER*7 units

    it1=itime; it2=itime+1

    nro=(/nx, ny, nz, nt/)

    !Read data
    CALL read_ecmwf_nc_file(filein,'t',T,units,nro,it1,it2)
  	CALL read_ecmwf_nc_file(filein,'u',U,units,nro,it1,it2)
    CALL read_ecmwf_nc_file(filein,'v',V,units,nro,it1,it2)
    CALL read_ecmwf_nc_file(filetrc,'pv',PV,units,nro,it1,it2)

  END SUBROUTINE read_curr

  !Used to interpolate the winds fields to selected, potential
  !temperature surface, and times.
  SUBROUTINE updatew(theta,rtime)
    USE ptotheta
    USE thsurf
    USE parametros
    IMPLICIT none
    REAL, INTENT(IN):: theta,rtime      ! selected potential temperature level

    REAL Tpr(nz),Ppr(nz),Varpr(nz)  ! Store temporal profiles
    REAL rt1,rt2             ! serial date,
    INTEGER ttime,I,J,Z,t1,itime
    !Get the initial time of the simulation, not alway the same
    !as the meteorological data

    !Interpolate data to requiered time and potential temperature
    IF((rtime.LT.time(1)).OR.(rtime.GE.time(nt))) THEN
       WRITE(0,*) 'Data outside range on updatew'
       WRITE(0,*) 'time(1)',time(1),'time(nt_f)',time(nt),&
            'rtime',rtime
       STOP
    END IF

    DO I=1,nt-1
       IF((rtime.GE.time(I)).AND.(rtime.LE.time(I+1))) THEN
          t1=I
          rt1=time(I)
          rt2=time(I+1)
       END IF
       IF(time(I).GE.time(I+1)) THEN
          WRITE(0,*) 'Data outside range on updatew 2'
          WRITE(0,*) 'time(I)',time(I),'time(I+1)',time(I+1)
          STOP
       END IF
    END DO

    !WRITE(*,*) rt1,rt2,rtime,t1
    itime=t1
    CALL read_curr(itime)

	t1=itime
    DO I=1,nx
       DO J=1,ny
          Ppr(1:nz)=P(1:nz)
          Tpr(1:nz)=T(I,J,1:nz,1)
          Varpr(1:nz)=T(I,J,1:nz,1)
          Ts(I,J,1)=p2theta(Ppr,Tpr,theta,Varpr,nz)
          Varpr(1:nz)=P(1:nz)
          Ps(I,J,1)=p2theta(Ppr,Tpr,theta,Varpr,nz)
          Varpr(1:nz)=PV(I,J,1:nz,1)
          PVs(I,J,1)=p2theta(Ppr,Tpr,theta,Varpr,nz)
          !Varpr(1:nz)=O3(I,J,1:nz,1)
          !O3s(I,J,1)=p2theta(Ppr,Tpr,theta,Varpr,nz)
          Varpr(1:nz)=U(I,J,1:nz,1)
          Us(I,J,1)=p2theta(Ppr,Tpr,theta,Varpr,nz)
          Varpr(1:nz)=V(I,J,1:nz,1)
          Vs(I,J,1)=p2theta(Ppr,Tpr,theta,Varpr,nz)

          Ppr(1:nz)=P(1:nz)
          Tpr(1:nz)=T(I,J,1:nz,2)
          Varpr(1:nz)=T(I,J,1:nz,2)
          ! va en todos lados t1+1 ? o en las coordenadas t1
          Ts(I,J,2)=p2theta(Ppr,Tpr,theta,Varpr,nz)
          Varpr(1:nz)=P(1:nz)
          Ps(I,J,2)=p2theta(Ppr,Tpr,theta,Varpr,nz)
          Varpr(1:nz)=PV(I,J,1:nz,2)
          PVs(I,J,2)=p2theta(Ppr,Tpr,theta,Varpr,nz)
          !Varpr(1:nz)=O3(I,J,1:nz,2)
          !O3s(I,J,2)=p2theta(Ppr,Tpr,theta,Varpr,nz)
          Varpr(1:nz)=U(I,J,1:nz,2)
          Us(I,J,2)=p2theta(Ppr,Tpr,theta,Varpr,nz)
          Varpr(1:nz)=V(I,J,1:nz,2)
          Vs(I,J,2)=p2theta(Ppr,Tpr,theta,Varpr,nz)
       END DO
    END DO

    IF(REAL(time(t1)).NE.rtime) THEN
       th_surf=theta
       DO I=1,nx
          DO j=1,ny
             T_surf(I,J)=intp1d(rt1,rt2,rtime,Ts(I,J,:))
             P_surf(I,J)=intp1d(rt1,rt2,rtime,Ps(I,J,:))
             PV_surf(I,J)=intp1d(rt1,rt2,rtime,PVs(I,J,:))
             !O3_surf(I,J)=intp1d(rt1,rt2,rtime,O3s(I,J,:))
             U_surf(I,J)=intp1d(rt1,rt2,rtime,Us(I,J,:))
             V_surf(I,J)=intp1d(rt1,rt2,rtime,Vs(I,J,:))
             lat_surf(I,J)=lat(J)
             long_surf(I,J)=lon(I)
          END DO
       END DO
       DO J=1,ny
          T_surf(nx+1,J)=intp1d(rt1,rt2,rtime,Ts(1,J,:))
          lat_surf(nx+1,J)=lat(1)
          long_surf(nx+1,J)=360.
          P_surf(nx+1,J)=intp1d(rt1,rt2,rtime,Ps(1,J,:))
          pv_surf(nx+1,J)=intp1d(rt1,rt2,rtime,PVs(1,J,:))
         ! O3_surf(nx+1,J)=intp1d(rt1,rt2,rtime,O3s(1,J,:))
          U_surf(nx+1,J)=intp1d(rt1,rt2,rtime,Us(1,J,:))
          V_surf(nx+1,J)=intp1d(rt1,rt2,rtime,Vs(1,J,:))
       END DO
    ELSE
       th_surf=theta
       T_surf(1:nx,1:ny)=Ts(1:nx,1:ny,1)
       P_surf(1:nx,1:ny)=Ps(1:nx,1:ny,1)
       PV_surf(1:nx,1:ny)=PVs(1:nx,1:ny,1)
       !O3_surf(1:nx,1:ny)=O3s(1:nx,1:ny,1)
       U_surf(1:nx,1:ny)=Us(1:nx,1:ny,1)
       V_surf(1:nx,1:ny)=Vs(1:nx,1:ny,1)
       DO I=1,nx
          lat_surf(I,1:ny)=lat(1:ny)
       END DO
       DO J=1,ny
          long_surf(1:nx,J)=lon(1:nx)
       END DO

       T_surf(nx+1,1:ny)=Ts(1,1:ny,1)
       lat_surf(nx+1,1:ny)=lat(1:ny)
       long_surf(nx+1,1:ny)=360.
       P_surf(nx+1,1:ny)=Ps(1,1:ny,1)
       pv_surf(nx+1,1:ny)=PVs(1,1:ny,1)
       !O3_surf(nx+1,1:ny)=O3s(1,1:ny,1)
       U_surf(nx+1,1:ny)=Us(1,1:ny,1)
       V_surf(nx+1,1:ny)=Vs(1,1:ny,1)

    ENDIF
  END SUBROUTINE updatew


  SUBROUTINE winds_finish

    DEALLOCATE(U,V,P,T,PV,O3,time)
    DEALLOCATE(Us,Vs,Ps,Ts,PVs,O3s)

  END SUBROUTINE winds_finish

  REAL FUNCTION intp1d(var1,var2,rval,fun)
    REAL, INTENT(IN) ::  fun(2),rval,var1,var2

    intp1d=(fun(2)-fun(1))*((rval-var1)/(var2-var1))+ &
         fun(1)
  END FUNCTION intp1d

END MODULE winds



!//////////////////////////////////////////////////////////////////////