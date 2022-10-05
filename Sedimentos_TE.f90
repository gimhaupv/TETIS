
!**********************************************
!  *******************
!  * TRAP EFFICIENCY *
!  *******************
!**********************************************
! Archivo .f90 añadido en 11/2011

!****************************************************************************************
!* Esta subrutina es para leer el archivo con los datos sobre las pequeñas presas (TRAP EFFICIENCY)
!****************************************************************************************
SUBROUTINE  leeTE
USE modtet
IMPLICIT NONE
CHARACTER*25 cdummy01
!abre el fichero con los datos de los azudes
OPEN(18,file=archsed(10),iostat=ios,err=201)
!cuenta el numero de azudes y alocata la matriz
nsect=10
READ(18,*)
ios=0
numpresas=0
DO WHILE (ios.ne.-1)
    READ(18,*,iostat=ios) cdummy01
    IF (ios.ne.-1) numpresas=numpresas+1
ENDDO
REWIND(18)
IF (ALLOCATED(dam)) DEALLOCATE(dam)
ALLOCATE(dam(numpresas))
!lee los datos
READ(18,*)
DO i=1,numpresas
  ALLOCATE(dam(i).csector(nsect,3))
  ALLOCATE(dam(i).hsector(nsect))
  ALLOCATE(dam(i).vsector(nsect))
  ALLOCATE(dam(i).lsector(nsect))
  ALLOCATE(dam(i).qinTE(nt),dam(i).sedinTE(nt,3),dam(i).sedoutTE(nt,3),dam(i).depositTE(nt),dam(i).DepthTE(nt))
  dam(i).csector=0.0
  dam(i).hsector=0.0
  dam(i).vsector=0.0
  dam(i).volumen=0.0
  dam(i).sus_tot=0.0
  dam(i).depositTE=0.0
  dam(i).PalturaNew=0.0
  dam(i).depthTE=0.0
  dam(i).dep_tot=0.0
  dam(i).qinTE=0.0
  dam(i).sedinTE=0.0
  dam(i).sedoutTE=0.0
  READ(18,*,err=202)dam(i).Pnombre,dam(i).UTMx,dam(i).UTMy,dam(i).Paltura,dam(i).Pancho,dam(i).Dlong,dam(i).Dancho,dam(i).pend,dam(i).dBD
  dam(i).lsector=dam(i).dlong/REAL(nsect)
  dam(i).fila=IFIX((dam(i).UTMx-cw+(dx/2.0))/dx)
  dam(i).col=IFIX((cn-dam(i).UTMy+(dy/2.0))/dy)
ENDDO
DO n=1,ncel
  DO l=1,numpresas
    IF (dam(l).fila.eq.cell(n).fil)THEN
      IF (dam(l).col.eq.cell(n).col)THEN
        dam(l).pos=n
      ENDIF
    ENDIF
  ENDDO
ENDDO
CLOSE(18)
GO TO 95
201 mensaje=strings(205)
errr=1
CALL errores
GO TO 94
202 mensaje=strings(206)
errr=1
CALL errores
goto 94 !ch

94 WRITE(*,*)strings(800)
95 END SUBROUTINE leeTE
   
   
!!*******************************************************************************************************
!!* Esta subrutina es para escribir el archivo con los datos sobre las pequeñas presas (TRAP EFFICIENCY)
!!*******************************************************************************************************
!SUBROUTINE  escribeTE
!USE modtet
!IMPLICIT NONE
!
!!abre el fichero con los datos de los azudes
!OPEN(18,file=archsed(10),iostat=ios,err=201)
!!cuenta el numero de azudes y alocata la matriz
!
!WRITE(18,*)
!
!DO i=1,numpresas
!  WRITE(18,'(A25,2I15,4F12.4,F12.8,F7.4)',err=202)dam(i).Pnombre,dam(i).UTMx,dam(i).UTMy,dam(i).Paltura,dam(i).Pancho,dam(i).Dlong,dam(i).Dancho,dam(i).pend,dam(i).dBD
!ENDDO
!
!CLOSE(18)
!GO TO 95
!201 mensaje=' El fichero '//TRIM(ADJUSTL(archsed(10)))//' no existe'
!errr=1
!CALL errores(errr,mensaje,lang)
!GO TO 94
!202 mensaje='Lectura de datos errónea en '//TRIM(ADJUSTL(archsed(10)))
!errr=1
!CALL errores(errr,mensaje,lang)
!goto 94 !ch
!
!94 WRITE(*,*)'Se han encontrado ERRORES !!! '
!95 END SUBROUTINE escribeTE


!************************************************************
!* Lectura de datos de embalses - trap efficiency (1ª Parte)
!************************************************************
SUBROUTINE lee_emb1TE
USE modtet
IMPLICIT NONE

sale=0

!Lee curvas de altura vs volumen para los embalses
DO i=1,numpresas
    dam(i).datos=0
    OPEN(11,file=archsed(11),status='old',err=233)
    ios=0
    DO WHILE (ios.ne.-1)
      nam='1234567890123456789012345'
      READ (11,*,iostat=ios)nam
      IF (nam.eq.dam(i).Pnombre) dam(i).datos=dam(i).datos+1
    ENDDO
    CLOSE(11)
ENDDO

GOTO 95


233 mensaje=strings(207)
errr=1
CALL errores
goto 94

94 WRITE(*,*)strings(800)
sale=2

95 END SUBROUTINE


!*************************************************************
!* Lectura de datos de embalses - trap efficiency  (2ª Parte)
!*************************************************************
SUBROUTINE lee_emb2TE
USE modtet
IMPLICIT NONE

sale=0
DO i=1,numpresas
    dam(i).h=0.0
	dam(i).vol=0.0
	OPEN(11,file=archsed(11),status='old',err=233)
    ios=0
    DO WHILE (ios.ne.-1)
      nam='1234567890123456789012345'
      READ (11,*,iostat=ios)nam
      IF (nam.eq.dam(i).Pnombre) THEN
        BACKSPACE(11)
	    DO j=1,dam(i).datos
          READ(11,*,err=235)nam,dam(i).h(j),dam(i).vol(j)
        ENDDO
      ENDIF
    ENDDO
    CLOSE(11)
ENDDO

GOTO 95

233 mensaje=strings(207)
errr=1
CALL errores
GOTO 94
234 mensaje=strings(208)
errr=1
CALL errores
GOTO 94
235 mensaje=strings(209)
errr=1
CALL errores
goto 94  !ch

94 WRITE(*,*)strings(800)
sale=2

95 END SUBROUTINE

!*********************************************************************************************
!* Esta subrutina es para leer el archivo de estado inicial de los embalses (TRAP EFFICIENCY)
!*********************************************************************************************
SUBROUTINE  leeTEini
USE modtet
IMPLICIT NONE

OPEN(18,file=archsed(12),iostat=ios,err=201)
!
DO i=1, numpresas
  READ(18,*) dam(i).PalturaNew
ENDDO

CLOSE(18)

GO TO 95
201 mensaje=strings(210)
errr=1
CALL errores
goto 94  !ch

94 WRITE(*,*)strings(800)
95 END SUBROUTINE leeTEini

!*************************************************************************************************
!* Esta subrutina es para escribir el archivo de estado inicial de los embalses (TRAP EFFICIENCY)
!*************************************************************************************************
SUBROUTINE  escribeTEini
USE modtet
IMPLICIT NONE

OPEN(18,file=archsed(13))
!
DO i=1, numpresas
  WRITE(18,'(F15.6)') dam(i).PalturaNew
ENDDO

CLOSE(18)

95 END SUBROUTINE escribeTEini
   
   
!****************************************************************************************
!* Esta subrutina calcula los sedimentos retenidos en las pequeñas presas
!* METODO STEP (Verstraeten y Poesen, 2002)
!****************************************************************************************
SUBROUTINE  TrapEfficiency (qin,susin)
USE modtet
IMPLICIT NONE
INTEGER z,dummy01,tmax !nwet
REAL arena,limo,arcilla
REAL qin,susin(3),qout,deposit(3),salidaTOT(3) !,salida(3) !en m3
REAL qoutdt
REAL newh,volmax
REAL Qinf(nsect), Qe(nsect),ConcIn(nsect) !,DEP(nsect)
REAL MasaIN(nsect),MasaOUT(nsect),MasaDEP(nsect),MasaPRES(nsect),MasaVAR(nsect) !kg
REAL,ALLOCATABLE :: concentr(:,:,:),volumenes(:,:),alturas(:,:)
!
REAL H_TE,V_TE
REAL temp01,temp02,temp03,temp04,temp05

! calcula la nueva altura de la presa
dam(l).dep_tot=V_TE(dam(l).Paltura-dam(l).PalturaNew)
newh=dam(l).Paltura-H_TE(dam(l).dep_tot)
!La rutina no se activa si el volumen de sedimentos es superior al volumen de agua
!esto pasa al principio de la simulación,por problemas numéricos, cuando el caudal es muy pequeño
temp05=Qin/(susin(1)+susin(2)+susin(3))
IF (temp05.le.1) GO TO 123
!La rutina no se activa si la presa está llena
IF (newh.gt.0.0) THEN
  !dam(l).PalturaNew=newh
  !calcula el volumen max de agua que la presa puede almacenar
  volmax=V_TE(dam(l).Paltura)-dam(l).dep_tot
  !inicialización
  dam(l).sus_tot=0.0
  deposit=0
  salidaTOT=0
  ALLOCATE(concentr(0:INT(dts),nsect,3))
  ALLOCATE(volumenes(0:INT(dts),nsect))
  ALLOCATE(alturas(0:INT(dts),nsect))
  concentr=0.0
  volumenes=0.0
  alturas=0.0
  !valores iniciales de concentracion,volumen y altura
  DO k=1,nsect
    DO i=1,3
      concentr(0,k,i)=dam(l).csector(k,i)
    ENDDO
    volumenes(0,k)=dam(l).vsector(k)
    alturas(0,k)=dam(l).hsector(k)  
  ENDDO
  ! calcula volumen y altura de agua 
  qout=0
  qoutdt=0
  !elección dt
  SELECT CASE(INT(dts))
    CASE(0:600)
      tmax=INT(dts)
    CASE (601:3600)
      tmax=INT(INT(dts)/6)
    CASE (3601:)
      tmax=INT(INT(dts)/25)
  END SELECT
  !Iteración en el tiempo  
  DO z=1,tmax
    dam(l).volumen=dam(l).volumen+qin/tmax
    IF(dam(l).volumen.le.volmax) THEN
      dam(l).hw=H_TE(dam(l).volumen+dam(l).dep_tot)-H_TE(dam(l).dep_tot) !m
    ELSE
      dam(l).hw=newh+(dam(l).volumen-volmax)/(dam(l).Paltura/dam(l).pend*dam(l).Pancho) !m
    ENDIF
    ! calcula el caudal de salida (aliviadero) - m3
    IF (dam(l).hw.gt.newh) THEN
      qoutdt=MIN(qin/tmax,((2*9.80665)**0.5)*0.35*dam(l).Pancho*((dam(l).hw-newh)**(3/2))) !m3
    ELSE
    !modificar, tener en cuenta los drenes!!!
      qoutdt=0
    ENDIF
    ! calcula volumen y altura de agua 
    dam(l).volumen=dam(l).volumen-qoutdt
    IF(dam(l).volumen.le.volmax) THEN
      dam(l).hw=H_TE(dam(l).volumen+dam(l).dep_tot)-H_TE(dam(l).dep_tot) !m
    ELSE
      dam(l).hw=newh+(dam(l).volumen-volmax)/(dam(l).Paltura/dam(l).pend*dam(l).Pancho) !m
    ENDIF
    qout=qout+qoutdt
  ! inicialización
    Qe(nsect)=qoutdt !m3/s
    Qinf(1)=qin/tmax !m3/s
    dummy01=0
    DO k=1,nsect
    ! Calcula el caudal de salida del sector
      IF (k.ne.nsect) Qe(k)=(qin/tmax*((REAL(nsect)-REAL(k))/REAL(nsect))+qoutdt*(REAL(k)/REAL(nsect))) !m3/s
    ! Calcula el caudal de entrada del sector
      IF (k.ne.1) Qinf(k)=Qe(k-1) !m3/s
    ! actualiza la altura de lámina libre del sector
    !  alturas(z,k)=MAX((qin/tmax)/(dam(l).Dancho*dam(l).lsector(k)),dam(l).hw-dam(l).dlong*dam(l).pend*((REAL(nsect)-REAL(k)+0.5)/REAL(nsect))) !m
      alturas(z,k)=dam(l).hw-dam(l).hw*((REAL(nsect)-REAL(k)+0.5)/REAL(nsect)) !m
      IF (alturas(z,k).lt.0.0) dummy01=dummy01+1
    ! Actualiza el volumen del sector
      volumenes(z,k)=alturas(z,k)*dam(l).Pancho*dam(l).lsector(k) !m3
    ENDDO
    ! Corrige las alturas negativas
    DO k=1,dummy01
      alturas(z,k)=qin/tmax !m
      volumenes(z,k)=alturas(z,k)*dam(l).Pancho*dam(l).lsector(k) !m3
    ENDDO
    ! Por cada fracción de tamaño
    ConcIn=0.0
    DO i=1,3
      ConcIN(1)=susin(i)/qin*2.65 !kg/m3
      ! BALANCE DE SEDIMENTOS - Hace el balance de sedimentos en el sector (resultado: concentración de sedimentos en el sector)
      DO k=1,nsect
        ! Concentración entrante a cada sector
        ! BALANCE
        IF (k.ne.1) ConcIN(k)=concentr(z-1,k-1,i)        
        !IF (k.ne.1) THEN
        !  IF (alturas(z-1,k-1).gt.(0.0)) THEN 
        !    ConcIN(k)=MAX(0.0,concentr(z-1,k-1,i)-ws(i)*concentr(z-1,k-1,i)/(2*alturas(z-1,k-1)))  !sin turbulencia
        !  ELSE
        !    ConcIN(k)=0.0
        !  ENDIF
        !ENDIF
        !IF (k.ne.1) ConcIN(k)=MasaOUT(k-1)/Qe(k-1) 
        ! kg/m3
        concentr(z,k,i)=(ConcIN(k)*Qinf(k)+volumenes(z-1,k)*concentr(z-1,k,i))/(volumenes(z,k)+Qe(k)+volumenes(z,k)*ws(i)/alturas(z,k)) ! sin turbulencia
        IF (volumenes(z,k).eq.0.0) concentr(z,k,i)=0.0
        !concentr(z,k,i)=(ConcIN(k)*Qinf(k)+volumenes(z-1,k)*concentr(z-1,k,i))/(volumenes(z,k)+Qe(k)+volumenes(z,k)*(1-EXP(1-ws(i)/alturas(z,k)))) ! con turbulencia      
        !Variacion de masa solida (kg)
        MasaVAR(k)=concentr(z,k,i)*volumenes(z,k)-concentr(z-1,k,i)*volumenes(z-1,k)
        !Masa solida depositada (kg)
        MasaDEP(k)=MAX(0.0,volumenes(z,k)*concentr(z,k,i)*ws(i)/alturas(z,k)) !-ws(i)*Qe(k)*concentr(z,k,i)/(2*alturas(z,k))) !sin turbulencia
        !MasaDEP(k)=volumenes(z,k)*concentr(z,k,i)*ws(i)/alturas(z,k) !sin turbulencia
        !MasaDEP(k)=volumenes(z,k)*concentr(z,k,i)*(1-EXP(1-ws(i)/alturas(z,k)))-ws(i)*Qe(k)*concentr(z,k,i)/(11.2104*alturas(z,k)) !con turbulencia
        !Masa solida que se transfiere al sector siguiente (kg)
        MasaOUT(k)=MAX(0.0,Qe(k)*concentr(z,k,i)) !-ws(i)*Qe(k)*concentr(z,k,i)/(2*alturas(z,k)))  !sin turbulencia
        !MasaOUT(k)=Qe(k)*concentr(z,k,i)  !sin turbulencia
        !MasaOUT(k)=Qe(k)*concentr(z,k,i)-ws(i)*Qe(k)*concentr(z,k,i)/(11.2104*alturas(z,k))  !con turbulencia
        !
        MasaPRES(k)=concentr(z,k,i)*volumenes(z,k)
        !
        MasaIN(k)=ConcIN(k)*Qinf(k)
        !
        !Material depositado acumulado en m3
        deposit(i)=deposit(i)+MasaDEP(k)/dam(l).dBD
        !
!        IF ((concentr(z,k,i).lt.(0.0)).OR.(MasaDEP(k).lt.(0.0)).OR.(MasaOUT(k).lt.(0.0)).OR.(MasaIN(k).lt.(0.0)).OR.(volumenes(z,k)).lt.(0.0)) THEN
!          temp01=0.0          
!        ENDIF
      ENDDO
      ! calcula el total de material depositado - m3
      !salida(i)=MasaOUT(nsect)
      salidaTOT(i)=salidaTOT(i)+MasaOUT(nsect)/2.65
    ENDDO
  ENDDO  
  DO k=1,nsect
    DO i=1,3
      dam(l).csector(k,i)=concentr(tmax,k,i) 
    ENDDO
    dam(l).vsector(k)=volumenes(tmax,k) 
    dam(l).hsector(k)=alturas(tmax,k)   
  ENDDO
! actualiza las cargas de sedimento que salen de la presa
  DO i=1,3
    dam(l).dep_tot=dam(l).dep_tot+deposit(i)
    DO k=1,nsect
      dam(l).sus_tot=dam(l).sus_tot+dam(l).csector(k,i)*volumenes(tmax,k)/2.65
    ENDDO
  !  dam(l).dep_tot=dam(l).dep_tot+susin(i)-salidaTOT(i)
    susin(i)=salidaTOT(i)
  ENDDO
  ! controla que el volumen depositado no sea mayor que el volumen máximo
  temp01=V_TE(dam(l).Paltura)-dam(l).dep_tot
  IF (temp01.lt.0) THEN
    dam(l).dep_tot=V_TE(dam(l).Paltura)
    ! actualiza las cargas de sedimento que salen de la presa
    DO k=1,nsect
       dam(l).sus_tot=dam(l).sus_tot-temp01
    ENDDO
    DO i=1,3
       susin(i)=susin(i)-temp01/3.0
    ENDDO
  ENDIF
  dam(l).PalturaNew=dam(l).Paltura-MAX(0.0,H_TE(dam(l).dep_tot))
  !dam(l).dep_tot=dam(l).dep_tot-dam(l).sus_tot
  DEALLOCATE(concentr)
  DEALLOCATE(volumenes)
  DEALLOCATE(alturas)
!ELSE
!  dam(l).dep_tot=dam(l).dep_tot+dam(l).sus_tot
!  dam(l).sus_tot=0.0    
ENDIF
GO TO 124
!arena=susin(1)
!limo=susin(2)
!arcilla=susin(3)
123 DO i=1,3
  dam(l).dep_tot=dam(l).dep_tot+susin(i)
  susin(i)=0.0
ENDDO
dam(l).PalturaNew=dam(l).Paltura-MAX(0.0,H_TE(dam(l).dep_tot))

!
124 END SUBROUTINE TrapEfficiency


!************************************************************
!Interpolación lineal de la curva de embalse (trap efficiency)
! conociendo V proporciona H
!
!************************************************************
REAL FUNCTION H_TE(v)
USE modtet
IMPLICIT NONE

REAL v,h
INTEGER b1,ndatos

b1=0
qsale=0.0
h=0.0
ndatos=dam(l).datos
IF (v.le.dam(l).vol(1)) THEN
    h=dam(l).h(1)
ELSE
    DO j=1,ndatos
      IF (b1.eq.0) THEN
        IF (v.lt.dam(l).vol(j)) THEN
          IF ((dam(l).vol(j)-dam(l).vol(j-1)).eq.0.0) THEN
 		    h=0.0
 		    mensaje=strings(904)
		    errr=2
            CALL errores
          ELSE
            h=(v-dam(l).vol(j-1))*(dam(l).h(j)-dam(l).h(j-1))
	        h=h/(dam(l).vol(j)-dam(l).vol(j-1))+dam(l).h(j-1)
	      ENDIF
          b1=1
        ENDIF
      ENDIF
    ENDDO
    IF (v.eq.dam(l).vol(ndatos)) h=dam(l).h(ndatos)
    IF (v.gt.dam(l).vol(ndatos)) THEN
      mensaje=strings(905)
      errr=2
      CALL errores
      IF (dam(l).vol(ndatos)-dam(l).vol(ndatos-1).eq.0.0) THEN
        h=0.0
	    mensaje=strings(906)
	    errr=2
        CALL errores
      ELSE
        h=(v-dam(l).vol(ndatos))*(dam(l).h(ndatos)-dam(l).h(ndatos-1))  
        h=h/(dam(l).vol(ndatos)-dam(l).vol(ndatos-1))+dam(l).h(ndatos)
      ENDIF
    ENDIF
    IF (h.lt.0.0) THEN
      mensaje=strings(909)
      errr=2
      CALL errores
      h=0.0
    ENDIF
ENDIF
H_TE=h
RETURN
END


!************************************************************
!Interpolación lineal de la curva de embalse (trap efficiency)
! conociendo H proporciona V
!
!************************************************************
REAL FUNCTION V_TE(h)
USE modtet
IMPLICIT NONE

REAL v,h
INTEGER b1,ndatos

b1=0
qsale=0.0
v=0
ndatos=dam(l).datos
IF (h.le.dam(l).h(1)) THEN
    v=dam(l).vol(1)
ELSE
    DO j=1,ndatos
      IF (b1.eq.0) THEN
        IF (h.lt.dam(l).h(j)) THEN
          IF ((dam(l).h(j)-dam(l).h(j-1)).eq.0.0) THEN
 		    v=0.0
 		    mensaje=strings(904)
		    errr=2
            CALL errores
          ELSE
            v=(h-dam(l).h(j-1))*(dam(l).vol(j)-dam(l).vol(j-1))
	        v=v/(dam(l).h(j)-dam(l).h(j-1))+dam(l).vol(j-1)
	      ENDIF
          b1=1
        ENDIF
      ENDIF
    ENDDO
    IF (h.eq.dam(l).h(ndatos)) v=dam(l).vol(ndatos)
    IF (h.gt.dam(l).h(ndatos)) THEN
      mensaje= strings(905)
      errr=2
      CALL errores
      IF (dam(l).vol(ndatos)-dam(l).vol(ndatos-1).eq.0.0) THEN
        h=0.0
	    mensaje= strings(906)
	    errr=2
        CALL errores
      ELSE
        v=(h-dam(l).h(ndatos))*(dam(l).vol(ndatos)-dam(l).vol(ndatos-1))  
        v=v/(dam(l).h(ndatos)-dam(l).h(ndatos-1))+dam(l).vol(ndatos)
      ENDIF
    ENDIF
    IF (h.lt.0.0) THEN
      mensaje=strings(909)
      errr=2
      CALL errores
      h=0.0
    ENDIF
ENDIF
V_TE=v
RETURN
END