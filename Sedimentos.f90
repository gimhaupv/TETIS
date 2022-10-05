!****************************************************************************
!* Subrutina que realiza calculos de procesos sedimentarios
!* en caso que el usuario haya seleccionado esta opcion previamente
!****************************************************************************
SUBROUTINE sim_celdased
USE modtet
!USE DFLIB
!USE DFPORT
IMPLICIT NONE
REAL sedin,sedout,temp01
!1.REINICIAR EN CERO LAS VARIABLES, PARA CADA PASO DE TIEMPO Y PARA CADA CELDA 
!  (Reset.c en CASC2D-SED)
!cell(n).t_SusSed: Flujo total de sedimentos en suspensión
!cell(n).t_DepSed: Flujo total de sedimentos depositados
!cell(n).t_SedFlujo: Flujo total de sedimentos que salen de la celda (m3/s)
!cell(n).SedFlujo(i): Flujo de sedimentos por fracción de tamaño, m3/s
!i=clase de tamaño: i=1:Arena, i=2:Limo, i=3:Arcilla
cell(n).t_SusSedLAD=0.0
cell(n).t_SusSedRED=0.0
cell(n).t_DepSedLAD=0.0
cell(n).t_DepSedRED=0.0
cell(n).t_SedFlujo=0.0
DO i=1,3
  cell(n).SedFlujo(i)=0.0
ENDDO
 
!2.CALCULOS INICIALES, RELACIONES GEOMORFOLÓGICAS

!2.1. Relaciones hidrológicas en ladera

cell(n).qladera=(cell(n).y(2)*arcel/1000.0)/dts


!Velocidad de flujo en ladera (m/s)
cell(n).velladera=r(4)*cell(n).veloc

!Altura del flujo superficial en ladera (m)
IF (cell(n).velladera.gt.0.0) THEN !Condicional (evita división por cero)
  cell(n).hladera=cell(n).qladera/(cell(n).velladera*dx)
ELSE
  cell(n).hladera=0.0
ENDIF

!2.2. Relaciones geomorfológicas en cárcava
IF (nw(2,ncp).le.cell(n).acum.AND.nw(3,ncp).gt.cell(n).acum) THEN  !Celda con cárcava
  Qbc=dc(1,ncp)*(arac**ec(1,ncp))                    !Qbc=Caudal de la cárcava a sección llena, m3/s
  wcarcavasl=dc(3,ncp)*Qbc**ec(3,ncp)                !wcarcavasl=Ancho de la cárcava a sección llena, m
  cell(n).qcarcava=salrio/dts                        !cell(n).qcarcava=Caudal en cárcava, m3/s
  cell(n).velcarcava=veldts/dts                      !cell(n).velcarcava=Velocidad de flujo en la cárcava en m/s
  IF (cell(n).velcarcava.GT.0.0) THEN                !Condicional (evita división por cero)
    Acarcava=cell(n).qcarcava/cell(n).velcarcava     !Acarcava=Area de la cárcava para el caudal que está pasando, m2	
  ELSE
    Acarcava=0.0
  ENDIF
  IF (Qbc.GT.0.0) THEN                               !Condicional (evita división por cero)
    wqc=wcarcavasl*(cell(n).qcarcava/Qbc)**ec(4,ncp) !wqc=Ancho de la cárcava para el caudal que está pasando
  ELSE
    wqc=0.0
  ENDIF
  IF (wqc.GT.0.0) THEN                               !Condicional (evita división por cero)
    cell(n).hcarcava=Acarcava/wqc                    !cell(n).hcarcava=Altura de agua en la cárcava para el caudal que está pasando en m
  ELSE
    cell(n).hcarcava=0.0
  ENDIF
  IF ((wqc+2.0*cell(n).hcarcava).GT.0.0) THEN        !Condicional (evita división por cero)
    Rhcarcava=Acarcava/(wqc+2.0*cell(n).hcarcava)    !Rhcarcava= Radio hidráulico en cárcava para el caudal que pasa (suponiendo sección rectangular), m	
  ELSE
    Rhcarcava=0.0
  ENDIF
ENDIF

!2.3. Relaciones geomorfológicas en cauce
IF (nw(3,ncp).le.cell(n).acum) THEN                  !Celda con cauce
  Qb=d(1,ncp)*(arac**e(1,ncp))                       !Qb=Caudal a sección llena, m3/s
  wchan=d(3,ncp)*Qb**e(3,ncp)                        !wchan=Ancho del canal a sección llena, m
  cell(n).velcanal=veldts/dts                        !cell(n).velcanal=Velocidad del flujo en el canal en m/s
  cell(n).qcanal=salrio/dts                          !cell(n).qcanal=Caudal en canal m3/s
  IF (cell(n).velcanal.GT.0.0) THEN                  !Condicional (evita división por cero)
    Achan=cell(n).qcanal/cell(n).velcanal            !Achan=Area del canal para el caudal que está pasando, m2	
  ELSE
    Achan=0.0
  ENDIF
  IF (Qb.GT.0.0) THEN                                !Condicional (evita división por cero)
    wq=wchan*(cell(n).qcanal/Qb)**e(4,cell(n).codpar)!wq=Ancho del canal para el caudal que está pasando
  ELSE
    wq=0.0
  ENDIF
  IF (wq.GT.0.0) THEN                                !Condicional (evita división por cero)
    cell(n).hcanal=Achan/wq                          !cell(n).hcanal=Altura de agua en el canal para el caudal que está pasando en m
  ELSE
    cell(n).hcanal=0.0
  ENDIF
  IF ((wq+2.0*cell(n).hcanal).GT.0.0) THEN           !Condicional (evita división por cero)
    Rh=Achan/(wq+2.0*cell(n).hcanal)                 !Rh= Radio hidráulico en el canal para el caudal que pasa (suponiendo sección rectangular), m		
  ELSE
    Rh=0.0
  ENDIF
ENDIF

!3.PROCESOS SEDIMENTARIOS EN LADERA (RoutSedOvrl.c en CASC2D-SED)
!*****************************************************************************************
!La rutina de procesos sedimentarios en laderas se efectúa en las celdas de ladera, los 
!sedimentos generados (Kilinc-Richardson) se añaden a los sedimentos en la ladera de la 
!celda aguas abajo, si el area umbral es mayor que el area de captación de la celda, así 
!el transporte de sedimentos entre celdas se efectuará por la ladera.  
!Si el area umbral es menor o igual que el área de captación, los sedimentos generados se 
!añaden a la cárcava o canal de la celda aguas abajo.
!*****************************************************************************************
!Celdas en ladera.  Se encuentra agua en la ladera?
!Comienzan los procesos en ladera
!IF (nw(2,ncp).gt.cell(n).acum.AND.cell(n).hladera.gt.0.0) THEN 
IF (cell(n).hladera.gt.0.0) THEN
  !Se encuentra agua en la ladera?  Comienzan los proc.en laderas

  !DEFINICIÓN DE VARIABLES

  !Vol.de sedimentos transportados desde la celda aportante hasta la celda receptora,m3.

  !Sedimentos transportados en suspensión:
  !qsSUS(i):Volumen de sedimentos transportados en suspensión, para cada i.
  !qsSUStot:Volumen total de sedimentos transportados en suspensión
  !ADVcapacidadLADERA(i):Volumen de sedimentos transportados por advección, en ladera,
  !                      para cada i, m3
  !KRcapacidad(i):Capacidad de transporte de sedimentos para cada i, según la ecuación de 
  !               Kilinc-Richardson, en ladera, m3
  
  !Sedimentos transportados como material de lecho:
  !qsBM(i):Volumen de sedimentos transportado como material de lecho,para cada i.
  !qsBMtot:Volumen total de sedimentos transportado como material de lecho.
  
  !Sedimentos transportados del material parental erodado:
  !qsEROS(i):Volumen de sedimentos transportado del material parental erodado,por cada i.
  !qsEROStot:Volumen total de sedimentos transportado del material parental erodado
    
  !Volumen total de sedimentos transportados:
  !qs(i): Volumen de sedimentos transportado total,por cada i.
  !cell(n).t_SedFlujo: Flujo total de sedimentos transportados

  !Volumen de sedimentos disponibles en la celda aportante (variables de estado que se 
  !actualizan en cada proceso), en m3:
  !cell(n).SusSed(i): Volumen de sedimentos en suspensión, para cada i
  !SUStot:Volumen total de sedimentos en suspensión
  !cell(n).DepSed(i):Vol.de sed. en deposición (como material de lecho), para cada i
  !DEPtot: Volumen total de sedimentos en deposición (como material de lecho)

  !3.1.DEPOSITACIÓN DE SEDIMENTOS EN LADERA.

  !*****************************************************************************************
  !Para cada paso de tiempo y para cada i,se calcula el vol. de sedimentos que se deposita.
  !Este volumen se sustrae de la porción de sedimentos en suspensión (cell(n).SusSed(i)) 
  !y se añade a la porción de sedimentos depositados(cell(n).DepSed(i)).
  !*****************************************************************************************

  !DEFINICIÓN DE VARIABLES
  !PorcentDep(i): Porcentaje de sedimentos en suspensión que puede depositarse para cada i, 
  !               adimensional (entre 0 y 1)
  !depositacion(i): Volumen de sedimentos que se depositan para cada i, en m3
  !cell(n).SusSedLad(i): Volumen de sedimentos en suspensión en ladera, para cada i, en m3
  !cell(n).DepSedLad(i): Volumen de sedimentos en deposición en ladera, para cada i, en m3. En la rutina
  !                    de depositación, es en la única parte donde se modifica este valor
  DO i=1,3	                              !Para cada clase de tamaño
	IF (cell(n).hladera.GT.ws(i)*dts) THEN 
	  PorcentDep(i)=ws(i)*dts/cell(n).hladera
	ELSE
      PorcentDep(i)=1.0
	ENDIF
! ***************Cris: Para considerar un input de sedimentos***************
	If (nw(2,ncp).gt.cell(n).acum) THEN
	    IF (i.eq.1) THEN
	    DO l=1,kadised1
	        IF (n.eq.aportsed1(l).pos) then
	        cell(n).SusSedLAD(1)=cell(n).SusSedLAD(1)+aportsed1(l).obs(t)*dts
	        END IF
	    END DO
	    ELSE IF (i.eq.2) THEN
	    DO l=1,kadised2
	        IF (n.eq.aportsed2(l).pos) then
	        cell(n).SusSedLAD(2)=cell(n).SusSedLAD(2)+aportsed2(l).obs(t)*dts
	        END IF
	    END DO
	    ELSE
	    DO l=1,kadised3
	        IF (n.eq.aportsed3(l).pos) then
	        cell(n).SusSedLAD(3)=cell(n).SusSedLAD(3)+aportsed3(l).obs(t)*dts
	        END IF
	    END DO
	    END IF
    END IF
	depositacion(i)=cell(n).SusSedLAD(i)*PorcentDep(i)
	cell(n).DepSedLAD(i)=cell(n).DepSedLAD(i)+depositacion(i)
	cell(n).SusSedLAD(i)=cell(n).SusSedLAD(i)-depositacion(i)
  ENDDO
  
  !Cris (03/2017) Nitrógeno: acumulación de estados iniciales de suspendidos/depositados de arcillas para poder calcular los flujos de nitrógeno
  If (modulos2(3)) then  
    cell(n).volsusini=cell(n).SusSedLAD(3)+depositacion(3)
    cell(n).voldepini=0.0  !En ladera no me hace falta el depositado, siempre es el suelo
  End if

  !3.2.Iniciar los volumenes totales en cero
  qsSUStot=0.0
  qsBMtot=0.0
  qsEROStot=0.0
  SUStot=0.0
  DEPtot=0.0

  !3.3.CALCULAR LA CAPACIDAD DE TRANSPORTE EN LADERA (ECUACIÓN DE KILINC-RICHARDSON), m3

  ! Calcular el caudal unitario (caudal/ancho de flujo), en m2/seg:
  CaudalUnitario=cell(n).qladera/dx
  ! Calcular la capacidad de transporte, en m3:
  qsKR=rsed(1)*58390.0*((cell(n).pend)**1.66)*(CaudalUnitario**2.035)*cell(n).Cusle*     &
  cell(n).Kusle*cell(n).Pusle*dx*dts

  !3.4.INICIAR LOS VOLUMENES EN CERO PARA CADA i
  DO i=1,3
	qsSUS(i)=0.0
	qsBM(i)=0.0
	qsEROS(i)=0.0
	qs(i)=0.0
    !Vol.total de sed.en suspensión y en deposición en la celda aportante
	SUStot=SUStot+cell(n).SusSedLAD(i)    
	DEPtot=DEPtot+cell(n).DepSedLAD(i)
  ENDDO

  !3.5.TRANSPORTAR EL VOLUMEN DE SEDIMENTOS EN SUSPENSIÓN, PARA CADA i
  DO i=1,3	                                                  !Para cada clase de tamaño
	IF (cell(n).SusSedLAD(i).GT.0.0) THEN	                  !Hay material en suspensión de la clase de tamaño?
	  IF (qsKR.LT.SUStot) THEN                                !La cap.de trans. es menor que el vol.total en suspensión?
	    KRcapacidad(i)=qsKR*cell(n).SusSedLAD(i)/SUStot       !Vol.que puede transportarse según K-R
        !ADVcapacidadLADERA(i)=rsed(1)*cell(n).SusSedLAD(i)*cell(n).velladera*dts/(dx+cell(n).velladera*dts) !Vol.trans.advect
        ADVcapacidadLADERA(i)=cell(n).SusSedLAD(i)*cell(n).velladera*dts/(dx+cell(n).velladera*dts) !Vol.trans.advect
		qsSUS(i)=MAX(KRcapacidad(i),ADVcapacidadLADERA(i))    !Trans.el máximo 	
	  ELSE	                                                  !La capacidad de transporte es mayor que el material total en suspensión?
		qsSUS(i)=cell(n).SusSedLAD(i)                         !Transportar todos los sedimentos en suspensión
	  ENDIF
	ENDIF
	!Transferir qsSUS(i) de la celda aportante a los sed.en susp. de la celda receptora
	!cell(n).SusSed(i)=(cell(n).SusSed(i)-qsSUS(i))*nsurcoent(ncp) !Sustraer el vol.de la celda aportante, para todos los surcos
    !cell(cell(n).dest).SusSed(i)=cell(cell(n).dest).SusSed(i)+qsSUS(i)*nsurcoent(ncp)   !Para todos los surcos
	cell(n).SusSedLAD(i)=cell(n).SusSedLAD(i)-qsSUS(i)         !Sustraer el vol.de la celda aportante
   
	IF (nw(2,ncp).gt.cell(cell(n).dest).acum.AND.nw(2,ncp).gt.cell(n).acum) THEN             !de celda de ladera a celda de ladera
      cell(cell(n).dest).SusSedLAD(i)=cell(cell(n).dest).SusSedLAD(i)+qsSUS(i)
    ELSE IF (nw(2,ncp).le.cell(cell(n).dest).acum.AND.nw(2,ncp).gt.cell(n).acum) THEN        !de celda de ladera a celda de cárcava (o cauce)
      cell(cell(n).dest).SusSedRED(i)=cell(cell(n).dest).SusSedRED(i)+qsSUS(i)
    !de celda de cárcava (o cauce) a celda de cárcava (o cauce)
    ELSE
      cell(cell(n).dest).SusSedRED(i)=cell(cell(n).dest).SusSedRED(i)+qsSUS(i)
    ENDIF
	!Conservar el volumen total de sedimentos en suspensión proveniente de la porción de
	!sedimentos en suspensión de la celda aportante
	qsSUStot=qsSUStot+qsSUS(i)
  ENDDO

  !3.6.CALCULAR LA CAPACIDAD DE TRANSPORTE EN EXCESO (EXCcapacidadLAD)
  !Reducir la cap.de trans.del volumen suspendido transportado en la celda aportante
  
  EXCcapacidadLAD=MAX(0.0,qsKR-qsSUStot)

  !3.7.TRANSPORTAR EL VOL.DE SEDIMENTOS DEPOSITADOS (COMO MATERIAL DE LECHO), PARA CADA i
  !Si existe capacidad de trans.en exceso y la celda aportante posee material depositado
  !previamente, se usa dicha capacidad de trans.en exceso para poner los sedimentos en 
  !suspensión y transportarlo a la porción de sed.en suspensión de la celda receptora
 
  IF ((EXCcapacidadLAD.GT.0.0).AND.(DEPtot.GT.0.0)) THEN
	DO i=1,3	                                         !Para cada clase de tamaño
      IF (cell(n).DepSedLAD(i).GT.0.0) THEN              !Esta la clase de tamaño como material deposit?
	    IF(EXCcapacidadLAD.LE.DEPtot) THEN               !Cap.de trans.(exceso) menor que vol.dep?
		  qsBM(i)=EXCcapacidadLAD*cell(n).DepSedLAD(i)/DEPtot
		ELSE	                                         !La cap. de transp.(exceso) es mayor que el vol. total depositado?
		  qsBM(i)=cell(n).DepSedLAD(i)                   !Transportar todos los sedimentos depositados
		ENDIF
	  ENDIF
      !Transferir qsBM(i) de la celda aport. a los sed. en suspensión de la celda recept.
	  cell(n).DepSedLAD(i)=cell(n).DepSedLAD(i)-qsBM(i)  !Sustraer vol.trans.de la celda aportante
!      cell(cell(n).dest).SusSed(i)=cell(cell(n).dest).SusSed(i)+qsBM(i)*nsurcoent(ncp)   !Para todos los surcos
    !de celda de ladera a celda de ladera
	IF (nw(2,ncp).gt.cell(cell(n).dest).acum.AND.nw(2,ncp).gt.cell(n).acum) THEN 
      cell(cell(n).dest).SusSedLAD(i)=cell(cell(n).dest).SusSedLAD(i)+qsBM(i)
    !de celda de ladera a celda de cárcava (o cauce)
    ELSE IF (nw(2,ncp).le.cell(cell(n).dest).acum.AND.nw(2,ncp).gt.cell(n).acum) THEN
      cell(cell(n).dest).SusSedRED(i)=cell(cell(n).dest).SusSedRED(i)+qsBM(i)
    !de celda de cárcava (o cauce) a celda de cárcava (o cauce)
    ELSE
      cell(cell(n).dest).SusSedRED(i)=cell(cell(n).dest).SusSedRED(i)+qsBM(i)
    ENDIF
	 !Conservar el volumen total de sedimentos depositados proveniente de la porción de 
	 !sedimentos depositados de la celda aportante
	 qsBMtot=qsBMtot+qsBM(i)
    ENDDO
  ENDIF


  !3.8.CALCULAR LA CAPACIDAD DE TRANSPORTE RESIDUAL
  !Reducir la cap. de transp. en exceso del vol. depos. transportado en la celda aportante
  RESIDcapacidadLAD=MAX(0.0,EXCcapacidadLAD-qsBMtot)

  !3.9.TRANSPORTAR EL VOLUMEN DE SEDIMENTOS ERODADO (MATERIAL PARENTAL), PARA CADA i
  !La capacidad de transporte residual se usa para erodar el material parental.La erosión 
  !por fracción de tamaño es proporcional a su porcentaje en el material parental
  IF (RESIDcapacidadLAD.GT.0.0) THEN
	DO i=1,3	!Para cada clase de tamaño
      qsEROS(i)=RESIDcapacidadLAD*cell(n).porcentaje(i)
	  !Transferir qsEROS(i) de la celda aport. a los sed. en suspensión de la celda recep.
	  cell(n).ErodSed(i)=cell(n).ErodSed(i)-qsEROS(i) !Sustr.el vol.trans.de la celda aportante
!      cell(cell(n).dest).SusSed(i)=cell(cell(n).dest).SusSed(i)+qsEROS(i)*nsurcoent(ncp)  !Para todos los surcos
	IF (nw(2,ncp).gt.cell(cell(n).dest).acum.AND.nw(2,ncp).gt.cell(n).acum) THEN        !de celda de ladera a celda de ladera
      cell(cell(n).dest).SusSedLAD(i)=cell(cell(n).dest).SusSedLAD(i)+qsEROS(i)
    ELSE IF (nw(2,ncp).le.cell(cell(n).dest).acum.AND.nw(2,ncp).gt.cell(n).acum) THEN   !de celda de ladera a celda de cárcava (o cauce)
      cell(cell(n).dest).SusSedRED(i)=cell(cell(n).dest).SusSedRED(i)+qsEROS(i)
    ELSE
      cell(cell(n).dest).SusSedRED(i)=cell(cell(n).dest).SusSedRED(i)+qsEROS(i)         !de celda de cárcava (o cauce) a celda de cárcava (o cauce)
    ENDIF
	  !Conservar el volumen total de sedimentos erodados proveniente de la porción de 
	  !sedimentos erodados de la celda aportante
	  qsEROStot=qsEROStot+qsEROS(i)
    ENDDO
  ENDIF
      
  ! Flujo total de sedimentos que sale de la celda aportante, en m3/s, para todos los surcos - solo si la celda descarga aguas abajo y no en la propia cárcava
!  cell(n).t_SedFlujo=cell(n).t_SedFlujo+(qsSUStot+qsBmtot+qsEROStot)*nsurcoent(ncp)/dts
	IF (nw(2,ncp).gt.cell(n).acum.AND.cell(n).hladera.gt.0.0) THEN 
      cell(n).t_SedFlujo=(qsSUStot+qsBmtot+qsEROStot)/dts
      DO i=1,3	!Para cada clase de tamaño
       !Volumen total de sedimentos que salen de la celda aportante para cada i, en m3, para todos los surcos
       !qs(i)=(qsSUS(i)+qsBM(i)+qsEROS(i))*nsurcoent(ncp)
	   qs(i)=qsSUS(i)+qsBM(i)+qsEROS(i)
       cell(n).SedFlujo(i)=qs(i)/dts	                 !Flujo de sed.para cada i, en m3/s, para todos los surcos
      ENDDO
    ELSE
      cell(n).t_SedFlujo=0.0
      DO i=1,3	!Para cada clase de tamaño
        cell(n).SedFlujo(i)=0.0
      ENDDO
    ENDIF
    
  !Cris (03/2017) Nitrógeno: acumulación de flujos para poder utilizarlos en sim_celdanitr
  IF ((nw(2,ncp).gt.cell(n).acum)) THEN !Para almacenar en las de ladera sólo lo de ladera, no está al inicio
      If (modulos2(3)) then
        cell(n).depositacion=depositacion(3)
        cell(n).qsuspendido=qsSUS(1)+qsSUS(2)+qsSUS(3)
        cell(n).qdepositado=qsBM(1)+qsEROS(1)+qsBM(2)+qsEROS(2)+qsBM(3)+qsEROS(3)
        !!Cambio a 3 sólo, porque considero que el nitrógeno sólo va adsorbido a las arcillas
        cell(n).qsuspendidoar=qsSUS(3)
        cell(n).qdepositadoar=qsBM(3)+qsEROS(3)
      End if
  End if
else if (modulos2(3)) then !Si no hay agua en la ladera todo vale cero
    cell(n).depositacion=0.0
    cell(n).qsuspendido=0.0
    cell(n).qdepositado=0.0
    cell(n).qsuspendidoar=0.0
    cell(n).qdepositadoar=0.0
ENDIF	!Finaliza procesos sedimentarios en ladera (surcos)


!4.PROCESOS SEDIMENTARIOS EN CARCAVAS (RoutSedChn.c en CASC2D-SED)
!*****************************************************************************************
!La rutina de procesos sedimentarios en carcavas se efectúa en las celdas con cauce efímero,
!*****************************************************************************************
!DEFINICIÓN DE VARIABLES

!i=clase de tamaño; i=1=Arena, i=2=Limo, i=3=arcilla

!Sedimentos transportados en suspensión, en m3:
!SUSvol(i): Vol.de sed. transportados por advección, para cada i.

!Sedimentos transportados como material de lecho:
!BMvol(i): Vol.de sed. transportado como material de lecho, para cada i.
     
!Volumen total de sedimentos transportados:
!qs(i)=SUSvol(i)+BMvol(i): Vol. de sed. transportado total, para cada i.
!cell(n).t_SedFlujo=qs(i)/dts: Flujo de sedimentos en m3/s

!Volumen de sedimentos disponibles en la celda aportante, variables de estado, en m3
!cell(n).SusSed(i): Volumen de sedimentos en suspensión, para cada i
!cell(n).DepSed(i): Volumen de sedimentos en deposición (material de lecho), para cada i
!EHcapacidad(i):Capacidad de transporte de sedimentos para cada i, según la ecuación de
!               Engelund y Hansen, m3

IF (nw(2,ncp).le.cell(n).acum.AND.nw(3,ncp).gt.cell(n).acum.AND.cell(n).hcarcava.GT.0.0) THEN
  !Se encuentra agua en la celda?
  !Comienzan los procesos de sedimentos en carcavas
 
  !4.1.DEPOSITACIÓN DE SEDIMENTOS EN CARCAVAS.
  !*****************************************************************************************
  !Para cada paso de tiempo y para cada i, se calcula el volde sedimentos que se deposita.
  !Este volumen se sustrae de la porción de sedimentos en suspensión (cell(n).SusSed(i)) 
  !y se añade a la porción de sedimentos depositados(cell(n).DepSed(i)).
  !*****************************************************************************************

  !DEFINICIÓN DE VARIABLES
  !PorcentDep(i): Porcentaje de sedimentos en suspensión que puede depositarse para cada i, 
  !               adimensional (entre 0 y 1)
  !depositacion(i): Volumen de sedimentos que se depositan para cada i, en m3
  !cell(n).SusSed(i): Volumen de sedimentos en suspensión, para cada i, en m3
  !cell(n).DepSed(i): Volumen de sedimentos en deposición, para cada i, en m3. En la rutina
  !                    de depositación, es en la única parte donde se modifica este valor

  DO i=1,3	!Para cada clase de tamaño
	IF (cell(n).hcarcava.GT.ws(i)*dts) THEN 
	  PorcentDep(i)=ws(i)*dts/cell(n).hcarcava
	ELSE
      PorcentDep(i)=1.0
	ENDIF
! *************** Cris: Para considerar un input de sedimentos***************
	IF (i.eq.1) THEN
	  DO l=1,kadised1
	    IF (n.eq.aportsed1(l).pos) then
	      cell(n).SusSedRED(1)=cell(n).SusSedRED(1)+aportsed1(l).obs(t)*dts
	    END IF
	  END DO
	ELSE IF (i.eq.2) THEN
	  DO l=1,kadised2
	    IF (n.eq.aportsed2(l).pos) then
	      cell(n).SusSedRED(2)=cell(n).SusSedRED(2)+aportsed2(l).obs(t)*dts
	    END IF
	  END DO
	ELSE
	  DO l=1,kadised3
	    IF (n.eq.aportsed3(l).pos) then
	      cell(n).SusSedRED(3)=cell(n).SusSedRED(3)+aportsed3(l).obs(t)*dts
	    END IF
	  END DO
    END IF   

	depositacion(i)=cell(n).SusSedRED(i)*PorcentDep(i)
	cell(n).DepSedRED(i)=cell(n).DepSedRED(i)+depositacion(i)
	cell(n).SusSedRED(i)=cell(n).SusSedRED(i)-depositacion(i)

    ENDDO

    !Cris (03/2017) Nitrógeno: acumulación de estados iniciales de suspendidos/depositados de arcilla para poder calcular los flujos de nitrógeno
    If (modulos2(3)) then  
      cell(n).volsusinired=cell(n).SusSedRED(3)+depositacion(3)
      cell(n).voldepinired=cell(n).DepSedRED(3)-depositacion(3)
    End if
  
  !4.2.CALCULAR EL TRANSPORTE DE SEDIMENTOS EN CARCAVAS (ENGELUND-HANSEN), EN M3
  DO i=1,3	!Para cada clase de tamaño
	supply=cell(n).SusSedRED(i)+cell(n).DepSedRED(i)	!Suministro de sed. celda aportante
	qs(i)=0.0	!Iniciar en cero el volumen de sedimentos transportado, para cada i
	IF (supply.GT.0.0) THEN !Existe suministro de sedimentos en la celda aportante?
      !Calcular la cap.de trans. de sedimentos con la ecuación de Engelund-Hansen, en m3
	  !Concw(i): Concentración por peso, para cada i
	  Concw(i)=(0.05*(Ge/(Ge-1.0))*(cell(n).velcarcava*(cell(n).pend))/SQRT((Ge-1.0)*  &
	  gravedad*diamsed(i))*SQRT(Rhcarcava*(cell(n).pend)/((Ge-1.0)*diamsed(i))))
	  EHcapacidad(i)=rsed(2)*((cell(n).qcarcava*Concw(i))/2.65)*dts !Cap.de transporte(Engelund-Hansen)
	  !Calcular la capacidad de transporte de sedimentos suspendidos por advección,
	  !limitado por la disponiblidad de sedimentos
      adv_factor=MIN(veldts/(cell(n).lon+veldts),1.0)
	  !SUSvol(i)=MIN(rsed(2)*cell(n).SusSedRED(i)*adv_factor,cell(n).SusSedRED(i))
	  SUSvol(i)=cell(n).SusSedRED(i)*adv_factor
	  !Transf. SUSvol(i) de la celda aport. a los sed.en suspensión de la celda receptora
	  cell(n).SusSedRED(i)=cell(n).SusSedRED(i)-SUSvol(i) !Sustr. el vol de la celda apor.
	  !
	  cell(cell(n).dest).SusSedRED(i)=cell(cell(n).dest).SusSedRED(i)+SUSvol(i)
	  
	  !Calcular la cap.de trans.(exceso) para transportar sed. como material de lecho
	  EXCcapacidadCANAL(i)=MAX(0.0,EHcapacidad(i)-SUSvol(i))
	  !Calcular el vol.de sed. del material del lecho que se puede transportar por advecc.
	  BMvol(i)=cell(n).DepSedRED(i)*adv_factor
	  !Volumen que será transportado como material del lecho
	  Bmvol(i)=MIN(EXCcapacidadCANAL(i),BMvol(i))
	  !Transf. BMvol(i) de la celda aport.a los sed. en suspensión de la celda receptora
	  cell(n).DepSedRED(i)=cell(n).DepSedRED(i)-BMvol(i) !Sustr. vol.trans. de la celda apor.
	  !
	  cell(cell(n).dest).SusSedRED(i)=cell(cell(n).dest).SusSedRED(i)+BMvol(i) 
	  qs(i)=SUSvol(i)+BMvol(i)	                        !Volumen total transportado desde la celda aportante
	  cell(n).t_SedFlujo=cell(n).t_SedFlujo+qs(i)/dts   !Actualizar el flujo de sedimentos total transportado desde la celda aportante, m3/s
	  cell(n).SedFlujo(i)=qs(i)/dts                     !Flujo de sedimentos para cada i, en m3/s
	ENDIF
  ENDDO
!EFICIENCIA DE RETENCIÓN
  IF (trapeff) THEN
    DO l=1,numpresas
      IF (n.eq.dam(l).pos)THEN
        sedin=0.0
        sedout=0.0
        DO i=1,3
          sedin=sedin+cell(cell(n).dest).SusSedRED(i)
          dam(l).sedinTE(t,i)=cell(cell(n).dest).SusSedRED(i)
        ENDDO
        CALL TrapEfficiency(cell(n).qcarcava*dts,cell(cell(n).dest).SusSedRED)
        DO i=1,3
          sedout=sedout+cell(cell(n).dest).SusSedRED(i)
        ENDDO
        !variables para el archivo de salida
        dam(l).qinTE(t)=cell(n).qcarcava
        DO i=1,3
          dam(l).sedoutTE(t,i)=cell(cell(n).dest).SusSedRED(i)
        ENDDO
        dam(l).depthTE(t)=dam(l).PalturaNew
        dam(l).depositTE(t)=dam(l).dep_tot
        !WRITE(1110+l,'(F10.0,F12.4,2F15.8,F13.4,2F16.8)')t*dtmin,cell(n).qcarcava*dts,sedin,sedout,dam(l).PalturaNew,dam(l).dep_tot,dam(l).sus_tot
      ENDIF
    ENDDO
  ENDIF
    !Cris (03/2017) Nitrógeno: acumulación de flujos para poder utilizarlos en sim_celdanitr
    If (modulos2(3)) then
        cell(n).depositacion=depositacion(3)
        cell(n).qsuspendido=SUSvol(1)+SUSvol(2)+SUSvol(3)
        cell(n).qdepositado=BMvol(1)+BMvol(2)+BMvol(3)
        cell(n).qsuspendidoar=SUSvol(3)
        cell(n).qdepositadoar=BMvol(3)
    End if
ENDIF

!5.PROCESOS SEDIMENTARIOS EN CANALES (RoutSedChn.c en CASC2D-SED)
!*****************************************************************************************
!La rutina de procesos sedimentarios en canales se efectúa en las celdas con cauce permanente,
!esto es, en celdas con área de captación mayor o igual que el área umbral
!*****************************************************************************************
!DEFINICIÓN DE VARIABLES

!i=clase de tamaño; i=1=Arena, i=2=Limo, i=3=arcilla

!Sedimentos transportados en suspensión, en m3:
!SUSvol(i): Vol.de sed. transportados por advección, para cada i.

!Sedimentos transportados como material de lecho:
!BMvol(i): Vol.de sed. transportado como material de lecho, para cada i.
     
!Volumen total de sedimentos transportados:
!qs(i)=SUSvol(i)+BMvol(i): Vol. de sed. transportado total, para cada i.
!cell(n).t_SedFlujo=qs(i)/dts: Flujo de sedimentos en m3/s

!Volumen de sedimentos disponibles en la celda aportante, variables de estado, en m3
!cell(n).SusSed(i): Volumen de sedimentos en suspensión, para cada i
!cell(n).DepSed(i): Volumen de sedimentos en deposición (material de lecho), para cada i
!EHcapacidad(i):Capacidad de transporte de sedimentos para cada i, según la ecuación de
!               Engelund y Hansen, m3

IF (nw(3,ncp).le.cell(n).acum.AND.cell(n).hcanal.GT.0.0) THEN
  !Se encuentra agua en la celda?
  !Comienzan los procesos de sedimentos en canales
 
  !5.1.DEPOSITACIÓN DE SEDIMENTOS EN CANALES.
  !*****************************************************************************************
  !Para cada paso de tiempo y para cada i, se calcula el volde sedimentos que se deposita.
  !Este volumen se sustrae de la porción de sedimentos en suspensión (cell(n).SusSed(i)) 
  !y se añade a la porción de sedimentos depositados(cell(n).DepSed(i)).
  !*****************************************************************************************

  !DEFINICIÓN DE VARIABLES
  !PorcentDep(i): Porcentaje de sedimentos en suspensión que puede depositarse para cada i, 
  !               adimensional (entre 0 y 1)
  !depositacion(i): Volumen de sedimentos que se depositan para cada i, en m3
  !cell(n).SusSed(i): Volumen de sedimentos en suspensión, para cada i, en m3
  !cell(n).DepSed(i): Volumen de sedimentos en deposición, para cada i, en m3. En la rutina
  !                    de depositación, es en la única parte donde se modifica este valor
  !
    DO i=1,3	!Para cada clase de tamaño
	    IF (cell(n).hcanal.GT.ws(i)*dts) THEN 
	        PorcentDep(i)=ws(i)*dts/cell(n).hcanal
	    ELSE
            PorcentDep(i)=1.0
	    ENDIF
! ***************Cris: para considerar un input de sedimentos***************
	    IF (i.eq.1) THEN
	        DO l=1,kadised1
	            IF (n.eq.aportsed1(l).pos) then
	                 cell(n).SusSedRED(1)=cell(n).SusSedRED(1)+aportsed1(l).obs(t)*dts
	            END IF
	        END DO
	    ELSE IF (i.eq.2) THEN
	        DO l=1,kadised2
	            IF (n.eq.aportsed2(l).pos) then
	                cell(n).SusSedRED(2)=cell(n).SusSedRED(2)+aportsed2(l).obs(t)*dts
	            END IF
	        END DO
	    ELSE
	        DO l=1,kadised3
	            IF (n.eq.aportsed3(l).pos) then
	                cell(n).SusSedRED(3)=cell(n).SusSedRED(3)+aportsed3(l).obs(t)*dts
	            END IF
	        END DO
	    END IF

	    depositacion(i)=cell(n).SusSedRED(i)*PorcentDep(i)
	    cell(n).DepSedRED(i)=cell(n).DepSedRED(i)+depositacion(i)
	    cell(n).SusSedRED(i)=cell(n).SusSedRED(i)-depositacion(i)
    END DO
    
    !Cris (03/2017) Nitrógeno: acumulación de estados iniciales de suspendidos/depositados de arcilla para poder calcular los flujos de nitrógeno
    If (modulos2(3)) then  
      cell(n).volsusinired=cell(n).SusSedRED(3)+depositacion(3)
      cell(n).voldepinired=cell(n).DepSedRED(3)-depositacion(3)
    End if

  
  !5.2.CALCULAR EL TRANSPORTE DE SEDIMENTOS EN CANALES (ENGELUND-HANSEN), EN M3
  DO i=1,3	!Para cada clase de tamaño
	supply=cell(n).SusSedRED(i)+cell(n).DepSedRED(i)	!Suministro de sed. celda aportante
	qs(i)=0.0	!Iniciar en cero el volumen de sedimentos transportado, para cada i
	IF (supply.GT.0.0) THEN !Existe suministro de sedimentos en la celda aportante?
      !Calcular la cap.de trans. de sedimentos con la ecuación de Engelund-Hansen, en m3
	  !Concw(i): Concentración por peso, para cada i
	  Concw(i)=(0.05*(Ge/(Ge-1.0))*(cell(n).velcanal*(cell(n).pend))/SQRT((Ge-1.0)*  &
	  gravedad*diamsed(i))*SQRT(Rh*(cell(n).pend)/((Ge-1.0)*diamsed(i))))
	  EHcapacidad(i)=rsed(3)*((cell(n).qcanal*Concw(i))/2.65)*dts !Cap.de transporte(Engelund-Hansen)
	  !Calcular la capacidad de transporte de sedimentos suspendidos por advección,
	  !limitado por la disponiblidad de sedimentos
	  !adv_factor=MIN(cell(n).velcanal*dts/dx,1.0)
      adv_factor=MIN(veldts/(cell(n).lon+veldts),1.0)
	  !adv_factor=cell(n).velcanal*dts/(dx+cell(n).velcanal*dts)
	  !SUSvol(i)=MIN(rsed(3)*cell(n).SusSedRED(i)*adv_factor,cell(n).SusSedRED(i))
	  SUSvol(i)=cell(n).SusSedRED(i)*adv_factor
	  !Transf. SUSvol(i) de la celda aport. a los sed.en suspensión de la celda receptora
	  cell(n).SusSedRED(i)=cell(n).SusSedRED(i)-SUSvol(i) !Sustr. el vol de la celda apor.
	  cell(cell(n).dest).SusSedRED(i)=cell(cell(n).dest).SusSedRED(i)+SUSvol(i)
	  
	  EXCcapacidadCANAL(i)=MAX(0.0,EHcapacidad(i)-SUSvol(i))	  !Calcular la cap.de trans.(exceso) para transportar sed. como material de lecho
	  BMvol(i)=cell(n).DepSedRED(i)*adv_factor	                  !Calcular el vol.de sed. del material del lecho que se puede transportar por advecc.
	  Bmvol(i)=MIN(EXCcapacidadCANAL(i),BMvol(i))            	  !Volumen que será transportado como material del lecho
	  !Transf. BMvol(i) de la celda aport.a los sed. en suspensión de la celda receptora
	  cell(n).DepSedRED(i)=cell(n).DepSedRED(i)-BMvol(i) !Sustr. vol.trans. de la celda apor.
	  cell(cell(n).dest).SusSedRED(i)=cell(cell(n).dest).SusSedRED(i)+BMvol(i) 

	  qs(i)=SUSvol(i)+BMvol(i)	                                  !Volumen total transportado desde la celda aportante
	  cell(n).t_SedFlujo=cell(n).t_SedFlujo+qs(i)/dts	  	      !Actualizar el flujo de sedimentos total transportado desde la celda aportante, m3/s
	  cell(n).SedFlujo(i)=qs(i)/dts                         	  !Flujo de sedimentos para cada i, en m3/s
	ENDIF
  ENDDO
!EFICIENCIA DE RETENCIÓN
  IF (trapeff) THEN
    DO l=1,numpresas
      IF (n.eq.dam(l).pos)THEN
        sedin=0.0
        sedout=0.0
        DO i=1,3
          sedin=sedin+cell(cell(n).dest).SusSedRED(i)
          dam(l).sedinTE(t,i)=cell(cell(n).dest).SusSedRED(i)
        ENDDO
        CALL TrapEfficiency(cell(n).qcanal*dts,cell(cell(n).dest).SusSedRED)
        DO i=1,3
          sedout=sedout+cell(cell(n).dest).SusSedRED(i)
        ENDDO
        !variables para el archivo de salida
        dam(l).qinTE(t)=cell(n).qcanal
        DO i=1,3
          dam(l).sedoutTE(t,i)=cell(cell(n).dest).SusSedRED(i)
        ENDDO
        dam(l).depthTE(t)=dam(l).PalturaNew
        dam(l).depositTE(t)=dam(l).dep_tot
        !WRITE(1110+l,'(F10.0,F12.4,2F15.8,F13.4,2F16.8)')t*dtmin,cell(n).qcanal*dts,sedin,sedout,dam(l).PalturaNew,dam(l).dep_tot,dam(l).sus_tot
      ENDIF
    ENDDO
  ENDIF
    !Cris (03/2017) Nitrógeno: acumulación de flujos para poder utilizarlos en sim_celdanitr
  If (modulos2(3)) then
    cell(n).depositacionred=depositacion(3)
    cell(n).qsuspendidored=SUSvol(1)+SUSvol(2)+SUSvol(3)
    cell(n).qdepositadored=BMvol(1)+BMvol(2)+BMvol(3)
    cell(n).qsuspendidoarred=SUSvol(3)
    cell(n).qdepositadoarred=BMvol(3)
  End if
ENDIF

  !*************************************************************************************
  !En puntos de aforo, para cada intervalo temporal
DO l=1,naf !Series temporales de flujo de sedimentos
  IF (n.eq.aforo(l).pos)THEN   
    DO i=1,3	!Para cada clase de tamaño
	  aforo(l).sed_out(t,i)=cell(n).SedFlujo(i)  !flujo de sedimentos, para cada i, m3/s
	  aforo(l).sed_out(t,4)=cell(n).t_SedFlujo   !Flujo total de sedimentos, m3/s
	ENDDO
  ENDIF
ENDDO
  !En puntos de simulación, para cada intervalo temporal
DO l=1,knaf !Series temporales de flujo de sedimentos
  IF (n.eq.otros(l).pos)THEN   
    DO i=1,3	!Para cada clase de tamaño
	  otros(l).sed_out(t,i)=cell(n).SedFlujo(i)  !flujo de sedimentos, para cada i, m3/s
	  otros(l).sed_out(t,4)=cell(n).t_SedFlujo   !Flujo total de sedimentos, m3/s
	ENDDO
  ENDIF
ENDDO
  IF (cell(n).dest.eq.(ncel+1)) THEN
    DO i=1,3	!Para cada clase de tamaño
	  sed_SALIDA(i)=sed_SALIDA(i)+cell(n).SedFlujo(i)*dts 
	ENDDO          !Volumen de sedimentos que salen, m3
  ENDIF
!
  !*************************************************************************************
  !En puntos de aforo de sedimentos, para cada intervalo temporal
  DO l=1,ksedq !Series temporales de flujo de sedimentos (sedimentos en ladera)
    IF (n.eq.aforosed(l).pos)THEN   
	  aforosed(l).sim(t)=cell(n).t_SedFlujo   !Flujo total de sedimentos, m3/s
      DO i=1,3	!Para cada clase de tamaño
	    aforosed(l).sed_out(t,i)=cell(n).SedFlujo(i)  !flujo de sedimentos, para cada i, m3/s
      ENDDO
	ENDIF
  ENDDO

    !*************************************************************************************

!6.CALCULOS TOTALES DE PROCESOS SEDIMENTARIOS (SedStats.c en CASC2D-SED)
totscourv = 0.0		!Iniciar en cero el total de sedimentos erodados

DO i=1,3		!Para cada clase de tamaño
  cell(n).t_SusSedLAD=cell(n).t_SusSedLAD+cell(n).SusSedLAD(i)  
  cell(n).t_SusSedRED=cell(n).t_SusSedRED+cell(n).SusSedRED(i)  
  cell(n).t_DepSedLAD=cell(n).t_DepSedLAD+cell(n).DepSedLAD(i)
  cell(n).t_DepSedRED=cell(n).t_DepSedRED+cell(n).DepSedRED(i)
  totscourv=totscourv+cell(n).ErodSed(i)					!m³ de sed.total erodados
ENDDO

!Calcular la erosión total (cambio en la altura de la celda), en mm
cell(n).t_NetEros=(cell(n).t_DepSedLAD+totscourv)*1000/(dx*dx)
!Modificar el Modelo de Elevación Digital, en m
cell(n).cotacorr=cell(n).cotacorr+(cell(n).t_NetEros*0.001)


!Concentración de sedimentos en suspensión (kg/m3)
!Masa de sedimentos en suspensión sobre volumen de mezcla (agua-sedimentos en suspensión)
IF (cell(n).y(2).GT.0.0) THEN   !Evita división por cero
    DO i=1,3
!	  cell(n).SedConc(i)=cell(n).SusSed(i)*nsurcoent(ncp)*Ge*1000.0/(salrio+cell(n).SusSed(i)*nsurcoent(ncp))
	  cell(n).SedConcLAD(i)=cell(n).SusSedLAD(i)*Ge*1000.0/(cell(n).y(2)*arcel/1000.0+cell(n).SusSedLAD(i))   
    ENDDO
!    cell(n).t_SedConc=cell(n).t_SusSed*nsurcoent(ncp)*Ge*1000.0/(salrio+cell(n).t_SusSed*nsurcoent(ncp))
	cell(n).t_SedConcLAD=cell(n).t_SusSedLAD*Ge*1000.0/(cell(n).y(2)*arcel/1000.0+cell(n).t_SusSedLAD)
ELSE
    DO i=1,3
	  cell(n).SedConcRED(i)=0.0   
    ENDDO
    cell(n).t_SedConcRED=0.0
ENDIF
!solo para cárcavas y cauces
IF (salrio.GT.0.0.AND.nw(2,ncp).le.cell(n).acum) THEN
    DO i=1,3
	  cell(n).SedConcRED(i)=cell(n).SusSedRED(i)*Ge*1000.0/(salrio+cell(n).SusSedRED(i))
    ENDDO
    cell(n).t_SedConcRED=cell(n).t_SusSedRED*Ge*1000.0/(salrio+cell(n).t_SusSedRED)
ELSE
    DO i=1,3
	  cell(n).SedConcRED(i)=0.0   
    ENDDO
    cell(n).t_SedConcRED=0.0
ENDIF

!**********************************************************************************  
!En puntos de aforo
DO l=1,naf !Series temporales de concentración de sedimentos transportados en suspensión
  IF (n.eq.aforo(l).pos)THEN   	  
	DO i=1,3
      aforo(l).sed_out(t,i+4)=cell(n).SedConcRED(i) !Concentracion de sed, Kg/m3
	ENDDO
	aforo(l).sed_out(t,8)=cell(n).t_SedConcRED !Concentración total sed, kg/m3
  ENDIF
ENDDO
!En puntos de simulación
DO l=1,knaf !Series temporales de concentración de sedimentos transportados en suspensión
  IF (n.eq.otros(l).pos)THEN   	  
	DO i=1,3
      otros(l).sed_out(t,i+4)=cell(n).SedConcRED(i) !Concentracion de sed, Kg/m3
	ENDDO
	otros(l).sed_out(t,8)=cell(n).t_SedConcRED !Concentración total sed, kg/m3
  ENDIF
ENDDO

!**********************************************************************************
!Series temporales (para verificar funcionamiento del modelo)

DO l=1,naf !Series temporales de variables importantes en los procesos sedimentarios
  IF (n.eq.aforo(l).pos)THEN
    !IF (nw(2,ncp).gt.cell(n).acum) THEN !Celdas en ladera
      !Relaciones geomorfológicas de flujo
      aforo(l).sed_temp(t,1)=cell(n).hladera		!Altura de agua en ladera, en m
      aforo(l).sed_temp(t,2)=cell(n).velladera	    !Velocidad de flujo en ladera, en m/s
      aforo(l).sed_temp(t,3)=cell(n).qladera		!Caudal en ladera, en m3/s
!      aforo(l).sed_temp(t,4)=Qbs   					!Caudal a sección llena surco, en m3/s
!      aforo(l).sed_temp(t,5)=wsurcosl				!Ancho del surco a sección llena, en m!
!	  aforo(l).sed_temp(t,6)=wqs					!Ancho del surco para el caudal que pasa, m
!      aforo(l).sed_temp(t,7)=Asurco					!Area del surco para el caudal que pasa, en m2
!      aforo(l).sed_temp(t,8)=Rhsurco				!Radio hidráulico del surco para el caudal, en m		
      !Procesos en ladera
!      aforo(l).sed_temp(t,9)=KRcapacidad(1)			!Volumen de arena , según K-R, m3
!	  aforo(l).sed_temp(t,10)=KRcapacidad(2)		!Volumen de limo, según K-R, m3
!      aforo(l).sed_temp(t,11)=KRcapacidad(3)		!Volumen de arcilla, según K-R, m3
!      aforo(l).sed_temp(t,12)=ADVcapacidadLADERA(1)	!Volumen de arena por proc.adv, m3
!      aforo(l).sed_temp(t,13)=ADVcapacidadLADERA(2)	!Volumen de limo por proc.adv, m3
!      aforo(l).sed_temp(t,14)=ADVcapacidadLADERA(3)	!Volumen de arcilla por proc.adv, m3
      aforo(l).sed_temp(t,9)=qsSUS(1)				!Vol arena transportada en suspensión, m3....IMPORTANTE
      aforo(l).sed_temp(t,10)=qsSUS(2)				!Vol limo transportado en suspensión, m3....IMPORTANTE
      aforo(l).sed_temp(t,11)=qsSUS(3)				!Vol arcilla transportada en suspensión,m3....IMPORTANTE
!	  aforo(l).sed_temp(t,18)=CaudalUnitario			!Caudal de flujo unitario, en m2/s
!      aforo(l).sed_temp(t,19)=qsKR					!Capacidad de transporte según K-R, en m3
!      aforo(l).sed_temp(t,20)=EXCcapacidadLAD		!Capacidad de transporte en exceso, m3
      aforo(l).sed_temp(t,12)=qsBM(1)				!Vol arena tranp material del lecho, m3....IMPORTANTE
      aforo(l).sed_temp(t,13)=qsBM(2)				!Vol limo tranp material del lecho, m3....IMPORTANTE
      aforo(l).sed_temp(t,14)=qsBM(3)				!Vol arcilla tranp material del lecho, m3....IMPORTANTE
!      aforo(l).sed_temp(t,24)=RESIDcapacidadLAD		!Capacidad de transporte residual, m3
      aforo(l).sed_temp(t,15)=qsEROS(1)				!Volumen de arena erosionada, m3....IMPORTANTE
      aforo(l).sed_temp(t,16)=qsEROS(2)				!Volumen de limo erosionado, m3....IMPORTANTE
      aforo(l).sed_temp(t,17)=qsEROS(3)				!Volumen de arcilla erosionada, m3....IMPORTANTE
	!ENDIF
	IF (nw(2,ncp).le.cell(n).acum.AND.nw(3,ncp).gt.cell(n).acum) THEN  !Celda con cárcava
	  !Relaciones geomorfológicas de flujo
      aforo(l).sed_temp(t,1)=cell(n).hcarcava		!Altura de agua en la cárcava, en m
      aforo(l).sed_temp(t,2)=cell(n).velcarcava	    !Velocidad de flujo en la cárcava, en m/s
      aforo(l).sed_temp(t,3)=cell(n).qcarcava		!Caudal en la cárcava, en m3/s
      aforo(l).sed_temp(t,4)=Qbc   					!Caudal a sección llena en la carcava, en m3/s
      aforo(l).sed_temp(t,5)=wcarcavasl				!Ancho de la cárcava a sección llena, en m
	  aforo(l).sed_temp(t,6)=wqc					!Ancho de la cárcava para el caudal que pasa, m
      aforo(l).sed_temp(t,7)=Acarcava				!Area de la cárcava para el caudal que pasa, en m2
      aforo(l).sed_temp(t,8)=Rhcarcava				!Radio hidráulico de la cárcava para el caudal, en m
      !Procesos en cárcava
!      aforo(l).sed_temp(t,9)=Concw(1)				!Concentración por peso de arena, %
!      aforo(l).sed_temp(t,10)=Concw(2)				!Concentración por peso de limo, %
!      aforo(l).sed_temp(t,11)=Concw(3)				!Concentración por peso de arcilla, %
!      aforo(l).sed_temp(t,12)=EHcapacidad(1)		!Cap transporte de arena, según E-H, m3
!      aforo(l).sed_temp(t,13)=EHcapacidad(2)		!Cap transporte de limo, según E-H, m3
!      aforo(l).sed_temp(t,14)=EHcapacidad(3)		!Cap transporte de arcilla, según E-H, m3
      aforo(l).sed_temp(t,9)=SUSvol(1)				!Vol arena transportada proc.advec,m3....IMPORTANTE
      aforo(l).sed_temp(t,10)=SUSvol(2)				!Vol limo transportado proc.advec,m3....IMPORTANTE
      aforo(l).sed_temp(t,11)=SUSvol(3)				!Vol arcilla transportada proc.advec,m3....IMPORTANTE
!      aforo(l).sed_temp(t,18)=EXCcapacidadCANAL(1)	!Cap transporte en exceso de arena, m3
!      aforo(l).sed_temp(t,19)=EXCcapacidadCANAL(2)	!Cap transporte en exceso de limo, m3
!      aforo(l).sed_temp(t,20)=EXCcapacidadCANAL(3)	!Cap transporte en exceso de arcilla, m3
      aforo(l).sed_temp(t,12)=BMvol(1)				!Vol arena transpor material de lecho, m3....IMPORTANTE
      aforo(l).sed_temp(t,13)=BMvol(2)				!Vol limo transpor material de lecho, m3....IMPORTANTE
      aforo(l).sed_temp(t,14)=BMvol(3)				!Vol arcilla transpor material lecho, m3....IMPORTANTE
	ENDIF
    IF (nw(3,ncp).le.cell(n).acum) THEN  !Celda con cauce
	  !Relaciones geomorfológicas de flujo
      aforo(l).sed_temp(t,1)=cell(n).hcanal			!Altura de agua en el cauce, en m
      aforo(l).sed_temp(t,2)=cell(n).velcanal	    !Velocidad de flujo en el cauce, en m/s
      aforo(l).sed_temp(t,3)=cell(n).qcanal			!Caudal en el cauce, en m3/s
      aforo(l).sed_temp(t,4)=Qb   					!Caudal a sección llena en el cauce, en m3/s
      aforo(l).sed_temp(t,5)=wchan					!Ancho del cauce a sección llena, en m
	  aforo(l).sed_temp(t,6)=wq						!Ancho del cauce para el caudal que pasa, m
      aforo(l).sed_temp(t,7)=Achan					!Area del cauce para el caudal que pasa, en m2
      aforo(l).sed_temp(t,8)=Rh						!Radio hidráulico del cauce para el caudal, en m
      !Procesos en cárcava
 !     aforo(l).sed_temp(t,9)=Concw(1)				!Concentración por peso de arena, %
 !     aforo(l).sed_temp(t,10)=Concw(2)				!Concentración por peso de limo, %
 !     aforo(l).sed_temp(t,11)=Concw(3)				!Concentración por peso de arcilla, %
 !     aforo(l).sed_temp(t,12)=EHcapacidad(1)		!Cap transporte de arena, según E-H, m3
 !     aforo(l).sed_temp(t,13)=EHcapacidad(2)		!Cap transporte de limo, según E-H, m3
 !     aforo(l).sed_temp(t,14)=EHcapacidad(3)		!Cap transporte de arcilla, según E-H, m3
      aforo(l).sed_temp(t,9)=SUSvol(1)				!Vol arena transportada proc.advec,m3....IMPORTANTE
      aforo(l).sed_temp(t,10)=SUSvol(2)				!Vol limo transportado proc.advec,m3....IMPORTANTE
      aforo(l).sed_temp(t,11)=SUSvol(3)				!Vol arcilla transportada proc.advec,m3....IMPORTANTE
 !     aforo(l).sed_temp(t,18)=EXCcapacidadCANAL(1)	!Cap transporte en exceso de arena, m3
 !     aforo(l).sed_temp(t,19)=EXCcapacidadCANAL(2)	!Cap transporte en exceso de limo, m3
 !     aforo(l).sed_temp(t,20)=EXCcapacidadCANAL(3)	!Cap transporte en exceso de arcilla, m3
      aforo(l).sed_temp(t,12)=BMvol(1)				!Vol arena transpor material de lecho, m3....IMPORTANTE
      aforo(l).sed_temp(t,13)=BMvol(2)				!Vol limo transpor material de lecho, m3....IMPORTANTE
      aforo(l).sed_temp(t,14)=BMvol(3)				!Vol arcilla transpor material lecho, m3....IMPORTANTE
	ENDIF   
  ENDIF
ENDDO
!!puntos de simulación
DO l=1,knaf !Series temporales de variables importantes en los procesos sedimentarios
  IF (n.eq.otros(l).pos)THEN
    !IF (nw(2,ncp).gt.cell(n).acum) THEN !Celdas en ladera
      !Relaciones geomorfológicas de flujo
      otros(l).sed_temp(t,1)=cell(n).hladera		!Altura de agua en ladera, en m
      otros(l).sed_temp(t,2)=cell(n).velladera	    !Velocidad de flujo en ladera, en m/s
      otros(l).sed_temp(t,3)=cell(n).qladera		!Caudal en ladera, en m3/s
      otros(l).sed_temp(t,9)=qsSUS(1)				!Vol arena transportada en suspensión, m3....IMPORTANTE
      otros(l).sed_temp(t,10)=qsSUS(2)				!Vol limo transportado en suspensión, m3....IMPORTANTE
      otros(l).sed_temp(t,11)=qsSUS(3)				!Vol arcilla transportada en suspensión,m3....IMPORTANTE
      otros(l).sed_temp(t,12)=qsBM(1)				!Vol arena tranp material del lecho, m3....IMPORTANTE
      otros(l).sed_temp(t,13)=qsBM(2)				!Vol limo tranp material del lecho, m3....IMPORTANTE
      otros(l).sed_temp(t,14)=qsBM(3)				!Vol arcilla tranp material del lecho, m3....IMPORTANTE
      otros(l).sed_temp(t,15)=qsEROS(1)				!Volumen de arena erosionada, m3....IMPORTANTE
      otros(l).sed_temp(t,16)=qsEROS(2)				!Volumen de limo erosionado, m3....IMPORTANTE
      otros(l).sed_temp(t,17)=qsEROS(3)				!Volumen de arcilla erosionada, m3....IMPORTANTE
	!ENDIF
	IF (nw(2,ncp).le.cell(n).acum.AND.nw(3,ncp).gt.cell(n).acum) THEN  !Celda con cárcava
	  !Relaciones geomorfológicas de flujo
      otros(l).sed_temp(t,1)=cell(n).hcarcava		!Altura de agua en la cárcava, en m
      otros(l).sed_temp(t,2)=cell(n).velcarcava	    !Velocidad de flujo en la cárcava, en m/s
      otros(l).sed_temp(t,3)=cell(n).qcarcava		!Caudal en la cárcava, en m3/s
      otros(l).sed_temp(t,4)=Qbc   					!Caudal a sección llena en la carcava, en m3/s
      otros(l).sed_temp(t,5)=wcarcavasl				!Ancho de la cárcava a sección llena, en m
	  otros(l).sed_temp(t,6)=wqc					!Ancho de la cárcava para el caudal que pasa, m
      otros(l).sed_temp(t,7)=Acarcava				!Area de la cárcava para el caudal que pasa, en m2
      otros(l).sed_temp(t,8)=Rhcarcava				!Radio hidráulico de la cárcava para el caudal, en m
      otros(l).sed_temp(t,9)=SUSvol(1)				!Vol arena transportada proc.advec,m3....IMPORTANTE
      otros(l).sed_temp(t,10)=SUSvol(2)				!Vol limo transportado proc.advec,m3....IMPORTANTE
      otros(l).sed_temp(t,11)=SUSvol(3)				!Vol arcilla transportada proc.advec,m3....IMPORTANTE
      otros(l).sed_temp(t,12)=BMvol(1)				!Vol arena transpor material de lecho, m3....IMPORTANTE
      otros(l).sed_temp(t,13)=BMvol(2)				!Vol limo transpor material de lecho, m3....IMPORTANTE
      otros(l).sed_temp(t,14)=BMvol(3)				!Vol arcilla transpor material lecho, m3....IMPORTANTE
	ENDIF
    IF (nw(3,ncp).le.cell(n).acum) THEN  !Celda con cauce
	  !Relaciones geomorfológicas de flujo
      otros(l).sed_temp(t,1)=cell(n).hcanal			!Altura de agua en el cauce, en m
      otros(l).sed_temp(t,2)=cell(n).velcanal	    !Velocidad de flujo en el cauce, en m/s
      otros(l).sed_temp(t,3)=cell(n).qcanal			!Caudal en el cauce, en m3/s
      otros(l).sed_temp(t,4)=Qb   					!Caudal a sección llena en el cauce, en m3/s
      otros(l).sed_temp(t,5)=wchan					!Ancho del cauce a sección llena, en m
	  otros(l).sed_temp(t,6)=wq						!Ancho del cauce para el caudal que pasa, m
      otros(l).sed_temp(t,7)=Achan					!Area del cauce para el caudal que pasa, en m2
      otros(l).sed_temp(t,8)=Rh						!Radio hidráulico del cauce para el caudal, en m
      otros(l).sed_temp(t,9)=SUSvol(1)				!Vol arena transportada proc.advec,m3....IMPORTANTE
      otros(l).sed_temp(t,10)=SUSvol(2)				!Vol limo transportado proc.advec,m3....IMPORTANTE
      otros(l).sed_temp(t,11)=SUSvol(3)				!Vol arcilla transportada proc.advec,m3....IMPORTANTE
      otros(l).sed_temp(t,12)=BMvol(1)				!Vol arena transpor material de lecho, m3....IMPORTANTE
      otros(l).sed_temp(t,13)=BMvol(2)				!Vol limo transpor material de lecho, m3....IMPORTANTE
      otros(l).sed_temp(t,14)=BMvol(3)				!Vol arcilla transpor material lecho, m3....IMPORTANTE
	ENDIF   
  ENDIF
ENDDO

!temp001=cell(22761).depsedlad(1)  
!balance de sedimentos en m3
balanc_sed(t,1)=balanc_sed(t,1)+cell(n).SusSedLAD(1)
balanc_sed(t,2)=balanc_sed(t,2)+cell(n).SusSedLAD(2)
balanc_sed(t,3)=balanc_sed(t,3)+cell(n).SusSedLAD(3)
!IF (nw(2,ncp).le.cell(n).acum) THEN
balanc_sed(t,4)=balanc_sed(t,4)+cell(n).SusSedRED(1)
balanc_sed(t,5)=balanc_sed(t,5)+cell(n).SusSedRED(2)
balanc_sed(t,6)=balanc_sed(t,6)+cell(n).SusSedRED(3)
balanc_sed(t,7)=balanc_sed(t,7)+cell(n).DepSedLAD(1)
balanc_sed(t,8)=balanc_sed(t,8)+cell(n).DepSedLAD(2)
balanc_sed(t,9)=balanc_sed(t,9)+cell(n).DepSedLAD(3)
balanc_sed(t,10)=balanc_sed(t,10)+cell(n).DepSedRED(1)
balanc_sed(t,11)=balanc_sed(t,11)+cell(n).DepSedRED(2)
balanc_sed(t,12)=balanc_sed(t,12)+cell(n).DepSedRED(3)
balanc_sed(t,13)=balanc_sed(t,13)+cell(n).ErodSed(1)
balanc_sed(t,14)=balanc_sed(t,14)+cell(n).ErodSed(2)
balanc_sed(t,15)=balanc_sed(t,15)+cell(n).ErodSed(3)
balanc_sed(t,16)=balanc_sed(t,16)+qsSUS(1)
balanc_sed(t,17)=balanc_sed(t,17)+qsSUS(2)
balanc_sed(t,18)=balanc_sed(t,18)+qsSUS(3)
balanc_sed(t,19)=balanc_sed(t,19)+qsBM(1)
balanc_sed(t,20)=balanc_sed(t,20)+qsBM(2)
balanc_sed(t,21)=balanc_sed(t,21)+qsBM(3)
balanc_sed(t,22)=balanc_sed(t,22)+SUSvol(1)
balanc_sed(t,23)=balanc_sed(t,23)+SUSvol(2)
balanc_sed(t,24)=balanc_sed(t,24)+SUSvol(3)
balanc_sed(t,25)=balanc_sed(t,25)+BMvol(1)
balanc_sed(t,26)=balanc_sed(t,26)+BMvol(2)
balanc_sed(t,27)=balanc_sed(t,27)+BMvol(3)
balanc_sed(t,28)=balanc_sed(t,28)+qsEROS(1)
balanc_sed(t,29)=balanc_sed(t,29)+qsEROS(2)
balanc_sed(t,30)=balanc_sed(t,30)+qsEROS(3)

ENDSUBROUTINE


!********************************************************************************************
!* Calcula al final de la simulación los volúmenes de sedimentos (SedVolumes.c en CASC2D-SED)
!********************************************************************************************
SUBROUTINE sedvolumenes
USE modtet
!USE DFLIB
!USE DFPORT

IMPLICIT NONE


DO n=1,ncel  !Para todas las celdas
  DO i=1,3		!Para cada clase de tamaño
!    IF (nw(2,ncp).gt.cell(n).acum) THEN !Celdas con surcos
!!      tot_ERODADO(i)=tot_ERODADO(i)+cell(n).ErodSed(i)*nsurcoent(ncp)
!      tot_ERODADO(i)=tot_ERODADO(i)+cell(n).ErodSed(i)
!!      sus_TOTAL(i)=sus_TOTAL(i)+cell(n).SusSed(i)*nsurcoent(ncp)
!	  sus_TOTAL(i)=sus_TOTAL(i)+cell(n).SusSed(i)
!!      dep_TOTAL(i)=dep_TOTAL(i)+cell(n).DepSed(i)*nsurcoent(ncp)
!	  dep_TOTAL(i)=dep_TOTAL(i)+cell(n).DepSed(i)
!!      sed_TOTAL(i)=sed_TOTAL(i)+(cell(n).SusSed(i)+cell(n).DepSed(i))*nsurcoent(ncp)
!	  sed_TOTAL(i)=sed_TOTAL(i)+cell(n).SusSed(i)+cell(n).DepSed(i)
!	ELSE   !Celdas con carcava o cauce
      tot_ERODADO(i)=tot_ERODADO(i)+cell(n).ErodSed(i)
      sus_TOTAL(i)=sus_TOTAL(i)+cell(n).SusSedLAD(i)+cell(n).SusSedRED(i)
      dep_TOTAL(i)=dep_TOTAL(i)+cell(n).DepSedLAD(i)+cell(n).DepSedRED(i)
      sed_TOTAL(i)=sed_TOTAL(i)+cell(n).SusSedLAD(i)+cell(n).DepSedLAD(i)+cell(n).SusSedRED(i)+cell(n).DepSedRED(i)+cell(n).ErodSed(i)
!	ENDIF
  ENDDO
ENDDO

!Resta los volumenes iniciales de sedimentos depositados (para cerrar el balance)
!Lee volumenes inciales de sedimentos depositados
artem=archsed(7)
CALL lee_sedantec

DO n=1,ncel
  DO i=1,3
    DepSedIni(i)=DepSedIni(i)+cell(n).DepSedLAD(i)+cell(n).DepSedRED(i)
    SusSedIni(i)=SusSedIni(i)+cell(n).SusSedLAD(i)+cell(n).SusSedRED(i)
    ErodSedIni(i)=ErodSedIni(i)+cell(n).ErodSed(i)+cell(n).ErodSed(i)
  ENDDO
ENDDO

DO i=1,3
  dep_TOTAL(i)=dep_TOTAL(i)-DepSedIni(i)
  sus_TOTAL(i)=sus_TOTAL(i)-SusSedIni(i)
  tot_ERODADO(i)=tot_ERODADO(i)-ErodSedIni(i)
  sed_TOTAL(i)=sed_TOTAL(i)-DepSedIni(i)-SusSedIni(i)-ErodSedIni(i)
ENDDO


!**************************************************************
!Cálculo de sedimentos totales  (WriteSummSed.c en CASC2D-SED)
!**************************************************************

!Iniciar variables en cero
tot_EROSION=0.0
tot_SUSREM=0.0
tot_DEPREM=0.0
tot_REM=0.0
tot_SEDSALIDA=0.0

!Actualizar variables
DO i=1,3
  tot_EROSION=tot_EROSION+tot_ERODADO(i)
  tot_SUSREM=tot_SUSREM+sus_TOTAL(i)
  tot_DEPREM=tot_DEPREM+dep_TOTAL(i)
  tot_REM=tot_REM+sed_TOTAL(i)
  tot_SEDSALIDA=tot_SEDSALIDA+sed_SALIDA(i)
ENDDO
TOT_presas=0
IF (trapeff) THEN
  DO i=1,numpresas
    DO j=1,nt
          TOT_presas=TOT_presas+(dam(i).sedinTE(j,1)+dam(i).sedinTE(j,2)+dam(i).sedinTE(j,3)-dam(i).sedoutTE(j,1)-dam(i).sedoutTE(j,2)-dam(i).sedoutTE(j,3))
    ENDDO
  ENDDO
ENDIF

error_porcent_SED=100.0*(tot_SEDSALIDA+TOT_presas+tot_REM)/tot_SEDSALIDA     !Error en el balance de masa de sedimentos (en m3), en porcentaje

ENDSUBROUTINE


!***************************************************************************
!* Rutina que lee el fichero de condiciones iniciales o finales de sedimentos
!***************************************************************************
SUBROUTINE lee_sedantec
USE Modtet
IMPLICIT NONE
INTEGER ncel2

sale=0
OPEN(26,file=artem,status='old',err=227) 
READ (26,*,err=228)c12,cn,cs
READ (26,*,err=228)c12,ce,cw
READ (26,*,err=228)c12,mi
READ (26,*,err=228)c12,mj
READ (26,*,err=228)c12,ncol
READ (26,*,err=228)c12,nfil
READ (26,*,err=228)c12,ncel2
READ (26,*,err=228)c12,fecin2,horin2
IF (ncel2.ne.ncel) THEN
  GOTO 91
ELSE
  DO n=1,ncel
      cell(n).SusSedLAD=0.0
      cell(n).DepSedLAD=0.0
      cell(n).SusSedRED=0.0
      cell(n).DepSedRED=0.0
      cell(n).ErodSed=0.0
    READ (26,*,err=229)cell(n).SusSedLAD(1),cell(n).SusSedLAD(2),cell(n).SusSedLAD(3), &
  cell(n).SusSedRED(1),cell(n).SusSedRED(2),cell(n).SusSedRED(3), &
  cell(n).DepSedLAD(1),cell(n).DepSedLAD(2),cell(n).DepSedLAD(3), &
  cell(n).DepSedRED(1),cell(n).DepSedRED(2),cell(n).DepSedRED(3)!, &
  !cell(n).ErodSed(1),cell(n).ErodSed(2),cell(n).ErodSed(3) !La erosión se inicia en cero siempre, para que no la vaya acumulando
  ENDDO
ENDIF
91 CLOSE(26)

GOTO 95

227 mensaje=strings(202)
errr=1
CALL errores
GOTO 94
228 mensaje=strings(203)
errr=1
CALL errores
GOTO 94
229 mensaje=strings(204)
errr=1
CALL errores
goto 94

94 WRITE(*,*)strings(800)
sale=2

95 END SUBROUTINE



!**********************************************************************************
!* Rutina que escribe el fichero sedantec.sds (Condiciones iniciales de sedimentos)
!**********************************************************************************
SUBROUTINE escribe_sedantec
USE Modtet
IMPLICIT NONE

OPEN(26,file=artem) 
  IF (lang.eq.1) THEN
    WRITE (26,'(A12,3x,2F14.4)')'NORTE-SUR:  ',cn,cs
    WRITE (26,'(A12,3x,2F14.4)')'ESTE-OESTE: ',ce,cw
    WRITE (26,'(A12,3x,I8)')'COLUMNAS:   ',mi
    WRITE (26,'(A12,3x,I8)')'FILAS:      ',mj
    WRITE (26,'(A12,3x,I8)')'COL-FINAL:  ',ncol
    WRITE (26,'(A12,3x,I8)')'FIL-FINAL:  ',nfil
    WRITE (26,'(A12,3x,I8)')'NUM-CELDAS: ',ncel
    WRITE (26,'(A12,A11,x,A8)')'FECHA-HORA: ',fecfin,horfin
  ELSE IF (lang.eq.2) THEN
    WRITE (26,'(A12,3x,2F14.4)')'NORTH-SOUTH:',cn,cs
    WRITE (26,'(A12,3x,2F14.4)')'EAST-WEST:  ',ce,cw
    WRITE (26,'(A12,3x,I8)')'COLUMNS:    ',mi
    WRITE (26,'(A12,3x,I8)')'ROWS:       ',mj
    WRITE (26,'(A12,3x,I8)')'COL-FINAL:  ',ncol
    WRITE (26,'(A12,3x,I8)')'ROW-FINAL:  ',nfil
    WRITE (26,'(A12,3x,I8)')'CELL-NUMBER:',ncel
    WRITE (26,'(A12,A11,x,A8)')'DATE-TIME:  ',fecfin,horfin
  ENDIF

DO n=1,ncel
  WRITE (26,104)cell(n).SusSedLAD(1),cell(n).SusSedLAD(2),cell(n).SusSedLAD(3), &
  cell(n).SusSedRED(1),cell(n).SusSedRED(2),cell(n).SusSedRED(3), &
  cell(n).DepSedLAD(1),cell(n).DepSedLAD(2),cell(n).DepSedLAD(3), &
  cell(n).DepSedRED(1),cell(n).DepSedRED(2),cell(n).DepSedRED(3), &
  cell(n).ErodSed(1),cell(n).ErodSed(2),cell(n).ErodSed(3)
ENDDO
104 FORMAT (15(F17.8,x))
CLOSE(26)
END SUBROUTINE


!*********************************************************
!* Lee el nombre de los ficheros de entrada de sedimentos
!*********************************************************
SUBROUTINE leearchsed(dirtra_,archsed_,sale_)
USE modtet
IMPLICIT NONE

INTEGER sale_ !,i, errr
CHARACTER archsed_*128(13),dirtra_*128 !,mensaje*200

sale_=0

!Lee nombres de los ficheros a utilizar
OPEN(9,file='filessed.txt',status='old',err=200)
DO i=1,13
  READ(9,*,err=201)archsed_(i)
  archsed_(i)=TRIM(ADJUSTL(dirtra_))//TRIM(ADJUSTL(archsed_(i)))
ENDDO
CLOSE(9)
CALL labels_sed !llama la subrutina que define los mensajes de error con la ruta de los ficheros de sedimentos
GOTO 95

200 mensaje=strings(200)
errr=1
CALL errores
GOTO 94
201 mensaje=strings(201)
errr=1
CALL errores
GOTO 94

94 WRITE(*,*)strings(800)
sale=2

95 END SUBROUTINE


!*********************************************************************
!* Rutina que inicia variables para procesos sedimentarios
!*********************************************************************
SUBROUTINE inicia_varsed
USE modtet
!USE DFLIB
IMPLICIT NONE

!Inicializa las variables de sedimentos en ceros(InitializeVars.c en CASC2D-SED)
qsSUStot=0.0
qsBMtot=0.0
qsEROStot=0.0
SUStot=0.0
DEPtot=0.0
CaudalUnitario=0.0
qsKR=0.0
EXCcapacidadLAD=0.0
RESIDcapacidadLAD=0.0
supply=0.0
adv_factor=0.0
DO i=1,3			!Para cada clase de tamaño
  tot_ERODADO(i)=0.0  !Vol.total de sed.erodados durante la simulación, por cada i
  sus_TOTAL(i)=0.0    !Vol.total de sed.suspendidos, por cada i
  dep_TOTAL(i)=0.0    !Vol.total de sed.depositados, por cada i
  sed_TOTAL(i)=0.0    !Vol.total de sed (suspen+depos), por cada i
  sed_SALIDA(i)=0.0	  !Vol.total de sed. que sale de la cuenca, por cada i
  DepSedIni(i)=0.0    !Volumen total de sedimentos depositados al inicio del evento
  depositacion(i)=0.0
  PorcentDep(i)=0.0
  qsSUS(i)=0.0
  qsBM(i)=0.0
  qsEROS(i)=0.0
  qs(i)=0.0
  KRcapacidad(i)=0.0
  ADVcapacidadLADERA(i)=0.0
  Concw(i)=0.0
  EHcapacidad(i)=0.0
  SUSvol(i)=0.0
  EXCcapacidadCANAL(i)=0.0
  BMvol(i)=0.0
ENDDO
DO n=1,ncel			!Para todas las celdas
  cell(n).qladera=0.0
  cell(n).hladera=0.0
  cell(n).cotacorr=cell(n).cota
  cell(n).t_SedFlujo=0.0
  cell(n).t_SedConcLAD=0.0
  cell(n).t_SedConcRED=0.0
  cell(n).t_SusSedLAD=0.0
  cell(n).t_DepSedLAD=0.0
  cell(n).t_SusSedRED=0.0
  cell(n).t_DepSedRED=0.0
  cell(n).t_NetEros=0.0
  DO i=1,3			!Para cada clase de tamaño
	cell(n).SedFlujo(i)=0.0
	cell(n).SedConcLAD(i)=0.0
	cell(n).SedConcRED(i)=0.0
  ENDDO
ENDDO



!Definición de Constantes de los sedimentos
Ge=2.65					!Gravedad específica, adimensional
gravedad=9.80665    	!Aceleración de la gravedad, m2/s
!KRcoef=58390.0			!Constante de Kilinc, adimensional
!KRcoef=3100.0			!Constante de Kilinc, adimensional, para Goodwin Creek
diamsed(1)=0.00035		!Diámetro de las partículas de arena, en m
diamsed(2)=0.000016		!Diámetro de las partículas de limo, en m
diamsed(3)=0.000001		!Diámetro de las partículas de arcilla, en m
ws(1)=0.36				!Velocidad de caida para las partículas de arena, en m/s
ws(2)=0.00022			!Velocidad de caida para las partículas de limo, en m/s
ws(3)=0.00000086		!Velocidad de caida para las partículas de arcilla, en m/s

END SUBROUTINE



!****************************************************************************************
!* Esta subrutina es para borrar los ficheros Temporales e innecesarios de sedimentos
!****************************************************************************************
SUBROUTINE  borraficsed(checked)
!USE DFLIB
!USE PORTLIB	
USE modtet
IMPLICIT NONE

LOGICAL(KIND=4)checked
!Se elimina los archivos sin preguntar(Vicente. Nov 2019)
!message=MESSAGEBOXQQ('Desea eliminar TODOS los ficheros temporales?'C, &
          !'Borrando...'C,    &
          !MB$ICONQUESTION.OR.MB$YESNO.OR.MB$DEFBUTTON1)

!IF (message.eq.6) THEN
  !Lee nombres de los ficheros a utilizar
  CALL lecfiles(dirtra,arch,resul)
  !Lee ficheros de sedimentos
  CALL leearchsed(dirtra,archsed,sale)

  !artem=TRIM(ADJUSTL(dirtra))//'~*.*'
  !resul=DELFILESQQ(artem)
  !artem=arch(21)
  !resul=DELFILESQQ(artem)
  !artem=arch(3)
  !resul=DELFILESQQ(artem)
  !artem=arch(4)
  !resul=DELFILESQQ(artem)
  !artem=arch(8)
  !resul=DELFILESQQ(artem)
  !artem=arch(10)
  !resul=DELFILESQQ(artem)
  !artem=arch(11)
  !resul=DELFILESQQ(artem) 
  !artem=archsed(7)
  !resul=DELFILESQQ(artem)
  !artem=archsed(8)
  !resul=DELFILESQQ(artem)
  !Subtituimos la rutina de eliminar fichero para no emplear librerias(Vicente.Nov-2019)
  CALL deletefich(TRIM(ADJUSTL(dirtra))//'~*.*')
  CALL deletefich(arch(21))
  CALL deletefich(arch(3))
  CALL deletefich(arch(4))
  CALL deletefich(arch(8))
  CALL deletefich(arch(10))
  CALL deletefich(arch(11))
  CALL deletefich(archsed(7))
  CALL deletefich(archsed(8))  
!ENDIF

RETURN
END SUBROUTINE borraficsed
!
