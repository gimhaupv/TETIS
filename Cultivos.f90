! ************************************************************************************
! ************************************************************************************
! **Subrutinas de lectura de los ficheros exclusivos de TETIS Nitrógeno con Cultivos**
! ************************************************************************************
! ************************************************************************************
    
!***Lee las características de cada uno de los cultivos***
SUBROUTINE lee_carcult
    USE modtet
    IMPLICIT NONE

    sale=0
    
    k=0
    OPEN(10,file=archnit(16),status='old',err=248)
    ios=0
    DO WHILE (ios.ne.-1)
      READ(10,*,iostat=ios) j
      IF (ios.ne.-1) k=k+1
    ENDDO
    REWIND(10)
    IF (ALLOCATED(carcult)) DEALLOCATE(carcult)
    ALLOCATE(carcult(k,14))
    carcult=-99
    DO i=1,k
        READ(10,*,err=249)(carcult(i,j),j=1,12)
        !carcult(i,14)=carcult(i,13) !No se está modificando el input de nitrógeno orgánico tras la cosecha.
        carcult(i,13)=(alog(carcult(i,7))+carcult(i,7)-alog(carcult(i,5))-carcult(i,5))/(carcult(i,4)-carcult(i,3)) !K2 se calcula aquí para no hacerlo cada paso de tiempo. No cambia
        If (carcult(i,13)<0) then
            carcult(i,13)=(alog(carcult(i,7))+carcult(i,7)-alog(carcult(i,5))-carcult(i,5))/(365-carcult(i,3)+carcult(i,4))
        End if
    ENDDO
    cultmax=k !Almacena el número total de cultivos que hay en rotación en todos los usos del suelo
    CLOSE(10)
    GOTO 111

    248 mensaje=strings(418)
    errr=1
    CALL errores
    GOTO 94
    249 mensaje=strings(419)
    errr=1
    CALL errores
    GOTO 94

    94  WRITE(*,*)strings(800)
    sale=2
111 END SUBROUTINE
    
 !***Lee el código de aquellos usos del suelo que se quiere que funcionen con la opción de cultivos y el número de cultivos en rotación en cada uso del suelo***
SUBROUTINE lee_codcult
    USE modtet
    IMPLICIT NONE

    sale=0
    
    k=0
    OPEN(10,file=archnit(15),status='old',err=248)
    ios=0
    DO WHILE (ios.ne.-1)
      READ(10,*,iostat=ios) j
      IF (ios.ne.-1) k=k+1
    ENDDO
    REWIND(10)
    IF (ALLOCATED(codcult)) DEALLOCATE(codcult)
    IF (ALLOCATED(nrotacion)) DEALLOCATE(nrotacion)
    ALLOCATE(codcult(k),nrotacion(k))
    codcult=-99
    DO i=1,k
        READ(10,*,err=249)codcult(i),nrotacion(i)
    ENDDO
    ncult=k
    CLOSE(10)
    GOTO 111

    248 mensaje=strings(415)
    errr=1
    CALL errores
    GOTO 94
    249 mensaje=strings(416)
    errr=1
    CALL errores
    GOTO 94

    94  WRITE(*,*)strings(800)
    sale=2
111 END SUBROUTINE   
 
!***Lee el valor del factor de vegetación diario para los usos del suelo que funcionan en modo cultivo***
!El fichero escribe en columnas el factor de vegetación diario, las columnas en el orden de usos del suelo. Pero sólo se escriben los que estén en modo cultivo   
SUBROUTINE lee_factvegcultivos
    USE modtet
    IMPLICIT NONE
    
    Integer massimocodveg
    massimocodveg=maxval(cell(:).codveg)
    
    sale=0

    k=0
    OPEN(10,file=arch(40),status='old',err=248)
    IF (ALLOCATED(codcult)) DEALLOCATE(codcult)
    IF (ALLOCATED(fvegcult)) DEALLOCATE(fvegcult)
    IF (ALLOCATED(dummy7)) DEALLOCATE(dummy7)
    Read(10,*,err=249)ncult
    ALLOCATE(fvegcult(365,massimocodveg),dummy7(366,ncult),codcult(ncult))
    fvegcult=-99
    DO i=1,366
        READ(10,*,err=249)(dummy7(i,j),j=1,ncult)
    ENDDO
    Do i=1,ncult
        codcult(i)=dummy7(1,i)
    End do

    !Rellenar la matriz en las columnas adecuadas, para que coincida columna con uso del suelo, de esta forma aunque gasta más memoria, es mucho más sencillo rescatar el valor luego
    Do i=1,massimocodveg
        Do j=1,ncult
            If (i==dummy7(1,j)) then
                Do k=1,365
                    fvegcult(k,i)=dummy7(k+1,j)
                End do
            End if
        End do
    End do
    CLOSE(10)
    GOTO 111

    248 mensaje=strings(420)
    errr=1
    CALL errores
    GOTO 94
    249 mensaje=strings(421)
    errr=1
    CALL errores
    GOTO 94

    94  WRITE(*,*)strings(800)
    sale=2
111 END SUBROUTINE 
    

!Lee los inputs de nitrógeno orgánico, amonio y nitrato de las celdas en modo cultivo    
SUBROUTINE lee_Ninputsuelo_Cultivos
    USE Modtet
    IMPLICIT NONE
    Real,allocatable:: matriz(:,:)
    Integer ndatos,massimocodveg

    massimocodveg=maxval(cell(:).codveg)
    sale=0
    
    k=0
    OPEN(10,file=archnit(17),status='old',err=248)
    IF (ALLOCATED(codcult)) DEALLOCATE(codcult)
    IF (ALLOCATED(amonioscult)) DEALLOCATE(amonioscult)
    IF (ALLOCATED(dummy7)) DEALLOCATE(dummy7)
    Read(10,*,err=249)ncult
    ALLOCATE(amonioscult(365,massimocodveg),dummy7(366,ncult),codcult(ncult))
    amonioscult=0.0
    DO i=1,366
        READ(10,*,err=249)(dummy7(i,j),j=1,ncult)
    ENDDO
    Do i=1,ncult
        codcult(i)=dummy7(1,i)
    End do
    !Rellenar la matriz en las columnas adecuadas, para que coincida columna con uso del suelo, de esta forma aunque gasta más memoria, es mucho más sencillo rescatar el valor luego
    Do i=1,massimocodveg
        Do j=1,ncult
            If (i==dummy7(1,j)) then
                Do k=1,365
                    amonioscult(k,i)=dummy7(k+1,j)/(24*60)*dtmin   !Se almacena en cada njulian el valor del dt
                End do
            End if
        End do
    End do
    CLOSE(10)
    
    k=0
    OPEN(10,file=archnit(18),status='old',err=250)
    IF (ALLOCATED(codcult)) DEALLOCATE(codcult)
    IF (ALLOCATED(nitratoscult)) DEALLOCATE(nitratoscult)
    IF (ALLOCATED(dummy7)) DEALLOCATE(dummy7)
    Read(10,*,err=249)ncult
    ALLOCATE(nitratoscult(365,massimocodveg),dummy7(366,ncult),codcult(ncult))
    nitratoscult=0.0
    DO i=1,366
        READ(10,*,err=251)(dummy7(i,j),j=1,ncult)
    ENDDO
    Do i=1,ncult
        codcult(i)=dummy7(1,i)
    End do
    !Rellenar la matriz en las columnas adecuadas, para que coincida columna con uso del suelo, de esta forma aunque gasta más memoria, es mucho más sencillo rescatar el valor luego
    Do i=1,massimocodveg
        Do j=1,ncult
            If (i==dummy7(1,j)) then
                Do k=1,365
                    nitratoscult(k,i)=dummy7(k+1,j)/(24*60)*dtmin   !Se almacena en cada njulian el valor del dt
                End do
            End if
        End do
    End do
    CLOSE(10)
    GOTO 111

    248 mensaje=strings(422)
    errr=1
    CALL errores
    GOTO 94
    249 mensaje=strings(423)
    errr=1
    CALL errores
    GOTO 94
    250 mensaje=strings(424)
    errr=1
    CALL errores
    GOTO 94
    251 mensaje=strings(425)
    errr=1
    CALL errores
    GOTO 94

    94  WRITE(*,*)strings(800)
    sale=2
111 END SUBROUTINE
    
    
!*************************************************************************************
!*************************************************************************************
!***Subrutina que calcula la fila de cultivo en la que se está trabajando el día n****
!*************************************************************************************
!*************************************************************************************
Subroutine crop_growth
    USE modtet
    IMPLICIT NONE
    
    !poscodcult almacena la fila del uso del suelo, para poder rescatar nrotacion de ese uso del suelo
    compcultivo2=0
    Do i=1,cultmax
        If (cell(n).codveg==carcult(i,1)) then
            Do j=i,i-1+nrotacion(poscodcult)
                If (njulian>=carcult(j,3).and.njulian<=carcult(j,4)) then
                    Cultivo=j
                    call crop_growth2
                    compcultivo2=1
                    Cycle !Sale del do interior, pero no del exterior
                End if
            End do
            plantacionultimo=0
            cosechaultimo=0
            filaultimo=0
            Do j=i,i-1+nrotacion(poscodcult)
                If(plantacionultimo<carcult(j,3)) then
                    plantacionultimo=carcult(j,3)
                    cosechaultimo=carcult(j,4)
                    filaultimo=j
                End if
            End do
            If (compcultivo2==1) then
                cell(n).w=cell(n).w
                Exit
            Else if (njulian>=plantacionultimo.or.njulian<=cosechaultimo) then
                If(plantacionultimo>cosechaultimo) then
                    Cultivo=filaultimo
                    call crop_growth2
                    Exit
                else
                    cell(n).w=0.0
                    cell(n).fcncult=0.0
                    cell(n).nw=0.0
                    !La asimilación es cero si no hay cultivos
                    cell(n).fn(9)=0.0
                    cell(n).fn(10)=0.0
                    cell(n).fn(30)=0.0
                    cell(n).fn(31)=0.0
                    Exit
                End if
            Else
                cell(n).w=0.0
                cell(n).nw=0.0
                cell(n).fcncult=0.0
                !La asimilación es cero si no hay cultivos
                cell(n).fn(9)=0.0
                cell(n).fn(10)=0.0
                cell(n).fn(30)=0.0
                cell(n).fn(31)=0.0
                Exit
            End if
        End if
    End do
    End subroutine
    
    
!*************************************************************************************
!*************************************************************************************
!******Subrutina que calcula el crecimiento del cultivo y la asimilación activa*******
!*************************************************************************************
!*************************************************************************************
Subroutine crop_growth2
    USE modtet
    IMPLICIT NONE
    
    !Se entra con la variable cultivo que es el número de la fila que toca de carcult(i,j)
    !Hay que distinguir varios casos:
        !t=1, sólo se conoce cell(n).w pero no el nitrógeno extraído hasta este momento
        !njulian=fecha de siembra. se conoce el cell(n).w con el que se planta pero no el nitrógeno extraído hasta el momento
        !Resto, se conoce cell(n).w y cell(n).nw, sin problemas
    If (t==1.and.njulian/=carcult(cultivo,3)) then
        crecimiento(n)=0  !Se inicia la variable
        !Este caso es especial porque sabemos wini porque lo lee de cultantec, el problema es que no sabemos cuánto nitrógeno ha extraído hasta esa fecha. Por lo tanto,
        !asumimos que el día anterior creció sin límite de nitrógeno y con Gt y Gw de t=1. De esta forma se calcula Wanterior y con ella Ncrit. Al final tomamos Ncrit como valor
        !de nitrógeno extraído hasta t=1.
        !Cálculo de la distribución entre evaporación de suelo desnudo y transpiración
        If (cell(n).x(7)>0.0.and.tiporiego(cell(n).codrie)==3) then
            !Riego por goteo (se aplica en las raíces directamente, primero transpira y lo que sobra se evapora)
            cell(n).tr=Min(cell(n).y(1),((evpt*r(2)-cell(n).ei)*fvegcult(njulian,cell(n).codveg)*functeta*cell(n).fcncult),cell(n).x(7))
            cell(n).es=cell(n).y(1)-cell(n).tr
            If(cell(n).tr<(evpt*r(2)-cell(n).ei)*fvegcult(njulian,cell(n).codveg)*functeta*cell(n).fcncult) then
                If(cell(n).es>0.0) then
                    cell(n).tr=cell(n).tr+cell(n).es*cell(n).fcncult
                    cell(n).es=cell(n).es*(1-cell(n).fcncult)
                End if
            End if
        Else !Sin riego o riego por aspersión o a manta, es homogéneo luego sólo se divide por el factor de cubierta
            cell(n).tr=cell(n).y(1)*cell(n).fcncult  
            cell(n).es=cell(n).y(1)-cell(n).tr
        End if
        cell(n).w=cell(n).w*10/arcel !Paso a (t/ha)
        !Cálculo de los factores limitantes de crecimiento (nitrógeno, transpiración (agua) y temperatura)
        Gn=1
        If(cell(n).tr<0.0000005) then
            Gw=0.0
        else
            Gw=Min(cell(n).tr/((evpt*r(2)-cell(n).ei)*fvegcult(njulian,cell(n).codveg)*cell(n).fcncult),1.0) !Transpirado/Potencial
        end if
        If (cell(n).x(0)>10) then
            Gt=1.0
        Else if (cell(n).x(0)<carcult(cultivo,9)) then
            Gt=0.0
        Else
            Gt=(cell(n).x(0)-carcult(cultivo,9))/(10-carcult(cultivo,9))
        End if
        !Cálculo del crecimiento teórico (aproximación)
        dw=(carcult(cultivo,13)*Gw*Gn*Gt*cell(n).w)/(1+cell(n).w) !(t/ha)
        !Cálculo del peso seco anterior
        Want=cell(n).w-dW !(t/ha)
        !Cálculo del nitrógeno crítico anterior
        Ncritant=carcult(cultivo,10)*(1+carcult(cultivo,11)*exp(-0.26*Want))/100 !% del peso seco en tanto por 1
        !Nitrógeno extraído hasta este dt. Necesario para calcular cuánto debe extraer en t=1
        Nextrant=Ncritant*Want !(tN/ha)
        !Almacenar en la variable
        cell(n).nw=Nextrant*arcel/10 !(kgN/celda)
    Else if (njulian==carcult(cultivo,3)) then 
        crecimiento(n)=0  !Se inicia la variable
        !Caso similar a t=1, pero para empezar con cultivo nuevo asumimos que hasta ahora ha crecido en condiciones óptimas porque vendrá de invernadero.
        !En este caso cell(n).w corresponde al inicial del cultivo, escrito en características
        cell(n).w=carcult(cultivo,5) !ya está en (t/ha)
        cell(n).fcncult=carcult(cultivo,6)
        !Cálculo de la distribución entre evaporación de suelo desnudo y transpiración
        If (cell(n).x(7)>0.0.and.tiporiego(cell(n).codrie)==3) then
            !Riego por goteo (se aplica en las raíces directamente, primero transpira y lo que sobra se evapora)
            cell(n).tr=Min(cell(n).y(1),((evpt*r(2)-cell(n).ei)*fvegcult(njulian,cell(n).codveg)*functeta*cell(n).fcncult),cell(n).x(7))
            cell(n).es=cell(n).y(1)-cell(n).tr
            If(cell(n).tr<(evpt*r(2)-cell(n).ei)*fvegcult(njulian,cell(n).codveg)*functeta*cell(n).fcncult) then
                If(cell(n).es>0.0) then
                    cell(n).tr=cell(n).tr+cell(n).es*cell(n).fcncult
                    cell(n).es=cell(n).es*(1-cell(n).fcncult)
                End if
            End if
        Else !Sin riego o riego por aspersión o a manta, es homogéneo luego sólo se divide por el factor de cubierta
            cell(n).tr=cell(n).y(1)*cell(n).fcncult  
            cell(n).es=cell(n).y(1)-cell(n).tr
        End if
        !Cálculo de los factores limitantes de crecimiento (nitrógeno, transpiración (agua) y temperatura)
        Gn=1
        Gw=1
        Gt=1
        !Cálculo del crecimiento teórico
        dw=(carcult(cultivo,13)*Gw*Gn*Gt*cell(n).w)/(1+cell(n).w) !(t/ha)
        !Cálculo del peso seco anterior
        Want=cell(n).w-dw !(t/ha)
        !Cálculo del nitrógeno crítico anterior
        Ncritant=carcult(cultivo,10)*(1+carcult(cultivo,11)*exp(-0.26*Want))/100 !% del peso seco en tanto por 1
        !Nitrógeno extraído hasta este dt. Necesario para calcular cuánto debe extraer en t=1
        Nextrant=Ncritant*Want !(tN/ha)
        !Almacenar en la variable
        cell(n).nw=Nextrant*arcel/10 !(kgN/celda)
    Else
        !Cálculo de la distribución entre evaporación de suelo desnudo y transpiración
        If (cell(n).x(7)>0.0.and.tiporiego(cell(n).codrie)==3) then
            !Riego por goteo (se aplica en las raíces directamente, primero transpira y lo que sobra se evapora)
            cell(n).tr=Min(cell(n).y(1),((evpt*r(2)-cell(n).ei)*fvegcult(njulian,cell(n).codveg)*functeta*cell(n).fcncult),cell(n).x(7))
            cell(n).es=cell(n).y(1)-cell(n).tr
            If(cell(n).tr<(evpt*r(2)-cell(n).ei)*fvegcult(njulian,cell(n).codveg)*functeta*cell(n).fcncult) then
                If(cell(n).es>0.0) then
                    cell(n).tr=cell(n).tr+cell(n).es*cell(n).fcncult
                    cell(n).es=cell(n).es*(1-cell(n).fcncult)
                End if
            End if
        Else !Sin riego o riego por aspersión o a manta, es homogéneo luego sólo se divide por el factor de cubierta
            cell(n).tr=cell(n).y(1)*cell(n).fcncult  
            cell(n).es=cell(n).y(1)-cell(n).tr
        End if
        cell(n).w=cell(n).w*10/arcel !Paso a (t/ha)
        !Nitrógeno extraído hasta este dt
        Nextrant=cell(n).nw*10/arcel !(tN/ha)
    End if
    
    !Cálculo del nitrógeno crítico. Nitrógeno que debería haber extraído la planta hasta t + el que debería extraer ahora para que no sea limitante
    cell(n).Ncrit=(carcult(cultivo,10)*(1+carcult(cultivo,11)*exp(-0.26*cell(n).w)))/100 !% del peso seco en tanto por 1.

    !Cálculo de la asimilación pasiva
    !Volumen de agua que queda en el suelo
    Volag3=h1_ini+cell(n).d(1)+h3_ini+cell(n).x(3)-cell(n).x(4)-cell(n).es+cell(n).hlim
    If (Volag3<0.000005.or.cell(n).tr<0.000005) then  
        cell(n).fn(9)=0.0    !no se actualizan los tanques porque no hay salida
        cell(n).fn(10)=0.0
    Else if (abs(Volag3-cell(n).tr<0.0000005)) then  !El volumen de agua en el suelo es igual al transpirado. Sale todo o lo potencial
        cell(n).fn(9)=cell(n).hn(1)                               !Volag3 lo trae de simcelda_nitr
        cell(n).fn(10)=cell(n).hn(2)
        cell(n).hn(1)=0.0
        cell(n).hn(2)=0.0
    else
        !Cálculo de las concentraciones en agua de amonio y nitrato (kg/mm)
        ConcNH4=cell(n).hn(1)/Volag3
        ConcNO3=cell(n).hn(2)/Volag3
        !Cálculo de las salidas por transpiración 
        cell(n).fn(9)=ConcNH4*cell(n).tr
        cell(n).fn(10)=ConcNO3*cell(n).tr
        !Actualización de los tanques en suelo
        cell(n).hn(1)=cell(n).hn(1)-cell(n).fn(9)
        cell(n).hn(2)=cell(n).hn(2)-cell(n).fn(10)
    End if
    
    !Cálculo del nitrógeno que debería extraer en este paso de tiempo como asimilación activa.
    cell(n).Nextr=(cell(n).Ncrit*cell(n).w-Nextrant)*arcel/10-cell(n).fn(9)-cell(n).fn(10) !(kgN/celda) se hace la transformación de fn9 y fn10

    !Cálculo de los flujos de nitrógeno de asimilación activa
    If (cell(n).Nextr>0.and.Volag11>0.0000005) then
        !Cálculo de las concentraciones en agua de amonio y nitrato (kg/mm)
        ConcNH4=cell(n).hn(1)/Volag11     !Volag11 lo trae de simcelda_nitr
        ConcNO3=cell(n).hn(2)/Volag11
        !Cálculo de las salidas por asimilación activa
        cell(n).fn(30)=Min((ConcNH4*F(cell(n).codveg)*((Volag11)/(cell(n).psuelo*r(1)*1000))**3.0),cell(n).Nextr*(1-carcult(cultivo,12)),cell(n).hn(1)) !(kgN/celda) !Extrae lo que le toca o lo que hay en el suelo
        cell(n).fn(31)=Min((ConcNO3*F(cell(n).codveg)*((Volag11)/(cell(n).psuelo*r(1)*1000))**3.0),cell(n).Nextr*(carcult(cultivo,12)),cell(n).hn(2)) !(kgN/celda)  
        Compactiva1=0
        Compactiva2=0
        If (cell(n).Nextr>(cell(n).fn(30)+cell(n).fn(31))) then !Se completa en caso de que no se llegue por haber establecido preferencias
            If (abs(cell(n).fn(30)-Min((ConcNH4*F(cell(n).codveg)*((Volag11)/(cell(n).psuelo*r(1)*1000))**3.0),cell(n).hn(1)))<0.0000005) then !Llega al límite de NH4
                If (abs(cell(n).fn(31)-Min((ConcNO3*F(cell(n).codveg)*((Volag11)/(cell(n).psuelo*r(1)*1000))**3.0),cell(n).hn(2)))>0.0000005) then !Pero no al de NO3, completo con NO3 hasta demanda o límite difusivo
                    Compactiva1=1
                End if
            End if           
            If (abs(cell(n).fn(31)-Min((ConcNO3*F(cell(n).codveg)*((Volag11)/(cell(n).psuelo*r(1)*1000))**3.0),cell(n).hn(2)))<0.0000005) then !Llego al límite de NO3
                If (abs(cell(n).fn(30)-Min((ConcNH4*F(cell(n).codveg)*((Volag11)/(cell(n).psuelo*r(1)*1000))**3.0),cell(n).hn(1)))>0.0000005) then !Pero no al de NH4, completo con NH4 hasta demanda o límite difusivo
                    Compactiva2=1
                End if
            End if
            !Se completan las salidas en caso de no haber llegado a alguno de los límites
            If (Compactiva1==1) then
                !Recalculo la concentración tras la salida
                ConcNO3=(cell(n).hn(2)-cell(n).fn(31))/Volag11
                cell(n).fn(31)=cell(n).fn(31)+Min((ConcNO3*F(cell(n).codveg)*((Volag11)/(cell(n).psuelo*r(1)*1000))**3.0),cell(n).hn(2)-cell(n).fn(31),cell(n).Nextr-(cell(n).fn(30)+cell(n).fn(31)))
            End if
            If (Compactiva2==1) then
                !Recalculo la concentración tras la salida
                ConcNH4=(cell(n).hn(1)-cell(n).fn(30)*18/14)/Volag11
                cell(n).fn(30)=cell(n).fn(30)+Min((ConcNH4*F(cell(n).codveg)*((Volag11)/(cell(n).psuelo*r(1)*1000))**3.0),cell(n).hn(1)-cell(n).fn(30),cell(n).Nextr-(cell(n).fn(30)+cell(n).fn(31)))
            End if  
        End if
    Else !Si con la asimilación pasiva se ha llegado a la cantidad de nitrógeno requerida, la vegetación no activa el flujo difusivo para asimilación activa.
        cell(n).fn(30)=0.0
        cell(n).fn(31)=0.0
    End if
    !Los almacenamientos tras la asimilación activa se actualizan en simcelda_nitr al final del paso de asimilación activa

    !Cálculo del nitrógeno extraído hasta este paso de tiempo + el de este dt
    cell(n).nw=cell(n).nw+cell(n).fn(9)+cell(n).fn(10)+cell(n).fn(30)+cell(n).fn(31) !(kgN/celda)
    
    !Cálculo de los factores limitantes de crecimiento (nitrógeno, transpiración (agua) y temperatura)
    porcentajeN=cell(n).fn(9)+cell(n).fn(10)+cell(n).fn(30)+cell(n).fn(31) !Cantidad de nitrógeno extraída en este paso de tiempo
    
    !Cálculo del nitrógeno que debería haber extraído en este paso de tiempo (demanda potencial de este paso de tiempo) en KgN/celda
    If((cell(n).Ncrit*cell(n).w-Nextrant)*arcel/10<0.0) then
        cell(n).Nextr=0.0
        Gn=1
    Else
        cell(n).Nextr=(cell(n).Ncrit*cell(n).w-Nextrant)*arcel/10 !(kgN/celda)
        Gn=min(porcentajeN/cell(n).Nextr,1.0)
    End if
    If(cell(n).tr<0.0000005) then
        Gw=0.0
    else
        Gw=Min(cell(n).tr/((evpt*r(2)-cell(n).ei)*fvegcult(njulian,cell(n).codveg)*cell(n).fcncult),1.0) !Transpirado/Potencial
    end if
    If (cell(n).x(0)>10) then
        Gt=1.0
    Else if (cell(n).x(0)<carcult(cultivo,9)) then
        Gt=0.0
    Else
        Gt=(cell(n).x(0)-carcult(cultivo,9))/(10-carcult(cultivo,9))
    End if

    !Cálculo del crecimineto en este dt
    dw=(carcult(cultivo,13)*Gw*Gn*Gt*cell(n).w)/(1+cell(n).w) !(t/ha)
    !Cálculo del peso seco y el factor de cubierta final del intervalo
    cell(n).w=cell(n).w+dw !(t/ha)
    !El factor de cubierta aumenta de forma proporcional al peso seco
    !Se corrige el factor de cubierta en función al factor de vegeteación poruqe realmente cuando ha alcanzado la etapa de "mediados de temporada" según la FAO,
    !aunque siga creciendo algo, las necesidades de agua son las mismas, por eso el factor de vegetación no aumenta.
    If(crecimiento(n)==0) then
        If(njulian==1) then
            If(fvegcult(njulian,cell(n).codveg)<fvegcult(365,cell(n).codveg).and.njulian/=carcult(cultivo,3)) then
                crecimiento(n)=1
            Else
                cell(n).fcncult=(cell(n).w-carcult(cultivo,5))/(carcult(cultivo,7)-carcult(cultivo,5))*(carcult(cultivo,8)-carcult(cultivo,6))+carcult(cultivo,6)
            End if
        Else
            If(fvegcult(njulian,cell(n).codveg)<fvegcult(njulian-1,cell(n).codveg).and.njulian/=carcult(cultivo,3)) then
                crecimiento(n)=1
            Else
                cell(n).fcncult=(cell(n).w-carcult(cultivo,5))/(carcult(cultivo,7)-carcult(cultivo,5))*(carcult(cultivo,8)-carcult(cultivo,6))+carcult(cultivo,6)
            End if
        End if
    Else
        cell(n).fcncult=cell(n).fcncult
    End if
    if(cell(n).fcncult>carcult(cultivo,8)) then
        cell(n).fcncult=carcult(cultivo,8)
    End if
    cell(n).w=cell(n).w*arcel/10 !(kg/celda)
        
End subroutine
    
    