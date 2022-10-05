! ************************************************************************************
! ************************************************************************************
! **Subrutinas de lectura de los ficheros exclusivos de TETIS Nitr�geno con Cultivos**
! ************************************************************************************
! ************************************************************************************
    
!***Lee las caracter�sticas de cada uno de los cultivos***
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
        !carcult(i,14)=carcult(i,13) !No se est� modificando el input de nitr�geno org�nico tras la cosecha.
        carcult(i,13)=(alog(carcult(i,7))+carcult(i,7)-alog(carcult(i,5))-carcult(i,5))/(carcult(i,4)-carcult(i,3)) !K2 se calcula aqu� para no hacerlo cada paso de tiempo. No cambia
        If (carcult(i,13)<0) then
            carcult(i,13)=(alog(carcult(i,7))+carcult(i,7)-alog(carcult(i,5))-carcult(i,5))/(365-carcult(i,3)+carcult(i,4))
        End if
    ENDDO
    cultmax=k !Almacena el n�mero total de cultivos que hay en rotaci�n en todos los usos del suelo
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
    
 !***Lee el c�digo de aquellos usos del suelo que se quiere que funcionen con la opci�n de cultivos y el n�mero de cultivos en rotaci�n en cada uso del suelo***
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
 
!***Lee el valor del factor de vegetaci�n diario para los usos del suelo que funcionan en modo cultivo***
!El fichero escribe en columnas el factor de vegetaci�n diario, las columnas en el orden de usos del suelo. Pero s�lo se escriben los que est�n en modo cultivo   
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

    !Rellenar la matriz en las columnas adecuadas, para que coincida columna con uso del suelo, de esta forma aunque gasta m�s memoria, es mucho m�s sencillo rescatar el valor luego
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
    

!Lee los inputs de nitr�geno org�nico, amonio y nitrato de las celdas en modo cultivo    
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
    !Rellenar la matriz en las columnas adecuadas, para que coincida columna con uso del suelo, de esta forma aunque gasta m�s memoria, es mucho m�s sencillo rescatar el valor luego
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
    !Rellenar la matriz en las columnas adecuadas, para que coincida columna con uso del suelo, de esta forma aunque gasta m�s memoria, es mucho m�s sencillo rescatar el valor luego
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
!***Subrutina que calcula la fila de cultivo en la que se est� trabajando el d�a n****
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
                    !La asimilaci�n es cero si no hay cultivos
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
                !La asimilaci�n es cero si no hay cultivos
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
!******Subrutina que calcula el crecimiento del cultivo y la asimilaci�n activa*******
!*************************************************************************************
!*************************************************************************************
Subroutine crop_growth2
    USE modtet
    IMPLICIT NONE
    
    !Se entra con la variable cultivo que es el n�mero de la fila que toca de carcult(i,j)
    !Hay que distinguir varios casos:
        !t=1, s�lo se conoce cell(n).w pero no el nitr�geno extra�do hasta este momento
        !njulian=fecha de siembra. se conoce el cell(n).w con el que se planta pero no el nitr�geno extra�do hasta el momento
        !Resto, se conoce cell(n).w y cell(n).nw, sin problemas
    If (t==1.and.njulian/=carcult(cultivo,3)) then
        crecimiento(n)=0  !Se inicia la variable
        !Este caso es especial porque sabemos wini porque lo lee de cultantec, el problema es que no sabemos cu�nto nitr�geno ha extra�do hasta esa fecha. Por lo tanto,
        !asumimos que el d�a anterior creci� sin l�mite de nitr�geno y con Gt y Gw de t=1. De esta forma se calcula Wanterior y con ella Ncrit. Al final tomamos Ncrit como valor
        !de nitr�geno extra�do hasta t=1.
        !C�lculo de la distribuci�n entre evaporaci�n de suelo desnudo y transpiraci�n
        If (cell(n).x(7)>0.0.and.tiporiego(cell(n).codrie)==3) then
            !Riego por goteo (se aplica en las ra�ces directamente, primero transpira y lo que sobra se evapora)
            cell(n).tr=Min(cell(n).y(1),((evpt*r(2)-cell(n).ei)*fvegcult(njulian,cell(n).codveg)*functeta*cell(n).fcncult),cell(n).x(7))
            cell(n).es=cell(n).y(1)-cell(n).tr
            If(cell(n).tr<(evpt*r(2)-cell(n).ei)*fvegcult(njulian,cell(n).codveg)*functeta*cell(n).fcncult) then
                If(cell(n).es>0.0) then
                    cell(n).tr=cell(n).tr+cell(n).es*cell(n).fcncult
                    cell(n).es=cell(n).es*(1-cell(n).fcncult)
                End if
            End if
        Else !Sin riego o riego por aspersi�n o a manta, es homog�neo luego s�lo se divide por el factor de cubierta
            cell(n).tr=cell(n).y(1)*cell(n).fcncult  
            cell(n).es=cell(n).y(1)-cell(n).tr
        End if
        cell(n).w=cell(n).w*10/arcel !Paso a (t/ha)
        !C�lculo de los factores limitantes de crecimiento (nitr�geno, transpiraci�n (agua) y temperatura)
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
        !C�lculo del crecimiento te�rico (aproximaci�n)
        dw=(carcult(cultivo,13)*Gw*Gn*Gt*cell(n).w)/(1+cell(n).w) !(t/ha)
        !C�lculo del peso seco anterior
        Want=cell(n).w-dW !(t/ha)
        !C�lculo del nitr�geno cr�tico anterior
        Ncritant=carcult(cultivo,10)*(1+carcult(cultivo,11)*exp(-0.26*Want))/100 !% del peso seco en tanto por 1
        !Nitr�geno extra�do hasta este dt. Necesario para calcular cu�nto debe extraer en t=1
        Nextrant=Ncritant*Want !(tN/ha)
        !Almacenar en la variable
        cell(n).nw=Nextrant*arcel/10 !(kgN/celda)
    Else if (njulian==carcult(cultivo,3)) then 
        crecimiento(n)=0  !Se inicia la variable
        !Caso similar a t=1, pero para empezar con cultivo nuevo asumimos que hasta ahora ha crecido en condiciones �ptimas porque vendr� de invernadero.
        !En este caso cell(n).w corresponde al inicial del cultivo, escrito en caracter�sticas
        cell(n).w=carcult(cultivo,5) !ya est� en (t/ha)
        cell(n).fcncult=carcult(cultivo,6)
        !C�lculo de la distribuci�n entre evaporaci�n de suelo desnudo y transpiraci�n
        If (cell(n).x(7)>0.0.and.tiporiego(cell(n).codrie)==3) then
            !Riego por goteo (se aplica en las ra�ces directamente, primero transpira y lo que sobra se evapora)
            cell(n).tr=Min(cell(n).y(1),((evpt*r(2)-cell(n).ei)*fvegcult(njulian,cell(n).codveg)*functeta*cell(n).fcncult),cell(n).x(7))
            cell(n).es=cell(n).y(1)-cell(n).tr
            If(cell(n).tr<(evpt*r(2)-cell(n).ei)*fvegcult(njulian,cell(n).codveg)*functeta*cell(n).fcncult) then
                If(cell(n).es>0.0) then
                    cell(n).tr=cell(n).tr+cell(n).es*cell(n).fcncult
                    cell(n).es=cell(n).es*(1-cell(n).fcncult)
                End if
            End if
        Else !Sin riego o riego por aspersi�n o a manta, es homog�neo luego s�lo se divide por el factor de cubierta
            cell(n).tr=cell(n).y(1)*cell(n).fcncult  
            cell(n).es=cell(n).y(1)-cell(n).tr
        End if
        !C�lculo de los factores limitantes de crecimiento (nitr�geno, transpiraci�n (agua) y temperatura)
        Gn=1
        Gw=1
        Gt=1
        !C�lculo del crecimiento te�rico
        dw=(carcult(cultivo,13)*Gw*Gn*Gt*cell(n).w)/(1+cell(n).w) !(t/ha)
        !C�lculo del peso seco anterior
        Want=cell(n).w-dw !(t/ha)
        !C�lculo del nitr�geno cr�tico anterior
        Ncritant=carcult(cultivo,10)*(1+carcult(cultivo,11)*exp(-0.26*Want))/100 !% del peso seco en tanto por 1
        !Nitr�geno extra�do hasta este dt. Necesario para calcular cu�nto debe extraer en t=1
        Nextrant=Ncritant*Want !(tN/ha)
        !Almacenar en la variable
        cell(n).nw=Nextrant*arcel/10 !(kgN/celda)
    Else
        !C�lculo de la distribuci�n entre evaporaci�n de suelo desnudo y transpiraci�n
        If (cell(n).x(7)>0.0.and.tiporiego(cell(n).codrie)==3) then
            !Riego por goteo (se aplica en las ra�ces directamente, primero transpira y lo que sobra se evapora)
            cell(n).tr=Min(cell(n).y(1),((evpt*r(2)-cell(n).ei)*fvegcult(njulian,cell(n).codveg)*functeta*cell(n).fcncult),cell(n).x(7))
            cell(n).es=cell(n).y(1)-cell(n).tr
            If(cell(n).tr<(evpt*r(2)-cell(n).ei)*fvegcult(njulian,cell(n).codveg)*functeta*cell(n).fcncult) then
                If(cell(n).es>0.0) then
                    cell(n).tr=cell(n).tr+cell(n).es*cell(n).fcncult
                    cell(n).es=cell(n).es*(1-cell(n).fcncult)
                End if
            End if
        Else !Sin riego o riego por aspersi�n o a manta, es homog�neo luego s�lo se divide por el factor de cubierta
            cell(n).tr=cell(n).y(1)*cell(n).fcncult  
            cell(n).es=cell(n).y(1)-cell(n).tr
        End if
        cell(n).w=cell(n).w*10/arcel !Paso a (t/ha)
        !Nitr�geno extra�do hasta este dt
        Nextrant=cell(n).nw*10/arcel !(tN/ha)
    End if
    
    !C�lculo del nitr�geno cr�tico. Nitr�geno que deber�a haber extra�do la planta hasta t + el que deber�a extraer ahora para que no sea limitante
    cell(n).Ncrit=(carcult(cultivo,10)*(1+carcult(cultivo,11)*exp(-0.26*cell(n).w)))/100 !% del peso seco en tanto por 1.

    !C�lculo de la asimilaci�n pasiva
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
        !C�lculo de las concentraciones en agua de amonio y nitrato (kg/mm)
        ConcNH4=cell(n).hn(1)/Volag3
        ConcNO3=cell(n).hn(2)/Volag3
        !C�lculo de las salidas por transpiraci�n 
        cell(n).fn(9)=ConcNH4*cell(n).tr
        cell(n).fn(10)=ConcNO3*cell(n).tr
        !Actualizaci�n de los tanques en suelo
        cell(n).hn(1)=cell(n).hn(1)-cell(n).fn(9)
        cell(n).hn(2)=cell(n).hn(2)-cell(n).fn(10)
    End if
    
    !C�lculo del nitr�geno que deber�a extraer en este paso de tiempo como asimilaci�n activa.
    cell(n).Nextr=(cell(n).Ncrit*cell(n).w-Nextrant)*arcel/10-cell(n).fn(9)-cell(n).fn(10) !(kgN/celda) se hace la transformaci�n de fn9 y fn10

    !C�lculo de los flujos de nitr�geno de asimilaci�n activa
    If (cell(n).Nextr>0.and.Volag11>0.0000005) then
        !C�lculo de las concentraciones en agua de amonio y nitrato (kg/mm)
        ConcNH4=cell(n).hn(1)/Volag11     !Volag11 lo trae de simcelda_nitr
        ConcNO3=cell(n).hn(2)/Volag11
        !C�lculo de las salidas por asimilaci�n activa
        cell(n).fn(30)=Min((ConcNH4*F(cell(n).codveg)*((Volag11)/(cell(n).psuelo*r(1)*1000))**3.0),cell(n).Nextr*(1-carcult(cultivo,12)),cell(n).hn(1)) !(kgN/celda) !Extrae lo que le toca o lo que hay en el suelo
        cell(n).fn(31)=Min((ConcNO3*F(cell(n).codveg)*((Volag11)/(cell(n).psuelo*r(1)*1000))**3.0),cell(n).Nextr*(carcult(cultivo,12)),cell(n).hn(2)) !(kgN/celda)  
        Compactiva1=0
        Compactiva2=0
        If (cell(n).Nextr>(cell(n).fn(30)+cell(n).fn(31))) then !Se completa en caso de que no se llegue por haber establecido preferencias
            If (abs(cell(n).fn(30)-Min((ConcNH4*F(cell(n).codveg)*((Volag11)/(cell(n).psuelo*r(1)*1000))**3.0),cell(n).hn(1)))<0.0000005) then !Llega al l�mite de NH4
                If (abs(cell(n).fn(31)-Min((ConcNO3*F(cell(n).codveg)*((Volag11)/(cell(n).psuelo*r(1)*1000))**3.0),cell(n).hn(2)))>0.0000005) then !Pero no al de NO3, completo con NO3 hasta demanda o l�mite difusivo
                    Compactiva1=1
                End if
            End if           
            If (abs(cell(n).fn(31)-Min((ConcNO3*F(cell(n).codveg)*((Volag11)/(cell(n).psuelo*r(1)*1000))**3.0),cell(n).hn(2)))<0.0000005) then !Llego al l�mite de NO3
                If (abs(cell(n).fn(30)-Min((ConcNH4*F(cell(n).codveg)*((Volag11)/(cell(n).psuelo*r(1)*1000))**3.0),cell(n).hn(1)))>0.0000005) then !Pero no al de NH4, completo con NH4 hasta demanda o l�mite difusivo
                    Compactiva2=1
                End if
            End if
            !Se completan las salidas en caso de no haber llegado a alguno de los l�mites
            If (Compactiva1==1) then
                !Recalculo la concentraci�n tras la salida
                ConcNO3=(cell(n).hn(2)-cell(n).fn(31))/Volag11
                cell(n).fn(31)=cell(n).fn(31)+Min((ConcNO3*F(cell(n).codveg)*((Volag11)/(cell(n).psuelo*r(1)*1000))**3.0),cell(n).hn(2)-cell(n).fn(31),cell(n).Nextr-(cell(n).fn(30)+cell(n).fn(31)))
            End if
            If (Compactiva2==1) then
                !Recalculo la concentraci�n tras la salida
                ConcNH4=(cell(n).hn(1)-cell(n).fn(30)*18/14)/Volag11
                cell(n).fn(30)=cell(n).fn(30)+Min((ConcNH4*F(cell(n).codveg)*((Volag11)/(cell(n).psuelo*r(1)*1000))**3.0),cell(n).hn(1)-cell(n).fn(30),cell(n).Nextr-(cell(n).fn(30)+cell(n).fn(31)))
            End if  
        End if
    Else !Si con la asimilaci�n pasiva se ha llegado a la cantidad de nitr�geno requerida, la vegetaci�n no activa el flujo difusivo para asimilaci�n activa.
        cell(n).fn(30)=0.0
        cell(n).fn(31)=0.0
    End if
    !Los almacenamientos tras la asimilaci�n activa se actualizan en simcelda_nitr al final del paso de asimilaci�n activa

    !C�lculo del nitr�geno extra�do hasta este paso de tiempo + el de este dt
    cell(n).nw=cell(n).nw+cell(n).fn(9)+cell(n).fn(10)+cell(n).fn(30)+cell(n).fn(31) !(kgN/celda)
    
    !C�lculo de los factores limitantes de crecimiento (nitr�geno, transpiraci�n (agua) y temperatura)
    porcentajeN=cell(n).fn(9)+cell(n).fn(10)+cell(n).fn(30)+cell(n).fn(31) !Cantidad de nitr�geno extra�da en este paso de tiempo
    
    !C�lculo del nitr�geno que deber�a haber extra�do en este paso de tiempo (demanda potencial de este paso de tiempo) en KgN/celda
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

    !C�lculo del crecimineto en este dt
    dw=(carcult(cultivo,13)*Gw*Gn*Gt*cell(n).w)/(1+cell(n).w) !(t/ha)
    !C�lculo del peso seco y el factor de cubierta final del intervalo
    cell(n).w=cell(n).w+dw !(t/ha)
    !El factor de cubierta aumenta de forma proporcional al peso seco
    !Se corrige el factor de cubierta en funci�n al factor de vegeteaci�n poruqe realmente cuando ha alcanzado la etapa de "mediados de temporada" seg�n la FAO,
    !aunque siga creciendo algo, las necesidades de agua son las mismas, por eso el factor de vegetaci�n no aumenta.
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
    
    