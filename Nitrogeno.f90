!*************************************************************************************
!************C�lculo de los factores correctores por temperatura y humedad************   Versi�n final
!*************************************************************************************
Subroutine correciones_nitr
    USE modtet
    IMPLICIT NONE
    
    !Interpola la temperatura en el punto de inter�s 
    DO k=1,tnest2
      nip=cell(n).ind_int_t(k)
      cell(n).x(0)=cell(n).x(0)+(temper(nip).obs(t)-0.0065*(cell(n).cota-temper(nip).elev))*cell(n).fac_int_t(k) !�C
    ENDDO
    cell(n).tempsuelo=cell(n).x(0)-MTD*sin((3*3.1415926*njulian)/(2*365)) !Aproximaci�n para el c�lculo de la temperatura del suelo a partir de la del aire (Green and Harding, 1979)
    !Correcci�n temperatura en suelo
    cell(n).fts=Tethas**(cell(n).tempsuelo-Topts)
    !Correcci�n temperatura en cauces (se toma como temperatura del agua, la del aire)
    cell(n).ftc=Tethac**(cell(n).x(0)-Toptc)
    
    If ((cell(n).u(1)*r(1))<0.0001) then
        cell(n).fh1=0.0
        cell(n).fh2=0.0
        cell(n).fh3=0.0
    Else
        !Correcci�n humedad mineralizaci�n/inmovilizaci�n
        If ((cell(n).h(1)+cell(n).h(3)).lt.(cell(n).u(1)*r(1))) then
            cell(n).fh1=cell(n).h(1)/(cell(n).u(1)*r(1))
        Else
            cell(n).fh1=(cell(n).u(1)*r(1)+cell(n).hlim)/(cell(n).h(1)+cell(n).h(3)+cell(n).hlim)
        End if
        !C�lculo del contenido de agua en el suelo correspodiente a saturaci�n. Al m�ximo almacenamiento del tanuqe gravitacional hay que sumarle el est�tico.
        cell(n).h3max=alpha/100*cell(n).u(1)*r(1)
        cell(n).sat=cell(n).h3max+cell(n).u(1)*r(1)+cell(n).hlim
        !Correcci�n humedad nitrificaci�n
        If ((cell(n).h(1)+cell(n).h(3)).lt.(cell(n).u(1)*r(1))) then
            cell(n).fh2=(cell(n).h(1)+cell(n).hlim)/(cell(n).u(1)*r(1)+cell(n).hlim)
        Else
            cell(n).fh2=(cell(n).sat-cell(n).h(1)-cell(n).h(3))/(cell(n).sat-cell(n).u(1)*r(1))
            if(cell(n).fh2<0) then  !Por si se supera saturaci�n. Porque el tema de saturaci�n no est� muy bien simulado en tetis
                cell(n).fh2=0.0
            end if
        End if
        !Correcci�n humedad desnitrificaci�n
        If ((cell(n).h(1)+cell(n).h(3)).lt.(cell(n).u(1)*r(1))) then
            cell(n).fh3=0.0
        Else
            If ((cell(n).h(1)+cell(n).h(3)).lt.cell(n).sat) then
                cell(n).fh3=((cell(n).h(1)+cell(n).h(3)+cell(n).hlim-cell(n).u(1)*r(1))**2.0)/((cell(n).sat-(cell(n).u(1)*r(1)))**2.0)
            else
                cell(n).fh3=1
            End if
        End if
    End if
    
End subroutine

!*******************************************************************************************************
!********Almacena los estados iniciales de los tanques para poder hacer el balance de nitr�geno ********
!*******No son iniciales del todo, son tras los aportes horizontales de la celda de aguas arriba********
!*******************************************************************************************************
Subroutine alm_estadosini
    USE modtet
    IMPLICIT NONE
    
    h1_ini=cell(n).h(1)
    h2_ini=cell(n).h(2)
    h3_ini=cell(n).h(3)
    h4_ini=cell(n).h(4)
    !h(5) no hace falta porque no se usa
        
End subroutine


!*************************************************************************************
!***Subrutina que realiza los calculos de los procesos relacionados con el nitr�geno**
!*************************************************************************************
Subroutine sim_celdanitr
    USE modtet
    IMPLICIT NONE
    REAL totInput, totPercolation, totUptake
    

    !***Paso 1: Resoluci�n de los procesos de nitr�geno en suelo***
        !Actualizaci�n de los almacenamientos por el input de NH4 y NO3
        If (modulos2(4)) then !Usos del suelo con simulaci�n en modo cultivos en aquellos que se haya indicado, el input var�a diariamente, en los otros es igual d�a a d�a
            compcultivo=0
            Do i=1,ncult
                If (cell(n).codveg==codcult(i)) then
                    cell(n).hn(1)=cell(n).hn(1)+amonioscult(njulian,cell(n).codveg)*arcel/(10**4)
                    cell(n).hn(2)=cell(n).hn(2)+nitratoscult(njulian,cell(n).codveg)*arcel/(10**4)
                    cell(n).fn(40)=amonioscult(njulian,cell(n).codveg)*arcel/(10**4)  !NH4 kgN/celda/dt
                    cell(n).fn(41)=nitratoscult(njulian,cell(n).codveg)*arcel/(10**4) !NO3 kgN/celda/dt
                    compcultivo=1
                    Cycle
                End if
            End do
            If (compcultivo==0) then !Funcionamiento en modo normal
                cell(n).hn(1)=cell(n).hn(1)+amonios(cell(n).codveg,INT(mesin))*arcel/(10**4)
                cell(n).hn(2)=cell(n).hn(2)+nitratos(cell(n).codveg,INT(mesin))*arcel/(10**4)
                cell(n).fn(40)=amonios(cell(n).codveg,INT(mesin))*arcel/(10**4) !NH4 kgN/celda/dt
                cell(n).fn(41)=nitratos(cell(n).codveg,INT(mesin))*arcel/(10**4) !NO3 kgN/celda/dt
            End if
        !Vicente
        Else If (modulos(5)) then
            If (cell(n).x(7)>0.0) then
                cell(n).hn(1)=cell(n).hn(1)+amonios(cell(n).codveg,INT(mesin))*arcel/(10**4)
                cell(n).hn(2)=cell(n).hn(2)+nitratos(cell(n).codveg,INT(mesin))*arcel/(10**4)
                cell(n).fn(40)=amonios(cell(n).codveg,INT(mesin))*arcel/(10**4) !NH4 kgN/celda/dt
                cell(n).fn(41)=nitratos(cell(n).codveg,INT(mesin))*arcel/(10**4) !NO3 kgN/celda/dt
            Else
                cell(n).fn(40)=0
                cell(n).fn(41)=0  
            End If            
        Else
            cell(n).hn(1)=cell(n).hn(1)+amonios(cell(n).codveg,INT(mesin))*arcel/(10**4)
            cell(n).hn(2)=cell(n).hn(2)+nitratos(cell(n).codveg,INT(mesin))*arcel/(10**4)
            cell(n).fn(40)=amonios(cell(n).codveg,INT(mesin))*arcel/(10**4) !NH4 kgN/celda/dt
            cell(n).fn(41)=nitratos(cell(n).codveg,INT(mesin))*arcel/(10**4) !NO3 kgN/celda/dt
        End if
        
        !Actualizaci�n de los almacenamientos por deposici�n atmosf�rica
        cell(n).hn(1)=cell(n).hn(1)+cell(n).depamonio
        cell(n).hn(2)=cell(n).hn(2)+cell(n).depnitrato
        cell(n).fn(45)=cell(n).depamonio/(24*60)*dtmin !NH4 kgN/celda dt   !Los valores en topolco son diarios
        cell(n).fn(46)=cell(n).depnitrato/(24*60)*dtmin !NO3 kgN/celda dt
        
        !Almacenamiento en una variable tipo dummy los estados de los tanques iniciales para poder corregir, en caso de ser necesario, 
        !la inestabilidad num�rica por degradaci�n de un volumen de masa superior al existente. Se corrige aplicando un orden a los procesos.
        !Si los par�metros est�n bien elegidos, no deber�a ocurrir.
        dummy1=cell(n).hn(0)
        dummy2=cell(n).hn(1)
        dummy3=cell(n).hn(2)
        !Mineralizaci�n
        cell(n).fn(0)=cell(n).hn(0)*kmin(cell(n).codveg)*cell(n).fts*cell(n).fh1
        !Inmovilizaci�n
        cell(n).fn(1)=cell(n).hn(1)*kinm(cell(n).codveg)*cell(n).fts*cell(n).fh1
        !Volatilizaci�n
        cell(n).fn(44)=cell(n).hn(1)*kvol(cell(n).codveg)     !Tomado de LEACH-M, que no corrige porque es m�s dependiente del pH del suelo.
        !Nitrificaci�n
        cell(n).fn(2)=cell(n).hn(1)*knit(cell(n).codveg)*cell(n).fts*cell(n).fh2
        !Fijaci�n
        cell(n).fn(3)=kfi(cell(n).codveg) !kgN
        !Desnitrificaci�n
        cell(n).fn(4)=cell(n).hn(2)*kdes(cell(n).codveg)*cell(n).fts*cell(n).fh3
        !Actualizaci�n de los almacenamientos (los datos de input de nitr�geno est�n en kg/ha, se transforman a kg/celda)     !Considero que hn(0) es constante en el tiempo (humus).
        cell(n).hn(1)=cell(n).hn(1)+cell(n).fn(0)-cell(n).fn(1)-cell(n).fn(44)-cell(n).fn(2)
        If (cell(n).hn(1)<0) then
            cell(n).hn(1)=dummy2+cell(n).fn(0)-cell(n).fn(1)-cell(n).fn(44)
            If (cell(n).hn(1)<0) then
                cell(n).hn(1)=dummy2+cell(n).fn(0)-cell(n).fn(1)
                If (cell(n).hn(1)<0) then
                    cell(n).hn(1)=0.0
                    cell(n).fn(1)=dummy2+cell(n).fn(0)
                    cell(n).fn(44)=0.0
                    cell(n).fn(2)=0.0
                Else
                    cell(n).hn(1)=0.0
                    cell(n).fn(44)=dummy2+cell(n).fn(0)-cell(n).fn(1)
                    cell(n).fn(2)=0.0
                End if
            Else
                cell(n).hn(1)=0.0
                cell(n).fn(2)=dummy2+cell(n).fn(0)-cell(n).fn(1)-cell(n).fn(44)
            End if
        End if
        cell(n).hn(2)=cell(n).hn(2)+cell(n).fn(2)+cell(n).fn(3)-cell(n).fn(4)
        If (cell(n).hn(2)<0) then
            cell(n).fn(4)=dummy3+cell(n).fn(2)+cell(n).fn(3)
            cell(n).hn(2)=0.0
        End if
    
    !***Paso 2: Resoluci�n de los procesos de nitr�geno en cauce (s�lo se ve afectado el nitr�geno disuelto)***
    !Almacenamiento de los estados iniciales en una variable dummy para corregir la inestabilidad num�rica
    dummy4=cell(n).hn(3)
    dummy5=cell(n).hn(4)
    dummy6=cell(n).hn(5)
    !Mineralizaci�n
    cell(n).fn(15)=cell(n).hn(3)*kminc*cell(n).ftc
    !Nitrificaci�n
    cell(n).fn(16)=cell(n).hn(4)*knitc*cell(n).ftc
    !Desnitrificaci�n
    cell(n).fn(17)=cell(n).hn(5)*kdesc*cell(n).ftc
    !Actualizaci�n de los almacenamientos en cauce
    cell(n).hn(3)=cell(n).hn(3)-cell(n).fn(15)
    If (cell(n).hn(3)<0) then
        cell(n).fn(15)=dummy4
        cell(n).hn(3)=0.0
    End if
    cell(n).hn(4)=cell(n).hn(4)+cell(n).fn(15)-cell(n).fn(16)
    If (cell(n).hn(4)<0) then
        cell(n).fn(16)=dummy5+cell(n).fn(15)
        cell(n).hn(4)=0.0
    End if
    cell(n).hn(5)=cell(n).hn(5)+cell(n).fn(16)-cell(n).fn(17)
    If (cell(n).hn(5)<0) then
        cell(n).fn(17)=dummy6+cell(n).fn(16)
        cell(n).hn(5)=0.0
    End if
    
    !***Paso 3: Movimiento de amonio y nitrato en percolaci�n***
    !Volumen a computar: ser�a el agua inicial en el suelo (h1+h3) + el agua que entra al suelo (d1+x2-d2, que equivale a d1+x3), se excluye el agua en superficie
    Volag1=h1_ini+cell(n).d(1)+cell(n).x(3)+h3_ini+cell(n).hlim
    If (Volag1<0.000005.or.cell(n).x(4)<0.000005) then !Si no hay agua o la salida es cero    
        cell(n).fn(5)=0.0
        cell(n).fn(6)=0.0
    Else if (abs(Volag1-cell(n).x(4))<0.000005) then !Si hay agua, pero coincide con la salida. Se produce un lavado del suelo, sale todo.
        cell(n).fn(5)=cell(n).hn(1)
        cell(n).fn(6)=cell(n).hn(2)
        cell(n).hn(1)=0.0
        cell(n).hn(2)=0.0
    Else
        !C�lculo de las concentraciones en agua de amonio y nitrato (kg/mm)
        ConcNH4=cell(n).hn(1)/Volag1
        ConcNO3=cell(n).hn(2)/Volag1
        !C�lculo de las salidas por percolaci�n (Mov. advectivo)
        cell(n).fn(5)=ConcNH4*cell(n).x(4)
        cell(n).fn(6)=ConcNO3*cell(n).x(4)
        !Actualizaci�n de los tanques en suelo
        cell(n).hn(1)=cell(n).hn(1)-cell(n).fn(5)
        cell(n).hn(2)=cell(n).hn(2)-cell(n).fn(6)
    End if
    !Actualizaci�n de los tanques en acu�fero
    cell(n).hn(6)=cell(n).hn(6)+cell(n).fn(5)
    cell(n).hn(7)=cell(n).hn(7)+cell(n).fn(6)
    
    !If (cell(n).acuif==1) then !Normal, el acu�fero superficial (conectado)
    !    cell(n).fn(36)=cell(n).fn(5)
    !    cell(n).fn(37)=cell(n).fn(6)  !Almaceno por separado la percolaci�n a celdas de cuaternario y plioceno    Para Murcia hay que separarlo antes porque aunque en agua pod�a restar las p�rdidas que eran recarga
    !    cell(n).fn(38)=0.0                                                                                       !a Plioceno, aqu� ya no funciona porque las p�rdidas van con la concentraci�n del acu�fero por lo que almaceno
    !    cell(n).fn(39)=0.0                                                                                       !la percolaci�n y el balance del acu�fero se hace fuera de l�nea
    !Else !cell(n).acuif==2 Acu�fero no conectado
    !    cell(n).fn(36)=0.0
    !    cell(n).fn(37)=0.0            !Almaceno por separado la percolaci�n a celdas de cuaternario y plioceno
    !    cell(n).fn(38)=cell(n).fn(5)
    !    cell(n).fn(39)=cell(n).fn(6)
    !end if
    !Actualizaci�n de los tanques en acu�fero
    !If (cell(n).acuif==1) then !Normal, el acu�fero superficial
        !cell(n).hn(6)=cell(n).hn(6)+cell(n).fn(5)
        !cell(n).hn(7)=cell(n).hn(7)+cell(n).fn(6)
    !Else !cell(n).acuif==2 !No entra al tanque de acu�efro, directamente pasa a p�rdidas
    !    cell(n).hn(6)=0.0
    !    cell(n).hn(7)=0.0
    !End if
    
    !***Paso 4: Movimiento de amonio y nitrato en p�rdidas subterr�neas***
    !Volumen a computar: el del acu�fero inicial + lo que llega por percolaci�n
    Volag2=h4_ini+cell(n).x(4)
    If (Volag2<0.000005.or.cell(n).x(5)<0.000005) then !No hay agua, no hay salidas  
        cell(n).fn(7)=0.0
        cell(n).fn(8)=0.0
        !cell(n).fn(32)=0.0
        !cell(n).fn(33)=0.0
        !cell(n).fn(34)=0.0
        !cell(n).fn(35)=0.0
    Else if (abs(Volag2-cell(n).x(5))<0.000005) then !El agua que hay es igual a la salida, sale todo
        cell(n).fn(7)=cell(n).hn(6)
        cell(n).fn(8)=cell(n).hn(7)
        cell(n).hn(6)=0.0
        cell(n).hn(7)=0.0
        !If (cell(n).acuif==1) then !Normal, el acu�fero superficial (conectado)
        !    cell(n).fn(7)=cell(n).hn(6)
        !    cell(n).fn(8)=cell(n).hn(7)
        !    cell(n).fn(32)=cell(n).fn(7)
        !    cell(n).fn(33)=cell(n).fn(8)
        !    cell(n).fn(34)=0.0
        !    cell(n).fn(35)=0.0
        !    cell(n).hn(6)=0.0
        !    cell(n).hn(7)=0.0
        !Else !cell(n).acuif==2 !No entra al tanque de acu�fero, sale lo mismo que antes se ha percolado
        !    cell(n).fn(7)=cell(n).fn(5)
        !    cell(n).fn(8)=cell(n).fn(6)
        !    cell(n).fn(32)=0.0
        !    cell(n).fn(33)=0.0
        !    cell(n).fn(34)=cell(n).fn(7)
        !    cell(n).fn(35)=cell(n).fn(8)
        !    cell(n).hn(6)=0.0
        !    cell(n).hn(7)=0.0
        !End if
    else 
        !C�lculo de las concentraciones en agua de amonio y nitrato (kg/mm)
        ConcNH4=cell(n).hn(6)/Volag2
        ConcNO3=cell(n).hn(7)/Volag2
        !C�lculo de las salidas por p�rdidas subterr�neas (Mov. advectivo)
        cell(n).fn(7)=ConcNH4*cell(n).x(5)
        cell(n).fn(8)=ConcNO3*cell(n).x(5)
        !Actualizaci�n de los tanques en acu�fero
        cell(n).hn(6)=cell(n).hn(6)-cell(n).fn(7)
        cell(n).hn(7)=cell(n).hn(7)-cell(n).fn(8)
        !If (cell(n).acuif==1) then !Normal, el acu�fero superficial (conectado)
        !    cell(n).fn(7)=ConcNH4*cell(n).x(5)
        !    cell(n).fn(8)=ConcNO3*cell(n).x(5)
        !    cell(n).fn(32)=cell(n).fn(7)
        !    cell(n).fn(33)=cell(n).fn(8)
        !    cell(n).fn(34)=0.0
        !    cell(n).fn(35)=0.0
        !    !Actualizaci�n de los tanques en acu�fero
        !    cell(n).hn(6)=cell(n).hn(6)-cell(n).fn(7)
        !    cell(n).hn(7)=cell(n).hn(7)-cell(n).fn(8)
        !Else !cell(n).acuif==2 !No entra al tanque de acu�fero, sale lo mismo que antes se ha percolado
        !    cell(n).fn(7)=cell(n).fn(5)
        !    cell(n).fn(8)=cell(n).fn(6)
        !    cell(n).fn(32)=0.0
        !    cell(n).fn(33)=0.0
        !    cell(n).fn(34)=cell(n).fn(7)
        !    cell(n).fn(35)=cell(n).fn(8)
        !    cell(n).hn(6)=0.0
        !    cell(n).hn(7)=0.0
        !End if
    End if
    
    !***Paso 5: Asimilaci�n de nitr�geno por parte de la vegetaci�n***
    !**Paso 1: Asimilaci�n pasiva (con el flujo de transpiraci�n)**
	If (modulos2(4)) then !Usos del suelo con simulaci�n en modo cultivos en aquellos que se haya indicado
		compcultivo=0
		Do i=1,ncult
			If (cell(n).codveg==codcult(i)) then
				cell(n).fn(9)=0.0
				cell(n).fn(10)=0.0     !La asimilaci�n pasiva de nitr�geno en caso de cultivos, se calcula en Crop_growth 1 o 2. 
				compcultivo=1          !Es ah� donde se actualiza el valor del factor de cubierta de este paso de tiempo. 
				Cycle
			End if
		End do
		If (compcultivo==0) then !Funcionamiento en modo normal
			!C�lculo de la distribuci�n entre evaporaci�n de suelo desnudo y transpiraci�n
			If (cell(n).x(7)>0.0.and.tiporiego(cell(n).codrie)==3) then 
				!Si hay riego por goteo, el volumen de agua se concentrar� en el bulbo, por lo que no se puede aplicar directamente el factor de cubierta, se hace como se detalla:
				cell(n).tr=Min(cell(n).y(1),((evpt*r(2)-cell(n).ei)*lamb(cell(n).codveg,INT(mesin))*functeta*fcn(cell(n).codveg,INT(mesin))),cell(n).x(7))
				cell(n).es=cell(n).y(1)-cell(n).tr
				If(cell(n).tr<(evpt*r(2)-cell(n).ei)*lamb(cell(n).codveg,INT(mesin))*functeta*fcn(cell(n).codveg,INT(mesin))) then
					If(cell(n).es>0.0) then
						cell(n).tr=cell(n).tr+cell(n).es*fcn(cell(n).codveg,INT(mesin))
						cell(n).es=cell(n).es*(1-fcn(cell(n).codveg,INT(mesin)))
					End if
				End if
			Else !Si no hay riego, o es por aspersion/manta al ser homog�neo, la divisi�n es directa con el factor de cubierta
				cell(n).tr=cell(n).y(1)*fcn(cell(n).codveg,INT(mesin))
				cell(n).es=cell(n).y(1)*(1-fcn(cell(n).codveg,INT(mesin)))
			End if
			!Volumen de agua que queda en el suelo
			Volag3=h1_ini+cell(n).d(1)+h3_ini+cell(n).x(3)-cell(n).x(4)-cell(n).es+cell(n).hlim
			If (Volag3<0.000005.or.cell(n).tr<0.000005) then  
				cell(n).fn(9)=0.0  
				cell(n).fn(10)=0.0
			Else if (abs(Volag3-cell(n).tr)<0.000005) then  !El volumen de agua en el suelo es igual al transpirado. Sale todo
				cell(n).fn(9)=cell(n).hn(1)
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
			end if
		End if
	Else  !Sin modo cultivos activado
		!C�lculo de la distribuci�n entre evaporaci�n de suelo desnudo y transpiraci�n
		!If (cell(n).x(7)>0.0) then !DEBUG
		If (cell(n).x(7)>0.0.and.tiporiego(cell(n).codrie)==3) then !Hay riego por goteo, por ahora s�lo se puede por goteo, si no es por goteo habr�a que pensarlo. 
			!Si hay riego por goteo, el volumen de agua se concentrar� en el bulbo, por lo que no se puede aplicar directamente el factor de cubierta, se hace como se detalla:
			cell(n).tr=Min(cell(n).y(1),((evpt*r(2)-cell(n).ei)*lamb(cell(n).codveg,INT(mesin))*functeta*fcn(cell(n).codveg,INT(mesin))),cell(n).x(7))
			cell(n).es=cell(n).y(1)-cell(n).tr
			If(cell(n).tr<(evpt*r(2)-cell(n).ei)*lamb(cell(n).codveg,INT(mesin))*functeta*fcn(cell(n).codveg,INT(mesin))) then
				If(cell(n).es>0.0) then
					cell(n).tr=cell(n).tr+cell(n).es*fcn(cell(n).codveg,INT(mesin))
					cell(n).es=cell(n).es*(1-fcn(cell(n).codveg,INT(mesin)))
				End if
			End if
		Else !Si no hay riego, o es por aspersion/manta al ser homog�neo, la divisi�n es directa con el factor de cubierta
			cell(n).tr=cell(n).y(1)*fcn(cell(n).codveg,INT(mesin))
			cell(n).es=cell(n).y(1)*(1-fcn(cell(n).codveg,INT(mesin)))
		End if
		!Volumen de agua que queda en el suelo
		Volag3=h1_ini+cell(n).d(1)+h3_ini+cell(n).x(3)-cell(n).x(4)-cell(n).es+cell(n).hlim
		If (Volag3<0.000005.or.cell(n).tr<0.000005) then  
			cell(n).fn(9)=0.0    !no se actualizan los tanques porque no hay salida
			cell(n).fn(10)=0.0
		Else if (abs((Volag3-cell(n).tr)<0.000005)) then  !El volumen de agua en el suelo es igual al transpirado. Sale todo
			cell(n).fn(9)=cell(n).hn(1)
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
		end if
	End if
    
    !**Paso 2: Asimilaci�n activa (flujo difusivo)**
	!Volumen a computar: volumen en el suelo tras flujos verticales y transpiraci�n
	Volag11=h1_ini+cell(n).d(1)-cell(n).y(1)+h3_ini+cell(n).x(3)-cell(n).x(4)+cell(n).hlim
	If (modulos2(4)) then !Usos del suelo con simulaci�n en modo cultivos en aquellos que se haya indicado
		compcultivo=0
		Do i=1,ncult
			If (cell(n).codveg==codcult(i)) then
				poscodcult=i !Almaceno la fila porque hace falta para "call crop_growth"
				call crop_growth
				compcultivo=1
				Exit
			End if
		End do
		If (compcultivo==0) then !Significa que el uso del suelo no funciona en modo cultivo, simulaci�n normal
			cell(n).w=0.0
			cell(n).fcncult=0.0
			If (Volag11<0.000005.or.cell(n).psuelo*r(1)<0.000005.or.Ndem(cell(n).codveg)<0.000005) then
				cell(n).fn(30)=0.0
				cell(n).fn(31)=0.0
			else
				!C�lculo de las concentraciones en agua de amonio y nitrato (kg/mm)
				ConcNH4=cell(n).hn(1)/Volag11
				ConcNO3=cell(n).hn(2)/Volag11
				!C�lculo de las salidas por flujo difusivo
				If (Ndem(cell(n).codveg)<=cell(n).fn(9)+cell(n).fn(10)) then
					cell(n).fn(30)=0.0
					cell(n).fn(31)=0.0
				else !M�nimo entre lo m�ximo que se puede asimilar por flujo difusivo, la demanda menos lo ya asimilado pasivamente y lo que hay en el suelo
					cell(n).fn(30)=Min((ConcNH4*F(cell(n).codveg)*((Volag11)/(cell(n).psuelo*r(1)*1000))**3.0),(Ndem(cell(n).codveg)-cell(n).fn(9)-cell(n).fn(10))*(1-prefNO3(cell(n).codveg)),cell(n).hn(1))
					cell(n).fn(31)=Min((ConcNO3*F(cell(n).codveg)*((Volag11)/(cell(n).psuelo*r(1)*1000))**3.0),(Ndem(cell(n).codveg)-cell(n).fn(9)-cell(n).fn(10))*prefNO3(cell(n).codveg),cell(n).hn(2))
					!C�lculo de lo asimilado hasta ahora
					AsimilacionTot=cell(n).fn(9)+cell(n).fn(10)+cell(n).fn(30)+cell(n).fn(31)
					Compactiva1=0
					Compactiva2=0
					If (Ndem(cell(n).codveg)>AsimilacionTot) then !Se completa en caso de que no se llegue por haber establecido preferencias (si prefNO3=1 pero con NO3 no se llega, se activa NH4 hasta Ndem)
						If (abs(cell(n).fn(30)-Min((ConcNH4*F(cell(n).codveg)*((Volag11)/(cell(n).psuelo*r(1)*1000))**3.0),cell(n).hn(1)))<0.000005) then !Llego al l�mite de NH4
							If (abs(cell(n).fn(31)-Min((ConcNO3*F(cell(n).codveg)*((Volag11)/(cell(n).psuelo*r(1)*1000))**3.0),cell(n).hn(2)))>0.000005) then !Pero no al de NO3, completo con NO3 hasta demanda o l�mite difusivo
								Compactiva1=1
							End if
						End if
						If (abs(cell(n).fn(31)-Min((ConcNO3*F(cell(n).codveg)*((Volag11)/(cell(n).psuelo*r(1)*1000))**3.0),cell(n).hn(2)))<0.000005) then !Llego al l�mite de NO3
							If (abs(cell(n).fn(30)-Min((ConcNH4*F(cell(n).codveg)*((Volag11)/(cell(n).psuelo*r(1)*1000))**3.0),cell(n).hn(1)))>0.000005) then !Pero no al de NH4, completo con NH4 hasta demanda o l�mite difusivo
								Compactiva2=1
							End if
						End if
						!Se completan las salidas en caso de no haber llegado a alguno de los l�mites
						If (Compactiva1==1) then
							!Recalculo la concentraci�n tras la salida
							ConcNO3=(cell(n).hn(2)-cell(n).fn(31))/Volag11
							cell(n).fn(31)=cell(n).fn(31)+Min((ConcNO3*F(cell(n).codveg)*((Volag11)/(cell(n).psuelo*r(1)*1000))**3.0),cell(n).hn(2)-cell(n).fn(31),Ndem(cell(n).codveg)-AsimilacionTot)
						End if
						If (Compactiva2==1) then
							!Recalculo la concentraci�n tras la salida
							ConcNH4=(cell(n).hn(1)-cell(n).fn(30))/Volag11
							cell(n).fn(30)=cell(n).fn(30)+Min((ConcNH4*F(cell(n).codveg)*((Volag11)/(cell(n).psuelo*r(1)*1000))**3.0),cell(n).hn(1)-cell(n).fn(30),Ndem(cell(n).codveg)-AsimilacionTot)
						End if   
					End if
				End if
			End if  
		End if
	else !Modo cultivos desactivado, todos los usos del suelo funcionan en modo normal. No hay crecimiento.
		cell(n).w=0.0
		cell(n).fcncult=0.0
		If (Volag11<0.000005.or.cell(n).psuelo*r(1)<0.000005.or.Ndem(cell(n).codveg)<0.000005) then
			cell(n).fn(30)=0.0
			cell(n).fn(31)=0.0
		else
			!C�lculo de las concentraciones en agua de amonio y nitrato (kg/mm)
			ConcNH4=cell(n).hn(1)/Volag11
			ConcNO3=cell(n).hn(2)/Volag11
			!C�lculo de las salidas por flujo difusivo
			If (Ndem(cell(n).codveg)<=cell(n).fn(9)+cell(n).fn(10)) then   
				cell(n).fn(30)=0.0   !No hace falta actualizar almacenamientos
				cell(n).fn(31)=0.0
			else !M�nimo entre lo m�ximo que se puede asimilar por flujo difusivo, la demanda menos lo ya asimilado pasivamente y lo que hay en el suelo
				cell(n).fn(30)=Min((ConcNH4*F(cell(n).codveg)*((Volag11)/(cell(n).psuelo*r(1)*1000))**3.0),(Ndem(cell(n).codveg)-cell(n).fn(9)-cell(n).fn(10))*(1-prefNO3(cell(n).codveg)),cell(n).hn(1))
				cell(n).fn(31)=Min((ConcNO3*F(cell(n).codveg)*((Volag11)/(cell(n).psuelo*r(1)*1000))**3.0),(Ndem(cell(n).codveg)-cell(n).fn(9)-cell(n).fn(10))*prefNO3(cell(n).codveg),cell(n).hn(2))
				!C�lculo de lo asimilado hasta ahora
				AsimilacionTot=cell(n).fn(9)+cell(n).fn(10)+cell(n).fn(30)+cell(n).fn(31)
				Compactiva1=0
				Compactiva2=0
				If (Ndem(cell(n).codveg)>AsimilacionTot) then !Se completa en caso de que no se llegue por haber establecido preferencias (si prefNO3=1 pero con NO3 no se llega, se activa NH4 hasta Ndem)
					If (abs(cell(n).fn(30)-Min((ConcNH4*F(cell(n).codveg)*((Volag11)/(cell(n).psuelo*r(1)*1000))**3.0),cell(n).hn(1)))<0.000005) then !Llego al l�mite de NH4
						If (abs(cell(n).fn(31)-Min((ConcNO3*F(cell(n).codveg)*((Volag11)/(cell(n).psuelo*r(1)*1000))**3.0),cell(n).hn(2)))>0.000005) then !Pero no al de NO3, completo con NO3 hasta demanda o l�mite difusivo
							Compactiva1=1
						End if
					End if
					If (abs(cell(n).fn(31)-Min((ConcNO3*F(cell(n).codveg)*((Volag11)/(cell(n).psuelo*r(1)*1000))**3.0),cell(n).hn(2)))<0.000005) then !Llego al l�mite de NO3
						If (abs(cell(n).fn(30)-Min((ConcNH4*F(cell(n).codveg)*((Volag11)/(cell(n).psuelo*r(1)*1000))**3.0),cell(n).hn(1)))>0.000005) then !Pero no al de NH4, completo con NH4 hasta demanda o l�mite difusivo
							Compactiva2=1
						End if
					End if
					!Se completan las salidas en caso de no haber llegado a alguno de los l�mites
					If (Compactiva1==1) then
						!Recalculo la concentraci�n tras la salida
						ConcNO3=(cell(n).hn(2)-cell(n).fn(31))/Volag11
						cell(n).fn(31)=cell(n).fn(31)+Min((ConcNO3*F(cell(n).codveg)*((Volag11)/(cell(n).psuelo*r(1)*1000))**3.0),cell(n).hn(2)-cell(n).fn(31),Ndem(cell(n).codveg)-AsimilacionTot)
					End if
					If (Compactiva2==1) then
						!Recalculo la concentraci�n tras la salida
						ConcNH4=(cell(n).hn(1)-cell(n).fn(30))/Volag11
						cell(n).fn(30)=cell(n).fn(30)+Min((ConcNH4*F(cell(n).codveg)*((Volag11)/(cell(n).psuelo*r(1)*1000))**3.0),cell(n).hn(1)-cell(n).fn(30),Ndem(cell(n).codveg)-AsimilacionTot)
					End if   
				End if
			End if
		End if  
	End if
	
    !Actualizaci�n de los tanques en suelo    Aqu� hace falta comprobar el absoluto porque puede que con la activa se haya agotado hn1 o hn2 y entonces hay que hacerlos cero, para evitar negativos
    If (abs(cell(n).hn(1)-cell(n).fn(30))<0.000005.and.cell(n).fn(30)>0.000005) then
        cell(n).fn(30)=cell(n).hn(1)
        cell(n).hn(1)=0.0
    Else
        cell(n).hn(1)=cell(n).hn(1)-cell(n).fn(30)
    End if
    If (abs(cell(n).hn(2)-cell(n).fn(31))<0.000005.and.cell(n).fn(31)>0.000005) then
        cell(n).fn(31)=cell(n).hn(2)
        cell(n).hn(2)=0.0
    Else
        cell(n).hn(2)=cell(n).hn(2)-cell(n).fn(31)
    End if
    
    !***Paso 6.1: Salidas por exfiltraci�n***
    If (cell(n).exfiltr>0.0) then !Se est� produciendo exfiltraci�n
        Volag4=h1_ini+cell(n).d(1)-cell(n).y(1)+h3_ini+cell(n).x(3)-cell(n).x(4)+cell(n).hlim
        cell(n).fn(42)=cell(n).hn(1)/Volag4*cell(n).exfiltr
        cell(n).fn(43)=cell(n).hn(2)/Volag4*cell(n).exfiltr  !Nunca puede ser igual el volumen a exfiltraci�n
        !Actualizaci�n de los almacenamientos
        cell(n).hn(1)=cell(n).hn(1)-cell(n).fn(42)
        cell(n).hn(2)=cell(n).hn(2)-cell(n).fn(43)
        !Actualizaci�n de los almacenamientos en superficie
        cell(n).hn(4)=cell(n).hn(4)+cell(n).fn(42)
        cell(n).hn(5)=cell(n).hn(5)+cell(n).fn(43)
    End if
    
    !***Paso 6.2: Salidas por interflujo***
    !Volumen a computar: volumen en el suelo tras flujos verticales y transpiraci�n (vuelve a ser el mismo porque con el flujo difusivo hay que calcular concentraci�n en agua, pero no hay salida de agua)
    Volag4=h1_ini+cell(n).d(1)-cell(n).y(1)+h3_ini+cell(n).x(3)-cell(n).x(4)-cell(n).exfiltr+cell(n).hlim
    If (Volag4<0.000005.or.cell(n).y(3)<0.000005) then 
        cell(n).fn(11)=0.0
        cell(n).fn(12)=0.0
    Else if (abs(Volag4-cell(n).y(3))<0.000005) then
        cell(n).fn(11)=cell(n).hn(1)
        cell(n).fn(12)=cell(n).hn(2)
        cell(n).hn(1)=0.0
        cell(n).hn(2)=0.0
    Else
        !C�lculo de las concentraciones en agua de amonio y nitrato (kg/mm)
        ConcNH4=cell(n).hn(1)/Volag4
        ConcNO3=cell(n).hn(2)/Volag4
        !C�lculo de las salidas por interflujo
        cell(n).fn(11)=ConcNH4*cell(n).y(3)
        cell(n).fn(12)=ConcNO3*cell(n).y(3)
        !Actualizaci�n de los tanques en suelo
        cell(n).hn(1)=cell(n).hn(1)-cell(n).fn(11)
        cell(n).hn(2)=cell(n).hn(2)-cell(n).fn(12)
    End if
    
    !***Paso 7: Salidas por flujo base***
    !Volumen a computar: volumen en el acu�fero tras p�rdidas subterr�neas
    Volag5=h4_ini+cell(n).x(4)-cell(n).x(5)
    If (Volag5<0.000005.or.cell(n).y(4)<0.000005) then 
        cell(n).fn(13)=0.0
        cell(n).fn(14)=0.0
    Else if (abs(Volag5-cell(n).y(4))<0.000005) then
        cell(n).fn(13)=cell(n).hn(6)
        cell(n).fn(14)=cell(n).hn(7)
        cell(n).hn(6)=0.0
        cell(n).hn(7)=0.0
    else
        !C�lculo de las concentraciones en agua de amonio y nitrato (kg/mm)
        ConcNH4=cell(n).hn(6)/Volag5
        ConcNO3=cell(n).hn(7)/Volag5
        !C�lculo de las salidas por flujo base
        cell(n).fn(13)=ConcNH4*cell(n).y(4)
        cell(n).fn(14)=ConcNO3*cell(n).y(4)
        !Actualizaci�n de los tanques en acu�fero
        cell(n).hn(6)=cell(n).hn(6)-cell(n).fn(13)
        cell(n).hn(7)=cell(n).hn(7)-cell(n).fn(14)
    End if

    
    !***Paso 8: Transmisi�n entre celdas de nitr�geno en interflujo y en flujo base (escorrent�a sin sedimentos va limpia de nitr�geno, interflujo y flujo base sin sedimentos no arrastran norg�nico)***
    IF(cell(n).codkarst.eq.1)THEN !CELDA KARST
        IF(cell(cell(n).dest).codkarst.eq.1)THEN !CELDA RECEPTORA KARST
            IF(logicman)THEN !La celda n es manantial, sale a superficie
                cell(n).hn(4)=cell(n).hn(4)+cell(n).fn(11)+cell(n).fn(13) !NH4 en interflujo y flujo base pasan al tanque hn4 (NH4 cauce) de la propia celda
                cell(n).hn(5)=cell(n).hn(5)+cell(n).fn(12)+cell(n).fn(14) !NO3 en interflujo y flujo base pasan al tanque hn5 (NO3 cauce) de la propia celda
            Else !Los flujos no salen a superficie, se mueven en horizontal
                cell(cell(n).dest).hn(1)=cell(cell(n).dest).hn(1)+cell(n).fn(11) !NH4 en interflujo pasa al tanque hn1 (NH4 suelo) de la celda destino
                cell(cell(n).dest).hn(2)=cell(cell(n).dest).hn(2)+cell(n).fn(12) !NO3 en interflujo pasa al tanque hn2 (NO3 suelo) de la celda destino
                cell(cell(n).dest).hn(6)=cell(cell(n).dest).hn(6)+cell(n).fn(13) !Nh4 en flujo base pasa al tanque hn6 (NH4 acu�fero) de la celda destino
                cell(cell(n).dest).hn(7)=cell(cell(n).dest).hn(7)+cell(n).fn(14) !NO3 en flujo base pasa al tanque hn7 (NO3 acu�fero) de la celda destino
            End if
        Else !CELDA RECEPTORA NO KARST    (Todo el borde es manantial, sale a superficie)
            cell(n).hn(4)=cell(n).hn(4)+cell(n).fn(11)+cell(n).fn(13) !NH4 en interflujo y flujo base pasa al tanque hn4 (NH4 cauce) de la propia celda
            cell(n).hn(5)=cell(n).hn(5)+cell(n).fn(12)+cell(n).fn(14) !NO3 en interflujo y flujo base pasa al tanque hn5 (NO3 cauce) de la propia celda
        End if
    Else !CELDA NO KARST
        IF(cell(cell(n).dest).codkarst.eq.1)THEN !CELDA RECEPTORA KARST
            IF(logicman2)THEN !La celda destino es manantial.
                cell(cell(n).dest).hn(4)=cell(cell(n).dest).hn(4)+cell(n).fn(11)+cell(n).fn(13) !NH4 en interflujo y flujo base pasa al tanque hn4 (NH4 cauce) de la celda destino
                cell(cell(n).dest).hn(5)=cell(cell(n).dest).hn(5)+cell(n).fn(12)+cell(n).fn(14) !NO3 en interflujo y flujo base pasa al tanque hn5 (NO3 cauce) de la celda destino
            else
                cell(cell(n).dest).hn(1)=cell(cell(n).dest).hn(1)+cell(n).fn(11) !NH4 en interflujo pasa al tanque hn1 (NH4 suelo) de la celda destino
                cell(cell(n).dest).hn(2)=cell(cell(n).dest).hn(2)+cell(n).fn(12) !NO3 en interflujo pasa al tanque hn2 (NO3 suelo) de la celda destino
                cell(cell(n).dest).hn(6)=cell(cell(n).dest).hn(6)+cell(n).fn(13) !Nh4 en flujo base pasa al tanque hn6 (NH4 acu�fero) de la celda destino
                cell(cell(n).dest).hn(7)=cell(cell(n).dest).hn(7)+cell(n).fn(14) !NO3 en flujo base pasa al tanque hn7 (NO3 acu�fero) de la celda destino
            end if
        else !CELDA RECEPTORA NO KARST
            IF(nw(2,ncp).gt.cell(n).acum) THEN !LADERA
                IF (nw(2,ncp).gt.cell(cell(n).dest).acum) THEN !Celda receptora con ladera
                    cell(cell(n).dest).hn(1)=cell(cell(n).dest).hn(1)+cell(n).fn(11) !NH4 en interflujo pasa al tanque hn1 (NH4 suelo) de la celda destino
                    cell(cell(n).dest).hn(2)=cell(cell(n).dest).hn(2)+cell(n).fn(12) !NO3 en interflujo pasa al tanque hn2 (NO3 suelo) de la celda destino
                    cell(cell(n).dest).hn(6)=cell(cell(n).dest).hn(6)+cell(n).fn(13) !Nh4 en flujo base pasa al tanque hn6 (NH4 acu�fero) de la celda destino
                    cell(cell(n).dest).hn(7)=cell(cell(n).dest).hn(7)+cell(n).fn(14) !NO3 en flujo base pasa al tanque hn7 (NO3 acu�fero) de la celda destino
                ELSE IF (nw(3,ncp).gt.cell(cell(n).dest).acum) THEN !Celda receptora con carcava
                    cell(cell(n).dest).hn(4)=cell(cell(n).dest).hn(4)+cell(n).fn(11) !NH4 en interflujo pasa al tanque hn4 (NH4 cauce) de la celda destino
                    cell(cell(n).dest).hn(5)=cell(cell(n).dest).hn(5)+cell(n).fn(12) !NO3 en interflujo pasa al tanque hn5 (NO3 cauce) de la celda destino
                    cell(cell(n).dest).hn(6)=cell(cell(n).dest).hn(6)+cell(n).fn(13) !NH4 en flujo base pasa al tanque hn6 (NH4 acu�fero) de la celda destino
                    cell(cell(n).dest).hn(7)=cell(cell(n).dest).hn(7)+cell(n).fn(14) !NO3 en flujo base pasa al tanque hn7 (NO3 acu�fero) de la celda destino
                ELSE !Celda receptora con cauce
                    cell(cell(n).dest).hn(4)=cell(cell(n).dest).hn(4)+cell(n).fn(11)+cell(n).fn(13) !NH4 en interflujo y flujo base pasa al tanque hn4 (NH4 cauce) de la celda destino
                    cell(cell(n).dest).hn(5)=cell(cell(n).dest).hn(5)+cell(n).fn(12)+cell(n).fn(14) !NO3 en interflujo y flujo base pasa al tanque hn5 (NO3 cauce) de la celda destino
                ENDIF
            ELSE IF (nw(3,ncp).gt.cell(n).acum) THEN !CARCAVA
                cell(n).hn(4)=cell(n).hn(4)+cell(n).fn(11) !NH4 en interflujo pasa al tanque hn4 (NH4 cauce) de la propia celda
                cell(n).hn(5)=cell(n).hn(5)+cell(n).fn(12) !NO3 en interflujo pasa al tanque hn5 (NO3 cauce) de la propia celda
                IF (nw(3,ncp).gt.cell(cell(n).dest).acum) THEN !Celda receptora con carcava
                    cell(cell(n).dest).hn(6)=cell(cell(n).dest).hn(6)+cell(n).fn(13) !Nh4 en flujo base pasa al tanque hn6 (NH4 acu�fero) de la celda destino
                    cell(cell(n).dest).hn(7)=cell(cell(n).dest).hn(7)+cell(n).fn(14) !NO3 en flujo base pasa al tanque hn7 (NO3 acu�fero) de la celda destino
                ELSE !Celda receptora con cauce
                    cell(cell(n).dest).hn(4)=cell(cell(n).dest).hn(4)+cell(n).fn(13) !NH4 en flujo base pasa al tanque hn6 (NH4 cauce) de la celda destino
                    cell(cell(n).dest).hn(5)=cell(cell(n).dest).hn(5)+cell(n).fn(14) !NO3 en flujo base pasa al tanque hn7 (NO3 cauce) de la celda destino
                END IF
            ELSE !CAUCE
                cell(n).hn(4)=cell(n).hn(4)+cell(n).fn(11)+cell(n).fn(13) !NH4 en interflujo y flujo base pasan al tanque hn4 (NH4 cauce) de la propia celda
                cell(n).hn(5)=cell(n).hn(5)+cell(n).fn(12)+cell(n).fn(14) !NO3 en interflujo y flujo base pasan al tanque hn5 (NO3 cauce) de la propia celda
            END IF
        End if
    End if
    
    !***Paso 9: movimiento de nitr�geno en superficie***
    If (config(4)) then
        !**Paso 1: depositaci�n de nitr�geno org�nico y amonio adsorbido con sedimentos**
        !Va asociado s�lo a las arcillas porque el proceso de adsorci�n/desorci�n s�lo se produce con las arcillas (volsusini y depositacion son arcillas s�lo)
        !Primero se calcula para ladera, que en verdad son todas las celdas, luego para c�rcava y cauce se recalcula de nuevo. (Sub-modelo de sedimentos deposita dos veces en c�rcava/cauce)
        If (cell(n).volsusini<0.000005) then
            cell(n).fn(21)=0.0
            cell(n).fn(22)=0.0
        Else if (abs(cell(n).volsusini-cell(n).depositacion)<0.000005) then !Si lo suspendido se deposita todo
            !Vol�menes depositados (kg)
            cell(n).fn(21)=cell(n).hn(9)
            cell(n).fn(22)=cell(n).hn(10)
            !Actualizaci�n de los almacenamientos de cauce
            cell(n).hn(9)=0.0
            cell(n).hn(10)=0.0
        Else        
            !Concentraci�n de nitr�geno en el volumen de arcilla (kg/m3)
            ConcNO=cell(n).hn(9)/cell(n).volsusini
            ConcNH4=cell(n).hn(10)/cell(n).volsusini
            !Vol�menes depositados (kg)
            cell(n).fn(21)=ConcNO*cell(n).depositacion
            cell(n).fn(22)=ConcNH4*cell(n).depositacion
            !Actualizaci�n de los almacenamientos de cauce
            cell(n).hn(9)=cell(n).hn(9)-cell(n).fn(21)
            cell(n).hn(10)=cell(n).hn(10)-cell(n).fn(22)
        End if
        !Actualizaci�n de los almacenamentos de suelo (ladera)
        cell(n).hn(8)=cell(n).hn(8)+cell(n).fn(22)
        
        If (nw(2,ncp).le.cell(n).acum) then!C�rcava y cauce (tanques de depositados en red)
            If (cell(n).volsusinired<0.000005) then
                cell(n).fn(21)=0.0
                cell(n).fn(22)=0.0
            Else if (abs(cell(n).volsusinired-cell(n).depositacionred)<0.000005) then !Si lo suspendido se deposita todo
                !Vol�menes depositados (kg)
                cell(n).fn(21)=cell(n).hn(9)
                cell(n).fn(22)=cell(n).hn(10)
                !Actualizaci�n de los almacenamientos de cauce
                cell(n).hn(9)=0.0
                cell(n).hn(10)=0.0
            Else        
                !Concentraci�n de nitr�geno en el volumen de arcilla (kg/m3)
                ConcNO=cell(n).hn(9)/cell(n).volsusinired
                ConcNH4=cell(n).hn(10)/cell(n).volsusinired
                !Vol�menes depositados (kg)
                cell(n).fn(21)=ConcNO*cell(n).depositacionred
                cell(n).fn(22)=ConcNH4*cell(n).depositacionred
                !Actualizaci�n de los almacenamientos de cauce
                cell(n).hn(9)=cell(n).hn(9)-cell(n).fn(21)
                cell(n).hn(10)=cell(n).hn(10)-cell(n).fn(22)
            End if
            !Actualizaci�n de los almacenamentos de depositados (c�rca y cauce)
            cell(n).hn(11)=cell(n).hn(11)+cell(n).fn(21)
            cell(n).hn(12)=cell(n).hn(12)+cell(n).fn(22)
        End if
        !**Paso 2: volumen de nitr�geno disuelto (NO disuelto, NH4 disuelto y NO3) transportado en escorrent�a**
        !Se resuelve ladera para todas las celdas, en las de ladera se mueve a la seguiente celda mientras que en las de c�rcava/cauce se queda en superficie para mover con salrio, que ya inluye y(2). C�rcava y cauce se resuelven luego
            If (cell(n).qdepositado<0.000005) then !No se erosiona nada, no hay salida del suelo, s�lo se mantiene como posible salida lo que haya en superficie
                If ((h2_ini+cell(n).x(2)-cell(n).x(3)+cell(n).exfiltr)<0.000005.or.cell(n).y(2)<0.000005) then
                    cell(n).fn(23)=0.0
                    cell(n).fn(24)=0.0  !No es necesario actualizar almacenamientos
                    cell(n).fn(25)=0.0
                else if (abs((h2_ini+cell(n).x(2)-cell(n).x(3))-cell(n).y(2)+cell(n).exfiltr)<0.000005) then
                    !Se calculan las salidas de cauce
                    cell(n).fn(23)=cell(n).hn(3)
                    cell(n).fn(24)=cell(n).hn(4)
                    cell(n).fn(25)=cell(n).hn(5)
                    If (nw(2,ncp).gt.cell(n).acum) then !Ladera, se mueve a celda destino. C�rcava y cauce se queda en la propia celda y se mueve con salrio, que ya incluye y(2). No hace falta actualizar almacenamientos.
                        !Se actualizan los almacenamientos en cauce celda actual
                        cell(n).hn(3)=0.0
                        cell(n).hn(4)=0.0
                        cell(n).hn(5)=0.0
                        !Se actualizan los almacenamientos en cauce celda destino
                        cell(cell(n).dest).hn(3)=cell(cell(n).dest).hn(3)+cell(n).fn(23)
                        cell(cell(n).dest).hn(4)=cell(cell(n).dest).hn(4)+cell(n).fn(24)
                        cell(cell(n).dest).hn(5)=cell(cell(n).dest).hn(5)+cell(n).fn(25)
                    End if 
                Else
                    !Se calculan las salidas de cauce
                    cell(n).fn(23)=cell(n).hn(3)/(h2_ini+cell(n).x(2)-cell(n).x(3)+cell(n).exfiltr)*cell(n).y(2)
                    cell(n).fn(24)=cell(n).hn(4)/(h2_ini+cell(n).x(2)-cell(n).x(3)+cell(n).exfiltr)*cell(n).y(2)
                    cell(n).fn(25)=cell(n).hn(5)/(h2_ini+cell(n).x(2)-cell(n).x(3)+cell(n).exfiltr)*cell(n).y(2)
                    If (nw(2,ncp).gt.cell(n).acum) then !Ladera, se mueve a celda destino. C�rcava y cauce se queda en la propia celda y se mueve con salrio, que ya incluye y(2). No hace falta actualizar almacenamientos.
                        !Se actualizan los almacenamientos en cauce celda actual
                        cell(n).hn(3)=cell(n).hn(3)-cell(n).fn(23)
                        cell(n).hn(4)=cell(n).hn(4)-cell(n).fn(24)
                        cell(n).hn(5)=cell(n).hn(5)-cell(n).fn(25)
                        !Se actualizan los almacenamientos en cauce celda destino
                        cell(cell(n).dest).hn(3)=cell(cell(n).dest).hn(3)+cell(n).fn(23)
                        cell(cell(n).dest).hn(4)=cell(cell(n).dest).hn(4)+cell(n).fn(24)
                        cell(cell(n).dest).hn(5)=cell(cell(n).dest).hn(5)+cell(n).fn(25)
                    End if !C�rcava y cauce, se queda en la propia celda y se mueve con salrio, que ya incluye y(2)    
                End if
            Else if (arcel*cell(n).psuelo*r(1)<=cell(n).qdepositado) then !Se erosiona todo el suelo, se lleva todo lo que hab�a en el suelo, que ser� el nuevo volumen que puede ser salida, junto con lo que ya hab�a
                If ((h2_ini+cell(n).x(2)-cell(n).x(3)+cell(n).exfiltr)<0.000005.or.cell(n).y(2)<0.000005) then
                    cell(n).fn(23)=0.0
                    cell(n).fn(24)=0.0
                    cell(n).fn(25)=0.0
                else if (abs((h2_ini+cell(n).x(2)-cell(n).x(3)+cell(n).exfiltr)-cell(n).y(2)<0.000005)) then
                    cell(n).fn(23)=cell(n).hn(3)
                    cell(n).fn(24)=cell(n).hn(4)+cell(n).hn(1)
                    cell(n).fn(25)=cell(n).hn(5)+cell(n).hn(2)
                    !Se actualizan los almacenamientos en cauce celda actual, suelo
                    cell(n).hn(1)=0.0
                    cell(n).hn(2)=0.0
                    If (nw(2,ncp).gt.cell(n).acum) then !Ladera, se mueve a celda destino
                        !Se actualizan los almacenamientos en cauce celda actual
                        cell(n).hn(3)=0.0
                        cell(n).hn(4)=0.0
                        cell(n).hn(5)=0.0
                        !Se actualizan los almacenamientos en cauce celda destino
                        cell(cell(n).dest).hn(3)=cell(cell(n).dest).hn(3)+cell(n).fn(23)
                        cell(cell(n).dest).hn(4)=cell(cell(n).dest).hn(4)+cell(n).fn(24)
                        cell(cell(n).dest).hn(5)=cell(cell(n).dest).hn(5)+cell(n).fn(25)
                    Else !C�rcava y cauce, se queda en la propia celda y se mueve con salrio, que ya incluye y(2)
                        cell(n).hn(3)=cell(n).fn(23)
                        cell(n).hn(4)=cell(n).fn(24)
                        cell(n).hn(5)=cell(n).fn(25)
                    End if
                Else
                    !Se actualizan de forma intermedia los almacenamientos en cauce
                    cell(n).hn(3)=cell(n).hn(3)
                    cell(n).hn(4)=cell(n).hn(4)+cell(n).hn(1)
                    cell(n).hn(5)=cell(n).hn(5)+cell(n).hn(2)
                    !Se actualizan los almacenamientos en suelo
                    cell(n).hn(1)=0.0
                    cell(n).hn(2)=0.0
                    !Se calculan las salidas del cauce
                    cell(n).fn(23)=cell(n).hn(3)/(h2_ini+cell(n).x(2)-cell(n).x(3)+cell(n).exfiltr)*cell(n).y(2)
                    cell(n).fn(24)=cell(n).hn(4)/(h2_ini+cell(n).x(2)-cell(n).x(3)+cell(n).exfiltr)*cell(n).y(2)
                    cell(n).fn(25)=cell(n).hn(5)/(h2_ini+cell(n).x(2)-cell(n).x(3)+cell(n).exfiltr)*cell(n).y(2)
                    If (nw(2,ncp).gt.cell(n).acum) then !Ladera, se mueve a celda destino
                        !Se actualizan los almacenamientos en cauce celda actual
                        cell(n).hn(3)=cell(n).hn(3)-cell(n).fn(23)
                        cell(n).hn(4)=cell(n).hn(4)-cell(n).fn(24)
                        cell(n).hn(5)=cell(n).hn(5)-cell(n).fn(25)
                        !Se actualizan los almacenamientos en cauce celda destino
                        cell(cell(n).dest).hn(3)=cell(cell(n).dest).hn(3)+cell(n).fn(23)
                        cell(cell(n).dest).hn(4)=cell(cell(n).dest).hn(4)+cell(n).fn(24)
                        cell(cell(n).dest).hn(5)=cell(cell(n).dest).hn(5)+cell(n).fn(25)
                    End if
                End if
            Else
                If ((h2_ini+cell(n).x(2)-cell(n).x(3)+cell(n).exfiltr)<0.000005.or.cell(n).y(2)<0.000005) then
                    cell(n).fn(23)=0.0
                    cell(n).fn(24)=0.0
                    cell(n).fn(25)=0.0
                Else if (abs((h2_ini+cell(n).x(2)-cell(n).x(3)+cell(n).exfiltr)-cell(n).y(2)<0.000005)) then
                    cell(n).fn(23)=cell(n).hn(3)
                    cell(n).fn(24)=cell(n).hn(4)+cell(n).hn(1)/(arcel*cell(n).psuelo*r(1))*cell(n).qdepositado
                    cell(n).fn(25)=cell(n).hn(5)+cell(n).hn(2)/(arcel*cell(n).psuelo*r(1))*cell(n).qdepositado
                    !Se actualizan los almacenamientos en suelo
                    cell(n).hn(1)=cell(n).hn(1)-cell(n).hn(1)/(arcel*cell(n).psuelo*r(1))*cell(n).qdepositado
                    cell(n).hn(2)=cell(n).hn(2)-cell(n).hn(2)/(arcel*cell(n).psuelo*r(1))*cell(n).qdepositado
                    If (nw(2,ncp).gt.cell(n).acum) then !Ladera, se mueve a celda destino
                        !Se actualizan los almacenamientos en cauce celda actual
                        cell(n).hn(3)=0.0
                        cell(n).hn(4)=0.0
                        cell(n).hn(5)=0.0
                        !Se actualizan los almacenamientos en cauce celda destino
                        cell(cell(n).dest).hn(3)=cell(cell(n).dest).hn(3)+cell(n).fn(23)
                        cell(cell(n).dest).hn(4)=cell(cell(n).dest).hn(4)+cell(n).fn(24)
                        cell(cell(n).dest).hn(5)=cell(cell(n).dest).hn(5)+cell(n).fn(25)
                    Else !Se queda en superifice para moverlo luego
                        cell(n).hn(3)=cell(n).fn(23)
                        cell(n).hn(4)=cell(n).fn(24)
                        cell(n).hn(5)=cell(n).fn(25)
                    End if
                Else
                    !Se actualizan de forma intermedia los almacenamientos en cauce
                    cell(n).hn(3)=cell(n).hn(3)
                    cell(n).hn(4)=cell(n).hn(4)+cell(n).hn(1)/(arcel*cell(n).psuelo*r(1))*cell(n).qdepositado !Aqu� suelo, no s�lo arcilla
                    cell(n).hn(5)=cell(n).hn(5)+cell(n).hn(2)/(arcel*cell(n).psuelo*r(1))*cell(n).qdepositado
                    !Se actualizan los almacenamientos en suelo
                    cell(n).hn(1)=cell(n).hn(1)-cell(n).hn(1)/(arcel*cell(n).psuelo*r(1))*cell(n).qdepositado
                    cell(n).hn(2)=cell(n).hn(2)-cell(n).hn(2)/(arcel*cell(n).psuelo*r(1))*cell(n).qdepositado
                    !Se calculan las salidas del cauce
                    cell(n).fn(23)=cell(n).hn(3)/(h2_ini+cell(n).x(2)-cell(n).x(3)+cell(n).exfiltr)*cell(n).y(2)
                    cell(n).fn(24)=cell(n).hn(4)/(h2_ini+cell(n).x(2)-cell(n).x(3)+cell(n).exfiltr)*cell(n).y(2)
                    cell(n).fn(25)=cell(n).hn(5)/(h2_ini+cell(n).x(2)-cell(n).x(3)+cell(n).exfiltr)*cell(n).y(2)
                    If (nw(2,ncp).gt.cell(n).acum) then !Ladera, se mueve a celda destino
                        !Se actualizan los almacenamientos en cauce celda actual
                        cell(n).hn(3)=cell(n).hn(3)-cell(n).fn(23)
                        cell(n).hn(4)=cell(n).hn(4)-cell(n).fn(24)
                        cell(n).hn(5)=cell(n).hn(5)-cell(n).fn(25)
                        !Se actualizan los almacenamientos en cauce celda destino
                        cell(cell(n).dest).hn(3)=cell(cell(n).dest).hn(3)+cell(n).fn(23)
                        cell(cell(n).dest).hn(4)=cell(cell(n).dest).hn(4)+cell(n).fn(24)
                        cell(cell(n).dest).hn(5)=cell(cell(n).dest).hn(5)+cell(n).fn(25)
                    End if
                End if
            End if    
        If (nw(2,ncp).le.cell(n).acum) then !C�rcava y cauce
            !Volumen de agua a computar: agua en escorrent�a + agua en cauce (agua en superficie)
            !((Al tomar h5 final y sumarle salrio ya estoy incluyendo el volumen  de agua de la propia celda de escorrent�a, interflujo y flujo base, pero hay que sumar h2 final por si quedase algo en el tanque))
            Volag8=cell(n).h(2)/1000*arcel+cell(n).h(5)+salrio
            If (Volag8<0.000005.or.salrio<0.000005) then   
                cell(n).fn(18)=0.0
                cell(n).fn(19)=0.0
                cell(n).fn(20)=0.0
            Else if (abs(Volag8-salrio)<0.000005) then
                !C�lculo de las salidas con el caudal
                cell(n).fn(18)=cell(n).hn(3)
                cell(n).fn(19)=cell(n).hn(4)
                cell(n).fn(20)=cell(n).hn(5)
                !Actualizaci�n de los almacenamientos en celda cauce actual
                cell(n).hn(3)=0.0
                cell(n).hn(4)=0.0
                cell(n).hn(5)=0.0
                !Actualizaci�n de los almacenamientos en celda cauce destino
                cell(cell(n).dest).hn(3)=cell(cell(n).dest).hn(3)+cell(n).fn(18)
                cell(cell(n).dest).hn(4)=cell(cell(n).dest).hn(4)+cell(n).fn(19)
                cell(cell(n).dest).hn(5)=cell(cell(n).dest).hn(5)+cell(n).fn(20)
           else     
                !Concentraci�n en el volumen de agua en superficie
                ConcNO=cell(n).hn(3)/Volag8
                ConcNH4=cell(n).hn(4)/Volag8
                ConcNO3=cell(n).hn(5)/Volag8
                !C�lculo de las salidas con el caudal
                cell(n).fn(18)=ConcNO*salrio
                cell(n).fn(19)=ConcNH4*salrio
                cell(n).fn(20)=ConcNO3*salrio
                !Actualizaci�n de los almacenamientos en celda cauce actual
                cell(n).hn(3)=cell(n).hn(3)-cell(n).fn(18)
                cell(n).hn(4)=cell(n).hn(4)-cell(n).fn(19)
                cell(n).hn(5)=cell(n).hn(5)-cell(n).fn(20)
                !Actualizaci�n de los almacenamientos en celda cauce destino
                cell(cell(n).dest).hn(3)=cell(cell(n).dest).hn(3)+cell(n).fn(18)
                cell(cell(n).dest).hn(4)=cell(cell(n).dest).hn(4)+cell(n).fn(19)
                cell(cell(n).dest).hn(5)=cell(cell(n).dest).hn(5)+cell(n).fn(20)
           End if
           !Se ceran las variables de ladera en c�rcava/cauce, porque realmente ya se incluye en la fn(18), fn(19) y fn(20)
           cell(n).fn(23)=0.0
           cell(n).fn(24)=0.0
           cell(n).fn(25)=0.0
        End if
        
        !**Paso 3: volumen de nitr�geno no disuelto (NO y NH4 particulado) transportado en escorrent�a**
        !Se considera que el NO y el NH4 van adsorbidos a la arcilla �nicamente, por eso los vol�menes de sedimentos son s�lo de arcillas
        !Ladera se resuelve para todas las celdas!!!!!! (como el suelo es infinito siempre considero el mismo volumen arcel*psuelo)
            If (cell(n).qdepositadoar<0.000005) then !No se erosiona nada, no hay salida del suelo, s�lo se mantiene como posible salida lo que haya en superficie
                cell(n).fn(26)=0.0
                cell(n).fn(27)=0.0
            Else if ((arcel*cell(n).psuelo*r(1)*cell(n).porcentaje(3))<cell(n).qdepositadoar) then !Si lo erosionado es superior al suelo, se va todo
                cell(n).fn(26)=cell(n).hn(0)
                cell(n).fn(27)=cell(n).hn(8)
                !Actualizaci�n de los almacenamientos en cauce tras la suspensi�n (kg)
                cell(n).hn(9)=cell(n).hn(9)+cell(n).fn(26)
                cell(n).hn(10)=cell(n).hn(10)+cell(n).fn(27)
                !Actualizaci�n de los almacenamientos en suelo tras la suspensi�n (kg)
                cell(n).hn(8)=0.0
            Else
                !Volumen de nitr�geno a computar: amonio adsorbido y nitr�geno org�nico en el volumen de suelo correspondiente a los m3 de erosi�n/depositado (kg)
                VolNO=cell(n).hn(0)/(arcel*cell(n).psuelo*r(1)*cell(n).porcentaje(3))*cell(n).qdepositadoar
                VolNH4=cell(n).hn(8)/(arcel*cell(n).psuelo*r(1)*cell(n).porcentaje(3))*cell(n).qdepositadoar
                !Concentraci�n del volumen de suelo erosionado/depositado (kg/m3)
                ConcNO=VolNO/cell(n).qdepositadoar
                ConcNH4=VolNH4/cell(n).qdepositadoar
                !Volumen de nitr�geno que pasa a estar suspendido (kg)
                cell(n).fn(26)=ConcNO*cell(n).qdepositadoar
                cell(n).fn(27)=ConcNH4*cell(n).qdepositadoar
                !Actualizaci�n de los almacenamientos en cauce tras la suspensi�n (kg)
                cell(n).hn(9)=cell(n).hn(9)+cell(n).fn(26)
                cell(n).hn(10)=cell(n).hn(10)+cell(n).fn(27)
                !Actualizaci�n de los almacenamientos en suelo tras la suspensi�n (kg)
                cell(n).hn(8)=cell(n).hn(8)-cell(n).fn(27)
            End if
        If (nw(2,ncp).le.cell(n).acum) then !C�rcava y cauce (el volumen de suelo es el depositado, porque aqu� no hay erosi�n de parental. voldepini ya est� almacenando s�lo arcillas)
            If (cell(n).voldepinired+cell(n).depositacionred<0.000005) then  !volumen depositado
                cell(n).fn(26)=0.0
                cell(n).fn(27)=0.0
            Else
                !Volumen a computar: los m3 de suelo depositados (m3)
                !Volumen de nitr�geno a computar: amonio adsorbido y nitr�geno org�nico en el volumen de suelo correspondiente a los m3 de depositados (kg)
                !Concentraci�n del volumen de suelo depositado (kg/m3)
                ConcNO=cell(n).hn(11)/(cell(n).voldepinired+cell(n).depositacionred)
                ConcNH4=cell(n).hn(12)/(cell(n).voldepinired+cell(n).depositacionred)
                !Volumen de nitr�geno que pasa a estar susepndido (kg)
                cell(n).fn(26)=ConcNO*cell(n).qdepositadoarred
                cell(n).fn(27)=ConcNH4*cell(n).qdepositadoarred
            End if
            !Actualizaci�n de los almacenamientos en cauce tras la suspensi�n (kg)
            cell(n).hn(9)=cell(n).hn(9)+cell(n).fn(26)
            cell(n).hn(10)=cell(n).hn(10)+cell(n).fn(27)
            !Actualizaci�n de los almacenamientos en suelo depositado tras la suspensi�n (kg)
            cell(n).hn(11)=cell(n).hn(11)-cell(n).fn(26)
            cell(n).hn(12)=cell(n).hn(12)-cell(n).fn(27)
        End if
        !C�lculo de las salidas con el caudal de sedimentos
        !Volumen a computar: m3 de sedimentos suspendidos inicialmente - depositados + resuspendidos (m3)  (volsusini y depositacion son arcillas s�lo)
        IF (nw(2,ncp).gt.cell(n).acum) then !Ladera
            Volag10=cell(n).volsusini-cell(n).depositacion+cell(n).qdepositadoar
            If (Volag10<0.0005.or.cell(n).qsuspendidoar+cell(n).qdepositadoar<0.0005) then
                cell(n).fn(28)=0.0
                cell(n).fn(29)=0.0
            else if (abs(cell(n).volsusini-cell(n).depositacion<0.0005).or.abs(Volag10-(cell(n).qsuspendidoar+cell(n).qdepositadoar))<0.0005) then
                cell(n).fn(28)=cell(n).hn(9)
                cell(n).fn(29)=cell(n).hn(10)
                !Actualizaci�n de los almacenamientos en cauce celda actual
                cell(n).hn(9)=0.0
                cell(n).hn(10)=0.0
                !Actualizaci�n de los almacenamientos en cauce celda destino
                cell(cell(n).dest).hn(9)=cell(cell(n).dest).hn(9)+cell(n).fn(28)
                cell(cell(n).dest).hn(10)=cell(cell(n).dest).hn(10)+cell(n).fn(29)
            Else
                !Concentraci�n del volumen de sedimentos suspendidos (kg/m3)
                ConcNO=cell(n).hn(9)/Volag10
                ConcNH4=cell(n).hn(10)/Volag10
                !C�lculo de las salidas con el caudal de sedimentos
                cell(n).fn(28)=ConcNO*(cell(n).qsuspendidoar+cell(n).qdepositadoar)
                cell(n).fn(29)=ConcNH4*(cell(n).qsuspendidoar+cell(n).qdepositadoar)
                !Actualizaci�n de los almacenamientos en cauce celda actual
                cell(n).hn(9)=cell(n).hn(9)-cell(n).fn(28)
                cell(n).hn(10)=cell(n).hn(10)-cell(n).fn(29)  
                !Actualizaci�n de los almacenamientos en cauce celda destino
                cell(cell(n).dest).hn(9)=cell(cell(n).dest).hn(9)+cell(n).fn(28)
                cell(cell(n).dest).hn(10)=cell(cell(n).dest).hn(10)+cell(n).fn(29)
            End if      
        Else
            Volag10=cell(n).volsusini-cell(n).depositacion+cell(n).qdepositadoar+cell(n).volsusinired-cell(n).depositacionred+cell(n).qdepositadoarred
            If (Volag10<0.0005.or.cell(n).qsuspendidoar+cell(n).qdepositadoar+cell(n).qsuspendidoarred+cell(n).qdepositadoarred<0.0005) then
                cell(n).fn(28)=0.0
                cell(n).fn(29)=0.0
            else if (abs(cell(n).volsusini-cell(n).depositacion+cell(n).volsusinired-cell(n).depositacionred<0.0005).or.abs(Volag10-(cell(n).qsuspendidoar+cell(n).qdepositadoar+cell(n).qsuspendidoarred+cell(n).qdepositadoarred))<0.0005) then
                cell(n).fn(28)=cell(n).hn(9)
                cell(n).fn(29)=cell(n).hn(10)
                !Actualizaci�n de los almacenamientos en cauce celda actual
                cell(n).hn(9)=0.0
                cell(n).hn(10)=0.0
                !Actualizaci�n de los almacenamientos en cauce celda destino
                cell(cell(n).dest).hn(9)=cell(cell(n).dest).hn(9)+cell(n).fn(28)
                cell(cell(n).dest).hn(10)=cell(cell(n).dest).hn(10)+cell(n).fn(29)
            Else
                !Concentraci�n del volumen de sedimentos suspendidos (kg/m3)
                ConcNO=cell(n).hn(9)/Volag10
                ConcNH4=cell(n).hn(10)/Volag10
                !C�lculo de las salidas con el caudal de sedimentos
                cell(n).fn(28)=ConcNO*(cell(n).qsuspendidoar+cell(n).qdepositadoar+cell(n).qsuspendidoarred+cell(n).qdepositadoarred)
                cell(n).fn(29)=ConcNH4*(cell(n).qsuspendidoar+cell(n).qdepositadoar+cell(n).qsuspendidoarred+cell(n).qdepositadoarred)
                !Actualizaci�n de los almacenamientos en cauce celda actual
                cell(n).hn(9)=cell(n).hn(9)-cell(n).fn(28)
                cell(n).hn(10)=cell(n).hn(10)-cell(n).fn(29)  
                !Actualizaci�n de los almacenamientos en cauce celda destino
                cell(cell(n).dest).hn(9)=cell(cell(n).dest).hn(9)+cell(n).fn(28)
                cell(cell(n).dest).hn(10)=cell(cell(n).dest).hn(10)+cell(n).fn(29)
            End if
        End if
        !End if
    Else !Si no est�n los sedientos activados, no hay transmisi�n de nitr�geno disuelto en suelo a nitr�geno disuelto en superficie. Se hace esta aproximaci�n porque en la realidad ser�a casi nula.
        !**Paso 4: volumen de nitr�geno disuelto transportado en superficie con sedimentos desactivados**
        If (nw(2,ncp).gt.cell(n).acum) then !Ladera, lo normal ser� que el agua vaya limpia, pero por si hay vertido la muevo
            If ((h2_ini+cell(n).x(2)-cell(n).x(3))<0.000005.or.cell(n).y(2)<0.000005) then
                cell(n).fn(23)=0.0
                cell(n).fn(24)=0.0
                cell(n).fn(25)=0.0
            else if (abs((h2_ini+cell(n).x(2)-cell(n).x(3))-cell(n).y(2))<0.000005) then
                !Se calculan las salidas de cauce
                cell(n).fn(23)=cell(n).hn(3)
                cell(n).fn(24)=cell(n).hn(4)
                cell(n).fn(25)=cell(n).hn(5)
                !Se actualizan los almacenamientos en cauce celda actual
                cell(n).hn(3)=0.0
                cell(n).hn(4)=0.0
                cell(n).hn(5)=0.0
                !Se actualizan los almacenamientos en cauce celda destino
                cell(cell(n).dest).hn(3)=cell(cell(n).dest).hn(3)+cell(n).fn(23)
                cell(cell(n).dest).hn(4)=cell(cell(n).dest).hn(4)+cell(n).fn(24)
                cell(cell(n).dest).hn(5)=cell(cell(n).dest).hn(5)+cell(n).fn(25)    
            Else
                !Volumen a computar: agua en escorrent�a (mm) 
                !Volumen de nitr�geno a computar: nitr�geno org�nico disuelto, amonio disuelto y nitrato en cauce (kg)
                !Concentraci�n del volumen de agua en superficie (kg/mm)
                ConcNO=cell(n).hn(3)/(h2_ini+cell(n).x(2)-cell(n).x(3)+cell(n).exfiltr)
                ConcNH4=cell(n).hn(4)/(h2_ini+cell(n).x(2)-cell(n).x(3)+cell(n).exfiltr)
                ConcNO3=cell(n).hn(5)/(h2_ini+cell(n).x(2)-cell(n).x(3)+cell(n).exfiltr)
                !C�lculo de las salidas de nitr�geno disuelto por escorrent�a directa (en superficie) (kg)
                cell(n).fn(23)=ConcNO*cell(n).y(2)
                cell(n).fn(24)=ConcNH4*cell(n).y(2)
                cell(n).fn(25)=ConcNO3*cell(n).y(2)
                !Actualizaci�n de los almacenamientos en cauce celda actual (kg)
                cell(n).hn(3)=cell(n).hn(3)-cell(n).fn(23)
                cell(n).hn(4)=cell(n).hn(4)-cell(n).fn(24)
                cell(n).hn(5)=cell(n).hn(5)-cell(n).fn(25)
                !Actualizaci�n de los almacenamientos en cauce celda destino (kg)
                cell(cell(n).dest).hn(3)=cell(cell(n).dest).hn(3)+cell(n).fn(23)
                cell(cell(n).dest).hn(4)=cell(cell(n).dest).hn(4)+cell(n).fn(24)
                cell(cell(n).dest).hn(5)=cell(cell(n).dest).hn(5)+cell(n).fn(25)
            End if
        Else !C�rcava y cauce
            !Volumen a computar: volumen antes de la transferencia a la siguiente celda de cauce
            Volag6=cell(n).h(2)/1000*arcel+cell(n).h(5)+salrio
            If (Volag6<0.000005.or.salrio<0.000005) then  
                cell(n).fn(18)=0.0
                cell(n).fn(19)=0.0
                cell(n).fn(20)=0.0
            Else if (abs(Volag6-salrio)<0.000005) then
                !C�lculo de las salidas con el caudal
                cell(n).fn(18)=cell(n).hn(3)
                cell(n).fn(19)=cell(n).hn(4)
                cell(n).fn(20)=cell(n).hn(5)
                !Actualizaci�n de los almacenamientos en celda cauce actual
                cell(n).hn(3)=0.0
                cell(n).hn(4)=0.0
                cell(n).hn(5)=0.0
                !Actualizaci�n de los almacenamientos en celda cauce destino
                cell(cell(n).dest).hn(3)=cell(cell(n).dest).hn(3)+cell(n).fn(18)
                cell(cell(n).dest).hn(4)=cell(cell(n).dest).hn(4)+cell(n).fn(19)
                cell(cell(n).dest).hn(5)=cell(cell(n).dest).hn(5)+cell(n).fn(20)
            Else
                !C�lculo de la concentraci�n en cauce (kg/m3)
                ConcNO=cell(n).hn(3)/Volag6
                ConcNH4=cell(n).hn(4)/Volag6
                ConcNO3=cell(n).hn(5)/Volag6
                !C�lculo de las salidas con el caudal (kg)
                cell(n).fn(18)=ConcNO*salrio
                cell(n).fn(19)=ConcNH4*salrio
                cell(n).fn(20)=ConcNO3*salrio
                !Actualizaci�n de los almacenamientos en celda cauce actual
                cell(n).hn(3)=cell(n).hn(3)-cell(n).fn(18)
                cell(n).hn(4)=cell(n).hn(4)-cell(n).fn(19)
                cell(n).hn(5)=cell(n).hn(5)-cell(n).fn(20)
                !Actualizaci�n de los almacenamientos en celda cauce destino
                cell(cell(n).dest).hn(3)=cell(cell(n).dest).hn(3)+cell(n).fn(18)
                cell(cell(n).dest).hn(4)=cell(cell(n).dest).hn(4)+cell(n).fn(19)
                cell(cell(n).dest).hn(5)=cell(cell(n).dest).hn(5)+cell(n).fn(20)
            End if
        End if
        !Anulaci�n de los flujos que s�lo existen si sedimentos est� activado
        cell(n).fn(21)=0.0
        cell(n).fn(22)=0.0
        cell(n).fn(26)=0.0
        cell(n).fn(27)=0.0
        cell(n).fn(28)=0.0
        cell(n).fn(29)=0.0
    End if
    
    !***Paso 10: Proceso de adsorci�n/desorci�n de NH4 en suelo***
    NH4total=cell(n).hn(1)+cell(n).hn(8)
    if((cell(n).h(1)+cell(n).h(3)+cell(n).hlim)>0.000005) then
        cell(n).hn(1)=((cell(n).h(1)+cell(n).h(3)+cell(n).hlim)/1000*NH4total)/((cell(n).h(1)+cell(n).h(3)+cell(n).hlim)/1000+cell(n).kd*fckd*cell(n).psuelo*r(1)*cell(n).daparente)
        Cell(n).hn(8)=NH4total-cell(n).hn(1)
        If(cell(n).hn(8)<0.0) then
            cell(n).hn(1)=NH4total
            cell(n).hn(8)=0.0
        End if
        If(cell(n).kd<0.0001.or.cell(n).psuelo*r(1)<0.0001.or.cell(n).daparente<0.0001) then
            cell(n).hn(1)=cell(n).hn(1)
            cell(n).hn(8)=cell(n).hn(8)
        End if
    else
        cell(n).hn(1)=0.0
        cell(n).hn(8)=NH4total
    end if
    
    !***Paso 11: Almacenamiento de los caudales (kg/s) y concentraciones en superficie (mg/l en ql o kg/m3 en qs)***
    If (nw(2,ncp).gt.cell(n).acum) then !Ladera
        cell(n).qlNO=cell(n).fn(23)/dts
        cell(n).qlNH4=cell(n).fn(24)/dts
        cell(n).qlNO3=cell(n).fn(25)/dts
        If ((cell(n).y(2)/1000*arcel)/dts<0.0000005) then
            cell(n).ConcqlNO=0.0
            cell(n).ConcqlNH4=0.0
            cell(n).ConcqlNO3=0.0
        Else
            cell(n).ConcqlNO=(cell(n).fn(23)/(cell(n).y(2)/1000*arcel))*1000
            cell(n).ConcqlNH4=(cell(n).fn(24)/(cell(n).y(2)/1000*arcel))*1000
            cell(n).ConcqlNO3=(cell(n).fn(25)/(cell(n).y(2)/1000*arcel))*1000
        End if
    Else !C�rcava y cauce
        cell(n).qlNO=cell(n).fn(18)/dts
        cell(n).qlNH4=cell(n).fn(19)/dts
        cell(n).qlNO3=cell(n).fn(20)/dts
        If (salrio/dts<0.0000005) then
            cell(n).ConcqlNO=0.0
            cell(n).ConcqlNH4=0.0
            cell(n).ConcqlNO3=0.0
        Else
            cell(n).ConcqlNO=cell(n).fn(18)/salrio*1000
            cell(n).ConcqlNH4=cell(n).fn(19)/salrio*1000
            cell(n).ConcqlNO3=cell(n).fn(20)/salrio*1000
        End if
    End if
    If (config(4)) then
        cell(n).qsNO=cell(n).fn(28)/dts
        cell(n).qsNH4=cell(n).fn(29)/dts
        If (nw(2,ncp).gt.cell(n).acum) then !Ladera
            IF ((cell(n).qsuspendido+cell(n).qdepositado).le.0.0) then
                cell(n).ConcqsNO=0.0
                cell(n).ConcqsNH4=0.0
            ELSE    
                cell(n).ConcqsNO=cell(n).fn(28)/(cell(n).qsuspendido+cell(n).qdepositado)   !Aunque el NO y el NH4 vayan adsorbidos a las arcillas s�lo, la concentraci�n se expresa sobre el total de sedimentos 
                cell(n).ConcqsNH4=cell(n).fn(29)/(cell(n).qsuspendido+cell(n).qdepositado)
            END IF
        else
            IF ((cell(n).qsuspendidored+cell(n).qdepositadored).le.0.0) then
                cell(n).ConcqsNO=0.0
                cell(n).ConcqsNH4=0.0
            ELSE    
                cell(n).ConcqsNO=cell(n).fn(28)/(cell(n).qsuspendidored+cell(n).qdepositadored)   !Aunque el NO y el NH4 vayan adsorbidos a las arcillas s�lo, la concentraci�n se expresa sobre el total de sedimentos 
                cell(n).ConcqsNH4=cell(n).fn(29)/(cell(n).qsuspendidored+cell(n).qdepositadored)
            END IF
        end if   
    Else
        cell(n).qsNO=0.0
        cell(n).qsNH4=0.0
        cell(n).ConcqsNO=0.0
        cell(n).ConcqsNH4=0.0
    End if
    
    !***Paso 12: Almacenamiento de las concentraciones en los puntos de aforo y los observados. Las concentraciones corresponden a la concentraci�n del caudal que sale***
    !Almacena resultados en las celdas Q
    Do i=1,naf
        If (n.eq.aforo(i).pos) then
            norgql(i).sim(t)=cell(n).ConcqlNO !(mg/l)
            amonioql(i).sim(t)=cell(n).ConcqlNH4 !(mg/l)
            nitratoql(i).sim(t)=cell(n).ConcqlNO3 !(mg/l)
            If (config(4)) then
                norgqs(i).sim(t)=cell(n).ConcqsNO !(kg/m3)
                amonioqs(i).sim(t)=cell(n).ConcqsNH4 !(kg/m3)
            End if
        End if
    End do
    !Almacena resultados en las celdas NO
    Do i=1,kno
        If (n.eq.norg(i).pos) then
            norg(i).sim(t)=cell(n).ConcqlNO !(mg/l)
        End if
    End do
    !Almacena resultados en las celdas AM
    Do i=1,kam
        If (n.eq.amonio(i).pos) then
            amonio(i).sim(t)=cell(n).ConcqlNH4 !(mg/l)
        End if
    End do
    !Almacena resultados en las celdas NI
    Do i=1,kni
        If (n.eq.nitrato(i).pos) then
            nitrato(i).sim(t)=cell(n).ConcqlNO3 !(mg/l)
        End if
    End do
    
    !**Paso 13: Almacena los flujos y estados
    !Control de negativos por almacenamiento de variables en fortran, pero s�lo hasta un margen, si es m�s negativo algo est� mal.
    Do i=0,46
        If (cell(n).fn(i)<0.0.and.cell(n).fn(i)>=-0.00001) then
            cell(n).fn(i)=0.0
        End if
    End do
    Do i=0,12
        If (cell(n).hn(i)<0.0.and.cell(n).hn(i)>=-0.00001) then
            cell(n).hn(i)=0.0
        End if
    End do
    
    !**Obtenemos las eficiencias
    totInput = cell(n).fn(40)+cell(n).fn(41)+cell(n).fn(45)+cell(n).fn(46)+max(0.0,cell(n).fn(0)-cell(n).fn(1))
    totUptake = cell(n).fn(9)+cell(n).fn(10)+cell(n).fn(30)+cell(n).fn(31)
    totPercolation = cell(n).fn(5)+cell(n).fn(6)
    IF ( totInput == 0.0) THEN
        cell(n).uptEff = 0.0
        cell(n).recEff = 0.0
    ELSE
        cell(n).uptEff = totUptake / totInput
        cell(n).recEff = totPercolation / totInput        
    END IF
    cell(n).allUptake = cell(n).allUptake + totUptake
    cell(n).allPercolation = cell(n).allPercolation + totPercolation
    cell(n).allInput = cell(n).allInput + totInput
    
    balanc_nitr(t,1)=balanc_nitr(t,1)+cell(n).fn(0) !Mineralizaci�n suelo
    balanc_nitr(t,2)=balanc_nitr(t,2)+cell(n).fn(1) !Inmovilizaci�n suelo
    balanc_nitr(t,3)=balanc_nitr(t,3)+cell(n).fn(2) !Nitrificaci�n suelo
    balanc_nitr(t,4)=balanc_nitr(t,4)+cell(n).fn(3) !Fijaci�n suelo
    balanc_nitr(t,5)=balanc_nitr(t,5)+cell(n).fn(4) !Desnitrificaci�n suelo
    balanc_nitr(t,6)=balanc_nitr(t,6)+cell(n).fn(5) !Percolaci�n NH4
    balanc_nitr(t,7)=balanc_nitr(t,7)+cell(n).fn(6) !Percolaci�n NO3
    balanc_nitr(t,8)=balanc_nitr(t,8)+cell(n).fn(7) !P�rdidas subterr�neas NH4
    balanc_nitr(t,9)=balanc_nitr(t,9)+cell(n).fn(8) !P�rdidas subterr�neas NO3
    balanc_nitr(t,10)=balanc_nitr(t,10)+cell(n).fn(9) !Transpiraci�n (asimilaci�n pasiva) NH4
    balanc_nitr(t,11)=balanc_nitr(t,11)+cell(n).fn(10) !Transpiraci�n (asimilaci�n pasiva) NO3
    balanc_nitr(t,12)=balanc_nitr(t,12)+cell(n).fn(11) !Interflujo NH4
    balanc_nitr(t,13)=balanc_nitr(t,13)+cell(n).fn(12) !Interflujo NO3
    balanc_nitr(t,14)=balanc_nitr(t,14)+cell(n).fn(13) !Flujo base NH4
    balanc_nitr(t,15)=balanc_nitr(t,15)+cell(n).fn(14) !Flujo base NO3
    balanc_nitr(t,16)=balanc_nitr(t,16)+cell(n).fn(15) !Mineralizaci�n cauce
    balanc_nitr(t,17)=balanc_nitr(t,17)+cell(n).fn(16) !Nitrificaci�n cauce
    balanc_nitr(t,18)=balanc_nitr(t,18)+cell(n).fn(17) !Desnitrificaci�n cauce
    balanc_nitr(t,19)=balanc_nitr(t,19)+cell(n).fn(18) !Caudal NO
    balanc_nitr(t,20)=balanc_nitr(t,20)+cell(n).fn(19) !Caudal NH4
    balanc_nitr(t,21)=balanc_nitr(t,21)+cell(n).fn(20) !Caudal NO3
    balanc_nitr(t,22)=balanc_nitr(t,22)+cell(n).fn(21) !Depositaci�n NO
    balanc_nitr(t,23)=balanc_nitr(t,23)+cell(n).fn(22) !Depositaci�n NH4 adsorbido
    balanc_nitr(t,24)=balanc_nitr(t,24)+cell(n).fn(23) !Escorrent�a NO disuelto
    balanc_nitr(t,25)=balanc_nitr(t,25)+cell(n).fn(24) !Escorrent�a NH4 disuelto
    balanc_nitr(t,26)=balanc_nitr(t,26)+cell(n).fn(25) !Escorrent�a NO3
    balanc_nitr(t,27)=balanc_nitr(t,27)+cell(n).fn(26) !Suspensi�n NO
    balanc_nitr(t,28)=balanc_nitr(t,28)+cell(n).fn(27) !Suspensi�n NH4 adsorbido
    balanc_nitr(t,29)=balanc_nitr(t,29)+cell(n).fn(28) !Caudal sedimentos NO
    balanc_nitr(t,30)=balanc_nitr(t,30)+cell(n).fn(29) !Caudal sedimentos NH4
    balanc_nitr(t,31)=balanc_nitr(t,31)+cell(n).fn(30) !Activa NH4
    balanc_nitr(t,32)=balanc_nitr(t,32)+cell(n).fn(31) !Activa NO3
    
    !balanc_nitr(t,46)=balanc_nitr(t,46)+cell(n).fn(32) !P�rdidas subterr�neas en acu�fero conectado NH4
    !balanc_nitr(t,47)=balanc_nitr(t,47)+cell(n).fn(33) !P�rdidas subterr�neas en acu�fero conectado NO3
    !balanc_nitr(t,48)=balanc_nitr(t,48)+cell(n).fn(34) !P�rdidas subterr�neas en acu�fero NO conectado NH4 (toda la percolaci�n)
    !balanc_nitr(t,49)=balanc_nitr(t,49)+cell(n).fn(35) !P�rdidas subterr�neas en acu�fero NO conectado NO3 (toda la percolaci�n)
    !balanc_nitr(t,50)=balanc_nitr(t,50)+cell(n).fn(36) !Percolaci�n en acu�fero conectado NH4
    !balanc_nitr(t,51)=balanc_nitr(t,51)+cell(n).fn(37) !Percolaci�n en acu�fero conectado NO3
    !balanc_nitr(t,52)=balanc_nitr(t,52)+cell(n).fn(38) !Percolaci�n  en acu�fero NO conectado NH4
    !balanc_nitr(t,53)=balanc_nitr(t,53)+cell(n).fn(39) !Percolaci�n  en acu�fero NO conectado NO3
    balanc_nitr(t,54)=balanc_nitr(t,54)+cell(n).fn(40) !Input NH4
    balanc_nitr(t,55)=balanc_nitr(t,55)+cell(n).fn(41) !Input NO3
    balanc_nitr(t,56)=balanc_nitr(t,56)+cell(n).fn(42) !Exfiltraci�n NH4
    balanc_nitr(t,57)=balanc_nitr(t,57)+cell(n).fn(43) !Exfiltraci�n NO3
    balanc_nitr(t,58)=balanc_nitr(t,58)+cell(n).fn(44) !Volatilizaci�n
    balanc_nitr(t,59)=balanc_nitr(t,59)+cell(n).fn(45) !Input por deposici�n atmosf�rica NH4
    balanc_nitr(t,60)=balanc_nitr(t,60)+cell(n).fn(46) !Input por deposici�n atmosf�rica NO3
    
    balanc_nitr(t,33)=balanc_nitr(t,33)+cell(n).hn(0) !NO suelo
    balanc_nitr(t,34)=balanc_nitr(t,34)+cell(n).hn(1) !NH4 disuelto suelo
    balanc_nitr(t,35)=balanc_nitr(t,35)+cell(n).hn(2) !NO3 suelo
    balanc_nitr(t,36)=balanc_nitr(t,36)+cell(n).hn(8) !NH4 particulado suelo
    balanc_nitr(t,37)=balanc_nitr(t,37)+cell(n).hn(3) !NO disuelto cauce
    balanc_nitr(t,38)=balanc_nitr(t,38)+cell(n).hn(4) !NH4 disuelto cauce
    balanc_nitr(t,39)=balanc_nitr(t,39)+cell(n).hn(5) !NO3 cauce
    balanc_nitr(t,40)=balanc_nitr(t,40)+cell(n).hn(9) !NO sed. suspendidos cauce
    balanc_nitr(t,41)=balanc_nitr(t,41)+cell(n).hn(10) !NH4 sed. suspendidos cauce
    balanc_nitr(t,42)=balanc_nitr(t,42)+cell(n).hn(11) !NO sed. depositados cauce
    balanc_nitr(t,43)=balanc_nitr(t,43)+cell(n).hn(12) !NH4 sed. depositados cauce
    balanc_nitr(t,44)=balanc_nitr(t,44)+cell(n).hn(6) !NH4 disuelto acu�fero
    balanc_nitr(t,45)=balanc_nitr(t,45)+cell(n).hn(7) !NO3 disuelto acu�fero
    
    
End subroutine


    
    
 