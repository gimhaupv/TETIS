!*****************************************************************************
!*  MODELO DE SIMULACION DE EVENTOS DE CRECIDA. ++ TETIS ++
!*  
!*
!*****************************************************************************
Subroutine tetis
!USE DFLIB
!USE DFPORT
USE modtet
IMPLICIT NONE




CHARACTER itext*1
CHARACTER text*11


!integer massimocodveg
!LOGICAL(KIND=4)CHECKED
!
!!Lee nombres de los ficheros a utilizar 
!CALL lecfiles(dirtra,arch,sale)
!IF (sale.eq.2) GOTO 95
!
!CALL lee_settings
!IF (sale.eq.2) GOTO 95
!
!CALL DATE_AND_TIME(dia,hora)
!CALL write_date
!
!!Llama la subrutina que define los mensajes de error genéricos, los que no neceistan más información que los nombres de archivo
!CALL labels
!
!artem=TRIM(ADJUSTL(dirtra))//TRIM(ADJUSTL('~errores.txt'))  
!OPEN(22,file=artem)                                        
!WRITE(22,'(A54)')strings(802)
!CLOSE(22) 
!
!sim2=.true.
!balancsalrio=0.0
!
!If (config(5)) CALL leearchveg(dirtra,archveg,sale)
!IF (sale.eq.2) GOTO 95
!
!!Crea la carpeta para poner los mapas ASCII
!IF (printascii) THEN
!  deldirectory='ECHO S | del '//TRIM(ADJUSTL(dirtra))//'_ASCII\*.*'
!  res=SYSTEM(deldirectory)
!  makedirectory = 'mkdir ' // TRIM(ADJUSTL(dirtra)) // '\_ASCII'
!  res=SYSTEM(makedirectory)
!  CALL lee_printvariables
!  If (modulos2(3)) then
!      call lee_printvariablesnitr
!  End if
!!ELSE
!!  res=DELDIRQQ(TRIM(ADJUSTL(dirtra))//'\_ASCII')
!ENDIF
!
!!GUIOMAR(22/10/2015):para obtener mapas LAI
!!IF (config(5)) THEN
!  ! deldirectory='ECHO S | del '//TRIM(ADJUSTL(dirtra))//'_ASCII\*.*'
!  !res=SYSTEM(deldirectory)
!  !makedirectory = 'mkdir ' // TRIM(ADJUSTL(dirtra)) // '\_ASCII'
!  !res=SYSTEM(makedirectory)
!  !CALL lee_printvariables
!!ELSE
! ! res=DELDIRQQ(TRIM(ADJUSTL(dirtra))//'\_ASCII')
!!ENDIF
!!Nuevo
!IF(simevents) THEN
!    simevents=.false.  !esto tiene que ser verdadero solo si se quiere hacer una calibración automática multi-evento porque vuelve a leer los hantec para cada uno de ellos
!    artem=arch(34)
!    CALL escri_settings
!END IF 
!
!!Lee parametros geomorfologicos
!CALL lee_pargeo
!IF (sale.eq.2) GOTO 95
!
!!Lee nombre de los ficheros con sedimentos
!IF (config(4)) THEN
!  CALL leearchsed(dirtra,archsed,sale)
!  IF (sale.eq.2) GOTO 95
!ENDIF
!
!!Lee nombre de los ficheros de nitrógeno
!IF (modulos2(3)) THEN
!  CALL leearchnit(dirtra,archnit,sale)
!  IF (sale.eq.2) GOTO 95
!ENDIF
!
!!Llama la subrutina que define los mensajes de error genéricos, los que no neceistan más información que los nombres de archivo
!
!CALL labels
!
!!Cuenta el número de puntos de control
!ncon=0
!OPEN(9,file=arch(21),status='old',err=206)
!ios=0
!DO WHILE (ios.ne.-1)
!  READ(9,*,iostat=ios) j
!  IF (ios.ne.-1) ncon=ncon+1
!ENDDO
!REWIND(9)
!ALLOCATE (control(ncon))
!
!
!CALL lee_pcon
!IF (sale.eq.2) GOTO 95
!
!!Lee fichero con factores de cultivo mensuales para el calculo de la ETP
!!Guiomar (28/01/2014): He introducido un cambio. Tal y como estaba antes lo que se hacía era comprobar si existía el fichero factorETmes.txt y en caso de no existir se generaba uno por defecto con todos los lamb igual a 1 e Imax igual a cero.
!!Este procedimiento tenía un problema si se activaba la opción de vegetación dinámica.
!!En este caso, factorETmes no guarda los lambdas sino otros parámetros que no se pueden generar por defecto. Necesariamente tiene que ser introducidos por el usuario.
!IF (config(5)) THEN
!    INQUIRE (FILE=arch(41), EXIST=existe)
!    IF (.NOT.existe) THEN
!        mensaje=strings(312)
!        errr=1
!        CALL errores(errr,mensaje,lang)
!        goto 95
!    ENDIF
!ELSE
!    INQUIRE (FILE=arch(6),EXIST=existe)!chiara
!    IF (.NOT.existe) THEN
!        IF (ALLOCATED(lamb)) DEALLOCATE(lamb)
!        ALLOCATE(lamb(1,13))
!        OPEN(28,file=arch(6),status='new')
!        WRITE(28,*)'*'
!        WRITE(28,*)'*'
!        lamb(1,1)=1.
!        lamb(1,2)=1.
!        lamb(1,3)=1.
!        lamb(1,4)=1.
!        lamb(1,5)=1.
!        lamb(1,6)=1.
!        lamb(1,7)=1.
!        lamb(1,8)=1.
!        lamb(1,9)=1.
!        lamb(1,10)=1.
!        lamb(1,11)=1.
!        lamb(1,12)=1.
!        lamb(1,13)=0.
!        WRITE(28,980)(lamb(1,j), j=1,13)
!        CLOSE(28)
!        mensaje=strings(63)
!        errr=2
!        CALL errores(errr,mensaje,lang)
!    ENDIF 
!ENDIF
!980 FORMAT (<13>F12.5)
!    
!If (config(5)) then  !Si vegetación dinámica está activada, no se lee FactorETmes.txt y se lee Calibveg.txt
!    Call leecalibveg
!Else
!    CALL leefactoret
!end if
!
!!Lee fichero con Riego.txt, es un fichero con: 1) cantidad de riego en mm para cada mes (son 12)
!! y 2) la segunda fila corresponde a la frecuencia de riego en dias (2 filas por cada zona de riego)
!xriego=0.0
!friego=0.0
!k=0
!erie=0
!OPEN(10,file=arch(22),status='old',err=48)
!ios=0
!DO WHILE (ios.ne.-1)
!  READ(10,*,iostat=ios) j
!  READ(10,*,iostat=ios) j
!  IF (ios.ne.-1) k=k+1
!ENDDO
!REWIND(10)
!erie=k
!DO i=1,erie
!  READ(10,*,err=249)(xriego(i,j),j=1,12)  
!  READ(10,*,err=249)(friego(i,j),j=1,12)  
!ENDDO
!48 CLOSE(10)
!!Lee el tipo de riego de cada zona (1 Gravedad, 2 Aspersión, 3 Goteo)  Cris 03/2017 (Falta ponerlo todo bien, errores, archivo en filessp...)
!ALLOCATE(tiporiego(erie))
!tiporiego=0
!OPEN(10,file=arch(38),status='old',err=49)
!Do i=1,erie
!    Read(10,*,err=250) tiporiego(i)
!End do
!49 Close(10)
!
!!Lee topologia, propiedades del suelo y tipologia
!OPEN(14,file=arch(3),status='old',err=209)
!READ (14,*,err=210)tit(1),cn,cs
!READ (14,*,err=210)tit(2),ce,cw
!READ (14,*,err=210)tit(3),mi
!READ (14,*,err=210)tit(4),mj
!READ (14,*,err=210)tit(5),ncol
!READ (14,*,err=210)tit(6),nfil
!READ (14,*,err=210)tit(7),ncel
!
!ALLOCATE(cell(ncel+1),banriego(erie),contriego(erie))
!
!CALL lee_topol
!IF (sale.eq.2) GOTO 95
!
!!Calcular ancho y área de celdas
!dx=(ce-cw)/mi				!ancho en metros
!dy=(cn-cs)/mj				!largo en metros
!arcel=dx*dy					!area en m2
!arcelkm=arcel/1000000.0		!area en km2
!
!IF (modulos2(2)) THEN
!    !Calcula los puntos de manantiales
!    INQUIRE (FILE=arch(37),EXIST=existe)
!    IF (existe)THEN
!        OPEN(15,file=arch(37),status='old',err=212)
!        ios=0
!        nman=0
!        DO WHILE (ios.ne.-1)
!            READ(15,*,iostat=ios) j
!            IF (ios.ne.-1) nman=nman+1
!        ENDDO
!        artem=arch(36)
!        INQUIRE (FILE=artem,EXIST=existe)
!        IF(.not.existe) THEN
!            mensaje=strings(113)
!            errr=1
!            CALL errores(errr,mensaje,lang)
!        ENDIF
!    
!        REWIND(15)
!        IF (ALLOCATED(manantial)) DEALLOCATE(manantial)
!        ALLOCATE (manantial(nman))
!        DO i=1,nman
!            READ (15,*,err=213) manantial(i).name,manantial(i).utmx,manantial(i).utmy,manantial(i).coef
!	        manantial(i).fila=INT((manantial(i).utmx-cw+(dx/2.0))/dx)
!	        manantial(i).columna=INT((cn-manantial(i).utmy+(dy/2.0))/dy)
!	    ENDDO
!	    CLOSE(15)
!    ENDIF
!ENDIF
!    
!
!!Lee fichero con caracteristicas de los azudes (para SEDIMENTOS)
!IF (config(4)) THEN
!  IF (trapeff) CALL leeTE
!ENDIF
!
!!Lee ficheros del evento (lo hace en dos partes)
!CALL lee_evto1
!IF (sale.eq.2) GOTO 95
!
!dt=dtmin/60.   !horas
!dts=dt*3600.0   !segundos
!nest=MIN(nest,kppt)
!nnest=MIN(nest,kniv)
!tnest=MIN(nest,ktem)
!enest=MIN(nest,kevp)
!!GUIOMAR (21/07/2015): Preparando cambios para poder interpolar la radiacion
!rnest=MIN(nest,nradiacion)
!nb=MAX(kppt,kevp,ktem,kniv,nradiacion)
!
!ALLOCATE(pluvio(kppt),band_p(kppt),disn(nb),preac(nt,2))
!ALLOCATE(arsecnew(ncel))
!arsecnew=0.0
!IF (kniv.gt.0) ALLOCATE(nieve(kniv))
!IF (knaf.gt.0) ALLOCATE(otros(knaf))
!IF (naf.gt.0)  ALLOCATE(aforo(naf))
!
!IF (config(4)) THEN
!  IF (ksedq.gt.0) THEN
!    ALLOCATE(aforosed(ksedq))
!  ENDIF
!  IF (kadised1.gt.0) THEN  ! Añadido Cris
!    ALLOCATE(aportsed1(kadised1))
!  ENDIF
!  IF (kadised2.gt.0) THEN
!    ALLOCATE(aportsed2(kadised2))
!  ENDIF
!  IF (kadised3.gt.0) THEN
!    ALLOCATE(aportsed3(kadised3))
!  ENDIF
!ENDIF
!
!If (modulos2(3)) then
!    If (kno.gt.0) then
!        Allocate(norg(kno))
!    End if
!    If (kam.gt.0) then
!        Allocate(amonio(kam))
!    End if    
!    If (kni.gt.0) then
!        Allocate(nitrato(kni))
!    End if
!    If (naf.gt.0) then
!        Allocate(norgql(naf),amonioql(naf),nitratoql(naf),norgqs(naf),amonioqs(naf))
!    End if
!End if
!
!!Guiomar(30/10/2014): tengo que 'alocatar' las estaciones relacionadas con vegetación
!IF (config(5)) THEN
!    IF (nveg.gt.0) THEN
!        ALLOCATE(veg(nveg))
!        !(Guiomar-Vicente) alocato nuevas variables de estado
!        ALLOCATE(veg1_point(nveg))
!        ALLOCATE(veg2_point(nveg))
!        ALLOCATE(tr_point(nveg))
!    ENDIF
!    IF (nradiacion.gt.0) THEN
!        ALLOCATE(radiacion(nradiacion))
!    ENDIF
!ENDIF
!
!!Lee fichero con caracteristicas de los azudes (para SEDIMENTOS)
!IF (config(4)) THEN
!  IF (trapeff) THEN
!    CALL leeTE
!    CALL lee_emb1TE
!    DO i=1,numpresas
!      k=dam(i).datos
!      ALLOCATE(dam(i).h(0:k),dam(i).vol(0:k))
!    ENDDO
!    CALL lee_emb2TE
!    CALL leeTEini
!  ENDIF
!ENDIF
!
!DO n=1,ncel
!  ALLOCATE(cell(n).ind_int_p(nest),cell(n).fac_int_p(nest))
!  cell(n).ind_int_p=0
!  cell(n).fac_int_p=0.0
!ENDDO
!IF(ALLOCATED(balanc)) DEALLOCATE(balanc)
!IF(ALLOCATED(balanc_sed)) DEALLOCATE(balanc_sed)
!IF(ALLOCATED(balanc_nitr)) DEALLOCATE(balanc_nitr)
!IF(ALLOCATED(balancqb)) DEALLOCATE(balancqb)
!IF(ALLOCATED(balancexp)) DEALLOCATE(balancexp)
!IF(ALLOCATED(estad)) DEALLOCATE(estad)
!IF(ALLOCATED(RSRindex)) DEALLOCATE(RSRindex)
!IF(ALLOCATED(estadsed)) DEALLOCATE(estadsed)
!!GUIOMAR (15/10/2015): cambio el número de miembros de balanc incrementándolo a 27 para que almacene nuevas variables
!!Cris (03/2017): aumento a 30 para riego y pérdidas en diferentes acuíferos
!ALLOCATE(balanc(nt,28),balancqb(nt),balancexp(nt),estad(naf*2+knaf+kadi,22),estadsed(ksedq*2,12),balanc_sed(nt,30),RSRindex(naf*2+knaf+kadi),balanc_nitr(nt,60))
!balanc=0.0
!balanc_sed=0.0
!estad=0.0
!disn=0.0
!preac=0.0
!balancqb=0.0  
!contcauce=0
!RSRindex=0.0
!balancexp=0.0
!balanc_nitr=0.0
!IF (kevp.gt.0) THEN
!  ALLOCATE(evapo(kevp),band_e(kevp))
!  DO n=1,ncel
!    ALLOCATE(cell(n).ind_int_e(enest),cell(n).fac_int_e(enest))
!	cell(n).ind_int_e=0
!	cell(n).fac_int_e=0.0
!  ENDDO
!ENDIF
!IF (ktem.gt.0) THEN
!  ALLOCATE(band_t(ktem),temper(ktem),acuniv(nt))
!  DO n=1,ncel
!    ALLOCATE(cell(n).ind_int_t(tnest),cell(n).fac_int_t(tnest))
!	cell(n).ind_int_t=0
!	cell(n).fac_int_t=0.0
!  ENDDO
!ENDIF
!!Guiomar (21/07/2015): Preparando para poder interpolar la radiacion
!IF (nradiacion.gt.0) THEN
!  ALLOCATE(band_r(nradiacion))
!  DO n=1,ncel
!    ALLOCATE(cell(n).ind_int_r(rnest),cell(n).fac_int_r(rnest))
!	cell(n).ind_int_r=0
!	cell(n).fac_int_r=0.0
!  ENDDO
!ENDIF
!nb=nemb+vnemb+knemb
!IF (nb.gt.0) THEN 
!  ALLOCATE(nivel(nb),pulm(nb),emb(nb))
!  pulm=0
!  ALLOCATE(volum(nb))
!  ALLOCATE(qemb(nb))
!ENDIF
!
!IF (kadi.gt.0) THEN
!  ALLOCATE(aport(kadi))
!ENDIF
!
!IF (config(2)) THEN    !Falta activar el cambio en el nombre del episodio
!  CALL lee_evto2col
!ELSE
!  CALL lee_evto2
!ENDIF
!IF (sale.eq.2) GOTO 95
!
!CALL lee_fechainicio !para liberar el nombre del fichero de entrada
!    IF (sale.eq.2) GOTO 95
!  
!!Define la fecha inicial del nombre del evento
!fecin=archin(1:2)//'/'//archin(3:4)//'/'//archin(11:14)
!read(archin(11:14),*)nyear
!horin=archin(5:6)//':'//archin(7:8)//':00'
!!mes y día inicial en formato real
!aa1=archin(3:4)
!READ(aa1,*)mesin
!aa1=archin(1:2)
!READ(aa1,*)diain
!aa1=archin(5:6)
!READ(aa1,*)horain
!aa1=archin(7:8)
!READ(aa1,*)minin
!minInDay = horain*60 + minin
!nhora = INT(horain)
!nmonth = INT(mesin)
!nday= INT(diain)
!nmin = minin
!
!!Cuenta el numero de celdas a regar por cada zona de riego
!banriego=0
!contriego=0
!DO i=1,erie
!  DO n=1,ncel
!    IF (cell(n).codrie.eq.i) banriego(i)=banriego(i)+1
!  ENDDO
!ENDDO
!
!!Lee parametros de calibracion (factores correctores)
!CALL lee_calib
!IF (sale.eq.2) GOTO 95
!
!!Copia respaldo de los ficheros CALIB y PARAMGEO para luego ser recuperados
!CALL  conserva_fic 
!
!! Escribe parametros de calibracion (factores correctores)
!artem=arch(2)
!CALL escri_calib
!
!! Escribe parametros geomorfologicos
!artem=arch(1)
!CALL escri_parg
!
!! Calcula el número de celdas para flujos superficiales
!! El umbral para la escorrentía directa es igual a cero
!!IF (ALLOCATED(nw)) DEALLOCATE(nw)
!!Allocate(nw(3,npar)) Cris(17/10/2016) Regiones homogéneas allocatable, ahora fijo a 50.
!DO n=1,ncel
!  ncp=cell(n).codpar
!  DO i=2,3
!	nw(i,ncp)=areaumbral(i,ncp)/arcelkm
!  ENDDO
!ENDDO
!
!
!IF (config(4)) THEN  !Lee condiciones iniciales de sedimentos
!  artem=archsed(7)
!  CALL lee_sedantec
!  IF (sale.eq.2) GOTO 95
!ENDIF
!
!!Lee humedad antecedente
!artem=arch(4)
!CALL lee_human
!IF (sale.eq.2) GOTO 95
!!Guiomar(27/03/2014): Desactivo estas líneas porque he incluido el cálculo del estado inicial en HANTEC
!!Lee condiciones iniciales de rveg y waterstress
!!IF (config(5)) THEN
!  !artem=archveg(4)
!  !CALL lee_ci_veg
!  !IF (sale.eq.2) GOTO 95
!!ENDIF
!! Asigna a cada celda los valores de los parametros de vegetación
!!Guiomar (27/01/2014): voy a desactivar todas estas líneas porque ahora está programado de otra manera
!!IF (config(5)) THEN
!  !DO n=1,ncel
!    !cell(n).imx=lamb(cell(n).codveg,13)
!    !cell(n).kleaf1=lamb(cell(n).codveg,14)
!    !cell(n).kleaf2=lamb(cell(n).codveg,15)
!    !cell(n).kleaf3=lamb(cell(n).codveg,16)
!    !cell(n).kleaf4=lamb(cell(n).codveg,17)
!    !cell(n).kleaf5=lamb(cell(n).codveg,18)
!    !cell(n).tmx=lamb(cell(n).codveg,19)
!    !cell(n).alfa=lamb(cell(n).codveg,20)
!    !cell(n).cveg=lamb(cell(n).codveg,21)
!    !cell(n).qveg=lamb(cell(n).codveg,22)
!  !ENDDO
!!ELSE
!
!!Lee todos los ficheros relativos al submodelo de nitrógeno Cris (03/2016)
!!No cambiar de sitio porque lee_Ninputsuelo necesita saber el valor de nt para comprobar que tiene la misma longitud que el fichero de entrada.
!!Para calcular la adsorción/desorción es necesario haber leído hantec
!If (modulos2(3)) then
!    call lee_fcubiertan
!    call lee_calibnit
!    call lee_Ninputsuelo
!    Do i=1,ncel
!        cell(n).fn=0.0
!    end do
!    !Se ceran las variables de cultivos en caso de estar activado para que las celdas que no sean cultivos tomen valor cero y se hace aquí porque tiene 
!    !que ser antes de que lea el estado inicial, que ya tiene las variables nw y fcncult
!    If (modulos2(4)) then
!        Do i=1,ncel
!            cell(n).w=0.0
!            cell(n).fcncult=0.0
!            cell(n).nw=0.0
!            cell(n).Ncrit=0.0
!            cell(n).Nextr=0.0
!            cell(n).restcosecha=0.0
!        End do
!        IF (ALLOCATED(crecimiento)) DEALLOCATE(crecimiento)
!        ALLOCATE (crecimiento(ncel))
!        crecimiento=0
!    End if
!    artem=archnit(13)
!    call lee_nantec
!    If (sale==2) Go to 94
!
!    Do n=1,ncel
!        !Proceso de adsorción/desorción de NH4 en suelo, para completar la columna de hn(8)
!        !Si se genera nantec por usos del suelo, se da una concentración en suelo de NH4, que hay que distribuir en disuelto/particulado según kd
!        !Se hace aquí para el inicial, para no tener que generar nantec cada vez que se cambian los parámetros
!        !Luego se hace al final de cada paso de tiempo desde sim_celdanitr, si se utilizase un nantec2 como inicial, lo haría otra vez aquí pero daría el mismo resultado. Así que no hay problema.
!        NH4total=cell(n).hn(1)+cell(n).hn(8)
!        if((cell(n).h(1)+cell(n).h(3)+cell(n).hlim)>0.000005) then
!            cell(n).hn(1)=((cell(n).h(1)+cell(n).h(3)+cell(n).hlim)/1000*NH4total)/((cell(n).h(1)+cell(n).h(3)+cell(n).hlim)/1000+cell(n).kd*fckd*cell(n).psuelo*r(1)*cell(n).daparente)
!            Cell(n).hn(8)=NH4total-cell(n).hn(1)
!            If(cell(n).hn(8)<0.0) then
!                cell(n).hn(1)=NH4total
!                cell(n).hn(8)=0.0
!            End if
!            If (cell(n).hn(8)<0.0) then
!                write(*,*)NH4total,cell(n).hn(1),n,'hn8_tetis'
!                pause
!            End if
!            If(cell(n).kd<0.0001.or.cell(n).psuelo*r(1)<0.0001.or.cell(n).daparente<0.0001) then
!                cell(n).hn(1)=cell(n).hn(1)
!                cell(n).hn(8)=cell(n).hn(8)
!            End if
!            If(cell(n).hn(1)<0.0) then
!                write(*,*)(cell(n).h(1)+cell(n).h(3)+cell(n).hlim)/1000*NH4total,((cell(n).h(1)+cell(n).h(3)+cell(n).hlim)/1000+cell(n).kd*fckd*cell(n).psuelo*r(1)*cell(n).daparente),n,'fn1_n11'
!                write(*,*)cell(n).h(1),cell(n).h(3),cell(n).hlim,cell(n).kd,fckd,cell(n).psuelo*r(1),cell(n).daparente
!                pause
!            End if
!        else
!            cell(n).hn(1)=0.0
!            cell(n).hn(8)=NH4total
!        end if
!    End do
!    If (modulos2(4)) then !Lee los archivos relacionados con nitrógeno-cultivos
!        call lee_codcult
!        call lee_carcult
!        call lee_factvegcultivos
!        call lee_Ninputsuelo_Cultivos
!    End if
!Else if (modulos2(4)) then !Si cultivos está activado pero no nitrógeno, lee sólo el fichero factvegcultivos que es lo único que hace falta
!    call lee_factvegcultivos
!End if
!If (sale==2) goto 95
!
!!Lee fichero con factores de cultivo mensuales para el calculo de la ETP
!!Guiomar (28/01/2014). Explicación sobre lo ya programado: Abre el archivo factorETmes, cuenta sus líneas a partir de las dos primeras y después comprueba que el número de líneas coincida con el número de coberturas
!If (config(5)) then
!    k=0
!    OPEN(10,file=arch(41),status='old',err=257)
!    ios=0
!    DO WHILE (ios.ne.-1)
!      READ(10,*,iostat=ios) j
!      IF (ios.ne.-1) k=k+1
!    ENDDO
!    CLOSE(10)
!
!    massimocodveg=maxval(cell(:).codveg)
!    IF(k.lt.massimocodveg)then
!        mensaje=strings(312)
!        errr=1
!        CALL errores(errr,mensaje,lang)
!        goto 94
!    ENDIF
!Else
!    k=0
!    OPEN(10,file=arch(6),status='old',err=248)
!    READ(10,*)
!    READ(10,*)
!    ios=0
!    DO WHILE (ios.ne.-1)
!      READ(10,*,iostat=ios) j
!      IF (ios.ne.-1) k=k+1
!    ENDDO
!    CLOSE(10)
!
!    massimocodveg=maxval(cell(:).codveg)
!    IF(k.lt.massimocodveg)then
!        mensaje=strings(64)
!        errr=1
!        CALL errores(errr,mensaje,lang)
!        goto 94
!    ENDIF
!End if
!!Cris (03/2017). Hace lo mismo, pero en este caso para el fichero de factor de cubierta del submodelo de nitrógeno, los parámetros y el fichero ninputsuelo
!If (modulos2(3)) then
!    !Factor de cubierta
!    k=0 
!    OPEN(10,file=archnit(9),status='old',err=252)
!    READ(10,*)
!    READ(10,*)
!    ios=0
!    DO WHILE (ios.ne.-1)
!      READ(10,*,iostat=ios) j
!      IF (ios.ne.-1) k=k+1
!    ENDDO
!    CLOSE(10)
!    IF(k.lt.massimocodveg)then
!        mensaje=strings(402)
!        errr=1
!        CALL errores(errr,mensaje,lang)
!        goto 94
!    ENDIF
!    !Parámetros 
!    k=0
!    OPEN(10,file=archnit(8),status='old',err=253)
!    ios=0
!    DO WHILE (ios.ne.-1)
!      READ(10,*,iostat=ios) j
!      IF (ios.ne.-1) k=k+1
!    ENDDO
!    CLOSE(10)
!    IF((k-2).lt.massimocodveg)then !Se compara k-2 porque hay unos específicos de cauce y los de temperatura y humedad. Hay usos del suelo + 2filas
!        mensaje=strings(405)
!        errr=1
!        CALL errores(errr,mensaje,lang)
!        goto 94
!    ENDIF
!    !Inputs de nitrógeno en suelo
!    !!k=0
!    !!OPEN(10,file=archnit(7),status='old',err=254)
!    !!ios=0
!    !!DO WHILE (ios.ne.-1)
!    !!  READ(10,*,iostat=ios) j
!    !!  IF (ios.ne.-1) k=k+1
!    !!ENDDO
!    !!CLOSE(10)
!    !!IF((k).lt.massimocodveg)then
!    !!    mensaje=strings(414)
!    !!    errr=1
!    !!    CALL errores(errr,mensaje,lang)
!    !!    goto 94
!    !!ENDIF
!    k=0
!    OPEN(10,file=archnit(10),status='old',err=255)
!    ios=0
!    DO WHILE (ios.ne.-1)
!      READ(10,*,iostat=ios) j
!      IF (ios.ne.-1) k=k+1
!    ENDDO
!    CLOSE(10)
!    IF((k).lt.massimocodveg)then
!        mensaje=strings(430)
!        errr=1
!        CALL errores(errr,mensaje,lang)
!        goto 94
!    ENDIF
!        k=0
!    OPEN(10,file=archnit(11),status='old',err=256)
!    ios=0
!    DO WHILE (ios.ne.-1)
!      READ(10,*,iostat=ios) j
!      IF (ios.ne.-1) k=k+1
!    ENDDO
!    CLOSE(10)
!    IF((k).lt.massimocodveg)then
!        mensaje=strings(431)
!        errr=1
!        CALL errores(errr,mensaje,lang)
!        goto 94
!    ENDIF
!End if
!!Guiomar (28/01/2014): el imax es la 13 columna del factorETmes sólo en el caso de no tener activada la vegetación dinámica. Por eso, he añadido un if
!IF (.NOT.(config(5))) THEN
!  DO n=1,ncel
!    cell(n).imx=lamb(cell(n).codveg,13)
!  ENDDO
!END IF
!!Calcula la humedad antecedente de la cuenca para cerrar el balance
!DO i=0,8
!  almini(i)=0.0
!ENDDO
!DO n=1,ncel
!  DO i=0,4
!    almini(i)=almini(i)+cell(n).h(i)
!  ENDDO
!  almini(6)=almini(6)+cell(n).h(6)
!  IF (nw(2,ncp).gt.cell(n).acum) cell(n).h(5)=0.0    !celda con ladera, no hay volumen inicial en cauces
!  almini(5)=almini(5)+cell(n).h(5)*1000.0/arcel      !mm
!  !Guiomar (28/01/2013): voy a añadir un if config(5) para que calcule el almacenamiento del tanque añadido en el modelo
!  IF (config(5)) THEN
!      almini(8)=almini(8)+cell(n).h(8)
!  ENDIF
!ENDDO
!!Calcula la humedad antecedente media
!DO i=0,4
!  almini(i)=almini(i)/ncel
!ENDDO
!almini(6)=almini(6)/ncel
!!Guiomar (28/01/2013): voy a añadir un if config(5) para que calcule la humedad antecedente media del tanque añadido
!IF (config(5)) THEN
!    almini(8)=almini(8)/ncel
!ENDIF
!
!!Lee curvas de embalse
!nb=nemb+vnemb+knemb
!IF (nb.gt.0) THEN
!  DO i=1,nb
!    emb(i).caso=0
!  ENDDO
!  CALL cal_caso
!
!  CALL lee_emb1
!  IF (sale.eq.2) GOTO 95
!
!  DO i=1,nb
!	IF (emb(i).caso.gt.0) THEN
!      k=emb(i).datos
!      ALLOCATE(emb(i).h(0:k),emb(i).vol(0:k),emb(i).sup(0:k),emb(i).out(0:k,2))
!      ALLOCATE(emb(i).fpul(0:k))
!	ENDIF
!  ENDDO
!
!  CALL lee_emb2
!  IF (sale.eq.2) GOTO 95
!ENDIF
!
!!Distancia entre celdas
!CALL dis_cel
!
!!Calcula puntos importantes
!CALL ptos_imp
!
!!Inicia variables de estado
!CALL inicia_var
!
!!Inicia variables de estado de sedimentos y realiza interpolaciones
!IF (config(4)) CALL inicia_varsed
!sale=0

CALL iniciaTetis(1,sale) !1, porque se llama desde TETIS
if(sale.eq.2)go to 95


!write(*,*)'Fin de la lectura de ficheros. Se incia la simulación'
CALL sim_tpo
if(sale.eq.2)go to 95

!(Vicente) Se calcula fecha final
WRITE(fecfin,'(I2.2,A1,I2.2,A1,I4)')nday,'/',nmonth,'/',nyear
WRITE(horfin,'(I2.2,A1,I2.2,A3)')nhora,':',nmin,':00'

!t1=nt-1
!CALL tposal(fecin,horin,nt,dtmin,fecfin,horfin)

IF (config(4)) THEN
  !Escribe el fichero de sedimentos final
  artem=archsed(8)	!Contiene condiciones finales de sedimentos
  CALL escribe_sedantec

  !Calcula al final de la simulación los volúmenes de sedimentos totales
  CALL sedvolumenes
ENDIF

IF (modulos2(3)) THEN
  !Escribe el fichero de nitrógeno final
  artem=archnit(14)
  CALL escribe_nantec
ENDIF
!Escribe el fichero de humedad final
DO n=1,ncel
  IF (nw(2,ncp).gt.cell(n).acum) THEN  !para celdas de ladera (sin canal)...volumen
    cell(n).h(5)=0.0
  ENDIF
  cell(n).h(5)=cell(n).h(5)*1000.0/arcel  !Pasa de m³ a mm
ENDDO
artem=arch(8)
CALL escr_wdad

IF (kniv.ne.0.AND.ktem.ne.0) THEN
  IF (ALLOCATED(masc)) DEALLOCATE(masc)
  ALLOCATE(masc(mi,mj))
  masc=0
  DO n=1,ncel
    masc(cell(n).fil,cell(n).col)=INT(cell(n).h(0))
  ENDDO
  !Escribe mapa con estado final de la nieve en formato i,j
  OPEN(17,file=arch(11))
  WRITE(17,*)'ncols',mi
  WRITE(17,*)'nrows',mj
  WRITE(17,*)'xllcorner  ',cw
  WRITE(17,*)'yllcorner  ',cs
  WRITE(17,*)'cellsize   ',dx
  WRITE(17,*)'NODATA_value  -9999'
  DO j=1,mj
    WRITE(17,232)(masc(i,j),i=1,mi)
  ENDDO
  232 FORMAT(<mi>(F10.4,1x))
  CLOSE(17)

ENDIF

CALL forma_cedex

IF (config(3)) THEN
  CALL escribe_col
  !Call Qmax_Anual !Cris (06/2017) obtiene los caudales máximos anuales
  IF (config(4)) CALL escribesed_col    
ENDIF

If (modulos2(3)) then !Cris (03/2017) Por ahora sólo sale en columna
    call escribenitr_col
End if
GOTO 95


94 WRITE(*,*)strings(800)
95 WRITE(*,*)strings(801)
CALL DATE_AND_TIME(dia,hora)
CALL write_date

!Libera la memoria dinamica
CALL libera_mem

END SUBROUTINE tetis

    
    
! ***************************************************************
! * Subrutina referente al principio de la subrutina Tetis y de los programas de calibracion 
! * 
! *  Se emplea la variable tipoParent, para determinar el origen que la llamada
! ***************************************************************
Subroutine iniciaTetis(tipoParent,sale_)
!USE DFLIB
USE IFPORT
USE modtet
IMPLICIT NONE

integer sale_,tipoParent
LOGICAL(KIND=4)CHECKED
integer massimocodveg
INTEGER nx
CHARACTER*50 ffich1
CHARACTER*50 ffich2
!character makedirectory*256
character deldirectory*356
!character deldirectory*356
!***
! tipoParent: determinar el origen de la llamada
! 1 : tetis
! 2 : calibautom
! 3 : calibautomsed
! 4 : calibautomveg
! 5 : calibautomnitr
! 6 : calibQmax
! 7 : calibautoMulti


sale_=0
!Se hace así para que si hay error, lang tenga un valor y pueda escribir. Por defecto está en inglés
lang=2
!llama la subrutina que define los mensajes de error genéricos, los que no neceistan más información que los nombres de archivo
CALL labels

!Lee nombres de los ficheros a utilizar, por el momento no general el el FILESSP!!!!! filessp tiene que estár!
CALL lecfiles(dirtra,arch,sale)
IF (sale.eq.2) GOTO 94

CALL lee_settings
IF (sale.eq.2) GOTO 94
IF (tipoParent.ne.1) printascii = .FALSE. !Solo se anula PRINTASCII cuando es una calibracion


CALL DATE_AND_TIME(dia,hora)
CALL write_date

CALL labels

!res=DELFILESQQ(TRIM(ADJUSTL(dirtra))//TRIM(ADJUSTL('errores.txt')))
!Subtituimos la rutina de eliminar fichero para no emplear librerias(Vicente.Nov-2019)
CALL deletefich(TRIM(ADJUSTL(dirtra))//TRIM(ADJUSTL('errores.txt')))
artem=TRIM(ADJUSTL(dirtra))//TRIM(ADJUSTL('~errores.txt'))  
OPEN(22,file=artem)                                        
WRITE(22,'(A54)')strings(802)
CLOSE(22) 

IF (tipoParent.eq.1) THEN !Solo entra si es TETIS
	sim2=.true.
	balancsalrio=0.0
ELSE
	sim2=.false.	
END IF	

CALL lee_pargeo
IF (sale.eq.2) GOTO 95

!Lee nombre de los ficheros con sedimentos
IF (config(4)) THEN
  CALL leearchsed(dirtra,archsed,sale)
  IF (sale.eq.2) GOTO 95
ENDIF

!Lee nombre de los ficheros de nitrógeno
IF (modulos2(3)) THEN
  CALL leearchnit(dirtra,archnit,sale)
  IF (sale.eq.2) GOTO 95
ENDIF

If (config(5)) THEN
    CALL leearchveg(dirtra,archveg,sale)
    IF (sale.eq.2) GOTO 95
ENDIF    

IF(tipoParent.eq.2) THEN !Solo para Calibracion Automatica SCEUA
	IF (simevents)THEN

		!pregunta si existe el fichero con los nombres de todos los eventos y respectivos hantec
		artem=TRIM(ADJUSTL(dirtra))//'MultiCalib.txt'  !nobre obligatorio de fichero de entrada!!!
		INQUIRE (FILE=artem,EXIST=existe)
		IF (.NOT.existe) THEN
			mensaje=strings(114)
			errr=1
			CALL errores
			CALL DATE_AND_TIME(dia,hora)
			CALL write_date
			GOTO 94
		ENDIF

		OPEN (39,file=artem) !abre el fichero multiple.txt
		ios=0
		nexe=0
		DO WHILE (ios.ne.-1)
			READ(39,*,iostat=ios) 
			IF (ios.ne.-1) nexe=nexe+1  !cuenta el número de eventos que se quieren calibrar automaticamente a la vez
		ENDDO
		REWIND(39)

		IF(ALLOCATED(estadohum))DEALLOCATE(estadohum)
		IF(ALLOCATED(multievento))DEALLOCATE(multievento)
		IF(ALLOCATED(nstep))DEALLOCATE(nstep)
		IF(ALLOCATED(datainiz))DEALLOCATE(datainiz)
		IF(ALLOCATED(orainiz))DEALLOCATE(orainiz)
		ALLOCATE(estadohum(nexe),multievento(nexe),nstep(nexe),datainiz(nexe),orainiz(nexe))
		
		DO nx=1,nexe
			READ(39,*)ffich1,ffich2
			estadohum(nx)=TRIM(ADJUSTL(dirtra))//TRIM(ADJUSTL(ffich1))     !Hantec.sds
			multievento(nx)=TRIM(ADJUSTL(dirtra))//TRIM(ADJUSTL(ffich2))   !epsiodio
		END DO
		close(39)
		
		
		if(config(2))then 
			call megaevent_col
		else
			call megaevent_cedex
		endif
		
		
		arch(5)=TRIM(ADJUSTL(dirtra))//'MultiEvento.txt'
	ENDIF       
	!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!Fine!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
ELSE
	simevents=.false.
END IF

IF (printascii) THEN
  !Agrupamos en una sentencia el vaciado del directorio, y su creación en caso de no existir
  if(stma_op==0) then  
      !deldirectory='rm -rf '//TRIM(ADJUSTL(dirtra))//'_ASCII/*' !Linux
      deldirectory = '[ -d ' // TRIM(ADJUSTL(dirtra))//'_ASCII/ ] && rm -rf '//TRIM(ADJUSTL(dirtra))//'_ASCII/* || mkdir ' // TRIM(ADJUSTL(dirtra)) // '_ASCII' !Linux            
  else    
      !deldirectory='ECHO S | del '//TRIM(ADJUSTL(dirtra))//'_ASCII\*.*' !Windows
      deldirectory='IF EXIST '//TRIM(ADJUSTL(dirtra))//'_ASCII/ ( ECHO S | del '//TRIM(ADJUSTL(dirtra))//'_ASCII\*.* ) ELSE ( mkdir '//TRIM(ADJUSTL(dirtra))//'_ASCII )'      
  end if  
  res=SYSTEM(deldirectory)
  !makedirectory = 'mkdir ' // TRIM(ADJUSTL(dirtra)) // '_ASCII'
  !res=SYSTEM(makedirectory)
  !deldirectory='IF EXIST '//TRIM(ADJUSTL(dirtra))//'_ASCII/ ( ECHO S | del '//TRIM(ADJUSTL(dirtra))//'_ASCII\*.* ) ELSE ( mkdir '//TRIM(ADJUSTL(dirtra))//'_ASCII )'      
  !res=SYSTEM(deldirectory)
  CALL lee_printvariables
  If (modulos2(3)) then
      call lee_printvariablesnitr
  End if
END IF

!Cuenta el número de puntos de control
ncon=0
OPEN(9,file=arch(21),status='old',err=206)
ios=0
DO WHILE (ios.ne.-1)
  READ(9,*,iostat=ios) j
  IF (ios.ne.-1) ncon=ncon+1
ENDDO
REWIND(9)
ALLOCATE (control(ncon))

CALL lee_pcon
IF (sale.eq.2) GOTO 95

!Lee fichero con factores de cultivo mensuales para el calculo de la ETP
IF(config(5)) THEN
    CALL leecalibveg
    IF (sale.eq.2) GOTO 95
ELSE
    INQUIRE (FILE=arch(6),EXIST=existe)
     IF (existe)THEN
       CALL leefactoret 
     ELSE
       mensaje=strings(63)
       errr=2
       CALL errores
       
       IF (ALLOCATED(lamb)) DEALLOCATE(lamb)
       ALLOCATE(lamb(1,13))
  
       OPEN(28,file=arch(6),status='new')
  
       WRITE(28,*)'*'
       WRITE(28,*)'*'
        
       lamb(1,1)=1.0
       lamb(1,2)=1.0
       lamb(1,3)=1.0
       lamb(1,4)=1.0
       lamb(1,5)=1.0
       lamb(1,6)=1.0
       lamb(1,7)=1.0
       lamb(1,8)=1.0
       lamb(1,9)=1.0
       lamb(1,10)=1.0
       lamb(1,11)=1.0
       lamb(1,12)=1.0
       lamb(1,13)=0.0
       WRITE(28,980)(lamb(1,j), j=1,13) 
       CLOSE(28)
     ENDIF 
    980 FORMAT (<13>F12.5)
END IF

!Lee fichero de regadio
!(Vicente)CAMBIAR para cuando se junte con riego

!Lee fichero con Riego.txt, es un fichero con: 1) cantidad de riego en mm para cada mes (son 12)
! y 2) la segunda fila corresponde a la frecuencia de riego en dias (2 filas por cada zona de riego)
IF(modulos(5)) THEN !Riego   
    k=0
    erie=0
    OPEN(10,file=arch(22),status='old',err=246)
    ios=0
    DO WHILE (ios.ne.-1)
      READ(10,*,iostat=ios) j
      READ(10,*,iostat=ios) j
      IF (ios.ne.-1) k=k+1
    ENDDO
    REWIND(10)
    erie=k
    ALLOCATE(xriego(erie,12),periodRiego(erie,12),tiporiego(erie),grupoOrdenRiego(erie),celdasRegadas(erie),banriego(erie),contRiegoInDay(erie))
    ALLOCATE(contriego(erie),isZonaConsiderada(erie),isZonaRegada(erie),contarPasoTiempoRegado(erie),pasoTiempoRegados(erie))
    xriego=0.0
    periodRiego=0.0
    DO i=1,erie
      READ(10,*,err=249)(xriego(i,j),j=1,12)  
      READ(10,*,err=249)(periodRiego(i,j),j=1,12)
    ENDDO
    grupoOrdenRiego = 1
    tiporiego=0
    celdasRegadas=0
    isZonaConsiderada=0
    isZonaRegada=0
    contarPasoTiempoRegado = 0
    pasoTiempoRegados = 0
    contRiegoInDay=0
    contriego=0
    Do i=1,n
        cell(n).Riegosmes=0
    End do    
    !Lee el tipo de riego de cada zona (1 Gravedad, 2 Aspersión, 3 Goteo)  Cris 03/2017
    OPEN(10,file=arch(38),status='old',err=247)
    Do i=1,erie
        Read(10,*,err=250) tiporiego(i)
    End do  

END IF
!Lee topologia, propiedades del suelo y tipologia
OPEN(14,file=arch(3),status='old',err=209)
READ (14,*,err=210)tit(1),cn,cs
READ (14,*,err=210)tit(2),ce,cw
READ (14,*,err=210)tit(3),mi
READ (14,*,err=210)tit(4),mj
READ (14,*,err=210)tit(5),ncol
READ (14,*,err=210)tit(6),nfil
READ (14,*,err=210)tit(7),ncel

IF (ALLOCATED(cell)) DEALLOCATE(cell)
ALLOCATE(cell(ncel+1))

CALL lee_topol
IF (sale.eq.2) GOTO 95

!Calcular ancho y área de celdas
dx=(ce-cw)/mi				!ancho en metros
dy=(cn-cs)/mj				!largo en metros
arcel=dx*dy					!area en m2
arcelkm=arcel/1000000.0		!area en km2

IF (modulos2(2)) THEN
    !Calcula los puntos de manantiales
    INQUIRE (FILE=arch(37),EXIST=existe)
    IF (existe)THEN
        OPEN(15,file=arch(37),status='old',err=212)
        ios=0
        nman=0
        DO WHILE (ios.ne.-1)
            READ(15,*,iostat=ios) j
            IF (ios.ne.-1) nman=nman+1
        ENDDO
        artem=arch(36)
        INQUIRE (FILE=artem,EXIST=existe)
        IF(.not.existe) THEN
            mensaje=strings(113)
            errr=1
            CALL errores
        ENDIF
    
        REWIND(15)
        IF (ALLOCATED(manantial)) DEALLOCATE(manantial)
        ALLOCATE (manantial(nman))
        DO i=1,nman
            READ (15,*,err=213) manantial(i).name,manantial(i).utmx,manantial(i).utmy,manantial(i).coef
	        manantial(i).fila=INT((manantial(i).utmx-cw+(dx/2.0))/dx)
	        manantial(i).columna=INT((cn-manantial(i).utmy+(dy/2.0))/dy)
	    ENDDO
	    CLOSE(15)
    ENDIF
ENDIF

!(Vicente)ESTO SE PODRIA CONDICIONAR AL TIPO DE CALIBRACION
!Lee fichero con caracteristicas de los azudes (para SEDIMENTOS)
IF (config(4)) THEN
  IF (trapeff) CALL leeTE
ENDIF	

!Lee ficheros del evento (lo hace en dos partes)
CALL lee_evto1
IF (sale.eq.2) GOTO 95

dt=dtmin/60.   !horas
dts=dt*3600.0   !segundos
nest=MIN(nest,kppt)
nnest=MIN(nest,kniv)
tnest=MIN(nest,ktem)
enest=MIN(nest,kevp)
!GUIOMAR (21/07/2015): Preparando cambios para poder interpolar la radiacion
rnest=MIN(nest,nradiacion)
nb=MAX(kppt,kevp,ktem,kniv,nradiacion)

ALLOCATE(pluvio(kppt),band_p(kppt),disn(nb),preac(nt,2))
ALLOCATE(arsecnew(ncel))
arsecnew=0.0
IF (kniv.gt.0) ALLOCATE(nieve(kniv))
IF (knaf.gt.0) ALLOCATE(otros(knaf))
IF (naf.gt.0)  ALLOCATE(aforo(naf))

IF (config(4)) THEN
  IF (ksedq.gt.0) THEN
    ALLOCATE(aforosed(ksedq))
  ENDIF
  IF (kadised1.gt.0) THEN  ! Añadido Cris
    ALLOCATE(aportsed1(kadised1))
  ENDIF
  IF (kadised2.gt.0) THEN
    ALLOCATE(aportsed2(kadised2))
  ENDIF
  IF (kadised3.gt.0) THEN
    ALLOCATE(aportsed3(kadised3))
  ENDIF
  IF(tipoParent.ne.1.and.tipoParent.ne.3.and.tipoParent.ne.5) THEN!Solo entra si no es TETIS ni CALIBSED ni CALIBNITR
	config(4)=.false. !/se añade para que no haga todo el balance de sedimentos 
  END IF
ENDIF

If (modulos2(3)) then
    If (kno.gt.0) then
        Allocate(norg(kno))
    End if
    If (kam.gt.0) then
        Allocate(amonio(kam))
    End if    
    If (kni.gt.0) then
        Allocate(nitrato(kni))
    End if
    If (naf.gt.0) then
        Allocate(norgql(naf),amonioql(naf),nitratoql(naf),norgqs(naf),amonioqs(naf))
    End if
	IF(tipoParent.ne.1.and.tipoParent.ne.5) THEN!Solo entra si no es TETIS ni CALIBNITR
		modulos2(3)=.false.
	END IF	
End if

!Guiomar(30/10/2014): tengo que 'alocatar' las estaciones relacionadas con vegetación
IF (config(5)) THEN
    IF (nveg.gt.0) THEN
        ALLOCATE(veg(nveg))
        !(Guiomar-Vicente) alocato nuevas variables de estado
        ALLOCATE(veg1_point(nveg))
        ALLOCATE(veg2_point(nveg))
        ALLOCATE(tr_point(nveg))
    ENDIF
    IF (nradiacion.gt.0) THEN
        ALLOCATE(radiacion(nradiacion))
    ENDIF
ENDIF

!Lee fichero con caracteristicas de los azudes (para SEDIMENTOS)
IF (config(4)) THEN
  IF (trapeff) THEN
    CALL leeTE
    CALL lee_emb1TE
    IF (sale.eq.2) GOTO 95
    DO i=1,numpresas
      k=dam(i).datos
      ALLOCATE(dam(i).h(0:k),dam(i).vol(0:k))
    ENDDO
    CALL lee_emb2TE
    IF (sale.eq.2) GOTO 95
    CALL leeTEini
  ENDIF
ENDIF

DO n=1,ncel
  ALLOCATE(cell(n).ind_int_p(nest),cell(n).fac_int_p(nest))
  cell(n).ind_int_p=0
  cell(n).fac_int_p=0.0
ENDDO
IF(ALLOCATED(balanc)) DEALLOCATE(balanc)
IF(ALLOCATED(balanc_sed)) DEALLOCATE(balanc_sed)
IF(ALLOCATED(balanc_nitr)) DEALLOCATE(balanc_nitr)
IF(ALLOCATED(balancqb)) DEALLOCATE(balancqb)
IF(ALLOCATED(balancexp)) DEALLOCATE(balancexp)
IF(ALLOCATED(estad)) DEALLOCATE(estad)
IF(ALLOCATED(RSRindex)) DEALLOCATE(RSRindex)
IF(ALLOCATED(estadsed)) DEALLOCATE(estadsed)
!GUIOMAR (15/10/2015): cambio el número de miembros de balanc incrementándolo a 27 para que almacene nuevas variables
!Cris (03/2017): aumento a 30 para riego y pérdidas en diferentes acuíferos
ALLOCATE(balanc(nt,28),balancqb(nt),balancexp(nt),estad(naf*2+knaf+kadi,22),estadsed(ksedq*2,12),balanc_sed(nt,30),RSRindex(naf*2+knaf+kadi),balanc_nitr(nt,60))
balanc=0.0
balanc_sed=0.0
estad=0.0
disn=0.0
preac=0.0
balancqb=0.0  
contcauce=0
RSRindex=0.0
balancexp=0.0
balanc_nitr=0.0
IF (kevp.gt.0) THEN
  ALLOCATE(evapo(kevp),band_e(kevp))
  DO n=1,ncel
    ALLOCATE(cell(n).ind_int_e(enest),cell(n).fac_int_e(enest))
	cell(n).ind_int_e=0
	cell(n).fac_int_e=0.0
  ENDDO
ENDIF
IF (ktem.gt.0) THEN
  ALLOCATE(band_t(ktem),temper(ktem),acuniv(nt))
  DO n=1,ncel
    ALLOCATE(cell(n).ind_int_t(tnest),cell(n).fac_int_t(tnest))
	cell(n).ind_int_t=0
	cell(n).fac_int_t=0.0
  ENDDO
ENDIF
!Guiomar (21/07/2015): Preparando para poder interpolar la radiacion
IF (nradiacion.gt.0) THEN
  ALLOCATE(band_r(nradiacion))
  DO n=1,ncel
    ALLOCATE(cell(n).ind_int_r(rnest),cell(n).fac_int_r(rnest))
	cell(n).ind_int_r=0
	cell(n).fac_int_r=0.0
  ENDDO
ENDIF
nb=nemb+vnemb+knemb
IF (nb.gt.0) THEN 
  ALLOCATE(nivel(nb),pulm(nb),emb(nb))
  pulm=0
  ALLOCATE(volum(nb))
  ALLOCATE(qemb(nb))
ENDIF

IF (kadi.gt.0) THEN
  ALLOCATE(aport(kadi))
ENDIF

IF (config(2)) THEN 
  CALL lee_evto2col
ELSE
  CALL lee_evto2
ENDIF
IF (sale.eq.2) GOTO 95

CALL lee_fechainicio !para liberar el nombre del fichero de entrada
IF (sale.eq.2) GOTO 95

!Define la fecha inicial del nombre del evento
fecin=archin(1:2)//'/'//archin(3:4)//'/'//archin(11:14)
read(archin(11:14),*)nyear
horin=archin(5:6)//':'//archin(7:8)//':00'
!mes y día inicial en formato real
aa1=archin(3:4)
READ(aa1,*)mesin
aa1=archin(1:2)
READ(aa1,*)diain
aa1=archin(5:6)
READ(aa1,*)horain
aa1=archin(7:8)
READ(aa1,*)minin
minInDay = horain*60 + minin
nhora = INT(horain)
nmonth = INT(mesin)
nday= INT(diain)
nmin = minin

!Cuenta el numero de celdas a regar por cada zona de riego
IF(modulos(5)) THEN !riego
    banriego=0
    DO i=1,erie
      DO n=1,ncel
        IF (cell(n).codrie.eq.i) banriego(i)=banriego(i)+1
      ENDDO

    ENDDO
END IF    

!Lee parametros de calibracion (factores correctores)
CALL lee_calib
IF (sale.eq.2) GOTO 95

IF(tipoParent.eq.1) THEN !Solo entra si es TETIS
	!Copia respaldo de los ficheros CALIB y PARAMGEO para luego ser recuperados
	CALL  conserva_fic 

	! Escribe parametros de calibracion (factores correctores)
	artem=arch(2)
	CALL escri_calib

	! Escribe parametros geomorfologicos
	artem=arch(1)
	CALL escri_parg
END IF

! Calcula el número de celdas para flujos superficiales
! El umbral para la escorrentía directa es igual a cero
DO n=1,ncel
  ncp=cell(n).codpar
  DO i=2,3
	nw(i,ncp)=areaumbral(i,ncp)/arcelkm
  ENDDO
ENDDO	

IF (config(4)) THEN  !Lee condiciones iniciales de sedimentos
  artem=archsed(7)
  CALL lee_sedantec
  IF (sale.eq.2) GOTO 95
ENDIF

IF (simevents) THEN
	arch(4)=estadohum(1) !si se está usando la calib autom multievento aquí lee el ficher hantec del primer evento considerado
END IF

!Lee humedad antecedente
artem=arch(4)
CALL lee_human
IF (sale.eq.2) GOTO 95

!Lee todos los ficheros relativos al submodelo de nitrógeno Cris (03/2016)
!No cambiar de sitio porque lee_Ninputsuelo necesita saber el valor de nt para comprobar que tiene la misma longitud que el fichero de entrada.
!Para calcular la adsorción/desorción es necesario haber leído hantec
If (modulos2(3)) then
    call lee_fcubiertan
    If (sale==2) Go to 95
    call lee_calibnit
    If (sale==2) Go to 95    
    call lee_Ninputsuelo
    If (sale==2) Go to 95
    Do i=1,ncel
        cell(n).fn=0.0
        cell(n).allUptake=0.0
        cell(n).allPercolation=0.0
        cell(n).allInput=0.0
    end do
    !Se ceran las variables de cultivos en caso de estar activado para que las celdas que no sean cultivos tomen valor cero y se hace aquí porque tiene 
    !que ser antes de que lea el estado inicial, que ya tiene las variables nw y fcncult
    If (modulos2(4)) then
        Do i=1,ncel
            cell(n).w=0.0
            cell(n).fcncult=0.0
            cell(n).nw=0.0
            cell(n).Ncrit=0.0
            cell(n).Nextr=0.0
            cell(n).restcosecha=0.0
        End do
        IF (ALLOCATED(crecimiento)) DEALLOCATE(crecimiento)
        ALLOCATE (crecimiento(ncel))
        crecimiento=0
    End if
    artem=archnit(13)
    call lee_nantec
    If (sale==2) Go to 95

    Do n=1,ncel
        !Proceso de adsorción/desorción de NH4 en suelo, para completar la columna de hn(8)
        !Si se genera nantec por usos del suelo, se da una concentración en suelo de NH4, que hay que distribuir en disuelto/particulado según kd
        !Se hace aquí para el inicial, para no tener que generar nantec cada vez que se cambian los parámetros
        !Luego se hace al final de cada paso de tiempo desde sim_celdanitr, si se utilizase un nantec2 como inicial, lo haría otra vez aquí pero daría el mismo resultado. Así que no hay problema.
        NH4total=cell(n).hn(1)+cell(n).hn(8)
        if((cell(n).h(1)+cell(n).h(3)+cell(n).hlim)>0.000005) then
            cell(n).hn(1)=((cell(n).h(1)+cell(n).h(3)+cell(n).hlim)/1000*NH4total)/((cell(n).h(1)+cell(n).h(3)+cell(n).hlim)/1000+cell(n).kd*fckd*cell(n).psuelo*r(1)*cell(n).daparente)
            Cell(n).hn(8)=NH4total-cell(n).hn(1)
            If(cell(n).hn(8)<0.0) then
                cell(n).hn(1)=NH4total
                cell(n).hn(8)=0.0
            End if
            !If (cell(n).hn(8)<0.0) then
            !    write(*,*)NH4total,cell(n).hn(1),n,'hn8_tetis'
            !    pause
            !End if
            If(cell(n).kd<0.0001.or.cell(n).psuelo*r(1)<0.0001.or.cell(n).daparente<0.0001) then
                cell(n).hn(1)=cell(n).hn(1)
                cell(n).hn(8)=cell(n).hn(8)
            End if
            !If(cell(n).hn(1)<0.0) then
            !    write(*,*)(cell(n).h(1)+cell(n).h(3)+cell(n).hlim)/1000*NH4total,((cell(n).h(1)+cell(n).h(3)+cell(n).hlim)/1000+cell(n).kd*fckd*cell(n).psuelo*r(1)*cell(n).daparente),n,'fn1_n11'
            !    write(*,*)cell(n).h(1),cell(n).h(3),cell(n).hlim,cell(n).kd,fckd,cell(n).psuelo*r(1),cell(n).daparente
            !    pause
            !End if
        else
            cell(n).hn(1)=0.0
            cell(n).hn(8)=NH4total
        end if
    End do
    If (modulos2(4)) then !Lee los archivos relacionados con nitrógeno-cultivos
        call lee_codcult
        If (sale==2) Go to 95
        call lee_carcult
        If (sale==2) Go to 95
        call lee_factvegcultivos
        If (sale==2) Go to 95
        call lee_Ninputsuelo_Cultivos
        If (sale==2) Go to 95
    End if
Else if (modulos2(4)) then !Si cultivos está activado pero no nitrógeno, lee sólo el fichero factvegcultivos que es lo único que hace falta
    call lee_factvegcultivos
    If (sale==2) Go to 94
End if
If (sale==2) goto 95

!Lee fichero con factores de cultivo mensuales para el calculo de la ETP
!Guiomar (28/01/2014). Explicación sobre lo ya programado: Abre el archivo factorETmes, cuenta sus líneas a partir de las dos primeras y después comprueba que el número de líneas coincida con el número de coberturas
If (config(5)) then
    k=0
    OPEN(10,file=arch(41),status='old',err=257)
    ios=0
    DO WHILE (ios.ne.-1)
      READ(10,*,iostat=ios) j
      IF (ios.ne.-1) k=k+1
    ENDDO
    CLOSE(10)

    massimocodveg=maxval(cell(:).codveg)
    IF(k.lt.massimocodveg)then
        mensaje=strings(312)
        errr=1
        CALL errores
        goto 94
    ENDIF
Else
    k=0
    OPEN(10,file=arch(6),status='old',err=248)
    READ(10,*)
    READ(10,*)
    ios=0
    DO WHILE (ios.ne.-1)
      READ(10,*,iostat=ios) j
      IF (ios.ne.-1) k=k+1
    ENDDO
    CLOSE(10)

    massimocodveg=maxval(cell(:).codveg)
    IF(k.lt.massimocodveg)then
        mensaje=strings(64)
        errr=1
        CALL errores
        goto 94
    ENDIF
End if
!Cris (03/2017). Hace lo mismo, pero en este caso para el fichero de factor de cubierta del submodelo de nitrógeno, los parámetros y el fichero ninputsuelo
If (modulos2(3)) then
    !Factor de cubierta
    k=0 
    OPEN(10,file=archnit(9),status='old',err=252)
    ios=0
    DO WHILE (ios.ne.-1)
      READ(10,*,iostat=ios) j
      IF (ios.ne.-1) k=k+1
    ENDDO
    CLOSE(10)
    IF(k.lt.massimocodveg)then
        mensaje=strings(402)
        errr=1
        CALL errores
        goto 94
    ENDIF
    !Parámetros 
    k=0
    OPEN(10,file=archnit(8),status='old',err=253)
    ios=0
    DO WHILE (ios.ne.-1)
      READ(10,*,iostat=ios) j
      IF (ios.ne.-1) k=k+1
    ENDDO
    CLOSE(10)
    IF((k-2).lt.massimocodveg)then !Se compara k-2 porque hay unos específicos de cauce y los de temperatura y humedad. Hay usos del suelo + 2filas
        mensaje=strings(405)
        errr=1
        CALL errores
        goto 94
    ENDIF
    !Inputs de nitrógeno en suelo
    !!k=0
    !!OPEN(10,file=archnit(7),status='old',err=254)
    !!ios=0
    !!DO WHILE (ios.ne.-1)
    !!  READ(10,*,iostat=ios) j
    !!  IF (ios.ne.-1) k=k+1
    !!ENDDO
    !!CLOSE(10)
    !!IF((k).lt.massimocodveg)then
    !!    mensaje=strings(414)
    !!    errr=1
    !!    CALL errores(errr,mensaje,lang)
    !!    goto 94
    !!ENDIF
    k=0
    OPEN(10,file=archnit(10),status='old',err=255)
    ios=0
    DO WHILE (ios.ne.-1)
      READ(10,*,iostat=ios) j
      IF (ios.ne.-1) k=k+1
    ENDDO
    CLOSE(10)
    IF((k).lt.massimocodveg)then
        mensaje=strings(430)
        errr=1
        CALL errores
        goto 94
    ENDIF
        k=0
    OPEN(10,file=archnit(11),status='old',err=256)
    ios=0
    DO WHILE (ios.ne.-1)
      READ(10,*,iostat=ios) j
      IF (ios.ne.-1) k=k+1
    ENDDO
    CLOSE(10)
    IF((k).lt.massimocodveg)then
        mensaje=strings(431)
        errr=1
        CALL errores
        goto 94
    ENDIF
End if
!Guiomar (28/01/2014): el imax es la 13 columna del factorETmes sólo en el caso de no tener activada la vegetación dinámica. Por eso, he añadido un if
IF (.NOT.(config(5))) THEN
  DO n=1,ncel
    cell(n).imx=lamb(cell(n).codveg,13)
  ENDDO
END IF

!Calcula la humedad antecedente de la cuenca para cerrar el balance
DO i=0,8
  almini(i)=0.0
ENDDO
DO n=1,ncel
  DO i=0,4
    almini(i)=almini(i)+cell(n).h(i)
  ENDDO
  almini(6)=almini(6)+cell(n).h(6)
  IF (nw(2,ncp).gt.cell(n).acum) cell(n).h(5)=0.0    !celda con ladera, no hay volumen inicial en cauces
  almini(5)=almini(5)+cell(n).h(5)*1000.0/arcel      !mm
  !Guiomar (28/01/2013): voy a añadir un if config(5) para que calcule el almacenamiento del tanque añadido en el modelo
  IF (config(5)) THEN
      almini(8)=almini(8)+cell(n).h(8)
  ENDIF
ENDDO
!Calcula la humedad antecedente media
DO i=0,4
  almini(i)=almini(i)/ncel
ENDDO
almini(6)=almini(6)/ncel
!Guiomar (28/01/2013): voy a añadir un if config(5) para que calcule la humedad antecedente media del tanque añadido
IF (config(5)) THEN
    almini(8)=almini(8)/ncel
ENDIF

!Lee curvas de embalse
nb=nemb+vnemb+knemb
IF (nb.gt.0) THEN
  DO i=1,nb
    emb(i).caso=0
  ENDDO
  CALL cal_caso

  CALL lee_emb1
  IF (sale.eq.2) GOTO 95

  DO i=1,nb
	IF (emb(i).caso.gt.0) THEN
      k=emb(i).datos
      ALLOCATE(emb(i).h(0:k),emb(i).vol(0:k),emb(i).sup(0:k),emb(i).out(0:k,2))
      ALLOCATE(emb(i).fpul(0:k))
	ENDIF
  ENDDO

  CALL lee_emb2
  IF (sale.eq.2) GOTO 95
ENDIF

!Distancia entre celdas
CALL dis_cel

!Calcula puntos importantes
CALL ptos_imp

!Inicia variables de estado
CALL inicia_var

!Inicia variables de estado de sedimentos y realiza interpolaciones
IF (config(4)) CALL inicia_varsed

GO TO 95

206 mensaje=strings(101)
errr=1
CALL errores
GOTO 94

209 mensaje=strings(31)
errr=1
CALL errores
GOTO 94
210 mensaje=strings(32)
errr=1
CALL errores
GOTO 94
212 mensaje=strings(103)
errr=1
CALL errores
GOTO 94
213 mensaje=strings(104)
errr=1
CALL errores
GOTO 94
246 mensaje=strings(109)
errr=1
CALL errores
GOTO 94
247 mensaje=strings(111)
errr=1
CALL errores
GOTO 94
248 mensaje=strings(61)
errr=1
CALL errores
GOTO 94
249 mensaje=strings(110)
errr=1
CALL errores
GOTO 94
250 mensaje=strings(112)
errr=1
CALL errores
GOTO 94
252 mensaje=strings(400)
errr=1
CALL errores
GOTO 94
253 mensaje=strings(403)
errr=1
CALL errores
GOTO 94
255 mensaje=strings(426)
errr=1
CALL errores
GOTO 94
256 mensaje=strings(427)
errr=1
CALL errores
GOTO 94
257 mensaje=strings(312)
errr=1
CALL errores
GOTO 94

94 WRITE(*,*)strings(800)
sale_=2

95 END SUBROUTINE    

!************************************************************************
!* Identifica los puntos importantes (aforos, embalses y otros sitios)
!************************************************************************
SUBROUTINE ptos_imp
USE modtet
IMPLICIT NONE


DO n=1,ncel
  DO l=1,nemb
    IF (nivel(l).fila.eq.cell(n).fil)THEN
      IF (nivel(l).columna.eq.cell(n).col)THEN
        nivel(l).pos=n
		nivel(l).area=cell(n).acum*dx*dy/1000000.
      ENDIF
    ENDIF
  ENDDO
  DO l=1,vnemb
    IF (volum(l).fila.eq.cell(n).fil)THEN
      IF (volum(l).columna.eq.cell(n).col)THEN
        volum(l).pos=n
		volum(l).area=cell(n).acum*dx*dy/1000000.
      ENDIF
    ENDIF
  ENDDO
  DO l=1,knemb
    IF (qemb(l).fila.eq.cell(n).fil)THEN
      IF (qemb(l).columna.eq.cell(n).col)THEN
        qemb(l).pos=n
		qemb(l).area=cell(n).acum*dx*dy/1000000.
      ENDIF
    ENDIF
  ENDDO
  DO l=1,naf
    IF (aforo(l).fila.eq.cell(n).fil)THEN
      IF (aforo(l).columna.eq.cell(n).col)THEN
        aforo(l).pos=n
		aforo(l).area=cell(n).acum*dx*dy/1000000.
      ENDIF
    ENDIF
  ENDDO
  DO l=1,knaf
    IF (otros(l).fila.eq.cell(n).fil)THEN
      IF (otros(l).columna.eq.cell(n).col)THEN
        otros(l).pos=n
		otros(l).area=cell(n).acum*dx*dy/1000000.
      ENDIF
    ENDIF
  ENDDO
  DO l=1,kadi
    IF (aport(l).fila.eq.cell(n).fil)THEN
      IF (aport(l).columna.eq.cell(n).col)THEN
        aport(l).pos=n
		aport(l).area=cell(n).acum*dx*dy/1000000.
      ENDIF
    ENDIF
  ENDDO
  IF (config(4)) THEN
    DO l=1,ksedq
      IF (aforosed(l).fila.eq.cell(n).fil)THEN
        IF (aforosed(l).columna.eq.cell(n).col)THEN
          aforosed(l).pos=n
		  aforosed(l).area=cell(n).acum*dx*dy/1000000.
		END IF
	  END IF	
    END DO
    DO l=1,kadised1   ! Cris (11/2015)
      IF (aportsed1(l).fila.eq.cell(n).fil)THEN
        IF (aportsed1(l).columna.eq.cell(n).col)THEN
          aportsed1(l).pos=n
		  aportsed1(l).area=cell(n).acum*dx*dy/1000000.
        ENDIF
      ENDIF
    ENDDO
    DO l=1,kadised2
      IF (aportsed2(l).fila.eq.cell(n).fil)THEN
        IF (aportsed2(l).columna.eq.cell(n).col)THEN
          aportsed2(l).pos=n
		  aportsed2(l).area=cell(n).acum*dx*dy/1000000.
        ENDIF
      ENDIF
    ENDDO
    DO l=1,kadised3
      IF (aportsed3(l).fila.eq.cell(n).fil)THEN
        IF (aportsed3(l).columna.eq.cell(n).col)THEN
          aportsed3(l).pos=n
		  aportsed3(l).area=cell(n).acum*dx*dy/1000000.
        ENDIF
      ENDIF
    ENDDO
  ENDIF
  IF(config(5))THEN
    DO l=1,nveg
        IF (veg(l).fila.eq.cell(n).fil)THEN
        IF (veg(l).columna.eq.cell(n).col)THEN
          veg(l).pos=n
		  veg(l).area=cell(n).acum*dx*dy/1000000.
        ENDIF
      ENDIF
    END DO
  END IF
  IF (modulos2(3)) THEN
    DO l=1,kno
      IF (norg(l).fila.eq.cell(n).fil)THEN
        IF (norg(l).columna.eq.cell(n).col)THEN
          norg(l).pos=n
		  norg(l).area=cell(n).acum*dx*dy/1000000.
		END IF
	  END IF	
    END DO
    DO l=1,kam
      IF (amonio(l).fila.eq.cell(n).fil)THEN
        IF (amonio(l).columna.eq.cell(n).col)THEN
          amonio(l).pos=n
		  amonio(l).area=cell(n).acum*dx*dy/1000000.
		END IF
	  END IF	
    END DO
    DO l=1,kni
      IF (nitrato(l).fila.eq.cell(n).fil)THEN
        IF (nitrato(l).columna.eq.cell(n).col)THEN
          nitrato(l).pos=n
		  nitrato(l).area=cell(n).acum*dx*dy/1000000.
		END IF
	  END IF	
    END DO
  End if
ENDDO


DO i=1,nemb
  IF (emb(i).nombre.eq.nivel(i).name) emb(i).pos=nivel(i).pos
ENDDO
DO i=1,vnemb
  IF (emb(nemb+i).nombre.eq.volum(nemb+i).name) emb(nemb+i).pos=volum(nemb+i).pos
ENDDO
DO i=1,knemb
  IF (emb(nemb+vnemb+i).nombre.eq.qemb(nemb+vnemb+i).name) emb(nemb+vnemb+i).pos=qemb(nemb+vnemb+i).pos
ENDDO


END SUBROUTINE


!*********************************************************************
!* Rutina que inicia variables de estado y realiza interpolaciones
!*********************************************************************
SUBROUTINE inicia_var
USE modtet
!USE DFLIB
IMPLICIT NONE

!Inicializa las variables actualizando los valores
DO n=1,ncel
  cell(n).q=0.0
  cell(n).vel=0.0
  cell(n).u=0.0
  cell(n).x=0.0
  cell(n).y=0.0
  !Inicializamos las variables relacionados con la vegetación dinámica en el caso de que sea seleccionada esa opción
  IF (config(5)) THEN
      cell(n).ei=0.0
      cell(n).tr1=0.0
      cell(n).tr2=0.0
      cell(n).tr=0.0
      cell(n).es=0.0
      cell(n).pg=0.0
      cell(n).resp=0.0
      cell(n).muerte=0.0
      cell(n).dxveg=0.0
      sla=lamb(cell(n).codveg,10)
      cell(n).xveg=cell(n).lai/(sla*cell(n).fc)
  ENDIF
  ncp=cell(n).codpar
!Calculo de relaciones geomorfologicas para la OCG en cárcavas
  IF (nw(2,ncp).le.cell(n).acum.AND.nw(3,ncp).gt.cell(n).acum) THEN
    eyc=2.0/3.0-ec(5,ncp)*ec(6,ncp)
    ezc=1.0/(1.0+eyc*ec(4,ncp))
    cvelc=dc(6,ncp)*(dc(5,ncp)**ec(6,ncp))*(dc(3,ncp)**eyc)*(dc(1,ncp)**(eyc*(ec(3,ncp)-ec(4,ncp))))
    cvelc=cvelc**(-ezc)
    eslc=ezc*(1.0/2.0-ec(5,ncp)*ec(6,ncp))
    eacc=ezc*eyc*ec(1,ncp)*(ec(4,ncp)-ec(3,ncp))
  ENDIF
!Calculo de relaciones geomorfologicas para la OCG en cauces
  IF (nw(3,ncp).le.cell(n).acum) THEN
    ey=2.0/3.0-e(5,ncp)*e(6,ncp)
    ez=1.0/(1.0+ey*e(4,ncp))
    cvel=d(6,ncp)*(d(5,ncp)**e(6,ncp))*(d(3,ncp)**ey)*(d(1,ncp)**(ey*(e(3,ncp)-e(4,ncp))))
    cvel=cvel**(-ez)
    esl=ez*(1.0/2.0-e(5,ncp)*e(6,ncp))
    eac=ez*ey*e(1,ncp)*(e(4,ncp)-e(3,ncp))
  ENDIF
  slp=cell(n).pend
  arac=arcelkm*cell(n).acum
  cell(n).u(6)=cell(n).imx                  !U(6,n)=Agua interceptada por la vegetación (mm)
  cell(n).u(1)=cell(n).hu                   !U(1,n)=Agua disponible (Agua útil) (mm)
  cell(n).u(2)=dt*cell(n).ks                !U(2,n) permeabilidad de la capa superior del suelo en cond de sat en (mm/hora)*dt  (mm)
  cell(n).u(3)= dt*cell(n).kp               !U(3,n) permeabilidad del estrato de roca en cond de sat en (mm/hora)*dt  (mm)
  cell(n).u(4)= dt*cell(n).kps              !U(4,n)=Pérdidas (mm)
  cell(n).u(7)=dt*cell(n).kss               !velocidad interflujo  (mm)
  cell(n).u(8)=dt*cell(n).ksa               !velocidad flujo base  (mm)

  IF (nw(2,ncp).le.cell(n).acum.AND.nw(3,ncp).gt.cell(n).acum) THEN 
    cell(n).u(5)=dts*r(9)*cvelc*(arac**eacc)*(slp**eslc)     !U(5,n) longitud que avanza el flujo en dt en (m) para cárcavas
  ENDIF
 
  IF (nw(3,ncp).le.cell(n).acum) THEN 
    cell(n).u(5)=dts*r(9)*cvel*(arac**eac)*(slp**esl)        !U(5,n) longitud que avanza el flujo en dt en (m) para cauces
  ENDIF

ENDDO

!Pregunta si desea interpolar cada tiempo o no
IF (.NOT.bint) THEN
  !Interpolación de la lluvia
  nest2=nest
  band_p=.true.
  CALL interp_pluvio
  !Interpolación de la temperatura 
  IF (ktem.gt.0) THEN
    tnest2=tnest
	band_t=.true.
	CALL interp_temp
  ENDIF
  !Interpolación de la evapotranspiracion
  IF (kevp.gt.0) THEN
    enest2=enest
    band_e=.true.
    CALL interp_evapo
  ENDIF
  !Guiomar(21/07/2015): Preparando para interpolar radiacion
  IF (nradiacion.gt.0) THEN
    rnest2=rnest
    band_r=.true.
    CALL interp_radiacion
  ENDIF
ENDIF

END SUBROUTINE


!*************************************************************************
!* Rutina para el calculo de los factores de interpolación de lluvia
!* METODO:Inverso de la distancia al cuadrado
!* Tiene en cuenta los valores no registrados (banp=0, la serie debe 
!* o banp=-1 si se desea incluir el efecto de dato faltante -1)
!*************************************************************************
SUBROUTINE interp_pluvio
!USE DFLIB
USE modtet
IMPLICIT NONE

INTEGER k0
CHARACTER*16 string16

DO n=1,ncel
  disn=99999.9
  !Calcula factores de interpolación de k1 estaciones
  DO k=1,kppt
    IF (band_p(k)) THEN
      disn(k)=(((cell(n).fil-pluvio(k).fila)*(cell(n).fil-pluvio(k).fila)+  &
	        (cell(n).col-pluvio(k).columna)*(cell(n).col-pluvio(k).columna))**0.5+0.001)
	ENDIF
  ENDDO
  !Calcula posición de las nest cercanos en los nest disponibles
  k0=0
  DO k=1,kppt
    IF (band_p(k)) THEN
      non=MINLOC(disn)
      disn(non(1))=99999.9
      k0=k0+1
	  cell(n).ind_int_p(k0)=non(1)
	ENDIF
	IF (k0.eq.nest2) EXIT
  ENDDO
  !Calcula coeficientes de interpolación
  sinvdis=0.0
  DO k=1,k0
    nb=cell(n).ind_int_p(k)
    disn(nb)=(((cell(n).fil-pluvio(nb).fila)*(cell(n).fil-pluvio(nb).fila)+  &
	        (cell(n).col-pluvio(nb).columna)*(cell(n).col-pluvio(nb).columna))**0.5+0.001)
    sinvdis=sinvdis+1.0/(disn(nb)*disn(nb))
  ENDDO
  DO k=1,k0
    nb=cell(n).ind_int_p(k)
    cell(n).fac_int_p(k)=(1.0/(disn(nb))**2.)/sinvdis 
    !WRITE(string16,'(F16.8)')cell(n).fac_int_p(k)
  ENDDO
ENDDO

END SUBROUTINE

!*************************************************************************
!* Rutina para el calculo de los factores de interpolación de etp
!* METODO:Inverso de la distancia al cuadrado
!* Tiene en cuenta los valores no registrados (bane=0, la serie debe 
!* o bane=-1 si se desea incluir el efecto de dato faltante -1)
!*************************************************************************
SUBROUTINE interp_evapo
!USE DFLIB
USE modtet
IMPLICIT NONE

INTEGER k0

DO n=1,ncel
  disn=99999.9
  !Calcula factores de interpolación de k1 estaciones
  DO k=1,kevp
    IF (band_e(k)) THEN
      disn(k)=(((cell(n).fil-evapo(k).fila)*(cell(n).fil-evapo(k).fila)+  &
	        (cell(n).col-evapo(k).columna)*(cell(n).col-evapo(k).columna))**0.5+0.001)
	ENDIF
  ENDDO
  !Calcula posición de las enest cercanos en los kevp disponibles
  k0=0
  DO k=1,kevp
    IF (band_e(k)) THEN
      non=MINLOC(disn)
      disn(non(1))=99999.9
	  k0=k0+1
      cell(n).ind_int_e(k0)=non(1)
	ENDIF
	IF (k0.eq.enest2) EXIT
  ENDDO
  !Calcula coeficientes de interpolación
  sinvdis=0.0
  DO k=1,k0
    nb=cell(n).ind_int_e(k)
    disn(nb)=(((cell(n).fil-evapo(nb).fila)*(cell(n).fil-evapo(nb).fila)+  &
	        (cell(n).col-evapo(nb).columna)*(cell(n).col-evapo(nb).columna))**0.5+0.001)
    sinvdis=sinvdis+1.0/(disn(nb)*disn(nb)) 
  ENDDO
  DO k=1,k0
    nb=cell(n).ind_int_e(k)
    cell(n).fac_int_e(k)=(1.0/(disn(nb)*disn(nb)))/sinvdis 
  ENDDO
ENDDO

END SUBROUTINE

!*************************************************************************
!* Rutina para el calculo de los factores de interpolación de temperatura
!* METODO:Inverso de la distancia al cuadrado
!* Tiene en cuenta los valores no registrados (bant=0, la serie debe 
!* o bant=-1 si se desea incluir el efecto de dato faltante -99)
!*************************************************************************
SUBROUTINE interp_temp
!USE DFLIB
USE modtet
IMPLICIT NONE

INTEGER k0

DO n=1,ncel
  disn=99999.9
  !Calcula factores de interpolación de k1 estaciones
  DO k=1,ktem
    IF (band_t(k)) THEN
      disn(k)=(((cell(n).fil-temper(k).fila)*(cell(n).fil-temper(k).fila)+  &
	        (cell(n).col-temper(k).columna)*(cell(n).col-temper(k).columna))**0.5+0.001)
    ENDIF
  ENDDO
  !Calcula posición de las tnest cercanos en los ktem disponibles
  k0=0
  DO k=1,ktem
    IF (band_t(k)) THEN
	  non=MINLOC(disn)
      disn(non(1))=99999.9
	  k0=k0+1
	  cell(n).ind_int_t(k0)=non(1)
    ENDIF
	IF (k0.eq.tnest2) EXIT
  ENDDO
  !Calcula coeficientes de interpolación
  sinvdis=0.0
  DO k=1,k0
    nb=cell(n).ind_int_t(k)
    disn(nb)=(((cell(n).fil-temper(nb).fila)*(cell(n).fil-temper(nb).fila)+  &
	        (cell(n).col-temper(nb).columna)*(cell(n).col-temper(nb).columna))**0.5+0.001)
    sinvdis=sinvdis+1.0/(disn(nb)*disn(nb))
  ENDDO
  DO k=1,k0
    nb=cell(n).ind_int_t(k)
    cell(n).fac_int_t(k)=(1.0/(disn(nb)*disn(nb)))/sinvdis
  ENDDO
ENDDO

END SUBROUTINE
!*************************************************************************
!* Rutina para el calculo de los factores de interpolación de radiacion
!* METODO:Inverso de la distancia al cuadrado
!* Tiene en cuenta los valores no registrados (bane=0, la serie debe 
!* o bane=-1 si se desea incluir el efecto de dato faltante -1)
!Guiomar (21/07/2015)
!*************************************************************************
SUBROUTINE interp_radiacion
!USE DFLIB
USE modtet
IMPLICIT NONE

INTEGER k0

DO n=1,ncel
  disn=99999.9
  !Calcula factores de interpolación de k1 estaciones
  DO k=1,nradiacion
    IF (band_r(k)) THEN
      disn(k)=(((cell(n).fil-radiacion(k).fila)*(cell(n).fil-radiacion(k).fila)+  &
	        (cell(n).col-radiacion(k).columna)*(cell(n).col-radiacion(k).columna))**0.5+0.001)
	ENDIF
  ENDDO
  !Calcula posición de las rnest cercanos en los nradiacion disponibles
  k0=0
  DO k=1,nradiacion
    IF (band_r(k)) THEN
      non=MINLOC(disn)
      disn(non(1))=99999.9
	  k0=k0+1
      cell(n).ind_int_r(k0)=non(1)
	ENDIF
	IF (k0.eq.rnest2) EXIT
  ENDDO
  !Calcula coeficientes de interpolación
  sinvdis=0.0
  DO k=1,k0
    nb=cell(n).ind_int_r(k)
    disn(nb)=(((cell(n).fil-radiacion(nb).fila)*(cell(n).fil-radiacion(nb).fila)+  &
	        (cell(n).col-radiacion(nb).columna)*(cell(n).col-radiacion(nb).columna))**0.5+0.001)
    sinvdis=sinvdis+1.0/(disn(nb)*disn(nb)) 
  ENDDO
  DO k=1,k0
    nb=cell(n).ind_int_r(k)
    cell(n).fac_int_r(k)=(1.0/(disn(nb)*disn(nb)))/sinvdis 
  ENDDO
ENDDO

END SUBROUTINE

!*************************************************************************
!* Rutina de simulación para todo el intervalo temporal corresponde 
!* a la primera parte de la rutina principal del modelo TETIS
!*************************************************************************
SUBROUTINE sim_tpo
USE modtet
!USE DFLIB
!USE DFPORT
IMPLICIT NONE

!TYPE (rccoord) curpos
INTEGER k4,cc, dttemp_int
REAL V_TE

!Empieza la simulación

!Se inician variables
preac=0.0
dttemp_int = 0
DO i = 1,ncel
    cell(i).xascii=0
    cell(i).yascii=0
    cell(i).hascii=0
    cell(i).SusLADascii=0
    cell(i).SusREDascii=0
    cell(i).DepLADascii=0
    cell(i).DepREDascii=0
    cell(i).ErodSedascii=0
    cell(i).ErodTotalascii=0
    cell(i).SedFlujoascii=0
    cell(i).SedFlujoTotalascii=0
    cell(i).fnascii=0
    cell(i).hnascii=0
    cell(i).laiascii=0
    cell(i).lairascii=0
    cell(i).imaxascii=0
    cell(i).eiascii=0
    cell(i).trascii=0
    cell(i).tr1ascii=0
    cell(i).tr2ascii=0
    cell(i).esascii=0
    cell(i).xvegascii=0
ENDDO


IF (config(4)) THEN
  DO i=1,ksedq
    aforosed(i).sim=0
  ENDDO
ENDIF
DO i=1,knaf
  DO j=1,nt
    otros(i).sim(j)=0.0
  ENDDO
ENDDO

nmonth=INT(mesin)
nday=INT(diain)

if(simevents) cc=1 !si queremos realizar una calibración multievento

If (modulos2(3)) THEN
    !Transformaciones de para cualquier dt
    DO i=1,cantUsSueNitr
        kmin(i)=kmin2(i)/(24*60)*dtmin !días-1, dt-1
        kinm(i)=kinm2(i)/(24*60)*dtmin !días-1, dt-1
        kvol(i)=kvol2(i)/(24*60)*dtmin !días-1, dt-1
        knit(i)=knit2(i)/(24*60)*dtmin !días-1, dt-1
        kfi(i)=kfi2(i)*arcel/10000/(24*60)*dtmin !Transformación kg/hadía a kg/celdadt
        kdes(i)=kdes2(i)/(24*60)*dtmin !días-1, dt-1
        F(i)=F2(i)*1000/(24*60)*dtmin !Transformación de m/día a mm/dt
        Ndem(i)=Ndem2(i)*arcel/10000/365/(24*60)*dtmin !Transformación de kg/haaño a Kg/celdadt
        !PrefNO3 es adimensional, no hay que transformarla
    ENDDO
    kminc=kminc2/(24*60)*dtmin
    knitc=knitc2/(24*60)*dtmin
    kdesc=kdesc2/(24*60)*dtmin
    !Los parámetros de las correcciones de temperatura no hace falta transformarlas, no dependen del dt
END IF

DO t=1,nt
  IF (sim2) THEN
    write(*,'(F9.2)')t*100./nt
  ENDIF
  
!Calcula los dias del mes ((Cris 06/2017 añado diferencia entre años bisiestos porque para cultivos es muy importante. Si el año es bisiesto el día 29 se vuelve a convertir en 28))
      SELECT CASE (INT(mesin))
      CASE (1,3,5,7,8,10,12)
          fmes=31
        CASE (2)
          if(typeyear==1) then
             If (mod(nyear,4).eq.0.and.mod(nyear,100).ne.0.or.mod(nyear,400).eq.0) then !Comprobación de año bisiesto                                    
                  fmes=29
              Else
	            fmes=28
              End if
          Else
	        fmes=28  !Si typeyear==2, febrero siempre tiene 28 días
          End if
        CASE (4,6,9,11)
         fmes=30
      END SELECT
      IF (nmonth.gt.2) THEN
        njulian=INT((275*nmonth/9)-30+nday)-2
      ELSE
        njulian=INT((275*nmonth/9)-30+nday)
        if(typeyear==1.and.njulian.eq.60) then
             If (mod(nyear,4).eq.0.and.mod(nyear,100).ne.0.or.mod(nyear,400).eq.0) then !Comprobación de año bisiesto
                  njulian=59
              End if
        End if        
      ENDIF  
  !Pregunta si interpola en cada tiempo (es para considerar el efecto de datos -1 variables)
  !Guiomar(21/07/2015): Preparando para interpolar radiacion
  rnest2=rnest
  nest2=nest
  enest2=enest
  tnest2=tnest
  IF (bint) THEN
    !Interpolación de la lluvia
    band_p=.true.
    k4=0
    DO k=1,kppt
      k4=k4+1
      IF(pluvio(k).obs(t).lt.0) THEN
	    band_p(k)=.false.
		k4=k4-1
	  ENDIF
    ENDDO
    nest2=MIN(nest,k4)

    CALL interp_pluvio

    IF (ktem.gt.0) THEN
      !Interpolación de la temperatura 
      band_t=.true.
	  k4=0
      DO k=1,ktem
        k4=k4+1
        IF(temper(k).obs(t).le.-98.0) THEN
	      band_t(k)=.false.
	      k4=k4-1
	    ENDIF
      ENDDO
      tnest2=MIN(tnest,k4)
	  CALL interp_temp
    ENDIF
   IF (nradiacion.gt.0) THEN
      !Interpolación de la radiacion 
      band_r=.true.
	  k4=0
      DO k=1,nradiacion
        k4=k4+1
        IF(radiacion(k).obs(t).lt.0) THEN
	      band_r(k)=.false.
	      k4=k4-1
	    ENDIF
      ENDDO
      rnest2=MIN(rnest,k4)
	  CALL interp_radiacion
    ENDIF

    !Interpolación de la evapotranspiracion
    band_e=.true.
    k4=0
    DO k=1,kevp
      k4=k4+1
      IF(evapo(k).obs(t).lt.0) THEN
	    band_e(k)=.false.
	    k4=k4-1
	  ENDIF
    ENDDO
    enest2=MIN(enest,k4)
    CALL interp_evapo
  ENDIF
  !LEE LLUVIA (las correcciones de datos faltantes se han realizado antes)
  !define iflagp: indicador de que no hay lluvia en ninguna estacion
  iflagp=0
  tfn=ro1  !Tasa de fusion sin lluvia
  DO k=1,kppt
    IF (pluvio(k).obs(t)>0.0)THEN
      iflagp=1
      tfn=ro2  !Tasa de fusion con lluvia
    ENDIF
  ENDDO
  
  !Inicia rutina para todas la celdas
  CALL sim_celda
  
  
  IF (trapeff) THEN
    DO i=1,numpresas
      l=i
      dam(i).depthTE(t)=dam(i).PalturaNew
      dam(i).depositTE(t)=V_TE(dam(i).Paltura)-dam(i).dep_tot
    ENDDO
  ENDIF
 
  !(Vicente) Actualizacion correcta de las fechas. Gracias a fmes se consideran bisiesto segun settings
  minInDay = minInDay + dtmin
  IF (minInDay >= 1440) THEN
      nday = nday + INT(minInDay/1440)
      minInDay = minInDay - INT(minInDay/1440)*1440
      nhora=0
      diain = REAL(nday)
      !Vicente; al cambiar de dia, modificamos las matrices de riego
      IF (modulos(5)) THEN          
        DO i=1,erie 
            IF(isZonaRegada(i)==1) THEN
                IF(pasoTiempoRegados(i).ne.0) THEN
                    contriego(i)=contriego(i) + INT(contRiegoInDay(i)/pasoTiempoRegados(i))
                END IF    
                pasoTiempoRegados(i)=0
                isZonaRegada(i)=0
            END IF            
            grupoOrdenRiego(i) = grupoOrdenRiego(i) + 1
            celdasRegadas(i) = contriego(i)
            contRiegoInDay(i) = 0
                IF(contriego(i) == banriego(i)) THEN
                    celdasRegadas(i) = 0
                    grupoOrdenRiego(i) = 1
                    contriego(i) = 0                    
                END IF  
        END DO
      END IF
  END IF
  nhora = INT(minInDay/60)
  nmin = minInDay - nhora*60 

  IF(nday>fmes) THEN
      nday = nday - fmes
      diain = REAL(nday)
      nmonth=nmonth+1
      mesin=REAL(nmonth)
      !Vicente; al cambiar de mes, inicializamos las matrices de riego
      IF(modulos(5)) THEN
          grupoOrdenRiego = 1
          celdasRegadas = 0
          contRiegoInDay = 0
          contriego = 0
      END IF  
  END IF
  IF (nmonth>12) THEN
      nyear = nyear + INT(nmonth/12)
      nmonth= nmonth - INT(nmonth/12)*12      
      mesin=REAL(nmonth)
  END IF
    
  
if(simevents)then
    if(t.eq.nstep(cc).and.cc.ne.nexe)then
        cc=cc+1  
        artem=estadohum(cc)
        CALL lee_human
        IF (sale.eq.2) GOTO 955
        fecin=datainiz(cc)
        horin=orainiz(cc)
        
        READ(fecin(7:10),*)nyear
        READ(fecin(4:5),*)mesin
        READ(fecin(1:2),*)diain
        nmonth=INT(mesin)
        nday=INT(diain)
    ENDIF
end if
!escribe mapas de variables de estado
IF (printascii) THEN
    do i=0,6
        do j=1,ncel
          cell(j)%yascii(i)=cell(j)%yascii(i)+cell(j)%y(i)
          !cell(j)%hascii(i)=cell(j)%hascii(i)+cell(j)%h(i) !(Vicente) Mapas Vegetacion
        enddo
    enddo
    !(Vicente) Mapas Vegetacion
    do i=0,8
        do j=1,ncel
          cell(j)%hascii(i)=cell(j)%hascii(i)+cell(j)%h(i)
        enddo
    enddo
    do i=0,9!11
        do j=1,ncel
            cell(j)%xascii(i)=cell(j)%xascii(i)+cell(j)%x(i)
        end do
    end do
    do i=1,3
        do j=1,ncel
            cell(j)%SusLADascii(i)=cell(j)%SusLADascii(i)+cell(j)%SusSedLAD(i)
            cell(j)%SusREDascii(i)=cell(j)%SusREDascii(i)+cell(j)%SusSedRED(i)
            cell(j)%DepLADascii(i)=cell(j)%DepLADascii(i)+cell(j)%DepSedLAD(i)
            cell(j)%DepREDascii(i)=cell(j)%DepREDascii(i)+cell(j)%DepSedRED(i)
            cell(j)%ErodSedascii(i)=cell(j)%ErodSed(i) !Cris (24/10/2016) Erodsed ya es una variable acumulada, no hace falta acumlarla aquí de nuevo
            cell(j)%SedFlujoascii(i)=cell(j)%SedFlujoascii(i)+cell(j)%SedFlujo(i)
        enddo
    enddo
    do j=1,ncel
        cell(j)%ErodTotalascii=cell(j)%ErodSed(1)+cell(j)%ErodSed(2)+cell(j)%ErodSed(3) !Cris (24/10/2016) Erodsed ya es una variable acumulada, no hace falta acumlarla aquí de nuevo
        cell(j)%SedFlujoTotalascii=cell(j)%SedFlujoTotalascii+cell(j)%SedFlujo(1)+cell(j)%SedFlujo(2)+cell(j)%SedFlujo(3)
    enddo
    
    Do j=1,ncel !(Vicente) Mapas Vegetacion
        cell(j)%laiascii=cell(j)%laiascii+cell(j)%lai
        cell(j)%lairascii=cell(j)%lairascii+cell(j)%lair
        cell(j)%imaxascii=cell(j)%imaxascii+cell(j)%imax
        cell(j)%eiascii=cell(j)%eiascii+cell(j)%ei
        cell(j)%trascii=cell(j)%trascii+cell(j)%tr
        cell(j)%tr1ascii=cell(j)%tr1ascii+cell(j)%tr1
        cell(j)%tr2ascii=cell(j)%tr2ascii+cell(j)%tr2
        cell(j)%esascii=cell(j)%esascii+cell(j)%es
        cell(j)%xvegascii=cell(j)%xvegascii+cell(j)%xveg
    END DO
    
    If (modulos2(3)) then !Cris (03/2017) mapas de nitrógeno
        Do i=0,46
            Do j=1,ncel
                cell(j)%fnascii(i)=cell(j)%fnascii(i)+cell(j)%fn(i)
            End do
        End do
        Do i=0,12
            Do j=1,ncel
                cell(j)%hnascii(i)=cell(j)%hnascii(i)+cell(j)%hn(i)
            End do
        End do
    End if
    
    dttemp_int = dttemp_int + 1
    IF (dttemp_int.ge.dtascii) THEN
      CALL print_ascii
      If (modulos2(3)) then
          call print_asciinitr
      End if
      dttemp_int = dttemp_int - dtascii
      do j=1,ncel
          cell(j)%xascii=0
          cell(j)%yascii=0
          cell(j)%hascii=0
          cell(j)%SusLADascii=0
          cell(j)%SusREDascii=0
          cell(j)%DepLADascii=0
          cell(j)%DepREDascii=0
          cell(j)%ErodSedascii=0
          cell(j)%ErodTotalascii=0
          cell(j)%SedFlujoascii=0
          cell(j)%SedFlujoTotalascii=0
          cell(j)%fnascii=0
          cell(j)%hnascii=0
          cell(j)%laiascii=0
          cell(j)%lairascii=0
          cell(j)%imaxascii=0
          cell(j)%eiascii=0
          cell(j)%trascii=0
          cell(j)%tr1ascii=0
          cell(j)%tr2ascii=0
          cell(j)%esascii=0
          cell(j)%xvegascii=0
      enddo
    ENDIF
    IF (t.eq.nt) THEN
        Call print_finalascii
        If (modulos2(3)) then
          call print_finalasciinitr
      End if
    End if
ENDIF

!GUIOMAR (22/10/2015): Obtención de los mapas de LAIr para cada delta t. Supongo que en el futuro se podrá incluir en printascii
!IF (config(5)) THEN
 !   CALL print_lair
!ENDIF
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!Fin!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 
ENDDO
balanc=balanc/ncel
IF(config(4))balanc_sed=balanc_sed/ncel
IF(modulos2(3))balanc_nitr=balanc_nitr/ncel
preac=preac/ncel   !mm totales Valor medio de precipitación en la cuenca
balancqb=balancqb/contcauce

wdadfin(1)=balanc(nt,13)*100/(averagepar(1)*r(1)) ! porcentaje de humedad final media de H1 para la cuenca con respecto al Humax
wdadfin(2)=balanc(nt,14)                          ! valor medio final de mm en el tanque de agua superficial
wdadfin(3)=balanc(nt,15)                          ! valor medio final de mm en el tanque de agua gravitacional                      
wdadfin(4)=balanc(nt,16)                          ! valor medio final de mm en el tanque de aquífero
wdadfin(5)=balancqb(nt)*100                       ! porcentaje medio de condiciones finales en cauce respecto a sección llena
if(imaxmedio.eq.0)then
    wdadfin(6)=0
else
    wdadfin(6)= balanc(nt,12)*100/Imaxmedio       ! porcentaje de humedad final media de H6 para la cuenca con respecto a Imax
endif

955 continue
END SUBROUTINE


!***************************************************************************
!* Rutina de simulación celda a celda para toda la cuenca y que corresponde
!* a la segunda parte de la rutina principal del modelo TETIS
!***************************************************************************
SUBROUTINE sim_celda
USE modtet
!USE DFLIB
!USE DFPORT
IMPLICIT NONE

REAL temp2,khojas
INTEGER temp
INTEGER dayRiego

!Inicia balance en todas la celdas de la cuenca
DO n=1,ncel
  ncp=cell(n).codpar
  !Calculo de relaciones geomorfologicas para la OCG en cárcavas
  IF (nw(2,ncp).le.cell(n).acum.AND.nw(3,ncp).gt.cell(n).acum) THEN
    eyc=2.0/3.0-ec(5,ncp)*ec(6,ncp)
    ezc=1.0/(1.0+eyc*ec(4,ncp))
	easc=eyc*ezc*(1.0-ec(4,ncp))
	veldts=dts*r(9)
  ENDIF
  !Calculo de relaciones geomorfologicas para la OCG en cauces
  IF (nw(3,ncp).le.cell(n).acum) THEN
    ey=2.0/3.0-e(5,ncp)*e(6,ncp)
    ez=1.0/(1.0+ey*e(4,ncp))
	eas=ey*ez*(1.0-e(4,ncp))  !omega 1
	veldts=dts*r(9)
  ENDIF

  !calcula evapotranspiracion en el intervalo
  evpt=0.0
  DO k=1,enest2
    nip=cell(n).ind_int_e(k)!
	evpt=evpt+evapo(nip).obs(t)*cell(n).fac_int_e(k)
  ENDDO
  !Guiomar(21/07/2015): calculando radiacion
    rdn=0.0
  DO k=1,rnest2
    nip=cell(n).ind_int_r(k)!
	rdn=rdn+radiacion(nip).obs(t)*cell(n).fac_int_r(k)
  ENDDO
   preac(t,2)=preac(t,2)+evpt*r(2)  
!  cell(n).PET=evpt*r(2)
  !actualiza la  información para el punto
  cell(n).x=0.0
  cell(n).y=0.0
  cell(n).vel=0.0 
  salrio=0.0
  cell(n).qladera=0.0
  !cell(n).hladera=0.0
  !cell(n).qsurco=0.0
  !cell(n).hsurco=0.0
  cell(n).exfiltr=0.0

  !En caso de estar activado nutrientes, antes de pasar a los cálculos de los tanques, se llama a correcciones_nitr. Esto es así porque las correcciones por humedad (y temperatura, aunque en este caso
  !no se ve afectada) se calculan con la humedad antecedente, y si lo calculas después, los valores son los finales.
  If (modulos2(3)) then
    CALL correciones_nitr
    Call alm_estadosini 
  End if

  IF (iflagp.eq.1)THEN
      IF(.not.betalin)THEN !si betalin es FALSE entonces se utiliza la ecuación lineal del beta.
       !interpola la lluvia en el punto de interés 
       DO k=1,nest2
         nip=cell(n).ind_int_p(k)
         cell(n).x(1)=cell(n).x(1)+cell(n).fac_int_p(k)*MAX(0.0,pluvio(nip).obs(t)+betappt*(cell(n).cota-pluvio(nip).elev))  !Considerando la cota (multiplicar este segundo término por pluvio(nip).obs(t) para que la corrección sea proporcional al valor registrado en la estación
       ENDDO
    ELSE
       !En las siguientes lineas está el beta como porcentaje de la ppt observada (betalin es TRUE)
        DO k=1,nest2
          nip=cell(n).ind_int_p(k)
          cell(n).x(1)=cell(n).x(1)+cell(n).fac_int_p(k)*MAX(0.0,pluvio(nip).obs(t)+pluvio(nip).obs(t)*betappt*(cell(n).cota-pluvio(nip).elev))
        ENDDO
    ENDIF
  ENDIF
  cell(n).x(1) = cell(n).x(1) * r(10) !(Vicente) Aplicamos el factor corrector correspondiente a la lluvia
  
  preac(t,1)=preac(t,1)+cell(n).x(1)
  
  !T0 NIEVE
  !Calcula la nieve que se convierte en escorrentia
  !VICENTE (29/9/2020): Cambio en el IF porque con la vegetación ya no es sólo la nieve la que utiliza estaciones con temperaturas (SE AÑADE ELSE IF)
  IF (modulos(1)) THEN
    !interpola la temperatura en el punto de interés 
    DO k=1,tnest2
      nip=cell(n).ind_int_t(k)      
      cell(n).x(0)=cell(n).x(0)+(temper(nip).obs(t)-betatemp*(cell(n).cota-temper(nip).elev))*cell(n).fac_int_t(k) !ºC      
      !cell(n).x(0)=cell(n).x(0)+(temper(nip).obs(t)-0.0065*(cell(n).cota-temper(nip).elev))*cell(n).fac_int_t(k) !ºC
    ENDDO
        
    IF (cell(n).x(0).ge.tbase) THEN
      !calcula la fusión de nieve  
      IF (nmonth.le.6) THEN                 
        fusion=MAX(0.0,((tfn*cell(n).rad(nmonth))*(cell(n).x(0)-tbase)*dt/24.0))
      ELSE
        fusion=MAX(0.0,((tfn*cell(n).rad(13-nmonth))*(cell(n).x(0)-tbase)*dt/24.0))
      ENDIF
   
      !si no existiera la nieve no distribuida
      !fusion=MAX(0.0,tfn*(cell(n).x(0)-tbase)*dt/24.0)
      
      IF (fusion.ge.cell(n).h(0)) THEN
        fusion=cell(n).h(0)
        cell(n).h(0)=0.0
      ELSE
        cell(n).h(0)=cell(n).h(0)-fusion
        !cell(n).x(1)=cell(n).x(1)+fusion  Comentario que hace notar que la funsión no tiene que pasar por el tanque de intercepción (T6)
      ENDIF
    ELSE  !hay lluvia y temperatura bajo temperatura base, acumula nieve
      fusion=0.0
      cell(n).h(0)=cell(n).h(0)+cell(n).x(1)
      cell(n).x(1)=0.0
    ENDIF
    cell(n).y(0)=fusion
    acuniv(t)=acuniv(t)+cell(n).h(0)*arcel/1000.0  
    DO k=1,kniv
      IF (cell(n).fil.eq.nieve(k).fila.AND.cell(n).col.eq.nieve(k).columna) THEN
        nieve(k).sim(t)=cell(n).h(0)
	  ENDIF
    ENDDO
  ELSE IF (config(5)) THEN
    DO k=1,tnest2
      nip=cell(n).ind_int_t(k)      
      cell(n).x(0)=cell(n).x(0)+(temper(nip).obs(t)-betatemp*(cell(n).cota-temper(nip).elev))*cell(n).fac_int_t(k) !ºC
      !cell(n).x(0)=cell(n).x(0)+(temper(nip).obs(t)-0.0065*(cell(n).cota-temper(nip).elev))*cell(n).fac_int_t(k) !ºC
    ENDDO    
  ENDIF
  
  balanc(t,1)=cell(n).x(1)+balanc(t,1) !Téngase en cuenta que no se tiene en cuenta la fusión de nieve. balanc(1) es solo la precipitación liquida (lluvia)

  !Calcula el aporte de agua por riego (en caso de que exista)
  !No tiene en cuenta la lluvia acumulada para ser descontada
  IF(cell(n).codrie.gt.0) THEN
    IF(t==1.and.isZonaConsiderada(cell(n).codrie)==0) THEN
        isZonaConsiderada(cell(n).codrie)=1  !(Vicente) Cada zona solo puede entrar una vez a este calculo
        dayRiego = diain
        
        IF(dayRiego > periodRiego(cell(n).codrie,INT(mesin))) THEN
            dayRiego = dayRiego - INT(dayRiego/periodRiego(cell(n).codrie,INT(mesin)))*periodRiego(cell(n).codrie,INT(mesin))
        END IF
        
        IF(dayRiego == 0) dayRiego = periodRiego(cell(n).codrie,INT(mesin))

        IF(dayRiego == 1) THEN
            contriego(cell(n).codrie) = 0
        ELSE
            contriego(cell(n).codrie) = 0
            DO WHILE(dayRiego>1)
                dummy1 = (banriego(cell(n).codrie)-contriego(cell(n).codrie))/(periodRiego(cell(n).codrie,INT(mesin))-grupoOrdenRiego(cell(n).codrie)+1)
                IF(dummy1 - INT(dummy1) > 0.5 ) THEN 
                    contriego(cell(n).codrie) = contriego(cell(n).codrie) + INT(dummy1) + 1
                ELSE
                    contriego(cell(n).codrie) = contriego(cell(n).codrie) + INT(dummy1)
                END IF
                dayRiego = dayRiego - 1
                grupoOrdenRiego(cell(n).codrie) = grupoOrdenRiego(cell(n).codrie) + 1
            END DO
            celdasRegadas(cell(n).codrie) = contriego(cell(n).codrie)
        END IF
        
        Do i=1,n !Si no se empieza en el día 1, cuenta el número de días que debería haberse regado cada celda hasta ese día. Se comete un pequeño error, si la división no es exacta se deja el número inferior
            If(cell(i).codrie==cell(n).codrie) then
                cell(i).Riegosmes=INT(diain/periodRiego(cell(n).codrie,INT(mesin)))
            end if
        End do
    END IF
      
      
    dummy1 = (banriego(cell(n).codrie)-contriego(cell(n).codrie))/(periodRiego(cell(n).codrie,INT(mesin))-grupoOrdenRiego(cell(n).codrie)+1)
    IF (dummy1 - INT(dummy1) > 0.5 ) THEN
        dummy1 = dummy1 + 1
    END IF
      

    IF(cell(n).ordrie>celdasRegadas(cell(n).codrie).and.cell(n).ordrie<=celdasRegadas(cell(n).codrie)+INT(dummy1)) THEN
         If (nday>0.and.nday<periodRiego(cell(n).codrie,INT(mesin))+1) then
             cell(n).Riegosmes=0 !Cuando empieza el mes se reinicia el contador
         End if
         If (cell(n).Riegosmes<INT(fmes/periodRiego(cell(n).codrie,INT(mesin)))) then
            cell(n).x(7)=xriego(cell(n).codrie,INT(mesin))/INT(fmes/(1440/dtmin)/periodRiego(cell(n).codrie,INT(mesin)))
         else
            cell(n).x(7)=0
         end if
         isZonaRegada(cell(n).codrie)=1
         contRiegoInDay(cell(n).codrie) = contRiegoInDay(cell(n).codrie)+1
         cell(n).Riegosmes = cell(n).Riegosmes+1 !cris (16/04/2019)
         IF(contarPasoTiempoRegado(cell(n).codrie)==0) THEN
             contarPasoTiempoRegado(cell(n).codrie)=1
             pasoTiempoRegados(cell(n).codrie) = pasoTiempoRegados(cell(n).codrie) + 1
         END IF 
    END IF
  END IF  
  
  
!!!!!!! TETIS_VEG: modificaciónes
! FLUJOS VERTICALES
! cell(n).x(1)----> aportes (precipitación, nieve, riego) (NOMBRE DE LA VARIABLE CONSERVADO)
! cell(n).x(6)----> flujo excedente no interceptado por la vegetación (Pg)
! cell(n).x(2)----> excedente para percolacón y escorrentá superficial (NOMBRE DE LA VARIABLE CONSERVADO)
! FLUJOS HORIZONTALES
! ?? ----> flujo interceptado por la vegetación (DI)
! ?? ----> flujo almacenado en la zona capilar del suelo (DH)
! SALIDAS DEL SISTEMA
! cell(n).ei=cell(n).y(6) -----> evaporación directa
! cell(n).es -----> evaporación suelo desnudo
! cell(n).tr -----> transpiración
! cell(n).y(1)=cell(n).tr+cell(n).es
! cell(n).evptot -> evapotranspiración total (NOMBRE DE LA VARIABLE CONSERVADO)
! ESTADOS DE ALMACENAMIENTO
! cell(n).h(6)----> almacenamiento del tanque "intercepción" (I)
! cell(n).h(1)----> almacenamiento del tanque "capilar" (H)
! VARIABLES USADAS
! cell(n).rveg ----------> Evolución de la vegetación
! cell(n).waterstress -----------> Water stress (Porporato etal. 2001)
! functeta --------------> Función de la humedad del suelo 
! drdt ------------------> Derivada de la evolución de la vegetación
! khojas ----------------> Coeficiente de caida de hojas (funcion de la estación)
! Inicialización
  cell(n).ei=0.0
  cell(n).es=0.0
  cell(n).tr=0.0
  functeta=0.0
  cell(n).evptot=0.0
  !cell(n).xveg= 0.7 Aquí no debería inicializar xveg porque no es un flujo sino un estado y depende de su valor anterior. Lo he movido a la misma línea donde está lee_human
 ! cell(n).fialloc=1-cell(n).lai/laimax Aquí no debería inicializar FIALLOC porque no es un flujo sino un estado y depende de su valor anterior. Lo he movido a la misma línea donde está lee_human
  !
  ! T6 - INTERCEPCIÓN
  !Guiomar 21/01/2014: hay cambio en el tanque de intercepción en el caso de vegetación dinámica. El Imax dejará de ser un parámetro y se convertirá en una variable de estado cuyo valor se calcula como el producto entre el almacencamiento máximo de las hojas por el LAI
  IF (config(5)) THEN
      !cell(n).x(6)=max(0.0,cell(n).x(1)-cell(n).u(6)*cell(n).rveg+cell(n).h(6))
      !Guiomar (27/01/2014) el parámetro alm_max se lee del factorETmes y ocupa la posición 3
      alm_max=lamb(cell(n).codveg,3)
      cell(n).imax=cell(n).lai*alm_max
      IF(cell(n).codrie.gt.0) THEN  !(Riego)
          If(tiporiego(cell(n).codrie)==2) THEN !Riego por aspersion
              cell(n).x(6)=max(0.0,cell(n).x(1)+cell(n).x(7)-cell(n).imax*cell(n).fc+cell(n).h(6))
          ELSE !Riego. A Manta o por goteo
              cell(n).x(6)=max(0.0,cell(n).x(1)-cell(n).imax*cell(n).fc+cell(n).h(6))
          END IF
      ELSE     !Riego desactivado o celdas no regadas   
        cell(n).x(6)=max(0.0,cell(n).x(1)-cell(n).imax*cell(n).fc+cell(n).h(6))
      END IF
  ELSE if (modulos2(4)) then !Si está la opción de cultivos activada, las celdas en modo cultivo funcionan con un factor de vegetación diario, no mensual. Cris (05/2017)
      compcultivo3=0
      Do i=1,ncult
          If (cell(n).codveg==codcult(i)) then
              IF(cell(n).codrie.gt.0) THEN  !(Riego)
                  If(tiporiego(cell(n).codrie)==2) THEN  !Riego por aspersion
                      cell(n).x(6)=max(0.0,cell(n).x(1)+cell(n).x(7)-cell(n).u(6)*fvegcult(njulian,cell(n).codveg)+cell(n).h(6))
                  ELSE !Riego. A Manta o por goteo
                      cell(n).x(6)=max(0.0,cell(n).x(1)-cell(n).u(6)*fvegcult(njulian,cell(n).codveg)+cell(n).h(6))
                  END IF
              ELSE ! Riego desactivado o celdas no regadas    
                  cell(n).x(6)=max(0.0,cell(n).x(1)-cell(n).u(6)*fvegcult(njulian,cell(n).codveg)+cell(n).h(6))
              END IF              
              compcultivo3=1
              Exit
          End if
      End do    
      If (compcultivo3==0) then !Si el uso del suelo no funciona en modo cultivo, se usa la forma tradicional
        IF(cell(n).codrie.gt.0) THEN  !(Riego)
            If(tiporiego(cell(n).codrie)==2) THEN  !Riego por aspersion
                cell(n).x(6)=max(0.0,cell(n).x(1)+cell(n).x(7)-cell(n).u(6)*lamb(cell(n).codveg,INT(mesin))+cell(n).h(6))
            ELSE !Riego. A Manta o por goteo
                cell(n).x(6)=max(0.0,cell(n).x(1)-cell(n).u(6)*lamb(cell(n).codveg,INT(mesin))+cell(n).h(6))
            END IF
        ELSE ! Riego desactivado o celdas no regadas    
            cell(n).x(6)=max(0.0,cell(n).x(1)-cell(n).u(6)*lamb(cell(n).codveg,INT(mesin))+cell(n).h(6))
        END IF         
      End if
  Else !Modo tradicional
      IF(cell(n).codrie.gt.0) THEN  !(Riego)
            If(tiporiego(cell(n).codrie)==2) THEN  !Riego por aspersion
               cell(n).x(6)=max(0.0,cell(n).x(1)+cell(n).x(7)-cell(n).u(6)*lamb(cell(n).codveg,INT(mesin))+cell(n).h(6))
            ELSE !Riego. A Manta o por goteo
                cell(n).x(6)=max(0.0,cell(n).x(1)-cell(n).u(6)*lamb(cell(n).codveg,INT(mesin))+cell(n).h(6))
            END IF
        ELSE !Riego desactivado o celdas no regadas    
            cell(n).x(6)=max(0.0,cell(n).x(1)-cell(n).u(6)*lamb(cell(n).codveg,INT(mesin))+cell(n).h(6))
        END IF
    End if
    
  IF(cell(n).codrie.gt.0) THEN  !(Riego)
        If(tiporiego(cell(n).codrie)==2) THEN  !Riego por aspersion
            cell(n).ei=min(evpt*r(2),(cell(n).x(1)+cell(n).x(7)-cell(n).x(6)+cell(n).h(6)))  
            cell(n).y(6)=cell(n).ei
            cell(n).h(6)=cell(n).h(6)+(cell(n).x(1)+cell(n).x(7)-cell(n).x(6))-cell(n).ei
        ELSE !Riego. A Manta o por goteo
            cell(n).ei=min(evpt*r(2),(cell(n).x(1)-cell(n).x(6)+cell(n).h(6)))  
            cell(n).y(6)=cell(n).ei
            cell(n).h(6)=cell(n).h(6)+(cell(n).x(1)-cell(n).x(6))-cell(n).ei
        END IF
   ELSE !Riego desactivado o celdas no regadas     
        cell(n).ei=min(evpt*r(2),(cell(n).x(1)-cell(n).x(6)+cell(n).h(6)))  
        cell(n).y(6)=cell(n).ei
        cell(n).h(6)=cell(n).h(6)+(cell(n).x(1)-cell(n).x(6))-cell(n).ei
   END IF
  
  
  
  ! T1 - ALMACENAMIENTO CAPILAR
  !Guiomar 21/01/2014: En modelo de vegetación el tanque estático se divide en dos. Escribiré todo el modelo de vegetación al principio y luego estará el modelo que ha habido tradicionalmente en TETIS con respecto a la vegetación
  IF (config(5)) THEN
      !Guiomar: flujos en el primero de los tanques estáticos
      IF(cell(n).codrie.gt.0) THEN  !(Riego)
          If((tiporiego(cell(n).codrie)==1.or.tiporiego(cell(n).codrie)==3)) THEN !Riego. A Manta o por goteo
            cell(n).d(8)=min((cell(n).x(6)+fusion+cell(n).x(7))*(1-(cell(n).h(8)/(cell(n).hu1*r(1))))**expinf,cell(n).hu1*r(1)-cell(n).h(8))
            cell(n).x(8)=cell(n).x(6)+fusion+cell(n).x(7)-cell(n).d(8)
          else !(Riego por aspersión 
            cell(n).d(8)=min((cell(n).x(6)+fusion)*(1-(cell(n).h(8)/(cell(n).hu1*r(1))))**expinf,cell(n).hu1*r(1)-cell(n).h(8))
            cell(n).x(8)=cell(n).x(6)+fusion-cell(n).d(8)
          end if
      ELSE  ! Riego desactivado o celdas no regadas
         cell(n).d(8)=min((cell(n).x(6)+fusion)*(1-(cell(n).h(8)/(cell(n).hu1*r(1))))**expinf,cell(n).hu1*r(1)-cell(n).h(8))
         cell(n).x(8)=cell(n).x(6)+fusion-cell(n).d(8)
      END IF      
      cell(n).h(8)=cell(n).h(8)+cell(n).d(8)
      !Guiomar:flujos en el segundo de los tanques estáticos
      !cell(n).d(1)=min((cell(n).x(8))*(1-(cell(n).h(1)/(cell(n).hu2*r(1))))**expinf,cell(n).hu2*r(1)-cell(n).h(1))
      cell(n).d(1)=max(min((cell(n).x(8))*(1-(cell(n).h(1)/(cell(n).hu2*r(1))))**expinf,cell(n).hu2*r(1)-cell(n).h(1)),0.0)!Guiomar-Vicente      
      cell(n).x(2)=cell(n).x(8)-cell(n).d(1)
      cell(n).h(1)=cell(n).h(1)+cell(n).d(1)
      !Guiomar: en las siguientes líneas se definirá la función de disponibilidad de agua para la transpiración en el primero de los tanques estáticos. Más información en: tesis doctoral de Marta Pasquato y tesis doctoral de Diana Quevedo
      !hlim es el punto de marchitez: la planta no transpira. hstar es el punto crítico u óptimo: la planta empieza a transpirar sin limitación por el agua
      IF (cell(n).h(8).le.0.0) THEN
          functeta1=0
      ELSE IF (cell(n).h(8).ge.(r(1)*cell(n).hstar1-cell(n).hlim1)) THEN
          functeta1=1
      ELSE
          !Guiomar (27/01/2014) el qdispo se lee del archivo factorETmes y ocupa la posición 1
          qdispo=lamb(cell(n).codveg,1)
          functeta1=(cell(n).h(8)/(r(1)*cell(n).hstar1-cell(n).hlim1))**qdispo
      ENDIF
      !Guiomar: lo mismo pero para el segundo de los tanques estáticos
      IF (cell(n).h(1).le.0.0) THEN
          functeta2=0
      ELSE IF (cell(n).h(1).ge.(r(1)*cell(n).hstar2-cell(n).hlim2)) THEN
          functeta2=1
      ELSE
          qdispo=lamb(cell(n).codveg,1)
          functeta2=(cell(n).h(1)/(r(1)*cell(n).hstar2-cell(n).hlim2))**qdispo
      ENDIF
      !Guiomar: cálculo de transpiraciones, evaporación del suelo desnudo y actualización de los tanques
      raices=lamb(cell(n).codveg,4)
      cell(n).tr1=min((evpt*r(2)*cell(n).fc-min(evpt*r(2)*cell(n).fc,cell(n).ei))*min(cell(n).lai,1.0)*functeta1*raices,cell(n).h(8))
      cell(n).tr2=min((evpt*r(2)*cell(n).fc-min(evpt*r(2)*cell(n).fc,cell(n).ei))*min(cell(n).lai,1.0)*functeta2*(1-raices),cell(n).h(1))
      cell(n).tr=cell(n).tr1+cell(n).tr2
      !Guiomar: Actualización de los tanques
      cell(n).h(8)=cell(n).h(8)-cell(n).tr1
      cell(n).h(1)=cell(n).h(1)-cell(n).tr2
      !Guiomar: ahora se calcula functeta para el suelo desnudo. En este caso estará implicada sólo la primera capa del suelo
      IF (cell(n).h(8).le.0.0) THEN
          functetabs=0
      ELSE IF (cell(n).h(8).ge.(r(1)*cell(n).hstar1-cell(n).hlim1)) THEN
          functetabs=1
      ELSE
          !Guiomar (27/01/2014) el qdispo se lee del archivo factorETmes y ocupa la posición 1
          qdispo=lamb(cell(n).codveg,1)
          functetabs=(cell(n).h(8)/(r(1)*cell(n).hstar1-cell(n).hlim1))
      ENDIF
      !Guiomar: Evaporación suelo desnudo. Sólo se produce desde el tanque estático 1
      cell(n).es=min((evpt*r(2)-min(evpt*r(2),cell(n).ei))*(1-cell(n).fc)*functetabs,cell(n).h(8))
      !Guiomar: Actualización del primer tanque estático
      cell(n).h(8)=cell(n).h(8)-cell(n).es
      cell(n).evptot=cell(n).ei+cell(n).tr+cell(n).es
      !Guiomar 21/01/2014. Empieza el modelo de vegetación basado en el modelo LUE desarrollado por Marta a escala de celda.
      !Guiomar (27/01/2014): kdecay se lee del factorETmes y ocupa la posición 5
      !Guiomar (21/07/2015): radiacion asignada
      radiacion_solar=rdn 
      par=0.48*radiacion_solar
      kdecay=lamb(cell(n).codveg,5)
      fpar=0.95*(1-exp(-kdecay*cell(n).lai))
      !Guiomar: cálculo del factor de estrés hídrico
      !primer tanque estático
      IF (cell(n).h(8).ge.(r(1)*cell(n).hstar1-cell(n).hlim1)) THEN
          funcestres1=0
      ELSE IF (cell(n).h(8).le.0.0) THEN
          funcestres1=1
      ELSE
          !Guiomar (27/01/2014): qestres se lee del factorETmes y ocupa la posición 2
          qestres=lamb(cell(n).codveg,2)
          funcestres1=((r(1)*cell(n).hstar1-cell(n).hlim1-cell(n).h(8))/(r(1)*cell(n).hstar1-cell(n).hlim1))**qestres
      ENDIF
      !Segundo tanque estático
      IF (cell(n).h(1).ge.(r(1)*cell(n).hstar2-cell(n).hlim2)) THEN
          funcestres2=0
      ELSE IF (cell(n).h(1).le.0.0) THEN
          funcestres2=1
      ELSE
          qestres=lamb(cell(n).codveg,2)
          funcestres2=((r(1)*cell(n).hstar2-cell(n).hlim2-cell(n).h(1))/(r(1)*cell(n).hstar2-cell(n).hlim2))**qestres
      ENDIF   
      !Calculamos funcestres combinado teniendo en cuenta el porcentaje de raíces en cada capa
      funcestres=funcestres1*raices+funcestres2*(1-raices)
      !Calculamos coeficiente de estrés por temperatura
      !Guiomar (27/01/2014): topt se lee del archivo factorETmes y ocupa la posición 6
      topt=lamb(cell(n).codveg,6)
      !VICENTE (29/9/2020): Utilizamos x(0) no temper(1) como valor de temperatura
      testres=1.1918603*(1/(1+exp(0.20*(topt-10-cell(n).x(0)))))*(1/(1+exp(0.30*(-topt-10+cell(n).x(0)))))
      !testres=1.1918603*(1/(1+exp(0.20*(topt-10-temper(1).obs(t)))))*(1/(1+exp(0.30*(-topt-10+temper(1).obs(t)))))
      
      !Una vez calculados los coeficientes de estrés se puede calcular Pg que determinará el crecimiento de la biomasa foliar
      !Guiomar (27/01/2014): lue se lee del archivo factorETmes y ocupa la posición 7
      lue=lamb(cell(n).codveg,7)
      cell(n).pg=par*fpar*(1-funcestres)*testres*lue/1000 !lue:Light Use Efficiency index
      !Ahora se calculan los términos de respiración y muerte. La respiración se calcula haciendo uso de una expresión experimental que depende de la temperatura g(T) (aquí gtemp)
      !VICENTE (29/9/2020): Utilizamos x(0) no temper(1) como valor de temperatura
      gtemp=exp(308.56*((1/56.02)-(1/(cell(n).x(0)+46.02))))
      !gtemp=exp(308.56*((1/56.02)-(1/(temper(1).obs(t)+46.02))))
      !Guiomar (27/01/2014): rresp se lee del archivo factorETmes y ocupa la posición 8
      rresp=lamb(cell(n).codveg,8)
      cell(n).resp=((rresp*cell(n).xveg/2.2)/29)*gtemp !rresp es el ratio de respiración
      !Guiomar (27/01/2014): kmuerte se lee del archivo factorETmes y ocupa la posición 9
      kmuerte=lamb(cell(n).codveg,9)
      cell(n).muerte=kmuerte*cell(n).xveg
      laimax=lamb(cell(n).codveg,11)
      cell(n).fialloc=max(1-(cell(n).lai/laimax),0.0)
      cell(n).dxveg=(cell(n).pg-cell(n).resp)*cell(n).fialloc-cell(n).muerte
      cell(n).xveg=MAX(cell(n).xveg+cell(n).dxveg,0.0)
      !Pasamos a convertirlo en LAI
      !Guiomar (27/01/2014): sla y el laimax se leen del factorETmes y ocupan la posición 10 y 11 respectivamente
      sla=lamb(cell(n).codveg,10)
      cell(n).lai=cell(n).xveg*sla*cell(n).fc
      !LAIr sirve para comparar con el NDVI ya que tiene en cuenta el estrés hídrico
      cell(n).lair=cell(n).lai*(1-funcestres)
      !Guiomar (26/11/2014): Calculo cell(n).y(1) para que lo escriba posteriormente en el fichero de salida
      cell(n).y(1)=cell(n).tr+cell(n).es
      !Guiomar (06/11/2014): preparando para que se escriba en el fichero de salida
      DO cont_veg=1,nveg
          IF (cell(n).fil.eq.veg(cont_veg).fila) THEN
              IF (cell(n).col.eq.veg(cont_veg).columna) THEN
                  !(Guiomar-Vicente)
                  veg(cont_veg).sim(t)=cell(n).lai
                  !veg1_point(cont_veg).sim(t)=cell(n).lai
                  veg2_point(cont_veg).sim(t)=cell(n).lair
                  tr_point(cont_veg).sim(t)=cell(n).tr
              ENDIF
          ENDIF
      ENDDO
  ELSE If (modulos2(4)) then !Cris(05/2017) Si está el modo de cultivos activado los usos en modo cultivo funcionan de forma diferente
      IF(cell(n).codrie.gt.0) THEN  !(Riego)
        If((tiporiego(cell(n).codrie)==1.or.tiporiego(cell(n).codrie)==3)) THEN !Riego. A Manta o por goteo
            cell(n).d(1)=min((cell(n).x(6)+fusion+cell(n).x(7))*(1-(cell(n).h(1)/(cell(n).u(1)*r(1))))**expinf,cell(n).u(1)*r(1)-cell(n).h(1))
            cell(n).x(2)=max(0.0,cell(n).x(6)+fusion+cell(n).x(7)-cell(n).d(1))
            cell(n).h(1)=cell(n).h(1)+(cell(n).x(6)+fusion+cell(n).x(7)-cell(n).x(2))
        ELSE !Riego por aspersion
            cell(n).d(1)=min((cell(n).x(6)+fusion)*(1-(cell(n).h(1)/(cell(n).u(1)*r(1))))**expinf,cell(n).u(1)*r(1)-cell(n).h(1))
            cell(n).x(2)=max(0.0,cell(n).x(6)+fusion-cell(n).d(1))
            cell(n).h(1)=cell(n).h(1)+(cell(n).x(6)+fusion-cell(n).x(2))
        END IF
      ELSE !Riego desactivado o celda no regada
          cell(n).d(1)=min((cell(n).x(6)+fusion)*(1-(cell(n).h(1)/(cell(n).u(1)*r(1))))**expinf,cell(n).u(1)*r(1)-cell(n).h(1))
          cell(n).x(2)=max(0.0,cell(n).x(6)+fusion-cell(n).d(1))
          cell(n).h(1)=cell(n).h(1)+(cell(n).x(6)+fusion-cell(n).x(2))
      END IF             
       
      
      IF ((cell(n).hstar).eq.0) THEN 
          functeta=1
      ELSE IF ((cell(n).h(1)).lt.(cell(n).hstar)) THEN
          functeta=(cell(n).h(1))/(cell(n).hstar)
      ELSE
          functeta=1
      ENDIF
      !Hasta aquí no hay diferencia
      compcultivo4=0
      Do i=1,ncult
          If (cell(n).codveg==codcult(i)) then
              cell(n).y(1)=MAX(0.0,MIN((evpt*r(2)-cell(n).ei)*fvegcult(njulian,cell(n).codveg)*functeta,cell(n).h(1)))
              cell(n).rveg=fvegcult(njulian,cell(n).codveg)
              if(abs(cell(n).h(1)-cell(n).y(1))<0.000005) then
                  cell(n).h(1)=0.0
              else
                cell(n).h(1)=cell(n).h(1)-cell(n).y(1)
              end if
              cell(n).evptot=cell(n).ei+cell(n).tr+cell(n).es
              compcultivo4=1
              Cycle
          End if
      End do    
      If (compcultivo4==0) then !Si el uso del suelo no funciona en modo cultivo, se usa la forma tradicional
          cell(n).y(1)=MAX(0.0,MIN((evpt*r(2)-cell(n).ei)*lamb(cell(n).codveg,INT(mesin))*functeta,cell(n).h(1)))
          cell(n).rveg=lamb(cell(n).codveg,INT(mesin))
          if(abs(cell(n).h(1)-cell(n).y(1))<0.000005) then
            cell(n).h(1)=0.0
          else
            cell(n).h(1)=cell(n).h(1)-cell(n).y(1)
          end if
          cell(n).evptot=cell(n).ei+cell(n).tr+cell(n).es
      End if
  ELSE !Modo tradicional
      IF(cell(n).codrie.gt.0) THEN  !(Riego)
        If((tiporiego(cell(n).codrie)==1.or.tiporiego(cell(n).codrie)==3)) THEN !Riego. A Manta o por goteo
            cell(n).d(1)=min((cell(n).x(6)+fusion+cell(n).x(7))*(1-(cell(n).h(1)/(cell(n).u(1)*r(1))))**expinf,cell(n).u(1)*r(1)-cell(n).h(1)) !chiara: flujo de entrada al tanque estático
            cell(n).x(2)=max(0.0,cell(n).x(6)+fusion+cell(n).x(7)-cell(n).d(1))
            cell(n).h(1)=cell(n).h(1)+(cell(n).x(6)+fusion+cell(n).x(7)-cell(n).x(2))
        ELSE !Riego por aspersion
            cell(n).d(1)=min((cell(n).x(6)+fusion)*(1-(cell(n).h(1)/(cell(n).u(1)*r(1))))**expinf,cell(n).u(1)*r(1)-cell(n).h(1)) !chiara: flujo de entrada al tanque estático
            cell(n).x(2)=max(0.0,cell(n).x(6)+fusion-cell(n).d(1))
            cell(n).h(1)=cell(n).h(1)+(cell(n).x(6)+fusion-cell(n).x(2))
        END IF
      ELSE !Riego desactivado o celda no regada
          cell(n).d(1)=min((cell(n).x(6)+fusion)*(1-(cell(n).h(1)/(cell(n).u(1)*r(1))))**expinf,cell(n).u(1)*r(1)-cell(n).h(1)) !chiara: flujo de entrada al tanque estático    
          cell(n).x(2)=max(0.0,cell(n).x(6)+fusion-cell(n).d(1))
          cell(n).h(1)=cell(n).h(1)+(cell(n).x(6)+fusion-cell(n).x(2))
      END IF
      !Guiomar: a partir de aquí se mantiene la programación que había antes
       IF ((cell(n).hstar).eq.0) THEN 
          functeta=1
      ELSE IF ((cell(n).h(1)).lt.(cell(n).hstar)) THEN
          functeta=(cell(n).h(1))/(cell(n).hstar)
      ELSE
          functeta=1
      ENDIF
  !IF (config(5)) THEN
    !IF (((cell(n).h(1)).le.0).or.(evpt*r(2)-cell(n).ei).le.0) THEN
      !cell(n).tr=0
    !ELSE
      !cell(n).tr=MIN(evpt*r(2)*cell(n).rveg*functeta,evpt*r(2)-cell(n).ei,cell(n).h(1))
      !ENDIF
    !
  !IF (((evpt*r(2)-cell(n).ei-cell(n).tr).le.0).or.((cell(n).rveg).ge.1).or.((cell(n).h(1)-cell(n).tr).le.0)) THEN
      !cell(n).es=0
  !ELSE
      !cell(n).es=MIN((cell(n).h(1)-cell(n).tr)*cell(n).rs*(1-cell(n).rveg),evpt*r(2)-cell(n).ei-cell(n).tr)
  !ENDIF 
    ! 
    !SELECT CASE (njulian)
        !CASE(:81,357:) !winter
            !khojas=cell(n).kleaf1*dtmin/1440
        !CASE(82:120) !spring a
            !khojas=cell(n).kleaf2*dtmin/1440
        !CASE(121:172) !spring b
            !khojas=cell(n).kleaf3*dtmin/1440
        !CASE(173:265) !winter
            !khojas=cell(n).kleaf4*dtmin/1440
        !CASE DEFAULT !autumn
            !khojas=cell(n).kleaf5*dtmin/1440
    !END SELECT
    !IF((cell(n).h(1)).ge.(cell(n).hstar)) THEN
      !cell(n).waterstress=0
    !ELSE IF (((cell(n).h(1)).gt.0).AND.((cell(n).h(1)).lt.(cell(n).hstar))) THEN
        !cell(n).waterstress=((cell(n).hstar-cell(n).h(1))/(cell(n).hstar))**cell(n).qveg
    !ELSE
      !cell(n).waterstress=1
    !ENDIF
    !drdt=cell(n).alfa/365*dtmin/1440*((cell(n).tr/(cell(n).tmx*dtmin/1440))**cell(n).cveg)-khojas*cell(n).rveg*(1+cell(n).waterstress)
    !cell(n).rveg=cell(n).rveg+drdt
    !ELSE
    cell(n).es=0
    !IF(((cell(n).h(1)).eq.0).or.(evpt*r(2)-cell(n).ei).eq.0) THEN
        !cell(n).tr=0
    !ELSE
        !cell(n).tr=MIN((evpt*r(2)-cell(n).ei)*lamb(cell(n).codveg,INT(mesin))*functeta,cell(n).h(1))
    !ENDIF
    cell(n).tr=MAX(0.0,MIN((evpt*r(2)-cell(n).ei)*lamb(cell(n).codveg,INT(mesin))*functeta,cell(n).h(1)))
    cell(n).rveg=lamb(cell(n).codveg,INT(mesin))
    cell(n).h(1)=cell(n).h(1)-cell(n).es-cell(n).tr
    cell(n).y(1)=cell(n).tr+cell(n).es
    cell(n).evptot=cell(n).ei+cell(n).tr+cell(n).es
 ENDIF
  
! VIEJO TETIS - NO CONSIDERABA VEGETACIÓN (lo dejamos en verde por si en algún momento queremos desactivar la vegetación)
!  !
!calcula excedentes del almacenamiento en el suelo Nivel 1
!  ! CASE (1) !modelo simple TETIS !!! original 
!  cell(n).x(2)=MAX(0.0,(cell(n).x(1)-cell(n).u(1)*r(1)+cell(n).h(1)))
!  ! CASE (4) !Kirkby
!  !cell(n).x(2)=MAX(0.0,cell(n).x(1)-MIN(cell(n).u(1)*r(1)-cell(n).h(1),(cell(n).h(1)**2+dts*r(7)**2)**0.5-cell(n).h(1)))
!  !Tetis v72_actual
!  ! CASE (2) !GR2
!  !cell(n).x(2)=MAX(cell(n).x(1)*(cell(n).h(1)/(cell(n).u(1)*r(1)))**2,(cell(n).x(1)-cell(n).u(1)*r(1)+cell(n).h(1)))
!  cell(n).h(1)=cell(n).h(1)+cell(n).x(1)-cell(n).x(2)
!
!  !calcula evapotranspiracion en el intervalo
!  evpt=0.0
!  DO k=1,enest2
!    nip=cell(n).ind_int_e(k)!!
!	evpt=evpt+evapo(nip).obs(t)*cell(n).fac_int_e(k)
!  ENDDO
!  preac(t,2)=preac(t,2)+evpt*r(2)
!  cell(n).y(1)=MIN(lamb(cell(n).codveg,INT(mesin))*evpt*r(2),cell(n).h(1))   !Incluye COBVEG
!  cell(n).h(1)=cell(n).h(1)-cell(n).y(1)  !Para los casos 1,2 y 3
! FIN VIEJO TETIS NO VEG  
!!!!!!! STOP CAMBIOS

  ! T2 Escorrentia directa
  cell(n).x(3)=MIN(cell(n).x(2),cell(n).u(2)*r(3))        !Calcula cantidad de infiltración (excedentes para el Nivel 2)
  cell(n).h(2)=cell(n).h(2)+cell(n).x(2)-cell(n).x(3)     !Actualiza el nivel del tanque 2 (Almacenamiento gravitacional)
  !Calcula salida del tanque 2 (Escorrentía directa), por embalse lineal
  !orig=(r(4)*dt*3600.*cell(n).veloc)
  !orig=(1.0-(dx/((r(4)*dt*3600.*cell(n).veloc)+dx)))

  !TANQUES 3 (Flujo subsuperficial)
  cell(n).x(4)=MIN(cell(n).x(3),cell(n).u(3)*r(5))    ! percolación = x(4)
  !If (cell(n).acuif==1) then 
  !  cell(n).x(9)=cell(n).x(4)   !Recarga a acuífero conectado
  !  cell(n).x(10)=0.0
  !Else !cell(n).acuif==2
  !  cell(n).x(9)=0.0
  !  cell(n).x(10)=cell(n).x(4)    !Recarga a acuífero no conectado
  !  actacuifero=1
  !End if
  cell(n).h3max=alpha/100*cell(n).u(1)*r(1)  !capacidad máxima del tanque

  cell(n).h(3)=cell(n).h(3)+cell(n).x(3)-cell(n).x(4) 
  if(cell(n).h(3).gt.cell(n).h3max)then
    cell(n).exfiltr=cell(n).h(3)-cell(n).h3max
    cell(n).h(3)=cell(n).h3max
    cell(n).h(2)=cell(n).h(2)+cell(n).exfiltr  !suma al tanque superficial la exfiltración del tanque gravitacional
  end if
    cell(n).y(2)=MIN(cell(n).h(2),cell(n).h(2)*(1.0-(dx/((r(4)*dt*3600.*cell(n).veloc)+dx)))) !mm escorrentía superficial
    cell(n).y(3)=MIN(cell(n).h(3),cell(n).h(3)*(1.0-(dx/(r(6)*cell(n).u(7)/1000.+dx))))

  IF(cell(n).dest.eq.0) cell(n).dest=ncel+1   
  
  !TANQUES 4  (flujo base)
    cell(n).x(5)=MIN(cell(n).x(4),cell(n).u(4)*r(7)) 
    !!Modificado para contemplar la posibilidad de saber qué celdas pertenecen al acuífero conectado y cuáles no
    !!Si no se sabe, todas funcionan en el modo tradicional, cell(n).acuif=1     (Cris 03/2016)
    !If (cell(n).acuif==1) then 
    !    cell(n).x(5)=MIN(cell(n).x(4),cell(n).u(4)*r(7))    !pérdidas subterráneas = x(5), x(5) corresponde a toda la cuenca
    !    cell(n).x(9)=cell(n).x(5)
    !    cell(n).x(10)=0.0
    !Else !cell(n).acuif==2
    !    cell(n).x(5)=cell(n).x(4) !En el caso de no ser celda de acuífero conectado, todo pasa a pérdidas subterráneas
    !    cell(n).x(10)=cell(n).x(5)
    !    cell(n).x(9)=0.0
    !    actacuifero=1
    !End if
    
    cell(n).h(4)=cell(n).h(4)+cell(n).x(4)-cell(n).x(5)
    orig=(1.0-(dx/(r(8)*cell(n).u(8)/1000.+dx)))
    cell(n).y(4)=MIN(cell(n).h(4),cell(n).h(4)*orig)
    
    !If (cell(n).acuif==2) then
    !    cell(n).y(4)=0.0
    !Else
    !    If (cell(cell(n).dest).acuif==1) then !Si la celda destino es del acuífero conectado, hay salida
    !        cell(n).y(4)=MIN(cell(n).h(4),cell(n).h(4)*orig)
    !    Else !Si la celda destino es del acuífero no conectado, no hay salida, salvo que sea el punto de desagüe de la cuenca
    !        cell(n).y(4)=0
    !        If (n==ncel.and.cell(n).acuif==1) then !En el caso de que la última celda sea conectada, siempre hay Y4 aunque no se sepa a dónde vierte. Si es no conectada, no hay Y4, porque h4=0
    !            cell(n).y(4)=MIN(cell(n).h(4),cell(n).h(4)*orig)
    !        end if
    !    End if
    !End if

  
 !BLOQUE DE TRANSFERENCIA ED, FS, FB TENIENDO EN CUENTA LAS AREAS UMBRALES
 !DE LA CELDA MISMA Y DE LA CELDA DE DESTINO
 
 !KARST
 IF (modulos2(2)) THEN
   DO l=1,nman
    IF (manantial(l).fila.eq.cell(n).fil)THEN
      IF (manantial(l).columna.eq.cell(n).col)THEN
        manantial(l).pos=n
        cmex=manantial(l).coef
		manantial(l).area=cell(n).acum*dx*dy/1000000.
		logicman=.TRUE.
      ENDIF
    ENDIF
   ENDDO
 ENDIF

   !Para la escorrentía directa, flujo subsuperficial y flujo base para celdas karst
    IF(cell(n).codkarst.eq.1)THEN !CELDA KARST
      IF(cell(cell(n).dest).codkarst.eq.1)THEN !CELDA RECEPTORA KARST
          IF(logicman)THEN !Cris (01/2017) Ahora saca el agua aunque el punto de manantial no sea borde
                cell(n).vel=cell(n).vel+(cell(n).y(2)+cell(n).y(3)+cell(n).y(4)+((cell(n).y(3)+cell(n).y(4))/100*cmex))*arcel/1000.0
          Else
	            cell(cell(n).dest).h(3)=cell(cell(n).dest).h(3)+cell(n).y(3)  !mm
                cell(cell(n).dest).h(4)=cell(cell(n).dest).h(4)+cell(n).y(4)  !mm
	            IF(nw(2,ncp).gt.cell(n).acum) THEN !LADERA
	                IF (nw(2,ncp).gt.cell(cell(n).dest).acum) THEN !Celda receptora con ladera, la escorrentía directa va a la ladera de la celda receptora
	                    cell(cell(n).dest).h(2)=cell(cell(n).dest).h(2)+cell(n).y(2) !mm
	                ELSE 
	                    cell(cell(n).dest).h(5)=cell(cell(n).dest).h(5)+(cell(n).y(2))*arcel/1000.0   !m3 
	                ENDIF
	            ELSE !CARCAVA Y CAUCE
	                    cell(n).vel=cell(n).vel+cell(n).y(2)
                ENDIF
          ENDIF
        ELSE  !CELDA RECEPTORA NO KARST
            !IF(logicman)THEN  !MANANTIAL
                cell(n).vel=cell(n).vel+(cell(n).y(2)+cell(n).y(3)+cell(n).y(4)+((cell(n).y(3)+cell(n).y(4))/100*cmex))*arcel/1000.0   !m3
                !****Cris (01/2017) Ahora todo el borde es manantial*****
     !       ELSE !CELDA NORMAL
     !           cell(cell(n).dest).h(3)=cell(cell(n).dest).h(3)+cell(n).y(3)  !mm
	    !        cell(cell(n).dest).h(4)=cell(cell(n).dest).h(4)+cell(n).y(4)  !mm
	    !        IF(nw(2,ncp).gt.cell(n).acum) THEN !LADERA
	    !            IF (nw(2,ncp).gt.cell(cell(n).dest).acum) THEN !Celda receptora con ladera, la escorrentía directa va a la ladera de la celda receptora
	    !                cell(cell(n).dest).h(2)=cell(cell(n).dest).h(2)+cell(n).y(2) !mm
	    !            ELSE 
	    !                cell(cell(n).dest).h(5)=cell(cell(n).dest).h(5)+(cell(n).y(2))*arcel/1000.0   !m3
	    !            ENDIF
	    !        ELSE !CÁRCAVA Y CAUCE
	    !                cell(n).vel=cell(n).vel+cell(n).y(2)
     !           ENDIF  
     !      ENDIF
	    ENDIF   
    ENDIF
    
    IF (modulos2(2)) THEN
      DO l=1,nman
        IF (manantial(l).fila.eq.cell(cell(n).dest).fil)THEN
          IF (manantial(l).columna.eq.cell(cell(n).dest).col)THEN
            manantial(l).pos=cell(n).dest
            cmex=manantial(l).coef
		    manantial(l).area=cell(cell(n).dest).acum*dx*dy/1000000.
		    logicman2=.TRUE.
          ENDIF
        ENDIF
      ENDDO
    ENDIF
    
   IF(cell(n).codkarst.ne.1)THEN !CELDA NO KARST
    IF(cell(cell(n).dest).codkarst.eq.1)THEN !CELDA RECEPTORA KARST
        IF(logicman2)THEN !celda manantial
            cell(cell(n).dest).h(5)=cell(cell(n).dest).h(5)+cell(n).y(2)+cell(n).y(3)+cell(n).y(4)   !m3
        ELSE
	        cell(cell(n).dest).h(3)=cell(cell(n).dest).h(3)+cell(n).y(3)  !mm
	        cell(cell(n).dest).h(4)=cell(cell(n).dest).h(4)+cell(n).y(4)  !mm
	        IF(nw(2,ncp).gt.cell(n).acum) THEN !LADERA
	            IF (nw(2,ncp).gt.cell(cell(n).dest).acum) THEN !Celda receptora con ladera, la escorrentía directa va a la ladera de la celda receptora
	                cell(cell(n).dest).h(2)=cell(cell(n).dest).h(2)+cell(n).y(2) !mm
	            ELSE 
	                cell(cell(n).dest).h(5)=cell(cell(n).dest).h(5)+(cell(n).y(2))*arcel/1000.0   !m3
	            ENDIF
	        ELSE !CARCAVA Y CAUCE
	                cell(n).vel=cell(n).vel+cell(n).y(2)
	        ENDIF
	    ENDIF
    ELSE  !CELDA RECEPTORA NO KARST
        !Para la escorrentía directa, flujo subsuperficial y flujo base
        IF(nw(2,ncp).gt.cell(n).acum) THEN !LADERA
	        !salrio=cell(n).y(2)*arcel/1000.0  !m³ - volumen de escorrentia que se usa para la prod. de sedimentos en ladera (Kilinc-Richardson)
            IF (nw(2,ncp).gt.cell(cell(n).dest).acum) THEN !Celda receptora con ladera, la escorrentía directa y el interflujo van a la ladera de la celda receptora
	            cell(cell(n).dest).h(2)=cell(cell(n).dest).h(2)+cell(n).y(2)  !mm
	            cell(cell(n).dest).h(3)=cell(cell(n).dest).h(3)+cell(n).y(3)  !mm
	            cell(cell(n).dest).h(4)=cell(cell(n).dest).h(4)+cell(n).y(4)  !mm
	        ELSE IF (nw(3,ncp).gt.cell(cell(n).dest).acum) THEN !Celda receptora con carcava
                cell(cell(n).dest).h(5)=cell(cell(n).dest).h(5)+(cell(n).y(2)+cell(n).y(3))*arcel/1000.0   !m3
	            cell(cell(n).dest).h(4)=cell(cell(n).dest).h(4)+cell(n).y(4)  !mm
            ELSE !Celda receptora con cauce
                cell(cell(n).dest).h(5)=cell(cell(n).dest).h(5)+(cell(n).y(2)+cell(n).y(3)+cell(n).y(4))*arcel/1000.0   !m3
            ENDIF
        ELSE IF (nw(3,ncp).gt.cell(n).acum) THEN !CARCAVA
            cell(n).vel=cell(n).vel+cell(n).y(2)+cell(n).y(3)
	        IF (nw(3,ncp).gt.cell(cell(n).dest).acum) THEN !Celda receptora con carcava
	        cell(cell(n).dest).h(4)=cell(cell(n).dest).h(4)+cell(n).y(4)  !mm
            ELSE !Celda receptora con cauce
	        cell(cell(n).dest).h(5)=cell(cell(n).dest).h(5)+cell(n).y(4)*arcel/1000.0 !m3
	        ENDIF
        ELSE !CAUCE
            cell(n).vel=cell(n).vel+cell(n).y(2)+cell(n).y(3)+cell(n).y(4)
        ENDIF
     ENDIF
  ENDIF
  !Actualiza el nivel del tanque						
  cell(n).h(2)=cell(n).h(2)-cell(n).y(2) !mm
  cell(n).h(3)=cell(n).h(3)-cell(n).y(3) !mm
  cell(n).h(4)=cell(n).h(4)-cell(n).y(4) !mm
  
!INICIO OCG - Propagación en cárcavas y cauces

  IF (nw(2,ncp).le.cell(n).acum.AND.nw(3,ncp).gt.cell(n).acum) THEN !Transferencia en cárcavas
    !calcula volumen en el cauce y longitud en cárcavas
    cell(n).h(5)=cell(n).h(5)+cell(n).vel*arcel/1000.0     !m³
	!Añade o descuenta el caudal suministrado en un punto
    DO l=1,kadi
      IF (n.eq.aport(l).pos)THEN    !Si coincide la celda con la posición del punto adicional
        cell(n).h(5)=cell(n).h(5)+aport(l).obs(t)*dts    !m³  Cantidad adicional
        IF (cell(n).h(5).lt.0.0) cell(n).h(5)=0.0
      ENDIF
    ENDDO
	!calcula tranferencias de la cárcava
    IF (cell(n).h(5).gt.0.0) THEN
      IF (t.eq.1.or.arsecnew(n).lt.0.0001) arsecnew(n)=(cell(n).u(5)/dts)**(-1./easc) !Valor inicial, corresponde a una velocidad de 1 m/s
      deltaarsec=arsecnew(n)
      DO WHILE(ABS(deltaarsec).gt.(0.005*arsecnew(n)))
        deltaarsec=(1.0/cell(n).lon*(cell(n).h(5)-cell(n).u(5)*arsecnew(n)**(easc+1.0))-arsecnew(n))/(cell(n).u(5)/cell(n).lon*(easc+1.0)*arsecnew(n)**(easc)+1.0)
        arsecnew(n)=arsecnew(n)+deltaarsec
      ENDDO
      salrio=cell(n).u(5)*arsecnew(n)**(easc+1)  !m³ (Volumen de salida, cálculo nuevo con Newton-Raphson)
      veldts=salrio/arsecnew(n)  !(Vel*dts, cálculo nuevo con Newton-Raphson)
      cell(n).h(5)=cell(n).h(5)-salrio !Este es el volumen que queda en la celda después de hacer la transferencia
    ELSE 
      cell(n).h(5)=0.0
      salrio=0.0
      veldts=0.0
    ENDIF
    cell(cell(n).dest).h(5)=cell(cell(n).dest).h(5)+salrio 
  ENDIF

  IF (nw(3,ncp).le.cell(n).acum) THEN  !Transferencia en cauces
    !Calcula volumen en el cauce y longitud en cauces
	cell(n).h(5)=cell(n).h(5)+cell(n).vel*arcel/1000.0
	!Añade o descuenta el caudal suministrado en un punto
    DO l=1,kadi
      IF (n.eq.aport(l).pos)THEN    !Si coincide la celda con la posición del punto adicional
        cell(n).h(5)=cell(n).h(5)+aport(l).obs(t)*dts    !m³  Cantidad adicional
        IF (cell(n).h(5).lt.0.0) cell(n).h(5)=0.0
      ENDIF
    ENDDO
    IF (cell(n).h(5).gt.0.0) THEN
      IF (t.eq.1.or.arsecnew(n).lt.0.0001) arsecnew(n)=(cell(n).u(5)/dts)**(-1./eas) !Valor inicial, corresponde a una velocidad de 1 m/s
      deltaarsec=arsecnew(n)
      DO WHILE(ABS(deltaarsec).gt.(0.005*arsecnew(n)))
        deltaarsec=(1.0/cell(n).lon*(cell(n).h(5)-cell(n).u(5)*arsecnew(n)**(eas+1.0))-arsecnew(n))/(cell(n).u(5)/cell(n).lon*(eas+1.0)*arsecnew(n)**(eas)+1.0)
        arsecnew(n)=arsecnew(n)+deltaarsec
      ENDDO
      salrio=cell(n).u(5)*arsecnew(n)**(eas+1)  !m³ (Volumen de salida, cálculo nuevo con Newton-Raphson)  
      veldts=salrio/arsecnew(n)  !(Vel*dts, cálculo nuevo con Newton-Raphson)
      cell(n).h(5)=cell(n).h(5)-salrio !Este es el volumen que queda en la celda después de hacer la transferencia
    ELSE 
      cell(n).h(5)=0.0
      salrio=0.0
      veldts=0.0
    ENDIF
    
    cell(cell(n).dest).h(5)=cell(cell(n).dest).h(5)+salrio   !m³
    
  ENDIF
  
!FIN OCG
  
  IF (cell(n).acum.lt.nw(2,ncp)) salrio=cell(n).y(2)*arcel/1000.0

  !En puntos de aforo
  DO l=1,naf
    IF (n.eq.aforo(l).pos)THEN  
	  cell(n).q=salrio/dts
      cell(n).vel=veldts/dts
      aforo(l).sim(t)=aforo(l).sim(t)+cell(n).q	 !m³/s  Aporte de la celda
    ENDIF
  ENDDO

  !En puntos OTROS puntos de la cuenca (no aforo)
  DO l=1,knaf
    IF (n.eq.otros(l).pos)THEN
	  cell(n).q=salrio/dts
      cell(n).vel=veldts/dts
      otros(l).sim(t)=otros(l).sim(t)+cell(n).q	 !m³/s  Aporte de la celda
    ENDIF
  ENDDO
  !En puntos de Q adicional
  DO l=1,kadi
    IF (n.eq.aport(l).pos)THEN
	  cell(n).q=salrio/dts
      cell(n).vel=veldts/dts
      aport(l).sim(t)=aport(l).sim(t)+cell(n).q	 !m³/s  Aporte de la celda
    ENDIF
  ENDDO
  
  !controles
  !Guiomar (20/01/2015): voy a añadir en todos los if de este control que el nivel observado no sea -1 para que no se corrijan los datos faltantes
  DO i=1,nemb+vnemb+knemb
    IF (emb(i).caso.gt.0) THEN
      IF (n.eq.emb(i).pos)THEN
        SELECT CASE (emb(i).caso)    
          CASE (1)!Conocido N, calcula S por balance y V por curva
            IF((nivel(i).obs(t).lt.emb(i).h(1)).AND.(nivel(i).obs(t).ne.-1)) THEN
                IF (lang.eq.1) THEN
                    mensaje='El nivel observado del embalse '//TRIM(ADJUSTL(emb(i).nombre))//' es inferior al límite mínimo de la curva de embalse. Se ha corregido.'
                ELSEIF (lang.eq.2) THEN
                    mensaje='The observed level of the reservoir '//TRIM(ADJUSTL(emb(i).nombre))//' is less than the minimum limit of the reservoir curve. It has been corrected'
                ENDIF
                errr=2
              CALL errores
              nivel(i).obs(t) = emb(i).h(1) + 0.01
            ENDIF
          CASE (2)!Conocido V, calcula S por balance y N por curva
          
          CASE (3)!Conocido S, calcula V por balance y N por curva
            
          CASE (4)!Conocido S y N, calcula I por balance y V por curva
            IF((nivel(i).obs(t).lt.emb(i).h(1)).AND.(nivel(i).obs(t).ne.-1)) THEN
                IF (lang.eq.1) THEN
                    mensaje='El nivel observado del embalse '//TRIM(ADJUSTL(emb(i).nombre))//' es inferior al límite mínimo de la curva de embalse. Se ha corregido.'
                ELSEIF (lang.eq.2) THEN
                    mensaje='The observed level of the reservoir '//TRIM(ADJUSTL(emb(i).nombre))//' is less than the minimum limit of the reservoir curve. It has been corrected'
                ENDIF
              errr=2
              CALL errores
              nivel(i).obs(t) = emb(i).h(1) + 0.01
            ENDIF
          CASE (5)!Conocido S y V, calcula I por balance y N por curva

          CASE (6)!Conocido N y V, calcula S por balance
            IF((nivel(i).obs(t).lt.emb(i).h(1)).AND.(nivel(i).obs(t).ne.-1)) THEN
                IF (lang.eq.1) THEN
                    mensaje='El nivel observado del embalse '//TRIM(ADJUSTL(emb(i).nombre))//' es inferior al límite mínimo de la curva de embalse. Se ha corregido.'
                ELSEIF (lang.eq.2) THEN
                    mensaje='The observed level of the reservoir '//TRIM(ADJUSTL(emb(i).nombre))//' is less than the minimum limit of the reservoir curve. It has been corrected'
                ENDIF
              errr=2
              CALL errores
              nivel(i).obs(t) = emb(i).h(1) + 0.01
            ENDIF

          CASE (7)!Conocido S, V y N
            IF((nivel(i).obs(t).lt.emb(i).h(1)).AND.(nivel(i).obs(t).ne.-1)) THEN
                IF (lang.eq.1) THEN
                    mensaje='El nivel observado del embalse '//TRIM(ADJUSTL(emb(i).nombre))//' es inferior al límite mínimo de la curva de embalse. Se ha corregido.'
                ELSEIF (lang.eq.2) THEN
                    mensaje='The observed level of the reservoir '//TRIM(ADJUSTL(emb(i).nombre))//' is less than the minimum limit of the reservoir curve. It has been corrected'
                ENDIF
              errr=2
              CALL errores
              nivel(i).obs(t) = emb(i).h(1) + 0.01
            ENDIF

          CASE (8)!Desconocidos S y N, pero existe linea N, calcula S por Pulso modificado
            
	    END SELECT
	  ENDIF
	ENDIF
  ENDDO
  
  !En puntos de embalses
  DO i=1,nemb+vnemb+knemb
    IF (emb(i).caso.gt.0) THEN
      IF (n.eq.emb(i).pos)THEN
	    qsale1=0.0
        qsale0=0.0
		k=i
        SELECT CASE (emb(i).caso)    
          CASE (1)  
		    !Conocido solo N, calcula S por balance y V por curva
			qemb(k).sim(t)=salrio/dts    !Qentrada segun TETIS m³/s sin embalse
		    qemb(k).bal(t)=qemb(k).sim(t)    !m³/s
		    nivel(k).obs(0)=nivel(k).obs(1) !inicializar cálculo
            ! IF para resolver caso de datos faltantes de N
		    IF (t.ne.1.and.nivel(k).obs(t).eq.-1) THEN !si hay un dato faltante, se utiliza pulso modificado para el deltat
		        niveldado=nivel(k).obs(t-1)
		        CALL cau_sal
		        qemb(k).inst(t-1)=qsale
		        GOTO 116 
		    ENDIF
110		    niveldado=nivel(k).obs(t-1)
			CALL vol_sal
			qsale0=qsale

			niveldado=nivel(k).obs(t)
		    CALL vol_sal
			qsale1=qsale
            volum(k).obs(t)=qsale1/1000000.   !Vol Hm³
		    qemb(k).obs(t)=qemb(k).sim(t)-((qsale1-qsale0)/dts)   !m³/s 
            !Otras variables
			volum(k).sim(t)=volum(k).obs(t)       !Hm³ (U=V)
			nivel(k).sim(t)=nivel(k).obs(t)       !m   (L=N)
		  CASE (2)		
		    !Conocido solo V, calcula S por balance y N por curva
			qemb(k).sim(t)=salrio/dts    !Qentrada segun TETIS m³/s sin embalse
		    qemb(k).bal(t)=qemb(k).sim(t)    !m³/s
		    volum(k).obs(0)=volum(k).obs(1) !Para inicializar cálculo
            ! IF para resolver caso de datos faltantes de V
		    IF (t.ne.1.and.volum(k).obs(t).eq.-1) THEN !si hay un dato faltante, se utiliza pulso modificado para el deltat
		        niveldado=nivel(k).obs(t-1)
		        CALL cau_sal
		        qemb(k).inst(t-1)=qsale
		        GOTO 116 
		    ENDIF
111		    volin=volum(k).obs(t)*1000000.   !m³
            CALL niv_sal
  	        nivel(k).obs(t)=qsale	  !m
		    qemb(k).obs(t)=qemb(k).sim(t)-((volum(k).obs(t)-volum(k).obs(t-1))*1000000./dts)   !m³/s 
            !Otras variables
			volum(k).sim(t)=volum(k).obs(t)       !Hm³ (U=V)
			nivel(k).sim(t)=nivel(k).obs(t)       !m   (L=N)
		  CASE (3)
		    !Conocido "S" (y eventualmente el nivel inicial) calcula V por balance y N por curva
            qemb(k).sim(t)=salrio/dts    !Qentrada segun TETIS m³/s sin embalse
            qemb(k).bal(t)=qemb(k).sim(t)    !m³/s  Qentrada (I=A)
            !Bloque IF para inicializar el cálculo. Se toma como primer nivel el que haya especificado en el archivo de entrada, si es que está especificado
!Guiomar(24/02/2015): Hago cambio para que se tome como primer nivel el que haya especificado en el archivo de entrada si es que está especificado
		    IF (t.eq.1) THEN
                IF ((nivel(k).obs(1)).ne.0.0) THEN
                    nivel(k).obs(0)=nivel(k).obs(1)
                ELSE
		            qsale=qemb(k).obs(1)	   !m³/s 
			        CALL caud_ent !De aquí sale el volumen instantáneo al inicio del intervalo
		            CALL niv_sal
		            nivel(k).obs(0)=qsale
                ENDIF
		    ENDIF
            !Bloque IF para resolver caso de datos faltantes de S
		    IF (t.ne.1.and.qemb(k).obs(t).eq.-1) THEN !si hay un dato faltante, se mantiene S del tiempo anterior
		        qemb(k).obs(t)=qemb(k).obs(t-1)
112		        niveldado=nivel(k).obs(t-1)
		    ENDIF
			CALL vol_sal
		    qsale0=qsale
		    !Controlar que el caudal objetivo esté en el rango de descarga para ese nivel, sino, se adopta el valor limitante que corresponde
		    IF (qemb(k).obs(t).lt.emb(i).out(w,1)) qemb(k).obs(t)=emb(i).out(w,1)
		    IF (qemb(k).obs(t).gt.emb(i).out(w,2)) qemb(k).obs(t)=emb(i).out(w,2)

			volin=qemb(k).sim(t)*dts-qemb(k).obs(t)*dts+qsale0   !m³
            volum(k).obs(t)=volin/1000000.   !Hm³
		    CALL niv_sal
		    nivel(k).obs(t)=qsale	  !m
            !Otras variables
			volum(k).sim(t)=volum(k).obs(t)       !Hm³ (U=V)
			nivel(k).sim(t)=nivel(k).obs(t)       !m   (L=N)
          CASE (4)
	        !Conocidos Qsalida "S" y niveles "N", calcula Qentrada "I" por balance y V por curva
            qemb(k).sim(t)=salrio/dts    !Qentrada segun TETIS m³/s sin embalse
		    nivel(k).obs(0)=nivel(k).obs(1) !Inicializar cálculo
            !Bloque IF para resolver caso de datos faltantes de S y/o N
		    IF (t.ne.1.and.qemb(k).obs(t).eq.-1.and.nivel(k).obs(t).eq.-1) THEN !Si S y N son faltantes simultáneamente, se mantiene S del tiempo anterior
		        qemb(k).obs(t)=qemb(k).obs(t-1)
		        GOTO 112
		    ELSEIF (t.ne.1.and.qemb(k).obs(t).eq.-1.and.nivel(k).obs(t).ne.-1) THEN !si sólo S es faltante utilizo caso 1 para el deltat
		       GOTO 110
		    ELSEIF (t.ne.1.and.qemb(k).obs(t).ne.-1.and.nivel(k).obs(t).eq.-1) THEN !si sólo N es faltante utilizo caso 3 para el deltat
		       GOTO 112
		    ENDIF
113		    niveldado=nivel(k).obs(t-1)
	        CALL vol_sal
            qsale0=qsale
		    niveldado=nivel(k).obs(t)
            CALL vol_sal
            qsale1=qsale
			qemb(k).bal(t)=((qsale1-qsale0)/dts)+qemb(k).obs(t)  !m³/s  Qentrada(por balance conocido St)
            volum(k).obs(t)=qsale1/1000000.   !Vol Hm³
            !Otras variables
			volum(k).sim(t)=volum(k).obs(t)       !Hm³ (U=V)
			nivel(k).sim(t)=nivel(k).obs(t)       !m   (L=N)
		  CASE (5)		
		    !Conocido "S" y "V", calcula Qentrada "I" por balance, y N por curva
            qemb(k).sim(t)=salrio/dts    !Qentrada "A" segun TETIS m³/s sin embalse
		    volum(k).obs(0)=volum(k).obs(1) !inicializar cálculo
            !Bloque IF para resolver caso de datos faltantes de S y/o V
		    IF (t.ne.1.and.qemb(k).obs(t).eq.-1.and.volum(k).obs(t).eq.-1) THEN !Si S y V son faltantes simultáneamente, se mantiene S del tiempo anterior
		        qemb(k).obs(t)=qemb(k).obs(t-1)
		        GOTO 112
		    ELSEIF (t.ne.1.and.qemb(k).obs(t).eq.-1.and.volum(k).obs(t).ne.-1) THEN !si sólo S es faltante utilizo caso 2 para el deltat
		       GOTO 111
		    ELSEIF (t.ne.1.and.qemb(k).obs(t).ne.-1.and.volum(k).obs(t).eq.-1) THEN !si sólo V es faltante utilizo caso 3 para el deltat
		       GOTO 112
		    ENDIF
114		    qemb(k).bal(t)=(1000000.*(volum(k).obs(t)-volum(k).obs(t-1))/dts)+qemb(k).obs(t) 
		                                    !m³/s  Qentrada(por balance conocido St)
		    volin=volum(k).obs(t)*1000000.  !m³
		    CALL niv_sal
		    nivel(k).obs(t)=qsale  !m
            !Otras variables
			volum(k).sim(t)=volum(k).obs(t)       !Hm³ (U=V)
			nivel(k).sim(t)=nivel(k).obs(t)       !m   (L=N)
		  CASE (6)
 		    !Conocido N y V, calcula S por balance 
            qemb(k).sim(t)=salrio/dts    !Qentrada segun TETIS m³/s sin embalse
		    qemb(k).bal(t)=qemb(k).sim(t)    !m³/s
		    nivel(k).obs(0)=nivel(k).obs(1) !inicializar cálculo
            !Bloque IF para resolver caso de datos faltantes de N y/o V
		    IF (t.ne.1.and.nivel(k).obs(t).eq.-1.and.volum(k).obs(t).eq.-1) THEN !!! si N y V son faltantes simultáneamente, se utiliza pulso modificado para el deltat
		        niveldado=nivel(k).obs(t-1)
		        CALL cau_sal
		        qemb(k).inst(t-1)=qsale
		        GOTO 116
		    ELSEIF (t.ne.1.and.nivel(k).obs(t).eq.-1.and.volum(k).obs(t).ne.-1) THEN !si sólo N es faltante, se utiliza caso 2 para el deltat
		       GOTO 111
		    ELSEIF (t.ne.1.and.nivel(k).obs(t).ne.-1.and.volum(k).obs(t).eq.-1) THEN !si sólo V es faltante, se utiliza caso 1 para el deltat
		       GOTO 110
		    ENDIF
115		    niveldado=nivel(k).obs(t-1)
		    CALL vol_sal
			qsale0=qsale
		    niveldado=nivel(k).obs(t)
		    CALL vol_sal
		    qemb(k).obs(t)=qemb(k).sim(t)-((qsale-qsale0)/dts)   !m³/s 
            !Otras variables
			volum(k).sim(t)=volum(k).obs(t)       !Hm³ (U=V)
			nivel(k).sim(t)=nivel(k).obs(t)       !m   (L=N)
          CASE (7)
		    !Conocidos Qsalida "S", Volumen "V" y Niveles "N"
		    qemb(k).sim(t)=salrio/dts    !Qentrada segun TETIS m³/s sin embalse
		    volum(k).obs(0)=volum(k).obs(1) !inicializar cálculo
            !Bloque IF para resolver caso de datos faltantes de S y/o N y/o V
		    IF (t.ne.1) THEN
		        IF(qemb(k).obs(t).eq.-1.and.volum(k).obs(t).eq.-1.and.nivel(k).obs(t).eq.-1) THEN !Las tres variables son faltantes simultáneamente: Se utiliza pulso modificado para el deltat
		           niveldado=nivel(k).obs(t-1)
		           CALL cau_sal
		           qemb(k).inst(t-1)=qsale
		           GOTO 116
		        ELSEIF (qemb(k).obs(t).eq.-1.and.volum(k).obs(t).ne.-1.and.nivel(k).obs(t).ne.-1) THEN !sólo S es faltante: Se utiliza caso 6 para el deltat
		            GOTO 115
		        ELSEIF (qemb(k).obs(t).ne.-1.and.volum(k).obs(t).eq.-1.and.nivel(k).obs(t).ne.-1) THEN !sólo V es faltante: Se utiliza caso 4 para el deltat
		            GOTO 113
		        ELSEIF (qemb(k).obs(t).ne.-1.and.volum(k).obs(t).ne.-1.and.nivel(k).obs(t).eq.-1) THEN !sólo N es faltante: Se utiliza caso 5 para el deltat
		            GOTO 114
		        ELSEIF (qemb(k).obs(t).eq.-1.and.volum(k).obs(t).eq.-1.and.nivel(k).obs(t).ne.-1) THEN !faltan S y V simultáneamente: Se utiliza caso 1 para el deltat
		            GOTO 110
		        ELSEIF (qemb(k).obs(t).eq.-1.and.volum(k).obs(t).ne.-1.and.nivel(k).obs(t).eq.-1) THEN !faltan S y N simultáneamente: Se utiliza caso 2 para el deltat
		            GOTO 111
		        ELSEIF (qemb(k).obs(t).ne.-1.and.volum(k).obs(t).eq.-1.and.nivel(k).obs(t).eq.-1) THEN !faltan V y N simultáneamente: Se utiliza caso 3 para el deltat
		            GOTO 112
		        ENDIF
		    ENDIF

		    qemb(k).bal(t)=1000000.*(volum(k).obs(t)-volum(k).obs(t-1))/dts+qemb(k).obs(t) !m³/s
            !Otras variables
			volum(k).sim(t)=volum(k).obs(t)       !Hm³ (U=V)
			nivel(k).sim(t)=nivel(k).obs(t)       !m   (L=N)
		  CASE (8)		
	        !Desconocidos "S" y "N", pero existe linea N, calcula S por Pulso modificado
		    IF (t.eq.1) THEN
		      niveldado=nivel(k).obs(0)
			  CALL cau_sal
		      qemb(k).obs(0)=qsale	  !m³/s  Qsalida (S)
		      qemb(k).inst(0)=qsale	  !m³/s  Qsalida instantáneo al inicio del intervalo (S)
		      qemb(k).sim(0)=salrio/dts	  !m³/s  Qentrada  !!!asumo que al principio del primer intevalo llega lo mismo que al final, para comenzar el algoritmo
              qemb(k).sim(1)=salrio/dts    !Qentrada al final del primer intervalo (obtenido de qmedio TETIS m³/s, sin embalse)  !!!modificado Camilo!!!!
            ELSE
              qemb(k).sim(t)=salrio/dts
		    ENDIF
116		    qemb(k).bal(t)=qemb(k).sim(t)   !caud(t,l)    !m³/s  Q entra (I=A)
		    qsale=qemb(k).inst(t-1)	   !m³/s
			CALL caud_ent !De aquí sale el volumen instantáneo al inicio del intervalo
		    pder=2*qemb(k).sim(t)+(2.0*volin/dts-qsale)
            volant=volin    !Se almacena en esta variable el volumen instantáneo al inicio del intervalo

		    !Algoritmo real de pulso modificado
            nb=0
		    DO WHILE(pder.ge.emb(k).fpul(nb).and.nb.lt.emb(k).datos)
		        nb=nb+1
		    ENDDO
		    IF (nb.gt.1.and.nb.lt.emb(k).datos) THEN
		        fpulmin=emb(k).fpul(nb-1)
		        fpulmax=emb(k).fpul(nb)
		        qsale0=emb(k).out(nb-1,2)
		        qsale1=emb(k).out(nb,2)
		        qsale=qsale0+(qsale1-qsale0)*(Pder-fpulmin)/(fpulmax-fpulmin)
		    ELSEIF (nb.eq.1) THEN !extrapolación a la izquierda
		        fpulmin=0
		        fpulmax=emb(k).fpul(1)
		        qsale0=0.
		        qsale1=emb(k).out(1,2)
		        IF (fpulmax.lt.0.001.or.emb(k).out(1,2).lt.0.0001) THEN
		            qsale=0.
		        ELSE
		            qsale=qsale0+(qsale1-qsale0)*(Pder-fpulmin)/(fpulmax-fpulmin)
		        ENDIF
		    ELSE   !extrapolación a la derecha
                fpulmin=emb(k).fpul(nb-1)
		        fpulmax=emb(k).fpul(nb)
		        qsale0=emb(k).out(nb-1,2)
		        qsale1=emb(k).out(nb,2)
		        qsale=qsale0+(qsale1-qsale0)*(Pder-fpulmin)/(fpulmax-fpulmin)
		    ENDIF
		    qemb(k).inst(t)=qsale  !m³/s    !!!Caudal de salida instantáneo al final del intervalo, Pulso modificado
		    volin=(pder-qsale)*dts/2. !!!De aquí sale el volumen instantáneo al final del intervalo (volin)
		    qemb(k).obs(t)=qemb(k).sim(t)-(volin-volant)/dts !!!Caudal de salida medio en el intervalo cumpliendo balance
		    qemb(k).inst(t)=qsale  !m³/s    !Caudal de salida instantáneo al final del intervalo, Pulso modificado
		    volin=(pder-qsale)*dts/2. !De aquí sale el volumen instantáneo al final del intervalo (volin)   
		    qemb(k).obs(t)=qemb(k).sim(t)-(volin-volant)/dts !Caudal de salida medio en el intervalo cumpliendo balance
            CALL niv_sal
		    nivel(k).obs(t)=qsale      !m   Nivel de embalse instantáneo al final del intervalo, Pulso modificado
            volum(k).obs(t)=volin/1000000. 
            !Otras variables
			volum(k).sim(t)=volum(k).obs(t)       !Hm³ (U=V)
			nivel(k).sim(t)=nivel(k).obs(t)       !m   (L=N)

        END SELECT
	    cell(cell(n).dest).h(5)=cell(cell(n).dest).h(5)-salrio
        salrio=qemb(k).obs(t)*dts   !m³  
	    cell(cell(n).dest).h(5)=cell(cell(n).dest).h(5)+salrio     !m³
        cell(n).q=salrio/dts	   !m³/s  Aporte de la celda
        cell(n).vel=veldts/dts	   !m/s
	  ENDIF
	ENDIF
  ENDDO
  
    !Control
  DO i=1,nemb+vnemb+knemb
    IF (emb(i).caso.gt.0) THEN
      IF (n.eq.emb(i).pos)THEN
        SELECT CASE (emb(i).caso)    
          CASE (1)!Conocido N, calcula S por balance y V por curva

          CASE (2)!Conocido V, calcula S por balance y N por curva
            IF(nivel(i).obs(t).lt.emb(i).h(1)) THEN
                IF (lang.eq.1) THEN
                    mensaje='El nivel observado del embalse '//TRIM(ADJUSTL(emb(i).nombre))//' es inferior al límite mínimo de la curva de embalse. Se ha corregido'
                ELSEIF (lang.eq.2) THEN
                    mensaje='The observed level of the reservoir '//TRIM(ADJUSTL(emb(i).nombre))//' is less than the minimum limit of the reservoir curve. It has been corrected'
                ENDIF
              errr=2
              CALL errores
              nivel(i).obs(t) = emb(i).h(1) + 0.01
            ENDIF
          CASE (3)!Conocido S, calcula V por balance y N por curva
            IF(nivel(i).obs(t).lt.emb(i).h(1)) THEN
                IF (lang.eq.1) THEN
                    mensaje='El nivel observado del embalse '//TRIM(ADJUSTL(emb(i).nombre))//' es inferior al límite mínimo de la curva de embalse. Se ha corregido'
                ELSEIF (lang.eq.2) THEN
                    mensaje='The observed level of the reservoir '//TRIM(ADJUSTL(emb(i).nombre))//' is less than the minimum limit of the reservoir curve. It has been corrected'
                ENDIF
              errr=2
              CALL errores
              nivel(i).obs(t) = emb(i).h(1) + 0.01
            ENDIF

          CASE (4)!Conocido S y N, calcula I por balance y V por curva

          CASE (5)!Conocido S y V, calcula I por balance y N por curva
            IF(nivel(i).obs(t).lt.emb(i).h(1)) THEN
                IF (lang.eq.1) THEN
                    mensaje='El nivel observado del embalse '//TRIM(ADJUSTL(emb(i).nombre))//' es inferior al límite mínimo de la curva de embalse. Se ha corregido'
                ELSEIF (lang.eq.2) THEN
                    mensaje='The observed level of the reservoir '//TRIM(ADJUSTL(emb(i).nombre))//' is less than the minimum limit of the reservoir curve. It has been corrected'
                ENDIF
              errr=2
              CALL errores
              nivel(i).obs(t) = emb(i).h(1) + 0.01
            ENDIF
          CASE (6)!Conocido N y V, calcula S por balance

          CASE (7)!Conocido S, V y N

          CASE (8)!Desconocidos S y N, pero existe linea N, calcula S por Pulso modificado
            IF(nivel(i).obs(t).lt.emb(i).h(1)) THEN
                IF (lang.eq.1) THEN
                    mensaje='El nivel observado del embalse '//TRIM(ADJUSTL(emb(i).nombre))//' es inferior al límite mínimo de la curva de embalse. Se ha corregido'
                ELSEIF (lang.eq.2) THEN
                    mensaje='The observed level of the reservoir '//TRIM(ADJUSTL(emb(i).nombre))//' is less than the minimum limit of the reservoir curve. It has been corrected'
                ENDIF
              errr=2
              CALL errores
              nivel(i).obs(t) = emb(i).h(1) + 0.01
            ENDIF
            IF(nivel(i).sim(t).lt.emb(i).h(1)) THEN
                IF (lang.eq.1) THEN
                    mensaje='El nivel simulado del embalse '//TRIM(ADJUSTL(emb(i).nombre))//' es inferior al límite mínimo de la curva de embalse. Se ha corregido'
                ELSEIF (lang.eq.2) THEN
                    mensaje='The simulated level of the reservoir '//TRIM(ADJUSTL(emb(i).nombre))//' is less than the minimum limit of the reservoir curve. It has been corrected'
                ENDIF
              errr=2
              CALL errores
              nivel(i).sim(t) = emb(i).h(1) + 0.01
            ENDIF
	    END SELECT
	  ENDIF
	ENDIF
  ENDDO
  
  
  !Cálculo del volumen máximo a sección llena para cada celda cauce o carcava
  IF (nw(2,ncp).gt.cell(n).acum) THEN  !para celdas de ladera (sin canal)--> Volumen=0
    cell(n).qb=0.0
  ENDIF
  IF (nw(2,ncp).le.cell(n).acum.AND.nw(3,ncp).gt.cell(n).acum) THEN  !para celdas de cárcava o cauce efímero
    cell(n).qb=dc(1,ncp)*((arcelkm*cell(n).acum)**ec(1,ncp))
  ENDIF
  IF (nw(3,ncp).le.cell(n).acum) THEN !para celdas con cauce permanente
    cell(n).qb=d(1,ncp)*((arcelkm*cell(n).acum)**ec(1,ncp))
  ENDIF
 !Balance hidrológico en mm
  balanc(t,2)=cell(n).x(6)+balanc(t,2) !throughfall
  balanc(t,3)=cell(n).x(2)+balanc(t,3) !excedente
  balanc(t,4)=cell(n).x(3)+balanc(t,4) !infiltración
  balanc(t,5)=cell(n).x(4)+balanc(t,5) !percolación
  balanc(t,6)=cell(n).x(5)+balanc(t,6) !pérdidas subterraneas
  balanc(t,7)=cell(n).y(6)+balanc(t,7) !evaporación directa
  balanc(t,8)=cell(n).y(1)+balanc(t,8) !evaporación suelo + transpiración
  
  balanc(t,28)=cell(n).x(7)+balanc(t,28) !Riego (Cris 03/2017)
  !balanc(t,29)=cell(n).x(9)+balanc(t,29) !Pérdidas subterráneas de las celdas con acuífero conectado (Cris 03/2017) 
  !balanc(t,30)=cell(n).x(10)+balanc(t,30) !Pérdidas subterráneas de las celdas no conectadas (Cris 03/2017)

  IF (config(5)) THEN
      balanc(t,23)=cell(n).h(8)+balanc(t,23)
      balanc(t,24)=cell(n).x(8)+balanc(t,24)
      balanc(t,25)=cell(n).tr1+balanc(t,25)
      balanc(t,26)=cell(n).tr2+balanc(t,26)
      balanc(t,27)=cell(n).es+balanc(t,27)
  ENDIF
  IF (cell(cell(n).dest).acum.ge.nw(2,ncp)) balanc(t,9)=cell(n).y(2)+balanc(t,9) !ED
  
  IF (modulos2(2)) THEN
      IF (cell(n).codkarst.eq.1)then
          IF(logicman)THEN !MANANTIAL
                balanc(t,10)=cell(n).y(3)+balanc(t,10)!Flujo Sub-Superficial
                balanc(t,11)=cell(n).y(4)+balanc(t,11) !Flujo Base
                balancexp(t)=(cell(n).y(3)+cell(n).y(4))/100*cmex  !Aumento o descenso del acuífero de karst fuera de la cuenca
          ENDIF
      ELSE 
         IF(logicman2)THEN !CELDA DESTINO MANANTIAL
                balanc(t,10)=cell(n).y(3)+balanc(t,10)!Flujo Sub-Superficial
                balanc(t,11)=cell(n).y(4)+balanc(t,11) !Flujo Base
                !balanc(t,21)=(cell(n).y(3)+cell(n).y(4))/100*cmex  !incremento o decremento de acuífero de karst fuera de la cuenca
         ENDIF
         IF(cell(cell(n).dest).codkarst.ne.1)THEN
            IF(cell(cell(n).dest).acum.ge.nw(2,ncp)) balanc(t,10)=cell(n).y(3)+balanc(t,10) !FSS
            IF(cell(cell(n).dest).acum.ge.nw(3,ncp)) balanc(t,11)=cell(n).y(4)+balanc(t,11) !FB
         ENDIF
      ENDIF
  ELSE
    IF(cell(cell(n).dest).acum.ge.nw(2,ncp)) balanc(t,10)=cell(n).y(3)+balanc(t,10) !FSS
    IF(cell(cell(n).dest).acum.ge.nw(3,ncp)) balanc(t,11)=cell(n).y(4)+balanc(t,11) !FB
  ENDIF
  
  balanc(t,12)=cell(n).h(6)+balanc(t,12) !almacenamiento de las hojas
  balanc(t,13)=cell(n).h(1)+balanc(t,13) !almacenamiento capilar
  balanc(t,14)=cell(n).h(2)+balanc(t,14) !almacenamiento superficial
  balanc(t,15)=cell(n).h(3)+balanc(t,15) !almacenamiento suelo 1
  balanc(t,16)=cell(n).h(4)+balanc(t,16) !almacenamiento suelo 2
  balanc(t,17)=cell(n).h(5)*1000.0/arcel+balanc(t,17)
  
  balanc(t,18)=cell(n).h(0)+balanc(t,18)
  balanc(t,19)=cell(n).x(0)+balanc(t,19) ! Temperatura media
  balanc(t,20)=cell(n).y(0)+balanc(t,20) ! Fusión de nieve
  balanc(t,21)=cell(n).h(0)+balanc(t,21) ! Almacenamiento de nieve
  balanc(t,22)=cell(n).exfiltr+balanc(t,22) ! Volumen de agua que se transfiere del tanque 3 al tanque 2
  balanc(t,22)= balanc(t,22)+ 0.0

  IF (cell(n).qb.ne.0.and.t.eq.nt) then
    contcauce=contcauce+1
    balancqb(t)=cell(n).h(5)/(cell(n).qb*dts)+ balancqb(t) !relación entre volumen final del cauce o carcava y su volumen a sección llena
  end if
  !prueba para celdas que superan el volumen a sección llena
  !if(cell(n).h(5)>cell(n).qb.and.t.eq.nt)then  !prueba para ver cuando el canal va desbordado
    !continue
  !end if
 
  if(n.eq.ncel)then
    balancsalrio=balancsalrio+(cell(ncel).q*dts) !m3
  endif
  IF (config(4)) CALL sim_celdased
  IF (modulos2(3)) CALL sim_celdanitr

ENDDO
    
!(Vicente)Riego. Se inicializa para cada paso de tiempo
contarPasoTiempoRegado=0        


END SUBROUTINE

!**************************************************************************************

SUBROUTINE fin_ejec
!USE DFLIB
USE modtet
IMPLICIT NONE

INTEGER errcod
Integer STATUS
real GETCWD

INQUIRE (FILE='filessp.txt',EXIST=existe) 
IF (.NOT.existe) goto 200

!Lee nombres de los ficheros a utilizar
REWIND(9)
OPEN(9,file='filessp.txt')
READ(9,'(a128)') dirtra
ldirtra=len_trim(dirtra)
!if(dirtra(ldirtra:ldirtra).ne.'\')dirtra(ldirtra+1:ldirtra+1)='\'
if(dirtra(ldirtra:ldirtra).ne.path_separator)dirtra(ldirtra+1:ldirtra+1)=path_separator
CLOSE(9)
artem=TRIM(ADJUSTL(dirtra))//'Endtetis.txt'

!resul=DELFILESQQ(artem)
!Subtituimos la rutina de eliminar fichero para no emplear librerias(Vicente.Nov-2019)
CALL deletefich(artem)

OPEN(8,FILE=artem)
CALL DATE_AND_TIME(dia,hora)
WRITE(8,*)'Fecha:'//dia(7:8)//'/'//dia(5:6)//'/'//dia(1:4)//' Hora:'//hora(1:2)//':'//hora(3:4)//':'//hora(5:10)
WRITE(8,*)strings(801)
CLOSE(8)

goto 99

200 STATUS=GETCWD(dirtra)
ldirtra=len_trim(dirtra)
if(dirtra(ldirtra:ldirtra).ne.path_separator)dirtra(ldirtra+1:ldirtra+1)=path_separator
artem=TRIM(ADJUSTL(dirtra))//'Endtetis.txt'
OPEN(8,FILE=artem)
CALL DATE_AND_TIME(dia,hora)
WRITE(8,*)'Fecha:'//dia(7:8)//'/'//dia(5:6)//'/'//dia(1:4)//' Hora:'//hora(1:2)//':'//hora(3:4)//':'//hora(5:10)
WRITE(8,*)strings(801)
CLOSE(8)


99  END SUBROUTINE
    
