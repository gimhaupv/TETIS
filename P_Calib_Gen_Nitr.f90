! ***************************************************************
! * Programa para la calibración automática de los parámetros
! * o factores correctores R() del modelo TETIS-SP de simulación
! * usando el SCE-UA propuesto por Duan et al (1992)
!*  Ultima actualización: Julio 2016
! ***************************************************************
program calib_gen_nitr
!USE DFLIB
USE IFPORT
USE modtet
IMPLICIT NONE
character copyFicheros*256

!INTEGER nx
!CHARACTER*20 ffich1,ffich2
!LOGICAL(KIND=4)CHECKED
!INTEGER ist,massimocodveg
!CHARACTER*30 arg
!    
!!Se hace así para que si hay error, lang tenga un valor y pueda escribir. Por defecto está en inglés
!lang=2
!!llama la subrutina que define los mensajes de error genéricos, los que no neceistan más información que los nombres de archivo
!CALL labels
!
!!Lee nombres de los ficheros a utilizar, por el momento no general el el FILESSP!!!!! filessp tiene que estár!
!CALL lecfiles(dirtra,arch,sale)
!IF (sale.eq.2) GOTO 94
!
!CALL lee_settings
!IF (sale.eq.2) GOTO 94
!IF (printascii) printascii='F'
!
!CALL DATE_AND_TIME(dia,hora)
!CALL write_date
!WRITE(*,*)''
!
!!llama la subrutina que define los mensajes de error genéricos, los que no neceistan más información que los nombres de archivo
!CALL labels
!
!res=DELFILESQQ(TRIM(ADJUSTL(dirtra))//TRIM(ADJUSTL('errores.txt')))
!artem=TRIM(ADJUSTL(dirtra))//TRIM(ADJUSTL('~errores.txt'))  
!OPEN(22,file=artem)                                        
!WRITE(22,'(A54)') strings(802)
!CLOSE(22)
!
!sim2=.false.
!simevents=.false.
!
!If (config(5)) CALL leearchveg(dirtra,archveg,sale)
!IF (sale.eq.2) GOTO 95
!
!
!!Lee parametros geomorfologicos
!CALL lee_pargeo
!IF (sale.eq.2) GOTO 95
!
!
!    
!IF (config(4)) THEN
!    !Lee nombre de los ficheros con sedimentos
!    CALL leearchsed(dirtra,archsed,sale)
!    IF (sale.eq.2) GOTO 95
!ENDIF
!    
!CALL leearchnit(dirtra,archnit,sale)
!
!
!CALL labels
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!modificaciones para calibración multi-evento Pais Vasco!!!!!!!!!!!!!!!!!!!!!!!!!
!IF (simevents)THEN
!
!    !pregunta si existe el fichero con los nombres de todos los eventos y respectivos hantec
!    artem=TRIM(ADJUSTL(dirtra))//'MULTICALIB.TXT'  !nobre obligatorio de fichero de entrada!!!
!    INQUIRE (FILE=artem,EXIST=existe)
!    IF (.NOT.existe) THEN
!        mensaje=strings(819)
!        errr=1
!        CALL errores(errr,mensaje,lang)
!        CALL DATE_AND_TIME(dia,hora)
!        CALL write_date
!        GOTO 94
!    ENDIF
!
!    OPEN (39,file=artem) !abre el fichero multicalib.txt
!    ios=0
!    nexe=0
!    DO WHILE (ios.ne.-1)
!        READ(39,*,iostat=ios) 
!        IF (ios.ne.-1) nexe=nexe+1  !cuenta el número de eventos que se quieren calibrar automaticamente a la vez
!    ENDDO
!    REWIND(39)
!
!    IF(ALLOCATED(estadohum))DEALLOCATE(estadohum)
!    IF(ALLOCATED(multievento))DEALLOCATE(multievento)
!    IF(ALLOCATED(nstep))DEALLOCATE(nstep)
!    IF(ALLOCATED(datainiz))DEALLOCATE(datainiz)
!    IF(ALLOCATED(orainiz))DEALLOCATE(orainiz)
!    ALLOCATE(estadohum(nexe),multievento(nexe),nstep(nexe),datainiz(nexe),orainiz(nexe))
!    
!    DO nx=1,nexe
!        READ(39,*)ffich1,ffich2
!        estadohum(nx)=TRIM(ADJUSTL(dirtra))//TRIM(ADJUSTL(ffich1))     !Hantec.sds
!        multievento(nx)=TRIM(ADJUSTL(dirtra))//TRIM(ADJUSTL(ffich2))   !epsiodio
!    END DO
!    close(39)
!    
!    
!    if(config(2))then 
!        call megaevent_col
!    else
!        call megaevent_cedex
!    endif
!    
!    
!    arch(5)=TRIM(ADJUSTL(dirtra))//'MULTIEVENTO.TXT'
!ENDIF       
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!Fine!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!!Cuenta el número de puntos de control
!ncon=0
!OPEN(9,file=arch(21),status='old',err=206)
!ios=0
!DO WHILE (ios.ne.-1)
!    READ(9,*,iostat=ios) j
!    IF (ios.ne.-1) ncon=ncon+1
!ENDDO
!REWIND(9)
!ALLOCATE (control(ncon))
!
!CALL lee_pcon
!IF (sale.eq.2) GOTO 95
!    
!!Lee fichero con factores de cultivo mensuales para el calculo de la ETP
!INQUIRE (FILE=arch(6),EXIST=existe)
!    IF (existe)THEN
!    CALL leefactoret !Ojo que acá se vuelven a leer los settings, por eso no conviene cambiar antes el config(4) en caso de ser .true.!!!!!!
!    ELSE
!    IF (ALLOCATED(lamb)) DEALLOCATE(lamb)
!    ALLOCATE(lamb(1,13))
!  
!    OPEN(28,file=arch(6),status='new')
!  
!    WRITE(28,*)'*'
!    WRITE(28,*)'*'
!   
!    lamb(1,1)=1.0
!    lamb(1,2)=1.0
!    lamb(1,3)=1.0
!    lamb(1,4)=1.0
!    lamb(1,5)=1.0
!    lamb(1,6)=1.0
!    lamb(1,7)=1.0
!    lamb(1,8)=1.0
!    lamb(1,9)=1.0
!    lamb(1,10)=1.0
!    lamb(1,11)=1.0
!    lamb(1,12)=1.0
!    lamb(1,13)=0.0
!    WRITE(28,980)(lamb(1,j), j=1,13)
!    CLOSE(28)
!    ENDIF 
!980 FORMAT (<13>F12.5)
!
!!Lee fichero con REGADIO, es un fichero con: 1) cantidad de riego en mm para cada mes (son 12)
!! y 2) la segunda fila corresponde a la frecuencia de riego en dias
!xriego=0.0
!friego=0.0
!k=0
!erie=0
!OPEN(10,file=arch(22),status='old',err=48)
!ios=0
!DO WHILE (ios.ne.-1)
!    READ(10,*,iostat=ios) j
!    READ(10,*,iostat=ios) j
!    IF (ios.ne.-1) k=k+1
!ENDDO
!REWIND(10)
!erie=k
!DO i=1,erie
!    READ(10,*,err=249)(xriego(i,j),j=1,12)  
!    READ(10,*,err=249)(friego(i,j),j=1,12)  
!ENDDO
!48 CLOSE(10)
!!Lee el tipo de riego de cada zona (1 Gravedad, 2 Aspersión, 3 Goteo)  Cris 03/2017 (Falta ponerlo todo bien, errores, archivo en filessp...)
!ALLOCATE(tiporiego(erie))
!tiporiego=0
!OPEN(10,file=arch(38),status='old',err=49)
!Do i=1,erie
!    Read(10,*,err=249) tiporiego(i)
!End do
!49 Close(10)
!
!!Lee topologia, propiedades del suelo y tipologia
!OPEN(14,file=arch(3),status='old',err=209)
!!
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
!!Lee fichero con caracteristicas de los azudes (para SEDIMENTOS)
!IF (config(4)) THEN
!    IF (trapeff) CALL leeTE
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
!    IF (ksedq.gt.0) THEN
!    ALLOCATE(aforosed(ksedq))
!    ENDIF
!    IF (kadised1.gt.0) THEN  ! Añadido Cris
!    ALLOCATE(aportsed1(kadised1))
!    ENDIF
!    IF (kadised2.gt.0) THEN
!    ALLOCATE(aportsed2(kadised2))
!    ENDIF
!    IF (kadised3.gt.0) THEN
!    ALLOCATE(aportsed3(kadised3))
!    ENDIF
!ENDIF
!    
!!If (modulos2(3)) then !(Vicente) No necesario pues ya se comprueba previamente
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
!!End if
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
!    config(5)=.false.
!ENDIF
!    
!!Lee fichero con caracteristicas de los azudes (para SEDIMENTOS)
!IF (config(4)) THEN
!    IF (trapeff) THEN
!    CALL leeTE
!    CALL lee_emb1TE
!    DO i=1,numpresas
!        k=dam(i).datos
!        ALLOCATE(dam(i).h(0:k),dam(i).vol(0:k))
!    ENDDO
!    CALL lee_emb2TE
!    CALL leeTEini
!    ENDIF
!ENDIF
!        
!DO n=1,ncel
!    ALLOCATE(cell(n).ind_int_p(nest),cell(n).fac_int_p(nest))
!    cell(n).ind_int_p=0
!    cell(n).fac_int_p=0.0
!ENDDO
!nafn = kno+kam+kni !(Vicente. Num estaciones Nitrogeno)
!    
!IF(ALLOCATED(balanc)) DEALLOCATE(balanc)
!IF(ALLOCATED(balanc_sed)) DEALLOCATE(balanc_sed)
!IF(ALLOCATED(balanc_nitr)) DEALLOCATE(balanc_nitr)
!IF(ALLOCATED(balancqb)) DEALLOCATE(balancqb)
!IF(ALLOCATED(balancexp)) DEALLOCATE(balancexp)
!IF(ALLOCATED(estad)) DEALLOCATE(estad)
!IF(ALLOCATED(RSRindex)) DEALLOCATE(RSRindex)
!IF(ALLOCATED(estadsed)) DEALLOCATE(estadsed)
!IF(ALLOCATED(estadnitr))DEALLOCATE(estadnitr)    
!!GUIOMAR (15/10/2015): cambio el número de miembros de balanc incrementándolo a 27 para que almacene nuevas variables
!!Cris (03/2017): aumento a 30 para riego y pérdidas en diferentes acuíferos    
!ALLOCATE(balanc(nt,28),balancqb(nt),balancexp(nt),estad(naf*2+knaf+kadi,22),estadsed(ksedq*2,12),balanc_sed(nt,30),RSRindex(naf*2+knaf+kadi),balanc_nitr(nt,60),estadnitr(nafn*2,13))
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
!estadnitr=0.0
!
!IF (kevp.gt.0) THEN
!    ALLOCATE(evapo(kevp),band_e(kevp))
!    DO n=1,ncel
!    ALLOCATE(cell(n).ind_int_e(enest),cell(n).fac_int_e(enest))
!	cell(n).ind_int_e=0
!	cell(n).fac_int_e=0.0
!    ENDDO
!ENDIF
!IF (ktem.gt.0) THEN
!    ALLOCATE(band_t(ktem),temper(ktem),acuniv(nt))
!    DO n=1,ncel
!    ALLOCATE(cell(n).ind_int_t(tnest),cell(n).fac_int_t(tnest))
!	cell(n).ind_int_t=0
!	cell(n).fac_int_t=0.0
!    ENDDO
!ENDIF
!!Guiomar (21/07/2015): Preparando para poder interpolar la radiacion
!IF (nradiacion.gt.0) THEN
!    ALLOCATE(band_r(nradiacion))
!    DO n=1,ncel
!    ALLOCATE(cell(n).ind_int_r(rnest),cell(n).fac_int_r(rnest))
!	cell(n).ind_int_r=0
!	cell(n).fac_int_r=0.0
!    ENDDO
!ENDIF
!nb=nemb+vnemb+knemb
!IF (nb.gt.0) THEN 
!    ALLOCATE(nivel(nb),pulm(nb),emb(nb))
!    pulm=0
!    ALLOCATE(volum(nb))
!    ALLOCATE(qemb(nb))
!ENDIF
!
!IF (kadi.gt.0) THEN
!    ALLOCATE(aport(kadi))
!ENDIF
!
!IF (config(2)) THEN    
!    CALL lee_evto2col
!ELSE
!    CALL lee_evto2
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
!! mes y día inicial en formato real
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
!!Cuenta el numero de celdas a regar por cada zona de riego
!banriego=0
!contriego=0
!DO i=1,erie
!    DO n=1,ncel
!    IF (cell(n).codrie.eq.i) banriego(i)=banriego(i)+1
!    ENDDO
!ENDDO
!
!!Lee parametros de calibracion (factores correctores)
!CALL lee_calib
!IF (sale.eq.2) GOTO 95
!
!! Calcula el número de celdas para flujos superficiales
!! El umbral para la escorrentía directa es igual a cero
!
!DO n=1,ncel
!    ncp=cell(n).codpar
!    DO i=2,3
!	nw(i,ncp)=areaumbral(i,ncp)/arcelkm
!    ENDDO
!ENDDO
!
!IF (config(4)) THEN  !Lee condiciones iniciales de sedimentos
!    artem=archsed(7)
!    CALL lee_sedantec
!    IF (sale.eq.2) GOTO 95
!ENDIF
!
!IF (simevents) THEN
!    arch(4)=estadohum(1) !si se está usando la calib autom multievento aquí lee el ficher hantec del primer evento considerado
!END IF
!
!artem=arch(4)
!CALL lee_human
!IF (sale.eq.2) GOTO 95
!!Guiomar(27/03/2014): Desactivo estas líneas porque he incluido el cálculo del estado inicial en HANTEC
!!Lee condiciones iniciales de rveg y waterstress
!!IF (config(5)) THEN
!    !artem=archveg(4)
!    !CALL lee_ci_veg
!    !IF (sale.eq.2) GOTO 95
!!ENDIF
!! Asigna a cada celda los valores de los parametros de vegetación
!!Guiomar (27/01/2014): voy a desactivar todas estas líneas porque ahora está programado de otra manera
!!IF (config(5)) THEN
!    !DO n=1,ncel
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
!    !ENDDO
!!ELSE
!
!!Lee todos los ficheros relativos al submodelo de nitrógeno Cris (03/2016)
!!No cambiar de sitio porque lee_Ninputsuelo necesita saber el valor de nt para comprobar que tiene la misma longitud que el fichero de entrada.
!!Para calcular la adsorción/desorción es necesario haber leído hantec
!!If (modulos2(3)) then
!    call lee_fcubiertan
!    call lee_calibnit
!    call lee_Ninputsuelo
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
!        if((cell(n).h(1)+cell(n).h(3)).gt.0.0) then
!            cell(n).hn(1)=((cell(n).h(1)+cell(n).h(3))/1000*NH4total)/((cell(n).h(1)+cell(n).h(3))/1000+cell(n).kd*fckd*cell(n).psuelo*r(1)*cell(n).daparente) !La profundidad del suelo se ve afectada por el factor corrector 1, almacenamiento estático
!            Cell(n).hn(8)=NH4total-cell(n).hn(1)
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
!!Else if (modulos2(4)) then !Si cultivos está activado pero no nitrógeno, lee sólo el fichero factvegcultivos que es lo único que hace falta
!    !call lee_factvegcultivos
!!End if 
!    
!!Cris (03/2017). Hace lo mismo, pero en este caso para el fichero de factor de cubierta del submodelo de nitrógeno, los parámetros y el fichero ninputsuelo
!!If (modulos2(3)) then
!    !Factor de cubierta
!    k=0 
!    OPEN(10,file=archnit(9),status='old',err=252)
!    READ(10,*)
!    READ(10,*)
!    ios=0
!    DO WHILE (ios.ne.-1)
!        READ(10,*,iostat=ios) j
!        IF (ios.ne.-1) k=k+1
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
!        READ(10,*,iostat=ios) j
!        IF (ios.ne.-1) k=k+1
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
!        READ(10,*,iostat=ios) j
!        IF (ios.ne.-1) k=k+1
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
!        READ(10,*,iostat=ios) j
!        IF (ios.ne.-1) k=k+1
!    ENDDO
!    CLOSE(10)
!    IF((k).lt.massimocodveg)then
!        mensaje=strings(431)
!        errr=1
!        CALL errores(errr,mensaje,lang)
!        goto 94
!    ENDIF
!!End if
!!Guiomar (28/01/2014): el imax es la 13 columna del factorETmes sólo en el caso de no tener activada la vegetación dinámica. Por eso, he añadido un if
!!IF (.NOT.(config(5))) THEN
!    DO n=1,ncel
!    cell(n).imx=lamb(cell(n).codveg,13)
!    ENDDO
!!END IF
!    
!!Calcula la humedad antecedente de la cuenca para cerrar el balance
!DO i=0,8
!almini(i)=0.0
!ENDDO
!DO n=1,ncel
!    DO i=0,4
!    almini(i)=almini(i)+cell(n).h(i)
!    ENDDO
!    almini(6)=almini(6)+cell(n).h(6)
!    IF (nw(2,ncp).gt.cell(n).acum) cell(n).h(5)=0.0    !celda con ladera, no hay volumen inicial en cauces
!    almini(5)=almini(5)+cell(n).h(5)*1000.0/arcel      !mm
!    !Guiomar (28/01/2013): voy a añadir un if config(5) para que calcule el almacenamiento del tanque añadido en el modelo
!    !IF (config(5)) THEN
!        !almini(8)=almini(8)+cell(n).h(8)
!    !ENDIF
!ENDDO
!!Calcula la humedad antecedente media
!DO i=0,4
!    almini(i)=almini(i)/ncel
!ENDDO
!almini(6)=almini(6)/ncel
!!Guiomar (28/01/2013): voy a añadir un if config(5) para que calcule la humedad antecedente media del tanque añadido
!!IF (config(5)) THEN
!    !almini(8)=almini(8)/ncel
!!ENDIF
!    
!!Lee curvas de embalse
!nb=nemb+vnemb+knemb
!IF (nb.gt.0) THEN
!    DO i=1,nb
!    emb(i).caso=0
!    ENDDO
!    CALL cal_caso
!
!    CALL lee_emb1
!    IF (sale.eq.2) GOTO 95
!
!    DO i=1,nb
!	IF (emb(i).caso.gt.0) THEN
!        k=emb(i).datos
!        ALLOCATE(emb(i).h(0:k),emb(i).vol(0:k),emb(i).sup(0:k),emb(i).out(0:k,2))
!        ALLOCATE(emb(i).fpul(0:k))
!	ENDIF
!    ENDDO
!
!    CALL lee_emb2
!    IF (sale.eq.2) GOTO 95
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
!Inicia variables de estado de sedimentos y realiza interpolaciones
!IF (config(4)) CALL inicia_varsed

CALL iniciaTetis(5,sale) !5, porque se llama desde CalibAutomNitr
if(sale.eq.2)go to 95
!Tras iniciar Tetis, se alocata la matriz de los estadisticos de nitrogeno
IF(ALLOCATED(estadnitr)) DEALLOCATE(estadnitr)
ALLOCATE(estadnitr(nafn*2,13))

artem=TRIM(ADJUSTL(dirtra))//'resumen-SCEUA-NITR.txt'
OPEN (33,file=artem)

!Inicia el proceso de optimización automática SCE-UA
artem=TRIM(ADJUSTL(dirtra))//'Var-SCEUA_Nitr.txt'

CALL SCEUA_gen_nit

!Revisa el optimo y reescribe los respectivos ficheros CALIB y PARAMGEO
REWIND(33)
ios=0
ncon=0
pizq=99999999.999
DO WHILE (ios.ne.-1)
    READ(33,*,iostat=ios) pder
    IF (ios.ne.-1) THEN
    ncon=ncon+1
	IF (pder.lt.pizq) pizq=pder
    DO i=1,cantUsSueNitr+2
        READ(33,*)        
    ENDDO
    ENDIF        
ENDDO
REWIND(33)
DO i=1,ncon
    write(*,*)ncon,i,pizq
    READ(33,*)pder
    DO j=1,cantUsSueNitr
    READ(33,*)kmin2(j),kinm2(j),kvol2(j),knit2(j),kfi2(j),kdes2(j),F2(j),Ndem2(j),PrefNO3(j)
    ENDDO
    READ(33,*)kminc2,knitc2,kdesc2
    READ(33,*)mtd,tethas,topts,tethac,toptc,fckd
    IF (pder.eq.pizq) THEN          
        CALL escr_calibnit
    EXIT
    ENDIF
ENDDO
CLOSE (33)

!artem=TRIM(ADJUSTL(dirtra))//'resumen-SCEUA-NITR.txt'
!resul=DELFILESQQ(artem)
!res=DELFILESQQ(TRIM(ADJUSTL(dirtra))//TRIM(ADJUSTL('~errores.txt')))
!Subtituimos la rutina para no emplear librerias(Vicente.Nov-2019)
CALL deletefich(TRIM(ADJUSTL(dirtra))//'resumen-SCEUA-NITR.txt')
        
GOTO 95


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
211 mensaje=strings(33)
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
249 mensaje= strings(109)
errr=1
CALL errores
GOTO 94

252 mensaje=strings(400)
errr=1
CALL errores
253 mensaje=strings(403)
errr=1
CALL errores
254 mensaje=strings(413)
errr=1
CALL errores
255 mensaje=strings(426)
errr=1
CALL errores
256 mensaje=strings(427)
errr=1
CALL errores


94 WRITE(*,*)strings(800)

95 CALL libera_mem
   
if(stma_op==0) then
    !copyFicheros = 'cp "' // TRIM(ADJUSTL(dirtra))//TRIM(ADJUSTL('~errores.txt')) // '" "' // TRIM(ADJUSTL(dirtra))//TRIM(ADJUSTL('errores.txt')) // '" /Y' !Linux
    copyFicheros = '[ -f ' // TRIM(ADJUSTL(dirtra))//TRIM(ADJUSTL('~errores.txt')) //' ] && cp ' // TRIM(ADJUSTL(dirtra))//TRIM(ADJUSTL('~errores.txt')) // ' ' // TRIM(ADJUSTL(dirtra))//TRIM(ADJUSTL('errores.txt')) !Linux
else
    copyFicheros = 'copy "' // TRIM(ADJUSTL(dirtra))//TRIM(ADJUSTL('~errores.txt')) // '" "' // TRIM(ADJUSTL(dirtra))//TRIM(ADJUSTL('errores.txt')) // '" /Y' !Windows
end if
res = SYSTEM (copyFicheros)
CALL deletefich(TRIM(ADJUSTL(dirtra))//TRIM(ADJUSTL('~errores.txt')))


!RETURN  !para ejecutable
END program


!*****************************************************************************
!*  Esta subrutina corresponde a TETIS. Incluye embalses, para la
!*  realizacion de la calibracion automatica de los parametros del modelo de.
!*  simulación segun el SCE-UA. Ultima actualización: Mayo de 2001
!*****************************************************************************
SUBROUTINE FcnSCE_gen_nitr
USE modtet
IMPLICIT NONE

!Cris pruebo
!*************************************************************
!Inicializa los balances
IF(ALLOCATED(balanc)) DEALLOCATE(balanc)
IF(ALLOCATED(balanc_sed)) DEALLOCATE(balanc_sed)
IF(ALLOCATED(balanc_nitr)) DEALLOCATE(balanc_nitr)
IF(ALLOCATED(balancqb)) DEALLOCATE(balancqb)
IF(ALLOCATED(balancexp)) DEALLOCATE(balancexp)
IF(ALLOCATED(estad)) DEALLOCATE(estad)
IF(ALLOCATED(RSRindex)) DEALLOCATE(RSRindex)
IF(ALLOCATED(estadsed)) DEALLOCATE(estadsed)
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
!*************************************************************

  DO l=1,naf
    aforo(l).sim=0.0
  ENDDO
  DO l=1,knaf
    otros(l).sim=0.0
  ENDDO
  DO l=1,nemb
    nivel(l).sim=0.0
  ENDDO
  DO l=1,vnemb
    volum(l).sim=0.0
  ENDDO
  DO l=1,knemb
    qemb(l).sim=0.0
    qemb(l).bal=0.0
  ENDDO
  DO l=1,kadi
    aport(l).sim=0.0
  ENDDO
  DO l=1,kno
     norg(jno).sim=0.0 
  END DO
  DO l=1,kam
     amonio(jam).sim=0.0 
  END DO
  DO l=1,kni
     nitrato(jni).sim=0.0 
  END DO
!ENDDO

!Inicializa la humedad antecedente y umbral del tanque 5 (u5)
artem=arch(4)
CALL lee_human

!Inicia variables de 
CALL inicia_var

!Realiza la simulación en cada celda durante todo el tiempo
CALL sim_tpo


! Llama subrutina que calcula estadisticos; Nash, HMLE, RMSE....
CALL calc_estad_nitr

95 cont=cont+1
   
if(idfo.eq.6.or.idfo.eq.13)then!Vicente(10-2020)NSE,KGE
    WRITE(*,*)'iteracion ',cont,'   F.Obj ',1.0-fobj!,'  Q max sim',estad(naf*2,1)!Para NITROGENO, no aporta nada
else
    WRITE(*,*)'iteracion ',cont,'   F.Obj ',fobj !,'  Q max sim',estad(naf*2,1)!Para NITROGENO, no aporta nada
endif

END	SUBROUTINE


! ******************************************************************
! * Subrutina de evolución competitiva de complejos CCE (Nelder y
! * Mead 1965.
! ******************************************************************
SUBROUTINE eccNM_gen_nitr
USE modtet
IMPLICIT NONE

REAL b(qvar,nparam+1),tri(mvar),c(nparam),g(nparam),lecc(qvar)

alfafo=1	  !Parámetro especificado por el usuario
betafo=mvar
nrand=0.5
	
DO ib=1,betafo
  !Asignación de distribución de probabilidad triangular
  DO iecc=1,mvar
    tri(iecc)=2.0*(mvar+1.0-iecc)/(mvar*(mvar+1.0))
  ENDDO

  !Selección aleatoria del subcomplejo de "q" puntos (PADRES)
  jecc=1
  ban=0
  lecc=0
  DO WHILE (jecc.le.qvar)
    CALL RANDOM_NUMBER(nrand)
	ban=0
	DO iecc=1,mvar
	  IF (nrand.gt.tri(iecc).AND.ban.eq.0) THEN
	    ban=1
        DO kecc=1,jecc-1	         !Verifica que no se repita el numero
	      IF (lecc(kecc).eq.iecc) THEN
	        jecc=jecc-1
		    ban=0
		    EXIT
		  ENDIF
        ENDDO
        IF(ban.eq.0) CYCLE !(Vicente) Se añade para SI evitar que se repita el mismo numero en B
	    DO kecc=1,nparam+1
	      b(jecc,kecc)=dvar(iecc,kecc)
	    ENDDO
	    lecc(jecc)=iecc
	    jecc=jecc+1
	  ENDIF
	ENDDO
  ENDDO
  DO ia=1,alfafo
    !Generacion de la descendencia
    g=0.0
    !r=0.0
	!humr=0.0
    c=0.0
    ! a) Ordena el vector B y L descendentemente y calcula g
    DO iecc=1,qvar
	  DO jecc=iecc+1,qvar
	    IF (b(iecc,nparam+1).gt.b(jecc,nparam+1)) THEN
          !Cambio de posición
	      DO kecc=1,nparam+1
	        pder=b(iecc,kecc)
	        b(iecc,kecc)=b(jecc,kecc)
	        b(jecc,kecc)=pder            
          ENDDO
	    ENDIF
	  ENDDO
    ENDDO           
    !Calcula el valor del centroide g
	DO iecc=1,nparam
	  sum=0.0
	  DO jecc=1,qvar-1
	    sum=sum+b(jecc,iecc)
	  ENDDO
	  g(iecc)=(1.0/(qvar-1.0))*sum
	ENDDO
	! b) Calcula el nuevo punto r
	cont2=0
    
    DO isce_nitr=1,cantUsSueNitr
        IF (xchk_kmin(isce_nitr)) THEN 
	      cont2=cont2+1
	      kmin2(isce_nitr)=2.0*g(cont2)-b(qvar,cont2)
        ENDIF
        IF (xchk_kinm(isce_nitr)) THEN 
	      cont2=cont2+1
	      kinm2(isce_nitr)=2.0*g(cont2)-b(qvar,cont2)
        ENDIF
        IF (xchk_kvol(isce_nitr)) THEN 
	      cont2=cont2+1
	      kvol2(isce_nitr)=2.0*g(cont2)-b(qvar,cont2)
        ENDIF
        IF (xchk_knit(isce_nitr)) THEN 
	      cont2=cont2+1
	      knit2(isce_nitr)=2.0*g(cont2)-b(qvar,cont2)
        ENDIF
        IF (xchk_kfi(isce_nitr)) THEN 
	      cont2=cont2+1
	      kfi2(isce_nitr)=2.0*g(cont2)-b(qvar,cont2)
        ENDIF
        IF (xchk_kdes(isce_nitr)) THEN 
	      cont2=cont2+1
	      kdes2(isce_nitr)=2.0*g(cont2)-b(qvar,cont2)
        ENDIF
        IF (xchk_F(isce_nitr)) THEN 
	      cont2=cont2+1
	      F2(isce_nitr)=2.0*g(cont2)-b(qvar,cont2)
        ENDIF
        IF (xchk_Ndem(isce_nitr)) THEN 
	      cont2=cont2+1
	      Ndem2(isce_nitr)=2.0*g(cont2)-b(qvar,cont2)
        ENDIF
        IF (xchk_PrefNO3(isce_nitr)) THEN 
	      cont2=cont2+1
	      PrefNO3(isce_nitr)=2.0*g(cont2)-b(qvar,cont2)
        ENDIF
      ENDDO
      IF (xchk_kminc) THEN 
	      cont2=cont2+1
	      kminc2=2.0*g(cont2)-b(qvar,cont2)
      ENDIF
      IF (xchk_knitc) THEN 
	      cont2=cont2+1
	      knitc2=2.0*g(cont2)-b(qvar,cont2)
      ENDIF
      IF (xchk_kdesc) THEN 
	      cont2=cont2+1
	      kdesc2=2.0*g(cont2)-b(qvar,cont2)
      ENDIF
      IF (xchk_mtd) THEN 
	      cont2=cont2+1
	      mtd=2.0*g(cont2)-b(qvar,cont2)
      ENDIF
      IF (xchk_tethas) THEN 
	      cont2=cont2+1
	      tethas=2.0*g(cont2)-b(qvar,cont2)
      ENDIF
      IF (xchk_topts) THEN 
	      cont2=cont2+1
	      topts=2.0*g(cont2)-b(qvar,cont2)
      ENDIF
      IF (xchk_tethac) THEN 
	      cont2=cont2+1
	      tethac=2.0*g(cont2)-b(qvar,cont2)
      ENDIF
      IF (xchk_toptc) THEN 
	      cont2=cont2+1
	      toptc=2.0*g(cont2)-b(qvar,cont2)
      ENDIF
      IF (xchk_fckd) THEN 
	      cont2=cont2+1
	      fckd=2.0*g(cont2)-b(qvar,cont2)
      ENDIF
    
    ! c) Verifica que el punto r se encuentre en el espacio factible H y sigue a d)
	! de lo contrario genera un valor aleatorio dentro del espacio (mutacion)
	cont2=0
    
    DO isce_nitr=1,cantUsSueNitr
        IF (xchk_kmin(isce_nitr)) THEN 
	      cont2=cont2+1
	      IF (kmin2(isce_nitr).lt.xlb_kmin(isce_nitr).OR.kmin2(isce_nitr).gt.xub_kmin(isce_nitr)) THEN
	          !Genera aleatoriamente un número cualquiera
              CALL RANDOM_NUMBER(nrand)
  	          kmin2(isce_nitr)=xlb_kmin(isce_nitr)+(xub_kmin(isce_nitr)-xlb_kmin(isce_nitr))*nrand
          ENDIF
        ENDIF
        IF (xchk_kinm(isce_nitr)) THEN 
	      cont2=cont2+1
	      IF (kinm2(isce_nitr).lt.xlb_kinm(isce_nitr).OR.kinm2(isce_nitr).gt.xub_kinm(isce_nitr)) THEN
	          !Genera aleatoriamente un número cualquiera
              CALL RANDOM_NUMBER(nrand)
  	          kinm2(isce_nitr)=xlb_kinm(isce_nitr)+(xub_kinm(isce_nitr)-xlb_kinm(isce_nitr))*nrand
          ENDIF
        ENDIF
        IF (xchk_kvol(isce_nitr)) THEN 
	      cont2=cont2+1
	      IF (kvol2(isce_nitr).lt.xlb_kvol(isce_nitr).OR.kvol2(isce_nitr).gt.xub_kvol(isce_nitr)) THEN
	          !Genera aleatoriamente un número cualquiera
              CALL RANDOM_NUMBER(nrand)
  	          kvol2(isce_nitr)=xlb_kvol(isce_nitr)+(xub_kvol(isce_nitr)-xlb_kvol(isce_nitr))*nrand
          ENDIF
        ENDIF
        IF (xchk_knit(isce_nitr)) THEN 
	      cont2=cont2+1
	      IF (knit2(isce_nitr).lt.xlb_knit(isce_nitr).OR.knit2(isce_nitr).gt.xub_knit(isce_nitr)) THEN
	          !Genera aleatoriamente un número cualquiera
              CALL RANDOM_NUMBER(nrand)
  	          knit2(isce_nitr)=xlb_knit(isce_nitr)+(xub_knit(isce_nitr)-xlb_knit(isce_nitr))*nrand
          ENDIF
        ENDIF
        IF (xchk_kfi(isce_nitr)) THEN 
	      cont2=cont2+1
	      IF (kfi2(isce_nitr).lt.xlb_kfi(isce_nitr).OR.kfi2(isce_nitr).gt.xub_kfi(isce_nitr)) THEN
	          !Genera aleatoriamente un número cualquiera
              CALL RANDOM_NUMBER(nrand)
  	          kfi2(isce_nitr)=xlb_kfi(isce_nitr)+(xub_kfi(isce_nitr)-xlb_kfi(isce_nitr))*nrand
          ENDIF
        ENDIF
        IF (xchk_kdes(isce_nitr)) THEN 
	      cont2=cont2+1
	      IF (kdes2(isce_nitr).lt.xlb_kdes(isce_nitr).OR.kdes2(isce_nitr).gt.xub_kdes(isce_nitr)) THEN
	          !Genera aleatoriamente un número cualquiera
              CALL RANDOM_NUMBER(nrand)
  	          kdes2(isce_nitr)=xlb_kdes(isce_nitr)+(xub_kdes(isce_nitr)-xlb_kdes(isce_nitr))*nrand
          ENDIF
        ENDIF
        IF (xchk_F(isce_nitr)) THEN 
	      cont2=cont2+1
	      IF (F2(isce_nitr).lt.xlb_F(isce_nitr).OR.F2(isce_nitr).gt.xub_F(isce_nitr)) THEN
	          !Genera aleatoriamente un número cualquiera
              CALL RANDOM_NUMBER(nrand)
  	          F2(isce_nitr)=xlb_F(isce_nitr)+(xub_F(isce_nitr)-xlb_F(isce_nitr))*nrand
          ENDIF
        ENDIF
        IF (xchk_Ndem(isce_nitr)) THEN 
	      cont2=cont2+1
	      IF (Ndem2(isce_nitr).lt.xlb_Ndem(isce_nitr).OR.Ndem2(isce_nitr).gt.xub_Ndem(isce_nitr)) THEN
	          !Genera aleatoriamente un número cualquiera
              CALL RANDOM_NUMBER(nrand)
  	          Ndem2(isce_nitr)=xlb_Ndem(isce_nitr)+(xub_Ndem(isce_nitr)-xlb_Ndem(isce_nitr))*nrand
          ENDIF
        ENDIF
        IF (xchk_PrefNO3(isce_nitr)) THEN 
	      cont2=cont2+1
	      IF (PrefNO3(isce_nitr).lt.xlb_PrefNO3(isce_nitr).OR.PrefNO3(isce_nitr).gt.xub_PrefNO3(isce_nitr)) THEN
	          !Genera aleatoriamente un número cualquiera
              CALL RANDOM_NUMBER(nrand)
  	          PrefNO3(isce_nitr)=xlb_PrefNO3(isce_nitr)+(xub_PrefNO3(isce_nitr)-xlb_PrefNO3(isce_nitr))*nrand
          ENDIF
        ENDIF
    ENDDO
    IF (xchk_kminc) THEN 
	      cont2=cont2+1
	      IF (kminc2.lt.xlb_kminc.OR.kminc2.gt.xub_kminc) THEN
	          !Genera aleatoriamente un número cualquiera
              CALL RANDOM_NUMBER(nrand)
  	          kminc2=xlb_kminc+(xub_kminc-xlb_kminc)*nrand
          ENDIF
      ENDIF
      IF (xchk_knitc) THEN 
	      cont2=cont2+1
	      IF (knitc2.lt.xlb_knitc.OR.knitc2.gt.xub_knitc) THEN
	          !Genera aleatoriamente un número cualquiera
              CALL RANDOM_NUMBER(nrand)
  	          knitc2=xlb_knitc+(xub_knitc-xlb_knitc)*nrand
          ENDIF
      ENDIF
      IF (xchk_kdesc) THEN 
	      cont2=cont2+1
	      IF (kdesc2.lt.xlb_kdesc.OR.kdesc2.gt.xub_kdesc) THEN
	          !Genera aleatoriamente un número cualquiera
              CALL RANDOM_NUMBER(nrand)
  	          kdesc2=xlb_kdesc+(xub_kdesc-xlb_kdesc)*nrand
          ENDIF
      ENDIF
      IF (xchk_mtd) THEN 
	      cont2=cont2+1
	      IF (mtd.lt.xlb_mtd.OR.mtd.gt.xub_mtd) THEN
	          !Genera aleatoriamente un número cualquiera
              CALL RANDOM_NUMBER(nrand)
  	          mtd=xlb_mtd+(xub_mtd-xlb_mtd)*nrand
          ENDIF
      ENDIF
      IF (xchk_tethas) THEN 
	      cont2=cont2+1
	      IF (tethas.lt.xlb_tethas.OR.tethas.gt.xub_tethas) THEN
	          !Genera aleatoriamente un número cualquiera
              CALL RANDOM_NUMBER(nrand)
  	          tethas=xlb_tethas+(xub_tethas-xlb_tethas)*nrand
          ENDIF
      ENDIF
      IF (xchk_topts) THEN 
	      cont2=cont2+1
	      IF (topts.lt.xlb_topts.OR.topts.gt.xub_topts) THEN
	          !Genera aleatoriamente un número cualquiera
              CALL RANDOM_NUMBER(nrand)
  	          topts=xlb_topts+(xub_topts-xlb_topts)*nrand
          ENDIF
      ENDIF
      IF (xchk_tethac) THEN 
	      cont2=cont2+1
	      IF (tethac.lt.xlb_tethac.OR.tethac.gt.xub_tethac) THEN
	          !Genera aleatoriamente un número cualquiera
              CALL RANDOM_NUMBER(nrand)
  	          tethac=xlb_tethac+(xub_tethac-xlb_tethac)*nrand
          ENDIF
      ENDIF
      IF (xchk_toptc) THEN 
	      cont2=cont2+1
	      IF (toptc.lt.xlb_toptc.OR.toptc.gt.xub_toptc) THEN
	          !Genera aleatoriamente un número cualquiera
              CALL RANDOM_NUMBER(nrand)
  	          toptc=xlb_toptc+(xub_toptc-xlb_toptc)*nrand
          ENDIF
      ENDIF
      IF (xchk_fckd) THEN 
	      cont2=cont2+1
	      IF (fckd.lt.xlb_fckd.OR.fckd.gt.xub_fckd) THEN
	          !Genera aleatoriamente un número cualquiera
              CALL RANDOM_NUMBER(nrand)
  	          fckd=xlb_fckd+(xub_fckd-xlb_fckd)*nrand
          ENDIF
      ENDIF
    
	!Calculo el valor de Funcion para el nuevo punto
	CALL FcnSCE_gen_nitr
    
	xpar(cont,nparam+1)=fobj
    cont2=0
    
    DO isce_nitr=1,cantUsSueNitr
        IF (xchk_kmin(isce_nitr)) THEN 
	      cont2=cont2+1
          xpar(cont,cont2)=kmin2(isce_nitr)	      
        ENDIF
        IF (xchk_kinm(isce_nitr)) THEN 
	      cont2=cont2+1
	      xpar(cont,cont2)=kinm2(isce_nitr)
        ENDIF
        IF (xchk_kvol(isce_nitr)) THEN 
	      cont2=cont2+1
	      xpar(cont,cont2)=kvol2(isce_nitr)
        ENDIF
        IF (xchk_knit(isce_nitr)) THEN 
	      cont2=cont2+1
	      xpar(cont,cont2)=knit2(isce_nitr)
        ENDIF
        IF (xchk_kfi(isce_nitr)) THEN 
	      cont2=cont2+1
	      xpar(cont,cont2)=kfi2(isce_nitr)
        ENDIF
        IF (xchk_kdes(isce_nitr)) THEN 
	      cont2=cont2+1
	      xpar(cont,cont2)=kdes2(isce_nitr)
        ENDIF
        IF (xchk_F(isce_nitr)) THEN 
	      cont2=cont2+1
	      xpar(cont,cont2)=F2(isce_nitr)
        ENDIF
        IF (xchk_Ndem(isce_nitr)) THEN 
	      cont2=cont2+1
	      xpar(cont,cont2)=Ndem2(isce_nitr)
        ENDIF
        IF (xchk_PrefNO3(isce_nitr)) THEN 
	      cont2=cont2+1
	      xpar(cont,cont2)=PrefNO3(isce_nitr)
        ENDIF
    ENDDO
    IF (xchk_kminc) THEN 
	    cont2=cont2+1
	    xpar(cont,cont2)=kminc2
    ENDIF
    IF (xchk_knitc) THEN 
	    cont2=cont2+1
	    xpar(cont,cont2)=knitc2
    ENDIF
    IF (xchk_kdesc) THEN 
	    cont2=cont2+1
	    xpar(cont,cont2)=kdesc2
    ENDIF
    IF (xchk_mtd) THEN 
	    cont2=cont2+1
	    xpar(cont,cont2)=mtd
    ENDIF
    IF (xchk_tethas) THEN 
	    cont2=cont2+1
	    xpar(cont,cont2)=tethas
    ENDIF
    IF (xchk_topts) THEN 
	    cont2=cont2+1
	    xpar(cont,cont2)=topts
    ENDIF
    IF (xchk_tethac) THEN 
	    cont2=cont2+1
	    xpar(cont,cont2)=tethac
    ENDIF
    IF (xchk_toptc) THEN 
	    cont2=cont2+1
	    xpar(cont,cont2)=toptc
    ENDIF
    IF (xchk_fckd) THEN 
	    cont2=cont2+1
	    xpar(cont,cont2)=fckd
    ENDIF
    
    CALL imprestad_nitr

	! d) Decide si hay contraccion
	cont2=0
    IF (fobj.lt.b(qvar,nparam+1)) THEN  !es minimizacion, así que es "menor que"
	  DO isce_nitr=1,cantUsSueNitr
            IF (xchk_kmin(isce_nitr)) THEN 
	          cont2=cont2+1
              b(qvar,cont2)=kmin2(isce_nitr)	      
            ENDIF
            IF (xchk_kinm(isce_nitr)) THEN 
	          cont2=cont2+1
	          b(qvar,cont2)=kinm2(isce_nitr)
            ENDIF
            IF (xchk_kvol(isce_nitr)) THEN 
	          cont2=cont2+1
	          b(qvar,cont2)=kvol2(isce_nitr)
            ENDIF
            IF (xchk_knit(isce_nitr)) THEN 
	          cont2=cont2+1
	          b(qvar,cont2)=knit2(isce_nitr)
            ENDIF
            IF (xchk_kfi(isce_nitr)) THEN 
	          cont2=cont2+1
	          b(qvar,cont2)=kfi2(isce_nitr)
            ENDIF
            IF (xchk_kdes(isce_nitr)) THEN 
	          cont2=cont2+1
	          b(qvar,cont2)=kdes2(isce_nitr)
            ENDIF
            IF (xchk_F(isce_nitr)) THEN 
	          cont2=cont2+1
	          b(qvar,cont2)=F2(isce_nitr)
            ENDIF
            IF (xchk_Ndem(isce_nitr)) THEN 
	          cont2=cont2+1
	          b(qvar,cont2)=Ndem2(isce_nitr)
            ENDIF
            IF (xchk_PrefNO3(isce_nitr)) THEN 
	          cont2=cont2+1
	          b(qvar,cont2)=PrefNO3(isce_nitr)
            ENDIF
        ENDDO
        IF (xchk_kminc) THEN 
	        cont2=cont2+1
	        b(qvar,cont2)=kminc2
        ENDIF
        IF (xchk_knitc) THEN 
	        cont2=cont2+1
	        b(qvar,cont2)=knitc2
        ENDIF
        IF (xchk_kdesc) THEN 
	        cont2=cont2+1
	        b(qvar,cont2)=kdesc2
        ENDIF
        IF (xchk_mtd) THEN 
	        cont2=cont2+1
	        b(qvar,cont2)=mtd
        ENDIF
        IF (xchk_tethas) THEN 
	        cont2=cont2+1
	        b(qvar,cont2)=tethas
        ENDIF
        IF (xchk_topts) THEN 
	        cont2=cont2+1
	        b(qvar,cont2)=topts
        ENDIF
        IF (xchk_tethac) THEN 
	        cont2=cont2+1
	        b(qvar,cont2)=tethac
        ENDIF
        IF (xchk_toptc) THEN 
	        cont2=cont2+1
	        b(qvar,cont2)=toptc
        ENDIF
        IF (xchk_fckd) THEN 
	        cont2=cont2+1
	        b(qvar,cont2)=fckd
        ENDIF
        b(qvar,nparam+1)=fobj
    ELSE
      !Contraccion
      DO isce_nitr=1,cantUsSueNitr
            IF (xchk_kmin(isce_nitr)) THEN 
	          cont2=cont2+1
              c(cont2)=(g(cont2)+b(qvar,cont2))/2.0
	          kmin2(isce_nitr)=c(cont2)                  
            ENDIF
            IF (xchk_kinm(isce_nitr)) THEN 
	          cont2=cont2+1
	          c(cont2)=(g(cont2)+b(qvar,cont2))/2.0
              kinm2(isce_nitr)=c(cont2)
            ENDIF
            IF (xchk_kvol(isce_nitr)) THEN 
	          cont2=cont2+1
	          c(cont2)=(g(cont2)+b(qvar,cont2))/2.0
              kvol2(isce_nitr)=c(cont2)
            ENDIF
            IF (xchk_knit(isce_nitr)) THEN 
	          cont2=cont2+1
	          c(cont2)=(g(cont2)+b(qvar,cont2))/2.0
              knit2(isce_nitr)=c(cont2)
            ENDIF
            IF (xchk_kfi(isce_nitr)) THEN 
	          cont2=cont2+1
	          c(cont2)=(g(cont2)+b(qvar,cont2))/2.0
              kfi2(isce_nitr)=c(cont2)
            ENDIF
            IF (xchk_kdes(isce_nitr)) THEN 
	          cont2=cont2+1
	          c(cont2)=(g(cont2)+b(qvar,cont2))/2.0
              kdes2(isce_nitr)=c(cont2)
            ENDIF
            IF (xchk_F(isce_nitr)) THEN 
	          cont2=cont2+1
	          c(cont2)=(g(cont2)+b(qvar,cont2))/2.0
              F2(isce_nitr)=c(cont2)
            ENDIF
            IF (xchk_Ndem(isce_nitr)) THEN 
	          cont2=cont2+1
	          c(cont2)=(g(cont2)+b(qvar,cont2))/2.0
              Ndem2(isce_nitr)=c(cont2)
            ENDIF
            IF (xchk_PrefNO3(isce_nitr)) THEN 
	          cont2=cont2+1
	          c(cont2)=(g(cont2)+b(qvar,cont2))/2.0
              PrefNO3(isce_nitr)=c(cont2)
            ENDIF
        ENDDO
        IF (xchk_kminc) THEN 
	        cont2=cont2+1
	        c(cont2)=(g(cont2)+b(qvar,cont2))/2.0
            kminc2=c(cont2)
        ENDIF
        IF (xchk_knitc) THEN 
	        cont2=cont2+1
	        c(cont2)=(g(cont2)+b(qvar,cont2))/2.0
            knitc2=c(cont2)
        ENDIF
        IF (xchk_kdesc) THEN 
	        cont2=cont2+1
	        c(cont2)=(g(cont2)+b(qvar,cont2))/2.0
            kdesc2=c(cont2)
        ENDIF
        IF (xchk_mtd) THEN 
	        cont2=cont2+1
	        c(cont2)=(g(cont2)+b(qvar,cont2))/2.0
            mtd=c(cont2)
        ENDIF
        IF (xchk_tethas) THEN 
	        cont2=cont2+1
	        c(cont2)=(g(cont2)+b(qvar,cont2))/2.0
            tethas=c(cont2)
        ENDIF
        IF (xchk_topts) THEN 
	        cont2=cont2+1
	        c(cont2)=(g(cont2)+b(qvar,cont2))/2.0
            topts=c(cont2)
        ENDIF
        IF (xchk_tethac) THEN 
	        cont2=cont2+1
	        c(cont2)=(g(cont2)+b(qvar,cont2))/2.0
            tethac=c(cont2)
        ENDIF
        IF (xchk_toptc) THEN 
	        cont2=cont2+1
	        c(cont2)=(g(cont2)+b(qvar,cont2))/2.0
            toptc=c(cont2)
        ENDIF
        IF (xchk_fckd) THEN 
	        cont2=cont2+1
	        c(cont2)=(g(cont2)+b(qvar,cont2))/2.0
            fckd=c(cont2)
        ENDIF  
     	CALL FcnSCE_gen_nitr

      xpar(cont,nparam+1)=fobj
      cont2=0
    
        DO isce_nitr=1,cantUsSueNitr
            IF (xchk_kmin(isce_nitr)) THEN 
	          cont2=cont2+1
              xpar(cont,cont2)=kmin2(isce_nitr)	      
            ENDIF
            IF (xchk_kinm(isce_nitr)) THEN 
	          cont2=cont2+1
	          xpar(cont,cont2)=kinm2(isce_nitr)
            ENDIF
            IF (xchk_kvol(isce_nitr)) THEN 
	          cont2=cont2+1
	          xpar(cont,cont2)=kvol2(isce_nitr)
            ENDIF
            IF (xchk_knit(isce_nitr)) THEN 
	          cont2=cont2+1
	          xpar(cont,cont2)=knit2(isce_nitr)
            ENDIF
            IF (xchk_kfi(isce_nitr)) THEN 
	          cont2=cont2+1
	          xpar(cont,cont2)=kfi2(isce_nitr)
            ENDIF
            IF (xchk_kdes(isce_nitr)) THEN 
	          cont2=cont2+1
	          xpar(cont,cont2)=kdes2(isce_nitr)
            ENDIF
            IF (xchk_F(isce_nitr)) THEN 
	          cont2=cont2+1
	          xpar(cont,cont2)=F2(isce_nitr)
            ENDIF
            IF (xchk_Ndem(isce_nitr)) THEN 
	          cont2=cont2+1
	          xpar(cont,cont2)=Ndem2(isce_nitr)
            ENDIF
            IF (xchk_PrefNO3(isce_nitr)) THEN 
	          cont2=cont2+1
	          xpar(cont,cont2)=PrefNO3(isce_nitr)
            ENDIF
        ENDDO
        IF (xchk_kminc) THEN 
	        cont2=cont2+1
	        xpar(cont,cont2)=kminc2
        ENDIF
        IF (xchk_knitc) THEN 
	        cont2=cont2+1
	        xpar(cont,cont2)=knitc2
        ENDIF
        IF (xchk_kdesc) THEN 
	        cont2=cont2+1
	        xpar(cont,cont2)=kdesc2
        ENDIF
        IF (xchk_mtd) THEN 
	        cont2=cont2+1
	        xpar(cont,cont2)=mtd
        ENDIF
        IF (xchk_tethas) THEN 
	        cont2=cont2+1
	        xpar(cont,cont2)=tethas
        ENDIF
        IF (xchk_topts) THEN 
	        cont2=cont2+1
	        xpar(cont,cont2)=topts
        ENDIF
        IF (xchk_tethac) THEN 
	        cont2=cont2+1
	        xpar(cont,cont2)=tethac
        ENDIF
        IF (xchk_toptc) THEN 
	        cont2=cont2+1
	        xpar(cont,cont2)=toptc
        ENDIF
        IF (xchk_fckd) THEN 
	        cont2=cont2+1
	        xpar(cont,cont2)=fckd
        ENDIF
     CALL imprestad_nitr

	  !e)
	  IF (fobj.lt.b(qvar,nparam+1)) THEN   !es minimizacion, así que es "menor que"
	    DO iecc=1,nparam
	      b(qvar,iecc)=c(iecc)
	    ENDDO
	    b(qvar,nparam+1)=fobj
      ELSE
          
      !Genera aleatoriamente un nueva valor    
         cont2=0 
         DO isce_nitr=1,cantUsSueNitr !(Vicente) Añadimos 2 ultimos FCs
            IF (xchk_kmin(isce_nitr)) THEN 
	          cont2=cont2+1
	          CALL RANDOM_NUMBER(nrand)
              c(cont2)=xlb_kmin(isce_nitr)+(xub_kmin(isce_nitr)-xlb_kmin(isce_nitr))*nrand
              kmin2(isce_nitr)=c(cont2)              
            ENDIF
            IF (xchk_kinm(isce_nitr)) THEN 
	          cont2=cont2+1
	          CALL RANDOM_NUMBER(nrand)
	          c(cont2)=xlb_kinm(isce_nitr)+(xub_kinm(isce_nitr)-xlb_kinm(isce_nitr))*nrand
              kinm2(isce_nitr)=c(cont2)
            ENDIF
            IF (xchk_kvol(isce_nitr)) THEN 
	          cont2=cont2+1
	          CALL RANDOM_NUMBER(nrand)
	          c(cont2)=xlb_kvol(isce_nitr)+(xub_kvol(isce_nitr)-xlb_kvol(isce_nitr))*nrand
              kvol2(isce_nitr)=c(cont2)
            ENDIF
            IF (xchk_knit(isce_nitr)) THEN 
	          cont2=cont2+1
	          CALL RANDOM_NUMBER(nrand)
	          c(cont2)=xlb_kinm(isce_nitr)+(xub_kinm(isce_nitr)-xlb_kinm(isce_nitr))*nrand
              kinm2(isce_nitr)=c(cont2)
            ENDIF
            IF (xchk_kfi(isce_nitr)) THEN 
	          cont2=cont2+1
	          CALL RANDOM_NUMBER(nrand)
	          c(cont2)=xlb_kfi(isce_nitr)+(xub_kfi(isce_nitr)-xlb_kfi(isce_nitr))*nrand
              kfi2(isce_nitr)=c(cont2)
            ENDIF
            IF (xchk_kdes(isce_nitr)) THEN 
	          cont2=cont2+1
	          CALL RANDOM_NUMBER(nrand)
	          c(cont2)=xlb_kdes(isce_nitr)+(xub_kdes(isce_nitr)-xlb_kdes(isce_nitr))*nrand
              kdes2(isce_nitr)=c(cont2)
            ENDIF
            IF (xchk_F(isce_nitr)) THEN 
	          cont2=cont2+1
	          CALL RANDOM_NUMBER(nrand)
	          c(cont2)=xlb_F(isce_nitr)+(xub_F(isce_nitr)-xlb_F(isce_nitr))*nrand
              F2(isce_nitr)=c(cont2)
            ENDIF
            IF (xchk_Ndem(isce_nitr)) THEN 
	          cont2=cont2+1
	          CALL RANDOM_NUMBER(nrand)
	          c(cont2)=xlb_Ndem(isce_nitr)+(xub_Ndem(isce_nitr)-xlb_Ndem(isce_nitr))*nrand
              Ndem2(isce_nitr)=c(cont2)
            ENDIF
            IF (xchk_PrefNO3(isce_nitr)) THEN 
	          cont2=cont2+1
	          CALL RANDOM_NUMBER(nrand)
	          c(cont2)=xlb_PrefNO3(isce_nitr)+(xub_PrefNO3(isce_nitr)-xlb_PrefNO3(isce_nitr))*nrand
              PrefNO3(isce_nitr)=c(cont2)
            ENDIF
          ENDDO
          IF (xchk_kminc) THEN 
	          cont2=cont2+1
	          CALL RANDOM_NUMBER(nrand)
	          c(cont2)=xlb_kminc+(xub_kminc-xlb_kminc)*nrand
              kminc2=c(cont2)
          ENDIF
          IF (xchk_knitc) THEN 
	          cont2=cont2+1
	          CALL RANDOM_NUMBER(nrand)
	          c(cont2)=xlb_knitc+(xub_knitc-xlb_knitc)*nrand
              knitc2=c(cont2)
          ENDIF
          IF (xchk_kdesc) THEN 
	          cont2=cont2+1
	          CALL RANDOM_NUMBER(nrand)
	          c(cont2)=xlb_kdesc+(xub_kdesc-xlb_kdesc)*nrand
              kdesc2=c(cont2)
          ENDIF
          IF (xchk_mtd) THEN 
	          cont2=cont2+1
	          CALL RANDOM_NUMBER(nrand)
	          c(cont2)=xlb_mtd+(xub_mtd-xlb_mtd)*nrand
              mtd=c(cont2)
          ENDIF
          IF (xchk_tethas) THEN 
	          cont2=cont2+1
	          CALL RANDOM_NUMBER(nrand)
	          c(cont2)=xlb_tethas+(xub_tethas-xlb_tethas)*nrand
              tethas=c(cont2)
          ENDIF
          IF (xchk_topts) THEN 
	          cont2=cont2+1
	          CALL RANDOM_NUMBER(nrand)
	          c(cont2)=xlb_topts+(xub_topts-xlb_topts)*nrand
              topts=c(cont2)
          ENDIF
          IF (xchk_tethac) THEN 
	          cont2=cont2+1
	          CALL RANDOM_NUMBER(nrand)
	          c(cont2)=xlb_tethac+(xub_tethac-xlb_tethac)*nrand
              tethac=c(cont2)
          ENDIF
          IF (xchk_toptc) THEN 
	          cont2=cont2+1
	          CALL RANDOM_NUMBER(nrand)
	          c(cont2)=xlb_toptc+(xub_toptc-xlb_toptc)*nrand
              toptc=c(cont2)
          ENDIF
          IF (xchk_fckd) THEN 
	          cont2=cont2+1
	          CALL RANDOM_NUMBER(nrand)
	          c(cont2)=xlb_fckd+(xub_fckd-xlb_fckd)*nrand
              fckd=c(cont2)
          ENDIF

		CALL FcnSCE_gen_nitr
        
        xpar(cont,nparam+1)=fobj
        cont2=0
        
        DO isce_nitr=1,cantUsSueNitr
            IF (xchk_kmin(isce_nitr)) THEN 
	          cont2=cont2+1
              xpar(cont,cont2)=kmin2(isce_nitr)	      
            ENDIF
            IF (xchk_kinm(isce_nitr)) THEN 
	          cont2=cont2+1
	          xpar(cont,cont2)=kinm2(isce_nitr)
            ENDIF
            IF (xchk_kvol(isce_nitr)) THEN 
	          cont2=cont2+1
	          xpar(cont,cont2)=kvol2(isce_nitr)
            ENDIF
            IF (xchk_knit(isce_nitr)) THEN 
	          cont2=cont2+1
	          xpar(cont,cont2)=knit2(isce_nitr)
            ENDIF
            IF (xchk_kfi(isce_nitr)) THEN 
	          cont2=cont2+1
	          xpar(cont,cont2)=kfi2(isce_nitr)
            ENDIF
            IF (xchk_kdes(isce_nitr)) THEN 
	          cont2=cont2+1
	          xpar(cont,cont2)=kdes2(isce_nitr)
            ENDIF
            IF (xchk_F(isce_nitr)) THEN 
	          cont2=cont2+1
	          xpar(cont,cont2)=F2(isce_nitr)
            ENDIF
            IF (xchk_Ndem(isce_nitr)) THEN 
	          cont2=cont2+1
	          xpar(cont,cont2)=Ndem2(isce_nitr)
            ENDIF
            IF (xchk_PrefNO3(isce_nitr)) THEN 
	          cont2=cont2+1
	          xpar(cont,cont2)=PrefNO3(isce_nitr)
            ENDIF
        ENDDO
        IF (xchk_kminc) THEN 
	        cont2=cont2+1
	        xpar(cont,cont2)=kminc2
        ENDIF
        IF (xchk_knitc) THEN 
	        cont2=cont2+1
	        xpar(cont,cont2)=knitc2
        ENDIF
        IF (xchk_kdesc) THEN 
	        cont2=cont2+1
	        xpar(cont,cont2)=kdesc2
        ENDIF
        IF (xchk_mtd) THEN 
	        cont2=cont2+1
	        xpar(cont,cont2)=mtd
        ENDIF
        IF (xchk_tethas) THEN 
	        cont2=cont2+1
	        xpar(cont,cont2)=tethas
        ENDIF
        IF (xchk_topts) THEN 
	        cont2=cont2+1
	        xpar(cont,cont2)=topts
        ENDIF
        IF (xchk_tethac) THEN 
	        cont2=cont2+1
	        xpar(cont,cont2)=tethac
        ENDIF
        IF (xchk_toptc) THEN 
	        cont2=cont2+1
	        xpar(cont,cont2)=toptc
        ENDIF
        IF (xchk_fckd) THEN 
	        cont2=cont2+1
	        xpar(cont,cont2)=fckd
        ENDIF
        
        CALL imprestad_nitr

		DO iecc=1,nparam
		  b(qvar,iecc)=c(iecc)
		ENDDO
		b(qvar,nparam+1)=fobj
	  ENDIF
	ENDIF
  ENDDO
  !f)
  !Reemplaza los padres por la descendencia
  DO jecc=1,qvar
    DO kecc=1,mvar
	  IF (lecc(jecc).eq.kecc) THEN
	    DO iecc=1,nparam+1
	      dvar(kecc,iecc)=b(jecc,iecc)
	    ENDDO
	    EXIT
	  ENDIF
	ENDDO
  ENDDO
  !Ordena nuevamente el vector de
  DO iecc=1,mvar
    DO jecc=iecc+1,mvar
	  IF (dvar(iecc,nparam+1).gt.dvar(jecc,nparam+1)) THEN
        !Cambio de posición
	    DO kecc=1,nparam+1
	      pder=dvar(iecc,kecc)
	      dvar(iecc,kecc)=dvar(jecc,kecc)
	      dvar(jecc,kecc)=pder				 
	    ENDDO
	  ENDIF
	ENDDO
  ENDDO
ENDDO
!Regresa al programa principal SCE-UA
	
END SUBROUTINE

!**********************************************************
!* Subrutina que inicia la primera etapa del metodo del
!* Suffled Complex Evolution - University of Arizona
!**********************************************************
SUBROUTINE SCEUA_gen_nit
USE modtet
!USE DFLIB
IMPLICIT NONE

REAL varPrinted

ALLOCATE(xguess_kmin(cantUsSueNitr),xguess_kinm(cantUsSueNitr),xguess_kvol(cantUsSueNitr),xguess_knit(cantUsSueNitr),xguess_kfi(cantUsSueNitr),xguess_kdes(cantUsSueNitr))
ALLOCATE(xguess_F(cantUsSueNitr),xguess_Ndem(cantUsSueNitr),xguess_PrefNO3(cantUsSueNitr))
ALLOCATE(xlb_kmin(cantUsSueNitr),xlb_kinm(cantUsSueNitr),xlb_kvol(cantUsSueNitr),xlb_knit(cantUsSueNitr),xlb_kfi(cantUsSueNitr),xlb_kdes(cantUsSueNitr))
ALLOCATE(xlb_F(cantUsSueNitr),xlb_Ndem(cantUsSueNitr),xlb_PrefNO3(cantUsSueNitr))
ALLOCATE(xub_kmin(cantUsSueNitr),xub_kinm(cantUsSueNitr),xub_kvol(cantUsSueNitr),xub_knit(cantUsSueNitr),xub_kfi(cantUsSueNitr),xub_kdes(cantUsSueNitr))
ALLOCATE(xub_F(cantUsSueNitr),xub_Ndem(cantUsSueNitr),xub_PrefNO3(cantUsSueNitr))
ALLOCATE(xchk_kmin(cantUsSueNitr),xchk_kinm(cantUsSueNitr),xchk_kvol(cantUsSueNitr),xchk_knit(cantUsSueNitr),xchk_kfi(cantUsSueNitr),xchk_kdes(cantUsSueNitr))
ALLOCATE(xchk_F(cantUsSueNitr),xchk_Ndem(cantUsSueNitr),xchk_PrefNO3(cantUsSueNitr))

!Inicializa variables segun fichero con datos (Rango de Optimizacion)
INQUIRE (FILE=artem,EXIST=existe)
IF (.NOT.existe) THEN
  mensaje=strings(442)
  errr=1
  CALL errores  
  idpon=1 !Ponderación con el área por defecto
  idfo=8  !funcion objetivo HMLE por defecto
  fol=2.0 !Lambda de la funcion objetivo HMLE  
ELSE
  OPEN(10,file=artem,err=247)
  READ(10,*) nifo !Intervalo de tiempo inicial para evaluación de F.O.
  READ(10,*) idfo !Código entero de F.O. seleccionada
  !!!Nash-Sutc: idfo=6
  !!!RMSE: idfo=4
  !!!HMLE: idfo=8
  !!!RMSE mensual: idfo=10  
  !!!KGE: idfo=13
  !!!Volumen acum.: idfo=5
  READ(10,*) fol !Parám. de forma lambda para el HMLE
  READ(10,*) folon !Longitud mes para RMSE mensual
  READ(10,*) idpon !Ponderación con áreas de las cuencas (1 ó 0)

  CLOSE(10)
ENDIF


OPEN(10, file=TRIM(ADJUSTL(dirtra))//'xguess_nitr-SCEUA.txt',err=248)
DO i=1,cantUsSueNitr
    READ(10,*,err=249)xguess_kmin(i),xguess_kinm(i),xguess_kvol(i),xguess_knit(i),xguess_kfi(i),xguess_kdes(i),xguess_F(i),xguess_Ndem(i),xguess_PrefNO3(i)
    !Transformacion
    !xguess_kmin(i)=xguess_kmin(i)/(24*60)*dtmin !días-1, dt-1
    !xguess_kinm(i)=xguess_kinm(i)/(24*60)*dtmin !días-1, dt-1
    !xguess_kvol(i)=xguess_kvol(i)/(24*60)*dtmin !días-1, dt-1
    !xguess_knit(i)=xguess_knit(i)/(24*60)*dtmin !días-1, dt-1
    !xguess_kfi(i)=xguess_kfi(i)*arcel/10000/(24*60)*dtmin !Transformación kg/hadía a kg/celdadt
    !xguess_kdes(i)=xguess_kdes(i)/(24*60)*dtmin !días-1, dt-1
    !xguess_F(i)=xguess_F(i)*1000/(24*60)*dtmin !Transformación de m/día a mm/dt
    !xguess_Ndem(i)=xguess_Ndem(i)*arcel/10000/365/(24*60)*dtmin !Transformación de kg/haaño a Kg/celdadt    
ENDDO
READ(10,*,err=249)xguess_kminc,xguess_knitc,xguess_kdesc
    !xguess_kminc=xguess_kminc/(24*60)*dtmin !días-1, dt-1
    !xguess_knitc=xguess_knitc/(24*60)*dtmin !días-1, dt-1
    !xguess_kdesc=xguess_kdesc/(24*60)*dtmin !días-1, dt-1
READ(10,*,err=249)xguess_mtd,xguess_tethas,xguess_topts,xguess_tethac,xguess_toptc,xguess_fckd
CLOSE(10)
OPEN(10, file=TRIM(ADJUSTL(dirtra))//'xub_nitr-SCEUA.txt',err=250)
DO i=1,cantUsSueNitr
    READ(10,*,err=251)xub_kmin(i),xub_kinm(i),xub_kvol(i),xub_knit(i),xub_kfi(i),xub_kdes(i),xub_F(i),xub_Ndem(i),xub_PrefNO3(i)
    !Transformacion
    !xub_kmin(i)=xub_kmin(i)/(24*60)*dtmin !días-1, dt-1
    !xub_kinm(i)=xub_kinm(i)/(24*60)*dtmin !días-1, dt-1
    !xub_kvol(i)=xub_kvol(i)/(24*60)*dtmin !días-1, dt-1
    !xub_knit(i)=xub_knit(i)/(24*60)*dtmin !días-1, dt-1
    !xub_kfi(i)=xub_kfi(i)*arcel/10000/(24*60)*dtmin !Transformación kg/hadía a kg/celdadt
    !xub_kdes(i)=xub_kdes(i)/(24*60)*dtmin !días-1, dt-1
    !xub_F(i)=xub_F(i)*1000/(24*60)*dtmin !Transformación de m/día a mm/dt
    !xub_Ndem(i)=xub_Ndem(i)*arcel/10000/365/(24*60)*dtmin !Transformación de kg/haaño a Kg/celdadt  
ENDDO
READ(10,*,err=251)xub_kminc,xub_knitc,xub_kdesc
    !xub_kminc=xub_kminc/(24*60)*dtmin !días-1, dt-1
    !xub_knitc=xub_knitc/(24*60)*dtmin !días-1, dt-1
    !xub_kdesc=xub_kdesc/(24*60)*dtmin !días-1, dt-1
READ(10,*,err=251)xub_mtd,xub_tethas,xub_topts,xub_tethac,xub_toptc,xub_fckd
CLOSE(10)

OPEN(10, file=TRIM(ADJUSTL(dirtra))//'xlb_nitr-SCEUA.txt',err=252)
DO i=1,cantUsSueNitr
    READ(10,*,err=253)xlb_kmin(i),xlb_kinm(i),xlb_kvol(i),xlb_knit(i),xlb_kfi(i),xlb_kdes(i),xlb_F(i),xlb_Ndem(i),xlb_PrefNO3(i)
    !Transformacion
    !xlb_kmin(i)=xlb_kmin(i)/(24*60)*dtmin !días-1, dt-1
    !xlb_kinm(i)=xlb_kinm(i)/(24*60)*dtmin !días-1, dt-1
    !xlb_kvol(i)=xlb_kvol(i)/(24*60)*dtmin !días-1, dt-1
    !xlb_knit(i)=xlb_knit(i)/(24*60)*dtmin !días-1, dt-1
    !xlb_kfi(i)=xlb_kfi(i)*arcel/10000/(24*60)*dtmin !Transformación kg/hadía a kg/celdadt
    !xlb_kdes(i)=xlb_kdes(i)/(24*60)*dtmin !días-1, dt-1
    !xlb_F(i)=xlb_F(i)*1000/(24*60)*dtmin !Transformación de m/día a mm/dt
    !xlb_Ndem(i)=xlb_Ndem(i)*arcel/10000/365/(24*60)*dtmin !Transformación de kg/haaño a Kg/celdadt 
ENDDO
READ(10,*,err=253)xlb_kminc,xlb_knitc,xlb_kdesc
    !xlb_kminc=xlb_kminc/(24*60)*dtmin !días-1, dt-1
    !xlb_knitc=xlb_knitc/(24*60)*dtmin !días-1, dt-1
    !xlb_kdesc=xlb_kdesc/(24*60)*dtmin !días-1, dt-1
READ(10,*,err=253)xlb_mtd,xlb_tethas,xlb_topts,xlb_tethac,xlb_toptc,xlb_fckd
CLOSE(10)

OPEN(10, file=TRIM(ADJUSTL(dirtra))//'xchk_nitr-SCEUA.txt',err=254)
DO i=1,cantUsSueNitr
    READ(10,*,err=255)xchk_kmin(i),xchk_kinm(i),xchk_kvol(i),xchk_knit(i),xchk_kfi(i),xchk_kdes(i),xchk_F(i),xchk_Ndem(i),xchk_PrefNO3(i)
ENDDO
READ(10,*,err=255)xchk_kminc,xchk_knitc,xchk_kdesc
READ(10,*,err=255)xchk_mtd,xchk_tethas,xchk_topts,xchk_tethac,xchk_toptc,xchk_fckd
CLOSE(10)
    
cont=0

WRITE(*,*)strings(820)

!!Escribe rango de variables al fichero ~var.txt
!OPEN(9,file=arch(26),status='replace')
!DO isce=1,16
!  WRITE(9,*) xlb(isce),xub(isce),xguess(isce),xchk(isce)
!ENDDO
!CLOSE(9)

!Calcula el número de parámetros
nparam=0

DO isce_nitr=1,cantUsSueNitr
    IF(xchk_kmin(isce_nitr)) nparam=nparam+1
    IF(xchk_kinm(isce_nitr)) nparam=nparam+1
    IF(xchk_kvol(isce_nitr)) nparam=nparam+1
    IF(xchk_knit(isce_nitr)) nparam=nparam+1
    IF(xchk_kfi(isce_nitr)) nparam=nparam+1
    IF(xchk_kdes(isce_nitr)) nparam=nparam+1
    IF(xchk_F(isce_nitr)) nparam=nparam+1
    IF(xchk_Ndem(isce_nitr)) nparam=nparam+1
    IF(xchk_PrefNO3(isce_nitr)) nparam=nparam+1   
END DO
IF(xchk_kminc) nparam=nparam+1
IF(xchk_knitc) nparam=nparam+1
IF(xchk_kdesc) nparam=nparam+1
IF(xchk_mtd) nparam=nparam+1
IF(xchk_tethas) nparam=nparam+1
IF(xchk_topts) nparam=nparam+1
IF(xchk_tethac) nparam=nparam+1
IF(xchk_toptc) nparam=nparam+1
IF(xchk_fckd) nparam=nparam+1


!Define variables iniciales del SCE-UA (p,q,m,s)
mvar=2*nparam+1
pvar=2*nparam+1         
pmin=INT(pvar/2)
svar=pvar*mvar
qvar=nparam+1
nrand=0.5

IF(allocated(dvar))deallocate(dvar)
IF(allocated(xpar))deallocate(xpar)
ALLOCATE (dvar(svar,nparam+1),xpar(max(2000,(2*nparam+1)**3),nparam+1+(kno+kam+kni)*13)) 
xpar=0.0
dvar=0.0

!Aleatoriamente selecciona "s" puntos siguiendo una FDP uniforme
DO ksce=1,svar
  cont2=0
  
  DO isce_nitr=1,cantUsSueNitr !(Vicente) Añadimos 2 ultimos FCs
    IF (xchk_kmin(isce_nitr)) THEN 
	  cont2=cont2+1
	  CALL RANDOM_NUMBER(nrand)
	  dvar(ksce,cont2)=xlb_kmin(isce_nitr)+(xub_kmin(isce_nitr)-xlb_kmin(isce_nitr))*nrand
    ENDIF
    IF (xchk_kinm(isce_nitr)) THEN 
	  cont2=cont2+1
	  CALL RANDOM_NUMBER(nrand)
	  dvar(ksce,cont2)=xlb_kinm(isce_nitr)+(xub_kinm(isce_nitr)-xlb_kinm(isce_nitr))*nrand
    ENDIF
    IF (xchk_kvol(isce_nitr)) THEN 
	  cont2=cont2+1
	  CALL RANDOM_NUMBER(nrand)
	  dvar(ksce,cont2)=xlb_kvol(isce_nitr)+(xub_kvol(isce_nitr)-xlb_kvol(isce_nitr))*nrand
    ENDIF
    IF (xchk_knit(isce_nitr)) THEN 
	  cont2=cont2+1
	  CALL RANDOM_NUMBER(nrand)
	  dvar(ksce,cont2)=xlb_knit(isce_nitr)+(xub_knit(isce_nitr)-xlb_knit(isce_nitr))*nrand
    ENDIF
    IF (xchk_kfi(isce_nitr)) THEN 
	  cont2=cont2+1
	  CALL RANDOM_NUMBER(nrand)
	  dvar(ksce,cont2)=xlb_kfi(isce_nitr)+(xub_kfi(isce_nitr)-xlb_kfi(isce_nitr))*nrand
    ENDIF
    IF (xchk_kdes(isce_nitr)) THEN 
	  cont2=cont2+1
	  CALL RANDOM_NUMBER(nrand)
	  dvar(ksce,cont2)=xlb_kdes(isce_nitr)+(xub_kdes(isce_nitr)-xlb_kdes(isce_nitr))*nrand
    ENDIF
    IF (xchk_F(isce_nitr)) THEN 
	  cont2=cont2+1
	  CALL RANDOM_NUMBER(nrand)
	  dvar(ksce,cont2)=xlb_F(isce_nitr)+(xub_F(isce_nitr)-xlb_F(isce_nitr))*nrand
    ENDIF
    IF (xchk_Ndem(isce_nitr)) THEN 
	  cont2=cont2+1
	  CALL RANDOM_NUMBER(nrand)
	  dvar(ksce,cont2)=xlb_Ndem(isce_nitr)+(xub_Ndem(isce_nitr)-xlb_Ndem(isce_nitr))*nrand
    ENDIF
    IF (xchk_PrefNO3(isce_nitr)) THEN 
	  cont2=cont2+1
	  CALL RANDOM_NUMBER(nrand)
	  dvar(ksce,cont2)=xlb_PrefNO3(isce_nitr)+(xub_PrefNO3(isce_nitr)-xlb_PrefNO3(isce_nitr))*nrand
    ENDIF
  ENDDO
  IF (xchk_kminc) THEN 
	  cont2=cont2+1
	  CALL RANDOM_NUMBER(nrand)
	  dvar(ksce,cont2)=xlb_kminc+(xub_kminc-xlb_kminc)*nrand
  ENDIF
  IF (xchk_knitc) THEN 
	  cont2=cont2+1
	  CALL RANDOM_NUMBER(nrand)
	  dvar(ksce,cont2)=xlb_knitc+(xub_knitc-xlb_knitc)*nrand
  ENDIF
  IF (xchk_kdesc) THEN 
	  cont2=cont2+1
	  CALL RANDOM_NUMBER(nrand)
	  dvar(ksce,cont2)=xlb_kdesc+(xub_kdesc-xlb_kdesc)*nrand
  ENDIF
  IF (xchk_mtd) THEN 
	  cont2=cont2+1
	  CALL RANDOM_NUMBER(nrand)
	  dvar(ksce,cont2)=xlb_mtd+(xub_mtd-xlb_mtd)*nrand
  ENDIF
  IF (xchk_tethas) THEN 
	  cont2=cont2+1
	  CALL RANDOM_NUMBER(nrand)
	  dvar(ksce,cont2)=xlb_tethas+(xub_tethas-xlb_tethas)*nrand
  ENDIF
  IF (xchk_topts) THEN 
	  cont2=cont2+1
	  CALL RANDOM_NUMBER(nrand)
	  dvar(ksce,cont2)=xlb_topts+(xub_topts-xlb_topts)*nrand
  ENDIF
  IF (xchk_tethac) THEN 
	  cont2=cont2+1
	  CALL RANDOM_NUMBER(nrand)
	  dvar(ksce,cont2)=xlb_tethac+(xub_tethac-xlb_tethac)*nrand
  ENDIF
  IF (xchk_toptc) THEN 
	  cont2=cont2+1
	  CALL RANDOM_NUMBER(nrand)
	  dvar(ksce,cont2)=xlb_toptc+(xub_toptc-xlb_toptc)*nrand
  ENDIF
  IF (xchk_fckd) THEN 
	  cont2=cont2+1
	  CALL RANDOM_NUMBER(nrand)
	  dvar(ksce,cont2)=xlb_fckd+(xub_fckd-xlb_fckd)*nrand
  ENDIF
ENDDO


 cont2=0
 DO isce_nitr=1,cantUsSueNitr
    IF (xchk_kmin(isce_nitr)) THEN 
	  cont2=cont2+1
	  dvar(1,cont2)=xguess_kmin(isce_nitr)
    ENDIF
    IF (xchk_kinm(isce_nitr)) THEN 
	  cont2=cont2+1
	  dvar(1,cont2)=xguess_kinm(isce_nitr)
    ENDIF
    IF (xchk_kvol(isce_nitr)) THEN 
	  cont2=cont2+1
	  dvar(1,cont2)=xguess_kvol(isce_nitr)
    ENDIF
    IF (xchk_knit(isce_nitr)) THEN 
	  cont2=cont2+1
	  dvar(1,cont2)=xguess_knit(isce_nitr)
    ENDIF
    IF (xchk_kfi(isce_nitr)) THEN 
	  cont2=cont2+1
	  dvar(1,cont2)=xguess_kfi(isce_nitr)
    ENDIF
    IF (xchk_kdes(isce_nitr)) THEN 
	  cont2=cont2+1
	  dvar(1,cont2)=xguess_kdes(isce_nitr)
    ENDIF
    IF (xchk_F(isce_nitr)) THEN 
	  cont2=cont2+1
	  dvar(1,cont2)=xguess_F(isce_nitr)
    ENDIF
    IF (xchk_Ndem(isce_nitr)) THEN 
	  cont2=cont2+1
	  dvar(1,cont2)=xguess_Ndem(isce_nitr)
    ENDIF
    IF (xchk_PrefNO3(isce_nitr)) THEN 
	  cont2=cont2+1
	  dvar(1,cont2)=xguess_PrefNO3(isce_nitr)
    ENDIF
  ENDDO
  IF (xchk_kminc) THEN 
	  cont2=cont2+1
	  dvar(1,cont2)=xguess_kminc
  ENDIF
  IF (xchk_knitc) THEN 
	  cont2=cont2+1
	  dvar(1,cont2)=xguess_knitc
  ENDIF
  IF (xchk_kdesc) THEN 
	  cont2=cont2+1
	  dvar(1,cont2)=xguess_kdesc
  ENDIF
  IF (xchk_mtd) THEN 
	  cont2=cont2+1
	  dvar(1,cont2)=xguess_mtd
  ENDIF
  IF (xchk_tethas) THEN 
	  cont2=cont2+1
	  dvar(1,cont2)=xguess_tethas
  ENDIF
  IF (xchk_topts) THEN 
	  cont2=cont2+1
	  dvar(1,cont2)=xguess_topts
  ENDIF
  IF (xchk_tethac) THEN 
	  cont2=cont2+1
	  dvar(1,cont2)=xguess_tethac
  ENDIF
  IF (xchk_toptc) THEN 
	  cont2=cont2+1
	  dvar(1,cont2)=xguess_toptc
  ENDIF
  IF (xchk_fckd) THEN 
	  cont2=cont2+1
	  dvar(1,cont2)=xguess_fckd
  ENDIF

CALL imprencgen_nitr

humr=0.0
!Evalua la función en los "s" puntos seleccionados
DO ksce=1,svar
  cont2=0  
  DO isce_nitr=1,cantUsSueNitr
    IF (xchk_kmin(isce_nitr)) THEN 
	  cont2=cont2+1
	  kmin2(isce_nitr)=dvar(ksce,cont2)
    ENDIF
    IF (xchk_kinm(isce_nitr)) THEN 
	  cont2=cont2+1
	  kinm2(isce_nitr)=dvar(ksce,cont2)
    ENDIF
    IF (xchk_kvol(isce_nitr)) THEN 
	  cont2=cont2+1
	  kvol2(isce_nitr)=dvar(ksce,cont2)
    ENDIF
    IF (xchk_knit(isce_nitr)) THEN 
	  cont2=cont2+1
	  knit2(isce_nitr)=dvar(ksce,cont2)
    ENDIF
    IF (xchk_kfi(isce_nitr)) THEN 
	  cont2=cont2+1
	  kfi2(isce_nitr)=dvar(ksce,cont2)
    ENDIF
    IF (xchk_kdes(isce_nitr)) THEN 
	  cont2=cont2+1
	  kdes2(isce_nitr)=dvar(ksce,cont2)
    ENDIF
    IF (xchk_F(isce_nitr)) THEN 
	  cont2=cont2+1
	  F2(isce_nitr)=dvar(ksce,cont2)
    ENDIF
    IF (xchk_Ndem(isce_nitr)) THEN 
	  cont2=cont2+1
	  Ndem2(isce_nitr)=dvar(ksce,cont2)
    ENDIF
    IF (xchk_PrefNO3(isce_nitr)) THEN 
	  cont2=cont2+1
	  PrefNO3(isce_nitr)=dvar(ksce,cont2)
    ENDIF
  ENDDO
  IF (xchk_kminc) THEN 
	  cont2=cont2+1
	  kminc2=dvar(ksce,cont2)
  ENDIF
  IF (xchk_knitc) THEN 
	  cont2=cont2+1
	  knitc2=dvar(ksce,cont2)
  ENDIF
  IF (xchk_kdesc) THEN 
	  cont2=cont2+1
	  kdesc2=dvar(ksce,cont2)
  ENDIF
  IF (xchk_mtd) THEN 
	  cont2=cont2+1
	  mtd=dvar(ksce,cont2)
  ENDIF
  IF (xchk_tethas) THEN 
	  cont2=cont2+1
	  tethas=dvar(ksce,cont2)
  ENDIF
  IF (xchk_topts) THEN 
	  cont2=cont2+1
	  topts=dvar(ksce,cont2)
  ENDIF
  IF (xchk_tethac) THEN 
	  cont2=cont2+1
	  tethac=dvar(ksce,cont2)
  ENDIF
  IF (xchk_toptc) THEN 
	  cont2=cont2+1
	  toptc=dvar(ksce,cont2)
  ENDIF
  IF (xchk_fckd) THEN 
	  cont2=cont2+1
	  fckd=dvar(ksce,cont2)
  ENDIF

  CALL FcnSCE_gen_nitr

  dvar(ksce,nparam+1)=fobj
  cont2=0
  DO isce_nitr=1,cantUsSueNitr
    IF (xchk_kmin(isce_nitr)) THEN 
	  cont2=cont2+1
	  xpar(cont,cont2)=dvar(ksce,cont2)
    ENDIF
    IF (xchk_kinm(isce_nitr)) THEN 
	  cont2=cont2+1
	  xpar(cont,cont2)=dvar(ksce,cont2)
    ENDIF
    IF (xchk_kvol(isce_nitr)) THEN 
	  cont2=cont2+1
	  xpar(cont,cont2)=dvar(ksce,cont2)
    ENDIF
    IF (xchk_knit(isce_nitr)) THEN 
	  cont2=cont2+1
	  xpar(cont,cont2)=dvar(ksce,cont2)
    ENDIF
    IF (xchk_kfi(isce_nitr)) THEN 
	  cont2=cont2+1
	  xpar(cont,cont2)=dvar(ksce,cont2)
    ENDIF
    IF (xchk_kdes(isce_nitr)) THEN 
	  cont2=cont2+1
	  xpar(cont,cont2)=dvar(ksce,cont2)
    ENDIF
    IF (xchk_F(isce_nitr)) THEN 
	  cont2=cont2+1
	  xpar(cont,cont2)=dvar(ksce,cont2)
    ENDIF
    IF (xchk_Ndem(isce_nitr)) THEN 
	  cont2=cont2+1
	  xpar(cont,cont2)=dvar(ksce,cont2)
    ENDIF
    IF (xchk_PrefNO3(isce_nitr)) THEN 
	  cont2=cont2+1
	  xpar(cont,cont2)=dvar(ksce,cont2)
    ENDIF
  ENDDO
  IF (xchk_kminc) THEN 
	  cont2=cont2+1
	  xpar(cont,cont2)=dvar(ksce,cont2)
  ENDIF
  IF (xchk_knitc) THEN 
	  cont2=cont2+1
	  xpar(cont,cont2)=dvar(ksce,cont2)
  ENDIF
  IF (xchk_kdesc) THEN 
	  cont2=cont2+1
	  xpar(cont,cont2)=dvar(ksce,cont2)
  ENDIF
  IF (xchk_mtd) THEN 
	  cont2=cont2+1
	  xpar(cont,cont2)=dvar(ksce,cont2)
  ENDIF
  IF (xchk_tethas) THEN 
	  cont2=cont2+1
	  xpar(cont,cont2)=dvar(ksce,cont2)
  ENDIF
  IF (xchk_topts) THEN 
	  cont2=cont2+1
	  xpar(cont,cont2)=dvar(ksce,cont2)
  ENDIF
  IF (xchk_tethac) THEN 
	  cont2=cont2+1
	  xpar(cont,cont2)=dvar(ksce,cont2)
  ENDIF
  IF (xchk_toptc) THEN 
	  cont2=cont2+1
	  xpar(cont,cont2)=dvar(ksce,cont2)
  ENDIF
  IF (xchk_fckd) THEN 
	  cont2=cont2+1
	  xpar(cont,cont2)=dvar(ksce,cont2)
  ENDIF   
  xpar(cont,nparam+1)=dvar(ksce,nparam+1)
  CALL imprestad_nitr
ENDDO

DO WHILE (pmin.lt.pvar)
  !Ordena los "s" puntos descendentemente
  DO isce=1,svar
    DO jsce=isce+1,svar
	  IF (dvar(isce,nparam+1).gt.dvar(jsce,nparam+1)) THEN
        !Cambio de posición
		DO ksce=1,nparam+1
	      pder=dvar(isce,ksce)
		  dvar(isce,ksce)=dvar(jsce,ksce)
		  dvar(jsce,ksce)=pder
		ENDDO
	  ENDIF
	ENDDO
  ENDDO
	
  !Realiza la partición en "p" complejos
  DO ksce=1,pvar      
    CALL eccNM_gen_nitr
  ENDDO
	
  !Reemplaza los complejos en el vector original
  write(*,*) strings(821),cont
   
  !Criterio de convergencia		  ??REVISAR!!!
  fvalue=99999.
  DO ksce=1,svar
    !write(*,*)(d(k,i),i=1,n+1)
    IF (dvar(ksce,nparam+1).lt.fvalue) THEN
      fvalue=dvar(ksce,nparam+1)
      cont2=0     

        DO isce_nitr=1,cantUsSueNitr
        IF (xchk_kmin(isce_nitr)) THEN 
	        cont2=cont2+1
	        kmin2(isce_nitr)=dvar(ksce,cont2)
        ENDIF
        IF (xchk_kinm(isce_nitr)) THEN 
	        cont2=cont2+1
	        kinm2(isce_nitr)=dvar(ksce,cont2)
        ENDIF
        IF (xchk_kvol(isce_nitr)) THEN 
	        cont2=cont2+1
	        kvol2(isce_nitr)=dvar(ksce,cont2)
        ENDIF
        IF (xchk_knit(isce_nitr)) THEN 
	        cont2=cont2+1
	        knit2(isce_nitr)=dvar(ksce,cont2)
        ENDIF
        IF (xchk_kfi(isce_nitr)) THEN 
	        cont2=cont2+1
	        kfi2(isce_nitr)=dvar(ksce,cont2)
        ENDIF
        IF (xchk_kdes(isce_nitr)) THEN 
	        cont2=cont2+1
	        kdes2(isce_nitr)=dvar(ksce,cont2)
        ENDIF
        IF (xchk_F(isce_nitr)) THEN 
	        cont2=cont2+1
	        F2(isce_nitr)=dvar(ksce,cont2)
        ENDIF
        IF (xchk_Ndem(isce_nitr)) THEN 
	        cont2=cont2+1
	        Ndem2(isce_nitr)=dvar(ksce,cont2)
        ENDIF
        IF (xchk_PrefNO3(isce_nitr)) THEN 
	        cont2=cont2+1
	        PrefNO3(isce_nitr)=dvar(ksce,cont2)
        ENDIF
        ENDDO
        IF (xchk_kminc) THEN 
	        cont2=cont2+1
	        kminc2=dvar(ksce,cont2)
        ENDIF
        IF (xchk_knitc) THEN 
	        cont2=cont2+1
	        knitc2=dvar(ksce,cont2)
        ENDIF
        IF (xchk_kdesc) THEN 
	        cont2=cont2+1
	        kdesc2=dvar(ksce,cont2)
        ENDIF
        IF (xchk_mtd) THEN 
	        cont2=cont2+1
	        mtd=dvar(ksce,cont2)
        ENDIF
        IF (xchk_tethas) THEN 
	        cont2=cont2+1
	        tethas=dvar(ksce,cont2)
        ENDIF
        IF (xchk_topts) THEN 
	        cont2=cont2+1
	        topts=dvar(ksce,cont2)
        ENDIF
        IF (xchk_tethac) THEN 
	        cont2=cont2+1
	        tethac=dvar(ksce,cont2)
        ENDIF
        IF (xchk_toptc) THEN 
	        cont2=cont2+1
	        toptc=dvar(ksce,cont2)
        ENDIF
        IF (xchk_fckd) THEN 
	        cont2=cont2+1
	        fckd=dvar(ksce,cont2)
        ENDIF 
	ENDIF
  ENDDO
 
  !Resultados
  write(*,*) strings(822)
  
  DO isce_nitr=1,cantUsSueNitr
    IF (xchk_kmin(isce_nitr)) THEN
	    write(*,'(A12,I1,x,F15.5)')'kmin Use-',isce_nitr,kmin2(isce_nitr)
    ENDIF
    IF (xchk_kinm(isce_nitr)) THEN 
	    write(*,'(A12,I1,x,F15.5)')'kinm Use-',isce_nitr,kinm2(isce_nitr)
    ENDIF
    IF (xchk_kvol(isce_nitr)) THEN 
	    write(*,'(A12,I1,x,F15.5)')'kvol Use-',isce_nitr,kvol2(isce_nitr)
    ENDIF
    IF (xchk_knit(isce_nitr)) THEN 
	    write(*,'(A12,I1,x,F15.5)')'knit Use-',isce_nitr,knit2(isce_nitr)
    ENDIF
    IF (xchk_kfi(isce_nitr)) THEN
	    write(*,'(A12,I1,x,F15.5)')'kfi Use-',isce_nitr,kfi2(isce_nitr)
    ENDIF
    IF (xchk_kdes(isce_nitr)) THEN 
	    write(*,'(A12,I1,x,F15.5)')'kdes Use-',isce_nitr,kdes2(isce_nitr)
    ENDIF
    IF (xchk_F(isce_nitr)) THEN 
	    write(*,'(A12,I1,x,F15.5)')'F Use-',isce_nitr,F2(isce_nitr)
    ENDIF
    IF (xchk_Ndem(isce_nitr)) THEN 
	    write(*,'(A12,I1,x,F15.5)')'Ndem Use-',isce_nitr,Ndem2(isce_nitr)
    ENDIF
    IF (xchk_PrefNO3(isce_nitr)) THEN 
	    write(*,'(A12,I1,x,F15.5)')'PrefNO3 Use-',isce_nitr,PrefNO3(isce_nitr)
    ENDIF
ENDDO
IF (xchk_kminc) THEN
	write(*,'(A7,x,F15.5)')'kminc',kminc2
ENDIF
IF (xchk_knitc) THEN
	write(*,'(A7,x,F15.5)')'knitc',knitc2
ENDIF
IF (xchk_kdesc) THEN 
	write(*,'(A7,x,F15.5)')'kdesc',kdesc2
ENDIF
IF (xchk_mtd) THEN 
	write(*,'(A7,x,F15.5)')'mtd',mtd
ENDIF
IF (xchk_tethas) THEN 
	write(*,'(A7,x,F15.5)')'tethas',tethas
ENDIF
IF (xchk_topts) THEN 
	write(*,'(A7,x,F15.5)')'topts',topts
ENDIF
IF (xchk_tethac) THEN 
	write(*,'(A7,x,F15.5)')'tethac',tethac
ENDIF
IF (xchk_toptc) THEN 
	write(*,'(A7,x,F15.5)')'toptc',toptc
ENDIF
IF (xchk_fckd) THEN 
	write(*,'(A7,x,F15.5)')'fckd',fckd
ENDIF
  if(idfo.eq.6.or.idfo.eq.13)then!Vicente(10-2020)NSE,KGE
    WRITE(*,*)strings(825),1.0-fvalue
  else
    WRITE(*,*)strings(825),fvalue
  endif
  WRITE(33,'(F14.6)') fvalue
  DO i=1,cantUsSueNitr
      WRITE(33,'(9(F14.6,x))') kmin2(i),kinm2(i),kvol2(i),knit2(i),kfi2(i),kdes2(i),F2(i),Ndem2(i),PrefNO3(i)
  ENDDO
  WRITE(33,'(3(F14.6,x))')kminc2,knitc2,kdesc2
  WRITE(33,'(6(F14.6,x))')mtd,tethas,topts,tethac,toptc,fckd
  
  pvar=pvar-1	       !Sugerencia de Sorooshian et al. (1993)
  CALL DATE_AND_TIME(dia,hora)
  WRITE(*,*)'Fecha:'//dia(7:8)//'/'//dia(5:6)//'/'//dia(1:4)//' hora '//hora(1:2)//':'//hora(3:4)//':'//hora(5:8)
ENDDO
CLOSE (13)
122 FORMAT(<nparam+1>F13.5,2x,<naf*9>F13.4,x,I7)
GOTO 95

247 mensaje=strings(442)
errr=1
CALL errores
GOTO 94
248 mensaje=strings(433)
errr=1
CALL errores
GOTO 94
249 mensaje=strings(434)
errr=1
CALL errores
GOTO 94
250 mensaje=strings(435)
errr=1
CALL errores
GOTO 94
251 mensaje=strings(436)
errr=1
CALL errores
GOTO 94
252 mensaje=strings(437)
errr=1
CALL errores
GOTO 94
253 mensaje=strings(438)
errr=1
CALL errores
GOTO 94
254 mensaje=strings(439)
errr=1
CALL errores
GOTO 94
255 mensaje=strings(440)
errr=1
CALL errores
GOTO 94

94 WRITE(*,*)strings(800)

95 END SUBROUTINE