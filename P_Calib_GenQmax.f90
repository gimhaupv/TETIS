! ***************************************************************
! * Programa para la calibración automática de los parámetros
! * o factores correctores R() del modelo TETIS-SP de simulación
! * usando el SCE-UA propuesto por Duan et al (1992)
!*  Ultima actualización: Julio 2016
! ***************************************************************
program calib_gen
!USE DFLIB
USE IFPORT
USE modtet
IMPLICIT NONE
character copyFicheros*256

!INTEGER nx
!integer massimocodveg
!CHARACTER*20 ffich1
!CHARACTER*50 ffich2
!LOGICAL(KIND=4)CHECKED
!INTEGER nargu,ist
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
!IF(config(5)) CALL leearchveg(dirtra,archveg,sale)
!IF (sale.eq.2) GOTO 95
!
!!Lee parametros geomorfologicos
!CALL lee_pargeo
!IF (sale.eq.2) GOTO 95
!
!
!!IF (config(4)) THEN
!!  !Lee nombre de los ficheros con sedimentos
!!  CALL leearchsed(dirtra,archsed,sale)
!!  IF (sale.eq.2) GOTO 95
!!ENDIF
!CALL labels
!
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
!CALL lee_pcon
!IF (sale.eq.2) GOTO 95
!
!!Lee fichero con factores de cultivo mensuales para el calculo de la ETP
!IF(config(5)) THEN
!    CALL leecalibveg
!ELSE
!    INQUIRE (FILE=arch(6),EXIST=existe)
!     IF (existe)THEN
!       CALL leefactoret !Ojo que acá se vuelven a leer los settings, por eso no conviene cambiar antes el config(4) en caso de ser .true.!!!!!!
!     ELSE
!       IF (ALLOCATED(lamb)) DEALLOCATE(lamb)
!       ALLOCATE(lamb(1,13))
!  
!       OPEN(28,file=arch(6),status='new')
!  
!       WRITE(28,*)'*'
!       WRITE(28,*)'*'
!   
!       lamb(1,1)=1.0
!       lamb(1,2)=1.0
!       lamb(1,3)=1.0
!       lamb(1,4)=1.0
!       lamb(1,5)=1.0
!       lamb(1,6)=1.0
!       lamb(1,7)=1.0
!       lamb(1,8)=1.0
!       lamb(1,9)=1.0
!       lamb(1,10)=1.0
!       lamb(1,11)=1.0
!       lamb(1,12)=1.0
!       lamb(1,13)=0.0
!       WRITE(28,980)(lamb(1,j), j=1,13)
!       CLOSE(28)
!     ENDIF 
!    980 FORMAT (<13>F12.5)
!END IF
!
!!CALL leefactoret !Aquí se vuelven a leer los settings, por eso no conviene cambiar antes el config(4) en caso de ser .true.
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
!
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
!            mensaje='009 El fichero '//TRIM(ADJUSTL(arch(36)))//' no existe'
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
!nb=MAX(kppt,kevp,ktem,kniv)
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
!  config(4)=.false. !/se añade en calibAutom para que no haga todo el balance de sedimentos si queremos calibrar sólo la hidrología 
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
!    modulos2(3)=.false.
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
!
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
!IF (config(2)) THEN    
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
!  DO n=1,ncel
!    IF (cell(n).codrie.eq.i) banriego(i)=banriego(i)+1
!  ENDDO
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
!  ncp=cell(n).codpar
!  DO i=2,3
!	nw(i,ncp)=areaumbral(i,ncp)/arcelkm
!  ENDDO
!ENDDO
!
!!(Vicente) No es necesario pues config(4) se ha establecido FALSE previamente
!!IF (config(4)) THEN  !Lee condiciones iniciales de sedimentos
!!  artem=archsed(7)
!!  CALL lee_sedantec
!!  IF (sale.eq.2) GOTO 95
!!ENDIF
!
!IF (simevents) THEN
!    arch(4)=estadohum(1) !si se está usando la calib autom multievento aquí lee el ficher hantec del primer evento considerado
!END IF
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
!!IF (config(5)) THEN
!!  DO n=1,ncel
!!    cell(n).imx=lamb(cell(n).codveg,13)
!!    cell(n).kleaf1=lamb(cell(n).codveg,14)
!!    cell(n).kleaf2=lamb(cell(n).codveg,15)
!!    cell(n).kleaf3=lamb(cell(n).codveg,16)
!!    cell(n).kleaf4=lamb(cell(n).codveg,17)
!!    cell(n).kleaf5=lamb(cell(n).codveg,18)
!!    cell(n).tmx=lamb(cell(n).codveg,19)
!!    cell(n).alfa=lamb(cell(n).codveg,20)
!!    cell(n).cveg=lamb(cell(n).codveg,21)
!!    cell(n).qveg=lamb(cell(n).codveg,22)
!!  ENDDO
!!ELSE
!!  DO n=1,ncel
!!    cell(n).imx=lamb(cell(n).codveg,13)
!!  ENDDO
!!ENDIF  
!
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
!        mensaje=strings(117)
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
!
!!Guiomar (28/01/2014): el imax es la 13 columna del factorETmes sólo en el caso de no tener activada la vegetación dinámica. Por eso, he añadido un if
!IF (.NOT.(config(5))) THEN
!  DO n=1,ncel
!    cell(n).imx=lamb(cell(n).codveg,13)
!  ENDDO
!END IF
!
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

CALL iniciaTetis(6,sale) !6, porque se llama desde CalibQMax
if(sale.eq.2)go to 95

artem=TRIM(ADJUSTL(dirtra))//'resumenCalibQMax-SCEUA.txt'
OPEN (33,file=artem)

CALL SCEUA_qMax_gen

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
  ENDIF
ENDDO
REWIND(33)
DO i=1,ncon
  write(*,*)ncon,i,pizq
   !READ(33,'(2F12.4)')pder,r(10)
   READ(33,*)pder,r(10)
  IF (pder.eq.pizq) THEN
    artem=arch(1)
    CALL escri_parg
    artem=arch(2)
    CALL escri_calib
    EXIT
  ENDIF
ENDDO
CLOSE (33)

!artem=TRIM(ADJUSTL(dirtra))//'resumenCalibQMax-SCEUA.txt'
!resul=DELFILESQQ(artem)
!res=SYSTEM('cp "' // TRIM(ADJUSTL(dirtra))//TRIM(ADJUSTL('~errores.txt')) // '" "' // TRIM(ADJUSTL(dirtra))//TRIM(ADJUSTL('errores.txt')) // '" /Y')
!res=DELFILESQQ(TRIM(ADJUSTL(dirtra))//TRIM(ADJUSTL('~errores.txt')))
!Subtituimos la rutina para no emplear librerias(Vicente.Nov-2019)
CALL deletefich(TRIM(ADJUSTL(dirtra))//'resumenCalibQMax-SCEUA.txt')

GOTO 95

206 mensaje=strings(101)
errr=1
CALL errores
GOTO 94
207 mensaje=strings(102)
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
248 mensaje=strings(61)
errr=1
CALL errores
GOTO 94
249 mensaje= strings(109)
errr=1
CALL errores
GOTO 94
250 mensaje=strings(901)
errr=2
CALL errores
GO TO 94
257 mensaje=strings(312)
errr=1
CALL errores
GO TO 94

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
SUBROUTINE FcnSCE_qMax_gen
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

!Inicializa los caudales simulados en cero
!DO t=0,nt
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
!ENDDO

!Inicializa la humedad antecedente y umbral del tanque 5 (u5)
artem=arch(4)
CALL lee_human

!Inicia variables de estado
CALL inicia_var

!Realiza la simulación en cada celda durante todo el tiempo
CALL sim_tpo

! Llama subrutina que calcula estadisticos
CALL calc_estad_qMax

95 cont=cont+1

WRITE(*,*)'iteracion ',cont,'fobj',fobj_qMax,' Q max sim',qMax_sim



END	SUBROUTINE


! ******************************************************************
! * Subrutina de evolución competitiva de complejos CCE (Nelder y
! * Mead 1965.
! ******************************************************************
SUBROUTINE eccNM_gen
USE modtet
IMPLICIT NONE

REAL b(qvar_qMax,nparam_qMax+1),tri(mvar_qMax),c(nparam_qMax),g(nparam_qMax),lecc(qvar_qMax)

alfafo=1	  !Parámetro especificado por el usuario
betafo=mvar_qMax
nrand=0.5
	
DO ib=1,betafo
  !Asignación de distribución de probabilidad triangular
  DO iecc=1,mvar_qMax
    tri(iecc)=2.0*(mvar_qMax+1.0-iecc)/(mvar_qMax*(mvar_qMax+1.0))
  ENDDO

  !Selección aleatoria del subcomplejo de "q" puntos (PADRES)
  jecc=1
  ban=0
  lecc=0
  DO WHILE (jecc.le.qvar_qMax)
    CALL RANDOM_NUMBER(nrand)
	ban=0
	DO iecc=1,mvar_qMax
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
	    DO kecc=1,nparam_qMax+1
	      b(jecc,kecc)=dvar_qMax(iecc,kecc)
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
    DO iecc=1,qvar_qMax
	  DO jecc=iecc+1,qvar_qMax
	    IF (b(iecc,nparam_qMax+1).gt.b(jecc,nparam_qMax+1)) THEN
          !Cambio de posición
	      DO kecc=1,nparam_qMax+1
	        pder=b(iecc,kecc)
	        b(iecc,kecc)=b(jecc,kecc)
	        b(jecc,kecc)=pder
	      ENDDO
	    ENDIF
	  ENDDO
    ENDDO
    !Calcula el valor del centroide g
	DO iecc=1,nparam_qMax
	  sum=0.0
	  DO jecc=1,qvar_qMax-1
	    sum=sum+b(jecc,iecc)
	  ENDDO
	  g(iecc)=(1.0/(qvar_qMax-1.0))*sum
	ENDDO
	! b) Calcula el nuevo punto r
	cont2=1
	r(10)=2.0*g(1)-b(qvar_qMax,1)
	! c) Verifica que el punto r se encuentre en el espacio factible H y sigue a d)
	! de lo contrario genera un valor aleatorio dentro del espacio (mutacion)
	IF (r(10).lt.xlb_qMax.OR.r(10).gt.xub_qMax) THEN
	      !Genera aleatoriamente un número cualquiera
        CALL RANDOM_NUMBER(nrand)
  	    r(10)=xlb_qMax+(xub_qMax-xlb_qMax)*nrand
	ENDIF
	
	!Calculo el valor de Funcion para el nuevo punto
	CALL FcnSCE_qMax_gen
    
    
	WRITE(13,'(3(F13.5,2x),I7)')r(10),qMax_sim,fobj_qMax,cont  

	! d) Decide si hay contraccion
	cont2=0
	
	IF (fobj_qMax.lt.b(qvar_qMax,nparam_qMax+1)) THEN  !es minimizacion, así que es "menor que"
	  cont2=1
	  b(qvar_qMax,cont2)=r(10)
	  b(qvar_qMax,nparam_qMax+1)=fobj_qMax
	ELSE
	  !Contraccion
	  cont2=1
	  c(cont2)=(g(cont2)+b(qvar_qMax,cont2))/2.0
	  r(10)=c(cont2)
	  
	  CALL FcnSCE_qMax_gen

      WRITE(13,'(3(F13.5,2x),I7)')r(10),qMax_sim,fobj_qMax,cont            
 
	  !e)
	  IF (fobj_qMax.lt.b(qvar_qMax,nparam_qMax+1)) THEN   !es minimizacion, así que es "menor que"
	    DO iecc=1,nparam_qMax
	      b(qvar_qMax,iecc)=c(iecc)
	    ENDDO
	    b(qvar_qMax,nparam_qMax+1)=fobj_qMax
	  ELSE
	    !Genera aleatoriamente un nueva valor
		  cont2=1
		  CALL RANDOM_NUMBER(nrand)
		  c(cont2)=xlb_qMax+(xub_qMax-xlb_qMax)*nrand
		  r(10)=c(cont2)
		  
		CALL FcnSCE_qMax_gen
	
        WRITE(13,'(2(F13.5,2x),I7)')r(10),qMax_sim,fobj_qMax,cont 
        
		DO iecc=1,nparam_qMax
		  b(qvar_qMax,iecc)=c(iecc)
		ENDDO
		b(qvar_qMax,nparam_qMax+1)=fobj_qMax
	  ENDIF
	ENDIF
  ENDDO  
  !f)
  !Reemplaza los padres por la descendencia
  DO jecc=1,qvar_qMax
    DO kecc=1,mvar_qMax
	  IF (lecc(jecc).eq.kecc) THEN
	    DO iecc=1,nparam_qMax+1
	      dvar_qMax(kecc,iecc)=b(jecc,iecc)
	    ENDDO
	    EXIT
	  ENDIF
	ENDDO
  ENDDO  
  !Ordena nuevamente el vector de
  DO iecc=1,mvar_qMax
    DO jecc=iecc+1,mvar_qMax
	  IF (dvar_qMax(iecc,nparam_qMax+1).gt.dvar_qMax(jecc,nparam_qMax+1)) THEN
        !Cambio de posición
	    DO kecc=1,nparam_qMax+1
	      pder=dvar_qMax(iecc,kecc)
	      dvar_qMax(iecc,kecc)=dvar_qMax(jecc,kecc)
	      dvar_qMax(jecc,kecc)=pder				 
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
SUBROUTINE SCEUA_qMax_gen
USE modtet
!USE DFLIB
IMPLICIT NONE

WRITE(*,*)strings(820)

!Inicializa variables segun fichero con datos (Rango de Optimizacion)
OPEN(10,file=TRIM(ADJUSTL(dirtra))//'VarQmax-SCEUA.txt',err=249)
READ(10,*) nifo_qMax
READ(10,*) estObj_qMax !Estacion a calibrar
READ(10,*) qMaxObj !Código entero de F.O. seleccionada
READ(10,*,err=248)xlb_qMax,xub_qMax,xguess_qMax  
CLOSE(10)

cont=0
j = 0
DO l=1,naf
    IF(aforo(l).name.eq.estObj_qMax) THEN        
        pos_estMax = l;
    END IF  
END DO
DO l=1,knaf
    j = l + naf
    IF(otros(l).name.eq.estObj_qMax) THEN        
        pos_estMax = j;
    END IF    
ENDDO

nparam_qMax=1

!Define variables iniciales del SCE-UA (p,q,m,s)
mvar_qMax=2*nparam_qMax+1
pvar_qMax=2*nparam_qMax+1         
pmin_qMax=INT(pvar_qMax/2)
svar_qMax=pvar_qMax*mvar_qMax
qvar_qMax=nparam_qMax+1
nrand=0.5

IF(allocated(dvar_qMax))deallocate(dvar_qMax)
IF(allocated(xpar_qMax))deallocate(xpar_qMax)
ALLOCATE (dvar_qMax(svar_qMax,nparam_qMax+1),xpar_qMax(max(2000,(2*nparam_qMax+1)**3),nparam_qMax+1+naf*18)) 
xpar_qMax=0.0
dvar_qMax=0.0

!Aleatoriamente selecciona "s" puntos siguiendo una FDP uniforme
DO ksce=1,svar_qMax
  cont2=1
  CALL RANDOM_NUMBER(nrand)  !ksce numero de fila, cont2 numero de columna
  dvar_qMax(ksce,cont2)=xlb_qMax+(xub_qMax-xlb_qMax)*nrand  ! crea una matriz dove per ogni parametro a calibrare si generano e salvano 's'=sval valori aleatori di tentativo
END DO
dvar_qMax(1,cont2)=xguess_qMax ! qui sostituisce al primo valore aleatorio quello iniziale proposto dall'usuario
CALL imprencgen_qMax

humr=0.0
!Evalua la función en los "s" puntos seleccionados
DO ksce=1,svar_qMax
  cont2=1
  r(10)=dvar_qMax(ksce,cont2)! Para factor corrector r(10)  
  CALL FcnSCE_qMax_gen
  dvar_qMax(ksce,nparam_qMax+1)=fobj_qMax
  !CALL imprestad
  WRITE(13,'(3(F13.5,2x),I7)')r(10),qMax_sim,fobj_qMax,cont
  
ENDDO

DO WHILE (pmin.lt.pvar_qMax)
  !Ordena los "s" puntos descendentemente
  DO isce=1,svar_qMax
    DO jsce=isce+1,svar_qMax
	  IF (dvar_qMax(isce,nparam_qMax+1).gt.dvar_qMax(jsce,nparam_qMax+1)) THEN
        !Cambio de posición
		DO ksce=1,nparam_qMax+1
	      pder=dvar_qMax(isce,ksce)
		  dvar_qMax(isce,ksce)=dvar_qMax(jsce,ksce)
		  dvar_qMax(jsce,ksce)=pder
		ENDDO
	  ENDIF
	ENDDO
  ENDDO
	
  !Realiza la partición en "p" complejos
  DO ksce=1,pvar_qMax
    CALL eccNM_gen
  ENDDO
	
  !Reemplaza los complejos en el vector original
  write(*,*) strings(821),cont
   
  !Criterio de convergencia		  ??REVISAR!!!
  fvalue_qMax=99999.
  DO ksce=1,svar_qMax
    !write(*,*)(d(k,i),i=1,n+1)
    IF (dvar_qMax(ksce,nparam_qMax+1).lt.fvalue_qMax) THEN
      fvalue_qMax=dvar_qMax(ksce,nparam_qMax+1)
      cont2=1
      r(10)=dvar_qMax(ksce,cont2)	  
	ENDIF
  ENDDO
 
  !Resultados
  write(*,*) strings(822)
  
  write(*,'(A4,x,F15.5)')'R-10',r(10)
  write(*,*) strings(825),fobj_qMax
  write(33,'(2(F11.4,x))')fvalue_qMax,r(10)
  !WRITE(33,'(20F12.4)') fvalue_qMax,(r(ksce),ksce=1,9),betappt,(humr(ksce),ksce=1,5),bbeta,ro1,ro2,tbase
  
  pvar_qMax=pvar_qMax-1	       !Sugerencia de Sorooshian et al. (1993)
  CALL DATE_AND_TIME(dia,hora)
  WRITE(*,*)'Fecha:'//dia(7:8)//'/'//dia(5:6)//'/'//dia(1:4)//' hora '//hora(1:2)//':'//hora(3:4)//':'//hora(5:8)
ENDDO
CLOSE (13)
122 FORMAT(<nparam_qMax+1>F13.5,2x,<naf*9>F13.4,x,I7)
GOTO 95

246 mensaje=strings(109)
errr=1
CALL errores
GOTO 94
247 mensaje=strings(116)
errr=1
CALL errores
GOTO 94
248 mensaje=strings(118)
errr=1
CALL errores
GOTO 94
249 mensaje=strings(117)
errr=1
CALL errores
GOTO 94


94 WRITE(*,*)strings(800)

95 END SUBROUTINE