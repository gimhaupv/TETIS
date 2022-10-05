!***********************************************************************
!* Subrutina de cambio en la longitud del registro del episodio
!* por un intervalo dado de menor longitud (no cambia dt, ni interpola)
!*  Ultima actualización: Julio 2016
!***********************************************************************
Program red_episod
USE IFPORT 
!USE DFLIB
USE modtet

IMPLICIT NONE

INTEGER nano,nano0,mes,ano,hor,min,day0,mes0,ano0,hor0,min0,day,isFin,typeyearRec
INTEGER(8) nto,ntini,ntfin,newnt,t2
REAL tpo
CHARACTER fecinor*11,horinor*8,fecfinor*11,horfinor*8,nnom*256,fecfin2*11,horfin2*8
CHARACTER*2 dd,mm,hh,ss,dd0,mm0,hh0,aa0,ss0
LOGICAL(KIND=4) checked
INTEGER nargu,ist
CHARACTER*30 arg
character copyFicheros*256

isFin=0
!Se hace así para que si hay error, lang tenga un valor y pueda escribir. Por defecto está en inglés
lang=2
!llama la subrutina que define los mensajes de error genéricos, los que no neceistan más información que los nombres de archivo
CALL labels

!Lee nombres de los ficheros a utilizar, por el momento no general el el FILESSP!!!!! filessp tiene que estár!
CALL lecfiles(dirtra,arch,sale)
IF (sale.eq.2) GOTO 94

CALL lee_settings
IF (sale.eq.2) GOTO 94

CALL DATE_AND_TIME(dia,hora)
CALL write_date
WRITE(*,*)''

!llama la subrutina que define los mensajes de error genéricos, los que no neceistan más información que los nombres de archivo
CALL labels

!res=DELFILESQQ(TRIM(ADJUSTL(dirtra))//TRIM(ADJUSTL('errores.txt')))
!Subtituimos la rutina para no emplear librerias(Vicente.Nov-2019)
CALL deletefich(TRIM(ADJUSTL(dirtra))//TRIM(ADJUSTL('errores.txt')))
artem=TRIM(ADJUSTL(dirtra))//TRIM(ADJUSTL('~errores.txt'))  
OPEN(22,file=artem)                                        
WRITE(22,'(A54)') strings(802)
CLOSE(22) 

!Lee parametros geomorfologicos
!CALL lee_pargeo        !!!Lo desactivamos porque no hace falta
!IF (sale.eq.2) GOTO 95

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


dx=(ce-cw)/mi
dy=(cn-cs)/mj

!Lee ficheros del evento (lo hace en dos partes)
CALL lee_evto1
IF (sale.eq.2) GOTO 95
nto=nt
dt=dtmin/60.   !horas
dts=dt*3600.0   !segundos

ALLOCATE(pluvio(kppt))
IF (naf.gt.0) ALLOCATE(aforo(naf))
IF (kevp.gt.0) ALLOCATE(evapo(kevp))
IF (ktem.gt.0) ALLOCATE(temper(ktem))
IF (kniv.gt.0) ALLOCATE(nieve(kniv))
IF (knaf.gt.0) ALLOCATE(otros(knaf))

nb=nemb+vnemb+knemb
IF (nb.gt.0) THEN 
  ALLOCATE(nivel(nb),pulm(nb),emb(nb))
  pulm=0
  ALLOCATE(volum(nb))
  ALLOCATE(qemb(nb))
ENDIF
IF (kadi.gt.0) ALLOCATE(aport(kadi))
IF (ksedq.gt.0) ALLOCATE(aforosed(ksedq))
!**********Sedimentos (06/2016) Cris****************
IF (kadised1.gt.0) ALLOCATE(aportsed1(kadised1))
IF (kadised2.gt.0) ALLOCATE(aportsed2(kadised2))
IF (kadised3.gt.0) ALLOCATE(aportsed3(kadised3))
!Añado las estaciones de vegetación dinámica Cris 23/11/2016
IF (nveg.gt.0) THEN
      ALLOCATE(veg(nveg))
      !(Guiomar-Vicente) alocato nuevas variables de estado
      ALLOCATE(veg1_point(nveg))
      ALLOCATE(veg2_point(nveg))
      ALLOCATE(tr_point(nveg))
ENDIF
IF (nradiacion.gt.0) ALLOCATE(radiacion(nradiacion))

!Nitrogeno
IF (kno.gt.0) ALLOCATE(norg(kno))
IF (kam.gt.0) ALLOCATE(amonio(kam))
IF (kni.gt.0) ALLOCATE(nitrato(kni))
If (naf.gt.0) Allocate(norgql(naf),amonioql(naf),nitratoql(naf),norgqs(naf),amonioqs(naf))



!Lectura de episodio
bint=.true.
IF (config(2)) THEN 
  CALL lee_evto2col
ELSE
  CALL lee_evto2
ENDIF
IF (sale.eq.2) GOTO 95

CALL lee_fechainicio !para liberar el nombre del fichero de entrada
    IF (sale.eq.2) GOTO 95

fecin=archin(1:2)//'/'//archin(3:4)//'/'//archin(11:14)
horin=archin(5:6)//':'//archin(7:8)//':00'
fecinor=fecin
horinor=horin

ntini=1
ntfin=nto

!!
READ(archin(3:4),*)mesin
READ(archin(1:2),*)diain
READ(archin(5:6),*)horain
READ(archin(7:8),*)minin
READ(archin(11:14),*)nyear
minInDay = horain*60 + minin
nhora = INT(horain)
nmonth = INT(mesin)
nday= INT(diain)
nmin = minin



!CALL tposal(fecin,horin,nto,dtmin,fecfin,horfin)

artem=TRIM(ADJUSTL(dirtra))//'Recortepisodio.txt'  !nombre OBLIGATORIO de fichero de entrada
INQUIRE (FILE=artem,EXIST=existe)
IF (.NOT.existe) THEN
  mensaje=strings(919)
  errr=1
  CALL errores
  CALL DATE_AND_TIME(dia,hora)
  CALL write_date
  GOTO 94
ENDIF

OPEN (39,file=artem)
READ (39,10) fecin2,horin2 
horin2=adjustl(trim(horin2))
READ (39,10) fecfin2,horfin2
horfin2=adjustl(trim(horfin2))
READ (39,'(I1)') typeyearRec
CLOSE (39)

!Guiomar: cambios porque tal y como estaba programado sólo contemplaba la posibilidad de que hayan años por encima de 1920 y por debajo de 2019!!
READ(fecin2(1:2),*)day0
READ(fecin2(4:5),*)mes0
READ(fecin2(7:10),*)ano0
READ(horin2(1:2),*)hor0
READ(horin2(4:5),*)min0

READ(fecfin2(1:2),*)day
READ(fecfin2(4:5),*)mes
READ(fecfin2(7:10),*)ano
READ(horfin2(1:2),*)hor
READ(horfin2(4:5),*)min

nano=ano
nano0=ano0

fecin2=fecin2(1:2)//'/'//fecin2(4:5)//'/'//fecin2(7:10)
fecfin2=fecfin2(1:2)//'/'//fecfin2(4:5)//'/'//fecfin2(7:10)

IF (day.gt.31.OR.day0.gt.31.OR.mes.gt.12.OR.mes0.gt.12.OR.nano.lt.nano0) THEN
  !No se recorta
  GOTO 207 
END IF
!Funcionamiento normal
DO t=1,nto-1
    SELECT CASE (INT(mesin))
    CASE (1,3,5,7,8,10,12)
        fmes=31
    CASE (2)
    if(typeyearRec==1) then
        If (mod(nyear,4).eq.0.and.mod(nyear,100).ne.0.or.mod(nyear,400).eq.0) then !Comprobación de año bisiesto                                    
            fmes=29
        Else
	    fmes=28
        End if
    Else
	fmes=28  !Si typeyearRec==2, febrero siempre tiene 28 días
    End if
    CASE (4,6,9,11)
        fmes=30
    END SELECT
     
    !(Vicente) Actualizacion correcta de las fechas. Gracias a fmes se consideran bisiesto segun settings
    minInDay = minInDay + dtmin
    IF (minInDay >= 1440) THEN
        nday = nday + INT(minInDay/1440)
        minInDay = minInDay - INT(minInDay/1440)*1440
        nhora=0
        diain = REAL(nday)
    END IF
    nhora = INT(minInDay/60)
    nmin = minInDay - nhora*60 

    IF(nday>fmes) THEN
        nday = nday - fmes
        diain = REAL(nday)
        nmonth=nmonth+1
        mesin=REAL(nmonth)
    END IF
    IF (nmonth>12) THEN
        nyear = nyear + INT(nmonth/12)
        nmonth= nmonth - INT(nmonth/12)*12      
        mesin=REAL(nmonth)
    END IF
    WRITE(fecfin,'(I2.2,A1,I2.2,A1,I4)')nday,'/',nmonth,'/',nyear
    WRITE(horfin,'(I2.2,A1,I2.2,A3)')nhora,':',nmin,':00'
    
    IF (fecfin.eq.fecin2.AND.horfin.eq.horin2) THEN  !si coincide la fecha inical; se reescribe ntini
        ntini=t+1
        fecinor=fecfin
        horinor=horfin
    ENDIF
    IF (fecfin.eq.fecfin2.AND.horfin.eq.horfin2) THEN  !si coincide la fecha inical; se reescribe ntini
        ntfin=t+1
        fecfinor=fecfin
        horfinor=horfin
        isFin=1
    ENDIF
END DO 
IF(isFin.eq.0) THEN !Si no coincide con ninguna
    fecfinor=fecfin
    horfinor=horfin
ENDIF    

newnt=ntfin-ntini+1

!Define el nuevo nombre del episodio
  !   OPEN(9,file='filessp.txt')
  !   READ(9,'(a128)') dirtra !lee el nombre del directorio de trabajo como una stringa de 128 caracteres incluyendo espacios blancos
  !   ldirtra_=len_trim(dirtra)
  !   !if(dirtra(ldirtra_:ldirtra_).ne.'\')dirtra(ldirtra_+1:ldirtra_+1)='\'
  !   if(dirtra(ldirtra_:ldirtra_).ne.path_separator)dirtra(ldirtra_+1:ldirtra_+1)=path_separator
  !   DO i=1,34
  !     READ(9,'(a128)')arch(i)!lee con formato también los nombres de los ficheros usados de manera que se puedan escribir con espacios
  !   ENDDO
  !CLOSE(9)
nnom=TRIM(ADJUSTL((TRIM(ADJUSTL(arch(5)))//'.cut.txt')))
    
!DO t=1,nto-1
!  IF (fecin.eq.fecin2.AND.horin.eq.horin2) THEN  !se la data e ora iniziale non sono cambiate il passo di tempo iniciale é il primo
!    ntini=t
!  ENDIF
!  CALL tposal(fecin,horin,1,dtmin,fecfin,horfin)
!  fecin=fecfin
!  horin=horfin
!  IF (fecin.eq.fecfin2.AND.horin.eq.horfin2) ntfin=t+1
!ENDDO

!Define el nuevo nombre del episodio
!arch(5)=TRIM(ADJUSTL(dirtra))//TRIM(ADJUSTL(nnom))
arch(5)=nnom

!Escribe el nuevo episodio (En formato CEDEX)
OPEN (14,file=arch(5))
WRITE (14,'(A2)') '* '
!fecinor=fecin2
!horinor=horin2
!DO t=ntini,ntfin
!  fecin=fecfin
!  horin=horfin
!  CALL tposal(fecin,horin,1,dtmin,fecfin,horfin)
!ENDDO
!WRITE (14,'(A25,3x,A10,x,A5,x,A8,x,A10,x,A5)') '* Intervalo de fechas  : ',fecin2,horin2(1:5),' hasta ',fecfin2,horfin2(1:5)
WRITE (14,'(A25,3x,A10,x,A5,x,A8,x,A10,x,A5)') '* Intervalo de fechas  : ',fecinor,horinor(1:5),' hasta ',fecfinor,horfinor(1:5) !Se cambia para establecer la correcta fecha de corte
WRITE (14,'(A25,I5,A4)') '* Incremento de tiempo : ',INT(dtmin),' min'
WRITE (14,'(A25,I5)') '* Número de Intervalos : ',newnt
WRITE (14,'(A47)')    '* Datos en mm. por intervalo para pluviometros.'
WRITE (14,'(A61)') '* Datos en m. al final del intervalo para niveles de embalse.'
WRITE (14,'(A67)') '* Datos en m³/s al final del intervalo para caudales desembalsados.'
WRITE (14,'(A44)') '* Coordenadas XYZ UTM (huso 30) aproximadas.'
WRITE (14,'(A2)') '* '

!fecfin=fecinor
!horfin=horinor
!DO t=ntini,ntfin
!  fecin=fecfin
!  horin=horfin
!  CALL tposal(fecin,horin,1,dtmin,fecfin,horfin)
!ENDDO

!Escribe linea F si es formato columna
fecinor(3:3)='-'
fecinor(6:6)='-'
!WRITE (14,'(A2,5x,A10,2x,A10)') 'F ',fecinor,horinor  !para que escriba la linea F en los dos casos, de fichero horizontal y columna
WRITE (14,'(A2,5x,A10,2x,A10)') 'F ',fecinor,horinor !Se cambia para establecer la correcta fecha de corte
WRITE (14,'(A2)') '* '
aa='G '
WRITE (14,'(A2,5x,I5,2x,I5)') aa,newnt,INT(dtmin)
WRITE (14,'(A2)') '* '
67 FORMAT(A53,<nto>I8)
68 FORMAT(A53,<nto>A8)
10 FORMAT(A10,x,A8)

IF (config(2)) THEN
  !Escribe localizacion de las estaciones
  WRITE(14,'(A45)') '* RESUMEN DE INFORMACION SOBRE LAS ESTACIONES'
  WRITE (14,'(A2)') '* '
  WRITE(14,'(A70)') '* "Nombre de la estacion    "   Este(UTM-X) Norte(UTM-Y)   Cota(msnm) '!  celda-(i,j)'
  DO i=1,kppt
    WRITE(14,75)pluvio(i).codigo,'"',pluvio(i).name,'"',INT(pluvio(i).utmx),  &
                INT(pluvio(i).utmy),INT(pluvio(i).elev)
  ENDDO
  DO i=1,nemb
    WRITE(14,75)nivel(i).codigo,'"',nivel(i).name,'"',INT(nivel(i).utmx),  &
                INT(nivel(i).utmy),INT(nivel(i).elev)
  ENDDO
  DO i=nemb+1,nemb+vnemb !(Vicente 30/03/2017)
    WRITE(14,75)volum(i).codigo,'"',volum(i).name,'"',INT(volum(i).utmx),  &
                INT(volum(i).utmy),INT(volum(i).elev)
  ENDDO
  DO i=nemb+vnemb+1,nemb+vnemb+knemb !(Vicente 30/03/2017)
    WRITE(14,75)qemb(i).codigo,'"',qemb(i).name,'"',INT(qemb(i).utmx),  &
                INT(qemb(i).utmy),INT(qemb(i).elev)
  ENDDO
  DO i=1,naf
    WRITE(14,75)aforo(i).codigo,'"',aforo(i).name,'"',INT(aforo(i).utmx),  &
                INT(aforo(i).utmy),INT(aforo(i).elev)
  ENDDO
  DO i=1,knaf
    WRITE(14,75)otros(i).codigo,'"',otros(i).name,'"',INT(otros(i).utmx),  &
                INT(otros(i).utmy),INT(otros(i).elev)
  ENDDO
  DO i=1,kniv
    WRITE(14,75)nieve(i).codigo,'"',nieve(i).name,'"',INT(nieve(i).utmx),  &
                INT(nieve(i).utmy),INT(nieve(i).elev)
  ENDDO
  DO i=1,ktem
    WRITE(14,75)temper(i).codigo,'"',temper(i).name,'"',INT(temper(i).utmx),  &
                INT(temper(i).utmy),INT(temper(i).elev)
  ENDDO
  DO i=1,kevp
    WRITE(14,75)evapo(i).codigo,'"',evapo(i).name,'"',INT(evapo(i).utmx),  &
              INT(evapo(i).utmy),INT(evapo(i).elev)
  ENDDO
  DO i=1,kadi
    WRITE(14,75)aport(i).codigo,'"',aport(i).name,'"',INT(aport(i).utmx),  &
                INT(aport(i).utmy),INT(aport(i).elev)
  ENDDO
  !***************Sedimentos (06/2016) Cris********************
  DO i=1,ksedq
    WRITE(14,75)aforosed(i).codigo,'"',aforosed(i).name,'"',INT(aforosed(i).utmx),  &
                INT(aforosed(i).utmy),INT(aforosed(i).elev)
  ENDDO
  !****************Vegetación dinámica Cris (23/11/2016)****************
  DO i=1,nveg
    WRITE(14,75)veg(i).codigo,'"',veg(i).name,'"',INT(veg(i).utmx),  &
                INT(veg(i).utmy),INT(veg(i).elev)
  ENDDO  
  DO i=1,nradiacion
    WRITE(14,75)radiacion(i).codigo,'"',radiacion(i).name,'"',INT(radiacion(i).utmx),  &
                INT(radiacion(i).utmy),INT(radiacion(i).elev)
  ENDDO  
 !***************Sedimentos (06/2016) Cris********************
  DO i=1,kadised1
    WRITE(14,75)aportsed1(i).codigo,'"',aportsed1(i).name,'"',INT(aportsed1(i).utmx),  &
                INT(aportsed1(i).utmy),INT(aportsed1(i).elev)
  ENDDO
  DO i=1,kadised2
    WRITE(14,75)aportsed2(i).codigo,'"',aportsed2(i).name,'"',INT(aportsed2(i).utmx),  &
                INT(aportsed2(i).utmy),INT(aportsed2(i).elev)
  ENDDO
  DO i=1,kadised3
    WRITE(14,75)aportsed3(i).codigo,'"',aportsed3(i).name,'"',INT(aportsed3(i).utmx),  &
                INT(aportsed3(i).utmy),INT(aportsed3(i).elev)
  ENDDO
  !***************Nitrogeno (12/2017) Cris Vicente********************
  DO i=1,kno
    WRITE(14,75) norg(i).codigo,'"',norg(i).name,'"',INT(norg(i).utmx),INT(norg(i).utmy),INT(norg(i).elev)
  End do
  DO i=1,kam
    WRITE(14,75) amonio(i).codigo,'"',amonio(i).name,'"',INT(amonio(i).utmx),INT(amonio(i).utmy),INT(amonio(i).elev)
  End do
  DO i=1,kni
    WRITE(14,75) nitrato(i).codigo,'"',nitrato(i).name,'"',INT(nitrato(i).utmx),INT(nitrato(i).utmy),INT(nitrato(i).elev)
  End do
75 FORMAT(A2,x,A1,A25,A1,x,3(x,I11))
  WRITE (14,'(A2)') '* '

  !Escribe series temporales en el mismo orden
  !El fichero de columna debe tener el orden P,N,V,S,Q,B,H,T,E,D,X,W,R,DA,DL,DC
  WRITE (14,'(A30)') '* Series temporales de entrada' 
  WRITE (14,'(A2)') '* '
  WRITE(14,'(A1,A9,<ktotal>(A7,A2))') '*',' ------DT',(' ------',pluvio(j).codigo,j=1,kppt),  &
               (' ------',nivel(j).codigo,j=1,nemb),(' ------',volum(j).codigo,j=1,vnemb),  &
	           (' ------',qemb(j).codigo,j=1,knemb),(' ------',aforo(j).codigo,j=1,naf),    &
	           (' ------',otros(j).codigo,j=1,knaf),(' ------',nieve(j).codigo,j=1,kniv),   &
	           (' ------',temper(j).codigo,j=1,ktem),(' ------',evapo(j).codigo,j=1,kevp),  &
	           (' ------',aport(j).codigo,j=1,kadi),(' ------',aforosed(j).codigo,j=1,ksedq), &
	           (' ------',veg(j).codigo,j=1,nveg),(' ------',radiacion(j).codigo,j=1,nradiacion), &
	           (' ------',aportsed1(j).codigo,j=1,kadised1),(' ------',aportsed2(j).codigo,j=1,kadised2), &
	           (' ------',aportsed3(j).codigo,j=1,kadised3),&    !!Añadido Sedimentos (06/2016) Cris Vegetación (23/11/2016)
               (' ------',norg(j).codigo,j=1,kno), (' ------',amonio(j).codigo,j=1,kam),(' ------',nitrato(j).codigo,j=1,kni) !!Añadido Nitrogeno (12/2017)
  orig=0.0
  t1=0
  DO t=ntini,ntfin
    t1=t1+1
    tpo=t1*dtmin
    WRITE(14,'(<ktotal+1>(x,F8.3))')tpo,(pluvio(j).obs(t),j=1,kppt),  &
       (nivel(j).obs(t),j=1,nemb),(volum(j).obs(t),j=1,vnemb),(qemb(j).obs(t),j=1,knemb), &
	   (aforo(j).obs(t),j=1,naf),(orig,j=1,knaf),(nieve(j).obs(1),j=1,kniv),       &
 	   (temper(j).obs(t),j=1,ktem),(evapo(j).obs(t),j=1,kevp),(aport(j).obs(t),j=1,kadi),(aforosed(j).obs(t),j=1,ksedq), &
 	   (veg(j).obs,j=1,nveg),(radiacion(j).obs,j=1,nradiacion),(aportsed1(j).obs(t),j=1,kadised1), &
 	   (aportsed2(j).obs(t),j=1,kadised2),(aportsed3(j).obs(t),j=1,kadised3), &!Añadido Sedimentos (06/2016) Cris Vegetación (23/11/2016)
       (norg(j).obs(t),j=1,kno),(amonio(j).obs(t),j=1,kam),(nitrato(j).obs(t),j=1,kni)!!Añadido Nitrogeno (12/2017)
  ENDDO
ELSE
    !Guiomar (28/05/2014): desactivo algunas líneas porque no son necesarias y quedan mal en el archivo de entrada
 ! WRITE (14,67) '* Nombre                           X      Y     Z   O',(t,t=1,newnt)
 ! WRITE (14,68) '* ----- --------------------- ------ ------- ---- ---',(' -------',t=1,newnt)
  orig=0.0
  DO k=1,kppt
    WRITE (14,30) pluvio(k).codigo,pluvio(k).name,INT(pluvio(k).utmx),INT(pluvio(k).utmy),  &
	             INT(pluvio(k).elev),orig,(pluvio(k).obs(t),t=ntini,ntfin)
  ENDDO

  DO j=1,nemb      !Nivel del embalse 
    WRITE (14,36) nivel(j).codigo,nivel(j).name,INT(nivel(j).utmx),INT(nivel(j).utmy),   &
	              INT(nivel(j).elev),orig,(nivel(j).obs(t),t=ntini,ntfin)	 
  ENDDO

  DO j=1,vnemb    !Volumen en el embalse 
  WRITE (14,36) volum(j).codigo,volum(j).name,INT(volum(j).utmx),INT(volum(j).utmy),     &
                INT(volum(j).elev),orig,(volum(j).obs(t),t=ntini,ntfin)	 
  ENDDO

  DO j=1,knemb    !Q salida del embalse 
    WRITE (14,36) qemb(j).codigo,qemb(j).name,INT(qemb(j).utmx),INT(qemb(j).utmy),   &
	              INT(qemb(j).elev),orig,(qemb(j).obs(t),t=ntini,ntfin)	 
  ENDDO

  DO j=1,naf
    WRITE (14,35) aforo(j).codigo,aforo(j).name,INT(aforo(j).utmx),INT(aforo(j).utmy),  &
	              INT(aforo(j).elev),orig,(aforo(j).obs(t),t=ntini,ntfin)
  ENDDO

  DO j=1,knaf
    WRITE (14,35) otros(j).codigo,otros(j).name,INT(otros(j).utmx),INT(otros(j).utmy),  &
	              INT(otros(j).elev)
  ENDDO

  IF (ktem.ne.0)THEN
    DO j=1,kniv
      WRITE (14,34) nieve(j).codigo,nieve(j).name,INT(nieve(j).utmx),INT(nieve(j).utmy),   &
	                INT(nieve(j).elev),orig,nieve(j).obs(1)
    ENDDO
    DO j=1,ktem
      WRITE (14,35) temper(j).codigo,temper(j).name,INT(temper(j).utmx),INT(temper(j).utmy),  &
	                INT(temper(j).elev),orig,(temper(j).obs(t),t=ntini,ntfin)
    ENDDO
  ENDIF

  DO j=1,kevp
    DO t=ntini,ntfin
      evapo(j).obs(t)=evapo(j).obs(t)*24.0/dt  !mm/dia
    ENDDO
    WRITE (14,35) evapo(j).codigo,evapo(j).name,INT(evapo(j).utmx),INT(evapo(j).utmy),    &
                  INT(evapo(j).elev),orig,(evapo(j).obs(t),t=ntini,ntfin)
  ENDDO

  DO j=1,kadi
    WRITE (14,30) aport(j).codigo,aport(j).name,INT(aport(j).utmx),INT(aport(j).utmy),   &
	              INT(aport(j).elev),orig,(aport(j).obs(t),t=ntini,ntfin)
  ENDDO
  IF (ksedq.ne.0) THEN
      DO j=1,ksedq
          WRITE (14,35) aforosed(j).codigo,aforosed(j).name,INT(aforosed(j).utmx),INT(aforosed(j).utmy),   &
	              INT(aforosed(j).elev),orig,(aforosed(j).obs(t),t=ntini,ntfin)
      ENDDO
  ENDIF
    !****************Vegetación dinámica Cris (23/11/2016)****************
  If (nradiacion.ne.0) then
    Do j=1,nradiacion
        WRITE(14,35)radiacion(j).codigo,radiacion(j).name,INT(radiacion(j).utmx),INT(radiacion(j).utmy),  &
                INT(radiacion(j).elev),orig,(radiacion(j).obs(t),t=ntini,ntfin)
    End do  
  End if
  If (nveg.ne.0) then
    Do j=1,nveg
        WRITE(14,35)veg(j).codigo,veg(j).name,INT(veg(j).utmx),INT(veg(j).utmy),  &
                INT(veg(j).elev),orig,(veg(j).obs(t),t=ntini,ntfin)
    End do
  End if  
  !***********Cambios Sedimentos (06/2016) Cris**************
  IF (kadised1.ne.0) THEN
      DO j=1,kadised1
          WRITE (14,35) aportsed1(j).codigo,aportsed1(j).name,INT(aportsed1(j).utmx),INT(aportsed1(j).utmy),   &
	              INT(aportsed1(j).elev),orig,(aportsed1(j).obs(t),t=ntini,ntfin)
      ENDDO
  ENDIF
  IF (kadised2.ne.0) THEN
      DO j=1,kadised2
          WRITE (14,35) aportsed2(j).codigo,aportsed2(j).name,INT(aportsed2(j).utmx),INT(aportsed2(j).utmy),   &
	              INT(aportsed2(j).elev),orig,(aportsed2(j).obs(t),t=ntini,ntfin)
      ENDDO
  ENDIF
  IF (kadised3.ne.0) THEN
      DO j=1,kadised3
          WRITE (14,35) aportsed3(j).codigo,aportsed3(j).name,INT(aportsed3(j).utmx),INT(aportsed3(j).utmy),   &
	              INT(aportsed3(j).elev),orig,(aportsed3(j).obs(t),t=ntini,ntfin)
      ENDDO
  ENDIF
  !***************Nitrogeno (12/2017) Cris Vicente********************
  IF (kno.ne.0) THEN
      DO j=1,kno
        WRITE(14,35) norg(j).codigo,norg(j).name,INT(norg(j).utmx),INT(norg(j).utmy),INT(norg(j).elev),orig,(norg(j).obs(t),t=ntini,ntfin)
      End do
  END IF
  IF (kam.ne.0) THEN
      DO j=1,kam
        WRITE(14,35) amonio(j).codigo,amonio(j).name,INT(amonio(j).utmx),INT(amonio(j).utmy),INT(amonio(j).elev),orig,(amonio(j).obs(t),t=ntini,ntfin)
      End do
  END IF
  IF (kam.ne.0) THEN
      DO j=1,kni
        WRITE(14,35) nitrato(j).codigo,nitrato(j).name,INT(nitrato(j).utmx),INT(nitrato(j).utmy),INT(nitrato(j).elev),orig,(nitrato(j).obs(t),t=ntini,ntfin)
      End do
  END IF
ENDIF
CLOSE(14)

30 FORMAT(A2,x,'"',A25,'"',x,I7,x,I8,x,I4,x,F3.1,<newnt>(x,F9.2))
34 FORMAT(A2,x,'"',A25,'"',x,I7,x,I8,x,I4,x,F3.1,x,F9.3)
35 FORMAT(A2,x,'"',A25,'"',x,I7,x,I8,x,I4,x,F3.1,<newnt>(x,F9.3))
36 FORMAT(A2,x,'"',A25,'"',x,I7,x,I8,x,I4,x,F3.1,<newnt>(x,F9.2))
37 FORMAT(A2,x,'"',A25,'"',A10,F8.1,x,A10,F8.1,A7,F8.1,x,A10,F8.2,x,A10,F8.2,x,A10,F8.2,A10,F8.3,A10,F8.3,x,A10,F8.2)

DEALLOCATE(pluvio)
IF (kevp.gt.0) DEALLOCATE(evapo)
IF (naf.gt.0) DEALLOCATE(aforo)
IF (knaf.gt.0) DEALLOCATE(otros)
IF (ktem.gt.0) DEALLOCATE(temper)
IF (kniv.gt.0) DEALLOCATE(nieve)
IF (nemb.gt.0) DEALLOCATE(nivel)
IF (vnemb.gt.0) DEALLOCATE(volum)
IF (knemb.gt.0) DEALLOCATE(qemb)
IF (kadi.gt.0) DEALLOCATE(aport)
!************Sedimentos (06/2016)**************
IF (ksedq.gt.0) DEALLOCATE(aforosed)
IF (kadised1.gt.0) DEALLOCATE(aportsed1)
IF (kadised2.gt.0) DEALLOCATE(aportsed2)
IF (kadised3.gt.0) DEALLOCATE(aportsed3)
!***********Vegetación (23/11/2013)******************
IF (nradiacion.gt.0) DEALLOCATE(radiacion)
IF (nveg.gt.0) DEALLOCATE(veg)
!Nitrogeno
IF (kno.gt.0) DEALLOCATE(norg)
IF (kam.gt.0) DEALLOCATE(amonio)
IF (kni.gt.0) DEALLOCATE(nitrato)


WRITE(*,*) strings(808)
WRITE(*,*) arch(5)
GOTO 95

206 mensaje=strings(101)
errr=1
CALL errores
GOTO 94

207 mensaje=strings(807)
WRITE(*,*) mensaje
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

94 WRITE(*,*)strings(800)

95 CALL libera_mem
!RETURN 

!res=SYSTEM('cp "' // TRIM(ADJUSTL(dirtra))//TRIM(ADJUSTL('~errores.txt')) // '" "' // TRIM(ADJUSTL(dirtra))//TRIM(ADJUSTL('errores.txt')) // '" /Y')
!res=DELFILESQQ(TRIM(ADJUSTL(dirtra))//TRIM(ADJUSTL('~errores.txt')))
!Subtituimos la rutina para no emplear librerias(Vicente.Nov-2019)
if(stma_op==0) then
    !copyFicheros = 'cp "' // TRIM(ADJUSTL(dirtra))//TRIM(ADJUSTL('~errores.txt')) // '" "' // TRIM(ADJUSTL(dirtra))//TRIM(ADJUSTL('errores.txt')) // '" /Y' !Linux
    copyFicheros = '[ -f ' // TRIM(ADJUSTL(dirtra))//TRIM(ADJUSTL('~errores.txt')) //' ] && cp ' // TRIM(ADJUSTL(dirtra))//TRIM(ADJUSTL('~errores.txt')) // ' ' // TRIM(ADJUSTL(dirtra))//TRIM(ADJUSTL('errores.txt')) !Linux
else
    copyFicheros = 'copy "' // TRIM(ADJUSTL(dirtra))//TRIM(ADJUSTL('~errores.txt')) // '" "' // TRIM(ADJUSTL(dirtra))//TRIM(ADJUSTL('errores.txt')) // '" /Y' !Windows
end if
res = SYSTEM(copyFicheros)
CALL deletefich(TRIM(ADJUSTL(dirtra))//TRIM(ADJUSTL('~errores.txt')))

END	program





