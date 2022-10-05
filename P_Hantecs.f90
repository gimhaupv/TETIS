!***********************************************************
!* Rutina que lee humedades generales para generar fichero
!* de humedad antecedente para ejecutar la simulación
!*  Ultima actualización: Julio 2016
!***********************************************************
Program hantecs
!USE DFLIB
USE IFPORT
USE modtet
IMPLICIT NONE

LOGICAL(KIND=4)CHECKED, warn
CHARACTER tit2*11(7)
REAL tc(4) 
integer errrch
      
INTEGER nargu,ist
CHARACTER*30 arg
character copyFicheros*256

!Se hace así para que si hay error, lang tenga un valor y pueda escribir. Por defecto está en inglés
lang=2
!llama la subrutina que define los mensajes de error genéricos, los que no neceistan más información que los nombres de archivo
CALL labels

sale=0
!Lee nombres de los ficheros a utilizar
CALL lecfiles(dirtra,arch,sale)
IF (sale.eq.2) GOTO 94

CALL lee_settings
IF (sale.eq.2) GOTO 95

CALL DATE_AND_TIME(dia,hora)
CALL write_date
WRITE(*,*)''

CALL labels

!res=DELFILESQQ(TRIM(ADJUSTL(dirtra))//TRIM(ADJUSTL('errores.txt')))
!Subtituimos la rutina para no emplear librerias(Vicente.Nov-2019)
CALL deletefich(TRIM(ADJUSTL(dirtra))//TRIM(ADJUSTL('errores.txt')))
artem=TRIM(ADJUSTL(dirtra))//TRIM(ADJUSTL('~errores.txt'))  
OPEN(22,file=artem)                                        
WRITE(22,'(A54)') strings(802)
CLOSE(22) 

!Lee parametros geomorfologicos y valores generales de humedad antecedente
CALL lee_pargeo
IF (sale.eq.2) GOTO 95

CALL lee_fechainicio !para liberar el nombre del fichero de entrada
IF (sale.eq.2) GOTO 95

!Actualiza la fecha del estado de humedad inicial
  fecfin=archin(1:2)//'/'//archin(3:4)//'/'//archin(11:14)
  horfin=archin(5:6)//':'//archin(7:8)//':00'                   !Lee la fecha y hora inicio del episodio desde el mismo fichero de entrada con codigo F

artem=arch(3)
!lee topología (por la unidad 14)
OPEN(14,file=artem,status='old',err=208)
READ (14,*,err=210)tit(1),cn,cs
READ (14,*,err=210)tit(2),ce,cw
READ (14,*,err=210)tit(3),mi
READ (14,*,err=210)tit(4),mj
READ (14,*,err=210)tit(5),ncol
READ (14,*,err=210)tit(6),nfil
READ (14,*,err=210)tit(7),ncel

ALLOCATE(cell(ncel))


CALL lee_topol


dx=(ce-cw)/mi
dy=(cn-cs)/mj
arcel=dx*dy
arcelkm=dx*dy/1000000.0
      
!Lee fichero de evento para determinar el dt
OPEN(12,file=arch(5),status='old',err=209)
ios=0
DO WHILE (ios.ne.-1)
  aa='-*'
  READ (12,10,iostat=ios) aa
  SELECT CASE (aa)
    CASE ('G ')
      BACKSPACE (12)
      READ (12,*,err=211) aa,nt,dtmin
      dt=dtmin/60.
  END SELECT
ENDDO
10 FORMAT(A2)
CLOSE(12)
!Lee parametros de calibracion (factores correctores)
  CALL lee_calib
  IF (sale.eq.2) GOTO 95
!Guiomar (27/03/2014): voy a hacer ligero cambio porque no tiene sentido calcular Imaxmedio en el caso de tener activa la vegetación dinámica
!INQUIRE (FILE=arch(6),EXIST=existe)
!    IF (existe) CALL leefactoret    !Cris y Vicente lo desactivamos porque lo hace bajo (29/09/2017)
IF (config(5))THEN
    Imaxmedio=0.0
    Call leecalibveg !Cris y Vicente lee el fichero de vegetación dinámica (29/09/2017)
ELSE
    INQUIRE (FILE=arch(6),EXIST=existe)
    IF (existe)THEN
        CALL leefactoret
        DO n=1,ncel
        cell(n).imx=lamb(cell(n).codveg,13)
        Imaxmedio=Imaxmedio+cell(n).imx
        ENDDO
    ELSE
        DO n=1,ncel
        cell(n).imx=0
        Imaxmedio=Imaxmedio+cell(n).imx
        ENDDO
 
        mensaje=strings(61)
        !mensaje=strings(63)
        errr=2
        CALL errores(errr,mensaje,lang)
    ENDIF
ENDIF

Imaxmedio=Imaxmedio/ncel  !para calculo del Imax medio 
   
!Calcula la humedad antecedente
WRITE(*,*)strings(804)

warn = .FALSE.
!If (ALLOCATED(nw)) DEALLOCATE(nw)
!Allocate (nw(3,npar)) !Cris (17/10/2016) Regiones homogéneas allocatable, ahora fijo a 50.
DO n=1,ncel
  ncp=cell(n).codpar
  DO i=2,3
	nw(i,ncp)=areaumbral(i,ncp)/arcelkm
  ENDDO
  cell(n).h(0)=0.0
  cell(n).h(1)=0.0
  cell(n).h(2)=0.0
  cell(n).h(3)=0.0
  cell(n).h(4)=0.0
  cell(n).h(5)=0.0
  cell(n).h(6)=0.0
  cell(n).h3max=0.0
  IF (config(5)) THEN
      cell(n).h(8)=0.0
      cell(n).lai=0.0
  ENDIF
  
  IF (cell(n).codpar.eq.0)  THEN
    mensaje=strings(809)
	write(*,*)n,r(1),mensaje
	errrch=2
  ENDIF
  
  cell(n).h3max=alpha/100*cell(n).hu*r(1) 
 IF (config(5)) THEN
     !Guiomar(30/10/2014): es necesario leer factorETmes para que conozca el valor de alm_max (lamb(cell(n).codveg,3)
     !CALL leefactoret  Cris y Vicente lo desactivamos para que no lo lea en cada celda (29/09/2017)
     cell(n).h(8)=cell(n).hu1*wdad(7,ncp)*r(1)/100.
     cell(n).h(1)=cell(n).hu2*wdad(8,ncp)*r(1)/100.
     cell(n).lai=laiini(ncp)
     cell(n).h(6)=lamb(cell(n).codveg,3)*laiini(ncp)*wdad(6,ncp)/100.
 ELSE
    cell(n).h(1)=cell(n).hu*wdad(1,ncp)*r(1)/100.
    cell(n).h(6)=cell(n).imx*wdad(6,ncp)/100.
 ENDIF
  cell(n).h(2)=wdad(2,ncp) !Por ejemplo, inactivo para el Tajo
  
 IF(wdad(3,ncp).gt.cell(n).h3max)THEN  !para cuando se utiliza el mecanismo de DUNNE no hay incoherencias con las condiciones iniciales
    cell(n).h(3)=cell(n).h3max
    warn = .TRUE.
 ELSE
    cell(n).h(3)=wdad(3,ncp)
 ENDIF 

  cell(n).h(4)=wdad(4,ncp)
  IF (nw(2,ncp).gt.cell(n).acum) THEN  !Volumen para celdas de ladera (sin canal)
  cell(n).h(5)=0.0
  ENDIF
  IF (nw(2,ncp).le.cell(n).acum.AND.nw(3,ncp).gt.cell(n).acum) THEN  !para celdas con cárcava o cauce efímero
  cell(n).h(5)=dc(1,ncp)*((arcelkm*cell(n).acum)**ec(1,ncp))*(wdad(5,ncp)/100.0)
  ENDIF
  IF (nw(3,ncp).le.cell(n).acum) THEN !para celdas con cauce permanente
  cell(n).h(5)=d(1,ncp)*((arcelkm*cell(n).acum)**e(1,ncp))*(wdad(5,ncp)/100.0)
  ENDIF
ENDDO

if(errrch.eq.2)then
    mensaje=strings(917)
    errr=errrch
	CALL errores(errr,mensaje,lang)
endif

IF (warn)THEN
    errr=2
    mensaje=strings(918)
    CALL errores(errr,mensaje,lang)
ENDIF

!Lee fichero ASC con las altura de nieve en (mm)
ALLOCATE (mascreal(mi,mj))
INQUIRE (FILE=arch(10),EXIST=existe)
IF (existe) THEN
  artem = arch(10)  
  CALL leeficASCi(mascreal)
  IF (sale.eq.2) GOTO 95
  DO n=1,ncel
    cell(n).h(0)=mascreal(cell(n).fil,cell(n).col)
  ENDDO
ENDIF
DEALLOCATE(mascreal)

!Contiene las humedades antecedentes
artem=arch(4)
CALL escr_wdad

GOTO 95


208 mensaje=strings(31)
GOTO 94
209 mensaje=strings(51)
GOTO 94
210 mensaje=strings(52)
GOTO 94
211 mensaje=strings(501)
GOTO 94 

94 errr=1
   CALL errores(errr,mensaje,lang)
   WRITE(*,*)strings(800)
   
95 CALL libera_mem
   
if(stma_op==0) then
    !copyFicheros = 'cp "' // TRIM(ADJUSTL(dirtra))//TRIM(ADJUSTL('~errores.txt')) // '" "' // TRIM(ADJUSTL(dirtra))//TRIM(ADJUSTL('errores.txt')) // '" /Y' !Linux
    copyFicheros = '[ -f ' // TRIM(ADJUSTL(dirtra))//TRIM(ADJUSTL('~errores.txt')) //' ] && cp ' // TRIM(ADJUSTL(dirtra))//TRIM(ADJUSTL('~errores.txt')) // ' ' // TRIM(ADJUSTL(dirtra))//TRIM(ADJUSTL('errores.txt')) !Linux   
else
    copyFicheros = 'copy "' // TRIM(ADJUSTL(dirtra))//TRIM(ADJUSTL('~errores.txt')) // '" "' // TRIM(ADJUSTL(dirtra))//TRIM(ADJUSTL('errores.txt')) // '" /Y' !Windows
end if
res = SYSTEM (copyFicheros)
CALL deletefich(TRIM(ADJUSTL(dirtra))//TRIM(ADJUSTL('~errores.txt')))

WRITE(*,*)strings(803)
CALL DATE_AND_TIME(dia,hora)
CALL write_date

END Program

