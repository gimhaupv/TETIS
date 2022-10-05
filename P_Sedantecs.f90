!*************************************************************************
!* Rutina que lee  y escribe el estado incial de sedimentos
!*  Ultima actualización: Julio 2016
!*************************************************************************
Program sedantecs
!USE DFLIB
USE IFPORT
USE modtet
IMPLICIT NONE

LOGICAL(KIND=4)CHECKED
CHARACTER tit2*11(7) 
INTEGER nargu,ist
CHARACTER*30 arg
character copyFicheros*256

!Se hace así para que si hay error, lang tenga un valor y pueda escribir. Por defecto está en inglés
lang=2
!llama la subrutina que define los mensajes de error genéricos, los que no neceistan más información que los nombres de archivo
CALL labels



!Lee nombres de los ficheros a utilizar
CALL lecfiles(dirtra,arch,sale)
IF (sale.eq.2) GOTO 95

CALL lee_settings
IF (sale.eq.2) GOTO 95

CALL DATE_AND_TIME(dia,hora)
CALL write_date

CALL labels

!res=DELFILESQQ(TRIM(ADJUSTL(dirtra))//TRIM(ADJUSTL('errores.txt')))
!Subtituimos la rutina para no emplear librerias(Vicente.Nov-2019)
CALL deletefich(TRIM(ADJUSTL(dirtra))//TRIM(ADJUSTL('errores.txt')))
artem=TRIM(ADJUSTL(dirtra))//TRIM(ADJUSTL('~errores.txt'))  
OPEN(22,file=artem)                                        
WRITE(22,'(A54)') strings(802)
CLOSE(22) 


!Lee ficheros de sedimentos
CALL leearchsed(dirtra,archsed,sale)
IF (sale.eq.2) GOTO 95

!Lee parametros geomorfologicos y valores generales de humedad antecedente
CALL lee_pargeo
IF (sale.eq.2) GOTO 95

CALL lee_fechainicio !para liberar el nombre del fichero de entrada
IF (sale.eq.2) GOTO 95

!Actualiza la fecha del estado inicial
fecfin=archin(1:2)//'/'//archin(3:4)//'/'//archin(11:14)
horfin=archin(5:6)//':'//archin(7:8)//':00'                  

!Lee topologia, propiedades del suelo y tipologia
OPEN(14,file=arch(3),status='old',err=209)
READ (14,*,err=210)tit(1),cn,cs
READ (14,*,err=210)tit(2),ce,cw
READ (14,*,err=210)tit(3),mi
READ (14,*,err=210)tit(4),mj
READ (14,*,err=210)tit(5),ncol
READ (14,*,err=210)tit(6),nfil
READ (14,*,err=210)tit(7),ncel
ALLOCATE(cell(ncel+1),banriego(erie),contriego(erie))
CALL lee_topol
IF (sale.eq.2) GOTO 94
CLOSE(14)

!Calcula el estado inicial de sedimentos
WRITE(*,*)strings(805)
!inicializa en cero

INQUIRE(file=TRIM(ADJUSTL(dirtra))//'sedantec_val.txt', EXIST = existe)
IF (.NOT. existe) THEN
  sedantec=0
ELSE
  CALL leesedantecval
ENDIF

!Calcular ancho y área de celdas
dx=(ce-cw)/mi				!ancho en metros
dy=(cn-cs)/mj				!largo en metros
arcel=dx*dy					!area en m2
arcelkm=arcel/1000000.0		!area en km2	
!IF (ALLOCATED(nw)) DEALLOCATE(nw)
!Allocate(nw(3,npar)) Cris(17/10/2016) Regiones homogéneas allocatable, ahora fijo en 50
DO n=1,ncel
ncp=cell(n).codpar
  DO i=2,3
	nw(i,ncp)=areaumbral(i,ncp)/arcelkm
  ENDDO
ENDDO

nw(2,ncp)=(areaumbral(2,ncp)/arcelkm)
nw(3,ncp)=(areaumbral(3,ncp)/arcelkm)

DO n=1,ncel
    cell(n).SusSedLAD=0.0
    cell(n).SusSedRED=0.0
    cell(n).ErodSed=0.0
    cell(n).DepSedLAD=0.0
    cell(n).DepSedRED=0.0
    ncp=cell(n).codpar
    DO i=1,3		!Para todas las clases de tamaño
      cell(n).SusSedLAD(i)=sedantec(i+3,ncp)
      cell(n).SusSedRED(i)=sedantec(i+3,ncp)
	  cell(n).ErodSed(i)=sedantec(i,ncp)
	  !ladera
      IF ((nw(2,ncp).gt.cell(n).acum)) THEN
      cell(n).DepSedLAD(i)=sedantec(i+6,ncp)	  
	  !cárcava
	  ELSE IF ((nw(2,ncp).le.cell(n).acum).and.(nw(3,ncp).ge.cell(n).acum)) THEN
	    cell(n).DepSedRED(i)=sedantec(i+12,ncp)
	  !cauce
	  ELSE IF (nw(3,ncp).lt.cell(n).acum) THEN
        cell(n).DepSedRED(i)=sedantec(i+9,ncp)
	  ENDIF
    ENDDO
ENDDO

artem=archsed(7)	!Contiene el estado inicial de sedimentos
CALL escribe_sedantec

GOTO 95

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
   
if(stma_op==0) then
    !copyFicheros = 'cp "' // TRIM(ADJUSTL(dirtra))//TRIM(ADJUSTL('~errores.txt')) // '" "' // TRIM(ADJUSTL(dirtra))//TRIM(ADJUSTL('errores.txt')) // '" /Y' !Linux
    copyFicheros = '[ -f ' // TRIM(ADJUSTL(dirtra))//TRIM(ADJUSTL('~errores.txt')) //' ] && cp ' // TRIM(ADJUSTL(dirtra))//TRIM(ADJUSTL('~errores.txt')) // ' ' // TRIM(ADJUSTL(dirtra))//TRIM(ADJUSTL('errores.txt')) !Linux
else
    copyFicheros = 'copy "' // TRIM(ADJUSTL(dirtra))//TRIM(ADJUSTL('~errores.txt')) // '" "' // TRIM(ADJUSTL(dirtra))//TRIM(ADJUSTL('errores.txt')) // '" /Y' !Windows
end if
res = SYSTEM (copyFicheros)
CALL deletefich(TRIM(ADJUSTL(dirtra))//TRIM(ADJUSTL('~errores.txt')))   

WRITE(*,*)strings(806)
CALL DATE_AND_TIME(dia,hora)
CALL write_date

!RETURN   
END Program

!**************************************************************************************************
!* Subrutina para poner el estado final de sedimentos como estado inicial (sedimentos depositados)
!**************************************************************************************************
SUBROUTINE  sednewsedold(checked)
!USE DFLIB
USE modtet	
IMPLICIT NONE

LOGICAL(KIND=4)checked 
      
CALL DATE_AND_TIME(dia,hora)
CALL write_date

!Lee nombres de los ficheros a utilizar
CALL lecfiles(dirtra,arch,sale)
IF (sale.eq.2) GOTO 95

!Lee ficheros de sedimentos
CALL leearchsed(dirtra,archsed,sale)
IF (sale.eq.2) GOTO 95

!Lee fichero con el estado final de sedimentos
artem=archsed(8)
CALL lee_sedantec

CALL lee_fechainicio !para liberar el nombre del fichero de entrada
    IF (sale.eq.2) GOTO 95
  
  fecfin=archin(1:2)//'/'//archin(3:4)//'/'//archin(11:14)   
  horfin=archin(5:6)//':'//archin(7:8)//':00'                   

!Escribe el estado inicial de sedimentos
artem=archsed(7)	!Contiene el estado inicial de sedimentos
CALL escribe_sedantec

GOTO 95

209 mensaje=strings(31)
errr=1
CALL errores
GOTO 94

94 WRITE(*,*)strings(800)

95 WRITE(*,*)strings(806)
CALL DATE_AND_TIME(dia,hora)
CALL write_date

DEALLOCATE(cell)
CALL libera_mem

RETURN
END SUBROUTINE

!****************************************************************************************
!* Subrutina para escribir el estado incial de sedimentos
!****************************************************************************************
SUBROUTINE escribesedantecval
USE modtet
IMPLICIT NONE
!Escribe el estado inicial de sedimentos
OPEN(34,file=TRIM(ADJUSTL(dirtra))//'sedantec_val.txt')
IF (lang.eq.1) THEN 
    WRITE(34,*)'* Valores elegidos como condiciones iniciales de sedimentos en toda la cuenca'
    WRITE(34,*)'* Material parental (arena, limo, arcilla)'
    WRITE(34,'(3(x,F11.5))')sedantec(1,1),sedantec(2,1),sedantec(3,1)
    WRITE(34,*)'* Sedimentos en suspensión (arena, limo, arcilla)'
    WRITE(34,'(3(x,F11.5))')sedantec(4,1),sedantec(5,1),sedantec(6,1)
    WRITE(34,*)'* Sedimentos depositados en ladera (arena, limo, arcilla)'
    WRITE(34,'(3(x,F11.5))')sedantec(7,1),sedantec(8,1),sedantec(9,1)
    WRITE(34,*)'* Sedimentos depositados en carcavas (arena, limo, arcilla)'
    WRITE(34,'(3(x,F11.5))')sedantec(13,1),sedantec(14,1),sedantec(15,1)
    WRITE(34,*)'* Sedimentos depositados en el cauce (arena, limo, arcilla)'
    WRITE(34,'(3(x,F11.5))')sedantec(10,1),sedantec(11,1),sedantec(12,1)
ELSEIF (lang.eq.2) THEN
        WRITE(34,*)'* Initial sediment contition for the whole cathcment'
    WRITE(34,*)'* Parent material (sand, silt, clay)'
    WRITE(34,'(3(x,F11.5))')sedantec(1,1),sedantec(2,1),sedantec(3,1)
    WRITE(34,*)'* Suspended sediment (sand, silt, clay)'
    WRITE(34,'(3(x,F11.5))')sedantec(4,1),sedantec(5,1),sedantec(6,1)
    WRITE(34,*)'* Hillslope sediment (sand, silt, clay)'
    WRITE(34,'(3(x,F11.5))')sedantec(7,1),sedantec(8,1),sedantec(9,1)
    WRITE(34,*)'* Gully deposits (sand, silt, clay)'
    WRITE(34,'(3(x,F11.5))')sedantec(13,1),sedantec(14,1),sedantec(15,1)
    WRITE(34,*)'* River channel deposits (sand, silt, clay)'
    WRITE(34,'(3(x,F11.5))')sedantec(10,1),sedantec(11,1),sedantec(12,1)
ENDIF
CLOSE(34)
END SUBROUTINE
!****************************************************************************************
!* Subrutina para leer el estado incial de sedimentos
!****************************************************************************************
SUBROUTINE leesedantecval
USE modtet
IMPLICIT NONE
OPEN(34,file=TRIM(ADJUSTL(dirtra))//'sedantec_val.txt')
READ(34,*)
READ(34,*)
READ(34,*)sedantec(1,1),sedantec(2,1),sedantec(3,1)
READ(34,*)
READ(34,*)sedantec(4,1),sedantec(5,1),sedantec(6,1)
READ(34,*)
READ(34,*)sedantec(7,1),sedantec(8,1),sedantec(9,1)
READ(34,*)
READ(34,*)sedantec(13,1),sedantec(14,1),sedantec(15,1)
READ(34,*)
READ(34,*)sedantec(10,1),sedantec(11,1),sedantec(12,1)
CLOSE(34)
END SUBROUTINE