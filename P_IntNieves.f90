! ************************************************************************
! * Subrutina que interpola los valores de la altura equivalente de agua
! * provenientes del fichero episodio (que incluya las letras C y T).
! * Lee información del fichero tipo ASC para interpolar en esas celdas
!*  Ultima actualización: Julio 2016
! ************************************************************************
program intnieves
!USE DFLIB
USE IFPORT
USE modtet
IMPLICIT NONE

LOGICAL(KIND=4)CHECKED
INTEGER mpola
CHARACTER tex*12
INTEGER, ALLOCATABLE::inie(:,:)
REAL,ALLOCATABLE::kn(:,:)
INTEGER nargu,ist
CHARACTER*30 arg
character copyFicheros*256
    
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
CALL lee_pargeo
IF (sale.eq.2) GOTO 95
artem=arch(3)

!Lee topologia, propiedades del suelo y tipologia
OPEN(14,file=artem,status='old',err=203)
READ (14,*,err=204)tit(1),cn,cs
READ (14,*,err=204)tit(2),ce,cw
READ (14,*,err=204)tit(3),mi
READ (14,*,err=204)tit(4),mj
READ (14,*,err=204)tit(5),ncol
READ (14,*,err=204)tit(6),nfil
READ (14,*,err=204)tit(7),ncel
dx=(ce-cw)/mi
dy=(cn-cs)/mj
ALLOCATE(cell(ncel))

sale=1


CALL lee_topol
IF (sale.eq.2) GOTO 95

!Lee ficheros del evento: conta numero di stazioni di neve e temperatura
OPEN(12,file=arch(5),status='old',err=206)
kniv=0
ktem=0
ios=0
DO WHILE (ios.ne.-1)
  aa='-*'
  READ (12,10,iostat=ios) aa
  SELECT CASE (aa)
    CASE ('H ')     !Altura equivalente de nieve (mm)
      kniv=kniv+1
    CASE ('T ')     !Temperatura del aire (nieve) (ºC)
      ktem=ktem+1
  END SELECT
ENDDO
REWIND(12)

!Control de existencia de datos de nieve
IF (ktem.eq.0) THEN
  mensaje=strings(907)
  errr=1
  CALL errores
  GOTO 94
ENDIF

jh=0
jt=0
ios=0
DO WHILE (ios.ne.-1)
  aa='-*'
  READ (12,10,iostat=ios) aa
  SELECT CASE (aa)
    CASE ('G ')
      BACKSPACE (12)
      READ (12,*,err=208) aa,nt,dtmin
      dt=dtmin/60.
      dts=dt*3600.0
      mpola=MAX(kniv,ktem)
      ALLOCATE(inie(ncel,kniv),disn(mpola)) 
      ALLOCATE(kn(ncel,kniv))
      ALLOCATE(temper(ktem),nieve(kniv))
    CASE ('T ')
      jt=jt+1
      BACKSPACE (12)
	  ALLOCATE(temper(jt).obs(nt))
	  READ (12,*,err=209) temper(jt).codigo,temper(jt).name,temper(jt).utmx,temper(jt).utmy,  &
	                      temper(jt).elev,orig,(temper(jt).obs(i),i=1,nt)
      temper(jt).fila=HFIX((temper(jt).utmx-cw)/dx)
	  temper(jt).columna=HFIX((cn-temper(jt).utmy)/dy)
    CASE ('H ')
      jh=jh+1
      BACKSPACE (12)
	  ALLOCATE(nieve(jh).obs(1)) !nieve observada en el primer paso de tiempo (la inicial)
      READ (12,*,err=215) nieve(jh).codigo,nieve(jh).name,nieve(jh).utmx,nieve(jh).utmy,  &
	                      nieve(jh).elev,orig,nieve(jh).obs(1)
      nieve(jh).fila=HFIX((nieve(jh).utmx-cw)/dx)
	  nieve(jh).columna=HFIX((cn-nieve(jh).utmy)/dy)
	  IF (nieve(jh).obs(1).lt.0.0) THEN
        mensaje='094 Valor negativo encontrado en '//TRIM(ADJUSTL(nieve(jh).name))
        errr=2
        CALL errores
	  	nieve(jh).obs(1)=0.0  !pone cero a los valores negativos iniciales de nieve
	  ENDIF 
  END SELECT
END DO
10 FORMAT(A2)
CLOSE(12)

!Coloca codigo=1 en celdas donde ASC es mayor que 1
ALLOCATE(mascreal(mi,mj))
IF (ktem.ne.0) THEN
  !Escribe al fichero tipo ASC los resultados de nieve
  artem = arch(10)
  CALL leeficASCI(mascreal)
  
  DO n=1,ncel 
    IF (mascreal(cell(n).fil,cell(n).col).GT.0) THEN
       cell(n).codnie=1
    ENDIF
  ENDDO
ENDIF

!Lee parametros de calibracion (factores correctores)
CALL lee_calib
IF (sale.eq.2) GOTO 94

WRITE(*,*)strings(810)

artem=arch(10)
CALL interfus

!Calcula la interpolación al inicio de la ejecución del modelo
mascreal=0.0
nnest=MIN(6,kniv)
DO n=1,ncel
  !Interpolación de la altura de nieve LINEAL - Si codigo(n)>=1
  IF (cell(n).codnie.ge.1.AND.kniv.ne.0) THEN
    disn=99999.9
    !Calcula factores de interpolación de nieve para nnest estaciones
    DO k=1,kniv
      disn(k)=(((cell(n).fil-nieve(k).fila)*(cell(n).fil-nieve(k).fila)+   &
	         (cell(n).col-nieve(k).columna)*(cell(n).col-nieve(k).columna))**0.5+0.001)  !Calcula la distancia de cada celda a la estación H de nieve
    ENDDO
    !Calcula posición de las nnest cercanos en los kniv disponibles
    DO k=1,nnest
      non=MINLOC(disn) !Determina la ubicación de los elementos. Determines the location of the element in the array with the minimum value
      disn(non(1))=99999.0
      inie(n,k)=non(1) !Este vector debe contener las estaciones de nieve ordenados por distancia desde el más cercano al más lejano
    ENDDO
    !Calcula coeficientes de interpolación de la nieve
    sinvdis=0.0
    DO k=1,nnest
      nb=inie(n,k) !nb es la posición de la estación de nieve en la matriz iniev
      disn(nb)=(((cell(n).fil-nieve(nb).fila)*(cell(n).fil-nieve(nb).fila)+  &
	          (cell(n).col-nieve(nb).columna)*(cell(n).col-nieve(nb).columna))**0.5+0.001)
      sinvdis=sinvdis+1.0/(disn(nb)*disn(nb)) !suma de la distancia inversa al cuadrado
    ENDDO
    
    sum=0.0
    DO k=1,nnest
      kn(n,k)=(1.0/(disn(inie(n,k))*disn(inie(n,k))))/sinvdis !Esto son los pesos de cada estación
	    sum=(nieve(inie(n,k)).obs(1)+max(0.0,bbeta*(cell(n).cota-nieve(inie(n,k)).elev)))*kn(n,k)+sum 
    ENDDO
	cell(n).h(0)=MAX(0.0,sum)
    mascreal(cell(n).fil,cell(n).col)=cell(n).h(0)
  ENDIF
ENDDO

!Finalizado el proceso de calibración reescribe ficheros
IF (ktem.ne.0) THEN
  !Escribe al fichero tipo ASC los resultados de nieve
  artem=arch(10)  
  CALL escribascint(INT(mascreal))

  !Escribe los parametros de calibracion (factores correctores)
  artem=arch(2)
  CALL escri_calib
ENDIF
j=1

GOTO 95

203 mensaje= strings(31)
errr=1
CALL errores
GOTO 94
204 mensaje= strings(32)
errr=1
CALL errores
GOTO 94
205 mensaje= strings(33)
errr=1
CALL errores
GOTO 94

206 mensaje=strings(51)
errr=1
CALL errores
GOTO 94
207 mensaje=strings(52)
errr=1
CALL errores
GOTO 94
208 mensaje=strings(501)
errr=1
CALL errores
GOTO 94
209 mensaje=strings(508)
errr=1
CALL errores
GOTO 94
210 mensaje= strings(509)
errr=1
CALL errores
GOTO 94
215 mensaje=strings(513)
errr=1
CALL errores
GOTO 94

211 mensaje=strings(21)
errr=1
CALL errores
GOTO 94
212 mensaje=strings(22)
errr=1
CALL errores
GOTO 94

213 mensaje=strings(22)
errr=1
CALL errores
GOTO 94
214 mensaje=strings(23)
errr=1
CALL errores
goto 94

94 WRITE(*,*)strings(800)
95 WRITE(*,*)strings(811)
CALL DATE_AND_TIME(dia,hora)
CALL write_date

CALL libera_mem
IF (ALLOCATED(kn)) DEALLOCATE(kn)
IF (ALLOCATED(inie)) DEALLOCATE(inie)

!res=DELFILESQQ(TRIM(ADJUSTL(dirtra))//TRIM(ADJUSTL('~errores.txt')))
!Subtituimos la rutina para no emplear librerias(Vicente.Nov-2019)
if(stma_op==0) then
    !copyFicheros = 'cp "' // TRIM(ADJUSTL(dirtra))//TRIM(ADJUSTL('~errores.txt')) // '" "' // TRIM(ADJUSTL(dirtra))//TRIM(ADJUSTL('errores.txt')) // '" /Y' !Linux
    copyFicheros = '[ -f ' // TRIM(ADJUSTL(dirtra))//TRIM(ADJUSTL('~errores.txt')) //' ] && cp ' // TRIM(ADJUSTL(dirtra))//TRIM(ADJUSTL('~errores.txt')) // ' ' // TRIM(ADJUSTL(dirtra))//TRIM(ADJUSTL('errores.txt')) !Linux
else
    copyFicheros = 'copy "' // TRIM(ADJUSTL(dirtra))//TRIM(ADJUSTL('~errores.txt')) // '" "' // TRIM(ADJUSTL(dirtra))//TRIM(ADJUSTL('errores.txt')) // '" /Y' !Windows
end if
res = SYSTEM (copyFicheros)
CALL deletefich(TRIM(ADJUSTL(dirtra))//TRIM(ADJUSTL('~errores.txt')))

!RETURN  !para ejecutable de interpolación de altura de nieve
END program 
