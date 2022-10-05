!*************************************************************************
!* Rutina que lee  y escribe el estado incial del submodelo de nitrógeno
!*  Ultima actualización: Julio 2016
!*************************************************************************
Program nantec
!USE DFLIB
USE IFPORT
USE modtet
IMPLICIT NONE

LOGICAL(KIND=4)CHECKED
CHARACTER tit2*11(7) 
INTEGER nargu,ist
Integer massimocodveg
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


!Lee ficheros de nitrógeno
CALL leearchnit(dirtra,archnit,sale)
    IF (sale.eq.2) GOTO 95
 
If (config(4)) then !Lee ficheros de sedimentos 
    CALL leearchsed(dirtra,archsed,sale)
    IF (sale.eq.2) GOTO 95
End if
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

!Lee humedad antecedente para poder transformar las concentraciones de entrada en kg/tanque
artem=arch(4)
CALL lee_human
IF (sale.eq.2) GOTO 94

!Lee los estados iniciales de sedimentos para poder transformar las concentraciones de entrada en kg/tanque en sedimentos en cauce
If (config(4)) then
    artem=archsed(7)!Contiene el estado inicial de sedimentos
    CALL lee_sedantec
    IF (sale.eq.2) GOTO 94
End if

!Calcula el estado inicial de nitrógeno
WRITE(*,*)strings(827)

!inicializa en cero
INQUIRE(file=TRIM(ADJUSTL(dirtra))//'nantec_val.txt', EXIST = existe)
IF (.NOT. existe) THEN
  nantecs=0.0
  nantecc=0.0
  nanteca=0.0
ELSE
  CALL leenantecval !Lee el estado inicial de nantec_val.txt
ENDIF

!En caso de cultivos activado (05/2017)
If (modulos2(4)) then
    !inicializa en cero
    INQUIRE(file=TRIM(ADJUSTL(dirtra))//'cultantec_val.txt', EXIST = existe)
    IF (.NOT. existe) THEN
      cultantecs=0.0
    ELSE
      CALL leecultantecval !Lee el estado inicial de cultantec_val.txt
      If (sale==2) then
          Go To 94
      End if
    ENDIF
End if

!Calcular ancho y área de celdas
dx=(ce-cw)/mi				!ancho en metros
dy=(cn-cs)/mj				!largo en metros
arcel=dx*dy					!area en m2
arcelkm=arcel/1000000.0		!area en km2	

DO n=1,ncel
ncp=cell(n).codpar
  DO i=2,3
	nw(i,ncp)=areaumbral(i,ncp)/arcelkm
  ENDDO
ENDDO

nw(2,ncp)=(areaumbral(2,ncp)/arcelkm)
nw(3,ncp)=(areaumbral(3,ncp)/arcelkm)


DO n=1,ncel
    cell(n).hn=0.0
    IF ((nw(2,ncp).gt.cell(n).acum)) THEN !Ladera
        If(modulos2(5)) then
            cell(n).hn(0)=cell(n).hn0ini*arcel/10000  !Pasamos de kg/ha a kg/celda
        Else
            cell(n).hn(0)=nantecs((cell(n).codveg),1)*arcel*cell(n).psuelo*r(1) !kg NO en suelo (nantec kgNO/m3suelo)
        End if
        cell(n).hn(1)=nantecs((cell(n).codveg),2)*arcel*cell(n).psuelo*r(1) !kg NH4 en suelo (nantec kgNH4/m3suelo)
        cell(n).hn(2)=nantecs((cell(n).codveg),3)*arcel*cell(n).psuelo*r(1) !kg NO3 en suelo (nantec kgNO3/m3suelo)
        cell(n).hn(3)=nantecc(1)/1000*cell(n).h(2)*arcel/1000 !kg NO en cauce (nantec mgNO/l). h5=0, así que sólo habrá en h2, si h2>0
        cell(n).hn(4)=nantecc(2)/1000*cell(n).h(2)*arcel/1000 !kg NH4 en cauce (nantec mgNH4/l)
        cell(n).hn(5)=nantecc(3)/1000*cell(n).h(2)*arcel/1000 !kg NO3 en cauce (nantec mgNO3/l)
        cell(n).hn(6)=nanteca(1)/1000*cell(n).h(4)*arcel/1000 !kg NH4 en acuífero (nantec mgNH4/l)
        cell(n).hn(7)=nanteca(2)/1000*cell(n).h(4)*arcel/1000 !kg NO3 en acuífero (nantec mgNO3/l)
        cell(n).hn(8)=0.0 !kg NH4 adsorbido en suelo (la distribución se hace antes de ejecutar tetis con el parámetro kd)
        If (config(4)) then
            cell(n).hn(9)=nantecc(4)*(cell(n).SusSedLAD(1)+cell(n).SusSedLAD(2)+cell(n).SusSedLAD(3)) !kg NO en sedimentos suspendidos en cauce (nantec kgNO/m3)
            cell(n).hn(10)=nantecc(5)*(cell(n).SusSedLAD(1)+cell(n).SusSedLAD(2)+cell(n).SusSedLAD(3)) !kg NH4 en sedimentos suspendidos en cauce (nantec kgNH4/m3)
            cell(n).hn(11)=0.0 !Siempre es 0 porque se considera que no hay sedimentos depositados en cauce, siempre es el suelo
            cell(n).hn(12)=0.0 !Siempre es 0 porque se considera que no hay sedimentos depositados en cauce, siempre es el suelo        
        else
            cell(n).hn(9)=0.0
            cell(n).hn(10)=0.0
            cell(n).hn(11)=0.0
            cell(n).hn(12)=0.0
        End if
    ELSE IF ((nw(2,ncp).le.cell(n).acum).and.(nw(3,ncp).gt.cell(n).acum)) THEN !Cárcava
        If(modulos2(5)) then
            cell(n).hn(0)=cell(n).hn0ini  !Lo copia del mapa, que directamente está en kg/celda
        Else
            cell(n).hn(0)=nantecs((cell(n).codveg),1)*arcel*cell(n).psuelo*r(1) !kg NO en suelo (nantec kgNO/m3suelo)
        End if
        cell(n).hn(1)=nantecs((cell(n).codveg),2)*arcel*cell(n).psuelo*r(1) !kg NH4 en suelo (nantec kgNH4/m3suelo)
        cell(n).hn(2)=nantecs((cell(n).codveg),3)*arcel*cell(n).psuelo*r(1) !kg NO3 en suelo (nantec kgNO3/m3suelo)
        cell(n).hn(3)=nantecc(1)/1000*cell(n).h(2)*arcel/1000+nantecc(1)/1000*cell(n).h(5) !kg NO en cauce (nantec mgNO/l) 
        cell(n).hn(4)=nantecc(2)/1000*cell(n).h(2)*arcel/1000+nantecc(2)/1000*cell(n).h(5) !kg NH4 en cauce (nantec mgNH4/l)
        cell(n).hn(5)=nantecc(3)/1000*cell(n).h(2)*arcel/1000+nantecc(3)/1000*cell(n).h(5) !kg NO3 en cauce (nantec mgNO3/l)
        cell(n).hn(6)=nanteca(1)/1000*cell(n).h(4)*arcel/1000 !kg NH4 en acuífero (nantec mgNH4/l)
        cell(n).hn(7)=nanteca(2)/1000*cell(n).h(4)*arcel/1000 !kg NO3 en acuífero (nantec mgNO3/l)
        cell(n).hn(8)=0.0 !No hay suelo
        If (config(4)) then
            cell(n).hn(9)=nantecc(4)*(cell(n).SusSedRED(1)+cell(n).SusSedRED(2)+cell(n).SusSedRED(3)) !kg NO en sedimentos suspendidos en cauce (nantec kgNO/m3)
            cell(n).hn(10)=nantecc(5)*(cell(n).SusSedRED(1)+cell(n).SusSedRED(2)+cell(n).SusSedRED(3)) !kg NH4 en sedimentos suspendidos en cauce (nantec kgNH4/m3)
            cell(n).hn(11)=nantecc(4)*(cell(n).DepSedRED(1)+cell(n).DepSedRED(2)+cell(n).DepSedRED(3)) !kg NO en sedimentos depositados en cauce (nantec kgNO/m3)
            cell(n).hn(12)=nantecc(5)*(cell(n).DepSedRED(1)+cell(n).DepSedRED(2)+cell(n).DepSedRED(3)) !kg NH4 en sedimentos depositados en cauce (nantec kgNH4/m3)
        else
            cell(n).hn(9)=0.0
            cell(n).hn(10)=0.0
            cell(n).hn(11)=0.0
            cell(n).hn(12)=0.0
        End if
    ELSE IF (nw(3,ncp).le.cell(n).acum) THEN !Cauce
       If(modulos2(5)) then
            cell(n).hn(0)=cell(n).hn0ini  !Lo copia del mapa, que directamente está en kg/celda
        Else
            cell(n).hn(0)=nantecs((cell(n).codveg),1)*arcel*cell(n).psuelo*r(1) !kg NO en suelo (nantec kgNO/m3suelo)
        End if
        cell(n).hn(1)=nantecs((cell(n).codveg),2)*arcel*cell(n).psuelo*r(1) !kg NH4 en suelo (nantec kgNH4/m3suelo)
        cell(n).hn(2)=nantecs((cell(n).codveg),3)*arcel*cell(n).psuelo*r(1) !kg NO3 en suelo (nantec kgNO3/m3suelo)
        cell(n).hn(3)=nantecc(1)/1000*cell(n).h(2)*arcel/1000+nantecc(1)/1000*cell(n).h(5) !kg NO en cauce (nantec mgNO/l) 
        cell(n).hn(4)=nantecc(2)/1000*cell(n).h(2)*arcel/1000+nantecc(2)/1000*cell(n).h(5) !kg NH4 en cauce (nantec mgNH4/l)
        cell(n).hn(5)=nantecc(3)/1000*cell(n).h(2)*arcel/1000+nantecc(3)/1000*cell(n).h(5) !kg NO3 en cauce (nantec mgNO3/l)
        cell(n).hn(6)=nanteca(1)/1000*cell(n).h(4)*arcel/1000 !kg NH4 en acuífero (nantec mgNH4/l)
        cell(n).hn(7)=nanteca(2)/1000*cell(n).h(4)*arcel/1000 !kg NO3 en acuífero (nantec mgNO3/l)
        cell(n).hn(8)=0.0 !No hay suelo
        If (config(4)) then
            cell(n).hn(9)=nantecc(4)*(cell(n).SusSedRED(1)+cell(n).SusSedRED(2)+cell(n).SusSedRED(3)) !kg NO en sedimentos suspendidos en cauce (nantec kgNO/m3)
            cell(n).hn(10)=nantecc(5)*(cell(n).SusSedRED(1)+cell(n).SusSedRED(2)+cell(n).SusSedRED(3)) !kg NH4 en sedimentos suspendidos en cauce (nantec kgNH4/m3)
            cell(n).hn(11)=nantecc(4)*(cell(n).DepSedRED(1)+cell(n).DepSedRED(2)+cell(n).DepSedRED(3)) !kg NO en sedimentos depositados en cauce (nantec kgNO/m3)
            cell(n).hn(12)=nantecc(5)*(cell(n).DepSedRED(1)+cell(n).DepSedRED(2)+cell(n).DepSedRED(3)) !kg NH4 en sedimentos depositados en cauce (nantec kgNH4/m3)
        else
            cell(n).hn(9)=0.0
            cell(n).hn(10)=0.0
            cell(n).hn(11)=0.0
            cell(n).hn(12)=0.0
        End if
    ENDIF
ENDDO

If (modulos2(4)) then !Cultivos Cris(05/2017)
    massimocodveg=maxval(cell(:).codveg)
    Do n=1,ncel
        Do i=1,massimocodveg
            IF ((nw(2,ncp).gt.cell(n).acum)) THEN !Ladera
                IF (i==cell(n).codveg) then
                    cell(n).w=cultantecs(cell(n).codveg,1)*arcel/10000 !Transformado a kg/celda
                    cell(n).fcncult=cultantecs(cell(n).codveg,2)
                End if
            Else !Si no es celda de ladera, es como si no hubiera suelo y no se simula crecimiento de nada
                If (i==cell(n).codveg) then
                    cell(n).w=0.0
                    cell(n).fcncult=0.0
                End if
            End if
        End do
    End do
Else
    cell(n).w=0.0
    cell(n).fcncult=0.0
End if

artem=archnit(13)	!Contiene el estado inicial de nitrógeno
CALL escribe_nantec

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
   
WRITE(*,*)strings(828)
CALL DATE_AND_TIME(dia,hora)
CALL write_date

END Program


!****************************************************************************************
!* Subrutina para leer el estado incial de nitrógeno
!****************************************************************************************
!Lee el fichero nantec_val.txt que tiene tantas filas como usos del suelo +1 (cauce) y 3 columnas NO, NH4, NO3
!La concentración de cauce se aplica también al tanque de h2 en aquellas celdas en las que h2>0
!No se hace comprobación de que esté bien porque este fichero lo creará y lo eliminará la interfaz (siempre estará bien).
SUBROUTINE leenantecval
USE modtet
IMPLICIT NONE

k=0
OPEN(34,file=TRIM(ADJUSTL(dirtra))//'nantec_val.txt')
    ios=0
    DO WHILE (ios.ne.-1)
      READ(34,*,iostat=ios) j
      IF (ios.ne.-1) k=k+1
    ENDDO
    REWIND(34)
    Read(34,*) !Encabezado
    DO i=1,k-5 !Lee los de usos del suelo
        READ(34,*)(nantecs(i,j),j=1,3)
    ENDDO
    Read(34,*) !Encabezado
    Read(34,*) nantecc(1),nantecc(2),nantecc(3),nantecc(4),nantecc(5)
    Read(34,*) !Encabezado
    Read(34,*) nanteca(1),nanteca(2)
    kcau=k-1
    kacu=k
    
CLOSE(34)
    END SUBROUTINE
    
!****************************************************************************************
!* Subrutina para leer el estado incial de los cultivos (peso seco en t/ha)
!****************************************************************************************
!Tendrá que tener tantas filas como usos del suelo
SUBROUTINE leecultantecval
USE modtet
IMPLICIT NONE

Integer massimocodveg

sale=0
k=0
massimocodveg=maxval(cell(:).codveg)
OPEN(34,file=TRIM(ADJUSTL(dirtra))//'cultantec_val.txt')
    ios=0
    DO WHILE (ios.ne.-1)
      READ(34,*,iostat=ios) j
      IF (ios.ne.-1) k=k+1
    ENDDO
    REWIND(34)
    Read(34,*) !Encabezado
    DO i=2,k 
        READ(34,*)cultantecs(i-1,1),cultantecs(i-1,2)
    ENDDO
CLOSE(34)
IF(k.lt.massimocodveg)then
    mensaje=strings(417)
    errr=1
    CALL errores
    sale=2
ENDIF
END SUBROUTINE