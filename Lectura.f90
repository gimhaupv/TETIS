! *****************************************************************
! * Subrutinas de lectura de ficheros empleados en TETIS
! * Lectura de filessp.txt
! *****************************************************************
SUBROUTINE lecfiles(dirtra_,arch_,sale_)
USE modtet
IMPLICIT NONE

!INTEGER sale,i,ldirtra,errr,lmensaje
INTEGER sale_,ldirtra_,STATUS
!CHARACTER arch*128(37),artem*128,archin*24,mensaje*200,dirtra*128,hora*10,dia*8
CHARACTER arch_*128(42),dirtra_*128,genericpath*512
!LOGICAL EXISTE
real GETCWD

sale_=0
!Lee nombres de los ficheros a utilizar

!Se determina el separador de los Paths según el Sistema Operativo en el que se ejecute Tetis
CALL GET_ENVIRONMENT_VARIABLE('PATH',genericpath)
path_separator=genericpath(1:1)!Se extrae el primer caracter del path del entorno.
IF(path_separator == '/') THEN
    stma_op = 0
ELSE !Si no es '/' (corresp. a Linux) se establece el correspondiente a Windows ('\')
    stma_op = 1
    path_separator = '\'
END IF
 

INQUIRE (FILE='filessp.txt',EXIST=existe) 
IF (.NOT.existe) goto 200

OPEN(9,file='filessp.txt',status='old',err=200)
READ(9,'(a128)') dirtra_
ldirtra_=len_trim(dirtra_)
!if(dirtra_(ldirtra_:ldirtra_).ne.'\')dirtra_(ldirtra_+1:ldirtra_+1)='\'
if(dirtra_(ldirtra_:ldirtra_).ne.path_separator) dirtra_(ldirtra_+1:ldirtra_+1)=path_separator
DO i=1,42
    READ(9,'(a128)',err=202,end=202)arch_(i)
ENDDO
CLOSE(9)

!artem=TRIM(ADJUSTL(dirtra))//TRIM(ADJUSTL('~errores.txt'))  ch 16/01/2012
!OPEN(22,file=artem)                                        ch 16/01/2012
!WRITE(22,'(A54)') ' Descripcion de errores detectados en el modelo TETIS ' ch 16/01/2012
!CLOSE(22) ch 16/01/2012
!archin=TRIM(ADJUSTL(arch(5)))!este es el nombre del fichero inicial que tiene que ver con las fechas

arch_(1)=TRIM(ADJUSTL(dirtra_))//TRIM(ADJUSTL(arch_(1)))
arch_(2)=TRIM(ADJUSTL(dirtra_))//TRIM(ADJUSTL(arch_(2)))
arch_(3)=TRIM(ADJUSTL(dirtra_))//TRIM(ADJUSTL(arch_(3)))
arch_(4)=TRIM(ADJUSTL(dirtra_))//TRIM(ADJUSTL(arch_(4)))
arch_(5)=TRIM(ADJUSTL(dirtra_))//TRIM(ADJUSTL(arch_(5)))
arch_(6)=TRIM(ADJUSTL(dirtra_))//TRIM(ADJUSTL(arch_(6)))
arch_(7)=TRIM(ADJUSTL(dirtra_))//TRIM(ADJUSTL(arch_(7)))
arch_(8)=TRIM(ADJUSTL(dirtra_))//TRIM(ADJUSTL(arch_(8)))
arch_(9)=TRIM(ADJUSTL(dirtra_))//TRIM(ADJUSTL(arch_(9)))
arch_(10)=TRIM(ADJUSTL(dirtra_))//TRIM(ADJUSTL(arch_(10)))
arch_(11)=TRIM(ADJUSTL(dirtra_))//TRIM(ADJUSTL(arch_(11)))
arch_(12)=TRIM(ADJUSTL(dirtra_))//TRIM(ADJUSTL(arch_(12)))
arch_(13)=TRIM(ADJUSTL(dirtra_))//TRIM(ADJUSTL(arch_(13)))
arch_(14)=TRIM(ADJUSTL(dirtra_))//TRIM(ADJUSTL(arch_(14)))
arch_(15)=TRIM(ADJUSTL(dirtra_))//TRIM(ADJUSTL(arch_(15)))
arch_(16)=TRIM(ADJUSTL(dirtra_))//TRIM(ADJUSTL(arch_(16)))
arch_(17)=TRIM(ADJUSTL(dirtra_))//TRIM(ADJUSTL(arch_(17)))
arch_(18)=TRIM(ADJUSTL(dirtra_))//TRIM(ADJUSTL(arch_(18)))
arch_(19)=TRIM(ADJUSTL(dirtra_))//TRIM(ADJUSTL(arch_(19)))
arch_(20)=TRIM(ADJUSTL(dirtra_))//TRIM(ADJUSTL(arch_(20)))
arch_(21)=TRIM(ADJUSTL(dirtra_))//TRIM(ADJUSTL(arch_(21)))
arch_(22)=TRIM(ADJUSTL(dirtra_))//TRIM(ADJUSTL(arch_(22)))
arch_(23)=TRIM(ADJUSTL(dirtra_))//TRIM(ADJUSTL(arch_(23)))
arch_(24)=TRIM(ADJUSTL(dirtra_))//TRIM(ADJUSTL(arch_(24)))
arch_(25)=TRIM(ADJUSTL(dirtra_))//TRIM(ADJUSTL(arch_(25)))
arch_(26)=TRIM(ADJUSTL(dirtra_))//TRIM(ADJUSTL(arch_(26)))
arch_(27)=TRIM(ADJUSTL(dirtra_))//TRIM(ADJUSTL(arch_(27)))
arch_(28)=TRIM(ADJUSTL(dirtra_))//TRIM(ADJUSTL(arch_(28)))
arch_(29)=TRIM(ADJUSTL(dirtra_))//TRIM(ADJUSTL(arch_(29)))
arch_(30)=TRIM(ADJUSTL(dirtra_))//TRIM(ADJUSTL(arch_(30)))
arch_(31)=TRIM(ADJUSTL(dirtra_))//TRIM(ADJUSTL(arch_(31)))
arch_(32)=TRIM(ADJUSTL(dirtra_))//TRIM(ADJUSTL(arch_(32)))
arch_(33)=TRIM(ADJUSTL(dirtra_))//TRIM(ADJUSTL(arch_(33)))
arch_(34)=TRIM(ADJUSTL(dirtra_))//TRIM(ADJUSTL(arch_(34)))
arch_(35)=TRIM(ADJUSTL(dirtra_))//TRIM(ADJUSTL(arch_(35)))
arch_(36)=TRIM(ADJUSTL(dirtra_))//TRIM(ADJUSTL(arch_(36)))
arch_(37)=TRIM(ADJUSTL(dirtra_))//TRIM(ADJUSTL(arch_(37)))
arch_(38)=TRIM(ADJUSTL(dirtra_))//TRIM(ADJUSTL(arch_(38)))
arch_(39)=TRIM(ADJUSTL(dirtra_))//TRIM(ADJUSTL(arch_(39)))
arch_(40)=TRIM(ADJUSTL(dirtra_))//TRIM(ADJUSTL(arch_(40)))
arch_(41)=TRIM(ADJUSTL(dirtra_))//TRIM(ADJUSTL(arch_(41)))
arch_(42)=TRIM(ADJUSTL(dirtra_))//TRIM(ADJUSTL(arch_(42)))
GOTO 95

200 mensaje=strings(1)
errr=1 
STATUS=GETCWD(dirtra_)
dirtra_=TRIM(ADJUSTL(dirtra_))//path_separator !Añadimos el separador al final de la direccion leida (sin el separador)
CALL errores
artem=TRIM(ADJUSTL(dirtra_))//'~errores.txt'
lmensaje=len_trim(mensaje)
OPEN(8,FILE=artem)
WRITE(8,'(A54)') strings(802) !ch 16/01/2012
CALL DATE_AND_TIME(dia,hora)
WRITE(8,*)'* Fecha:'//dia(7:8)//'/'//dia(5:6)//'/'//dia(1:4)//' Hora:'//hora(1:2)//':'//hora(3:4)//':'//hora(5:10)
WRITE(8,18)errr, mensaje
CLOSE(8)
WRITE(*,*)mensaje !Incluimos la escritura por pantalla de los errores

GOTO 94

201 mensaje=strings(2)
errr=1
artem=TRIM(ADJUSTL(dirtra_))//'~errores.txt'
lmensaje=len_trim(mensaje)
OPEN(8,FILE=artem)
WRITE(8,'(A54)') strings(802)
CALL DATE_AND_TIME(dia,hora)
WRITE(8,*)'* Fecha:'//dia(7:8)//'/'//dia(5:6)//'/'//dia(1:4)//' Hora:'//hora(1:2)//':'//hora(3:4)//':'//hora(5:10)
WRITE(8,18)errr, mensaje
CLOSE(8)
WRITE(*,*)mensaje !Incluimos la escritura por pantalla de los errores

GOTO 94

202 mensaje=strings(3)
errr=1
artem=TRIM(ADJUSTL(dirtra_))//'~errores.txt'
lmensaje=len_trim(mensaje)
OPEN(8,FILE=artem)
WRITE(8,'(A54)') strings(802)
CALL DATE_AND_TIME(dia,hora)
WRITE(8,*)'* Fecha:'//dia(7:8)//'/'//dia(5:6)//'/'//dia(1:4)//' Hora:'//hora(1:2)//':'//hora(3:4)//':'//hora(5:10)
WRITE(8,18)errr, mensaje
CLOSE(8)
WRITE(*,*)mensaje !Incluimos la escritura por pantalla de los errores
18 FORMAT(I2,3x,a<lmensaje>)

!CALL errores(errr,mensaje,lang)

94 WRITE(*,*)strings(800)
sale_=2

95 END SUBROUTINE

!**********************************
!* Lectura de PARAMGEO.TXT
!**********************************
SUBROUTINE lee_pargeo
USE modtet
IMPLICIT NONE

sale=0
!Lee parametros geomorfologicos
OPEN(10,file=arch(1),status='old',err=203)
READ(10,*,err=204,end=204) npar
!Cris(17/10/2015) Regiones homogéneas allocatable, ahora fijo a 50.
!IF (ALLOCATED(d)) DEALLOCATE(d)
!IF (ALLOCATED(e)) DEALLOCATE(e)
!IF (ALLOCATED(dc)) DEALLOCATE(dc)
!IF (ALLOCATED(ec)) DEALLOCATE(ec)
!IF (ALLOCATED(wdad)) DEALLOCATE(wdad)
!IF (ALLOCATED(areaumbral)) DEALLOCATE(areaumbral)
!Allocate (d(6,npar),e(6,npar),dc(6,npar),ec(6,npar),wdad(8,npar),areaumbral(8,npar))
DO i=1,6
  IF (i.ne.2) THEN
    IF (i.ne.4)THEN
      READ(10,*,err=205,end=205)(d(i,j),j=1,npar)
    ENDIF
    READ(10,*,err=205,end=205)(e(i,j),j=1,npar)
  ENDIF
ENDDO
DO i=1,6
  IF (i.ne.2) THEN
    IF (i.ne.4)THEN
      READ(10,*,err=205,end=205)(dc(i,j),j=1,npar)
    ENDIF
    READ(10,*,err=205,end=205)(ec(i,j),j=1,npar)
  ENDIF
ENDDO
READ(10,*,err=205,end=205) nest,bint,betalin
DO i=1,6  !Lee humedades antecedentes para eco datos
  READ(10,*,err=205,end=205)(wdad(i,j),j=1,npar)
ENDDO
READ(10,*,err=205,end=205)hped
DO i=1,3  !Lee areas umbrales para eco datos
      READ(10,*,err=205,end=205)(areaumbral(i,j),j=1,npar)
ENDDO
!READ(10,*)(config(i),i=1,5),trapeff
!Guiomar (29/01/2014): En el PARAMGEO si está activada la vegetación dinámica habrá al final del archivo el estado inicial de los dos tanques estáticos y el de vegetación
IF (config(5)) THEN
    READ(10,*,err=205,end=205)(wdad(7,j),j=1,npar)
    READ(10,*,err=205,end=205)(wdad(8,j),j=1,npar)
!    IF (ALLOCATED(laiini)) DEALLOCATE(laiini)
!    Allocate (laiini(npar))  Cris(17/10/2016) Regiones homogéneas allocatable, ahora fijo a 50.
    READ(10,*,err=205,end=205)(laiini(j),j=1,npar)
ENDIF
CLOSE(10)

GOTO 95

203 mensaje=strings(11)
errr=1
CALL errores
GOTO 94
204 mensaje=strings(12)
errr=1
CALL errores
GOTO 94
205 mensaje=strings(13)
errr=1
CALL errores
GOTO 94


94 WRITE(*,*)strings(800)
sale=2

95 END SUBROUTINE


!**********************************
!* Lectura de PUNTOS DE CONTROL
!**********************************
SUBROUTINE lee_pcon
USE modtet
IMPLICIT NONE

sale=0
!Lee puntos de control
DO i=1,ncon
  READ(9,101,err=207) control(i).fila,control(i).columna,control(i).name
ENDDO
CLOSE(9)

101 FORMAT(I5,2X,I5,8X,A25)

GOTO 95

207 mensaje=strings(102)
errr=1
CALL errores
GOTO 94

94 WRITE(*,*)strings(800)
sale=2

95 END SUBROUTINE


!********************************************
!* Lectura de fichero de Humedad antecedente
!********************************************
SUBROUTINE lee_human
USE modtet
IMPLICIT NONE

sale=0
!Lee humedad antecedente
OPEN(15,file=artem,status='old',err=227)
READ (15,*,err=228)c8,cn,cs,c8,ce,cw,c8,mi,c8,mj,c8,i,c8,j,c8,ncel
READ (15,*,err=228)c8,fecin2,horin2
!Guiomar (03/12/2014): cambios para tener en cuenta que si se activa el módulo de vegetación el hantec tiene columnas adicionales a leer
IF(config(5)) THEN
    DO n=1,ncel
    cell(n).h=0.0
        READ (15,*,err=229,end=229)cell(n).h(1),cell(n).h(2),cell(n).h(3),cell(n).h(4),cell(n).h(5),cell(n).h(0),cell(n).h(6),cell(n).h(8),cell(n).h(1),cell(n).lai
        cell(n).h(5)=cell(n).h(5)*arcel/1000.0  !Pasa de mm a m³
        !chiara: para evitar picos iniciales ficticios cuando se cambia FC1 y no se vuelve a calcular el hantec
        if(cell(n).h(8).gt.cell(n).hu1*r(1)) then 
            cell(n).h(8)=cell(n).hu1*r(1)
        endif
        IF(cell(n).h(1).gt.cell(n).hu2*r(1)) THEN
            cell(n).h(1)=cell(n).hu2*r(1)
        ENDIF
        laimax=lamb(cell(n).codveg,11)
        IF(cell(n).lai.gt.laimax) THEN
            cell(n).lai=laimax
        ENDIF
    ENDDO
ELSE
  DO n=1,ncel
      cell(n).h=0.0
      READ (15,*,err=229,end=229)cell(n).h(1),cell(n).h(2),cell(n).h(3),cell(n).h(4),  &
                         cell(n).h(5),cell(n).h(0),cell(n).h(6)
      cell(n).h(5)=cell(n).h(5)*arcel/1000.0  !Pasa de mm a m³
      !chiara: para evitar picos iniciales ficticios cuando se cambia FC1 y no se vuelve a calcular el hantec
      if(cell(n).h(1).gt.cell(n).hu*r(1)) then 
         cell(n).h(1)=cell(n).hu*r(1)
      endif
  ENDDO
ENDIF
CLOSE(15)

GOTO 95

227 mensaje=strings(41)
errr=1
CALL errores
GOTO 94
228 mensaje=strings(42)
errr=1
CALL errores
GOTO 94
229 mensaje=strings(43)
errr=1
CALL errores
GOTO 94  !AÑADIDO CHIARA

94 WRITE(*,*)strings(800)
sale=2

95 END SUBROUTINE

!***********************************************************
!* Lectura de TOPOLCO.SDS (2a parte)
!***********************************************************
SUBROUTINE lee_topol
USE modtet
IMPLICIT NONE

sale=0
!Lee topologia, propiedades del suelo y tipologia
DO n=1,ncel
  READ (14,*,err=211) cell(n).fil,cell(n).col,cell(n).dest,cell(n).acum,cell(n).pend,  &
                 cell(n).cota,cell(n).codnie,cell(n).hu,cell(n).ks,cell(n).kp,  &
                 cell(n).kss,cell(n).ksa,cell(n).kps,cell(n).veloc,  &
                 cell(n).codpar,cell(n).codveg,cell(n).codrie,cell(n).codcal, &
				 cell(n).ordrie,cell(n).hstar,cell(n).dc,cell(n).rs, &
				 cell(n).rad(1),cell(n).rad(2),cell(n).rad(3),cell(n).rad(4),cell(n).rad(5),cell(n).rad(6),  &
				 cell(n).porcentaje(1),cell(n).porcentaje(2),cell(n).porcentaje(3), &
				 cell(n).Cusle,cell(n).Kusle,cell(n).Pusle,cell(n).codkarst, cell(n).hlim1,cell(n).hstar1,cell(n).hlim2,cell(n).hstar2,cell(n).fc,cell(n).hu1,cell(n).hu2, &
                 cell(n).hlim,cell(n).psuelo,cell(n).daparente,cell(n).kd,cell(n).hn0ini,cell(n).depamonio,cell(n).depnitrato       
                 !cell(n).Acuif,cell(n).hlim,cell(n).psuelo,cell(n).daparente,cell(n).kd,cell(n).hn0ini
  
  IF (cell(n).veloc.eq.0) THEN 
    cell(n).veloc=0.01
  ENDIF 
  IF (cell(n).codveg.eq.0) THEN
    cell(n).codveg=1
	mensaje=strings(916)
    errr=2
    CALL errores
  ENDIF
  IF (cell(n).pend.eq.0) THEN 
    cell(n).pend=1
    mensaje=strings(903)
    errr=2
    CALL errores
  ENDIF
ENDDO
READ(14,*)
READ(14,*)
READ(14,*)
READ(14,*)
READ(14,'(8F12.4)',err=2111)(averagepar(j), j=1,7),Imaxmedio
CLOSE(14)

GOTO 95

211 mensaje=strings(33)
!2111 mensaje=strings(34)
errr=1
CALL errores
GOTO 94 !AÑADIDO CHIARA

2111 mensaje=strings(34)
errr=1
CALL errores
GOTO 94 !AÑADIDO CHIARA

94 WRITE(*,*)strings(800)
sale=2

95 END SUBROUTINE


!************************************
!* Lectura de CALIB.TXT
!************************************
SUBROUTINE lee_calib
USE modtet
IMPLICIT NONE

sale=0

!Lee parametros de calibracion (factores correctores)
OPEN (11,file=arch(2),status='old',err=223)
DO i=1,10
  READ(11,*,err=224,end=224)r(i)
ENDDO
READ(11,*,err=226,end=226)bbeta
READ(11,*,err=226,end=226)ro1     !sin lluvia
READ(11,*,err=226,end=226)ro2     !con lluvia
READ(11,*,err=226,end=226)tbase
READ(11,*,err=224,end=224)betappt
READ(11,*,err=227,end=227)rsed(1)
READ(11,*,err=227,end=227)rsed(2)
READ(11,*,err=227,end=227)rsed(3)
READ(11,*,err=224,end=224)expinf
READ(11,*,err=224,end=224)alpha
READ(11,*,err=226,end=226)betatemp  !0.0065 por defecto
CLOSE(11)

GOTO 95

223 mensaje=strings(21)
errr=1
CALL errores
GOTO 94
224 mensaje=strings(22)
errr=1
CALL errores
GOTO 94
226 mensaje=strings(23)
errr=1
CALL errores
GOTO 94 !AÑADIDO CHIARA
227 mensaje=strings(24)
errr=1
CALL errores
GOTO 94 !AÑADIDO CHIARA

94 WRITE(*,*)strings(800)
sale=2

95 END SUBROUTINE

!********************************************
!* Lectura del episodio de lluvia (1ª parte)
!********************************************
SUBROUTINE lee_evto1
USE modtet
IMPLICIT NONE

sale=0

kppt=0
naf=0
nemb=0
knemb=0
knaf=0
kniv=0
ktem=0
kevp=0
kadi=0
vnemb=0
ios=0
ksedq=0
nradiacion=0
!Guiomar (28/01/2014): voy a introducir las estaciones en las que tenemos datos relativos a la vegetación
nveg=0
!Q adicional sedimentos (Cris 11/2015)
kadised1=0
kadised2=0
kadised3=0
!Cris (03/2017): estaciones relativas al submodelo de nitrógeno
kno=0
kam=0
kni=0
!Lee ficheros del evento
OPEN(12,file=arch(5),status='old',err=212)
DO WHILE (ios.ne.-1)
  aa='-*'
  READ (12,10,iostat=ios) aa
  SELECT CASE (aa)
    CASE ('G ')
      BACKSPACE (12)
      READ (12,*,err=214) aa,nt,dtmin
    CASE ('P ')     !Estacion de lluvia (mm)
      kppt=kppt+1
    CASE ('N ')     !Nivel del embalse (m)
      !IF(.NOT. modulos(2)) GO TO 215
      nemb=nemb+1
    CASE ('V ')     !Volumen en el embalse (Hm3) 
      !IF(.NOT. modulos(2)) GO TO 215
      vnemb=vnemb+1
    CASE ('S ')     !Caudal de salida del embalse (m3/s) 
      !IF(.NOT. modulos(2)) GO TO 215
      knemb=knemb+1
    CASE ('Q ')     !Estacion de aforo (m3/s)
      naf=naf+1    
    CASE ('X ')     !Estacion de aforo de sedimentos (m3/s)
      IF(.NOT. config(4)) GO TO 216
      ksedq=ksedq+1
    CASE ('B ')     !Estacion sin registro histórico (m3/s)
      knaf=knaf+1
    CASE ('H ')     !Altura de agua equivalente de nieve (mm)
     IF(.NOT. modulos(1)) GO TO 217
     kniv=kniv+1
    CASE ('T ')     !Temperatura del aire (nieve) (ºC)
      IF(.NOT. modulos(1)) THEN
          IF(.NOT. config(5)) THEN
             IF(.NOT. modulos2(3)) THEN 
                 GO TO 220
             END IF
          END IF
      END IF
      ktem=ktem+1
    CASE ('E ')     !Evapotranspiración (mm/dia)
      kevp=kevp+1
    CASE ('D ')     !Caudal a adicionar o descontar en un punto (m3/s)
      kadi=kadi+1
    CASE ('W ')     !Guiomar (28/01/2014):Vegetación
      IF(.NOT. config(5)) GO TO 218
      nveg=nveg+1
    CASE ('R ') !Guiomar (17/02/2014): radiación solar
      IF(.NOT. config(5)) GO TO 218
      nradiacion=nradiacion+1
    CASE ('DA ')   !Caudal sólido de arena a adicionar o descontar en un punto (m3/s) (Cris 11/2015)
      IF(.NOT. config(4)) GO TO 216
      kadised1=kadised1+1
    CASE ('DL ')   !Caudal sólido de limo a adicionar o descontar en un punto (m3/s) (Cris 11/2015)
      IF(.NOT. config(4)) GO TO 216
      kadised2=kadised2+1
    CASE ('DC ')   !Caudal sólido de arcilla a adicionar o descontar en un punto (m3/s) (Cris 11/2015)
      IF(.NOT. config(4)) GO TO 216
      kadised3=kadised3+1   
    CASE ('NO')   !Concentración de nitrógeno orgánico en cauce (mg/l) (Cris 03/2017)
      IF(.NOT. modulos2(3)) GO TO 219
      kno=kno+1 
    CASE ('AM')   !Concentración de amonio en cauce (mg/l) (Cris 03/2017)
      IF(.NOT. modulos2(3)) GO TO 219
      kam=kam+1 
    CASE ('NI')   !Concentración de nitrato en cauce (mg/l) (Cris 03/2017)
      IF(.NOT. modulos2(3)) GO TO 219
      kni=kni+1 
  END SELECT
ENDDO
nafn = kno+kni+kam !Ene2020-Vicente. Obtenemos numero de estaciones de aforo de nitrogeno totales
REWIND(12)
!Guiomar (28/01/2014): En las siguientes líneas se comprueba que hayan al menos una estación de altura de nieve y una estación de temperatura para que se pueda simular la fusión de nieve. 
!Lo único que he incluido es que este control sólo lo haga en el caso de tener activada la fusión de nieve (modulos(1))
IF (modulos(1))THEN
    IF (kniv.ne.0.AND.ktem.eq.0) THEN
      mensaje=strings(907)
      errr=1
      CALL errores
      GOTO 94
    ENDIF
    IF (ktem.ne.0.AND.kniv.eq.0) THEN
      mensaje=strings(908)
      errr=2
      CALL errores
    ENDIF
ENDIF
!Guiomar (28/01/2014): Voy a incluir un control parecido al de nieve pero para la vegetación. Si no hay temperaturas no se podrá simular la vegetación y si no hay estaciones de vegetación no se podrá evaluar el crecimiento de la vegetación
!Guiomar (17/02/2014): si no hay datos de radiación no se podrá simular la vegetación
IF (config(5))THEN
    IF (ktem.eq.0) THEN
        mensaje=strings(307)
        errr=1
        CALL errores
        GOTO 94
    ENDIF
    IF (nradiacion.eq.0) THEN
        mensaje=strings(309)
        errr=1
        CALL errores
        GOTO 94
    ENDIF
    IF (nveg.eq.0) THEN
        mensaje=strings(308)
        errr=2
        CALL errores
    ENDIF
ENDIF
!Cris (03/2016): Lo mismo para nitrógeno (Sólo es necesaria la serie de temperatura, el resto son sólo observados)
IF (modulos2(3))THEN
    IF (ktem.eq.0) THEN
        mensaje=strings(406)
        errr=1
        CALL errores
        GOTO 94
    ENDIF
ENDIF
10 FORMAT(A2)

ktotal=kppt+nemb+vnemb+knemb+naf+knaf+kniv+ktem+kevp+kadi+ksedq+kadised1+kadised2+kadised3+nveg+nradiacion+kno+kam+kni

GOTO 95

212 mensaje=strings(51)
errr=1
CALL errores
GOTO 94
213 mensaje=strings(52)
errr=1
CALL errores
GOTO 94
214 mensaje=strings(501)
errr=1
CALL errores
goto 94
215 mensaje=strings(923)
errr=1
CALL errores
GOTO 94    
216 mensaje=strings(214)
errr=1
CALL errores
GOTO 94
217 mensaje=strings(922)
errr=1
CALL errores
GOTO 94
218 mensaje=strings(314)
errr=1
CALL errores
GOTO 94
219 mensaje=strings(441)
errr=1
CALL errores
GOTO 94
220 mensaje=strings(924)
errr=1
CALL errores
GOTO 94

94 WRITE(*,*)strings(800)
sale=2

95 END SUBROUTINE
!*************************************************************
!* Lectura del episodio de lluvia (2ª parte) y formato CEDEX
!*************************************************************
SUBROUTINE lee_evto2
USE modtet
IMPLICIT NONE

sale=0
!Lee ficheros del evento
OPEN(12,file=arch(5),status='old',err=212)
jp=0
jq=0
jn=0
jh=0
je=0
jt=0
ja=0
ios=0
ji=0
jv=nemb
!js=0
js=nemb+vnemb
!Guiomar (29/01/2014): Añado lo correspondiente a las estaciones de vegetación
jw=0
!Guiomar (17/02/2014): Añado lo correspondiente a las estaciones con datos de radiación
jr=0
jsedq=0 !No estaba iniciada (Cris 11/2015)
jsedv=0 !No estaba iniciada (Cris 11/2015)
jsedadi1=0 !Cris (11/2015)
jsedadi2=0 !Cris (11/2015)
jsedadi3=0 !Cris (11/2015)
!Cris (03/2017): Añado lo relativo a nitrógeno
jno=0
jam=0
jni=0

DO WHILE (ios.ne.-1)
  aa='-*'
  READ (12,10,iostat=ios) aa
  SELECT CASE (aa)
    CASE ('P ')
      jp=jp+1
      BACKSPACE (12)
      ALLOCATE(pluvio(jp).obs(0:nt))
      pluvio(jp).obs=0.0
	  READ (12,*,err=215) pluvio(jp).codigo,pluvio(jp).name,pluvio(jp).utmx,  &
	                      pluvio(jp).utmy,pluvio(jp).elev,orig,(pluvio(jp).obs(i),i=1,nt)
	  IF (.NOT.bint) THEN  !Control de valores negativos y -1
	  	ncero=0
	    DO i=1,nt
	      IF (pluvio(jp).obs(i).lt.0.0) THEN
	        ncero=ncero+1
		    pluvio(jp).obs(i)=0.0
	   	  ENDIF
	    END DO
	  ENDIF
	  IF (ncero.gt.0) THEN 
                IF (lang.eq.1) THEN
                    mensaje='Valores negativos encontrados en '//TRIM(ADJUSTL(pluvio(jp).name))
                ELSEIF (lang.eq.2) THEN
                    mensaje='Negative values found in '//TRIM(ADJUSTL(pluvio(jp).name))
                ENDIF
	        		    errr=2
            CALL errores
      ENDIF
      !Guiomar (14/04/2014): Cambio para resolver error del punto de desagüe
      !pluvio(jp).fila=IFIX((pluvio(jp).utmx-cw+(dx/2.0))/dx)
      !pluvio(jp).columna=IFIX((cn-pluvio(jp).utmy+(dy/2.0))/dy)
      pluvio(jp).fila=IFIX((pluvio(jp).utmx-cw)/dx)+1
      pluvio(jp).columna=IFIX((cn-pluvio(jp).utmy)/dy)+1
    CASE ('N ')
      jn=jn+1
      BACKSPACE (12)
	  ALLOCATE(nivel(jn).obs(0:nt),nivel(jn).sim(0:nt))
	  ALLOCATE(volum(jn).obs(0:nt),volum(jn).sim(0:nt))
	  ALLOCATE(qemb(jn).obs(0:nt),qemb(jn).sim(0:nt),qemb(jn).bal(0:nt),qemb(jn).inst(0:nt)) !!!último agregado por Camilo
	  nivel(jn).obs=0.0
	  nivel(jn).sim=0.0
	  volum(jn).obs=0.0
	  volum(jn).sim=0.0
	  qemb(jn).obs=0.0
	  qemb(jn).sim=0.0
	  qemb(jn).bal=0.0
      READ (12,*,err=216) nivel(jn).codigo,nivel(jn).name,nivel(jn).utmx,nivel(jn).utmy,  &
	                      nivel(jn).elev,orig,(nivel(jn).obs(i),i=1,nt)

      !Guiomar (19/01/2015): cambio para que haga una asignación más correcta de si debe o no entrar al pulso modificado
      suma_niveles=0.0
      DO t_niveles=2,nt
          suma_niveles=suma_niveles + nivel(jn).obs(t_niveles)
      ENDDO
      IF (suma_niveles.eq.0.0) THEN 
          pulm(jn)=1
          mensaje=strings(902)
		  errr=2
          CALL errores
      ENDIF
      GOTO 116
      
216   mensaje=strings(502)
      errr=2 !para mi debería ser errr=1 y falta un goto 94
      CALL errores
      GOTO 94
116   IF (.NOT.bint) THEN  !Control de valores negativos y -1
	  	ncero=0
	    IF (nivel(jn).obs(1).lt.0.0) THEN
	      nivel(jn).obs(1)=0.0
	  	  ncero=ncero+1
	    ENDIF
	    DO i=2,nt
	      IF (nivel(jn).obs(i).lt.0.0) THEN
	  	    ncero=ncero+1
		    !nivel(jn).obs(i)=nivel(jn).obs(i-1)
		  ENDIF
	    ENDDO
        IF (ncero.gt.0) THEN
                IF (lang.eq.1) THEN
                    mensaje='Valores N negativos encontrados en '//TRIM(ADJUSTL(nivel(jn).name))
                ELSEIF (lang.eq.2) THEN
                    mensaje='Negative N values found in '//TRIM(ADJUSTL(nivel(jn).name))
                ENDIF
		    errr=2
            CALL errores
		ENDIF
      ENDIF
	  DO i=1,ncon
        IF (nivel(jn).name.eq.control(i).name) THEN
          nivel(jn).fila=control(i).fila
          nivel(jn).columna=control(i).columna
        ENDIF
      ENDDO
      DO i=1,ncel
        IF (nivel(jn).fila.eq.cell(i).fil) THEN
          IF (nivel(jn).columna.eq.cell(i).col) cell(i).codnie=-jn
        ENDIF
      ENDDO
      nivel(jn).obs(0)=nivel(jn).obs(1)
	  nivel(jn).sim(0)=nivel(jn).obs(1)
      emb(jn).nombre=nivel(jn).name
	  volum(jn).codigo=nivel(jn).codigo
	  volum(jn).name=nivel(jn).name
	  volum(jn).utmx=nivel(jn).utmx
	  volum(jn).utmy=nivel(jn).utmy
	  volum(jn).elev=nivel(jn).elev
      qemb(jn).codigo=nivel(jn).codigo
	  qemb(jn).name=nivel(jn).name
	  qemb(jn).utmx=nivel(jn).utmx
	  qemb(jn).utmy=nivel(jn).utmy
	  qemb(jn).elev=nivel(jn).elev
    CASE ('V ')
      jv=jv+1
      BACKSPACE (12)
	  ALLOCATE(nivel(jv).obs(0:nt),nivel(jv).sim(0:nt))
	  ALLOCATE(volum(jv).obs(0:nt),volum(jv).sim(0:nt))
	  ALLOCATE(qemb(jv).obs(0:nt),qemb(jv).sim(0:nt),qemb(jv).bal(0:nt),qemb(jv).inst(0:nt))
	  nivel(jv).obs=0.0
	  nivel(jv).sim=0.0
	  volum(jv).obs=0.0
	  volum(jv).sim=0.0
	  qemb(jv).obs=0.0
	  qemb(jv).sim=0.0
	  qemb(jv).bal=0.0
      READ (12,*,err=246) volum(jv).codigo,volum(jv).name,volum(jv).utmx,volum(jv).utmy,  &
	                      volum(jv).elev,orig,(volum(jv).obs(i),i=1,nt)
      GOTO 146
246   mensaje=strings(503)
      errr=1
      CALL errores
      GOTO 94
146   IF (.NOT.bint) THEN   !Control de valores negativos y -1
	  	ncero=0
	    IF (volum(jv).obs(1).lt.0.0) THEN
	      volum(jv).obs(1)=0.0
	  	  ncero=ncero+1
	    ENDIF
	    DO i=2,nt
	      IF (volum(jv).obs(i).lt.0.0) THEN
	  	    ncero=ncero+1
		    !volum(jv).obs(i)=volum(jv).obs(i-1)
		  ENDIF
	    ENDDO
        IF (ncero.gt.0) THEN
                IF (lang.eq.1) THEN
                    mensaje='Valores V negativos encontrados en '//TRIM(ADJUSTL(volum(jv).name))
                ELSEIF (lang.eq.2) THEN
                    mensaje='Negative V values found in '//TRIM(ADJUSTL(volum(jv).name))
                ENDIF
		    errr=2
            CALL errores
        ENDIF
      ENDIF
	  DO i=1,ncon
        IF (volum(jv).name.eq.control(i).name) THEN
          volum(jv).fila=control(i).fila
          volum(jv).columna=control(i).columna
        ENDIF
      ENDDO
      emb(jv).nombre=volum(jv).name
	  nivel(jv).codigo=volum(jv).codigo
	  nivel(jv).name=volum(jv).name
	  nivel(jv).utmx=volum(jv).utmx
	  nivel(jv).utmy=volum(jv).utmy
	  nivel(jv).elev=volum(jv).elev
      qemb(jv).codigo=volum(jv).codigo
	  qemb(jv).name=volum(jv).name
	  qemb(jv).utmx=volum(jv).utmx
	  qemb(jv).utmy=volum(jv).utmy
	  qemb(jv).elev=volum(jv).elev
      volum(jv).obs(0)=volum(jv).obs(1)  
	  volum(jv).sim(0)=volum(jv).obs(1)
	CASE ('S ')
      js=js+1
      BACKSPACE (12)
	  ALLOCATE(nivel(js).obs(0:nt),nivel(js).sim(0:nt))
 	  ALLOCATE(volum(js).obs(0:nt),volum(js).sim(0:nt))
	  ALLOCATE(qemb(js).obs(0:nt),qemb(js).sim(0:nt),qemb(js).bal(0:nt),qemb(js).inst(0:nt))
	  nivel(js).obs=0.0
	  nivel(js).sim=0.0
	  volum(js).obs=0.0
	  volum(js).sim=0.0
	  qemb(js).obs=0.0
	  qemb(js).sim=0.0
	  qemb(js).bal=0.0
	  READ (12,*,err=217) qemb(js).codigo,qemb(js).name,qemb(js).utmx,qemb(js).utmy,  &
	                      qemb(js).elev,orig,(qemb(js).obs(i),i=1,nt)
      emb(js).nombre=qemb(js).name
	  volum(js).codigo=qemb(js).codigo
	  volum(js).name=qemb(js).name
	  volum(js).utmx=qemb(js).utmx
	  volum(js).utmy=qemb(js).utmy
	  volum(js).elev=qemb(js).elev
      nivel(js).codigo=qemb(js).codigo
	  nivel(js).name=qemb(js).name
	  nivel(js).utmx=qemb(js).utmx
	  nivel(js).utmy=qemb(js).utmy
	  nivel(js).elev=qemb(js).elev
	  IF (.NOT.bint) THEN  !control de valores negativos y -1
	  	ncero=0
	    IF (qemb(js).obs(1).lt.0.0) THEN
	  	  ncero=ncero+1
	      qemb(js).obs(1)=0.0
	    ENDIF
	    DO i=2,nt
	      IF (qemb(js).obs(i).lt.0.0) THEN
    	  	ncero=ncero+1
		    !qemb(js).obs(i)=qemb(js).obs(i-1)
		  ENDIF
	    ENDDO
        IF (ncero.gt.0) THEN
                IF (lang.eq.1) THEN
                    mensaje='Valores S negativos encontrados en '//TRIM(ADJUSTL(qemb(js).name))
                ELSEIF (lang.eq.2) THEN
                    mensaje='Negative S values found in '//TRIM(ADJUSTL(qemb(js).name))
                ENDIF

		    errr=2
            CALL errores
        ENDIF
      ENDIF
	  DO i=1,ncon
        IF (qemb(js).name.eq.control(i).name) THEN
          qemb(js).fila=control(i).fila
          qemb(js).columna=control(i).columna
        ENDIF
      ENDDO
      qemb(js).obs(0)=qemb(js).obs(1)
    CASE ('Q ')
      jq=jq+1
      BACKSPACE (12)
	  ALLOCATE(aforo(jq).obs(0:nt),aforo(jq).sim(0:nt))
	  aforo(jq).obs=0.0
      aforo(jq).sim=0.0
      READ (12,*,err=218) aforo(jq).codigo,aforo(jq).name,aforo(jq).utmx,aforo(jq).utmy,  &
	                      aforo(jq).elev,orig,(aforo(jq).obs(i),i=1,nt)
      IF (.NOT.bint) THEN   !Control de valores negativos y -1
          !Guiomar (29/01/2014): Introduzco cambio porque creo que hay un error en esta parte del código al inicializar ncero en ncero + 1 y no en cero
    	!ncero=ncero+1
        ncero=0
	    IF (aforo(jq).obs(1).lt.0.0) THEN
    	 ncero=ncero+1
	      !aforo(jq).obs(1)=0.0
	    ENDIF
	    DO i=2,nt
	      IF (aforo(jq).obs(i).lt.0.0) THEN
    	  	ncero=ncero+1
		    !aforo(jq).obs(i)=MAX(0.0,aforo(jq).obs(i-1))
		  ENDIF
	    ENDDO
        IF (ncero.gt.0) THEN
                IF (lang.eq.1) THEN
                    mensaje='Valores negativos encontrados en '//TRIM(ADJUSTL(aforo(jq).name))
                ELSEIF (lang.eq.2) THEN
                    mensaje='Negative values found in '//TRIM(ADJUSTL(aforo(jq).name))
                ENDIF
		    errr=2
            CALL errores
	    ENDIF
	  ENDIF
	  DO i=1,ncon
        IF (aforo(jq).name.eq.control(i).name) THEN
          aforo(jq).fila=control(i).fila
          aforo(jq).columna=control(i).columna
        ENDIF
      ENDDO
      !!Cambios - GIAMBA 11/2011
      IF (config(4)) THEN
	     !Allocata las series temporales de sedimentos y las inicia en cero
	     ALLOCATE (aforo(jq).sed_out(nt,8))
	     ALLOCATE (aforo(jq).sed_temp(nt,20))
	     aforo(jq).sed_out=0.0
	     aforo(jq).sed_temp=0.0
      ENDIF
      !Nitrógeno, Cris (03/2017)
      IF (modulos2(3)) THEN
	     !Allocata las series temporales de nitrógeno y las inicia en cero (Para obtener datos en las Q)
	     ALLOCATE (norgql(jq).sim(nt),amonioql(jq).sim(nt),nitratoql(jq).sim(nt),norgqs(jq).sim(nt),amonioqs(jq).sim(nt))
	     norgql(jq).sim=0.0
	     amonioql(jq).sim=0.0
         nitratoql(jq).sim=0.0
         norgqs(jq).sim=0.0
         amonioqs(jq).sim=0.0
      ENDIF
	  aforo(jq).obs(0)=aforo(jq).obs(1)
    CASE ('X ')
    IF (config(4)) THEN
      jsedq=jsedq+1
      BACKSPACE (12)
	  ALLOCATE(aforosed(jsedq).obs(0:nt),aforosed(jsedq).sim(0:nt),aforosed(jsedq).sed_out(nt,3))
	  aforosed(jsedq).obs=0.0
      aforosed(jsedq).sim=0.0
      aforosed(jsedq).sed_out=0.0
      READ (12,*,err=226) aforosed(jsedq).codigo,aforosed(jsedq).name,aforosed(jsedq).utmx,aforosed(jsedq).utmy,  &
	                      aforosed(jsedq).elev,orig,(aforosed(jsedq).obs(i),i=1,nt)
      IF (.NOT.bint) THEN   !Control de valores negativos y -1
    	ncero=ncero+1
	    IF (aforosed(jsedq).obs(1).lt.0.0) THEN
    	 ncero=ncero+1
	      !aforo(jq).obs(1)=0.0
	    ENDIF
	    DO i=2,nt
	      IF (aforosed(jsedq).obs(i).lt.0.0) THEN
    	  	ncero=ncero+1
		    !aforo(jq).obs(i)=MAX(0.0,aforo(jq).obs(i-1))
		  ENDIF
	    ENDDO
        IF (ncero.gt.0) THEN
                IF (lang.eq.1) THEN
                    mensaje='Valores negativos encontrados en '//TRIM(ADJUSTL(aforosed(jsedq).name))
                ELSEIF (lang.eq.2) THEN
                    mensaje='Negative values found in '//TRIM(ADJUSTL(aforosed(jsedq).name))
                ENDIF
		    errr=2
            CALL errores
	    ENDIF
	  ENDIF

	  DO i=1,ncon
        IF (aforosed(jsedq).name.eq.control(i).name) THEN
          aforosed(jsedq).fila=control(i).fila
          aforosed(jsedq).columna=control(i).columna
        ENDIF
      ENDDO
!	  aforo(jq).obs(0)=aforo(jq).obs(1)
      aforosed(jsedq).obs(0)=aforosed(jsedq).obs(1) !Cambio (Cris 11/2015)
	ENDIF  

    CASE ('B ')
      ja=ja+1
      BACKSPACE (12)
	  ALLOCATE(otros(ja).sim(0:nt))
	  otros(ja).sim=0.0
      READ (12,*,err=219) otros(ja).codigo,otros(ja).name,otros(ja).utmx,otros(ja).utmy,  &
	                      otros(ja).elev
      DO i=1,ncon
        IF (otros(ja).name.eq.control(i).name) THEN
          otros(ja).fila=control(i).fila
          otros(ja).columna=control(i).columna
        ENDIF
      ENDDO
      
      !!Cambios - GIAMBA 11/2011
      IF (config(4)) THEN
	     !Allocata las series temporales de sedimentos y las inicia en cero
	     ALLOCATE (otros(ja).sed_out(nt,8))
	     ALLOCATE (otros(ja).sed_temp(nt,20))
         otros(ja).sed_out=0.0
	     otros(ja).sed_temp=0.0
      ENDIF
      
    CASE ('T ')
      jt=jt+1
      IF (ktem.gt.0) THEN
	    BACKSPACE (12)
	    ALLOCATE(temper(jt).obs(0:nt))
		temper(jt).obs=0.0
        READ (12,*,err=220) temper(jt).codigo,temper(jt).name,temper(jt).utmx,    &
		                    temper(jt).utmy,temper(jt).elev,orig,(temper(jt).obs(i),i=1,nt)  !ºC/dia
        !Guiomar (14/04/2014): Cambio para resolver error de localización de los puntos
        !temper(jt).fila=IFIX((temper(jt).utmx-cw+(dx/2.0))/dx)
        !temper(jt).columna=IFIX((cn-temper(jt).utmy+(dy/2.0))/dy)
        temper(jt).fila=IFIX((temper(jt).utmx-cw)/dx)+1
        temper(jt).columna=IFIX((cn-temper(jt).utmy)/dy)+1
	 ENDIF
    CASE ('H ')
      jh=jh+1
      IF (kniv.gt.0) THEN
        BACKSPACE (12)
	    ALLOCATE(nieve(jh).obs(1),nieve(jh).sim(0:nt))
	    nieve(jh).obs=0.0
        nieve(jh).sim=0.0
	    READ (12,*,err=221) nieve(jh).codigo,nieve(jh).name,nieve(jh).utmx,nieve(jh).utmy,  &
	                        nieve(jh).elev,orig,nieve(jh).obs(1)	   !mm
        !Guiomar (14/04/2014): Cambio para resolver error de localización de los puntos
        !nieve(jh).fila=IFIX((nieve(jh).utmx-cw+(dx/2.0))/dx)
        !nieve(jh).columna=IFIX((cn-nieve(jh).utmy+(dy/2.0))/dy)
        nieve(jh).fila=IFIX((nieve(jh).utmx-cw)/dx)+1
        nieve(jh).columna=IFIX((cn-nieve(jh).utmy)/dy)+1
	    IF (.NOT.bint) THEN  !control de valores negativos y -1
	        IF (nieve(jh).obs(1).lt.0.0) THEN
                IF (lang.eq.1) THEN
                    mensaje='Valores negativos encontrados en '//TRIM(ADJUSTL(nieve(jh).name))
                ELSEIF (lang.eq.2) THEN
                    mensaje='Negative values found in '//TRIM(ADJUSTL(nieve(jh).name))
                ENDIF
                errr=2
                CALL errores
	  	        nieve(jh).obs(1)=0.0
	        ENDIF 
	    ENDIF
	  ENDIF
    CASE ('E ')
      je=je+1
      BACKSPACE (12)
      ALLOCATE(evapo(je).obs(0:nt))
	  evapo(je).obs=0.0
      READ (12,*,err=222) evapo(je).codigo,evapo(je).name,evapo(je).utmx,evapo(je).utmy,  &
	                      evapo(je).elev,orig,(evapo(je).obs(i),i=1,nt)  !mm/dia
      !Guiomar (14/04/2014): Cambio para resolver error de localización de los puntos
      !evapo(je).fila=IFIX((evapo(je).utmx-cw)/dx)
      !evapo(je).columna=IFIX((cn-evapo(je).utmy)/dy)
      evapo(je).fila=IFIX((evapo(je).utmx-cw)/dx)+1
      evapo(je).columna=IFIX((cn-evapo(je).utmy)/dy)+1
	  IF (.NOT.bint) THEN  !Control de -1 y de valores negativos
        ncero=0
	    DO i=1,nt
	      IF (evapo(je).obs(i).lt.0.0) THEN
    	  	ncero=ncero+1
		    evapo(je).obs(i)=0.0
		  ENDIF
        ENDDO
        IF (ncero.gt.0) THEN
                IF (lang.eq.1) THEN
                    mensaje='Valores negativos encontrados en '//TRIM(ADJUSTL(evapo(je).name))
                ELSEIF (lang.eq.2) THEN
                    mensaje='Negative values found in '//TRIM(ADJUSTL(evapo(je).name))
                ENDIF
		    errr=2
            CALL errores
        ENDIF        
      ENDIF
    CASE ('D ')
      ji=ji+1
      BACKSPACE (12)
      ALLOCATE(aport(ji).obs(0:nt),aport(ji).sim(0:nt))
	  aport(ji).obs=0.0
	  aport(ji).sim=0.0
      READ (12,*,err=223) aport(ji).codigo,aport(ji).name,aport(ji).utmx,aport(ji).utmy,  &
	                      aport(ji).elev,orig,(aport(ji).obs(i),i=1,nt)   !m³ o mm ???
	  DO i=1,ncon
        IF (aport(ji).name.eq.control(i).name) THEN
          aport(ji).fila=control(i).fila
          aport(ji).columna=control(i).columna
        ENDIF
      ENDDO
	  aport(ji).obs(0)=aport(ji).obs(1)
    CASE ('W ')
      jw=jw+1
      BACKSPACE (12)
      ALLOCATE(veg(jw).obs(0:nt),veg(jw).sim(0:nt),veg1_point(jw).sim(0:nt),veg2_point(jw).sim(0:nt),tr_point(jw).sim(0:nt))! (Vicente-Guiomar)
	  veg(jw).obs=0.0
      veg(jw).sim=0.0
      READ (12,*,err=225) veg(jw).codigo,veg(jw).name,veg(jw).utmx,veg(jw).utmy,  &
	                      veg(jw).elev,orig,(veg(jw).obs(i),i=1,nt)
      DO i=1,ncon
          IF(veg(jw).name.eq.control(i).name) THEN
              veg(jw).fila=control(i).fila
              veg(jw).columna=control(i).columna
          ENDIF
      ENDDO
      IF (.NOT.bint) THEN   !Control de valores negativos y -1
        !Guiomar (29/01/2014): Introduzco cambio porque creo que hay un error en esta parte del código al inicializar ncero en ncero + 1 y no en cero
    	!ncero=ncero+1
        ncero=0
	    DO i=1,nt
	      IF (veg(jw).obs(i).lt.0.0) THEN
    	  	ncero=ncero+1
		  ENDIF
	    ENDDO
        IF (ncero.gt.0) THEN
                IF (lang.eq.1) THEN
                    mensaje='Valores negativos encontrados en '//TRIM(ADJUSTL(veg(jw).name))
                ELSEIF (lang.eq.2) THEN
                    mensaje='Negative values found in '//TRIM(ADJUSTL(veg(jw).name))
                ENDIF
		    errr=2
            CALL errores
	    ENDIF
      ENDIF
      !Guiomar (29/01/2014): Por ahora, no voy a incluir los puntos de vegetación en los puntos de control, pero creo que sí hará falta hacer el cambio en el futuro. Por eso dejo la parte del código a emplear en ese supuesto
	  !DO i=1,ncon
       ! IF (veg(jw).name.eq.control(i).name) THEN
         ! veg(jw).fila=control(i).fila
         ! veg(jw).columna=control(i).columna
       ! ENDIF
      !ENDDO
    CASE ('R ')
      jr=jr+1
      BACKSPACE (12)
	  ALLOCATE(radiacion(jr).obs(0:nt))
	  radiacion(jr).obs=0.0
      READ (12,*,err=225) radiacion(jr).codigo,radiacion(jr).name,radiacion(jr).utmx,radiacion(jr).utmy,  &
	                      radiacion(jr).elev,orig,(radiacion(jr).obs(i),i=1,nt)
      IF (.NOT.bint) THEN   !Control de valores negativos y -1
        !Guiomar (29/01/2014): Introduzco cambio porque creo que hay un error en esta parte del código al inicializar ncero en ncero + 1 y no en cero
    	!ncero=ncero+1
        ncero=0
	    DO i=1,nt
	      IF (radiacion(jr).obs(i).lt.0.0) THEN
    	  	ncero=ncero+1
		  ENDIF
	    ENDDO
        IF (ncero.gt.0) THEN
                IF (lang.eq.1) THEN
                    mensaje='Valores negativos encontrados en '//TRIM(ADJUSTL(radiacion(jr).name))
                ELSEIF (lang.eq.2) THEN
                    mensaje='Negative values found in '//TRIM(ADJUSTL(radiacion(jr).name))
                ENDIF
		    errr=2
            CALL errores
	    ENDIF
      ENDIF
      radiacion(jr).fila=ifix((radiacion(jr).utmx-cw+(dx/2.0))/dx)
      radiacion(jr).columna=ifix((cn-radiacion(jr).utmy+(dy/2.0))/dy)
    CASE ('DA ') ! (Cris 11/2015)
    IF (config(4)) THEN
      jsedadi1=jsedadi1+1
      BACKSPACE (12)
	  ALLOCATE(aportsed1(jsedadi1).obs(0:nt),aportsed1(jsedadi1).sim(0:nt))
	  aportsed1(jsedadi1).obs=0.0
      aportsed1(jsedadi1).sim=0.0
      READ (12,*,err=227) aportsed1(jsedadi1).codigo,aportsed1(jsedadi1).name,aportsed1(jsedadi1).utmx,  &
	                      aportsed1(jsedadi1).utmy,aportsed1(jsedadi1).elev,orig,(aportsed1(jsedadi1).obs(i),i=1,nt)
	  DO i=1,ncon
        IF (aportsed1(jsedadi1).name.eq.control(i).name) THEN
          aportsed1(jsedadi1).fila=control(i).fila
          aportsed1(jsedadi1).columna=control(i).columna
        ENDIF
      ENDDO
      aportsed1(jsedadi1).obs(0)=aportsed1(jsedadi1).obs(1)
	ENDIF  
	CASE ('DL ') ! (Cris 11/2015)
    IF (config(4)) THEN
      jsedadi2=jsedadi2+1
      BACKSPACE (12)
	  ALLOCATE(aportsed2(jsedadi2).obs(0:nt),aportsed2(jsedadi2).sim(0:nt))
	  aportsed2(jsedadi2).obs=0.0
      aportsed2(jsedadi2).sim=0.0
      READ (12,*,err=228) aportsed2(jsedadi2).codigo,aportsed2(jsedadi2).name,aportsed2(jsedadi2).utmx,  &
	                      aportsed2(jsedadi2).utmy,aportsed2(jsedadi2).elev,orig,(aportsed2(jsedadi2).obs(i),i=1,nt)
	  DO i=1,ncon
        IF (aportsed2(jsedadi2).name.eq.control(i).name) THEN
          aportsed2(jsedadi2).fila=control(i).fila
          aportsed2(jsedadi2).columna=control(i).columna
        ENDIF
      ENDDO
      aportsed2(jsedadi2).obs(0)=aportsed2(jsedadi2).obs(1)
	ENDIF 
	CASE ('DC ') ! (Cris 11/2015)
    IF (config(4)) THEN
      jsedadi3=jsedadi3+1
      BACKSPACE (12)
	  ALLOCATE(aportsed3(jsedadi3).obs(0:nt),aportsed3(jsedadi3).sim(0:nt))
	  aportsed3(jsedadi3).obs=0.0
      aportsed3(jsedadi3).sim=0.0
      READ (12,*,err=229) aportsed3(jsedadi3).codigo,aportsed3(jsedadi3).name,aportsed3(jsedadi3).utmx,  &
	                      aportsed3(jsedadi3).utmy,aportsed3(jsedadi3).elev,orig,(aportsed3(jsedadi3).obs(i),i=1,nt)
	  DO i=1,ncon
        IF (aportsed3(jsedadi3).name.eq.control(i).name) THEN
          aportsed3(jsedadi3).fila=control(i).fila
          aportsed3(jsedadi3).columna=control(i).columna
        ENDIF
      ENDDO
      aportsed3(jsedadi3).obs(0)=aportsed3(jsedadi3).obs(1)
    ENDIF
    CASE ('NO')
      jno=jno+1
      BACKSPACE (12)
	  ALLOCATE(norg(jno).obs(0:nt),norg(jno).sim(0:nt))
	  norg(jno).obs=0.0
      norg(jno).sim=0.0
      READ (12,*,err=230) norg(jno).codigo,norg(jno).name,norg(jno).utmx,norg(jno).utmy,  &
	                      norg(jno).elev,orig,(norg(jno).obs(i),i=1,nt)
      IF (.NOT.bint) THEN   !Control de valores negativos y -1
        ncero=0
	    DO i=1,nt
	      IF (norg(jno).obs(i).lt.0.0) THEN
    	  	ncero=ncero+1
		  ENDIF
	    ENDDO
        IF (ncero.gt.0) THEN
                IF (lang.eq.1) THEN
                    mensaje='Valores negativos encontrados en '//TRIM(ADJUSTL(norg(jno).name))
                ELSEIF (lang.eq.2) THEN
                    mensaje='Negative values found in '//TRIM(ADJUSTL(norg(jno).name))
                ENDIF
		    errr=2
            CALL errores
	    ENDIF
      ENDIF
      DO i=1,ncon
        IF (norg(jno).name.eq.control(i).name) THEN
          norg(jno).fila=control(i).fila
          norg(jno).columna=control(i).columna
        ENDIF
      ENDDO
      norg(jno).obs(0)=norg(jno).obs(1)
      CASE ('AM')
      jam=jam+1
      BACKSPACE (12)
	  ALLOCATE(amonio(jam).obs(0:nt),amonio(jam).sim(0:nt))
	  amonio(jam).obs=0.0
      amonio(jam).sim=0.0
      READ (12,*,err=231) amonio(jam).codigo,amonio(jam).name,amonio(jam).utmx,amonio(jam).utmy,  &
	                      amonio(jam).elev,orig,(amonio(jam).obs(i),i=1,nt)
      IF (.NOT.bint) THEN   !Control de valores negativos y -1
        ncero=0
	    DO i=1,nt
	      IF (amonio(jam).obs(i).lt.0.0) THEN
    	  	ncero=ncero+1
		  ENDIF
	    ENDDO
        IF (ncero.gt.0) THEN
                IF (lang.eq.1) THEN
                    mensaje='Valores negativos encontrados en '//TRIM(ADJUSTL(amonio(jam).name))
                ELSEIF (lang.eq.2) THEN
                    mensaje='Negative values found in '//TRIM(ADJUSTL(amonio(jam).name))
                ENDIF
		    errr=2
            CALL errores
	    ENDIF
      ENDIF
      DO i=1,ncon
        IF (amonio(jam).name.eq.control(i).name) THEN
          amonio(jam).fila=control(i).fila
          amonio(jam).columna=control(i).columna
        ENDIF
      ENDDO
      amonio(jam).obs(0)=amonio(jam).obs(1)
      CASE ('NI')
      jni=jni+1
      BACKSPACE (12)
	  ALLOCATE(nitrato(jni).obs(0:nt),nitrato(jni).sim(0:nt))
	  nitrato(jni).obs=0.0
      nitrato(jni).sim=0.0
      READ (12,*,err=232) nitrato(jni).codigo,nitrato(jni).name,nitrato(jni).utmx,nitrato(jni).utmy,  &
	                      nitrato(jni).elev,orig,(nitrato(jni).obs(i),i=1,nt)
      IF (.NOT.bint) THEN   !Control de valores negativos y -1
        ncero=0
	    DO i=1,nt
	      IF (nitrato(jni).obs(i).lt.0.0) THEN
    	  	ncero=ncero+1
		  ENDIF
	    ENDDO
        IF (ncero.gt.0) THEN
                IF (lang.eq.1) THEN
                    mensaje='Valores negativos encontrados en '//TRIM(ADJUSTL(amonio(jam).name))
                ELSEIF (lang.eq.2) THEN
                    mensaje='Negative values found in '//TRIM(ADJUSTL(amonio(jam).name))
                ENDIF
		    errr=2
            CALL errores
	    ENDIF
      ENDIF
      DO i=1,ncon
        IF (nitrato(jni).name.eq.control(i).name) THEN
          nitrato(jni).fila=control(i).fila
          nitrato(jni).columna=control(i).columna
        ENDIF
      ENDDO
      nitrato(jni).obs(0)=nitrato(jni).obs(1)
  END SELECT
ENDDO
10 FORMAT(A2)
CLOSE(12)

!Pasa valores de Evapo diaria a valores del intervalo
DO k=1,kevp
  DO i=1,nt
    evapo(k).obs(i)=evapo(k).obs(i)*dt/24.
  ENDDO
ENDDO
!!!!!!!ESTO HAY QUE QUITARLO!!!!!!!!!! CRIS Y VICENTE
!nt=25

GOTO 95

212 mensaje=strings(51)
errr=1
CALL errores
GOTO 94
215 IF (lang.eq.1) THEN
        mensaje='056 Lectura de datos errónea en la línea P de '//TRIM(ADJUSTL(arch(5)))//', nombre: '//TRIM(ADJUSTL(pluvio(jp).name))
    ELSEIF (lang.eq.2) THEN
        mensaje='056 Error reading the P line of '//TRIM(ADJUSTL(arch(5)))//', name: '//TRIM(ADJUSTL(pluvio(jp).name))
    ENDIF
errr=1
CALL errores
GOTO 94
217 IF (lang.eq.1) THEN
        mensaje='056 Lectura de datos errónea en la línea S de '//TRIM(ADJUSTL(arch(5)))//', nombre: '//TRIM(ADJUSTL(qemb(js).name))
    ELSEIF (lang.eq.2) THEN
        mensaje='056 Error reading the S line of '//TRIM(ADJUSTL(arch(5)))//', name: '//TRIM(ADJUSTL(qemb(js).name))
    ENDIF
errr=1
CALL errores
GOTO 94
218 IF (lang.eq.1) THEN
        mensaje='056 Lectura de datos errónea en la línea Q de '//TRIM(ADJUSTL(arch(5)))//', nombre: '//TRIM(ADJUSTL(aforo(jq).name))
    ELSEIF (lang.eq.2) THEN
        mensaje='056 Error reading the Q line of '//TRIM(ADJUSTL(arch(5)))//', name: '//TRIM(ADJUSTL(aforo(jq).name))
    ENDIF
    errr=1
CALL errores
GOTO 94
219 IF (lang.eq.1) THEN
        mensaje='056 Lectura de datos errónea en la línea B de '//TRIM(ADJUSTL(arch(5)))//', nombre: '//TRIM(ADJUSTL(otros(ja).name))
    ELSEIF (lang.eq.2) THEN
        mensaje='056 Error reading the B line of '//TRIM(ADJUSTL(arch(5)))//', name: '//TRIM(ADJUSTL(otros(ja).name))
    ENDIF
    errr=1
CALL errores
GOTO 94
220 IF (lang.eq.1) THEN
        mensaje='056 Lectura de datos errónea en la línea T de '//TRIM(ADJUSTL(arch(5)))//', nombre: '//TRIM(ADJUSTL(temper(jt).name))
    ELSEIF (lang.eq.2) THEN
        mensaje='056 Error reading the T line of '//TRIM(ADJUSTL(arch(5)))//', name: '//TRIM(ADJUSTL(temper(jt).name))
    ENDIF
    errr=1
CALL errores
GOTO 94
221 IF (lang.eq.1) THEN
        mensaje='056 Lectura de datos errónea en la línea C de '//TRIM(ADJUSTL(arch(5)))//', nombre: '//TRIM(ADJUSTL(nieve(jh).name))
    ELSEIF (lang.eq.2) THEN
        mensaje='056 Error reading the C line of '//TRIM(ADJUSTL(arch(5)))//', name: '//TRIM(ADJUSTL(nieve(jh).name))
    ENDIF
    errr=1
CALL errores
GOTO 94
222 IF (lang.eq.1) THEN
        mensaje='056 Lectura de datos errónea en la línea E de '//TRIM(ADJUSTL(arch(5)))//', nombre: '//TRIM(ADJUSTL(evapo(je).name))
    ELSEIF (lang.eq.2) THEN
        mensaje='056 Error reading the E line of '//TRIM(ADJUSTL(arch(5)))//', name: '//TRIM(ADJUSTL(evapo(je).name))
    ENDIF
    errr=1
CALL errores
GOTO 94
223 IF (lang.eq.1) THEN
        mensaje='056 Lectura de datos errónea en la línea I de '//TRIM(ADJUSTL(arch(5)))//', nombre: '//TRIM(ADJUSTL(aport(ji).name))
    ELSEIF (lang.eq.2) THEN
        mensaje='056 Error reading the I line of '//TRIM(ADJUSTL(arch(5)))//', name: '//TRIM(ADJUSTL(aport(ji).name))
    ENDIF
    errr=1
CALL errores
goto 94
226 IF (lang.eq.1) THEN
        mensaje='056 Lectura de datos errónea en la línea X de '//TRIM(ADJUSTL(arch(5)))//', nombre: '//TRIM(ADJUSTL(aforosed(jsedq).name))
    ELSEIF (lang.eq.2) THEN
        mensaje='056 Error reading the X line of '//TRIM(ADJUSTL(arch(5)))//', name: '//TRIM(ADJUSTL(aforosed(jsedq).name))
    ENDIF
    errr=1
CALL errores
goto 94
225 IF (lang.eq.1) THEN
        mensaje='056 Lectura de datos errónea en la línea W de '//TRIM(ADJUSTL(arch(5)))//', nombre:'//TRIM(ADJUSTL(veg(jw).name))
    ELSEIF (lang.eq.2) THEN
        mensaje='056 Error reading the W line of '//TRIM(ADJUSTL(arch(5)))//', name: '//TRIM(ADJUSTL(veg(jw).name))
    ENDIF
    errr=1
CALL errores
goto 94
227 IF (lang.eq.1) THEN  !(Cris 11/2015)
        mensaje='056 Lectura de datos errónea en la línea DA de '//TRIM(ADJUSTL(arch(5)))//', nombre: '//TRIM(ADJUSTL(aportsed1(jsedadi1).name))
    ELSEIF (lang.eq.2) THEN
        mensaje='056 Error reading the DA line of '//TRIM(ADJUSTL(arch(5)))//', name: '//TRIM(ADJUSTL(aportsed1(jsedadi1).name))
    ENDIF
    errr=1
CALL errores
goto 94
228 IF (lang.eq.1) THEN  !(Cris 11/2015)
        mensaje='056 Lectura de datos errónea en la línea DL de '//TRIM(ADJUSTL(arch(5)))//', nombre: '//TRIM(ADJUSTL(aportsed2(jsedadi2).name))
    ELSEIF (lang.eq.2) THEN
        mensaje='056 Error reading the DL line of '//TRIM(ADJUSTL(arch(5)))//', name: '//TRIM(ADJUSTL(aportsed2(jsedadi2).name))
    ENDIF
    errr=1
CALL errores
goto 94
229 IF (lang.eq.1) THEN  !(Cris 11/2015)
        mensaje='056 Lectura de datos errónea en la línea DC de '//TRIM(ADJUSTL(arch(5)))//', nombre: '//TRIM(ADJUSTL(aportsed3(jsedadi3).name))
    ELSEIF (lang.eq.2) THEN
        mensaje='056 Error reading the DC line of '//TRIM(ADJUSTL(arch(5)))//', name: '//TRIM(ADJUSTL(aportsed3(jsedadi3).name))
    ENDIF
    errr=1
CALL errores
230 IF (lang.eq.1) THEN  !(Cris 11/2015)
        mensaje='056 Lectura de datos errónea en la línea NO de '//TRIM(ADJUSTL(arch(5)))//', nombre: '//TRIM(ADJUSTL(aportsed3(jsedadi3).name))
    ELSEIF (lang.eq.2) THEN
        mensaje='056 Error reading the DC line of '//TRIM(ADJUSTL(arch(5)))//', name: '//TRIM(ADJUSTL(aportsed3(jsedadi3).name))
    ENDIF
    errr=1
CALL errores
231 IF (lang.eq.1) THEN  !(Cris 11/2015)
        mensaje='056 Lectura de datos errónea en la línea AM de '//TRIM(ADJUSTL(arch(5)))//', nombre: '//TRIM(ADJUSTL(aportsed3(jsedadi3).name))
    ELSEIF (lang.eq.2) THEN
        mensaje='056 Error reading the DC line of '//TRIM(ADJUSTL(arch(5)))//', name: '//TRIM(ADJUSTL(aportsed3(jsedadi3).name))
    ENDIF
    errr=1
CALL errores
232 IF (lang.eq.1) THEN  !(Cris 11/2015)
        mensaje='056 Lectura de datos errónea en la línea NI de '//TRIM(ADJUSTL(arch(5)))//', nombre: '//TRIM(ADJUSTL(aportsed3(jsedadi3).name))
    ELSEIF (lang.eq.2) THEN
        mensaje='056 Error reading the DC line of '//TRIM(ADJUSTL(arch(5)))//', name: '//TRIM(ADJUSTL(aportsed3(jsedadi3).name))
    ENDIF
    errr=1
CALL errores
goto 94

94 WRITE(*,*)strings(800)
sale=2

95 END SUBROUTINE


!*************************************************************
!* Lectura del episodio de lluvia (2ª parte) y formato COLUMNA
!*************************************************************
SUBROUTINE lee_evto2col
USE modtet
IMPLICIT NONE

!quitar!! es una prueba
REAL fecha,lluvia,caudal,temperatura,ept
sale=0
!Lee ficheros del evento
OPEN(12,file=arch(5),status='old',err=212)
jp=0
jq=0
jn=0
jh=0
je=0
jt=0
ja=0
ios=0
ji=0
jsedq=0
jsedv=0
jv=nemb
js=nemb+vnemb
!Guiomar (29/01/2014): añado lo correspondiente a vegetación
jw=0
jr=0
jsedadi1=0 !(Cris 11/2015)
jsedadi2=0 !(Cris 11/2015)
jsedadi3=0 !(Cris 11/2015)
!Cris nitrógeno (12/2017)
jno=0
jam=0
jni=0
DO WHILE (ios.ne.-1)
  aa='-*'
  READ (12,10,iostat=ios) aa
  SELECT CASE (aa)
    CASE ('P ')
      jp=jp+1
      BACKSPACE (12)
      ALLOCATE(pluvio(jp)%obs(nt))
	  pluvio(jp).obs=0.0
	  READ (12,*,err=215) pluvio(jp).codigo,pluvio(jp).name,pluvio(jp).utmx,  &
	                      pluvio(jp).utmy,pluvio(jp).elev
      !Guiomar (29/01/2014): Aquí no se puede hacer el control de negativos de esta manera. Lo desactivo.
	  !IF (.NOT.bint) THEN !Si no existen -1 en la serie de datos
	    !DO i=1,nt
	     ! IF (pluvio(jp).obs(i).lt.0.0) THEN
		  !  pluvio(jp).obs(i)=0.0
           !     IF (lang.eq.1) THEN
            !        mensaje='Valores negativos encontrados en '//TRIM(ADJUSTL(pluvio(jp).name))
             !   ELSEIF (lang.eq.2) THEN
              !      mensaje='Negative values found in '//TRIM(ADJUSTL(pluvio(jp).name))
               ! ENDIF
		    !errr=2
            !CALL errores(errr,mensaje,lang)
	   	  !ENDIF
	    !END DO
	  !ENDIF
      !Guiomar (14/04/2014): Cambio para resolver error de localización de los puntos
      !pluvio(jp).fila=IFIX((pluvio(jp).utmx-cw+(dx/2.0))/dx)
      !pluvio(jp).columna=IFIX((cn-pluvio(jp).utmy+(dy/2.0))/dy)
      pluvio(jp).fila=IFIX((pluvio(jp).utmx-cw)/dx)+1
      pluvio(jp).columna=IFIX((cn-pluvio(jp).utmy)/dy)+1
    CASE ('N ')
      jn=jn+1
      BACKSPACE (12)
	  ALLOCATE(nivel(jn).obs(0:nt),nivel(jn).sim(0:nt))
	  ALLOCATE(volum(jn).obs(0:nt),volum(jn).sim(0:nt))
	  ALLOCATE(qemb(jn).obs(0:nt),qemb(jn).sim(0:nt),qemb(jn).bal(0:nt))
	  nivel(jn).obs=0.0
	  nivel(jn).sim=0.0
	  volum(jn).obs=0.0
	  volum(jn).sim=0.0
	  qemb(jn).obs=0.0
	  qemb(jn).sim=0.0
	  qemb(jn).bal=0.0
      READ (12,*,err=216) nivel(jn).codigo,nivel(jn).name,nivel(jn).utmx,nivel(jn).utmy,  &
	                      nivel(jn).elev
      GOTO 116
216   mensaje=strings(502)
      errr=2
      CALL errores
116	  DO i=1,ncon
        IF (nivel(jn).name.eq.control(i).name) THEN
          nivel(jn).fila=control(i).fila
          nivel(jn).columna=control(i).columna
        ENDIF
      ENDDO
      DO i=1,ncel
        IF (nivel(jn).fila.eq.cell(i).fil) THEN
          IF (nivel(jn).columna.eq.cell(i).col) cell(i).codnie=-jn
        ENDIF
      ENDDO
	  emb(jn).nombre=nivel(jn).name
      volum(jn).codigo=nivel(jn).codigo
	  volum(jn).name=nivel(jn).name
	  volum(jn).utmx=nivel(jn).utmx
	  volum(jn).utmy=nivel(jn).utmy
	  volum(jn).elev=nivel(jn).elev
      qemb(jn).codigo=nivel(jn).codigo
	  qemb(jn).name=nivel(jn).name
	  qemb(jn).utmx=nivel(jn).utmx
	  qemb(jn).utmy=nivel(jn).utmy
	  qemb(jn).elev=nivel(jn).elev
    CASE ('V ')
      jv=jv+1
      BACKSPACE (12)
	  ALLOCATE(nivel(jv).obs(0:nt),nivel(jv).sim(0:nt))
	  ALLOCATE(volum(jv).obs(0:nt),volum(jv).sim(0:nt))
	  ALLOCATE(qemb(jv).obs(0:nt),qemb(jv).sim(0:nt),qemb(jv).bal(0:nt))
	  nivel(jv).obs=0.0
	  nivel(jv).sim=0.0
	  volum(jv).obs=0.0
	  volum(jv).sim=0.0
	  qemb(jv).obs=0.0
	  qemb(jv).sim=0.0
	  qemb(jv).bal=0.0
      READ (12,*,err=246) volum(jv).codigo,volum(jv).name,volum(jv).utmx,volum(jv).utmy,  &
	                      volum(jv).elev
      GOTO 146
246   mensaje=strings(503)
      errr=1
      CALL errores
      GOTO 94
146   DO i=1,ncon
        IF (volum(jv).name.eq.control(i).name) THEN
          volum(jv).fila=control(i).fila
          volum(jv).columna=control(i).columna
        ENDIF
      ENDDO
	  emb(jv).nombre=volum(jv).name
	  qemb(jv).codigo=volum(jv).codigo
      qemb(jv).name=volum(jv).name
	  qemb(jv).utmx=volum(jv).utmx
	  qemb(jv).utmy=volum(jv).utmy
	  qemb(jv).elev=volum(jv).elev
      nivel(jv).codigo=volum(jv).codigo
	  nivel(jv).name=volum(jv).name
	  nivel(jv).utmx=volum(jv).utmx
	  nivel(jv).utmy=volum(jv).utmy
	  nivel(jv).elev=volum(jv).elev
	CASE ('S ')
      js=js+1
      BACKSPACE (12)
	  ALLOCATE(nivel(js).obs(0:nt),nivel(js).sim(0:nt))
	  ALLOCATE(volum(js).obs(0:nt),volum(js).sim(0:nt))
	  ALLOCATE(qemb(js).obs(0:nt),qemb(js).sim(0:nt),qemb(js).bal(0:nt))
	  nivel(js).obs=0.0
	  nivel(js).sim=0.0
	  volum(js).obs=0.0
	  volum(js).sim=0.0
	  qemb(js).obs=0.0
	  qemb(js).sim=0.0
	  qemb(js).bal=0.0
	  READ (12,*,err=217) qemb(js).codigo,qemb(js).name,qemb(js).utmx,qemb(js).utmy,  &
	                      qemb(js).elev
      emb(js).nombre=qemb(js).name
	  volum(js).codigo=qemb(js).codigo
      volum(js).name=qemb(js).name
	  volum(js).utmx=qemb(js).utmx
	  volum(js).utmy=qemb(js).utmy
	  volum(js).elev=qemb(js).elev
      nivel(js).codigo=qemb(js).codigo
	  nivel(js).name=qemb(js).name
	  nivel(js).utmx=qemb(js).utmx
	  nivel(js).utmy=qemb(js).utmy
	  nivel(js).elev=qemb(js).elev
	  DO i=1,ncon
        IF (qemb(js).name.eq.control(i).name) THEN
          qemb(js).fila=control(i).fila
          qemb(js).columna=control(i).columna
        ENDIF
      ENDDO
    CASE ('Q ')
      jq=jq+1
      BACKSPACE (12)
	  ALLOCATE(aforo(jq).obs(0:nt),aforo(jq).sim(0:nt))
	  aforo(jq).obs=0.0
	  aforo(jq).sim=0.0
      READ (12,*,err=218) aforo(jq).codigo,aforo(jq).name,aforo(jq).utmx,aforo(jq).utmy,  &
	                      aforo(jq).elev
	  DO i=1,ncon
        IF (aforo(jq).name.eq.control(i).name) THEN
          aforo(jq).fila=control(i).fila
          aforo(jq).columna=control(i).columna
        ENDIF
      ENDDO
      
      !!Cambios - GIAMBA 11/2011
      IF (ALLOCATED(aforo(jq).sed_out)) DEALLOCATE(aforo(jq).sed_out)
      IF (ALLOCATED(aforo(jq).sed_temp)) DEALLOCATE(aforo(jq).sed_temp)
      IF (config(4)) THEN
	     !Allocata las series temporales de sedimentos y las inicia en cero
	     ALLOCATE (aforo(jq).sed_out(nt,8))
	     ALLOCATE (aforo(jq).sed_temp(nt,20))
	     aforo(jq).sed_out=0.0
	     aforo(jq).sed_temp=0.0
      ENDIF
      
      
    CASE ('B ')
      ja=ja+1
      BACKSPACE (12)
	  ALLOCATE(otros(ja).sim(0:nt))
	  otros(ja).obs=0.0
      READ (12,*,err=219) otros(ja).codigo,otros(ja).name,otros(ja).utmx,otros(ja).utmy,  &
	                      otros(ja).elev
      DO i=1,ncon
        IF (otros(ja).name.eq.control(i).name) THEN
          otros(ja).fila=control(i).fila
          otros(ja).columna=control(i).columna
        ENDIF
      ENDDO
      
      !!Cambios - GIAMBA 11/2011
      IF (config(4)) THEN
	     !Allocata las series temporales de sedimentos y las inicia en cero
	     ALLOCATE (otros(ja).sed_out(nt,8))
	     ALLOCATE (otros(ja).sed_temp(nt,20))
         otros(ja).sed_out=0.0
	     otros(ja).sed_temp=0.0
      ENDIF      
      
    CASE ('T ')
      jt=jt+1
      IF (ktem.gt.0) THEN
	    BACKSPACE (12)
	    ALLOCATE(temper(jt).obs(0:nt))
		temper(jt).obs=0.0
        READ (12,*,err=220) temper(jt).codigo,temper(jt).name,temper(jt).utmx,    &
		                    temper(jt).utmy,temper(jt).elev
        !Guiomar (14/04/2014): Cambio para resolver error de localización de los puntos
        !temper(jt).fila=IFIX((temper(jt).utmx-cw+(dx/2.0))/dx)
        !temper(jt).columna=IFIX((cn-temper(jt).utmy+(dy/2.0))/dy)
        temper(jt).fila=IFIX((temper(jt).utmx-cw)/dx)+1
        temper(jt).columna=IFIX((cn-temper(jt).utmy)/dy)+1
	 ENDIF
    CASE ('H ')
      jh=jh+1
      BACKSPACE (12)
	  ALLOCATE(nieve(jh).obs(1),nieve(jh).sim(0:nt))
	  nieve(jh).obs=0.0
	  nieve(jh).sim=0.0
	  READ (12,*,err=221) nieve(jh).codigo,nieve(jh).name,nieve(jh).utmx,nieve(jh).utmy,  &
	                      nieve(jh).elev
      !Guiomar (14/04/2014): Cambio para resolver error de localización de los puntos
      !nieve(jh).fila=IFIX((nieve(jh).utmx-cw+(dx/2.0))/dx)
      !nieve(jh).columna=IFIX((cn-nieve(jh).utmy+(dy/2.0))/dy)
      nieve(jh).fila=IFIX((nieve(jh).utmx-cw)/dx)+1
      nieve(jh).columna=IFIX((cn-nieve(jh).utmy)/dy)+1
    CASE ('E ')
      je=je+1
      BACKSPACE (12)
      ALLOCATE(evapo(je).obs(0:nt))
	  evapo(je).obs=0.0
      READ (12,*,err=222) evapo(je).codigo,evapo(je).name,evapo(je).utmx,evapo(je).utmy,  &
	                      evapo(je).elev
      !Guiomar (14/04/2014): Cambio para resolver error de localización de los puntos
      !evapo(je).fila=IFIX((evapo(je).utmx-cw-(dx/2.0))/dx)
      !evapo(je).columna=IFIX((cn-evapo(je).utmy-(dy/2.0))/dy)
      evapo(je).fila=IFIX((evapo(je).utmx-cw)/dx)+1
      evapo(je).columna=IFIX((cn-evapo(je).utmy)/dy)+1
    CASE ('D ')
      ji=ji+1
      BACKSPACE (12)
      ALLOCATE(aport(ji).obs(0:nt),aport(ji).sim(0:nt))
	  aport(ji).obs=0.0
	  aport(ji).sim=0.0
      READ (12,*,err=223) aport(ji).codigo,aport(ji).name,aport(ji).utmx,aport(ji).utmy,  &
	                      aport(ji).elev
	  DO i=1,ncon
        IF (aport(ji).name.eq.control(i).name) THEN
          aport(ji).fila=control(i).fila
          aport(ji).columna=control(i).columna
        ENDIF
      ENDDO
    CASE ('X ')
     IF (config(4)) THEN
      jsedq=jsedq+1
      BACKSPACE (12)
	  ALLOCATE(aforosed(jsedq).obs(0:nt),aforosed(jsedq).sim(0:nt),aforosed(jsedq).sed_out(nt,3))
	  aforosed(jsedq).obs=0.0
	  aforosed(jsedq).sim=0.0
      aforosed(jsedq).sed_out=0.0
      READ (12,*,err=229) aforosed(jsedq).codigo,aforosed(jsedq).name,aforosed(jsedq).utmx,aforosed(jsedq).utmy,  &
	                      aforosed(jsedq).elev
      !Guiomar (14/04/2014): Cambio para resolver error de localización de los puntos
      !aforosed(jsedq).fila=IFIX((aforosed(jsedq).utmx-cw-(dx/2.0))/dx)+1
      !aforosed(jsedq).columna=IFIX((cn-aforosed(jsedq).utmy-(dy/2.0))/dy)+1
      aforosed(jsedq).fila=IFIX((aforosed(jsedq).utmx-cw)/dx)+1
      aforosed(jsedq).columna=IFIX((cn-aforosed(jsedq).utmy)/dy)+1
     ELSE
      jsedq=0
      ksedq=0
     ENDIF
    CASE ('W ')
     IF (config(5)) THEN
      jw=jw+1
      BACKSPACE (12)
	  ALLOCATE(veg(jw).obs(0:nt),veg(jw).sim(0:nt))
	  veg(jw).obs=0.0
	  veg(jw).sim=0.0
      READ (12,*,err=224) veg(jw).codigo,veg(jw).name,veg(jw).utmx,veg(jw).utmy,  &
	                      veg(jw).elev
      DO i=1,ncon
          IF(veg(jw).name.eq.control(i).name) THEN
              veg(jw).fila=control(i).fila
              veg(jw).columna=control(i).columna
          ENDIF
      ENDDO
     ELSE
      jw=0
      nveg=0
     ENDIF
    CASE ('R ')
     IF (config(5)) THEN
      jr=jr+1
      BACKSPACE (12)
	  ALLOCATE(radiacion(jr).obs(0:nt))
	  radiacion(jr).obs=0.0
      READ (12,*,err=225) radiacion(jr).codigo,radiacion(jr).name,radiacion(jr).utmx,radiacion(jr).utmy,  &
	                      radiacion(jr).elev
      radiacion(jr).fila=IFIX((radiacion(jr).utmx-cw-(dx/2.0))/dx)+1
      radiacion(jr).columna=IFIX((cn-radiacion(jr).utmy-(dy/2.0))/dy)+1
     ELSE
      jr=0
      nradiacion=0
     ENDIF
    CASE ('DA ') ! (Cris 11/2015)
    IF (config(4)) THEN
      jsedadi1=jsedadi1+1
      BACKSPACE (12)
	  ALLOCATE(aportsed1(jsedadi1).obs(0:nt),aportsed1(jsedadi1).sim(0:nt))
	  aportsed1(jsedadi1).obs=0.0
      aportsed1(jsedadi1).sim=0.0
      READ (12,*,err=230) aportsed1(jsedadi1).codigo,aportsed1(jsedadi1).name,aportsed1(jsedadi1).utmx,  &
	                      aportsed1(jsedadi1).utmy,aportsed1(jsedadi1).elev
	  aportsed1(jsedadi1).fila=IFIX((aportsed1(jsedadi1).utmx-cw)/dx)+1
      aportsed1(jsedadi1).columna=IFIX((cn-aportsed1(jsedadi1).utmy)/dy)+1
    ELSE
      jsedadi1=0
      kadised1=0
	ENDIF  
	CASE ('DL ') ! (Cris 11/2015)
    IF (config(4)) THEN
      jsedadi2=jsedadi2+1
      BACKSPACE (12)
	  ALLOCATE(aportsed2(jsedadi2).obs(0:nt),aportsed2(jsedadi2).sim(0:nt))
	  aportsed2(jsedadi2).obs=0.0
      aportsed2(jsedadi2).sim=0.0
      READ (12,*,err=231) aportsed2(jsedadi2).codigo,aportsed2(jsedadi2).name,aportsed2(jsedadi2).utmx,  &
	                      aportsed2(jsedadi2).utmy,aportsed2(jsedadi2).elev
	  aportsed2(jsedadi2).fila=IFIX((aportsed2(jsedadi2).utmx-cw)/dx)+1
      aportsed2(jsedadi2).columna=IFIX((cn-aportsed2(jsedadi2).utmy)/dy)+1
    ELSE
      jsedadi2=0
      kadised2=0
	ENDIF  
	CASE ('DC ') ! (Cris 11/2015)
    IF (config(4)) THEN
      jsedadi3=jsedadi3+1
      BACKSPACE (12)
	  ALLOCATE(aportsed3(jsedadi3).obs(0:nt),aportsed3(jsedadi3).sim(0:nt))
	  aportsed3(jsedadi3).obs=0.0
      aportsed3(jsedadi3).sim=0.0
      READ (12,*,err=232) aportsed3(jsedadi3).codigo,aportsed3(jsedadi3).name,aportsed3(jsedadi3).utmx,  &
	                      aportsed3(jsedadi3).utmy,aportsed3(jsedadi3).elev
	  aportsed3(jsedadi3).fila=IFIX((aportsed3(jsedadi3).utmx-cw)/dx)+1
      aportsed3(jsedadi3).columna=IFIX((cn-aportsed3(jsedadi3).utmy)/dy)+1
    ELSE
      jsedadi3=0
      kadised3=0
    ENDIF
    CASE ('NO ') ! (Cris 12/2017)
    IF (modulos2(3)) THEN
      jno=jno+1
      BACKSPACE (12)
	  ALLOCATE(norg(jno).obs(0:nt),norg(jno).sim(0:nt))
	  norg(jno).obs=0.0
      norg(jno).sim=0.0
      READ (12,*,err=230) norg(jno).codigo,norg(jno).name,norg(jno).utmx,norg(jno).utmy,norg(jno).elev
	  norg(jno).fila=IFIX((norg(jno).utmx-cw)/dx)+1
      norg(jno).columna=IFIX((cn-norg(jno).utmy)/dy)+1
    ELSE
      jno=0
      kno=0
    ENDIF
    CASE ('AM ') ! (Cris 12/2017)
    IF (modulos2(3)) THEN
      jam=jam+1
      BACKSPACE (12)
	  ALLOCATE(amonio(jam).obs(0:nt),amonio(jam).sim(0:nt))
	  amonio(jam).obs=0.0
      amonio(jam).sim=0.0
      READ (12,*,err=230) amonio(jam).codigo,amonio(jam).name,amonio(jam).utmx,amonio(jam).utmy,amonio(jam).elev
	  amonio(jam).fila=IFIX((amonio(jam).utmx-cw)/dx)+1
      amonio(jam).columna=IFIX((cn-amonio(jam).utmy)/dy)+1
    ELSE
      jam=0
      kam=0
    ENDIF
    CASE ('NI ') ! (Cris 12/2017)
    IF (modulos2(3)) THEN
      jni=jni+1
      BACKSPACE (12)
	  ALLOCATE(nitrato(jni).obs(0:nt),nitrato(jni).sim(0:nt))
	  nitrato(jni).obs=0.0
      nitrato(jni).sim=0.0
      READ (12,*,err=230) nitrato(jni).codigo,nitrato(jni).name,nitrato(jni).utmx,nitrato(jni).utmy,nitrato(jni).elev
	  nitrato(jni).fila=IFIX((nitrato(jni).utmx-cw)/dx)+1
      nitrato(jni).columna=IFIX((cn-nitrato(jni).utmy)/dy)+1
    ELSE
      jni=0
      kni=0
    ENDIF
    
  END SELECT
ENDDO
10 FORMAT(A2)

REWIND(12)
ios=0
DO WHILE (ios.ne.-1)
  aa='-*'
  READ (12,10,iostat=ios) aa
  SELECT CASE (aa)
    CASE('  ')
	  BACKSPACE(12)
	  t=0
      DO t=1,nt  
		!El fichero de columna debe tener el orden P,N,V,S,Q,B,H,T,E,D,X,W,R,DA,DL,DC
		READ(12,*,err=226,end=299) orig,(pluvio(i).obs(t),i=1,kppt),(nivel(i).obs(t),i=1,nemb),  &
           (volum(i).obs(t),i=nemb+1,nemb+vnemb),(qemb(i).obs(t),i=nemb+vnemb+1,nemb+vnemb+knemb),       &
           	(aforo(i).obs(t),i=1,naf),(orig,i=1,knaf),(nieve(i).obs(1),i=1,kniv),  &
			(temper(i).obs(t),i=1,ktem),(evapo(i).obs(t),i=1,kevp),        &
			(aport(i).obs(t),i=1,kadi),(aforosed(i).obs(t),i=1,ksedq),(veg(i).obs(t),i=1,nveg),(radiacion(i).obs(t),i=1,nradiacion),    &
			(aportsed1(i).obs(t),i=1,kadised1),(aportsed2(i).obs(t),i=1,kadised2),(aportsed3(i).obs(t),i=1,kadised3), &  ! DA,DL,DC Cris (11/2015)
            (norg(i).obs(t),i=1,kno),(amonio(i).obs(t),i=1,kam),(nitrato(i).obs(t),i=1,kni)
        GO TO 1001
226     mensaje=strings(53)
        errr=1
        CALL errores
        GO TO 94
1001	ENDDO
      ios=-1
	  EXIT
  END SELECT
ENDDO
DO i=1,nemb+vnemb+knemb
  nivel(i).obs(0)=nivel(i).obs(1)
  volum(i).obs(0)=volum(i).obs(1)
  qemb(i).obs(0)=qemb(i).obs(1)
ENDDO
CLOSE(12)

if(t.lt.nt)then
	mensaje=strings(53)
	errr=1
	CALL errores
	goto 94
endif 

!Pasa valores de Evapo diaria a valores del intervalo
DO k=1,kevp
  DO i=1,nt
    evapo(k).obs(i)=evapo(k).obs(i)*dt/24.
  ENDDO
ENDDO

GOTO 95

212 mensaje=strings(51)
errr=1
CALL errores
GOTO 94
215 mensaje=strings(504)
errr=1
CALL errores
GOTO 94
217 mensaje=strings(505)
errr=1
CALL errores
GOTO 94
218 mensaje=strings(506)
errr=1
CALL errores
GOTO 94
219 mensaje=strings(507)
errr=1
CALL errores
GOTO 94
220 mensaje=strings(508)
errr=1
CALL errores
GOTO 94
221 mensaje=strings(509)
errr=1
CALL errores
GOTO 94
222 mensaje=strings(510)
errr=1
CALL errores
GOTO 94
223 mensaje=strings(511)
errr=1
CALL errores
goto 94 !chiara
!Guiomar (29/01/2014): mensaje de error añadido para la lectura de la vegetación
224 mensaje=strings(515)
errr=1
CALL errores
GOTO 94
225 mensaje=strings(516)
errr=1
CALL errores
goto 94
299 mensaje=strings(54)
errr=1
CALL errores
GOTO 94
229 mensaje=strings(514)!Cris
errr=1
CALL errores
GOTO 94
230 mensaje=strings(517)!Cris
errr=1
CALL errores
GOTO 94
231 mensaje=strings(518)
errr=1
CALL errores
GOTO 94
232 mensaje=strings(519)
errr=1
CALL errores
GOTO 94
94 WRITE(*,*)strings(800)
CLOSE(12)
sale=2

95 END SUBROUTINE


!*******************************************
!* Lectura de datos de embalses  (1ª Parte)
!*******************************************
SUBROUTINE lee_emb1
USE modtet
IMPLICIT NONE

sale=0

!Lee curvas de altura vs volumen para los embalses
DO i=1,nemb+knemb+vnemb
  IF (emb(i).caso.gt.0) THEN
    emb(i).datos=0
    OPEN(11,file=arch(7),status='old',err=233)
    ios=0
    DO WHILE (ios.ne.-1)
      nam='1234567890123456789012345'
      READ (11,*,iostat=ios)nam
      IF (nam.eq.emb(i).nombre) emb(i).datos=emb(i).datos+1
    ENDDO
    CLOSE(11)
  ENDIF
ENDDO

GOTO 95


233 mensaje=strings(71)
errr=1
CALL errores
goto 94

94 WRITE(*,*)strings(800)
sale=2

95 END SUBROUTINE


!*******************************************
!* Lectura de datos de embalses  (2ª Parte)
!*******************************************
SUBROUTINE lee_emb2
USE modtet
IMPLICIT NONE

sale=0
DO i=1,nemb+knemb+vnemb
  IF (emb(i).caso.gt.0) THEN
    emb(i).h=0.0
	emb(i).vol=0.0
	emb(i).sup=0.0
	emb(i).out=0.0
    emb(i).fpul=0.0
	OPEN(11,file=arch(7),status='old',err=233)
    ios=0
    DO WHILE (ios.ne.-1)
      nam='1234567890123456789012345'
      READ (11,*,iostat=ios)nam
      IF (nam.eq.emb(i).nombre) THEN
        BACKSPACE(11)
	    DO j=1,emb(i).datos
          READ(11,*,err=235)nam,fecemb,emb(i).h(j),emb(i).sup(j),emb(i).vol(j),  &
		                    emb(i).out(j,1),emb(i).out(j,2)
		  emb(i).fpul(j)=2*emb(i).vol(j)/dts+emb(i).out(j,2) !!!!agregada por Camilo!!!!!
		  !GIAMBA. enero de 2012
		  !cuando, dado un nivel, el volumen de embalse es menor que el caudal de salida multiplicado por el delta t
		  IF (emb(i).vol(j).lt.emb(i).out(j,2)*dts) THEN
		    emb(i).out(j,2)=(emb(i).vol(j)-0.01)/dts
                IF (lang.eq.2) THEN
                    mensaje='The outlet discharge of reservoir '//TRIM(ADJUSTL(emb(i).nombre))//' is greater than the corresponding volume divided by dt. It has been corrected'
                ELSEIF (lang.eq.1) THEN
                    mensaje='El caudal de salida en la curva del embalse '//TRIM(ADJUSTL(emb(i).nombre))//' es superior al volumen correspondiente dividido por dt. Se ha corregido.'
                ENDIF
            
            errr=2
            CALL errores
		  ENDIF
        ENDDO
      ENDIF
    ENDDO
    CLOSE(11)
  ENDIF
ENDDO

GOTO 95

233 mensaje=strings(71)
errr=1
CALL errores
GOTO 94
234 mensaje=strings(72)
errr=1
CALL errores
GOTO 94
235 mensaje=strings(73)
errr=1
CALL errores
goto 94 !chiara

94 WRITE(*,*)strings(800)
sale=2

95 END SUBROUTINE


!************************************************
!* Lectura de ficheros en formato GRASS (*.dtm)
!************************************************
SUBROUTINE leemapaij(artem,h1,h2,h3,h4,h5,h6,h7,h8,np,nz,mi,mj,ce,cn,cs,cw,matriz,sale)
IMPLICIT NONE

INTEGER mi,mj,i,j,matriz(mi,mj),sale,np,nz,errr
REAL cn,ce,cs,cw
CHARACTER artem*128,mensaje*200
CHARACTER*8 h1,h2,h3,h4,h5,h6,h7,h8

sale=0
OPEN(10,file=artem,status='old',err=210)
READ (10,*,err=211)h1,np,h2,nz,h3,cn,h4,cs,h5,ce,h6,cw,h7,mi,h8,mj
READ (10,*,err=212) ((matriz(i,j),i=1,mi),j=1,mj)
CLOSE (10)  

GOTO 95

210 mensaje='No existe el fichero '//TRIM(ADJUSTL(artem))
errr=1
CALL errores
GOTO 94
211 mensaje='Lectura de datos errónea en el encabezado de '//TRIM(ADJUSTL(artem))
errr=1
CALL errores
GOTO 94
212 mensaje='Lectura de datos errónea en '//TRIM(ADJUSTL(artem))
errr=1
CALL errores
goto 94 !chiara

94 WRITE(*,*)'Se han encontrado ERRORES !!! '
sale=2

95 END SUBROUTINE



!************************************************
!* Lectura de ficheros en formato ASCII (*.ASC)
!************************************************
SUBROUTINE leeficASCi(masc_fic)
USE modtet
IMPLICIT NONE

INTEGER mi1,mj1
REAL dx1,cs1,cw1,negat,masc_fic(mi,mj)

sale=0
!Lee fichero ASC con las altura de nieve en (mm)
OPEN(17,file=artem,status='old',err=211)
READ(17,*,err=212)tit(1),mi1
READ(17,*,err=212)tit(2),mj1
READ(17,*,err=212)tit(3),cw1
READ(17,*,err=212)tit(4),cs1
READ(17,*,err=212)tit(5),dx1
READ(17,*,err=212)tit(6),negat

IF (mi1.ne.mi.OR.mj1.ne.mj) THEN
    IF (lang.eq.1) THEN
        mensaje='No coinciden el numero de filas/columnas en los ficheros'//TRIM(ADJUSTL(artem))
    ELSE IF (lang.eq.2) THEN
        mensaje='The number of rows/columns does not match in files '//TRIM(ADJUSTL(artem))    
    ENDIF
    errr=1
    CALL errores
    GOTO 94
ENDIF
IF (dx1.ne.dx) THEN
    IF (lang.eq.1) THEN
        mensaje='No coincide el tamaño de la celda en'//TRIM(ADJUSTL(artem))
    ELSE IF (lang.eq.2) THEN
        mensaje='The cell size does not match in'//TRIM(ADJUSTL(artem))  
    ENDIF
    errr=1
    CALL errores
    GOTO 94
ENDIF
IF (cw1.ne.cw.OR.cs1.ne.cs) THEN
    IF (lang.eq.1) THEN
        mensaje='No coinciden las coordenadas en los ficheros '//TRIM(ADJUSTL(artem))
    ELSE IF (lang.eq.2) THEN
        mensaje='The coordinates do not match in files '//TRIM(ADJUSTL(artem))
    ENDIF
    errr=1
    CALL errores
    GOTO 94
ENDIF

DO j=1,mj1
  READ(17,*,err=213)(masc_fic(i,j),i=1,mi1)
ENDDO
CLOSE(17)

GOTO 95

211 IF (lang.eq.1) THEN
        mensaje='No existe el fichero '//TRIM(ADJUSTL(artem))
    ELSE IF (lang.eq.2) THEN
        mensaje='The following file is missing: '//TRIM(ADJUSTL(artem))
    ENDIF
    errr=1
CALL errores
GOTO 94
212 IF (lang.eq.1) THEN
        mensaje='Lectura de datos errónea en el encabezado de '//TRIM(ADJUSTL(artem))
    ELSE IF (lang.eq.2) THEN
        mensaje='Errors reading the header of '//TRIM(ADJUSTL(artem))
    ENDIF
    errr=1
CALL errores
GOTO 94
213 IF (lang.eq.1) THEN
        mensaje='Lectura de datos errónea en filas de '//TRIM(ADJUSTL(artem))
    ELSE IF (lang.eq.2) THEN
        mensaje='Error reading values of '//TRIM(ADJUSTL(artem))
    ENDIF
errr=1
CALL errores
goto 94 !chiara

94 IF (lang.eq.1) THEN
        WRITE(*,*)'Se han encontrado ERRORES !!!'
    ELSE IF (lang.eq.2) THEN
        WRITE(*,*)'ERROS were found!!!'
    ENDIF
sale=2

95 END SUBROUTINE

!!************************************************
!!* Lectura de ficheros en formato ASCII (*.ASC)
!!************************************************
!SUBROUTINE leeficASC(masc,artem,mi,mj,cw,cs,dx,sale)
!IMPLICIT NONE
!
!INTEGER i,j,mi,mj,sale,mi1,mj1,errr
!REAL masc(mi,mj),dx1,cs1,cw1,dx,cs,cw,negat
!CHARACTER mensaje*200,artem*128,tit*11(7)
!
!sale=0
!!Lee fichero ASC con las altura de nieve en (mm)
!OPEN(17,file=artem,status='old',err=211)
!READ(17,*,err=212)tit(1),mi1
!READ(17,*,err=212)tit(2),mj1
!READ(17,*,err=212)tit(3),cw1
!READ(17,*,err=212)tit(4),cs1
!READ(17,*,err=212)tit(5),dx1
!READ(17,*,err=212)tit(6),negat
!
!IF (mi1.ne.mi.OR.mj1.ne.mj) THEN
!  mensaje='No coinciden el numero de filas/columnas en los ficheros'//TRIM(ADJUSTL(artem))
!ENDIF
!IF (dx1.ne.dx) THEN
!  mensaje='No coincide el tamaño de la celda'//TRIM(ADJUSTL(artem))
!ENDIF
!IF (cw1.ne.cw.OR.cs1.ne.cs) THEN
!  mensaje='No coinciden las coordenadas en los ficheros'//TRIM(ADJUSTL(artem))
!ENDIF
!
!DO j=1,mj1
!  READ(17,*,err=213)(masc(i,j),i=1,mi1)
!ENDDO
!CLOSE(17)
!
!GOTO 95
!
!211 mensaje='047 No existe el fichero '//TRIM(ADJUSTL(artem))
!errr=1
!CALL errores(errr,mensaje,1)
!GOTO 94
!212 mensaje='048 Lectura de datos errónea en el encabezado de '//TRIM(ADJUSTL(artem))
!errr=1
!CALL errores(errr,mensaje,1)
!GOTO 94
!213 mensaje='049 Lectura de datos errónea en filas de '//TRIM(ADJUSTL(artem))
!errr=1
!CALL errores(errr,mensaje,1)
!goto 94 !ch
!
!94 WRITE(*,*)'Se han encontrado ERRORES !!!'
!sale=2
!
!95 END SUBROUTINE


!!***********************************************
!!* Lectura de VARIABLES AMBIENTALES TIPO DUMMY
!!***********************************************
!SUBROUTINE lee_dummy
!USE modtet
!IMPLICIT NONE
!
!sale=0
!!Lee fichero dummy.txt
!OPEN (9,file=artem,status='old',err=200)
!!Variable principal
!READ (9,*)dirdum
!READ (9,*)fileppal(1)
!READ (9,*)fileppal(2)
!READ (9,*)fileppal(3)
!!Variables Continuas que dependen del MED
!READ (9,*)chkcon(1)
!READ (9,*)chkcon(2)
!READ (9,*)chkcon(3)
!READ (9,*)chkcon(4)
!!Variables Dummy que dependen del MED
!READ(9,*,err=201)chkdum(1),umbdum(1)
!READ(9,*,err=201)filedum(1)
!READ(9,*,err=201)chkdum(2),umbdum(2)
!READ(9,*,err=201)filedum(2)
!READ(9,*,err=201)chkdum(3),umbdum(3),umbdum(4)
!READ(9,*,err=201)filedum(3)
!!Variables dummy (Otras)
!DO i=4,12
!  READ(9,*,err=201)chkdum(i),filedum(i)
!ENDDO
!CLOSE(9)
!
!GOTO 95
!
!200 mensaje='1501 El fichero DUMMY.TXT no existe'
!errr=2
!CALL errores(errr,mensaje,lang)
!sale=1
!201 mensaje='1502 El fichero DUMMY.TXT tiene errores'
!errr=2
!CALL errores(errr,mensaje,lang)
!sale=1
!
!95 END SUBROUTINE

!***********************************************
!* Lectura de factoretmes.txt
!***********************************************
SUBROUTINE leefactoret
USE modtet
IMPLICIT NONE
!CALL lee_settings
!CALL lee_pargeo
!Lee fichero con factores de cultivo mensuales para el  calculo de la ETP
k=0
OPEN(10,file=arch(6),status='old',err=248)
READ(10,*)
READ(10,*)
ios=0
DO WHILE (ios.ne.-1)
  READ(10,*,iostat=ios) j
  IF (ios.ne.-1) k=k+1
ENDDO
REWIND(10)
READ(10,*)
READ(10,*)
IF (ALLOCATED(lamb)) DEALLOCATE(lamb)
  ALLOCATE(lamb(k,13))
  lamb=-99
  DO i=1,k
    READ(10,*,err=249)(lamb(i,j),j=1,13)  
  ENDDO
CLOSE(10)
GOTO 111
248 mensaje=strings(61)
errr=1
CALL errores
GOTO 94
249 mensaje=strings(62)
errr=1
CALL errores
GOTO 94

94 WRITE(*,*)strings(800)
111 END SUBROUTINE

!***********************************************
!* Lectura de calib Veg
!***********************************************
SUBROUTINE leecalibveg
USE modtet
IMPLICIT NONE
CALL lee_settings
CALL lee_pargeo
!Lee el fichero con los parámetros de vegetación dinámica
sale=0
k=0
OPEN(10,file=arch(41),status='old',err=248)
ios=0
DO WHILE (ios.ne.-1)
  READ(10,*,iostat=ios) j
  IF (ios.ne.-1) k=k+1
ENDDO
cantUsosSuelosVeg = k !(Vicente. Obtenemos la cantidad de usos de suelo en el fichero CalibVeg.txt)
REWIND(10)
IF (ALLOCATED(lamb)) DEALLOCATE(lamb)
  ALLOCATE(lamb(k,11))
  lamb=-99
  DO i=1,k
    READ(10,*,err=249)(lamb(i,j),j=1,11)
  ENDDO
CLOSE(10)
GOTO 111

248 mensaje=strings(310)
errr=1
CALL errores
GOTO 94
249 mensaje=strings(311)
errr=1
CALL errores
GOTO 94

94 WRITE(*,*)strings(800)
sale=2
111 END SUBROUTINE
!**********************************************************************************
!* Lectura de SETTINGS.TXT
!**********************************************************************************
SUBROUTINE lee_settings
USE modtet
IMPLICIT NONE


INQUIRE (FILE=arch(34),EXIST=existe) 
IF (.NOT.existe) goto 200

!Config (1ª línea)
!7!  1 Formato mapa ArcGis (siempre T)
!1!  2 Formato input columna
!-!  !3 Formato output columna (siempre T)
!2!  4 Sedimentos
!5 Vegetación dinámica
!trapeff Eficiencia retención embalses sedimentos
!simevents Calibración automática multievento
!6 Correción por humedad para el cálculo de la evapotranspiración (y(1))

!Módulos(2ª línea)
!1 Fusión de nieve
!2 Embalses
!3 Recorte cuencas
!4 Diferencia entre cárcavas y cauces
!5 Riego

!Imprimir mapas(3ª línea)
!1 Se imprimen mapas
!2 dt para imprimir los mapas

!Módulos2 (4ª línea)
!1 Nieve con parametrización distribuida
!2 Fenómenos kársticos
!3 Nitrógeno
!4 Cultivos para el sub-modelo de nitrógeno (también funciona sin nitrógeno)
!5 Mapa nitrógeno orgánico (sí T no F)


sale=0
OPEN(55,file=arch(34),status='old',err=2111)
printascii="F"
READ(55,*) lang,typeyear
!READ(55,*,end=2111)(config(i),i=1,5),trapeff,simevents  
READ(55,*,end=2111)(config(i),i=1,7)
    !Reposicionar los valores, para evitar tener que cambiarlo en todo TETIS
    config(6)=config(6)
    simevents=config(5)
    trapeff=config(4)
    config(5)=config(3)
    config(4)=config(2)
    config(3)=.true.
    config(2)=config(1)
    config(1)=config(7)
READ(55,*,end=2112)(modulos(i),i=1,5)
READ(55,*,end=2114)printascii, dtascii
READ(55,*,end=2115)(modulos2(i),i=1,5)
CLOSE(55)

GOTO 95

!si no existe la segunda fila (ni la tercera ni la cuarta)
2112 mensaje=strings(83)
     errr=1
     CALL errores
     modulos(1) = "F"
     modulos(2) = "F"
     modulos(3) = "F"
     modulos(4) = "F"
     modulos(5) = "F"
!si no existe la tercera fila (ni la tercera)
2114 mensaje=strings(84)
     errr=1
     CALL errores
     printascii = "F"     
     dtascii = 10
!si no existe la cuarta fila
2115 mensaje=strings(85)
     errr=1
     CALL errores
     modulos2(1) = "F"
     modulos2(2) = "F"
     modulos2(3) = "F"
     modulos2(4) = "F"
     modulos2(5) = "F"
2113 CLOSE(55)
GOTO 95


200 mensaje=strings(81)
errr=1
CALL errores
GOTO 94

2111 mensaje=strings(82)
errr=1
CALL errores
GOTO 94

94 WRITE(*,*)strings(800)
sale=2

95 END SUBROUTINE


!********************************************
!* Lectura de la fecha de incio desde episodio
!********************************************

SUBROUTINE lee_fechainicio
USE modtet
IMPLICIT NONE

character tdata*100
integer ii, icheck

sale=0

ios=0
!Lee ficheros del evento
OPEN(12,file=arch(5),status='old',err=212)
DO WHILE (ios.ne.-1)
  aa='-*'
  READ (12,'(A2)',iostat=ios) aa
  SELECT CASE (aa)
!Chiara legge data iniziale dal fichero de evento
    CASE ('F ')
      BACKSPACE (12)
      READ (12,'(a100)', err=214) tdata
      icheck=0
      do ii=1,100
        if(tdata(ii:ii).eq.'-'.and.icheck.eq.0)then
            read(tdata(ii-2:ii-1),*)archin(1:2)
            read(tdata(ii+1:ii+2),*)archin(3:4)
            read(tdata(ii+4:ii+7),*)archin(11:14)
            icheck=1
        endif
        if(tdata(ii:ii).eq.':')then
            read(tdata(ii-2:ii-1),*)archin(5:6)
            read(tdata(ii+1:ii+2),*)archin(7:8)
            goto 1
        endif
      enddo
     1 continue
  END SELECT
 ENDDO
REWIND(12)
goto 95

212 mensaje=strings(51)
errr=1
CALL errores
GOTO 94
214 mensaje=strings(512)
errr=1
CALL errores
goto 94 !ch

94 WRITE(*,*)strings(800)
sale=2

95 END SUBROUTINE

!************************************
!* Lectura de UMBRALESQ.TXT
!************************************
SUBROUTINE lee_umbralesQ
USE modtet
IMPLICIT NONE

sale=0

!Lee parametros de calibracion (factores correctores)
OPEN (11,file=artem,status='old',err=223)
DO i=1,2
  READ(11,*,err=224)rango(i)
ENDDO
DO i=1,3
  READ(11,*,err=224)peso(i)
ENDDO
CLOSE(11)

GOTO 95

223 mensaje=strings(105)
errr=1
CALL errores
GOTO 94
224 mensaje=strings(106)
errr=1
CALL errores
GOTO 94

94 WRITE(*,*)strings(800)
sale=2

95 END SUBROUTINE
   
!************************************
!* Lectura de printvariables.TXT
!*Esta subrutinase usa para leer del archivo en el que están puestas las variables de estado a imprimir en formato esri-ascii
!* GIAMBA - agosto 2013
!************************************
SUBROUTINE lee_printvariables
USE modtet
IMPLICIT NONE
character*3 dummy
!comprueba que existe el fichero de variables a imprimir en archivos ASCII
INQUIRE (FILE=TRIM(ADJUSTL(dirtra))//"Printvariables.txt", EXIST = existe)
IF (.NOT.existe) goto 200
!lee el archivo
OPEN(26, file=TRIM(ADJUSTL(dirtra))//"Printvariables.txt")
  DO i=1,53
     READ(26,'(A3,L1)') dummy,variablesascii(i)   
  ENDDO
  DO i=1,9
    READ(26,'(A3,L1)') dummy,finalesascii(i)
  End do
  DO i=1,16
    READ(26,'(A4,L1)') dummy,finalesascii(i+9)
  End do
CLOSE(26)

GO TO 95
200 printascii = .FALSE.
    artem = arch(34)
    CALL escri_settings
95 END SUBROUTINE
