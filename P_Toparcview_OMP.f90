! **********************************************************************
! * Genera la Topología según mapas originales (direccion de flujo, MED,
! * pendientes, acumulados, Cobeveg, Hu, Ks y Kp) en formato ASC de
! * ARC/INFO.
! * GENERACION DE TOPOLOGIA PARALELIZADA (se emplea la libre OMP)
! **********************************************************************

program toparcview
USE IFPORT 
!USE DFLIB
!USE DFLOGM 
USE modtet

IMPLICIT NONE

LOGICAL(KIND=4)CHECKED 
INTEGER ip,flag1,length,busca
INTEGER mini,minj,nmi,nmj,cel,con2
integer massimocodveg
INTEGER, ALLOCATABLE:: ndest(:),mdir(:,:),maskint(:,:)
REAL, ALLOCATABLE:: matriz(:,:),mask(:,:),aux(:,:)
REAL, ALLOCATABLE:: aux0(:)
LOGICAL evapot_1
! Asignación de parámetros del suelo para cuantificar la erosión:
! Cusle : valor del factor C de la USLE, multiplicado por 1000
! Kusle : valor del factor K de la USLE, multiplicado por 1000
! Pusle : valor del factor P de la USLE, multiplicado por 1000
! porcentaje(1): porcentaje de arena (sand), entre 0 y 100
! porcentaje(2): porcentaje de limo (silt), entre 0 y 100
! porcentaje(3): porcentaje de arcilla (clay), entre 0 y 100
REAL cn2,cs2,ce2,cw2
REAL, ALLOCATABLE:: mdir2(:,:)
TYPE(celdas), ALLOCATABLE :: t_cell(:)
INTEGER,ALLOCATABLE:: fils(:)
TYPE (fil__cols), ALLOCATABLE:: fil_cols(:),t_fil_cols(:)
INTEGER posK,posN
CHARACTER text*11,text1*1
!INTEGER n_acum(10)
INTEGER n_acum(47)
INTEGER pos_array_acum,max_acum,pos_max_acum
LOGICAL is_max_acum

INTEGER nargu,ist
CHARACTER*30 arg
character copyFicheros*256

REAL hu

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
!Subtituimos la rutina de eliminar fichero para no emplear librerias
CALL deletefich(TRIM(ADJUSTL(dirtra))//TRIM(ADJUSTL('errores.txt')))

artem=TRIM(ADJUSTL(dirtra))//TRIM(ADJUSTL('~errores.txt'))  
OPEN(22,file=artem)                                        
WRITE(22,'(A54)') strings(802)
CLOSE(22) 

If (config(5)) CALL leearchveg(dirtra,archveg,sale)

IF (sale.eq.2) GOTO 94
!Lee ficheros de sedimentos
IF (config(4)) THEN
    CALL leearchsed(dirtra,archsed,sale)
    IF (sale.eq.2) GOTO 94
ENDIF

!Lee ficheros del submodelo de nitrógeno Cris(03/2017)
IF (modulos2(3)) THEN
    CALL leearchnit(dirtra,archnit,sale)
    IF (sale.eq.2) GOTO 94
ENDIF


!Lee mapa de topografia para obtener coordenadas
artem=arch(12)
OPEN (11,file=artem)
READ (11,*) text,mi
READ (11,*) text,mj
READ (11,*) text,cw
READ (11,*) text,cs
READ (11,*) text,dx
CLOSE(11)
dy=dx
ce=cw+dx*mi
cn=cs+dy*mj
ALLOCATE(mdir(mi,mj),mdir2(mi,mj),mask(mi,mj),aux(mi,mj),maskint(mi,mj))
mdir=0
mdir2=0
mask=0
aux=0

! Fichero con la direccion de flujo
artem=arch(18)
CALL leeficASCi(mdir2)
mdir=INT(mdir2)
WRITE(*,*)strings(816)
WRITE(*,*)''

! Genera los puntos de control segun información del episodio

OPEN(12,file=arch(5),status='old',err=212)
ncon=0  
ios=0
DO WHILE (ios.ne.-1)
  aa='-*'

  READ (12,100,iostat=ios) aa
  SELECT CASE (aa)
    CASE ('N ','V ','S ','Q ','B ','D ','X ','W ','DA','DL','DC','NO','AM','NI') !Cris (03/2017): añado nitrógeno
      ncon=ncon+1
  END SELECT
ENDDO
REWIND(12)
IF (ALLOCATED(control)) DEALLOCATE(control)
ALLOCATE (control(ncon))
IF (ncon.eq.0) THEN
  mensaje=strings(55)
  errr=1
  CALL errores
  GOTO 94
ENDIF
ios=0
i=0
DO WHILE (ios.ne.-1)
  aa='-*'
  READ (12,100,iostat=ios) aa
  SELECT CASE (aa)
    CASE ('N ','V ','S ','Q ','B ','D ','X ','W ','DA','DL','DC','NO','AM','NI') 
      BACKSPACE (12)
	  i=i+1
      READ (12,*,err=213) aa,control(i).name,control(i).utmx,control(i).utmy
      control(i).fila=INT((control(i).utmx-cw)/dx)+1
      control(i).columna=INT((cn-control(i).utmy)/dy)+1
  END SELECT
ENDDO
100 FORMAT(A2)
CLOSE(12)

cont=0
aux=0
DO i=1,ncon
  !cont=cont+1
  mask(control(i).fila,control(i).columna)=1
  !aux(control(i).fila,control(i).columna)=cont
ENDDO

WRITE(*,*)strings(817)
!Calcula los puntos de manantiales
INQUIRE (FILE=arch(37),EXIST=existe)
IF (existe)THEN
    OPEN(15,file=arch(37),status='old')
    ios=0
    nman=0
    DO WHILE (ios.ne.-1)
        READ(15,*,iostat=ios) j
        IF (ios.ne.-1) nman=nman+1
    ENDDO
    REWIND(15)
    IF (ALLOCATED(manantial)) DEALLOCATE(manantial)
    ALLOCATE (manantial(nman))
    DO i=1,nman
       READ (15,*,err=213) manantial(i).name,manantial(i).utmx,manantial(i).utmy,manantial(i).coef
       manantial(i).columna=INT((cn-manantial(i).utmy)/dy)+1
       manantial(i).fila=INT((manantial(i).utmx-cw)/dx)+1        
	ENDDO
	CLOSE(15)
ENDIF


flag1=1
!'busca aguas arriba
DO WHILE (flag1.eq.1)
  flag1=0
  !$OMP PARALLEL DO COLLAPSE(2)
  DO i=1,mi !columnas (W-E)
    DO j=1,mj !filas (S-N)
	  IF (mask(i,j).eq.1) THEN
	    mask(i,j)=mask(i,j)+1
        IF (j.gt.1.AND.i.gt.1.) THEN
            IF (mdir(i-1,j-1).eq.2) THEN
		        mask(i-1,j-1)=1
  	            cont=cont+1
		        IF (aux(i-1,j-1).eq.0) THEN
		            aux(i-1,j-1)=cont
		        ELSE
		            cont=cont-1
		        ENDIF
		        flag1=1
		    ENDIF
		ENDIF
		IF (j.gt.1) THEN
		    IF (mdir(i,j-1).eq.4) THEN
		        mask(i,j-1)=1
  	            cont=cont+1
		        IF (aux(i,j-1).eq.0) THEN
		            aux(i,j-1)=cont
		        ELSE
		            cont=cont-1
		        ENDIF
		        flag1=1
		    ENDIF
		ENDIF
		IF (j.gt.1.AND.i.lt.mi) THEN
		    IF (mdir(i+1,j-1).eq.8) THEN
		        mask(i+1,j-1)=1
  	            cont=cont+1
		        IF (aux(i+1,j-1).eq.0) THEN
		            aux(i+1,j-1)=cont
		        ELSE
		            cont=cont-1
		        ENDIF
		        flag1=1
		    ENDIF
		ENDIF
		IF (i.gt.1) THEN
		    IF (mdir(i-1,j).eq.1) THEN
		        mask(i-1,j)=1
  	            cont=cont+1
		        IF (aux(i-1,j).eq.0) THEN
		            aux(i-1,j)=cont
		        ELSE
		            cont=cont-1
		        ENDIF
		        flag1=1
		    ENDIF
		ENDIF
		IF (i.lt.mi) THEN
		    IF (mdir(i+1,j).eq.16) THEN
		        mask(i+1,j)=1
  	            cont=cont+1
		        IF (aux(i+1,j).eq.0) THEN
		            aux(i+1,j)=cont
		        ELSE
		            cont=cont-1
		        ENDIF
		        flag1=1
		    ENDIF
		ENDIF
		IF (j.lt.mj.AND.i.gt.1) THEN
		    IF (mdir(i-1,j+1).eq.128) THEN
		        mask(i-1,j+1)=1
  	            cont=cont+1
		        IF (aux(i-1,j+1).eq.0) THEN
		            aux(i-1,j+1)=cont
		        ELSE
		            cont=cont-1
		        ENDIF
		        flag1=1
		    ENDIF
		ENDIF
		IF (j.lt.mj) THEN
		    IF (mdir(i,j+1).eq.64) THEN
		        mask(i,j+1)=1
  	            cont=cont+1
		        IF (aux(i,j+1).eq.0) THEN
		            aux(i,j+1)=cont
		        ELSE
		            cont=cont-1
		        ENDIF
		        flag1=1
		    ENDIF
		ENDIF
		IF (j.lt.mj.AND.i.lt.mi) THEN
		    IF (mdir(i+1,j+1).eq.32) THEN 
		        mask(i+1,j+1)=1
  	            cont=cont+1
		        IF (aux(i+1,j+1).eq.0) THEN
		            aux(i+1,j+1)=cont
		        ELSE
		            cont=cont-1
		        ENDIF
		            flag1=1
		    ENDIF
		ENDIF
      END IF
    END DO
  END DO
  !$OMP END PARALLEL DO
END DO

ncel=COUNT(mask.gt.0)
maskint=INT(mask)
!IF (mi.LT.600.AND.mj.LT.600) CALL maparaster(mi,mj,maskint,'Cuenca',25) CH
!Lee celdas acumuladas
ALLOCATE(cell(ncel),fil_cols(ncel),t_fil_cols(ncel),fils(ncel))
cell.codnie=0
cell.codrie=0
cell.codveg=1
cell.codpar=1
cell.codcal=1
cell.codkarst=0
!cell.subCue=0 !Se emplea para la subdivision interna por subcuencas

ALLOCATE(matriz(mi,mj))
matriz=0
artem=arch(19)
cont=0
CALL leeficASCi(matriz)
DO i=1,mi
  DO j=1,mj
    IF (mask(i,j).gt.0) THEN
      cont=cont+1
	  cell(cont).fil=i
	  cell(cont).col=j
	  cell(cont).acum=INT(matriz(i,j)+1)
	ENDIF
  ENDDO
ENDDO
!Ordena de mayor a menor las celdas acumuladas
!CALL MergeSort_serial(cell,ncel,t_cell)
CALL MergeSort_serial(cell,ncel)
!DO i=1,ncel
!  DO j=i+1,ncel
!    IF (cell(j).acum.lt.cell(i).acum) THEN
!	  cel=cell(j).acum
!	  cell(j).acum=cell(i).acum
!	  cell(i).acum=cel
!	  cel=cell(j).fil
!	  cell(j).fil=cell(i).fil
!	  cell(i).fil=cel
!	  cel=cell(j).col
!	  cell(j).col=cell(i).col
!	  cell(i).col=cel
!	ENDIF
!  ENDDO
! ENDDO
!$OMP PARALLEL DO
DO i=1,ncel
    fil_cols(i).fil = cell(i).fil
    fil_cols(i).col = cell(i).col
    fil_cols(i).n = i
END DO
!$OMP END PARALLEL DO
!CALL MergeSort_serial_filCol(fil_cols,ncel,t_fil_cols)
CALL MergeSort_serial_filCol(fil_cols,ncel)
!$OMP PARALLEL DO
DO i=1,ncel
    fils(i) = fil_cols(i).fil
END DO
!$OMP END PARALLEL DO

!Lee topografia (cotas)
matriz=0
artem=arch(12)
CALL leeficASCi(matriz)
DO i=1,ncel
  cell(i).cota=INT(matriz(cell(i).fil,cell(i).col))
  If (cell(i).cota==-9999) then
      Go to 215
  End if
ENDDO

!Lee pendiente (y la convierte en 4 por mil los valores cero) (Repo) 4 por mil?
matriz=0.0
artem=arch(17)
CALL leeficASCi(matriz)
DO i=1,ncel
  cell(i).pend=matriz(cell(i).fil,cell(i).col)
  IF (cell(i).pend.le.0.004) cell(i).pend=0.004
ENDDO

!Lee Mapa de Hu
matriz=0
artem=arch(13)
CALL leeficASCi(matriz)
DO i=1,ncel
  cell(i).hu=matriz(cell(i).fil,cell(i).col)
  averagepar(1)=averagepar(1)+cell(i).hu  !para calcular el Hu medio de la cuenca
  If (cell(i).hu==-9999) then
      Go to 215
  End if
ENDDO
averagepar(1)=averagepar(1)/ncel  !valor medio de Hu para la cuenca 

matriz=0
artem=arch(14)
CALL leeficASCi(matriz)
DO i=1,ncel
  cell(i).ks=matriz(cell(i).fil,cell(i).col)
  If (cell(i).ks==-9999) then
      Go to 215
  End if
  averagepar(2)=averagepar(2)+cell(i).ks   !para calcular el ks medio de la cuenca 
ENDDO
averagepar(2)=averagepar(2)/ncel  !para calcular el ks medio de la cuenca

!Lee Mapa de Kp
matriz=0
artem=arch(15)
CALL leeficASCi(matriz)
DO i=1,ncel
  cell(i).kp=matriz(cell(i).fil,cell(i).col)
  If (cell(i).kp==-9999) then
      Go to 215
  End if
  averagepar(3)=averagepar(3)+cell(i).kp
ENDDO
averagepar(3)=averagepar(3)/ncel

!Lee Mapa de Kss
matriz=0
artem=arch(30)
CALL leeficASCi(matriz)
DO i=1,ncel
  cell(i).kss=matriz(cell(i).fil,cell(i).col)
  If (cell(i).kss==-9999) then
      Go to 215
  End if
  averagepar(4)=averagepar(4)+cell(i).kss 
ENDDO
averagepar(4)=averagepar(4)/ncel

!Lee Mapa de Ksa
matriz=0
artem=arch(31)
CALL leeficASCi(matriz)
DO i=1,ncel
  cell(i).ksa=matriz(cell(i).fil,cell(i).col)
  If (cell(i).ksa==-9999) then
      Go to 215
  End if
  averagepar(5)=averagepar(5)+cell(i).ksa
ENDDO
averagepar(5)=averagepar(5)/ncel

!Lee Mapa de Kps
matriz=0
artem=arch(32)
CALL leeficASCi(matriz)
DO i=1,ncel
  cell(i).kps=matriz(cell(i).fil,cell(i).col)
  If (cell(i).kps==-9999) then
      Go to 215
  End if
  averagepar(6)=averagepar(6)+cell(i).kps 
ENDDO
averagepar(6)=averagepar(6)/ncel

!Lee Mapa de velocidades
matriz=0
artem=arch(33)
CALL leeficASCi(matriz)
DO i=1,ncel
  cell(i).veloc=matriz(cell(i).fil,cell(i).col)
  If (cell(i).veloc==-9999) then
      Go to 215
  End if
  IF (cell(i).veloc.eq.0) cell(i).veloc=0.04472
  averagepar(7)=averagepar(7)+cell(i).veloc
ENDDO
averagepar(7)=averagepar(7)/ncel

!!Lee Mapa de acuíferos (Cris 03/2017)
!matriz=0
!artem=arch(39)
!INQUIRE (FILE=artem,EXIST=existe)!añadido porque no está programado el tanque de charcos todavía y por lo tanto no tiene sentido que el mapa de charcos sea obligatorio
!IF (existe) THEN
!    CALL leeficASCi(matriz)
!    DO i=1,ncel
!        cell(i).acuif=matriz(cell(i).fil,cell(i).col)
!    ENDDO
!ELSE
!    cell.acuif=1
!ENDIF

! Mapas de la parte de vegetación
!Lee Mapa hstar, si se va a utilizar, si no se utiliza se anula
If (config(6)) then
    matriz=0
    artem=arch(42)
    CALL leeficASCi(matriz)
    DO i=1,ncel
      cell(i).hstar=(matriz(cell(i).fil,cell(i).col))
      If (cell(i).hstar==-9999) then
          Go to 215
      End if
    ENDDO
Else
    cell.hstar=0.0
End if
!Lee Mapa dc
matriz=0
artem=archveg(2)
INQUIRE (FILE=artem,EXIST=existe)!añadido porque no está programado el tanque de charcos todavía y por lo tanto no tiene sentido que el mapa de charcos sea obligatorio
IF (existe) THEN
CALL leeficASCi(matriz)
end if
DO i=1,ncel
  cell(i).dc=(matriz(cell(i).fil,cell(i).col))
  If (cell(i).dc==-9999) then
      Go to 215
  End if
ENDDO
!Lee Mapa rs
matriz=0
artem=archveg(3)
INQUIRE (FILE=artem,EXIST=existe)!añadido porque esto solo se debería activar con el modulo de vegetación dinámica
if (existe) then
CALL leeficASCi(matriz)
end if
DO i=1,ncel
  cell(i).rs=(matriz(cell(i).fil,cell(i).col))
  If (cell(i).rs==-9999) then
      Go to 215
  End if
ENDDO
!** Fin mapas de la parte de vegetación del modelo antiguo
!Guiomar (27/01/2014): empieza lo nuevo
IF (config(5)) THEN
    !Lee el mapa de hlim1
    matriz=0
    artem=archveg(2)
    CALL leeficASCi(matriz)
    DO i=1,ncel
        cell(i).hlim1=(matriz(cell(i).fil,cell(i).col))
        If (cell(i).hlim1==-9999) then
            Go to 215
        End if
    ENDDO    
    !Lee el mapa de hstar1
    matriz=0
    artem=archveg(3)
    CALL leeficASCi(matriz)
    DO i=1,ncel
        cell(i).hstar1=(matriz(cell(i).fil,cell(i).col))
        If (cell(i).hstar1==-9999) then
            Go to 215
        End if
    ENDDO  
    !Lee el mapa de hlim2
    matriz=0
    artem=archveg(4)
    CALL leeficASCi(matriz)
    DO i=1,ncel
        cell(i).hlim2=(matriz(cell(i).fil,cell(i).col))
        If (cell(i).hlim2==-9999) then
            Go to 215
        End if
    ENDDO   
    !Lee el mapa de hstar2
    matriz=0
    artem=archveg(5)
    CALL leeficASCi(matriz)
    DO i=1,ncel
        cell(i).hstar2=(matriz(cell(i).fil,cell(i).col))
        If (cell(i).hstar2==-9999) then
            Go to 215
        End if
    ENDDO 
    !Lee el mapa de fc
    matriz=0
    artem=archveg(6)
    CALL leeficASCi(matriz)
    DO i=1,ncel
        cell(i).fc=(matriz(cell(i).fil,cell(i).col))
        If (cell(i).fc==-9999) then
            Go to 215
        End if
    ENDDO  
    !Lee el mapa hu1
    matriz=0
    artem=archveg(7)
    CALL leeficASCi(matriz)
    DO i=1,ncel
        cell(i).hu1=(matriz(cell(i).fil,cell(i).col))
        If (cell(i).hu1==-9999) then
            Go to 215
        End if
    ENDDO 
    !Lee el mapa hu2
    matriz=0
    artem=archveg(8)
    CALL leeficASCi(matriz)
    DO i=1,ncel
        cell(i).hu2=(matriz(cell(i).fil,cell(i).col))
        If (cell(i).hu2==-9999) then
            Go to 215
        End if
    ENDDO 
    !Lee el mapa z1
    !matriz=0
    !artem=archveg(9)
    !CALL leeficASCi(matriz)
    !DO i=1,ncel
     !   cell(i).z1=(matriz(cell(i).fil,cell(i).col))
    !ENDDO 
    !Lee el mapa z2
    !matriz=0
    !artem=archveg(10)
    !CALL leeficASCi(matriz)
    !DO i=1,ncel
     !   cell(i).z2=(matriz(cell(i).fil,cell(i).col))
    !ENDDO 
ELSE
    cell.hlim1=0.0
    cell.hstar1=0.0
    cell.hlim2=0.0
    cell.hstar2=0.0
    cell.fc=0.0
    cell.hu1=0.0
    cell.hu2=0.0
    !cell.z1=0.0
    !cell.z2=0.0
ENDIF    
IF (config(4)) THEN
  !Lee Mapa de factor C de la USLE
  matriz=0
  artem=archsed(1)
  CALL leeficASCi(matriz)
  DO i=1,ncel
    cell(i).Cusle=matriz(cell(i).fil,cell(i).col)
    If (cell(i).Cusle==-9999) then
        Go to 215
    End if
  ENDDO

  !Lee Mapa de factor K de la USLE
  matriz=0
  artem=archsed(2)
  CALL leeficASCi(matriz)
  DO i=1,ncel
    cell(i).Kusle=matriz(cell(i).fil,cell(i).col)
    If (cell(i).Kusle==-9999) then
        Go to 215
    End if
  ENDDO

  !Lee Mapa de factor P de la USLE
  matriz=0
  artem=archsed(3)
  CALL leeficASCi(matriz)
  DO i=1,ncel
    cell(i).Pusle=matriz(cell(i).fil,cell(i).col)
    If (cell(i).Pusle==-9999) then
        Go to 215
    End if
  ENDDO
  
  !Lee Mapa de porcentaje de arena de la capa superficial del suelo (entero, entre 0 y 100)
  matriz=0
  artem=archsed(4)
  CALL leeficASCi(matriz)
  DO i=1,ncel
    cell(i).porcentaje(1)=matriz(cell(i).fil,cell(i).col)/100.
    If (cell(i).porcentaje(1)==-9999) then
        Go to 215
    End if
  ENDDO

  !Lee Mapa de porcentaje de limo de la capa superficial del suelo (entero, entre 0 y 100)
  matriz=0
  artem=archsed(5)
  CALL leeficASCi(matriz)
  DO i=1,ncel
    cell(i).porcentaje(2)=matriz(cell(i).fil,cell(i).col)/100.
    If (cell(i).porcentaje(2)==-9999) then
        Go to 215
    End if
  ENDDO

  !Lee Mapa de porcentaje de arcilla de la capa superficial del suelo (entero, entre 0 y 100)
  matriz=0
  artem=archsed(6)
  CALL leeficASCi(matriz)
  DO i=1,ncel
    cell(i).porcentaje(3)=matriz(cell(i).fil,cell(i).col)/100.
    If (cell(i).porcentaje(3)==-9999) then
        Go to 215
    End if
  ENDDO
ELSE
    cell.Kusle=0.0
    cell.Cusle=0.0
    cell.Pusle=0.0
    cell.porcentaje(1)=0
    cell.porcentaje(2)=0
    cell.porcentaje(3)=0
ENDIF

!Lectura de los mapas necesarios para nitrógeno
IF (modulos2(3)) THEN
  !Lee Mapa de hlim (mm)
  matriz=0
  artem=archnit(1)
  CALL leeficASCi(matriz)
  DO i=1,ncel
    cell(i).hlim=matriz(cell(i).fil,cell(i).col)
    If (cell(i).hlim==-9999) then
        Go to 215
    End if
  ENDDO
  !Lee Mapa de profundidad del suelo (m)
  matriz=0
  artem=archnit(2)
  CALL leeficASCi(matriz)
  DO i=1,ncel
    cell(i).psuelo=matriz(cell(i).fil,cell(i).col)
    If (cell(i).psuelo==-9999) then
        Go to 215
    End if
  ENDDO
  !Lee Mapa de densidad aparente para poder tener en cuenta el proceso de adsorción/desorción de NH4 (g/cm3)
  matriz=0
  artem=archnit(3)
  CALL leeficASCi(matriz)
  DO i=1,ncel
    cell(i).daparente=matriz(cell(i).fil,cell(i).col)
    If (cell(i).daparente==-9999) then
        Go to 215
    End if
  ENDDO
  !Lee Mapa de coeficiente de partición/distribución para el proceso de adsorción/desorción de NH4 (dm3/kg)
  matriz=0
  artem=archnit(4)
  CALL leeficASCi(matriz)
  DO i=1,ncel
    cell(i).kd=matriz(cell(i).fil,cell(i).col)
    If (cell(i).kd==-9999) then
        Go to 215
    End if
  ENDDO
  If (modulos2(5)) then
      !Lee Mapa de nitrógeno orgánico (kgN)
      matriz=0
      artem=archnit(5)
      CALL leeficASCi(matriz)
      DO i=1,ncel
        cell(i).hn0ini=matriz(cell(i).fil,cell(i).col)
        If (cell(i).hn0ini==-9999) then
            Go to 215
        End if
      ENDDO
  Else
      cell.hn0ini=0.0
  End if
  !Lee mapa de deposición atmosférica en forma de NH4 (KgN/año)
  matriz=0
  artem=archnit(6)
  CALL leeficASCi(matriz)
  DO i=1,ncel
    cell(i).depamonio=matriz(cell(i).fil,cell(i).col)/365  !Se transforma a día
    If (cell(i).depamonio==-9999) then
        Go to 215
    End if
  ENDDO
  !Lee mapa de deposición atmosférica en forma de NO3 (KgN/año)
  matriz=0
  artem=archnit(7)
  CALL leeficASCi(matriz)
  DO i=1,ncel
    cell(i).depnitrato=matriz(cell(i).fil,cell(i).col)/365  !Se transforma a día
    If (cell(i).depnitrato==-9999) then
        Go to 215
    End if
  ENDDO
Else
    cell.hlim=0.0
    cell.psuelo=0.0
    cell.daparente=0.0
    cell.kd=0.0
    cell.hn0ini=0.0
    cell.depamonio=0.0
    cell.depnitrato=0.0
ENDIF

!Lee Mapa de Índices de Regadio (RIEGO) en caso que exista
IF(modulos(5)) THEN
    matriz=0
    artem=arch(20)
    !INQUIRE (FILE=artem,EXIST=existe)
    !IF (existe) THEN
    CALL leeficASCi(matriz,artem,mi,mj,cw,cs,dx,sale,lang)
    !ENDIF
    DO i=1,ncel
      cell(i).codrie=INT(matriz(cell(i).fil,cell(i).col))
      If (cell(i).codrie==-9999) then
        Go to 215
      End if
    ENDDO
    !Lee Mapa de Orden para el Regadio (ORDEN-RIEGO) en caso que exista
    matriz=0
    artem=arch(29)
    !INQUIRE (FILE=artem,EXIST=existe)
    !IF (existe) THEN
    CALL leeficASCi(matriz,artem,mi,mj,cw,cs,dx,sale,lang)
    !ENDIF
    DO i=1,ncel
      cell(i).ordrie=INT(matriz(cell(i).fil,cell(i).col))
      If (cell(i).ordrie==-9999) then
        Go to 215
      End if
    ENDDO
ELSE
    cell.codrie = 0.0
    cell.ordrie = 0.0
END IF    

!Lee Mapa de Índices de Cobertura Vegetal en caso que exista
matriz=0
artem=arch(16)
evapot_1=.false.
INQUIRE (FILE=artem,EXIST=existe)
IF (existe) THEN
  CALL leeficASCi(matriz)
  DO i=1,ncel
     cell(i).codveg=INT(matriz(cell(i).fil,cell(i).col))
     If (cell(i).codveg==-9999) then
       Go to 215
     End if
    IF (cell(i)%codveg.eq.0) THEN
        cell(i).codveg=1
        evapot_1=.true.
    ENDIF
  ENDDO
ENDIF
IF (evapot_1) THEN
     errr=2
     mensaje=strings(306)
     CALL errores
ENDIF
!Lee Mapa de Índices de Factores de Calibración en caso que exista
matriz=1
artem=arch(28)
INQUIRE (FILE=artem,EXIST=existe)
IF (existe) THEN
  CALL leeficASCi(matriz)
ENDIF
DO i=1,ncel
  cell(i).codcal=INT(matriz(cell(i).fil,cell(i).col))
  If (cell(i).codcal==-9999) then
    Go to 215
  End if
ENDDO

!Lee mapa de regiones homogeneas en caso que exista
matriz=0
artem=arch(24)
INQUIRE (FILE=artem,EXIST=existe)
IF (existe) THEN
  CALL leeficASCi(matriz)
  DO i=1,ncel
    cell(i).codpar=INT(matriz(cell(i).fil,cell(i).col))
    If (cell(i).codpar==-9999) then
      Go to 215
    End if
  ENDDO
ENDIF

!Lee mapas de factor de radiación
IF (modulos2(1)) THEN
  DO k=1,6
    matriz=0
    WRITE(text1,1000)k
    !crea el nombre del fichero
    length=LEN_TRIM(arch(35))
    artem=arch(35)
    artem=artem(1:length-5)//text1//".asc"
    !artem=TRIM(ADJUSTL(archveg(5)))//'0'//text1
    CALL leeficASCi(matriz)
    DO i=1,ncel
	  cell(i).rad(k)=matriz(cell(i).fil,cell(i).col)
      If (cell(i).rad(k)==-9999) then
        Go to 215
      End if
    ENDDO
  ENDDO
ELSE
  DO k=1,6
	  cell.rad(k)=1.0
  ENDDO  
ENDIF


!Lee Mapa de codigos de Karst si existe
IF (modulos2(2)) THEN
    matriz=0
    artem=arch(36)
    INQUIRE (FILE=artem,EXIST=existe)
    IF (existe) THEN
      CALL leeficASCi(matriz)
      DO i=1,ncel
        cell(i).codkarst=INT(matriz(cell(i).fil,cell(i).col))
        If (cell(i).codkarst==-9999) then
          Go to 215
        End if
      ENDDO
    INQUIRE (FILE=arch(37),EXIST=existe)
    IF(.not.existe) THEN
    GOTO 214
    ENDIF
   ENDIF
ELSE
    cell.codkarst=0
ENDIF


DEALLOCATE(matriz)
1000 FORMAT	(I1)

!Calcula la celda destino
cell.dest=0
!$OMP PARALLEL DO COLLAPSE(2)
DO i=1,mi
  DO j=1,mj
    IF (mask(i,j).gt.0) THEN
	  SELECT CASE (mdir(i,j))
	    CASE(1)
          ip=i+1
		  jp=j
	    CASE(2)
          ip=i+1
		  jp=j+1
	    CASE(4)
          ip=i
		  jp=j+1
	    CASE(8)
          ip=i-1
		  jp=j+1
	    CASE(16)
          ip=i-1
		  jp=j
	    CASE(32)
          ip=i-1
		  jp=j-1
	    CASE(64)
          ip=i
		  jp=j-1
	    CASE(128)
          ip=i+1
		  jp=j-1
        END SELECT
      !posN = findloc(fils,ip)
      posN = minloc(abs(fils - ip), 1)
      bucleN: DO n=posN,MIN(posN+mj,ncel)
          IF(fil_cols(n).col.eq.jp.AND.fil_cols(n).fil.eq.ip) THEN
              !posK = FINDLOC(fils,i)
              posK = minloc(abs(fils - i), 1)
              bucleK: DO k=posK,MIN(posK+mj,ncel)
                IF(fil_cols(k).col.eq.j.AND.fil_cols(k).fil.eq.i) THEN  
                    cell(fil_cols(k).n).dest=fil_cols(n).n                    
                    EXIT bucleK                
                END IF  
              END DO bucleK 
              EXIT bucleN          
          END IF    
      END DO bucleN
              
  !    bucleN: DO n=1,ncel
	 !   IF (cell(n).fil.eq.ip.AND.cell(n).col.eq.jp) THEN
	 !     bucleK: DO k=1,ncel
		!    IF (cell(k).fil.eq.i.AND.cell(k).col.eq.j) THEN
		!	  cell(k).dest=n
		!	  EXIT bucleK
		!	ENDIF
		!  ENDDO bucleK
		!  EXIT bucleN	  
		!ENDIF
	 ! ENDDO bucleN
	ENDIF
  ENDDO
ENDDO
!$OMP END PARALLEL DO
!pos_array_acum = 1
!DO i=ncel-1,1,-1
!    max_acum = 0
!    IF(cell(i).dest.ne.i+1) EXIT !son iguales cuando estamos en el flujo principal
!    !IF(pos_array_acum == 11) EXIT
!    IF(pos_array_acum == 48) EXIT
!    
!    DO j=i-2,1,-1 !-2, porque si es destino del siguiente no nos vale
!        !IF(cell(j).dest == i .and. cell(j).acum > ncel/50 .and. cell(j).acum > max_acum) THEN
!        IF(cell(j).dest == i .and. cell(j).acum > 100 .and. cell(j).acum > max_acum) THEN
!            max_acum = cell(j).acum
!            pos_max_acum = j
!        END IF
!    END DO
!    IF (max_acum > 0) THEN
!        is_max_acum = .false.
!        !DO k=1,10
!        DO k=1,47
!            IF(cell(pos_max_acum).dest == n_acum(k))  is_max_acum = .true.
!        END DO
!        if(is_max_acum == .false.) THEN
!            n_acum(pos_array_acum) = pos_max_acum
!            pos_array_acum = pos_array_acum +1
!        end if 
!    END IF
!END DO
!
!numSubcuencas = pos_array_acum-1 !Guardamos el numero de subcuencas obtenido
!DO i=1,numSubcuencas !Recorremos el array con los puntos que marcan el inicio de las subcuencas
!    j = n_acum(i)
!    cell(j).subCue = i 
!    DO k=j,1,-1
!        busca=k
!        IF (cell(k).subCue.eq.i) THEN
!            DO n=k-1,1,-1
!                IF (busca.eq.cell(n).dest) cell(n).subCue = i                    
!            ENDDO
!        ENDIF
!    END DO    
!END DO
!
!maskint=-9999.0
!DO n=1,ncel
!    !mask(cell(n).fil,cell(n).col)=10**cell(n).subCue
!    maskint(cell(n).fil,cell(n).col)=cell(n).subCue
!ENDDO
!artem=TRIM(ADJUSTL(dirtra))//'subcuencas.txt'
!CALL escribascint(maskint)

column=mj
row=mi
!Máximo y minimo de la zona de estudio
nmi=MAXVAL(cell.fil)-MINVAL(cell.fil)+1
nmj=MAXVAL(cell.col)-MINVAL(cell.col)+1
mini=MINVAL(cell.fil)
minj=MINVAL(cell.col)
cn2=cn-dy*(minj-1)
cs2=cn2-nmj*dy   
cw2=cw+(mini-1)*dx   
ce2=cw2+dx*nmi

!Genera el fichero con puntos de control
OPEN(9,file=arch(21),status='replace')
DO i=1,ncon
  control(i).fila=control(i).fila-mini+1
  control(i).columna=control(i).columna-minj+1
  WRITE(9,101) control(i).fila,control(i).columna,control(i).name
ENDDO
CLOSE(9)
101 FORMAT(I5,2X,I5,8X,A25)   !Formato fijo para los puntos de control
    
    
    
!Genera el fichero con puntos de manantial
INQUIRE (FILE=arch(37),EXIST=existe)
IF (existe)THEN
    artem=TRIM(ADJUSTL(dirtra))//'MANANTIAL2.TXT'
        OPEN(13,file=artem,status='replace')
        DO i=1,nman
            manantial(i).fila=manantial(i).fila-mini+1
            manantial(i).columna=manantial(i).columna-minj+1
            WRITE(13,102) manantial(i).fila,manantial(i).columna,manantial(i).name
        ENDDO
        CLOSE(13)
        102 FORMAT(I5,2X,I5,8X,A25)   !Formato fijo para los puntos de control
ENDIF


IF (.NOT.config(5)) THEN
    INQUIRE (FILE=arch(6),EXIST=existe)
    IF (existe)THEN
        OPEN(10,file=arch(6),status='old',err=248)
        READ(10,*)
        READ(10,*)
        ios=0
        k=0
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
    END IF
END IF

Imaxmedio=Imaxmedio/ncel  !cálculo del Imax medio 

!Escribe topologia, edafologia y sedimentos
!IF (config(4)) THEN
!  artem=archsed(7)
!ELSE
artem=arch(3)
!ENDIF


OPEN (18,file=artem)
  IF (lang.eq.1) THEN
        WRITE (18,*)'NORTE-SUR:',cn2,cs2
        WRITE (18,*)'ESTE-OESTE:',ce2,cw2
        WRITE (18,*)'COLUMNAS:',Nmi
        WRITE (18,*)'FILAS:',Nmj
        WRITE (18,*)'COL-FINAL:',cell(ncel).fil-mini+1
        WRITE (18,*)'FIL-FINAL:',cell(ncel).col-minj+1
        WRITE (18,*)'NUM-CELDAS:',ncel
  ELSE IF (lang.eq.2) THEN
        WRITE (18,*)'NORTH-SOUTH:',cn2,cs2
        WRITE (18,*)'EAST-WEST:',ce2,cw2
        WRITE (18,*)'COLUMNS:',Nmi
        WRITE (18,*)'ROWS:',Nmj
        WRITE (18,*)'COL-FINAL:',cell(ncel).fil-mini+1
        WRITE (18,*)'ROW-FINAL:',cell(ncel).col-minj+1
        WRITE (18,*)'CELL-NUM:',ncel
  ENDIF

!Escribe el topolco (a parte la cabecera, que ya está escrita)
DO n=1,ncel                                   
cell(n).fil=cell(n).fil-mini+1
cell(n).col=cell(n).col-minj+1
WRITE (18,108) cell(n).fil,cell(n).col,cell(n).dest,cell(n).acum,cell(n).pend,  &
                cell(n).cota,cell(n).codnie,cell(n).hu,cell(n).ks,cell(n).kp,  &
                cell(n).kss,cell(n).ksa,cell(n).kps,cell(n).veloc,  &
                cell(n).codpar,cell(n).codveg,cell(n).codrie,cell(n).codcal, &
				cell(n).ordrie,cell(n).hstar,cell(n).dc,cell(n).rs, &
				cell(n).rad(1),cell(n).rad(2),cell(n).rad(3),cell(n).rad(4),cell(n).rad(5),cell(n).rad(6),  &
				cell(n).porcentaje(1),cell(n).porcentaje(2),cell(n).porcentaje(3), &
				cell(n).Cusle,cell(n).Kusle,cell(n).Pusle,cell(n).codkarst,cell(n).hlim1,cell(n).hstar1,cell(n).hlim2,cell(n).hstar2,cell(n).fc,cell(n).hu1,cell(n).hu2, &
                cell(n).hlim,cell(n).psuelo,cell(n).daparente,cell(n).kd,cell(n).hn0ini,cell(n).depamonio,cell(n).depnitrato
                !cell(n).hlim,cell(n).psuelo,cell(n).daparente,cell(n).kd,cell(n).hn0ini,cell(n).depamonio,cell(n).depnitrato,cell(n).subCue
ENDDO
WRITE (18,'(A2)') '* '
!WRITE (18,'(A15)') 'Num subcuencas '
!WRITE (18,'(I2)') numSubcuencas
!WRITE (18,'(A2)') '* '
IF (lang.eq.1) THEN
    WRITE (18,'(A47)') '* Valores medios mapas de parámetros utilizados: '
ELSE IF (lang.eq.2) THEN
    WRITE (18,'(A47)') '* Mean values of the used parameter maps:        '
ENDIF
WRITE (18,'(A2)') '* '
WRITE (18,'(A100)') '* ----Hu(mm)-- -Ks(mm/h)-- -Kp(mm/h)-- -Kss(mm/h)- -Ksa(mm/h)- -Kps(mm/h)- --Veloc(m/s)-- --Imax(mm)--'
WRITE (18,'(8(x,F11.4))') (averagepar(j), j=1,7),Imaxmedio

CLOSE(18)

!106 FORMAT(4I9,2I7,I5,3F12.4,4I5,I7,3F8.3,3F8.2)
108 FORMAT(4(x,I8),x,F11.5,x,I6,x,I4,6(x,F14.7),x,F11.5,4(x,I4),x,I6,3(x,F11.4),6(x,F11.5),6(x,F11.3),x,I4,14(x,F11.5),x,I2)
107 FORMAT(4(x,I8),x,F11.5,x,I6,x,I4,6(x,F14.7),x,F11.5,4(x,I4),x,I6,3(x,F11.4),6(x,F11.5))
           
CALL libera_mem
CLOSE(25)    
    
GOTO 95

212 mensaje=strings(51)
errr=1
CALL errores
    
GOTO 94

213 IF (lang.eq.1) THEN
        mensaje='056 Lectura de datos errónea en la línea '//TRIM(ADJUSTL(aa))//' de '//TRIM(ADJUSTL(arch(5)))
    ELSE IF (lang.eq.2) THEN
        mensaje='056 Error reading the line '//TRIM(ADJUSTL(aa))//' of '//TRIM(ADJUSTL(arch(5)))
    ENDIF
    errr=1
CALL errores
GOTO 94

214 mensaje=strings(113)
errr=2
CALL errores
GOTO 94

215 If(lang==1) then
        mensaje='035 Se ha encontrado una celda con valor -9999 en el mapa' //TRIM(ADJUSTL(artem))
    Else
        mensaje='035 A cell with value -9999 has been found in the map' //TRIM(ADJUSTL(artem))
    End if
    errr=1
    CALL errores
GOTO 94

248 mensaje=strings(61)
errr=1
CALL errores
GOTO 94

94 WRITE(*,*)strings(800)

95 CALL libera_mem 
!res=SYSTEM('cp "' // TRIM(ADJUSTL(dirtra))//TRIM(ADJUSTL('~errores.txt')) // '" "' // TRIM(ADJUSTL(dirtra))//TRIM(ADJUSTL('errores.txt')) // '" /Y')
!res=DELFILESQQ(TRIM(ADJUSTL(dirtra))//TRIM(ADJUSTL('~errores.txt')))
!Subtituimos la rutina de eliminar fichero para no emplear librerias   
if(stma_op==0) then
    !copyFicheros = 'cp ' // TRIM(ADJUSTL(dirtra))//TRIM(ADJUSTL('~errores.txt')) // ' ' // TRIM(ADJUSTL(dirtra))//TRIM(ADJUSTL('errores.txt')) !Linux
    copyFicheros = '[ -f ' // TRIM(ADJUSTL(dirtra))//TRIM(ADJUSTL('~errores.txt')) //' ] && cp ' // TRIM(ADJUSTL(dirtra))//TRIM(ADJUSTL('~errores.txt')) // ' ' // TRIM(ADJUSTL(dirtra))//TRIM(ADJUSTL('errores.txt')) !Linux   
else
    copyFicheros = 'copy "' // TRIM(ADJUSTL(dirtra))//TRIM(ADJUSTL('~errores.txt')) // '" "' // TRIM(ADJUSTL(dirtra))//TRIM(ADJUSTL('errores.txt')) // '" /Y' !Windows
end if
res=system (copyFicheros)
CALL deletefich(TRIM(ADJUSTL(dirtra))//TRIM(ADJUSTL('~errores.txt')))
CALL DATE_AND_TIME(dia,hora)
CALL write_date
!RETURN CH

END program
