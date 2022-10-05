! **********************************************************************
! * PROGRAMA DE RECORTE DE AREA AGUAS ARRIBA DE LOS EMBALSES
!*  Ultima actualización: Julio 2016
! **********************************************************************
Program recort_emb
!USE DFLIB
USE modtet
USE IFPORT

IMPLICIT NONE

LOGICAL(KIND=4)CHECKED

INTEGER mi1,mj1,maxitp,minitp,maxjtp,minjtp,nsal,ncon2,nelim,contcell,con2,busca,mc,ip
INTEGER, ALLOCATABLE:: band(:),aux2(:),aux(:),ndest(:),nacum(:)
REAL cc1,cc2
CHARACTER*25,ALLOCATABLE::namelim(:)
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

! Lee el fichero reduce.txt y Cuenta el número de embalses a eliminar
nelim=0
OPEN(19,file=arch(23),status='old',err=216)
ios=0
DO WHILE (ios.ne.-1)
  READ(19,'(A2)',iostat=ios) aa
  IF (ios.ne.-1) nelim=nelim+1
ENDDO
REWIND(19)
ALLOCATE (namelim(nelim))
! Lee puntos a eliminar
DO i=1,nelim
  READ(19,'(A25)',err=217) nam
  namelim(i)=nam
  WRITE(*,*)'Se elimina :',i, nam
ENDDO
CLOSE(19)

! Lee el fichero de puntos de control y Cuenta el número de puntos de control
ncon=0
OPEN(9,file=arch(21),status='old',err=210)
ios=0
DO WHILE (ios.ne.-1)
  READ(9,*,iostat=ios) j
  IF (ios.ne.-1) ncon=ncon+1
ENDDO
REWIND(9)
ALLOCATE (control(ncon),band(ncon))

CALL lee_pcon
IF (sale.eq.2) GOTO 95

!Asigna valor negativo al pto a eliminar
band=9999
nsal=0
DO i=1,ncon
  DO j=1,nelim
    IF (control(i).name.eq.namelim(j)) THEN
	  band(i)=-9999
	  nsal=nsal+1
	ENDIF
  ENDDO
ENDDO

artem=arch(3)

!Lee topologia, propiedades del suelo y tipologia
OPEN(14,file=artem,status='old',err=203)
READ (14,*,err=204)tit(1),cn,cs
READ (14,*,err=205)tit(2),ce,cw
READ (14,*,err=205)tit(3),mi
READ (14,*,err=205)tit(4),mj
READ (14,*,err=205)tit(5),ncol
READ (14,*,err=205)tit(6),nfil
READ (14,*,err=205)tit(7),ncel

ALLOCATE(cell(ncel+1))
ALLOCATE(nacum(ncel),ndest(ncel),aux2(ncel),aux(ncel))



CALL lee_topol

IF (sale.eq.2) GOTO 95
dx=(ce-cw)/mi
dy=(cn-cs)/mj
DO n=1,ncel
  nacum(n)=cell(n).acum
ENDDO
CLOSE(8)

!Inicia rutina de recorte de la cuenca

!Pone aux2=0 a las que estan aguas arriba del que tiene codigo C (band<1)
aux=1
aux2=1
DO i=1,ncon
  DO j=1,ncel
    IF (control(i).fila.eq.cell(j).fil)THEN
      IF (control(i).columna.eq.cell(j).col)THEN    !Encuentra un punto de control
        IF (band(i).lt.0) THEN
		  aux2(j)=0
		  DO k=j,1,-1
            busca=k
            IF (aux2(k).eq.0) THEN
              DO n=k,1,-1
                IF (busca.eq.cell(n).dest) aux2(n)=0
              ENDDO
            ENDIF
          ENDDO
		  aux2(j)=1
		  !Actualiza las celdas acumuladas hacia abajo
		  busca=cell(j).dest   !Inicio
		  con2=nacum(j)
		  DO k=j,ncel
			IF (busca.eq.k) THEN
			  nacum(k)=nacum(k)-con2+1
			  busca=cell(k).dest
			ENDIF
         ENDDO   !fin
		ENDIF
      ENDIF
    ENDIF
  ENDDO
ENDDO

!Elimina celdas innecesaria y ordena topologia
contcell=0
con2=0
ndest=0
DO i=1,ncel    !-1
  aux(i)=aux2(i)*aux(i)
ENDDO

DO i=1,ncel    !-1
  IF (aux(i).eq.1) THEN
    contcell=contcell+1
    cont=0
    DO j=cell(i).dest-1,1,-1
      IF (aux(j).eq.0) THEN
         cont=cont+1
      ENDIF
    ENDDO
    ndest(i)=cell(i).dest-cont
	mc=i
  ENDIF
ENDDO

!Grafica mapa de la cuenca recortada
maxitp=0  
minitp=9999  
maxjtp=0
minjtp=9999
DO n=1,ncel
  IF (aux(n).eq.1) THEN
    IF (cell(n).fil.GT.maxitp) maxitp=cell(n).fil
    IF (cell(n).fil.LT.minitp) minitp=cell(n).fil
    IF (cell(n).col.GT.maxjtp) maxjtp=cell(n).col
    IF (cell(n).col.LT.minjtp) minjtp=cell(n).col
  ENDIF
ENDDO
mi1=maxitp-minitp+1
mj1=maxjtp-minjtp+1
ALLOCATE(masc(mi1,mj1))
masc=0
DO n=1,ncel
  IF (aux(n).eq.1) THEN
    ip=cell(n).fil-minitp+1
    jp=cell(n).col-minjtp+1
    masc(ip,jp)=cell(n).codpar
  ENDIF
ENDDO

!Escribe topologia recortada a un fichero
artem=arch(3)
OPEN(10,file=artem) 
cc1=cn-dy*(minjtp-1)
cc2=cc1-dy*mj1
WRITE (10,*)tit(1),cc1,cc2
cc1=cw+dx*(minitp-1)
cc2=cc1+dx*mi1
WRITE (10,*)tit(2),cc2,cc1
WRITE (10,*)tit(3),mi1
WRITE (10,*)tit(4),mj1
WRITE (10,*)tit(5),INT(cell(mc).fil-minitp+1)
WRITE (10,*)tit(6),INT(cell(mc).col-minjtp+1)
WRITE (10,*)tit(7),contcell

cont=0

DO n=1,ncel
IF (aux(n).eq.1) THEN
    cont=cont+1
    ip=INT(cell(n).fil-minitp+1)
    jp=INT(cell(n).col-minjtp+1)                                   
    WRITE (10,108) ip,jp,ndest(n),cell(n).acum,cell(n).pend,  &
                    cell(n).cota,cell(n).codnie,cell(n).hu,cell(n).ks,cell(n).kp,  &
                    cell(n).kss,cell(n).ksa,cell(n).kps,cell(n).veloc,  &
                    cell(n).codpar,cell(n).codveg,cell(n).codrie,cell(n).codcal, &
				    cell(n).ordrie,cell(n).hstar,cell(n).dc,cell(n).rs, &
				    cell(n).rad(1),cell(n).rad(2),cell(n).rad(3),cell(n).rad(4),cell(n).rad(5),cell(n).rad(6),&
				    cell(n).porcentaje(1),cell(n).porcentaje(2),cell(n).porcentaje(3), &
				    cell(n).Cusle,cell(n).Kusle,cell(n).Pusle,cell(n).codkarst,cell(n).hlim1,cell(n).hstar1,cell(n).hlim2,cell(n).hstar2,cell(n).fc,cell(n).hu1,cell(n).hu2,  &
                    cell(n).hlim,cell(n).psuelo,cell(n).daparente,cell(n).kd,cell(n).hn0ini,cell(n).depamonio,cell(n).depnitrato
                    !cell(n).Acuif,cell(n).hlim,cell(n).psuelo,cell(n).daparente,cell(n).kd,cell(n).hn0ini
ENDIF
ENDDO
WRITE (10,'(A2)') '* '
WRITE (10,'(A47)') '* Valores medios mapas de parámetros utilizados: '
WRITE (10,'(A2)') '* '
WRITE (10,'(A100)') '* ----Hu(mm)-- -Ks(mm/h)-- -Kp(mm/h)-- -Kss(mm/h)- -Ksa(mm/h)- -Kps(mm/h)- --Veloc(m/s)-- --Imax(mm)--'
WRITE (10,'(8(x,F11.4))') (averagepar(j), j=1,7),Imaxmedio
CLOSE(10)
!108 FORMAT(4I9,F12.5,I7,I5,6F15.7,F12.5,4I5,I7,3F12.4,6F12.5,6F12.3,I5)
108 FORMAT(4(x,I8),x,F11.5,x,I6,x,I4,6(x,F14.7),x,F11.5,4(x,I4),x,I6,3(x,F11.4),6(x,F11.5),6(x,F11.3),x,I4,14(x,F11.5))
107 FORMAT(4(x,I8),x,F8.5,x,I6,x,I4,6(x,F14.7),x,F8.5,4(x,I4),x,I6,3(x,F8.4),6(x,F9.5))
!Escribe nuevo fichero de puntos de control
OPEN(9,file=arch(21))
DO i=1,ncon
  WRITE(9,101) INT(control(i).fila-minitp+1),INT(control(i).columna-minjtp+1),control(i).name
ENDDO
CLOSE(9)
101 FORMAT(I5,2X,I5,8X,A25)   !Formato fijo para los puntos de control

GOTO 95

203 mensaje=strings(31)
errr=1
CALL errores
GOTO 94
204 mensaje=strings(32)
errr=1
CALL errores
GOTO 94
205 mensaje=strings(33)
errr=1
CALL errores
GOTO 94

210 mensaje=strings(101)
errr=1
CALL errores
GOTO 94

212 mensaje=strings(51)
errr=1
CALL errores
GOTO 94
213 mensaje=strings(502)
errr=1
CALL errores
GOTO 94
214 mensaje=strings(506)
errr=1
CALL errores
GOTO 94
215 mensaje=strings(507)
errr=1
CALL errores
GOTO 94

216 mensaje=strings(920)
errr=1
CALL errores
GOTO 94
217 mensaje=strings(921)
errr=1
CALL errores
GOTO 94

94 WRITE(*,*)strings(800)


95 CALL libera_mem
!res=SYSTEM('cp "' // TRIM(ADJUSTL(dirtra))//TRIM(ADJUSTL('~errores.txt')) // '" "' // TRIM(ADJUSTL(dirtra))//TRIM(ADJUSTL('errores.txt')) // '" /Y')
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

IF(ALLOCATED(band)) DEALLOCATE(band)
IF(ALLOCATED(aux2)) DEALLOCATE(aux2)
IF(ALLOCATED(nacum)) DEALLOCATE(nacum)
IF(ALLOCATED(ndest)) DEALLOCATE(ndest)
IF(ALLOCATED(aux)) DEALLOCATE(aux)
IF(ALLOCATED(namelim)) DEALLOCATE(namelim)

!RETURN

END PROGRAM
