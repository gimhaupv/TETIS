! ************************************************************************
! * Rutina que define el contorno (de nieve) basado en el MDT del terreno
! * coloca codigo 1 a valores superiores a la cota seleccionada
!*  Ultima actualización: Julio 2016
! ************************************************************************
Program defconie
!USE DFLIB
!USE SCIGRAPH   !para ejecutable
USE IFPORT
USE modtet
IMPLICIT NONE


LOGICAL(KIND=4)CHECKED
!TYPE (rccoord) curpos
CHARACTER*30 arg
INTEGER  mia,mja,mi2,mj2,maxx,maxy,ist,nargu
REAL ley
character copyFicheros*256

!**********************************************************************
!para que desde la interfaz se dé como input un argumento
!**********************************************************************
!nargu=nargs()-1  !nargus es una función incluida en la libreria, esto me da el número de argumentos que tiene que leer
!IF (nargu.eq.0) then
!    mensaje="No arguments were founds"
!    errr=1
!    CALL errores(errr,mensaje,2)
!    goto 94
!endif
!CALL getarg(1,arg,ist) !getarg es una función incluidas en las librerias, lee los argumentos, el número 1 significa que lee el primer argumento, en arg guarda el texto del argumento que tiene 'ist' caracteres
!IF (arg(1:3).ne.'-a=') then
!    mensaje="Wrong argument format"
!    errr=1
!    CALL errores(errr,mensaje,2)
!    goto 94
!ENDIF 
!READ (arg(4:ist),*) ley  !en este caso lee el argumento a que es la cota a partir de la cual hay nieve


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

WRITE(*,*)strings(815)

!res=DELFILESQQ(TRIM(ADJUSTL(dirtra))//TRIM(ADJUSTL('errores.txt')))
!Subtituimos la rutina para no emplear librerias(Vicente.Nov-2019)
CALL deletefich(TRIM(ADJUSTL(dirtra))//TRIM(ADJUSTL('errores.txt')))
artem=TRIM(ADJUSTL(dirtra))//TRIM(ADJUSTL('~errores.txt'))  
OPEN(22,file=artem)                                        
WRITE(22,'(A54)') strings(802)
CLOSE(22) 


CALL lee_pargeo
artem=arch(3)

!lee topología (por la unidad 14)
OPEN(14,file=artem,status='old',err=203)
READ (14,*,err=204)tit(1),cn,cs
READ (14,*,err=204)tit(2),ce,cw
READ (14,*,err=204)tit(3),mi
READ (14,*,err=204)tit(4),mj
READ (14,*,err=204)tit(5),ncol
READ (14,*,err=204)tit(6),nfil
READ (14,*,err=204)tit(7),ncel

ALLOCATE(cell(ncel),masc(mi,mj),matrix(mi,mj))


CALL lee_topol

CLOSE(14)

artem=TRIM(ADJUSTL(dirtra))//TRIM(ADJUSTL('defconie.txt'))  
OPEN(25,file=artem,status='unknown',err=206)
READ(25,*)ley
CLOSE(25)

dx=(ce-cw)/mi
dy=(cn-cs)/mj
arcel=dx*dy
      
masc=0
matrix=0
DO n=1,ncel
  masc(cell(n).fil,cell(n).col)=cell(n).cota
END DO

!********************************************************************************************************
!se han comentado las siguientes lineas para obtener el ejecutable sin interfaz gráfica
!*********************************************************************************************************
!CALL maparaster(mi,mj,masc,'Topografía',31)        
!CALL SETTEXTPOSITION(1,1,curpos)                   
!write(31,*)'Presione una celda para ver su cota'   
!*********************************************************************************************************

!write(*,*)'Escribe cota en m'
!read(*,*)ley                     

!Selecciona valores por encima de una cota para considerar el efecto de nieve
!c8='msnm'
!raton=1
!DO WHILE (raton.eq.1)
!65 raton=WAITONMOUSEEVENT(MOUSE$LBUTTONDOWN.or.MOUSE$rBUTTONDOWN,keystate,xmouse,ymouse)

  !IF (mi.gt.400.or.mj.gt.400) THEN
    !mi2=mi
    !mj2=mj
  !ELSE
    !mi2=INT(400.0*mi/mj)
    !mj2=400
 ! ENDIF
  
  !maxy=INT(mj2/mj)
  !maxx=INT(mi2/mi)
  !mia=INT(xmouse/maxx)+1
  !mja=INT(ymouse/maxy)+1
        
 ! IF (mia.gt.mi.or.mja.gt.mj)    GOTO 65 
    !ley=masc(mia,mja)
    !CALL SETTEXTPOSITION(INT2(mj2/16+1),INT2(1),curpos)
    !WRITE (31,101)  mia,mja,ley,c8
!101 FORMAT(' Columna=',I4,2x,'Fila=',I4,4x,'Valor=',f10.3,1x,A8)
!ENDDO
!CLOSE(31)

DO i=1,mi
  DO j=1,mj
    IF (masc(i,j).ge.ley) THEN
      matrix(i,j)=1
    ENDIF
  ENDDO
ENDDO

!CALL maparaster(mi,mj,matrix,'Zona cubierta con nieve',31)   

!!Escribe fichero con contorno de nieve tipo *.ASC
OPEN(10,file=arch(10))
WRITE(10,*)'ncols      ',mi
WRITE(10,*)'nrows      ',mj
WRITE(10,*)'xllcorner  ',cw
WRITE(10,*)'yllcorner  ',cs
WRITE(10,*)'cellsize   ',dx
WRITE(10,*)'NODATA_value  -9999'

DO j=1,mj
  WRITE(10,'(<mi>(I1,x))')(matrix(i,j),i=1,mi)
ENDDO
CLOSE(10)

!Actualiza los valores del código de nieve
DO n=1,ncel
  cell(n).codnie=matrix(cell(n).fil,cell(n).col)
ENDDO

!!Re-escribe el fichero de topología  (Cris 03/2017 se desactiva porque codnie no se está utilizando y no es necesario reescribir topolco)

GOTO 95

203 mensaje=strings(31)
errr=1
CALL errores
GOTO 95

204 mensaje=strings(32)
errr=1
CALL errores
GOTO 95

205 mensaje=strings(33)
errr=1
CALL errores
GOTO 95

206 mensaje=strings(120)
errr=1
CALL errores
GOTO 95

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
   
!RETURN   !para ejecutable
END program 
