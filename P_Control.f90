!*****************************************************************************
! Ejecutable para recalcular los puntos de control (dentro de la cuenca actual)
!*****************************************************************************

!Se están escribiendo las columnas y las filas cambiadas!!!

!Cris (14/10/2016)
Program P_Control
USE IFPORT 
!USE DFLIB
!USE DFLOGM 
USE modtet
Implicit none

INTEGER mini,minj

CHARACTER text*11,text1*1
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


!***Lectura de la cabecera de Topolco para almacenar en memoria***
!********los datos necesarios para hacer la transformación********
OPEN(14,file=arch(3),status='old',err=209)
READ (14,*,err=210)tit(1),cn,cs
READ (14,*,err=210)tit(2),ce,cw
READ (14,*,err=210)tit(3),mi
READ (14,*,err=210)tit(4),mj
READ (14,*,err=210)tit(5),ncol
READ (14,*,err=210)tit(6),nfil
READ (14,*,err=210)tit(7),ncel
!***Lectura de las dos primeras columnas de Topolco para comprobar que***
!*********los nuevos puntos de control están en la cuenca actual*********
ALLOCATE(cell(ncel+1))
DO n=1,ncel
    READ (14,*,err=211) cell(n).fil,cell(n).col
END DO

!Calcular ancho y área de celdas
dx=(ce-cw)/mi				!ancho en metros
dy=(cn-cs)/mj				!largo en metros

OPEN(202,file=arch(5),status='old')
ncon=0  
ios=0
DO WHILE (ios.ne.-1)
  aa='-*'

  READ (202,100,iostat=ios) aa
  SELECT CASE (aa)
  CASE ('N ','V ','S ','Q ','B ','D ','X ','W ','DA','DL','DC','NO','AM','NI') !Vicente y Cris (12/2017): añado nitrógeno
      ncon=ncon+1
  END SELECT
ENDDO
REWIND(202)
IF (ALLOCATED(control)) DEALLOCATE(control)
ALLOCATE (control(ncon))
IF (ncon.eq.0) THEN
  mensaje=strings(55)
  errr=1
  CALL errores

ENDIF
ios=0
i=0
DO WHILE (ios.ne.-1)
  aa='-*'
  READ (202,100,iostat=ios) aa
  SELECT CASE (aa)
    CASE ('N ','V ','S ','Q ','B ','D ','X ','W ','DA','DL','DC','NO','AM','NI') !Vicente y Cris (12/2017): añado nitrógeno
      BACKSPACE (202)
	  i=i+1
      READ (202,*) aa,control(i).name,control(i).utmx,control(i).utmy
      control(i).fila=INT((control(i).utmx-cw)/dx)+1
      control(i).columna=INT((cn-control(i).utmy)/dy)+1
  END SELECT
ENDDO
100 FORMAT(A2)
CLOSE(202)

!Comprobación de pertenencia a la cuenca
Do i=1,ncon
    Do n=1,ncel
        If (control(i).fila==cell(n).fil) then
           Exit  !El punto pertenece a la cuenca
        Else
            If (n==ncel) then !El punto no pertenece a la cuenca
                errr=1
                mensaje =strings(119)
                CALL errores 
                GOTO 94
           End if
        End if
    End do
End do

    
Do i=1,ncon
    Do n=1,ncel
        If (control(i).columna==cell(n).col) then
           Exit  !El punto pertenece a la cuenca
        Else
           If (n==ncel) then !El punto no pertenece a la cuenca
                errr=1
                mensaje =strings(119)
                CALL errores 
                GOTO 94
           End if
        End if
    End do
End do
         

OPEN(203,file=arch(21),status='replace')
DO i=1,ncon
  WRITE(203,101) control(i).fila,control(i).columna,control(i).name
ENDDO
CLOSE(203)
101 FORMAT(I5,2X,I5,8X,A25)   !Formato fijo para los puntos de control

GOTO 95

209 mensaje=strings(31)
errr=1
CALL errores
GOTO 94
210 mensaje=strings(32)
errr=1
CALL errores
GOTO 94
211 mensaje=strings(33)
CALL errores
GOTO 94

94 WRITE(*,*)strings(800)

95 CALL libera_mem

if(stma_op==0) then
    !copyFicheros = 'cp ' // TRIM(ADJUSTL(dirtra))//TRIM(ADJUSTL('~errores.txt')) // ' ' // TRIM(ADJUSTL(dirtra))//TRIM(ADJUSTL('errores.txt')) !Linux
    copyFicheros = '[ -f ' // TRIM(ADJUSTL(dirtra))//TRIM(ADJUSTL('~errores.txt')) //' ] && cp ' // TRIM(ADJUSTL(dirtra))//TRIM(ADJUSTL('~errores.txt')) // ' ' // TRIM(ADJUSTL(dirtra))//TRIM(ADJUSTL('errores.txt')) !Linux
else
    copyFicheros = 'copy "' // TRIM(ADJUSTL(dirtra))//TRIM(ADJUSTL('~errores.txt')) // '" "' // TRIM(ADJUSTL(dirtra))//TRIM(ADJUSTL('errores.txt')) // '" /Y' !Windows
end if
res = SYSTEM (copyFicheros)
CALL deletefich(TRIM(ADJUSTL(dirtra))//TRIM(ADJUSTL('~errores.txt')))   
CALL write_date


END PROGRAM