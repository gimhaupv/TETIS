!**************************************************************************************
! Subrutinas de la parte de vegetación del modelo
! Febrero 2009
!lee las condiciones iniciales de vegetación (rveg) y de estres hídrico (waterstress)
!
!**************************************************************************************
SUBROUTINE lee_ci_veg
USE modtet
IMPLICIT NONE

sale=0
!
OPEN(15,file=artem,status='old',err=227)
READ (15,*,err=228)c8,cn,cs,c8,ce,cw,c8,mi,c8,mj,c8,i,c8,j,c8,ncel
READ (15,*,err=228)c8,fecin2,horin2
DO n=1,ncel
  cell(n).h(8)=0.0
  cell(n).lai=0.0
  READ (15,*,err=229)cell(n).h(8),cell(n).lai
ENDDO
CLOSE(15)

GOTO 95

227 mensaje=strings(300)
errr=1
CALL errores
GOTO 94
228 mensaje=strings(301)
errr=1
CALL errores
GOTO 94
229 mensaje=strings(302)
errr=1
CALL errores
goto 94 !ch

94 WRITE(*,*)strings(800)
sale=2

95 END SUBROUTINE

!*********************************************************************
!* Escribe las variables rveg y waterstress en el archivo vegantec
!* 
!*********************************************************************
SUBROUTINE escr_ci_veg
!Guiomar (28/01/2014): esta subrutina no es necesaria realmente ya que tendrá que haber un ejecutable para que se genere el vegantec antes de lanzar TETIS.
!No obstante, la voy a modificar para que sirva de ejemplo
USE modtet
IMPLICIT NONE
!
OPEN(16,file=archveg(7)) 

  IF (lang.eq.1) THEN
    WRITE (16,'(A12,3x,2F14.4)')'NORTE-SUR:  ',cn,cs
    WRITE (16,'(A12,3x,2F14.4)')'ESTE-OESTE: ',ce,cw
    WRITE (16,'(A12,3x,I8)')'COLUMNAS:   ',mi
    WRITE (16,'(A12,3x,I8)')'FILAS:      ',mj
    WRITE (16,'(A12,3x,I8)')'COL-FINAL:  ',ncol
    WRITE (16,'(A12,3x,I8)')'FIL-FINAL:  ',nfil
    WRITE (16,'(A12,3x,I8)')'NUM-CELDAS: ',ncel
    WRITE (16,'(A12,A11,x,A8)')'FECHA-HORA: ',fecfin,horfin
  ELSE IF (lang.eq.2) THEN
    WRITE (16,'(A12,3x,2F14.4)')'NORTH-SOUTH:',cn,cs
    WRITE (16,'(A12,3x,2F14.4)')'EAST-WEST:  ',ce,cw
    WRITE (16,'(A12,3x,I8)')'COLUMNS:    ',mi
    WRITE (16,'(A12,3x,I8)')'ROWS:       ',mj
    WRITE (16,'(A12,3x,I8)')'COL-FINAL:  ',ncol
    WRITE (16,'(A12,3x,I8)')'ROW-FINAL:  ',nfil
    WRITE (16,'(A12,3x,I8)')'CELL-NUMBER:',ncel
    WRITE (16,'(A12,A11,x,A8)')'DATE-TIME:  ',fecfin,horfin
  ENDIF

!
!
DO n=1,ncel
  WRITE (16,101)cell(n).h(8),cell(n).lai
ENDDO
101 FORMAT (2(F16.8,x))
CLOSE(16)
END SUBROUTINE


!!*********************************************************************
!!* Escribe las variables rveg y waterstress en el archivo vegantec
!!* 
!!*********************************************************************
!SUBROUTINE civeg
!USE modtet
!IMPLICIT NONE
!
!rini=0.0
!wsini=0.0
!!Lee nombres de los ficheros a utilizar
!CALL lecfiles(dirtra,arch,sale)
!IF (sale.eq.2) GOTO 95
!
!CALL lee_settings
!IF (sale.eq.2) GOTO 95
!
!!Lee parametros geomorfologicos y valores generales de humedad antecedente
!CALL lee_pargeo
!IF (sale.eq.2) GOTO 95
!CALL leearchveg(dirtra,archveg,sale)
!IF (sale.eq.2) GOTO 95
!
!CALL lee_fechainicio !chiara para liberar el nombre del fichero de entrada
!    IF (sale.eq.2) GOTO 95
!    
!!Actualiza la fecha del estado de humedad inicial
!!IF (config(2)) THEN
!  fecfin=archin(1:2)//'/'//archin(3:4)//'/'//archin(11:14)
!  horfin=archin(5:6)//':'//archin(7:8)//':00'                  
!!ELSE
!  !aa1=archin(11:12)
!  !READ(aa1,*)ad
!  !IF (ad.gt.90) THEN
!    !fecfin=archin(1:2)//'/'//archin(3:4)//'/19'//archin(11:14)
!  !ELSE
!    !fecfin=archin(1:2)//'/'//archin(3:4)//'/20'//archin(11:14)
!  !ENDIF
!  !horfin=archin(5:6)//':'//archin(7:8)//':00'
!!ENDIF
!
!artem=arch(3)
!!lee topología (por la unidad 14)
!OPEN(14,file=artem,status='old',err=209)
!READ (14,*,err=210)tit(1),cn,cs
!READ (14,*,err=210)tit(2),ce,cw
!READ (14,*,err=210)tit(3),mi
!READ (14,*,err=210)tit(4),mj
!READ (14,*,err=210)tit(5),ncol
!READ (14,*,err=210)tit(6),nfil
!READ (14,*,err=210)tit(7),ncel
!
!ALLOCATE(cell(ncel))
!
!CALL lee_topol
!
!
!dx=(ce-cw)/mi
!dy=(cn-cs)/mj
!arcel=dx*dy
!arcelkm=dx*dy/1000000.0
!
!!se ha quitado la siguiente linea para obtener el ejecutable de joaquin, este mapa se manejará de la interfaz nueva
!!CALL vegantec(rini,wsini,sale)
!DO n=1,ncel
!  cell(n).rveg=rini
!  cell(n).waterstress=wsini
!END DO
!CALL escr_ci_veg
!IF (sale.eq.2) GOTO 94
!GOTO 95
!
!209 mensaje=strings(51)
!errr=1
!CALL errores(errr,mensaje,lang)
!GOTO 94
!210 mensaje='010 Lectura errónea en la primera linea de '//TRIM(ADJUSTL(arch(5)))
!errr=1
!CALL errores(errr,mensaje,lang)
!GOTO 94
!211 mensaje='011 Lectura de datos errónea en la linea G de '//TRIM(ADJUSTL(arch(5)))
!errr=1
!CALL errores(errr,mensaje,lang)
!GOTO 94 
!
!94 WRITE(*,*)'Se han encontrado ERRORES !!!'
!95 WRITE(*,*)'Fin de la ejecución del módulo vegantec'
!CALL DATE_AND_TIME(dia,hora)
!CALL write_date
!CALL libera_mem
!RETURN
!END SUBROUTINE



!*********************************************************************
!* Esta subrutina es para leer los nombres de los archivos de la
!* parte de vegetación de tetis
!*********************************************************************
SUBROUTINE leearchveg(dirtra_,archveg_,sale_)
USE modtet
IMPLICIT NONE

INTEGER sale_
CHARACTER dirtra_*128,archveg_*128(10)

sale_=0

INQUIRE (FILE='filesveg.txt',EXIST=existe) 
IF (.NOT.existe) goto 200

!Lee nombres de los ficheros a utilizar
OPEN (16,file='filesveg.txt',status='old',err=206)
!Guiomar (29/10/2014): introduzco un if porque en función de si está activa o no la vegetación tendremos más o menos ficheros en filesveg
IF (config(5)) THEN
    DO i=1,7
        READ(16,'(a128)',err=205)archveg_(i+1)
        archveg(i+1)=TRIM(ADJUSTL(dirtra))//TRIM(ADJUSTL(archveg(i+1)))
    ENDDO
!ELSE
!    DO i=1,5
!        READ(16,'(a128)',err=205)archveg_(i)
!        archveg(i)=TRIM(ADJUSTL(dirtra))//TRIM(ADJUSTL(archveg(i)))
!    ENDDO
ENDIF
CLOSE(16)
CALL labels_veg  !llama la subrutina que define los mensajes de error con la ruta de los ficheros de vegetación
GOTO 195

200 mensaje=strings(303)
errr=1
CALL errores
!artem=TRIM(ADJUSTL(dirtra))//'~errores.txt'
!lmensaje=len_trim(mensaje)
!OPEN(8,FILE=artem)
!WRITE(8,'(A54)') strings(802)
!CALL DATE_AND_TIME(dia,hora)
!WRITE(8,*)'* Fecha:'//dia(7:8)//'/'//dia(5:6)//'/'//dia(1:4)//' Hora:'//hora(1:2)//':'//hora(3:4)//':'//hora(5:10)
!WRITE(8,18)errr, mensaje
!CLOSE(8)
!18 FORMAT(I2,3x,a<lmensaje>)
GOTO 194
!
205 mensaje=strings(304)
errr=1
CALL errores
GOTO 194

206 mensaje=strings(305)
errr=1
CALL errores
GOTO 194
!
194 WRITE(*,*)strings(800)
sale_=2
195 END SUBROUTINE 

