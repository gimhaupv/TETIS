!******************************************************************
!* Escribe mapas de variables de estado
!******************************************************************
Subroutine encabezado
!USE DFPORT
!USE DFLIB
Use modtet

Implicit none

IF (mii<=9) then
        WRITE(17,'(A14,I1)')'ncols         ',mii
    ELSE IF (mii<=99) then
        WRITE(17,'(A14,I2)')'ncols         ',mii
    ELSE IF (mii<=999) then
        WRITE(17,'(A14,I3)')'ncols         ',mii
    ELSE IF (mii<=9999) then
        WRITE(17,'(A14,I4)')'ncols         ',mii
    ELSE IF (mii<=99999) then
        WRITE(17,'(A14,I5)')'ncols         ',mii
    ELSE IF (mii<=999999) then
        WRITE(17,'(A14,I6)')'ncols         ',mii
    ELSE IF (mii<=9999999) then
        WRITE(17,'(A14,I7)')'ncols         ',mii
    ELSE
        WRITE(17,'(A14,I8)')'ncols         ',mii
    END IF
    
    IF (mji<=9) then
        WRITE(17,'(A14,I1)')'nrows         ',mji
    ELSE IF (mji<=99) then
        WRITE(17,'(A14,I2)')'nrows         ',mji
    ELSE IF (mji<=999) then
        WRITE(17,'(A14,I3)')'nrows         ',mji
    ELSE IF (mji<=9999) then
        WRITE(17,'(A14,I4)')'nrows         ',mji
    ELSE IF (mji<=99999) then
        WRITE(17,'(A14,I5)')'nrows         ',mji
    ELSE IF (mji<=999999) then
        WRITE(17,'(A14,I6)')'nrows         ',mji
    ELSE IF (mji<=9999999) then
        WRITE(17,'(A14,I7)')'nrows         ',mji
    ELSE
        WRITE(17,'(A14,I8)')'nrows         ',mji
    END IF

    IF (abs(cw)<10.0) then
        WRITE(17,'(A14,F7.5)')'xllcorner     ',cw
    ELSE IF (abs(cw)<100.0) then
        WRITE(17,'(A14,F8.5)')'xllcorner     ',cw
    ELSE IF (abs(cw)<1000.0) then
        WRITE(17,'(A14,F9.5)')'xllcorner     ',cw
    ELSE IF (abs(cw)<10000.0) then
        WRITE(17,'(A14,F10.5)')'xllcorner     ',cw
    ELSE IF (abs(cw)<100000.0) then
        WRITE(17,'(A14,F11.5)')'xllcorner     ',cw
    ELSE IF (abs(cw)<1000000.0) then
        WRITE(17,'(A14,F12.5)')'xllcorner     ',cw
    ELSE IF (abs(cw)<10000000.0) then
        WRITE(17,'(A14,F13.5)')'xllcorner     ',cw
    ELSE
        WRITE(17,'(A14,F14.5)')'xllcorner     ',cw
    END IF
    
    IF (abs(cs)<10.0) then
        WRITE(17,'(A14,F7.5)')'yllcorner     ',cs
    ELSE IF (abs(cs)<100.0) then
        WRITE(17,'(A14,F8.5)')'yllcorner     ',cs
    ELSE IF (abs(cs)<1000.0) then
        WRITE(17,'(A14,F9.5)')'yllcorner     ',cs
    ELSE IF (abs(cs)<10000.0) then
        WRITE(17,'(A14,F10.5)')'yllcorner     ',cs
    ELSE IF (abs(cs)<100000.0) then
        WRITE(17,'(A14,F11.5)')'yllcorner     ',cs
    ELSE IF (abs(cs)<1000000.0) then
        WRITE(17,'(A14,F12.5)')'yllcorner     ',cs
    ELSE IF (abs(cs)<10000000.0) then
        WRITE(17,'(A14,F13.5)')'yllcorner     ',cs
    ELSE
        WRITE(17,'(A14,F14.5)')'yllcorner     ',cs
    END IF

    IF (dx<10.0) then
        WRITE(17,'(A14,F7.5)')'cellsize      ',dx
    ELSE IF (dx<100.0) then
        WRITE(17,'(A14,F8.5)')'cellsize      ',dx
    ELSE IF (dx<1000.0) then
        WRITE(17,'(A14,F9.5)')'cellsize      ',dx
    ELSE IF (dx<10000.0) then
        WRITE(17,'(A14,F10.5)')'cellsize      ',dx
    ELSE IF (dx<100000.0) then
        WRITE(17,'(A14,F11.5)')'cellsize      ',dx
    ELSE IF (dx<1000000.0) then
        WRITE(17,'(A14,F12.5)')'cellsize      ',dx
    ELSE IF (dx<10000000.0) then
        WRITE(17,'(A14,F13.5)')'cellsize      ',dx
    ELSE
        WRITE(17,'(A14,F14.5)')'cellsize      ',dx
    END IF

End subroutine


SUBROUTINE print_ascii
!USE DFPORT 
!USE DFLIB
USE modtet

IMPLICIT NONE

CHARACTER*12 fecha
CHARACTER*500 filename
REAL,ALLOCATABLE:: mask (:,:)
REAL temp

IF (ALLOCATED(mask)) DEALLOCATE(mask)
ALLOCATE(mask(mi,mj))
mii = INT(mi)
mji = INT(mj)

!establece la fecha
!fecin = archin(1:2)//'/'//archin(3:4)//'/'//archin(11:14)
!horin = archin(5:6)//':'//archin(7:8)//':00'
!CALL tposal(fecin,horin,t,dtmin,fecfin,horfin)
!convierte la fecha en cadena de caracteres
!fecha = fecfin(7:10)//fecfin(4:5)//fecfin(1:2)//horfin(1:2)//horfin(4:5)
!WRITE(fecha,'(I4,4I2)')anyoFecha,mesFecha,diaFecha,horaFecha,minFecha
WRITE(fecha,'(I4,4(I2.2))')nyear,nmonth,nday,nhora,nmin

233 FORMAT(<mii>(F12.5,1x))
234 FORMAT(<mii>(ES12.5,1x))

!TEMPERATURA media en el intervalo dtascii - X0 - ºC
IF (variablesascii(1)) THEN
    filename=TRIM(ADJUSTL(dirtra))//'_ASCII'//path_separator//'X0_'//fecha//'.asc'
    filename=TRIM(ADJUSTL(filename))
    mask=-9999.0
    DO n=1,ncel
        mask(cell(n).fil,cell(n).col)=cell(n).xascii(0)/dtascii
    ENDDO
    OPEN(17,file=filename, status='unknown')
    Call encabezado
    WRITE(17,'(A19)')'NODATA_value  -9999'
    DO j=1,mji
        WRITE(17,233) (mask(i,j),i=1,mii)
    ENDDO
    CLOSE(17)
ENDIF

!PRECIPITACION media en el intervalo dtascii - X1 - mm
IF (variablesascii(2)) THEN
    filename=TRIM(ADJUSTL(dirtra))//'_ASCII'//path_separator//'X1_'//fecha//'.asc'
    filename=TRIM(ADJUSTL(filename))
    mask=-9999.0
    DO n=1,ncel
           mask(cell(n).fil,cell(n).col)=cell(n).xascii(1)/dtascii
    ENDDO
    OPEN(17,file=filename, status='unknown')
    Call encabezado
    WRITE(17,'(A19)')'NODATA_value  -9999'
    DO j=1,mji
        WRITE(17,233) (mask(i,j),i=1,mii)
    ENDDO
    CLOSE(17)
ENDIF

!EXCEDENTE medio en el intervalo dtascii - X2 - mm
IF (variablesascii(3)) THEN
    filename=TRIM(ADJUSTL(dirtra))//'_ASCII'//path_separator//'X2_'//fecha//'.asc'
    filename=TRIM(ADJUSTL(filename))
    mask=-9999.0
    DO n=1,ncel
           mask(cell(n).fil,cell(n).col)=cell(n).xascii(2)/dtascii
    ENDDO
    OPEN(17,file=filename, status='unknown')
    Call encabezado
    WRITE(17,'(A19)')'NODATA_value  -9999'
    DO j=1,mji
        WRITE(17,233) (mask(i,j),i=1,mii)
    ENDDO
    CLOSE(17)
ENDIF

!INFILTRACIÓN media en el intervalo dtascii - X3 - mm
IF (variablesascii(4)) THEN
    filename=TRIM(ADJUSTL(dirtra))//'_ASCII'//path_separator//'X3_'//fecha//'.asc'
    filename=TRIM(ADJUSTL(filename))
    mask=-9999.0
    DO n=1,ncel
           mask(cell(n).fil,cell(n).col)=cell(n).xascii(3)/dtascii
    ENDDO
    OPEN(17,file=filename, status='unknown')
    Call encabezado
    WRITE(17,'(A19)')'NODATA_value  -9999'
    DO j=1,mji
        WRITE(17,233) (mask(i,j),i=1,mii)
    ENDDO
    CLOSE(17)
ENDIF

!PERCOLACIÓN media en el intervalo dtascii - X4 - mm
IF (variablesascii(5)) THEN
    filename=TRIM(ADJUSTL(dirtra))//'_ASCII'//path_separator//'X4_'//fecha//'.asc'
    filename=TRIM(ADJUSTL(filename))
    mask=-9999.0
    DO n=1,ncel
           mask(cell(n).fil,cell(n).col)=cell(n).xascii(4)/dtascii
    ENDDO
    OPEN(17,file=filename, status='unknown')
    Call encabezado
    WRITE(17,'(A19)')'NODATA_value  -9999'
    DO j=1,mji
        WRITE(17,233) (mask(i,j),i=1,mii)
    ENDDO
    CLOSE(17)
ENDIF

!PÉRDIDAS medias en el intervalo dtascii - X5 - mm
IF (variablesascii(6)) THEN
    filename=TRIM(ADJUSTL(dirtra))//'_ASCII'//path_separator//'X5_'//fecha//'.asc'
    filename=TRIM(ADJUSTL(filename))
    mask=-9999.0
    DO n=1,ncel
           mask(cell(n).fil,cell(n).col)=cell(n).xascii(5)/dtascii
    ENDDO
    OPEN(17,file=filename, status='unknown')
    Call encabezado
    WRITE(17,'(A19)')'NODATA_value  -9999'
    DO j=1,mji
        WRITE(17,233) (mask(i,j),i=1,mii)
    ENDDO
    CLOSE(17)
ENDIF

!THROUGHFALL medio en el intervalo dtascii - X6 - mm
IF (variablesascii(7)) THEN
    filename=TRIM(ADJUSTL(dirtra))//'_ASCII'//path_separator//'X6_'//fecha//'.asc'
    filename=TRIM(ADJUSTL(filename))
    mask=-9999.0
    DO n=1,ncel
          mask(cell(n).fil,cell(n).col)=cell(n).xascii(6)/dtascii
    ENDDO
    OPEN(17,file=filename, status='unknown')
    Call encabezado
    WRITE(17,'(A19)')'NODATA_value  -9999'
    DO j=1,mji
        WRITE(17,233) (mask(i,j),i=1,mii)
    ENDDO
    CLOSE(17)
ENDIF

!FUSIÓN DE NIEVE media en el intervalo dtascii - Y0 - mm
IF (variablesascii(8)) THEN
    filename=TRIM(ADJUSTL(dirtra))//'_ASCII'//path_separator//'Y0_'//fecha//'.asc'
    filename=TRIM(ADJUSTL(filename))
    mask=-9999.0
    DO n=1,ncel
           mask(cell(n).fil,cell(n).col)=cell(n).yascii(0)/dtascii
    ENDDO
    OPEN(17,file=filename, status='unknown')
    Call encabezado
    WRITE(17,'(A19)')'NODATA_value  -9999'
    DO j=1,mji
        WRITE(17,233) (mask(i,j),i=1,mii)
    ENDDO
    CLOSE(17)
ENDIF

!ET REAL media en el intervalo dtascii - Y1 + Y6 - mm/dia
IF (variablesascii(9)) THEN
    filename=TRIM(ADJUSTL(dirtra))//'_ASCII'//path_separator//'Y1_'//fecha//'.asc'
    filename=TRIM(ADJUSTL(filename))
    mask=-9999.0
    DO n=1,ncel
           mask(cell(n).fil,cell(n).col)=(cell(n).yascii(1)/dtascii + cell(n).yascii(6)/dtascii)*60/dtmin*24
    ENDDO
    OPEN(17,file=filename, status='unknown')
    Call encabezado
    WRITE(17,'(A19)')'NODATA_value  -9999'
    DO j=1,mji
        WRITE(17,233) (mask(i,j),i=1,mii)
    ENDDO
    CLOSE(17)
ENDIF

!ESCORRENTÍA DIRECTA media en el intervalo dtascii - Y2 - mm
IF (variablesascii(10)) THEN
    filename=TRIM(ADJUSTL(dirtra))//'_ASCII'//path_separator//'Y2_'//fecha//'.asc'
    filename=TRIM(ADJUSTL(filename))
    mask=-9999.0
    DO n=1,ncel
           mask(cell(n).fil,cell(n).col)=cell(n).yascii(2)/dtascii
    ENDDO
    OPEN(17,file=filename, status='unknown')
    Call encabezado
    WRITE(17,'(A19)')'NODATA_value  -9999'
    DO j=1,mji
        WRITE(17,233) (mask(i,j),i=1,mii)
    ENDDO
    CLOSE(17)
ENDIF

!INTERFLUJO medio en el intervalo dtascii - Y3 - mm
IF (variablesascii(11)) THEN
    filename=TRIM(ADJUSTL(dirtra))//'_ASCII'//path_separator//'Y3_'//fecha//'.asc'
    filename=TRIM(ADJUSTL(filename))
    mask=-9999.0
    DO n=1,ncel
           mask(cell(n).fil,cell(n).col)=cell(n).yascii(3)/dtascii
    ENDDO
    OPEN(17,file=filename, status='unknown')
    Call encabezado
    WRITE(17,'(A19)')'NODATA_value  -9999'
    DO j=1,mji
        WRITE(17,233) (mask(i,j),i=1,mii)
    ENDDO
    CLOSE(17)
ENDIF

!FLUJO BASE medio en el intervalo dtascii - Y4 - mm
IF (variablesascii(12)) THEN
    filename=TRIM(ADJUSTL(dirtra))//'_ASCII'//path_separator//'Y4_'//fecha//'.asc'
    filename=TRIM(ADJUSTL(filename))
    mask=-9999.0
    DO n=1,ncel
           mask(cell(n).fil,cell(n).col)=cell(n).yascii(4)/dtascii
    ENDDO
    OPEN(17,file=filename, status='unknown')
    Call encabezado
    WRITE(17,'(A19)')'NODATA_value  -9999'
    DO j=1,mji
        WRITE(17,233) (mask(i,j),i=1,mii)
    ENDDO
    CLOSE(17)
ENDIF

!ALMACENAMIENTO DE NIEVE medio en el intervalo dtascii - H0 - mm
IF (variablesascii(15)) THEN
    filename=TRIM(ADJUSTL(dirtra))//'_ASCII'//path_separator//'H0_'//fecha//'.asc'
    filename=TRIM(ADJUSTL(filename))
    mask=-9999.0
    DO n=1,ncel
           mask(cell(n).fil,cell(n).col)=cell(n).hascii(0)/dtascii
    ENDDO
    OPEN(17,file=filename, status='unknown')
    Call encabezado
    WRITE(17,'(A19)')'NODATA_value  -9999'
    DO j=1,mji
        WRITE(17,233) (mask(i,j),i=1,mii)
    ENDDO
    CLOSE(17)
ENDIF

!HUMEDAD DEL SUELO media en el intervalo dtascii - H1 - %
IF (variablesascii(16)) THEN
    filename=TRIM(ADJUSTL(dirtra))//'_ASCII'//path_separator//'H1_'//fecha//'.asc'
    filename=TRIM(ADJUSTL(filename))
    mask=-9999.0
    DO n=1,ncel
       IF (cell(n).u(1).gt.0) THEN
           mask(cell(n).fil,cell(n).col)=(cell(n).hascii(1)/dtascii/(cell(n).u(1)*r(1))*100)
       ELSE
           mask(cell(n).fil,cell(n).col)=100.0
       ENDIF
    ENDDO
    OPEN(17,file=filename, status='unknown')
    Call encabezado
    WRITE(17,'(A19)')'NODATA_value  -9999'
    DO j=1,mji
        WRITE(17,233) (mask(i,j),i=1,mii)
    ENDDO
    CLOSE(17)
ENDIF

!ALMACENAMIENTO SUPERFICIAL medio en el intervalo dtascii - H2 - mm
IF (variablesascii(17)) THEN
    filename=TRIM(ADJUSTL(dirtra))//'_ASCII'//path_separator//'H2_'//fecha//'.asc'
    filename=TRIM(ADJUSTL(filename))
    mask=-9999.0
    DO n=1,ncel
           mask(cell(n).fil,cell(n).col)=cell(n).hascii(2)/dtascii
    ENDDO
    OPEN(17,file=filename, status='unknown')
    Call encabezado
    WRITE(17,'(A19)')'NODATA_value  -9999'
    DO j=1,mji
        WRITE(17,233) (mask(i,j),i=1,mii)
    ENDDO
    CLOSE(17)
ENDIF

!ALMACENAMIENTO GRAVITACIONAL medio en el intervalo dtascii - H3 - mm
IF (variablesascii(18)) THEN
    filename=TRIM(ADJUSTL(dirtra))//'_ASCII'//path_separator//'H3_'//fecha//'.asc'
    filename=TRIM(ADJUSTL(filename))
    mask=-9999.0
    DO n=1,ncel
           mask(cell(n).fil,cell(n).col)=cell(n).hascii(3)/dtascii
    ENDDO
    OPEN(17,file=filename, status='unknown')
    Call encabezado
    WRITE(17,'(A19)')'NODATA_value  -9999'
    DO j=1,mji
        WRITE(17,233) (mask(i,j),i=1,mii)
    ENDDO
    CLOSE(17)
ENDIF

!ALMACENAMIENTO EN EL ACUÍFERO medio en el intervalo dtascii - H4 - mm
IF (variablesascii(19)) THEN
    filename=TRIM(ADJUSTL(dirtra))//'_ASCII'//path_separator//'H4_'//fecha//'.asc'
    filename=TRIM(ADJUSTL(filename))
    mask=-9999.0
    DO n=1,ncel
           mask(cell(n).fil,cell(n).col)=cell(n).hascii(4)/dtascii
    ENDDO
    OPEN(17,file=filename, status='unknown')
    Call encabezado
    WRITE(17,'(A19)')'NODATA_value  -9999'
    DO j=1,mji
        WRITE(17,233) (mask(i,j),i=1,mii)
    ENDDO
    CLOSE(17)
ENDIF

!ALMACENAMIENTO EN CAUCES medio en el intervalo dtascii - H5 - mm
IF (variablesascii(20)) THEN
    filename=TRIM(ADJUSTL(dirtra))//'_ASCII'//path_separator//'H5_'//fecha//'.asc'
    filename=TRIM(ADJUSTL(filename))
    mask=-9999.0
    DO n=1,ncel
           mask(cell(n).fil,cell(n).col)=cell(n).hascii(5)/dtascii
    ENDDO
    OPEN(17,file=filename, status='unknown')
    Call encabezado
    WRITE(17,'(A19)')'NODATA_value  -9999'
    DO j=1,mji
        WRITE(17,233) (mask(i,j),i=1,mii)
    ENDDO
    CLOSE(17)
ENDIF

!INTERCEPCIÓN POR LAS HOJAS media en el intervalo dtascii - H6 - mm
IF (variablesascii(21)) THEN
    filename=TRIM(ADJUSTL(dirtra))//'_ASCII'//path_separator//'H6_'//fecha//'.asc'
    filename=TRIM(ADJUSTL(filename))
    mask=-9999.0
    DO n=1,ncel
           mask(cell(n).fil,cell(n).col)=cell(n).hascii(6)/dtascii
    ENDDO
    OPEN(17,file=filename, status='unknown')
    Call encabezado
    WRITE(17,'(A19)')'NODATA_value  -9999'
    DO j=1,mji
        WRITE(17,233) (mask(i,j),i=1,mii)
    ENDDO
    CLOSE(17)
ENDIF

!GUIOMAR (29/07/2014): Añadiendo variables de sedimentos
!Suspendido en ladera medio en el intervalo dtascii ARENA - SusSedLAD(1) - m3
IF (variablesascii(22)) THEN
    filename=TRIM(ADJUSTL(dirtra))//'_ASCII'//path_separator//'S1_'//fecha//'.asc'
    filename=TRIM(ADJUSTL(filename))
    mask=-9999.0
    DO n=1,ncel
           mask(cell(n).fil,cell(n).col)=cell(n).SusLADascii(1)/dtascii
    ENDDO
    OPEN(17,file=filename, status='unknown')
    Call encabezado
    WRITE(17,'(A19)')'NODATA_value  -9999'
    DO j=1,mji
        WRITE(17,233) (mask(i,j),i=1,mii)
    ENDDO
    CLOSE(17)
ENDIF

!Suspendido en ladera medio en el intervalo dtascii LIMO - SusSedLAD(2) - m3
IF (variablesascii(23)) THEN
    filename=TRIM(ADJUSTL(dirtra))//'_ASCII'//path_separator//'S3_'//fecha//'.asc'
    filename=TRIM(ADJUSTL(filename))
    mask=-9999.0
    DO n=1,ncel
           mask(cell(n).fil,cell(n).col)=cell(n).SusLADascii(2)/dtascii
    ENDDO
    OPEN(17,file=filename, status='unknown')
    Call encabezado
    WRITE(17,'(A19)')'NODATA_value  -9999'
    DO j=1,mji
        WRITE(17,233) (mask(i,j),i=1,mii)
    ENDDO
    CLOSE(17)
ENDIF

!Suspendido en ladera medio en el intervalo dtascii ARCILLA - SusSedLAD(3) - m3
IF (variablesascii(24)) THEN
    filename=TRIM(ADJUSTL(dirtra))//'_ASCII'//path_separator//'S5_'//fecha//'.asc'
    filename=TRIM(ADJUSTL(filename))
    mask=-9999.0
    DO n=1,ncel
           mask(cell(n).fil,cell(n).col)=cell(n).SusLADascii(3)/dtascii
    ENDDO
    OPEN(17,file=filename, status='unknown')
    Call encabezado
    WRITE(17,'(A19)')'NODATA_value  -9999'
    DO j=1,mji
        WRITE(17,233) (mask(i,j),i=1,mii)
    ENDDO
    CLOSE(17)
ENDIF

!Suspendido en red medio en el intervalo dtascii ARENA - SusSedRED(1) - m3
IF (variablesascii(25)) THEN
    filename=TRIM(ADJUSTL(dirtra))//'_ASCII'//path_separator//'S2_'//fecha//'.asc'
    filename=TRIM(ADJUSTL(filename))
    mask=-9999.0
    DO n=1,ncel
           mask(cell(n).fil,cell(n).col)=cell(n).SusREDascii(1)/dtascii
    ENDDO
    OPEN(17,file=filename, status='unknown')
    Call encabezado
    WRITE(17,'(A19)')'NODATA_value  -9999'
    DO j=1,mji
        WRITE(17,233) (mask(i,j),i=1,mii)
    ENDDO
    CLOSE(17)
ENDIF

!Suspendido en red medio en el intervalo dtascii LIMO - SusSedRED(2) - m3
IF (variablesascii(26)) THEN
    filename=TRIM(ADJUSTL(dirtra))//'_ASCII'//path_separator//'S4_'//fecha//'.asc'
    filename=TRIM(ADJUSTL(filename))
    mask=-9999.0
    DO n=1,ncel
           mask(cell(n).fil,cell(n).col)=cell(n).SusREDascii(2)/dtascii
    ENDDO
    OPEN(17,file=filename, status='unknown')
    Call encabezado
    WRITE(17,'(A19)')'NODATA_value  -9999'
    DO j=1,mji
        WRITE(17,233) (mask(i,j),i=1,mii)
    ENDDO
    CLOSE(17)
ENDIF

!Suspendido en red medio en el intervalo dtascii ARCILLA - SusSedRED(3) - m3
IF (variablesascii(27)) THEN
    filename=TRIM(ADJUSTL(dirtra))//'_ASCII'//path_separator//'S6_'//fecha//'.asc'
    filename=TRIM(ADJUSTL(filename))
    mask=-9999.0
    DO n=1,ncel
           mask(cell(n).fil,cell(n).col)=cell(n).SusREDascii(3)
    ENDDO
    OPEN(17,file=filename, status='unknown')
    Call encabezado
    WRITE(17,'(A19)')'NODATA_value  -9999'
    DO j=1,mji
        WRITE(17,233) (mask(i,j),i=1,mii)
    ENDDO
    CLOSE(17)
ENDIF

!Depositado en ladera medio en el intervalo dtascii ARENA - DepSedLAD(1) - m3
IF (variablesascii(28)) THEN
    filename=TRIM(ADJUSTL(dirtra))//'_ASCII'//path_separator//'D1_'//fecha//'.asc'
    filename=TRIM(ADJUSTL(filename))
    mask=-9999.0
    DO n=1,ncel
           mask(cell(n).fil,cell(n).col)=cell(n).DepLADascii(1)/dtascii
    ENDDO
    OPEN(17,file=filename, status='unknown')
    Call encabezado
    WRITE(17,'(A19)')'NODATA_value  -9999'
    DO j=1,mji
        WRITE(17,233) (mask(i,j),i=1,mii)
    ENDDO
    CLOSE(17)
ENDIF

!Depositado en ladera medio en el intervalo dtascii LIMO - DepSedLAD(2) - m3
IF (variablesascii(29)) THEN
    filename=TRIM(ADJUSTL(dirtra))//'_ASCII'//path_separator//'D3_'//fecha//'.asc'
    filename=TRIM(ADJUSTL(filename))
    mask=-9999.0
    DO n=1,ncel
           mask(cell(n).fil,cell(n).col)=cell(n).DepLADascii(2)/dtascii
    ENDDO
    OPEN(17,file=filename, status='unknown')
    Call encabezado
    WRITE(17,'(A19)')'NODATA_value  -9999'
    DO j=1,mji
        WRITE(17,233) (mask(i,j),i=1,mii)
    ENDDO
    CLOSE(17)
ENDIF

!Depositado en ladera medio en el intervalo dtascii ARCILLA - DepSedLAD(3) - m3
IF (variablesascii(30)) THEN
    filename=TRIM(ADJUSTL(dirtra))//'_ASCII'//path_separator//'D5_'//fecha//'.asc'
    filename=TRIM(ADJUSTL(filename))
    mask=-9999.0
    DO n=1,ncel
           mask(cell(n).fil,cell(n).col)=cell(n).DepLADascii(3)/dtascii
    ENDDO
    OPEN(17,file=filename, status='unknown')
    Call encabezado
    WRITE(17,'(A19)')'NODATA_value  -9999'
    DO j=1,mji
        WRITE(17,233) (mask(i,j),i=1,mii)
    ENDDO
    CLOSE(17)
ENDIF

!Depositado en red medio en el intervalo dtascii ARENA - DepSedRED(1) - m3
IF (variablesascii(31)) THEN
    filename=TRIM(ADJUSTL(dirtra))//'_ASCII'//path_separator//'D2_'//fecha//'.asc'
    filename=TRIM(ADJUSTL(filename))
    mask=-9999.0
    DO n=1,ncel
           mask(cell(n).fil,cell(n).col)=cell(n).DepREDascii(1)/dtascii
    ENDDO
    OPEN(17,file=filename, status='unknown')
    Call encabezado
    WRITE(17,'(A19)')'NODATA_value  -9999'
    DO j=1,mji
        WRITE(17,233) (mask(i,j),i=1,mii)
    ENDDO
    CLOSE(17)
ENDIF

!Depositado en red medio en el intervalo dtascii LIMO - DepSedRED(2) - m3
IF (variablesascii(32)) THEN
    filename=TRIM(ADJUSTL(dirtra))//'_ASCII'//path_separator//'D4_'//fecha//'.asc'
    filename=TRIM(ADJUSTL(filename))
    mask=-9999.0
    DO n=1,ncel
           mask(cell(n).fil,cell(n).col)=cell(n).DepREDascii(2)/dtascii
    ENDDO
    OPEN(17,file=filename, status='unknown')
    Call encabezado
    WRITE(17,'(A19)')'NODATA_value  -9999'
    DO j=1,mji
        WRITE(17,233) (mask(i,j),i=1,mii)
    ENDDO
    CLOSE(17)
ENDIF

!Depositado en red medio en el intervalo dtascii ARCILLA - DepSedRED(3) - m3
IF (variablesascii(33)) THEN
    filename=TRIM(ADJUSTL(dirtra))//'_ASCII'//path_separator//'D6_'//fecha//'.asc'
    filename=TRIM(ADJUSTL(filename))
    mask=-9999.0
    DO n=1,ncel
           mask(cell(n).fil,cell(n).col)=cell(n).DepREDascii(3)/dtascii
    ENDDO
    OPEN(17,file=filename, status='unknown')
    Call encabezado
    WRITE(17,'(A19)')'NODATA_value  -9999'
    DO j=1,mji
        WRITE(17,233) (mask(i,j),i=1,mii)
    ENDDO
    CLOSE(17)
ENDIF

!Material parental medio en el intervalo dtascii (erosión media) ARENA - ErodSed(1) - m3
IF (variablesascii(34)) THEN
    filename=TRIM(ADJUSTL(dirtra))//'_ASCII'//path_separator//'P1_'//fecha//'.asc'
    filename=TRIM(ADJUSTL(filename))
    mask=-9999.0
    DO n=1,ncel
           mask(cell(n).fil,cell(n).col)=cell(n).ErodSedascii(1)/dtascii
    ENDDO
    OPEN(17,file=filename, status='unknown')
    Call encabezado
    WRITE(17,'(A19)')'NODATA_value  -9999'
    DO j=1,mji
        WRITE(17,233) (mask(i,j),i=1,mii)
    ENDDO
    CLOSE(17)
ENDIF

!Material parental medio en el intervalo dtascii (erosión media) LIMO - ErodSed(2) - m3
IF (variablesascii(35)) THEN
    filename=TRIM(ADJUSTL(dirtra))//'_ASCII'//path_separator//'P2_'//fecha//'.asc'
    filename=TRIM(ADJUSTL(filename))
    mask=-9999.0
    DO n=1,ncel
           mask(cell(n).fil,cell(n).col)=cell(n).ErodSedascii(2)/dtascii
    ENDDO
    OPEN(17,file=filename, status='unknown')
    Call encabezado
    WRITE(17,'(A19)')'NODATA_value  -9999'
    DO j=1,mji
        WRITE(17,233) (mask(i,j),i=1,mii)
    ENDDO
    CLOSE(17)
ENDIF

!Material parental medio en el intervalo dtascii (erosión media) ARCILLA - ErodSed(3) - m3
IF (variablesascii(36)) THEN
    filename=TRIM(ADJUSTL(dirtra))//'_ASCII'//path_separator//'P3_'//fecha//'.asc'
    filename=TRIM(ADJUSTL(filename))
    mask=-9999.0
    DO n=1,ncel
           mask(cell(n).fil,cell(n).col)=cell(n).ErodSedascii(3)/dtascii
    ENDDO
    OPEN(17,file=filename, status='unknown')
    Call encabezado
    WRITE(17,'(A19)')'NODATA_value  -9999'
    DO j=1,mji
        WRITE(17,233) (mask(i,j),i=1,mii)
    ENDDO
    CLOSE(17)
ENDIF

!Material parental total medio (erosión) en el intervalo dtascii - ErodTotal - m3
IF (variablesascii(37)) THEN
    filename=TRIM(ADJUSTL(dirtra))//'_ASCII'//path_separator//'P4_'//fecha//'.asc'
    filename=TRIM(ADJUSTL(filename))
    mask=-9999.0
    DO n=1,ncel
           mask(cell(n).fil,cell(n).col)=cell(n).ErodTotalascii/dtascii
    ENDDO
    OPEN(17,file=filename, status='unknown')
    Call encabezado
    WRITE(17,'(A19)')'NODATA_value  -9999'
    DO j=1,mji
        WRITE(17,233) (mask(i,j),i=1,mii)
    ENDDO
    CLOSE(17)
ENDIF

!Cris (27/10/2016) Caudal de sedimentos
!Flujo de arena medio en el intervalo dtascii - SedFlujo(1) - (m3/s)
IF (variablesascii(38)) THEN
    filename=TRIM(ADJUSTL(dirtra))//'_ASCII'//path_separator//'FS1_'//fecha//'.asc'
    filename=TRIM(ADJUSTL(filename))
    mask=-9999.0
    DO n=1,ncel
           mask(cell(n).fil,cell(n).col)=cell(n).SedFlujoascii(1)/dtascii
    ENDDO
    OPEN(17,file=filename, status='unknown')
    Call encabezado
    WRITE(17,'(A19)')'NODATA_value  -9999'
    DO j=1,mji
        WRITE(17,233) (mask(i,j),i=1,mii)
    ENDDO
    CLOSE(17)
ENDIF

!Flujo de limo medio en el intervalo dtascii - SedFlujo(2) - (m3/s)
IF (variablesascii(39)) THEN
    filename=TRIM(ADJUSTL(dirtra))//'_ASCII'//path_separator//'FS2_'//fecha//'.asc'
    filename=TRIM(ADJUSTL(filename))
    mask=-9999.0
    DO n=1,ncel
           mask(cell(n).fil,cell(n).col)=cell(n).SedFlujoascii(2)/dtascii
    ENDDO
    OPEN(17,file=filename, status='unknown')
    Call encabezado
    WRITE(17,'(A19)')'NODATA_value  -9999'
    DO j=1,mji
        WRITE(17,233) (mask(i,j),i=1,mii)
    ENDDO
    CLOSE(17)
ENDIF

!Flujo de arcilla medio en el intervalo dtascii - SedFlujo(3) - (m3/s)
IF (variablesascii(40)) THEN
    filename=TRIM(ADJUSTL(dirtra))//'_ASCII'//path_separator//'FS3_'//fecha//'.asc'
    filename=TRIM(ADJUSTL(filename))
    mask=-9999.0
    DO n=1,ncel
           mask(cell(n).fil,cell(n).col)=cell(n).SedFlujoascii(3)/dtascii
    ENDDO
    OPEN(17,file=filename, status='unknown')
    Call encabezado
    WRITE(17,'(A19)')'NODATA_value  -9999'
    DO j=1,mji
        WRITE(17,233) (mask(i,j),i=1,mii)
    ENDDO
    CLOSE(17)
ENDIF

!Flujo de sedimentos medio en el intervalo dtascii - SedFlujoTotal - (m3/s)
IF (variablesascii(41)) THEN
    filename=TRIM(ADJUSTL(dirtra))//'_ASCII'//path_separator//'FS4_'//fecha//'.asc'
    filename=TRIM(ADJUSTL(filename))
    mask=-9999.0
    DO n=1,ncel
           mask(cell(n).fil,cell(n).col)=cell(n).SedFlujoTotalascii/dtascii
    ENDDO
    OPEN(17,file=filename, status='unknown')
    Call encabezado
    WRITE(17,'(A19)')'NODATA_value  -9999'
    DO j=1,mji
        WRITE(17,233) (mask(i,j),i=1,mii)
    ENDDO
    CLOSE(17)
ENDIF

!(Vicente)Mapas de vegetacion

!X8 (Excedence of the shallow soil layer) (mm/deltat)
IF (variablesascii(42)) THEN
    filename=TRIM(ADJUSTL(dirtra))//'_ASCII'//path_separator//'X8_'//fecha//'.asc'
    filename=TRIM(ADJUSTL(filename))
    mask=-9999.0
    DO n=1,ncel
           mask(cell(n).fil,cell(n).col)=cell(n).xascii(8)/dtascii
    ENDDO
    OPEN(17,file=filename, status='unknown')
    Call encabezado
    WRITE(17,'(A19)')'NODATA_value  -9999'
    DO j=1,mji
        WRITE(17,233) (mask(i,j),i=1,mii)
    ENDDO
    CLOSE(17)
ENDIF

!H8 (Storage of the shallow soil layer) (mm)
IF (variablesascii(43)) THEN
    filename=TRIM(ADJUSTL(dirtra))//'_ASCII'//path_separator//'H8_'//fecha//'.asc'
    filename=TRIM(ADJUSTL(filename))
    mask=-9999.0
    DO n=1,ncel
           mask(cell(n).fil,cell(n).col)=cell(n).hascii(8)/dtascii
    ENDDO
    OPEN(17,file=filename, status='unknown')
    Call encabezado
    WRITE(17,'(A19)')'NODATA_value  -9999'
    DO j=1,mji
        WRITE(17,233) (mask(i,j),i=1,mii)
    ENDDO
    CLOSE(17)
ENDIF

!LAI (m2/m2)
IF (variablesascii(44)) THEN
    filename=TRIM(ADJUSTL(dirtra))//'_ASCII'//path_separator//'V1_'//fecha//'.asc'
    filename=TRIM(ADJUSTL(filename))
    mask=-9999.0
    DO n=1,ncel
           mask(cell(n).fil,cell(n).col)=cell(n).laiascii/dtascii
    ENDDO
    OPEN(17,file=filename, status='unknown')
    Call encabezado
    WRITE(17,'(A19)')'NODATA_value  -9999'
    DO j=1,mji
        WRITE(17,233) (mask(i,j),i=1,mii)
    ENDDO
    CLOSE(17)
ENDIF

!LAIr (m2/m2)
IF (variablesascii(45)) THEN
    filename=TRIM(ADJUSTL(dirtra))//'_ASCII'//path_separator//'V2_'//fecha//'.asc'
    filename=TRIM(ADJUSTL(filename))
    mask=-9999.0
    DO n=1,ncel
           mask(cell(n).fil,cell(n).col)=cell(n).lairascii/dtascii
    ENDDO
    OPEN(17,file=filename, status='unknown')
    Call encabezado
    WRITE(17,'(A19)')'NODATA_value  -9999'
    DO j=1,mji
        WRITE(17,233) (mask(i,j),i=1,mii)
    ENDDO
    CLOSE(17)
ENDIF

!Imax (mm)
IF (variablesascii(46)) THEN
    filename=TRIM(ADJUSTL(dirtra))//'_ASCII'//path_separator//'V3_'//fecha//'.asc'
    filename=TRIM(ADJUSTL(filename))
    mask=-9999.0
    DO n=1,ncel
           mask(cell(n).fil,cell(n).col)=cell(n).imaxascii/dtascii
    ENDDO
    OPEN(17,file=filename, status='unknown')
    Call encabezado
    WRITE(17,'(A19)')'NODATA_value  -9999'
    DO j=1,mji
        WRITE(17,233) (mask(i,j),i=1,mii)
    ENDDO
    CLOSE(17)
ENDIF

!ei (mm/deltat)
IF (variablesascii(47)) THEN
    filename=TRIM(ADJUSTL(dirtra))//'_ASCII'//path_separator//'V4_'//fecha//'.asc'
    filename=TRIM(ADJUSTL(filename))
    mask=-9999.0
    DO n=1,ncel
           mask(cell(n).fil,cell(n).col)=cell(n).eiascii/dtascii
    ENDDO
    OPEN(17,file=filename, status='unknown')
    Call encabezado
    WRITE(17,'(A19)')'NODATA_value  -9999'
    DO j=1,mji
        WRITE(17,233) (mask(i,j),i=1,mii)
    ENDDO
    CLOSE(17)
ENDIF

!tr (mm/deltat)
IF (variablesascii(48)) THEN
    filename=TRIM(ADJUSTL(dirtra))//'_ASCII'//path_separator//'V5_'//fecha//'.asc'
    filename=TRIM(ADJUSTL(filename))
    mask=-9999.0
    DO n=1,ncel
           mask(cell(n).fil,cell(n).col)=cell(n).trascii/dtascii
    ENDDO
    OPEN(17,file=filename, status='unknown')
    Call encabezado
    WRITE(17,'(A19)')'NODATA_value  -9999'
    DO j=1,mji
        WRITE(17,233) (mask(i,j),i=1,mii)
    ENDDO
    CLOSE(17)
ENDIF

!tr1 (mm/deltat)
IF (variablesascii(49)) THEN
    filename=TRIM(ADJUSTL(dirtra))//'_ASCII'//path_separator//'V6_'//fecha//'.asc'
    filename=TRIM(ADJUSTL(filename))
    mask=-9999.0
    DO n=1,ncel
           mask(cell(n).fil,cell(n).col)=cell(n).tr1ascii/dtascii
    ENDDO
    OPEN(17,file=filename, status='unknown')
    Call encabezado
    WRITE(17,'(A19)')'NODATA_value  -9999'
    DO j=1,mji
        WRITE(17,233) (mask(i,j),i=1,mii)
    ENDDO
    CLOSE(17)
ENDIF

!tr2 (mm/deltat)
IF (variablesascii(50)) THEN
    filename=TRIM(ADJUSTL(dirtra))//'_ASCII'//path_separator//'V7_'//fecha//'.asc'
    filename=TRIM(ADJUSTL(filename))
    mask=-9999.0
    DO n=1,ncel
           mask(cell(n).fil,cell(n).col)=cell(n).tr2ascii/dtascii
    ENDDO
    OPEN(17,file=filename, status='unknown')
    Call encabezado
    WRITE(17,'(A19)')'NODATA_value  -9999'
    DO j=1,mji
        WRITE(17,233) (mask(i,j),i=1,mii)
    ENDDO
    CLOSE(17)
ENDIF

!es (mm/deltat)
IF (variablesascii(51)) THEN
    filename=TRIM(ADJUSTL(dirtra))//'_ASCII'//path_separator//'V8_'//fecha//'.asc'
    filename=TRIM(ADJUSTL(filename))
    mask=-9999.0
    DO n=1,ncel
           mask(cell(n).fil,cell(n).col)=cell(n).esascii/dtascii
    ENDDO
    OPEN(17,file=filename, status='unknown')
    Call encabezado
    WRITE(17,'(A19)')'NODATA_value  -9999'
    DO j=1,mji
        WRITE(17,233) (mask(i,j),i=1,mii)
    ENDDO
    CLOSE(17)
ENDIF

!xveg (kg/m2)
IF (variablesascii(52)) THEN
    filename=TRIM(ADJUSTL(dirtra))//'_ASCII'//path_separator//'V9_'//fecha//'.asc'
    filename=TRIM(ADJUSTL(filename))
    mask=-9999.0
    DO n=1,ncel
           mask(cell(n).fil,cell(n).col)=cell(n).xvegascii/dtascii
    ENDDO
    OPEN(17,file=filename, status='unknown')
    Call encabezado
    WRITE(17,'(A19)')'NODATA_value  -9999'
    DO j=1,mji
        WRITE(17,233) (mask(i,j),i=1,mii)
    ENDDO
    CLOSE(17)
ENDIF

!RIEGO medio en el intervalo dtascii - X7 - ºC
IF (variablesascii(53)) THEN
    filename=TRIM(ADJUSTL(dirtra))//'_ASCII'//path_separator//'X7_'//fecha//'.asc'
    filename=TRIM(ADJUSTL(filename))
    mask=-9999.0
    DO n=1,ncel
        mask(cell(n).fil,cell(n).col)=cell(n).xascii(7)/dtascii
    ENDDO
    OPEN(17,file=filename, status='unknown')
    Call encabezado
    WRITE(17,'(A19)')'NODATA_value  -9999'
    DO j=1,mji
        WRITE(17,233) (mask(i,j),i=1,mii)
    ENDDO
    CLOSE(17)
ENDIF

!Acuífero conectado
!IF (variablesascii(42)) THEN
!    filename=TRIM(ADJUSTL(dirtra))//'_ASCII'//path_separator//'X9_'//fecha//'.asc'
!    filename=TRIM(ADJUSTL(filename))
!    mask=-9999.0
!    DO n=1,ncel
!           mask(cell(n).fil,cell(n).col)=cell(n).xascii(9)/dtascii
!    ENDDO
!    OPEN(17,file=filename, status='unknown')
!    Call encabezado
!    WRITE(17,'(A19)')'NODATA_value  -9999'
!    DO j=1,mji
!        WRITE(17,233) (mask(i,j),i=1,mii)
!    ENDDO
!    CLOSE(17)
!ENDIF

!Acuífero no conectado
!IF (variablesascii(43)) THEN
!    filename=TRIM(ADJUSTL(dirtra))//'_ASCII'//path_separator//'X10_'//fecha//'.asc'
!    filename=TRIM(ADJUSTL(filename))
!    mask=-9999.0
!    DO n=1,ncel
!           mask(cell(n).fil,cell(n).col)=cell(n).xascii(10)/dtascii
!    ENDDO
!    OPEN(17,file=filename, status='unknown')
!    Call encabezado
!    WRITE(17,'(A19)')'NODATA_value  -9999'
!    DO j=1,mji
!        WRITE(17,233) (mask(i,j),i=1,mii)
!    ENDDO
!    CLOSE(17)
!ENDIF

END SUBROUTINE


!******** Cris (27/10/2016) Subrutina de escritura de los ********
!**************** estados finales de los tanques *****************
SUBROUTINE print_finalascii
!USE DFPORT 
!USE DFLIB
USE modtet

IMPLICIT NONE

CHARACTER*12 fecha
CHARACTER*500 filename
REAL,ALLOCATABLE:: mask (:,:)
REAL temp

IF (ALLOCATED(mask)) DEALLOCATE(mask)
ALLOCATE(mask(mi,mj))
mii = INT(mi)
mji = INT(mj)

!establece la fecha
!fecin = archin(1:2)//'/'//archin(3:4)//'/'//archin(11:14)
!horin = archin(5:6)//':'//archin(7:8)//':00'
!CALL tposal(fecin,horin,t,dtmin,fecfin,horfin)
!!convierte la fecha en cadena de caracteres
!fecha = fecfin(7:10)//fecfin(4:5)//fecfin(1:2)//horfin(1:2)//horfin(4:5)
WRITE(fecha,'(I4,4(I2.2))')nyear,nmonth,nday,nhora,nmin


233 FORMAT(<mii>(F12.5,x))
234 FORMAT(<mii>(ES12.5,x))

!ALMACENAMIENTO DE NIEVE Final - H0 - mm
IF (finalesascii(1)) THEN
    filename=TRIM(ADJUSTL(dirtra))//'_ASCII'//path_separator//'EF1_'//fecha//'.asc'
    filename=TRIM(ADJUSTL(filename))
    mask=-9999.0
    DO n=1,ncel
           mask(cell(n).fil,cell(n).col)=cell(n).h(0)
    ENDDO
    OPEN(17,file=filename, status='unknown')
    Call encabezado
    WRITE(17,'(A19)')'NODATA_value  -9999'
    DO j=1,mji
        WRITE(17,233) (mask(i,j),i=1,mii)
    ENDDO
    CLOSE(17)
ENDIF

!HUMEDAD DEL SUELO Final - H1 - %
IF (finalesascii(2)) THEN
    filename=TRIM(ADJUSTL(dirtra))//'_ASCII'//path_separator//'EF2_'//fecha//'.asc'
    filename=TRIM(ADJUSTL(filename))
    mask=-9999.0
    DO n=1,ncel
           mask(cell(n).fil,cell(n).col)=cell(n).h(1)
    ENDDO
    OPEN(17,file=filename, status='unknown')
    Call encabezado
    WRITE(17,'(A19)')'NODATA_value  -9999'
    DO j=1,mji
        WRITE(17,233) (mask(i,j),i=1,mii)
    ENDDO
    CLOSE(17)
ENDIF

!ALMACENAMIENTO SUPERFICIAL Final - H2 - mm
IF (finalesascii(3)) THEN
    filename=TRIM(ADJUSTL(dirtra))//'_ASCII'//path_separator//'EF3_'//fecha//'.asc'
    filename=TRIM(ADJUSTL(filename))
    mask=-9999.0
    DO n=1,ncel
           mask(cell(n).fil,cell(n).col)=cell(n).h(2)
    ENDDO
    OPEN(17,file=filename, status='unknown')
    Call encabezado
    WRITE(17,'(A19)')'NODATA_value  -9999'
    DO j=1,mji
        WRITE(17,233) (mask(i,j),i=1,mii)
    ENDDO
    CLOSE(17)
ENDIF

!ALMACENAMIENTO GRAVITACIONAL Final - H3 - mm
IF (finalesascii(4)) THEN
    filename=TRIM(ADJUSTL(dirtra))//'_ASCII'//path_separator//'EF4_'//fecha//'.asc'
    filename=TRIM(ADJUSTL(filename))
    mask=-9999.0
    DO n=1,ncel
           mask(cell(n).fil,cell(n).col)=cell(n).h(3)
    ENDDO
    OPEN(17,file=filename, status='unknown')
    Call encabezado
    WRITE(17,'(A19)')'NODATA_value  -9999'
    DO j=1,mji
        WRITE(17,233) (mask(i,j),i=1,mii)
    ENDDO
    CLOSE(17)
ENDIF

!ALMACENAMIENTO EN EL ACUÍFERO Final - H4 - mm
IF (finalesascii(5)) THEN
    filename=TRIM(ADJUSTL(dirtra))//'_ASCII'//path_separator//'EF5_'//fecha//'.asc'
    filename=TRIM(ADJUSTL(filename))
    mask=-9999.0
    DO n=1,ncel
           mask(cell(n).fil,cell(n).col)=cell(n).h(4)
    ENDDO
    OPEN(17,file=filename, status='unknown')
    Call encabezado
    WRITE(17,'(A19)')'NODATA_value  -9999'
    DO j=1,mji
        WRITE(17,233) (mask(i,j),i=1,mii)
    ENDDO
    CLOSE(17)
ENDIF

!ALMACENAMIENTO EN CAUCES Final - H5 - mm
IF (finalesascii(6)) THEN
    filename=TRIM(ADJUSTL(dirtra))//'_ASCII'//path_separator//'EF6_'//fecha//'.asc'
    filename=TRIM(ADJUSTL(filename))
    mask=-9999.0
    DO n=1,ncel
           mask(cell(n).fil,cell(n).col)=cell(n).h(5)
    ENDDO
    OPEN(17,file=filename, status='unknown')
    Call encabezado
    WRITE(17,'(A19)')'NODATA_value  -9999'
    DO j=1,mji
        WRITE(17,233) (mask(i,j),i=1,mii)
    ENDDO
    CLOSE(17)
ENDIF

!INTERCEPCIÓN POR LAS HOJAS Final - H6 - mm
IF (finalesascii(7)) THEN
    filename=TRIM(ADJUSTL(dirtra))//'_ASCII'//path_separator//'EF7_'//fecha//'.asc'
    filename=TRIM(ADJUSTL(filename))
    mask=-9999.0
    DO n=1,ncel
           mask(cell(n).fil,cell(n).col)=cell(n).h(6)
    ENDDO
    OPEN(17,file=filename, status='unknown')
    Call encabezado
    WRITE(17,'(A19)')'NODATA_value  -9999'
    DO j=1,mji
        WRITE(17,233) (mask(i,j),i=1,mii)
    ENDDO
    CLOSE(17)
ENDIF

!SUSPENDIDO EN LADERA Final ARENA - SusSedLAD(1) - m3
IF (finalesascii(8)) THEN
    filename=TRIM(ADJUSTL(dirtra))//'_ASCII'//path_separator//'EF8_'//fecha//'.asc'
    filename=TRIM(ADJUSTL(filename))
    mask=-9999.0
    DO n=1,ncel
           mask(cell(n).fil,cell(n).col)=cell(n).SusSedLAD(1)
    ENDDO
    OPEN(17,file=filename, status='unknown')
    Call encabezado
    WRITE(17,'(A19)')'NODATA_value  -9999'
    DO j=1,mji
        WRITE(17,233) (mask(i,j),i=1,mii)
    ENDDO
    CLOSE(17)
ENDIF

!SUSPENDIDO EN LADERA Final LIMO - SusSedLAD(2) - m3
IF (finalesascii(9)) THEN
    filename=TRIM(ADJUSTL(dirtra))//'_ASCII'//path_separator//'EF9_'//fecha//'.asc'
    filename=TRIM(ADJUSTL(filename))
    mask=-9999.0
    DO n=1,ncel
           mask(cell(n).fil,cell(n).col)=cell(n).SusSedLAD(2)
    ENDDO
    OPEN(17,file=filename, status='unknown')
    Call encabezado
    WRITE(17,'(A19)')'NODATA_value  -9999'
    DO j=1,mji
        WRITE(17,233) (mask(i,j),i=1,mii)
    ENDDO
    CLOSE(17)
ENDIF

!SUSPENDIDO EN LADERA Final ARCILLA - SusSedLAD(3) - m3
IF (finalesascii(10)) THEN
    filename=TRIM(ADJUSTL(dirtra))//'_ASCII'//path_separator//'EF10_'//fecha//'.asc'
    filename=TRIM(ADJUSTL(filename))
    mask=-9999.0
    DO n=1,ncel
           mask(cell(n).fil,cell(n).col)=cell(n).SusSedLAD(3)
    ENDDO
    OPEN(17,file=filename, status='unknown')
    Call encabezado
    WRITE(17,'(A19)')'NODATA_value  -9999'
    DO j=1,mji
        WRITE(17,233) (mask(i,j),i=1,mii)
    ENDDO
    CLOSE(17)
ENDIF

!SUSPENDIDO EN RED Final ARENA - SusSedRED(1) - m3
IF (finalesascii(11)) THEN
    filename=TRIM(ADJUSTL(dirtra))//'_ASCII'//path_separator//'EF11_'//fecha//'.asc'
    filename=TRIM(ADJUSTL(filename))
    mask=-9999.0
    DO n=1,ncel
           mask(cell(n).fil,cell(n).col)=cell(n).SusSedRED(1)
    ENDDO
    OPEN(17,file=filename, status='unknown')
    Call encabezado
    WRITE(17,'(A19)')'NODATA_value  -9999'
    DO j=1,mji
        WRITE(17,233) (mask(i,j),i=1,mii)
    ENDDO
    CLOSE(17)
ENDIF

!SUSPENDIDO EN RED Final LIMO - SusSedRED(2) - m3
IF (finalesascii(12)) THEN
    filename=TRIM(ADJUSTL(dirtra))//'_ASCII'//path_separator//'EF12_'//fecha//'.asc'
    filename=TRIM(ADJUSTL(filename))
    mask=-9999.0
    DO n=1,ncel
           mask(cell(n).fil,cell(n).col)=cell(n).SusSedRED(2)
    ENDDO
    OPEN(17,file=filename, status='unknown')
    Call encabezado
    WRITE(17,'(A19)')'NODATA_value  -9999'
    DO j=1,mji
        WRITE(17,233) (mask(i,j),i=1,mii)
    ENDDO
    CLOSE(17)
ENDIF

!SUSPENDIDO EN RED Final ARCILLA - SusSedRED(3) - m3
IF (finalesascii(13)) THEN
    filename=TRIM(ADJUSTL(dirtra))//'_ASCII'//path_separator//'EF13_'//fecha//'.asc'
    filename=TRIM(ADJUSTL(filename))
    mask=-9999.0
    DO n=1,ncel
           mask(cell(n).fil,cell(n).col)=cell(n).SusSedRED(3)
    ENDDO
    OPEN(17,file=filename, status='unknown')
    Call encabezado
    WRITE(17,'(A19)')'NODATA_value  -9999'
    DO j=1,mji
        WRITE(17,233) (mask(i,j),i=1,mii)
    ENDDO
    CLOSE(17)
ENDIF

!DEPOSITADO EN LADERA Final ARENA - DepSedLAD(1) - m3
IF (finalesascii(14)) THEN
    filename=TRIM(ADJUSTL(dirtra))//'_ASCII'//path_separator//'EF14_'//fecha//'.asc'
    filename=TRIM(ADJUSTL(filename))
    mask=-9999.0
    DO n=1,ncel
           mask(cell(n).fil,cell(n).col)=cell(n).DepSedLAD(1)
    ENDDO
    OPEN(17,file=filename, status='unknown')
    Call encabezado
    WRITE(17,'(A19)')'NODATA_value  -9999'
    DO j=1,mji
        WRITE(17,233) (mask(i,j),i=1,mii)
    ENDDO
    CLOSE(17)
ENDIF

!DEPOSITADO EN LADERA Final LIMO - DepSedLAD(2) - m3
IF (finalesascii(15)) THEN
    filename=TRIM(ADJUSTL(dirtra))//'_ASCII'//path_separator//'EF15_'//fecha//'.asc'
    filename=TRIM(ADJUSTL(filename))
    mask=-9999.0
    DO n=1,ncel
           mask(cell(n).fil,cell(n).col)=cell(n).DepSedLAD(2)
    ENDDO
    OPEN(17,file=filename, status='unknown')
    Call encabezado
    WRITE(17,'(A19)')'NODATA_value  -9999'
    DO j=1,mji
        WRITE(17,233) (mask(i,j),i=1,mii)
    ENDDO
    CLOSE(17)
ENDIF

!DEPOSITADO EN LADERA Final ARCILLA - DepSedLAD(3) - m3
IF (finalesascii(16)) THEN
    filename=TRIM(ADJUSTL(dirtra))//'_ASCII'//path_separator//'EF16_'//fecha//'.asc'
    filename=TRIM(ADJUSTL(filename))
    mask=-9999.0
    DO n=1,ncel
           mask(cell(n).fil,cell(n).col)=cell(n).DepSedLAD(3)
    ENDDO
    OPEN(17,file=filename, status='unknown')
    Call encabezado
    WRITE(17,'(A19)')'NODATA_value  -9999'
    DO j=1,mji
        WRITE(17,233) (mask(i,j),i=1,mii)
    ENDDO
    CLOSE(17)
ENDIF

!DEPOSITADO EN RED Final ARENA - DepSedRED(1) - m3
IF (finalesascii(17)) THEN
    filename=TRIM(ADJUSTL(dirtra))//'_ASCII'//path_separator//'EF17_'//fecha//'.asc'
    filename=TRIM(ADJUSTL(filename))
    mask=-9999.0
    DO n=1,ncel
           mask(cell(n).fil,cell(n).col)=cell(n).DepSedRED(1)
    ENDDO
    OPEN(17,file=filename, status='unknown')
    Call encabezado
    WRITE(17,'(A19)')'NODATA_value  -9999'
    DO j=1,mji
        WRITE(17,233) (mask(i,j),i=1,mii)
    ENDDO
    CLOSE(17)
ENDIF

!DEPOSITADO EN RED Final LIMO - DepSedRED(2) - m3
IF (finalesascii(18)) THEN
    filename=TRIM(ADJUSTL(dirtra))//'_ASCII'//path_separator//'EF18_'//fecha//'.asc'
    filename=TRIM(ADJUSTL(filename))
    mask=-9999.0
    DO n=1,ncel
           mask(cell(n).fil,cell(n).col)=cell(n).DepSedRED(2)
    ENDDO
    OPEN(17,file=filename, status='unknown')
    Call encabezado
    WRITE(17,'(A19)')'NODATA_value  -9999'
    DO j=1,mji
        WRITE(17,233) (mask(i,j),i=1,mii)
    ENDDO
    CLOSE(17)
ENDIF

!DEPOSITADO EN RED Final ARCILLA - DepSedRED(3) - m3
IF (finalesascii(19)) THEN
    filename=TRIM(ADJUSTL(dirtra))//'_ASCII'//path_separator//'EF19_'//fecha//'.asc'
    filename=TRIM(ADJUSTL(filename))
    mask=-9999.0
    DO n=1,ncel
           mask(cell(n).fil,cell(n).col)=cell(n).DepSedRED(3)
    ENDDO
    OPEN(17,file=filename, status='unknown')
    Call encabezado
    WRITE(17,'(A19)')'NODATA_value  -9999'
    DO j=1,mji
        WRITE(17,233) (mask(i,j),i=1,mii)
    ENDDO
    CLOSE(17)
ENDIF

!MATERIAL PARENTAL Final ARENA - ErodSed(1) - m3
IF (finalesascii(20)) THEN
    filename=TRIM(ADJUSTL(dirtra))//'_ASCII'//path_separator//'EF20_'//fecha//'.asc'
    filename=TRIM(ADJUSTL(filename))
    mask=-9999.0
    DO n=1,ncel
           mask(cell(n).fil,cell(n).col)=cell(n).ErodSed(1)
    ENDDO
    OPEN(17,file=filename, status='unknown')
    Call encabezado
    WRITE(17,'(A19)')'NODATA_value  -9999'
    DO j=1,mji
        WRITE(17,233) (mask(i,j),i=1,mii)
    ENDDO
    CLOSE(17)
ENDIF

!MATERIAL PARENTAL Final LIMO - ErodSed(2) - m3
IF (finalesascii(21)) THEN
    filename=TRIM(ADJUSTL(dirtra))//'_ASCII'//path_separator//'EF21_'//fecha//'.asc'
    filename=TRIM(ADJUSTL(filename))
    mask=-9999.0
    DO n=1,ncel
           mask(cell(n).fil,cell(n).col)=cell(n).ErodSed(2)
    ENDDO
    OPEN(17,file=filename, status='unknown')
    Call encabezado
    WRITE(17,'(A19)')'NODATA_value  -9999'
    DO j=1,mji
        WRITE(17,233) (mask(i,j),i=1,mii)
    ENDDO
    CLOSE(17)
ENDIF

!MATERIAL PARENTAL Final ARCILLA - ErodSed(3) - m3
IF (finalesascii(22)) THEN
    filename=TRIM(ADJUSTL(dirtra))//'_ASCII'//path_separator//'EF22_'//fecha//'.asc'
    filename=TRIM(ADJUSTL(filename))
    mask=-9999.0
    DO n=1,ncel
           mask(cell(n).fil,cell(n).col)=cell(n).ErodSed(3)
    ENDDO
    OPEN(17,file=filename, status='unknown')
    Call encabezado
    WRITE(17,'(A19)')'NODATA_value  -9999'
    DO j=1,mji
        WRITE(17,233) (mask(i,j),i=1,mii)
    ENDDO
    CLOSE(17)
ENDIF

!H8 Storage of the shallow soil layer
IF (finalesascii(23)) THEN
    filename=TRIM(ADJUSTL(dirtra))//'_ASCII'//path_separator//'EF23_'//fecha//'.asc'
    filename=TRIM(ADJUSTL(filename))
    mask=-9999.0
    DO n=1,ncel
           mask(cell(n).fil,cell(n).col)=cell(n).h(8)
    ENDDO
    OPEN(17,file=filename, status='unknown')
    Call encabezado
    WRITE(17,'(A19)')'NODATA_value  -9999'
    DO j=1,mji
        WRITE(17,233) (mask(i,j),i=1,mii)
    ENDDO
    CLOSE(17)
ENDIF

!Xveg Biomass
IF (finalesascii(24)) THEN
    filename=TRIM(ADJUSTL(dirtra))//'_ASCII'//path_separator//'EF24_'//fecha//'.asc'
    filename=TRIM(ADJUSTL(filename))
    mask=-9999.0
    DO n=1,ncel
           mask(cell(n).fil,cell(n).col)=cell(n).xveg
    ENDDO
    OPEN(17,file=filename, status='unknown')
    Call encabezado
    WRITE(17,'(A19)')'NODATA_value  -9999'
    DO j=1,mji
        WRITE(17,233) (mask(i,j),i=1,mii)
    ENDDO
    CLOSE(17)
ENDIF

!IMax Maximum interceptation
IF (finalesascii(25)) THEN
    filename=TRIM(ADJUSTL(dirtra))//'_ASCII'//path_separator//'EF25_'//fecha//'.asc'
    filename=TRIM(ADJUSTL(filename))
    mask=-9999.0
    DO n=1,ncel
           mask(cell(n).fil,cell(n).col)=cell(n).imax
    ENDDO
    OPEN(17,file=filename, status='unknown')
    Call encabezado
    WRITE(17,'(A19)')'NODATA_value  -9999'
    DO j=1,mji
        WRITE(17,233) (mask(i,j),i=1,mii)
    ENDDO
    CLOSE(17)
ENDIF

END SUBROUTINE


!******************************************************************
!* Escribe mapas de variables de estado de vegetación dinámica
!******************************************************************
SUBROUTINE print_lair
!USE DFPORT 
!USE DFLIB
USE modtet

IMPLICIT NONE

CHARACTER*12 fecha
CHARACTER*500 filename
REAL,ALLOCATABLE:: mask (:,:)
REAL temp
INTEGER cwi, csi, dxi

IF (ALLOCATED(mask)) DEALLOCATE(mask)
ALLOCATE(mask(mi,mj))

!establece la fecha
!fecin = archin(1:2)//'/'//archin(3:4)//'/'//archin(11:14)
!horin = archin(5:6)//':'//archin(7:8)//':00'
!CALL tposal(fecin,horin,t,dtmin,fecfin,horfin)
!!convierte la fecha en cadena de caracteres
!fecha = fecfin(7:10)//fecfin(4:5)//fecfin(1:2)//horfin(1:2)//horfin(4:5)
WRITE(fecha,'(I4,4(I2.2))')nyear,nmonth,nday,nhora,nmin

filename=TRIM(ADJUSTL(dirtra))//'_MAPAS'//path_separator//'h1'//fecha//'.asc'
filename=TRIM(ADJUSTL(filename))
mask=-9999.0
DO n=1,ncel
   mask(cell(n).fil,cell(n).col)=cell(n).h(1)
ENDDO
!print *,mask
!print *,mi
!print *,mj
!print *,mask(1,1)
!print *,size(mask)
!CALL escribasc(filename,mi,mj,cw,cs,dx,mask)
!INTEGER mi,mj,i,j
!REAL dx,cw,cs,temp,mask(mi,mj)
!GUIOMAR (22/10/2015): cambio mask a allocatable para evitar problemas de memoria
!REAL dx,cw,cs,temp
!REAL,ALLOCATABLE:: mask(:,:)
!INTEGER mii, mji, cwi, csi, dxi
!CHARACTER artem*128

!print *,mi
!print *,mj
!print *,mask(1,1)
mii = INT(mi)
mji = INT(mj)
!cwi = INT(cw)
!csi = INT(cs)
!dxi = INT(dx)

OPEN(17,file=filename, status='unknown')
SELECT CASE(mii)
    CASE(:9)
        WRITE(17,'(A14,I1)')'ncols         ',mii
    CASE(10:99)
        WRITE(17,'(A14,I2)')'ncols         ',mii
    CASE(100:999)
        WRITE(17,'(A14,I3)')'ncols         ',mii
    CASE(1000:9999)
        WRITE(17,'(A14,I4)')'ncols         ',mii
    CASE(10000:99999)
        WRITE(17,'(A14,I5)')'ncols         ',mii
    CASE(100000:999999)
        WRITE(17,'(A14,I6)')'ncols         ',mii
    CASE(1000000:9999999)
        WRITE(17,'(A14,I7)')'ncols         ',mii
    CASE(10000000:99999999)
        WRITE(17,'(A14,I8)')'ncols         ',mii
END SELECT
SELECT CASE(mji)
    CASE(:9)
        WRITE(17,'(A14,I1)')'nrows         ',mji
    CASE(10:99)
        WRITE(17,'(A14,I2)')'nrows         ',mji
    CASE(100:999)
        WRITE(17,'(A14,I3)')'nrows         ',mji
    CASE(1000:9999)
        WRITE(17,'(A14,I4)')'nrows         ',mji
    CASE(10000:99999)
        WRITE(17,'(A14,I5)')'nrows         ',mji
    CASE(100000:999999)
        WRITE(17,'(A14,I6)')'nrows         ',mji
    CASE(1000000:9999999)
        WRITE(17,'(A14,I7)')'nrows         ',mji
    CASE(10000000:99999999)
        WRITE(17,'(A14,I8)')'nrows         ',mji
END SELECT
 
WRITE(17,'(A14,F12.5)')'xllcorner     ',cw
WRITE(17,'(A14,F13.5)')'yllcorner     ',cs
WRITE(17,'(A14,F12.5)')'cellsize      ',dx
WRITE(17,'(A19)')'NODATA_value  -9999'
DO j=1,mji
    WRITE(17,233) (mask(i,j),i=1,mii)
ENDDO
CLOSE(17)
!233 FORMAT(<mii>F15.3)
!GUIOMAR(22/10/2015): Tengo problemas con el formato
233 FORMAT(<mii>(F15.8,1x))
234 FORMAT(<mii>(ES15.8,1x))

END SUBROUTINE

SUBROUTINE print_asciinitr
!USE DFPORT 
!USE DFLIB
USE modtet

IMPLICIT NONE

CHARACTER*12 fecha
CHARACTER*500 filename
REAL,ALLOCATABLE:: mask (:,:)
REAL temp

IF (ALLOCATED(mask)) DEALLOCATE(mask)
ALLOCATE(mask(mi,mj))
mii = INT(mi)
mji = INT(mj)

!establece la fecha
!fecin = archin(1:2)//'/'//archin(3:4)//'/'//archin(11:14)
!horin = archin(5:6)//':'//archin(7:8)//':00'
!CALL tposal(fecin,horin,t,dtmin,fecfin,horfin)
!!convierte la fecha en cadena de caracteres
!fecha = fecfin(7:10)//fecfin(4:5)//fecfin(1:2)//horfin(1:2)//horfin(4:5)
WRITE(fecha,'(I4,4(I2.2))')nyear,nmonth,nday,nhora,nmin

233 FORMAT(<mii>(F12.5,1x))
234 FORMAT(<mii>(ES12.5,1x))
    
!INPUT NH4 media en el intervalo dtascii - FN40 - kg
IF (variablesasciinitr(1)) THEN
    filename=TRIM(ADJUSTL(dirtra))//'_ASCII'//path_separator//'N1_'//fecha//'.asc'
    filename=TRIM(ADJUSTL(filename))
    mask=-9999.0
    DO n=1,ncel
           mask(cell(n).fil,cell(n).col)=cell(n).fnascii(40)/dtascii
    ENDDO
    OPEN(17,file=filename, status='unknown')
    Call encabezado
    WRITE(17,'(A19)')'NODATA_value  -9999'
    DO j=1,mji
        WRITE(17,233) (mask(i,j),i=1,mii)
    ENDDO
    CLOSE(17)
ENDIF

!INPUT NO3 media en el intervalo dtascii - FN41 - kg
IF (variablesasciinitr(2)) THEN
    filename=TRIM(ADJUSTL(dirtra))//'_ASCII'//path_separator//'N2_'//fecha//'.asc'
    filename=TRIM(ADJUSTL(filename))
    mask=-9999.0
    DO n=1,ncel
           mask(cell(n).fil,cell(n).col)=cell(n).fnascii(41)/dtascii
    ENDDO
    OPEN(17,file=filename, status='unknown')
    Call encabezado
    WRITE(17,'(A19)')'NODATA_value  -9999'
    DO j=1,mji
        WRITE(17,233) (mask(i,j),i=1,mii)
    ENDDO
    CLOSE(17)
ENDIF

!DEPOSICIÓN ATMOSFÉRICA NH4 media en el intervalo dtascii - FN45 - kg
IF (variablesasciinitr(3)) THEN
    filename=TRIM(ADJUSTL(dirtra))//'_ASCII'//path_separator//'N3_'//fecha//'.asc'
    filename=TRIM(ADJUSTL(filename))
    mask=-9999.0
    DO n=1,ncel
           mask(cell(n).fil,cell(n).col)=cell(n).fnascii(45)/dtascii
    ENDDO
    OPEN(17,file=filename, status='unknown')
    Call encabezado
    WRITE(17,'(A19)')'NODATA_value  -9999'
    DO j=1,mji
        WRITE(17,233) (mask(i,j),i=1,mii)
    ENDDO
    CLOSE(17)
ENDIF

!DEPOSICIÓN ATMOSFÉRICA NO3 media en el intervalo dtascii - FN46 - kg
IF (variablesasciinitr(4)) THEN
    filename=TRIM(ADJUSTL(dirtra))//'_ASCII'//path_separator//'N4_'//fecha//'.asc'
    filename=TRIM(ADJUSTL(filename))
    mask=-9999.0
    DO n=1,ncel
           mask(cell(n).fil,cell(n).col)=cell(n).fnascii(46)/dtascii
    ENDDO
    OPEN(17,file=filename, status='unknown')
    Call encabezado
    WRITE(17,'(A19)')'NODATA_value  -9999'
    DO j=1,mji
        WRITE(17,233) (mask(i,j),i=1,mii)
    ENDDO
    CLOSE(17)
ENDIF

!MINERALIZACIÓN media en el intervalo dtascii - FN0 - kg
IF (variablesasciinitr(5)) THEN
    filename=TRIM(ADJUSTL(dirtra))//'_ASCII'//path_separator//'N5_'//fecha//'.asc'
    filename=TRIM(ADJUSTL(filename))
    mask=-9999.0
    DO n=1,ncel
           mask(cell(n).fil,cell(n).col)=cell(n).fnascii(0)/dtascii
    ENDDO
    OPEN(17,file=filename, status='unknown')
    Call encabezado
    WRITE(17,'(A19)')'NODATA_value  -9999'
    DO j=1,mji
        WRITE(17,233) (mask(i,j),i=1,mii)
    ENDDO
    CLOSE(17)
ENDIF    

!INMOVILIZACIÓN media en el intervalo dtascii - FN1 - kg
IF (variablesasciinitr(6)) THEN
    filename=TRIM(ADJUSTL(dirtra))//'_ASCII'//path_separator//'N6_'//fecha//'.asc'
    filename=TRIM(ADJUSTL(filename))
    mask=-9999.0
    DO n=1,ncel
           mask(cell(n).fil,cell(n).col)=cell(n).fnascii(1)/dtascii
    ENDDO
    OPEN(17,file=filename, status='unknown')
    Call encabezado
    WRITE(17,'(A19)')'NODATA_value  -9999'
    DO j=1,mji
        WRITE(17,233) (mask(i,j),i=1,mii)
    ENDDO
    CLOSE(17)
ENDIF

!VOLATILIZACIÓN media en el intervalo dtascii - FN44 - kg
IF (variablesasciinitr(7)) THEN
    filename=TRIM(ADJUSTL(dirtra))//'_ASCII'//path_separator//'N7_'//fecha//'.asc'
    filename=TRIM(ADJUSTL(filename))
    mask=-9999.0
    DO n=1,ncel
           mask(cell(n).fil,cell(n).col)=cell(n).fnascii(44)/dtascii
    ENDDO
    OPEN(17,file=filename, status='unknown')
    Call encabezado
    WRITE(17,'(A19)')'NODATA_value  -9999'
    DO j=1,mji
        WRITE(17,233) (mask(i,j),i=1,mii)
    ENDDO
    CLOSE(17)
ENDIF

!NITRIFICACIÓN media en el intervalo dtascii - FN2 - kg
IF (variablesasciinitr(8)) THEN
    filename=TRIM(ADJUSTL(dirtra))//'_ASCII'//path_separator//'N8_'//fecha//'.asc'
    filename=TRIM(ADJUSTL(filename))
    mask=-9999.0
    DO n=1,ncel
           mask(cell(n).fil,cell(n).col)=cell(n).fnascii(2)/dtascii
    ENDDO
    OPEN(17,file=filename, status='unknown')
    Call encabezado
    WRITE(17,'(A19)')'NODATA_value  -9999'
    DO j=1,mji
        WRITE(17,233) (mask(i,j),i=1,mii)
    ENDDO
    CLOSE(17)
ENDIF 

!FIJACIÓN media en el intervalo dtascii - FN3 - kg
IF (variablesasciinitr(9)) THEN
    filename=TRIM(ADJUSTL(dirtra))//'_ASCII'//path_separator//'N9_'//fecha//'.asc'
    filename=TRIM(ADJUSTL(filename))
    mask=-9999.0
    DO n=1,ncel
           mask(cell(n).fil,cell(n).col)=cell(n).fnascii(3)/dtascii
    ENDDO
    OPEN(17,file=filename, status='unknown')
    Call encabezado
    WRITE(17,'(A19)')'NODATA_value  -9999'
    DO j=1,mji
        WRITE(17,233) (mask(i,j),i=1,mii)
    ENDDO
    CLOSE(17)
ENDIF 

!DESNITRIFICACIÓN media en el intervalo dtascii - FN4 - kg
IF (variablesasciinitr(10)) THEN
    filename=TRIM(ADJUSTL(dirtra))//'_ASCII'//path_separator//'N10_'//fecha//'.asc'
    filename=TRIM(ADJUSTL(filename))
    mask=-9999.0
    DO n=1,ncel
           mask(cell(n).fil,cell(n).col)=cell(n).fnascii(4)/dtascii
    ENDDO
    OPEN(17,file=filename, status='unknown')
    Call encabezado
    WRITE(17,'(A19)')'NODATA_value  -9999'
    DO j=1,mji
        WRITE(17,233) (mask(i,j),i=1,mii)
    ENDDO
    CLOSE(17)
ENDIF 

!ASIMILACIÓN PASIVA NH4 media en el intervalo dtascii - FN9 - kg
IF (variablesasciinitr(11)) THEN
    filename=TRIM(ADJUSTL(dirtra))//'_ASCII'//path_separator//'N11_'//fecha//'.asc'
    filename=TRIM(ADJUSTL(filename))
    mask=-9999.0
    DO n=1,ncel
           mask(cell(n).fil,cell(n).col)=cell(n).fnascii(9)/dtascii
    ENDDO
    OPEN(17,file=filename, status='unknown')
    Call encabezado
    WRITE(17,'(A19)')'NODATA_value  -9999'
    DO j=1,mji
        WRITE(17,233) (mask(i,j),i=1,mii)
    ENDDO
    CLOSE(17)
ENDIF

!ASIMILACIÓN PASIVA NO3 media en el intervalo dtascii - FN10 - kg
IF (variablesasciinitr(12)) THEN
    filename=TRIM(ADJUSTL(dirtra))//'_ASCII'//path_separator//'N12_'//fecha//'.asc'
    filename=TRIM(ADJUSTL(filename))
    mask=-9999.0
    DO n=1,ncel
           mask(cell(n).fil,cell(n).col)=cell(n).fnascii(10)/dtascii
    ENDDO
    OPEN(17,file=filename, status='unknown')
    Call encabezado
    WRITE(17,'(A19)')'NODATA_value  -9999'
    DO j=1,mji
        WRITE(17,233) (mask(i,j),i=1,mii)
    ENDDO
    CLOSE(17)
ENDIF

!Asimilación activa NH4 media en el intervalo dtascii - FN30 - kg
IF (variablesasciinitr(13)) THEN
    filename=TRIM(ADJUSTL(dirtra))//'_ASCII'//path_separator//'N13_'//fecha//'.asc'
    filename=TRIM(ADJUSTL(filename))
    mask=-9999.0
    DO n=1,ncel
           mask(cell(n).fil,cell(n).col)=cell(n).fnascii(30)/dtascii
    ENDDO
    OPEN(17,file=filename, status='unknown')
    Call encabezado
    WRITE(17,'(A19)')'NODATA_value  -9999'
    DO j=1,mji
        WRITE(17,233) (mask(i,j),i=1,mii)
    ENDDO
    CLOSE(17)
ENDIF

!Asimilación activa NO3 media en el intervalo dtascii - FN31 - kg
IF (variablesasciinitr(14)) THEN
    filename=TRIM(ADJUSTL(dirtra))//'_ASCII'//path_separator//'N14_'//fecha//'.asc'
    filename=TRIM(ADJUSTL(filename))
    mask=-9999.0
    DO n=1,ncel
           mask(cell(n).fil,cell(n).col)=cell(n).fnascii(31)/dtascii
    ENDDO
    OPEN(17,file=filename, status='unknown')
    Call encabezado
    WRITE(17,'(A19)')'NODATA_value  -9999'
    DO j=1,mji
        WRITE(17,233) (mask(i,j),i=1,mii)
    ENDDO
    CLOSE(17)
ENDIF

!PERCOLACIÓN NH4 media en el intervalo dtascii - FN5 - kg
IF (variablesasciinitr(15)) THEN
    filename=TRIM(ADJUSTL(dirtra))//'_ASCII'//path_separator//'N15_'//fecha//'.asc'
    filename=TRIM(ADJUSTL(filename))
    mask=-9999.0
    DO n=1,ncel
           mask(cell(n).fil,cell(n).col)=cell(n).fnascii(5)/dtascii
    ENDDO
    OPEN(17,file=filename, status='unknown')
    Call encabezado
    WRITE(17,'(A19)')'NODATA_value  -9999'
    DO j=1,mji
        WRITE(17,233) (mask(i,j),i=1,mii)
    ENDDO
    CLOSE(17)
ENDIF
    
!PERCOLACIÓN NO3 media en el intervalo dtascii - FN6 - kg
IF (variablesasciinitr(16)) THEN
    filename=TRIM(ADJUSTL(dirtra))//'_ASCII'//path_separator//'N16_'//fecha//'.asc'
    filename=TRIM(ADJUSTL(filename))
    mask=-9999.0
    DO n=1,ncel
           mask(cell(n).fil,cell(n).col)=cell(n).fnascii(6)/dtascii
    ENDDO
    OPEN(17,file=filename, status='unknown')
    Call encabezado
    WRITE(17,'(A19)')'NODATA_value  -9999'
    DO j=1,mji
        WRITE(17,233) (mask(i,j),i=1,mii)
    ENDDO
    CLOSE(17)
ENDIF

!PÉRD. SUBTERRÁNEAS NH4 media en el intervalo dtascii - FN7 - kg
IF (variablesasciinitr(17)) THEN
    filename=TRIM(ADJUSTL(dirtra))//'_ASCII'//path_separator//'N17_'//fecha//'.asc'
    filename=TRIM(ADJUSTL(filename))
    mask=-9999.0
    DO n=1,ncel
           mask(cell(n).fil,cell(n).col)=cell(n).fnascii(7)/dtascii
    ENDDO
    OPEN(17,file=filename, status='unknown')
    Call encabezado
    WRITE(17,'(A19)')'NODATA_value  -9999'
    DO j=1,mji
        WRITE(17,233) (mask(i,j),i=1,mii)
    ENDDO
    CLOSE(17)
ENDIF

!PÉRD. SUBTERRÁNEAS NO3 media en el intervalo dtascii - FN8 - kg
IF (variablesasciinitr(18)) THEN
    filename=TRIM(ADJUSTL(dirtra))//'_ASCII'//path_separator//'N18_'//fecha//'.asc'
    filename=TRIM(ADJUSTL(filename))
    mask=-9999.0
    DO n=1,ncel
           mask(cell(n).fil,cell(n).col)=cell(n).fnascii(8)/dtascii
    ENDDO
    OPEN(17,file=filename, status='unknown')
    Call encabezado
    WRITE(17,'(A19)')'NODATA_value  -9999'
    DO j=1,mji
        WRITE(17,233) (mask(i,j),i=1,mii)
    ENDDO
    CLOSE(17)
ENDIF

!MINERALIZACIÓN CAUCE media en el intervalo dtascii - FN15 - kg
IF (variablesasciinitr(19)) THEN
    filename=TRIM(ADJUSTL(dirtra))//'_ASCII'//path_separator//'N19_'//fecha//'.asc'
    filename=TRIM(ADJUSTL(filename))
    mask=-9999.0
    DO n=1,ncel
           mask(cell(n).fil,cell(n).col)=cell(n).fnascii(15)/dtascii
    ENDDO
    OPEN(17,file=filename, status='unknown')
    Call encabezado
    WRITE(17,'(A19)')'NODATA_value  -9999'
    DO j=1,mji
        WRITE(17,233) (mask(i,j),i=1,mii)
    ENDDO
    CLOSE(17)
ENDIF

!NITRIFICACIÓN CAUCE media en el intervalo dtascii - FN16 - kg
IF (variablesasciinitr(20)) THEN
    filename=TRIM(ADJUSTL(dirtra))//'_ASCII'//path_separator//'N20_'//fecha//'.asc'
    filename=TRIM(ADJUSTL(filename))
    mask=-9999.0
    DO n=1,ncel
           mask(cell(n).fil,cell(n).col)=cell(n).fnascii(16)/dtascii
    ENDDO
    OPEN(17,file=filename, status='unknown')
    Call encabezado
    WRITE(17,'(A19)')'NODATA_value  -9999'
    DO j=1,mji
        WRITE(17,233) (mask(i,j),i=1,mii)
    ENDDO
    CLOSE(17)
ENDIF

!DESNITRIFICACIÓN CAUCE media en el intervalo dtascii - FN17 - kg
IF (variablesasciinitr(21)) THEN
    filename=TRIM(ADJUSTL(dirtra))//'_ASCII'//path_separator//'N21_'//fecha//'.asc'
    filename=TRIM(ADJUSTL(filename))
    mask=-9999.0
    DO n=1,ncel
           mask(cell(n).fil,cell(n).col)=cell(n).fnascii(17)/dtascii
    ENDDO
    OPEN(17,file=filename, status='unknown')
    Call encabezado
    WRITE(17,'(A19)')'NODATA_value  -9999'
    DO j=1,mji
        WRITE(17,233) (mask(i,j),i=1,mii)
    ENDDO
    CLOSE(17)
ENDIF

!ESCORRENTÍA NO medio en el intervalo dtascii - FN23 - kg
IF (variablesasciinitr(22)) THEN
    filename=TRIM(ADJUSTL(dirtra))//'_ASCII'//path_separator//'N22_'//fecha//'.asc'
    filename=TRIM(ADJUSTL(filename))
    mask=-9999.0
    DO n=1,ncel
           mask(cell(n).fil,cell(n).col)=(cell(n).fnascii(23))/dtascii
    ENDDO
    OPEN(17,file=filename, status='unknown')
    Call encabezado
    WRITE(17,'(A19)')'NODATA_value  -9999'
    DO j=1,mji
        WRITE(17,233) (mask(i,j),i=1,mii)
    ENDDO
    CLOSE(17)
ENDIF

!ESCORRENTÍA NH4 medio en el intervalo dtascii - FN24 - kg
IF (variablesasciinitr(23)) THEN
    filename=TRIM(ADJUSTL(dirtra))//'_ASCII'//path_separator//'N23_'//fecha//'.asc'
    filename=TRIM(ADJUSTL(filename))
    mask=-9999.0
    DO n=1,ncel
           mask(cell(n).fil,cell(n).col)=(cell(n).fnascii(24))/dtascii
    ENDDO
    OPEN(17,file=filename, status='unknown')
    Call encabezado
    WRITE(17,'(A19)')'NODATA_value  -9999'
    DO j=1,mji
        WRITE(17,233) (mask(i,j),i=1,mii)
    ENDDO
    CLOSE(17)
ENDIF

!ESCORRENTÍA NO3 medio en el intervalo dtascii - FN25- kg
IF (variablesasciinitr(24)) THEN
    filename=TRIM(ADJUSTL(dirtra))//'_ASCII'//path_separator//'N24_'//fecha//'.asc'
    filename=TRIM(ADJUSTL(filename))
    mask=-9999.0
    DO n=1,ncel
           mask(cell(n).fil,cell(n).col)=(cell(n).fnascii(25))/dtascii
    ENDDO
    OPEN(17,file=filename, status='unknown')
    Call encabezado
    WRITE(17,'(A19)')'NODATA_value  -9999'
    DO j=1,mji
        WRITE(17,233) (mask(i,j),i=1,mii)
    ENDDO
    CLOSE(17)
ENDIF

!INTERFLUJO NH4 media en el intervalo dtascii - FN11 - kg
IF (variablesasciinitr(25)) THEN
    filename=TRIM(ADJUSTL(dirtra))//'_ASCII'//path_separator//'N25_'//fecha//'.asc'
    filename=TRIM(ADJUSTL(filename))
    mask=-9999.0
    DO n=1,ncel
           mask(cell(n).fil,cell(n).col)=cell(n).fnascii(11)/dtascii
    ENDDO
    OPEN(17,file=filename, status='unknown')
    Call encabezado
    WRITE(17,'(A19)')'NODATA_value  -9999'
    DO j=1,mji
        WRITE(17,233) (mask(i,j),i=1,mii)
    ENDDO
    CLOSE(17)
ENDIF

!INTERFLUJO NO3 media en el intervalo dtascii - FN12 - kg
IF (variablesasciinitr(26)) THEN
    filename=TRIM(ADJUSTL(dirtra))//'_ASCII'//path_separator//'N26_'//fecha//'.asc'
    filename=TRIM(ADJUSTL(filename))
    mask=-9999.0
    DO n=1,ncel
           mask(cell(n).fil,cell(n).col)=cell(n).fnascii(12)/dtascii
    ENDDO
    OPEN(17,file=filename, status='unknown')
    Call encabezado
    WRITE(17,'(A19)')'NODATA_value  -9999'
    DO j=1,mji
        WRITE(17,233) (mask(i,j),i=1,mii)
    ENDDO
    CLOSE(17)
ENDIF

!FLUJO BASE NH4 media en el intervalo dtascii - FN13 - kg
IF (variablesasciinitr(27)) THEN
    filename=TRIM(ADJUSTL(dirtra))//'_ASCII'//path_separator//'N27_'//fecha//'.asc'
    filename=TRIM(ADJUSTL(filename))
    mask=-9999.0
    DO n=1,ncel
           mask(cell(n).fil,cell(n).col)=cell(n).fnascii(13)/dtascii
    ENDDO
    OPEN(17,file=filename, status='unknown')
    Call encabezado
    WRITE(17,'(A19)')'NODATA_value  -9999'
    DO j=1,mji
        WRITE(17,233) (mask(i,j),i=1,mii)
    ENDDO
    CLOSE(17)
ENDIF

!FLUJO BASE NO3 media en el intervalo dtascii - FN14 - kg
IF (variablesasciinitr(28)) THEN
    filename=TRIM(ADJUSTL(dirtra))//'_ASCII'//path_separator//'N28_'//fecha//'.asc'
    filename=TRIM(ADJUSTL(filename))
    mask=-9999.0
    DO n=1,ncel
           mask(cell(n).fil,cell(n).col)=cell(n).fnascii(14)/dtascii
    ENDDO
    OPEN(17,file=filename, status='unknown')
    Call encabezado
    WRITE(17,'(A19)')'NODATA_value  -9999'
    DO j=1,mji
        WRITE(17,233) (mask(i,j),i=1,mii)
    ENDDO
    CLOSE(17)
ENDIF

!CAUDAL NO medio en el intervalo dtascii - FN18 - kg
IF (variablesasciinitr(29)) THEN
    filename=TRIM(ADJUSTL(dirtra))//'_ASCII'//path_separator//'N29_'//fecha//'.asc'
    filename=TRIM(ADJUSTL(filename))
    mask=-9999.0
    DO n=1,ncel
           mask(cell(n).fil,cell(n).col)=cell(n).fnascii(18)/dtascii
    ENDDO
    OPEN(17,file=filename, status='unknown')
    Call encabezado
    WRITE(17,'(A19)')'NODATA_value  -9999'
    DO j=1,mji
        WRITE(17,233) (mask(i,j),i=1,mii)
    ENDDO
    CLOSE(17)
ENDIF

!CAUDAL NH4 medio en el intervalo dtascii - FN19 - kg
IF (variablesasciinitr(30)) THEN
    filename=TRIM(ADJUSTL(dirtra))//'_ASCII'//path_separator//'N30_'//fecha//'.asc'
    filename=TRIM(ADJUSTL(filename))
    mask=-9999.0
    DO n=1,ncel
           mask(cell(n).fil,cell(n).col)=cell(n).fnascii(19)/dtascii
    ENDDO
    OPEN(17,file=filename, status='unknown')
    Call encabezado
    WRITE(17,'(A19)')'NODATA_value  -9999'
    DO j=1,mji
        WRITE(17,233) (mask(i,j),i=1,mii)
    ENDDO
    CLOSE(17)
ENDIF

!CAUDAL NO3 medio en el intervalo dtascii - FN20 - kg
IF (variablesasciinitr(31)) THEN
    filename=TRIM(ADJUSTL(dirtra))//'_ASCII'//path_separator//'N31_'//fecha//'.asc'
    filename=TRIM(ADJUSTL(filename))
    mask=-9999.0
    DO n=1,ncel
           mask(cell(n).fil,cell(n).col)=cell(n).fnascii(20)/dtascii
    ENDDO
    OPEN(17,file=filename, status='unknown')
    Call encabezado
    WRITE(17,'(A19)')'NODATA_value  -9999'
    DO j=1,mji
        WRITE(17,233) (mask(i,j),i=1,mii)
    ENDDO
    CLOSE(17)
ENDIF

!FLUJO SUPERFICIAL CON SEDIMENTOS NO medio en el intervalo dtascii - FN28 - kg
IF (variablesasciinitr(32)) THEN
    filename=TRIM(ADJUSTL(dirtra))//'_ASCII'//path_separator//'N32_'//fecha//'.asc'
    filename=TRIM(ADJUSTL(filename))
    mask=-9999.0
    DO n=1,ncel
           mask(cell(n).fil,cell(n).col)=cell(n).fnascii(28)/dtascii
    ENDDO
    OPEN(17,file=filename, status='unknown')
    Call encabezado
    WRITE(17,'(A19)')'NODATA_value  -9999'
    DO j=1,mji
        WRITE(17,233) (mask(i,j),i=1,mii)
    ENDDO
    CLOSE(17)
ENDIF

!FLUJO SUPERFICIAL CON SEDIMENTOS NH4 medio en el intervalo dtascii - FN29 - kg
IF (variablesasciinitr(33)) THEN
    filename=TRIM(ADJUSTL(dirtra))//'_ASCII'//path_separator//'N33_'//fecha//'.asc'
    filename=TRIM(ADJUSTL(filename))
    mask=-9999.0
    DO n=1,ncel
           mask(cell(n).fil,cell(n).col)=cell(n).fnascii(29)/dtascii
    ENDDO
    OPEN(17,file=filename, status='unknown')
    Call encabezado
    WRITE(17,'(A19)')'NODATA_value  -9999'
    DO j=1,mji
        WRITE(17,233) (mask(i,j),i=1,mii)
    ENDDO
    CLOSE(17)
ENDIF

!!SUSPENSIÓN NH4 medio en el intervalo dtascii - FN27 - kg
!IF (variablesasciinitr(25)) THEN
!    filename=TRIM(ADJUSTL(dirtra))//'_ASCII'//path_separator//'N25_'//fecha//'.asc'
!    filename=TRIM(ADJUSTL(filename))
!    mask=-9999.0
!    DO n=1,ncel
!           mask(cell(n).fil,cell(n).col)=cell(n).fnascii(27)/dtascii
!    ENDDO
!    OPEN(17,file=filename, status='unknown')
!    Call encabezado
!    WRITE(17,'(A19)')'NODATA_value  -9999'
!    DO j=1,mji
!        WRITE(17,233) (mask(i,j),i=1,mii)
!    ENDDO
!    CLOSE(17)
!ENDIF

!!Pérdidas subterráneas en acuífero conectado NH4 media en el intervalo dtascii - FN32 - kg
!IF (variablesasciinitr(30)) THEN
!    filename=TRIM(ADJUSTL(dirtra))//'_ASCII'//path_separator//'N29_'//fecha//'.asc'
!    filename=TRIM(ADJUSTL(filename))
!    mask=-9999.0
!    DO n=1,ncel
!           mask(cell(n).fil,cell(n).col)=cell(n).fnascii(32)/dtascii
!    ENDDO
!    OPEN(17,file=filename, status='unknown')
!    Call encabezado
!    WRITE(17,'(A19)')'NODATA_value  -9999'
!    DO j=1,mji
!        WRITE(17,233) (mask(i,j),i=1,mii)
!    ENDDO
!    CLOSE(17)
!ENDIF
!
!!Pérdidas subterráneas en acuífero conectado NO3 media en el intervalo dtascii - FN33 - kg
!IF (variablesasciinitr(31)) THEN
!    filename=TRIM(ADJUSTL(dirtra))//'_ASCII'//path_separator//'N31_'//fecha//'.asc'
!    filename=TRIM(ADJUSTL(filename))
!    mask=-9999.0
!    DO n=1,ncel
!           mask(cell(n).fil,cell(n).col)=cell(n).fnascii(33)/dtascii
!    ENDDO
!    OPEN(17,file=filename, status='unknown')
!    Call encabezado
!    WRITE(17,'(A19)')'NODATA_value  -9999'
!    DO j=1,mji
!        WRITE(17,233) (mask(i,j),i=1,mii)
!    ENDDO
!    CLOSE(17)
!ENDIF
!
!!Pérdidas subterráneas en acuífero no conectado NH4 media en el intervalo dtascii - FN34 - kg
!IF (variablesasciinitr(32)) THEN
!    filename=TRIM(ADJUSTL(dirtra))//'_ASCII'//path_separator//'N32_'//fecha//'.asc'
!    filename=TRIM(ADJUSTL(filename))
!    mask=-9999.0
!    DO n=1,ncel
!           mask(cell(n).fil,cell(n).col)=cell(n).fnascii(34)/dtascii
!    ENDDO
!    OPEN(17,file=filename, status='unknown')
!    Call encabezado
!    WRITE(17,'(A19)')'NODATA_value  -9999'
!    DO j=1,mji
!        WRITE(17,233) (mask(i,j),i=1,mii)
!    ENDDO
!    CLOSE(17)
!ENDIF
!
!!Pérdidas subterráneas en acuífero no conectado NO3 media en el intervalo dtascii - FN35 - kg
!IF (variablesasciinitr(33)) THEN
!    filename=TRIM(ADJUSTL(dirtra))//'_ASCII'//path_separator//'N33_'//fecha//'.asc'
!    filename=TRIM(ADJUSTL(filename))
!    mask=-9999.0
!    DO n=1,ncel
!           mask(cell(n).fil,cell(n).col)=cell(n).fnascii(35)/dtascii
!    ENDDO
!    OPEN(17,file=filename, status='unknown')
!    Call encabezado
!    WRITE(17,'(A19)')'NODATA_value  -9999'
!    DO j=1,mji
!        WRITE(17,233) (mask(i,j),i=1,mii)
!    ENDDO
!    CLOSE(17)
!ENDIF

!NITRÓGENO ORGÁNICO EN SUELO medio en el intervalo dtascii - HN0 - kg
IF (variablesasciinitr(34)) THEN
    filename=TRIM(ADJUSTL(dirtra))//'_ASCII'//path_separator//'HN0_'//fecha//'.asc'
    filename=TRIM(ADJUSTL(filename))
    mask=-9999.0
    DO n=1,ncel
           mask(cell(n).fil,cell(n).col)=cell(n).hnascii(0)/dtascii
    ENDDO
    OPEN(17,file=filename, status='unknown')
    Call encabezado
    WRITE(17,'(A19)')'NODATA_value  -9999'
    DO j=1,mji
        WRITE(17,233) (mask(i,j),i=1,mii)
    ENDDO
    CLOSE(17)
ENDIF

!AMONIO DISUELTO EN SUELO medio en el intervalo dtascii - HN1 - kg
IF (variablesasciinitr(35)) THEN
    filename=TRIM(ADJUSTL(dirtra))//'_ASCII'//path_separator//'HN1_'//fecha//'.asc'
    filename=TRIM(ADJUSTL(filename))
    mask=-9999.0
    DO n=1,ncel
           mask(cell(n).fil,cell(n).col)=cell(n).hnascii(1)/dtascii
    ENDDO
    OPEN(17,file=filename, status='unknown')
    Call encabezado
    WRITE(17,'(A19)')'NODATA_value  -9999'
    DO j=1,mji
        WRITE(17,233) (mask(i,j),i=1,mii)
    ENDDO
    CLOSE(17)
ENDIF

!NITRATO EN SUELO medio en el intervalo dtascii - HN2 - kg
IF (variablesasciinitr(36)) THEN
    filename=TRIM(ADJUSTL(dirtra))//'_ASCII'//path_separator//'HN2_'//fecha//'.asc'
    filename=TRIM(ADJUSTL(filename))
    mask=-9999.0
    DO n=1,ncel
           mask(cell(n).fil,cell(n).col)=cell(n).hnascii(2)/dtascii
    ENDDO
    OPEN(17,file=filename, status='unknown')
    Call encabezado
    WRITE(17,'(A19)')'NODATA_value  -9999'
    DO j=1,mji
        WRITE(17,233) (mask(i,j),i=1,mii)
    ENDDO
    CLOSE(17)
ENDIF

!NITRÓGENO ORGÁNICO DISUELTO EN SUPERFICIE medio en el intervalo dtascii - HN3 - kg
IF (variablesasciinitr(37)) THEN
    filename=TRIM(ADJUSTL(dirtra))//'_ASCII'//path_separator//'HN3_'//fecha//'.asc'
    filename=TRIM(ADJUSTL(filename))
    mask=-9999.0
    DO n=1,ncel
           mask(cell(n).fil,cell(n).col)=cell(n).hnascii(3)/dtascii
    ENDDO
    OPEN(17,file=filename, status='unknown')
    Call encabezado
    WRITE(17,'(A19)')'NODATA_value  -9999'
    DO j=1,mji
        WRITE(17,233) (mask(i,j),i=1,mii)
    ENDDO
    CLOSE(17)
ENDIF

!AMONIO DISUELTO EN SUPERFICIE medio en el intervalo dtascii - HN4 - kg
IF (variablesasciinitr(38)) THEN
    filename=TRIM(ADJUSTL(dirtra))//'_ASCII'//path_separator//'HN4_'//fecha//'.asc'
    filename=TRIM(ADJUSTL(filename))
    mask=-9999.0
    DO n=1,ncel
           mask(cell(n).fil,cell(n).col)=cell(n).hnascii(4)/dtascii
    ENDDO
    OPEN(17,file=filename, status='unknown')
    Call encabezado
    WRITE(17,'(A19)')'NODATA_value  -9999'
    DO j=1,mji
        WRITE(17,233) (mask(i,j),i=1,mii)
    ENDDO
    CLOSE(17)
ENDIF

!NITRATO EN SUPERFICIE medio en el intervalo dtascii - HN5 - kg
IF (variablesasciinitr(39)) THEN
    filename=TRIM(ADJUSTL(dirtra))//'_ASCII'//path_separator//'HN5_'//fecha//'.asc'
    filename=TRIM(ADJUSTL(filename))
    mask=-9999.0
    DO n=1,ncel
           mask(cell(n).fil,cell(n).col)=cell(n).hnascii(5)/dtascii
    ENDDO
    OPEN(17,file=filename, status='unknown')
    Call encabezado
    WRITE(17,'(A19)')'NODATA_value  -9999'
    DO j=1,mji
        WRITE(17,233) (mask(i,j),i=1,mii)
    ENDDO
    CLOSE(17)
ENDIF

!AMONIIO EN ACUÍFERO medio en el intervalo dtascii - HN6 - kg
IF (variablesasciinitr(40)) THEN
    filename=TRIM(ADJUSTL(dirtra))//'_ASCII'//path_separator//'HN6_'//fecha//'.asc'
    filename=TRIM(ADJUSTL(filename))
    mask=-9999.0
    DO n=1,ncel
           mask(cell(n).fil,cell(n).col)=cell(n).hnascii(6)/dtascii
    ENDDO
    OPEN(17,file=filename, status='unknown')
    Call encabezado
    WRITE(17,'(A19)')'NODATA_value  -9999'
    DO j=1,mji
        WRITE(17,233) (mask(i,j),i=1,mii)
    ENDDO
    CLOSE(17)
ENDIF

!NITRATO EN ACUÍFERO medio en el intervalo dtascii - HN7 - kg
IF (variablesasciinitr(41)) THEN
    filename=TRIM(ADJUSTL(dirtra))//'_ASCII'//path_separator//'HN7_'//fecha//'.asc'
    filename=TRIM(ADJUSTL(filename))
    mask=-9999.0
    DO n=1,ncel
           mask(cell(n).fil,cell(n).col)=cell(n).hnascii(7)/dtascii
    ENDDO
    OPEN(17,file=filename, status='unknown')
    Call encabezado
    WRITE(17,'(A19)')'NODATA_value  -9999'
    DO j=1,mji
        WRITE(17,233) (mask(i,j),i=1,mii)
    ENDDO
    CLOSE(17)
ENDIF

!AMONIO ADSORBIDO EN SUELO medio en el intervalo dtascii - HN8 - kg
IF (variablesasciinitr(42)) THEN
    filename=TRIM(ADJUSTL(dirtra))//'_ASCII'//path_separator//'HN8_'//fecha//'.asc'
    filename=TRIM(ADJUSTL(filename))
    mask=-9999.0
    DO n=1,ncel
           mask(cell(n).fil,cell(n).col)=cell(n).hnascii(8)/dtascii
    ENDDO
    OPEN(17,file=filename, status='unknown')
    Call encabezado
    WRITE(17,'(A19)')'NODATA_value  -9999'
    DO j=1,mji
        WRITE(17,233) (mask(i,j),i=1,mii)
    ENDDO
    CLOSE(17)
ENDIF

!NITRÓGENO ORGÁNICO EN SEDIMENTOS EN SUSPENSIÓN medio en el intervalo dtascii - HN9 - kg
IF (variablesasciinitr(43)) THEN
    filename=TRIM(ADJUSTL(dirtra))//'_ASCII'//path_separator//'HN9_'//fecha//'.asc'
    filename=TRIM(ADJUSTL(filename))
    mask=-9999.0
    DO n=1,ncel
           mask(cell(n).fil,cell(n).col)=cell(n).hnascii(9)/dtascii
    ENDDO
    OPEN(17,file=filename, status='unknown')
    Call encabezado
    WRITE(17,'(A19)')'NODATA_value  -9999'
    DO j=1,mji
        WRITE(17,233) (mask(i,j),i=1,mii)
    ENDDO
    CLOSE(17)
ENDIF

!AMONIO ADSORBIDO EN SEDIMENTOS EN SUSPENSIÓN medio en el intervalo dtascii - HN10 - kg
IF (variablesasciinitr(44)) THEN
    filename=TRIM(ADJUSTL(dirtra))//'_ASCII'//path_separator//'HN10_'//fecha//'.asc'
    filename=TRIM(ADJUSTL(filename))
    mask=-9999.0
    DO n=1,ncel
           mask(cell(n).fil,cell(n).col)=cell(n).hnascii(10)/dtascii
    ENDDO
    OPEN(17,file=filename, status='unknown')
    Call encabezado
    WRITE(17,'(A19)')'NODATA_value  -9999'
    DO j=1,mji
        WRITE(17,233) (mask(i,j),i=1,mii)
    ENDDO
    CLOSE(17)
ENDIF

!NITRÓGENO ORGÁNICO EN SEDIMENTOS DEPOSITADOS EN RED medio en el intervalo dtascii - HN11 - kg
IF (variablesasciinitr(45)) THEN
    filename=TRIM(ADJUSTL(dirtra))//'_ASCII'//path_separator//'HN11_'//fecha//'.asc'
    filename=TRIM(ADJUSTL(filename))
    mask=-9999.0
    DO n=1,ncel
           mask(cell(n).fil,cell(n).col)=cell(n).hnascii(11)/dtascii
    ENDDO
    OPEN(17,file=filename, status='unknown')
    Call encabezado
    WRITE(17,'(A19)')'NODATA_value  -9999'
    DO j=1,mji
        WRITE(17,233) (mask(i,j),i=1,mii)
    ENDDO
    CLOSE(17)
ENDIF

!AMONIO ADSORBIDO EN SEDIMENTOS DEPOSITADOS EN RED medio en el intervalo dtascii - HN12 - kg
IF (variablesasciinitr(46)) THEN
    filename=TRIM(ADJUSTL(dirtra))//'_ASCII'//path_separator//'HN12_'//fecha//'.asc'
    filename=TRIM(ADJUSTL(filename))
    mask=-9999.0
    DO n=1,ncel
           mask(cell(n).fil,cell(n).col)=cell(n).hnascii(12)/dtascii
    ENDDO
    OPEN(17,file=filename, status='unknown')
    Call encabezado
    WRITE(17,'(A19)')'NODATA_value  -9999'
    DO j=1,mji
        WRITE(17,233) (mask(i,j),i=1,mii)
    ENDDO
    CLOSE(17)
ENDIF

!IMPRIMIMOS EFICIENCIAS DE NITROGENO

IF (variablesasciinitr(47)) THEN    
    filename=TRIM(ADJUSTL(dirtra))//'_ASCII'//path_separator//'EN1_'//fecha//'.asc'
    filename=TRIM(ADJUSTL(filename))
    mask=-9999.0
    DO n=1,ncel
         mask(cell(n).fil,cell(n).col)=cell(n).uptEff
    ENDDO
    OPEN(17,file=filename, status='unknown')
    Call encabezado
    WRITE(17,'(A19)')'NODATA_value  -9999'
    DO j=1,mji
        WRITE(17,233) (mask(i,j),i=1,mii)
    ENDDO
ENDIF

IF (variablesasciinitr(48)) THEN
    filename=TRIM(ADJUSTL(dirtra))//'_ASCII'//path_separator//'EN2_'//fecha//'.asc'
    filename=TRIM(ADJUSTL(filename))
    mask=-9999.0
    DO n=1,ncel
         mask(cell(n).fil,cell(n).col)=cell(n).recEff
    ENDDO
    OPEN(17,file=filename, status='unknown')
    Call encabezado
    WRITE(17,'(A19)')'NODATA_value  -9999'
    DO j=1,mji
        WRITE(17,233) (mask(i,j),i=1,mii)
    ENDDO    
ENDIF

End subroutine
    
!******** Cris (03/2017) Subrutina de escritura de los ********
!**************** Estados finales de los tanques *****************
SUBROUTINE print_finalasciinitr
!USE DFPORT 
!USE DFLIB
USE modtet

IMPLICIT NONE

CHARACTER*12 fecha
CHARACTER*500 filename
REAL,ALLOCATABLE:: mask (:,:)
REAL temp

IF (ALLOCATED(mask)) DEALLOCATE(mask)
ALLOCATE(mask(mi,mj))
mii = INT(mi)
mji = INT(mj)

!!establece la fecha
!fecin = archin(1:2)//'/'//archin(3:4)//'/'//archin(11:14)
!horin = archin(5:6)//':'//archin(7:8)//':00'
!CALL tposal(fecin,horin,t,dtmin,fecfin,horfin)
!!convierte la fecha en cadena de caracteres
!fecha = fecfin(7:10)//fecfin(4:5)//fecfin(1:2)//horfin(1:2)//horfin(4:5)
WRITE(fecha,'(I4,4(I2.2))')nyear,nmonth,nday,nhora,nmin


233 FORMAT(<mii>(F12.5,1x))
234 FORMAT(<mii>(ES12.5,1x))
    
    
!Nitrógeno orgánico en fase sólida en suelo Final - hn0 - kg
IF (finalesasciinitr(1)) THEN
    filename=TRIM(ADJUSTL(dirtra))//'_ASCII'//path_separator//'EFN1_'//fecha//'.asc'
    filename=TRIM(ADJUSTL(filename))
    mask=-9999.0
    DO n=1,ncel
           mask(cell(n).fil,cell(n).col)=cell(n).hn(0)
    ENDDO
    OPEN(17,file=filename, status='unknown')
    Call encabezado
    WRITE(17,'(A19)')'NODATA_value  -9999'
    DO j=1,mji
        WRITE(17,233) (mask(i,j),i=1,mii)
    ENDDO
    CLOSE(17)
ENDIF

!Amonio disuelto en suelo Final - hn1 - kg
IF (finalesasciinitr(2)) THEN
    filename=TRIM(ADJUSTL(dirtra))//'_ASCII'//path_separator//'EFN2_'//fecha//'.asc'
    filename=TRIM(ADJUSTL(filename))
    mask=-9999.0
    DO n=1,ncel
           mask(cell(n).fil,cell(n).col)=cell(n).hn(1)
    ENDDO
    OPEN(17,file=filename, status='unknown')
    Call encabezado
    WRITE(17,'(A19)')'NODATA_value  -9999'
    DO j=1,mji
        WRITE(17,233) (mask(i,j),i=1,mii)
    ENDDO
    CLOSE(17)
ENDIF

!Nitrato en suelo Final - hn2 - kg
IF (finalesasciinitr(3)) THEN
    filename=TRIM(ADJUSTL(dirtra))//'_ASCII'//path_separator//'EFN3_'//fecha//'.asc'
    filename=TRIM(ADJUSTL(filename))
    mask=-9999.0
    DO n=1,ncel
           mask(cell(n).fil,cell(n).col)=cell(n).hn(2)
    ENDDO
    OPEN(17,file=filename, status='unknown')
    Call encabezado
    WRITE(17,'(A19)')'NODATA_value  -9999'
    DO j=1,mji
        WRITE(17,233) (mask(i,j),i=1,mii)
    ENDDO
    CLOSE(17)
ENDIF

!Nitrógeno orgánico disuelto en cauce Final - hn3 - kg
IF (finalesasciinitr(4)) THEN
    filename=TRIM(ADJUSTL(dirtra))//'_ASCII'//path_separator//'EFN4_'//fecha//'.asc'
    filename=TRIM(ADJUSTL(filename))
    mask=-9999.0
    DO n=1,ncel
           mask(cell(n).fil,cell(n).col)=cell(n).hn(3)
    ENDDO
    OPEN(17,file=filename, status='unknown')
    Call encabezado
    WRITE(17,'(A19)')'NODATA_value  -9999'
    DO j=1,mji
        WRITE(17,233) (mask(i,j),i=1,mii)
    ENDDO
    CLOSE(17)
ENDIF

!Amonio disuelto en cauce Final - hn4 - kg
IF (finalesasciinitr(5)) THEN
    filename=TRIM(ADJUSTL(dirtra))//'_ASCII'//path_separator//'EFN5_'//fecha//'.asc'
    filename=TRIM(ADJUSTL(filename))
    mask=-9999.0
    DO n=1,ncel
           mask(cell(n).fil,cell(n).col)=cell(n).hn(4)
    ENDDO
    OPEN(17,file=filename, status='unknown')
    Call encabezado
    WRITE(17,'(A19)')'NODATA_value  -9999'
    DO j=1,mji
        WRITE(17,233) (mask(i,j),i=1,mii)
    ENDDO
    CLOSE(17)
ENDIF

!Nitrato en cauce Final - hn5 - kg
IF (finalesasciinitr(6)) THEN
    filename=TRIM(ADJUSTL(dirtra))//'_ASCII'//path_separator//'EFN6_'//fecha//'.asc'
    filename=TRIM(ADJUSTL(filename))
    mask=-9999.0
    DO n=1,ncel
           mask(cell(n).fil,cell(n).col)=cell(n).hn(5)
    ENDDO
    OPEN(17,file=filename, status='unknown')
    Call encabezado
    WRITE(17,'(A19)')'NODATA_value  -9999'
    DO j=1,mji
        WRITE(17,233) (mask(i,j),i=1,mii)
    ENDDO
    CLOSE(17)
ENDIF

!Amonio en acuífero Final - hn6 - kg
IF (finalesasciinitr(7)) THEN
    filename=TRIM(ADJUSTL(dirtra))//'_ASCII'//path_separator//'EFN7_'//fecha//'.asc'
    filename=TRIM(ADJUSTL(filename))
    mask=-9999.0
    DO n=1,ncel
           mask(cell(n).fil,cell(n).col)=cell(n).hn(6)
    ENDDO
    OPEN(17,file=filename, status='unknown')
    Call encabezado
    WRITE(17,'(A19)')'NODATA_value  -9999'
    DO j=1,mji
        WRITE(17,233) (mask(i,j),i=1,mii)
    ENDDO
    CLOSE(17)
ENDIF

!Nitrato en acuífero Final - hn7 - kg
IF (finalesasciinitr(8)) THEN
    filename=TRIM(ADJUSTL(dirtra))//'_ASCII'//path_separator//'EFN8_'//fecha//'.asc'
    filename=TRIM(ADJUSTL(filename))
    mask=-9999.0
    DO n=1,ncel
           mask(cell(n).fil,cell(n).col)=cell(n).hn(7)
    ENDDO
    OPEN(17,file=filename, status='unknown')
    Call encabezado
    WRITE(17,'(A19)')'NODATA_value  -9999'
    DO j=1,mji
        WRITE(17,233) (mask(i,j),i=1,mii)
    ENDDO
    CLOSE(17)
ENDIF

!Amonio en fase sólida (adsorbido) en suelo Final - hn8 - kg
IF (finalesasciinitr(9)) THEN
    filename=TRIM(ADJUSTL(dirtra))//'_ASCII'//path_separator//'EFN9_'//fecha//'.asc'
    filename=TRIM(ADJUSTL(filename))
    mask=-9999.0
    DO n=1,ncel
           mask(cell(n).fil,cell(n).col)=cell(n).hn(8)
    ENDDO
    OPEN(17,file=filename, status='unknown')
    Call encabezado
    WRITE(17,'(A19)')'NODATA_value  -9999'
    DO j=1,mji
        WRITE(17,233) (mask(i,j),i=1,mii)
    ENDDO
    CLOSE(17)
ENDIF

!Nitrógeno orgánico en fase sólida (sedimentos suspendidos) en cauce Final - hn9 - kg
IF (finalesasciinitr(10)) THEN
    filename=TRIM(ADJUSTL(dirtra))//'_ASCII'//path_separator//'EFN10_'//fecha//'.asc'
    filename=TRIM(ADJUSTL(filename))
    mask=-9999.0
    DO n=1,ncel
           mask(cell(n).fil,cell(n).col)=cell(n).hn(9)
    ENDDO
    OPEN(17,file=filename, status='unknown')
    Call encabezado
    WRITE(17,'(A19)')'NODATA_value  -9999'
    DO j=1,mji
        WRITE(17,233) (mask(i,j),i=1,mii)
    ENDDO
    CLOSE(17)
ENDIF

!Amonio en fase sólida (sedimentos suspendidos) en cauce Final - hn10 - kg
IF (finalesasciinitr(11)) THEN
    filename=TRIM(ADJUSTL(dirtra))//'_ASCII'//path_separator//'EFN11_'//fecha//'.asc'
    filename=TRIM(ADJUSTL(filename))
    mask=-9999.0
    DO n=1,ncel
           mask(cell(n).fil,cell(n).col)=cell(n).hn(10)
    ENDDO
    OPEN(17,file=filename, status='unknown')
    Call encabezado
    WRITE(17,'(A19)')'NODATA_value  -9999'
    DO j=1,mji
        WRITE(17,233) (mask(i,j),i=1,mii)
    ENDDO
    CLOSE(17)
ENDIF

!Nitrógeno orgánico en fase sólida (sedimentos depositados) en cauce Final - hn11 - kg
IF (finalesasciinitr(12)) THEN
    filename=TRIM(ADJUSTL(dirtra))//'_ASCII'//path_separator//'EFN12_'//fecha//'.asc'
    filename=TRIM(ADJUSTL(filename))
    mask=-9999.0
    DO n=1,ncel
           mask(cell(n).fil,cell(n).col)=cell(n).hn(11)
    ENDDO
    OPEN(17,file=filename, status='unknown')
    Call encabezado
    WRITE(17,'(A19)')'NODATA_value  -9999'
    DO j=1,mji
        WRITE(17,233) (mask(i,j),i=1,mii)
    ENDDO
    CLOSE(17)
ENDIF

!Amonio en fase sólida (sedimentos depositados) en cauce Final - hn12 - kg
IF (finalesasciinitr(13)) THEN
    filename=TRIM(ADJUSTL(dirtra))//'_ASCII'//path_separator//'EFN13_'//fecha//'.asc'
    filename=TRIM(ADJUSTL(filename))
    mask=-9999.0
    DO n=1,ncel
           mask(cell(n).fil,cell(n).col)=cell(n).hn(12)
    ENDDO
    OPEN(17,file=filename, status='unknown')
    Call encabezado
    WRITE(17,'(A19)')'NODATA_value  -9999'
    DO j=1,mji
        WRITE(17,233) (mask(i,j),i=1,mii)
    ENDDO
    CLOSE(17)
ENDIF

!IMPRIMIMOS EFICIENCIAS DE NITROGENO ACUMULADAS

IF (finalesasciinitr(14)) THEN
    filename=TRIM(ADJUSTL(dirtra))//'_ASCII'//path_separator//'E1T_'//fecha//'.asc'
    filename=TRIM(ADJUSTL(filename))
    mask=-9999.0
    DO n=1,ncel
         mask(cell(n).fil,cell(n).col)=cell(n).allUptake/cell(n).allInput
    ENDDO
    OPEN(17,file=filename, status='unknown')
    Call encabezado
    WRITE(17,'(A19)')'NODATA_value  -9999'
    DO j=1,mji
        WRITE(17,233) (mask(i,j),i=1,mii)
    ENDDO
ENDIF

IF (finalesasciinitr(15)) THEN
    filename=TRIM(ADJUSTL(dirtra))//'_ASCII'//path_separator//'E2T_'//fecha//'.asc'
    filename=TRIM(ADJUSTL(filename))
    mask=-9999.0
    DO n=1,ncel
         mask(cell(n).fil,cell(n).col)=cell(n).allPercolation/cell(n).allInput
    ENDDO
    OPEN(17,file=filename, status='unknown')
    Call encabezado
    WRITE(17,'(A19)')'NODATA_value  -9999'
    DO j=1,mji
        WRITE(17,233) (mask(i,j),i=1,mii)
    ENDDO    
ENDIF

End subroutine