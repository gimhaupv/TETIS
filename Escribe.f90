
!******************************************************************
!* Coleccion de rutinas de escritura para el modelo TETIS v9
!* Escribe CALIB.TXT
!******************************************************************
SUBROUTINE escri_calib
USE modtet
IMPLICIT NONE

OPEN (11,file=artem)
DO i=1,10
   WRITE(11,44)r(i)
ENDDO
WRITE(11,45)bbeta
WRITE(11,45)ro1     !sin lluvia
WRITE(11,45)ro2     !con lluvia
WRITE(11,45)tbase
WRITE(11,44)betappt
WRITE(11,44)rsed(1)
WRITE(11,44)rsed(2)
WRITE(11,44)rsed(3)
WRITE(11,45)expinf
WRITE(11,45)alpha
WRITE(11,45)betatemp
44 FORMAT(F19.7)
45 FORMAT(F16.5)
CLOSE(11)

END SUBROUTINE

!******************************************************************
!* Escribe el fichero de humedad, sea inicial o final, (codf)
!******************************************************************
SUBROUTINE escr_wdad
USE modtet
IMPLICIT NONE

OPEN(16,file=artem) 
  IF (lang.eq.1) THEN
    WRITE (16,'(A12,2x,2(x,F14.4))')'NORTE-SUR:  ',cn,cs
    WRITE (16,'(A12,2x,2(x,F14.4))')'ESTE-OESTE: ',ce,cw
    WRITE (16,'(A12,3x,I8)')'COLUMNAS:   ',mi
    WRITE (16,'(A12,3x,I8)')'FILAS:      ',mj
    WRITE (16,'(A12,3x,I8)')'COL-FINAL:  ',ncol
    WRITE (16,'(A12,3x,I8)')'FIL-FINAL:  ',nfil
    WRITE (16,'(A12,3x,I8)')'NUM-CELDAS: ',ncel
    WRITE (16,'(A12,A11,x,A8)')'FECHA-HORA: ',fecfin,horfin
  ELSE IF (lang.eq.2) THEN
    WRITE (16,'(A12,2x,2(x,F14.4))')'NORTH-SOUTH:',cn,cs
    WRITE (16,'(A12,2x,2(x,F14.4))')'EAST-WEST:  ',ce,cw
    WRITE (16,'(A12,3x,I8)')'COLUMNS:    ',mi
    WRITE (16,'(A12,3x,I8)')'ROWS:       ',mj
    WRITE (16,'(A12,3x,I8)')'COL-FINAL:  ',ncol
    WRITE (16,'(A12,3x,I8)')'ROW-FINAL:  ',nfil
    WRITE (16,'(A12,3x,I8)')'CELL-NUMBER:',ncel
    WRITE (16,'(A12,A11,x,A8)')'DATE-TIME:  ',fecfin,horfin
  ENDIF
!Guiomar (28/01/2014): Voy a introducir un cambio para que en este archivo de humedades finales también salga la humedad del primer tanque estático h(8). 
!Para mantener un hantec2 parecido al que se utilizaría sin vegetación dinámica, he puesto en primera columna la suma de los dos tanques estáticos y al final cada uno por separado
IF (config(5)) THEN
    DO n=1,ncel
        WRITE (16,104)cell(n).h(8)+cell(n).h(1),cell(n).h(2),cell(n).h(3),cell(n).h(4),cell(n).h(5),cell(n).h(0),cell(n).h(6),cell(n).h(8),cell(n).h(1),cell(n).lai
    ENDDO
    104 FORMAT (10(F14.4,x))
    CLOSE(16)
ELSE
    DO n=1,ncel
  WRITE (16,103)cell(n).h(1),cell(n).h(2),cell(n).h(3),cell(n).h(4),cell(n).h(5),cell(n).h(0),cell(n).h(6)
    ENDDO
    103 FORMAT (7(F14.4,x))
    CLOSE(16)
ENDIF
END SUBROUTINE

!***************************************
!* Escribe Parametros Geomorfologicos
!***************************************
 SUBROUTINE escri_parg
USE modtet
IMPLICIT NONE

OPEN (12,file=artem,status='replace')
if (npar.eq.0)npar=1
WRITE(12,*)npar 
DO i=1,6
  IF (i.ne.2) THEN
    IF (i.ne.4)THEN
      WRITE(12,432)(d(i,j),j=1,npar)
    ENDIF
    WRITE(12,432)(e(i,j),j=1,npar)
  ENDIF
ENDDO
DO i=1,6
  IF (i.ne.2) THEN
    IF (i.ne.4)THEN
      WRITE(12,432)(dc(i,j),j=1,npar)
    ENDIF
    WRITE(12,432)(ec(i,j),j=1,npar)
  ENDIF
ENDDO
WRITE(12,*) nest,bint,betalin
DO i=1,6  
  WRITE(12,432)(wdad(i,j),j=1,npar)
ENDDO
WRITE(12,432) hped
DO i=1,3
  WRITE(12,432)(areaumbral(i,j),j=1,npar)
ENDDO
!Guiomar (29/01/2014): Tengo que añadir las tres líneas correspondientes a los estados iniciales del modelo de vegetación
IF (config(5)) THEN
    WRITE(12,432)(wdad(7,j),j=1,npar)
    WRITE(12,432)(wdad(8,j),j=1,npar)
    WRITE(12,432)(laiini(j),j=1,npar)
ENDIF
CLOSE(12)
432 FORMAT(<npar>(F14.8,x))

END SUBROUTINE

!Cris (07/10/2016) No se está utilizando la subrutina "escribmapij", la desactivo.
!!*****************************************************
!!* Escribe un mapa de datos (*.dtm) en formato GRASS
!!*****************************************************
!SUBROUTINE escribmapij(artem,mi,mj,ce,cn,cs,cw,matriz)
!IMPLICIT NONE
!
!INTEGER mi,mj,i,j,matriz(mi,mj),nz,np
!REAL cn,cs,ce,cw
!CHARACTER artem*128
!CHARACTER*8 h1,h2,h3,h4,h5,h6,h7,h8
!
!OPEN (unit=15,file=artem,status='unknown') 
!WRITE(15,*)'proj:       0'
!WRITE(15,*)'zone:       0'
!WRITE(15,*)'north:    ',cn
!WRITE(15,*)'south:    ',cs
!WRITE(15,*)'east:     ',ce
!WRITE(15,*)'west:     ',cw
!WRITE(15,*)'cols:     ',mi
!WRITE(15,*)'rows:     ',mj
!
!DO j=1,mj
!  WRITE(15,105)(matriz(i,j),i=1,mi) 
!END DO
!CLOSE(15) 
!
!105 FORMAT(<mi>i10)
!      
!
!END SUBROUTINE

!!****************************************************************
!!* Escribe fichero con formato expotacion Arc/Info (*.ASC)
!!*Se utiliza en el módulo de nieve
!!****************************************************************
SUBROUTINE escribascint(mask)
USE modtet
IMPLICIT NONE

REAL temp
INTEGER cwi, csi, dxi,mask(mi,mj)


mii = INT(mi)
mji = INT(mj)
cwi = INT(cw)
csi = INT(cs)
dxi = INT(dx)

OPEN(17,file=artem, status='unknown')
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
SELECT CASE(cwi)
    CASE(:9)
        WRITE(17,'(A14,I1)')'xllcorner     ',cwi
    CASE(10:99)
        WRITE(17,'(A14,I2)')'xllcorner     ',cwi
    CASE(100:999)
        WRITE(17,'(A14,I3)')'xllcorner     ',cwi
    CASE(1000:9999)
        WRITE(17,'(A14,I4)')'xllcorner     ',cwi
    CASE(10000:99999)
        WRITE(17,'(A14,I5)')'xllcorner     ',cwi
    CASE(100000:999999)
        WRITE(17,'(A14,I6)')'xllcorner     ',cwi
    CASE(1000000:9999999)
        WRITE(17,'(A14,I7)')'xllcorner     ',cwi
    CASE(10000000:99999999)
        WRITE(17,'(A14,I8)')'xllcorner     ',cwi
END SELECT
SELECT CASE(csi)
    CASE(:9)
        WRITE(17,'(A14,I1)')'yllcorner     ',csi
    CASE(10:99)
        WRITE(17,'(A14,I2)')'yllcorner     ',csi
    CASE(100:999)
        WRITE(17,'(A14,I3)')'yllcorner     ',csi
    CASE(1000:9999)
        WRITE(17,'(A14,I4)')'yllcorner     ',csi
    CASE(10000:99999)
        WRITE(17,'(A14,I5)')'yllcorner     ',csi
    CASE(100000:999999)
        WRITE(17,'(A14,I6)')'yllcorner     ',csi
    CASE(1000000:9999999)
        WRITE(17,'(A14,I7)')'yllcorner     ',csi
    CASE(10000000:99999999)
        WRITE(17,'(A14,I8)')'yllcorner     ',csi
END SELECT
SELECT CASE(dxi)
    CASE(:9)
        WRITE(17,'(A14,I1)')'cellsize      ',dxi
    CASE(10:99)
        WRITE(17,'(A14,I2)')'cellsize      ',dxi
    CASE(100:999)
        WRITE(17,'(A14,I3)')'cellsize      ',dxi
    CASE(1000:9999)
        WRITE(17,'(A14,I4)')'cellsize      ',dxi
    CASE(10000:99999)
        WRITE(17,'(A14,I5)')'cellsize      ',dxi
    CASE(100000:999999)
        WRITE(17,'(A14,I6)')'cellsize      ',dxi
    CASE(1000000:9999999)
        WRITE(17,'(A14,I7)')'cellsize      ',dxi
    CASE(10000000:99999999)
        WRITE(17,'(A14,I8)')'cellsize      ',dxi
END SELECT  
WRITE(17,'(A19)')'NODATA_value  -9999'
DO j=1,mji
    WRITE(17,233)(mask(i,j),i=1,mii)
ENDDO
CLOSE(17)
233 FORMAT(<mii>(I8,x))
END SUBROUTINE


!Esta subrutina es la que hacía que se imprimiesen los mapas de forma encadenada, pero no he logrado hacerlo
!si declaras mask como allocatable para que pueda imprimir cualquier tamaño de mapa. Por ahora la desactivo.
!La subrutina que imprime mapas ahora esta en el archivo PrintAscii.f90    Cris(07/10/2016)
!!****************************************************************
!!* Escribe fichero con formato expotacion Arc/Info (*.ASC)
!!****************************************************************
!SUBROUTINE escribasc(artem,mi,mj,cw,cs,dx,mask)
!IMPLICIT NONE
!
!
!INTEGER mi,mj,i,j
!REAL dx,cw,cs,temp,mask(mi,mj)
!INTEGER mii, mji, cwi, csi, dxi
!
!CHARACTER artem*128
!
!mii = INT(mi)
!mji = INT(mj)
!cwi = INT(cw)
!csi = INT(cs)
!dxi = INT(dx)
!
!OPEN(17,file=artem, status='unknown')
!SELECT CASE(mii)
!    CASE(:9)
!        WRITE(17,'(A14,I1)')'ncols         ',mii
!    CASE(10:99)
!        WRITE(17,'(A14,I2)')'ncols         ',mii
!    CASE(100:999)
!        WRITE(17,'(A14,I3)')'ncols         ',mii
!    CASE(1000:9999)
!        WRITE(17,'(A14,I4)')'ncols         ',mii
!    CASE(10000:99999)
!        WRITE(17,'(A14,I5)')'ncols         ',mii
!    CASE(100000:999999)
!        WRITE(17,'(A14,I6)')'ncols         ',mii
!    CASE(1000000:9999999)
!        WRITE(17,'(A14,I7)')'ncols         ',mii
!    CASE(10000000:99999999)
!        WRITE(17,'(A14,I8)')'ncols         ',mii
!END SELECT
!SELECT CASE(mji)
!    CASE(:9)
!        WRITE(17,'(A14,I1)')'nrows         ',mji
!    CASE(10:99)
!        WRITE(17,'(A14,I2)')'nrows         ',mji
!    CASE(100:999)
!        WRITE(17,'(A14,I3)')'nrows         ',mji
!    CASE(1000:9999)
!        WRITE(17,'(A14,I4)')'nrows         ',mji
!    CASE(10000:99999)
!        WRITE(17,'(A14,I5)')'nrows         ',mji
!    CASE(100000:999999)
!        WRITE(17,'(A14,I6)')'nrows         ',mji
!    CASE(1000000:9999999)
!        WRITE(17,'(A14,I7)')'nrows         ',mji
!    CASE(10000000:99999999)
!        WRITE(17,'(A14,I8)')'nrows         ',mji
!END SELECT
!SELECT CASE(cwi)
!    CASE(0:9)
!        WRITE(17,'(A14,I1)')'xllcorner     ',cwi
!    CASE(-9:-1,10:99)
!        WRITE(17,'(A14,I2)')'xllcorner     ',cwi
!    CASE(-99:-10,100:999)
!        WRITE(17,'(A14,I3)')'xllcorner     ',cwi
!    CASE(-999:-100,1000:9999)
!        WRITE(17,'(A14,I4)')'xllcorner     ',cwi
!    CASE(-9999:-1000,10000:99999)
!        WRITE(17,'(A14,I5)')'xllcorner     ',cwi
!    CASE(-99999:-10000,100000:999999)
!        WRITE(17,'(A14,I6)')'xllcorner     ',cwi
!    CASE(-999999:-100000,1000000:9999999)
!        WRITE(17,'(A14,I7)')'xllcorner     ',cwi
!    CASE(-9999999:-1000000,10000000:99999999)
!        WRITE(17,'(A14,I8)')'xllcorner     ',cwi
!END SELECT
!SELECT CASE(csi)
!    CASE(0:9)
!        WRITE(17,'(A14,I1)')'yllcorner     ',csi
!    CASE(-9:-1,10:99)
!        WRITE(17,'(A14,I2)')'yllcorner     ',csi
!    CASE(-99:-10,100:999)
!        WRITE(17,'(A14,I3)')'yllcorner     ',csi
!    CASE(-999:-100,1000:9999)
!        WRITE(17,'(A14,I4)')'yllcorner     ',csi
!    CASE(-9999:-1000,10000:99999)
!        WRITE(17,'(A14,I5)')'yllcorner     ',csi
!    CASE(-99999:-10000,100000:999999)
!        WRITE(17,'(A14,I6)')'yllcorner     ',csi
!    CASE(-999999:-100000,1000000:9999999)
!        WRITE(17,'(A14,I7)')'yllcorner     ',csi
!    CASE(-9999999:-1000000,10000000:99999999)
!        WRITE(17,'(A14,I8)')'yllcorner     ',csi
!END SELECT
!SELECT CASE(dxi)
!    CASE(:9)
!        WRITE(17,'(A14,I1)')'cellsize      ',dxi
!    CASE(10:99)
!        WRITE(17,'(A14,I2)')'cellsize      ',dxi
!    CASE(100:999)
!        WRITE(17,'(A14,I3)')'cellsize      ',dxi
!    CASE(1000:9999)
!        WRITE(17,'(A14,I4)')'cellsize      ',dxi
!    CASE(10000:99999)
!        WRITE(17,'(A14,I5)')'cellsize      ',dxi
!    CASE(100000:999999)
!        WRITE(17,'(A14,I6)')'cellsize      ',dxi
!    CASE(1000000:9999999)
!        WRITE(17,'(A14,I7)')'cellsize      ',dxi
!    CASE(10000000:99999999)
!        WRITE(17,'(A14,I8)')'cellsize      ',dxi
!END SELECT  
!WRITE(17,'(A19)')'NODATA_value  -9999'
!DO j=1,mji
!    WRITE(17,233)(mask(i,j),i=1,mii)
!    WRITE(17,233) (mask(i,j),i=1,mii)
!ENDDO
!CLOSE(17)
!
!233 FORMAT(F12.5)
!234 FORMAT(<mii>(ES12.5,1x))
!END SUBROUTINE

!***************************************
!** Formato de Impresión en columna
!***************************************
SUBROUTINE escribe_col
USE modtet
IMPLICIT NONE

INTEGER newnt,ktot2,posQ1,posW1
REAL tpo,sum2,sumed,sumed2,valmax,valmin,mtpo,temp,sumxy,sumx,sumy,sumx2,sumy2,denominador
CHARACTER, ALLOCATABLE:: codtex*3(:),serieChar*12(:,:)

!Escribe a un fichero de resultados 
OPEN(21,file=arch(9))
    IF (lang.eq.1) THEN
        WRITE (21,'(A2)') '* '
        WRITE (21,'(A55)') '* MODELACION HIDROLOGICA DISTRIBUIDA DE TIPO CONCEPTUAL'
        WRITE (21,'(A50)') '* MODELO DE SIMULACION - T E T I S   v.9 -      '
        WRITE (21,'(A2)') '* '
        WRITE (21,'(A20)') '* Desarrollado en:  '
        WRITE (21,'(A55)') '* UNIVERSITAT POLITÈCNICA DE VALÈNCIA  '
        WRITE (21,'(A63)') '* Instituto de ingeniería del Agua y Medio Ambiente (IIAMA)'
        WRITE (21,'(A57)') '* Grupo de Investigación en Modelación Hidrológica y Medioambiental (GIHMA)'
        WRITE (21,'(A2)') '* '
        WRITE (21,'(A45)') '*  DATOS DEL MODELO TETIS EN FORMATO COLUMNA '
        WRITE (21,'(A2)') '* '
        WRITE (21,'(A25,A128)')'* Directorio de Trabajo: ',dirtra
        WRITE (21,'(A2)') '* '
        !Escibe la fecha de inicio del episodio 
        WRITE (21,'(A48)') '* Fecha de inicio del episodio (dd-mm-aa  hh:mm)'
        WRITE(21,'(A2,2x,A2,A1,A2,A1,A4,2x,A2,A1,A2)') 'F ',archin(1:2),'-',archin(3:4),'-',archin(11:14),archin(5:6),':',archin(7:8)
        WRITE (21,'(A2)') '* '
        WRITE (21,'(A48)')'* Numero de datos - Intervalo temporal (minutos)'
        WRITE (21,'(A2,2x,I9,x,I10)') 'G ',nt,INT(dtmin)
        WRITE (21,'(A26,F15.3)') '* Área de la cuenca (km²) ',ncel*arcelkm
        WRITE (21,'(A19,I10)') '* Número de celdas ',ncel
        WRITE (21,'(A29,I2)') '* Estaciones para interpolar: ', nest  
        WRITE (21,'(A22,I2)') '* Regiones homogéneas: ',npar          
        WRITE (21,'(A2)') '* '
        !Escribe localizacion de las estaciones
        !El fichero de columna debe tener el orden P,N,V,S,Q,B,H,T,E,D
        WRITE(21,'(A45)') '* RESUMEN DE INFORMACION SOBRE LAS ESTACIONES'
        WRITE (21,'(A2)') '* '
        WRITE(21,'(A70)') '* "Nombre de la estación    "   Este(UTM-X) Norte(UTM-Y)   Cota(msnm) '
    ELSE IF (lang.eq.2) THEN
        WRITE (21,'(A2)') '* '
        WRITE (21,'(A55)') '* DISTRIBUTED CONCEPTUAL HYDROLOGICAL MODELLING        '
        WRITE (21,'(A50)') '* MODEL T E T I S   v.9 -                       '
        WRITE (21,'(A2)') '* '
        WRITE (21,'(A20)') '* Developed at:     '
        WRITE (21,'(A55)') '* UNIVERSITAT POLITÈCNICA DE VALÈNCIA  '
        WRITE (21,'(A63)') '* Instituto de ingeniería del Agua y Medio Ambiente (IIAMA)'
        WRITE (21,'(A57)') '* Grupo de Investigación en Modelación Hidrológica y Medioambiental (GIHMA)'
        WRITE (21,'(A2)') '* '
        WRITE (21,'(A45)') '*  TETIS MODEL RESULTS IN COLUMN FORMAT      '
        WRITE (21,'(A2)') '* '
        WRITE (21,'(A25,A128)')'* Working directory:     ',dirtra
        WRITE (21,'(A2)') '* '
        !Escibe la fecha de inicio del episodio 
        WRITE (21,'(A48)') '* Starting date (dd-mm-yy  hh:mm)               '
        WRITE(21,'(A2,2x,A2,A1,A2,A1,A4,2x,A2,A1,A2)') 'F ',archin(1:2),'-',archin(3:4),'-',archin(11:14),archin(5:6),':',archin(7:8)
        WRITE (21,'(A2)') '* '
        WRITE (21,'(A48)')'* Number of data  - Time step          (minutes)'
        WRITE (21,'(A2,2x,I9,x,I10)') 'G ',nt,INT(dtmin)
        WRITE (21,'(A26,F15.3)') '* Catchment area    (km²) ',ncel*arcelkm
        WRITE (21,'(A19,I10)') '* Cell number      ',ncel
        WRITE (21,'(A22,I2)') '* Homogeneous regions: ',npar         
        WRITE (21,'(A29,I2)') '* Stations to interpolate: ', nest       
        WRITE (21,'(A2)') '* '
        !Escribe localizacion de las estaciones
        !El fichero de columna debe tener el orden P,N,V,S,Q,B,H,T,E,D
        WRITE(21,'(A45)') '* INFORMATION ABOUT STATIONS                 '
        WRITE (21,'(A2)') '* '
        WRITE(21,'(A70)') '* "Station name             "   East(UTM-X) North(UTM-Y) Height(msnm) '
    ENDIF    
DO i=1,kppt
  WRITE(21,12)pluvio(i).codigo,'"',pluvio(i).name,'"',  &
       INT(pluvio(i).utmx),INT(pluvio(i).utmy),INT(pluvio(i).elev) 
ENDDO
DO i=1,nemb
  WRITE(21,12)nivel(i).codigo,'"',nivel(i).name,'"',  &
      INT(nivel(i).utmx),INT(nivel(i).utmy),INT(nivel(i).elev)
ENDDO
DO i=nemb+1,nemb+vnemb
  WRITE(21,12)volum(i).codigo,'"',volum(i).name,'"',  &
      INT(volum(i).utmx),INT(volum(i).utmy),INT(volum(i).elev) 
ENDDO
DO i=nemb+vnemb+1,nemb+vnemb+knemb
  WRITE(21,12)qemb(i).codigo,'"',qemb(i).name,'"',  &
      INT(qemb(i).utmx),INT(qemb(i).utmy),INT(qemb(i).elev) 
ENDDO
DO i=1,naf
  WRITE(21,12)aforo(i).codigo,'"',aforo(i).name,'"',  &
      INT(aforo(i).utmx),INT(aforo(i).utmy),INT(aforo(i).elev) 
ENDDO
DO i=1,knaf
  WRITE(21,12)otros(i).codigo,'"',otros(i).name,'"',  &
      INT(otros(i).utmx),INT(otros(i).utmy),INT(otros(i).elev) 
ENDDO
DO i=1,kniv
  WRITE(21,12)nieve(i).codigo,'"',nieve(i).name,'"',  &
     INT(nieve(i).utmx),INT(nieve(i).utmy),INT(nieve(i).elev) 
ENDDO
DO i=1,ktem
  WRITE(21,12)temper(i).codigo,'"',temper(i).name,'"',  &
     INT(temper(i).utmx),INT(temper(i).utmy),INT(temper(i).elev) 
ENDDO
DO i=1,kevp
  WRITE(21,12)evapo(i).codigo,'"',evapo(i).name,'"',  &
     INT(evapo(i).utmx),INT(evapo(i).utmy),INT(evapo(i).elev) 
ENDDO
DO i=1,kadi
  WRITE(21,12)aport(i).codigo,'"',aport(i).name,'"',  &
     INT(aport(i).utmx),INT(aport(i).utmy),INT(aport(i).elev) 
ENDDO
!Guiomar (29/01/2014): Añado los datos de las estaciones de vegetación en el caso de que esté activada la vegetación dinámica
IF (config(5)) THEN
    DO i=1,nveg
        WRITE(21,12)veg(i).codigo,'"',veg(i).name,'"',  &
                     INT(veg(i).utmx),INT(veg(i).utmy),INT(veg(i).elev) 
    ENDDO
ENDIF

12 FORMAT(A2,A1,A25,A1,x,3(x,I12))

WRITE (21,'(A2)') '* '
!Guiomar (29/01/2014): las dimensiones de serie y de codtex deben añadir la vegetación en el caso de estar activada la vegetación dinámica
IF (config(5)) THEN
    ALLOCATE(serie(nt+11,ktotal+naf+kniv+kadi+nemb*6+knemb*6+vnemb*6+nveg*4))!(Guiomar-Vicente) Multipicamos nveg*4 para considerar las 4 variables de VEG
    ALLOCATE(codtex(ktotal+naf+kniv+kadi+nemb*6+knemb*6+vnemb*6+nveg*4)) !(Guiomar-Vicente) Multipicamos nveg*4 para considerar las 4 variables de VEG
ELSE
    ALLOCATE(serie(nt+10,ktotal+naf+kniv+kadi+nemb*6+knemb*6+vnemb*6))
    ALLOCATE(codtex(ktotal+naf+kniv+kadi+nemb*6+knemb*6+vnemb*6))
ENDIF
ALLOCATE(serieChar(12,ktotal+naf+kniv+kadi+nemb*6+knemb*6+vnemb*6+nveg*4))!(Vicente) Se emplea una matriz de Character para mostrar resultados
    

!Rellena los datos y resultados en una sola matriz
serie=0.0
serieChar='      -     '
codtex='--'
k=0
DO i=1,kppt    !PPT
  k=k+1
  codtex(k)='P '
  DO t=1,nt
    serie(t,k)=pluvio(i).obs(t)
  ENDDO
ENDDO

DO i=1,nemb+vnemb+knemb     !Embalses

  IF (emb(i).caso.gt.0) THEN
    k=k+1
    codtex(k)='No '
    DO t=1,nt
      serie(t,k)=nivel(i).obs(t)
    ENDDO
    k=k+1
    codtex(k)='Ns '
    DO t=1,nt
      serie(t,k)=nivel(i).sim(t)
    ENDDO
    k=k+1
    codtex(k)='Ib '
    DO t=1,nt
      serie(t,k)=qemb(i).bal(t)
    ENDDO
    k=k+1
    codtex(k)='Is '
    DO t=1,nt
      serie(t,k)=qemb(i).sim(t)
    ENDDO
    k=k+1
    codtex(k)='Vo '
    DO t=1,nt
      serie(t,k)=volum(i).obs(t)
    ENDDO
    k=k+1
    codtex(k)='Vs '
    DO t=1,nt
      serie(t,k)=volum(i).sim(t)
    ENDDO
    k=k+1
    codtex(k)='So '
    DO t=1,nt
      serie(t,k)=qemb(i).obs(t)
    ENDDO
  ENDIF
ENDDO
posQ1=k
DO i=1,naf     !Aforos
  k=k+1
  codtex(k)='Qo '
  DO t=1,nt
    serie(t,k)=aforo(i).obs(t)
  ENDDO
  k=k+1
  codtex(k)='Qs '
  DO t=1,nt
    serie(t,k)=aforo(i).sim(t)
  ENDDO
ENDDO
DO i=1,knaf     !Sin aforo
  k=k+1
  codtex(k)='Bs '
  DO t=1,nt
	serie(t,k)=otros(i).sim(t)
  ENDDO
ENDDO
DO i=1,kniv     !Nieve
  k=k+1
  codtex(k)='H  '
  DO t=1,nt
    serie(t,k)=balanc(t,18)
  ENDDO
  k=k+1
  codtex(k)='Z  '
  DO t=1,nt
    serie(t,k)=nieve(i).sim(t)
  ENDDO
ENDDO
DO i=1,ktem     !Temperatura
  k=k+1
  codtex(k)='T  '
  DO t=1,nt
    serie(t,k)=temper(i).obs(t)
  ENDDO
ENDDO
DO i=1,kevp     !Evapotranspiracion
  k=k+1
  codtex(k)='E  '
  DO t=1,nt
    serie(t,k)=evapo(i).obs(t)
  ENDDO
ENDDO
DO i=1,kadi     !Adicional
  k=k+1
  codtex(k)='D '
  DO t=1,nt
    serie(t,k)=aport(i).obs(t)
  ENDDO
  k=k+1
  codtex(k)='Ds '
  DO t=1,nt
    serie(t,k)=aport(i).sim(t)
  ENDDO
ENDDO
!Guiomar (29/01/2014): Añado lo correspondiente a las estaciones de vegetación en el caso de que esté activada la vegetación dinámica
IF (config(5)) THEN
    posW1=k
    DO i=1,nveg
        k=k+1
        codtex(k)='Wo '
        DO t=1,nt
            serie(t,k)=veg(i).obs(t)
        ENDDO
        k=k+1
        !(Guiomar-Vicente)
        codtex(k)='Ws '
        DO t=1,nt
            serie(t,k)=veg(i).sim(t)
        ENDDO
        k=k+1
        !(Guiomar-Vicente)
        !codtex(k)='Ws '
        !DO t=1,nt
        !    serie(t,k)=veg1_point(i).sim(t)
        !ENDDO
        !k=k+1
        codtex(k)='Wr '
        DO t=1,nt
            serie(t,k)=veg2_point(i).sim(t)
        ENDDO
        k=k+1
        codtex(k)='Tr '
        DO t=1,nt
            serie(t,k)=tr_point(i).sim(t)
        ENDDO
    ENDDO
ENDIF
ktot2=k
!Calcula estadisticos de las series
k=0
kveg=0
DO i=1,ktot2
  sum=0.0
  sum2=0.0
  sumed2=0.0
  sumed=0.0
  valmax=0.0
  valmin=999999.
  newnt=0
  IF (codtex(i).eq.'T  ') THEN
    DO t=1,nt
      IF (serie(t,i).gt.-90) THEN
  	    newnt=newnt+1
  	    sum=sum+serie(t,i)
  	    sum2=sum2+serie(t,i)**2.0
      IF (serie(t,i).gt.valmax) THEN
  	    valmax=serie(t,i)
  	    mtpo=t*dt
  	  ENDIF
  	    IF (serie(t,i).lt.valmin) valmin=serie(t,i)
  	    ENDIF
    ENDDO
  ELSEIF (codtex(i).eq.'Qo ') THEN
    k=k+1
    tpo=0.0
    newnt=0
    DO t=1,nt
  	    IF (aforo(k).obs(t).ge.0.0) THEN    !Condicional agregado para no contabilizar los faltantes
            serie(nt+7,i)=serie(nt+7,i)+(aforo(k).obs(t)-aforo(k).sim(t))**2.0 !Varianza de los errores para NSE
            tpo=tpo+aforo(k).obs(t)
            newnt=newnt+1
        ENDIF
    ENDDO
    tpo=tpo/newnt !Valor medio del caudal para el NSE
    DO t=1,nt
      IF (aforo(k).obs(t).ne.-1) serie(nt+8,i)=serie(nt+8,i)+(aforo(k).obs(t)-tpo)**2.0 !Varianza de los Qobs para NSE
    ENDDO
    
    !Indice sugerido por Moriasi 2007, RMSE-observations standard deviation ratio
    serie(nt+10,i)=(serie(nt+7,i))**0.5/(serie(nt+8,i))**0.5
    
    IF (serie(nt+8,i).gt.0.0) serie(nt+8,i)=1.0-(serie(nt+7,i)/serie(nt+8,i))
    serie(nt+7,i)=(serie(nt+7,i)/newnt)**0.5 !RMSE
    newnt=0
     DO t=1,nt
        IF (serie(t,i).ge.0.0) THEN
  	        newnt=newnt+1
  	        sum=sum+serie(t,i)
  	        sum2=sum2+serie(t,i)**2.0
  	        IF (serie(t,i).gt.valmax) THEN
  	            valmax=serie(t,i)
  	            mtpo=t*dt
  	        ENDIF
  	        IF (serie(t,i).lt.valmin) valmin=serie(t,i)
  	    ENDIF
    ENDDO
!Guiomar (30/01/2014): voy a introducir los estadísticos que creo que deberían calcularse para el caso de la vegetación (RMSE y coeficiente de correlación)
ELSEIF (codtex(i).eq.'Ws ') THEN
    kveg=kveg+1
    tpo=0.0
    newnt=0
    sumxy=0
    sumx=0
    sumy=0
    sumx2=0
    sumy2=0
    DO t=1,nt
  	    IF (veg(kveg).obs(t).ge.0.0) THEN
            serie(nt+7,i)=serie(nt+7,i)+(veg(kveg).obs(t)-veg(kveg).sim(t))**2.0 !(observado-simulado)^2
            newnt=newnt+1
            sumxy=sumxy+veg(kveg).obs(t)*veg(kveg).sim(t) !sumatorio de (observado*simulado) Esto es para calcular el coeficiente de correlación
            sumx=sumx+veg(kveg).obs(t) !sumatorio de observado
            sumy=sumy+veg(kveg).sim(t) !sumatorio de simulado
            sumx2=sumx2+veg(kveg).obs(t)*veg(kveg).obs(t) !sumatorio de observado^2
            sumy2=sumy2+veg(kveg).sim(t)*veg(kveg).sim(t) !sumatorio de simulado^2
        ENDIF
    ENDDO
    serie(nt+7,i)=(serie(nt+7,i)/newnt)**0.5 !RMSE
    !Ahora pasamos al cálculo de el coeficiente de correlación
    IF (((newnt*sumx2-sumx*sumx).gt.0.0).AND.((newnt*sumy2-sumy*sumy).gt.0.0)) THEN !Guiomar: Compruebo que lo de dentro de la raíz no es negativo
        denominador=((newnt*sumx2-sumx*sumx)**0.5)*((newnt*sumy2-sumy*sumy)**0.5)
        IF (denominador.ne.0.0) THEN
            serie(nt+11,i)=(newnt*sumxy-sumx*sumy)/denominador
  ELSE
            serie(nt+11,i)=-99999.99
        ENDIF
    ELSE
        serie(nt+11,i)=-99999.99
    ENDIF
    newnt=0
    DO t=1,nt
        IF (serie(t,i).ge.0.0) THEN
  	        newnt=newnt+1
  	        sum=sum+serie(t,i)
  	        sum2=sum2+serie(t,i)**2.0
  	        IF (serie(t,i).gt.valmax) THEN
  	            valmax=serie(t,i)
  	            mtpo=t*dt
  	        ENDIF
  	        IF (serie(t,i).lt.valmin) valmin=serie(t,i)
  	    ENDIF
    ENDDO
  ENDIF
  serie(nt+1,i)=sum
  serie(nt+2,i)=valmax
  serie(nt+9,i)=mtpo
  serie(nt+3,i)=valmin
  serie(nt+4,i)=-99999.99
  IF (newnt.eq.0) THEN
    serie(nt+4,i)=-99999.99
    serie(nt+5,i)=-99999.99
  ELSEIF (newnt.eq.1) THEN
    serie(nt+4,i)=-99999.99
    serie(nt+5,i)=sum
  ELSE
    IF ((newnt*sum2-sum**2.)/(newnt*(newnt-1.)).gt.0.0) serie(nt+4,i)=((newnt*sum2-sum**2.0)/(newnt*(newnt-1.0)))**0.5
    serie(nt+5,i)=sum/newnt
  ENDIF
  serie(nt+6,i)=REAL(newnt)
ENDDO

!Escribe series temporales en el mismo orden
!WRITE (21,'(A30)') '* SERIES TEMPORALES DE LLUVIA ' 

DO i=1,naf
   WRITE(serieChar(1,2*(i-1)+posQ1+1),'(F12.4)')estad(i,1)
   WRITE(serieChar(2,2*(i-1)+posQ1+2),'(F12.4)')estad(i+naf,1)
   WRITE(serieChar(3,2*(i-1)+posQ1+1),'(F12.4)')estad(i,4)
   WRITE(serieChar(4,2*(i-1)+posQ1+1),'(F12.4)')estad(i,2)
   WRITE(serieChar(5,2*(i-1)+posQ1+2),'(F12.4)')estad(i+naf,2)
   WRITE(serieChar(6,2*(i-1)+posQ1+1),'(F12.4)')estad(i,5)
   WRITE(serieChar(7,2*(i-1)+posQ1+1),'(F12.4)')estad(i,3)
   WRITE(serieChar(8,2*(i-1)+posQ1+2),'(F12.4)')estad(i+naf,3)
   WRITE(serieChar(9,2*(i-1)+posQ1+1),'(F12.4)')estad(i,6)
   WRITE(serieChar(10,2*(i-1)+posQ1+1),'(F12.4)')estad(i+naf,4)
   WRITE(serieChar(11,2*(i-1)+posQ1+1),'(F12.4)')aforo(i).area
   WRITE(serieChar(12,2*(i-1)+posQ1+1),'(F12.4)')RSRindex(i)        
END DO

IF(config(5)) THEN
    DO i=1,nveg
        WRITE(serieChar(1,4*(i-1)+posW1+1),'(F12.4)')estadveg(i,1)
        WRITE(serieChar(2,4*(i-1)+posW1+2),'(F12.4)')estadveg(i+nveg,1)
        WRITE(serieChar(3,4*(i-1)+posW1+1),'(F12.4)')estadveg(i,4)
        WRITE(serieChar(4,4*(i-1)+posW1+1),'(F12.4)')estadveg(i,2)
        WRITE(serieChar(5,4*(i-1)+posW1+2),'(F12.4)')estadveg(i+nveg,2)
        WRITE(serieChar(6,4*(i-1)+posW1+1),'(F12.4)')estadveg(i,5)

        WRITE(serieChar(10,4*(i-1)+posW1+1),'(F12.4)')estadveg(i+nveg,4)
        WRITE(serieChar(11,4*(i-1)+posW1+1),'(F12.4)')veg(i).area
    END DO
END IF    


WRITE (21,'(A2)') '* '

    IF (lang.eq.1) THEN
        WRITE (21,'(A30)') '* SERIES TEMPORALES DE SALIDA ' 
        WRITE (21,'(A2)') '* '
        WRITE(21,'(A1,A12,<ktot2-kppt>(A9,A3,x))') '*',' ------DT---',(' --------',codtex(j),j=kppt+1,ktot2)
        DO t=1,nt
        tpo=t*dtmin/60.
        WRITE(21,'(F12.4,x,<ktot2-kppt>(F12.4,x))')tpo,(serie(t,j),j=kppt+1,ktot2)
        ENDDO
        !WRITE(21,'(A1,A12,<ktot2>(A12,x))') '*',' -----------',(' -----------',j=1,ktot2)
        !Escribe los principales estadisticos de las series
        !WRITE(21,'(A13,<ktot2-kppt>F12.4)') '* SUMATOTAL  ',(serie(nt+1,j),j=kppt+1,ktot2)
        !WRITE(21,'(A13,<ktot2-kppt>F12.4)') '* MAXIMO     ',(serie(nt+2,j),j=kppt+1,ktot2)
        !WRITE(21,'(A13,<ktot2-kppt>F12.4)') '* MINIMO     ',(serie(nt+3,j),j=kppt+1,ktot2)
        !WRITE(21,'(A13,<ktot2-kppt>F12.4)') '* DESV-EST   ',(serie(nt+4,j),j=kppt+1,ktot2)
        !WRITE(21,'(A13,<ktot2-kppt>F12.4)') '* PROMEDIO   ',(serie(nt+5,j),j=kppt+1,ktot2)
        !WRITE(21,'(A13,<ktot2-kppt>F12.0)') '* NUM-DATOS  ',(serie(nt+6,j),j=kppt+1,ktot2)
        !WRITE(21,'(A13,<ktot2-kppt>F12.3)') '* TIEMPO-MAX ',(serie(nt+9,j),j=kppt+1,ktot2)
        !WRITE(21,'(A13,<ktot2-kppt>F12.3)') '* RMSE       ',(serie(nt+7,j),j=kppt+1,ktot2)
        !WRITE(21,'(A13,<ktot2-kppt>F12.4)') '* R2(Nash-S) ',(serie(nt+8,j),j=kppt+1,ktot2)
        !WRITE(21,'(A13,<ktot2-kppt>F12.4)') '* RSR Index  ',(serie(nt+10,j),j=kppt+1,ktot2)        
        !Guiomar (30/01/2014): Añado el coeficiente de correlación en el caso de que esté activada la vegetación dinámicae
        WRITE(21,'(<ktot2-kppt+1>(A12,x))') '* ----------',('------------',j=kppt+1,ktot2)
        WRITE(21,'(<ktot2-kppt+1>(A12,x))') '* MAX OBS   ',(serieChar(1,j),j=kppt+1,ktot2)
        WRITE(21,'(<ktot2-kppt+1>(A12,x))') '* MAX SIM   ',(serieChar(2,j),j=kppt+1,ktot2)
        WRITE(21,'(<ktot2-kppt+1>(A12,x))') '* RMSE      ',(serieChar(3,j),j=kppt+1,ktot2)
        WRITE(21,'(<ktot2-kppt+1>(A12,x))') '* T PICO OBS',(serieChar(4,j),j=kppt+1,ktot2)
        WRITE(21,'(<ktot2-kppt+1>(A12,x))') '* T PICO SIM',(serieChar(5,j),j=kppt+1,ktot2)
        WRITE(21,'(<ktot2-kppt+1>(A12,x))') '* ERR T PICO',(serieChar(6,j),j=kppt+1,ktot2)
        WRITE(21,'(<ktot2-kppt+1>(A12,x))') '* VOL OBS   ',(serieChar(7,j),j=kppt+1,ktot2)
        WRITE(21,'(<ktot2-kppt+1>(A12,x))') '* VOL SIM   ',(serieChar(8,j),j=kppt+1,ktot2)
        WRITE(21,'(<ktot2-kppt+1>(A12,x))') '* ERROR VOL ',(serieChar(9,j),j=kppt+1,ktot2)
        WRITE(21,'(<ktot2-kppt+1>(A12,x))') '* INDICE NSE',(serieChar(10,j),j=kppt+1,ktot2)
        WRITE(21,'(<ktot2-kppt+1>(A12,x))') '* AREA ACUM ',(serieChar(11,j),j=kppt+1,ktot2)
        WRITE(21,'(<ktot2-kppt+1>(A12,x))') '* RSR INDEX ',(serieChar(12,j),j=kppt+1,ktot2)
        !Escribe los parámetros empleados
        WRITE (21,'(A2)') '* '
        WRITE( 21,'(A23)') '* PARÁMETROS EMPLEADOS '
        WRITE (21,'(A2)') '* '
        WRITE (21,'(A48)')'*    Geomorfológicos: [Coeficiente y Exponente] '
        WRITE (21,'(A11)')'*    Cauce '

        WRITE (21,76)'*                 : ',(d(1,j),e(1,j),j=1,npar)
        WRITE (21,76)'*                 : ',(d(3,j),e(3,j),j=1,npar)
        WRITE (21,77)'*                 : ',(e(4,j),j=1,npar)
        WRITE (21,76)'*                 : ',(d(5,j),e(5,j),j=1,npar)
        WRITE (21,76)'*                 : ',(d(6,j),e(6,j),j=1,npar)
        !***************************************************************************
        WRITE (21,'(A48)')'*    Geomorfológicos: [Coeficiente y Exponente] '
        WRITE (21,'(A14)')'*    Carcavas '
        WRITE (21,76)'*                 : ',(dc(1,j),ec(1,j),j=1,npar)
        WRITE (21,76)'*                 : ',(dc(3,j),ec(3,j),j=1,npar)
        WRITE (21,77)'*                 : ',(ec(4,j),j=1,npar)
        WRITE (21,76)'*                 : ',(dc(5,j),ec(5,j),j=1,npar)
        WRITE (21,76)'*                 : ',(dc(6,j),ec(6,j),j=1,npar)
        !*************************************************************
        WRITE (21,'(A48)')'* Areas umbrales para flujo superficial (km2)   '
        WRITE (21,'(A43,<npar>F15.5)') '* Interflujo (inicio cárcavas efímeras)  : ',(areaumbral(2,j),j=1,npar)
        WRITE (21,'(A43,<npar>F15.5)') '* Flujo base (inicio cauces permanentes) : ',(areaumbral(3,j),j=1,npar)
        !WRITE (21,'(A2)') '* '
        WRITE (21,'(A23)')           '* Factores Correctores:'
        WRITE (21,13) '*','FC-1 Almacenamiento estatico           :',r(1)
        WRITE (21,13) '*','FC-2 Evapotranspiración                :',r(2)
        WRITE (21,13) '*','FC-3 Infiltración                      :',r(3)
        WRITE (21,13) '*','FC-4 Escorrentía directa               :',r(4)
        WRITE (21,13) '*','FC-5 Percolación                       :',r(5)
        WRITE (21,13) '*','FC-6 Interflujo                        :',r(6)
        WRITE (21,13) '*','FC-7 Perdidas Subterraneas             :',r(7)
        WRITE (21,13) '*','FC-8 Flujo subterráneo                 :',r(8)
        WRITE (21,13) '*','FC-9 Velocidad en los cauces           :',r(9)
        WRITE (21,13) '*','FC-0 Escalamiento de la precipitacion  :',r(10)
        WRITE (21,13) '*','Factor interpolacion PPT (mm/m)        :',betappt
        13 FORMAT(A1,15x,A40,F15.5)  
        WRITE (21,'(A2)') '* '
        WRITE (21,'(A20,I3,A9)')'* Humedad Inicial:  ',npar,' regiones'
        WRITE (21,14) '*','H-1 Almacenamiento estatico  (%)          :',(wdad(1,j),j=1,npar)
        WRITE (21,14) '*','H-2 Agua en superficie (mm)               :',(wdad(2,j),j=1,npar)
        WRITE (21,14) '*','H-3 Almacenamiento gravitac (mm)          :',(wdad(3,j),j=1,npar)
        WRITE (21,14) '*','H-4 Nivel del acuífero (mm)               :',(wdad(4,j),j=1,npar)
        WRITE (21,14) '*','H-5 Cauce a seccion llena (%)             :',(wdad(5,j),j=1,npar)    
        WRITE (21,14) '*','H-6 Tanque intercepción (%)               :',(wdad(6,j),j=1,npar)    
        !Guiomar (30/01/2014): Añado las condiciones iniciales correspondientes a la vegetación dinámica
        IF (config(5)) THEN
            WRITE (21,14) '*','H-8 Tanque estático más superficial (%)   :',(wdad(7,j),j=1,npar)
            WRITE (21,14) '*','H-1 Tanque estático menos superficial (%) :',(wdad(8,j),j=1,npar)
            WRITE (21,14) '*','LAI inicial                               :',(laiini(j),j=1,npar)
        ENDIF        
           
        WRITE (21,'(A2)') '* '
        WRITE (21,'(A60,I3)') '* Para interpolación espacial: Numero estaciones cercanas:  ',nest
        IF (.NOT.bint) THEN
        WRITE (21,'(A60)') '*                    NO INTERPOLA EN CADA INTERVALO TEMPORAL'
        ELSE
        WRITE (21,'(A60)') '*          INTERPOLA EN TODOS LOS INTERVALOS TEMPORALES (-1)'
        ENDIF
        WRITE (21,'(A2)') '* '
        IF (ktem.ne.0) THEN
        !WRITE (21,'(A39,4(x,F9.4))')'* Parámetros nieve (Beta,ro1,ro2,Tb) : ',bbeta,ro1,ro2,tbase
        WRITE (21,'(A48,4(f8.4,x),f9.6)')'* Parámetros nieve (Beta,ro1,ro2,Tb,betatemp) : ',bbeta,ro1,ro2,tbase,betatemp    
        WRITE (21,'(A2)') '* '
        ENDIF
        !Escribe los ficheros utilizados
        WRITE (21,'(A2)') '* '
        WRITE(21,'(A21)') '* FICHEROS EMPLEADOS '
        WRITE (21,'(A2)') '* '
        DO i=1,27
        WRITE (21,'(A2,A128)') '* ',arch(i)
        ENDDO
        !Escribe los valores medios de los mapas de parámetros utilizados
        WRITE (21,'(A2)') '* '
        WRITE (21,'(A47)') '* Valores medios mapas de parámetros utilizados: '
        WRITE (21,'(A2)') '* '
        WRITE (21,'(A100)') '*  --Hu(mm)--- --Ks(mm)--- --Kp(mm)--- --Kss(mm)-- --Ksa(mm)-- --Kps(mm)-- --Veloc(m/s)-- --Imax(mm)--'
        WRITE (21,'(A1,8(x,F11.4))') 'a',(averagepar(j), j=1,7),Imaxmedio
        !Escribe los valores medios de los estados finales de los tanques
        WRITE (21,'(A2)') '* '
        WRITE (21,'(A47)') '* Valores medios de los estados finales de los tanques: '
        WRITE (21,'(A2)') '* '
        WRITE (21,14) '*','H-1 Almacenamiento estatico  (%):',(wdadfin(1))
        WRITE (21,14) '*','H-2 Agua en superficie (mm)     :',(wdadfin(2))
        WRITE (21,14) '*','H-3 Almacenamiento gravitac (mm):',(wdadfin(3))
        WRITE (21,14) '*','H-4 Nivel del acuífero (mm)     :',(wdadfin(4))
        WRITE (21,14) '*','H-5 Cauce a seccion llena (%)   :',(wdadfin(5))
        WRITE (21,14) '*','H-6 Tanque intercepción (%)     :',(wdadfin(6))
        WRITE (21,'(A2)') '* '
        14 FORMAT(A1,15x,A43,<npar>(x,F9.3))
        !Escribe resumen de almacenamientos y flujos
        CALL almacenamientos
        WRITE (21,'(A2)') '* '
        IF (config(5)) THEN !El formato es similar a '(A14,<>A12)' pero todo junto al ser un unico String. Lo pongo el calculo en futuros cambios
                WRITE (21,'(A362)') '* ----DT------ --X-1(mm)-- --X-6(mm)-- --X-2(mm)-- --X-3(mm)-- --X-4(mm)-- --X-5(mm)-- --Y-6(mm)-- --Y-1(mm)-- --Y-2(mm)-- --Y-3(mm)-- --Y-4(mm)-- --H-6(mm)-- --H-1(mm)-- --H-2(mm)-- --H-3(mm)-- --H-4(mm)-- --H-5(mm)-- --H-0(mm)-- --X-0(ºC)-- --Y-0(mm)-- --ET0(mm)-- -Nieve(mm)- --Z3(mm)--- --X7(mm)--- --H8(mm)--- --X8(mm)--- --Tr1(mm)-- --Tr2(mm)-- --Es(mm)---'
                WRITE (21,'(A362)') '* ---(hrs)---- ----PPT---- -Throughf-- --Excess--- ---Infilt-- ---Percol-- ---Perdid-- -Edirecta-- -Esuelo+Tr- -Escorr-Di- ---Fl-Sub-- --Fl-Base-- -Intercepc- -AlmEstInf- ---Ag-sup-- --Alm-gra-- ---Acuif--- ---Cauces-- -Snowpack-- -Temperat-- --Fusión--- --ET0xFC2-- --Psolida-- --Exfiltr-- ---Riego--- -AlmEstSup- -ExcesSup-- -Tr1TEstat- -Tr2TEstat- -EvSuelDes-'
        ELSE
            !If(actacuifero.eq.1) then   
            !    WRITE (21,'(A326)') '* ----DT------ --X-1(mm)-- --X-6(mm)-- --X-2(mm)-- --X-3(mm)-- --X-4(mm)-- --X-5(mm)-- --Y-6(mm)-- --Y-1(mm)-- --Y-2(mm)-- --Y-3(mm)-- --Y-4(mm)-- --H-6(mm)-- --H-1(mm)-- --H-2(mm)-- --H-3(mm)-- --H-4(mm)-- --H-5(mm)-- --H-0(mm)-- --X-0(ºC)-- --Y-0(mm)-- --ET0(mm)-- -Nieve(mm)- --Z3(mm)--- --X7(mm)--- --X9(mm)--- --X10(mm)--'
            !    WRITE (21,'(A326)') '* ---(hrs)---- ----PPT---- -Throughf-- --Excess--- ---Infilt-- ---Percol-- ---Perdid-- -Edirecta-- -Esuelo+Tr- -Escorr-Di- ---Fl-Sub-- --Fl-Base-- -Intercepc- -StaticSto- ---Ag-sup-- --Alm-gra-- ---Acuif--- ---Cauces-- -Snowpack-- -Temperat-- --Fusión--- --ET0xFC2-- --Psolida-- --Exfiltr-- ---Riego--- --R.AcCon-- --R.AcNCon-'
            !Else
                WRITE (21,'(A302)') '* ----DT------ --X-1(mm)-- --X-6(mm)-- --X-2(mm)-- --X-3(mm)-- --X-4(mm)-- --X-5(mm)-- --Y-6(mm)-- --Y-1(mm)-- --Y-2(mm)-- --Y-3(mm)-- --Y-4(mm)-- --H-6(mm)-- --H-1(mm)-- --H-2(mm)-- --H-3(mm)-- --H-4(mm)-- --H-5(mm)-- --H-0(mm)-- --X-0(ºC)-- --Y-0(mm)-- --ET0(mm)-- -Nieve(mm)- --Z3(mm)--- --X7(mm)---'
                WRITE (21,'(A302)') '* ---(hrs)---- ----PPT---- -Throughf-- --Excess--- ---Infilt-- ---Percol-- ---Perdid-- -Edirecta-- -Esuelo+Tr- -Escorr-Di- ---Fl-Sub-- --Fl-Base-- -Intercepc- -AlmEstSue- ---Ag-sup-- --Alm-gra-- ---Acuif--- ---Cauces-- -Snowpack-- -Temperat-- --Fusión--- --ET0xFC2-- --Psolida-- --Exfiltr-- ---Riego---'
            !End if
        ENDIF
    ELSE IF (lang.eq.2) THEN
        WRITE (21,'(A30)') '* OUTPUT TIME SERIES          ' 
        WRITE (21,'(A2)') '* '
        WRITE(21,'(A1,A12,<ktot2-kppt>(A9,A3))') '*',' ------DT---',(' --------',codtex(j),j=kppt+1,ktot2)
        DO t=1,nt
        tpo=t*dtmin/60.
        WRITE(21,'(F12.4,<ktot2-kppt>F12.4)')tpo,(serie(t,j),j=kppt+1,ktot2)
        ENDDO
        !WRITE(21,'(A1,A12,<ktot2>(A12))') '*',' -----------',(' -----------',j=1,ktot2)
        !Escribe los principales estadisticos de las series
        !WRITE(21,'(A13,<ktot2-kppt>F12.4)') '* TOTALSUM   ',(serie(nt+1,j),j=kppt+1,ktot2)
        !WRITE(21,'(A13,<ktot2-kppt>F12.4)') '* MAXIMUM    ',(serie(nt+2,j),j=kppt+1,ktot2)
        !WRITE(21,'(A13,<ktot2-kppt>F12.4)') '* MINIMUM    ',(serie(nt+3,j),j=kppt+1,ktot2)
        !WRITE(21,'(A13,<ktot2-kppt>F12.4)') '* ST-DEV     ',(serie(nt+4,j),j=kppt+1,ktot2)
        !WRITE(21,'(A13,<ktot2-kppt>F12.4)') '* MEAN       ',(serie(nt+5,j),j=kppt+1,ktot2)
        !WRITE(21,'(A13,<ktot2-kppt>F12.0)') '* NUM-DATA   ',(serie(nt+6,j),j=kppt+1,ktot2)
        !WRITE(21,'(A13,<ktot2-kppt>F12.3)') '* MAX-TIME   ',(serie(nt+9,j),j=kppt+1,ktot2)
        !WRITE(21,'(A13,<ktot2-kppt>F12.3)') '* RMSE       ',(serie(nt+7,j),j=kppt+1,ktot2)
        !WRITE(21,'(A13,<ktot2-kppt>F12.4)') '* R2(Nash-S) ',(serie(nt+8,j),j=kppt+1,ktot2)
        !WRITE(21,'(A13,<ktot2-kppt>F12.4)') '* RSR Index  ',(serie(nt+10,j),j=kppt+1,ktot2)
        !!Guiomar (30/01/2014): Repetimos lo mismo que lo que estaba en la parte de castellano
        !IF (config(5)) THEN
        !    WRITE(21,'(A13,<ktot2-kppt>F12.4)') '* PEARSON  ',(serie(nt+11,j),j=kppt+1,ktot2)
        !ENDIF  
        WRITE(21,'(<ktot2-kppt+1>(A12,x))') '* ----------',('------------',j=kppt+1,ktot2)
        WRITE(21,'(<ktot2-kppt+1>(A12,x))') '* MAX OBS   ',(serieChar(1,j),j=kppt+1,ktot2)
        WRITE(21,'(<ktot2-kppt+1>(A12,x))') '* MAX SIM   ',(serieChar(2,j),j=kppt+1,ktot2)
        WRITE(21,'(<ktot2-kppt+1>(A12,x))') '* RMSE      ',(serieChar(3,j),j=kppt+1,ktot2)
        WRITE(21,'(<ktot2-kppt+1>(A12,x))') '* T PICO OBS',(serieChar(4,j),j=kppt+1,ktot2)
        WRITE(21,'(<ktot2-kppt+1>(A12,x))') '* T PICO SIM',(serieChar(5,j),j=kppt+1,ktot2)
        WRITE(21,'(<ktot2-kppt+1>(A12,x))') '* ERR T PICO',(serieChar(6,j),j=kppt+1,ktot2)
        WRITE(21,'(<ktot2-kppt+1>(A12,x))') '* VOL OBS   ',(serieChar(7,j),j=kppt+1,ktot2)
        WRITE(21,'(<ktot2-kppt+1>(A12,x))') '* VOL SIM   ',(serieChar(8,j),j=kppt+1,ktot2)
        WRITE(21,'(<ktot2-kppt+1>(A12,x))') '* ERROR VOL ',(serieChar(9,j),j=kppt+1,ktot2)
        WRITE(21,'(<ktot2-kppt+1>(A12,x))') '* INDICE NSE',(serieChar(10,j),j=kppt+1,ktot2)
        WRITE(21,'(<ktot2-kppt+1>(A12,x))') '* AREA ACUM ',(serieChar(11,j),j=kppt+1,ktot2)
        WRITE(21,'(<ktot2-kppt+1>(A12,x))') '* RSR INDEX ',(serieChar(12,j),j=kppt+1,ktot2)
        !Escribe los parámetros empleados
        WRITE (21,'(A2)') '* '
        WRITE( 21,'(A23)') '* PARAMETERS         S '
        WRITE (21,'(A2)') '* '
        WRITE (21,'(A48)')'*    Geomorfological: [Coefficient and Exponent]'
        WRITE (21,'(A11)')'*  Channels   '

        WRITE (21,76)'*                 : ',(d(1,j),e(1,j),j=1,npar)
        WRITE (21,76)'*                 : ',(d(3,j),e(3,j),j=1,npar)
        WRITE (21,77)'*                 : ',(e(4,j),j=1,npar)
        WRITE (21,76)'*                 : ',(d(5,j),e(5,j),j=1,npar)
        WRITE (21,76)'*                 : ',(d(6,j),e(6,j),j=1,npar)
        !*************************************************************************** chiara
        WRITE (21,'(A48)')'*    Geomorfological: [Coefficient and Exponent]'
        WRITE (21,'(A14)')'*    Carcavas '
        WRITE (21,76)'*                 : ',(dc(1,j),ec(1,j),j=1,npar)
        WRITE (21,76)'*                 : ',(dc(3,j),ec(3,j),j=1,npar)
        WRITE (21,77)'*                 : ',(ec(4,j),j=1,npar)
        WRITE (21,76)'*                 : ',(dc(5,j),ec(5,j),j=1,npar)
        WRITE (21,76)'*                 : ',(dc(6,j),ec(6,j),j=1,npar)
        !*************************************************************
        WRITE (21,'(A48)')'* Threshold areas                       (km²)   '
        WRITE (21,'(A43,F15.5)') '* Interflow  (starting ephemeral gullies): ',(areaumbral(2,j),j=1,npar)
        WRITE (21,'(A43,F15.5)') '* Base flow  (Starting permament channel): ',(areaumbral(3,j),j=1,npar)
        !WRITE (21,'(A2)') '* '
        WRITE (21,'(A23)')     '* Correction factors  :'
        WRITE (21,15) '*','FC-1 Static storage             :',r(1)
        WRITE (21,15) '*','FC-2 Evapotranspiration         :',r(2)
        WRITE (21,15) '*','FC-3 Infiltration               :',r(3)
        WRITE (21,15) '*','FC-4 Slope velocity             :',r(4)
        WRITE (21,15) '*','FC-5 Percolation                :',r(5)
        WRITE (21,15) '*','FC-6 Interflow                  :',r(6)
        WRITE (21,15) '*','FC-7 Deep percolation           :',r(7)
        WRITE (21,15) '*','FC-8 Connected aquifer flow     :',r(8)
        WRITE (21,15) '*','FC-9 Flow velocity              :',r(9)
        WRITE (21,15) '*','FC-0 Precipitation scaling      :',r(10)
        WRITE (21,15) '*','Interpolation factor of P(mm/m) :',betappt
        15 FORMAT(A1,15x,A33,F15.5) 
        WRITE (21,'(A2)') '* '
        WRITE (21,'(A20,I3,A9)')'* Initial soil moist',npar,' regions '
        WRITE (21,16) '*','H-1 Static storage           (%)  :',(wdad(1,j),j=1,npar)
        WRITE (21,16) '*','H-2 Surface water      (mm)       :',(wdad(2,j),j=1,npar)
        WRITE (21,16) '*','H-3 Gravitational storage   (mm)  :',(wdad(3,j),j=1,npar)
        WRITE (21,16) '*','H-4 Aquifer level      (mm)       :',(wdad(4,j),j=1,npar)
        WRITE (21,16) '*','H-5 Drainage network storage (%)  :',(wdad(5,j),j=1,npar)
        WRITE (21,16) '*','H-6 Interception        (%)       :',(wdad(6,j),j=1,npar)
        !Guiomar (30/01/2014): incluimos lo mismo que en castellano
        IF (config(5)) THEN
            WRITE (21,16) '*','H-8 Shallow Static Storage (%)    :',(wdad(7,j),j=1,npar)
            WRITE (21,16) '*','H-5 Deeper Static Storage (%)     :',(wdad(8,j),j=1,npar)
            WRITE (21,16) '*','Initial LAI                       :',(laiini(j),j=1,npar)
        ENDIF
        WRITE (21,'(A2)') '* '
        WRITE (21,'(A60,I3)') '*     Spatial interpolation: number of neighbour stations:  ',nest
        IF (.NOT.bint) THEN
        WRITE (21,'(A60)') '*                    NO INTERPOLATION EACH TIME STEP        '
        ELSE
        WRITE (21,'(A60)') '*          INTERPOLATION EVERY TIME STEP                (-1)'
        ENDIF
        WRITE (21,'(A2)') '* '
        IF (ktem.ne.0) THEN
        !WRITE (21,'(A39,4f9.4)')'* Snow parameters  (Beta,ro1,ro2,Tb) : ',bbeta,ro1,ro2,tbase
        WRITE (21,'(A48,4(f8.4,x),f9.6)')'* Snow parameters  (Beta,ro1,ro2,Tb,betatemp) : ',bbeta,ro1,ro2,tbase,betatemp
        WRITE (21,'(A2)') '* '
        ENDIF
        !Escribe los ficheros utilizados
        WRITE (21,'(A2)') '* '
        WRITE(21,'(A21)') '* USED FILES         '
        WRITE (21,'(A2)') '* '
        DO i=1,27
        WRITE (21,'(A2,A128)') '* ',arch(i)
        ENDDO
        !Escribe los valores medios de los mapas de parámetros utilizados
        WRITE (21,'(A2)') '* '
        WRITE (21,'(A47)') '* Mean values of the used parameter maps:        '
        WRITE (21,'(A2)') '* '
        WRITE (21,'(A100)') '* --Hu(mm)--- --Ks(mm)--- --Kp(mm)--- --Kss(mm)-- --Ksa(mm)-- --Kps(mm)-- Veloc(m/s)- -Imax(mm)--'
        WRITE (21,'(A1,8(x,F11.4))') 'a',(averagepar(j), j=1,7),Imaxmedio
        !Escribe los valores medios de los estados finales de los tanques
        WRITE (21,'(A2)') '* '
        WRITE (21,'(A47)') '* Mean values of the tank final states:                 '
        WRITE (21,'(A2)') '* '
        WRITE (21,16) '*','H-1 Static storage           (%)  :',(wdadfin(1))
        WRITE (21,16) '*','H-2 Surface water      (mm)       :',(wdadfin(2))
        WRITE (21,16) '*','H-3 Gravitational storage   (mm)  :',(wdadfin(3))
        WRITE (21,16) '*','H-4 Aquifer level      (mm)       :',(wdadfin(4))
        WRITE (21,16) '*','H-5 Drainage network storage (%)  :',(wdadfin(5))
        WRITE (21,16) '*','H-6 Interception        (%)       :',(wdadfin(6))
        16 FORMAT(A1,15x,A35,<npar>(x,F9.3))
        WRITE (21,'(A2)') '* '
        !Escribe resumen de almacenamientos y flujos
        CALL almacenamientos
        WRITE (21,'(A2)') '* '
        IF (config(5)) THEN !El formato es similar a '(A14,<>A12)' pero todo junto al ser un unico String. Lo pongo el calculo en futuros cambios
                WRITE (21,'(A362)') '* ----DT------ --X-1(mm)-- --X-6(mm)-- --X-2(mm)-- --X-3(mm)-- --X-4(mm)-- --X-5(mm)-- --Y-6(mm)-- --Y-1(mm)-- --Y-2(mm)-- --Y-3(mm)-- --Y-4(mm)-- --H-6(mm)-- --H-1(mm)-- --H-2(mm)-- --H-3(mm)-- --H-4(mm)-- --H-5(mm)-- --H-0(mm)-- --X-0(ºC)-- --Y-0(mm)-- --ET0(mm)-- -Nieve(mm)- --Z3(mm)--- --X7(mm)--- --H8(mm)--- --X8(mm)--- --Tr1(mm)-- --Tr2(mm)-- --Es(mm)---'
                WRITE (21,'(A362)') '* ---(hrs)---- ----PPT---- -Throughf-- --Excess--- ---Infilt-- ---Percol-- ---Losses-- -EfromVeg-- -realET---- -DirectRun- -SubSupFlo- -ConnFlow-- -Intercepc- -StaStoDee- --SupWat--- --GravSto-- ---Acuif--- -Channels-- -Snowpack-- -Temperat-- -SnowMelt-- --ET0xFC2-- --SnowFall- --Exfiltr-- --Irrigat-- -StaStoSha- -ExceShall- -Tr1StaSto- -Tr2StaSto- -EvBareSoi-'
        ELSE
            !If(actacuifero.eq.1) then   
            !    WRITE (21,'(A326)') '* ----DT------ --X-1(mm)-- --X-6(mm)-- --X-2(mm)-- --X-3(mm)-- --X-4(mm)-- --X-5(mm)-- --Y-6(mm)-- --Y-1(mm)-- --Y-2(mm)-- --Y-3(mm)-- --Y-4(mm)-- --H-6(mm)-- --H-1(mm)-- --H-2(mm)-- --H-3(mm)-- --H-4(mm)-- --H-5(mm)-- --H-0(mm)-- --X-0(ºC)-- --Y-0(mm)-- --ET0(mm)-- -Nieve(mm)- --Z3(mm)--- --X7(mm)--- --X9(mm)--- --X10(mm)--'
            !    WRITE (21,'(A326)') '* ---(hrs)---- ----PPT---- -Throughf-- --Excess--- ---Infilt-- ---Percol-- ---Losses-- -EfromVeg-- -realET---- -DirectRun- -SubSupFlo- -ConnFlow-- -Intercepc- -StaticSto- --SupWat--- --GravSto-- ---Acuif--- -Channels-- -Snowpack-- -Temperat-- -SnowMelt-- --ET0xFC2-- --SnowFall- --Exfiltr-- --Irrigat-- --ConAquif- -NConAquif-'
            !Else
                WRITE (21,'(A302)') '* ----DT------ --X-1(mm)-- --X-6(mm)-- --X-2(mm)-- --X-3(mm)-- --X-4(mm)-- --X-5(mm)-- --Y-6(mm)-- --Y-1(mm)-- --Y-2(mm)-- --Y-3(mm)-- --Y-4(mm)-- --H-6(mm)-- --H-1(mm)-- --H-2(mm)-- --H-3(mm)-- --H-4(mm)-- --H-5(mm)-- --H-0(mm)-- --X-0(ºC)-- --Y-0(mm)-- --ET0(mm)-- -Nieve(mm)- --Z3(mm)--- --X7(mm)---'
                WRITE (21,'(A302)') '* ---(hrs)---- ----PPT---- -Throughf-- --Excess--- ---Infilt-- ---Percol-- ---Losses-- -EfromVeg-- -realET---- -DirectRun- -SubSupFlo- -ConnFlow-- -Intercepc- -StaticSto- --SupWat--- --GravSto-- ---Acuif--- -Channels-- -Snowpack-- -Temperat-- -SnowMelt-- --ET0xFC2-- --SnowFall- --Exfiltr-- --Irrigat--'
            !End if
        ENDIF
        76 FORMAT(a20,<2*npar>F12.5)
        77 FORMAT(a20,<npar>F24.5)        
    End if
DO t=1,nt
  tpo=t*dtmin/60.
  !GUIOMAR (15/10/2015): Cambio para que salgan nuevas variables. Debe mejorarse la posición y lo que hay escrito sobre ellas
  IF (config(5)) THEN   
        WRITE(21,'(A2,30(x,F11.4))')'b ',tpo,(balanc(t,j),j=1,20),preac(t,2),preac(t,1)-balanc(t,1),balanc(t,22),balanc(t,28),balanc(t,23),balanc(t,24),balanc(t,25),balanc(t,26),balanc(t,27)
  Else
      !If (actacuifero.eq.1) then
      !  WRITE(21,'(A2,24F12.4,F17.9,2F12.4)')'b ',tpo,(balanc(t,j),j=1,20),preac(t,2),preac(t,1)-balanc(t,1),balanc(t,22),balanc(t,28),balanc(t,29),balanc(t,30)
      !ELSE
        WRITE(21,'(A2,25(x,F11.4))')'b ',tpo,(balanc(t,j),j=1,20),preac(t,2),preac(t,1)-balanc(t,1),balanc(t,22),balanc(t,28) !Cris(03/2017) Esta es la original + riego. Riego se escribe siempre, pero si no hay, será un cero
      !End if
  ENDIF
ENDDO
CLOSE(21)

IF (ALLOCATED(serie)) DEALLOCATE(serie)
IF (ALLOCATED(codtex)) DEALLOCATE(codtex)
END SUBROUTINE

!***************************************************************
!* Rutina que genera el Archivo de salida en formato CEDEX
!***************************************************************
SUBROUTINE forma_cedex
USE modtet
!use ieee_arithmetic

REAL evac(nt),sumxy,sumx,sumy,sumx2,sumy2
Character aaa*3
LOGICAL is_nan

!Comienza fichero de salida
IF (config(3)) THEN
  artem=TRIM(ADJUSTL(dirtra))//'~OUTPUT.TXT'
ELSE
  artem=arch(9)
ENDIF

OPEN (21,file=artem)
WRITE (21,'(A2)') '* '
WRITE (21,'(A55)') '* MODELACION HIDROLOGICA DISTRIBUIDA DE TIPO CONCEPTUAL'
WRITE (21,'(A55)') '*      MODELO DE SIMULACION - T E T I S   v.9 -      '
WRITE (21,'(A2)') '* '
WRITE (21,'(A20)') '* Desarrollado en:  '
WRITE (21,'(A55)') '* UNIVERSITAT POLITÈCNICA DE VALÈNCIA  '
WRITE (21,'(A63)') '* Instituto de ingeniería del Agua y Medio Ambiente (IIAMA)'
WRITE (21,'(A57)') '* Grupo de Investigación en Modelación Hidrológica y Medioambiental (GIHMA)'
WRITE (21,'(A2)') '* '
WRITE (21,'(A37)') '* '
WRITE (21,'(A41)') '* DATOS DEL MODELO TETIS EN FORMATO CEDEX'
WRITE (21,'(A2)') '* '
WRITE (21,'(A25,3x,A10,x,A5,x,A8,x,A10,x,A5)') '* Intervalo de fechas  : ',fecin,horin(1:5),' hasta ',fecfin,horfin(1:5)
WRITE (21,'(A29,I2)') '* Estaciones para interpolar: ', nest
WRITE (21,'(A23,I2)') '* Regiones homogéneas: ',npar   
WRITE (21,'(A25,I5,A4)') '* Incremento de tiempo : ',INT(dtmin),' min'
WRITE (21,'(A25,I5)') '* Número de Intervalos : ',nt
WRITE (21,'(A47)')    '* Datos en mm. por intervalo para pluviómetros.'
WRITE (21,'(A61)') '* Datos en m. al final del intervalo para niveles de embalse.'
WRITE (21,'(A67)') '* Datos en m3/s al final del intervalo para caudales desembalsados.'
WRITE (21,'(A44)') '* Coordenadas XYZ UTM (huso 30) aproximadas.'
WRITE (21,'(A2)') '* '

aaa='F  '
WRITE(21,'(A3,2x,A2,A1,A2,A1,A4,2x,A2,A1,A2)') aaa,archin(1:2),'-',archin(3:4),'-',archin(11:14),archin(5:6),':',archin(7:8)
WRITE (21,'(A2)') '* '
aaa='G  '
WRITE (21,'(A3,5x,I7,2x,I5)') aaa,nt,INT(dtmin)
WRITE (21,'(A2)') '* '
67 FORMAT(A53,<nt>(x,I11))
68 FORMAT(A53,<nt>(x,A11))
WRITE (21,67) '* Nombre                           X      Y     Z   O',(j,j=1,nt)
WRITE (21,68) '* ----- --------------------- ------ ------- ---- ---',('-----------',j=1,nt)
aaa='P  '
orig=0.0
DO i=1,kppt
  WRITE (21,30) aaa,pluvio(i).name,INT(pluvio(i).utmx),INT(pluvio(i).utmy),  &
               INT(pluvio(i).elev),orig,(pluvio(i).obs(t),t=1,nt)
ENDDO
WRITE (21,'(A2)') '* '
aaa='M  '
nam='X1-M Lluvia Media Cuenca '
WRITE (21,30) aaa,nam,INT(0.0),INT(0.0),INT(0.0),orig,(preac(t,1),t=1,nt)
WRITE (21,'(A2)')'* '

DO i=1,nemb+vnemb+knemb
  IF (emb(i).caso.gt.0) THEN
    aaa='No '		  !Nivel del embalse 
    WRITE (21,36) aaa,nivel(i).name,INT(nivel(i).utmx),INT(nivel(i).utmy),  &
               INT(nivel(i).elev),orig,(nivel(i).obs(t),t=1,nt)
    aaa='Ns '   !Nivel del embalse simulado (basado en 'A')
	WRITE (21,36) aaa,nivel(i).name,INT(nivel(i).utmx),INT(nivel(i).utmy),  &
               INT(nivel(i).elev),orig,(nivel(i).sim(t),t=1,nt)
    aaa='Ib '		 !Q entrada estimado por Balance
    WRITE (21,36) aaa,qemb(i).name,INT(qemb(i).utmx),INT(qemb(i).utmy),  &
               INT(qemb(i).elev),orig,(qemb(i).bal(t),t=1,nt)
    aaa='Is '		 !Q entrada simulado (en la celda)
    WRITE (21,36) aaa,qemb(i).name,INT(qemb(i).utmx),INT(qemb(i).utmy),  &
               INT(qemb(i).elev),orig,(qemb(i).sim(t),t=1,nt)
    aaa='Vo '  !Volumen observado *1000000m³ obtenido de la curva Cota vs Volumen segun nivel "N"
    WRITE (21,36) aaa,volum(i).name,INT(volum(i).utmx),INT(volum(i).utmy),  &
               INT(volum(i).elev),orig,(volum(i).obs(t),t=1,nt)
    aaa='Vs '  !Volumen simulado *1000m³ obtenido de la curva Cota vs Volumen segun nivel "L"
    WRITE (21,36) aaa,volum(i).name,INT(volum(i).utmx),INT(volum(i).utmy),  &
               INT(volum(i).elev),orig,(volum(i).sim(t),t=1,nt)
	aaa='So ' !Qsalida Observado
    WRITE (21,36) aaa,qemb(i).name,INT(qemb(i).utmx),INT(qemb(i).utmy),  &
               INT(qemb(i).elev),orig,(qemb(i).obs(t),t=1,nt)
  ENDIF
ENDDO

nindex=1
IF (nindex.gt.nt) nindex=nt
IF (nindex.lt.1) nindex=1
!Calcula estadísticos
DO i=1,naf
  aaa='Qo '
  WRITE (21,36) aaa,aforo(i).name,INT(aforo(i).utmx),INT(aforo(i).utmy),  &
               INT(aforo(i).elev),orig,(aforo(i).obs(t),t=1,nt)  
  aaa='Qs '
  WRITE (21,36) aaa,aforo(i).name,INT(aforo(i).utmx),INT(aforo(i).utmy),  &
               INT(aforo(i).elev),orig,(aforo(i).sim(t),t=1,nt)
    
  aaa='*  '
  pder=0.0
  newnt=0  !Almacena número total de datos descontando faltantes
  DO t=nindex,nt
  	IF (aforo(i).obs(t).ne.-1) THEN    !Condicional agregado para no contabilizar los faltantes
        estad(i,3)=estad(i,3)+aforo(i).obs(t)*dts/1000000.0   !Volumen obs en Mm³
        estad(i+naf,3)=estad(i+naf,3)+aforo(i).sim(t)*dts/1000000.0  !Volumen sim en Mm³
  	    IF (aforo(i).obs(t).gt.estad(i,1)) THEN
	        estad(i,1)=aforo(i).obs(t)	!Caudal pico Observado
	        estad(i,2)=dt*t       !Tiempo al pico Observado
	    ENDIF
  	    IF (aforo(i).sim(t).gt.estad(i+naf,1)) THEN
	        estad(i+naf,1)=aforo(i).sim(t)	!Caudal pico simulado
	        estad(i+naf,2)=dt*t       !Tiempo al pico simulado
	    ENDIF
	    estad(i,4)=estad(i,4)+(aforo(i).obs(t)-aforo(i).sim(t))*(aforo(i).obs(t)-aforo(i).sim(t))
        pder=pder+aforo(i).obs(t)
        newnt=newnt+1
    ELSE
  	    IF (aforo(i).sim(t).gt.estad(i+naf,1)) THEN
	        estad(i+naf,1)=aforo(i).sim(t)	!Caudal pico simulado
	        estad(i+naf,2)=dt*t       !Tiempo al pico simulado
	    ENDIF
    ENDIF
  ENDDO
  pder=pder/newnt   !Caudal medio para NSE
  DO t=1,nt
    IF (aforo(i).obs(t).ne.-1) estad(i+naf,4)=estad(i+naf,4)+(aforo(i).obs(t)-pder)**2.0 !condicional agregado
  ENDDO
  
   !Indice sugerido por Moriasi 2007, RMSE-observations standard deviation ratio
  IF (estad(i+naf,4).gt.0.0001) RSRindex(i)=(estad(i,4))**0.5/(estad(i+naf,4))**0.5
  
  IF (estad(i+naf,4).gt.0.0001) estad(i+naf,4)=1.0-estad(i,4)/estad(i+naf,4)  !NSE Nash
  estad(i,4)=(estad(i,4)/newnt)**0.5  !RMSE
  IF (estad(i,2).eq.0.0) THEN
    estad(i,5)=9999.
  ELSE
     estad(i,5)=(estad(i,2)-estad(i+naf,2))  !Error tiempo al pico en terminos absolutos con su signo
    !estad(i,5)=100.0*(estad(i,2)-estad(i+naf,2))/estad(i,2) !error en % para ponerlo en terminos absolutos
    IF (estad(i,5).gt.9999.9) estad(i,6)=9999.9
    IF (estad(i,5).lt.-9999.9) estad(i,6)=-9999.9
  ENDIF
  IF (estad(i,3).eq.0.0) THEN
    estad(i,6)=9999.
  ELSE
      estad(i,6)=-100.0*(estad(i,3)-estad(i+naf,3))/estad(i,3)
	  IF (estad(i,6).gt.9999.9) estad(i,6)=9999.9
	  IF (estad(i,6).lt.-9999.9) estad(i,6)=-9999.9
  ENDIF
  
  !Control de NaN
    DO j=1,20
        !IF (ieee_is_nan(estad(i,j))) THEN                
        !Subtituimos la rutina para no emplear librerias(Vicente.Nov-2019)
        IF (is_nan(estad(i,j))) THEN            
            estad(i,j)= -9999.0
        ENDIF
    ENDDO
  
  nam='Estadísticos principales'
  WRITE (21,37) aaa,nam,' Qmax Obs=',estad(i,1),'Qmax Sim=',estad(i+naf,1),' RMSE=', &
   estad(i,4),'Tpico Obs=',estad(i,2),'Tpico Sim=',estad(i+naf,2),'Err Tp=',estad(i,5),  & 
   ' Vol Obs= ',estad(i,3),' Vol Sim=',estad(i+naf,3),'Err Vol(%)=',estad(i,6),  &
   ' Indice NSE= ',estad(i+naf,4),' Area Acum= ',aforo(i).area, 'RSR Index= ',RSRindex(i)
  IF (modulos2(3)) THEN
    aaa='NOq'
    WRITE (21,36) aaa,aforo(i).name,INT(aforo(i).utmx),INT(aforo(i).utmy),INT(aforo(i).elev),orig,(norgql(i).sim(t),t=1,nt)  
    aaa='AMq'
    WRITE (21,36) aaa,aforo(i).name,INT(aforo(i).utmx),INT(aforo(i).utmy),INT(aforo(i).elev),orig,(amonioql(i).sim(t),t=1,nt)  
    aaa='NIq'
    WRITE (21,36) aaa,aforo(i).name,INT(aforo(i).utmx),INT(aforo(i).utmy),INT(aforo(i).elev),orig,(nitratoql(i).sim(t),t=1,nt)  
    If (config(4)) THEN
        aaa='QXs'
        WRITE (21,36) aaa,aforo(i).name,INT(aforo(i).utmx),INT(aforo(i).utmy),INT(aforo(i).elev),orig,(aforo(i).sed_out(t,4),t=1,nt)
        aaa='QXa'
        WRITE (21,36) aaa,aforo(i).name,INT(aforo(i).utmx),INT(aforo(i).utmy),INT(aforo(i).elev),orig,(aforo(i).sed_out(t,1),t=1,nt)
        aaa='QXl'
        WRITE (21,36) aaa,aforo(i).name,INT(aforo(i).utmx),INT(aforo(i).utmy),INT(aforo(i).elev),orig,(aforo(i).sed_out(t,2),t=1,nt)
        aaa='QXc'
        WRITE (21,36) aaa,aforo(i).name,INT(aforo(i).utmx),INT(aforo(i).utmy),INT(aforo(i).elev),orig,(aforo(i).sed_out(t,3),t=1,nt)
        aaa='NOx'
        WRITE (21,36) aaa,aforo(i).name,INT(aforo(i).utmx),INT(aforo(i).utmy),INT(aforo(i).elev),orig,(norgqs(i).sim(t),t=1,nt)  
        aaa='AMx'
        WRITE (21,36) aaa,aforo(i).name,INT(aforo(i).utmx),INT(aforo(i).utmy),INT(aforo(i).elev),orig,(amonioqs(i).sim(t),t=1,nt)  
    End if
  Else if (config(4)) THEN
    aaa='QXs'
    WRITE (21,36) aaa,aforo(i).name,INT(aforo(i).utmx),INT(aforo(i).utmy),INT(aforo(i).elev),orig,(aforo(i).sed_out(t,4),t=1,nt)
    aaa='QXa'
    WRITE (21,36) aaa,aforo(i).name,INT(aforo(i).utmx),INT(aforo(i).utmy),INT(aforo(i).elev),orig,(aforo(i).sed_out(t,1),t=1,nt)
    aaa='QXl'
    WRITE (21,36) aaa,aforo(i).name,INT(aforo(i).utmx),INT(aforo(i).utmy),INT(aforo(i).elev),orig,(aforo(i).sed_out(t,2),t=1,nt)
    aaa='QXc'
    WRITE (21,36) aaa,aforo(i).name,INT(aforo(i).utmx),INT(aforo(i).utmy),INT(aforo(i).elev),orig,(aforo(i).sed_out(t,3),t=1,nt)
  ENDIF
  aaa='* '
  WRITE (21,'(A3)') aaa
ENDDO
!Guiomar (30/01/2014): A continuación lo mismo que antes pero para las estaciones con vegetación
ALLOCATE (estadveg(2*nveg,12))
estadveg=0.0
ALLOCATE (RSRindexveg(nveg))
RSRindexveg=0.0
  
DO i=1,nveg
  aaa='Wo '
  WRITE (21,36) aaa,veg(i).name,INT(veg(i).utmx),INT(veg(i).utmy),  &
               INT(veg(i).elev),orig,(veg(i).obs(t),t=1,nt)
  aaa='Ws '
  WRITE (21,36) aaa,veg(i).name,INT(veg(i).utmx),INT(veg(i).utmy),  &
               INT(veg(i).elev),orig,(veg(i).sim(t),t=1,nt)
  aaa='Wr '
  WRITE (21,36) aaa,veg(i).name,INT(veg(i).utmx),INT(veg(i).utmy),  &
               INT(veg(i).elev),orig,(veg2_point(i).sim(t),t=1,nt)
  !(Vicente) Por ahora no mostramos esta serie
  !aaa='Tr '
  !WRITE (21,36) aaa,veg(i).name,INT(veg(i).utmx),INT(veg(i).utmy),  &
  !             INT(veg(i).elev),orig,(tr_point(i).sim(t),t=1,nt)  
  aaa='*  '
  pder=0.0
  newnt=0
  sumxy=0.0
  sumx=0.0
  sumy=0.0
  sumx2=0.0
  sumy2=0.0
  DO t=nindex,nt
  	IF (veg(i).obs(t).ne.-1) THEN    !Camilo: condicional agregado para no contabilizar los faltantes
  	    IF (veg(i).obs(t).gt.estadveg(i,1)) THEN
	        estadveg(i,1)=veg(i).obs(t)	!Caudal pico Observado
	        estadveg(i,2)=dt*t       !Tiempo al pico Observado
	    ENDIF
  	    IF (veg(i).sim(t).gt.estadveg(i+nveg,1)) THEN
	        estadveg(i+nveg,1)=veg(i).sim(t)	!Caudal pico simulado
	        estadveg(i+nveg,2)=dt*t       !Tiempo al pico simulado
	    ENDIF
	    estadveg(i,4)=estadveg(i,4)+(veg(i).obs(t)-veg(i).sim(t))*(veg(i).obs(t)-veg(i).sim(t))
        pder=pder+veg(i).obs(t)
        newnt=newnt+1
        !Guiomar (30/01/2014): hacemos todos los cálculos necesarios para hallar el coeficiente de correlación de Pearson
        sumxy=sumxy+veg(i).obs(t)*veg(i).sim(t)
        sumx=sumx+veg(i).obs(t)
        sumy=sumy+veg(i).sim(t)
        sumx2=sumx2+veg(i).obs(t)*veg(i).obs(t)
        sumy2=sumy2+veg(i).sim(t)*veg(i).sim(t)
    ELSE
  	    IF (veg(i).sim(t).gt.estadveg(i+nveg,1)) THEN
	        estadveg(i+nveg,1)=veg(i).sim(t)	!Caudal pico simulado
	        estadveg(i+nveg,2)=dt*t       !Tiempo al pico simulado
	    ENDIF
    ENDIF
  ENDDO
  !(Vicente) Añado calculode NSE Nash
  pder=pder/newnt   !Caudal medio para NSE
  DO t=1,nt
    IF (veg(i).obs(t).ne.-1) estadveg(i+nveg,4)=estadveg(i+nveg,4)+(veg(i).obs(t)-pder)**2.0 
  ENDDO
  
  !Indice sugerido por Moriasi 2007, RMSE-observations standard deviation ratio
  IF (estadveg(i+nveg,4).gt.0.0001) RSRindexveg(i)=(estadveg(i,4))**0.5/(estadveg(i+nveg,4))**0.5
  
  IF (estadveg(i+nveg,4).gt.0.0001) estadveg(i+nveg,4)=1.0-estadveg(i,4)/estadveg(i+nveg,4)  !NSE Nash   
  
  estadveg(i,4)=(estadveg(i,4)/newnt)**0.5  !RMSE
  IF (estadveg(i,2).eq.0.0) THEN
    estadveg(i,5)=9999.
  ELSE
    estadveg(i,5)=100.0*(estadveg(i,2)-estadveg(i+nveg,2))/estadveg(i,2)
    IF (estadveg(i,5).gt.9999.9) estadveg(i,6)=9999.9
    IF (estadveg(i,5).lt.-9999.9) estadveg(i,6)=-9999.9
  ENDIF
  !Guiomar (30/01/2014): Calculamos el coeficiente de Pearson y lo guardamos en estadveg(i,7)
  !estadveg(i,7)=(newnt*sumxy-sumx*sumy)/(((newnt*sumx2-sumx*sumx)**0.5)*((newnt*sumy2-sumy*sumy)**0.5)) ! (Vicente-Guiomar)
  !Control de NaN
    DO j=1,12
        !IF (ieee_is_nan(estadveg(i,j))) THEN
        !Subtituimos la rutina para no emplear librerias(Vicente.Nov-2019)
        IF (is_nan(estadveg(i,j))) THEN
            estadveg(i,j)= -1.0
        ENDIF
    ENDDO
  nam='Estadísticos principales'
  ! (Vicente-Guiomar)
  !WRITE (21,37) aaa,nam,' LAImax Obs=',estadveg(i,1),'LAImax Sim=',estadveg(i+nveg,1),' RMSE=', &
  ! estadveg(i,4),'Tpico Obs=',estadveg(i,2),'Tpico Sim=',estadveg(i+nveg,2),'Err Tp(%)=',estadveg(i,5),  & 
  ! ' Pearson= ',estadveg(i,7),' Area Acum= ',veg(i).area
  WRITE (21,39) aaa,nam,'LAImax Obs=',estadveg(i,1),'LAImax Sim=',estadveg(i+nveg,1),'RMSE= ', &
   estadveg(i,4),'Tpico Obs=',estadveg(i,2),'Tpico Sim=',estadveg(i+nveg,2),'Err Tp(%)=',estadveg(i,5),  & 
   'Indice NSE =',estadveg(i+nveg,4),'Area Acum= ',veg(i).area, 'RSR Index= ',RSRindexveg(i)
  aaa='* '
  WRITE (21,'(A3)') aaa
  
ENDDO
39 FORMAT(A3,'"',A25,'"',A11,F15.3,x,A11,F15.3,x,A6,F15.3,x,A10,F15.3,x,A10,F15.3,x,A10,F15.3,x,A12,F15.3,x,A12,F15.3,x,A11,F15.3)

ALLOCATE (RSRindexsed(ksedq))
RSRindexsed=0.0      
   
DO i=1,ksedq
  aaa='Xo '
  WRITE (21,36) aaa,aforosed(i).name,INT(aforosed(i).utmx),INT(aforosed(i).utmy),  &
               INT(aforosed(i).elev),orig,(aforosed(i).obs(t),t=1,nt)
  aaa='Xs '
  WRITE (21,36) aaa,aforosed(i).name,INT(aforosed(i).utmx),INT(aforosed(i).utmy),  &
               INT(aforosed(i).elev),orig,(aforosed(i).sim(t),t=1,nt)
  aaa='Xsa'
    WRITE (21,36) aaa,aforosed(i).name,INT(aforosed(i).utmx),INT(aforosed(i).utmy),INT(aforosed(i).elev),orig,(aforosed(i).sed_out(t,1),t=1,nt)
    aaa='Xsl'
    WRITE (21,36) aaa,aforosed(i).name,INT(aforosed(i).utmx),INT(aforosed(i).utmy),INT(aforosed(i).elev),orig,(aforosed(i).sed_out(t,2),t=1,nt)
    aaa='Xsc'
    WRITE (21,36) aaa,aforosed(i).name,INT(aforosed(i).utmx),INT(aforosed(i).utmy),INT(aforosed(i).elev),orig,(aforosed(i).sed_out(t,3),t=1,nt)
  aaa='* '
  pder=0.0
  newnt=0
  estadsed=0.0
  DO t=nindex,nt
  	IF (aforosed(i).obs(t).ne.-1) THEN    !Condicional agregado para no contabilizar los faltantes
        estadsed(i,3)=estadsed(i,3)+aforosed(i).obs(t)*dts/1000000.0   !Volumen obs en Mm³
        estadsed(i+ksedq,3)=estadsed(i+ksedq,3)+aforosed(i).sim(t)*dts/1000000.0  !Volumen sim en Mm³
  	    IF (aforosed(i).obs(t).gt.estadsed(i,1)) THEN
	        estadsed(i,1)=aforosed(i).obs(t)	!Caudal pico Observado
	        estadsed(i,2)=dt*t       !Tiempo al pico Observado
	    ENDIF
  	    IF (aforosed(i).sim(t).gt.estadsed(i+ksedq,1)) THEN
	        estadsed(i+ksedq,1)=aforosed(i).sim(t)	!Caudal pico simulado
	        estadsed(i+ksedq,2)=dt*t       !Tiempo al pico simulado
	    ENDIF
	    estadsed(i,4)=estadsed(i,4)+(aforosed(i).obs(t)-aforosed(i).sim(t))*(aforosed(i).obs(t)-aforosed(i).sim(t))
        pder=pder+aforosed(i).obs(t)
        newnt=newnt+1
    ELSE
  	    IF (aforosed(i).sim(t).gt.estadsed(i+ksedq,1)) THEN
	        estadsed(i+ksedq,1)=aforosed(i).sim(t)	!Caudal pico simulado
	        estadsed(i+ksedq,2)=dt*t       !Tiempo al pico simulado
	    ENDIF
    ENDIF
  ENDDO
  pder=pder/newnt   !Caudal medio para NSE
  DO t=1,nt
    IF (aforosed(i).obs(t).ne.-1) estadsed(i+ksedq,4)=estadsed(i+ksedq,4)+(aforosed(i).obs(t)-pder)**2.0
  ENDDO
  
  !Indice sugerido por Moriasi 2007, RMSE-observations standard deviation ratio
  IF (estadsed(i+ksedq,4).ne.0.0) RSRindexsed(i)=(estadsed(i,4))**0.5/(estadsed(i+ksedq,4))**0.5
  
  IF (estadsed(i+ksedq,4).ne.0.0) estadsed(i+ksedq,4)=1.0-estadsed(i,4)/estadsed(i+ksedq,4)  !NSE Nash
  estadsed(i,4)=(estadsed(i,4)/newnt)**0.5  !RMSE
  IF (estadsed(i,2).eq.0.0) THEN
    estadsed(i,5)=9999.
  ELSE
    estadsed(i,5)=100.0*(estadsed(i,2)-estadsed(i+ksedq,2))/estadsed(i,2)
    IF (estadsed(i,5).gt.9999.9) estadsed(i,6)=9999.9
    IF (estadsed(i,5).lt.-9999.9) estadsed(i,6)=-9999.9
  ENDIF
  IF (estadsed(i,3).eq.0.0) THEN
    estadsed(i,6)=9999.
  ELSE
      estadsed(i,6)=-100.0*(estadsed(i,3)-estadsed(i+ksedq,3))/estadsed(i,3)
	  IF (estadsed(i,6).gt.9999.9) estadsed(i,6)=9999.9
	  IF (estadsed(i,6).lt.-9999.9) estadsed(i,6)=-9999.9
  ENDIF
  
  !Control de NaN
    DO j=1,12
        !IF (ieee_is_nan(estadsed(i,j))) THEN
        !Subtituimos la rutina para no emplear librerias(Vicente.Nov-2019)
        IF (is_nan(estadsed(i,j))) THEN
            estadsed(i,j)= -9999.0
        ENDIF
    ENDDO
  
  nam='Estadísticos principales'
  WRITE (21,37) aaa,nam,'Qmax Obs=',estadsed(i,1),'Qmax Sim=',estadsed(i+ksedq,1),' RMSE=', &
   estadsed(i,4),'Tpico Obs=',estadsed(i,2),'Tpico Sim=',estadsed(i+ksedq,2),'Err Tp(%)=',estadsed(i,5),  & 
   ' Vol Obs= ',estadsed(i,3),' Vol Sim=',estadsed(i+ksedq,3),'Err Vol(%)=',estadsed(i,6),  &
   ' Indice NSE= ',estadsed(i+ksedq,4),' Area Acum= ',aforosed(i).area, 'RSR Index= ',RSRindexsed(i)
  
ENDDO




DO i=1,knaf
  aaa='B  '
  WRITE (21,35) aaa,otros(i).name,INT(otros(i).utmx),INT(otros(i).utmy),  &
               INT(otros(i).elev)
  aaa='Bs '
  WRITE (21,36) aaa,otros(i).name,INT(otros(i).utmx),INT(otros(i).utmy),  &
               INT(otros(i).elev),orig,(otros(i).sim(t),t=1,nt)
               
  aaa='*  '
  DO t=1,nt
    estad(i+2*naf,3)=estad(i+2*naf,3)+otros(i).sim(t)*dts/1000000.0  !Volumen Sim en Mm³
  	IF (otros(i).sim(t).gt.estad(i+2*naf,1)) THEN
	  estad(i+2*naf,1)=otros(i).sim(t)	!Caudal pico Simulado
	  estad(i+2*naf,2)=dt*t       !Tiempo al pico simulado
	ENDIF
  ENDDO
  nam='Estadísticos principales'
  WRITE (21,38) aaa,nam,'Qmax Sim=',estad(i+2*naf,1),'Tpico Sim=',estad(i+2*naf,2),'Vol Sim=',estad(i+2*naf,3),  &
                ' Area Acum= ',otros(i).area
    IF (config(4)) THEN
    aaa='BXs'
      WRITE (21,36) aaa,otros(i).name,INT(otros(i).utmx),INT(otros(i).utmy),INT(otros(i).elev),orig,(otros(i).sed_out(t,4),t=1,nt)
    ENDIF
      aaa='*  '
  WRITE (21,'(A3)') aaa   
ENDDO
WRITE (21,'(A3)')'*  '
38 FORMAT(A3,'"',A25,'"',x,A19,F11.3,x,A10,F11.3,x,A9,F11.3,x,A12,F12.3)

IF (ktem.ne.0)THEN
  DO i=1,kniv
    aaa='H  '
    WRITE (21,34) aaa,nieve(i).name,INT(nieve(i).utmx),INT(nieve(i).utmy),  &
               INT(nieve(i).elev),orig,nieve(i).obs(1)
    aaa='Z  '
    WRITE (21,35) aaa,nieve(i).name,INT(nieve(i).utmx),INT(nieve(i).utmy),  &
               INT(nieve(i).elev),orig,(nieve(i).sim(t),t=1,nt)
  ENDDO
  DO i=1,ktem
    aaa='T  '
    WRITE (21,35) aaa,temper(i).name,INT(temper(i).utmx),INT(temper(i).utmy),  &
               INT(temper(i).elev),orig,(temper(i).obs(t),t=1,nt)
  ENDDO

  !Reescribe el fichero topologia, propiedades del suelo y tipologia  
  !Comento esta parte porque codnie realmente no se está leyendo ni modificando de topolco, 
  !se hace en NIEVE.asc, pero no en topolco Cris(17/10/2016)
!  DO n=1,ncel
!    IF (cell(n).h(0).eq.0.AND.cell(n).codnie.gt.0) cell(n).codnie=0 
!    IF (cell(n).dest.eq.ncel+1) THEN
!        cell(n).dest=0
!    ENDIF
!  ENDDO
!  artem=arch(3)
!  CALL escr_topol
ENDIF

IF(modulos2(3))THEN
    CALL estadisticos_nitr
    WRITE (21,'(A2)')'* '
    DO i=1,kno
        aaa='NOo'
        WRITE (21,35) aaa,norg(i).name,INT(norg(i).utmx),INT(norg(i).utmy),INT(norg(i).elev),orig,(norg(i).obs(t),t=1,nt)
        aaa='NOs'
        WRITE (21,35) aaa,norg(i).name,INT(norg(i).utmx),INT(norg(i).utmy),INT(norg(i).elev),orig,(norg(i).sim(t),t=1,nt)
        aaa='*  '
        nam='Estadísticos principales'
        WRITE (21,37) aaa,nam,' Qmax Obs=',estadnitr(i,1),'Qmax Sim=',estadnitr(i+nafn,1),' RMSE=', &
       estadnitr(i+nafn,4),'Tpico Obs=',estadnitr(i,2),'Tpico Sim=',estadnitr(i+nafn,2),'Err Tp=',estadnitr(i,5),  & 
       ' Vol Obs= ',estadnitr(i,7),' Vol Sim=',estadnitr(i+nafn,7),'Err Vol(%)=',estadnitr(i+nafn,5),  &
       ' Indice NSE= ',estadnitr(i+nafn,6),' Area Acum= ',norg(i).area,'RSR Index= ',RSRindexnitr(i)
    END DO
    DO i=1,kam
        aaa='AMo'
        WRITE (21,35) aaa,amonio(i).name,INT(amonio(i).utmx),INT(amonio(i).utmy),INT(amonio(i).elev),orig,(amonio(i).obs(t),t=1,nt)
        aaa='AMs'
        WRITE (21,35) aaa,amonio(i).name,INT(amonio(i).utmx),INT(amonio(i).utmy),INT(amonio(i).elev),orig,(amonio(i).sim(t),t=1,nt)
        aaa='*  '
        nam='Estadísticos principales'
        j = i + kno
        WRITE (21,37) aaa,nam,' Qmax Obs=',estadnitr(j,1),'Qmax Sim=',estadnitr(j+nafn,1),' RMSE=', &
       estadnitr(j+nafn,4),'Tpico Obs=',estadnitr(j,2),'Tpico Sim=',estadnitr(j+nafn,2),'Err Tp=',estadnitr(j,5),  & 
       ' Vol Obs= ',estadnitr(j,7),' Vol Sim=',estadnitr(j+nafn,7),'Err Vol(%)=',estadnitr(j+nafn,5),  &
       ' Indice NSE= ',estadnitr(j+nafn,6),' Area Acum= ',amonio(i).area,'RSR Index= ',RSRindexnitr(i)
    END DO
     DO i=1,kni
        aaa='NIo'
        WRITE (21,35) aaa,nitrato(i).name,INT(nitrato(i).utmx),INT(nitrato(i).utmy),INT(nitrato(i).elev),orig,(nitrato(i).obs(t),t=1,nt)
        aaa='NIs'
        WRITE (21,35) aaa,nitrato(i).name,INT(nitrato(i).utmx),INT(nitrato(i).utmy),INT(nitrato(i).elev),orig,(nitrato(i).sim(t),t=1,nt)
        aaa='*  '
        nam='Estadísticos principales'
        j = i + kno + kam
        WRITE (21,37) aaa,nam,' Qmax Obs=',estadnitr(j,1),'Qmax Sim=',estadnitr(j+nafn,1),' RMSE=', &
       estadnitr(j+nafn,4),'Tpico Obs=',estadnitr(j,2),'Tpico Sim=',estadnitr(j+nafn,2),'Err Tp=',estadnitr(j,5),  & 
       ' Vol Obs= ',estadnitr(j,7),' Vol Sim=',estadnitr(j+nafn,7),'Err Vol(%)=',estadnitr(j+nafn,5),  &
       ' Indice NSE= ',estadnitr(j+nafn,6),' Area Acum= ',nitrato(i).area,'RSR Index= ',RSRindexnitr(i)
    END DO
END IF    


WRITE (21,'(A2)')'* '
DO i=1,kevp
  DO t=1,nt
    evapo(i).obs(t)=evapo(i).obs(t)*24.0/dt  !mm/dia
  ENDDO
  aaa='E  '
  WRITE (21,35) aaa,evapo(i).name,INT(evapo(i).utmx),INT(evapo(i).utmy),  &
               INT(evapo(i).elev),orig,(evapo(i).obs(t),t=1,nt)
ENDDO
WRITE (21,'(A2)')'* '
IF (kevp.gt.0) THEN
  aaa='* '
  nam='ETP Media Cuenca '
  WRITE (21,30) aaa,nam,INT(0.0),INT(0.0),INT(0.0),orig,(preac(t,2),t=1,nt)
ENDIF
WRITE (21,'(A2)')'* '

DO i=1,kadi
  WRITE (21,'(A2)')'* '
  aaa='D  '
  WRITE (21,30) aaa,aport(i).name,INT(aport(i).utmx),INT(aport(i).utmy),  &
               INT(aport(i).elev),orig,(aport(i).obs(t),t=1,nt)
  aaa='Ds '
  WRITE (21,30) aaa,aport(i).name,INT(aport(i).utmx),INT(aport(i).utmy),  &
               INT(aport(i).elev),orig,(aport(i).sim(t),t=1,nt)
  DO t=1,nt
    estad(i+2*naf+knaf,3)=estad(i+2*naf+knaf,3)+aport(i).sim(t)*dts/1000000.0  !Volumen Sim en Hm³
  	IF (aport(i).sim(t).gt.estad(i+2*naf+knaf,1)) THEN
	  estad(i+2*naf+knaf,1)=aport(i).sim(t)  	!Caudal pico Simulado
	  estad(i+2*naf+knaf,2)=dt*t       !Tiempo al pico simulado
	ENDIF
  ENDDO
  aaa='* '
  nam='Estadísticos principales'
  WRITE (21,38) aaa,nam,' Qmax Sim=',estad(i+2*naf+knaf,1),'Tpico Sim=',estad(i+2*naf+knaf,2),' Vol Sim=',  &
        estad(i+2*naf+knaf,3),' Area Acum= ',aport(i).area
ENDDO

CALL almacenamientos


WRITE (21,'(A2,A40)')'* ',' Almacenamientos promedios en la cuenca '
aaa='X  '		 !X1 Precipitación
nam='X-01 Precipitación                            (mm)'
if(lang.eq.2) nam='X-01 Precipitation                            (mm)'
WRITE (21,36) aaa,nam,INT(0.0),INT(0.0),INT(0.0),orig,(balanc(t,1),t=1,nt)
aaa='X  '		 !X6 Lluvia directa
nam='X-06 Lluvia directa                           (mm)'
if(lang.eq.2) nam='X-06 Direct rainfall                          (mm)'
WRITE (21,36) aaa,nam,INT(0.0),INT(0.0),INT(0.0),orig,(balanc(t,2),t=1,nt)
aaa='X  '		 !X2 Almacenamiento gravitacional
nam='X-02 Excedente                                (mm)'
if(lang.eq.2) nam='X-02 Excess water                             (mm)'
WRITE (21,36) aaa,nam,INT(0.0),INT(0.0),INT(0.0),orig,(balanc(t,3),t=1,nt)
aaa='X  '		 !X3 Infiltración
nam='X-03 Infiltración                             (mm)'
if(lang.eq.2) nam='X-03 Infiltration                             (mm)'
WRITE (21,36) aaa,nam,INT(0.0),INT(0.0),INT(0.0),orig,(balanc(t,4),t=1,nt)
aaa='X  '		 !X4 Percolación
nam='X-04 Percolación                              (mm)'
if(lang.eq.2) nam='X-04 Percolation                              (mm)'
WRITE (21,36) aaa,nam,INT(0.0),INT(0.0),INT(0.0),orig,(balanc(t,5),t=1,nt)
aaa='X  '		 !X5 Pérdidas subterráneas
nam='X-05 Pérdidas subterraneas                    (mm)'
if(lang.eq.2) nam='X-05 Deep underground floor                   (mm)'
WRITE (21,36) aaa,nam,INT(0.0),INT(0.0),INT(0.0),orig,(balanc(t,6),t=1,nt)
IF(modulos(5)) THEN !Riego
   aaa='X  '	 !X7 Riego	 
   nam='X-07 Riego                                    (mm)'
   if(lang.eq.2) nam='X-07 Irrigation                               (mm)'
   WRITE (21,36) aaa,nam,INT(0.0),INT(0.0),INT(0.0),orig,(balanc(t,28),t=1,nt)
END IF 
IF(config(5)) THEN !Vegetacion
   aaa='X  '	 !X8 Excedente de la capa más superficial	 
   nam='X-08 Excedente de la capa más superficial     (mm)'
   if(lang.eq.2) nam='X-08 Excedence of the shallow soil layer      (mm)'
   WRITE (21,36) aaa,nam,INT(0.0),INT(0.0),INT(0.0),orig,(balanc(t,24),t=1,nt)
END IF
WRITE (21,'(A2)')'* '
aaa='Y  '		 !Y0 Fusión de nieve 
nam='Y-00 Fusión de nieve en la cuenca             (mm)'
if(lang.eq.2) nam='Y-00 Snow melting                             (mm)'
WRITE (21,36) aaa,nam,INT(0.0),INT(0.0),INT(0.0),orig,(balanc(t,20),t=1,nt)
aaa='Y  '		 !Y1 Evaporación 
nam='Y-06 Evaporación agua interceptada por la veg.(mm)'
if(lang.eq.2) nam='Y-06 Evaporation from vegetation cover        (mm)'
WRITE (21,36) aaa,nam,INT(0.0),INT(0.0),INT(0.0),orig,(balanc(t,7),t=1,nt)
aaa='Y  '		 !Y2 Evapotranspiración 
nam='Y-01 Evapotranspiración                       (mm)'
if(lang.eq.2) nam='Y-01 Real evapotraspiration                   (mm)'
WRITE (21,36) aaa,nam,INT(0.0),INT(0.0),INT(0.0),orig,(balanc(t,8),t=1,nt)
IF(config(5)) THEN
    aaa='Y  '		 
    nam='Tr Transpiración total                        (mm)'
    if(lang.eq.2) nam='Tr Total Transpiración                        (mm)'
   WRITE (21,36) aaa,nam,INT(0.0),INT(0.0),INT(0.0),orig,(balanc(t,25)+balanc(t,26),t=1,nt)
   aaa='Y  '		 
   nam='Es Evaporación del suelo desnudo              (mm)'
   if(lang.eq.2) nam='Es Evaporation from bare soil                 (mm)'
   WRITE (21,36) aaa,nam,INT(0.0),INT(0.0),INT(0.0),orig,(balanc(t,27),t=1,nt)
END IF
aaa='Y  '		 !Y2 Escorrentía directa
nam='Y-02 Escorrentía directa                      (mm)'
if(lang.eq.2) nam='Y-02 Overland flow                            (mm)'
WRITE (21,36) aaa,nam,INT(0.0),INT(0.0),INT(0.0),orig,(balanc(t,9),t=1,nt)
aaa='Y  '		 !Y3 Flujo Subsuperficial
nam='Y-03 Interflujo                               (mm)'
if(lang.eq.2) nam='Y-03 Interflow                                (mm)'
WRITE (21,36) aaa,nam,INT(0.0),INT(0.0),INT(0.0),orig,(balanc(t,10),t=1,nt)
aaa='Y  '		 !Y4 Flujo Base   
nam='Y-04 Flujo Subterráneo                        (mm)'
if(lang.eq.2) nam='Y-04 Base flow                                (mm)'
WRITE (21,36) aaa,nam,INT(0.0),INT(0.0),INT(0.0),orig,(balanc(t,11),t=1,nt)
WRITE (21,'(A2)')'* '
aaa='J  '		 !H1 Almacenamiento en las hojas
nam='H-06 Volumen almacenado en las hojas          (mm)'
if(lang.eq.2) nam='H-06 Vegetation cover storage                 (mm)'
WRITE (21,36) aaa,nam,INT(0.0),INT(0.0),INT(0.0),orig,(balanc(t,12),t=1,nt)
aaa='J  '		 !H1 Almacenamiento en el suelo
IF(config(5)) THEN
    nam='H-01 Vol. almac. capilar en capa profunda     (mm)'
    if(lang.eq.2)  nam='H-01 Soil saturation in the deeper soil layer (mm)'
ELSE
    nam='H-01 Volumen almacenamiento capilar           (mm)'
    if(lang.eq.2)  nam='H-01 Soil saturation                          (mm)'
END IF
WRITE (21,36) aaa,nam,INT(0.0),INT(0.0),INT(0.0),orig,(balanc(t,13),t=1,nt)
aaa='J  '		 !H1 Almacenamiento en superficie
nam='H-02 Volumen almac. en superficie             (mm)'
if(lang.eq.2) nam='H-02 Superficial storage                      (mm)'
WRITE (21,36) aaa,nam,INT(0.0),INT(0.0),INT(0.0),orig,(balanc(t,14),t=1,nt)
aaa='J  '		 !H2 Almacenamiento en Suelo
nam='H-03 Volumen almac. subsuperficial            (mm)'
if(lang.eq.2) nam='H-03 Gravitational storage                    (mm)'
WRITE (21,36) aaa,nam,INT(0.0),INT(0.0),INT(0.0),orig,(balanc(t,15),t=1,nt)
aaa='J  '		 !H3  Almacenamiento en Roca
nam='H-04 Volumen almac. en el acuífero            (mm)'
if(lang.eq.2) nam='H-04 Aquifer storage                          (mm)'
WRITE (21,36) aaa,nam,INT(0.0),INT(0.0),INT(0.0),orig,(balanc(t,16),t=1,nt)
aaa='J  '		 !H5 Cauce (despues de transferencia O. C. G. )
nam='H-05 Volumen almac. en los cauces             (mm)'
if(lang.eq.2) nam='H-05 Channel storage                          (mm)'
WRITE (21,36) aaa,nam,INT(0.0),INT(0.0),INT(0.0),orig,(balanc(t,17),t=1,nt)
aaa='J  '		 !H6 Nieve
nam='H-00 Volumen Nieve                            (mm)'
if(lang.eq.2) nam='H-00 Snow pack                                (mm)'
WRITE (21,36) aaa,nam,INT(0.0),INT(0.0),INT(0.0),orig,(balanc(t,18),t=1,nt)
IF(config(5)) THEN
    aaa='J  '		 !H-08 Vol. almac. capilar en capa superficial
    nam='H-08 Vol. almac. capilar en capa superficial  (mm)'
    if(lang.eq.2) nam='H-08 Storage of the shallow soil layer        (mm)'
   WRITE (21,36) aaa,nam,INT(0.0),INT(0.0),INT(0.0),orig,(balanc(t,23),t=1,nt)
END IF
WRITE (21,'(A2)') '* '
WRITE(21,'(A23)')'* PARÁMETROS EMPLEADOS '
WRITE(21,'(A48)')'*    Geomorfológicos  [Coeficiente y Exponente] '
WRITE(21,'(A11)')'*    Cauce '
76 FORMAT(a20,<2*npar>(x,F11.5))
77 FORMAT(a20,<npar>(x,F23.5))
WRITE(21,76)'*                 : ',(d(1,j),e(1,j),j=1,npar)
WRITE(21,76)'*                 : ',(d(3,j),e(3,j),j=1,npar)
WRITE(21,77)'*                 : ',(e(4,j),j=1,npar)
WRITE(21,76)'*                 : ',(d(5,j),e(5,j),j=1,npar)
WRITE(21,76)'*                 : ',(d(6,j),e(6,j),j=1,npar)
!parametros para cárcavas
WRITE(21,'(A48)')'*    Geomorfológicos  [Coeficiente y Exponente] '
WRITE(21,'(A14)')'*    Cárcavas '
WRITE(21,76)'*                 : ',(dc(1,j),ec(1,j),j=1,npar)
WRITE(21,76)'*                 : ',(dc(3,j),ec(3,j),j=1,npar)
WRITE(21,77)'*                 : ',(ec(4,j),j=1,npar)
WRITE(21,76)'*                 : ',(dc(5,j),ec(5,j),j=1,npar)
WRITE(21,76)'*                 : ',(dc(6,j),ec(6,j),j=1,npar)
!**********************************
WRITE(21,'(A36,x,F12.7)')'* Factor de interpolación de la PPT:',betappt
WRITE(21,'(A53,x,F15.4)')'* Parámetro de control de la ET en la zona saturada: ',hped

!Escribe los valores medios de los mapas de parámetros utilizados
WRITE (21,'(A2)') '* '
WRITE (21,'(A47)') '* Valores medios mapas de parámetros utilizados: '
WRITE (21,'(A2)') '* '
WRITE (21,'(A100)') '*  --Hu(mm)--- --Ks(mm)--- --Kp(mm)--- --Kss(mm)-- --Ksa(mm)-- --Kps(mm)-- --Veloc(m/s)-- --Imax(mm)--'
WRITE (21,'(A1,8(x,F11.4))') 'a',(averagepar(j), j=1,7),Imaxmedio

!Escribe humedades antecedentes para eco datos
WRITE(21,'(A23)')'* Humedad antecedente  '
WRITE(21,87)(wdad(1,j),j=1,npar)
WRITE(21,87)(wdad(2,j),j=1,npar)
WRITE(21,87)(wdad(3,j),j=1,npar)
WRITE(21,87)(wdad(4,j),j=1,npar)
WRITE(21,87)(wdad(5,j),j=1,npar)
WRITE(21,87)(wdad(6,j),j=1,npar)
IF(config(5)) THEN
    WRITE(21,87)(wdad(7,j),j=1,npar)
    WRITE(21,87)(wdad(8,j),j=1,npar)
    WRITE(21,87)(laiini(j),j=1,npar)
END IF
87 FORMAT(<npar>(F10.5,x))
WRITE(21,'(a20,10(x,F12.5))')'*    Calibración    ',(r(j),j=1,10)
WRITE (21,'(A2)') '* '
IF (ktem.ne.0) THEN
  WRITE(21,'(a48,4(x,f9.4),f9.6)')'* Parámetros nieve (Beta,ro1,ro2,Tb,betatemp) :',bbeta,ro1,ro2,tbase,betatemp  
  WRITE (21,'(A2)') '* '
ENDIF
IF(modulos2(3)) THEN !NITROGENO ACTIVADO
    nam = 'Nitr: Flujos medios en la cuenca'
    if(lang.eq.2) nam='Nitr: Average flows within the catchment'  
    WRITE (21,'(A2,A45)')'* ',nam
    aaa='FN '
    nam='FN-1 NH4Input (kgN)'
    WRITE (21,36) aaa,nam,INT(0.0),INT(0.0),INT(0.0),orig,(balanc_nitr(t,54),t=1,nt)
    nam='FN-2 NO3Input (kgN)'
    WRITE (21,36) aaa,nam,INT(0.0),INT(0.0),INT(0.0),orig,(balanc_nitr(t,55),t=1,nt)
    nam='FN-3 NH4AtmDep (kgN)'
    WRITE (21,36) aaa,nam,INT(0.0),INT(0.0),INT(0.0),orig,(balanc_nitr(t,59),t=1,nt)
    nam='FN-4 NO3AtmDep (kgN)'
    WRITE (21,36) aaa,nam,INT(0.0),INT(0.0),INT(0.0),orig,(balanc_nitr(t,60),t=1,nt)
    nam='FN-5 SoilMin (kgN)'
    WRITE (21,36) aaa,nam,INT(0.0),INT(0.0),INT(0.0),orig,(balanc_nitr(t,1),t=1,nt)
    nam='FN-6 SoilImm (kgN)'
    WRITE (21,36) aaa,nam,INT(0.0),INT(0.0),INT(0.0),orig,(balanc_nitr(t,2),t=1,nt)
    nam='FN-7 SoilVol (kgN)'
    WRITE (21,36) aaa,nam,INT(0.0),INT(0.0),INT(0.0),orig,(balanc_nitr(t,58),t=1,nt)
    nam='FN-8 SoilNitr (kgN)'
    WRITE (21,36) aaa,nam,INT(0.0),INT(0.0),INT(0.0),orig,(balanc_nitr(t,3),t=1,nt)
    nam='FN-9 SoilFix (kgN)'
    WRITE (21,36) aaa,nam,INT(0.0),INT(0.0),INT(0.0),orig,(balanc_nitr(t,4),t=1,nt)
    nam='FN-10 SoilDenitr (kgN)'
    WRITE (21,36) aaa,nam,INT(0.0),INT(0.0),INT(0.0),orig,(balanc_nitr(t,5),t=1,nt)
    nam='FN-11 NH4PassiveUpt (kgN)'
    WRITE (21,36) aaa,nam,INT(0.0),INT(0.0),INT(0.0),orig,(balanc_nitr(t,10),t=1,nt)
    nam='FN-12 NO3PassiveUpt (kgN)'
    WRITE (21,36) aaa,nam,INT(0.0),INT(0.0),INT(0.0),orig,(balanc_nitr(t,11),t=1,nt)
    nam='FN-13 NH4ActiveUpt (kgN)'
    WRITE (21,36) aaa,nam,INT(0.0),INT(0.0),INT(0.0),orig,(balanc_nitr(t,31),t=1,nt)
    nam='FN-14 NO3ActiveUpt (kgN)'
    WRITE (21,36) aaa,nam,INT(0.0),INT(0.0),INT(0.0),orig,(balanc_nitr(t,32),t=1,nt)
    nam='FN-15 NH4Perc (kgN)'
    WRITE (21,36) aaa,nam,INT(0.0),INT(0.0),INT(0.0),orig,(balanc_nitr(t,6),t=1,nt)
    nam='FN-16 NO3Perc (kgN)'
    WRITE (21,36) aaa,nam,INT(0.0),INT(0.0),INT(0.0),orig,(balanc_nitr(t,7),t=1,nt)
    nam='FN-17 NH4Losses (kgN)'
    WRITE (21,36) aaa,nam,INT(0.0),INT(0.0),INT(0.0),orig,(balanc_nitr(t,8),t=1,nt)
    nam='FN-18 NO3Losses (kgN)'
    WRITE (21,36) aaa,nam,INT(0.0),INT(0.0),INT(0.0),orig,(balanc_nitr(t,9),t=1,nt)
    nam='FN-19 SupfFlowMin (kgN)'
    WRITE (21,36) aaa,nam,INT(0.0),INT(0.0),INT(0.0),orig,(balanc_nitr(t,16),t=1,nt)
    nam='FN-20 SupfFlowNitr (kgN)'
    WRITE (21,36) aaa,nam,INT(0.0),INT(0.0),INT(0.0),orig,(balanc_nitr(t,17),t=1,nt)
    nam='FN-21 SupfFlowNitr (kgN)'
    WRITE (21,36) aaa,nam,INT(0.0),INT(0.0),INT(0.0),orig,(balanc_nitr(t,18),t=1,nt)
    nam='FN-22 DirRunON (kgN)'
    WRITE (21,36) aaa,nam,INT(0.0),INT(0.0),INT(0.0),orig,(balanc_nitr(t,24),t=1,nt)
    nam='FN-23 NH4DirectRun (kgN)'
    WRITE (21,36) aaa,nam,INT(0.0),INT(0.0),INT(0.0),orig,(balanc_nitr(t,25),t=1,nt)
    nam='FN-24 NO3DirectRun (kgN)'
    WRITE (21,36) aaa,nam,INT(0.0),INT(0.0),INT(0.0),orig,(balanc_nitr(t,26),t=1,nt)
    nam='FN-25 NH4SubSupFlo (kgN)'
    WRITE (21,36) aaa,nam,INT(0.0),INT(0.0),INT(0.0),orig,(balanc_nitr(t,12),t=1,nt)
    nam='FN-26 NO3SubSupFlo (kgN)'
    WRITE (21,36) aaa,nam,INT(0.0),INT(0.0),INT(0.0),orig,(balanc_nitr(t,13),t=1,nt)
    nam='FN-27 NH4ConnFlow (kgN)'
    WRITE (21,36) aaa,nam,INT(0.0),INT(0.0),INT(0.0),orig,(balanc_nitr(t,14),t=1,nt)
    nam='FN-28 NO3ConnFlow (kgN)'
    WRITE (21,36) aaa,nam,INT(0.0),INT(0.0),INT(0.0),orig,(balanc_nitr(t,15),t=1,nt)
    nam='FN-29 ONFlow (kgN)'
    WRITE (21,36) aaa,nam,INT(0.0),INT(0.0),INT(0.0),orig,(balanc_nitr(t,19),t=1,nt)
    nam='FN-30 NH4Flow (kgN)'
    WRITE (21,36) aaa,nam,INT(0.0),INT(0.0),INT(0.0),orig,(balanc_nitr(t,20),t=1,nt)
    nam='FN-31 NO3Flow (kgN)'
    WRITE (21,36) aaa,nam,INT(0.0),INT(0.0),INT(0.0),orig,(balanc_nitr(t,21),t=1,nt)
    nam='FN-32 NH4Exfil (kgN)'
    WRITE (21,36) aaa,nam,INT(0.0),INT(0.0),INT(0.0),orig,(balanc_nitr(t,56),t=1,nt)
    nam='FN-33 NO3Exfil (kgN)'
    WRITE (21,36) aaa,nam,INT(0.0),INT(0.0),INT(0.0),orig,(balanc_nitr(t,57),t=1,nt)
    IF (config(4)) then
        nam='FN-34 ONSedFlow (kgN)'
        WRITE (21,36) aaa,nam,INT(0.0),INT(0.0),INT(0.0),orig,(balanc_nitr(t,29),t=1,nt)
        nam='FN-35 NH4SedFlow (kgN)'
        WRITE (21,36) aaa,nam,INT(0.0),INT(0.0),INT(0.0),orig,(balanc_nitr(t,30),t=1,nt)
    END IF
    
    nam = 'Nitr: Almacenamientos medios en la cuenca'
    if(lang.eq.2) nam='Nitr: Average storages within the catchment'  
    WRITE (21,'(A2,A50)')'* ',nam
    aaa='HN '
    nam='HN-0 SoilON (kgN)'
    WRITE (21,36) aaa,nam,INT(0.0),INT(0.0),INT(0.0),orig,(balanc_nitr(t,33),t=1,nt)
    nam='HN-8 SoilAdsorpNH4 (kgN)'
    WRITE (21,36) aaa,nam,INT(0.0),INT(0.0),INT(0.0),orig,(balanc_nitr(t,36),t=1,nt)
    nam='HN-1 SoilDissNH4 (kgN)'
    WRITE (21,36) aaa,nam,INT(0.0),INT(0.0),INT(0.0),orig,(balanc_nitr(t,34),t=1,nt)
    nam='HN-2 SoilNO3 (kgN)'
    WRITE (21,36) aaa,nam,INT(0.0),INT(0.0),INT(0.0),orig,(balanc_nitr(t,35),t=1,nt)
    nam='HN-3 SupfDissON (kgN)'
    WRITE (21,36) aaa,nam,INT(0.0),INT(0.0),INT(0.0),orig,(balanc_nitr(t,37),t=1,nt)
    nam='HN-4 SupfDissNH4 (kgN)'
    WRITE (21,36) aaa,nam,INT(0.0),INT(0.0),INT(0.0),orig,(balanc_nitr(t,38),t=1,nt)
    nam='HN-5 SupfNO3 (kgN)'
    WRITE (21,36) aaa,nam,INT(0.0),INT(0.0),INT(0.0),orig,(balanc_nitr(t,39),t=1,nt)
    nam='HN-6 AquifDissNH4 (kgN)'
    WRITE (21,36) aaa,nam,INT(0.0),INT(0.0),INT(0.0),orig,(balanc_nitr(t,44),t=1,nt)
    nam='HN-7 AquifDissNO3 (kgN)'
    WRITE (21,36) aaa,nam,INT(0.0),INT(0.0),INT(0.0),orig,(balanc_nitr(t,45),t=1,nt)
    IF (config(4)) then
        nam='HN-9 NOSedSusCauce (kgN)'
        WRITE (21,36) aaa,nam,INT(0.0),INT(0.0),INT(0.0),orig,(balanc_nitr(t,40),t=1,nt)
        nam='HN-10 NH4SedSusCauce (kgN)'
        WRITE (21,36) aaa,nam,INT(0.0),INT(0.0),INT(0.0),orig,(balanc_nitr(t,41),t=1,nt)
        nam='HN-11 NOSedDepCauce (kgN)'
        WRITE (21,36) aaa,nam,INT(0.0),INT(0.0),INT(0.0),orig,(balanc_nitr(t,42),t=1,nt)
        nam='HN-12 NH4SedDepCauce (kgN)'
        WRITE (21,36) aaa,nam,INT(0.0),INT(0.0),INT(0.0),orig,(balanc_nitr(t,43),t=1,nt)
    END IF
END IF  
!Escribe los valores medios de los estados finales de los tanques
WRITE (21,'(A2)') '* '
WRITE (21,'(A47)') '* Valores medios de los estados finales de los tanques: '
WRITE (21,'(A2)') '* '
WRITE (21,40) '*','H-1 Almacenamiento estatico (%) :',(wdadfin(1))    
WRITE (21,40) '*','H-2 Agua en superficie (mm)     :',(wdadfin(2))
WRITE (21,40) '*','H-3 Almacenamiento gravitac (mm):',(wdadfin(3))
WRITE (21,40) '*','H-4 Nivel del acuífero (mm)     :',(wdadfin(4))
WRITE (21,40) '*','H-5 Cauce a seccion llena (%)   :',(wdadfin(5))
WRITE (21,40) '*','H-6 Tanque intercepción (%)     :',(wdadfin(6))
WRITE (21,'(A2)') '* '

30 FORMAT(A3,'"',A25,'"',x,I8,x,I8,x,I4,x,F3.1,<nt>(x,F11.3))
34 FORMAT(A3,'"',A25,'"',x,I8,x,I8,x,I4,x,F3.1,x,F11.4)
35 FORMAT(A3,'"',A25,'"',x,I8,x,I8,x,I4,x,F3.1,<nt>(x,F11.4))
36 FORMAT(A3,'"',A50,'"',x,I8,x,I8,x,I4,x,F3.1,<nt>(x,F11.4))
37 FORMAT(A3,'"',A25,'"',x,A10,F15.3,x,A10,F15.3,A7,F15.3,x,A10,F15.3,x,A10,F15.3,x,A10,F15.3,A10,F15.3,A10,F15.3,x,A12,F15.3,x,A12,F15.4,x,A12,F15.3,x,A11,F15.4)
40 FORMAT(A1,x,A50,<npar>(x,F9.3))   

CLOSE(21)

END SUBROUTINE

!****************************************************************
!* Escribe fichero de Topologia y edafologia (*.SDS)
!* 23/02/09 - escribe también los parametros de vegetación
!****************************************************************
SUBROUTINE escr_topol !NO SE UTILIZA!!!!
USE modtet
IMPLICIT NONE

OPEN (18,file=artem)


  IF (lang.eq.1) THEN
        WRITE (18,*)'NORTE-SUR:',cn,cs
        WRITE (18,*)'ESTE-OESTE:',ce,cw
        WRITE (18,*)'COLUMNAS:',mi
        WRITE (18,*)'FILAS:',mj
        WRITE (18,*)'COL-FINAL:',cell(ncel).fil
        WRITE (18,*)'FIL-FINAL:',cell(ncel).col
        WRITE (18,*)'NUM-CELDAS:',ncel
  ELSE IF (lang.eq.2) THEN
        WRITE (18,*)'NORTH-SOUTH:',cn,cs
        WRITE (18,*)'EAST-WEST:',ce,cw
        WRITE (18,*)'COLUMNS:',mi
        WRITE (18,*)'ROWS:',mj
        WRITE (18,*)'COL-FINAL:',cell(ncel).fil
        WRITE (18,*)'ROW-FINAL:',cell(ncel).col
        WRITE (18,*)'CELL-NUM:',ncel
  ENDIF


  DO n=1,ncel                                   
    WRITE (18,108) cell(n).fil,cell(n).col,cell(n).dest,cell(n).acum,cell(n).pend,  &
                 cell(n).cota,cell(n).codnie,cell(n).hu,cell(n).ks,cell(n).kp,  &
                 cell(n).kss,cell(n).ksa,cell(n).kps,cell(n).veloc,  &
                 cell(n).codpar,cell(n).codveg,cell(n).codrie,cell(n).codcal, &
				 cell(n).ordrie,cell(n).hstar,cell(n).dc,cell(n).rs, &
				 cell(n).rad(1),cell(n).rad(2),cell(n).rad(3),cell(n).rad(4),cell(n).rad(5),cell(n).rad(6),  &
				 cell(n).porcentaje(1),cell(n).porcentaje(2),cell(n).porcentaje(3), &
				 cell(n).Cusle,cell(n).Kusle,cell(n).Pusle,cell(n).codkarst,cell(n).hlim1,cell(n).hstar1,cell(n).hlim2,cell(n).hstar2,cell(n).fc,cell(n).hu1,cell(n).hu2
  ENDDO
  WRITE (18,'(A2)') '* '
  WRITE (18,'(A47)') '* Valores medios mapas de parámetros utilizados: '
  WRITE (18,'(A2)') '* '
  WRITE (18,'(A100)') '* ----Hu(mm)-- -Ks(mm/h)-- -Kp(mm/h)-- -Kss(mm/h)- -Ksa(mm/h)- -Kps(mm/h)- --Veloc(m/s)-- --Imax(mm)--'
  WRITE (18,'(8F12.4)') (averagepar(j), j=1,7),Imaxmedio

CLOSE(18)

!106 FORMAT(4I9,2I7,I5,3F12.4,4I5,I7,3F8.3,3F8.2)
108 FORMAT(4I9,F12.5,I7,I5,6F15.7,F12.5,4I5,I7,3F12.4,6F12.5,6F12.3,I5,7F12.5)
107 FORMAT(4I9,F12.5,I7,I5,6F15.7,F12.5,4I5,I7,3F12.4,6F12.5)

END SUBROUTINE

!*************************************************************************
!* Rutina que escribe en el formato columna el listado de estaciones
!* y las series temporales. Requiere tener en memoria toda la información
!* y utiliza la UNIDAD 14 en el archivo de lectura
!*************************************************************************
SUBROUTINE escr_epis_col
USE modtet
!USE DFLIB
IMPLICIT none

INTEGER tpo

!Escribe localizacion de las estaciones
WRITE(14,'(A45)') '* RESUMEN DE INFORMACION SOBRE LAS ESTACIONES'
WRITE (14,'(A2)') '* '
WRITE(14,'(A70)') '* "Nombre de la estacion    "   Este(UTM-X) Norte(UTM-Y)   Cota(msnm) '!  celda-(i,j)'
DO i=1,kppt
  WRITE(14,75)pluvio(i).codigo,'"',pluvio(i).name,'"',INT(pluvio(i).utmx),  &
              INT(pluvio(i).utmy),INT(pluvio(i).elev)
ENDDO
DO i=1,nemb
  WRITE(14,75)nivel(i).codigo,'"',nivel(i).name,'"',INT(nivel(i).utmx),  &
              INT(nivel(i).utmy),INT(nivel(i).elev)
ENDDO
!DO i=nemb+1,nemb+vnemb Cris (23/11/2016)
DO i=1,vnemb
  WRITE(14,75)volum(i).codigo,'"',volum(i).name,'"',INT(volum(i).utmx),  &
              INT(volum(i).utmy),INT(volum(i).elev)
ENDDO
!DO i=nemb+vnemb+1,nemb+vnemb+knemb Cris (23/11/2016)
DO i=1,knemb
  WRITE(14,75)qemb(i).codigo,'"',qemb(i).name,'"',INT(qemb(i).utmx),  &
              INT(qemb(i).utmy),INT(qemb(i).elev)
ENDDO
DO i=1,naf
  WRITE(14,75)aforo(i).codigo,'"',aforo(i).name,'"',INT(aforo(i).utmx),  &
              INT(aforo(i).utmy),INT(aforo(i).elev)
ENDDO
DO i=1,knaf
  WRITE(14,75)otros(i).codigo,'"',otros(i).name,'"',INT(otros(i).utmx),  &
              INT(otros(i).utmy),INT(otros(i).elev)
ENDDO
DO i=1,kniv
  WRITE(14,75)nieve(i).codigo,'"',nieve(i).name,'"',INT(nieve(i).utmx),  &
              INT(nieve(i).utmy),INT(nieve(i).elev)
ENDDO
DO i=1,ktem
  WRITE(14,75)temper(i).codigo,'"',temper(i).name,'"',INT(temper(i).utmx),  &
              INT(temper(i).utmy),INT(temper(i).elev)
ENDDO
DO i=1,kevp
  WRITE(14,75)evapo(i).codigo,'"',evapo(i).name,'"',INT(evapo(i).utmx),  &
              INT(evapo(i).utmy),INT(evapo(i).elev)
ENDDO
DO i=1,kadi
  WRITE(14,75)aport(i).codigo,'"',aport(i).name,'"',INT(aport(i).utmx),  &
              INT(aport(i).utmy),INT(aport(i).elev)
ENDDO
DO i=1,ksedq
  WRITE(14,75)aforosed(i).codigo,'"',aforosed(i).name,'"',INT(aforosed(i).utmx),  &
              INT(aforosed(i).utmy),INT(aforosed(i).elev)
ENDDO
DO i=1,nveg
  WRITE(14,75)veg(i).codigo,'"',veg(i).name,'"',INT(veg(i).utmx),  &
              INT(veg(i).utmy),INT(veg(i).elev)
ENDDO
DO i=1,nradiacion
  WRITE(14,75)radiacion(i).codigo,'"',radiacion(i).name,'"',INT(radiacion(i).utmx),  &
              INT(radiacion(i).utmy),INT(radiacion(i).elev)
ENDDO
DO i=1,kadised1
  WRITE(14,75)aportsed1(i).codigo,'"',aportsed1(i).name,'"',INT(aportsed1(i).utmx),  &
              INT(aportsed1(i).utmy),INT(aportsed1(i).elev)
ENDDO
DO i=1,kadised2
  WRITE(14,75)aportsed2(i).codigo,'"',aportsed2(i).name,'"',INT(aportsed2(i).utmx),  &
              INT(aportsed2(i).utmy),INT(aportsed2(i).elev)
ENDDO
DO i=1,kadised3
  WRITE(14,75)aportsed3(i).codigo,'"',aportsed3(i).name,'"',INT(aportsed3(i).utmx),  &
              INT(aportsed3(i).utmy),INT(aportsed3(i).elev)
ENDDO
75 FORMAT(A2,A1,A25,A1,x,3(x,I11))
WRITE (14,'(A2)') '* '

!Escribe series temporales en el mismo orden
!El fichero de columna debe tener el orden P,N,V,S,Q,B,H,T,E,D,X,W,R,DA,DL,DC
WRITE (14,'(A30)') '* Series temporales de entrada' 
WRITE (14,'(A2)') '* '
WRITE(14,'(A1,A9,<ktotal>(A7,A2))') '*',' ------DT',(' ------',pluvio(j).codigo,j=1,kppt),  &
               (' ------',nivel(j).codigo,j=1,nemb),(' ------',volum(j).codigo,j=1,vnemb),  &
	           (' ------',qemb(j).codigo,j=1,knemb),(' ------',aforo(j).codigo,j=1,naf),    &
	           (' ------',otros(j).codigo,j=1,knaf),(' ------',nieve(j).codigo,j=1,kniv),   &
	           (' ------',temper(j).codigo,j=1,ktem),(' ------',evapo(j).codigo,j=1,kevp),  &
	           (' ------',aport(j).codigo,j=1,kadi),(' ------',aforosed(j).codigo,j=1,ksedq),     &
	           (' ------',veg(j).codigo,j=1,nveg),(' ------',radiacion(j).codigo,j=1,nradiacion), &
	           (' ------',aportsed1(j).codigo,j=1,kadised1),(' ------',aportsed2(j).codigo,j=1,kadised2), &
	           (' ------',aportsed3(j).codigo,j=1,kadised3) !Añado lo relativo a vegetación dinámica y sedimentos Cris (23/11/2016)
orig=0.0
DO t=1,nt
  tpo=t*dtmin/60.
  WRITE(14,'(<ktotal+1>(F8.3,x))')tpo,(pluvio(j).obs(t),j=1,kppt),  &
     (nivel(j).obs(t),j=1,nemb),(volum(j).obs(t),j=1,vnemb),(qemb(j).obs(t),j=1,knemb),     &
	 (aforo(j).obs(t),j=1,naf),(orig,j=1,knaf),(nieve(j).obs(1),j=1,kniv),       &
	 (temper(j).obs(t),j=1,ktem),(evapo(j).obs(t),j=1,kevp),(aport(j).obs(t),j=1,kadi),(aforosed(j).obs(t),j=1,ksedq), &
	 (veg(j).obs(t),j=1,nveg),(radiacion(j).obs(t),j=1,nradiacion),(aportsed1(j).obs(t),j=1,kadised1),  &
	 (aportsed2(j).obs(t),j=1,kadised2),(aportsed3(j).obs(t),j=1,kadised3) !Añado lo relativo a vegetación dinámica y sedimentos Cris (23/11/2016)
ENDDO

END SUBROUTINE

!******************************************************************
!* Escritura de VARIABLES AMBIENTALES TIPO DUMMY en fichero texto
!******************************************************************
SUBROUTINE escri_dum
USE modtet
IMPLICIT NONE

sale=0
!Lee fichero dummy.txt
OPEN (18,file=artem)
WRITE(18,*)dirdum
!Variable principal
WRITE(18,*)fileppal(1)
WRITE(18,*)fileppal(2)
WRITE(18,*)fileppal(3)
!Variables Continuas que dependen del MED
WRITE(18,*)chkcon(1)
WRITE(18,*)chkcon(4)
WRITE(18,*)chkcon(4)
WRITE(18,*)chkcon(4)
!Variables Dummy que dependen del MED
WRITE(18,*)chkdum(1),umbdum(1)
WRITE(18,*)filedum(1)
WRITE(18,*)chkdum(2),umbdum(2)
WRITE(18,*)filedum(2)
WRITE(18,*)chkdum(3),umbdum(3),umbdum(4)
WRITE(18,*)filedum(3)
!Variables dummy (Otras)
DO i=4,12
  WRITE(18,*)chkdum(i),filedum(i)
ENDDO
CLOSE(18)
END SUBROUTINE

!******************************************************************
!* Escritura de VARIABLES AMBIENTALES TIPO DUMMY en fichero texto
!******************************************************************
!Escribe resumen de almacenamientos y flujos
SUBROUTINE almacenamientos
! Antes algunas de estas variables estaban pensadas para ser escritas en Hm3
! Ahora se calcula todo en mm
USE modtet
IMPLICIT NONE

CALL labels_almac

pder=0.0
pizq=0.0
salrio=0.0
fus=0.0 !Fusion de nieve
ptot=0.0 !Variable que almacena la lluvia total, no sólo la lluvia líquida
ettot=0.0
DO t=1,nt
  ptot=ptot+preac(t,1)
  pder=pder+balanc(t,1)
  pizq=pizq+balanc(t,7)
  salrio=salrio+balanc(t,12)
  fus=fus+balanc(t,20) !Fusión de nieve
  ettot=ettot+preac(t,2)
ENDDO

WRITE (21,'(A57)')almac(1)
WRITE (21,'(A35,F15.5)') almac(2), ptot
WRITE (21,'(A35,F15.5)') almac(3), ettot
WRITE (21,'(A38)') almac(4)
WRITE (21,'(A35,F15.5)') almac(5),salrio/nt
!WRITE (21,'(A35,F15.5)') almac(6),pder*arcelkm*ncel/1000.
WRITE (21,'(A35,F15.5)') almac(6),pder
!WRITE (21,'(A35,F15.5)') almac(7),pizq*arcelkm*ncel/1000.
WRITE (21,'(A35,F15.5)') almac(7),pizq


lluviaHm3=(ptot)*arcelkm*ncel/1000
ETHm3=pizq*arcelkm*ncel/1000
pder=0.0
pizq=0.0
salrio=0.0
DO t=1,nt
  pder=pder+balanc(t,2)
  pizq=pizq+balanc(t,8)
  salrio=salrio+balanc(t,13)
ENDDO
WRITE (21,'(A27)') almac(8)
WRITE (21,'(A35,F15.5)') almac(9),salrio/nt
!WRITE (21,'(A35,F15.5)') 'BL Entrada por "throughfall"[Hm3]: ',pder*arcelkm*ncel/1000.
WRITE (21,'(A35,F15.5)') almac(10),pder
!WRITE (21,'(A35,F15.5)') 'BL Flujo de salida Es + T   [Hm3]: ',pizq*arcelkm*ncel/1000.
WRITE (21,'(A35,F15.5)') almac(11),pizq
ETHm3=ETHm3+pizq*arcelkm*ncel/1000
pder=0.0
pizq=0.0
salrio=0.0
DO t=1,nt
  pder=pder+balanc(t,3)
  pizq=pizq+balanc(t,9)
  salrio=salrio+balanc(t,14)
ENDDO
WRITE (21,'(A21)') almac(12)
WRITE (21,'(A35,F15.5)') almac(13),salrio/nt
!WRITE (21,'(A35,F15.5)') 'BL Excedente de Ppt         [Hm3]: ',pder*arcelkm*ncel/1000.
WRITE (21,'(A35,F15.5)') almac(14),pder
!WRITE (21,'(A35,F15.5)') 'BL Escorrentia directa      [Hm3]: ',pizq*arcelkm*ncel/1000.
WRITE (21,'(A35,F15.5)') almac(15),pizq
!temp=pder*arcelkm*ncel/1000.
EDHm3=pizq*arcelkm*ncel/1000.
PRHm3=pder*arcelkm*ncel/1000.
pder=0.0
pizq=0.0
salrio=0.0
DO t=1,nt
  pder=pder+balanc(t,4)
  pizq=pizq+balanc(t,10)
  salrio=salrio+balanc(t,15)
ENDDO
WRITE (21,'(A32)')almac(16)
WRITE (21,'(A35,F15.5)') almac(17),salrio/nt
!WRITE (21,'(A35,F15.5)') 'BL Cantidad de Infiltracion [Hm3]: ',pder*arcelkm*ncel/1000.
WRITE (21,'(A35,F15.5)') almac(18),pder
!WRITE (21,'(A35,F15.5)') 'BL Flujo Subsuperficial     [Hm3]: ',pizq*arcelkm*ncel/1000.
WRITE (21,'(A35,F15.5)') almac(19),pizq
IFHm3=pizq*arcelkm*ncel/1000
pder=0.0
pizq=0.0
salrio=0.0
DO t=1,nt
  pder=pder+balanc(t,5)
  pizq=pizq+balanc(t,11)
  salrio=salrio+balanc(t,16)
ENDDO
WRITE (21,'(A21)') almac(20)
WRITE (21,'(A35,F15.5)') almac(21),salrio/nt
!WRITE (21,'(A35,F15.5)') 'BL Entrada por Percolacion  [Hm3]: ',pder*arcelkm*ncel/1000.
WRITE (21,'(A35,F15.5)') almac(22),pder
!WRITE (21,'(A35,F15.5)') 'BL Flujo Base               [Hm3]: ',pizq*arcelkm*ncel/1000.
WRITE (21,'(A35,F15.5)') almac(23),pizq
FBHm3=pizq*arcelkm*ncel/1000
salrio=0.0
DO t=1,nt
  salrio=salrio+balanc(t,6)
ENDDO
WRITE (21,'(A2)') '* '
!WRITE (21,'(A35,F15.5)') 'BL Perdidas subterráneas    [Hm3]: ',salrio*arcelkm*ncel/1000.
WRITE (21,'(A35,F15.5)') almac(24),salrio
WRITE (21,'(A2)') '* '

PSHm3=salrio*arcelkm*ncel/1000
!salrio=0.0
!DO t=1,nt
  !salrio=salrio+balanc(t,19)
!ENDDO

!WRITE (21,'(A35,F15.5)') 'BL Cantidad de agua REGADA   [mm]: ',salrio/nt
WRITE (21,'(A2)') '* '
!WRITE (21,'(A40,F15.5)') 'BL Caudal de salida de la cuenca  [Hm3]:', balancsalrio/1000000
WRITE (21,'(A40,F15.5)') almac(25), balancsalrio/1000000./arcelkm/ncel*1000.
WRITE (21,'(A2)') '* '
salrio=0.0
DO t=1,nt
  salrio=salrio+balanc(t,17)
ENDDO
difalm=0.0
difalm1=0.0
difalm2=0.0
almfin(0)=0.0
almfin(6)=0.0
almfin(5)=0.0
DO i=1,4
  almfin(i)=0.0
  almfin(i)=almfin(i)+balanc(nt,i+12)
  difalm=difalm+almfin(i)-almini(i)
ENDDO
almfin(0)=almfin(0)+balanc(nt,21)
almfin(6)=almfin(6)+balanc(nt,12)
almfin(5)=almfin(5)+balanc(nt,17)
difalm=difalm+almfin(6)-(almini(6)/ncel)
difalm=difalm*arcelkm*ncel/1000
difalm1=difalm1+almfin(5)-(almini(5)/ncel)
difalm1=difalm1*arcelkm*ncel/1000
difalm1=difalm1+difalm
difalm2=difalm2+almfin(0)-(almini(0)/ncel)
difalm2=difalm2*arcelkm*ncel/1000
difalm2=difalm2+difalm1
difalmveg=almfin(1)-almini(1)+almfin(6)-almini(6)
difalmsuelo=almfin(4)-almini(4)+almfin(3)-almini(3)+almfin(2)-almini(2)

salrio=0.0
DO t=1,nt
  salrio=salrio+balancexp(t)
  salrio=salrio*arcel/1000000
ENDDO

WRITE (21,'(A2)') '* '
WRITE (21,'(A80,F15.10)') almac(26),100*(ETHm3+(balancsalrio/1000000)+PSHm3+difalm2-lluviaHm3)/lluviaHm3
WRITE (21,'(A18,F15.10)') almac(27), ETHm3+EDHm3+IFHm3+FBHm3+PSHm3
WRITE (21,'(A18,F15.10)') almac(28), lluviaHm3
WRITE (21,'(A31,F15.10)') almac(29), difalm2
WRITE (21,'(A44,F15.10)') almac(30), salrio

END SUBROUTINE

!*****************************************************
!* Escribe fichero settings.txt
!*****************************************************
SUBROUTINE escri_settings
USE modtet
IMPLICIT NONE

OPEN (11,file=artem,status='replace')
    WRITE(11,*)lang,typeyear
    WRITE(11,*)config(2),config(4),config(5),trapeff,simevents,config(6),config(1)
    WRITE(11,*)(modulos(i),i=1,5)
    WRITE(11,*)printascii, dtascii
    WRITE(11,*)(modulos2(i),i=1,5)
CLOSE(11)

END SUBROUTINE
    
!***********************************************
!* Lectura de calib Veg
!***********************************************
SUBROUTINE escri_calibveg
USE modtet
IMPLICIT NONE

OPEN(10,file=artem,status='replace')
    DO i=1,cantUsosSuelosVeg
        WRITE(10,'(11(F12.5,x))')(lamb(i,j),j=1,11)
    END DO
CLOSE(10)

END SUBROUTINE

!****************************************************************
!* Escribe fichero megaevento para calib automática multi-evento
!*****************************************************************
SUBROUTINE megaevent_col
USE modtet
IMPLICIT NONE
CHARACTER riga*500
INTEGER*4 lriga,nx,ii,nvalues,igf,icheck
REAL*8, ALLOCATABLE:: values(:)
REAL*8 origin

OPEN(16,file=TRIM(ADJUSTL(dirtra))//'MultiEvento.txt')
DO nx=1,nexe 
    OPEN(15,file=multievento(nx))
    aa='  '
        DO WHILE (aa.ne.'G ')
            READ(15,'(a2)')aa
        END DO
        BACKSPACE(15)
        IF (aa.eq.'G ') then
            READ(15,*)aa,nstep(nx),dtmin
            IF(nx.ne.1)nstep(nx)=nstep(nx)+nstep(nx-1)
        ENDIF
    REWIND(15)
    aa='  '
        DO WHILE (aa.ne.'F ') 
            READ(15,'(a2)')aa
        END DO
        BACKSPACE(15)
        IF (aa.eq.'F ') then
            READ(15,'(a500)')riga
            icheck=0
            do ii=1,500
                if(riga(ii:ii).eq.'-'.and.icheck.eq.0)then
                    read(riga(ii-2:ii-1),*)fecin(1:2)
                    fecin(3:3)='/'
                    read(riga(ii+1:ii+2),*)fecin(4:5)
                    fecin(6:6)='/'
                    read(riga(ii+4:ii+7),*)fecin(7:10)
                    icheck=1
                endif
                if(riga(ii:ii).eq.':')then
                    read(riga(ii-2:ii-1),*)horin(1:2)
                    horin(3:3)=':'
                    read(riga(ii+1:ii+2),*)horin(4:5)
                    horin(6:8)=':00'
                    exit
                endif
            enddo
            datainiz(nx)=fecin
            orainiz(nx)=horin
     END IF
    CLOSE(15)
ENDDO
OPEN(15,file=multievento(1))
nvalues=0
READ(15,'(a500)')riga
aa=riga(1:2)
DO WHILE(aa.ne.'G ')
    IF(aa.eq.'P '.or.aa.eq.'N '.or.aa.eq.'V '.or.aa.eq.'S '.or.aa.eq.'Q '.or.aa.eq.'X '.or.aa.eq.'B '.or.aa.eq.'T '.or.aa.eq.'H '.or.aa.eq.'E '.or.aa.eq.'D ')THEN
        nvalues=nvalues+1
    END IF
    lriga=LEN_TRIM(riga)
    WRITE(16,'(a<lriga>)')riga(1:lriga)    
    READ(15,'(a500)')riga
    aa=riga(1:2)
ENDDO
WRITE (16,'(A2,2x,I10,f12.5)') aa,nstep(nexe),dtmin
READ(15,'(a500)')riga
aa=riga(1:2)
DO WHILE (aa.ne.'  ')
    IF(aa.eq.'P '.or.aa.eq.'N '.or.aa.eq.'V '.or.aa.eq.'S '.or.aa.eq.'Q '.or.aa.eq.'X '.or.aa.eq.'B '.or.aa.eq.'T '.or.aa.eq.'H '.or.aa.eq.'E '.or.aa.eq.'D ')THEN
        nvalues=nvalues+1
    END IF
    lriga=LEN_TRIM(riga)
    WRITE(16,'(a<lriga>)')riga(1:lriga)
    READ(15,'(a500)')riga
    aa=riga(1:2)
ENDDO
IF(ALLOCATED(values))DEALLOCATE(values)
ALLOCATE(values(nvalues))
BACKSPACE(15)
DO
    READ(15,*,end=1) origin,values
    WRITE(16,'(a4,<nvalues>f15.8)')'   1',values
ENDDO
1 DO nx=2,nexe
    OPEN(15,file=multievento(nx))
    READ(15,'(a500)')riga
    aa=riga(1:2)
    DO WHILE(aa.ne.'  ')
        READ(15,'(a500)')riga
        aa=riga(1:2)
    ENDDO
    BACKSPACE(15)
    DO
        READ(15,*,end=2) origin,values
        WRITE(16,'(a2,i2,<nvalues>f15.8)')'  ',nx,values
    ENDDO
    2 CLOSE(15)
ENDDO
CLOSE(16)
END SUBROUTINE

!****************************************************************
!* Escribe fichero megaevento para calib automática multi-evento
!*****************************************************************
SUBROUTINE megaevent_cedex
USE modtet
IMPLICIT NONE
CHARACTER riga*500
INTEGER*4 lriga,nx,nstation,istation,igf,ii,icheck,ini
REAL*8, ALLOCATABLE:: values(:,:)
CHARACTER, ALLOCATABLE:: stationame(:)*25,stationcod(:)*2 
REAL*8 origin
character :: char_list( 19 )
char_list = ['P ','N ','V ','S ','Q ','X ','B ','T ','H ','E ','D ','W ','R ','DA','DL','DC','NO','AM','NI']

OPEN(16,file=TRIM(ADJUSTL(dirtra))//'MultiEvento.txt')
DO nx=1,nexe 
    OPEN(15,file=multievento(nx))
    aa='  '
        DO WHILE (aa.ne.'G ') !(aa.ne.'G '.AND.aa.ne.'F ')
            READ(15,'(a2)')aa
        END DO
        BACKSPACE(15)
        IF (aa.eq.'G ') then
            READ(15,*)aa,nstep(nx),dtmin
            IF(nx.ne.1)nstep(nx)=nstep(nx)+nstep(nx-1)
        ENDIF
    REWIND(15)
    aa='  '
        DO WHILE (aa.ne.'F ') 
            READ(15,'(a2)')aa
        END DO
        BACKSPACE(15)
        IF (aa.eq.'F ') then
            READ(15,'(a500)')riga
            icheck=0
            do ii=1,500
                if(riga(ii:ii).eq.'-'.and.icheck.eq.0)then
                    read(riga(ii-2:ii-1),*)fecin(1:2)
                    fecin(3:3)='/'
                    read(riga(ii+1:ii+2),*)fecin(4:5)
                    fecin(6:6)='/'
                    read(riga(ii+4:ii+7),*)fecin(7:10)
                    icheck=1
                endif
                if(riga(ii:ii).eq.':')then
                    read(riga(ii-2:ii-1),*)horin(1:2)
                    horin(3:3)=':'
                    read(riga(ii+1:ii+2),*)horin(4:5)
                    horin(6:8)=':00'
                    exit
                endif
            enddo
            datainiz(nx)=fecin
            orainiz(nx)=horin
     END IF
    CLOSE(15)
ENDDO

OPEN(15,file=multievento(1))
nstation=0
READ(15,'(a500)')riga
aa=riga(1:2)
DO WHILE(aa.ne.'G ')
    lriga=LEN_TRIM(riga)
    WRITE(16,'(a<lriga>)')riga(1:lriga)    
    READ(15,'(a500)')riga
    aa=riga(1:2)
ENDDO
WRITE (16,'(A2,2x,I10,f12.5)') aa,nstep(nexe),dtmin
READ(15,'(a500)')riga
aa=riga(1:2)

!DO WHILE (aa.ne.'P '.and.aa.ne.'N '.and.aa.ne.'V '.and.aa.ne.'S '.and.aa.ne.'Q '.and.aa.ne.'X '.and.aa.ne.'B '.and.aa.ne.'T '.and.aa.ne.'H '.and.aa.ne.'E '.and.aa.ne.'D ')
DO WHILE ( all( aa.ne.char_list) )   
    lriga=LEN_TRIM(riga)
    WRITE(16,'(a<lriga>)')riga(1:lriga)
    READ(15,'(a500)')riga
    aa=riga(1:2)
ENDDO

BACKSPACE (15)
DO 
    READ(15,'(A2)',end=1) aa
    !IF (aa.eq.'P '.or.aa.eq.'N '.or.aa.eq.'V '.or.aa.eq.'S '.or.aa.eq.'Q '.or.aa.eq.'X '.or.aa.eq.'B '.or.aa.eq.'T '.or.aa.eq.'H '.or.aa.eq.'E '.or.aa.eq.'D ')THEN
    IF ( any( aa.eq.char_list) )  then  
        nstation=nstation+1
    endif
END DO
1 IF(ALLOCATED(stationame))DEALLOCATE(stationame)
IF(ALLOCATED(stationcod))DEALLOCATE(stationcod)
IF(ALLOCATED(values))DEALLOCATE(values)
ALLOCATE(stationame(nstation),stationcod(nstation),values(nstation,nstep(nexe)+4))
REWIND(15)
READ(15,'(a2)')aa
!DO WHILE (aa.ne.'P '.and.aa.ne.'N '.and.aa.ne.'V '.and.aa.ne.'S '.and.aa.ne.'Q '.and.aa.ne.'X '.and.aa.ne.'B '.and.aa.ne.'T '.and.aa.ne.'H '.and.aa.ne.'E '.and.aa.ne.'D ')
DO WHILE ( all( aa.ne.char_list) )
    READ(15,'(a2)')aa
ENDDO
BACKSPACE(15)
istation=0
DO 
    READ(15,'(A2)',end=2) aa
    !IF (aa.eq.'P '.or.aa.eq.'N '.or.aa.eq.'V '.or.aa.eq.'S '.or.aa.eq.'Q '.or.aa.eq.'X '.or.aa.eq.'B '.or.aa.eq.'T '.or.aa.eq.'H '.or.aa.eq.'E '.or.aa.eq.'D ')then
    IF ( any( aa.eq.char_list) ) then
        BACKSPACE(15)
        istation=istation+1
        READ(15,*)stationcod(istation),stationame(istation),values(istation,1:nstep(1)+4)
    endif
END DO
2 CLOSE (15)

ini=nstep(1)+4
DO nx=2,nexe
    OPEN(15, file=multievento(nx))
    READ(15,'(a2)')aa
    !DO WHILE (aa.ne.'P '.and.aa.ne.'N '.and.aa.ne.'V '.and.aa.ne.'S '.and.aa.ne.'Q '.and.aa.ne.'X '.and.aa.ne.'B '.and.aa.ne.'T '.and.aa.ne.'H '.and.aa.ne.'E '.and.aa.ne.'D ')
    DO WHILE ( all( aa.ne.char_list) )  
        READ(15,'(a2)')aa
    ENDDO
    BACKSPACE(15)
    istation=0
    DO 
        READ(15,'(A2)',end=3) aa
        !IF (aa.eq.'P '.or.aa.eq.'N '.or.aa.eq.'V '.or.aa.eq.'S '.or.aa.eq.'Q '.or.aa.eq.'X '.or.aa.eq.'B '.or.aa.eq.'T '.or.aa.eq.'H '.or.aa.eq.'E '.or.aa.eq.'D ') then
        IF ( any( aa.eq.char_list) ) then
            BACKSPACE(15)
            istation=istation+1
            READ(15,*)stationcod(istation),stationame(istation),(origin,igf=1,4),values(istation,ini+1:ini+(nstep(nx)-nstep(nx-1)))
        endif
    END DO
    3 ini=ini+(nstep(nx)-nstep(nx-1))
    CLOSE (15)
enddo
 
DO istation=1,nstation
    write(16,'(a2,a1,a25,a1,3f15.3,<nstep(nexe)+1>f12.5)') stationcod(istation),'"',stationame(istation),'"',values(istation,1:nstep(nexe)+4)
ENDDO
close (16)
END SUBROUTINE

!******************************************************************
!* Coleccion de rutinas de escritura para el modelo TETIS v7.2
!* Escribe CALIB.TXT
!******************************************************************
SUBROUTINE  escribe_umbralesQ
USE modtet
IMPLICIT NONE

OPEN (11,file=artem)
DO i=1,2
   WRITE(11,*)rango(i)
ENDDO
DO i=1,3
   WRITE(11,*)peso(i)
ENDDO

CLOSE(11)

END SUBROUTINE
    
LOGICAL FUNCTION is_nan(numero)

IMPLICIT NONE
REAL, INTENT(IN) :: numero
LOGICAL is
is = .false.

if (numero.ne.numero) is = .true.

is_nan = is
END FUNCTION is_nan