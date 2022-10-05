!**********************************************************************************
!* Rutina que escribe el fichero nantec.sds (Condiciones iniciales de nitrógeno)
!**********************************************************************************
SUBROUTINE escribe_nantec
USE Modtet
IMPLICIT NONE

OPEN(26,file=artem) 
  IF (lang.eq.1) THEN
    WRITE (26,'(A12,3x,2F14.4)')'NORTE-SUR:  ',cn,cs
    WRITE (26,'(A12,3x,2F14.4)')'ESTE-OESTE: ',ce,cw
    WRITE (26,'(A12,3x,I8)')'COLUMNAS:   ',mi
    WRITE (26,'(A12,3x,I8)')'FILAS:      ',mj
    WRITE (26,'(A12,3x,I8)')'COL-FINAL:  ',ncol
    WRITE (26,'(A12,3x,I8)')'FIL-FINAL:  ',nfil
    WRITE (26,'(A12,3x,I8)')'NUM-CELDAS: ',ncel
    WRITE (26,'(A12,A11,x,A8)')'FECHA-HORA: ',fecfin,horfin
  ELSE IF (lang.eq.2) THEN
    WRITE (26,'(A12,3x,2F14.4)')'NORTH-SOUTH:',cn,cs
    WRITE (26,'(A12,3x,2F14.4)')'EAST-WEST:  ',ce,cw
    WRITE (26,'(A12,3x,I8)')'COLUMNS:    ',mi
    WRITE (26,'(A12,3x,I8)')'ROWS:       ',mj
    WRITE (26,'(A12,3x,I8)')'COL-FINAL:  ',ncol
    WRITE (26,'(A12,3x,I8)')'ROW-FINAL:  ',nfil
    WRITE (26,'(A12,3x,I8)')'CELL-NUMBER:',ncel
    WRITE (26,'(A12,A11,x,A8)')'DATE-TIME:  ',fecfin,horfin
  ENDIF

DO n=1,ncel
  WRITE (26,104)cell(n).hn(0),cell(n).hn(1),cell(n).hn(2),cell(n).hn(3),cell(n).hn(4),cell(n).hn(5),cell(n).hn(6),cell(n).hn(7),cell(n).hn(8), &
                cell(n).hn(9),cell(n).hn(10),cell(n).hn(11),cell(n).hn(12),cell(n).w,cell(n).fcncult
ENDDO
104 FORMAT (15(x,F17.8))
CLOSE(26)
END SUBROUTINE

    
!************************************************************************************************
!** Formato de Impresión en columna para resultados de los procesos relacionados con el nitrógeno
!************************************************************************************************
SUBROUTINE escribenitr_col
USE modtet
!USE DFLIB
IMPLICIT NONE

INTEGER ktot,posN1
REAL tpo
CHARACTER, ALLOCATABLE:: codtex*3(:),serieChar*15(:,:)

!Escribe a un fichero de resultados 
OPEN(14,file=archnit(12))
IF (lang.eq.1) THEN
    WRITE (14,'(A2)') '* '
    WRITE (14,'(A55)') '* MODELACION HIDROLOGICA DISTRIBUIDA DE TIPO CONCEPTUAL'
    WRITE (14,'(A55)') '* MODELO DE SIMULACION - T E T I S   v.9 -      '
    WRITE (14,'(A2)') '* '
    WRITE (14,'(A20)') '* Desarrollado en:  '
    WRITE (14,'(A55)') '* UNIVERSITAT POLITECNICA DE VALENCIA  '
    WRITE (14,'(A63)') '* Instituto de ingeniería del Agua y Medio Ambiente (IIAMA)'
    WRITE (14,'(A57)') '* Grupo de Investigación en Modelación Hidrológica y Medioambiental (GIHMA)'
    WRITE (14,'(A2)') '* '
    WRITE (14,'(A45)') '*  DATOS DEL MODELO TETIS EN FORMATO COLUMNA '
    WRITE (14,'(A2)') '* '
    WRITE (14,'(A25,A128)')'* Directorio de Trabajo: ',dirtra
    WRITE (14,'(A2)') '* '
    !Escibe la fecha de inicio del episodio 
    WRITE (14,'(A48)') '* Fecha de inicio del episodio (dd-mm-aa  hh:mm)'
ELSE iF (lang.eq.2) THEN
    WRITE (14,'(A2)') '* '
    WRITE (14,'(A55)') '* DISTRIBUTED CONCEPTUAL HYDROLOGICAL MODELLING        '
    WRITE (14,'(A55)') '* MODEL T E T I S   v.9 -                       '
    WRITE (14,'(A2)') '* '
    WRITE (14,'(A20)') '* Developed at:     '
    WRITE (14,'(A55)') '* UNIVERSITAT POLITECNICA DE VALENCIA  '
    WRITE (14,'(A63)') '* Instituto de ingeniería del Agua y Medio Ambiente (IIAMA)'
    WRITE (14,'(A57)') '* Grupo de Investigación en Modelación Hidrológica y Medioambiental (GIHMA)'
    WRITE (14,'(A2)') '* '
    WRITE (14,'(A45)') '*  TETIS MODEL RESULTS IN COLUMN FORMAT      '
    WRITE (14,'(A2)') '* '
    WRITE (14,'(A25,A128)')'* Working directory:     ',dirtra
    WRITE (14,'(A2)') '* '
    !Escibe la fecha de inicio del episodio 
    WRITE (14,'(A48)') '* Starting date (dd-mm-yy  hh:mm)               '
ENDIF
    
WRITE(14,'(A2,2x,A2,A1,A2,A1,A4,2x,A2,A1,A2)') 'F ',archin(1:2),'-',archin(3:4),'-',  &
                                           archin(11:14),archin(5:6),':',archin(7:8)
WRITE (14,'(A2)') '* '
IF (lang.eq.1) THEN
    WRITE (14,'(A48)')'* Numero de datos - Intervalo temporal (minutos)'
ELSE IF (lang.eq.2) THEN
    WRITE (14,'(A48)')'* Number of data - Time step (minutes)          '
ENDIF
WRITE(14,'(A2,2x,I10,x,I10)') 'G ',nt,INT(dtmin)
WRITE (14,'(A2)') '* '

WRITE (14,'(A2)') '* '
!Escribe localizacion de las estaciones
!El fichero de columna debe tener el orden P,N,V,S,Q,B,H,T,E,D
IF (lang.eq.1) THEN
    WRITE(14,'(A45)') '* RESUMEN DE INFORMACION SOBRE LAS ESTACIONES'
    WRITE (14,'(A2)') '* '
    WRITE(14,'(A70)') '* "Nombre de la estacion    "   Este(UTM-X) Norte(UTM-Y)   Cota(msnm) '
ELSE IF (lang.eq.2) THEN
    WRITE(14,'(A45)') '* INFORMATION ABOUT STATIONS                 '
    WRITE (14,'(A2)') '* '
    WRITE(14,'(A70)') '* "Name of the station      "   East(UTM-X) North(UTM-Y) Height(msnm) '
ENDIF

!Escribe sólo en los puntos en los que se escriben resultados
DO i=1,naf
  WRITE(14,'(A2,X,A1,A25,A1,x,3(x,I11))')aforo(i).codigo,'"',aforo(i).name,'"',  &
                     INT(aforo(i).utmx),INT(aforo(i).utmy),INT(aforo(i).elev) 
ENDDO
DO i=1,kno
  WRITE(14,'(A2,X,A1,A25,A1,x,3(x,I11))')norg(i).codigo,'"',norg(i).name,'"',  &
                     INT(norg(i).utmx),INT(norg(i).utmy),INT(norg(i).elev) 
ENDDO
DO i=1,kam
  WRITE(14,'(A2,X,A1,A25,A1,x,3(x,I11))')amonio(i).codigo,'"',amonio(i).name,'"',  &
                     INT(amonio(i).utmx),INT(amonio(i).utmy),INT(amonio(i).elev) 
ENDDO
DO i=1,kni
  WRITE(14,'(A2,X,A1,A25,A1,x,3(x,I11))')nitrato(i).codigo,'"',nitrato(i).name,'"',  &
                     INT(nitrato(i).utmx),INT(nitrato(i).utmy),INT(nitrato(i).elev) 
ENDDO

WRITE (14,'(A2)') '* '

If (config(4)) then
    ALLOCATE(serie(nt,naf*8+kno*2+kam*2+kni*2))
    ALLOCATE(codtex(naf*8+kno*2+kam*2+kni*2))
    ALLOCATE(serieChar(11,naf*8+kno*2+kam*2+kni*2))!(Vicente) Se emplea una matriz de Character para mostrar resultados
Else
    ALLOCATE(serie(nt,naf*5+kno*2+kam*2+kni*2))
    ALLOCATE(codtex(naf*5+kno*2+kam*2+kni*2))
    ALLOCATE(serieChar(11,naf*5+kno*2+kam*2+kni*2))!(Vicente) Se emplea una matriz de Character para mostrar resultados
End if


    
!Rellena los datos y resultados en una sola matriz
serie=0.0
codtex='---'
serieChar='      -     '
k=0

DO i=1,naf !Aforos (se escriben las concentraciones en agua y si sediemntos está activado, el caudal de sedimentos con la concentración)
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
  k=k+1
  codtex(k)='NOq'
  DO t=1,nt
    serie(t,k)=norgql(i).sim(t)
  ENDDO
  k=k+1
  codtex(k)='AMq'
  DO t=1,nt
    serie(t,k)=amonioql(i).sim(t)
  ENDDO
  k=k+1
  codtex(k)='NIq'
  DO t=1,nt
    serie(t,k)=nitratoql(i).sim(t)
  ENDDO
  If (config(4)) then
    k=k+1
    codtex(k)='QXs'
    DO t=1,nt
      !serie(t,k)=aforosed(i).sim(t)
      serie(t,k)=aforo(i).sed_out(t,4)      
    ENDDO  
    k=k+1
    codtex(k)='NOx'
    DO t=1,nt
      serie(t,k)=norgqs(i).sim(t)
    ENDDO
    k=k+1
    codtex(k)='AMx'
    DO t=1,nt
      serie(t,k)=amonioqs(i).sim(t)
    ENDDO
  End if
ENDDO
DO i=1,kno !Puntos con concentración observada de nitrógeno orgánico disuelto
  k=k+1
  codtex(k)='NOo'
  DO t=1,nt
	serie(t,k)=norg(i).obs(t)
  ENDDO
  k=k+1
  codtex(k)='NOs'
  DO t=1,nt
	serie(t,k)=norg(i).sim(t)
  ENDDO
ENDDO
DO i=1,kam !Puntos con concentración observada de amonio disuelto
  k=k+1
  codtex(k)='AMo'
  DO t=1,nt
	serie(t,k)=amonio(i).obs(t)
  ENDDO
  k=k+1
  codtex(k)='AMs'
  DO t=1,nt
	serie(t,k)=amonio(i).sim(t)
  ENDDO
ENDDO
DO i=1,kni !Puntos con concentración observada de nitrato disuelto
  k=k+1
  codtex(k)='NIo'
  DO t=1,nt
	serie(t,k)=nitrato(i).obs(t)
  ENDDO
  k=k+1
  codtex(k)='NIs'
  DO t=1,nt
	serie(t,k)=nitrato(i).sim(t)
  ENDDO
ENDDO
ktot=k

DO i=1,naf
   WRITE(serieChar(1,2*(i-1)+1),'(F20.8)')estad(i,1)
   WRITE(serieChar(2,2*(i-1)+2),'(F20.8)')estad(i+naf,1)
   WRITE(serieChar(3,2*(i-1)+1),'(F20.8)')estad(i,4)
   WRITE(serieChar(4,2*(i-1)+1),'(F20.8)')estad(i,2)
   WRITE(serieChar(5,2*(i-1)+2),'(F20.8)')estad(i+naf,2)
   WRITE(serieChar(6,2*(i-1)+1),'(F20.8)')estad(i,5)
   WRITE(serieChar(7,2*(i-1)+1),'(F20.8)')estad(i,3)
   WRITE(serieChar(8,2*(i-1)+2),'(F20.8)')estad(i+naf,3)
   WRITE(serieChar(9,2*(i-1)+1),'(F20.8)')estad(i,6)
   WRITE(serieChar(10,2*(i-1)+1),'(F20.8)')estad(i+naf,4)
   WRITE(serieChar(11,2*(i-1)+1),'(F20.8)')aforo(i).area
   WRITE(serieChar(12,2*(i-1)+1),'(F20.8)')RSRindex(i)        
END DO

IF(config(4)) THEN
    posN1=naf*8
ELSE
    posN1=naf*5
END IF    
DO i=1,kno
    WRITE(serieChar(1,2*(i-1)+posN1+1),'(F20.8)')estadnitr(i,1)
    WRITE(serieChar(2,2*(i-1)+posN1+2),'(F20.8)')estadnitr(i+nafn,1)
    WRITE(serieChar(3,2*(i-1)+posN1+1),'(F20.8)')estadnitr(i+nafn,4)
    WRITE(serieChar(4,2*(i-1)+posN1+1),'(F20.8)')estadnitr(i,2)
    WRITE(serieChar(5,2*(i-1)+posN1+2),'(F20.8)')estadnitr(i+nafn,2)
    WRITE(serieChar(6,2*(i-1)+posN1+1),'(F20.8)')estadnitr(i,5)
    WRITE(serieChar(7,2*(i-1)+posN1+1),'(F20.8)')estadnitr(i,7)
    WRITE(serieChar(8,2*(i-1)+posN1+2),'(F20.8)')estadnitr(i+nafn,7)
    WRITE(serieChar(9,2*(i-1)+posN1+1),'(F20.8)')estadnitr(i+nafn,5)
    WRITE(serieChar(10,2*(i-1)+posN1+1),'(F20.8)')estadnitr(i+nafn,6)
    WRITE(serieChar(11,2*(i-1)+posN1+1),'(F20.8)')norg(i).area
END DO
DO i=kno+1,kno+kam
    WRITE(serieChar(1,2*(i-1)+posN1+1),'(F20.8)')estadnitr(i,1)
    WRITE(serieChar(2,2*(i-1)+posN1+2),'(F20.8)')estadnitr(i+nafn,1)
    WRITE(serieChar(3,2*(i-1)+posN1+1),'(F20.8)')estadnitr(i+nafn,4)
    WRITE(serieChar(4,2*(i-1)+posN1+1),'(F20.8)')estadnitr(i,2)
    WRITE(serieChar(5,2*(i-1)+posN1+2),'(F20.8)')estadnitr(i+nafn,2)
    WRITE(serieChar(6,2*(i-1)+posN1+1),'(F20.8)')estadnitr(i,5)
    WRITE(serieChar(7,2*(i-1)+posN1+1),'(F20.8)')estadnitr(i,7)
    WRITE(serieChar(8,2*(i-1)+posN1+2),'(F20.8)')estadnitr(i+nafn,7)
    WRITE(serieChar(9,2*(i-1)+posN1+1),'(F20.8)')estadnitr(i+nafn,5)
    WRITE(serieChar(10,2*(i-1)+posN1+1),'(F20.8)')estadnitr(i+nafn,6)
    WRITE(serieChar(11,2*(i-1)+posN1+1),'(F20.8)')amonio(i-kno).area
END DO
DO i=kno+kni+1,kno+kam+kni
    WRITE(serieChar(1,2*(i-1)+posN1+1),'(F20.8)')estadnitr(i,1)
    WRITE(serieChar(2,2*(i-1)+posN1+2),'(F20.8)')estadnitr(i+nafn,1)
    WRITE(serieChar(3,2*(i-1)+posN1+1),'(F20.8)')estadnitr(i+nafn,4)
    WRITE(serieChar(4,2*(i-1)+posN1+1),'(F20.8)')estadnitr(i,2)
    WRITE(serieChar(5,2*(i-1)+posN1+2),'(F20.8)')estadnitr(i+nafn,2)
    WRITE(serieChar(6,2*(i-1)+posN1+1),'(F20.8)')estadnitr(i,5)
    WRITE(serieChar(7,2*(i-1)+posN1+1),'(F20.8)')estadnitr(i,7)
    WRITE(serieChar(8,2*(i-1)+posN1+2),'(F20.8)')estadnitr(i+nafn,7)
    WRITE(serieChar(9,2*(i-1)+posN1+1),'(F20.8)')estadnitr(i+nafn,5)
    WRITE(serieChar(10,2*(i-1)+posN1+1),'(F20.8)')estadnitr(i+nafn,6)
    WRITE(serieChar(11,2*(i-1)+posN1+1),'(F20.8)')nitrato(i-kno-kam).area
END DO




!Escribe series temporales en el mismo orden
WRITE (14,'(A2)') '* '
WRITE (14,'(A2)') '* '
    IF (lang.eq.1) THEN
        WRITE (14,'(A30)') '* SERIES TEMPORALES DE SALIDA ' 
        WRITE (14,'(A2)') '* '
        WRITE(14,'(A1,A15,<ktot>(A17,A4))') '*',' ------DT------',(' ---------',codtex(j),j=1,ktot)
        DO t=1,nt
        tpo=t*dtmin/60.
        WRITE(14,'(<ktot+1>(x,F20.8))')tpo,(serie(t,j),j=1,ktot)
        ENDDO
    Else
        WRITE (14,'(A30)') '* OUTPUT TIME SERIES          ' 
        WRITE (14,'(A2)') '* '
        WRITE(14,'(A1,A15,<ktot>(A17,A4))') '*',' ------DT------',(' ---------',codtex(j),j=1,ktot)
        DO t=1,nt
        tpo=t*dtmin/60.
        WRITE(14,'(<ktot+1>(x,F20.8))')tpo,(serie(t,j),j=1,ktot)
        ENDDO
    End if
    WRITE (14,'(A2)') '* '
    WRITE(14,'(<ktot+1>(A20,x))') '* -------------',('---------------',j=1,ktot)
    WRITE(14,'(<ktot+1>(A20,x))') '* MAX OBS      ',(serieChar(1,j),j=1,ktot)
    WRITE(14,'(<ktot+1>(A20,x))') '* MAX SIM      ',(serieChar(2,j),j=1,ktot)
    WRITE(14,'(<ktot+1>(A20,x))') '* RMSE         ',(serieChar(3,j),j=1,ktot)
    WRITE(14,'(<ktot+1>(A20,x))') '* T PICO OBS   ',(serieChar(4,j),j=1,ktot)
    WRITE(14,'(<ktot+1>(A20,x))') '* T PICO SIM   ',(serieChar(5,j),j=1,ktot)
    WRITE(14,'(<ktot+1>(A20,x))') '* ERR T PICO   ',(serieChar(6,j),j=1,ktot)
    WRITE(14,'(<ktot+1>(A20,x))') '* VOL OBS      ',(serieChar(7,j),j=1,ktot)
    WRITE(14,'(<ktot+1>(A20,x))') '* VOL SIM      ',(serieChar(8,j),j=1,ktot)
    WRITE(14,'(<ktot+1>(A20,x))') '* ERROR VOL    ',(serieChar(9,j),j=1,ktot)
    WRITE(14,'(<ktot+1>(A20,x))') '* INDICE NSE   ',(serieChar(10,j),j=1,ktot)
    WRITE(14,'(<ktot+1>(A20,x))') '* AREA ACUM    ',(serieChar(11,j),j=1,ktot)
    WRITE (14,'(A2)') '* '
    IF (lang.eq.1) THEN
        IF (config(4)) then
            WRITE (14,'(A28)') '* FLUJOS MEDIOS EN LA CUENCA' 
            WRITE (14,'(A577)') '* ---------DT--------- -----fn40(kgN)------ -----fn41(kgN)------ -----fn45(kgN)------ -----fn46(kgN)------ ------fn0(kgN)------ ------fn1(kgN)------ ------fn44(kgN)----- ------fn2(kgN)------ ------fn3(kgN)------ -------fn4(kgN)----- -------fn5(kgN)----- ------fn6(kgN)------ ------fn7(kgN)------ ------fn8(kgN)------ ------fn9(kgN)------ -----fn10(kgN)----- -----fn30(kgN)------ ------fn31(kgN)----- ------fn42(kgN)----- ------fn43(kgN)----- ------fn23(kgN)----- ------fn24(kgN)----- ------fn25(kgN)----- ------fn11(kgN)----- ------fn12(kgN)----- ------fn13(kgN)----- ------fn14(kgN)----- ------fn15(kgN)----- ------fn16(kgN)----- ------fn17(kgN)----- ------fn18(kgN)----- ------fn19(kgN)----- ------fn20(kgN)----- ------fn28(kgN)----- ------fn29(kgN)---'
            WRITE (14,'(A577)') '* --------(hrs)------- ------InputNH4------ ------InputNO3------ -----DepAtmNH4------ -----DepAtmNO3------ ------MinSuelo------ ------InmSuelo------ ------VolSuelo------ ------NitrSuelo----- -------FiSuelo------ -----DesnitSuelo---- -------PercNH4------ -------PercNO3------ -------PerdNH4------ -------PerdNO3------ -----APasivaNH4----- ----APasivaNO3----- ----AActivaNH4------ -----AActivaNO3----- ------ExfilNH4------ ------ExfilNO3------ ------EscDirNO------ ------EscDirNH4----- ------EscDirNO3----- ------FlSubNH4------ ------FlSubNO3------ ------FlBaseNH4----- ------FlBaseNO3----- -------MinQSupf----- ------NitrQSupf----- -----DesnitQSupf---- -------CaudalNO----- ------CaudalNH4----- ------CaudalNO3----- -----CaudalSedNO---- ----CaudalSedNH4--'
            DO t=1,nt
                tpo=t*dtmin/60.
                WRITE (14,'(36(x,F20.6))') tpo,(balanc_nitr(t,j),j=54,55),(balanc_nitr(t,j),j=59,60),(balanc_nitr(t,j),j=1,2),balanc_nitr(t,58),(balanc_nitr(t,j),j=3,11),balanc_nitr(t,31),balanc_nitr(t,32),(balanc_nitr(t,j),j=56,57),(balanc_nitr(t,j),j=24,26),(balanc_nitr(t,j),j=12,21),(balanc_nitr(t,j),j=29,30)
            END DO
            WRITE (14,'(A2)') '* '
            WRITE (14,'(A2)') '* '
            WRITE (14,'(A37)') '* ALMACENAMIENTOS MEDIOS EN LA CUENCA' 
            WRITE (14,'(A225)') '* ---------DT--------- ------hn0(kgN)----- ------hn8(kgN)------ ------hn1(kgN)------ ------hn2(kgN)------ -------hn3(kgN)----- ------hn4(kgN)------ ------hn5(kgN)------ ------hn6(kgN)------ ------hn7(kgN)------ ------hn9(kgN)------ ------hn10(kgN)----- ------hn11(kgN)----- ------hn12(kgN)-----'
            WRITE (14,'(A225)') '* --------(hrs)------- ------NOSuelo------ -----NH4AdsSuelo---- -----NH4DisSuelo---- ------NO3Suelo------ ------NODisSupf----- ------NH4DisSupf---- -------NO3Supf------ -----NH4DisAcuí----- -------NO3Acuí------ -----NOSedSusSupf--- ----NH4SedSusSupf--- ----NOSedDepCauce--- ---NH4SedDepCauce---'
            DO t=1,nt
                tpo=t*dtmin/60.
                WRITE (14,'(14(x,F20.6))') tpo,balanc_nitr(t,33),balanc_nitr(t,36),(balanc_nitr(t,j),j=34,35),(balanc_nitr(t,j),j=37,39),(balanc_nitr(t,j),j=44,45),(balanc_nitr(t,j),j=40,43)
            END DO
        Else
            WRITE (14,'(A28)') '* FLUJOS MEDIOS EN LA CUENCA' 
            WRITE (14,'(A577)') '* ---------DT--------- -----fn40(kgN)------ -----fn41(kgN)------ -----fn45(kgN)------ -----fn46(kgN)------ ------fn0(kgN)------ ------fn1(kgN)------ ------fn44(kgN)----- ------fn2(kgN)------ ------fn3(kgN)------ -------fn4(kgN)----- -------fn5(kgN)----- ------fn6(kgN)------ ------fn7(kgN)------ ------fn8(kgN)------ ------fn9(kgN)------ -----fn10(kgN)----- -----fn30(kgN)------ ------fn31(kgN)----- ------fn42(kgN)----- ------fn43(kgN)----- ------fn23(kgN)----- ------fn24(kgN)----- ------fn25(kgN)----- ------fn11(kgN)----- ------fn12(kgN)----- ------fn13(kgN)----- ------fn14(kgN)----- ------fn15(kgN)----- ------fn16(kgN)----- ------fn17(kgN)----- ------fn18(kgN)----- ------fn19(kgN)----- ------fn20(kgN)-----'
            WRITE (14,'(A577)') '* --------(hrs)------- ------InputNH4------ ------InputNO3------ -----DepAtmNH4------ -----DepAtmNO3------ ------MinSuelo------ ------InmSuelo------ ------VolSuelo------ ------NitrSuelo----- -------FiSuelo------ -----DesnitSuelo---- -------PercNH4------ -------PercNO3------ -------PerdNH4------ -------PerdNO3------ -----APasivaNH4----- ----APasivaNO3----- ----AActivaNH4------ -----AActivaNO3----- ------ExfilNH4------ ------ExfilNO3------ ------EscDirNO------ ------EscDirNH4----- ------EscDirNO3----- ------FlSubNH4------ ------FlSubNO3------ ------FlBaseNH4----- ------FlBaseNO3----- -------MinQSupf----- ------NitrQSupf----- -----DesnitQSupf---- -------CaudalNO----- ------CaudalNH4----- ------CaudalNO3-----'
            DO t=1,nt
                tpo=t*dtmin/60.
                WRITE (14,'(34(x,F20.6))') tpo,(balanc_nitr(t,j),j=54,55),(balanc_nitr(t,j),j=59,60),(balanc_nitr(t,j),j=1,2),balanc_nitr(t,58),(balanc_nitr(t,j),j=3,11),balanc_nitr(t,31),balanc_nitr(t,32),(balanc_nitr(t,j),j=56,57),(balanc_nitr(t,j),j=24,26),(balanc_nitr(t,j),j=12,21)
            END DO
            WRITE (14,'(A37)') '* ALMACENAMIENTOS MEDIOS EN LA CUENCA' 
            WRITE (14,'(A225)') '* ---------DT--------- ------hn0(kgN)----- ------hn8(kgN)------ ------hn1(kgN)------ ------hn2(kgN)------ -------hn3(kgN)----- ------hn4(kgN)------ ------hn5(kgN)------ ------hn6(kgN)------ ------hn7(kgN)------'
            WRITE (14,'(A225)') '* --------(hrs)------- ------NOSuelo------ -----NH4AdsSuelo---- -----NH4DisSuelo---- ------NO3Suelo------ ------NODisSupf----- ------NH4DisSupf---- -------NO3Supf------ -----NH4DisAcuí----- -------NO3Acuí------'
            DO t=1,nt
                tpo=t*dtmin/60.
                WRITE (14,'(14(x,F20.6))') tpo,balanc_nitr(t,33),balanc_nitr(t,36),(balanc_nitr(t,j),j=34,35),(balanc_nitr(t,j),j=37,39),(balanc_nitr(t,j),j=44,45)
            END DO
        END IF
    ELSE
        IF (config(4)) then
            WRITE (14,'(A36)') '* AVERAGE FLOWS WITHIN THE CATCHMENT' 
            WRITE (14,'(A577)') '* ---------DT--------- -----fn40(kgN)------ -----fn41(kgN)------ -----fn45(kgN)------ -----fn46(kgN)------ ------fn0(kgN)------ ------fn1(kgN)------ ------fn44(kgN)----- ------fn2(kgN)------ ------fn3(kgN)------ -------fn4(kgN)----- -------fn5(kgN)----- ------fn6(kgN)------ ------fn7(kgN)------ ------fn8(kgN)------ ------fn9(kgN)------ -----fn10(kgN)----- -----fn30(kgN)------ ------fn31(kgN)----- ------fn42(kgN)----- ------fn43(kgN)----- ------fn23(kgN)----- ------fn24(kgN)----- ------fn25(kgN)----- ------fn11(kgN)----- ------fn12(kgN)----- ------fn13(kgN)----- ------fn14(kgN)----- ------fn15(kgN)----- ------fn16(kgN)----- ------fn17(kgN)----- ------fn18(kgN)----- ------fn19(kgN)----- ------fn20(kgN)----- ------fn28(kgN)----- ------fn29(kgN)---'
            WRITE (14,'(A577)') '* --------(hrs)------- ------NH4Input------ ------NO3Input------ -----NH4AtmDep------ -----NO3AtmDep------ ------SoilMin------- ------SoilImm------- ------SoilVol------- ------SoilNitr------ -------SoilFix------ ------SoilDenitr---- -------NH4Perc------ -------NO3Perc------ ------NH4Losses----- ------NO3Losses----- ----NH4PassiveUpt--- ---NO3PassiveUpt--- -----NH4ActiveUpt--- ----NO3ActiveUpt---- ------NH4Exfil------ ------NO3Exfil------ ------DirRunON------ -----NH4DirectRun--- -----NO3DirectRun--- ----NH4SubSupFlo---- ----NO3SubSupFlo---- -----NH4ConnFlow---- -----NO3ConnFlow---- -----SupfFlowNitr--- ----SupfFlowNitr---- -----DesnitQSupf---- --------ONFlow------ -------NH4Flow------ -------NO3Flow------ ------ONSedFlow----- -----NH4SedFlow---'
           DO t=1,nt
                tpo=t*dtmin/60.
                WRITE (14,'(36(x,F20.6))') tpo,(balanc_nitr(t,j),j=54,55),(balanc_nitr(t,j),j=59,60),(balanc_nitr(t,j),j=1,2),balanc_nitr(t,58),(balanc_nitr(t,j),j=3,11),balanc_nitr(t,31),balanc_nitr(t,32),(balanc_nitr(t,j),j=56,57),(balanc_nitr(t,j),j=24,26),(balanc_nitr(t,j),j=12,21),(balanc_nitr(t,j),j=29,30)
            END DO
            WRITE (14,'(A2)') '* '
            WRITE (14,'(A2)') '* '
            WRITE (14,'(A39)') '* AVERAGE STORAGES WITHIN THE CATCHMENT' 
            WRITE (14,'(A225)') '* ---------DT--------- ------hn0(kgN)----- ------hn8(kgN)------ ------hn1(kgN)------ ------hn2(kgN)------ -------hn3(kgN)----- ------hn4(kgN)------ ------hn5(kgN)------ ------hn6(kgN)------ ------hn7(kgN)------ ------hn9(kgN)------ ------hn10(kgN)----- ------hn11(kgN)----- ------hn12(kgN)-----'
            WRITE (14,'(A225)') '* --------(hrs)------- -------SoilON------ ----SoilAdsorpNH4--- -----SoilDissNH4---- -------SoilNO3------ ------SupfDissON---- -----SupfDissNH4---- -------SupfNO3------ ----AquifDissNH4---- ----AquifDissNO3---- ----NOSedSusCauce--- ---NH4SedSusCauce--- ----NOSedDepCauce--- ---NH4SedDepCauce---'
            DO t=1,nt
                tpo=t*dtmin/60.
                WRITE(14,'(14(x,F120.6))') tpo,balanc_nitr(t,33),balanc_nitr(t,36),(balanc_nitr(t,j),j=34,35),(balanc_nitr(t,j),j=37,39),(balanc_nitr(t,j),j=44,45),(balanc_nitr(t,j),j=40,43)
            END DO
        Else
            WRITE (14,'(A36)') '* AVERAGE FLOWS WITHIN THE CATCHMENT' 
            WRITE (14,'(A577)') '* ---------DT--------- -----fn40(kgN)------ -----fn41(kgN)------ -----fn45(kgN)------ -----fn46(kgN)------ ------fn0(kgN)------ ------fn1(kgN)------ ------fn44(kgN)----- ------fn2(kgN)------ ------fn3(kgN)------ -------fn4(kgN)----- -------fn5(kgN)----- ------fn6(kgN)------ ------fn7(kgN)------ ------fn8(kgN)------ ------fn9(kgN)------ -----fn10(kgN)----- -----fn30(kgN)------ ------fn31(kgN)----- ------fn42(kgN)----- ------fn43(kgN)----- ------fn23(kgN)----- ------fn24(kgN)----- ------fn25(kgN)----- ------fn11(kgN)----- ------fn12(kgN)----- ------fn13(kgN)----- ------fn14(kgN)----- ------fn15(kgN)----- ------fn16(kgN)----- ------fn17(kgN)----- ------fn18(kgN)----- ------fn19(kgN)----- ------fn20(kgN)-----'
            WRITE (14,'(A577)') '* --------(hrs)------- ------NH4Input------ ------NO3Input------ -----NH4AtmDep------ -----NO3AtmDep------ ------SoilMin------- ------SoilImm------- ------SoilVol------- ------SoilNitr------ -------SoilFix------ ------SoilDenitr---- -------NH4Perc------ -------NO3Perc------ ------NH4Losses----- ------NO3Losses----- ----NH4PassiveUpt--- ---NO3PassiveUpt--- -----NH4ActiveUpt--- ----NO3ActiveUpt---- ------NH4Exfil------ ------NO3Exfil------ ------DirRunON------ -----NH4DirectRun--- -----NO3DirectRun--- ----NH4SubSupFlo---- ----NO3SubSupFlo---- -----NH4ConnFlow---- -----NO3ConnFlow---- -----SupfFlowNitr--- ----SupfFlowNitr---- -----DesnitQSupf---- --------ONFlow------ -------NH4Flow------ -------NO3Flow------'
           DO t=1,nt
                tpo=t*dtmin/60.
                WRITE (14,'(34(x,F20.6))') tpo,(balanc_nitr(t,j),j=54,55),(balanc_nitr(t,j),j=59,60),(balanc_nitr(t,j),j=1,2),balanc_nitr(t,58),(balanc_nitr(t,j),j=3,11),balanc_nitr(t,31),balanc_nitr(t,32),(balanc_nitr(t,j),j=56,57),(balanc_nitr(t,j),j=24,26),(balanc_nitr(t,j),j=12,21)
            END DO
            WRITE (14,'(A39)') '* AVERAGE STORAGES WITHIN THE CATCHMENT' 
            WRITE (14,'(A225)') '* ---------DT--------- ------hn0(kgN)----- ------hn8(kgN)------ ------hn1(kgN)------ ------hn2(kgN)------ -------hn3(kgN)----- ------hn4(kgN)------ ------hn5(kgN)------ ------hn6(kgN)------ ------hn7(kgN)------'
            WRITE (14,'(A225)') '* --------(hrs)------- -------SoilON------ ----SoilAdsorpNH4--- -----SoilDissNH4---- -------SoilNO3------ ------SupfDissON---- -----SupfDissNH4---- -------SupfNO3------ ----AquifDissNH4---- ----AquifDissNO3----'
            DO t=1,nt
                tpo=t*dtmin/60.
                WRITE(14,'(10(x,F20.6))') tpo,balanc_nitr(t,33),balanc_nitr(t,36),(balanc_nitr(t,j),j=34,35),(balanc_nitr(t,j),j=37,39),(balanc_nitr(t,j),j=44,45)
            END DO
        END IF
    END IF
        
Close(14)

    END SUBROUTINE
    
SUBROUTINE escr_calibnit
USE modtet
!USE DFLIB
IMPLICIT NONE

OPEN(10,file=archnit(8))

DO i=1,cantUsSueNitr
    WRITE(10,'(F14.6,8(x,F12.4))')kmin2(i),kinm2(i),kvol2(i),knit2(i),kfi2(i),kdes2(i),F2(i),Ndem2(i),PrefNO3(i)
ENDDO
WRITE(10,'(F14.6,2(x,F12.4))')kminc2,knitc2,kdesc2
WRITE(10,'(F14.6,5(x,F12.4))')mtd,tethas,topts,tethac,toptc,fckd

CLOSE(10)
    
END SUBROUTINE
    
SUBROUTINE estadisticos_nitr
USE modtet
!USE DFLIB
IMPLICIT NONE
REAL*8 varsim,varobs

!! Calcula estadísticos sedimentos observados VS sedimentos simulados
IF (ALLOCATED(estadnitr)) DEALLOCATE(estadnitr)
ALLOCATE(estadnitr((nafn)*2,13))
ALLOCATE(RSRindexnitr(nafn))
RSRindexnitr=0.0 
! ESTADISTICOS SEDIMENTOS (estadsed(:,:))
! (1,1) Qmax obs
! (1,2) T(Qmax) obs
! (1,3) Qmedio obs
! (1,4) Suma fact_ponderacion*(Qsim-Qobs)^2
! (1,5) Factor de ponderación (lambda=2.0)
! (1,6) --
! (1,7) Vol obs Hm³
! (1,8) (Qsim-Qmed)^2
! (1,9) Qmedio sim
! (1,10) RMSE mensual
! (1,11) (Qobs-Qm)*(Qsim-Qm)
! (1,12) Error medio simulado para LGA (mu)
! (1,13) --
! (1+k,1) Qmax sim
! (1+k,2) T(Qmax) sim 
! (1+k,3) (Qobs-Qmed)^2
! (1+k,4) RMSE
! (1+k,5) % Error Volumen
! (1+k,6) Nash-Sutcliffe
! (1+k,7) Vol Sim Hm³
! (1+k,8) HMLE
! (1+k,9) Coef. de eficiencia (Nash sin cuadrado)
! (1+k,10) RMSE mensual
! (1+k,11) Para HMLE autocorrelacionado y gaussiano (??)
! (1+k,12) HMLE gaussiano autocorrelac
! (1+k,13) KGE (En estadNitr, KGE es el 13, no el 15)
!
estadnitr=0.00000000000
DO l=1,kno
    newnt2=0 !Almacena número de registros sin contar los faltantes
    j=l
    k=j+nafn    
    DO t=nifo,nt
        IF (norg(l).obs(t).ge.0.0) THEN
          newnt2=newnt2+1
          estadnitr(j,3)=estadnitr(j,3)+norg(l).obs(t)
          estadnitr(j,9)=estadnitr(j,9)+norg(l).sim(t)
	      estadnitr(j,12)=estadnitr(j,12)+(norg(l).obs(t)-norg(l).sim(t))
        ENDIF        
    ENDDO
    IF (newnt2.gt.0) THEN
        estadnitr(j,3)=estadnitr(j,3)/newnt2   !Qmedio obs
        estadnitr(j,9)=estadnitr(j,9)/newnt2  !Qmedio sim
        estadnitr(j,12)=estadnitr(j,12)/newnt2  !Error medio simulado para LGA (mu)
    ENDIF
    DO t=nifo,nt
        IF (norg(l).obs(t).gt.estadnitr(j,1)) THEN !CALCULO DE MAXIMOS!OJO AL REPLICAR
          estadnitr(j,1)=norg(l).obs(t)	!Caudal pico Observado
          estadnitr(j,2)=dt*t       !Tiempo al pico Observado
        ENDIF
        IF (norg(l).sim(t).gt.estadnitr(k,1)) THEN
          estadnitr(k,1)=norg(l).sim(t)	!Caudal pico Simulado
          estadnitr(k,2)=dt*t       !Tiempo al pico simulado
        ENDIF
        IF (norg(l).obs(t).ge.0.0) THEN
	      estadnitr(j,7)=estadnitr(j,7)+(norg(l).obs(t)*dts/1000000.)  !Vol Obs Hm³
          estadnitr(k,7)=estadnitr(k,7)+(norg(l).sim(t)*dts/1000000.)  !Vol Sim Hm³
	      estadnitr(k,4)=estadnitr(k,4)+(norg(l).obs(t)-norg(l).sim(t))**2.0   !(Qobs-Qsim)^2
          estadnitr(k,3)=estadnitr(k,3)+(norg(l).obs(t)-estadnitr(j,3))**2.0  !(Qobs-Qmed)^2
	      estadnitr(j,8)=estadnitr(j,8)+(norg(l).sim(t)-estadnitr(j,9))**2.0 !(Qsim-Qmed)^2
	      estadnitr(j,5)=norg(l).obs(t)**(2.*(fol-1.)) !Factor de ponderación (lambda=2.0)
	      estadnitr(j,4)=estadnitr(j,4)+estadnitr(j,5)*(norg(l).sim(t)-norg(l).obs(t))*(norg(l).sim(t)-norg(l).obs(t))
          IF (estadnitr(j,5).gt.0.) estadnitr(j,6)=estadnitr(j,6)+LOG(estadnitr(j,5))  !log Productoria
	      estadnitr(k,9)=estadnitr(k,9)+ABS((norg(l).obs(t)-norg(l).sim(t))/(norg(l).obs(t)-estadnitr(j,3))) !Coef de eficiencia
	      estadnitr(j,11)=estadnitr(j,11)+(norg(l).obs(t)-estadnitr(j,3))*(norg(l).sim(t)-estadnitr(j,9))  !(Qobs-Qm)*(Qsim-Qm)
	      estadnitr(k,10)=estadnitr(k,10)+(norg(l).obs(t)-norg(l).sim(t))*(norg(l).obs(t)-norg(l).sim(t))/(norg(l).obs(t)*norg(l).obs(t))
        ENDIF
        newc=newc+1
	    IF (newc.eq.folon) THEN
	        newc=0
	        estadnitr(j,10)=estadnitr(j,10)+(estadnitr(k,10)/folon)**0.5 !RMSE mensual !Camilo: Ojo con los faltantes!!!!!
	        estadnitr(k,10)=0.0
        ENDIF    
    ENDDO   
     !GIAMBA - mayo 2013 - correlación linear para calcular el KGE (Gupta et al. 2009, JoH)
      estadnitr(k,13)=estadnitr(j,11)/sqrt(estadnitr(k,3)*estadnitr(j,8))
  
      varsim=sqrt(1/(REAL(newnt2))*estadnitr(j,8))
      varobs=sqrt(1/(REAL(newnt2))*estadnitr(k,3))
  
      !1-KGE
      estadnitr(k,13)=sqrt((estadnitr(k,13)-1)**2+(varsim/varobs-1)**2+(estadnitr(j,9)/estadnitr(j,3)-1)**2)
  
      IF (newnt2.gt.0) estadnitr(j,11)=(estadnitr(j,11)/newnt2)/(((estadnitr(k,3)/newnt2)**0.5)*((estadnitr(j,8)/newnt2)**0.5))  !Coef correlac. de errores (alfa)
      DO t=nifo+1,nt   !Para HMLE autocorrelacionado y gaussiano
        estadnitr(k,11)=estadnitr(k,11)+((norg(l).obs(t)-norg(l).sim(t))-estadnitr(j,12)-estadnitr(j,11)*(norg(l).obs(t-1)-norg(l).obs(t-1)))**2.0
      ENDDO
      
      !Indice sugerido por Moriasi 2007, RMSE-observations standard deviation ratio
      IF (estadnitr(k,3).gt.0.0) RSRindexnitr(j)=(estadnitr(k,4))**0.5/(estadnitr(k,3))**0.5
      
      IF (estadnitr(j,7).gt.0.0) estadnitr(k,5)=100.0*(estadnitr(j,7)-estadnitr(k,7))/estadnitr(j,7)         !% Error Volumen 
      IF (estadnitr(k,3).gt.0.0) estadnitr(k,6)=(estadnitr(k,4)/estadnitr(k,3))    !Indice de Nash      
  
      estadnitr(k,10)=estadnitr(j,10)   !RMSE mensual
      IF (newnt2.gt.0.0) estadnitr(k,4)=(estadnitr(k,4)/newnt2)**0.5   !RMSE
      IF (newnt2.gt.0.0) estadnitr(k,8)=EXP(LOG(1.0/newnt2)+LOG(estadnitr(j,4))-(estadnitr(j,6)/newnt2))  !HMLE

      estadnitr(k,6)=1.0-estadnitr(k,6)    !Indice de Nash
      estadnitr(k,9)=1.0-estadnitr(k,9)    !Coef. de eficiencia (Nash sin cuadrado)
      !GIAMBA mayo 2013 - Nueva función objetivo: KGE
      estadnitr(k,13) = 1.0 - estadnitr(k,13)   !KGE
      estadnitr(j,5)=(estadnitr(j,2)-estadnitr(k,2))  !Error tiempo al pico en terminos absolutos con su signo
END DO

DO l=1,kam
    newnt2=0 !Almacena número de registros sin contar los faltantes
    j=l+kno
    k=j+nafn
    DO t=nifo,nt
        IF (amonio(l).obs(t).ge.0.0) THEN
          newnt2=newnt2+1
          estadnitr(j,3)=estadnitr(j,3)+amonio(l).obs(t)
          estadnitr(j,9)=estadnitr(j,9)+amonio(l).sim(t)
	      estadnitr(j,12)=estadnitr(j,12)+(amonio(l).obs(t)-amonio(l).sim(t))
        ENDIF        
    ENDDO
    IF (newnt2.gt.0) THEN
        estadnitr(j,3)=estadnitr(j,3)/newnt2   !Qmedio obs
        estadnitr(j,9)=estadnitr(j,9)/newnt2  !Qmedio sim
        estadnitr(j,12)=estadnitr(j,12)/newnt2  !Error medio simulado para LGA (mu)
    ENDIF
    estadnitr(j,1) = 0.0
    DO t=nifo,nt
        IF (amonio(l).obs(t).gt.estadnitr(j,1)) THEN
          estadnitr(j,1)=amonio(l).obs(t)	!Caudal pico Observado
          estadnitr(j,2)=dt*t       !Tiempo al pico Observado
        ENDIF
        IF (amonio(l).sim(t).gt.estadnitr(k,1)) THEN
          estadnitr(k,1)=amonio(l).sim(t)	!Caudal pico Simulado
          estadnitr(k,2)=dt*t       !Tiempo al pico simulado
        ENDIF
        IF (amonio(l).obs(t).ge.0.0) THEN
	      estadnitr(j,7)=estadnitr(j,7)+(amonio(l).obs(t)*dts/1000000.)  !Vol Obs Hm³
          estadnitr(k,7)=estadnitr(k,7)+(amonio(l).sim(t)*dts/1000000.)  !Vol Sim Hm³
	      estadnitr(k,4)=estadnitr(k,4)+(amonio(l).obs(t)-amonio(l).sim(t))**2.0   !(Qobs-Qsim)^2
          estadnitr(k,3)=estadnitr(k,3)+(amonio(l).obs(t)-estadnitr(j,3))**2.0  !(Qobs-Qmed)^2
	      estadnitr(j,8)=estadnitr(j,8)+(amonio(l).sim(t)-estadnitr(j,9))**2.0 !(Qsim-Qmed)^2
	      estadnitr(j,5)=amonio(l).obs(t)**(2.*(fol-1.)) !Factor de ponderación (lambda=2.0)
	      estadnitr(j,4)=estadnitr(j,4)+estadnitr(j,5)*(amonio(l).sim(t)-amonio(l).obs(t))*(amonio(l).sim(t)-amonio(l).obs(t))
          IF (estadnitr(j,5).gt.0.) estadnitr(j,6)=estadnitr(j,6)+LOG(estadnitr(j,5))  !log Productoria
	      estadnitr(k,9)=estadnitr(k,9)+ABS((amonio(l).obs(t)-amonio(l).sim(t))/(amonio(l).obs(t)-estadnitr(j,3))) !Coef de eficiencia
	      estadnitr(j,11)=estadnitr(j,11)+(amonio(l).obs(t)-estadnitr(j,3))*(amonio(l).sim(t)-estadnitr(j,9))  !(Qobs-Qm)*(Qsim-Qm)
	      estadnitr(k,10)=estadnitr(k,10)+(amonio(l).obs(t)-amonio(l).sim(t))*(amonio(l).obs(t)-amonio(l).sim(t))/(amonio(l).obs(t)*amonio(l).obs(t))
        ENDIF
        newc=newc+1
	    IF (newc.eq.folon) THEN
	        newc=0
	        estadnitr(j,10)=estadnitr(j,10)+(estadnitr(k,10)/folon)**0.5 !RMSE mensual !Camilo: Ojo con los faltantes!!!!!
	        estadnitr(k,10)=0.0
        ENDIF    
    ENDDO   
     !GIAMBA - mayo 2013 - correlación linear para calcular el KGE (Gupta et al. 2009, JoH)
     !(Vicente) KGE en estadNitr es el 13(no el 15)
      estadnitr(k,13)=estadnitr(j,11)/sqrt(estadnitr(k,3)*estadnitr(j,8))
  
      varsim=sqrt(1/(REAL(newnt2))*estadnitr(j,8))
      varobs=sqrt(1/(REAL(newnt2))*estadnitr(k,3))
  
      !1-KGE
      estadnitr(k,13)=sqrt((estadnitr(k,13)-1)**2+(varsim/varobs-1)**2+(estadnitr(j,9)/estadnitr(j,3)-1)**2)
  
      IF (newnt2.gt.0) estadnitr(j,11)=(estadnitr(j,11)/newnt2)/(((estadnitr(k,3)/newnt2)**0.5)*((estadnitr(j,8)/newnt2)**0.5))  !Coef correlac. de errores (alfa)
      DO t=nifo+1,nt   !Para HMLE autocorrelacionado y gaussiano
        estadnitr(k,11)=estadnitr(k,11)+((amonio(l).obs(t)-amonio(l).sim(t))-estadnitr(j,12)-estadnitr(j,11)*(amonio(l).obs(t-1)-amonio(l).obs(t-1)))**2.0
      ENDDO
      
      !Indice sugerido por Moriasi 2007, RMSE-observations standard deviation ratio
      IF (estadnitr(k,3).gt.0.0) RSRindexnitr(j)=(estadnitr(k,4))**0.5/(estadnitr(k,3))**0.5
      IF (estadnitr(j,7).gt.0.0) estadnitr(k,5)=100.0*(estadnitr(j,7)-estadnitr(k,7))/estadnitr(j,7)         !% Error Volumen 
      IF (estadnitr(k,3).gt.0.0) estadnitr(k,6)=(estadnitr(k,4)/estadnitr(k,3))    !Indice de Nash      
  
      estadnitr(k,10)=estadnitr(j,10)   !RMSE mensual
      IF (newnt2.gt.0.0) estadnitr(k,4)=(estadnitr(k,4)/newnt2)**0.5   !RMSE
      IF (newnt2.gt.0.0) estadnitr(k,8)=EXP(LOG(1.0/newnt2)+LOG(estadnitr(j,4))-(estadnitr(j,6)/newnt2))  !HMLE

      estadnitr(k,6)=1.0-estadnitr(k,6)    !Indice de Nash
      estadnitr(k,9)=1.0-estadnitr(k,9)    !Coef. de eficiencia (Nash sin cuadrado)
      estadnitr(k,13) = 1.0 - estadnitr(k,13)   !KGE
      estadnitr(j,5)=(estadnitr(j,2)-estadnitr(k,2))  !Error tiempo al pico en terminos absolutos con su signo
END DO

DO l=1,kni
    newnt2=0 !Almacena número de registros sin contar los faltantes
    j=l+kam+kno
    k=j+nafn
    DO t=nifo,nt
        IF (nitrato(l).obs(t).ge.0.0) THEN
          newnt2=newnt2+1
          estadnitr(j,3)=estadnitr(j,3)+nitrato(l).obs(t)
          estadnitr(j,9)=estadnitr(j,9)+nitrato(l).sim(t)
	      estadnitr(j,12)=estadnitr(j,12)+(nitrato(l).obs(t)-nitrato(l).sim(t))
        ENDIF        
    ENDDO
    IF (newnt2.gt.0) THEN
        estadnitr(j,3)=estadnitr(j,3)/newnt2   !Qmedio obs
        estadnitr(j,9)=estadnitr(j,9)/newnt2  !Qmedio sim
        estadnitr(j,12)=estadnitr(j,12)/newnt2  !Error medio simulado para LGA (mu)
    ENDIF
    estadnitr(j,1) = 0.0
    DO t=nifo,nt
        IF (nitrato(l).obs(t).gt.estadnitr(j,1)) THEN
          estadnitr(j,1)=nitrato(l).obs(t)	!Caudal pico Observado
          estadnitr(j,2)=dt*t       !Tiempo al pico Observado
        ENDIF
        IF (nitrato(l).sim(t).gt.estadnitr(k,1)) THEN
          estadnitr(k,1)=nitrato(l).sim(t)	!Caudal pico Simulado
          estadnitr(k,2)=dt*t       !Tiempo al pico simulado
        ENDIF
        IF (nitrato(l).obs(t).ge.0.0) THEN
	      estadnitr(j,7)=estadnitr(j,7)+(nitrato(l).obs(t)*dts/1000000.)  !Vol Obs Hm³
          estadnitr(k,7)=estadnitr(k,7)+(nitrato(l).sim(t)*dts/1000000.)  !Vol Sim Hm³
	      estadnitr(k,4)=estadnitr(k,4)+(nitrato(l).obs(t)-nitrato(l).sim(t))**2.0   !(Qobs-Qsim)^2
          estadnitr(k,3)=estadnitr(k,3)+(nitrato(l).obs(t)-estadnitr(j,3))**2.0  !(Qobs-Qmed)^2
	      estadnitr(j,8)=estadnitr(j,8)+(nitrato(l).sim(t)-estadnitr(j,9))**2.0 !(Qsim-Qmed)^2
	      estadnitr(j,5)=nitrato(l).obs(t)**(2.*(fol-1.)) !Factor de ponderación (lambda=2.0)
	      estadnitr(j,4)=estadnitr(j,4)+estadnitr(j,5)*(nitrato(l).sim(t)-nitrato(l).obs(t))*(nitrato(l).sim(t)-nitrato(l).obs(t))
          IF (estadnitr(j,5).gt.0.) estadnitr(j,6)=estadnitr(j,6)+LOG(estadnitr(j,5))  !log Productoria
	      estadnitr(k,9)=estadnitr(k,9)+ABS((nitrato(l).obs(t)-nitrato(l).sim(t))/(nitrato(l).obs(t)-estadnitr(j,3))) !Coef de eficiencia
	      estadnitr(j,11)=estadnitr(j,11)+(nitrato(l).obs(t)-estadnitr(j,3))*(nitrato(l).sim(t)-estadnitr(j,9))  !(Qobs-Qm)*(Qsim-Qm)
	      estadnitr(k,10)=estadnitr(k,10)+(nitrato(l).obs(t)-nitrato(l).sim(t))*(nitrato(l).obs(t)-nitrato(l).sim(t))/(nitrato(l).obs(t)*nitrato(l).obs(t))
        ENDIF
        newc=newc+1
	    IF (newc.eq.folon) THEN
	        newc=0
	        estadnitr(j,10)=estadnitr(j,10)+(estadnitr(k,10)/folon)**0.5 !RMSE mensual !Camilo: Ojo con los faltantes!!!!!
	        estadnitr(k,10)=0.0
        ENDIF    
    ENDDO   
     !GIAMBA - mayo 2013 - correlación linear para calcular el KGE (Gupta et al. 2009, JoH)
      estadnitr(k,13)=estadnitr(j,11)/sqrt(estadnitr(k,3)*estadnitr(j,8))
  
      varsim=sqrt(1/(REAL(newnt2))*estadnitr(j,8))
      varobs=sqrt(1/(REAL(newnt2))*estadnitr(k,3))
  
      !1-KGE
      estadnitr(k,13)=sqrt((estadnitr(k,13)-1)**2+(varsim/varobs-1)**2+(estadnitr(j,9)/estadnitr(j,3)-1)**2)
  
      IF (newnt2.gt.0) estadnitr(j,11)=(estadnitr(j,11)/newnt2)/(((estadnitr(k,3)/newnt2)**0.5)*((estadnitr(j,8)/newnt2)**0.5))  !Coef correlac. de errores (alfa)
      DO t=nifo+1,nt   !Para HMLE autocorrelacionado y gaussiano
        estadnitr(k,11)=estadnitr(k,11)+((nitrato(l).obs(t)-nitrato(l).sim(t))-estadnitr(j,12)-estadnitr(j,11)*(nitrato(l).obs(t-1)-nitrato(l).obs(t-1)))**2.0
      ENDDO
      
      !Indice sugerido por Moriasi 2007, RMSE-observations standard deviation ratio
      IF (estadnitr(k,3).gt.0.0) RSRindexnitr(j)=(estadnitr(k,4))**0.5/(estadnitr(k,3))**0.5
      
      IF (estadnitr(j,7).gt.0.0) estadnitr(k,5)=100.0*(estadnitr(j,7)-estadnitr(k,7))/estadnitr(j,7)         !% Error Volumen 
      IF (estadnitr(k,3).gt.0.0) estadnitr(k,6)=(estadnitr(k,4)/estadnitr(k,3))    !Indice de Nash      
  
      estadnitr(k,10)=estadnitr(j,10)   !RMSE mensual
      IF (newnt2.gt.0.0) estadnitr(k,4)=(estadnitr(k,4)/newnt2)**0.5   !RMSE
      IF (newnt2.gt.0.0) estadnitr(k,8)=EXP(LOG(1.0/newnt2)+LOG(estadnitr(j,4))-(estadnitr(j,6)/newnt2))  !HMLE

      estadnitr(k,6)=1.0-estadnitr(k,6)    !Indice de Nash
      estadnitr(k,9)=1.0-estadnitr(k,9)    !Coef. de eficiencia (Nash sin cuadrado)
      estadnitr(k,13) = 1.0 - estadnitr(k,13)   !KGE
      estadnitr(j,5)=(estadnitr(j,2)-estadnitr(k,2))  !Error tiempo al pico en terminos absolutos con su signo
END DO

END SUBROUTINE