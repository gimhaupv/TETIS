!****************************************************************************
!** Formato de Impresión en columna para resultados de procesos sedimentarios
!****************************************************************************
SUBROUTINE escribesed_col
USE modtet
!USE DFLIB
IMPLICIT NONE

INTEGER newnt,ktot2
REAL tpo,sum2,sumed,sumed2,valmax,valmaxsim,valmin,valminsim,mtpo,mtposim
REAL, ALLOCATABLE :: seriesed(:,:)
CHARACTER, ALLOCATABLE:: codtex*12(:) 
CHARACTER tft*7(4),text1*13
CHARACTER tft2*7(2),aaa*3
!TYPE (rccoord) curpos

!Escribe a un fichero de resultados 
OPEN(14,file=archsed(9))
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
DO i=1,kppt
  WRITE(14,'(A2,A1,A25,A1,x,3(x,I11))')pluvio(i).codigo,'"',pluvio(i).name,'"',  &
                     INT(pluvio(i).utmx),INT(pluvio(i).utmy),INT(pluvio(i).elev) 
ENDDO
DO i=1,nemb
  WRITE(14,'(A2,A1,A25,A1,x,3(x,I11))')nivel(i).codigo,'"',nivel(i).name,'"',  &
                     INT(nivel(i).utmx),INT(nivel(i).utmy),INT(nivel(i).elev)
ENDDO
DO i=nemb+1,nemb+vnemb
  WRITE(14,'(A2,A1,A25,A1,x,3(x,I11))')volum(i).codigo,'"',volum(i).name,'"',  &
                     INT(volum(i).utmx),INT(volum(i).utmy),INT(volum(i).elev) 
ENDDO
DO i=nemb+vnemb+1,nemb+vnemb+knemb
  WRITE(14,'(A2,A1,A25,A1,x,3(x,I11))')qemb(i).codigo,'"',qemb(i).name,'"',  &
                     INT(qemb(i).utmx),INT(qemb(i).utmy),INT(qemb(i).elev) 
ENDDO
DO i=1,naf
  WRITE(14,'(A2,A1,A25,A1,x,3(x,I11))')aforo(i).codigo,'"',aforo(i).name,'"',  &
                     INT(aforo(i).utmx),INT(aforo(i).utmy),INT(aforo(i).elev) 
ENDDO
DO i=1,knaf
  WRITE(14,'(A2,A1,A25,A1,x,3(x,I11))')otros(i).codigo,'"',otros(i).name,'"',  &
                     INT(otros(i).utmx),INT(otros(i).utmy),INT(otros(i).elev) 
ENDDO
DO i=1,kniv
  WRITE(14,'(A2,A1,A25,A1,x,3(x,I11))')nieve(i).codigo,'"',nieve(i).name,'"',  &
                     INT(nieve(i).utmx),INT(nieve(i).utmy),INT(nieve(i).elev) 
ENDDO
DO i=1,ktem
  WRITE(14,'(A2,A1,A25,A1,x,3(x,I11))')temper(i).codigo,'"',temper(i).name,'"',  &
                     INT(temper(i).utmx),INT(temper(i).utmy),INT(temper(i).elev) 
ENDDO
DO i=1,kevp
  WRITE(14,'(A2,A1,A25,A1,x,3(x,I11))')evapo(i).codigo,'"',evapo(i).name,'"',  &
                     INT(evapo(i).utmx),INT(evapo(i).utmy),INT(evapo(i).elev) 
ENDDO
DO i=1,kadi
  WRITE(14,'(A2,A1,A25,A1,x,3(x,I11))')aport(i).codigo,'"',aport(i).name,'"',  &
                     INT(aport(i).utmx),INT(aport(i).utmy),INT(aport(i).elev) 
ENDDO

WRITE (14,'(A2)') '* '

!Calcula estadisticos de las series (FALTA)

!Escribe series temporales en el mismo orden
ALLOCATE(serie(1:nt,naf*126))
ALLOCATE(serie2(1:nt,knaf*126))
ALLOCATE(codtex(21))

!Rellena los datos y resultados en una sola matriz
serie=0.0
IF (lang.eq.1) THEN
    !Encabezado de las series temporales
    !Flujo, concentración, volumen
    codtex(1)='       Arena'		
    codtex(2)='        Limo'	
    codtex(3)='     Arcilla'		
    codtex(4)='       Total'
    !Relaciones geomorfológicas iniciales
    codtex(5)=' AlturaFl(m)'		!Altura del flujo para el caudal que pasa, m		
    codtex(6)='  VelFl(m/s)'		!Velocidad del flujo, m/s
    codtex(7)='   QFl(m3/s)'		!Caudal que pasa, m3/s
    codtex(8)='  Qsll(m3/s)'		!Caudal a sección llena, m3/s
    codtex(9)=' Anchosll(m)'		!Ancho a sección llena, m
    codtex(10)='  AnchoFl(m)'		!Ancho de flujo, m
    codtex(11)='  AreaFl(m2)'		!Area para el caudal que pasa, m2
    codtex(12)='     RhFl(m)'		!Radio Hidráulico para el caudal que pasa, m
    !Procesos  en suspensión
    codtex(13)=' ArenSUS(m3)'		!m3, Vol arena transportada en suspensión
    codtex(14)=' LimoSUS(m3)'		!m3, Vol limo transportado en suspensión
    codtex(15)=' ArciSUS(m3)'		!m3, Vol arcilla transportada en suspensión
    !Procesos como material de lecho
    codtex(16)='  ArenBM(m3)'		!m3, Volumen de arena transportada como material del lecho 
    codtex(17)='  LimoBM(m3)'		!m3, Volumen de limo transportado como material del lecho 
    codtex(18)='  ArciBM(m3)'		!m3, Volumen de arcilla transportada como material del lecho 
    !Procesos en ladera por erosión
    !codtex(26)=' CapTrRe(m3)'		!m3, Capacidad de transporte residual
    codtex(19)='  ArenER(m3)'		!m3, Volumen de arena erosionada
    codtex(20)='  LimoER(m3)'		!m3, Volumen de limo erosionado
    codtex(21)='  ArciER(m3)'		!m3, Volumen de arcilla erosionada
ELSE IF (lang.eq.2) THEN
    codtex(1)='        Sand'		
    codtex(2)='        Silt'	
    codtex(3)='        Clay'		
    codtex(4)='       Total'
    !Relaciones geomorfológicas iniciales
    codtex(5)=' HeightFl(m)'		!Altura del flujo para el caudal que pasa, m		
    codtex(6)='  VelFl(m/s)'		!Velocidad del flujo, m/s
    codtex(7)='   QFl(m3/s)'		!Caudal que pasa, m3/s
    codtex(8)='  Qsll(m3/s)'		!Caudal a sección llena, m3/s
    codtex(9)=' Widthsll(m)'		!Ancho a sección llena, m
    codtex(10)='  WidthFl(m)'		!Ancho de flujo, m
    codtex(11)='  AreaFl(m2)'		!Area para el caudal que pasa, m2
    codtex(12)='     RhFl(m)'		!Radio Hidráulico para el caudal que pasa, m
    !Procesos  en suspensión
    codtex(13)=' SandSUS(m3)'		!m3, Vol arena transportada en suspensión
    codtex(14)=' SiltSUS(m3)'		!m3, Vol limo transportado en suspensión
    codtex(15)=' ClaySUS(m3)'		!m3, Vol arcilla transportada en suspensión
    !Procesos como material de lecho
    codtex(16)='  SandBM(m3)'		!m3, Volumen de arena transportada como material del lecho 
    codtex(17)='  SiltBM(m3)'		!m3, Volumen de limo transportado como material del lecho 
    codtex(18)='  ClayBM(m3)'		!m3, Volumen de arcilla transportada como material del lecho 
    !Procesos en ladera por erosión
    !codtex(26)=' CapTrRe(m3)'		!m3, Capacidad de transporte residual
    codtex(19)='  SandER(m3)'		!m3, Volumen de arena erosionada
    codtex(20)='  SiltER(m3)'		!m3, Volumen de limo erosionado
    codtex(21)='  ClayER(m3)'		!m3, Volumen de arcilla erosionada
ENDIF
!Sedimentos en puntos de aforos, definición de series temporales
DO i=1,naf     
  DO t=1,nt
    !Series temporales de flujo y concentración de sedimentos
  	serie(t,(i-1)*126+1)=aforo(i).sed_out(t,1)
	serie(t,(i-1)*126+2)=aforo(i).sed_out(t,2)
	serie(t,(i-1)*126+3)=aforo(i).sed_out(t,3)
	serie(t,(i-1)*126+4)=aforo(i).sed_out(t,4)
	serie(t,(i-1)*126+5)=aforo(i).sed_out(t,5)
	serie(t,(i-1)*126+6)=aforo(i).sed_out(t,6)
	serie(t,(i-1)*126+7)=aforo(i).sed_out(t,7)
	serie(t,(i-1)*126+8)=aforo(i).sed_out(t,8)
    !Series temporales de variables importantes(control para ver el funcionamiento del modelo)
    !Relaciones geomorfologicas iniciales
	serie(t,(i-1)*126+9)=aforo(i).sed_temp(t,1)
	serie(t,(i-1)*126+10)=aforo(i).sed_temp(t,2)
	serie(t,(i-1)*126+11)=aforo(i).sed_temp(t,3)
	serie(t,(i-1)*126+12)=aforo(i).sed_temp(t,4)
	serie(t,(i-1)*126+13)=aforo(i).sed_temp(t,5)
	serie(t,(i-1)*126+14)=aforo(i).sed_temp(t,6)
	serie(t,(i-1)*126+15)=aforo(i).sed_temp(t,7)
	serie(t,(i-1)*126+16)=aforo(i).sed_temp(t,8)
	!Volumen de sedimentos transportado en suspensión
	serie(t,(i-1)*126+17)=aforo(i).sed_temp(t,9)
	serie(t,(i-1)*126+18)=aforo(i).sed_temp(t,10)
	serie(t,(i-1)*126+19)=aforo(i).sed_temp(t,11)
    !Volumen de sedimentos transportado como material del lecho
	serie(t,(i-1)*126+20)=aforo(i).sed_temp(t,12)
	serie(t,(i-1)*126+21)=aforo(i).sed_temp(t,13)
	serie(t,(i-1)*126+22)=aforo(i).sed_temp(t,14)
	!Volumen de sedimentos erosionado (en ladera)
	serie(t,(i-1)*126+23)=aforo(i).sed_temp(t,15)
	serie(t,(i-1)*126+24)=aforo(i).sed_temp(t,16)
	serie(t,(i-1)*126+25)=aforo(i).sed_temp(t,17)
  ENDDO
ENDDO
!Sedimentos en puntos de simulación, definición de series temporales
DO i=1,knaf     
  DO t=1,nt
    !Series temporales de flujo y concentración de sedimentos
  	serie2(t,(i-1)*126+1)=otros(i).sed_out(t,1)
	serie2(t,(i-1)*126+2)=otros(i).sed_out(t,2)
	serie2(t,(i-1)*126+3)=otros(i).sed_out(t,3)
	serie2(t,(i-1)*126+4)=otros(i).sed_out(t,4)
	serie2(t,(i-1)*126+5)=otros(i).sed_out(t,5)
	serie2(t,(i-1)*126+6)=otros(i).sed_out(t,6)
	serie2(t,(i-1)*126+7)=otros(i).sed_out(t,7)
	serie2(t,(i-1)*126+8)=otros(i).sed_out(t,8)
    !Series temporales de variables importantes(control para ver el funcionamiento del modelo)
    !Relaciones geomorfologicas iniciales
	serie2(t,(i-1)*126+9)=otros(i).sed_temp(t,1)
	serie2(t,(i-1)*126+10)=otros(i).sed_temp(t,2)
	serie2(t,(i-1)*126+11)=otros(i).sed_temp(t,3)
	serie2(t,(i-1)*126+12)=otros(i).sed_temp(t,4)
	serie2(t,(i-1)*126+13)=otros(i).sed_temp(t,5)
	serie2(t,(i-1)*126+14)=otros(i).sed_temp(t,6)
	serie2(t,(i-1)*126+15)=otros(i).sed_temp(t,7)
	serie2(t,(i-1)*126+16)=otros(i).sed_temp(t,8)
	!Volumen de sedimentos transportado en suspensión
	serie2(t,(i-1)*126+17)=otros(i).sed_temp(t,9)
	serie2(t,(i-1)*126+18)=otros(i).sed_temp(t,10)
	serie2(t,(i-1)*126+19)=otros(i).sed_temp(t,11)
    !Volumen de sedimentos transportado como material del lecho
	serie2(t,(i-1)*126+20)=otros(i).sed_temp(t,12)
	serie2(t,(i-1)*126+21)=otros(i).sed_temp(t,13)
	serie2(t,(i-1)*126+22)=otros(i).sed_temp(t,14)
	!Volumen de sedimentos erosionado (en ladera)
	serie2(t,(i-1)*126+23)=otros(i).sed_temp(t,15)
	serie2(t,(i-1)*126+24)=otros(i).sed_temp(t,16)
	serie2(t,(i-1)*126+25)=otros(i).sed_temp(t,17)
  ENDDO
ENDDO
!! Calcula estadísticos sedimentos observados VS sedimentos simulados
IF (ALLOCATED(estadsed)) DEALLOCATE(estadsed)
ALLOCATE(estadsed(ksedq*2,12))
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
! (1+ksedq,1) Qmax sim
! (1+ksedq,2) T(Qmax) sim 
! (1+ksedq,3) (Qobs-Qmed)^2
! (1+ksedq,4) RMSE
! (1+ksedq,5) % Error Volumen
! (1+ksedq,6) Nash-Sutcliffe
! (1+ksedq,7) Vol Sim Hm³
! (1+ksedq,8) HMLE
! (1+ksedq,9) Coef. de eficiencia (Nash sin cuadrado)
! (1+ksedq,10) RMSE mensual
! (1+ksedq,11) Para HMLE autocorrelacionado y gaussiano (??)
! (1+ksedq,12) HMLE gaussiano autocorrelac
!
estadsed=0.00000000000
DO j=1,ksedq
  DO t=1,nt
    estadsed(j,3)=estadsed(j,3)+aforosed(j).obs(t) !suma caudales observados
    estadsed(j,9)=estadsed(j,9)+aforosed(j).sim(t) !suma caudales simulados
	estadsed(j,12)=estadsed(j,12)+(aforosed(j).obs(t)-aforosed(j).sim(t)) !suma diferencias
  ENDDO
  estadsed(j,3)=estadsed(j,3)/(nt)   !Qmedio para el Indice de Nash
  estadsed(j,9)=estadsed(j,9)/(nt)   !Qmedio simulado para Likelihood Gaussian Autocorr 
  estadsed(j,12)=estadsed(j,12)/(nt)   !Error medio simulado para LGA (mu)
ENDDO
DO j=1,ksedq
  k=j+ksedq
!  l=j+ksedq
  DO t=1,nt
    IF (aforosed(j).obs(t).gt.estadsed(j,1)) THEN
      estadsed(j,1)=aforosed(j).obs(t)	!Caudal pico Observado
      estadsed(j,2)=dt*t       !Tiempo al pico Observado
    ENDIF
    IF (aforosed(j).sim(t).gt.estadsed(k,1)) THEN
      estadsed(k,1)=aforosed(j).sim(t)	!Caudal pico Simulado
      estadsed(k,2)=dt*t       !Tiempo al pico simulado
    ENDIF
	IF (aforosed(j).obs(t).le.0.0) aforosed(j).obs(t)=0.00
	estadsed(j,7)=estadsed(j,7)+(aforosed(j).obs(t)*dts)  !Vol Obs m³
    estadsed(k,7)=estadsed(k,7)+(aforosed(j).sim(t)*dts)  !Vol Sim m³
	estadsed(k,4)=estadsed(k,4)+(aforosed(j).obs(t)-aforosed(j).sim(t))**2.0  !(Qobs-Qsim)^2
    estadsed(k,3)=estadsed(k,3)+(aforosed(j).obs(t)-estadsed(j,3))**2.0  !(Qobs-Qmed)^2
	estadsed(j,8)=estadsed(j,8)+(aforosed(j).sim(t)-estadsed(j,9))**2.0 !(Qsim-Qmed)^2
	estadsed(j,5)=aforosed(j).obs(t)**(2.*(fol-1.)) !Factor de ponderación (lambda=2.0)
	estadsed(j,4)=estadsed(j,4)+estadsed(j,5)*(aforosed(j).sim(t)-aforosed(j).obs(t))*(aforosed(j).sim(t)-aforosed(j).obs(t))
    IF (estadsed(j,5).gt.0.) estadsed(j,6)=estadsed(j,6)+LOG(estadsed(j,5))  !log Productoria
	estadsed(k,9)=estadsed(k,9)+ABS((aforosed(j).obs(t)-aforosed(j).sim(t))/(aforosed(j).obs(t)-estadsed(j,3))) !Coef de eficiencia
	estadsed(j,11)=estadsed(j,11)+(aforosed(j).obs(t)-estadsed(j,3))*(aforosed(j).sim(t)-estadsed(j,9))  !(Qobs-Qm)*(Qsim-Qm)
	estadsed(k,10)=estadsed(k,10)+(aforosed(j).obs(t)-aforosed(j).sim(t))*(aforosed(j).obs(t)-aforosed(j).sim(t))/(aforosed(j).obs(t)*aforosed(j).obs(t))
  ENDDO
  estadsed(j,11)=(estadsed(j,11)/(nt))/(((estadsed(k,3)/(nt))**0.5)*  &
              ((estadsed(j,8)/(nt))**0.5))  !Coef correlac. de errores (alfa)
  DO t=1,nt   !Para HMLE autocorrelacionado y gaussiano
    estadsed(k,11)=estadsed(k,11)+((aforosed(j).obs(t)-aforosed(j).sim(t))-estadsed(j,12)-estadsed(j,11)*(aforosed(j).obs(t-1)-aforosed(j).obs(t-1)))**2.0
  ENDDO
  IF ((1.0-estadsed(j,11)**2.0).gt.0.0) THEN
    IF ((2.*3.1416*(estadsed(k,4)/(nt))).gt.0.0) THEN
      estadsed(k,12)=LOG(1.0-estadsed(j,11)**2.)/2.0-(nt)*LOG(2.*3.1416*(estadsed(k,4)/  &
	              (nt)))/2.0+(estadsed(k,11)+(1.0-estad(j,11)**2.)*(((aforosed(j).obs(1)-   &
				  aforosed(j).obs(1))-estadsed(j,12))**2.)*(-1./(2.*(estadsed(k,4)/(nt)))))
    ELSE
	  write(*,*) 'Error 2 - log negativo en Error Gauss. Autocorr.'
    ENDIF
  ELSE
    write(*,*) 'Error 1 - log negativo en Error Gauss. Autocorr.'
  ENDIF
  !salrio=salrio+cell(aforo(j).pos).acum*arcelkm
  estadsed(k,5)=100.0*(estadsed(j,7)-estadsed(k,7))/estadsed(j,7)         !% Error Volumen 
  estadsed(k,6)=(estadsed(k,4)/estadsed(k,3))    !Indice de Nash
  estadsed(k,10)=estadsed(j,10)   !RMSE mensual
  estadsed(k,4)=(estadsed(k,4)/(nt))**0.5   !RMSE
  estadsed(k,8)=EXP(LOG(1.0/(nt))+LOG(estadsed(j,4))-(estadsed(j,6)/(nt)))  !HMLE
  estadsed(k,6)=1.0-estadsed(k,6)    !Indice de Nash
  estadsed(k,9)=1.0-estadsed(k,9)    !Coef. de eficiencia (Nash sin cuadrado)
  !estadsed(k,12)=EXP(estadsed(k,12)) !HMLE gaussiano autocorrelac
ENDDO
!!!!!!!!!!
ALLOCATE(seriesed(nt+10,ksedq*2))
seriesed=0
DO i=1,ksedq
  DO t=1,nt
    seriesed(t,2*i-1)=aforosed(i)%obs(t)
    seriesed(t,2*i)=aforosed(i)%sim(t)
  ENDDO
ENDDO
!
WRITE (14,'(A2)') '* '
DO i=1,ksedq
  WRITE (14,'(A2)') '* ' 
  IF (lang.eq.1) THEN
      WRITE (14,'(A57)') '* SERIES TEMPORALES DE SALIDA ESTACIONES AFORO SEDIMENTOS' 
      WRITE(14,'(A2,A1,A25,A17,F16.4,A3)')aforosed(i).codigo,'"',aforosed(i).name,'Área acumulada',aforosed(i).area,'km²'
      WRITE (14,'(A73)') '*......................CAUDAL DE SEDIMENTOS [m³/s].......................'
      WRITE(14,'(A1,A13,2A24)') '*',' ----DT---- ',' --------Qobs--------- ',' --------Qsim--------- '
  ELSE IF (lang.eq.2) THEN
      WRITE (14,'(A57)') '* OUTPUT TIME SERIES AT SEDIMENT GAUGING STATIONS        ' 
      WRITE(14,'(A2,A1,A25,A17,F16.4,A3)')aforosed(i).codigo,'"',aforosed(i).name,'Accum. Area   ',aforosed(i).area,'km²'
      WRITE (14,'(A73)') '*......................SEDIMENT DISCHARGE [m³/s].........................'
      WRITE(14,'(A1,A13,2A24)') '*',' ----DT---- ',' --------Qobs--------- ',' --------Qsim--------- '
  ENDIF
  WRITE(14,*)

  DO t=1,nt
    tpo=t*dtmin/60.
	WRITE(14,'(x,F13.4,2(x,F23.15))')tpo,aforosed(i).obs(t),aforosed(i).sim(t)
  ENDDO
  WRITE (14,'(A2)') '* '
  IF (lang.eq.1) THEN
      WRITE(14,*) '* Estadísticos'
  ELSE IF (lang.eq.2) THEN
      WRITE(14,*) '* Statistics'
  ENDIF
  !! Calcula estadísticos sedimentos observados VS sedimentos simulados
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
! (1+ksedq,1) Qmax sim
! (1+ksedq,2) T(Qmax) sim 
! (1+ksedq,3) (Qobs-Qmed)^2
! (1+ksedq,4) RMSE
! (1+ksedq,5) % Error Volumen
! (1+ksedq,6) Nash-Sutcliffe
! (1+ksedq,7) Vol Sim Hm³
! (1+ksedq,8) HMLE
! (1+ksedq,9) Coef. de eficiencia (Nash sin cuadrado)
! (1+ksedq,10) RMSE mensual
! (1+ksedq,11) Para HMLE autocorrelacionado y gaussiano (??)
! (1+ksedq,12) HMLE gaussiano autocorrelac
  IF (lang.eq.1) THEN
      WRITE(14,1234)'* Caudal Máximo Obs. (m3/s)   ',estadsed(i,1) !seriesed(nt+2,2*i-1)
      WRITE(14,1234)'* Caudal Máximo Sim. (m3/s)   ',estadsed(i+ksedq,1) !seriesed(nt+2,2*i)
      WRITE(14,1234)'* RMSE                        ',estadsed(i+ksedq,4) !seriesed(nt+7,2*i-1)
      WRITE(14,1234)'* Tiempo al pico Observado    ',estadsed(i,2) !seriesed(nt+9,2*i-1)
      WRITE(14,1234)'* Tiempo al pico Simulado     ',estadsed(i+ksedq,2) !seriesed(nt+9,2*i)
      WRITE(14,1234)'* Error Tp (%)                ',ABS(estadsed(i,2)-estadsed(i+ksedq,2))/estadsed(i,2) !ABS(seriesed(nt+9,2*i-1)-seriesed(nt+9,2*i))/seriesed(nt+9,2*i-1)*100
      WRITE(14,1234)'* Volumen Observado  (m3)     ',estadsed(i,7) !seriesed(nt+10,2*i-1)
      WRITE(14,1234)'* Volumen Simulado   (m3)     ',estadsed(i+ksedq,7) !seriesed(nt+10,2*i)
      WRITE(14,1234)'* Error en Volumen (%)        ',estadsed(i+ksedq,5) !ABS(seriesed(nt+10,2*i-1)-seriesed(nt+10,2*i))/seriesed(nt+10,2*i-1)*100
      WRITE(14,1234)'* Indice Nash y Sut. (NSE)    ',estadsed(i+ksedq,6) !seriesed(nt+8,2*i-1)
      WRITE(14,1234)'* Indice RSR                  ',RSRindexsed(i) !calculado en forma_Cedex (a la que se llama previamente)
      WRITE (14,'(A2)') '* '
  ELSE IF (lang.eq.2) THEN
      WRITE(14,1234)'* Max obs. discharge (m³/s)   ',estadsed(i,1) !seriesed(nt+2,2*i-1)
      WRITE(14,1234)'* Max sim. discharge (m3/s)   ',estadsed(i+ksedq,1) !seriesed(nt+2,2*i)
      WRITE(14,1234)'* RMSE                        ',estadsed(i+ksedq,4) !seriesed(nt+7,2*i-1)
      WRITE(14,1234)'* Obs. peak time              ',estadsed(i,2) !seriesed(nt+9,2*i-1)
      WRITE(14,1234)'* Sim. peak time              ',estadsed(i+ksedq,2) !seriesed(nt+9,2*i)
      WRITE(14,1234)'* Error Tp (%)                ',ABS(estadsed(i,2)-estadsed(i+ksedq,2))/estadsed(i,2) !ABS(seriesed(nt+9,2*i-1)-seriesed(nt+9,2*i))/seriesed(nt+9,2*i-1)*100
      WRITE(14,1234)'* Observed volume  (m³)       ',estadsed(i,7) !seriesed(nt+10,2*i-1)
      WRITE(14,1234)'* Simulated volume (m³)       ',estadsed(i+ksedq,7) !seriesed(nt+10,2*i)
      WRITE(14,1234)'* Volume error (%)            ',estadsed(i+ksedq,5) !ABS(seriesed(nt+10,2*i-1)-seriesed(nt+10,2*i))/seriesed(nt+10,2*i-1)*100
      WRITE(14,1234)'* Nash&Sut. Efficiency (NSE)  ',estadsed(i+ksedq,6) !seriesed(nt+8,2*i-1)
      WRITE(14,1234)'* RSR index                  ',RSRindexsed(i) !calculado en forma_Cedex (a la que se llama previamente)
      WRITE (14,'(A2)') '* '
  ENDIF
ENDDO
aaa='sd '
IF (lang.eq.1) THEN
    WRITE (14,'(A20,I10)') '* Numero de celdas: ',ncel
    WRITE (14,'(A48)')'* Areas umbrales para flujo superficial (km2)   '
    WRITE (14,'(A43,F15.5)') '* Interflujo (inicio cárcavas efímeras)  : ',(areaumbral(2,j),j=1,npar)
    WRITE (14,'(A43,F15.5)') '* Flujo base (inicio cauces permanentes) : ',(areaumbral(3,j),j=1,npar)
    WRITE (14,'(A20,F15.5)') '* Tamaño de celda : ', dx
    WRITE (14,'(A33)') '* RESUMEN DE SALIDA DE SEDIMENTOS'
    WRITE (14,'(A41)') '* Por fracción de tamaño y totales, en m3'
    WRITE (14,'(A9)') '* EROSION'
    WRITE (14,'(A3,x,A22,F20.2,A4)') aaa,'         Arena erodada',ABS(tot_ERODADO(1)),'m³'
    WRITE (14,'(A3,x,A22,F20.2,A4)') aaa,'          Limo erodado',ABS(tot_ERODADO(2)),'m³'
    WRITE (14,'(A3,x,A22,F20.2,A4)') aaa,'       Arcilla erodada',ABS(tot_ERODADO(3)),'m³'
    WRITE (14,'(A3,x,A22,F20.2,A4)') aaa,'         EROSION TOTAL',ABS(tot_EROSION),'m³'
    WRITE (14,'(A40)') '* Sedimentos que PERMANECEN en la CUENCA'
    WRITE (14,'(A26)') '* Sedimentos en suspensión'
    WRITE (14,'(A3,x,A22,F20.2,A4)') aaa,'   Arena en suspensión',sus_TOTAL(1),'m³'
    WRITE (14,'(A3,x,A22,F20.2,A4)') aaa,'    Limo en suspensión',sus_TOTAL(2),'m³'
    WRITE (14,'(A3,x,A22,F20.2,A4)') aaa,' Arcilla en suspensión',sus_TOTAL(3),'m³'
    WRITE (14,'(A3,x,A22,F20.2,A4)') aaa,'   TOTAL EN SUSPENSIÓN',tot_SUSREM,'m³'
    WRITE (14,'(A24)') '* Sedimentos depositados'
    WRITE (14,'(A3,x,A22,F20.2,A4)') aaa,'      Arena depositada',dep_TOTAL(1),'m³'
    WRITE (14,'(A3,x,A22,F20.2,A4)') aaa,'       Limo depositado',dep_TOTAL(2),'m³'
    WRITE (14,'(A3,x,A22,F20.2,A4)') aaa,'    Arcilla depositada',dep_TOTAL(3),'m³'
    WRITE (14,'(A3,x,A22,F20.2,A4)') aaa,'      TOTAL DEPOSITADO',tot_DEPREM,'m³'
    WRITE (14,'(A26)') '* Totales ((-)ero+sus+dep)'
    WRITE (14,'(A3,x,A22,F20.2,A4)') aaa,'           Arena total',sed_TOTAL(1),'m³'
    WRITE (14,'(A3,x,A22,F20.2,A4)') aaa,'            Limo total',sed_TOTAL(2),'m³'
    WRITE (14,'(A3,x,A22,F20.2,A4)') aaa,'         Arcilla total',sed_TOTAL(3),'m³'
    WRITE (14,'(A3,x,A22,F20.2,A4)') aaa,'                 TOTAL',tot_REM,'m³'
    WRITE (14,'(A44)') '* Sedimentos erodados que SALEN de la CUENCA'
    WRITE (14,'(A3,x,A22,F20.2,A4)') aaa,'        Arena generada',sed_SALIDA(1),'m³'
    WRITE (14,'(A3,x,A22,F20.2,A4)') aaa,'         Limo generado',sed_SALIDA(2),'m³'
    WRITE (14,'(A3,x,A22,F20.2,A4)') aaa,'      Arcilla generada',sed_SALIDA(3),'m³'
    WRITE (14,'(A3,x,A22,F20.2,A4)') aaa,'  SEDIMENTOS GENERADOS',tot_SEDSALIDA,'m³'
    WRITE (14,'(A38)') '* Sedimentos depositados en las presas'
    WRITE (14,'(A3,x,A22,F20.2,A4)') aaa,'  SEDIMENTOS en presas',TOT_presas,'m³'
    WRITE (14,'(A59,F20.8,A4)') '* Error en el balance de masa de sedimentos, en porcentaje ',error_porcent_SED,'%'
    !calcula SDR
    !WRITE (14,'(A25,F10.3,A4)')'* Sediment Delivery Ratio',tot_SEDSALIDA/(ABS(tot_EROSION))*100,'%'
    !Serie de salida (flujos y concentración de sedimentos)
ELSE IF (lang.eq.2) THEN
 WRITE (14,'(A20,I10)') '* Number of cells:  ',ncel
          WRITE (14,'(A48)')'* Threshold areas                       (km²)   '
          WRITE (14,'(A43,F15.5)') '* Interflow  (starting ephemeral gullies): ',(areaumbral(2,j),j=1,npar)
          WRITE (14,'(A43,F15.5)') '* Base flow  (Starting permament channel): ',(areaumbral(3,j),j=1,npar)
          WRITE (14,'(A20,F15.5)') '* Cell size      : ', dx
          WRITE (14,'(A33)') '* RESUME OF SEDIMENT OUTPUT      '
          WRITE (14,'(A41)') '* By size fraction and totals, in m³     '
          WRITE (14,'(A9)') '* EROSION'
          WRITE (14,'(A3,x,A22,F20.2,A4)') aaa,'         Eroded sand  ',ABS(tot_ERODADO(1)),'m³'
          WRITE (14,'(A3,x,A22,F20.2,A4)') aaa,'         Eroded silt  ',ABS(tot_ERODADO(2)),'m³'
          WRITE (14,'(A3,x,A22,F20.2,A4)') aaa,'         Eroded clay  ',ABS(tot_ERODADO(3)),'m³'
          WRITE (14,'(A3,x,A22,F20.2,A4)') aaa,'         TOTAL EROSION',ABS(tot_EROSION),'m³'
          WRITE (14,'(A40)') '* Sediments within the ccatchment       '
          WRITE (14,'(A26)') '* Suspeded sediment       '
          WRITE (14,'(A3,x,A22,F20.2,A4)') aaa,'   Suspended sand     ',sus_TOTAL(1),'m³'
          WRITE (14,'(A3,x,A22,F20.2,A4)') aaa,'   Suspended silt     ',sus_TOTAL(2),'m³'
          WRITE (14,'(A3,x,A22,F20.2,A4)') aaa,'   Suspended clay     ',sus_TOTAL(3),'m³'
          WRITE (14,'(A3,x,A22,F20.2,A4)') aaa,'   TOTAL SUSPENDED    ',tot_SUSREM,'m³'
          WRITE (14,'(A24)') '* Deposited sediment    '
          WRITE (14,'(A3,x,A22,F20.2,A4)') aaa,'      Deposited sand  ',dep_TOTAL(1),'m³'
          WRITE (14,'(A3,x,A22,F20.2,A4)') aaa,'      Deposited silt  ',dep_TOTAL(2),'m³'
          WRITE (14,'(A3,x,A22,F20.2,A4)') aaa,'      Deposited clay  ',dep_TOTAL(3),'m³'
          WRITE (14,'(A3,x,A22,F20.2,A4)') aaa,'      TOTAL DEPOSITED ',tot_DEPREM,'m³'
          WRITE (14,'(A26)') '* Total   ((-)ero+sus+dep)'
          WRITE (14,'(A3,x,A22,F20.2,A4)') aaa,'           Total sand ',sed_TOTAL(1),'m³'
          WRITE (14,'(A3,x,A22,F20.2,A4)') aaa,'           Total silt ',sed_TOTAL(2),'m³'
          WRITE (14,'(A3,x,A22,F20.2,A4)') aaa,'           Total clay ',sed_TOTAL(3),'m³'
          WRITE (14,'(A3,x,A22,F20.2,A4)') aaa,'                 TOTAL',tot_REM,'m³'
          WRITE (14,'(A44)') '*Eroded sediment exported from the catchment'
          WRITE (14,'(A3,x,A22,F20.2,A4)') aaa,'        Generated sand',sed_SALIDA(1),'m³'
          WRITE (14,'(A3,x,A22,F20.2,A4)') aaa,'        Generated silt',sed_SALIDA(2),'m³'
          WRITE (14,'(A3,x,A22,F20.2,A4)') aaa,'        Generated clay',sed_SALIDA(3),'m³'
          WRITE (14,'(A3,x,A22,F20.2,A4)') aaa,'   GENERATED SEDIMENTS',tot_SEDSALIDA,'m³'
          WRITE (14,'(A38)') '* Sediments depoited inside reservoirs'
          WRITE (14,'(A3,x,A22,F20.2,A4)') aaa,'SEDIMENT in reservoirs',TOT_presas,'m³'
          WRITE (14,'(A59,F20.8,A4)') '* Mass balance error of total sediment, in percentage      ',error_porcent_SED,'%'
          !calcula SDR
          !WRITE (14,'(A25,F10.3,A4)')'* Sediment Delivery Ratio',tot_SEDSALIDA/(ABS(tot_EROSION))*100,'%'
          !Serie de salida (flujos y concentración de sedimentos)
END IF
WRITE (14,'(A2)') '* '
          
                       
DO i=1,naf
  IF (lang.eq.1) THEN
        WRITE (14,'(A61)') '* SERIES TEMPORALES DE SALIDA ESTACIONES AFORO CAUDAL LÍQUIDO'  
        WRITE(14,'(A2,A1,A25,A17,F16.4,A3)')aforo(i).codigo,'"',aforo(i).name,'Área acumulada',aforo(i).area,'km2'
        WRITE (14,'(A73)') '*......................CAUDAL DE SEDIMENTOS [m3/s].......................'
        WRITE(14,'(A1,A12,5A16)') '*',' ------DT----',' ---Qobs----',(codtex(j),j=1,4)
  ELSE IF (lang.eq.2) THEN
        WRITE (14,'(A61)') '* OUTPUT TIME SERIES AT WATER DISCHARGE GAUGING STATIONS     '  
        WRITE(14,'(A2,A1,A25,A17,F16.4,A3)')aforo(i).codigo,'"',aforo(i).name,'Acc. area     ',aforo(i).area,'km2'
        WRITE (14,'(A73)') '*......................SEDIMENT DISCHARGE   [m³/s].......................'
        WRITE(14,'(A1,A12,5(x,A16))') '*',' ------DT---',' ---Qobs----',(codtex(j),j=1,4)  
  ENDIF
  DO t=1,nt
    tpo=t*dtmin/60.
	WRITE(14,'(F13.4,5(x,F16.8))')tpo,aforo(i).obs(t),(serie(t,j),j=(i-1)*126+1,(i-1)*126+4)
  ENDDO
!  WRITE (14,'(A2)') '* '
!  WRITE (14,'(A50)') '*CONCENTRACIÓN DE SEDIMENTOS EN SUSPENSIÓN [Kg/m3]'
!  WRITE (14,'(A55)') '*Masa de sedimentos/Volumen de mezcla (agua+sedimentos)'
!  WRITE(14,'(A1,A12,4A15)') '*',' ------DT---',(codtex(j),j=1,4)
!  DO t=1,nt
!    tpo=t*dtmin/60.
!	WRITE(14,'(F13.4,4F15.5)')tpo,(serie(t,j),j=(i-1)*126+5,(i-1)*126+8)
!  ENDDO
  1234 FORMAT (A30,F15.8)
!  WRITE (14,'(A2)') '* '

!  !Series temporales de variables (control para ver el funcionamiento del modelo)
!  WRITE (14,'(A43)') '*SERIES TEMPORALES DE VARIABLES IMPORTANTES'
!  WRITE (14,'(A50)') '*Corresponde a la serie temporal aforo(i).sed_temp'
!  !Relaciones geomorfologicas iniciales
!  WRITE (14,'(A132)') '*...............................................Relaciones geomorfológicas iniciales................................................'
!!  WRITE (14,'(A233)') '*............FLUJO EN LADERA.............................................FLUJO EN CANAL..........................................................................................................CANAL A SECCIÓN LLENA...................'    
!  WRITE(14,'(A1,A12,8A15)') '*',' ------DT---',(codtex(j),j=5,12)
!  DO t=1,nt
!    tpo=t*dtmin/60.
!    WRITE(14,'(F13.4,8F15.5)')tpo,(serie(t,j),j=(i-1)*126+9,(i-1)*126+16)
!  ENDDO
!  WRITE (14,'(A2)') '* '

!  !Variables de control temporal (variables internas)
!  WRITE (14,'(A30)') '*Variables de control temporal'
!
!  WRITE (14,'(A60)') '*Procesos en suspensión, volumen de sedimentos transportados'
!  WRITE(14,'(A1,A12,3A15)') '*',' ------DT---',(codtex(j),j=13,15)
!  DO t=1,nt
!    tpo=t*dtmin/60.
!    WRITE(14,'(F13.4,3F15.5)')tpo,(serie(t,j),j=(i-1)*126+17,(i-1)*126+19)
!  ENDDO
!  WRITE (14,'(A2)') '* '
!
!  WRITE (14,'(A70)') '*Procesos como material del lecho, volumen de sedimentos transportados'
!  WRITE(14,'(A1,A12,3A15)') '*',' ------DT---',(codtex(j),j=16,18)
!  DO t=1,nt
!    tpo=t*dtmin/60.
!    WRITE(14,'(F13.4,3F15.5)')tpo,(serie(t,j),j=(i-1)*126+20,(i-1)*126+22)
!  ENDDO
!  WRITE (14,'(A2)') '* '

!balance
!  WRITE (14,'(A8)') '*Balance'
!  WRITE(14,'(A1,A12)') '*',' ------DT---'
!  DO t=1,nt
!    tpo=t*dtmin/60.
!    WRITE(14,'(F13.4,30F10.5)')tpo,(balanc_sed(t,j),j=1,30)
!  ENDDO
!  WRITE (14,'(A2)') '* '


!  WRITE (14,'(A74)') '*Procesos de erosión, volumen de sedimentos erodados (En celdas de ladera)'
!  WRITE(14,'(A1,A12,3A15)') '*',' ------DT---',(codtex(j),j=19,21)
!  DO t=1,nt
!    tpo=t*dtmin/60.
!    WRITE(14,'(F13.4,3F15.5)')tpo,(serie(t,j),j=(i-1)*126+23,(i-1)*126+25)
!  ENDDO
!  WRITE (14,'(A2)') '* '

ENDDO

!!!!!!!!!!!!!!!!!!!!!! Puntos de simulación sin registros
IF (knaf.gt.0) THEN
    IF (lang.eq.1) THEN
      WRITE (14,'(A2)') '* '
      WRITE (14,'(A2)') '* '
      WRITE (14,'(A61)') '* SERIES TEMPORALES DE SALIDA PUNTOS DE SIMULACIÓN'  
      WRITE (14,'(A2)') '* '
      WRITE (14,'(A73)') '*......................CAUDAL DE SEDIMENTOS [m3/s].......................'
      WRITE(14,'(A1,A12,<knaf>(x,A15))') '*',' ------DT-- ',(otros(i).name,i=1,knaf)
      DO t=1,nt
        tpo=t*dtmin/60.
	    WRITE(14,'(x,F12.3,<knaf>(x,F15.8))')tpo,(otros(i).sed_out(t,4),i=1,knaf)
      ENDDO
      WRITE (14,'(A2)') '* '
    ELSE IF (lang.eq.2) THEN
      WRITE (14,'(A2)') '* '
      WRITE (14,'(A2)') '* '
      WRITE (14,'(A61)') '* TIME SERIES AT SIMULATION POINTS                '  
      WRITE (14,'(A2)') '* '
      WRITE (14,'(A73)') '*......................SEDIMENT DISCHARGE   [m³/s].......................'
      WRITE(14,'(A1,A12,<knaf>(x,A15))') '*',' ------DT-- ',(otros(i).name,i=1,knaf)
      DO t=1,nt
        tpo=t*dtmin/60.
	    WRITE(14,'(F12.3,<knaf>(x,F15.8))')tpo,(otros(i).sed_out(t,4),i=1,knaf)
      ENDDO
      WRITE (14,'(A2)') '* '
    ENDIF
!  ALLOCATE(temp010(knaf))
!  WRITE (14,'(A60)') '*Procesos en suspensión, volumen de sedimentos transportados'
!  WRITE(14,'(A1,A12,<knaf>A25)') '*',' ------DT-- ',(otros(i).name,i=1,knaf)
!  temp010=0
!  DO t=1,nt
!    tpo=t*dtmin/60.
!    DO m=1,knaf
!      temp010(m)=serie2(t,(m-1)*126+17)+serie2(t,(m-1)*126+18)+serie2(t,(m-1)*126+19)
!    ENDDO  
!    WRITE(14,'(F12.4,<knaf>F25.7)')tpo,(temp010(i),i=1,knaf)
!  ENDDO
!  WRITE (14,'(A2)') '* '
!
!  WRITE (14,'(A70)') '*Procesos como material del lecho, volumen de sedimentos transportados'
!  WRITE(14,'(A1,A12,<knaf>A25)') '*',' ------DT-- ',(otros(i).name,i=1,knaf)
!  temp010=0
!  DO t=1,nt
!    tpo=t*dtmin/60.
!    DO m=1,knaf
!      temp010(m)=serie2(t,(m-1)*126+20)+serie2(t,(m-1)*126+21)+serie2(t,(m-1)*126+22)
!    ENDDO  
!    WRITE(14,'(F12.4,<knaf>F25.7)')tpo,(temp010(i),i=1,knaf)
!  ENDDO
!  WRITE (14,'(A2)') '* '
!  DEALLOCATE(temp010)

ENDIF

! EMBALSES (CASO DE TRAP EFFICIENCY) - GIAMBA 11/2011
IF (trapeff) THEN
  IF (lang.eq.1) THEN
    DO i=1,numpresas
      WRITE (14,'(A2)') '* '
      WRITE (14,'(A35)') '* SERIES TEMPORALES DE LOS EMBALSES'  
      WRITE (14,'(A2)') '* '
      WRITE (14,'(A35)')dam(i).Pnombre
      WRITE (14,'(10(A15,x))')'TIEMPO         ','CAUDAL_ENTRADA ','SED_IN_sand    ','SED_IN_silt    ','SED_IN_clay    ','SED_OUT_sand   ','SED_OUT_silt   ','SED_OUT_clay   ','PROFUNDIDAD    ','VOL_DEPOSITO   '
      DO t=1,nt
        tpo=t*dtmin/60.
        WRITE(14,'(F15.3,x,8(F15.8,x),F15.4,x)')tpo,dam(i).qinTE(t),dam(i).sedinTE(t,1),dam(i).sedinTE(t,2),dam(i).sedinTE(t,3),dam(i).sedoutTE(t,1),dam(i).sedoutTE(t,2),dam(i).sedoutTE(t,3),dam(i).depthTE(t),dam(i).depositTE(t)
      ENDDO
    ENDDO
  ELSE IF (lang.eq.2) THEN
    DO i=1,numpresas
      WRITE (14,'(A2)') '* '
      WRITE (14,'(A35)') '*SEDIMENT TIME SERIES IN RESERVOIRS'  
      WRITE (14,'(A2)') '* '
      WRITE (14,'(A35)')dam(i).Pnombre
      WRITE (14,'(10(A15,x))')'TIME           ','INLET DISCHARGE','SED_IN_sand    ','SED_IN_silt    ','SED_IN_clay    ','SED_OUT_sand   ','SED_OUT_silt   ','SED_OUT_clay   ','DEPTH          ','VOL_DEPOSIT    '
      DO t=1,nt
        tpo=t*dtmin/60.
        WRITE(14,'(F15.3,x,8(F15.8,x),F15.4,x)')tpo,dam(i).qinTE(t),dam(i).sedinTE(t,1),dam(i).sedinTE(t,2),dam(i).sedinTE(t,3),dam(i).sedoutTE(t,1),dam(i).sedoutTE(t,2),dam(i).sedoutTE(t,3),dam(i).depthTE(t),dam(i).depositTE(t)
      ENDDO
    ENDDO
  ENDIF
ENDIF

!Cambios - GIAMBA 11/2011
IF (trapeff) THEN
  CALL escribeTEini
ENDIF
132 FORMAT(f12.6)
133 FORMAT(f12.5)
134 FORMAT(I7)
135 FORMAT(f15.6)
136 FORMAT(f12.5)
137 FORMAT(f12.6)
WRITE (14,'(A2)') '* '

!Escribe los parámetros empleados (FALTA)
WRITE (14,'(A2)') '* '

!Escribe los ficheros utilizados
WRITE (14,'(A2)') '* '
WRITE(14,'(A21)') '* FILES              '
WRITE (14,'(A2)') '* '
DO i=1,10
  WRITE (14,'(A2,A128)') '* ',archsed(i)
ENDDO
WRITE (14,'(A2)') '* '

!Escribe resumen de almacenamientos y flujos (FALTA)

CLOSE(14)

IF (ALLOCATED(serie)) DEALLOCATE(serie)
IF (ALLOCATED(serie2)) DEALLOCATE(serie2)
IF (ALLOCATED(codtex)) DEALLOCATE(codtex)
IF (ALLOCATED(seriesed)) DEALLOCATE(seriesed)
END SUBROUTINE

