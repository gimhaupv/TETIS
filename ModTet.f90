!*************************************************************
!* Modulo de variables para TETIS 
!*************************************************************
MODULE modtet
!* Variables que se emplean en expresiones del MENU, QWIN,DIALOGOS y RATON
!****************************************************************************************
!* LOGICAS 
!    out  ------>  Para salir del programa TETIS debe ser .F.  
!    config() -->  Selecciona el tipo de información de entrada y de salida
!    modulos() ->  Variables lógicas para activar o desactivar unos modulos de la interfaz
!    1 - Nieve, 2 - Embalses, 3 - Recorte cuencas embalses, 4 - Calibración automática hidrología, 5 - Calibración automática sedimentos, 6 - Diferencia entre cárcavas y cauces
!    modulos2() ->  Variables lógicas para activar o desactivar unos modulos de la interfaz
!    1 - Nieve distribuida, 2 - Karst
!    existe  --->  Para decidir si un fichero ha sido creado previamente
!    resulta --->  Se emplea para activar o desactivar un Menu
!    sim2 ------>  Muestra % avance si es .T. (Simulación) y no muestra si .F. (Opt.Automatica)
!    printascii->  Imprime mapas ASCII de las variables de estado
!    variablesascii-> Variables a imprimir en formato esri ascii 
!    finalesascii--> Estados finales a imprimir en formato ascii
!    logicman, logicman2 --> Variables lógicas usudas cuando se hace balance en caso de presencia de karst
!* ENTERAS
!    message -----------------> Despues de ejecutar un acción de mensaje en Caja de dialogo
!    xmouse,ymouse,keystate --> Posición x,y en pantalla del click del mouse  
!    raton -------------------> Si se oprime click de raton es 1
!    resul,resul1 ------------> Valor entero despues de ejecutar un accion de QWIN
!    sale  -------------------> Si se sale con error de una subrutina es 2
!    mnum  -------------------> Asignación de posición en el Menu principal
!    ios ---------------------> Para detectar el End Of File (-1).
!    i4 ----------------------> Define color de pantalla
!    non() -------------------> Para ordenar variables MINLOC
!    dtascii -----------------> Imprime los mapas ASCII de variables de estado cada "dtascii" timesteps
!    lang --------------------> Idioma: 1 - español, 2 - inglés
!* CARACTER
!    mname -------------------> Nombre para el Menu principal
LOGICAL(KIND=4) out,resulta,config(7), modulos(5),modulos2(5),bint,existe,existe2,sim2,betalin,simevents, printascii, variablesascii(53),finalesascii(25),logicman,logicman2,variablesasciinitr(48),finalesasciinitr(15)
INTEGER non(1),message,xmouse,ymouse,keystate,i4,resul,sale,raton,mnum,resul1,ios,nexe,errr,lmensaje,dtascii,lang,typeyear
CHARACTER(LEN=50)mname
INTEGER res

!*---------------------------------------------------------------------------------------


!* Variables ENTEROS generales del MODELO
!****************************************************************************************
!   ncel ---------->  Nº de celdas
!   nt ------------>  Nº de intervalos temporales
!   nindex -------->  Intervalo a partir del cual se calcúlan los índices
!   npar ---------->  Nº de zonas con parámetros geomorfológicos diferentes
!   nest ---------->  Nº de estaciones cercanas a usar en la interpolación
!   naf,jq -------->  Nº de estaciones de aforo
!   knaf,ja ------->  Nº de sitios a simular sin datos de aforo
!   kppt,jp ------->  Nº de estaciones de lluvia
!   kevp,je ------->  Nº de estaciones de evapotranspiración
!   ktem,jt ------->  Nº de estaciones de temperatura
!   kniv,jh ------->  Nº de estaciones nivometricas
!   kadi,ji ------->  Nº de lugares de aporte de flujo base
!   nemb,jn ------->  Nº de estaciones con niveles de embalse
!   vnemb,jv ------>  Nº de estaciones con volumenes de embalse
!   ksedq,jsedq ----->  Nº de estaciones con caudal de sedimentos
!   ksedv,jsedv --->  Nº de estaciones con volumenes de sedimentos
!   knemb,js ------>  Nº de estaciones con caudales de salida del embalse
!   nveg,jw---------> Nº de estaciones con información del estado de la vegetación
!   nradiacion,jr---> Nº de estaciones con datos de radiación solar
!   kadised1,jsedadi1 ----->  Nº de lugares de aporte de caudal sólido, fracción Arena (Cristina, 12/11/2015)
!   kadised2,jsedadi2 ----->  Nº de lugares de aporte de caudal sólido, fracción Limo (Cristina, 12/11/2015)
!   kadised3,jsedadi3 ----->  Nº de lugares de aporte de caudal sólido, fracción Arcilla (Cristina, 12/11/2015)
!   nnest,nnest2 -->  Valor Minimo entre nest y kniv (interpolacion)
!   tnest,tnest2 -->  Valor Minimo entre nest y ktem (interpolacion)
!   enest,enest2 -->  Valor Minimo entre nest y kevp (interpolacion)
!   nfil,ncol ----->  Fila y columna generico (se usa en varias partes)
!   fmes ----------->  Numero de dias segun el mes (puede ser 28, 30 o 31)
!   iflagp -------->  Bandera para definir la existencia de lluvia
!   mi,mj --------->  Nº de filas y columnas de los mapas originales
!   ktotal -------->  Nº Total de estaciones
!   ncon ---------->  Nº de puntos de control (puntos importantes Q,N,V,S,B,D)
!   erie ---------->  Nº de zonas de riegos
!   nw(3,50) ------>  Umbral de escorrentía, interflujo y flujo base (Long. laderas)
!   nip,ncp ------->  Indices para reducir la ecuacion
!   i,j,k,l,niv,n,t,t1,w,n2,nb,n2b,ad ------>  Contadores varios
!   nsurcoent(50) ->  Número de surcos en una celda (entero)
!   class_tex -----> Variable clase de textura para la subrutina what_texture
!   nmonth ---------> Mes del paso de ejecucion t
!   nday -----------> Dia del paso de ejecución t
!   nhora, nmin, nseg -> Hora, minuto y segundo del paso de ejecucion t
!   njulian --------> Número del dia (0-365)
!   row ------------> Número de lineas de los ficheros originales (sirve para escribir los archivos de salida con el mismo formato de los de entrada)
!   column ---------> Número de columnas de los ficheros originales (sirve para escribir los archivos de salida con el mismo formato de los de entrada)
!   newnt2 ----------> Nuevo numero total de time-step que se usan en el cálculo de estadisticos
!   newc -----------> FOLON
!   BKCOLOR --------> color fondo
!   FCOLOR ---------> color texto
!   stma_op --------> sistema operativo (0: Linux, 1: Windows)
INTEGER n,nest,naf,nafn,kniv,ktem,knaf,kppt,nemb,vnemb,knemb,kevp,kadi,ktotal,ncon,ji,  &
  jn,jq,je,jp,js,jh,ja,jt,jv,i,j,k,l,niv,mi,mj,erie,nnest,tnest,enest,rnest,iflagp,ad,    &
  nip,ncp,n2,nb,ncel,npar,nfil,ncol,t,nt,n2b,t1,w,enest2,tnest2,nest2,rnest2, &
  class_tex, nmonth,nday,nyear,njulian,row,column,fmes,ksedq,jsedq,ksedv,jsedv,newc, &
  newnt2,BKCOLOR,FCOLOR,ncero,m,contcauce,nindex,nman,nveg,jw,nradiacion,jr,kveg,contador,cont_veg,t_niveles,kadised1,kadised2, &
  kadised3,jsedadi1,jsedadi2,jsedadi3,nw(3,50),mii,mji,nhora,nmin,nseg,stma_op
  !Real,Allocatable:: nw(:,:) Cris (17/10/2016) Regiones homogéneas allocatable, ahora está fijo en 50
!*---------------------------------------------------------------------------------------

!* Variables REALES generales del MODELO
!****************************************************************************************
!   r()  ----------->  Factores Correctores del modelo (a calibrar)
!   d(,),e(,) ------>  Coeficientes y exponentes de los parámetros geomorfológicos para cauces
!   dc(,),ec(,) ---->  Coeficientes y exponentes de los parámetros geomorfológicos para cárcavas
!   wdad(,) -------->  Estado de humedad inicial genéricos para toda la cuenca
!   sedantec(,) ---->  Estado de sedimentos inicial genéricos para toda la cuenca
!   bbeta ---------->  Factor de interpolación de la nieve con la cota
!   betappt -------->  Factor de interpolación de la lluvia con la elevación
!   betatemp  ------>  Factor de temperatura de la lluvia con la elevación
!   ro1,ro2 -------->  Tasa de fusion de nieve con y sin lluvia
!   lamb(,) -------->  Factor de cultivo para la evapotranspiracion mensual
!   hped ----------->  Parámetro de evapo del subxuelo (esta desactivado!!!)
!   cn,cw,cs,ce ---->  Coordenadas de la zona de analisis (malla)
!   dx,dy  --------->  Tamaño de la celda (ancho y largo en metros)
!   dt,dts,dtmin --->  Intervalo temporal en horas, segundos y minutos
!   arcel,arcelkm -->  Area de la celda
!   arac ----------->  Area acumulada en Km2 para cárcavas y cauces
!   slp ------------>  Pendiente
!   veldts,divel --->  Velocidad del flujo y divergencia en la OCG para cauces
!   cvel,eas,esl,
!   veldtsc,divelc ->  Velocidad del flujo y divergencia en la OCG para cárcavas
!   cvelc,easc,eslc,
!   cvels,eass,esls,
!   eac,ey,ez ------>  Valores para cada celda utilizados en la OCG para cauces
!   eacc,eyc,ezc --->  Valores para cada celda utilizados en la OCG para cárcavas
!   qsale,qsale0,
!   qsale1,pder,pizq,
!   orig,arsec,
!   salrio,errmed -->  Valores intermedios de ayuda que no indican nada
!   volin ---------->  Volumen inicial (para pasar de volumen a altura en embalses)
!   niveldado ------>  Nivel inicial (para pasar de altura a volumen en embalses)
!   mesin ---------->  Mes inicial, sirve para asociar la ETP y riego para cada mes
!   diain ---------->  Día inicial
!   tfn ------------>  Tasa de fusión de nieve (puede ser ro1 si hay ppt o ro2)
!   evpt ----------->  Evapotranspiracion antes de ser almacenada en serie
!   fusion --------->  Cantidad de agua producida por la fusión de la nieve
!   tbase ---------->  Temperatura base para inicio de la fusión de nieve
!   factor --------->  Factor de escala para unidades en el dibujo de mapas
!   sinvdis -------->  Suma en la sumatoria del inverso de la distancia
!   sum ------------>  Variable para las sumatorias
!   fol ------------>  Lambda en la función objetivo HMLE (Falta ponerla variable)
!   fobj ----------->  Valor de la Función Objetivo
!   nrand ---------->  Numero aleatorio entre 0 y 1
!   lluviaHm3 ------>  Volumen en Hm3 total de lluvia para el área de captación de la estación
!   ETHm3 ---------->  Volumen en Hm3 total de evapotranspiración para el área de captación de la estación
!   EDHm3 ---------->  Volumen en Hm3 total de escorrentía directa para el área de captación de la estación
!   IFHm3 ---------->  Volumen en Hm3 total de interflujo para el área de captación de la estación
!   FBHm3 ---------->  Volumen en Hm3 total de flujo base para el área de captación de la estación
!   PSHm3 ---------->  Volumen en Hm3 total de pérdidas subterráneas para el área de captación de la estación
!   PRHm3 ---------->  Volumen en Hm3 total de la percolación 
!	difalm  -------->  Diferencia de almacenamiento (Estado final-Estado final)
!	difalmveg  ----->  Diferencia de almacenamiento de los primeros 2 tanques (Estado final-Estado final)
!   almini(8) ------>  Almacenamiento Inicial de los tanques
!   almfin(8) ------>  Almacenamiento Final de los tanques
!   functeta --------------> Función de la humedad del suelo (tetis sed veg)
!   drdt ------------------> Derivada de la evolución de la vegetación (tetis sed veg)
!   nlad ----------->  Rugosidad en ladera (para el flujo superficial en ladera) (Juan Camilo2008)
!   incremq -------->  Incremento de caudal para resolver método de pulso modificado en embalses (Juan Camilo2008)
!   fpulmin ----------> Límite inferior de búsqueda en la Función pulso (Juan Camilo2008)
!   fpulmax ----------> Límite superior de búsqueda en la Función pulso (Juan Camilo2008)
!   rini -------------> Valor inicial de la evolución de la vegetación
!   wsini ---> Valor inicial del estres hídrico
!   arsecnew      --->  Área de la sección transversalen la OCG (Camilo, 2009)
!   deltaarsec    --->  Cambio en el área de la sección en la OCG (Camilo, 2009)


REAL cn,cw,cs,ce,dx,dy,dts,dt,arcel,dtmin,arac,betappt,betatemp,bbeta,r(10), &
  ro1,ro2,orig,pizq,pder,ptot,ettot,qsale,qsale0,qsale1,slp,arcelkm, &
  salrio,hped,veldts,divel,cvel,eas,esl,eac,ey,ez,tfn,volin,mesin,diain,horain,minin,segin,minInDay,minFinInDay,  &
  arsec,evpt,rdn,fusion,tbase,errmed,niveldado,factor,sinvdis,sum,fol,fobj,nrand,    &
  lluviaHm3,ETHm3,EDHm3,IFHm3,FBHm3,PSHm3,difalm,almini(0:8),almfin(0:8),veldtsc, &
  divelc,cvelc,easc,eslc,veldtss,divels,cvels,eass,esls,eacc,eyc,ezc,eacs,eys,ezs,aracs,divellad, &
  functeta,drdt,nlad,errmed0,errmed1,incremq,volant,&
  fpulmin,fpulmax,difalmveg,PRHm3,difalmsuelo,wsini,rini,sedantec(15,50),averagepar(7), &
  Imaxmedio,wdadfin(6),expinf,alpha,rango(2),peso(3),fus,balancsalrio,difalm1,difalm2,cmex,suma_niveles, &
  wdad(8,50),d(6,50),e(6,50),dc(6,50),ec(6,50)
  REAL*8 deltaarsec
REAL, ALLOCATABLE :: lamb(:,:)
!REAL, ALLOCATABLE :: wdad(:,:),d(:,:),e(:,:),dc(:,:),ec(:,:) Cris (17/10/2016) Regiones homogéneas allocatable,
!ahora está fijo en 50
REAL*8, ALLOCATABLE :: arsecnew(:)

!*---------------------------------------------------------------------------------------
!* Variables CHARACTER generales del MODELO
!****************************************************************************************
!   arch() --------->  Nombre de los ficheros a utilizar  (incluye el PATH)
!   archsed() ------>  Nombre de los ficheros de sedimentos a utilizar  (incluye el PATH)
!   archveg() ------>  Nombre de los ficheros de vegetacion a utilizar  (incluye el PATH)
!   archnit() ------>  Nombre de los ficheros de nitrógeno a utilizar  (incluye el PATH)
!   tit() ---------->  Titulos en encabezado de los ficheros ASC y MDT
!   artem ---------->  Nombre temporal para un fichero (incluye PATH)
!   mensaje -------->  Mensaje de error
!   dirtra --------->  Directorio de trabajo (se lee del fichero filessp.txt)
!   archin --------->  Nombre inicial de un fichero (sin incluir directorio)
!   nam ------------>  Variable auxiliar para el nombre de estaciones
!   titul ---------->  Texto en graficos que sirve de titulo
!   hora ----------->  Hora para indicar la ejecucion del modelo
!   dia ------------>  Dia (fecha) de ejecución del modelo 
!   fecin,fecin2 --->  Formato de fecha inicial
!   fecfin --------->  Formato de fecha final
!   horin,horin2 --->  Formato para la hora inicial
!   horfin --------->  Formato para la hora final
!   aa,aa1 --------->  Variable para el codigo inicial en el episodio (máximo 2 caracteres)
!   fecemb --------->  Fecha en el fichero de emblases (debe ser 01-01-01 y no 01/01/01)
!   c8 ------------->  Variable caracter de 8 espacios
!   c12 ------------>  Variable caracter de 12 espacios
!   ldirtra -------->  variabile con la longitud del nombre del directorio
!   larch ---------->  variable con la longitud del nombre de los archivos en el filessp
!   larchveg ------->  variable con la longitud del nombre de los archivos en el filesveg
!   path_separator ->  separador en los paths (\ para Windows, / para Linux)
!CHARACTER fecfin*11,horfin*8,fecin2*11,horin2*8,aa1*2,dirtra*128,   & 
!  aa*2,fecin*11,horin*8,nam*50,fecemb*10,c8*8,tit*11(8),c12*12,   &
!  arch*128(42),mensaje*200,archin*24,artem*128,hora*10,dia*8,titul*60,archsed*128(13),  &
 ! archveg*128(10),archnit*128(18),path_separator*
CHARACTER fecfin*11,horfin*8,fecin2*11,horin2*8,aa1*2,dirtra*128,   & 
  aa*2,fecin*11,horin*8,nam*50,fecemb*10,c8*8,c12*12,   &
  mensaje*200,archin*24,artem*128,hora*10,dia*8,titul*60, path_separator*1
character(len=11) tit(8)
character(len=128) arch(42),archsed(13),archveg(10),archnit(18),filedum(12),fileppal(3)
integer ldirtra,larch,larchveg,larchsed
CHARACTER,ALLOCATABLE:: estadohum(:)*128,multievento(:)*128,datainiz(:)*22,orainiz(:)*22
CHARACTER(LEN=99999) :: path
    
!*---------------------------------------------------------------------------------------

!* Codificación de los archivos input y generados por TETIS
!****************************************************************************************
!     ARCH(1)=Paramgeo.txt
!     ARCH(2)=Calib.txt
!     ARCH(3)=TOPOLCO.SDS
!     ARCH(4)=HANTEC.SDS
!     ARCH(5)="FICHERO_DE_ENTRADA.TXT"
!     ARCH(6)=FactorETmes.txt
!     ARCH(7)=CurvasHV.txt
!     ARCH(8)=HANTEC2.SDS
!     ARCH(9)=Fichero_resultados.res
!     ARCH(10)=Nieve.asc
!     ARCH(11)=Nieve2.asc
!     ARCH(12)=mdt.asc
!     ARCH(13)="Hu.asc"
!     ARCH(14)=Ks.asc
!     ARCH(15)=Kp.asc
!     ARCH(16)="usos.asc"
!     ARCH(17)=Slope.asc
!     ARCH(18)=dd.asc
!     ARCH(19)=cda.asc
!     ARCH(20)=Riego.asc
!     ARCH(21)=CONTROL.txt
!     ARCH(22)=Riego.txt
!     ARCH(23)=Recorta.txt
!     ARCH(24)=RegHomog.asc
!     ARCH(25)=PPTAcum.asc
!     ARCH(26)=Var-SCEUA.txt
!     ARCH(27)=Res-SCEUA.txt
!     ARCH(28)=MapaFCs.asc
!     ARCH(29)=OrdenRiego.asc
!     ARCH(30)=Kss.asc
!     ARCH(31)=Ksa.asc
!     ARCH(32)=Kps.asc
!     ARCH(33)=Vel.asc
!     ARCH(34)=Settings.txt
!     ARCH(35)=Rad01.asc
!     ARCH(36)=karst.asc
!     ARCH(37)=manantiales.txt
!     ARCH(38)=TipoRiego.txt
!     ARCH(39)=Acuiferos.asc
!     ARCH(40)=Factvegcultivos.txt
!     ARCH(41)=CalibVeg.txt
!     ARCH(42)=hstar.asc    !Sustituye al mapa cero.asc de la antigua vegetación dinámica, que estaba en fiseveg.txt que ahora sólo existe cuando la nueva vegetación dinámica está activada

!     archveg(1)=hstar.asc                   
!     archveg(2)=dc.asc                      
!     archveg(3)=rs.asc                      
!     archveg(4)=vegantec.sds                
!     archveg(5)=ins.asc                     

!     archsed(1)=cusle.asc                   
!     archsed(2)=kusle.asc                   
!     archsed(3)=pusle.asc                   
!     archsed(4)=sand.asc                    
!     archsed(5)=silt.asc                    
!     archsed(6)=clay.asc                    
!     archsed(7)=sedantec.sds                
!     archsed(8)=sedfinal.sds                
!     archsed(9)=series_sed.txt              
!     archsed(10)=azudes.txt                 
!     archsed(11)=curvasHVsed.txt            
!     archsed(12)=estado_embalses_ini.txt    
!     archsed(13)=estado_embalses_fin.txt    

!     archnit(1)=hlim.asc
!     archnit(2)=psuelo_efectiva.asc
!     archnit(3)=Dens_Aparente.asc
!     archnit(4)=kd.asc
!     archnit(5)=NO.asc
!     archnit(6)=depNH4.asc
!     archnit(7)=depNO3.asc
!     archnit(8)=CalibNit.txt
!     archnit(9)=Fcubiertan.txt
!     archnit(10)=NH4inputsuelo.txt
!     archnit(11)=NO3inputsuelo.txt
!     archnit(12)=Resultados_nitrógeno_CR2_80C.txt
!     archnit(13)=NANTEC.SDS
!     archnit(14)=NANTEC2.SDS
!     archnit(15)=CodCultivos.txt
!     archnit(16)=CarCultivos.txt
!     archnit(17)=NH4inputsueloCultivos.txt
!     archnit(18)=NO3inputsueloCultivos.txt



!---------------------------------------------------------------------------------------

!* Variables relacionadas con la PPT, nieve y los resultados
!***************************************************************************************
!* DINAMICAS LOGICAS
!    band_p() --------->  Si es .T. es que existe valor y se puede interpolar
!    band_e() --------->  Si es .F. es que no existe valor (-1) y NO se puede interpolar
!    band_t() --------->  Si es .T. es que existe valor y se puede interpolar
!    band_r() --------->  Si es .T. es que existe valor y se puede interpolar
!* DINAMICAS ENTERAS
!    pulm() ------------->  Si es 1 entonces se aplica Puls Modificado en embalses
!    matrix(,),masc(,) -->  Matriz auxiliar para graficar o 
!* DINAMICAS REALES
!   preac(,) -------->  Lluvia acumulada (nt,nest)
!   acuniv() -------->  Total acumulado de nieve
!   balanc(,) ------->  Valores medios para toda la cuenca (Balance)
!   disn()  --------->  Distancia entre la celda y las estaciones 
!   serie() --------->  Resultados ordenados de las series para la impresion final
!   estad(,) -------->  Descrpción de los principales estadisticos
!        (,1)  - Qpico observado           - Qpico simulado
!        (,2)  - Tpico observado           - Tpico simulado
!        (,3)  - Qmedio observado para el indice de Nash - (Qobs-Qmed)^2
!        (,4)  -                           - RMSE
!        (,5)  - Factor de ponderación (lambda=2.0) -  % Error Volumen
!        (,6)  -                           - Indice de Nash
!        (,7)  - Volumen observado (Hm³)   - Volumen simulado (Hm³)
!        (,8)  - (Qsim-Qmed)^2  - HMLE
!        (,9)  - Qmedio simulado para AMLE - Coef. de eficiencia (Nash sin cuadrado)
!        (,10) - RMSE mensual
!        (,11) - (Qobs-Qm)*(Qsim-Qm) Coef correlac. de errores (alfa) - 
!        (,12) - Error medio para LGA (mu) -
LOGICAL, ALLOCATABLE::band_p(:),band_e(:),band_t(:),band_r(:)
INTEGER,ALLOCATABLE::pulm(:),matrix(:,:),masc(:,:)
REAL,ALLOCATABLE::preac(:,:),disn(:),acuniv(:),balanc(:,:),serie(:,:),serie2(:,:),estad(:,:),mascreal(:,:),& 
balanc_sed(:,:),balancqb(:),balancexp(:),estadveg(:,:),RSRindex(:),RSRindexveg(:),RSRindexsed(:),RSRindexnitr(:)
!*---------------------------------------------------------------------------------------

!* Variables relacionadas con el riego y los resultados
!***************************************************************************************
!   periodRiego(,) ------>  Periodo de riego. Dias que pasan para que una celda vuelva a ser regada (Cada cuantos dias se riega...)
!   xriego(,) ------>  Cantidad de riego, valores mensuales (mm)
!   banriego() -------->  Cuenta el numero de celdas de una misma zona de riego (erie)
!   contriego() ------->  Contador de celdas han regado (para escala no diaria, cada celda solo se puede contar una vez por dia), es comparado con el orden de celda
!   contRiegoInDay() ------->  Contador de celdas regadas cada dia
!   tiporiego() ------->  Tipo de riego (se define uno por zona de riego). 1 Gravedad, 2 Aspersión, 3 Goteo
!   grupoOrdenRiego() ----> Cada zona se subdivide en grupos. En este vector se establece el grupo actual que se riega en cada zona
!   celdasRegadas()  ---> celdas actualmente regadas, antes del presente grupo
!   isZonaConsiderada()   ----> especifica si la zona ya se ha considerado para el calculo inicial en riego( 0 - NO ; 1 - SI). Cada zona solo puede entrar una vez a ese calculo
!   isZonaRegada()   ----> especifica si la zona ha sigo regada ese dia ( 0 - NO ; 1 - SI)
!   contarPasoTiempoRegado() --->   especifica si el paso de tiempo se ha regado ( 0 - NO ; 1 - SI)
!   pasoTiempoRegados()   -> pasos de tiempo que se riega durante un dia
REAL,ALLOCATABLE::xriego(:,:),periodRiego(:,:) !(Vicente)Las convierto en ALLOCATABLES
INTEGER,ALLOCATABLE::banriego(:),contRiegoInDay(:),contriego(:),nstep(:),ntaccum(:),tiporiego(:),grupoOrdenRiego(:),celdasRegadas(:),isZonaConsiderada(:),isZonaRegada(:),contarPasoTiempoRegado(:),pasoTiempoRegados(:)
!*---------------------------------------------------------------------------------------

!actacuifero ----------> Si vale 1 se está modelizando el el acuífero como conectado y no conectado. Necesario para la escritura del fichero de salida, ya que no se ha hecho como un módulo. (cris 06/2017)
!Integer actacuifero

!* Variables DERIVADAS para cada CELDA, No se incluyen las series temporales
!****************************************************************************************
!* ENTEROS
!   fil, col ------> Ubicación i,j en un malla rectangular de la celda actual
!   dest  ---------> Ubicación de la Celda destino en un fila (a la que drena aguas abajo)
!   acum  ---------> Nº de celdas acumuladas (no se incluye la celda misma)
!   cota  ---------> Elevación o cota de la celda (m.s.n.m)
!   codnie --------> Código 0 (NO) - 1 (SI) que indica presencia de nieve
!   codpar --------> Valor en el mapa de regiones homogeneas (parámetros geomorfológicos)
!   codveg --------> Valor de índice de vegetación para calcular la Evapotranspiración
!   codrie --------> Valor de índice de riego para determinar la cantidad a regar
!   ordrie --------> Orden de regadío para establecer una secuencia de riegos en la zona
!   codcal --------> Valor en el mapa de zonas con FC homegeneos (Falta por implementar!!!)
!   orient --------> Orientación de la ladera
!   acuif ---------> Acuífero al que recarga. Si no existe el mapa, toma valor 1.
!* ENTERO DINÁMICOS
!   ind_int_p(:) --> Vector de índicadores (orden) de estaciones PPT cercanas a interpolar
!   ind_int_e(:) --> Vector de índicadores (orden) de estaciones ETP cercanas a interpolar
!   ind_int_t(:) --> Vector de índicadores (orden) de estaciones T cercanas a interpolar
!   ind_int_r(:) --> Vector de índicadores (orden) de estaciones R cercanas a interpolar
!* REALES
!   pend  ---------> Pendiente de la celda en (%)*1000
!   hu,ks, kp -----> Parámetros del suelo en la celda (tomados de los mapas)
!   kss,ksa,kps ---> Parámetros del suelo en la celda (tomados de los mapas)
!   x(0:10) -------> Flujos verticales en (mm)
!   y(0:6) --------> Flujos horizontales en (mm)
!   h(0:6) --------> Niveles en los tanques (mm)
!   u(8) ----------> Valores de humax,ks,kp,kpp
!   lon -----------> Longitud de la celda (m)
!   vel -----------> Velocidad del flujo en la celda (m/s)
!   q -------------> Caudal de salida en la celda (m³/s)
!* REALES DINÁMICOS
!   fac_int_p(:) --> Factor de interpolación de estaciones PPT cercanas a interpolar
!   fac_int_e(:) --> Factor de interpolación de estaciones ETP cercanas a interpolar
!   fac_int_t(:) --> Factor de interpolación de estaciones T cercanas a interpolar
!   xascii(7),yascii(7),hascii(7) --> Matrices donde se guardan los vlaores de las variables de estado para crear los mapas ASCII
TYPE celdas
  INTEGER acum,dest,fil,col,orient,cota,codnie,codpar,codveg,codrie,codcal,ordrie,codkarst,acuif,Riegosmes
  INTEGER,ALLOCATABLE:: ind_int_p(:),ind_int_e(:),ind_int_t(:),ind_int_r(:)
  REAL:: x(0:10),y(0:6),h(0:8),u(8),lon,vel,q,hu,ks,kp,pend,kss,ksa,kps,veloc,qb=0,d(0:8),h3max,exfiltr,xascii(0:10),& 
  yascii(0:6),hascii(0:8),SusLADascii(0:3),SusREDascii(0:3),DepLADascii(0:3),DepREDascii(0:3),ErodSedascii(0:3),& 
  ErodTotalascii,SedFlujoascii(0:3),SedFlujoTotalascii
  REAL laiascii,lairascii,imaxascii,eiascii,trascii,tr1ascii,tr2ascii,esascii,xvegascii
  REAL,ALLOCATABLE:: fac_int_p(:),fac_int_e(:),fac_int_t(:),fac_int_r(:)
! Variables tipo celda para TETIS-SED
  REAL Cusle,Kusle,Pusle,porcentaje(3),   &
  SusSedLAD(3),DepSedLAD(3),SusSedRED(3),DepSedRED(3),ErodSed(3), &
  t_SedFlujo,SedFlujo(3),Erodtot,DeptotLAD,DeptotRED, &
  t_SusSedLAD,t_SusSedRED,t_DepSedLAD,t_DepSedRED,t_NetEros,cotacorr,SedConcLAD(3),t_SedConcLAD,SedConcRED(3),t_SedConcRED, &
  velladera,velcarcava,velcanal,hladera,hcarcava,hcanal,qladera,qcarcava,qcanal,fuente(3)
! Variables tipo celda para TETIS-VEG
! FLUJOS VERTICALES
! cell(n).x(1)----> aportes (precipitación, nieve, riego) (NOMBRE DE LA VARIABLE CONSERVADO)
! cell(n).x(6)----> flujo excedente no interceptado por la vegetación (Pg)
! cell(n).x(7)----> Aporte de riego
! cell(n).x(2)----> excedente para percolacón y escorrentá superficial (NOMBRE DE LA VARIABLE CONSERVADO)
! cell(n).x(3)----> Infiltración
! cell(n).x(4)----> Percolación
! cell(n).x(5)----> Pérdidas subterráneas
! cell(n).x(9)----> Péridas subterráneas desde celda de acuífero conectado
! cell(n).x(10) --> Péridas subterráneas desde celda de acuífero no conectado  
! FLUJOS HORIZONTALES
! cell(n).y(6)----> flujo interceptado por la vegetación y los charcos (DI)
! cell(n).y(2)----> flujo almacenado en la zona capilar del suelo (DH)
! SALIDAS DEL SISTEMA
! cell(n).ei -----> evaporación directa
! cell(n).es -----> evaporación suelo desnudo
! cell(n).tr -----> transpiración
! cell(n).evptot -> evapotranspiración total (NOMBRE DE LA VARIABLE CONSERVADO)
! ESTADOS DE ALMACENAMIENTO
! cell(n).h(6)----> almacenamiento del tanque "intercepción" (I)
! cell(n).h(1)----> almacenamiento del tanque "capacidad capilar" (H)
! OTRAS VARIABLES 
!   hstar --------> Optimum available water-soil content
!   tmx -----------> Maximum transpiration rate 
!   kleaf ---------> Leaf Shedding 
!   imx -----------> Maximum interception 
!   alfa ----------> Maximum net assimilation carbon / Potential leaf biomass 
!   rveg ----------> Evolución de la vegetación
!   PET -----------> Evapotranspiración potencial (mm/dia)
!   waterstress ---> Water stress 
!   rs ------------> zss/ze
!   dc ------------> almacenamiento en charcos (mm)
!   qveg ------------------> Exponente ecuación de cálculo del water stress
!   rad(6) --------> Factores de radiación
!GUIOMAR (23/01/2014) Variables tipo celda que se utilizan en el nuevo modelo de vegetación
!cell(n).lai----------> El valor del LAI en cada celda
!cell(n).imax---------> El valor de la intercepción máxima en cada celda que con el nuevo modelo es variable en el tiempo
!cell(n).ei-----------> Evaporación de la intercepción (ya estaba en el modelo anterior)
!cell(n).d(8)---------> Hay que añadir un término más a las d(:) para tener en cuenta la entrada al tanque estático más superficial
!cell(n).h(8)---------> idem pero con los almacenamientos
!cell(n).x(8)---------> idem pero para el excedente del tanque estático más superficial
!cell(n).hlim1--------> Punto de marchitez en mm para el tanque estático más superficial (1)
!cell(n).hstar1-------> Punto óptimo (o crítico) en mm para el tanque estático más superficial (1)
!cell(n).hlim2--------> Punto de marchitez en mm para el tanque estático 2
!cell(n).hstar2-------> Punto ótpimo (o crítico) en mm para el tanque estático 2
!cell(n).tr1----------> transpiración desde primer tanque estático
!cell(n).tr2----------> transpiración desde segundo tanque estático
!cell(n).tr-----------> transpiración total
!cell(n).es-----------> evaporación suelo desnudo (ya estaba en el modelo anterior)
!cell(n).pg-----------> crecimiento biomasa sin considerar ni respiración ni muerte
!cell(n).resp---------> respiración de la planta
!cell(n).xveg---------> Biomasa
!cell(n).muerte-------> muerte de la biomasa
!cell(n).dxveg--------> variación de la vegetación
!cell(n).fialloc------> término de distribución de la biomasa (allocation)
!cell(n).lair---------> transformación del LAI para que tenga en cuenta el estrés hídrico y poder comparar con el NDVI
!cell(n).fc-----------> fracción de cobertura (entre 0 y 1)
!cell(n).hu1----------> cada uno de los dos tanques estáticos tienen un valor diferente de hu
!cell(n).hu2----------> Idem
!cell(n).z1-----------> profundidad de la primera capa
!cell(n).z2-----------> profundidad de la segunda capa
!GUIOMAR (27/01/2014) Variables del modelo de vegetación que no están incluidos en cell
!qdispo-------> Exponente utilizado en la función de disponibilidad de agua. Lo he puesto variable en cada celda para poder hacer distinción entre especies ahorradoras o no ahorradoras
!qestres------> Exponente utilizdo en la función de estrés hídrico de la planta. Lo he puesto variable para poder distinguir entre especies vegetales
!alm_max------> Variable que se utiliza para calcular el imax
!raices-------> % de raíces en la parte superior 
!kdecay-------> constante de extinción lumínica que interviene en el cálculo del FPAR
!topt---------> temperatura de crecimiento óptimo de la planta
!lue----------> light use efficiency 
!rresp--------> tasa de respiración de la planta
!kmuerte------> constante de muerte
!sla----------> SLA: Specific Leaf Area
!laimax-------> máximo LAI alcanzable en el sistema
  REAL ei,es,tr,evptot,hstar,tmx,kleaf1,kleaf2,kleaf3,kleaf4,kleaf5,imx,alfa,&
  rveg,PET,waterstress,cveg,rs,dc,qveg,rad(6)
  !Añadido por Guiomar (23/01/2014)
  REAL lai,imax,hlim1,hstar1,hlim2,hstar2,tr1,tr2,pg,resp,xveg,muerte,dxveg,fialloc,lair,fc,hu1,hu2,z1,z2
  
!festres_ult10----> suma de la función de estrés hídrico en la planta combinado en los ultimos 10 dias
!festres_medUlt10-> media de la función de estrés hídrico en la planta combinado en los ultimos 10 dias
!func_estres_t(:)-> array que guardar la funcion de estres hidrico en cada celda de los ultimos 10 dias
REAL festres_ult10,festres_medUlt10
!REAL,ALLOCATABLE:: func_estres_t(:)
REAL:: func_estres_t(0:10)
!Variables tipo celda del submodelo de nitrógeno, explicación al final del módulo. Cris(03/2017)
Real psuelo,hlim,tempsuelo,hn(0:12),fts,ftc,fh1,fh2,fh3,sat,fn(0:46),daparente,kd
REAL uptEff, recEff, allInput, allPercolation, allUptake
Real depositacion,qdepositado,qdepositadoar,qsuspendido,qsuspendidoar,voldepini,volsusini
Real depositacionred,qdepositadored,qdepositadoarred,qsuspendidored,qsuspendidoarred,voldepinired,volsusinired
Real qlNO,qlNH4,qlNO3,qsNO,qsNH4,ConcqlNO,ConcqlNH4,ConcqlNO3,ConcqsNO,ConcqsNH4
Real fnascii(0:46),hnascii(0:12)
Real hn0ini !Variable para considerar el estado inicial de NO como mapa, se lee en topolco
Real depamonio,depnitrato
!Variables tipo celda del submodelo de nitrógeno con cultivos, explicación al final del módulo. Cris(05/2017)
Real w,nw,Ncrit,Nextr,fcncult,restcosecha

END TYPE celdas
!Añadido por Guiomar (27/01/2014)
REAL qdispo,qestres,alm_max,raices,kdecay,topt,lue,rresp,kmuerte,sla,laimax
!*---------------------------------------------------------------------------------------


!* Variables que se emplean en el algoritmo de optimización según el SCE-UA
!****************************************************************************************
!* ENTEROS 
!    nparam -------> Numero de parámetros a optimizar
!    mvar,pvar,svar,pmin,qvar ---------> Variables m,p,s y q del algoritmo SCE-UA
!    betafo y alfafo ------------------> Variables alfa y beta del algoritmo CCE-UA
!    isce,jsce,ksce,iecc,kecc,jecc ----> Contadores de ciclo
!    idfo ---------> Identificador del tipo de Funcion Objetivo (va de 1 a 6?? segun caso)
!    idpon --------> Identificador si se desea ponderar por el área 
!    folon --------> longitud a emplear en la F.O. RMSE mensual (se recomienda 30-31)
!    nifo ---------> Inicio de la F.O. (puede empezar en cualquier valor de 1 a nt)
!* REAL
!    fvalue -------> Valor de la funcion objetivo
!* DINAMICAS LOGICAS
!    xchk() -------> Para la seleccion de los parámetros, se incluye si es .T. (16)
!* DINAMICAS REALES
!    xlb(),xub() --> Límite inferior y superior de los parámetros a optimizar (16)
!    xguess() -----> Valor inicial de los parámetros a optimizar (16)
!    dvar() -------> Valores aleatorios iniciales y su FO (961,16)
!    xpar() -------> Valores utilizados en la optimizacion, FO y otros estadisticos
!                     (29989,215), esta variable es la que se escribe en la salida
!    humr() -------> Estado de humedad inicial (5)
INTEGER mvar,pvar,pmin,svar,qvar,isce,jsce,ksce,nparam,idfo,idpon,mesin0,  &
        cont,betafo,alfafo,ib,iecc,jecc,kecc,ban,ia,cont2,folon,unidad,nifo
LOGICAL, ALLOCATABLE::xchk(:)
REAL fvalue,laiini(50)
!Real,Allocatable:: laiini(:) !Cris (17/10/2016) Regiones homogéneas allocatable, ahora fijo a 50.
REAL,ALLOCATABLE::vmax(:,:),dvar(:,:),xpar(:,:),xguess(:),xlb(:),xub(:),humr(:),vmaxsed(:,:)
!*---------------------------------------------------------------------------------------
!* Variables que se emplean en el algoritmo de optimización de Qmax según el SCE-UA
!****************************************************************************************

REAL qMaxObj, xguess_qMax,xlb_qMax,xub_qMax,humr_qMax, qMax_obs, tPico_obs, qMax_sim, tPico_sim, fobj_qMax
CHARACTER estObj_qMax*25
REAL fvalue_qMax
INTEGER nparam_qMax,mvar_qMax,pvar_qMax,pmin_qMax,svar_qMax,qvar_qMax,nifo_qMax,pos_estMax
REAL,ALLOCATABLE::vmax_qMax(:,:),dvar_qMax(:,:),xpar_qMax(:,:),qObs_estMax(:)

!*---------------------------------------------------------------------------------------
!* Variables que se emplean en el algoritmo de optimización de Hidrologia y Vegetacion según el SCE-UA
!****************************************************************************************

INTEGER isce_veg,jsce_veg
LOGICAL, ALLOCATABLE::xchk_veg(:,:)
REAL,ALLOCATABLE::vmax_veg(:,:),dvar_veg(:,:),xpar_veg(:,:),xguess_veg(:,:),xlb_veg(:,:),xub_veg(:,:)

!*---------------------------------------------------------------------------------------
!* Variables que se emplean en el algoritmo de optimización de Nitrogeno según el SCE-UA
!****************************************************************************************

INTEGER isce_nitr,jsce_nitr
REAL,ALLOCATABLE::xguess_kmin(:),xguess_kinm(:),xguess_kvol(:),xguess_knit(:),xguess_kfi(:),xguess_kdes(:)
REAL,ALLOCATABLE::xguess_F(:),xguess_Ndem(:),xguess_PrefNO3(:)
REAL,ALLOCATABLE::xlb_kmin(:),xlb_kinm(:),xlb_kvol(:),xlb_knit(:),xlb_kfi(:),xlb_kdes(:)
REAL,ALLOCATABLE::xlb_F(:),xlb_Ndem(:),xlb_PrefNO3(:)
REAL,ALLOCATABLE::xub_kmin(:),xub_kinm(:),xub_kvol(:),xub_knit(:),xub_kfi(:),xub_kdes(:)
REAL,ALLOCATABLE::xub_F(:),xub_Ndem(:),xub_PrefNO3(:)
LOGICAL, ALLOCATABLE::xchk_kmin(:),xchk_kinm(:),xchk_kvol(:),xchk_knit(:),xchk_kfi(:),xchk_kdes(:)
LOGICAL, ALLOCATABLE::xchk_F(:),xchk_Ndem(:),xchk_PrefNO3(:)
REAL xguess_kminc,xguess_knitc,xguess_kdesc,xguess_mtd,xguess_tethas,xguess_topts,xguess_tethac,xguess_toptc,xguess_fckd
REAL xlb_kminc,xlb_knitc,xlb_kdesc,xlb_mtd,xlb_tethas,xlb_topts,xlb_tethac,xlb_toptc,xlb_fckd
REAL xub_kminc,xub_knitc,xub_kdesc,xub_mtd,xub_tethas,xub_topts,xub_tethac,xub_toptc,xub_fckd
LOGICAL xchk_kminc,xchk_knitc,xchk_kdesc,xchk_mtd,xchk_tethas,xchk_topts,xchk_tethac,xchk_toptc,xchk_fckd




!*---------------------------------------------------------------------------------------

! Variables para la parametrización (Menu de Generación de Parámetros del Modelo)
!****************************************************************************************
!   fileppal() --->  Nombre de los ficheros con los mapas de variables principales
!   filedum() ---->  Nombre de los ficheros con variables ambientales o DUMMY
!   dirdum ------->  Nombre del directorio con resultados de las parametros del suelo
!   chkdum()  ---->  Variable lógica que indica .T. si se incluye el fichero en el análisis
!   chkcon() ----->  .T. indica que se incluye la variable continua en el análisis
!   umbdum()  ---->  Valor REAL del umbral para definir la variable DUMMY
LOGICAL chkdum(12),chkcon(4)
!CHARACTER filedum*128(12),fileppal*128(3),
CHARACTER dirdum*128
REAL umbdum(12)
!*---------------------------------------------------------------------------------------


!!* Variables para SIMULACION CONTINUA CON CAMBIO DE DT
!!****************************************************************************************
!!   ndtmin ----------> nuevo dt
!!   filesnewdt ------> archivo con los parámetros de calibración
!!   nnt -------------> nevo numero de pasos temporales
!INTEGER ndtmin,ndt,ndts,nnt
!CHARACTER filenewdt*128
!INTEGER,ALLOCATABLE :: eventos(:,:)
!REAL,ALLOCATABLE :: eventos2(:,:)

!* Variables para SEDIMENTOS, TETIS-SED
!****************************************************************************************
!   EHcoef ----------> Factor corrector de la formula de Engelund - Hansen (Sedimentos)
!   rsed() ---------->  Factores Correctores del modelo (a calibrar) - SEDIMENTOS
!   estadsed(,) ----->  Descrpción de los principales estadisticos
!        (,1)  - Qpico observado           - Qpico simulado
!        (,2)  - Tpico observado           - Tpico simulado
!        (,3)  - Qmedio observado para el indice de Nash - (Qobs-Qmed)^2
!        (,4)  -                           - RMSE
!        (,5)  - Factor de ponderación (lambda=2.0) -  % Error Volumen
!        (,6)  -                           - Indice de Nash
!        (,7)  - Volumen observado (Hm³)   - Volumen simulado (Hm³)
!        (,8)  - (Qsim-Qmed)^2  - HMLE
!        (,9)  - Qmedio simulado para AMLE - Coef. de eficiencia (Nash sin cuadrado)
!        (,10) - RMSE mensual
!        (,11) - (Qobs-Qm)*(Qsim-Qm) Coef correlac. de errores (alfa) - 
!        (,12) - Error medio para LGA (mu) -
REAL qsSUS(3),ADVcapacidadLADERA(3),qsBM(3),KRcapacidad(3),qsEROS(3),qs(3), &
  qsSUStot,qsBMtot,qsEROStot,SUStot,DEPtot,supply, &
  qsKR,KRcoef,EHcoef,CaudalUnitario,EXCcapacidadLAD,RESIDcapacidadLAD, &
  Qb,Qbs,wsurcosl,wchan,Rhsurco,Achan,Asurco,wqs,wq,ychan,Rh,Concw(3),  &
  Ge,gravedad,diamsed(3),EHcapacidad(3),adv_factor,  &
  SUSvol(3),EXCcapacidadCANAL(3),BMvol(3),hagua,ws(3),PorcentDep(3),  &
  Qbc,wcarcavasl,Acarcava,wqc,Rhcarcava,  &
  depositacion(3),maxabsSus,maxabsConSus,voltransf,  &
  tot_ERODADO(3),sed_SALIDA(3),totscourv,sus_TOTAL(3),dep_TOTAL(3),sed_TOTAL(3), &
  tot_EROSION,tot_SUSREM,tot_DEPREM,tot_REM,  &
  tot_SEDSALIDA,error_porcent_SED,contemp,DepSedIni(3),rsed(3),volSIM,SusSedIni(3),ErodSedIni(3),areaumbral(3,50)
REAL, ALLOCATABLE :: estadsed(:,:)
!REAL, ALLOCATABLE :: areaumbral(:,:) !Cris (17/10/2016) Regiones homogéneas allocatable, ahora fijo a 50.
LOGICAL check(3,50)

! Fin de Variables para TETIS-SED
!*---------------------------------------------------------------------------------------

!* Variables para SEDIMENTOS, eficiencia de retención
!****************************************************************************************
!   trapeff -------> Variable lógica de activación del modulo
!   numpresas -----> Numero de presas en el fichero azudes
!   dam -----------> Variable tipo type que describe un azud. Incluye:
!     Pnombre -----> Nombre de la presa
!     fila,col ----> Fila y columna correspondientes a la presa
!     pos ---------> Numero de la celda donde está la presa
!     UTMx,UTMy ---> Coordenadas
!     Paltura -----> Altura de la presa
!     Pancho ------> Dimensión trasversal al cauce de la presa
!     Dlong -------> Longitud del embalse
!     Dancho ------> ancho del embalse
!     Pend --------> pendiente local
!   TOT_presas ----> Volumen total (para cerrar el balance)
LOGICAL trapeff
INTEGER numpresas,nsect
REAL TOT_presas
TYPE presas
  CHARACTER*128 Pnombre
  INTEGER UTMx,UTMy,fila,col,pos,datos
  REAL Paltura,Pancho,Dlong,Dancho,pend,PalturaNew
  REAL volumen,hw,dBD,dep_tot
  REAL, ALLOCATABLE :: lsector(:),hsector(:),csector(:,:),vsector(:),h(:),vol(:),qinTE(:),sedinTE(:,:),& 
  sedoutTE(:,:),depthTE(:),depositTE(:)
  REAL :: sus_tot
END TYPE presas
TYPE (presas), ALLOCATABLE:: dam(:)
! Fin de Variables para SEDIMENTOS, eficiencia de retención
!*---------------------------------------------------------------------------------------

!* Variables para VEGETACION, TETIS-SEDVEG
!*      
!****************************************************************************************
!   sand --------->  Valor usado en la subrutina param_clapp para el % de arena
!   clay --------->  Valor usado en la subrutina param_clapp para el % de arcilla
!   veget -------->  Matriz para escribir las variables vegetacion en un archivo 
REAL sand, clay, ol(12,9)
REAL, ALLOCATABLE :: veget(:,:)
REAL, ALLOCATABLE :: try01(:,:)
INTEGER cantUsosSuelosVeg

!* Variables DERIVADAS para cada ESTACION (PPT,ETP,Q,T,etc). 
!*      Se incluyen las series temporales
!****************************************************************************************
!* CARACTER
!   Name,codigo  ----->  Nombre y código de la estación 
!* ENTEROS
!   fila, columna ---->  Posición i,j de la estación en la malla rectangular(nueva)
!   pos  ------------->  Localización en el vector de topología
!* REAL
!   utmx,utmy,elev --->  Coodenadas UTMx,UTMy de la celda y su elevación (m.s.n.m.)
!   area  ------------>  Área acumulada hacia aguas arriba
!   coef  ------------>  Sirve para los manantiales en el caso de tener el karst activado
!* Variables DINAMICAS REALES
!   Obs()  ----------->  Serie observada en la estación
!   Sim()  ----------->  Serie simulada en la estación
!   Bal()  ----------->  Serie estimada por balance en la estación
!   sed_out(,) ------->  Serie de flujo de sedimentos en celdas de aforo
!   sed_temp(,) ------>  Serie temporales de variables importantes
!   inst() ----------->  Serie de caudal de salida instantánea por pulso modificado (Juan Camilo 2008)
!****************************************************************************************
TYPE estacion
  CHARACTER codigo*2,name*25
  REAL  utmx,utmy,elev,area,coef
  INTEGER fila,columna,pos
  REAL, ALLOCATABLE:: sim(:),bal(:),sed_out(:,:),sed_temp(:,:),inst(:)
  REAL, DIMENSION(:), ALLOCATABLE :: obs
END TYPE estacion
!*---------------------------------------------------------------------------------------


!* Variables DERIVADAS para  EMBALSE.  Curvas características del embalse
!****************************************************************************************
!* CARACTER
!*   Nombre  --------->  Nombre del embalse
!* ENTEROS
!*   datos  ---------->  Numero de datos de la curva característica
!*   caso  ----------->  Tipo de información para el embalse (1 a 8)
!*                         -1- Dispone de información de N
!*                         -2- Dispone de información de V
!*                         -3- Dispone de información de S
!*                         -4- Dispone de información de N,V
!*                         -5- Dispone de información de N,S
!*                         -6- Dispone de información de V,S
!*                         -7- Dispone de información de N,V,S
!*                         -8- No se dispone de información de N,V,S (pero existe N)
!*                              es el caso del Puls Modificado  
!*   posn,poss,posv -->  Posición ???
!*   pos  ------------>  Localización en el vector de topología
!* Variables DINAMICAS REALES
!*   h() ------------->  Elevacion (m)
!*   vol() ----------->  Volumen (m³)
!*   sup() ----------->  Superficie  (m²)
!*   out(,) ---------->  Caudal máximo y mínimo de salida del embalse 
!*                       (compuertas abiertas y cerradas) para Puls Modificado
!*   fpul() ---------->  Función pulso modificado construida con los puntos de la curva de embalse (compuertas abiertas) (Juan Camilo 2008)
TYPE embalse
  CHARACTER nombre*128
  INTEGER datos,caso,pos
  REAL,ALLOCATABLE:: h(:),vol(:),sup(:),qmin(:),qmax(:),out(:,:),fpul(:)
END TYPE embalse
!*---------------------------------------------------------------------------------------

TYPE (celdas),ALLOCATABLE:: cell(:)
TYPE (estacion),ALLOCATABLE:: control(:),pluvio(:),aforo(:),evapo(:),temper(:),aforosed(:),manantial(:),& 
veg(:),radiacion(:),tr_point(:),veg1_point(:),veg2_point(:)! (Vicente-Guiomar)
TYPE (estacion),ALLOCATABLE:: nivel(:),volum(:),qemb(:),nieve(:),otros(:),aport(:)
TYPE (estacion), ALLOCATABLE:: aportsed1(:),aportsed2(:),aportsed3(:) !Cris (11/2015)
TYPE (embalse),ALLOCATABLE:: emb(:)
!TYPE (estacion),ALLOCATABLE:: controln(:),pluvion(:),aforon(:),evapon(:),tempern(:),aforosedn(:)
!TYPE (estacion),ALLOCATABLE:: niveln(:),volumn(:),qembn(:),nieven(:),otrosn(:),aportn(:)
!TYPE (embalse),ALLOCATABLE:: embn(:)


!Variables para pruebas
REAL temp001, temp002, temp003, temp004, temp005, temp006, temp007
REAL, ALLOCATABLE :: temp010(:)

!Mensajes
CHARACTER strings(1000)*256,almac(1000)*256

!!* Variables para VEGETACION, TETIS-SEDVEG
!*  GUIOMAR (23/01/2014)    
!****************************************************************************************
!functeta1--------> función de disponibilidad de agua para transpiración en el primer tanque estático 1
!functeta2--------> función de disponibilidad de agua para transpiración en el segundo tanque estático 2
!functetabs-------> función de disponibilidad de agua para la evaporación directa del suelo desnudo
!par--------------> PAR: Photosynthetically Active Radiation
!fpar-------------> fracción de la PAR absorvida por la vegetación
!funcestres1------> función de estrés hídrico en la planta con respecto al primer tanque estático
!funcestres2------> función de estrés hídrico en la planta con respecto al segundo tanque estático
!funcestres-------> función de estrés hídrico en la planta combinado
!testres----------> coeficiente de estrés por temperatura
!gtemp------------> función de la temperatura empleada posteriormente en el cálculo de la respiración
REAL functeta1,functeta2,functetabs,par,fpar,funcestres1,funcestres2,funcestres,testres,gtemp,radiacion_solar

!*************************************************************************************************************
!***************************Variables y parámetros para TETIS nitrógeno (03/2017)*****************************
!*************************************************************************************************************
!!Parámetros
!kmin -------> Constante de mineralización (días-1)
!kinm -------> Constante de inmovilización (días-1)
!knit -------> Constante de nitrificación (días-1)
!kfi --------> Constante de fijación de NO3 (KgNO3/hadía)
!kdes -------> Constante de desnitrificación (días-1)
!F ----------> Coeficiente de difusión (m/día)
!Ndem -------> Demanda potencial anual de nitrógeno (kgN/ha)
!PrefNO3 ----> Preferencia de NO3 frente a NH4 (adim) La de NH4 se calcula como (1-prefNO3)
!kd ---------> Coeficiente de partición o de distribución, gobierna el proceso de adsorción/desorción (dm3/kg) Es un mapa.
!Knitc ------> Constante de nitrificación en cauce (días-1)
!kdesc ------> Constante de desnitrificación en cauce (días-1)
!Son vectores porque hay una constante por cada uso del suelo.
!En el caso de nitrificación desnitrificación, habrá una columna más que corresponderá a cauce
!kd es un mapa y está definida en variables tipo celda.
Real,allocatable:: kmin(:),kinm(:),knit(:),kfi(:),kdes(:),F(:),Ndem(:),PrefNO3(:),kvol(:)
Real kminc,kdesc,knitc
!Variables auxiliares. Se guardan los parámetros de entrada, previa transformacion
Real,allocatable:: kmin2(:),kinm2(:),knit2(:),kfi2(:),kdes2(:),F2(:),Ndem2(:),kvol2(:)
Real kminc2,kdesc2,knitc2


!Otros parámetros
!fcn --------> Factor de cubierta para nitrógeno. Va asociado a los usos del suelo y es variable mes a mes (adimensional)
!mtd --------> Máxima diferencia de temperatura netre verano e invierno
!tetha ------> Parámetro de la formulación de corrección por temperatura (s, suelo. c, cauce)
!topt -------> Temperatura óptima de la formulación de correción por temperatura (s, suelo. c, cauce)
!fckd -------> Factor corrector mapa de kd
Real mtd,tethas,topts,tethac,toptc,fckd
Real,allocatable:: fcn(:,:)
!psuelo -----> Profundidad del suelo (m)
!hlim -------> Contenido de agua en el suelo correspondiente al punto de marchitez (mm)
!daparente --> Densidad aparente del suelo en (g/cm3)
!h0ini ------> Contenido de nitrógeno orgánico (kg)
!depamonio --> Deposición atmosférica en forma de amonio (kgN/año, se trasnforma a kgN/día en topolco)
!depnitrato -> Deposición atmosférica en forma de nitrato (kgN/año, se trasnforma a kgN/día en topolco)
!psuelo y hlim, son mapas que habrá que introducir a TETIS. Son necesarios para calcular la concentración en agua y suelo. Definidas en variables tipo celda.
!Para vegetación dinámica ya se usa hlim, pero hay dos: hlim1 y hlim2, uno para cada capa de suelo. 

!Variables de estado
!hn(0:12) ----> Tanques de nitrógeno
!hn0 --------> Nitrógeno orgánico en fase sólida en suelo (kg)
!hn1 --------> Amonio disuelto en suelo (kg)
!hn2 --------> Nitrato en suelo (kg)
!hn3 --------> Nitrógeno orgánico disuelto en cauce (kg)
!hn4 --------> Amonio disuelto en cauce (kg)
!hn5 --------> Nitrato en cauce (kg)
!hn6 --------> Amonio en acuífero (kg)
!hn7 --------> Nitrato en acuífero (kg)
!hn8 --------> Amonio en fase sólida (adsorbido) en suelo (kg)
!hn9 --------> Nitrógeno orgánico en fase sólida (sedimentos suspendidos) en cauce (kg)
!hn10 -------> Amonio en fase sólida (sedimentos suspendidos) en cauce (kg)
!hn11 -------> Nitrógeno orgánico en fase sólida (sedimentos depositados) en cauce (kg)
!hn12 -------> Amonio en fase sólida (sedimentos depositados) en cauce (kg)
!Definidas en variables tipo celda

!Flujos de nitrógeno (kgN/dt)
!fn0 --------> Mineralización
!fn1 --------> Inmovilización
!fn2 --------> Nitrificación
!fn3 --------> Fijación
!fn4 --------> Desnirificación
!fn5 --------> Percolación NH4
!fn6 --------> Percolación NO3
!fn7 --------> Pérd. subterráneas NH4
!fn8 --------> Pérd. subterráneas NO3
!fn9 --------> Transpiración NH4, asimilación pasiva
!fn10 -------> Transpiración NO3, asimilación pasiva
!fn11 -------> Interflujo NH4
!fn12 -------> Interflujo NO3
!fn13 -------> Flujo base NH4
!fn14 -------> Flujo base NO3
!fn15 -------> Mineralización en cauce
!fn16 -------> Nitrificación en cauce
!fn17 -------> Desnitrificación en cauce
!fn18 -------> Caudal NO
!fn19 -------> Caudal NH4
!fn20 -------> Caudal NO3
!fn21 -------> Depositación de NO
!fn22 -------> Depositación de NH4 adsorbido
!fn23 -------> Escorrentía NO disuelto
!fn24 -------> Escorrentía NH4 disuelto
!fn25 -------> Escorrentía NO3 disuelto
!fn26 -------> Suspensión de NO
!fn27 -------> Suspensión de NH4 adsorbido
!fn28 -------> Caudal sedimentos NO
!fn29 -------> Caudal sedimentos NH4
!fn30 -------> Asimilación activa de NH4
!fn31 -------> Asimilación pasiva de NO3
!fn32 -------> Pérdidas acuífero conectado NH4
!fn33 -------> Pérdidas acuífero conectado NO3
!fn34 -------> Pérdidas acuífero no conectado NH4
!fn35 -------> Pérdidas acuífero no conectado NO3
!fn36 -------> Percolación a acuífero conectado NH4
!fn37 -------> Percolación a acuífero conectado NO3
!fn38 -------> Percolación a acuífero no conectado NH4
!fn39 -------> Percolación a acuífero no conectado NO3
!fn40 -------> Input NH4
!fn41 -------> Input NO3
!fn42 -------> Exfiltración de NH4
!fn43 -------> Exfiltración de NO3
!fn44 -------> Volatilización de NH4
!fn45 -------> Deposición atmosférica en forma de NH4
!fn46 -------> Deposición atmosférica en forma de NO3
!Definidas en variables tipo celda

!Flujos de nitrógeno en superficie (kgN/s)
!qlNO -------> Caudal de nitrógeno orgánico disuelto
!qlNH4 ------> Caudal de amonio disuelto
!qlNO3 ------> Caudal de nitrato
!qsNO -------> Caudal de nitrógeno orgánico particulado
!qsNH4 ------> Caudal de amonio particulado
!Definidas en variables tipo celda

!Otras variables
!tempsuelo --> Temperatura del suelo (ºC)
!fts --------> Factor corrector de la temperatura en suelo
!ftc --------> Factor corrector de la temperatura en cauce
!fh1 --------> Factor corrector por humedad para los procesos de inmovilización y mineralización
!fh2 --------> Factor corrector por humedad para el proceso de nitrificación
!fh3 --------> Factor corrector por humedad para el proceso de desnitrificación
!Definidas en variables tipo celda

!nantecs -----> Estados de nitrógeno iniciales genéricos para toda la cuenca. Va en función de los usos del suelo (fijos en un máximo de 49+cauce) y hay 3 tanques por uso
Real nantecs(50,3),nantecc(5),nanteca(2)

!Inputs en suelo (van según usos del suelo)
!norganicos -> Input de nitrógeno orgánico en el suelo    No se utiliza, por ahora se considera un parámetro y no una variable de estado
!amonios ----> Input de amonio en el suelo
!nitratos ---> Input de nitrato en el suelo
!amonioscult
!nitratoscult
Real,allocatable:: amonios(:,:),nitratos(:,:),amonioscult(:,:),nitratoscult(:,:)
!Real,allocatable:: norganicos(:,:),amonios(:,:),nitratos(:,:),norganicoscult(:,:),amonioscult(:,:),nitratoscult(:,:)

!Variables de estaciones
!NO----------> Código de estación de datos observados de nitrógeno orgánico disuelto (mg/l)
!AM ---------> Código de estación de datos observados de amonio disuelto (mg/l)
!NI ---------> Código de estación de datos observados de nitrato (mg/l)
!kno --------> Número de estaciones totales de nitrógeno orgánico
!kam --------> Número de estaciones totales de amonio
!kni --------> Número de estaciones totales de nitrato
!jno --------> Contador de estaciones de nitrógeno orgánico
!jam --------> Contador de estaciones de amonio
!jni --------> Contador de estaciones de nitrato
Integer kno,kam,kni,jno,jam,jni
!norg -------> Serie de nitrógeno orgánico disuelto (observada/simulada punto NO) (mg/l)
!amonio -----> Serie de amonio disuelto (observada/simulada punto AM)
!nitrato ----> Serie de nitrato (observada/simulada punto NI)
!norgql -----> Serie de nitrógeno orgánico disuelto, en caudal líquido (simulada punto Q) (mg/l)
!amonioql ---> Serie de amonio disuelto, en caudal líquido (simulada punto Q) (mg/l)
!nitratoql --> Serie de nitrato, en caudal líquido (simulada punto Q) (mg/l)
!norgqs -----> Serie de nitrógeno orgánico particulado, en caudal sólido (simulada punto Q) (kg/m3 de sedimento)
!amonioqs ---> Serie de amonio adsorbido, en caudal sólido (simulada punto Q) (kg/m3 de sedimento)
TYPE (estacion),ALLOCATABLE:: norg(:),amonio(:),nitrato(:),norgql(:),amonioql(:),nitratoql(:),norgqs(:),amonioqs(:)

!Variables que almacenan flujos y estados de sediemntos
!depositacion -> Almacena el volumen de arcillas que es depositado (de los que estaban suspendidos)
!qdepositado --> Almacena el volumen de sediemntos que es transportado a la celda aguas abajo y que procede de erosión y deposita en caso de ladera o de depositado en cárcava y cauce
!qdepositadoar-> Almacena el volumen de arcillas que es transportado a la celda aguas abajo y que procede de erosión y deposita en caso de ladera o de depositado en cárcava y cauce
!qsuspendido --> Almacena el volumen de sedimentos que es transportado a la celda aguas abajo y que procede de suspensión
!qsuspendidoar-> Almacena el volumen de arcillas que es transportado a la celda aguas abajo y que procede de suspensión
!volsusini ----> Almacena el volumen inicial de arcillas suspendido
!voldepini ----> Almacena el volumen inicial de arcillas depositado
!Definidas en variables tipo celda

!Variables de asimilación activa de nutrientes por parte de la vegetación
!AsimilaconTot-> Asimilación conjunta activa y pasiva total, para cálculo intermedio
Real AsimilacionTot

!Variables tipo dummy
!kcau -------> Número de las variables correspondientes a cauce en la subrutina del estado inicial leenantecval de P_Nantec. (Usos +1)
!kacu -------> Número de las variables correspondientes a acuífero en la subrutina del estado inicial leenantecval de P_Nantec. (Usos +2)
!dummy ------> Variables para almacenar datos que luego se reescriben
!h1_ini -----> Almacenamiento inicial del tanque de almacenamiento estático.
!Volag1 ----->
!Volag2
!Volag3
!Volag4
!Volag5
!ConcNH4
!ConcNO3
!ConcNO
!NH4total ---> Almacena la suma de adsorbido y desorbido para poder aplicar el coeficiente de partición
Integer kcau,kacu
Real dummy1,dummy2,dummy3,dummy4,dummy5,dummy6,dummy10,a1,a2
Real h1_ini,h2_ini,h3_ini,h4_ini,h5_ini,volag1,volag2,volag3,Volag4,Volag5,Volag6,ConcNH4,ConcNO3,ConcNO
Real NH4total
Real Volag7,Volag8,Volag9,Volag10,VolNO,VolNH4,VolNO3
Real Volag11
!cantUsosSueloNitr
INTEGER cantUsSueNitr


!balanc_nitr
Real,allocatable:: balanc_nitr(:,:)
!estadnitr
REAL, ALLOCATABLE :: estadnitr(:,:)


!*************************************************************************************************************
!********************Variables y parámetros para TETIS Nitrógeno con cultivos  (05/2017)**********************
!*************************************************************************************************************

!ncult ------> Número de usos del suelo que funcionan como cultivo
!cultmax ----> Número máximo de cultivos que hay en rotación (suma de todos los usos del suelo)
!codcult ----> Código de usos del suelo que funciona como cultivo
!carcult ----> Recoge las características de cada uno de los cultivos que están en rotación en cada uno de los usos del suelo
Integer ncult,cultmax
Integer,allocatable:: codcult(:),nrotacion(:)
Real,allocatable:: carcult(:,:),fvegcult(:,:),dummy7(:,:)

!Posiciones de carcult
!1 ----------> Código del uso del suelo
!2 ----------> Número del cultivo en rotación dentro del uso del suelo
!3 ----------> Día juliano de plantación (día [1-365])
!4 ----------> Día juliano de cosecha (día [1-365])
!5 ----------> Peso seco en plantación (t/ha)
!6 ----------> Factor de cubierta en plantación (adim)
!7 ----------> Peso seco esperado en cosecha (t/ha)
!8 ----------> Temepratura base para el desarrollo del cultivo (oC)
!9 ----------> Coeficiente a (Descripción del modelo EU-Rotate para cultivos) (adim)
!10 ---------> Coeficiente b (Descripción del modelo EU-Rotate para cultivos) (adim)
!11 ---------> Coeficiente k2 (t/ha), se calcula tras la lectura a partir del resto de datos.

!Variables de estado
!w ----------> Plant dry matter
!nw ---------> nitrógeno extraído (variable acumulada)
!Ncrit ------> Porcentaje de nitrógeno crítico (tanto por 1)
!Nextr ------> Cantidad de nitrógeno que debe asimilar de forma activa (kg)
!Definidas en variables tipo celda

!Otras variables
!dw ---------> Crecimiento diario (t/ha)
!Gn ---------> Factor limitante del crecimiento por nitrógeno (adim)
!Gt ---------> Factor limitante del crecimiento por temperatura (adim)
!Gw ---------> Factor limitante por agua (entra como transpiración en la fórmula) (adim)
!Ncritant ---> Variable que almacena el nrit anterior teórico (caso t=1 y de inicio de cultivo) (% en tanto por 1)
!Nextrant ---> Nitrógeno extraído hasta el dt actual. (No incluye el extraído en este paso de tiempo)
!want -------> Peso seco inicial del dt
!porcentajen > Porcentaje de nitrógeno sobre el peso seco tras haber extraído nitrógeno en el dt
!restcoseecha> Variable que almacena el valor de los restos de cosecha que pasan a incorporarse al suelo en el siguiente paso de tiempo (kg/celda). Variable tipo celda
Real dw,Gn,Gw,Gt,Ncritant,Nextrant,porcentajen,want

!Variables para cálculo de cultantec.sds
!cultantecs -> Variable que almacena w (peso seco) y el factor de cubierta de cada uso del suelo. Mismo orden que factoretmes. En los usos del suelo que no
!están en modo cultivo, se escribe 0 en las dos columnas, luego no se usará.
Real cultantecs(50,2)

!Variables dummy que almacenan datos
Integer poscodcult,plantacionultimo,cosechaultimo,filaultimo,Cultivo,compcultivo,compcultivo2,compcultivo3,&
 compcultivo4,Compactiva1,Compactiva2

Integer,allocatable:: crecimiento(:)
!crecimiento es un vector que almacena si el cultivo ha llegado a la etapa de "mediados de temporada" según la FAO, a partir de aquí sus necesidades de agua no varían.

!Vicente Sep-20 . Se añade este tipo para emplearlo en la paralelizacion de Topolco
TYPE fil__cols
    INTEGER fil,col,n    
END TYPE



END MODULE
