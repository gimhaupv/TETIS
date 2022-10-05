!Subrutina que define todos los mensajes de error que solo necesitan como información los nombres de los archivos
!Solo se llama una vez al principio de cada programa, no es necesario llamarla cada vez que se usa la subrutina "errores"
SUBROUTINE labels
    USE modtet
    IMPLICIT NONE
    if (lang.eq.1)then   
    !00 - FileSSP    
    strings(1)='001 El fichero filessp.txt no existe'
    strings(2)='002 Lectura erronea en el encabezado de filessp.txt '
    strings(3)='003 Lectura de datos erronea en filessp.txt '    
    !10 - Paramgeo
    strings(11)='011 No existe el fichero '//TRIM(ADJUSTL(arch(1)))
    strings(12)='012 Lectura erronea en el encabezado de '//TRIM(ADJUSTL(arch(1)))
    strings(13)='013 Lectura de datos erronea en '//TRIM(ADJUSTL(arch(1)))
    !20 - Calib
    strings(21)='021 No existe el fichero '//TRIM(ADJUSTL(arch(2)))
    strings(22)='022 Lectura de datos erronea en '//TRIM(ADJUSTL(arch(2)))
    strings(23)='023 Lectura de factores de nieve erronea en '//TRIM(ADJUSTL(arch(2)))
    strings(24)='024 Lectura de factores de sedimentos erronea en '//TRIM(ADJUSTL(arch(2)))
    !30 - Topolco
    strings(31)='031 No existe el fichero '//TRIM(ADJUSTL(arch(3)))
    strings(32)='032 Lectura erronea en el encabezado de '//TRIM(ADJUSTL(arch(3)))
    strings(33)='033 Lectura de datos erronea en '//TRIM(ADJUSTL(arch(3)))
    strings(34)='034 Lectura de datos erronea de los valores medios en '//TRIM(ADJUSTL(arch(3)))
    strings(35)='035 Se ha encontrado una celda con valor -9999 en el mapa' //TRIM(ADJUSTL(artem))
    !40 - Hantec
    strings(41)='041 No existe el fichero '//TRIM(ADJUSTL(arch(4)))
    strings(42)='042 Lectura erronea en el encabezado de '//TRIM(ADJUSTL(arch(4)))
    strings(43)='043 Lectura de datos erronea en '//TRIM(ADJUSTL(arch(4)))
    !50 - Input
    strings(51)='051 No existe el fichero '//TRIM(ADJUSTL(arch(5)))
    strings(52)='052 Lectura erronea en el encabezado de '//TRIM(ADJUSTL(arch(5)))
    strings(53)='053 Lectura de datos erronea en '//TRIM(ADJUSTL(arch(5)))
    strings(54)='054 La lectura del fichero '//TRIM(ADJUSTL(arch(5)))//' ha generado un error grave'
    strings(55)='055 No existen datos de N V S Q B D X W DA DL DC NO AM NI en '//TRIM(ADJUSTL(arch(5)))    
    !80 - Settings
    strings(81)='081 No existe el fichero '//TRIM(ADJUSTL(arch(34)))
    strings(82)='082 Lectura de datos erronea en la segunda linea de '//TRIM(ADJUSTL(arch(34)))
    strings(83)='083 Lectura de datos erronea en la tercera linea de '//TRIM(ADJUSTL(arch(34)))//'. Este fichero se ha sobrescrito con los valores por defecto. Considere revisarlo'
    strings(84)='084 Lectura de datos erronea en la cuarta linea de '//TRIM(ADJUSTL(arch(34)))//'. Este fichero se ha sobrescrito con los valores por defecto. Considere revisarlo'
    strings(85)='085 Lectura de datos erronea en la quinta linea de '//TRIM(ADJUSTL(arch(34)))//'. Este fichero se ha sobrescrito con los valores por defecto. Considere revisarlo'
    !500 - Input - errores de filas
    strings(501)='501 Lectura de datos erronea en la linea G de '//TRIM(ADJUSTL(arch(5)))
    strings(502)='502 Lectura de datos erronea en la linea N de '//TRIM(ADJUSTL(arch(5)))
    strings(503)='503 Lectura de datos erronea en la linea V de '//TRIM(ADJUSTL(arch(5)))
    strings(504)='504 Lectura de datos erronea en la linea P de '//TRIM(ADJUSTL(arch(5)))
    strings(505)='505 Lectura de datos erronea en la linea S de '//TRIM(ADJUSTL(arch(5)))
    strings(506)='506 Lectura de datos erronea en la linea Q de '//TRIM(ADJUSTL(arch(5)))
    strings(507)='507 Lectura de datos erronea en la linea B de '//TRIM(ADJUSTL(arch(5)))
    strings(508)='508 Lectura de datos erronea en la linea T de '//TRIM(ADJUSTL(arch(5)))
    strings(509)='509 Lectura de datos erronea en la linea C de '//TRIM(ADJUSTL(arch(5)))
    strings(510)='510 Lectura de datos erronea en la linea E de '//TRIM(ADJUSTL(arch(5)))
    strings(511)='511 Lectura de datos erronea en la linea I de '//TRIM(ADJUSTL(arch(5)))
    strings(512)='512 Lectura de datos erronea en la linea F de '//TRIM(ADJUSTL(arch(5)))
    strings(513)='513 Lectura de datos erronea en la linea H de '//TRIM(ADJUSTL(arch(5)))
    strings(514)='514 Lectura de datos erronea en la linea X de '//TRIM(ADJUSTL(arch(5)))
    strings(515)='515 Lectura de datos erronea en la linea W de '//TRIM(ADJUSTL(arch(5)))
    strings(516)='516 Lectura de datos erronea en la linea R de '//TRIM(ADJUSTL(arch(5)))
    strings(517)='517 Lectura de datos erronea en la linea DA de '//TRIM(ADJUSTL(arch(5))) !Cris
    strings(518)='518 Lectura de datos erronea en la linea DL de '//TRIM(ADJUSTL(arch(5)))
    strings(519)='519 Lectura de datos erronea en la linea DC de '//TRIM(ADJUSTL(arch(5)))
    !60 - FactorETmes
    strings(61)='061 No existe el fichero '//TRIM(ADJUSTL(arch(6)))
    strings(62)='062 Lectura erronea en el encabezado de '//TRIM(ADJUSTL(arch(6)))
    strings(63)='063 El fichero ' //TRIM(ADJUSTL(arch(6)))// ' no existe. Se ha generado un fichero con valores por defecto como ejemplo'
    strings(64)='064 El numero de filas del fichero ' //TRIM(ADJUSTL(arch(6)))// ' no coincide con el numero de categorias de cobertura vegetal'
    strings(65)='065 Lectura erronea del fichero ' //TRIM(ADJUSTL(arch(6)))
    !70 - CurvasHV
    strings(71)='071 No existe el fichero '//TRIM(ADJUSTL(arch(7)))
    strings(72)='072 Lectura erronea en el encabezado de '//TRIM(ADJUSTL(arch(7)))
    strings(73)='073 Lectura de datos erronea en '//TRIM(ADJUSTL(arch(7)))
    !100 otros archivos
    strings(101)='101 No existe el fichero '//TRIM(ADJUSTL(arch(21)))   
    strings(102)='102 Lectura de datos erronea en '//TRIM(ADJUSTL(arch(21)))
    strings(103)='103 No existe el fichero '//TRIM(ADJUSTL(arch(37)))
    strings(104)='104 Lectura de datos erronea en '//TRIM(ADJUSTL(arch(37))) 
    strings(105)='105 No existe el fichero '//TRIM(ADJUSTL(dirtra))//'UmbralesQ.txt'
    strings(106)='106 Lectura erronea en '//TRIM(ADJUSTL(dirtra))//'UmbralesQ.txt'
    strings(107)='107 Lectura de factores de prediccion erronea en '//TRIM(ADJUSTL(dirtra))//'UmbralesQ.txt'
    strings(108)='108 Lectura de factores de nieve erronea en '//TRIM(ADJUSTL(dirtra))//'UmbralesQ.txt'
    strings(109)='109 No existe el fichero '//TRIM(ADJUSTL(arch(22)))
    strings(110)='110 Lectura de datos erronea en '//TRIM(ADJUSTL(arch(22)))
    strings(111)='111 No existe el fichero '//TRIM(ADJUSTL(arch(38)))
    strings(112)='112 Lectura de datos erronea en '//TRIM(ADJUSTL(arch(38)))
    strings(113)='113 No existe el fichero '//TRIM(ADJUSTL(arch(36)))    
    strings(114)='114 No existe el fichero con los nombres de los multiples eventos para la calibracion automatica' 
    strings(115)='115 No existe el fichero '//TRIM(ADJUSTL(arch(26)))
    strings(116)='116 Lectura de datos erronea en '//TRIM(ADJUSTL(arch(26)))
    strings(117)='117 No existe el fichero: '//TRIM(ADJUSTL(dirtra))//'VarQmax-SCEUA.txt'
    strings(118)='118 Lectura de datos erronea en '//TRIM(ADJUSTL(dirtra))//'VarQmax-SCEUA.txt'     
    strings(119)='119 Uno de los puntos de control añadido no pertenece a la subcuenca de trabajo'  !Cris (17/10/2016)
    strings(120)='120 No existe el fichero '//TRIM(ADJUSTL('defconie.txt')) 
    !200 - Sedimentos
    strings(200)='200 El fichero filessed.txt no existe'
    strings(201)='201 Lectura erronea en alguna linea de filessed.txt '
    strings(202)='202 No existe el fichero '//TRIM(ADJUSTL(archsed(7)))
    strings(203)='203 Lectura erronea en el encabezado de '//TRIM(ADJUSTL(archsed(7)))
    strings(204)='204 Lectura de datos erronea en '//TRIM(ADJUSTL(archsed(7)))
    strings(205)='205 El fichero '//TRIM(ADJUSTL(archsed(10)))//' no existe'
    strings(206)='206 Lectura de datos erronea en '//TRIM(ADJUSTL(archsed(10)))
    strings(207)='207 No existe el fichero '//TRIM(ADJUSTL(archsed(11)))
    strings(208)='208 Lectura erronea en el encabezado de '//TRIM(ADJUSTL(archsed(11)))
    strings(209)='209 Lectura de datos erronea en '//TRIM(ADJUSTL(archsed(11)))
    strings(210)='210 No existe el fichero '//TRIM(ADJUSTL(archsed(12)))
    strings(211)='211 No existe el fichero '//TRIM(ADJUSTL(archsed(8)))
    strings(212)='212 No existe el fichero '//TRIM(ADJUSTL(dirtra))//'Var-SCEUA_Sed.txt'
    strings(213)='213 Lectura erronea del fichero de input de la calibracion automatica de sedimentos'
    strings(214)='214 El evento presenta una linea referente al submodelo de sedimentos, que esta desactivado'
    strings(215)='215 No esta activado el submodelo de sedimentos'
    !300 - Vegetacion
    strings(300)='300 No existe el fichero '//TRIM(ADJUSTL(archveg(7)))
    strings(301)='301 Lectura erronea en el encabezado de '//TRIM(ADJUSTL(archveg(7)))
    strings(302)='302 Lectura de datos erronea en '//TRIM(ADJUSTL(archveg(7)))
    strings(303)='303 El fichero filesveg.txt no existe'
    strings(304)='304 Lectura erronea en alguna linea de filesveg.txt '
    strings(305)='305 El fichero filesveg.txt no existe'
    strings(306)='306 Se han encontrado celdas con codigo de vegetacion = 0. Se ha modificado a 1.'
    strings(307)='307 Faltan datos de temperatura para evaluar la vegetacion '
    strings(308)='308 Faltan datos de puntos con informacion del estado de la vegetacion '
    strings(309)='309 Faltan datos de radiación para evaluar la vegetacion ' 
    strings(310)='310 No existe el fichero '//TRIM(ADJUSTL(arch(41)))
    strings(311)='311 Lectura de datos erronea en '//TRIM(ADJUSTL(arch(41)))
    strings(312)='312 El numero de filas del fichero ' //TRIM(ADJUSTL(arch(41)))//' no coincide con el numero de categorias de cobertura vegetal'
    strings(313)='313 No esta activado el submodelo de vegetacion dinamica'
    strings(314)='314 El evento presenta una linea referente al submodelo de vegetacion dinamica, que esta desactivado'
    strings(315)='315 No existe el fichero '//TRIM(ADJUSTL(dirtra))//'Var-SCEUA_HidrVeg.txt'
    strings(316)='316 Lectura erronea del fichero de input de la calibracion automatica de vegetacion e hidrologia'
    strings(317)='317 No existe el fichero: '//TRIM(ADJUSTL(dirtra))//'xlb_veg-SCEUA.txt'
    strings(318)='318 Lectura de datos erronea en '//TRIM(ADJUSTL(dirtra))//'xlb_veg-SCEUA.txt'
    strings(319)='319 No existe el fichero: '//TRIM(ADJUSTL(dirtra))//'xub_veg-SCEUA.txt' 
    strings(320)='320 Lectura de datos erronea en '//TRIM(ADJUSTL(dirtra))//'xub_veg-SCEUA.txt' 
    strings(321)='321 No existe el fichero: '//TRIM(ADJUSTL(dirtra))//'xguess_veg-SCEUA.txt'  
    strings(322)='322 Lectura de datos erronea en '//TRIM(ADJUSTL(dirtra))//'xguess_veg-SCEUA.txt'  
    strings(323)='323 No existe el fichero: '//TRIM(ADJUSTL(dirtra))//'xchk_veg-SCEUA.txt' 
    strings(324)='324 Lectura de datos erronea en '//TRIM(ADJUSTL(dirtra))//'xchk_veg-SCEUA.txt'
    !400 - Nitrogeno
    strings(400)='400 No existe el fichero '//TRIM(ADJUSTL(archnit(9)))
    strings(401)='401 Lectura erronea del fichero '//TRIM(ADJUSTL(archnit(9)))
    strings(402)='402 El numero de filas en ' //TRIM(ADJUSTL(archnit(9)))// ' no coincide con el numero de categorias de cobertura vegetal'
    strings(403)='403 No existe el fichero '//TRIM(ADJUSTL(archnit(8)))
    strings(404)='404 Lectura erronea del fichero '//TRIM(ADJUSTL(archnit(8)))
    strings(405)='405 El numero de filas -2 en ' //TRIM(ADJUSTL(archnit(8)))// ' no coincide con el numero de categorias de cobertura vegetal'
    strings(406)='406 Faltan datos de temperatura para evaluar el submodelo de nitrogeno '
    strings(407)='407 El fichero filesnit.txt no existe'
    strings(408)='408 Lectura erronea en alguna linea de filesnit.txt '
    strings(409)='409 No existe el fichero '//TRIM(ADJUSTL(archnit(13)))
    strings(410)='410 Lectura erronea en el encabezado de '//TRIM(ADJUSTL(archnit(13)))
    strings(411)='411 Lectura de datos erronea en '//TRIM(ADJUSTL(archnit(13)))
    strings(412)='412 Lectura erronea del fichero '//TRIM(ADJUSTL(archnit(7)))
    strings(413)='413 No existe el fichero '//TRIM(ADJUSTL(archnit(7)))
    strings(414)='414 El numero de filas en ' //TRIM(ADJUSTL(archnit(7)))// ' no coincide con el numero de categorias de cobertura vegetal'
    strings(415)='415 No existe el fichero '//TRIM(ADJUSTL(archnit(15)))
    strings(416)='416 Lectura erronea del fichero '//TRIM(ADJUSTL(archnit(15)))
    strings(417)='417 El numero de filas en cultantec_val.txt no coincide con el numero de categorias de cobertura vegetal'
    strings(418)='418 No existe el fichero '//TRIM(ADJUSTL(archnit(16)))
    strings(419)='419 Lectura erronea del fichero '//TRIM(ADJUSTL(archnit(16)))
    strings(420)='420 No existe el fichero '//TRIM(ADJUSTL(arch(40)))
    strings(421)='421 Lectura erronea del fichero '//TRIM(ADJUSTL(arch(40)))
    strings(422)='422 No existe el fichero '//TRIM(ADJUSTL(archnit(17)))
    strings(423)='423 Lectura erronea del fichero '//TRIM(ADJUSTL(archnit(17)))
    strings(424)='424 No existe el fichero '//TRIM(ADJUSTL(archnit(18)))
    strings(425)='425 Lectura erronea del fichero '//TRIM(ADJUSTL(archnit(18)))
    strings(426)='426 No existe el fichero '//TRIM(ADJUSTL(archnit(10)))
    strings(427)='427 No existe el fichero '//TRIM(ADJUSTL(archnit(11)))
    strings(428)='428 Lectura erronea del fichero '//TRIM(ADJUSTL(archnit(10)))
    strings(429)='429 Lectura erronea del fichero '//TRIM(ADJUSTL(archnit(11)))
    strings(430)='430 El numero de filas en ' //TRIM(ADJUSTL(archnit(10)))// ' no coincide con el numero de categorias de cobertura vegetal'
    strings(431)='431 El numero de filas en ' //TRIM(ADJUSTL(archnit(11)))// ' no coincide con el numero de categorias de cobertura vegetal'    
    strings(432)='432 No esta activado el submodelo de nitrogeno'
    strings(433)='433 No existe el fichero '//TRIM(ADJUSTL(dirtra))//'xguess_nitr-SCEUA.txt'
    strings(434)='434 Lectura erronea del fichero '//TRIM(ADJUSTL(dirtra))//'xguess_nitr-SCEUA.txt'
    strings(435)='435 No existe el fichero '//TRIM(ADJUSTL(dirtra))//'xub_nitr-SCEUA.txt'
    strings(436)='436 Lectura erronea del fichero '//TRIM(ADJUSTL(dirtra))//'xub_nitr-SCEUA.txt'
    strings(437)='437 No existe el fichero '//TRIM(ADJUSTL(dirtra))//'xlb_nitr-SCEUA.txt'
    strings(438)='438 Lectura erronea del fichero '//TRIM(ADJUSTL(dirtra))//'xlb_nitr-SCEUA.txt'
    strings(439)='439 No existe el fichero '//TRIM(ADJUSTL(dirtra))//'xchk_nitr-SCEUA.txt'
    strings(440)='440 Lectura erronea del fichero '//TRIM(ADJUSTL(dirtra))//'xchk_nitr-SCEUA.txt'
    strings(441)='441 El evento presenta una linea referente al submodelo de nitrogeno, que esta desactivado'
    strings(442)='442 No existe el fichero '//TRIM(ADJUSTL(dirtra))//'Var-SCEUA_Nitr.txt'
    strings(443)='443 Lectura erronea del fichero '//TRIM(ADJUSTL(dirtra))//'Var-SCEUA_Nitr.txt'
    !Mensajes a pantalla
    strings(800)='Se han encontrado ERRORES !!!'
    strings(801)='Fin de la ejecucion de TETIS 9'
    strings(802)='* Errores y advertencias detectados en TETIS '    
    strings(803)='Fin de la ejecucion del modulo HANTEC.EXE'    
    strings(804)='Calcula la humedad antecedente'
    strings(805)='Calcula el estado inicial de sedimentos'
    strings(806)='Fin de la ejecucion del modulo SEDANTECS.EXE'
    strings(807)='Las fechas elegidas no son coherentes'
    strings(808)='Se ha creado con exito el fichero de episodio reducido'
    strings(809)='Codigo de region homogenea es cero '
    strings(810)='Interpola los valores de altura de agua equivalente'
    strings(811)='Fin de la ejecucion del modulo INTNIEVES.EXE'
    strings(812)='Numero de argumentos superior a uno'
    strings(813)='Argumento diferente de -a= '
    strings(814)='Argumento diferente de -l= '
    strings(815)='Define el contorno de la region cubierta con nieve a partir de una cota'
    strings(816)='Genera puntos de control'
    strings(817)='Calculando Topolco, por favor espere...'
    strings(818)='Define las alturas equivalentes de nieve de la region previamente definida'
    strings(819)='Fin del cambio de informacion tipo CEDEX en Columna'
    strings(820)='Por Favor espere...'
    strings(821)='Numero total de evaluacion de la funcion '
    strings(822)='Resultados de optimizacion '
    strings(823)='Fact. FNsinlluvia'
    strings(824)='Fact. FNconlluvia'
    strings(825)='Funcion Objetivo'
    strings(826)='Se ha terminado la ejecucion del modulo de calibracion automatica' 
    strings(827)='Calcula el estado inicial de nitrogeno'
    strings(828)='Fin de la ejecucion del modulo NANTECS.EXE'
    !900 - Misc
    strings(901)='901 Error eliminando la memoria dinamica'
    strings(902)='902 Se emplea el Metodo del Pulso Modificado'
    strings(903)='903 La pendiente es cero, se ha modificado a 1 por mil'
    strings(904)='904 Revise la curva Altura vs Volumen'
    strings(905)='905 Valor del Volumen del embalse por fuera del rango'
    strings(906)='906 Revise la curva Altura vs Volumen'
    strings(907)='907 Faltan datos de temperatura para evaluar la nieve'
    strings(908)='908 Faltan datos de estaciones con altura de nieve'
    strings(909)='909 Valor del Volumen del embalse negativo... se ha corregido a 0.0'
    strings(910)='910 Valor del Nivel del embalse por fuera del rango'
    strings(911)='911 Revise la curva Caudal de salida vs Volumen'
    strings(912)='912 Revise la curva Altura vs Caudal de salida Qs'
    strings(913)='913 Valor del Nivel del embalse negativo... se ha corregido a 0.0 - Qs'
    strings(914)='914 Valor del Nivel del embalse por fuera del rango - Qs'
    strings(915)='915 El punto de manantial no se encuentra en una celda con cauce'
    strings(916)='916 El indice de evapotranspiracion se ha modificado a 1'
    strings(917)='917 Codigo de region homogenea es cero para alguna celdas'
    strings(918)='918 Las condiciones iniciales del tanque T3 son superiores a su capacidad maxima para algunas celdas y se han limitado al maximo consentido'
    strings(919)='919 No existe el fichero con las fechas para recortar el episodio'
    strings(920)='920 El fichero Recorta.txt no existe, (fichero con lista de embalses a eliminar)'
    strings(921)='921 Lectura de datos erronea en Recorta.txt'
    strings(922)='922 El evento presenta una linea referente al modulo de nieve, que esta desactivado'
    strings(923)='923 El evento presenta una linea referente al modulo de embalses, que esta desactivado'
    strings(924)='924 El evento presenta una linea T, y no esta activado el submodelo de vegetacion dinamica, ni nitrogeno ni nieve'
    else if (lang.eq.2) then
    !00 - FileSSP
    strings(1)='001 The filessp.txt file is missing'
    strings(2)='002 Error reading the header of filessp.txt '
    strings(3)='003 Error reading the following file: filessp.txt '
    !10 - Paramgeo
    strings(11)='011 The following file is missing: '//TRIM(ADJUSTL(arch(1)))
    strings(12)='012 Error reading the header of '//TRIM(ADJUSTL(arch(1)))
    strings(13)='013 Error reading the following file: '//TRIM(ADJUSTL(arch(1)))
    !20 - Calib
    strings(21)='021 The following file is missing: '//TRIM(ADJUSTL(arch(2)))
    strings(22)='022 Error reading the following file: '//TRIM(ADJUSTL(arch(2)))
    strings(23)='023 Error reading the snow correction factors in '//TRIM(ADJUSTL(arch(1)))
    strings(24)='024 Error reading the sediment correction factors in '//TRIM(ADJUSTL(arch(2)))
    !30 - Topolco
    strings(31)='031 The following file is missing: '//TRIM(ADJUSTL(arch(3)))
    strings(32)='032 Error reading the header of '//TRIM(ADJUSTL(arch(3)))
    strings(33)='033 Error reading the following file: '//TRIM(ADJUSTL(arch(3)))
    strings(34)='034 Error reading the parameter average values in '//TRIM(ADJUSTL(arch(3)))
    !strings(35)='035 A cell with value -9999 has been found in the map' //TRIM(ADJUSTL(artem))  !Realmente no se usa, esta directamente en toparc
    !40 - Hantec
    strings(41)='041 The following file is missing: '//TRIM(ADJUSTL(arch(4)))
    strings(42)='042 Error reading the header of '//TRIM(ADJUSTL(arch(4)))
    strings(43)='043 Error reading the following file: '//TRIM(ADJUSTL(arch(4)))
    !50 - Input
    strings(51)='051 The following file is missing: '//TRIM(ADJUSTL(arch(5)))
    strings(52)='052 Error reading the header of '//TRIM(ADJUSTL(arch(5)))
    strings(53)='053 Error reading the following file: '//TRIM(ADJUSTL(arch(5)))
    strings(54)='054 Error reading the following file (wrong format or missing data): '//TRIM(ADJUSTL(arch(5)))    
    strings(55)='055 No N V S Q B D X W DA DL DC NO AM NI lines were found in '//TRIM(ADJUSTL(arch(5))) 
    !60 - FactorETmes
    strings(61)='061 The following file is missing: '//TRIM(ADJUSTL(arch(6)))
    strings(62)='062 Error reading the header of '//TRIM(ADJUSTL(arch(6)))
    strings(63)='063 The following file is missing: ' //TRIM(ADJUSTL(arch(6)))// '. A default file was generated'
    strings(64)='064 The row numbers in ' //TRIM(ADJUSTL(arch(6)))// ' does not match with the number of land use categories'
    strings(65)='065 Error reading the following file: ' //TRIM(ADJUSTL(arch(6)))
    !70 - CurvasHV
    strings(71)='071 The following file is missing: '//TRIM(ADJUSTL(arch(7)))
    strings(72)='072 Error reading the header of '//TRIM(ADJUSTL(arch(7)))
    strings(73)='073 Error reading the following file: '//TRIM(ADJUSTL(arch(7)))
    !80 - Settings
    strings(81)='081 The following file is missing: '//TRIM(ADJUSTL(arch(34)))
    strings(82)='082 Error reading the second line of: '//TRIM(ADJUSTL(arch(34)))
    strings(83)='083 Error reading the third line of:'//TRIM(ADJUSTL(arch(34)))//'. This file has been overwritten with the default values. Consider revising it'
    strings(84)='084 Error reading the fourth line of:'//TRIM(ADJUSTL(arch(34)))//'. This file has been overwritten with the default values. Consider revising it'
    strings(85)='085 Error reading the fifth line of:'//TRIM(ADJUSTL(arch(34)))//'. This file has been overwritten with the default values. Consider revising it'
    !100 otros archivos
    strings(101)='101 The following file is missing: '//TRIM(ADJUSTL(arch(21)))
    strings(102)='102 Error reading the following file: '//TRIM(ADJUSTL(arch(21)))
    strings(103)='103 The following file is missing: '//TRIM(ADJUSTL(arch(37)))
    strings(104)='104 Error reading the following file: '//TRIM(ADJUSTL(arch(37))) 
    strings(105)='105 The following file is missing: '//TRIM(ADJUSTL(dirtra))//'UmbralesQ.txt'
    strings(106)='106 Error reading the following file '//TRIM(ADJUSTL(dirtra))//'UmbralesQ.txt'
    strings(107)='107 Error reading the following file '//TRIM(ADJUSTL(dirtra))//'UmbralesQ.txt (prediction factors)'
    strings(108)='108 Error reading the following file '//TRIM(ADJUSTL(dirtra))//'UmbralesQ.txt (snow correction factors)'
    strings(109)='109 The following file is missing: '//TRIM(ADJUSTL(arch(22)))
    strings(110)='110 Error reading the following file: '//TRIM(ADJUSTL(arch(22)))
    strings(111)='111 The following file is missing: '//TRIM(ADJUSTL(arch(38)))
    strings(112)='112 Error reading the following file: '//TRIM(ADJUSTL(arch(38)))
    strings(113)='113 The following file is missing: '//TRIM(ADJUSTL(arch(36)))    
    strings(114)='114 The file for the multiple events automatic calibration is missing' 
    strings(115)='115 The following file is missing: '//TRIM(ADJUSTL(arch(26)))
    strings(116)='116 Error reading the following file: '//TRIM(ADJUSTL(arch(26)))
    strings(117)='117 The following file is missing: '//TRIM(ADJUSTL(dirtra))//'VarQmax-SCEUA.txt'
    strings(118)='118 Error reading the following file: '//TRIM(ADJUSTL(dirtra))//'VarQmax-SCEUA.txt'    
    strings(119)='119 One of the control points added does not belong to the working subbasin'  !Cris (17/10/2016)
    strings(120)='120 The following file is missing: '//TRIM(ADJUSTL('defconie.txt'))
    !200 - Sedimentos
    strings(200)='200 The filessed.txt file is missing'
    strings(201)='201 Error reading the filessed.txt file'
    strings(202)='202 The following file is missing: '//TRIM(ADJUSTL(archsed(7)))
    strings(203)='203 Error reading the header of '//TRIM(ADJUSTL(archsed(7)))
    strings(204)='204 Error reading the following file: '//TRIM(ADJUSTL(archsed(7)))
    strings(205)='205 The following file is missing: '//TRIM(ADJUSTL(archsed(10)))
    strings(206)='206 Error reading the following file: '//TRIM(ADJUSTL(archsed(10)))
    strings(207)='207 The following file is missing: '//TRIM(ADJUSTL(archsed(11)))
    strings(208)='208 Error reading the header of '//TRIM(ADJUSTL(archsed(11)))
    strings(209)='219 Error reading the following file: '//TRIM(ADJUSTL(archsed(11)))
    strings(210)='210 The following file is missing: '//TRIM(ADJUSTL(archsed(12)))
    strings(211)='211 The following file is missing: '//TRIM(ADJUSTL(archsed(8)))
    strings(212)='212 The following file is missing: '//TRIM(ADJUSTL(dirtra))//'Var-SCEUA_Sed.txt'
    strings(213)='213 Error reading the the input file for automatic calibration of the sediment sub-module'
    strings(214)='214 The event file has a line which makes reference to sediment sub-model, which is not activated'
    strings(215)='215 The sediment sub-model is not activated'
    !300 - Vegetación
    strings(300)='300 The following file is missing: '//TRIM(ADJUSTL(archveg(7)))
    strings(301)='301 Error reading the header of '//TRIM(ADJUSTL(archveg(7)))
    strings(302)='302 Error reading the following file: '//TRIM(ADJUSTL(archveg(7)))
    strings(303)='303 The filesveg.txt file is missing'
    strings(304)='304 Error reading the filesveg.txt file'
    strings(305)='305 The filesveg.txt file is missing'
    strings(306)='306 Some cells inside the catchment have a land use code = 0. It has been modified to 1'
    strings(307)='307 No points with temperature data could be found'
    strings(308)='308 No points with vegetation data could be found '
    strings(309)='309 No points with radiation data could be found '
    strings(310)='310 The following file is missing: '//TRIM(ADJUSTL(arch(41)))
    strings(311)='311 Error reading the following file: '//TRIM(ADJUSTL(arch(41)))
    strings(312)='312 The row numbers in ' //TRIM(ADJUSTL(arch(41)))// ' does not match with the number of land use categories'
    strings(313)='313 The dynamic vegetation sub-model is not activated'
    strings(314)='314 The event file has a line which makes reference to vegetation sub-model, which is not activated'
    strings(315)='315 The following file is missing: '//TRIM(ADJUSTL(dirtra))//'Var-SCEUA_HidrVeg.txt'
    strings(316)='316 Error reading the the input file for automatic calibration of vegetation and hidrology'
    strings(317)='317 The following file is missing: '//TRIM(ADJUSTL(dirtra))//'xlb_veg-SCEUA.txt'
    strings(318)='318 Error reading the following file: '//TRIM(ADJUSTL(dirtra))//'xlb_veg-SCEUA.txt'
    strings(319)='319 The following file is missing: '//TRIM(ADJUSTL(dirtra))//'xub_veg-SCEUA.txt' 
    strings(320)='320 Error reading the following file: '//TRIM(ADJUSTL(dirtra))//'xub_veg-SCEUA.txt' 
    strings(321)='321 The following file is missing: '//TRIM(ADJUSTL(dirtra))//'xguess_veg-SCEUA.txt'  
    strings(322)='322 Error reading the following file: '//TRIM(ADJUSTL(dirtra))//'xguess_veg-SCEUA.txt' 
    strings(323)='323 The following file is missing: '//TRIM(ADJUSTL(dirtra))//'xchk_veg-SCEUA.txt' 
    strings(324)='324 Error reading the following file: '//TRIM(ADJUSTL(dirtra))//'xchk_veg-SCEUA.txt'
    !400 - Nitrógeno
    strings(400)='400 The following file is missing: '//TRIM(ADJUSTL(archnit(9)))
    strings(401)='401 Error reading the file '//TRIM(ADJUSTL(archnit(9)))
    strings(402)='402 The row numbers in ' //TRIM(ADJUSTL(archnit(9)))// ' does not match with the number of land use categories'
    strings(403)='403 The following file is missing: '//TRIM(ADJUSTL(archnit(8)))
    strings(404)='404 Error reading the file '//TRIM(ADJUSTL(archnit(8)))
    strings(405)='405 The row numbers -2 in ' //TRIM(ADJUSTL(archnit(8)))// ' does not match with the number of land use categories'
    strings(406)='406 Not enough data for computing nitrogen sub-model'
    strings(407)='407 The filesnit.txt file is missing'
    strings(408)='408 Error reading the filesnit.txt file'
    strings(409)='409 The following file is missing: '//TRIM(ADJUSTL(archnit(13)))
    strings(410)='410 Error reading the header of '//TRIM(ADJUSTL(archnit(13)))
    strings(411)='411 Error reading the following file: '//TRIM(ADJUSTL(archnit(13)))
    strings(412)='412 Error reading the file '//TRIM(ADJUSTL(archnit(7)))
    strings(413)='413 The following file is missing: '//TRIM(ADJUSTL(archnit(7)))
    strings(414)='414 The row numbers in ' //TRIM(ADJUSTL(archnit(7)))// ' does not match with the number of land use categories'
    strings(415)='415 The following file is missing: '//TRIM(ADJUSTL(archnit(15)))
    strings(416)='416 Error reading the file '//TRIM(ADJUSTL(archnit(15)))
    strings(417)='417 The row numbers in cultantec_val.txt does not match with the number of land use categories'
    strings(418)='418 The following file is missing: '//TRIM(ADJUSTL(archnit(16)))
    strings(419)='419 Error reading the file '//TRIM(ADJUSTL(archnit(16)))
    strings(420)='420 The following file is missing: '//TRIM(ADJUSTL(arch(40)))
    strings(421)='421 Error reading the file '//TRIM(ADJUSTL(arch(40)))
    strings(422)='422 The following file is missing: '//TRIM(ADJUSTL(archnit(17)))
    strings(423)='423 Error reading the file '//TRIM(ADJUSTL(archnit(17)))
    strings(424)='424 The following file is missing: '//TRIM(ADJUSTL(archnit(18)))
    strings(425)='425 Error reading the file '//TRIM(ADJUSTL(archnit(18)))    
    strings(426)='426 The following file is missing: '//TRIM(ADJUSTL(archnit(10)))
    strings(427)='427 The following file is missing: '//TRIM(ADJUSTL(archnit(11)))    
    strings(428)='428 Error reading the file '//TRIM(ADJUSTL(archnit(10)))
    strings(429)='429 Error reading the file '//TRIM(ADJUSTL(archnit(11)))    
    strings(430)='430 The row numbers in ' //TRIM(ADJUSTL(archnit(10)))// ' does not match with the number of land use categories'
    strings(431)='431 The row numbers in ' //TRIM(ADJUSTL(archnit(11)))// ' does not match with the number of land use categories'    
    strings(432)='432 The nitrogen sub-model is not activated'
    strings(433)='433 The following file is missing: '//TRIM(ADJUSTL(dirtra))//'xguess_nitr-SCEUA.txt'
    strings(434)='434 Error reading the file '//TRIM(ADJUSTL(dirtra))//'xguess_nitr-SCEUA.txt'
    strings(435)='435 The following file is missing: '//TRIM(ADJUSTL(dirtra))//'xub_nitr-SCEUA.txt'
    strings(436)='436 Error reading the file '//TRIM(ADJUSTL(dirtra))//'xub_nitr-SCEUA.txt'
    strings(437)='437 The following file is missing: '//TRIM(ADJUSTL(dirtra))//'xlb_nitr-SCEUA.txt'
    strings(438)='438 Error reading the file '//TRIM(ADJUSTL(dirtra))//'xlb_nitr-SCEUA.txt'
    strings(439)='439 The following file is missing: '//TRIM(ADJUSTL(dirtra))//'xchk_nitr-SCEUA.txt'
    strings(440)='440 Error reading the file '//TRIM(ADJUSTL(dirtra))//'xchk_nitr-SCEUA.txt'
    strings(441)='441 The event file has a line which makes reference to nitrogen sub-model, which is not activated'
    strings(442)='442 The following file is missing: '//TRIM(ADJUSTL(dirtra))//'Var-SCEUA_Nitr.txt'
    strings(443)='443 Error reading the file '//TRIM(ADJUSTL(dirtra))//'Var-SCEUA_Nitr.txt'
    !500 - Input - errores de filas
    strings(501)='501 Error reading the G line of '//TRIM(ADJUSTL(arch(5)))
    strings(502)='502 Error reading the N line of '//TRIM(ADJUSTL(arch(5)))
    strings(503)='503 Error reading the V line of '//TRIM(ADJUSTL(arch(5)))
    strings(504)='504 Error reading the P line of '//TRIM(ADJUSTL(arch(5)))
    strings(505)='505 Error reading the S line of '//TRIM(ADJUSTL(arch(5)))
    strings(506)='506 Error reading the Q line of '//TRIM(ADJUSTL(arch(5)))
    strings(507)='507 Error reading the B line of '//TRIM(ADJUSTL(arch(5)))
    strings(508)='508 Error reading the T line of '//TRIM(ADJUSTL(arch(5)))
    strings(509)='509 Error reading the C line of '//TRIM(ADJUSTL(arch(5)))
    strings(510)='510 Error reading the E line of '//TRIM(ADJUSTL(arch(5)))
    strings(511)='511 Error reading the I line of '//TRIM(ADJUSTL(arch(5)))
    strings(512)='512 Error reading the F line of '//TRIM(ADJUSTL(arch(5)))
    strings(513)='513 Error reading the H line of '//TRIM(ADJUSTL(arch(5)))
    strings(514)='514 Error reading the X line of '//TRIM(ADJUSTL(arch(5)))
    strings(515)='515 Error reading the W line of '//TRIM(ADJUSTL(arch(5)))
    strings(516)='516 Error reading the R line of '//TRIM(ADJUSTL(arch(5)))
    strings(517)='517 Error reading the DA line of '//TRIM(ADJUSTL(arch(5))) !Cris
    strings(518)='518 Error reading the DL line of '//TRIM(ADJUSTL(arch(5)))
    strings(519)='519 Error reading the DC line of '//TRIM(ADJUSTL(arch(5)))
    !Mensajes a pantalla
    strings(800)='ERRORS were found!!!'
    strings(801)='End of simulation with TETIS 9'
    strings(802)='* Description of the errors and warnings found - TETIS '    
    strings(803)='End of the HANTEC.EXE module'
    strings(804)='Computing the hydrological initial state'
    strings(805)='Computing the sediment initial state'
    strings(806)='End of the SEDANTECS.EXE module'
    strings(807)='The chosen dates are incoherents'
    strings(808)='The clipped input file has been created'
    strings(809)='Homogeneous region code = 0'
    strings(810)='Interpola los valores de altura de agua equivalente'
    strings(811)='End of the INTNIEVES.EXE module'
    strings(812)='More than one arguments have been found'
    strings(813)='The argument is not -a= '
    strings(814)='The argument is not -l= '
    strings(815)='Defining the perimeter of the area covered by snow above an altitude'
    strings(816)='Generating control points'
    strings(817)='Computing the topographical and pedological information file, please wait...'
    strings(818)='Define las alturas equivalentes de nieve de la region previamente definida'
    strings(819)='End of the module for changing from column to row format'
    strings(820)='Please wait...'
    strings(821)='Total number of function evaluation '
    strings(822)='Optimization results'
    strings(823)='Fact. FNnorain'
    strings(824)='Fact. FNwithrain'
    strings(825)='Objective function'
    strings(826)='The automatic calibration module is ended' 
    strings(827)='Computing the nitrogen initial state'
    strings(828)='End of the NANTECS.EXE module'
    !900 - Misc
    strings(901)='901 Error cleaning the dynamic memory'
    strings(902)='902 The Modified Pulse method is employed for reservoir modelling'
    strings(903)='903 Some cells inside the catchment have slope = 0. It has been modified to 0.001'
    strings(904)='904 Please check the reservoir Volume/Level curve'
    strings(905)='905 Reservoir volume out of the range'
    strings(906)='906 Please check the reservoir Volume/Level curve'
    strings(907)='907 Not enough data for computing snow melting'    
    strings(908)='908 No equivalent snow depth gauging stations could be found'
    strings(909)='909 Reservoir volume < 0. It has been modified to 0.0'
    strings(910)='910 Reservoir volume out of the range'
    strings(911)='911 Please check the reservoir Discharge/Level curve'
    strings(912)='912 Please check the reservoir Discharge/Level curve'
    strings(913)='913 Reservoir level < 0. It has been modified to 0.0'
    strings(914)='914 Reservoir level out of the range'
    strings(915)='915 The spring poing is not located on the stream network'
    strings(916)='916 The evapotranspiration index was modified to 1'
    strings(917)='917 Some cells inside the catchment have homogeneous region code = 0'
    strings(918)='918 Initial level of tank 3 is greater than the maximum allowed for some cells. It has been modified to the maximum possible level'
    strings(919)='919 The file for clipping the input file is missing'
    strings(920)='920 The file for clipping the cathcment up to reservoirs is missing'
    strings(921)='921 Error reading the following file: Recorta.txt'
    strings(922)='922 The event file has a line which makes reference to snow modul, which is not activated'
    strings(923)='923 The event file has a line which makes reference to reservoir modul, which is not activated'
    strings(924)='924 A T line has been found, and neither snow, nor nitrogen nor dinamic vegetation sub-model are activated' 
    endif
     
END SUBROUTINE

!Subrutina que cargar los nombres de los archivos de sedimentos en los mensajes de error correspondientes
SUBROUTINE labels_sed
    USE modtet
    IMPLICIT NONE
   if (lang.eq.1)then    
    !200 - Sedimentos
    strings(202)='202 No existe el fichero '//TRIM(ADJUSTL(archsed(7)))
    strings(203)='203 Lectura erronea en el encabezado de '//TRIM(ADJUSTL(archsed(7)))
    strings(204)='204 Lectura de datos erronea en '//TRIM(ADJUSTL(archsed(7)))
    strings(205)='205 El fichero '//TRIM(ADJUSTL(archsed(10)))//' no existe'
    strings(206)='206 Lectura de datos erronea en '//TRIM(ADJUSTL(archsed(10)))
    strings(207)='207 No existe el fichero '//TRIM(ADJUSTL(archsed(11)))
    strings(208)='208 Lectura erronea en el encabezado de '//TRIM(ADJUSTL(archsed(11)))
    strings(209)='209 Lectura de datos erronea en '//TRIM(ADJUSTL(archsed(11)))
    strings(210)='210 No existe el fichero '//TRIM(ADJUSTL(archsed(12)))
    strings(211)='211 No existe el fichero '//TRIM(ADJUSTL(archsed(8)))
    strings(212)='212 No existe el fichero '//TRIM(ADJUSTL(dirtra))//'Var-SCEUA_Sed.txt'
    strings(213)='213 Lectura erronea del fichero de input de la calibracion automatica de sedimentos'
    strings(214)='214 El evento presenta una linea referente al submodelo de sedimentos, que esta desactivado'
    strings(215)='215 No esta activado el submodelo de sedimentos'
   else if (lang.eq.2) then  
    strings(202)='202 The following file is missing: '//TRIM(ADJUSTL(archsed(7)))
    strings(203)='203 Error reading the header of '//TRIM(ADJUSTL(archsed(7)))
    strings(204)='204 Error reading the following file: '//TRIM(ADJUSTL(archsed(7)))
    strings(205)='205 The following file is missing: '//TRIM(ADJUSTL(archsed(10)))
    strings(206)='206 Error reading the following file: '//TRIM(ADJUSTL(archsed(10)))
    strings(207)='207 The following file is missing: '//TRIM(ADJUSTL(archsed(11)))
    strings(208)='208 Error reading the header of '//TRIM(ADJUSTL(archsed(11)))
    strings(209)='219 Error reading the following file: '//TRIM(ADJUSTL(archsed(11)))
    strings(210)='210 The following file is missing: '//TRIM(ADJUSTL(archsed(12)))
    strings(211)='211 The following file is missing: '//TRIM(ADJUSTL(archsed(8)))
    strings(212)='212 The following file is missing: '//TRIM(ADJUSTL(dirtra))//'Var-SCEUA_Sed.txt'
    strings(213)='213 Error reading the the input file for automatic calibration of the sediment sub-module'
    strings(214)='214 The event file has a line which makes reference to sediment sub-model, which is not activated'
    strings(215)='215 The sediment sub-model is not activated'   
   endif
   
END SUBROUTINE
 

!Subrutina que cargar los nombres de los archivos de vegetacion en los mensajes de error correspondientes    
SUBROUTINE labels_veg   
    USE modtet
    IMPLICIT NONE
    if (lang.eq.1)then 
        strings(300)='300 No existe el fichero '//TRIM(ADJUSTL(archveg(7)))
        strings(301)='301 Lectura errónea en el encabezado de '//TRIM(ADJUSTL(archveg(7)))
        strings(302)='302 Lectura de datos errónea en '//TRIM(ADJUSTL(archveg(7)))
    else if (lang.eq.2) then
        strings(300)='300 The following file is missing: '//TRIM(ADJUSTL(archveg(7)))
        strings(301)='301 Error reading the header of '//TRIM(ADJUSTL(archveg(7)))
        strings(302)='302 Error reading the following file: '//TRIM(ADJUSTL(archveg(7)))
   endif
   
END SUBROUTINE   
    
!Subrutina que cargar los nombres de los archivos de nitrogeno en los mensajes de error correspondientes    
SUBROUTINE labels_nitr
     USE modtet
    IMPLICIT NONE
    if (lang.eq.1)then
        strings(400)='400 No existe el fichero '//TRIM(ADJUSTL(archnit(9)))
        strings(401)='401 Lectura erronea del fichero '//TRIM(ADJUSTL(archnit(9)))
        strings(402)='402 El numero de filas en ' //TRIM(ADJUSTL(archnit(9)))// ' no coincide con el numero de categorias de cobertura vegetal'
        strings(403)='403 No existe el fichero '//TRIM(ADJUSTL(archnit(8)))
        strings(404)='404 Lectura erronea del fichero '//TRIM(ADJUSTL(archnit(8)))
        strings(405)='405 El numero de filas -2 en ' //TRIM(ADJUSTL(archnit(8)))// ' no coincide con el numero de categorias de cobertura vegetal'
        strings(406)='406 Faltan datos de temperatura para evaluar el submodelo de nitrogeno '
        strings(407)='407 El fichero filesnit.txt no existe'
        strings(408)='408 Lectura erronea en alguna linea de filesnit.txt '
        strings(409)='409 No existe el fichero '//TRIM(ADJUSTL(archnit(13)))
        strings(410)='410 Lectura erronea en el encabezado de '//TRIM(ADJUSTL(archnit(13)))
        strings(411)='411 Lectura de datos erronea en '//TRIM(ADJUSTL(archnit(13)))
        strings(412)='412 Lectura erronea del fichero '//TRIM(ADJUSTL(archnit(7)))
        strings(413)='413 No existe el fichero '//TRIM(ADJUSTL(archnit(7)))
        strings(414)='414 El numero de filas en ' //TRIM(ADJUSTL(archnit(7)))// ' no coincide con el numero de categorias de cobertura vegetal'
        strings(415)='415 No existe el fichero '//TRIM(ADJUSTL(archnit(15)))
        strings(416)='416 Lectura erronea del fichero '//TRIM(ADJUSTL(archnit(15)))
        strings(417)='417 El numero de filas en cultantec_val.txt no coincide con el numero de categorias de cobertura vegetal'
        strings(418)='418 No existe el fichero '//TRIM(ADJUSTL(archnit(16)))
        strings(419)='419 Lectura erronea del fichero '//TRIM(ADJUSTL(archnit(16)))
        strings(420)='420 No existe el fichero '//TRIM(ADJUSTL(arch(40)))
        strings(421)='421 Lectura erronea del fichero '//TRIM(ADJUSTL(arch(40)))
        strings(422)='422 No existe el fichero '//TRIM(ADJUSTL(archnit(17)))
        strings(423)='423 Lectura erronea del fichero '//TRIM(ADJUSTL(archnit(17)))
        strings(424)='424 No existe el fichero '//TRIM(ADJUSTL(archnit(18)))
        strings(425)='425 Lectura erronea del fichero '//TRIM(ADJUSTL(archnit(18)))
        strings(426)='426 No existe el fichero '//TRIM(ADJUSTL(archnit(10)))
        strings(427)='427 No existe el fichero '//TRIM(ADJUSTL(archnit(11)))
        strings(428)='428 Lectura erronea del fichero '//TRIM(ADJUSTL(archnit(10)))
        strings(429)='429 Lectura erronea del fichero '//TRIM(ADJUSTL(archnit(11)))
        strings(430)='430 El numero de filas en ' //TRIM(ADJUSTL(archnit(10)))// ' no coincide con el numero de categorias de cobertura vegetal'
        strings(431)='431 El numero de filas en ' //TRIM(ADJUSTL(archnit(11)))// ' no coincide con el numero de categorias de cobertura vegetal'    
        strings(432)='432 No esta activado el submodelo de nitrogeno'
        strings(433)='433 No existe el fichero '//TRIM(ADJUSTL(dirtra))//'xguess_nitr-SCEUA.txt'
        strings(434)='434 Lectura erronea del fichero '//TRIM(ADJUSTL(dirtra))//'xguess_nitr-SCEUA.txt'
        strings(435)='435 No existe el fichero '//TRIM(ADJUSTL(dirtra))//'xub_nitr-SCEUA.txt'
        strings(436)='436 Lectura erronea del fichero '//TRIM(ADJUSTL(dirtra))//'xub_nitr-SCEUA.txt'
        strings(437)='437 No existe el fichero '//TRIM(ADJUSTL(dirtra))//'xlb_nitr-SCEUA.txt'
        strings(438)='438 Lectura erronea del fichero '//TRIM(ADJUSTL(dirtra))//'xlb_nitr-SCEUA.txt'
        strings(439)='439 No existe el fichero '//TRIM(ADJUSTL(dirtra))//'xchk_nitr-SCEUA.txt'
        strings(440)='440 Lectura erronea del fichero '//TRIM(ADJUSTL(dirtra))//'xchk_nitr-SCEUA.txt'
        strings(441)='441 El evento presenta una linea referente al submodelo de nitrogeno, que esta desactivado'
        strings(442)='442 No existe el fichero '//TRIM(ADJUSTL(dirtra))//'Var-SCEUA_Nitr.txt'
        strings(443)='443 Lectura erronea del fichero '//TRIM(ADJUSTL(dirtra))//'Var-SCEUA_Nitr.txt'
    else if (lang.eq.2) then  
        strings(400)='400 The following file is missing: '//TRIM(ADJUSTL(archnit(9)))
        strings(401)='401 Error reading the file '//TRIM(ADJUSTL(archnit(9)))
        strings(402)='402 The row numbers in ' //TRIM(ADJUSTL(archnit(9)))// ' does not match with the number of land use categories'
        strings(403)='403 The following file is missing: '//TRIM(ADJUSTL(archnit(8)))
        strings(404)='404 Error reading the file '//TRIM(ADJUSTL(archnit(8)))
        strings(405)='405 The row numbers -2 in ' //TRIM(ADJUSTL(archnit(8)))// ' does not match with the number of land use categories'
        strings(406)='406 Not enough data for computing nitrogen sub-model'
        strings(407)='407 The filesnit.txt file is missing'
        strings(408)='408 Error reading the filesnit.txt file'
        strings(409)='409 The following file is missing: '//TRIM(ADJUSTL(archnit(13)))
        strings(410)='410 Error reading the header of '//TRIM(ADJUSTL(archnit(13)))
        strings(411)='411 Error reading the following file: '//TRIM(ADJUSTL(archnit(13)))
        strings(412)='412 Error reading the file '//TRIM(ADJUSTL(archnit(7)))
        strings(413)='413 The following file is missing: '//TRIM(ADJUSTL(archnit(7)))
        strings(414)='414 The row numbers in ' //TRIM(ADJUSTL(archnit(7)))// ' does not match with the number of land use categories'
        strings(415)='415 The following file is missing: '//TRIM(ADJUSTL(archnit(15)))
        strings(416)='416 Error reading the file '//TRIM(ADJUSTL(archnit(15)))
        strings(417)='417 The row numbers in cultantec_val.txt does not match with the number of land use categories'
        strings(418)='418 The following file is missing: '//TRIM(ADJUSTL(archnit(16)))
        strings(419)='419 Error reading the file '//TRIM(ADJUSTL(archnit(16)))
        strings(420)='420 The following file is missing: '//TRIM(ADJUSTL(arch(40)))
        strings(421)='421 Error reading the file '//TRIM(ADJUSTL(arch(40)))
        strings(422)='422 The following file is missing: '//TRIM(ADJUSTL(archnit(17)))
        strings(423)='423 Error reading the file '//TRIM(ADJUSTL(archnit(17)))
        strings(424)='424 The following file is missing: '//TRIM(ADJUSTL(archnit(18)))
        strings(425)='425 Error reading the file '//TRIM(ADJUSTL(archnit(18)))    
        strings(426)='426 The following file is missing: '//TRIM(ADJUSTL(archnit(10)))
        strings(427)='427 The following file is missing: '//TRIM(ADJUSTL(archnit(11)))    
        strings(428)='428 Error reading the file '//TRIM(ADJUSTL(archnit(10)))
        strings(429)='429 Error reading the file '//TRIM(ADJUSTL(archnit(11)))    
        strings(430)='430 The row numbers in ' //TRIM(ADJUSTL(archnit(10)))// ' does not match with the number of land use categories'
        strings(431)='431 The row numbers in ' //TRIM(ADJUSTL(archnit(11)))// ' does not match with the number of land use categories'    
        strings(432)='432 The nitrogen sub-model is not activated'
        strings(433)='433 The following file is missing: '//TRIM(ADJUSTL(dirtra))//'xguess_nitr-SCEUA.txt'
        strings(434)='434 Error reading the file '//TRIM(ADJUSTL(dirtra))//'xguess_nitr-SCEUA.txt'
        strings(435)='435 The following file is missing: '//TRIM(ADJUSTL(dirtra))//'xub_nitr-SCEUA.txt'
        strings(436)='436 Error reading the file '//TRIM(ADJUSTL(dirtra))//'xub_nitr-SCEUA.txt'
        strings(437)='437 The following file is missing: '//TRIM(ADJUSTL(dirtra))//'xlb_nitr-SCEUA.txt'
        strings(438)='438 Error reading the file '//TRIM(ADJUSTL(dirtra))//'xlb_nitr-SCEUA.txt'
        strings(439)='439 The following file is missing: '//TRIM(ADJUSTL(dirtra))//'xchk_nitr-SCEUA.txt'
        strings(440)='440 Error reading the file '//TRIM(ADJUSTL(dirtra))//'xchk_nitr-SCEUA.txt'
        strings(441)='441 The event file has a line which makes reference to nitrogen sub-model, which is not activated'
        strings(442)='442 The following file is missing: '//TRIM(ADJUSTL(dirtra))//'Var-SCEUA_Nitr.txt'
        strings(443)='443 Error reading the file '//TRIM(ADJUSTL(dirtra))//'Var-SCEUA_Nitr.txt'
    end if
    
END SUBROUTINE    
    
    
!Subrutina que escribe a pantalla la fecha y la hora
SUBROUTINE write_date
USE modtet
    IF (lang.eq.1) THEN
        WRITE(*,*)'Fecha:'//dia(7:8)//'/'//dia(5:6)//'/'//dia(1:4)//' Hora:'//hora(1:2)//':'//hora(3:4)//':'//hora(5:10)
    ELSE IF (lang.eq.2) THEN
       WRITE(*,*)'Date:'//dia(7:8)//'/'//dia(5:6)//'/'//dia(1:4)//' Hour:  '//hora(1:2)//':'//hora(3:4)//':'//hora(5:8)
    ENDIF
END SUBROUTINE
    

!subrutina que define todos los mensajes del fichero de output (sección almacenamientos) 
!solo se llama una vez cuando se ejecuta la subrutina "almacenamientos"

SUBROUTINE labels_almac
USE modtet
IMPLICIT NONE
    if (lang.eq.1)then
        almac(1)= '* RESUMEN DE ALMACENAMIENTOS Y FLUJOS MEDIOS EN LA CUENCA'
        almac(2)= 'BL Precipitación              [mm]:'
        almac(3)= 'BL ET0 total                  [mm]:'
        almac(4)= '* Cubierta vegetal                   :'
        almac(5)= 'BL Nivel medio en la cuenca  [mm]: '
        almac(6)= 'BL LLluvia                   [mm]: '
        almac(7)= 'BL Evaporación desde interc. [mm]: '
        almac(8)= '* Suelo (alm. estatico)   :'
        almac(9)= 'BL Nivel medio en la cuenca  [mm]: '
        almac(10)= 'BL Lluvia directa            [mm]: '
        almac(11)= 'BL Evapotranspiración        [mm]: '
        almac(12)= '* Agua en superficie:'
        almac(13)= 'BL Nivel medio en la cuenca  [mm]: '
        almac(14)= 'BL Excedente                 [mm]: '
        almac(15)= 'BL Escorrentia directa       [mm]: '
        almac(16)=  '* Almacenamiento gravitacional: '
        almac(17)= 'BL Nivel medio en la cuenca  [mm]: '
        almac(18)= 'BL Infiltracion              [mm]: '
        almac(19)= 'BL Interflujo                [mm]: '
        almac(20)= '* Acuifero          :'
        almac(21)= 'BL Nivel medio en la cuenca  [mm]: '
        almac(22)= 'BL Percolacion               [mm]: '
        almac(23)= 'BL Flujo subt. conectado     [mm]: '
        almac(24)= 'BL Flujo subt. profundo      [mm]: '
        almac(25)= 'BL Caudal de salida de la cuenca   [mm]:'
        almac(26)= '* Error en Balance (Salidas - Entradas + Cambio Almacenamiento / Entradas) [%]: '
        almac(27)= '* Salidas   [Hm3]:'
        almac(28)= '* Entradas  [Hm3]:'
        almac(29)= '* Cambio Almacenamiento  [Hm3]:'
        almac(30)= '* Incremento/Decremento flujo karstico (mm):'
    else if (lang.eq.2) then
        almac(1)= '* RESUME OF AVERAGE FLOWS AND STORAGE WITHIN THE CATCH. '
        almac(2)= 'BL Precipitation              [mm]:'
        almac(3)= 'BL ET0 total                  [mm]:'
        almac(4)= '* Vegetation cover                   :'
        almac(5)= 'BL Mean level in catchment   [mm]: '
        almac(6)= 'BL Rainfall                  [mm]: '
        almac(7)= 'BL Evaporation from interc.  [mm]: '
        almac(8)= '* Soil  (static storage)  :'
        almac(9)= 'BL Mean level in catchment   [mm]: '
        almac(10)= 'BL Direct rainfall           [mm]: '
        almac(11)= 'BL EEvapotranspiration       [mm]: '
        almac(12)= '* Surface water:     '
        almac(13)= 'BL Mean level in catchment   [mm]: '
        almac(14)= 'BL Excess water              [mm]: '
        almac(15)= 'BL Direct runoff             [mm]: '
        almac(16)=  '* Gravitational storage       : '
        almac(17)= 'BL Mean level in catchment   [mm]: '
        almac(18)= 'BL Infiltration              [mm]: '
        almac(19)= 'BL Interflow                 [mm]: '
        almac(20)= '* Aquifer           :'
        almac(21)= 'BL Mean level in catchment   [mm]: '
        almac(22)= 'BL Percolation               [mm]: '
        almac(23)= 'BL Connected sub. flow      [mm]: '
        almac(24)= 'BL Deep subterranean flow    [mm]: '
        almac(25)= 'BL Outlet discharge                [mm]:'
        almac(26)= '* Balance aerror   (Outflows- Inflows  + Storage variations    / Inflows ) [%]: '
        almac(27)= '* Inflows   [Hm3]:'
        almac(28)= '* Outflows  [Hm3]:'
        almac(29)= '* Storage variations     [Hm³]:'
        almac(30)= '* Increment /Decline    karstic flow   (mm):'
    endif
END SUBROUTINE


    
!SUBROUTINE errores(errr,mensaje,lang)
!!USE DFLIB
!IMPLICIT NONE
!CHARACTER mensaje*200,hora*10,dia*8,artem*128,dirtra*128,genericpath*512,path_separator*1
!integer ldirtra,lmensaje,errr,lang
!
!!Se determina el separador de los Paths según el Sistema Operativo en el que se ejecute Tetis
!CALL GET_ENVIRONMENT_VARIABLE('PATH',genericpath)
!path_separator=genericpath(1:1)!Se extra el primer caracter del path del entorno.
!IF(path_separator /= '/') path_separator = '\'
!
!!Lee nombres de los ficheros a utilizar
!OPEN(9,file='filessp.txt')
!READ(9,'(a128)') dirtra
!ldirtra=len_trim(dirtra)
!!if(dirtra(ldirtra:ldirtra).ne.'\')dirtra(ldirtra+1:ldirtra+1)='\'
!if(dirtra(ldirtra:ldirtra).ne.path_separator) dirtra(ldirtra+1:ldirtra+1)=path_separator
!CLOSE(9)
!
!artem=TRIM(ADJUSTL(dirtra))//'~errores.txt'
!
!lmensaje=len_trim(mensaje)
!
!OPEN(8,FILE=artem,ACCESS='APPEND')
!CALL DATE_AND_TIME(dia,hora)
!IF (lang.eq.1) THEN
!    WRITE(8,*)'* Fecha:'//dia(7:8)//'/'//dia(5:6)//'/'//dia(1:4)//' Hora:'//hora(1:2)//':'//hora(3:4)//':'//hora(5:10)
!ELSE IF (lang.eq.2) THEN
!    WRITE(8,*)'* Date:'//dia(7:8)//'/'//dia(5:6)//'/'//dia(1:4)//' Hour:'//hora(1:2)//':'//hora(3:4)//':'//hora(5:10)
!ENDIF
!WRITE(*,*)mensaje !Incluimos la escritura por pantalla de los errores
!WRITE(8,18)errr, mensaje
!CLOSE(8)
!
!18 FORMAT(I2,3x,a<lmensaje>)
!
!END SUBROUTINE
    
    
SUBROUTINE errores
USE modtet
IMPLICIT NONE
!CHARACTER mensaje*200
!integer lmensaje,errr,lang


artem=TRIM(ADJUSTL(dirtra))//'~errores.txt'

lmensaje=len_trim(mensaje)

OPEN(8,FILE=artem,status="old", position="append")
CALL DATE_AND_TIME(dia,hora)
IF (lang.eq.1) THEN
    WRITE(8,*)'* Fecha:'//dia(7:8)//'/'//dia(5:6)//'/'//dia(1:4)//' Hora:'//hora(1:2)//':'//hora(3:4)//':'//hora(5:10)
ELSE IF (lang.eq.2) THEN
    WRITE(8,*)'* Date:'//dia(7:8)//'/'//dia(5:6)//'/'//dia(1:4)//' Hour:'//hora(1:2)//':'//hora(3:4)//':'//hora(5:10)
ENDIF
WRITE(*,*)mensaje !Incluimos la escritura por pantalla de los errores
WRITE(8,18)errr, mensaje
CLOSE(8)

18 FORMAT(I2,3x,a<lmensaje>)

END SUBROUTINE    
    