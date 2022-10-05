!*****************************************************************************
!*  MODELO DE SIMULACION DE EVENTOS DE CRECIDA. ++ TETIS v 8.3++
!*  Ultima actualización: Julio 2016
!*****************************************************************************
Program P_tetis
    !USE DFLIB
    USE IFPORT
    USE modtet  !
    implicit none
    INTEGER nargu,ist
    CHARACTER*30 arg
    character copyFicheros*256

    !Se hace así para que si hay error, lang tenga un valor y pueda escribir. Por defecto está en inglés
    lang=2
    !llama la subrutina que define los mensajes de error genéricos, los que no neceistan más información que los nombres de archivo
    CALL labels
    
    call tetis
    if(stma_op==0) then
        !copyFicheros = 'cp "' // TRIM(ADJUSTL(dirtra))//TRIM(ADJUSTL('~errores.txt')) // '" "' // TRIM(ADJUSTL(dirtra))//TRIM(ADJUSTL('errores.txt')) // '" /Y' !Linux
        copyFicheros = '[ -f ' // TRIM(ADJUSTL(dirtra))//TRIM(ADJUSTL('~errores.txt')) //' ] && cp ' // TRIM(ADJUSTL(dirtra))//TRIM(ADJUSTL('~errores.txt')) // ' ' // TRIM(ADJUSTL(dirtra))//TRIM(ADJUSTL('errores.txt')) !Linux
    else
        copyFicheros = 'copy "' // TRIM(ADJUSTL(dirtra))//TRIM(ADJUSTL('~errores.txt')) // '" "' // TRIM(ADJUSTL(dirtra))//TRIM(ADJUSTL('errores.txt')) // '" /Y' !Windows
    end if
    res = SYSTEM(copyFicheros)    
    !res=DELFILESQQ(TRIM(ADJUSTL(dirtra))//TRIM(ADJUSTL('~errores.txt')))
    !Subtituimos la rutina de eliminar fichero para no emplear librerias(Vicente.Nov-2019)
    CALL deletefich(TRIM(ADJUSTL(dirtra))//TRIM(ADJUSTL('~errores.txt')))
    call fin_ejec
94 END Program P_tetis
   
   
   


