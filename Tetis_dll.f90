!*****************************************************************************
!*  MODELO DE SIMULACION DE EVENTOS DE CRECIDA. ++ TETIS v8.2 ++
!*  Ultima actualización: Agosto de 2011
!*
!*****************************************************************************
Subroutine tetisR
!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : "tetisR_" :: tetisR
    !USE DFLIB
    USE IFPORT
    USE modtet
    implicit none
    call tetis
    res=SYSTEM('cp "' // TRIM(ADJUSTL(dirtra))//TRIM(ADJUSTL('~errores.txt')) // '" "' // TRIM(ADJUSTL(dirtra))//TRIM(ADJUSTL('errores.txt')) // '" /Y')
    !res=DELFILESQQ(TRIM(ADJUSTL(dirtra))//TRIM(ADJUSTL('~errores.txt')))
    !Subtituimos la rutina para no emplear librerias(Vicente.Nov-2019)
    CALL deletefich(TRIM(ADJUSTL(dirtra))//TRIM(ADJUSTL('~errores.txt')))
    call fin_ejec
END Subroutine tetisR