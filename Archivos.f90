
      
!!*********************************************************************
!!* Nombre que se le da a cada uno de los ficheros
!!*********************************************************************
!
!  dirtra=TRIM(ADJUSTL(dir))//'Ejemplo\'
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
!     archveg(1)='hstar.asc                   '
!     archveg(2)='dc.asc                      '
!     archveg(3)='rs.asc                      '
!     archveg(4)='vegantec.sds                '
!     archveg(5)='ins.asc                     '
!     archsed(1)='cusle.asc                   '
!     archsed(2)='kusle.asc                   '
!     archsed(3)='pusle.asc                   '
!     archsed(4)='sand.asc                    '
!     archsed(5)='silt.asc                    '
!     archsed(6)='clay.asc                    '
!     archsed(7)='sedantec.sds                '
!     archsed(8)='sedfinal.sds                '
!     archsed(9)='series_sed.txt              '
!     archsed(10)='azudes.txt                 '
!     archsed(11)='curvasHVsed.txt            '
!     archsed(12)='estado_embalses_ini.txt    '
!     archsed(13)='estado_embalses_fin.txt    '


!****************************************************************************************
!* Esta subrutina es para borrar los ficheros Temporales e innecesarios
!****************************************************************************************
SUBROUTINE  borrafic(checked)
!USE DFLIB
!USE PORTLIB	
USE modtet
IMPLICIT NONE

LOGICAL(KIND=4)checked
      
!Se elimina los archivos sin preguntar(Vicente. Nov 2019)
!message=MESSAGEBOXQQ('¿Desea eliminar TODOS los ficheros temporales? (NO se eliminará ni el TOPOLCO.SDS ni el fichero de C. INICIALES)'C, &
          !'Borrando...'C,    &
          !MB$ICONQUESTION.OR.MB$YESNO.OR.MB$DEFBUTTON1)!aparece una ventana donde te pregunta este mensaje y message se pone = a 6

!IF (message.eq.6) THEN
  !Lee nombres de los ficheros a utilizar
  CALL lecfiles(dirtra,arch,resul)

  !artem=TRIM(ADJUSTL(dirtra))//'~*.*'
  !resul=DELFILESQQ(artem)
  !artem=arch(21) !file CONTROL.txt
  !resul=DELFILESQQ(artem)
  !artem=arch(8) !hantec2.sds
  !resul=DELFILESQQ(artem)
  !artem=arch(10) !Nieve.asc
  !resul=DELFILESQQ(artem)
  !artem=arch(11) !nieve2.asc
  !resul=DELFILESQQ(artem)
  !Subtituimos la rutina de eliminar fichero para no emplear librerias(Vicente.Nov-2019)
  CALL deletefich(TRIM(ADJUSTL(dirtra))//'~*.*')
  CALL deletefich(arch(21))
  CALL deletefich(arch(8))
  CALL deletefich(arch(10))
  CALL deletefich(arch(11))
!ENDIF

RETURN !Functions are terminated by the return statement instead of stop. 
END SUBROUTINE borrafic


!****************************************************************************************
!* Esta subrutina es para Conservar el conjunto de FC (Calib.txt y Paramgeo.txt)
!*  anteriores en caso que la ultima ejecución sea mas mala que la penultima
!****************************************************************************************
SUBROUTINE conserva_fic 
!USE DFLIB
USE modtet
IMPLICIT NONE

!Lee nombres de los ficheros a utilizar
CALL lecfiles(dirtra,arch,resul)

  artem=TRIM(ADJUSTL(dirtra))//'Old-paramgeo.txt'
  CALL escri_parg

  artem=TRIM(ADJUSTL(dirtra))//'Old-calib.txt'
  CALL escri_calib
  
  artem=TRIM(ADJUSTL(dirtra))//'Old-settings.txt'
  CALL escri_settings

RETURN
    END SUBROUTINE

!****************************************************************************************
! Nov - 2019 (Vicente)     
!* Esta subrutina es para eliminar un fichero
!* Su uso viene a sustituir a DELFILESQQ, evitando el empleo de librerias externas
!****************************************************************************************
    
SUBROUTINE  deletefich(rutafich)
USE modtet
IMPLICIT NONE

CHARACTER (*) rutafich  
    
ios=0
OPEN(1234, iostat=ios, file=rutafich, status='old')
if (ios == 0) close(1234, status='delete')

RETURN 
END SUBROUTINE deletefich