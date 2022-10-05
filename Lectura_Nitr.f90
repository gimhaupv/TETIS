! ***********************************************************************
! ***********************************************************************
! **Subrutinas de lectura de los ficheros exclusivos de TETIS Nitrógeno**
! ***********************************************************************
! ***********************************************************************

!***Lee el nombre de los archivos de nitrógeno***
SUBROUTINE leearchnit(dirtra_,archnit_,sale_)
    USE modtet
    IMPLICIT NONE

    INTEGER sale_ 
    CHARACTER archnit_*128(18),dirtra_*128

    sale_=0

    OPEN(9,file='filesnit.txt',status='old',err=200)
    DO i=1,18
      READ(9,*,err=201)archnit_(i)
      archnit_(i)=TRIM(ADJUSTL(dirtra_))//TRIM(ADJUSTL(archnit_(i)))
    ENDDO
    CLOSE(9)
    CALL labels_nitr !llama la subrutina que define los mensajes de error con la ruta de los ficheros de nitrogeno
    
    GOTO 95

    200 mensaje=strings(407)
    errr=1
    CALL errores
    GOTO 94
    201 mensaje=strings(408)
    errr=1
    CALL errores
    GOTO 94

    94 WRITE(*,*)strings(800)
    sale=2

95  END SUBROUTINE

!***Lee fichero con el factor de cubierta para separar transpiración de evaporación directa de suelo desnudo***
!***Básicamente corresponde al porcentaje de la evapotranspiración que es transpiración por parte de la vegetación***
SUBROUTINE lee_fcubiertan
    USE modtet
    IMPLICIT NONE
    
    sale=0
    
    k=0
    OPEN(10,file=archnit(9),status='old',err=248)
    ios=0
    DO WHILE (ios.ne.-1)
      READ(10,*,iostat=ios) j
      IF (ios.ne.-1) k=k+1
    ENDDO
    REWIND(10)
    IF (ALLOCATED(fcn)) DEALLOCATE(fcn)
    ALLOCATE(fcn(k,12))
      fcn=-99
      DO i=1,k
        READ(10,*,err=249)(fcn(i,j),j=1,12)  
      ENDDO
    CLOSE(10)
    GOTO 111

    248 mensaje=strings(400)
    errr=1
    CALL errores
    GOTO 94
    249 mensaje=strings(401)
    errr=1
    CALL errores
    GOTO 94

    94  WRITE(*,*)strings(800)
    sale=2
111 END SUBROUTINE
 

!***Lee el fichero de parámetros de nitrógeno. Está separado de calib, porque aquí son parámetros (no factores correctores) y porque hay un conjunto por usos del suelo***
!***Orden: nitrificación, desnitrificación, mineralización, inmovilización, fijación***
!***En la penúltima línea se escriben las constantes de mineralización, nitrificación y desnitrificación para cauces***
!***En la última línea se escriben los parámetros relacionados con la corrección de los procesos por temperatura***
!***Kd a pesar de ser parámetro no se escribe aquí porque es un mapa, hay que ver si se le añade un factor corrector para todo el mapa***
SUBROUTINE lee_calibnit
    USE modtet
    IMPLICIT NONE

    sale=0
    
    k=0
    OPEN(10,file=archnit(8),status='old',err=248)
    ios=0
    DO WHILE (ios.ne.-1)
      READ(10,*,iostat=ios) j
      IF (ios.ne.-1) k=k+1
    ENDDO
    REWIND(10)
    cantUsSueNitr = k-2 !Vicente
    IF (ALLOCATED(kmin)) DEALLOCATE(kmin)
    IF (ALLOCATED(kinm)) DEALLOCATE(kinm)
    IF (ALLOCATED(kfi)) DEALLOCATE(kfi)
    IF (ALLOCATED(knit)) DEALLOCATE(knit)
    IF (ALLOCATED(kdes)) DEALLOCATE(kdes)
    IF (ALLOCATED(F)) DEALLOCATE(F)
    IF (ALLOCATED(Ndem)) DEALLOCATE(Ndem)
    IF (ALLOCATED(PrefNO3)) DEALLOCATE(PrefNO3)
    IF (ALLOCATED(kmin2)) DEALLOCATE(kmin2)
    IF (ALLOCATED(kinm2)) DEALLOCATE(kinm2)
    IF (ALLOCATED(kfi2)) DEALLOCATE(kfi2)
    IF (ALLOCATED(knit2)) DEALLOCATE(knit2)
    IF (ALLOCATED(kdes2)) DEALLOCATE(kdes2)
    IF (ALLOCATED(F2)) DEALLOCATE(F2)
    IF (ALLOCATED(Ndem2)) DEALLOCATE(Ndem2)
    ALLOCATE(kmin(k),kinm(k),kvol(k),knit(k),kfi(k),kdes(k),F(k),Ndem(k),PrefNO3(k))
    ALLOCATE(kmin2(k),kinm2(k),kvol2(k),knit2(k),kfi2(k),kdes2(k),F2(k),Ndem2(k))
    kmin=-99
    kinm=-99
    kvol=-99
    knit=-99
    kfi=-99
    kdes=-99
    F=-99
    Ndem=-99
    PrefNO3=-99

    kmin2=-99
    kinm2=-99
    kvol2=-99
    knit2=-99
    kfi2=-99
    kdes2=-99
    F2=-99
    Ndem2=-99
    DO i=1,k-2
        READ(10,*,err=249)kmin2(i),kinm2(i),kvol2(i),knit2(i),kfi2(i),kdes2(i),F2(i),Ndem2(i),PrefNO3(i)
    ENDDO
    READ(10,*,err=249)kminc2,knitc2,kdesc2
    READ(10,*,err=249)mtd,tethas,topts,tethac,toptc,fckd   
    
    CLOSE(10)
    GOTO 111

    248 mensaje=strings(403)
    errr=1
    CALL errores
    GOTO 94
    249 mensaje=strings(404)
    errr=1
    CALL errores
    GOTO 94

    94  WRITE(*,*)strings(800)
    sale=2
111 END SUBROUTINE
    
    
!***Rutina que lee el fichero de condiciones iniciales o finales de nitrógeno***
SUBROUTINE lee_nantec
    USE Modtet
    IMPLICIT NONE
    INTEGER ncel2

    sale=0
    OPEN(26,file=artem,status='old',err=227) 
    READ (26,*,err=228)c12,cn,cs
    READ (26,*,err=228)c12,ce,cw
    READ (26,*,err=228)c12,mi
    READ (26,*,err=228)c12,mj
    READ (26,*,err=228)c12,ncol
    READ (26,*,err=228)c12,nfil
    READ (26,*,err=228)c12,ncel2
    READ (26,*,err=228)c12,fecin2,horin2
    IF (ncel2.ne.ncel) THEN
      GOTO 91
    ELSE
      DO n=1,ncel
        cell(n).hn=0.0
        cell(n).w=0.0
        READ (26,*,err=229)cell(n).hn(0),cell(n).hn(1),cell(n).hn(2),cell(n).hn(3),cell(n).hn(4),cell(n).hn(5),cell(n).hn(6),cell(n).hn(7),cell(n).hn(8), &
                           cell(n).hn(9),cell(n).hn(10),cell(n).hn(11),cell(n).hn(12),cell(n).w,cell(n).fcncult
      ENDDO
    ENDIF
    91 CLOSE(26)

    GOTO 95

    227 mensaje=strings(409)
    errr=1
    CALL errores
    GOTO 94
    228 mensaje=strings(410)
    errr=1
    CALL errores
    GOTO 94
    229 mensaje=strings(411)
    errr=1
    CALL errores
    goto 94

    94 WRITE(*,*)strings(800)
    sale=2

95  END SUBROUTINE   

!***Rutina que lee el fichero Ninput suelo, que contiene los inputs de NO,NH4 y NO3 por uso del suelo y en kg/hames, la transformación a kg/celdadía se hace en sim_celdanitr***
SUBROUTINE lee_Ninputsuelo
    USE Modtet
    IMPLICIT NONE
    
    sale=0
    
    !!k=0
    !!!Lee los datos de nitrógeno orgánico NOinputsuelo.txt
    !!OPEN(10,file=archnit(7),status='old',err=248)
    !!ios=0
    !!DO WHILE (ios.ne.-1)
    !!  READ(10,*,iostat=ios) j
    !!  IF (ios.ne.-1) k=k+1
    !!ENDDO
    !!REWIND(10)
    !!IF (ALLOCATED(norganicos)) DEALLOCATE(norganicos)
    !!ALLOCATE(norganicos(k,12))
    !!  norganicos=0.0
    !!  DO i=1,k
    !!    READ(10,*,err=249)(norganicos(i,j),j=1,12)
    !!  ENDDO
    !!CLOSE(10)
    
    k=0
    OPEN(11,file=archnit(10),status='old',err=248)
    ios=0
    DO WHILE (ios.ne.-1)
      READ(11,*,iostat=ios) j
      IF (ios.ne.-1) k=k+1
    ENDDO
    REWIND(11)
    IF (ALLOCATED(amonios)) DEALLOCATE(amonios)
    ALLOCATE(amonios(k,12))
      amonios=0.0
      DO i=1,k
        READ(11,*,err=249)(amonios(i,j),j=1,12)
      ENDDO
    CLOSE(11)
    
    k=0
    OPEN(12,file=archnit(11),status='old',err=250)
    ios=0
    DO WHILE (ios.ne.-1)
      READ(12,*,iostat=ios) j
      IF (ios.ne.-1) k=k+1
    ENDDO
    REWIND(12)
    IF (ALLOCATED(nitratos)) DEALLOCATE(nitratos)
    ALLOCATE(nitratos(k,12))
      nitratos=0.0
      DO i=1,k
        READ(12,*,err=251)(nitratos(i,j),j=1,12)
      ENDDO
    CLOSE(12)
    !Paso de valor mensual (kg/ha/mes) a diario (kg/ha/dt)
    
    
    !Vicente - Con riego, el valor de amonios dependerá de las veces que se riega al mes    
    If (modulos(5)) then
        DO i=1,k
            Do j=1,12
                If (j==1.or.j==3.or.j==5.or.j==7.or.j==8.or.j==10.or.j==12) then
                    !norganicos(i,j)=norganicos(i,j)/31/(24*60)*dtmin                    
                    amonios(i,j)=amonios(i,j)/INT(31/periodRiego(i,j))
                    nitratos(i,j)=nitratos(i,j)/INT(31/periodRiego(i,j))
                else if (j==2) then
                    !norganicos(i,j)=norganicos(i,j)/28/(24*60)*dtmin
                    amonios(i,j)=amonios(i,j)/INT(28/periodRiego(i,j))
                    nitratos(i,j)=nitratos(i,j)/INT(28/periodRiego(i,j))
                else
                    !norganicos(i,j)=norganicos(i,j)/30/(24*60)*dtmin
                    amonios(i,j)=amonios(i,j)/INT(30/periodRiego(i,j))
                    nitratos(i,j)=nitratos(i,j)/INT(30/periodRiego(i,j))
                end if
            end do
        end do
    else
    !Forma tradicional        
        DO i=1,k
            Do j=1,12
                If (j==1.or.j==3.or.j==5.or.j==7.or.j==8.or.j==10.or.j==12) then
                    !norganicos(i,j)=norganicos(i,j)/31/(24*60)*dtmin
                    amonios(i,j)=amonios(i,j)/31/(24*60)*dtmin
                    nitratos(i,j)=nitratos(i,j)/31/(24*60)*dtmin
                else if (j==2) then
                    !norganicos(i,j)=norganicos(i,j)/28/(24*60)*dtmin
                    amonios(i,j)=amonios(i,j)/28/(24*60)*dtmin
                    nitratos(i,j)=nitratos(i,j)/28/(24*60)*dtmin
                else
                    !norganicos(i,j)=norganicos(i,j)/30/(24*60)*dtmin
                    amonios(i,j)=amonios(i,j)/30/(24*60)*dtmin
                    nitratos(i,j)=nitratos(i,j)/30/(24*60)*dtmin
                end if
            end do
        end do
    End If    
    
    GOTO 111

    248 mensaje=strings(426)
    errr=1
    CALL errores
    GOTO 94
    249 mensaje=strings(428)
    errr=1
    CALL errores
    GOTO 94
    250 mensaje=strings(427)
    errr=1
    CALL errores
    GOTO 94
    251 mensaje=strings(429)
    errr=1
    CALL errores
    GOTO 94

    94  WRITE(*,*)strings(800)
    sale=2
111 END SUBROUTINE
    
!***Rutina que lee el archivo en el que están escritas las variables de estado a imprimir
SUBROUTINE lee_printvariablesnitr
USE modtet
IMPLICIT NONE
character*3 dummy
!comprueba que existe el fichero de variables a imprimir en archivos ASCII
INQUIRE (FILE=TRIM(ADJUSTL(dirtra))//"Printvariablesnitr.txt", EXIST = existe)
IF (.NOT.existe) goto 200
!lee el archivo
OPEN(26, file=TRIM(ADJUSTL(dirtra))//"Printvariablesnitr.txt")
  DO i=1,9
     READ(26,'(A3,L1)') dummy,variablesasciinitr(i)   
  ENDDO
  DO i=1,39
    READ(26,'(A4,L1)') dummy,variablesasciinitr(i+9)
  End do
  Do i=1,15
      Read(26,'(A4,L1)') dummy,finalesasciinitr(i)
  End do
CLOSE(26)

GO TO 95
200 printascii = .FALSE.
    WRITE(*,*)"3",printascii
    artem = arch(34)
    CALL escri_settings
95  END SUBROUTINE