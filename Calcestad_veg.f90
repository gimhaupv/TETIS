! *************************************************************************
! * Subrutina que imprime los estadisticos principales durante el proceso
! * de optimizacion en el fichero de salida, sirve para R,H o Todos.
! *************************************************************************
SUBROUTINE imprencgen_veg
USE modtet
IMPLICIT NONE

CHARACTER selec*15,artem2*128
CHARACTER fcChar*13(nparam)

!Escribe en formato compatible con el GLUE (faltan comas)
artem2=TRIM(ADJUSTL(dirtra))//'Res-SCEUA_HidrVeg.txt' 
OPEN(13,file=artem2) 

!OPEN(13,file=TRIM(ADJUSTL(dirtra))//'Res-SCEUA_HidrVeg.txt')
CALL DATE_AND_TIME(dia,hora)
WRITE(13,*)'Date:'//dia(7:8)//'/'//dia(5:6)//'/'//dia(1:4)//' hour '//hora(1:2)//':'//hora(3:4)//':'//hora(5:8)
WRITE(13,*)'Catchment     Location: ',TRIM(ADJUSTL(artem2))
WRITE(13,*)nparam,3,3

cont2=0

DO i=1,9
  IF (xchk(i)) THEN
      cont2 = cont2 + 1 
      WRITE(fcChar(cont2),'(A12,I1)')'     R' ,i
      WRITE(13,112)' R',i,xlb(i),xub(i),xguess(i)
      !WRITE(13,'(A2,I1,x,3F15.5)')' R',i,xlb(i),xub(i),xguess(i)
  ENDIF
ENDDO
DO i=10,14
  IF (xchk(i)) THEN
    cont2 = cont2 + 1 
    WRITE(fcChar(cont2),'(A12,I1)')'     H',i-9 
     WRITE(13,112)' H',i-9,xlb(i),xub(i),xguess(i)
    !WRITE(13,'(A2,I1,x,3F15.5)')' H',i-9,xlb(i),xub(i),xguess(i)
  ENDIF
ENDDO
IF (xchk(15)) THEN
    cont2 = cont2 + 1 
    WRITE(fcChar(cont2),'(A13)')' Beta-Ppt'    
    WRITE(13,113)' Beta-Ppt',xlb(15),xub(15),xguess(15)
END IF    
IF (xchk(16)) THEN
    cont2 = cont2 + 1 
    WRITE(fcChar(cont2),'(A13)')' Hp-Evp'
    WRITE(13,113)' Hp-Evp',xlb(16),xub(16),xguess(16)
END IF
IF (xchk(17)) THEN
    cont2 = cont2 + 1 
    WRITE(fcChar(cont2),'(A13)')' Beta-Psnow'
    WRITE(13,113)' Beta-Psnow',xlb(17),xub(17),xguess(17)
END IF
IF (xchk(18)) THEN
    cont2 = cont2 + 1 
    WRITE(fcChar(cont2),'(A13)')' DDF1/no-rain'
    WRITE(13,113)' DDF1/no-rain',xlb(18),xub(18),xguess(18)
END IF
IF (xchk(19)) THEN
    cont2 = cont2 + 1 
    WRITE(fcChar(cont2),'(A13)')' DDF2/rain'
    WRITE(13,113)' DDF2/rain',xlb(19),xub(19),xguess(19)
END IF
IF (xchk(20)) THEN
    cont2 = cont2 + 1 
    WRITE(fcChar(cont2),'(A13)')' Temp-base'
    WRITE(13,113)' Temp-base',xlb(20),xub(20),xguess(20)
END IF
IF (xchk(21)) THEN
    cont2 = cont2 + 1 
    WRITE(fcChar(cont2),'(A13)')'Exp-inf-Tet'
    WRITE(13,113)' Exp-inf-TEst',xlb(21),xub(21),xguess(21)
END IF
IF (xchk(22)) THEN
    cont2 = cont2 + 1 
    WRITE(fcChar(cont2),'(A13)')' %Hu'
    WRITE(13,113)' Cap_max_TGrav(%Hu)',xlb(22),xub(22),xguess(22)
END IF
IF (xchk(23)) THEN
    cont2 = cont2 + 1 
    WRITE(fcChar(cont2),'(A13)')' H8'
    WRITE(13,113)' H8',xlb(23),xub(23),xguess(23)
END IF
IF (xchk(24)) THEN
    cont2 = cont2 + 1 
    WRITE(fcChar(cont2),'(A13)') 'LaiIni'
    WRITE(13,113)' LaiIni',xlb(24),xub(24),xguess(24)
END IF

112 FORMAT(A12,I1,3F10.5)
113 FORMAT(A13,3F10.5)

DO isce_veg=1,cantUsosSuelosVeg
    DO jsce_veg=1,11
        IF (xchk_veg(isce_veg,jsce_veg)) THEN
            cont2 = cont2 + 1 
            WRITE(fcChar(cont2),'(A4,I2,A5,I2)')' Use',isce_veg,'lamb',jsce_veg
            WRITE(13,'(A4,I2,A5,I2,3F10.5)')' Use',isce_veg,' lamb',jsce_veg,xlb_veg(isce_veg,jsce_veg),xub_veg(isce_veg,jsce_veg),xguess_veg(isce_veg,jsce_veg)
        END IF
    ENDDO
ENDDO

!WRITE(13,*)' Obj. Funct. '
!WRITE(13,*)(' HMLE  ',i=1,naf)
!WRITE(13,*)(' RMSE  ',i=1,naf)
!WRITE(13,*)(' Nash ',i=1,naf)
!WRITE(13,*)(' RMSE-month',i=1,naf),folon
!WRITE(13,*)(' %Vol ',i=1,naf)
!WRITE(13,*)(' Coef-eff. ',i=1,naf)
!WRITE(13,*)(' Qmax ',i=1,naf)
!WRITE(13,*)(' Tpeak ',i=1,naf)
!WRITE(13,*)(' Vol ',i=1,naf)
!WRITE(13,*)(' ErrGA ',i=1,naf)
!WRITE(13,*)(' ErrorLog ',i=1,naf)  !chiara 09 diciembre 2011
!if(idpon.eq.20) WRITE(13,*)(' Nash-ranges ',i=1,naf)  !(Vicente) Se escribe solo cuando está activado
!WRITE(13,*)(' Log bias NSE',i=1,naf) !(Vicente)
!WRITE(13,*)(' Kling-Gupta Eff.',i=1,naf) !(Vicente)
!WRITE(13,*)
WRITE(13,*)
WRITE(13,*)'RMSE-month. Num days=',folon
WRITE(13,*)'HMLE lambda=',fol
WRITE(13,*)
IF (idpon.eq.1) THEN
  WRITE(13,*)'O.F. weighted by drainage area'
ELSE
  WRITE(13,*)'O.F. NOT weighted by drainage area'
ENDIF
SELECT CASE (idfo)
  CASE(8)
    selec='HMLE'
  CASE(4)
    selec='RMSE'
  CASE(6)
    selec='Nash'
  CASE(10)
    selec='RMSE-monthly'
  CASE(12)
    selec='ErrGA'
  CASE(13)
    selec='ErrorLog '
  CASE(20)
    selec='Nash-ranges '
  CASE(21)
    selec='Log-bias NSE '
  CASE(22)
    selec='Kling-Gupta Eff.'
END SELECT
WRITE(13,*)'The selected O.F. is: ',selec
WRITE(13,*)'    It has been computed from: ',nifo,' to ',nt
WRITE(13,*)

if(idfo.eq.20) then
    WRITE(13,122)(fcChar(i),i=1,nparam),'Obj Function',(('    HMLE Q',i),i=1,naf),(('    RMSE Q',i),i=1,naf),(('    Nash Q',i),i=1,naf),(('RMSE-mnt Q',i),i=1,naf),(('    %Vol Q',i),i=1,naf),&
        (('    Qmax Q',i),i=1,naf),(('   Tpeak Q',i),i=1,naf),((' Vol Obs Q',i),i=1,naf),(('   ErrGA Q',i),i=1,naf),(('  ErrLog Q',i),i=1,naf),(('Nash-rgs Q',i),i=1,naf),(('1st rnge Q',i),i=1,naf),(('2nd rnge Q',i),i=1,naf),&
        (('3rd rnge  Q',i),i=1,naf),(('LgBiaNSE Q',i),i=1,naf),(('     KGE Q',i),i=1,naf),'iteracion' !(Vicente)
else
    WRITE(13,123)(fcChar(i),i=1,nparam),'Obj Function',(('    HMLE Q',i),i=1,naf),(('    RMSE Q',i),i=1,naf),(('    Nash Q',i),i=1,naf),(('RMSE-mnt Q',i),i=1,naf),(('    %Vol Q',i),i=1,naf),&
        (('    Qmax Q',i),i=1,naf),(('   Tpeak Q',i),i=1,naf),((' Vol Obs Q',i),i=1,naf),(('   ErrGA Q',i),i=1,naf),(('  ErrLog Q',i),i=1,naf),(('LgBiaNSE Q',i),i=1,naf),(('     KGE Q',i),i=1,naf),'iteracion' !(Vicente)
endif

122 FORMAT(<nparam+1>(A13,x),x,<naf*16>(A10,I2,x),A10)
123 FORMAT(<nparam+1>(A13,x),x,<naf*12>(A10,I2,x),A10)

END SUBROUTINE

! *************************************************************************
! * Subrutina que imprime los estadisticos principales durante el proceso
! * de optimizacion en el fichero de salida, sirve para R,H o Todos.
! *************************************************************************
SUBROUTINE imprestad_veg
USE modtet
IMPLICIT NONE


DO i=1,naf
  xpar(cont,nparam+1+i)=estad(i,8)  !colonne dei valori tentativi per i parametri 
  xpar(cont,nparam+1+naf+i)=estad(naf+i,4) !(Qobs-Qsim)^2
  xpar(cont,nparam+1+naf*2+i)=estad(naf+i,6) !(Qobs-Qsim)^2/(Qobs-Qobsmedio)^2
  xpar(cont,nparam+1+naf*3+i)=estad(naf+i,10)!(Qobs-Qsim)^2/Qobs^2
  xpar(cont,nparam+1+naf*4+i)=estad(naf+i,5) !% Error Volumen
  xpar(cont,nparam+1+naf*5+i)=estad(naf+i,1) !Caudal pico Simulado
  xpar(cont,nparam+1+naf*6+i)=estad(naf+i,2) !Tiempo al pico simulado
  xpar(cont,nparam+1+naf*7+i)=estad(naf+i,7) !Vol Sim Hm³
  xpar(cont,nparam+1+naf*8+i)=estad(naf+i,12)!HMLE gaussiano autocorrelac
  xpar(cont,nparam+1+naf*9+i)=estad(naf+i,13)!(Log(Qobs)-Log(Qsim))^2
  !chiara nash por rangos
  if(idfo.eq.20)then
    xpar(cont,nparam+1+naf*10+i)=estad(naf+i,20)
    xpar(cont,nparam+1+naf*11+i)=(1-estad(i,17))
    xpar(cont,nparam+1+naf*12+i)=(1-estad(i,18))
    xpar(cont,nparam+1+naf*13+i)=(1-estad(i,19))
  endif
  xpar(cont,nparam+1+naf*14+i)=estad(naf+i,21)!Log bias NSE - GIAMBA mayo 2013
  xpar(cont,nparam+1+naf*15+i)=estad(naf+i,22)!KGE - GIAMBA mayo 2013
  
ENDDO
if(idfo.eq.6.or.idfo.eq.9.or.idfo.eq.20.or.idfo.eq.21.or.idfo.eq.22)then!Vicente(10-2020)NSE,Coef-eff,NSE-ranges,LogBian,KGE
    xpar(cont,nparam+1)=1.0 - xpar(cont,nparam+1) !fobj
endif

if(idfo.eq.20)then
    WRITE(13,122)(xpar(cont,i),i=1,nparam+1+naf*16),cont
else
    WRITE(13,123)(xpar(cont,i),i=1,nparam+1+naf*10),(xpar(cont,i),i=nparam+1+naf*14+1,nparam+1+naf*16),cont
endif

122 FORMAT(<nparam+1>(F13.5,x),x,<naf*16>(F12.4,x),I7)
123 FORMAT(<nparam+1>(F13.5,x),x,<naf*12>(F12.4,x),I7)




END SUBROUTINE



