! *************************************************************************
! * Subrutina que calcula los estadisticos principales durante el proceso
! * de optimizacion, Nash, HMLE, RMSE y otros
! *************************************************************************
SUBROUTINE calc_estad
USE modtet
IMPLICIT NONE
INTEGER num1,num2,num3
REAL*8 varsim,varobs

!fobj=0.00
!estad=0.0
!salrio=0.0
!newc=0
!DO j=1,naf
!  DO t=nifo,nt
!    estad(j,3)=estad(j,3)+aforo(j).obs(t)
!    estad(j,9)=estad(j,9)+aforo(j).sim(t)
!	estad(j,12)=estad(j,12)+(aforo(j).obs(t)-aforo(j).sim(t))
!  ENDDO
!  estad(j,3)=estad(j,3)/(nt-nifo+1)   !Qmedio para el Indice de Nash
!  estad(j,9)=estad(j,9)/(nt-nifo+1)   !Qmedio simulado para Likelihood Gaussian Autocorr 
!  estad(j,12)=estad(j,12)/(nt-nifo+1)   !Error medio simulado para LGA (mu)
!ENDDO
!DO j=1,naf
!  k=j+naf
!  l=j+naf+nemb+knemb
!  DO t=nifo,nt
!    IF (aforo(j).obs(t).gt.estad(j,1)) THEN
!      estad(j,1)=aforo(j).obs(t)	!Caudal pico Observado
!      estad(j,2)=dt*t       !Tiempo al pico Observado
!    ENDIF
!    IF (aforo(j).sim(t).gt.estad(k,1)) THEN
!      estad(k,1)=aforo(j).sim(t)	!Caudal pico Simulado
!      estad(k,2)=dt*t       !Tiempo al pico simulado
!    ENDIF
!	IF (aforo(j).obs(t).le.0.0) aforo(j).obs(t)=0.00
!	estad(j,7)=estad(j,7)+(aforo(j).obs(t)*dts/1000000.)  !Vol Obs Hm³
!    estad(k,7)=estad(k,7)+(aforo(j).sim(t)*dts/1000000.)  !Vol Sim Hm³
!	estad(k,4)=estad(k,4)+(aforo(j).obs(t)-aforo(j).sim(t))*(aforo(j).obs(t)-aforo(j).sim(t))   !(Qobs-Qsim)^2
!    estad(k,3)=estad(k,3)+(aforo(j).obs(t)-estad(j,3))*(aforo(j).obs(t)-estad(j,3))  !(Qobs-Qmed)^2
!	estad(j,8)=estad(j,8)+(aforo(j).sim(t)-estad(j,9))*(aforo(j).sim(t)-estad(j,9)) !(Qsim-Qmed)^2
!	estad(j,5)=aforo(j).obs(t)**(2.*(fol-1.)) !Factor de ponderación (lambda=2.0)
!	estad(j,4)=estad(j,4)+estad(j,5)*(aforo(j).sim(t)-aforo(j).obs(t))*(aforo(j).sim(t)-aforo(j).obs(t))
!    IF (estad(j,5).gt.0.) estad(j,6)=estad(j,6)+LOG(estad(j,5))  !log Productoria
!	estad(k,9)=estad(k,9)+ABS((aforo(j).obs(t)-aforo(j).sim(t))/(aforo(j).obs(t)-estad(j,3))) !Coef de eficiencia
!	estad(j,11)=estad(j,11)+(aforo(j).obs(t)-estad(j,3))*(aforo(j).sim(t)-estad(j,9))  !(Qobs-Qm)*(Qsim-Qm)
!	estad(k,10)=estad(k,10)+(aforo(j).obs(t)-aforo(j).sim(t))*(aforo(j).obs(t)-aforo(j).sim(t))/(aforo(j).obs(t)*aforo(j).obs(t))
!	newc=newc+1
!	IF (newc.eq.folon) THEN
!	  newc=0
!	  estad(j,10)=estad(j,10)+(estad(k,10)/folon)**0.5 !RMSE mensual
!	  estad(k,10)=0.0
!    ENDIF
!  ENDDO
!  estad(j,11)=(estad(j,11)/(nt-nifo+1))/(((estad(k,3)/(nt-nifo+1))**0.5)*  &
!              ((estad(j,8)/(nt-nifo+1))**0.5))  !Coef correlac. de errores (alfa)
!  DO t=nifo+1,nt   !Para HMLE autocorrelacionado y gaussiano
!    estad(k,11)=estad(k,11)+((aforo(j).obs(t)-aforo(j).sim(t))-estad(j,12)-estad(j,11)*(aforo(j).obs(t-1)-aforo(j).obs(t-1)))**2.0
!  ENDDO
!  IF ((1.0-estad(j,11)**2.0).gt.0.0) THEN
!    IF ((2.*3.1416*(estad(k,4)/(nt-nifo+1))).gt.0.0) THEN
!      estad(k,12)=LOG(1.0-estad(j,11)**2.)/2.0-(nt-nifo+1)*LOG(2.*3.1416*(estad(k,4)/  &
!	              (nt-nifo+1)))/2.0+(estad(k,11)+(1.0-estad(j,11)**2.)*(((aforo(j).obs(1)-   &
!				  aforo(j).obs(1))-estad(j,12))**2.)*(-1./(2.*(estad(k,4)/(nt-nifo+1)))))
!    ELSE
!	  write(*,*) 'Error 2 - log negativo en Error Gauss. Autocorr.'
!    ENDIF
!  ELSE
!    write(*,*) 'Error 1 - log negativo en Error Gauss. Autocorr.'
!  ENDIF
!  salrio=salrio+cell(aforo(j).pos).acum*arcelkm
!  estad(k,5)=100.0*(estad(j,7)-estad(k,7))/estad(j,7)         !% Error Volumen 
!  estad(k,6)=(estad(k,4)/estad(k,3))    !Indice de Nash
!  estad(k,10)=estad(j,10)   !RMSE mensual
!  estad(k,4)=(estad(k,4)/(nt-nifo+1))**0.5   !RMSE
!  estad(k,8)=EXP(LOG(1.0/(nt-nifo+1))+LOG(estad(j,4))-(estad(j,6)/(nt-nifo+1)))  !HMLE
! !   <---- Para revisar, poner multiobjetivo
!  IF (idpon.eq.0) THEN
!    fobj=fobj+estad(k,idfo)
!  ELSE
!    fobj=fobj+estad(k,idfo)*cell(aforo(j).pos).acum*arcelkm
!  ENDIF
!  estad(k,6)=1.0-estad(k,6)    !Indice de Nash
!  estad(k,9)=1.0-estad(k,9)    !Coef. de eficiencia (Nash sin cuadrado)
!  !estad(k,12)=EXP(estad(k,12)) !HMLE gaussiano autocorrelac
!ENDDO
!IF (salrio.ne.0.0.AND.idpon.ne.0) fobj=fobj/salrio

fobj=0.00
estad=0.0
salrio=0.0
newc=0

DO j=1,naf
  newnt2=0 !Almacena número de registros sin contar los faltantes
  num1=0
  num2=0
  num3=0
  k=j+naf
  l=j+naf+nemb+knemb
  DO t=nifo,nt
    !IF (aforo(j).obs(t).ne.-1) THEN
    IF (aforo(j).obs(t).ge.0.0) THEN
      newnt2=newnt2+1
      estad(j,3)=estad(j,3)+aforo(j).obs(t)
      estad(j,9)=estad(j,9)+aforo(j).sim(t)
	  estad(j,12)=estad(j,12)+(aforo(j).obs(t)-aforo(j).sim(t))
	ENDIF
	!chiara, nash por rangos
    if(aforo(j).obs(t).ge.0.0.AND.aforo(j).obs(t).le.rango(1))then
        num1=num1+1
        estad(j,14)=estad(j,14)+aforo(j).obs(t) !Total observado de caudal inferior al primer umbral para cada estación en todo el periodo considerado
    elseif(aforo(j).obs(t).gt.rango(1).AND.aforo(j).obs(t).le.rango(2))then
        num2=num2+1
        estad(j,15)=estad(j,15)+aforo(j).obs(t)
    else
        num3=num3+1
        estad(j,16)=estad(j,16)+aforo(j).obs(t)
    endif
  ENDDO
  
  IF (newnt2.gt.0) THEN
    !estad(j,3)=estad(j,3)/(nt-nifo+1)   !Qmedio para el Indice de Nash
    !estad(j,9)=estad(j,9)/(nt-nifo+1)   !Qmedio simulado para Likelihood Gaussian Autocorr 
    !estad(j,12)=estad(j,12)/(nt-nifo+1)   !Error medio simulado para LGA (mu)
    estad(j,3)=estad(j,3)/newnt2   !Qmedio obs  (nuevo Camilo)
    estad(j,9)=estad(j,9)/newnt2  !Qmedio sim (nuevo Camilo)
    estad(j,12)=estad(j,12)/newnt2   !Error medio simulado para LGA (mu) (nuevo Camilo)
  ENDIF  
  if(num1.gt.0)estad(j,14)=estad(j,14)/num1  !chiara: nash por rangos
  if(num2.gt.0)estad(j,15)=estad(j,15)/num2  !chiara: nash por rangos
  if(num3.gt.0)estad(j,16)=estad(j,16)/num3  !chiara: nash por rangos
  
  DO t=nifo,nt
    IF (aforo(j).obs(t).gt.estad(j,1)) THEN
      estad(j,1)=aforo(j).obs(t)	!Caudal pico Observado
      estad(j,2)=dt*t       !Tiempo al pico Observado
    ENDIF
    IF (aforo(j).sim(t).gt.estad(k,1)) THEN
      estad(k,1)=aforo(j).sim(t)	!Caudal pico Simulado
      estad(k,2)=dt*t       !Tiempo al pico simulado
    ENDIF
    
    IF (aforo(j).obs(t).ge.0.0) THEN
	  estad(j,7)=estad(j,7)+(aforo(j).obs(t)*dts/1000000.)  !Vol Obs Hm³
      estad(k,7)=estad(k,7)+(aforo(j).sim(t)*dts/1000000.)  !Vol Sim Hm³
	  estad(k,4)=estad(k,4)+(aforo(j).obs(t)-aforo(j).sim(t))**2.0   !(Qobs-Qsim)^2
      estad(k,3)=estad(k,3)+(aforo(j).obs(t)-estad(j,3))**2.0  !(Qobs-Qmed)^2
	  estad(j,8)=estad(j,8)+(aforo(j).sim(t)-estad(j,9))**2.0 !(Qsim-Qmed)^2
	  estad(j,5)=aforo(j).obs(t)**(2.*(fol-1.)) !Factor de ponderación (lambda=2.0)
	  estad(j,4)=estad(j,4)+estad(j,5)*(aforo(j).sim(t)-aforo(j).obs(t))*(aforo(j).sim(t)-aforo(j).obs(t))
      IF (estad(j,5).gt.0.) estad(j,6)=estad(j,6)+LOG(estad(j,5))  !log Productoria
	  !estad(k,9)=estad(k,9)+ABS((aforo(j).obs(t)-aforo(j).sim(t))/(aforo(j).obs(t)-estad(j,3))) !Coef de eficiencia
	  estad(j,11)=estad(j,11)+(aforo(j).obs(t)-estad(j,3))*(aforo(j).sim(t)-estad(j,9))  !(Qobs-Qm)*(Qsim-Qm)
      estad(k,10)=estad(k,10)+(aforo(j).obs(t)-aforo(j).sim(t))**2.0   
    ENDIF
     if(aforo(j).obs(t).ge.0.0.AND.aforo(j).obs(t).le.rango(1)) THEN
        estad(k,14)=estad(k,14)+(aforo(j).obs(t)-aforo(j).sim(t))**2.0   !(Qobs-Qsim)^2
        estad(k,15)=estad(k,15)+(aforo(j).obs(t)-estad(j,14))**2.0  !(Qobs-Qmed)^2
    else if(aforo(j).obs(t).gt.rango(1).AND.aforo(j).obs(t).le.rango(2))then
        estad(k,16)=estad(k,16)+(aforo(j).obs(t)-aforo(j).sim(t))**2.0   !(Qobs-Qsim)^2
        estad(k,17)=estad(k,17)+(aforo(j).obs(t)-estad(j,15))**2.0  !(Qobs-Qmed)^2
    else
        estad(k,18)=estad(k,18)+(aforo(j).obs(t)-aforo(j).sim(t))**2.0   !(Qobs-Qsim)^2
        estad(k,19)=estad(k,19)+(aforo(j).obs(t)-estad(j,16))**2.0  !(Qobs-Qmed)^2
    endif
    
    if((aforo(j).obs(t)).gt.0.0.and.(aforo(j).sim(t)).gt.0.0) then  
        estad(k,13)=estad(k,13)+(LOG(aforo(j).obs(t))-LOG(aforo(j).sim(t)))**2.0 ! (Log(Qobs)-Log(Qsim))^2
    end if
    
	newc=newc+1
	IF (newc.eq.folon) THEN
	    newc=0
	    estad(j,10)=estad(j,10)+(estad(k,10)/folon)**0.5 !RMSE mensual !Camilo: Ojo con los faltantes!!!!!
	    estad(k,10)=0.0
    ENDIF
  ENDDO
  !GIAMBA - mayo 2013 - correlación linear para calcular el KGE (Gupta et al. 2009, JoH)
  estad(k,22)=estad(j,11)/sqrt(estad(k,3)*estad(j,8))
  
  varsim=sqrt(1/(REAL(newnt2))*estad(j,8))
  varobs=sqrt(1/(REAL(newnt2))*estad(k,3))
  
  !1-KGE
  estad(k,22)=sqrt((estad(k,22)-1)**2+(varsim/varobs-1)**2+(estad(j,9)/estad(j,3)-1)**2)
  
  IF (newnt2.gt.0) estad(j,11)=(estad(j,11)/newnt2)/(((estad(k,3)/newnt2)**0.5)*((estad(j,8)/newnt2)**0.5))  !Coef correlac. de errores (alfa)
  DO t=nifo+1,nt   !Para HMLE autocorrelacionado y gaussiano
    estad(k,11)=estad(k,11)+((aforo(j).obs(t)-aforo(j).sim(t))-estad(j,12)-estad(j,11)*(aforo(j).obs(t-1)-aforo(j).obs(t-1)))**2.0
  ENDDO
  IF ((1.0-estad(j,11)**2.0).gt.0.0.and.newnt2.gt.0) THEN
    IF ((2.*3.1416*(estad(k,4)/newnt2)).gt.0.0) THEN
      estad(k,12)=LOG(1.0-estad(j,11)**2.)/2.0-newnt2*LOG(2.*3.1416*(estad(k,4)/  &
	              newnt2))/2.0+(estad(k,11)+(1.0-estad(j,11)**2.)*(((aforo(j).obs(1)-   &
				  aforo(j).obs(1))-estad(j,12))**2.)*(-1./(2.*(estad(k,4)/newnt2))))
    ELSE
	  write(*,*) 'Error 2 - negative log in Error Gauss. Autocorr.'
    ENDIF
  ELSE
    write(*,*) 'Error 1 - negative log in Error Gauss. Autocorr.'
  ENDIF
  !IF (estad(j,7).gt.0.0) estad(k,5)=-100.0*(estad(j,7)-estad(k,7))/estad(j,7)         !% Error Volumen 
  !Vicente(02/10/2020) Trabajaremos con el valor absoluto
  IF (estad(j,7).gt.0.0) estad(k,5)= 100.0*ABS(estad(j,7)-estad(k,7))/estad(j,7)  !% Error Volumen
  IF (estad(k,3).gt.0.0) estad(k,6)=(estad(k,4)/estad(k,3))    !Indice de Nash
  if(estad(k,15).gt.0.0) estad(j,17)=(estad(k,14)/estad(k,15))  !indice de Nash de caudales inferiores al primer umbral 
  if(estad(k,17).gt.0.0) estad(j,18)=(estad(k,16)/estad(k,17))  !indice de Nash de caudales superiores al primer umbral y inferiores al segundo
  if(estad(k,19).gt.0.0) estad(j,19)=(estad(k,18)/estad(k,19))  !indice de Nash de caudales superiores al primer umbral y inferiores al segundo
  estad(k,20)=(peso(1)*estad(j,17)+peso(2)*estad(j,18)+peso(3)*estad(j,19))/(peso(1)+peso(2)+peso(3))               !combinaciones de Nashes por rango
  
  estad(k,10)=estad(j,10)   !RMSE mensual
  !estad(k,4)=(estad(k,4)/(nt-nifo+1))**0.5   !RMSE   !estaba así antes de newnt!!!
  IF (newnt2.gt.0.0) estad(k,4)=(estad(k,4)/newnt2)**0.5   !RMSE
  !estad(k,8)=EXP(LOG(1.0/(nt-nifo+1))+LOG(estad(j,4))-(estad(j,6)/(nt-nifo+1)))  !HMLE   !estaba así antes de newnt!!!
  IF (newnt2.gt.0.0) estad(k,8)=EXP(LOG(1.0/newnt2)+LOG(estad(j,4))-(estad(j,6)/newnt2))  !HMLE

  !GIAMBA mayo 2013 - Nueva función objetivo: log-bias NSE
  estad(k,21)=estad(k,6)+5*abs(log(1+estad(k,5)/100))**2.5 !Vicente(10-2020) Se ha de sumar a estad(k,6), puesto que posterioremente se hará 1 - estad(k,21)
  
 !   <---- Para revisar, poner multiobjetivo
  salrio=salrio+cell(aforo(j).pos).acum*arcelkm !sumatoria de areas de las estaciones de aforo
  IF (idpon.eq.0) THEN
    fobj=fobj+estad(k,idfo) !Camilo: Si no se señala la casilla de ponderación se da el mismo peso a todas las estaciones de aforo!
  ELSE
    fobj=fobj+estad(k,idfo)*cell(aforo(j).pos).acum*arcelkm !Ponderación con el área de las estaciones de aforo
  ENDIF  
  estad(k,6)=1.0-estad(k,6)    !Indice de Nash
  !estad(k,9)=1.0-estad(k,9)    !Coef. de eficiencia (Nash sin cuadrado)
  estad(k,21) = 1.0 - estad(k,21)   !Log bias NSE
  !GIAMBA mayo 2013 - Nueva función objetivo: KGE
  estad(k,22) = 1.0 - estad(k,22)   !KGE
  !estad(k,12)=EXP(estad(k,12)) !HMLE gaussiano autocorrelac
  !(Vicente) Nash-rangos
  estad(j,17) = 1.0 - estad(j,17)
  estad(j,18) = 1.0 - estad(j,18)
  estad(j,19) = 1.0 - estad(j,19)
  estad(k,20)=(((estad(j,17))*peso(1))+((estad(j,18))*peso(2))+((estad(j,19))*peso(3)))/(peso(1)+peso(2)+peso(3))
  !estad(k,20)=(((1.0-estad(j,17))*peso(1))+((1-estad(j,18))*peso(2))+((1-estad(j,19))*peso(3)))/(peso(1)+peso(2)+peso(3))
ENDDO
IF (idpon.eq.0) THEN
    fobj=fobj/naf
Else
    IF (salrio.ne.0) fobj=fobj/salrio !Termina el cálculo de la ponderación con el área
End if
END SUBROUTINE

! *************************************************************************
! Calcula la función objetivo para la calibracion de Qmax
! *************************************************************************
SUBROUTINE calc_estad_qMax
USE modtet
IMPLICIT NONE
INTEGER num1,num2,num3
REAL*8 varsim,varobs

fobj_qMax=0.00
estad=0.0
salrio=0.0
newc=0

qMax_obs = qMaxObj
qMax_sim = 0.0 
IF(pos_estMax.le.naf) THEN
    DO t=nifo,nt
        IF (aforo(pos_estMax).sim(t).gt.qMax_sim) THEN
            qMax_sim=aforo(pos_estMax).sim(t)	!Caudal pico Simulado      
        ENDIF
    END DO 
ELSE
    pos_estMax = pos_estMax - naf
    DO t=nifo,nt
        IF (otros(pos_estMax).sim(t).gt.qMax_sim) THEN
            qMax_sim=otros(pos_estMax).sim(t)	!Caudal pico Simulado      
        ENDIF
    END DO
END IF
fobj_qMax = abs(qMax_sim-qMax_obs)

END SUBROUTINE


! *************************************************************************
! * Subrutina que imprime los estadisticos principales durante el proceso
! * de optimizacion en el fichero de salida, sirve para R,H o Todos.
! *************************************************************************
SUBROUTINE imprenc(cas)
USE modtet
IMPLICIT NONE

INTEGER cas
CHARACTER artem2*128,selec*15

SELECT CASE (cas)
  CASE (1)
    artem2=TRIM(ADJUSTL(dirtra))//'~opt-SCEUAhvirt.txt'
  CASE (2)
    artem2=TRIM(ADJUSTL(dirtra))//'~opt-AGr.txt'
  CASE (3)
    artem2=TRIM(ADJUSTL(dirtra))//'~opt-SCEUAr.txt'
  CASE (4)
    artem2=TRIM(ADJUSTL(dirtra))//'~opt-AG.txt'
  CASE (5)
    artem2=TRIM(ADJUSTL(dirtra))//'~opt-SCEUA.txt'
  CASE (6)
    artem2=TRIM(ADJUSTL(dirtra))//'~opt-AGh.txt'
  CASE (7)
    artem2=TRIM(ADJUSTL(dirtra))//'~opt-SCEUAh.txt'
  CASE (9)
    artem2=TRIM(ADJUSTL(dirtra))//'~opt-SCEUArp.txt'
  CASE (10)
    artem2=TRIM(ADJUSTL(dirtra))//'~opt-SCEUArhp.txt'
END SELECT

!Escribe en formato compatible con el GLUE (faltan comas)
OPEN(13,file=artem2)
CALL DATE_AND_TIME(dia,hora)
WRITE(13,*)'Date:'//dia(7:8)//'/'//dia(5:6)//'/'//dia(1:4)//' hour '//hora(1:2)//':'//hora(3:4)//':'//hora(5:8)
WRITE(13,*)'Catchment   '
WRITE(13,*)nparam,3,3

SELECT CASE (cas)
  CASE (2,3,4,5,9,10)
    WRITE(13,*)' R1  '
    WRITE(13,*)' R2  '
    WRITE(13,*)' R3  '
    WRITE(13,*)' R4  '
    WRITE(13,*)' R5  '
    WRITE(13,*)' R6  '
    WRITE(13,*)' R7  '
    WRITE(13,*)' R8  '
    WRITE(13,*)' R9  '
ENDSELECT

SELECT CASE (cas)
  CASE (9,10)
    WRITE(13,*)' Beta-Ppt'
END SELECT

SELECT CASE (cas)
  CASE (1,4,5,6,7,10)
    WRITE(13,*)' H1  '
    WRITE(13,*)' H2  '
    WRITE(13,*)' H3  '
    WRITE(13,*)' H4  '
    WRITE(13,*)' H5  '
ENDSELECT

WRITE(13,*)' Obj. Funct. '
WRITE(13,*)(' HMLE  ',i=1,naf)
WRITE(13,*)(' RMSE  ',i=1,naf)
WRITE(13,*)(' Nash ',i=1,naf)
WRITE(13,*)(' RMSE-month',i=1,naf),folon
WRITE(13,*)(' %Vol ',i=1,naf)
WRITE(13,*)(' Coef-eff. ',i=1,naf)
WRITE(13,*)(' Qmax ',i=1,naf)
WRITE(13,*)(' Tpeak ',i=1,naf)
WRITE(13,*)(' Vol ',i=1,naf)
WRITE(13,*)(' ErrGA ',i=1,naf)
WRITE(13,*)(' Log bias NSE',i=1,naf) !(Vicente)
WRITE(13,*)(' Kling-Gupta Eff.',i=1,naf) !(Vicente)
WRITE(13,*)
WRITE(13,*)'HMLE lambda=',fol
IF (idpon.eq.1) THEN
  WRITE(13,*)'O.F. weighted by drainage area'
ELSE
  WRITE(13,*)'O.F. NOT weighted by drainage area'
ENDIF
SELECT CASE (idfo)
  CASE(4)
    selec='RMSE'
  CASE(5)
    selec='%Vol'     
  CASE(6)
    selec='Nash'  
  CASE(8)
    selec='HMLE'  
  CASE(9)
    selec='Coef-eff.'
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

END SUBROUTINE

! *************************************************************************
! * Subrutina que imprime los estadisticos principales durante el proceso
! * de optimizacion en el fichero de salida, sirve para R,H o Todos.
! *************************************************************************
SUBROUTINE imprencgen
use modtet
IMPLICIT NONE

CHARACTER selec*15
CHARACTER fcChar*12(nparam)

!Escribe en formato compatible con el GLUE (faltan comas)
OPEN(13,file=arch(27))
CALL DATE_AND_TIME(dia,hora)
WRITE(13,*)'Date:'//dia(7:8)//'/'//dia(5:6)//'/'//dia(1:4)//' hour '//hora(1:2)//':'//hora(3:4)//':'//hora(5:8)
WRITE(13,*)'Catchment     Location: ',arch(27)
WRITE(13,'(I2,x,I1,x,I1)')nparam,3,3
cont2=0

DO i=1,9
  IF (xchk(i)) THEN
      cont2 = cont2 + 1 
      WRITE(fcChar(cont2),'(A6,I1)')'     R' ,i
      WRITE(13,'(A2,I1,x,3F15.5)')' R',i,xlb(i),xub(i),xguess(i)
  ENDIF
ENDDO
DO i=10,14
  IF (xchk(i)) THEN
    cont2 = cont2 + 1 
    WRITE(fcChar(cont2),'(A6,I1)')'     H',i-9 
    WRITE(13,'(A2,I1,x,3F15.5)')' H',i-9,xlb(i),xub(i),xguess(i)
  ENDIF
ENDDO
IF (xchk(15)) THEN
    cont2 = cont2 + 1 
    WRITE(fcChar(cont2),'(A12)')' Beta-Ppt'    
    WRITE(13,*)' Beta-Ppt',xlb(15),xub(15),xguess(15)
END IF    
IF (xchk(16)) THEN
    cont2 = cont2 + 1 
    WRITE(fcChar(cont2),'(A12)')' Hp-Evp'
    WRITE(13,*)' Hp-Evp',xlb(16),xub(16),xguess(16)
END IF
IF (xchk(17)) THEN
    cont2 = cont2 + 1 
    WRITE(fcChar(cont2),'(A12)')' Beta-Psnow'
    WRITE(13,*)' Beta-Psnow',xlb(17),xub(17),xguess(17)
END IF
IF (xchk(18)) THEN
    cont2 = cont2 + 1 
    WRITE(fcChar(cont2),'(A12)')' DDF1/no-rain'
    WRITE(13,*)' DDF1/no-rain',xlb(18),xub(18),xguess(18)
END IF
IF (xchk(19)) THEN
    cont2 = cont2 + 1 
    WRITE(fcChar(cont2),'(A12)')' DDF2/rain'
    WRITE(13,*)' DDF2/rain',xlb(19),xub(19),xguess(19)
END IF
IF (xchk(20)) THEN
    cont2 = cont2 + 1 
    WRITE(fcChar(cont2),'(A12)')' Temp-base'
    WRITE(13,*)' Temp-base',xlb(20),xub(20),xguess(20)
END IF
IF (xchk(21)) THEN
    cont2 = cont2 + 1 
    WRITE(fcChar(cont2),'(A12)')'Exp-inf-Tet'
    WRITE(13,*)' Exp-inf-TEst',xlb(21),xub(21),xguess(21)
END IF
IF (xchk(22)) THEN
    cont2 = cont2 + 1 
    WRITE(fcChar(cont2),'(A12)')' %Hu'
    WRITE(13,*)' Cap_max_TGrav(%Hu)',xlb(22),xub(22),xguess(22)
END IF
IF (config(5)) THEN
    IF (xchk(23)) THEN
        cont2 = cont2 + 1 
        WRITE(fcChar(cont2),'(A12)')' H-8'
        WRITE(13,*)' H-8',xlb(23),xub(23),xguess(23)
    END IF
    IF (xchk(24)) THEN
        cont2 = cont2 + 1 
        WRITE(fcChar(cont2),'(A12)') 'LaiIni'
        WRITE(13,*)' LaiIni',xlb(24),xub(24),xguess(24)
    END IF
END IF

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
!if(idfo.eq.20) THEN !(Vicente) Se escribe solo cuando está activado
!    WRITE(13,*)(' Nash-ranges ',i=1,naf)  
!    WRITE(13,*)('  1st range ',i=1,naf)
!    WRITE(13,*)('  2nd range ',i=1,naf)
!    WRITE(13,*)('  3rd range ',i=1,naf)
!END IF
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
  CASE(4)
    selec='RMSE'
  CASE(5)
    selec='%Vol'     
  CASE(6)
    selec='Nash'  
  CASE(8)
    selec='HMLE'
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
    WRITE(13,123)(fcChar(i),i=1,nparam),'Obj Function',(('    HMLE Q',i),i=1,naf),(('    RMSE Q',i),i=1,naf),(('    Nash Q',i),i=1,naf),(('RMSE-mnt Q',i),i=1,naf),(('   %Vol  Q',i),i=1,naf),&
        (('    Qmax Q',i),i=1,naf),(('   Tpeak Q',i),i=1,naf),((' Vol Obs Q',i),i=1,naf),(('   ErrGA Q',i),i=1,naf),((' ErrLog  Q',i),i=1,naf),(('LgBiaNSE Q',i),i=1,naf),(('     KGE Q',i),i=1,naf),'iteracion' !(Vicente)
endif

122 FORMAT(<nparam+1>(A12,x),x,<naf*16>(A10,I2,x),A10)
123 FORMAT(<nparam+1>(A12,x),x,<naf*12>(A10,I2,x),A10)    


END SUBROUTINE


! *************************************************************************
! * Imprimer para la calibración de QMax
! *************************************************************************
SUBROUTINE imprencgen_qMax
USE modtet
IMPLICIT NONE

CHARACTER selec*15

!Escribe en formato compatible con el GLUE (faltan comas)
OPEN(13,file=TRIM(ADJUSTL(dirtra))//'ResQmax-SCEUA.txt')
CALL DATE_AND_TIME(dia,hora)
WRITE(13,*)'Date:'//dia(7:8)//'/'//dia(5:6)//'/'//dia(1:4)//' hour '//hora(1:2)//':'//hora(3:4)//':'//hora(5:8)

WRITE(13,'(A4,x,3F15.5)')' R10',xlb_qMax,xub_qMax,xguess_qMax
selec="QMax"
WRITE(13,*)'The selected O.F. is: ',selec
WRITE(13,*)'    It has been computed from: ',nifo_qMax,' to ',nt
WRITE(13,*)
WRITE(13,'(3(A13,2x),A10)')'     R10     ','    Q max    ','    f obj    ','iteracion'

END SUBROUTINE

! *************************************************************************
! * Subrutina que imprime los estadisticos principales durante el proceso
! * de optimizacion en el fichero de salida, sirve para R,H o Todos.
! *************************************************************************
SUBROUTINE imprestad
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
    xpar(cont,nparam+1+naf*11+i)=(estad(i,17))!(Vicente) Nash-rango1
    xpar(cont,nparam+1+naf*12+i)=(estad(i,18))!(Vicente) Nash-rango2
    xpar(cont,nparam+1+naf*13+i)=(estad(i,19))!(Vicente) Nash-rango3
  endif
  xpar(cont,nparam+1+naf*14+i)=estad(naf+i,21)!Log bias NSE - GIAMBA mayo 2013
  xpar(cont,nparam+1+naf*15+i)=estad(naf+i,22)!KGE - GIAMBA mayo 2013
  
ENDDO
if(idfo.eq.6.or.idfo.eq.20.or.idfo.eq.21.or.idfo.eq.22)then!NSE,Coef-eff,NSE-ranges,LogBian,KGE
    xpar(cont,nparam+1)=1.0 - xpar(cont,nparam+1) !fobj
endif
if(idfo.eq.20)then
    WRITE(13,122)(xpar(cont,i),i=1,nparam+1+naf*16),cont
else
    WRITE(13,123)(xpar(cont,i),i=1,nparam+1+naf*10),(xpar(cont,i),i=nparam+1+naf*14+1,nparam+1+naf*16),cont
endif

122 FORMAT(<nparam+1>(F12.5,x),x,<naf*16>(F12.4,x),I7)
123 FORMAT(<nparam+1>(F12.5,x),x,<naf*12>(F12.4,x),I7)

END SUBROUTINE

!                              SEDIMENTOS
!
! *************************************************************************
! *************************************************************************
!
!
! *************************************************************************
! * Subrutina que calcula los estadisticos principales durante el proceso
! * de optimizacion, Nash, HMLE, RMSE y otros
! *************************************************************************
SUBROUTINE calc_estadsed
USE modtet
IMPLICIT NONE

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
! (2,1) Qmax sim
! (2,2) T(Qmax) sim 
! (2,3) (Qobs-Qmed)^2
! (2,4) RMSE
! (2,5) % Error Volumen
! (2,6) Nash-Sutcliffe
! (2,7) Vol Sim Hm³
! (2,8) HMLE
! (2,9) Coef. de eficiencia (Nash sin cuadrado)
! (2,10) RMSE mensual
! (2,11) Para HMLE autocorrelacionado y gaussiano (??)
! (2,12) HMLE gaussiano autocorrelac
!
fobj=0.00000
estadsed=0.00000000000
newc=0
!
DO j=1,ksedq
  DO t=nifo,nt
    estadsed(j,3)=estadsed(j,3)+aforosed(j).obs(t) !suma caudales observados
    estadsed(j,9)=estadsed(j,9)+aforosed(j).sim(t) !suma caudales simulados
	estadsed(j,12)=estadsed(j,12)+(aforosed(j).obs(t)-aforo(j).sim(t)) !suma diferencias
  ENDDO
  estadsed(j,3)=estadsed(j,3)/(nt-nifo+1)   !Qmedio para el Indice de Nash
  estadsed(j,9)=estadsed(j,9)/(nt-nifo+1)   !Qmedio simulado para Likelihood Gaussian Autocorr 
  estadsed(j,12)=estadsed(j,12)/(nt-nifo+1)   !Error medio simulado para LGA (mu)
ENDDO
DO j=1,ksedq
  k=j+ksedq
!  l=j+ksedq
  DO t=nifo,nt
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
	!estadsed(k,9)=estadsed(k,9)+ABS((aforosed(j).obs(t)-aforosed(j).sim(t))/(aforosed(j).obs(t)-estadsed(j,3))) !Coef de eficiencia
	estadsed(j,11)=estadsed(j,11)+(aforosed(j).obs(t)-estadsed(j,3))*(aforosed(j).sim(t)-estadsed(j,9))  !(Qobs-Qm)*(Qsim-Qm)
	estadsed(k,10)=estadsed(k,10)+(aforosed(j).obs(t)-aforosed(j).sim(t))**2.0
	newc=newc+1
	IF (newc.eq.folon) THEN
	  newc=0
	  estadsed(j,10)=estadsed(j,10)+(estadsed(k,10)/folon)**0.5 !RMSE mensual
	  estadsed(k,10)=0.0
    ENDIF
  ENDDO
  estadsed(j,11)=(estadsed(j,11)/(nt-nifo+1))/(((estadsed(k,3)/(nt-nifo+1))**0.5)*  &
              ((estadsed(j,8)/(nt-nifo+1))**0.5))  !Coef correlac. de errores (alfa)
  DO t=nifo+1,nt   !Para HMLE autocorrelacionado y gaussiano
    estadsed(k,11)=estadsed(k,11)+((aforosed(j).obs(t)-aforosed(j).sim(t))-estadsed(j,12)-estadsed(j,11)*(aforosed(j).obs(t-1)-aforosed(j).obs(t-1)))**2.0
  ENDDO
  IF ((1.0-estadsed(j,11)**2.0).gt.0.0) THEN
    IF ((2.*3.1416*(estadsed(k,4)/(nt-nifo+1))).gt.0.0) THEN
      estadsed(k,12)=LOG(1.0-estadsed(j,11)**2.)/2.0-(nt-nifo+1)*LOG(2.*3.1416*(estadsed(k,4)/  &
	              (nt-nifo+1)))/2.0+(estadsed(k,11)+(1.0-estad(j,11)**2.)*(((aforosed(j).obs(1)-   &
				  aforosed(j).obs(1))-estadsed(j,12))**2.)*(-1./(2.*(estadsed(k,4)/(nt-nifo+1)))))
    ELSE
	  write(*,*) 'Error 2 - negative log in Error Gauss. Autocorr.'
    ENDIF
  ELSE
    write(*,*) 'Error 1 - negative log in Error Gauss. Autocorr.'
  ENDIF
  !salrio=salrio+cell(aforo(j).pos).acum*arcelkm
  IF (estadsed(j,7).gt.0.0) estadsed(k,5)=100.0*ABS(estadsed(j,7)-estadsed(k,7))/estadsed(j,7)         !% Error Volumen 
  IF (estadsed(k,3).gt.0.0) estadsed(k,6)=(estadsed(k,4)/estadsed(k,3))    !Indice de Nash
  estadsed(k,10)=estadsed(j,10)   !RMSE mensual
  estadsed(k,4)=(estadsed(k,4)/(nt-nifo+1))**0.5   !RMSE
  estadsed(k,8)=EXP(LOG(1.0/(nt-nifo+1))+LOG(estadsed(j,4))-(estadsed(j,6)/(nt-nifo+1)))  !HMLE
   !   <---- Para revisar, poner multiobjetivo
  IF (idpon.eq.0) THEN
    fobj=fobj+estadsed(k,idfo)
  ELSE
    fobj=fobj+estadsed(k,idfo)*cell(aforosed(j).pos).acum*arcelkm
  ENDIF
  estadsed(k,6)=1.0-estadsed(k,6)    !Indice de Nash
  !estadsed(k,9)=1.0-estadsed(k,9)    !Coef. de eficiencia (Nash sin cuadrado)
  !estadsed(k,12)=EXP(estadsed(k,12)) !HMLE gaussiano autocorrelac
ENDDO
IF (idpon.eq.0) THEN
    fobj=fobj/ksedq
Else
    IF (salrio.ne.0) fobj=fobj/salrio !Termina el cálculo de la ponderación con el área
End if
!write(*,*)rsed
!pause
    

END SUBROUTINE
!
! *************************************************************************
! * Subrutina que imprime los estadisticos principales durante el proceso
! * de optimizacion en el fichero de salida, sirve para R,H o Todos.
! *************************************************************************
SUBROUTINE imprencgensed
uSE modtet
IMPLICIT NONE


CHARACTER artem2*128,selec*10
CHARACTER fcChar*12(nparam)


!Escribe en formato compatible con el GLUE (faltan comas)
artem2=TRIM(ADJUSTL(dirtra))//'Res-SCEUA_Sed.txt' 
OPEN(13,file=artem2)
!OPEN(13,file=arch(27))
CALL DATE_AND_TIME(dia,hora)
WRITE(13,*)'Date:'//dia(7:8)//'/'//dia(5:6)//'/'//dia(1:4)//' hour '//hora(1:2)//':'//hora(3:4)//':'//hora(5:8)
WRITE(13,*)'Catchment     Location: ',TRIM(ADJUSTL(artem2))
WRITE(13,*)nparam,3,3
cont2 = 0
DO i=1,3
  IF (xchk(i)) THEN
    cont2 = cont2 + 1  
    WRITE(fcChar(cont2),'(A6,I1)')'     R' ,i
    WRITE(13,'(A2,I1,x,3F15.5)')' R',i,xlb(i),xub(i),xguess(i)
  ENDIF
ENDDO

!WRITE(13,*)' Func-Obj  '
!WRITE(13,*)(' HMLE  ')
!WRITE(13,*)(' RMSE  ')
!WRITE(13,*)(' Nash ')
!WRITE(13,*)(' RMSE-month  '),folon
!WRITE(13,*)(' %Vol ')
!WRITE(13,*)(' Coef-eff. ')
!WRITE(13,*)(' Qmax ')
!WRITE(13,*)(' Tpeak ')
!WRITE(13,*)(' Vol ')
!WRITE(13,*)(' ErrGA ')
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
  CASE(4)
    selec='RMSE'
  CASE(5)
    selec='%Vol'     
  CASE(6)
    selec='Nash'  
  CASE(8)
    selec='HMLE'
  CASE(10)
    selec='RMSE-monthly'
  CASE(12)
    selec='ErrGA'
END SELECT
WRITE(13,*)'The selected O.F. is: ',selec
WRITE(13,*)'    It has been computed from: ',nifo,' to ',nt
WRITE(13,*)

WRITE(13,122)(fcChar(i),i=1,nparam),'Obj Function',(('    HMLE X',i),i=1,naf),(('    RMSE X',i),i=1,naf),(('    Nash X',i),i=1,naf),(('RMSE-mnt X',i),i=1,naf),(('    %Vol X',i),i=1,naf),&
        (('    Qmax X',i),i=1,naf),(('   Tpeak X',i),i=1,naf),((' Vol Obs X',i),i=1,naf),(('   ErrGA X',i),i=1,naf),'iteracion' !(Vicente)
122 FORMAT(<nparam+1>A18,2x,<ksedq*9>(A19,I2,x),x,A10) 

END SUBROUTINE
!
! *************************************************************************
! * Subrutina que imprime los estadisticos principales durante el proceso
! * de optimizacion en el fichero de salida, sirve para R,H o Todos.
! *************************************************************************
SUBROUTINE imprestadsed
USE modtet
IMPLICIT NONE

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
! (2,1) Qmax sim
! (2,2) T(Qmax) sim 
! (2,3) (Qobs-Qmed)^2
! (2,4) RMSE
! (2,5) % Error Volumen
! (2,6) Nash-Sutcliffe
! (2,7) Vol Sim Hm³
! (2,8) HMLE
! (2,9) Coef. de eficiencia (Nash sin cuadrado)
! (2,10) RMSE mensual
! (2,11) Para HMLE autocorrelacionado y gaussiano (??)
! (2,12) HMLE gaussiano autocorrelac
DO i=1,ksedq
  xpar(cont,nparam+1+i)=estadsed(i,8)                !HMLE
  xpar(cont,nparam+1+ksedq+i)=estadsed(ksedq+i,4)    !RMSE
  xpar(cont,nparam+1+ksedq*2+i)=estadsed(ksedq+i,6)  !Indice de Nash
  xpar(cont,nparam+1+ksedq*3+i)=estadsed(ksedq+i,10) !RMSE mensual
  xpar(cont,nparam+1+ksedq*4+i)=estadsed(ksedq+i,5)  !Error Volumen
  xpar(cont,nparam+1+ksedq*5+i)=estadsed(ksedq+i,1)  !Caudal pico Observado
  xpar(cont,nparam+1+ksedq*6+i)=estadsed(ksedq+i,2)  !Tiempo al pico Observado
  xpar(cont,nparam+1+ksedq*7+i)=estadsed(ksedq+i,7)  !Vol Obs m³
  xpar(cont,nparam+1+ksedq*8+i)=estadsed(ksedq+i,12) !HMLE gaussiano autocorrelac
ENDDO
if(idfo.eq.6)then!Vicente(10-2020)NSE
    xpar(cont,nparam+1)=1.0 - xpar(cont,nparam+1) !fobj
endif


WRITE(13,122)(xpar(cont,i),i=1,nparam+1+ksedq*9),cont
122 FORMAT(<nparam+1>F18.10,2x,<ksedq*9>F22.8,x,I7)

END SUBROUTINE