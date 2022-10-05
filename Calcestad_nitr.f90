! *************************************************************************
! * Subrutina que calcula los estadisticos principales durante el proceso
! * de optimizacion, Nash, HMLE, RMSE y otros
! *************************************************************************
SUBROUTINE calc_estad_nitr
USE modtet
IMPLICIT NONE
REAL*8 varsim,varobs

salrio=0.0
newc=0
fobj=0.0
estadnitr = 0.0
!(Vicente) KGE en estadNitr es el 13(no el 15)

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
        IF (norg(l).obs(t).gt.estadnitr(j,1)) THEN
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
	      !estadnitr(k,9)=estadnitr(k,9)+ABS((norg(l).obs(t)-norg(l).sim(t))/(norg(l).obs(t)-estadnitr(j,3))) !Coef de eficiencia
	      estadnitr(j,11)=estadnitr(j,11)+(norg(l).obs(t)-estadnitr(j,3))*(norg(l).sim(t)-estadnitr(j,9))  !(Qobs-Qm)*(Qsim-Qm)
	      estadnitr(k,10)=estadnitr(k,10)+(norg(l).obs(t)-norg(l).sim(t))**2.0
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
      
      IF (estadnitr(j,7).gt.0.0) estadnitr(k,5)=100.0*ABS(estadnitr(j,7)-estadnitr(k,7))/estadnitr(j,7)         !% Error Volumen 
      IF (estadnitr(k,3).gt.0.0) estadnitr(k,6)=(estadnitr(k,4)/estadnitr(k,3))    !Indice de Nash      
  
      estadnitr(k,10)=estadnitr(j,10)   !RMSE mensual
      IF (newnt2.gt.0.0) estadnitr(k,4)=(estadnitr(k,4)/newnt2)**0.5   !RMSE
      IF (newnt2.gt.0.0) estadnitr(k,8)=EXP(LOG(1.0/newnt2)+LOG(estadnitr(j,4))-(estadnitr(j,6)/newnt2))  !HMLE
      
      !   <---- Para revisar, poner multiobjetivo
      salrio=salrio+cell(norg(l).pos).acum*arcelkm !sumatoria de areas de las estaciones de aforo
      IF (idpon.eq.0) THEN
        fobj=fobj+estadnitr(k,idfo) !Camilo: Si no se señala la casilla de ponderación se da el mismo peso a todas las estaciones de aforo!
      ELSE
        fobj=fobj+estadnitr(k,idfo)*cell(norg(l).pos).acum*arcelkm !Ponderación con el área de las estaciones de aforo
      ENDIF
      estadnitr(k,6)=1.0-estadnitr(k,6)    !Indice de Nash
      !estadnitr(k,9)=1.0-estadnitr(k,9)    !Coef. de eficiencia (Nash sin cuadrado)
      !GIAMBA mayo 2013 - Nueva función objetivo: KGE
      estadnitr(k,13) = 1.0 - estadnitr(k,13)   !KGE
    !norg(jno).sim=0.0 
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
	      !estadnitr(k,9)=estadnitr(k,9)+ABS((amonio(l).obs(t)-amonio(l).sim(t))/(amonio(l).obs(t)-estadnitr(j,3))) !Coef de eficiencia
	      estadnitr(j,11)=estadnitr(j,11)+(amonio(l).obs(t)-estadnitr(j,3))*(amonio(l).sim(t)-estadnitr(j,9))  !(Qobs-Qm)*(Qsim-Qm)
	      estadnitr(k,10)=estadnitr(k,10)+(amonio(l).obs(t)-amonio(l).sim(t))**2.0 
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
        estadnitr(k,11)=estadnitr(k,11)+((amonio(l).obs(t)-amonio(l).sim(t))-estadnitr(j,12)-estadnitr(j,11)*(amonio(l).obs(t-1)-amonio(l).obs(t-1)))**2.0
      ENDDO
      
      IF (estadnitr(j,7).gt.0.0) estadnitr(k,5)=100.0*(estadnitr(j,7)-estadnitr(k,7))/estadnitr(j,7)         !% Error Volumen 
      IF (estadnitr(k,3).gt.0.0) estadnitr(k,6)=(estadnitr(k,4)/estadnitr(k,3))    !Indice de Nash      
  
      estadnitr(k,10)=estadnitr(j,10)   !RMSE mensual
      IF (newnt2.gt.0.0) estadnitr(k,4)=(estadnitr(k,4)/newnt2)**0.5   !RMSE
      IF (newnt2.gt.0.0) estadnitr(k,8)=EXP(LOG(1.0/newnt2)+LOG(estadnitr(j,4))-(estadnitr(j,6)/newnt2))  !HMLE

      
      !   <---- Para revisar, poner multiobjetivo
      salrio=salrio+cell(amonio(l).pos).acum*arcelkm !sumatoria de areas de las estaciones de aforo
      IF (idpon.eq.0) THEN
        fobj=fobj+estadnitr(k,idfo) !Camilo: Si no se señala la casilla de ponderación se da el mismo peso a todas las estaciones de aforo!
      ELSE
        fobj=fobj+estadnitr(k,idfo)*cell(amonio(l).pos).acum*arcelkm !Ponderación con el área de las estaciones de aforo
      ENDIF
      estadnitr(k,6)=1.0-estadnitr(k,6)    !Indice de Nash
      !estadnitr(k,9)=1.0-estadnitr(k,9)    !Coef. de eficiencia (Nash sin cuadrado)
      !GIAMBA mayo 2013 - Nueva función objetivo: KGE
      estadnitr(k,13) = 1.0 - estadnitr(k,13)   !KGE
    !norg(jno).sim=0.0
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
	      !estadnitr(k,9)=estadnitr(k,9)+ABS((nitrato(l).obs(t)-nitrato(l).sim(t))/(nitrato(l).obs(t)-estadnitr(j,3))) !Coef de eficiencia
	      estadnitr(j,11)=estadnitr(j,11)+(nitrato(l).obs(t)-estadnitr(j,3))*(nitrato(l).sim(t)-estadnitr(j,9))  !(Qobs-Qm)*(Qsim-Qm)
	      estadnitr(k,10)=estadnitr(k,10)+(nitrato(l).obs(t)-nitrato(l).sim(t))**2.0 
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
      
      IF (estadnitr(j,7).gt.0.0) estadnitr(k,5)=100.0*(estadnitr(j,7)-estadnitr(k,7))/estadnitr(j,7)         !% Error Volumen 
      IF (estadnitr(k,3).gt.0.0) estadnitr(k,6)=(estadnitr(k,4)/estadnitr(k,3))    !Indice de Nash      
  
      estadnitr(k,10)=estadnitr(j,10)   !RMSE mensual
      IF (newnt2.gt.0.0) estadnitr(k,4)=(estadnitr(k,4)/newnt2)**0.5   !RMSE
      IF (newnt2.gt.0.0) estadnitr(k,8)=EXP(LOG(1.0/newnt2)+LOG(estadnitr(j,4))-(estadnitr(j,6)/newnt2))  !HMLE

      
      !   <---- Para revisar, poner multiobjetivo
      salrio=salrio+cell(nitrato(l).pos).acum*arcelkm !sumatoria de areas de las estaciones de aforo
      IF (idpon.eq.0) THEN
        fobj=fobj+estadnitr(k,idfo) !Camilo: Si no se señala la casilla de ponderación se da el mismo peso a todas las estaciones de aforo!
      ELSE
        fobj=fobj+estadnitr(k,idfo)*cell(nitrato(l).pos).acum*arcelkm !Ponderación con el área de las estaciones de aforo
      ENDIF
      estadnitr(k,6)=1.0-estadnitr(k,6)    !Indice de Nash
      !estadnitr(k,9)=1.0-estadnitr(k,9)    !Coef. de eficiencia (Nash sin cuadrado)
      !GIAMBA mayo 2013 - Nueva función objetivo: KGE
      estadnitr(k,13) = 1.0 - estadnitr(k,13)   !KGE
    !norg(jno).sim=0.0 
END DO
!(Vicente)Modificamos el calculo de fobj para considerar un numero de estaciones superior a 1 (En el caso de idpon eq 1, ya está en salrio)
IF(idpon.eq.0) THEN
   fobj = fobj/ nafn
ELSE IF (salrio.ne.0.0) THEN
    fobj=fobj/salrio !Termina el cálculo de la ponderación con el área
END IF
!IF (salrio.ne.0.0.AND.idpon.ne.0) fobj=fobj/salrio !Termina el cálculo de la ponderación con el área
END SUBROUTINE

! *************************************************************************
! * Subrutina que imprime los estadisticos principales durante el proceso
! * de optimizacion en el fichero de salida, sirve para R,H o Todos.
! *************************************************************************
!SUBROUTINE imprenc(dirtra,naf,nparam,cas,idpon,idfo,folamb,nt,nifo,folon)
!IMPLICIT NONE
!
!INTEGER naf,nparam,i,cas,idfo,idpon,nt,nifo,folon
!REAL folamb
!CHARACTER dia*8,hora*10,dirtra*128,artem2*128,selec*15
!
!SELECT CASE (cas)
!  CASE (1)
!    artem2=TRIM(ADJUSTL(dirtra))//'~opt-SCEUAhvirt.txt'
!  CASE (2)
!    artem2=TRIM(ADJUSTL(dirtra))//'~opt-AGr.txt'
!  CASE (3)
!    artem2=TRIM(ADJUSTL(dirtra))//'~opt-SCEUAr.txt'
!  CASE (4)
!    artem2=TRIM(ADJUSTL(dirtra))//'~opt-AG.txt'
!  CASE (5)
!    artem2=TRIM(ADJUSTL(dirtra))//'~opt-SCEUA.txt'
!  CASE (6)
!    artem2=TRIM(ADJUSTL(dirtra))//'~opt-AGh.txt'
!  CASE (7)
!    artem2=TRIM(ADJUSTL(dirtra))//'~opt-SCEUAh.txt'
!  CASE (9)
!    artem2=TRIM(ADJUSTL(dirtra))//'~opt-SCEUArp.txt'
!  CASE (10)
!    artem2=TRIM(ADJUSTL(dirtra))//'~opt-SCEUArhp.txt'
!END SELECT
!
!!Escribe en formato compatible con el GLUE (faltan comas)
!OPEN(13,file=artem2)
!CALL DATE_AND_TIME(dia,hora)
!WRITE(13,*)'Date:'//dia(7:8)//'/'//dia(5:6)//'/'//dia(1:4)//' hour '//hora(1:2)//':'//hora(3:4)//':'//hora(5:8)
!WRITE(13,*)'Catchment   '
!WRITE(13,*)nparam,3,3
!
!SELECT CASE (cas)
!  CASE (2,3,4,5,9,10)
!    WRITE(13,*)' R1  '
!    WRITE(13,*)' R2  '
!    WRITE(13,*)' R3  '
!    WRITE(13,*)' R4  '
!    WRITE(13,*)' R5  '
!    WRITE(13,*)' R6  '
!    WRITE(13,*)' R7  '
!    WRITE(13,*)' R8  '
!    WRITE(13,*)' R9  '
!ENDSELECT
!
!SELECT CASE (cas)
!  CASE (9,10)
!    WRITE(13,*)' Beta-Ppt'
!END SELECT
!
!SELECT CASE (cas)
!  CASE (1,4,5,6,7,10)
!    WRITE(13,*)' H1  '
!    WRITE(13,*)' H2  '
!    WRITE(13,*)' H3  '
!    WRITE(13,*)' H4  '
!    WRITE(13,*)' H5  '
!ENDSELECT
!
!WRITE(13,*)' Func-Obj  '
!WRITE(13,*)(' HMLE  ',i=1,naf)
!WRITE(13,*)(' RMSE  ',i=1,naf)
!WRITE(13,*)(' Nash ',i=1,naf)
!WRITE(13,*)(' RMSE-month  ',i=1,naf),folon
!WRITE(13,*)(' Coef-eff. ',i=1,naf)
!WRITE(13,*)(' %Vol ',i=1,naf)
!WRITE(13,*)(' Qmax ',i=1,naf)
!WRITE(13,*)(' Tpeak ',i=1,naf)
!WRITE(13,*)(' KGE  ',i=1,naf)
!
!IF (idpon.eq.1) THEN
!  WRITE(13,*)'O.F. weighted by drainage area'
!ELSE
!  WRITE(13,*)'O.F. NOT weighted by drainage area'
!ENDIF
!SELECT CASE (idfo)
!  CASE(8)
!    selec='HMLE'
!  CASE(4)
!    selec='RMSE'
!  CASE(6)
!    selec='Nash'
!  CASE(10)
!    selec='RMSE-monthly'
!  CASE(9)
!    selec='Coef-eff.'
!  CASE(5)
!    selec='Acum vol.'
!  CASE(13)
!    selec='Kling-Gupta Eff.'
!END SELECT
!WRITE(13,*)'The selected O.F. is: ',selec
!WRITE(13,*)'    It has been computed from: ',nifo,' to ',nt
!WRITE(13,*)
!
!END SUBROUTINE

! *************************************************************************
! * Subrutina que imprime los estadisticos principales durante el proceso
! * de optimizacion en el fichero de salida, sirve para R,H o Todos.
! *************************************************************************
SUBROUTINE imprencgen_nitr
USE modtet
IMPLICIT NONE

CHARACTER selec*15,artem2*256
CHARACTER fcChar*12(nparam)

!Escribe en formato compatible con el GLUE (faltan comas)
artem2=TRIM(ADJUSTL(dirtra))//'Res-SCEUA_Nitr.txt' !Este nombre lo pongo fijo para que no machaque al de la hidrología
OPEN(13,file=artem2)
CALL DATE_AND_TIME(dia,hora)
WRITE(13,*)'Date:'//dia(7:8)//'/'//dia(5:6)//'/'//dia(1:4)//' hour '//hora(1:2)//':'//hora(3:4)//':'//hora(5:8)
WRITE(13,*)'Catchment     Location: ',TRIM(ADJUSTL(artem2))
WRITE(13,'(I2)')nparam

cont2=0

DO isce_nitr=1,cantUsSueNitr !(Vicente) Añadimos 2 ultimos FCs
    IF (xchk_kmin(isce_nitr)) THEN
        cont2 = cont2 + 1 
        WRITE(fcChar(cont2),'(A9,I2)')'kmin, Use',isce_nitr
	    WRITE(13,'(A9,I2,A2,3F15.5)')'kmin, Use',isce_nitr,': ',xlb_kmin(isce_nitr),xub_kmin(isce_nitr),xguess_kmin(isce_nitr)
    ENDIF
    IF (xchk_kinm(isce_nitr)) THEN
        cont2 = cont2 + 1
        WRITE(fcChar(cont2),'(A9,I2)')'kinm, Use',isce_nitr
	    WRITE(13,'(A9,I2,A2,3F15.5)')'kinm, Use',isce_nitr,': ',xlb_kinm(isce_nitr),xub_kinm(isce_nitr),xguess_kinm(isce_nitr)
    ENDIF
    IF (xchk_kvol(isce_nitr)) THEN
        cont2 = cont2 + 1
        WRITE(fcChar(cont2),'(A9,I2)')'kvol, Use',isce_nitr
	    WRITE(13,'(A9,I2,A2,3F15.5)')'kvol, Use',isce_nitr,': ',xlb_kvol(isce_nitr),xub_kvol(isce_nitr),xguess_kvol(isce_nitr)
    ENDIF
    IF (xchk_knit(isce_nitr)) THEN
        cont2 = cont2 + 1
        WRITE(fcChar(cont2),'(A9,I2)')'knit, Use',isce_nitr
	    WRITE(13,'(A9,I2,A2,3F15.5)')'knit, Use',isce_nitr,': ',xlb_knit(isce_nitr),xub_knit(isce_nitr),xguess_knit(isce_nitr)
    ENDIF
    IF (xchk_kfi(isce_nitr)) THEN
        cont2 = cont2 + 1
        WRITE(fcChar(cont2),'(A9,I2)')'kfi, Use',isce_nitr
	    WRITE(13,'(A9,I2,A2,3F15.5)')'kfi, Use',isce_nitr,': ',xlb_kfi(isce_nitr),xub_kfi(isce_nitr),xguess_kfi(isce_nitr)
    ENDIF
    IF (xchk_kdes(isce_nitr)) THEN
        cont2 = cont2 + 1
        WRITE(fcChar(cont2),'(A9,I2)')'kdes, Use',isce_nitr
	    WRITE(13,'(A9,I2,A2,3F15.5)')'kdes, Use',isce_nitr,': ',xlb_kdes(isce_nitr),xub_kdes(isce_nitr),xguess_kdes(isce_nitr)
    ENDIF
    IF (xchk_F(isce_nitr)) THEN
        cont2 = cont2 + 1
        WRITE(fcChar(cont2),'(A9,I2)')'F, Use',isce_nitr
	    WRITE(13,'(A9,I2,A2,3F15.5)')'F, Use',isce_nitr,': ',xlb_F(isce_nitr),xub_F(isce_nitr),xguess_F(isce_nitr)
    ENDIF
    IF (xchk_Ndem(isce_nitr)) THEN
        cont2 = cont2 + 1
        WRITE(fcChar(cont2),'(A9,I2)')'Ndem, Use',isce_nitr
	    WRITE(13,'(A9,I2,A2,3F15.5)')'Ndem, Use',isce_nitr,': ',xlb_Ndem(isce_nitr),xub_Ndem(isce_nitr),xguess_Ndem(isce_nitr)
    ENDIF
    IF (xchk_PrefNO3(isce_nitr)) THEN
        cont2 = cont2 + 1
        WRITE(fcChar(cont2),'(A9,I2)')'PfNO3,Use',isce_nitr
	    WRITE(13,'(A12,I2,A2,3F15.5)')'PrefNO3, Use',isce_nitr,': ',xlb_PrefNO3(isce_nitr),xub_PrefNO3(isce_nitr),xguess_PrefNO3(isce_nitr)
    ENDIF
ENDDO
IF (xchk_kminc) THEN
    cont2 = cont2 + 1
    WRITE(fcChar(cont2),'(A9)')'kminc'
	WRITE(13,'(A9,3F15.5)')'kminc: ',xlb_kminc,xub_kminc,xguess_kminc
ENDIF
IF (xchk_knitc) THEN
    cont2 = cont2 + 1
    WRITE(fcChar(cont2),'(A9)')'knitc'
	WRITE(13,'(A9,3F15.5)')'knitc: ',xlb_knitc,xub_knitc,xguess_knitc
ENDIF
IF (xchk_kdesc) THEN
    cont2 = cont2 + 1
    WRITE(fcChar(cont2),'(A9)')'kdesc'
	WRITE(13,'(A9,3F15.5)')'kdesc: ',xlb_kdesc,xub_kdesc,xguess_kdesc
ENDIF
IF (xchk_mtd) THEN
    cont2 = cont2 + 1
    WRITE(fcChar(cont2),'(A9)')'mtd'
	WRITE(13,'(A9,3F15.5)')'mtd: ',xlb_mtd,xub_mtd,xguess_mtd
ENDIF
IF (xchk_tethas) THEN
    cont2 = cont2 + 1
    WRITE(fcChar(cont2),'(A9)')'tethas'
	WRITE(13,'(A9,3F15.5)')'tethas: ',xlb_tethas,xub_tethas,xguess_tethas
ENDIF
IF (xchk_topts) THEN
    cont2 = cont2 + 1
    WRITE(fcChar(cont2),'(A9)')'topts'
	WRITE(13,'(A9,3F15.5)')'topts: ',xlb_topts,xub_topts,xguess_topts
ENDIF
IF (xchk_tethac) THEN
    cont2 = cont2 + 1
    WRITE(fcChar(cont2),'(A9)')'tethac'
	WRITE(13,'(A9,3F15.5)')'tethac: ',xlb_tethac,xub_tethac,xguess_tethac
ENDIF
IF (xchk_toptc) THEN
    cont2 = cont2 + 1
    WRITE(fcChar(cont2),'(A9)')'toptc'
	WRITE(13,'(A9,3F15.5)')'toptc: ',xlb_toptc,xub_toptc,xguess_toptc
ENDIF
IF (xchk_fckd) THEN
    cont2 = cont2 + 1
    WRITE(fcChar(cont2),'(A9)')'fckd'
	WRITE(13,'(A9,3F15.5)')'fckd: ',xlb_fckd,xub_fckd,xguess_fckd
ENDIF

!WRITE(13,*)' Obj. Funct. '
!WRITE(13,*)(' HMLE  ',i=1,nafn)
!WRITE(13,*)(' RMSE  ',i=1,nafn)
!WRITE(13,*)(' Nash ',i=1,nafn)
!WRITE(13,*)(' RMSE-month',i=1,nafn),folon
!WRITE(13,*)(' %Vol ',i=1,nafn)
!WRITE(13,*)(' Coef-eff. ',i=1,nafn)
!WRITE(13,*)(' Qmax ',i=1,nafn)
!WRITE(13,*)(' Tpeak ',i=1,nafn)
!WRITE(13,*)(' Vol ',i=1,nafn)
!WRITE(13,*)(' KGE ',i=1,nafn)!(Vicente)
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
  CASE(5)
    selec='Vol. acum'  
  CASE(13)
    selec='Kling-Gupta Eff.'
END SELECT
WRITE(13,*)'The selected O.F. is: ',selec
WRITE(13,*)'    It has been computed from: ',nifo,' to ',nt
WRITE(13,*)
WRITE(13,124)(fcChar(i),i=1,nparam),'Obj Function',(('   HMLE  NO',i),i=1,kno),(('   HMLE  AM',i),i=1,kam),(('   HMLE  NI',i),i=1,kni),&
    (('   RMSE  NO',i),i=1,kno),(('   RMSE  AM',i),i=1,kam),(('   RMSE  NI',i),i=1,kni),&
    (('   Nash  NO',i),i=1,kno),(('   Nash  AM',i),i=1,kam),(('   Nash  NI',i),i=1,kni),&
    (('RMSE-mnt NO',i),i=1,kno),(('RMSE-mnt AM',i),i=1,kam),(('RMSE-mnt NI',i),i=1,kni),&
    (('  %Vol   NO',i),i=1,kno),(('  %Vol   AM',i),i=1,kam),(('  %Vol   NI',i),i=1,kni),&
    (('  Qmax   NO',i),i=1,kno),(('  Qmax   AM',i),i=1,kam),(('  Qmax   NI',i),i=1,kni),&
    (('  Tpeak  NO',i),i=1,kno),(('  Tpeak  AM',i),i=1,kam),(('  Tpeak  NI',i),i=1,kni),&
    ((' Vol Obs NO',i),i=1,kno),((' Vol Obs AM',i),i=1,kam),((' Vol Obs NI',i),i=1,kni),&
    (('   KGE   NO',i),i=1,kno),(('   KGE   AM',i),i=1,kam),(('   KGE   NI',i),i=1,kni),'iteracion' 
124 FORMAT(<nparam+1>(A13,x),<nafn*9>(A12,I2,x),A10)
END SUBROUTINE


! *************************************************************************
! * Subrutina que imprime los estadisticos principales durante el proceso
! * de optimizacion en el fichero de salida, sirve para R,H o Todos.
! *************************************************************************
SUBROUTINE imprestad_nitr
USE modtet
IMPLICIT NONE

!nafn = kno + kam + kni
DO i=1,kno
  xpar(cont,nparam+1+i)=estadnitr(i,8)  !colonne dei valori tentativi per i parametri 
  xpar(cont,nparam+1+nafn+i)=estadnitr(nafn+i,4) !(Qobs-Qsim)^2
  xpar(cont,nparam+1+nafn*2+i)=estadnitr(nafn+i,6) !(Qobs-Qsim)^2/(Qobs-Qobsmedio)^2
  xpar(cont,nparam+1+nafn*3+i)=estadnitr(nafn+i,10)!(Qobs-Qsim)^2/Qobs^2
  xpar(cont,nparam+1+nafn*4+i)=estadnitr(nafn+i,5) !% Error Volumen
  xpar(cont,nparam+1+nafn*5+i)=estadnitr(nafn+i,1) !Caudal pico Simulado
  xpar(cont,nparam+1+nafn*6+i)=estadnitr(nafn+i,2) !Tiempo al pico simulado
  xpar(cont,nparam+1+nafn*7+i)=estadnitr(nafn+i,7) !Vol Sim Hm³
  xpar(cont,nparam+1+nafn*8+i)=estadnitr(nafn+i,13)!KGE - GIAMBA mayo 2013
ENDDO
DO i=kno+1, kno+kam
  xpar(cont,nparam+1+i)=estadnitr(i,8)  !colonne dei valori tentativi per i parametri 
  xpar(cont,nparam+1+nafn+i)=estadnitr(nafn+i,4) !(Qobs-Qsim)^2
  xpar(cont,nparam+1+nafn*2+i)=estadnitr(nafn+i,6) !(Qobs-Qsim)^2/(Qobs-Qobsmedio)^2
  xpar(cont,nparam+1+nafn*3+i)=estadnitr(nafn+i,10)!(Qobs-Qsim)^2/Qobs^2
  xpar(cont,nparam+1+nafn*4+i)=estadnitr(nafn+i,5) !% Error Volumen
  xpar(cont,nparam+1+nafn*5+i)=estadnitr(nafn+i,1) !Caudal pico Simulado
  xpar(cont,nparam+1+nafn*6+i)=estadnitr(nafn+i,2) !Tiempo al pico simulado
  xpar(cont,nparam+1+nafn*7+i)=estadnitr(nafn+i,7) !Vol Sim Hm³
  xpar(cont,nparam+1+nafn*8+i)=estadnitr(nafn+i,13)!KGE - GIAMBA mayo 2013 
END DO
DO i=kno+kam+1, kno+kam+kni
  xpar(cont,nparam+1+i)=estadnitr(i,8)  !colonne dei valori tentativi per i parametri 
  xpar(cont,nparam+1+nafn+i)=estadnitr(nafn+i,4) !(Qobs-Qsim)^2
  xpar(cont,nparam+1+nafn*2+i)=estadnitr(nafn+i,6) !(Qobs-Qsim)^2/(Qobs-Qobsmedio)^2
  xpar(cont,nparam+1+nafn*3+i)=estadnitr(nafn+i,10)!(Qobs-Qsim)^2/Qobs^2
  xpar(cont,nparam+1+nafn*4+i)=estadnitr(nafn+i,5) !% Error Volumen
  xpar(cont,nparam+1+nafn*5+i)=estadnitr(nafn+i,1) !Caudal pico Simulado
  xpar(cont,nparam+1+nafn*6+i)=estadnitr(nafn+i,2) !Tiempo al pico simulado
  xpar(cont,nparam+1+nafn*7+i)=estadnitr(nafn+i,7) !Vol Sim Hm³
  xpar(cont,nparam+1+nafn*8+i)=estadnitr(nafn+i,13)!KGE - GIAMBA mayo 2013 
END DO

if(idfo.eq.6.or.idfo.eq.13)then!Vicente(10-2020)NSE,Coef-eff,KGE
    xpar(cont,nparam+1)=1.0 - xpar(cont,nparam+1) !fobj
endif
WRITE(13,124)(xpar(cont,i),i=1,nparam+1+(nafn)*9),cont
124 FORMAT(<nparam+1>(F13.5,x),<nafn*9>(F14.5,x),I7)



END SUBROUTINE

