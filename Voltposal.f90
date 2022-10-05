!**********************************************************
!* Subrutina que estima el volumen (interpolacion lineal) 
!* de la curva altura volumen, dado un nivel
!**********************************************************
SUBROUTINE vol_sal
USE modtet
IMPLICIT NONE

INTEGER b1

w=0
b1=0
DO j=1,emb(i).datos
  IF (b1.eq.0) THEN
    IF (niveldado.lt.emb(i).h(j)) THEN
      IF ((emb(i).h(j)-emb(i).h(j-1)).eq.0.0) THEN
	    qsale=0.0
		mensaje=strings(904)
		errr=2
        CALL errores
	  ELSE
        qsale=(niveldado-emb(i).h(j-1))*(emb(i).vol(j)-emb(i).vol(j-1))
	    qsale=qsale/(emb(i).h(j)-emb(i).h(j-1))+emb(i).vol(j-1)
      ENDIF
	  b1=1
      w=j
      EXIT
    ENDIF
  ENDIF
ENDDO
IF (niveldado.gt.emb(i).h(emb(i).datos)) THEN
  mensaje=strings(906)
  errr=2
  CALL errores
  IF (emb(i).h(emb(i).datos)-emb(i).h(emb(i).datos-1).eq.0.0) THEN
    mensaje= strings(904)
	errr=2
    CALL errores
	qsale=0.0
  ELSE
    qsale=(niveldado-emb(i).h(emb(i).datos))*(emb(i).vol(emb(i).datos)-    &
	      emb(i).vol(emb(i).datos-1))
    qsale=qsale/(emb(i).h(emb(i).datos)-emb(i).h(emb(i).datos-1))+emb(i).vol(emb(i).datos)
    w=emb(i).datos
  ENDIF
ENDIF
IF (qsale.lt.0.0) THEN
  mensaje= strings(910)
  errr=2
  CALL errores
  qsale=0.0
ENDIF
END SUBROUTINE vol_sal


!**********************************************************
!* Subrutina que estima el nivel (interpolacion lineal) 
!* de la curva altura volumen, dado un volumen
!**********************************************************
SUBROUTINE niv_sal
USE modtet
IMPLICIT NONE

INTEGER b1,ndatos

b1=0
qsale=0.0
ndatos=emb(i).datos
DO j=1,ndatos
  IF (b1.eq.0) THEN
    IF (volin.lt.emb(i).vol(j)) THEN
      IF ((emb(i).vol(j)-emb(i).vol(j-1)).eq.0.0) THEN
 		qsale=0.0
 		mensaje= strings(904)
		errr=2
        CALL errores
      ELSE
        qsale=(volin-emb(i).vol(j-1))*(emb(i).h(j)-emb(i).h(j-1))
	    qsale=qsale/(emb(i).vol(j)-emb(i).vol(j-1))+emb(i).h(j-1)
	  ENDIF
      b1=1
    ENDIF
  ENDIF
ENDDO
IF (volin.gt.emb(i).vol(ndatos)) THEN
  mensaje= strings(905)
  errr=2
  CALL errores
  IF (emb(i).vol(ndatos)-emb(i).vol(ndatos-1).eq.0.0) THEN
    qsale=0.0
	mensaje= strings(904)
	errr=2
    CALL errores
  ELSE
    qsale=(volin-emb(i).vol(ndatos))*(emb(i).h(ndatos)-emb(i).h(ndatos-1))  
    qsale=qsale/(emb(i).vol(ndatos)-emb(i).vol(ndatos-1))+emb(i).h(ndatos)
  ENDIF
ENDIF
IF (qsale.lt.0.0) THEN
  mensaje= strings(905)
  errr=2
  CALL errores
  qsale=0.0
ENDIF
END SUBROUTINE niv_sal


!************************************************************
!* Subrutina que estima el volumen S para el metodo 
!* del Pulso modificado, se emplea cuando se dispone de
!* la información de caudal de salida al embalse.
!************************************************************
SUBROUTINE caud_ent
USE modtet
IMPLICIT NONE

INTEGER b1,ndatos

b1=0
volin=0.0
ndatos=emb(i).datos
DO j=1,ndatos
  IF (b1.eq.0) THEN
    IF (qsale.lt.emb(i).out(j,2)) THEN
      IF ((emb(i).out(j,2)-emb(i).out(j-1,2)).eq.0.0) THEN
 	    qsale=0.0
    	mensaje=strings(911)
		errr=2
        CALL errores
      ELSE
        volin=(qsale-emb(i).out(j-1,2))*(emb(i).vol(j)-emb(i).vol(j-1))
        volin=volin/(emb(i).out(j,2)-emb(i).out(j-1,2))+emb(i).vol(j-1)
	  ENDIF
      b1=1
	  w=j
    ENDIF
  ENDIF
ENDDO
IF (qsale.gt.emb(i).out(ndatos,2)) THEN
  IF ((emb(i).out(ndatos,2)-emb(i).out(ndatos-1,2)).eq.0.0) THEN 
    qsale=0.0
	mensaje=strings(911)
	errr=2
    CALL errores
  ELSE
    volin=(qsale-emb(i).out(ndatos,2))*(emb(i).vol(ndatos)-emb(i).vol(ndatos-1))
    volin=volin/(emb(i).out(ndatos,2)-emb(i).out(ndatos-1,2))+emb(i).vol(ndatos)
  ENDIF
ENDIF
IF (qsale.lt.0.0) THEN
  qsale=0.0
ENDIF
END SUBROUTINE caud_ent

!************************************************************
!* Subrutina que estima el caudal de salida A por balance, 
!* conocida la curva H vs Vol, se emplea cuando se dispone de
!* la información de niveles N, pero faltan los caudales de
!* de salida Q. La curva de desagüe es el Qmax de Sim o Pred
!************************************************************
SUBROUTINE cau_sal
USE modtet
IMPLICIT NONE

INTEGER b1,ndatos

w=0
b1=0
ndatos=emb(i).datos
DO j=1,ndatos
  IF (b1.eq.0) THEN
    IF (niveldado.le.emb(i).h(j)) THEN
      IF ((emb(i).h(j)-emb(i).h(j-1)).eq.0.0) THEN
        qsale=0.0
		mensaje=strings(912)
		errr=2
        CALL errores
	  ELSE
        qsale=(niveldado-emb(i).h(j-1))*(emb(i).out(j,2)-emb(i).out(j-1,2))
        qsale=qsale/(emb(i).h(j)-emb(i).h(j-1))+emb(i).out(j-1,2)
      ENDIF
	  b1=1
      w=j
      EXIT
    ENDIF
  ENDIF
ENDDO

IF (niveldado.gt.emb(i).h(ndatos)) THEN
  mensaje=strings(914)
  errr=2
  CALL errores
  IF ((emb(i).h(ndatos)-emb(i).h(ndatos-1)).eq.0.0) THEN 
    qsale=0.0
	mensaje=strings(912)
	errr=2
    CALL errores
  ELSE
    qsale=(niveldado-emb(i).h(ndatos))*(emb(i).out(ndatos,2)-emb(i).out(ndatos-1,2))
    qsale=qsale/(emb(i).h(ndatos)-emb(i).h(ndatos-1))+emb(i).out(ndatos,2)
    w=ndatos
  ENDIF
ENDIF

IF (qsale.lt.0.0) THEN
  mensaje=strings(913)
  errr=2
  CALL errores
  qsale=0.0
ENDIF
END SUBROUTINE cau_sal


!****************************************************************
!* Subrutina que estima el caso de embalse (son 8 segun N,S,V)
!* CASO 1: Conocido N  CASO 2: Conocido V  CASO 3: Conocido S,
!* CASO 4: Conocido N,S CASO 5: Conocido V,S CASO 6: Conocido N,V
!* CASO 7: Conocido N,V,S CASO 8: Puls Modif (solo N sin datos)
!****************************************************************
SUBROUTINE cal_caso
USE modtet
!USE DFLIB

IMPLICIT NONE

DO i=1,nemb
  emb(i).caso=1
  IF (pulm(i).eq.1) THEN
    emb(i).caso=8
  ENDIF
  DO j=1,vnemb
    IF (emb(i).nombre.eq.emb(nemb+j).nombre) THEN
      emb(i).caso=6
	  emb(nemb+j).caso=-1
	  DO k=1,knemb
	    IF (emb(i).nombre.eq.emb(nemb+vnemb+k).nombre) THEN
		  emb(i).caso=7
		  emb(nemb+j).caso=-1
		  emb(nemb+vnemb+k).caso=-1
		ENDIF
	  ENDDO
	ENDIF
  ENDDO
  !Guiomar (20/02/2015): cambio hecho por error detectado en la asignación del caso 3
  DO j=1,knemb
    IF ((emb(i).caso).eq.1) THEN
	  IF (emb(i).nombre.eq.emb(nemb+vnemb+j).nombre) THEN
        emb(i).caso=4
	    emb(nemb+vnemb+j).caso=-1
      ENDIF
    ELSE IF ((emb(i).caso).eq.8) THEN
	  IF (emb(i).nombre.eq.emb(nemb+vnemb+j).nombre) THEN
        emb(i).caso=3
	    emb(nemb+vnemb+j).caso=-1
      ENDIF      
	ENDIF
  ENDDO
ENDDO

DO i=1,vnemb
  IF (emb(nemb+i).caso.eq.0) THEN
    emb(nemb+i).caso=2
    DO j=1,knemb
      IF (emb(nemb+i).nombre.eq.emb(nemb+vnemb+j).nombre) THEN
        emb(nemb+i).caso=5
		emb(nemb+vnemb+j).caso=-1
	  ENDIF
    ENDDO
  ENDIF
ENDDO

DO i=1,knemb
  IF ((emb(nemb+vnemb+i).caso.eq.0).and.(emb(i).caso.eq.0)) THEN
    emb(nemb+vnemb+i).caso=3
  ENDIF
ENDDO

!Esta parte de la subrutina pone los mismos valores en las series observadas
DO i=1,nemb
  IF (emb(i).caso.eq.3) THEN
    DO j=1,knemb
	  IF (emb(i).nombre.eq.emb(nemb+vnemb+j).nombre) THEN
	    DO t=0,nt
		  nivel(nemb+vnemb+j).obs(t)=nivel(i).obs(t)
		  qemb(i).obs(t)=qemb(nemb+vnemb+j).obs(t)
		ENDDO
	  ENDIF
	ENDDO
  ENDIF
  IF (emb(i).caso.eq.4) THEN
    DO j=1,knemb
	  IF (emb(i).nombre.eq.emb(nemb+vnemb+j).nombre) THEN
	    DO t=0,nt
		  nivel(nemb+vnemb+j).obs(t)=nivel(i).obs(t)
		  qemb(i).obs(t)=qemb(nemb+vnemb+j).obs(t)
		ENDDO
	  ENDIF
	ENDDO
  ENDIF
  IF (emb(i).caso.eq.6) THEN
    DO j=1,vnemb
	  IF (emb(i).nombre.eq.emb(nemb+j).nombre) THEN
	    DO t=0,nt
		  nivel(nemb+j).obs(t)=nivel(i).obs(t)
		  volum(i).obs(t)=volum(nemb+j).obs(t)
		ENDDO
	  ENDIF
	ENDDO
  ENDIF
  IF (emb(i).caso.eq.7) THEN
    DO j=1,vnemb
	  IF (emb(i).nombre.eq.emb(nemb+j).nombre) THEN
	    DO k=1,knemb
	      IF (emb(i).nombre.eq.emb(nemb+vnemb+k).nombre) THEN
	        DO t=0,nt
		      nivel(nemb+j).obs(t)=nivel(i).obs(t)
			  nivel(nemb+vnemb+k).obs(t)=nivel(i).obs(t)
		      volum(i).obs(t)=volum(nemb+j).obs(t)
			  volum(nemb+vnemb+k).obs(t)=volum(nemb+j).obs(t)
			  qemb(i).obs(t)=qemb(nemb+vnemb+k).obs(t)
			  qemb(nemb+j).obs(t)=qemb(nemb+vnemb+k).obs(t)
		    ENDDO
		  ENDIF
        ENDDO
	  ENDIF
	ENDDO
  ENDIF
ENDDO
DO i=1,vnemb
  IF (emb(nemb+i).caso.eq.5) THEN
    DO j=1,knemb
      IF (emb(nemb+i).nombre.eq.emb(nemb+vnemb+j).nombre) THEN
        DO t=0,nt
		  volum(nemb+vnemb+j).obs(t)=volum(nemb+i).obs(t)
		  qemb(nemb+i).obs(t)=qemb(nemb+vnemb+j).obs(t)
		ENDDO
	  ENDIF 
	ENDDO
  ENDIF
ENDDO
!DO i=1,(nemb+vnemb+knemb)
 !   errr=2
  !  mensaje= 'Casos elegidos: ' emb(i).casos
   ! CALL errores(errr,mensaje,lang)
!ENDDO
END SUBROUTINE  cal_caso


!****************************************************************
!* Subrutina que estima la fecha del ultimo intervalo de lluvia
!****************************************************************
SUBROUTINE tposal(fecin,horin,nt,dtmin,fecfin,horfin)
!USE DFPORT
!USE DFLIB

IMPLICIT NONE
CHARACTER fecin*11,horin*8,fecfin*11,horfin*8,aa*2,a4*4,a3*3
INTEGER imin,ihora,idia,iseg,iano,imes,ibis
INTEGER nt,nseg,i400,i100,i4,j
REAL dtmin

aa=fecin(1:2)
READ(aa,*)idia
a4=fecin(7:10)
READ(a4,*)iano
aa=horin(1:2)
READ(aa,*)ihora
aa=horin(4:5)
READ(aa,*)imin
aa=horin(7:8)
READ(aa,*)iseg
a3=fecin(4:5)
SELECT CASE (a3)
  CASE('01')
    imes=1
  CASE('02')
    imes=2
  CASE('03')
    imes=3
  CASE('04')
    imes=4
  CASE('05')
    imes=5
  CASE('06')
    imes=6
  CASE('07')
    imes=7
  CASE('08')
    imes=8
  CASE('09')
    imes=9
  CASE('10')
    imes=10
  CASE('11')
    imes=11
  CASE('12')
    imes=12
END SELECT


Nseg=dtmin*60*nt

! Calcula el Año
DO j=1,INT(Nseg/(86400*365))
  iano=iano+1
  i400=iano-(iano/400)*400
  i4=iano-(iano/4)*4
  i100=iano-(iano/100)*100
  IF (i400.eq.0.OR.i4.eq.0.AND.i100.ne.0) THEN
    ibis=366
  ELSE
    ibis=365
  ENDIF
  Nseg=Nseg-86400*ibis
ENDDO

! Calcula el Mes
i400=iano-(iano/400)*400
i4=iano-(iano/4)*4
i100=iano-(iano/100)*100
SELECT CASE(imes)
  CASE(1,3,5,7,8,10,12)
    ibis=31
  CASE(2) 
    IF (i400.eq.0.OR.i4.eq.0.AND.i100.ne.0) THEN
      ibis=29
    ELSE
      ibis=28
    ENDIF
  CASE(4,6,9,11)
    ibis=30
END SELECT
DO j=1,INT(Nseg/(86400*30))
  imes=imes+1
  IF (imes.gt.12) THEN
    imes=imes-12
    iano=iano+1
  ENDIF
  i400=iano-(iano/400)*400
  i4=iano-(iano/4)*4
  i100=iano-(iano/100)*100
  SELECT CASE(imes)
    CASE(1,3,5,7,8,10,12)
	  ibis=31
	CASE(2) 
      IF (i400.eq.0.OR.i4.eq.0.AND.i100.ne.0) THEN
        ibis=29
      ELSE
        ibis=28
      ENDIF
	CASE(4,6,9,11)
	  ibis=30
  END SELECT
  Nseg=Nseg-86400*ibis
ENDDO

! Calcula el Dia
DO j=1,INT(Nseg/(86400))
  idia=idia+1
  IF (idia.gt.ibis) THEN
    idia=idia-ibis
	imes=imes+1
	IF (imes.gt.12) THEN
	  imes=imes-12
	  iano=iano+1
	ENDIF
    i400=iano-(iano/400)*400
    i4=iano-(iano/4)*4
    i100=iano-(iano/100)*100
    SELECT CASE(imes)
      CASE(1,3,5,7,8,10,12)
	    ibis=31
	  CASE(2) 
        IF (i400.eq.0.OR.i4.eq.0.AND.i100.ne.0) THEN
          ibis=29
        ELSE
          ibis=28
        ENDIF
	  CASE(4,6,9,11)
	   ibis=30
    END SELECT
  ENDIF
  Nseg=Nseg-86400
ENDDO

! Calcula las Horas
DO j=1,INT(Nseg/3600)
  ihora=ihora+1
  IF (ihora.ge.24) THEN
  	ihora=ihora-24
	idia=idia+1
    IF (idia.gt.ibis) THEN
      idia=idia-ibis
	  imes=imes+1
	  IF (imes.gt.12) THEN
	    imes=imes-12
	    iano=iano+1
	  ENDIF
      i400=iano-(iano/400)*400
      i4=iano-(iano/4)*4
      i100=iano-(iano/100)*100
      SELECT CASE(imes)
        CASE(1,3,5,7,8,10,12)
	      ibis=31
	    CASE(2) 
          IF (i400.eq.0.OR.i4.eq.0.AND.i100.ne.0) THEN
            ibis=29
          ELSE
            ibis=28
          ENDIF
	    CASE(4,6,9,11)
	      ibis=30
      END SELECT
  ENDIF
  ENDIF
  Nseg=Nseg-3600
ENDDO


! Calcula los Minutos
DO j=1,INT(Nseg/60)
  imin=imin+1
  IF (imin.ge.60)	 THEN
    imin=imin-60
    ihora=ihora+1
    IF (ihora.ge.24) THEN
  	  ihora=ihora-24
	  idia=idia+1
      IF (idia.gt.ibis) THEN
        idia=idia-ibis
	    imes=imes+1
	    IF (imes.gt.12) THEN
	      imes=imes-12
	      iano=iano+1
	    ENDIF
        i400=iano-(iano/400)*400
        i4=iano-(iano/4)*4
        i100=iano-(iano/100)*100
        SELECT CASE(imes)
          CASE(1,3,5,7,8,10,12)
	        ibis=31
	      CASE(2) 
            IF (i400.eq.0.OR.i4.eq.0.AND.i100.ne.0) THEN
              ibis=29
            ELSE
              ibis=28
            ENDIF
	      CASE(4,6,9,11)
	        ibis=30
        END SELECT
      ENDIF
    ENDIF
  ENDIF
  Nseg=Nseg-60
ENDDO

! Segundos
DO j=1,INT(Nseg)
  iseg=iseg+1
  IF (iseg.ge.60) THEN
    iseg=iseg-60
    imin=imin+1
    IF (imin.ge.60)	 THEN
      imin=imin-60
      ihora=ihora+1
      IF (ihora.ge.24) THEN
  	    ihora=ihora-24
	    idia=idia+1
        IF (idia.gt.ibis) THEN
          idia=idia-ibis
	      imes=imes+1
	      IF (imes.gt.12) THEN
	        imes=imes-12
	        iano=iano+1
	      ENDIF
          i400=iano-(iano/400)*400
          i4=iano-(iano/4)*4
          i100=iano-(iano/100)*100
          SELECT CASE(imes)
            CASE(1,3,5,7,8,10,12)
	          ibis=31
	        CASE(2) 
              IF (i400.eq.0.OR.i4.eq.0.AND.i100.ne.0) THEN
                ibis=29
              ELSE
                ibis=28
              ENDIF
	        CASE(4,6,9,11)
	          ibis=30
          END SELECT
        ENDIF
      ENDIF
    ENDIF
  ENDIF
ENDDO

fecfin(1:11)='  /  /     '
WRITE(aa,88)idia
IF (idia.lt.10) THEN
  aa(1:1)='0'
ENDIF
fecfin(1:2)=aa
SELECT CASE (imes)
  CASE(1)
    a3='01'
  CASE(2)
    a3='02'
  CASE(3)
    a3='03'
  CASE(4)
    a3='04'
  CASE(5)
    a3='05'
  CASE(6)
    a3='06'
  CASE(7)
    a3='07'
  CASE(8)
    a3='08'
  CASE(9)
    a3='09'
  CASE(10)
    a3='10'
  CASE(11)
    a3='11'
  CASE(12)
    a3='12'
END SELECT
fecfin(4:5)=a3
WRITE(a4,84)iano
fecfin(7:10)=a4

horfin='00:00:00'
WRITE(aa,88)ihora
IF (ihora.lt.10) THEN
  aa(1:1)='0'
  IF (ihora.eq.0) aa='00'
ENDIF
horfin(1:2)=aa
WRITE(aa,88)imin
IF (imin.lt.10) THEN
  aa(1:1)='0'
  IF (imin.eq.0) aa='00'
ENDIF
horfin(4:5)=aa
WRITE(aa,88)iseg
IF (iseg.lt.10) THEN
  aa(1:1)='0'
  IF (iseg.eq.0) aa='00'
ENDIF
horfin(7:8)=aa

84 FORMAT(I4)
88 FORMAT(I2)

END SUBROUTINE tposal


!**************************************************
!* Subrutina de calculo de distancia entre celdas
!**************************************************
SUBROUTINE dis_cel
USE modtet
IMPLICIT NONE

DO n=1,ncel
  IF (cell(n).dest.eq.0) THEN
    cell(n).lon=(dx*dy)**.5
  ELSE
	cell(n).lon=SQRT((dx*(cell(n).fil-cell(cell(n).dest).fil))*(dx*(cell(n).fil-   &
	            cell(cell(n).dest).fil))+(dy*(cell(n).col-cell(cell(n).dest).col))*     &
				(dy*(cell(n).col-cell(cell(n).dest).col)))
  ENDIF
ENDDO

END SUBROUTINE