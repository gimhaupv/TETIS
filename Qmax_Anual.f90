!SUBROUTINE Qmax_Anual
!USE modtet
!IMPLICIT NONE
!
!Real,allocatable:: Qmax(:,:),precip(:),evap(:),Eros(:),AcC(:),AcNC(:),precipriego(:),intercep(:),evapotrans(:),Flujobasean(:,:),FlujobaseMM(:),escorrentiaMM(:),percolacion(:)
!Real,allocatable:: NO3acu(:),NH4acu(:),ConcacuNH4(:),ConcacuNO3(:)
!Integer dd,mm,aaaa,adummy,bdummy,massimocodveg
!Integer,allocatable:: fecha(:,:),fecha2(:)
!Real Resta
!Real,allocatable:: norgkgatotal(:),amoniokgatotal(:),nitratokgatotal(:),norgkgstotal(:),amoniokgstotal(:)
!Real,allocatable:: norgkga(:,:),amoniokga(:,:),nitratokga(:,:),norgkgs(:,:),amoniokgs(:,:)
!
!Allocate (Qmax(nt,naf),precip(nt),fecha(nt,2),evap(nt),fecha2(nt),Eros(nt),AcC(nt),AcNC(nt))
!Allocate (precipriego(nt),intercep(nt),evapotrans(nt),flujobasean(nt,naf),flujobaseMM(nt),escorrentiaMM(nt),percolacion(nt))
!Allocate (NO3acu(nt),NH4acu(nt),ConcacuNH4(nt),ConcacuNO3(nt))
!
!Qmax=0.0
!precip=0.0
!Eros=0.0
!evap=0.0
!fecha=0
!AcC=0.0
!AcNC=0.0
!precipriego=0.0
!intercep=0.0
!evapotrans=0.0
!percolacion=0.0
!flujobasean=0.0 !Se guarda por aforo
!flujobaseMM=0.0 !Se guarda la suma de los aforos del proyecto
!escorrentiaMM=0.0 !Guarda la salida en mm de todos los aforos
!NO3acu=0.0
!NH4acu=0.0
!adummy=1
!bdummy=1
!aa1=archin(1:2)
!Read(aa1,*)dd
!aa1=archin(3:4)
!Read(aa1,*)mm
!Read(archin(11:14),*)aaaa
!fecha2(1)=aaaa
!
!
!Do i=1,nt
!    ConcacuNO3(i)=(balanc_nitr(i,45)*1000)/(balanc(i,16)/1000*arcel) !Concentraciones medias diarias en g/m3=mg/dm3=mg/l
!    ConcacuNH4(i)=(balanc_nitr(i,44)*1000)/(balanc(i,16)/1000*arcel)
!End do
!
!Do i=1,nt
!    Do k=1,naf
!        If (aforo(k).sim(i)>Qmax(adummy,k)) then
!            Qmax(adummy,k)=aforo(k).sim(i)
!        End if
!    End do
!    precip(bdummy)=precip(bdummy)+balanc(i,1)
!    evap(bdummy)=evap(bdummy)+preac(i,2)
!    precipriego(adummy)=precipriego(adummy)+balanc(i,1)+balanc(i,28)
!    intercep(adummy)=intercep(adummy)+balanc(i,7)
!    evapotrans(adummy)=evapotrans(adummy)+balanc(i,8)
!    Do j=1,naf
!        !flujobasean(adummy,j)=flujobasean(adummy,j)+flujobase(i,j)
!        !flujobaseMM(adummy)=flujobaseMM(adummy)+flujobase(i,j)
!        EscorrentiaMM(adummy)=EscorrentiaMM(adummy)+aforo(j).sim(i)
!    End do
!    percolacion(adummy)=percolacion(adummy)+balanc(i,5)
!    AcC(adummy)=AcC(adummy)+balanc(i,29)
!    AcNC(adummy)=AcNC(adummy)+balanc(i,30)
!    If(modulos2(3)) then
!        NO3acu(bdummy)=NO3acu(bdummy)+ConcacuNO3(i)
!        NH4acu(bdummy)=NH4acu(bdummy)+ConcacuNH4(i)
!    End if
!    dd=dd+1
!    If (mm.eq.1.or.mm.eq.3.or.mm.eq.5.or.mm.eq.7.or.mm.eq.8.or.mm.eq.10.or.mm.eq.12)then
!        If (dd.gt.31) then
!            dd=1
!            fecha(bdummy,1)=mm
!            fecha(bdummy,2)=aaaa
!            mm=mm+1
!            NO3acu(bdummy)=NO3acu(bdummy)/31
!            NH4acu(bdummy)=NH4acu(bdummy)/31
!            bdummy=bdummy+1
!            If (mm.gt.12) then
!                mm=1
!                fecha2(adummy)=aaaa
!                If (config(4)) then
!                    Resta=0.0
!                    Do j=1,adummy
!                        Resta=Resta+Eros(j)
!                    End do    
!                    Eros(adummy)=balanc_sed(i,13)*ncel+balanc_sed(i,14)*ncel+balanc_sed(i,15)*ncel-Resta !Erosión anual
!                End if
!                If(i/=nt) then
!                    adummy=adummy+1
!                    aaaa=aaaa+1
!                End if
!            End if
!        End if
!    Else if (mm.eq.4.or.mm.eq.6.or.mm.eq.9.or.mm.eq.11) then
!        If (dd.gt.30) then
!            dd=1
!            fecha(bdummy,1)=mm
!            fecha(bdummy,2)=aaaa
!            mm=mm+1
!            NO3acu(bdummy)=NO3acu(bdummy)/30
!            NH4acu(bdummy)=NH4acu(bdummy)/30
!            bdummy=bdummy+1
!        End if
!    Else
!        If (mod(aaaa,4).eq.0.and.mod(aaaa,100).ne.0.or.mod(aaaa,400).eq.0) then
!            If (dd.gt.29) then
!                dd=1
!                fecha(bdummy,1)=mm
!                fecha(bdummy,2)=aaaa
!                mm=mm+1
!                NO3acu(bdummy)=NO3acu(bdummy)/29
!                NH4acu(bdummy)=NH4acu(bdummy)/29
!                bdummy=bdummy+1
!            End if
!        Else
!            If (dd.gt.28) then
!                dd=1
!                fecha(bdummy,1)=mm
!                fecha(bdummy,2)=aaaa
!                mm=mm+1
!                NO3acu(bdummy)=NO3acu(bdummy)/28
!                NH4acu(bdummy)=NH4acu(bdummy)/28
!                bdummy=bdummy+1
!            End if
!        End if
!    End if
!    If(i==nt) then 
!        fecha2(adummy)=aaaa
!        fecha(bdummy,1)=mm
!        fecha(bdummy,2)=aaaa
!    End if
!End do
!
!
!
!
!!Open(unit=7,file='Qmax_Anual.txt',status='unknown',action='write')
!!Do i=1,adummy
!!    write(7,'(I4,F12.5,2X,<naf>F12.5)')fecha2(i),(Qmax(i,j),j=1,naf),(Qmax(i,j)*60*60*24,j=1,naf)
!!End do
!!Close(7)
!Open(unit=7,file='Datos_Murcia.txt',status='unknown',action='write')
!Write(7,'(A6)')'Murcia'
!Write(7,'(A15)')'SISTEMATIC DATA'
!Write(7,'(8X,I2)')adummy-1   !Estoy eliminando el primer año como calentamiento
!Do i=2,adummy
!    write(7,'(1X,<naf>F12.3,2X,I4)')(60*60*24*Qmax(i,j),j=1,naf),fecha2(i)
!End do
!Write(7,'(A13)')'CENSORED DATA'
!Write(7,'(8X,A1)')'0'
!Write(7,'(A12)')'LOWER BOUNDS'
!Write(7,'(8X,A1)')'0'
!Write(7,'(A12)')'UPPER BOUNDS'
!Write(7,'(8X,A1)')'0'
!Write(7,'(A6)')'RANGES'
!Write(7,'(8X,A1)')'0'
!Close(7)
!
!!Open(unit=7,file='Medias_Mensuales.txt',status='unknown',action='write')
!!Do i=1,bdummy
!!    write(7,'(I2,2X,I4,2X,2F12.5)')Fecha(i,1),Fecha(i,2),precip(i),evap(i)
!!End do
!!Close(7)
!
!If (config(4)) then
!    Open(unit=7,file='Erosión_anual.txt',status='unknown',action='write')
!    Do i=1,adummy
!        write(7,'(I4,F20.5)')fecha2(i),abs(eros(i)*2.65/(arcel*ncel)*10000) !Erosión en t/ha año
!    End do
!    Close(7)
!End if
!
!!Open(unit=7,file='Resultados_Anuales.txt',status='unknown',action='write')
!!Write(7,'(X,A3,8A25)')'Año','Precipitación+Riego','Evaporación_Itercepción','Evapotranspiración','Aportación_Escorrentía','X4_Percolación','Aportación_FlujoBase','X9_Pérdidas_Cuaternario','X10_RecDirecta_Plioceno'
!!Do i=1,adummy
!!    write(7,'(I4,2X,8F25.10)') fecha2(i),precipriego(i),intercep(i),evapotrans(i),escorrentiaMM(i)/365*60*60*24*365/(arcel*ncel)*1000,percolacion(i),flujobaseMM(i)/ncel,AcC(i),AcNC(i)    !mm por año
!!End do
!!Close(7)
!
!!Open(unit=7,file='Salidas_Flujobase_Acuífero_Diario.txt',status='unknown',action='write') !Flujo base que sale de la última celda en m3
!!write(7,'(<naf>A15)')(aforo(j).name,j=1,naf)
!!Do t=1,nt
!!    write(7,'(<naf>F15.5)') (Flujobase(t,j)*arcel/1000,j=1,naf)   !m3/día
!!End do
!!Close(7)
!!Open(unit=7,file='Salidas_Flujobase_Acuífero_Anual.txt',status='unknown',action='write') !Flujo base que sale de la última celda en m3
!!write(7,'(<naf>A21)')(aforo(j).name,j=1,naf)
!!Do t=1,adummy
!!    write(7,'(<naf>F15.5)') (Flujobasean(t,j)*arcel/1000,j=1,naf)   !m3/año
!!End do
!!Close(7)
!
!!If (config(4)) then
!!    Open(unit=7,file='ConcMedia_Mensual_Acuífero.txt',status='unknown',action='write')
!!    Do i=1,bdummy
!!        write(7,'(I2,2X,I4,2X,2F12.5)')Fecha(i,1),Fecha(i,2),NH4acu(bdummy),NO3acu(bdummy) !Concentración en el acuífero
!!    End do
!!End if
!
!If(modulos2(3))then
!    !Open(unit=7,file='AsimPasivaNH4_Diaria.txt',status='unknown',action='write')
!    !    Do i=1,nt
!    !        write(7,'(I6,X,19F15.8)')i,(PasivaNH4(i,j)*14/18/(celdas2(j)*arcel)*10000/1000,j=1,19)    !!!!tN/ha, para cada uso del suelo
!    !    End do
!    !Close (7)
!    !Open(unit=7,file='AsimPasivaNO3_Diaria.txt',status='unknown',action='write')
!    !    Do i=1,nt
!    !        write(7,'(I6,X,19F15.8)')i,(PasivaNO3(i,j)*14/62/(celdas2(j)*arcel)*10000/1000,j=1,19)
!    !    End do
!    !Close (7)
!    !Open(unit=7,file='AsimActivaNH4_Diaria.txt',status='unknown',action='write')
!    !    Do i=1,nt
!    !        write(7,'(I6,X,19F15.8)')i,(ActivaNH4(i,j)*14/18/(celdas2(j)*arcel)*10000/1000,j=1,19)
!    !    End do
!    !Close (7)
!    !Open(unit=7,file='AsimActivaNO3_Diaria.txt',status='unknown',action='write')    
!    !    Do i=1,nt
!    !        write(7,'(I6,X,19F15.8)')i,(ActivaNO3(i,j)*14/62/(celdas2(j)*arcel)*10000/1000,j=1,19)
!    !    End do
!    !Close (7)
!    !
!    !Open(unit=7,file='AsimPasivaNH4_Anual.txt',status='unknown',action='write')
!    !    Do i=1,contador2-1
!    !        write(7,'(I4,X,19F15.8)')fecha2(i),(PasivasumaNH4(i,j)*14/18/(celdas2(j)*arcel)*10000/1000,j=1,19)
!    !    End do
!    !Close (7)
!    !Open(unit=7,file='AsimPasivaNO3_Anual.txt',status='unknown',action='write')
!    !    Do i=1,contador2-1
!    !        write(7,'(I4,X,19F15.8)')fecha2(i),(PasivasumaNO3(i,j)*14/62/(celdas2(j)*arcel)*10000/1000,j=1,19)
!    !    End do
!    !Close (7)
!    !Open(unit=7,file='AsimActivaNH4_Anual.txt',status='unknown',action='write')
!    !    Do i=1,contador2-1
!    !        write(7,'(I4,X,19F15.8)')fecha2(i),(ActivasumaNH4(i,j)*14/18/(celdas2(j)*arcel)*10000/1000,j=1,19)
!    !    End do
!    !Close (7)
!    !Open(unit=7,file='AsimActivaNO3_Anual.txt',status='unknown',action='write')
!    !    Do i=1,contador2-1
!    !        write(7,'(I4,X,19F15.8)')fecha2(i),(ActivasumaNO3(i,j)*14/62/(celdas2(j)*arcel)*10000/1000,j=1,19)
!    !    End do
!    !Close (7)
!    Open(unit=7,file='Asim_Celdas.txt',status='unknown',action='write')
!        Do i=1,19
!            !write(7,*)celdas2(i)
!        End do
!    Close (7)
!    Open(unit=7,file='RestoNitrsuelo_Anual.txt',status='unknown',action='write')
!        Do i=1,contador2-1
!            !write(7,'(I4,X,19F15.8)')fecha2(i),(RestoNitrsuelo(i,j)/(celdas2(j)*arcel)*10000/1000,j=1,19)
!        End do
!    Close (7)
!    Open(unit=7,file='Nitrogeno_regadio.txt',status='unknown',action='write')
!        Do i=1,contador3
!            !write(7,'(2F15.8)')pesoseco1(i)/(celdas2(8)*arcel)*10000/1000,nitrogenoacum1(i)/(celdas2(8)*arcel)*10000/1000  !tN/ha     !t/ha
!        End do
!    Close (7)
!    !Open(unit=7,file='Nitrogeno_cebada.txt',status='unknown',action='write')
!    !    Do i=1,contador4
!    !        write(7,'(2F15.8)')pesoseco2(i)/(celdas2(7)*arcel)*10000/1000,nitrogenoacum2(i)/(celdas2(7)*arcel)*10000/1000
!    !    End do
!    !Close (7)
!    
!    Open(unit=7,file='Asimilación_Anual.txt',status='unknown',action='write')
!        Do i=1,contador2-1
!            Do j=1,19
!                PasivasumaNH4(i,j)=PasivasumaNH4(i,j)/celdas2(j)    !kgN/ha
!                If(PasivasumaNH4(i,j)/=PasivasumaNH4(i,j))then
!                    PasivasumaNH4(i,j)=-1
!                End if
!                PasivasumaNO3(i,j)=PasivasumaNO3(i,j)/celdas2(j)
!                If(PasivasumaNO3(i,j)/=PasivasumaNO3(i,j))then
!                    PasivasumaNO3(i,j)=-1
!                End if
!                ActivasumaNH4(i,j)=ActivasumaNH4(i,j)/celdas2(j)
!                If(ActivasumaNH4(i,j)/=ActivasumaNH4(i,j))then
!                    ActivasumaNH4(i,j)=-1
!                End if
!                ActivasumaNO3(i,j)=ActivasumaNO3(i,j)/celdas2(j)
!                If(ActivasumaNO3(i,j)/=ActivasumaNO3(i,j))then
!                    ActivasumaNO3(i,j)=-1
!                End if
!            End do
!        End do
!        Write(7,'(A1)') '*'
!        Do i=1,contador2-1
!            write(7,'(I4,X,19F15.8)')fecha2(i),(PasivasumaNH4(i,j),j=1,19)
!        End do
!        Write(7,'(A1)') '*'
!        Do i=1,contador2-1
!            write(7,'(I4,X,19F15.8)')fecha2(i),(PasivasumaNO3(i,j),j=1,19)
!        End do
!        Write(7,'(A1)') '*'
!        Do i=1,contador2-1
!            write(7,'(I4,X,19F15.8)')fecha2(i),(ActivasumaNH4(i,j),j=1,19)
!        End do
!        Write(7,'(A1)') '*'
!        Do i=1,contador2-1
!            write(7,'(I4,X,19F15.8)')fecha2(i),(ActivasumaNO3(i,j),j=1,19)
!        End do
!        Write(7,'(A1)') '*'
!        Do i=1,19
!            write(7,*)celdas2(i)
!        End do
!    Close (7)
!
!    Open(unit=7,file='Asim_Celdas.txt',status='unknown',action='write')
!        Do i=1,19
!            write(7,*)celdas2(i)
!        End do
!    Close (7)
!    Open(unit=7,file='RestoNitrsuelo_Anual.txt',status='unknown',action='write')
!        Do i=1,contador2-1
!            write(7,'(I4,X,19F15.8)')fecha2(i),(RestoNitrsuelo(i,j)/(celdas2(j)*arcel)*10000/1000,j=1,19)
!        End do
!    Close (7)
!    
!    
!    !Caudales nitrógeno
!    Allocate (norgkga(naf,nt),amoniokga(naf,nt),nitratokga(naf,nt),norgkgs(naf,nt),amoniokgs(naf,nt))
!    Allocate (norgkgatotal(nt),amoniokgatotal(nt),nitratokgatotal(nt),norgkgstotal(nt),amoniokgstotal(nt))
!    norgkga=0.0
!    amoniokga=0.0
!    nitratokga=0.0
!    norgkgs=0.0
!    amoniokgs=0.0
!    norgkgatotal=0.0
!    amoniokgatotal=0.0
!    nitratokgatotal=0.0
!    norgkgstotal=0.0
!    amoniokgstotal=0.0
!    Do i=1,naf
!        Do t=1,nt
!            norgkga(i,t)=aforo(i).sim(i)*norgql(i).sim(t)*24*60*60*1000/1000000    !kg
!            amoniokga(i,t)=aforo(i).sim(i)*amonioql(i).sim(t)*24*60*60*1000/1000000
!            nitratokga(i,t)=aforo(i).sim(i)*nitratoql(i).sim(t)*24*60*60*1000/1000000
!            norgkgs(i,t)=aforosed(i).sim(i)*norgqs(i).sim(t)*24*60*60
!            amoniokgs(i,t)=aforosed(i).sim(i)*amonioqs(i).sim(t)*24*60*60
!        End do
!    End do
!    
!    Do t=1,nt
!        Do i=1,naf
!            norgkgatotal(t)=norgkgatotal(t)+norgkga(i,t)
!            amoniokgatotal(t)=amoniokgatotal(t)+amoniokga(i,t)
!            nitratokgatotal(t)=nitratokgatotal(t)+nitratokga(i,t)
!            norgkgstotal(t)=norgkgstotal(t)+norgkgs(i,t)
!            amoniokgstotal(t)=amoniokgstotal(t)+amoniokgs(i,t)
!        End do
!    End do
!            
!    Open(unit=7,file='Caudales_Totales_nitrógeno.txt',status='unknown',action='write')
!        Do t=1,nt
!            write(7,'(5F16.8)')norgkgatotal(t),amoniokgatotal(t),nitratokgatotal(t),norgkgstotal(t),amoniokgstotal(t)
!        End do
!    Close (7)
!    Open(unit=7,file='Limitación_Crecimiento.txt',status='unknown',action='write')
!        Do t=1,nt
!            write(7,'(9F12.4)')Gwn1(t)/celdas2(8),Gnn1(t)/celdas2(8),Gtn1(t)/celdas2(8),et0n1(t)/celdas2(8),fvegetacn1(t)/celdas2(8),fcubiertan1(t)/celdas2(8),riegon1(t)/celdas2(8),nitrogenoacumn1(t)/celdas2(8),pesosecon1(t)/celdas2(8)
!        End do
!    Close (7)
!    Open(unit=7,file='Nitr_acum.txt',status='unknown',action='write')
!        Do t=1,nt
!            write(7,'(F12.4)')nitracumulado(t)/celdas2(8)
!        End do
!    Close (7)
!End if
!End subroutine
