SUBROUTINE Merger_serial(A_A,NA_N,B_B,NB_N,C_C,NC_N)
    use modtet
    IMPLICIT NONE
     
   integer, intent(in) :: NA_N,NB_N,NC_N         
   TYPE (celdas), intent(in out) :: A_A(NA_N)
   TYPE (celdas), intent(in)     :: B_B(NB_N)
   TYPE (celdas), intent(in out) :: C_C(NC_N)
 
   integer :: I_I,J_J,K_K
 
   I_I = 1; J_J = 1; K_K = 1;
   DO WHILE(I_I <= NA_N .and. J_J <= NB_N)
      IF (A_A(I_I).acum <= B_B(J_J).acum) THEN
         C_C(K_K) = A_A(I_I)
         I_I = I_I+1
      ELSE
         C_C(K_K) = B_B(J_J)
         J_J = J_J+1
      END IF
      K_K = K_K + 1
   END DO
   DO WHILE (I_I <= NA_N)
      C_C(K_K) = A_A(I_I)
      I_I = I_I + 1
      K_K = K_K + 1
   END DO
   RETURN
 
END SUBROUTINE Merger_serial


SUBROUTINE BinarySort(A_A, N_N) 
    use modtet
    IMPLICIT NONE
    
   integer, intent(in) :: N_N !size of array
   TYPE (celdas), dimension(N_N), intent(in out) :: A_A !array to be sorted
   
   TYPE (celdas) :: Tmp_tmp
   
   INTEGER :: I_I, J_J, Low, High, Mid
   

   DO I_I = 2, N_N !do every item A_A(1) is the starting piont and with be moved if need 
        !search the allready sorted
		Low = 1 
		High = I_I; !the new particle
		Mid = Low + ((High-Low)*0.5) !find the mid piont
    
		compare: DO WHILE (Low<High) !find the piont where it belongs
			IF (A_A(I_I).acum > A_A(Mid).acum) THEN 
				Low = Mid + 1; ! search right
			ELSE IF (A_A(I_I).acum < A_A(Mid).acum) THEN
				High = Mid; !search left
			ELSE
				EXIT compare !found spot exit 
			END IF 
			Mid = Low + ((High - Low)*0.5) ! next mid piont
		END DO compare
    
		IF (Mid < I_I .and. (A_A(I_I).acum .ne. A_A(I_I-1).acum)) THEN ! does the particle 
			Tmp_tmp =  A_A(I_I)
			DO J_J = I_I-1, mid, -1
				A_A(J_J+1) = A_A(J_J) !shift up
			END DO
			A_A(mid) = Tmp_tmp
		END IF
	END DO
	
    END SUBROUTINE BinarySort


RECURSIVE SUBROUTINE MergeSort_serial(A_A,N_N)
    use modtet
    IMPLICIT NONE
    
   integer, INTENT(in) :: N_N
   TYPE (celdas),  dimension(N_N), INTENT(in out) :: A_A
   TYPE (celdas), ALLOCATABLE :: T_T(:)
   integer :: NA_N,NB_N
 
   IF (N_N <= 64) THEN !64 is considered small I_I think it depends on cache size 
        CALL BinarySort (A_A, N_N) 
        RETURN
   END IF
   
   NA_N=(N_N+1)/2
   NB_N=N_N-NA_N
   
   CALL MergeSort_serial(A_A,NA_N)
   CALL MergeSort_serial(A_A(NA_N+1),NB_N)
   
   ALLOCATE(T_T(NA_N))
   IF (A_A(N_N).acum < A_A(1).acum) THEN
      T_T(1:NA_N)=A_A(1:NA_N)
      A_A(1:NB_N)=A_A(NA_N+1:N_N)
      A_A(NB_N+1:N_N)=T_T(1:NA_N)
   ELSE IF (A_A(NA_N).acum > A_A(NA_N+1).acum) THEN
      T_T(1:NA_N)=A_A(1:NA_N)
      CALL Merger_serial(T_T,NA_N,A_A(NA_N+1),NB_N,A_A,N_N) 
   END IF
   DEALLOCATE(T_T)
   RETURN
 
END SUBROUTINE MergeSort_serial
    
    
SUBROUTINE Merger_serial_filCol(A_A,NA_N,B_B,NB_N,C_C,NC_N)
    use modtet
    IMPLICIT NONE
     
   integer, intent(in) :: NA_N,NB_N,NC_N         ! Normal usage: NA_N+NB_N = NC_N
   TYPE (fil__cols), intent(in out) :: A_A(NA_N)        ! B_B overlays C_C(NA_N+1:NC_N)
   TYPE (fil__cols), intent(in)     :: B_B(NB_N)
   TYPE (fil__cols), intent(in out) :: C_C(NC_N)
 
   integer :: I_I,J_J,K_K
 
   I_I = 1; J_J = 1; K_K = 1;
   DO WHILE(I_I <= NA_N .and. J_J <= NB_N)
      IF (A_A(I_I).fil <= B_B(J_J).fil) THEN
         C_C(K_K) = A_A(I_I)
         I_I = I_I+1
      ELSE
         C_C(K_K) = B_B(J_J)
         J_J = J_J+1
      END IF
      K_K = K_K + 1
   END DO
   DO WHILE (I_I <= NA_N) !parrall
      C_C(K_K) = A_A(I_I)
      I_I = I_I + 1
      K_K = K_K + 1
   END DO
   RETURN
 
END SUBROUTINE Merger_serial_filCol



SUBROUTINE BinarySort_filCol(A_A, N_N) 
    use modtet
    IMPLICIT NONE
    
   integer, intent(in) :: N_N !size of array
   TYPE (fil__cols), dimension(N_N), intent(in out) :: A_A !array to be sorted
   
   TYPE (fil__cols) :: Tmp_tmp
   
   INTEGER :: I_I, J_J, Low, High, Mid
   

   DO I_I = 2, N_N !do every item A_A(1) is the starting piont and with be moved if need 
        !search the allready sorted
		Low = 1 
		High = I_I; !the new particle
		Mid = Low + ((High-Low)*0.5) !find the mid piont
    
		compare: DO WHILE (Low<High) !find the piont where it belongs
			IF (A_A(I_I).fil > A_A(Mid).fil) THEN 
				Low = Mid + 1; ! search right
			ELSE IF (A_A(I_I).fil < A_A(Mid).fil) THEN
				High = Mid; !search left
			ELSE
				EXIT compare !found spot exit 
			END IF 
			Mid = Low + ((High - Low)*0.5) ! next mid piont
		END DO compare
    
		IF (Mid < I_I .and. (A_A(I_I).fil .ne. A_A(I_I-1).fil)) THEN ! does the particle 
			Tmp_tmp =  A_A(I_I)
			DO J_J = I_I-1, mid, -1
				A_A(J_J+1) = A_A(J_J) !shift up
			END DO
			A_A(mid) = Tmp_tmp
		END IF
	END DO
	
    END SUBROUTINE BinarySort_filCol


RECURSIVE SUBROUTINE MergeSort_serial_filCol(A_A,N_N)
    use modtet
    IMPLICIT NONE
    
   integer, INTENT(in) :: N_N
   TYPE (fil__cols),  dimension(N_N), INTENT(in out) :: A_A
   TYPE (fil__cols), ALLOCATABLE :: T_T(:) 
   integer :: NA_N,NB_N
 
   IF (N_N <= 64) THEN !64 is considered small I_I think it depends on cache size 
        CALL BinarySort_filCol (A_A, N_N) 
        RETURN
   END IF
   
   NA_N=(N_N+1)/2
   NB_N=N_N-NA_N
   
   CALL MergeSort_serial_filCol(A_A,NA_N)
   CALL MergeSort_serial_filCol(A_A(NA_N+1),NB_N)
   
   ALLOCATE(T_T(NA_N)) 
   IF (A_A(N_N).fil < A_A(1).fil) THEN !arrays joined wrong way round eg. 56781234
      T_T(1:NA_N)=A_A(1:NA_N)
      A_A(1:NB_N)=A_A(NA_N+1:N_N)
      A_A(NB_N+1:N_N)=T_T(1:NA_N)
   ELSE IF (A_A(NA_N).fil > A_A(NA_N+1).fil) THEN
      T_T(1:NA_N)=A_A(1:NA_N)
      CALL Merger_serial_filCol(T_T,NA_N,A_A(NA_N+1),NB_N,A_A,N_N) !apperntly if you pass A_A(NA_N+1) into an array which is meant to be N_N long it pass the array and the start piont no need to do A_A(NA_N+1:N_N)
   END IF
   DEALLOCATE(T_T)
   RETURN
 
END SUBROUTINE MergeSort_serial_filCol    