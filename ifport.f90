! *
! ********************************************************************************
! * INTEL CORPORATION                                                            *
! * Copyright 2003-2013 Intel Corporation All Rights Reserved.                   *
! *                                                                              *
! * The source code contained or described herein and all documents related to   *
! * the source code ("Material") are owned by Intel Corporation or its suppliers *
! * or licensors. Title to the Material remains with Intel Corporation or its    *
! * suppliers and licensors. The Material contains trade secrets and proprietary *
! * and confidential information of Intel or its suppliers and licensors. The    *
! * Material is protected by worldwide copyright and trade secret laws and       *
! * treaty provisions. No part of the Material may be used, copied, reproduced,  *
! * modified, published, uploaded, posted, transmitted, distributed, or          *
! * disclosed in any way without Intel's prior express written permission.       *
! *                                                                              *
! * No license under any patent, copyright, trade secret or other intellectual   *
! * property right is granted to or conferred upon you by disclosure or delivery *
! * of the Materials, either expressly, by implication, inducement, estoppel or  *
! * otherwise. Any license under such intellectual property rights must be       *
! * express and approved by Intel in writing.                                    *
! ********************************************************************************
! *

!
! THIS FILE PROVIDES AN INTERFACE TO INTEL'S
! PORTABILITY LIBRARY ROUTINES.
!
! THESE ROUTINES PROVIDE THE FUNCTIONALITY OF MANY
! COMMON LIBRARY EXTENSIONS TO THE FORTRAN LANGUAGE.
!
!  Command line:
!    ifort -c -D_WIN32 ifport.f90              -- for Windows* IA-32 architecture
!    ifort -c -D_WIN32 -D_M_IA64 ifport.f90    -- for Windows* IA-64 architecture
!    ifort -c ifport.f90                       -- for Linux* IA-32 architecture
!    ifort -c -D_M_IA64 ifport.f90             -- for Linux*  IA-64 architecture 
!    ifort -c -D_WIN32 -D__x86_64__ ifport.f90 -- for Windows* Intel(R) 64 architecture
!    ifort -c -D__x86_64__ ifport.f90          -- for Linux* Intel(R) 64 architecture
!    ifort -c -D__APPLE__ ifport.f90           -- for OS X* IA-32 architecture
!
      MODULE IFPORT_TYPES

!DEC$ OPTIONS /WARN=NOALIGN

!DEC$ IF DEFINED(_M_IA64) .OR. DEFINED(_M_AMD64) .OR. DEFINED(__x86_64__)
      INTEGER, PARAMETER :: SIZEOF_TIME_T = 8 
      INTEGER, PARAMETER :: SIZEOF_SIZE_T  = 8 
      INTEGER, PARAMETER :: SIZEOF_CLOCK_T = 8 
!DEC$ELSE 
      INTEGER, PARAMETER :: SIZEOF_TIME_T = 4 
      INTEGER, PARAMETER :: SIZEOF_SIZE_T  = 4
      INTEGER, PARAMETER :: SIZEOF_CLOCK_T = 4
!DEC$ ENDIF
      INTEGER, PARAMETER  :: POINTER_LEN = INT_PTR_KIND() ! 4 for Intel(R) IA-32 architecture
                                                          ! 8 for Intel(R) 64 architecture
                                                          ! 8 for Intel(R) IA-64 architecture

      INTEGER, PARAMETER :: JHANDLE_SIZE = POINTER_LEN 

      TYPE FILE$INFO
      SEQUENCE
      INTEGER(4)   CREATION      ! CREATION TIME (-1 ON FAT)
      INTEGER(4)   LASTWRITE     ! LAST WRITE TO FILE
      INTEGER(4)   LASTACCESS    ! LAST ACCESS (-1 ON FAT)
      INTEGER(4)   LENGTH        ! LENGTH OF FILE
      INTEGER(4)   PERMIT        ! FILE ACCESS MODE
      CHARACTER(LEN=255)  NAME   ! FILE NAME
      END TYPE

      TYPE FILE$INFOI8
      SEQUENCE
      INTEGER(4)   CREATION      ! CREATION TIME (-1 ON FAT)
      INTEGER(4)   LASTWRITE     ! LAST WRITE TO FILE
      INTEGER(4)   LASTACCESS    ! LAST ACCESS (-1 ON FAT)
  !DEC$ IF DEFINED(_WIN64)
      INTEGER(4)   RESERVED      ! RESERVED FOR ALIGNMENT
  !DEC$ ENDIF
      INTEGER(8)   LENGTH        ! LENGTH OF FILE
      INTEGER(4)   PERMIT        ! FILE ACCESS MODE
      CHARACTER(LEN=255)  NAME   ! FILE NAME
      END TYPE

!DEC$ END OPTIONS

      END MODULE IFPORT_TYPES

      MODULE IFPORT
      use IFPORT_TYPES

! VALUES FOR SIGNALQQ, RAISEQQ
      INTEGER(4), PARAMETER :: SIG$ERR   = -1
      INTEGER(4), PARAMETER :: SIG$INT   =  2
      INTEGER(4), PARAMETER :: SIG$ILL   =  4
      INTEGER(4), PARAMETER :: SIG$FPE   =  8
      INTEGER(4), PARAMETER :: SIG$SEGV  = 11
      INTEGER(4), PARAMETER :: SIG$TERM  = 15
      INTEGER(4), PARAMETER :: SIG$USR1  = 16
      INTEGER(4), PARAMETER :: SIG$USR2  = 17
      INTEGER(4), PARAMETER :: SIG$USR3  = 20
      INTEGER(4), PARAMETER :: SIG$BREAK = 21
      INTEGER(4), PARAMETER :: SIG$ABORT = 22
      INTEGER(4), PARAMETER :: SIG$NSIG  = 23
! VALUES FOR SIGNAL, KILL
      INTEGER(4), PARAMETER :: SIGINT  = 2   ! CTRL+C signal
      INTEGER(4), PARAMETER :: SIGILL  = 4   ! Illegal instruction
      INTEGER(4), PARAMETER :: SIGABRT = 6   ! Abnormal termination
      INTEGER(4), PARAMETER :: SIGFPE  = 8   ! Floating Point error
      INTEGER(4), PARAMETER :: SIGKILL = 9   ! Kill Process
      INTEGER(4), PARAMETER :: SIGTERM = 15  ! Termination request
      INTEGER(4), PARAMETER :: SIGSEGV = 11  ! Illegal storage access

! CONSTANTS FOR MODE SETTINGS
      INTEGER(4), PARAMETER :: S_IFMT   = O'0170000'
      INTEGER(4), PARAMETER :: S_IFDIR  = O'0040000'
      INTEGER(4), PARAMETER :: S_IFCHR  = O'0020000'
      INTEGER(4), PARAMETER :: S_IFBLK  = O'0060000'
      INTEGER(4), PARAMETER :: S_IFREG  = O'0100000'
      INTEGER(4), PARAMETER :: S_IFLNK  = O'0120000'
      INTEGER(4), PARAMETER :: S_IFSOCK = O'0140000'
      INTEGER(4), PARAMETER :: S_ISUID  = O'0004000'
      INTEGER(4), PARAMETER :: S_ISGID  = O'0002000'
      INTEGER(4), PARAMETER :: S_ISVTX  = O'0001000'
      INTEGER(4), PARAMETER :: S_IRWXU  = O'0000700'
      INTEGER(4), PARAMETER :: S_IRUSR  = O'0000400'
      INTEGER(4), PARAMETER :: S_IREAD  = O'0000400'
      INTEGER(4), PARAMETER :: S_IWUSR  = O'0000200'
      INTEGER(4), PARAMETER :: S_IWRITE = O'0000200'
      INTEGER(4), PARAMETER :: S_IXUSR  = O'0000100'
      INTEGER(4), PARAMETER :: S_IEXEC  = O'0000100'
      INTEGER(4), PARAMETER :: S_IRWXG  = O'0000070'
      INTEGER(4), PARAMETER :: S_IRGRP  = O'0000040'
      INTEGER(4), PARAMETER :: S_IWGRP  = O'0000020'
      INTEGER(4), PARAMETER :: S_IXGRP  = O'0000010'
      INTEGER(4), PARAMETER :: S_IRWXO  = O'0000007'
      INTEGER(4), PARAMETER :: S_IROTH  = O'0000004'
      INTEGER(4), PARAMETER :: S_IWOTH  = O'0000002'
      INTEGER(4), PARAMETER :: S_IXOTH  = O'0000001'
      INTEGER(4), PARAMETER :: MAX_GETCWD_LENGTH = 260

! Maximum length for a Host name
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!! constant MAX_COMPUTERNAME_LENGTH was renamed to MAX_HOSTNAM_LENGTH !!!
!!!!    INTEGER, PARAMETER :: MAX_COMPUTERNAME_LENGTH = 15 !!!!!!!!!!!!!!!!
     INTEGER, PARAMETER :: MAX_HOSTNAM_LENGTH = 15
! -----------------------------------------------------------------
! Process Control
! -----------------------------------------------------------------
      INTERFACE
! ABORT CURRENT PROCESS, CLOSE ALL FILES
        SUBROUTINE ABORT (STRING)
        !DEC$ ATTRIBUTES DEFAULT :: ABORT
          CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: STRING
        END SUBROUTINE

        INTEGER(4) FUNCTION ALARM( TIME, PROC )
        !DEC$ ATTRIBUTES DEFAULT :: ALARM
          INTEGER(4) TIME
          EXTERNAL PROC
        END FUNCTION ALARM
    
        SUBROUTINE EXIT(STATUS)
        !DEC$ ATTRIBUTES DEFAULT :: EXIT
          INTEGER(4), OPTIONAL, INTENT(IN) :: STATUS
        END SUBROUTINE
    
! KILL A PROCESS
        INTEGER(4) FUNCTION KILL(PID, SIGNUM)
        !DEC$ ATTRIBUTES DEFAULT :: KILL
          INTEGER(4), INTENT(IN) :: PID, SIGNUM
        END FUNCTION

! MAKES CURRENT PROCESS SLEEP FOR TIME SECONDS
        SUBROUTINE SLEEP(TIME)
        !DEC$ ATTRIBUTES DEFAULT :: SLEEP
          INTEGER(4), INTENT(IN) :: TIME
        END SUBROUTINE

        FUNCTION SIGNAL(SIGNUM, PROC, FLAG)
        use ifport_types
        !DEC$ ATTRIBUTES DEFAULT :: SIGNAL
          INTEGER(POINTER_LEN)   :: SIGNAL
          INTEGER(4)             :: SIGNUM, FLAG
          INTEGER(4), EXTERNAL   :: PROC
        END FUNCTION
   
! SEND COMMAND TO OS SHELL
        INTEGER FUNCTION SYSTEM(COMMAND)
        !DEC$ ATTRIBUTES DEFAULT :: SYSTEM
          CHARACTER(LEN=*) COMMAND
        END FUNCTION SYSTEM

      END INTERFACE

 
! -----------------------------------------------------------------
! Numeric Routines
! -----------------------------------------------------------------
      INTERFACE
! ** BESSEL FUNCTIONS
        REAL(4) FUNCTION BESJ0(X)
        !DEC$ ATTRIBUTES DEFAULT :: BESJ0
          REAL(4) X
        END FUNCTION
    
        REAL(4) FUNCTION BESJ1(X)
        !DEC$ ATTRIBUTES DEFAULT :: BESJ1
          REAL(4) X
        END FUNCTION
    
        REAL(4) FUNCTION BESJN(N,X)
        !DEC$ ATTRIBUTES DEFAULT :: BESJN
          INTEGER(4) N
          REAL(4) X
        END FUNCTION
    
        REAL(4) FUNCTION BESY0(X)
        !DEC$ ATTRIBUTES DEFAULT :: BESY0
          REAL(4) X
        END FUNCTION
    
        REAL(4) FUNCTION BESY1(X)
        !DEC$ ATTRIBUTES DEFAULT :: BESY1
          REAL(4) X
        END FUNCTION
    
        REAL(4) FUNCTION BESYN(N,X)
        !DEC$ ATTRIBUTES DEFAULT :: BESYN
          INTEGER(4) N
          REAL(4) X
        END FUNCTION
    
        REAL(8) FUNCTION DBESJ0(X)
        !DEC$ ATTRIBUTES DEFAULT :: DBESJ0
          REAL(8) X
        END FUNCTION
    
        REAL(8) FUNCTION DBESJ1(X)
        !DEC$ ATTRIBUTES DEFAULT :: DBESJ1
          REAL(8) X
        END FUNCTION
    
        REAL(8) FUNCTION DBESJN(N,X)
        !DEC$ ATTRIBUTES DEFAULT :: DBESJN
          REAL(8) X
          INTEGER(4) N
        END FUNCTION
    
        REAL(8) FUNCTION DBESY0(X)
        !DEC$ ATTRIBUTES DEFAULT :: DBESY0
          REAL(8) X
        END FUNCTION
    
        REAL(8) FUNCTION DBESY1(X)
        !DEC$ ATTRIBUTES DEFAULT :: DBESY1
          REAL(8) X
        END FUNCTION
    
        REAL(8) FUNCTION DBESYN(N,X)
        !DEC$ ATTRIBUTES DEFAULT :: DBESYN
          REAL(8) X
          INTEGER(4) N
        END FUNCTION
     END INTERFACE

! ** BIT-LEVEL FUNCTIONS
! BIT CLEAR FOR INTEGERS
     INTERFACE BIC
        SUBROUTINE BIC(BITNUM, TARGET)
        !DEC$ ATTRIBUTES DEFAULT :: BIC
          INTEGER(4), INTENT(IN) :: BITNUM
          INTEGER(4), INTENT(INOUT) :: TARGET
        END SUBROUTINE
        SUBROUTINE BICI8(BITNUM, TARGET)
        !DEC$ ATTRIBUTES DEFAULT :: BICI8
          INTEGER(4), INTENT(IN) :: BITNUM
          INTEGER(8), INTENT(INOUT) :: TARGET
        END SUBROUTINE
     END INTERFACE

! BIT SET FOR INTEGERS
      INTERFACE BIS
        SUBROUTINE BIS(BITNUM, TARGET)
        !DEC$ ATTRIBUTES DEFAULT :: BIS
          INTEGER(4), INTENT(IN) :: BITNUM
          INTEGER(4), INTENT(INOUT) :: TARGET
        END SUBROUTINE
        SUBROUTINE BISI8(BITNUM, TARGET)
        !DEC$ ATTRIBUTES DEFAULT :: BISI8
          INTEGER(4), INTENT(IN) :: BITNUM
          INTEGER(8), INTENT(INOUT) :: TARGET
        END SUBROUTINE
      END INTERFACE

! BIT TEST FOR INTEGERS
       INTERFACE BIT
        LOGICAL(4) FUNCTION BIT(BITNUM, SOURCE)
        !DEC$ ATTRIBUTES DEFAULT :: BIT
          INTEGER(4), INTENT(IN) :: BITNUM
          INTEGER(4), INTENT(IN) :: SOURCE
        END FUNCTION
        LOGICAL(4) FUNCTION BITI8(BITNUM, SOURCE)
        !DEC$ ATTRIBUTES DEFAULT :: BITI8
          INTEGER(4), INTENT(IN) :: BITNUM
          INTEGER(8), INTENT(IN) :: SOURCE
        END FUNCTION
       END INTERFACE

!!  Should be intrinsic
!        REAL(4) FUNCTION AMOD(A,B)! REAL MODULUS
!        !DEC$ ATTRIBUTES DEFAULT :: AMOD
!          REAL(4), INTENT(IN) :: A,B
!        END FUNCTION AMOD
    
!        REAL(8) FUNCTION DMOD(DN,DD)
!        !DEC$ ATTRIBUTES DEFAULT :: DMOD
!          REAL(8), INTENT(IN) :: DN,DD
!        END FUNCTION
    
!        INTEGER(2) FUNCTION IMOD(IA,IB)
!        !DEC$ ATTRIBUTES DEFAULT :: IMOD
!          INTEGER(2), INTENT(IN) :: IA,IB
!        END FUNCTION

! EFFECTIVELY A BITWISE STORE UNDER MASK
     INTERFACE
        INTEGER(4) FUNCTION CSMG(X, Y, Z)
        !DEC$ ATTRIBUTES DEFAULT :: CSMG
          INTEGER(4), INTENT(IN) :: X, Y, Z
        END FUNCTION

      END INTERFACE

      INTERFACE COMPL
        INTEGER(4) FUNCTION COMPLINT(INVAL)
        !DEC$ ATTRIBUTES DEFAULT :: COMPLINT
          INTEGER(4), INTENT(IN) :: INVAL
        END FUNCTION
        REAL(4) FUNCTION COMPLREAL(INVAL)
        !DEC$ ATTRIBUTES DEFAULT :: COMPLREAL
          REAL(4), INTENT(IN) :: INVAL
        END FUNCTION
        LOGICAL(4) FUNCTION COMPLLOG(INVAL)
        !DEC$ ATTRIBUTES DEFAULT :: COMPLLOG
          LOGICAL(4), INTENT(IN) :: INVAL
        END FUNCTION
      END INTERFACE
    
      INTERFACE

        INTEGER(8) FUNCTION DSHIFTL(LEFT, RIGHT, SHIFT)
        !DEC$ ATTRIBUTES DEFAULT :: DSHIFTL
          INTEGER(8) :: LEFT, RIGHT, SHIFT
        END FUNCTION

        INTEGER(8) FUNCTION DSHIFTR(LEFT,RIGHT,SHIFT)
        !DEC$ ATTRIBUTES DEFAULT :: DSHIFTR
          INTEGER(8) :: LEFT,RIGHT,SHIFT
        END FUNCTION

        INTEGER(4) FUNCTION JABS(I)
        !DEC$ ATTRIBUTES DEFAULT :: JABS
          INTEGER(4), INTENT(IN) :: I
        END FUNCTION

! RETURN MAXIMUM POSITIVE INTEGER
        INTEGER(4) FUNCTION INMAX(INPUT)
        !DEC$ ATTRIBUTES DEFAULT :: INMAX
          INTEGER(4), INTENT(IN) :: INPUT
        END FUNCTION
  
! CONVERT INTEGER(4) TO INTEGER(2)
        INTEGER(2) FUNCTION INTC(INPUT)
        !DEC$ ATTRIBUTES DEFAULT :: INTC
          INTEGER(4), INTENT(IN) :: INPUT
        END FUNCTION

! CONVERT INTEGER(4) TO INTEGER(2)
        INTEGER(2) FUNCTION SHORT(INPUT)
        !DEC$ ATTRIBUTES DEFAULT :: SHORT
          INTEGER(4) INPUT
        END FUNCTION SHORT

! CONVERT INTEGER(2) TO INTEGER(4)
        INTEGER(4) FUNCTION LONG(INPUT)
        !DEC$ ATTRIBUTES DEFAULT :: LONG
          INTEGER(2), INTENT(IN) :: INPUT
        END FUNCTION
   
        INTEGER(4) FUNCTION SHIFTL(IVALUE, ISHIFTCOUNT)
        !DEC$ ATTRIBUTES DEFAULT :: SHIFTL
          INTEGER(4), INTENT(IN) :: IVALUE, ISHIFTCOUNT
        END FUNCTION
  
        INTEGER(4) FUNCTION SHIFTR(IVALUE, ISHIFTCOUNT)
        !DEC$ ATTRIBUTES DEFAULT :: SHIFTR
          INTEGER(4), INTENT(IN) :: IVALUE, ISHIFTCOUNT
        END FUNCTION

! CONVERT INTEGER(2) TO  REAL(4)
        REAL(4) FUNCTION IFLOATI(INPUT)
        !DEC$ ATTRIBUTES DEFAULT :: IFLOATI
          INTEGER(2), INTENT(IN) :: INPUT
        END FUNCTION
  
        REAL(4) FUNCTION IFLOATJ(INPUT)
        !DEC$ ATTRIBUTES DEFAULT :: IFLOATJ
          INTEGER(4), INTENT(IN) :: INPUT
        END FUNCTION
  
! CONVERT INTEGER(2) TO  REAL(8)
        REAL(8) FUNCTION DFLOATI(INPUT)
        !DEC$ ATTRIBUTES DEFAULT :: DFLOATI
           INTEGER(2), INTENT(IN) :: INPUT
        END FUNCTION

! CONVERT INTEGER(4) TO REAL(8)
        REAL(8) FUNCTION DFLOATJ(INPUT)
        !DEC$ ATTRIBUTES DEFAULT :: DFLOATJ
           INTEGER(4), INTENT(IN) :: INPUT
        END FUNCTION

! CONVERT INTEGER(8) TO REAL(8)
        REAL(8) FUNCTION DFLOATK(INPUT)
        !DEC$ ATTRIBUTES DEFAULT :: DFLOATK
           INTEGER(8), INTENT(IN) :: INPUT
        END FUNCTION
  
! CONVERT COMPLEX TO REAL(8)
        REAL(8) FUNCTION CDFLOAT(INPUT)
        !DEC$ ATTRIBUTES DEFAULT :: CDFLOAT
            COMPLEX(4), INTENT(IN) :: INPUT
        END FUNCTION
  
        REAL(8) FUNCTION IDFLOAT(INPUT)
        !DEC$ ATTRIBUTES DEFAULT :: IDFLOAT
           INTEGER(4), INTENT(IN) :: INPUT
        END FUNCTION
      END INTERFACE

! -----------------------------------------------------------------
! String manipulation
! -----------------------------------------------------------------
      INTERFACE
        INTEGER(4) FUNCTION LNBLNK(STRING)
        !DEC$ ATTRIBUTES DEFAULT :: LNBLNK
          CHARACTER(LEN=*), INTENT(IN) :: STRING
        END FUNCTION

! LOCATES THE INDEX OF THE LAST OCCURRENCE
! OF A SUBSTRING WITHIN A STRING
        INTEGER(4) FUNCTION RINDEX(S1, S2)
        !DEC$ ATTRIBUTES DEFAULT :: RINDEX
          CHARACTER(LEN=*), INTENT(IN) :: S1, S2
        END FUNCTION RINDEX
      END INTERFACE

! -----------------------------------------------------------------
! Sorting and Searching Arrays
! -----------------------------------------------------------------

      INTERFACE QSORT
           SUBROUTINE qsort_i1( array, len, isize, compar )
!DEC$ IF  DEFINED(_WIN32) 
           !DEC$ ATTRIBUTES DEFAULT,DECORATE,ALIAS:'QSORT' ::  QSORT_I1
!DEC$ ELSEIF DEFINED(__APPLE__)
           !DEC$ ATTRIBUTES DEFAULT,ALIAS:'_qsort_' :: QSORT_I1
!DEC$ ELSE
           !DEC$ ATTRIBUTES DEFAULT,ALIAS:'qsort_' :: QSORT_I1
!DEC$ ENDIF
           use IFPORT_TYPES
             integer(1) array(*)
             integer(SIZEOF_SIZE_T) len, isize
             integer(2), external :: compar
           END SUBROUTINE

           SUBROUTINE qsort_i2( array, len, isize, compar )
!DEC$ IF  DEFINED(_WIN32) 
           !DEC$ ATTRIBUTES DEFAULT,DECORATE,ALIAS:'QSORT' ::  QSORT_I2
!DEC$ ELSEIF DEFINED(__APPLE__)
           !DEC$ ATTRIBUTES DEFAULT,ALIAS:'_qsort_' :: QSORT_I2
!DEC$ ELSE
           !DEC$ ATTRIBUTES DEFAULT,ALIAS:'qsort_' :: QSORT_I2
!DEC$ ENDIF
           use IFPORT_TYPES
             integer(2) array(*)
             integer(SIZEOF_SIZE_T) len, isize
             integer(2), external :: compar
           END SUBROUTINE

           SUBROUTINE qsort_i4( array, len, isize, compar )
!DEC$ IF  DEFINED(_WIN32) 
           !DEC$ ATTRIBUTES DEFAULT,DECORATE,ALIAS:'QSORT' ::  QSORT_I4
!DEC$ ELSEIF DEFINED(__APPLE__)
           !DEC$ ATTRIBUTES DEFAULT,ALIAS:'_qsort_' :: QSORT_I4
!DEC$ ELSE
           !DEC$ ATTRIBUTES DEFAULT,ALIAS:'qsort_' :: QSORT_I4
!DEC$ ENDIF
           use IFPORT_TYPES
             integer(4) array(*)
             integer(SIZEOF_SIZE_T) len, isize
             integer(2), external :: compar
           END SUBROUTINE

           SUBROUTINE qsort_i8( array, len, isize, compar )
!DEC$ IF  DEFINED(_WIN32) 
           !DEC$ ATTRIBUTES DEFAULT,DECORATE,ALIAS:'QSORT' ::  QSORT_I8
!DEC$ ELSEIF DEFINED(__APPLE__)
           !DEC$ ATTRIBUTES DEFAULT,ALIAS:'_qsort_' :: QSORT_I8
!DEC$ ELSE
           !DEC$ ATTRIBUTES DEFAULT,ALIAS:'qsort_' :: QSORT_I8
!DEC$ ENDIF
           use IFPORT_TYPES
             integer(8) array(*)
             integer(SIZEOF_SIZE_T) len, isize
             integer(2), external :: compar
           END SUBROUTINE

           SUBROUTINE qsort_l1( array, len, isize, compar )
!DEC$ IF  DEFINED(_WIN32) 
           !DEC$ ATTRIBUTES DEFAULT,DECORATE,ALIAS:'QSORT' ::  QSORT_L1
!DEC$ ELSEIF DEFINED(__APPLE__)
           !DEC$ ATTRIBUTES DEFAULT,ALIAS:'_qsort_' :: QSORT_L1
!DEC$ ELSE
           !DEC$ ATTRIBUTES DEFAULT,ALIAS:'qsort_' :: QSORT_L1
!DEC$ ENDIF
           use IFPORT_TYPES
             logical(1) array(*)
             integer(SIZEOF_SIZE_T) len, isize
             integer(2), external :: compar
           END SUBROUTINE

           SUBROUTINE qsort_l2( array, len, isize, compar )
!DEC$ IF  DEFINED(_WIN32) 
           !DEC$ ATTRIBUTES DEFAULT,DECORATE,ALIAS:'QSORT' ::  QSORT_L2
!DEC$ ELSEIF DEFINED(__APPLE__)
           !DEC$ ATTRIBUTES DEFAULT,ALIAS:'_qsort_' :: QSORT_L2
!DEC$ ELSE
           !DEC$ ATTRIBUTES DEFAULT,ALIAS:'qsort_' :: QSORT_L2
!DEC$ ENDIF
           use IFPORT_TYPES
             logical(2) array(*)
             integer(SIZEOF_SIZE_T) len, isize
             integer(2), external :: compar
           END SUBROUTINE

           SUBROUTINE qsort_l4( array, len, isize, compar )
!DEC$ IF  DEFINED(_WIN32) 
           !DEC$ ATTRIBUTES DEFAULT,DECORATE,ALIAS:'QSORT' ::  QSORT_L4
!DEC$ ELSEIF DEFINED(__APPLE__)
           !DEC$ ATTRIBUTES DEFAULT,ALIAS:'_qsort_' :: QSORT_L4
!DEC$ ELSE
           !DEC$ ATTRIBUTES DEFAULT,ALIAS:'qsort_' :: QSORT_L4
!DEC$ ENDIF
           use IFPORT_TYPES
             logical(4) array(*)
             integer(SIZEOF_SIZE_T) len, isize
             integer(2), external :: compar
           END SUBROUTINE

           SUBROUTINE qsort_l8( array, len, isize, compar )
!DEC$ IF  DEFINED(_WIN32) 
           !DEC$ ATTRIBUTES DEFAULT,DECORATE,ALIAS:'QSORT' :: QSORT_L8
!DEC$ ELSEIF DEFINED(__APPLE__)
           !DEC$ ATTRIBUTES DEFAULT,ALIAS:'_qsort_' :: QSORT_L8
!DEC$ ELSE
           !DEC$ ATTRIBUTES DEFAULT,ALIAS:'qsort_' :: QSORT_L8
!DEC$ ENDIF
           use IFPORT_TYPES
             logical(8) array(*)
             integer(SIZEOF_SIZE_T) len, isize
             integer(2), external :: compar
           END SUBROUTINE

           SUBROUTINE qsort_r4( array, len, isize, compar )
!DEC$ IF  DEFINED(_WIN32) 
           !DEC$ ATTRIBUTES DEFAULT,DECORATE,ALIAS:'QSORT' ::  QSORT_R4
!DEC$ ELSEIF DEFINED(__APPLE__)
           !DEC$ ATTRIBUTES DEFAULT,ALIAS:'_qsort_' :: QSORT_R4
!DEC$ ELSE
           !DEC$ ATTRIBUTES DEFAULT,ALIAS:'qsort_' :: QSORT_R4
!DEC$ ENDIF
           use IFPORT_TYPES
             real(4) array(*)
             integer(SIZEOF_SIZE_T) len, isize
             integer(2), external :: compar
           END SUBROUTINE

           SUBROUTINE qsort_r8( array, len, isize, compar )
!DEC$ IF  DEFINED(_WIN32) 
           !DEC$ ATTRIBUTES DEFAULT,DECORATE,ALIAS:'QSORT' ::  QSORT_R8
!DEC$ ELSEIF DEFINED(__APPLE__)
           !DEC$ ATTRIBUTES DEFAULT,ALIAS:'_qsort_' :: QSORT_R8
!DEC$ ELSE
           !DEC$ ATTRIBUTES DEFAULT,ALIAS:'qsort_' :: QSORT_R8
!DEC$ ENDIF
           use IFPORT_TYPES
             real(8) array(*)
             integer(SIZEOF_SIZE_T) len, isize
             integer(2), external :: compar
           END SUBROUTINE

           SUBROUTINE qsort_r16( array, len, isize, compar )
!DEC$ IF  DEFINED(_WIN32) 
           !DEC$ ATTRIBUTES DEFAULT,DECORATE,ALIAS:'QSORT' ::  QSORT_R16
!DEC$ ELSEIF DEFINED(__APPLE__)
           !DEC$ ATTRIBUTES DEFAULT,ALIAS:'_qsort_' :: QSORT_R16
!DEC$ ELSE
           !DEC$ ATTRIBUTES DEFAULT,ALIAS:'qsort_' :: QSORT_R16
!DEC$ ENDIF
           use IFPORT_TYPES
             real(16) array(*)
             integer(SIZEOF_SIZE_T) len, isize
             integer(2), external :: compar
           END SUBROUTINE

           SUBROUTINE qsort_c8( array, len, isize, compar )
!DEC$ IF  DEFINED(_WIN32) 
           !DEC$ ATTRIBUTES DEFAULT,DECORATE,ALIAS:'QSORT' ::  QSORT_C8
!DEC$ ELSEIF DEFINED(__APPLE__)
           !DEC$ ATTRIBUTES DEFAULT,ALIAS:'_qsort_' :: QSORT_C8
!DEC$ ELSE
           !DEC$ ATTRIBUTES DEFAULT,ALIAS:'qsort_' :: QSORT_C8
!DEC$ ENDIF
           use IFPORT_TYPES
             complex(4) array(*)
             integer(SIZEOF_SIZE_T) len, isize
             integer(2), external :: compar
           END SUBROUTINE

           SUBROUTINE qsort_c16( array, len, isize, compar )
!DEC$ IF  DEFINED(_WIN32) 
           !DEC$ ATTRIBUTES DEFAULT,DECORATE,ALIAS:'QSORT' ::  QSORT_C16
!DEC$ ELSEIF DEFINED(__APPLE__)
           !DEC$ ATTRIBUTES DEFAULT,ALIAS:'_qsort_' :: QSORT_C16
!DEC$ ELSE
           !DEC$ ATTRIBUTES DEFAULT,ALIAS:'qsort_' :: QSORT_C16
!DEC$ ENDIF
           use IFPORT_TYPES
             complex(8) array(*)
             integer(SIZEOF_SIZE_T) len, isize
             integer(2), external :: compar
           END SUBROUTINE

           SUBROUTINE qsort_c32( array, len, isize, compar )
!DEC$ IF  DEFINED(_WIN32) 
           !DEC$ ATTRIBUTES DEFAULT,DECORATE,ALIAS:'QSORT' ::  QSORT_C32
!DEC$ ELSEIF DEFINED(__APPLE__)
           !DEC$ ATTRIBUTES DEFAULT,ALIAS:'_qsort_' :: QSORT_C32
!DEC$ ELSE
           !DEC$ ATTRIBUTES DEFAULT,ALIAS:'qsort_' :: QSORT_C32
!DEC$ ENDIF
           use IFPORT_TYPES
             complex(16) array(*)
             integer(SIZEOF_SIZE_T) len, isize
             integer(2), external :: compar
           END SUBROUTINE

           SUBROUTINE qsort_char( array, len, isize, compar )
!DEC$ IF  DEFINED(_WIN32) 
           !DEC$ ATTRIBUTES DEFAULT,DECORATE,ALIAS:'QSORT' ::  QSORT_CHAR
!DEC$ ELSEIF DEFINED(__APPLE__)
           !DEC$ ATTRIBUTES DEFAULT,ALIAS:'_qsort_' :: QSORT_CHAR
!DEC$ ELSE
           !DEC$ ATTRIBUTES DEFAULT,ALIAS:'qsort_' :: QSORT_CHAR
!DEC$ ENDIF
           use IFPORT_TYPES
             CHARACTER(*) array(*)
             integer(SIZEOF_SIZE_T) len, isize
             integer(2), external :: compar
           END SUBROUTINE


!DEC$ IF  DEFINED(_M_IA64) .OR. DEFINED(_M_AMD64) .OR. DEFINED(__x86_64__)
!
! Start of parallel "QSORT" routines to accept INTEGER*4 parameters on
! 64-bit platforms where "SIZEOF_SIZE_T" will be 64 bits and thus will
! not match the typical Fortran programmer's expectations that the 
! built-in functions "SIZE", "SIZEOF" and "LEN" can be used as the
! actual parameters to a call of "QSORT".
!
! For example, this is the natural way to code a call to "QSORT" of an
! array of character strings:
!
!    CALL QSORT(ARR, SIZE(ARR), LEN(ARR(1)), MY_COMPARE_ROUTINE)
!
! This is the natural way to code a call for an array of non-string items:
!
!    CALL QSORT(ARR, SIZE(ARR), SIZEOF(ARR(1)), MY_COMPARE_ROUTINE)
!
! "SIZE" amd "LEN" return a 32-bit result on both 32-bit and 64-bit
! platforms.  "SIZEOF" returns a 32-bit result on 32-bit platforms
! and a 64-bit result on 64-bit platforms.
!
! These calls will compile and run on 32-bit machines with the interfaces
! defined above this comment. But those interfaces will not match a call
! on a 64-bit machine.   If the interfaces below were not defined, the user
! would get an error message complaining that no specific routine could be
! found for a generic call.
!

           ! This first interface ("_ii") supports calls like
           ! "CALL QSORT(A, SIZE(A), LEN(A(1)),.." where both
           ! length and size parameters are INTEGER(4).  The
           ! only expected case here is CHARACTER(N) from "LEN"
           ! or "LEN_TRIM".
           !

           SUBROUTINE qsort_char_ii( array, len, isize, compar )
!DEC$ IF  DEFINED(_WIN32) 
           !DEC$ ATTRIBUTES DEFAULT,DECORATE,ALIAS:'QSORT_II' ::  QSORT_CHAR_II
!DEC$ ELSEIF DEFINED(__APPLE__)
           !DEC$ ATTRIBUTES DEFAULT,ALIAS:'_qsort_ii_' :: QSORT_CHAR_II
!DEC$ ELSE
           !DEC$ ATTRIBUTES DEFAULT,ALIAS:'qsort_ii_' :: QSORT_CHAR_II
!DEC$ ENDIF
           use IFPORT_TYPES
             CHARACTER(*) array(*)
             integer(4) len, isize
             integer(2), external :: compar
           END SUBROUTINE


           ! This second set of interfaces supports calls like
           ! "CALL QSORT(A, SIZE(A), SIZEOF(A(1))...".
           ! where the second parameter is INTEGER(4) and the third
           ! parameter is INTEGER(8).  There are many sub-cases.
           !

           SUBROUTINE qsort_i1_il( array, len, isize, compar )
!DEC$ IF  DEFINED(_WIN32) 
           !DEC$ ATTRIBUTES DEFAULT,DECORATE,ALIAS:'QSORT_IL' ::  QSORT_I1_IL
!DEC$ ELSEIF DEFINED(__APPLE__)
           !DEC$ ATTRIBUTES DEFAULT,ALIAS:'_qsort_il_' :: QSORT_I1_IL
!DEC$ ELSE
           !DEC$ ATTRIBUTES DEFAULT,ALIAS:'qsort_il_' :: QSORT_I1_IL
!DEC$ ENDIF
           use IFPORT_TYPES
             integer(1) array(*)
             integer(4) len
             integer(8) isize 
             integer(2), external :: compar
           END SUBROUTINE

           SUBROUTINE qsort_i2_il( array, len, isize, compar )
!DEC$ IF  DEFINED(_WIN32) 
           !DEC$ ATTRIBUTES DEFAULT,DECORATE,ALIAS:'QSORT_IL' ::  QSORT_I2_IL
!DEC$ ELSEIF DEFINED(__APPLE__)
           !DEC$ ATTRIBUTES DEFAULT,ALIAS:'_qsort_il_' :: QSORT_I2_IL
!DEC$ ELSE
           !DEC$ ATTRIBUTES DEFAULT,ALIAS:'qsort_il_' :: QSORT_I2_IL
!DEC$ ENDIF
           use IFPORT_TYPES
             integer(2) array(*)
             integer(4) len
             integer(8) isize 
             integer(2), external :: compar
           END SUBROUTINE

           SUBROUTINE qsort_i4_il( array, len, isize, compar )
!DEC$ IF  DEFINED(_WIN32) 
           !DEC$ ATTRIBUTES DEFAULT,DECORATE,ALIAS:'QSORT_IL' ::  QSORT_I4_IL
!DEC$ ELSEIF DEFINED(__APPLE__)
           !DEC$ ATTRIBUTES DEFAULT,ALIAS:'_qsort_il_' :: QSORT_I4_IL
!DEC$ ELSE
           !DEC$ ATTRIBUTES DEFAULT,ALIAS:'qsort_il_' :: QSORT_I4_IL
!DEC$ ENDIF
           use IFPORT_TYPES
             integer(4) array(*)
             integer(4) len
             integer(8) isize 
             integer(2), external :: compar
           END SUBROUTINE

           SUBROUTINE qsort_i8_il( array, len, isize, compar )
!DEC$ IF  DEFINED(_WIN32) 
           !DEC$ ATTRIBUTES DEFAULT,DECORATE,ALIAS:'QSORT_IL' ::  QSORT_I8_IL
!DEC$ ELSEIF DEFINED(__APPLE__)
           !DEC$ ATTRIBUTES DEFAULT,ALIAS:'_qsort_il_' :: QSORT_I8_IL
!DEC$ ELSE
           !DEC$ ATTRIBUTES DEFAULT,ALIAS:'qsort_il_' :: QSORT_I8_IL
!DEC$ ENDIF
           use IFPORT_TYPES
             integer(8) array(*)
             integer(4) len
             integer(8) isize 
             integer(2), external :: compar
           END SUBROUTINE

           SUBROUTINE qsort_l1_il( array, len, isize, compar )
!DEC$ IF  DEFINED(_WIN32) 
           !DEC$ ATTRIBUTES DEFAULT,DECORATE,ALIAS:'QSORT_IL' ::  QSORT_L1_IL
!DEC$ ELSEIF DEFINED(__APPLE__)
           !DEC$ ATTRIBUTES DEFAULT,ALIAS:'_qsort_il_' :: QSORT_L1_IL
!DEC$ ELSE
           !DEC$ ATTRIBUTES DEFAULT,ALIAS:'qsort_il_' :: QSORT_L1_IL
!DEC$ ENDIF
           use IFPORT_TYPES
             logical(1) array(*)
             integer(4) len
             integer(8) isize 
             integer(2), external :: compar
           END SUBROUTINE

           SUBROUTINE qsort_l2_il( array, len, isize, compar )
!DEC$ IF  DEFINED(_WIN32) 
           !DEC$ ATTRIBUTES DEFAULT,DECORATE,ALIAS:'QSORT_IL' ::  QSORT_L2_IL
!DEC$ ELSEIF DEFINED(__APPLE__)
           !DEC$ ATTRIBUTES DEFAULT,ALIAS:'_qsort_il_' :: QSORT_L2_IL
!DEC$ ELSE
           !DEC$ ATTRIBUTES DEFAULT,ALIAS:'qsort_il_' :: QSORT_L2_IL
!DEC$ ENDIF
           use IFPORT_TYPES
             logical(2) array(*)
             integer(4) len
             integer(8) isize 
             integer(2), external :: compar
           END SUBROUTINE

           SUBROUTINE qsort_l4_il( array, len, isize, compar )
!DEC$ IF  DEFINED(_WIN32) 
           !DEC$ ATTRIBUTES DEFAULT,DECORATE,ALIAS:'QSORT_IL' ::  QSORT_L4_IL
!DEC$ ELSEIF DEFINED(__APPLE__)
           !DEC$ ATTRIBUTES DEFAULT,ALIAS:'_qsort_il_' :: QSORT_L4_IL
!DEC$ ELSE
           !DEC$ ATTRIBUTES DEFAULT,ALIAS:'qsort_il_' :: QSORT_L4_IL
!DEC$ ENDIF
           use IFPORT_TYPES
             logical(4) array(*)
             integer(4) len
             integer(8) isize 
             integer(2), external :: compar
           END SUBROUTINE

           SUBROUTINE qsort_l8_il( array, len, isize, compar )
!DEC$ IF  DEFINED(_WIN32) 
           !DEC$ ATTRIBUTES DEFAULT,DECORATE,ALIAS:'QSORT_IL' :: QSORT_L8_IL
!DEC$ ELSEIF DEFINED(__APPLE__)
           !DEC$ ATTRIBUTES DEFAULT,ALIAS:'_qsort_il_' :: QSORT_L8_IL
!DEC$ ELSE
           !DEC$ ATTRIBUTES DEFAULT,ALIAS:'qsort_il_' :: QSORT_L8_IL
!DEC$ ENDIF
           use IFPORT_TYPES
             logical(8) array(*)
             integer(4) len
             integer(8) isize 
             integer(2), external :: compar
           END SUBROUTINE

           SUBROUTINE qsort_r4_il( array, len, isize, compar )
!DEC$ IF  DEFINED(_WIN32) 
           !DEC$ ATTRIBUTES DEFAULT,DECORATE,ALIAS:'QSORT_IL' ::  QSORT_R4_IL
!DEC$ ELSEIF DEFINED(__APPLE__)
           !DEC$ ATTRIBUTES DEFAULT,ALIAS:'_qsort_il_' :: QSORT_R4_IL
!DEC$ ELSE
           !DEC$ ATTRIBUTES DEFAULT,ALIAS:'qsort_il_' :: QSORT_R4_IL
!DEC$ ENDIF
           use IFPORT_TYPES
             real(4) array(*)
             integer(4) len
             integer(8) isize 
             integer(2), external :: compar
           END SUBROUTINE

           SUBROUTINE qsort_r8_il( array, len, isize, compar )
!DEC$ IF  DEFINED(_WIN32) 
           !DEC$ ATTRIBUTES DEFAULT,DECORATE,ALIAS:'QSORT_IL' ::  QSORT_R8_IL
!DEC$ ELSEIF DEFINED(__APPLE__)
           !DEC$ ATTRIBUTES DEFAULT,ALIAS:'_qsort_il_' :: QSORT_R8_IL
!DEC$ ELSE
           !DEC$ ATTRIBUTES DEFAULT,ALIAS:'qsort_il_' :: QSORT_R8_IL
!DEC$ ENDIF
           use IFPORT_TYPES
             real(8) array(*)
             integer(4) len
             integer(8) isize 
             integer(2), external :: compar
           END SUBROUTINE

           SUBROUTINE qsort_r16_il( array, len, isize, compar )
!DEC$ IF  DEFINED(_WIN32) 
           !DEC$ ATTRIBUTES DEFAULT,DECORATE,ALIAS:'QSORT_IL' ::  QSORT_R16_IL
!DEC$ ELSEIF DEFINED(__APPLE__)
           !DEC$ ATTRIBUTES DEFAULT,ALIAS:'_qsort_il_' :: QSORT_R16_IL
!DEC$ ELSE
           !DEC$ ATTRIBUTES DEFAULT,ALIAS:'qsort_il_' :: QSORT_R16_IL
!DEC$ ENDIF
           use IFPORT_TYPES
             real(16) array(*)
             integer(4) len
             integer(8) isize
             integer(2), external :: compar
           END SUBROUTINE

           SUBROUTINE qsort_c8_il( array, len, isize, compar )
!DEC$ IF  DEFINED(_WIN32) 
           !DEC$ ATTRIBUTES DEFAULT,DECORATE,ALIAS:'QSORT_IL' ::  QSORT_C8_IL
!DEC$ ELSEIF DEFINED(__APPLE__)
           !DEC$ ATTRIBUTES DEFAULT,ALIAS:'_qsort_il_' :: QSORT_C8_IL
!DEC$ ELSE
           !DEC$ ATTRIBUTES DEFAULT,ALIAS:'qsort_il_' :: QSORT_C8_IL
!DEC$ ENDIF
           use IFPORT_TYPES
             complex(4) array(*)
             integer(4) len
             integer(8) isize
             integer(2), external :: compar
           END SUBROUTINE

           SUBROUTINE qsort_c16_il( array, len, isize, compar )
!DEC$ IF  DEFINED(_WIN32) 
           !DEC$ ATTRIBUTES DEFAULT,DECORATE,ALIAS:'QSORT_IL' ::  QSORT_C16_IL
!DEC$ ELSEIF DEFINED(__APPLE__)
           !DEC$ ATTRIBUTES DEFAULT,ALIAS:'_qsort_il_' :: QSORT_C16_IL
!DEC$ ELSE
           !DEC$ ATTRIBUTES DEFAULT,ALIAS:'qsort_il_' :: QSORT_C16_IL
!DEC$ ENDIF
           use IFPORT_TYPES
             complex(8) array(*)
             integer(4) len
             integer(8) isize
             integer(2), external :: compar
           END SUBROUTINE

           SUBROUTINE qsort_c32_il( array, len, isize, compar )
!DEC$ IF  DEFINED(_WIN32) 
           !DEC$ ATTRIBUTES DEFAULT,DECORATE,ALIAS:'QSORT_IL' ::  QSORT_C32_IL
!DEC$ ELSEIF DEFINED(__APPLE__)
           !DEC$ ATTRIBUTES DEFAULT,ALIAS:'_qsort_il_' :: QSORT_C32_IL
!DEC$ ELSE
           !DEC$ ATTRIBUTES DEFAULT,ALIAS:'qsort_il_' :: QSORT_C32_IL
!DEC$ ENDIF
           use IFPORT_TYPES
             complex(16) array(*)
             integer(4) len
             integer(8) isize
             integer(2), external :: compar
           END SUBROUTINE

           SUBROUTINE qsort_char_il( array, len, isize, compar )
!DEC$ IF  DEFINED(_WIN32) 
           !DEC$ ATTRIBUTES DEFAULT,DECORATE,ALIAS:'QSORT_IL' ::  QSORT_CHAR_IL
!DEC$ ELSEIF DEFINED(__APPLE__)
           !DEC$ ATTRIBUTES DEFAULT,ALIAS:'_qsort_il_' :: QSORT_CHAR_IL
!DEC$ ELSE
           !DEC$ ATTRIBUTES DEFAULT,ALIAS:'qsort_il_' :: QSORT_CHAR_IL
!DEC$ ENDIF
           use IFPORT_TYPES
             CHARACTER(*) array(*)
             integer(4) len
             integer(8) isize
             integer(2), external :: compar
           END SUBROUTINE

!DEC$ ENDIF
!
!  End of routines for INTEGER(4) size parameters on 64-bit systems.


      END INTERFACE
!
!  End of 'qsort' interface


      INTERFACE

        SUBROUTINE SORTQQ(ADRARRAY, LENGTH, SIZE)
           !DEC$ ATTRIBUTES DEFAULT :: SORTQQ
           use IFPORT_TYPES
             INTEGER(POINTER_LEN)   ADRARRAY
             INTEGER(SIZEOF_SIZE_T) LENGTH
             INTEGER(4)  SIZE
        END SUBROUTINE

        INTEGER(4) FUNCTION BSEARCHQQ(ADRKEY, ADRARRAY,LENGTH, SIZE)
           !DEC$ ATTRIBUTES DEFAULT :: BSEARCHQQ
          use IFPORT_TYPES
           INTEGER(POINTER_LEN)   ADRKEY, ADRARRAY
           INTEGER(SIZEOF_SIZE_T) LENGTH
           INTEGER(4) SIZE
        END FUNCTION
      END INTERFACE

! -----------------------------------------------------------------
! Random Number Routines
! -----------------------------------------------------------------
! PSEUDO RANDOM NUMBER GENERATOR
      INTERFACE RANDOM
        ! this subroutine from libifcore 
        subroutine $$msflib$random( fval )
!DEC$ IF  DEFINED(_WIN32) 
        !DEC$ ATTRIBUTES DEFAULT,DECORATE,ALIAS:'RANDOM' :: $$msflib$random
!DEC$ ELSEIF DEFINED(__APPLE__)
        !DEC$ ATTRIBUTES DEFAULT,ALIAS:'_random_' :: $$msflib$random
!DEC$ ELSE
        !DEC$ ATTRIBUTES DEFAULT,ALIAS:'random_' :: $$msflib$random
!DEC$ ENDIF
          real(4) fval
        end subroutine

        ! this function from libifport
        real(4) function $$msportlib$random( iflag )
        !DEC$ ATTRIBUTES DEFAULT :: $$msportlib$random
          integer(4) iflag
        end function
      END INTERFACE

        INTERFACE

   ! Function SEED is in core library now. 
          SUBROUTINE SEED(ISEED)
          !DEC$ ATTRIBUTES DEFAULT :: SEED
            INTEGER(4) ISEED   
          END SUBROUTINE  

        INTEGER(4) FUNCTION IRAND(NEW_SEED)
           !DEC$ ATTRIBUTES DEFAULT :: IRAND
           INTEGER(4), OPTIONAL, INTENT(IN) :: NEW_SEED
        END FUNCTION

        INTEGER(4) FUNCTION IRANDM(NEW_SEED)
           !DEC$ ATTRIBUTES DEFAULT :: IRANDM
           INTEGER(4), INTENT(IN) :: NEW_SEED
        END FUNCTION

        REAL(8) FUNCTION DRAND(NEW_SEED)
          !DEC$ ATTRIBUTES DEFAULT :: DRAND
          INTEGER(4), INTENT(IN) :: NEW_SEED
        END FUNCTION
  
        REAL(8) FUNCTION DRANDM(NEW_SEED)
          !DEC$ ATTRIBUTES DEFAULT :: DRANDM
          INTEGER(4), INTENT(IN) :: NEW_SEED
        END FUNCTION


        REAL(4) FUNCTION RAN(ISEED)
          !DEC$ ATTRIBUTES DEFAULT :: RAN
          INTEGER(4), INTENT(IN) :: ISEED
        END FUNCTION

        REAL(4) FUNCTION RAND(ISEED)
          !DEC$ ATTRIBUTES DEFAULT :: RAND
          INTEGER(4), OPTIONAL, INTENT(IN) :: ISEED! NEW SEED
        END FUNCTION

! Get random number using C run-time generator
        REAL(4) FUNCTION RANF()
          !DEC$ ATTRIBUTES DEFAULT :: RANF
        END FUNCTION

! SET NEW SEED
        SUBROUTINE SRAND(NEW_SEED)
          !DEC$ ATTRIBUTES DEFAULT :: SRAND
          INTEGER(4), INTENT(IN) :: NEW_SEED
        END SUBROUTINE
 
! SET NEW SEED
        SUBROUTINE RANSET(RSEED)
          !DEC$ ATTRIBUTES DEFAULT :: RANSET
          REAL(4), INTENT(IN) :: RSEED
        END SUBROUTINE
 
        SUBROUTINE DRANSET(RSEED)
          !DEC$ ATTRIBUTES DEFAULT :: DRANSET
          REAL(8), INTENT(IN) :: RSEED
        END SUBROUTINE

        SUBROUTINE IRANSET(ISEED)
          !DEC$ ATTRIBUTES DEFAULT :: IRANSET
          INTEGER(4), INTENT(IN) :: ISEED
        END SUBROUTINE

 
        SUBROUTINE QRANSET(RSEED)
          !DEC$ ATTRIBUTES DEFAULT :: QRANSET
          REAL(16), INTENT(IN) :: RSEED
        END SUBROUTINE
 

! GET CURRENT SEED
        SUBROUTINE IRANGET(CURRENT_SEED)
          !DEC$ ATTRIBUTES DEFAULT :: IRANGET
          INTEGER(4), INTENT(OUT) :: CURRENT_SEED
        END SUBROUTINE
      END INTERFACE
     

! -----------------------------------------------------------------
! Input and Output Routines
! -----------------------------------------------------------------
      INTEGER(4), PARAMETER :: SEEK_SET = 0
      INTEGER(4), PARAMETER :: SEEK_CUR = 1
      INTEGER(4), PARAMETER :: SEEK_END = 2

      INTERFACE
! CHECK TO SEE WHAT ACCESS PERMISSIONS A FILE HAS,
! SUCH AS READ, WRITE, OR DELETE
        INTEGER(4) FUNCTION ACCESS(NAME, MODE)
          !DEC$ ATTRIBUTES DEFAULT :: ACCESS
          CHARACTER(LEN=*) NAME, MODE
        END FUNCTION ACCESS

! CHANGES THE ACCESS MODE OF A FILE.
        INTEGER(4) FUNCTION CHMOD(NAME, MODE)
          !DEC$ ATTRIBUTES DEFAULT :: CHMOD
          CHARACTER(LEN=*) NAME, MODE
        END FUNCTION CHMOD

        INTEGER(4) FUNCTION FGETC(LUNIT, CHAR)
          !DEC$ ATTRIBUTES DEFAULT :: FGETC
          INTEGER(4) :: LUNIT
          CHARACTER  :: CHAR
        END FUNCTION
    
        SUBROUTINE FLUSH(LUNIT)
          !DEC$ ATTRIBUTES DEFAULT :: FLUSH
          INTEGER(4) :: LUNIT
        END SUBROUTINE
    
        INTEGER(4) FUNCTION FPUTC(LUNIT, CHAR)
          !DEC$ ATTRIBUTES DEFAULT :: FPUTC
          INTEGER(4) :: LUNIT
          CHARACTER  :: CHAR
        END FUNCTION
        END INTERFACE
        
        INTERFACE FSEEK
         INTEGER(4) FUNCTION FSEEK(LUNIT, OFFSET, FROM)
          !DEC$ ATTRIBUTES DEFAULT :: FSEEK
           INTEGER(4), INTENT(IN) :: LUNIT, FROM
           INTEGER(4), INTENT(IN):: OFFSET
         END FUNCTION

         INTEGER(4) FUNCTION FSEEKI8(LUNIT, OFFSET, FROM)
          !DEC$ ATTRIBUTES DEFAULT :: FSEEKI8
           INTEGER(4), INTENT(IN) :: LUNIT, FROM
           INTEGER(8), INTENT(IN) :: OFFSET
         END FUNCTION

        END INTERFACE

        INTERFACE
           INTEGER(4) FUNCTION FTELL(LUNIT)
             !DEC$ ATTRIBUTES DEFAULT :: FTELL
                INTEGER(4), INTENT(IN) :: LUNIT
           END FUNCTION

           INTEGER(8) FUNCTION FTELLI8(LUNIT)
             !DEC$ ATTRIBUTES DEFAULT :: FTELLI8
                INTEGER(4), INTENT(IN) :: LUNIT
           END FUNCTION
        END INTERFACE

! GET CURRENT RECORD NUMBER POSITION FOR A FILE
        INTERFACE
          FUNCTION GETPOS(LUNIT)
          !DEC$ ATTRIBUTES DEFAULT :: GETPOS
             INTEGER(4) GETPOS
             INTEGER(4), INTENT(IN) :: LUNIT
          END FUNCTION

          FUNCTION GETPOSI8(LUNIT)
          !DEC$ ATTRIBUTES DEFAULT :: GETPOSI8
             INTEGER(8) GETPOSI8
             INTEGER(4), INTENT(IN) :: LUNIT
          END FUNCTION
        END INTERFACE

       INTERFACE
        INTEGER(4) FUNCTION GETC(CH)
          !DEC$ ATTRIBUTES DEFAULT :: GETC
          CHARACTER CH
        END FUNCTION
    
        INTEGER(4) FUNCTION PUTC(CH)
          !DEC$ ATTRIBUTES DEFAULT :: PUTC
          CHARACTER CH
        END FUNCTION
    
      END INTERFACE


! -----------------------------------------------------------------
! Error Handling Routines
! -----------------------------------------------------------------
!      INTEGER(4), PARAMETER :: MAX_GERROR_LENGTH = 130! 1-001

      INTEGER(4), PARAMETER :: EPERM   = 1   ! Insufficient Permission for Operation
      INTEGER(4), PARAMETER :: ENOENT  = 2   ! No Such File or Directory
      INTEGER(4), PARAMETER :: ESRCH   = 3   ! No Such Process
      INTEGER(4), PARAMETER :: EIO     = 5   ! I/O error
      INTEGER(4), PARAMETER :: E2BIG   = 7   ! Argument List Too Long
      INTEGER(4), PARAMETER :: ENOEXEC = 8   ! File Is Not Executable
      INTEGER(4), PARAMETER :: ENOMEM  = 12  ! Not Enough Resources
      INTEGER(4), PARAMETER :: EACCES  = 13  ! Permission Denied
      INTEGER(4), PARAMETER :: EXDEV   = 18  ! Cross Device Link
      INTEGER(4), PARAMETER :: ENOTDIR = 20  ! Not a Directory
      INTEGER(4), PARAMETER :: EINVAL  = 22  ! Invalid Argument

! Removed to core library
!
!        SUBROUTINE GERROR(ERRMSG)
!          !DEC$ ATTRIBUTES DEFAULT :: GERROR
!          CHARACTER(LEN=*), INTENT(OUT) :: ERRMSG
!        END SUBROUTINE

      INTERFACE
        INTEGER(4) FUNCTION GETLASTERROR()
          !DEC$ ATTRIBUTES DEFAULT :: GETLASTERROR
        END FUNCTION

! CURRENT VALUE OF ERRNO
        INTEGER(4) FUNCTION IERRNO()
          !DEC$ ATTRIBUTES DEFAULT :: IERRNO
        END FUNCTION

! Removed to core library
! PRINT A MESSAGE ON STDERR
!        SUBROUTINE PERROR(STRING)
!          !DEC$ ATTRIBUTES DEFAULT :: PERROR
!          CHARACTER (LEN=*), INTENT(IN) :: STRING
!        END SUBROUTINE PERROR
      END INTERFACE

! -----------------------------------------------------------------
! Error Handling Routines (extension)
! -----------------------------------------------------------------
      INTEGER(4), PARAMETER :: ERR$ZERO         =  0
      INTEGER(4), PARAMETER :: ERR$PERM         =  1
      INTEGER(4), PARAMETER :: ERR$NOENT        =  2
      INTEGER(4), PARAMETER :: ERR$SRCH         =  3
      INTEGER(4), PARAMETER :: ERR$INTR         =  4
      INTEGER(4), PARAMETER :: ERR$IO           =  5
      INTEGER(4), PARAMETER :: ERR$NXIO         =  6
      INTEGER(4), PARAMETER :: ERR$2BIG         =  7
      INTEGER(4), PARAMETER :: ERR$NOEXEC       =  8
      INTEGER(4), PARAMETER :: ERR$BADF         =  9
      INTEGER(4), PARAMETER :: ERR$CHILD        = 10
      INTEGER(4), PARAMETER :: ERR$AGAIN        = 11
      INTEGER(4), PARAMETER :: ERR$NOMEM        = 12
      INTEGER(4), PARAMETER :: ERR$ACCES        = 13
      INTEGER(4), PARAMETER :: ERR$FAULT        = 14
      INTEGER(4), PARAMETER :: ERR$NOTBLK       = 15
      INTEGER(4), PARAMETER :: ERR$BUSY         = 16
      INTEGER(4), PARAMETER :: ERR$EXIST        = 17
      INTEGER(4), PARAMETER :: ERR$XDEV         = 18
      INTEGER(4), PARAMETER :: ERR$NODEV        = 19
      INTEGER(4), PARAMETER :: ERR$NOTDIR       = 20
      INTEGER(4), PARAMETER :: ERR$ISDIR        = 21
      INTEGER(4), PARAMETER :: ERR$INVAL        = 22
      INTEGER(4), PARAMETER :: ERR$NFILE        = 23
      INTEGER(4), PARAMETER :: ERR$MFILE        = 24
      INTEGER(4), PARAMETER :: ERR$NOTTY        = 25
      INTEGER(4), PARAMETER :: ERR$TXTBSY       = 26
      INTEGER(4), PARAMETER :: ERR$FBIG         = 27
      INTEGER(4), PARAMETER :: ERR$NOSPC        = 28
      INTEGER(4), PARAMETER :: ERR$SPIPE        = 29
      INTEGER(4), PARAMETER :: ERR$ROFS         = 30
      INTEGER(4), PARAMETER :: ERR$MLINK        = 31
      INTEGER(4), PARAMETER :: ERR$PIPE         = 32
      INTEGER(4), PARAMETER :: ERR$DOM          = 33
      INTEGER(4), PARAMETER :: ERR$RANGE        = 34
      INTEGER(4), PARAMETER :: ERR$UCLEAN       = 35
      INTEGER(4), PARAMETER :: ERR$DEADLOCK     = 36
      INTEGER(4), PARAMETER :: ERR$NAMETOOLONG  = 38
      INTEGER(4), PARAMETER :: ERR$NOLCK        = 39
      INTEGER(4), PARAMETER :: ERR$NOSYS        = 40
      INTEGER(4), PARAMETER :: ERR$NOTEMPTY     = 41
      INTEGER(4), PARAMETER :: ERR$ILSEQ        = 42

! FOR SETERRORMODEQQ
      LOGICAL(4), PARAMETER :: ERR$HARDPROMPT   = .TRUE.
      LOGICAL(4), PARAMETER :: ERR$HARDFAIL     = .FALSE.


      INTERFACE
        INTEGER(4) FUNCTION GETLASTERRORQQ()
          !DEC$ ATTRIBUTES DEFAULT :: GETLASTERRORQQ
        END FUNCTION

        SUBROUTINE SETERRORMODEQQ(PMODE)
          !DEC$ ATTRIBUTES DEFAULT :: SETERRORMODEQQ
          LOGICAL(4) PMODE
        END SUBROUTINE
      END INTERFACE

! -----------------------------------------------------------------
! Date and Time Routines
! -----------------------------------------------------------------
      INTERFACE
        CHARACTER(LEN=8) FUNCTION CLOCK()! TIME OF DAY, HH:MM:SS
          !DEC$ ATTRIBUTES DEFAULT :: CLOCK
        END FUNCTION

        SUBROUTINE CLOCKX(CLOCK)         ! PROCESSOR TIME TO NEAREST
          !DEC$ ATTRIBUTES DEFAULT :: CLOCKX
          REAL(8), INTENT(OUT) :: CLOCK  ! MICROSECOND
        END SUBROUTINE

! CONVERT A TIME TO CHARACTER REPRESENTATION
! ITIME IS THE NUMBER OF SECONDS SINCE MIDNIGHT
! OF JANUARY 1, 1970 GREENWICH MEAN TIME
        CHARACTER(LEN=24) FUNCTION CTIME(ITIME)
          !DEC$ ATTRIBUTES DEFAULT :: CTIME
          INTEGER(4), INTENT(IN) :: ITIME
        END FUNCTION CTIME

        SUBROUTINE ITIME(IARRAY)
          !DEC$ ATTRIBUTES DEFAULT :: ITIME
          INTEGER(4) :: IARRAY(3)
        END SUBROUTINE
      END INTERFACE
          
      INTERFACE DTIME
        REAL(4) FUNCTION DTIME(TARRAY)
          !DEC$ ATTRIBUTES DEFAULT :: DTIME
          REAL(4), DIMENSION(2) :: TARRAY! USER TIME, SYSTEM TIME
        END FUNCTION DTIME
        REAL(8) FUNCTION DTIMER8(TARRAY)
          !DEC$ ATTRIBUTES DEFAULT :: DTIMER8
          REAL(8), DIMENSION(2) :: TARRAY! USER TIME, SYSTEM TIME
        END FUNCTION DTIMER8
      END INTERFACE
   
      INTERFACE
        REAL(4) FUNCTION ETIME(TARRAY)
          !DEC$ ATTRIBUTES DEFAULT :: ETIME
          REAL(4), DIMENSION(2) :: TARRAY
        END FUNCTION ETIME
     
        REAL(8) FUNCTION DCLOCK()
          !DEC$ ATTRIBUTES DEFAULT :: DCLOCK
        END FUNCTION
   
        SUBROUTINE DATE4(D4STRING)
          !DEC$ ATTRIBUTES DEFAULT :: DATE4
          CHARACTER(LEN=11) D4STRING
        END SUBROUTINE

! GET DATE -- WARNING, NOT Y2K COMPLIANT
        FUNCTION JDATE()
          !DEC$ ATTRIBUTES DEFAULT :: JDATE
          CHARACTER(LEN=8) :: JDATE
        END FUNCTION

! GET DATE -- Y2K COMPLIANT VERSION
        FUNCTION JDATE4()
          !DEC$ ATTRIBUTES DEFAULT :: JDATE4
          CHARACTER(LEN=10) :: JDATE4
        END FUNCTION
      END INTERFACE

      INTERFACE DATE
        FUNCTION $$msportlib$date_f()
!DEC$ IF  DEFINED(_WIN32) 
          !DEC$ ATTRIBUTES DEFAULT,DECORATE,ALIAS:'DATE' :: $$msportlib$date_f
!DEC$ ELSEIF DEFINED(__APPLE__)
          !DEC$ ATTRIBUTES DEFAULT,ALIAS:'_date_' :: $$msportlib$date_f
!DEC$ ELSE
          !DEC$ ATTRIBUTES DEFAULT,ALIAS:'date_' :: $$msportlib$date_f
!DEC$ ENDIF
          CHARACTER(LEN=8) $$msportlib$date_f
        END FUNCTION

        SUBROUTINE $$msportlib$date_s(DSTRING)
!DEC$ IF  DEFINED(_WIN32) 
          !DEC$ ATTRIBUTES DEFAULT,DECORATE,ALIAS:'DATE' :: $$msportlib$date_s
!DEC$ ELSEIF DEFINED(__APPLE__)
          !DEC$ ATTRIBUTES DEFAULT,ALIAS:'_date_' :: $$msportlib$date_s
!DEC$ ELSE
          !DEC$ ATTRIBUTES DEFAULT,ALIAS:'date_' :: $$msportlib$date_s
!DEC$ ENDIF
          CHARACTER(LEN=9) DSTRING
        END SUBROUTINE
      END INTERFACE
   
      INTERFACE FDATE
        SUBROUTINE $$msportlib$fdate_s(RETURNSTR)
!DEC$ IF  DEFINED(_WIN32) 
          !DEC$ ATTRIBUTES DEFAULT,DECORATE,ALIAS:'FDATE' :: $$msportlib$fdate_s
!DEC$ ELSEIF DEFINED(__APPLE__)
          !DEC$ ATTRIBUTES DEFAULT,ALIAS:'_fdate_' :: $$msportlib$fdate_s
!DEC$ ELSE
          !DEC$ ATTRIBUTES DEFAULT,ALIAS:'fdate_' :: $$msportlib$fdate_s
!DEC$ ENDIF
          CHARACTER(LEN=24) RETURNSTR
        END SUBROUTINE

        CHARACTER(24) FUNCTION $$msportlib$fdate_f()
!DEC$ IF  DEFINED(_WIN32) 
          !DEC$ ATTRIBUTES DEFAULT,DECORATE,ALIAS:'FDATE' :: $$msportlib$fdate_f
!DEC$ ELSEIF DEFINED(__APPLE__)
          !DEC$ ATTRIBUTES DEFAULT,ALIAS:'_fdate_' :: $$msportlib$fdate_f
!DEC$ ELSE
          !DEC$ ATTRIBUTES DEFAULT,ALIAS:'fdate_' :: $$msportlib$fdate_f
!DEC$ ENDIF
        END FUNCTION
      END INTERFACE
 


! GET DATE - WARNING - NOT Y2K COMPLIANT
      INTERFACE IDATE
        SUBROUTINE IDATE(SDATE)
          !DEC$ ATTRIBUTES DEFAULT :: IDATE
          INTEGER(4), INTENT(OUT) :: SDATE(3)
        END SUBROUTINE

        SUBROUTINE IDATE1(MONTH,DAY,YEAR)
          !DEC$ ATTRIBUTES DEFAULT :: IDATE1
          INTEGER(4), INTENT(OUT) :: MONTH, DAY, YEAR
        END SUBROUTINE
      END INTERFACE


! GET DATE, Y2K COMPLIANT VERSION
      INTERFACE IDATE4
        SUBROUTINE IDATE4(MONTH,DAY,YEAR)
          !DEC$ ATTRIBUTES DEFAULT :: IDATE4
          INTEGER(4), INTENT(OUT) :: MONTH, DAY, YEAR
        END SUBROUTINE
        SUBROUTINE F_IDATE4(SDATE)
          !DEC$ ATTRIBUTES DEFAULT :: F_IDATE4
          INTEGER(4), INTENT(OUT) :: SDATE(3)
        END SUBROUTINE
      END INTERFACE
  

! GET THE CURRENT DATE
      INTERFACE GETDAT
        SUBROUTINE GETDAT(IYEAR,IMONTH,IDAY)
          !DEC$ ATTRIBUTES DEFAULT :: GETDAT
            INTEGER(4), INTENT(OUT) :: IYEAR,IMONTH, IDAY
        END SUBROUTINE
        SUBROUTINE GETDATI2(IYEAR,IMONTH,IDAY)
          !DEC$ ATTRIBUTES DEFAULT :: GETDATI2
            INTEGER(2), INTENT(OUT) :: IYEAR,IMONTH, IDAY
        END SUBROUTINE
      END INTERFACE


! SET CURRENT DATE
      INTERFACE SETDAT
        LOGICAL(4) FUNCTION SETDAT(YEAR,MONTH,DAY)
          !DEC$ ATTRIBUTES DEFAULT :: SETDAT
          INTEGER(4) :: YEAR,MONTH,DAY! YEAR IS 4 DIGITS
        END FUNCTION
        LOGICAL(4) FUNCTION SETDATI2(YEAR,MONTH,DAY)
          !DEC$ ATTRIBUTES DEFAULT :: SETDATI2
          INTEGER(2) :: YEAR,MONTH,DAY! YEAR IS 4 DIGITS
        END FUNCTION
      END INTERFACE


      INTERFACE GETTIM
        SUBROUTINE GETTIM(HOUR,MIN,SEC,HDTS)
          !DEC$ ATTRIBUTES DEFAULT :: GETTIM
          INTEGER(4), INTENT(OUT) :: HOUR,MIN,SEC,HDTS
        END SUBROUTINE
        SUBROUTINE GETTIMI2(HOUR,MIN,SEC,HDTS)
          !DEC$ ATTRIBUTES DEFAULT :: GETTIMI2
          INTEGER(2), INTENT(OUT) :: HOUR,MIN,SEC,HDTS
        END SUBROUTINE
      END INTERFACE

! SETS SYSTEM CLOCK ON CURRENT COMPUTER SYSTEM WHERE THE PROCESS
! IS EXECUTING
      INTERFACE SETTIM
        LOGICAL(4) FUNCTION SETTIM(HOUR,MINUTE,SECOND,HUNDRETH)
          !DEC$ ATTRIBUTES DEFAULT :: SETTIM
          INTEGER(4), INTENT(IN) :: HOUR,MINUTE,SECOND,HUNDRETH
        END FUNCTION
        LOGICAL(4) FUNCTION SETTIMI2(HOUR,MINUTE,SECOND,HUNDRETH)
          !DEC$ ATTRIBUTES DEFAULT :: SETTIMI2
          INTEGER(2), INTENT(IN) :: HOUR,MINUTE,SECOND,HUNDRETH
        END FUNCTION
      END INTERFACE


      INTERFACE
! GET SYSTEM TIME IN HOURS, MINUTES, SECONDS, MILLISECONDS
        SUBROUTINE GETTIMEOFDAY(RET, ERR)
          !DEC$ ATTRIBUTES DEFAULT :: GETTIMEOFDAY
          INTEGER(4) RET(2)
          INTEGER(4) ERR
        END SUBROUTINE
  
! CURRENT TIME AND SYSTEM TIME
        SUBROUTINE LTIME(TIME, DATEARRAY)
          !DEC$ ATTRIBUTES DEFAULT :: LTIME
          INTEGER(4) TIME
          INTEGER(4) DATEARRAY(9)
        END SUBROUTINE LTIME
  
        SUBROUTINE GMTIME(STIME,DATEARRAY)
          !DEC$ ATTRIBUTES DEFAULT :: GMTIME
          INTEGER(4) STIME
          INTEGER(4) DATEARRAY(9)
        END SUBROUTINE

        REAL(8) FUNCTION RTC()
          !DEC$ ATTRIBUTES DEFAULT :: RTC
        END FUNCTION
  
! RETURNS NUMBER OF SECONDS SINCE MIDNIGHT IF TIME IS 0.0
! OR NUMBER OF SECONDS SINCE THE LAST CALL TO SECNDS
! FOR MORE ACCURATE TIMES, USE DCLOCK OR CPU_TIME
        REAL(4) FUNCTION SECNDS(TIME)
          !DEC$ ATTRIBUTES DEFAULT :: SECNDS
          REAL(4) TIME
        END FUNCTION
 
! SYSTEM TIME IN SECONDS
        REAL(8) FUNCTION TIMEF()
          !DEC$ ATTRIBUTES DEFAULT :: TIMEF
        END FUNCTION

! This interface is for compatibility with 7.0 Intel Fortran only
! The better way is to use call TIME function
        FUNCTION F_TIME()
!DEC$ IF  DEFINED(_WIN32) 
           !DEC$ ATTRIBUTES DEFAULT,DECORATE,ALIAS:'TIME' :: F_TIME
!DEC$ ELSEIF DEFINED(__APPLE__)
           !DEC$ ATTRIBUTES DEFAULT,ALIAS:'_time_' :: F_TIME
!DEC$ ELSE
           !DEC$ ATTRIBUTES DEFAULT,ALIAS:'time_' :: F_TIME
!DEC$ ENDIF
         INTEGER(4) F_TIME
        END FUNCTION


      END INTERFACE

      INTERFACE TIME
! SYSTEM TIME IN SECONDS
        FUNCTION $$msportlib$time_f()
!DEC$ IF  DEFINED(_WIN32) 
           !DEC$ ATTRIBUTES DEFAULT,DECORATE,ALIAS:'TIME' :: $$msportlib$time_f
!DEC$ ELSEIF DEFINED(__APPLE__)
           !DEC$ ATTRIBUTES DEFAULT,ALIAS:'_time_' :: $$msportlib$time_f
!DEC$ ELSE
           !DEC$ ATTRIBUTES DEFAULT,ALIAS:'time_' :: $$msportlib$time_f
!DEC$ ENDIF
         INTEGER(4) $$msportlib$time_f
        END FUNCTION

! TIME AS ASCII 
        SUBROUTINE $$msportlib$time_s(TIMESTR)
!DEC$ IF  DEFINED(_WIN32) 
           !DEC$ ATTRIBUTES DEFAULT,DECORATE,ALIAS:'TIME2' :: $$msportlib$time_s
!DEC$ ELSEIF DEFINED(__APPLE__)
           !DEC$ ATTRIBUTES DEFAULT,ALIAS:'_time2_' :: $$msportlib$time_s
!DEC$ ELSE
           !DEC$ ATTRIBUTES DEFAULT,ALIAS:'time2_' :: $$msportlib$time_s
!DEC$ ENDIF
          CHARACTER(LEN=8), INTENT(OUT) :: TIMESTR
        END SUBROUTINE

      END INTERFACE

  
! -----------------------------------------------------------------
! Information Retrieval
! -----------------------------------------------------------------
! FILE STATUS BY UNIT
      INTERFACE FSTAT
        INTEGER(4) FUNCTION FSTAT(LUNIT, STARRAY)
          !DEC$ ATTRIBUTES DEFAULT :: FSTAT
          INTEGER(4), INTENT(IN) :: LUNIT
          INTEGER(4), DIMENSION(12) :: STARRAY
        END FUNCTION
        INTEGER(4) FUNCTION FSTATI8(LUNIT, STARRAY)
          !DEC$ ATTRIBUTES DEFAULT :: FSTATI8
          INTEGER(4), INTENT(IN) :: LUNIT
          INTEGER(8), DIMENSION(12) :: STARRAY
        END FUNCTION
      END INTERFACE
  
! FILE STATUS BY NAME, FOR LINK NODE
      INTERFACE LSTAT
        INTEGER(4) FUNCTION LSTAT(NAME, STARRAY)
          !DEC$ ATTRIBUTES DEFAULT :: LSTAT
          CHARACTER(LEN=*) :: NAME
          INTEGER(4), DIMENSION(12) :: STARRAY
        END FUNCTION
        INTEGER(4) FUNCTION LSTATI8(NAME, STARRAY)
          !DEC$ ATTRIBUTES DEFAULT :: LSTATI8
          CHARACTER(LEN=*) :: NAME
          INTEGER(8), DIMENSION(12) :: STARRAY
        END FUNCTION
      END INTERFACE

! FILE STATUS BY NAME
      INTERFACE STAT
        INTEGER(4) FUNCTION STAT(NAME, STARRAY)
          !DEC$ ATTRIBUTES DEFAULT :: STAT
          CHARACTER(LEN=*) :: NAME
          INTEGER(4), DIMENSION(12) :: STARRAY
        END FUNCTION
        INTEGER(4) FUNCTION STATI8(NAME, STARRAY)
          !DEC$ ATTRIBUTES DEFAULT :: STATI8
          CHARACTER(LEN=*) :: NAME
          INTEGER(8), DIMENSION(12) :: STARRAY
        END FUNCTION
      END INTERFACE

      INTERFACE
        INTEGER(4) FUNCTION GETCWD(DIRECTORY)
          !DEC$ ATTRIBUTES DEFAULT :: GETCWD
          CHARACTER(LEN=*) DIRECTORY
        END FUNCTION GETCWD

!  RETURN THE VALUE OF A GIVEN ENVIRONMENT VARIABLE
        SUBROUTINE GETENV(VARIABLE, VALUE)
          !DEC$ ATTRIBUTES DEFAULT :: GETENV
          CHARACTER(LEN=*) VARIABLE, VALUE
        END SUBROUTINE

! GET THE GROUP ID OF THE CURRENT USER
        INTEGER(4) FUNCTION GETGID()
          !DEC$ ATTRIBUTES DEFAULT :: GETGID
        END FUNCTION
 
! GET THE CURRENT PROCESS ID
        INTEGER(4) FUNCTION GETPID()
          !DEC$ ATTRIBUTES DEFAULT :: GETPID
        END FUNCTION
 
! GET THE LOGIN NAME OF THE CURRENT USER
        SUBROUTINE GETLOG(LOGIN_NAME)
          !DEC$ ATTRIBUTES DEFAULT :: GETLOG
          CHARACTER(LEN=*), INTENT(OUT) :: LOGIN_NAME
        END SUBROUTINE

        INTEGER(4) FUNCTION GETUID()
          !DEC$ ATTRIBUTES DEFAULT :: GETUID
        END FUNCTION

! GET NAME OF HOST COMPUTER
        INTEGER(4) FUNCTION HOSTNAM(NAME)
          !DEC$ ATTRIBUTES DEFAULT :: HOSTNAM
          CHARACTER(LEN=*), INTENT(OUT) :: NAME
        END FUNCTION
    
        INTEGER(4) FUNCTION HOSTNM(NAME)
          !DEC$ ATTRIBUTES DEFAULT :: HOSTNM
          CHARACTER(LEN=*), INTENT(OUT) :: NAME
        END FUNCTION

! RENAME A FILE
        INTEGER(4) FUNCTION RENAME(FROM, TO)
          !DEC$ ATTRIBUTES DEFAULT :: RENAME
          CHARACTER(LEN=*), INTENT(IN) :: FROM, TO
        END FUNCTION

! SCAN THE ENVIRONMENT FOR AN ENVIRONMENT VARIABLE THAT MATCHES
! ENVNAME, AND RETURN THE VALUE OR STRING IT IS SET TO
        SUBROUTINE SCANENV(ENVNAME, ENVTEXT, ENVVALUE)
          !DEC$ ATTRIBUTES DEFAULT :: SCANENV
          CHARACTER(LEN=*), INTENT(IN) :: ENVNAME
          CHARACTER(LEN=*), INTENT(OUT) :: ENVTEXT, ENVVALUE
        END SUBROUTINE

        INTEGER(4) FUNCTION UNLINK(NAME)
          !DEC$ ATTRIBUTES DEFAULT :: UNLINK
          CHARACTER(LEN=*), INTENT(IN) :: NAME
        END FUNCTION

      END INTERFACE
 
! -----------------------------------------------------------------
! Keyboard and Speaker Routines
! -----------------------------------------------------------------
      INTERFACE
        SUBROUTINE BEEPQQ( FREQUENCY, DURATION)
          !DEC$ ATTRIBUTES DEFAULT :: BEEPQQ
          INTEGER(4) FREQUENCY, DURATION
        END SUBROUTINE
      END INTERFACE
 

! -----------------------------------------------------------------
! Terminal, Tape Handling Routines
! -----------------------------------------------------------------
      INTERFACE
! IS A DEVICE A TERMINAL (TTY) TYPE DEVICE?
        LOGICAL(4) FUNCTION ISATTY(LUNIT)
          !DEC$ ATTRIBUTES DEFAULT :: ISATTY
          INTEGER(4), INTENT(IN) :: LUNIT
        END FUNCTION

! NAME OF TTY DEVICE
       SUBROUTINE TTYNAM(NAME, LUNIT)
          !DEC$ ATTRIBUTES DEFAULT :: TTYNAM
          CHARACTER(LEN=*), INTENT(IN) :: NAME
          INTEGER(4), INTENT(IN) :: LUNIT
       END SUBROUTINE

      END INTERFACE
  
! -----------------------------------------------------------------
! Program call and control routines
! -----------------------------------------------------------------
      INTERFACE
        INTEGER(4) FUNCTION RAISEQQ(SIGNUMBER)
          !DEC$ ATTRIBUTES DEFAULT :: RAISEQQ
          INTEGER(4) SIGNUMBER
        END FUNCTION

! RUN A GIVEN PROGRAM
        INTEGER(2) FUNCTION RUNQQ(PROGNAME, COMMANDLINE)
          !DEC$ ATTRIBUTES DEFAULT :: RUNQQ
          CHARACTER(LEN=*) PROGNAME, COMMANDLINE
        END FUNCTION
      
        FUNCTION SIGNALQQ(SIGNUM, HANDLER)
          !DEC$ ATTRIBUTES DEFAULT :: SIGNALQQ
        use IFPORT_TYPES
          INTEGER(POINTER_LEN)     :: SIGNALQQ
          INTEGER(4)               :: SIGNUM
          INTEGER(4), EXTERNAL     :: HANDLER
        END FUNCTION

        SUBROUTINE SLEEPQQ(DURATION)
          !DEC$ ATTRIBUTES DEFAULT :: SLEEPQQ
          INTEGER(4) DURATION
        END SUBROUTINE
      END INTERFACE

! -----------------------------------------------------------------
! System, Drive, and Directory Routines
! -----------------------------------------------------------------
    
      INTERFACE
        LOGICAL(4) FUNCTION CHANGEDIRQQ(DIRNAME)
          !DEC$ ATTRIBUTES DEFAULT :: CHANGEDIRQQ
            CHARACTER(LEN=*) DIRNAME
        END FUNCTION

        LOGICAL(4) FUNCTION DELDIRQQ(DIRNAME)
          !DEC$ ATTRIBUTES DEFAULT :: DELDIRQQ
            CHARACTER(LEN=*) DIRNAME
        END FUNCTION

        LOGICAL(4) FUNCTION MAKEDIRQQ(DIRNAME)
          !DEC$ ATTRIBUTES DEFAULT :: MAKEDIRQQ
            CHARACTER(LEN=*) DIRNAME
        END FUNCTION

        LOGICAL(4) FUNCTION CHANGEDRIVEQQ(DRIVENAME)
          !DEC$ ATTRIBUTES DEFAULT :: CHANGEDRIVEQQ
          CHARACTER(LEN=*) DRIVENAME
        END FUNCTION

! GET THE CURRENT DIRECTORY FOR A GIVEN DRIVE
        INTEGER(4) FUNCTION GETDRIVEDIRQQ(DRIVEDIR)
          !DEC$ ATTRIBUTES DEFAULT :: GETDRIVEDIRQQ
          CHARACTER(LEN=*) DRIVEDIR
        END FUNCTION
      END INTERFACE

      INTERFACE  GETDRIVESIZEQQ
       LOGICAL(4) FUNCTION GETDRIVESIZEQQ(DRIVENM,TOTALNUM,AVAILNUM)
          !DEC$ ATTRIBUTES DEFAULT :: GETDRIVESIZEQQ
            CHARACTER(LEN=*) DRIVENM
            INTEGER(4) TOTALNUM, AVAILNUM
       END FUNCTION

       LOGICAL(4) FUNCTION GETDRIVESIZEQQI8(DRNM,TOTALNUM,AVAILNUM)
          !DEC$ ATTRIBUTES DEFAULT :: GETDRIVESIZEQQI8
            CHARACTER(LEN=*) DRNM
            INTEGER(8) TOTALNUM, AVAILNUM
        END FUNCTION
      END INTERFACE

      INTERFACE
        CHARACTER(26) FUNCTION GETDRIVESQQ()
          !DEC$ ATTRIBUTES DEFAULT :: GETDRIVESQQ
        END FUNCTION

! GETTING ENVIRONMENT VARIABLE VALUE
        INTEGER(4) FUNCTION GETENVQQ(VARNAME, VALUE)
          !DEC$ ATTRIBUTES DEFAULT :: GETENVQQ
          CHARACTER(LEN=*) VARNAME, VALUE
        END FUNCTION GETENVQQ

!  SET AN ENVIRONMENT VARIABLE FOR THE CURRENT PROCESS
        LOGICAL(4) FUNCTION SETENVQQ(INPUT_STRING)
          !DEC$ ATTRIBUTES DEFAULT :: SETENVQQ
          CHARACTER(LEN=*) INPUT_STRING
        END FUNCTION SETENVQQ

! SEND COMMAND TO DOS SHELL
        LOGICAL(4) FUNCTION SYSTEMQQ(COMMAND)
          !DEC$ ATTRIBUTES DEFAULT :: SYSTEMQQ
          CHARACTER(LEN=*) COMMAND
        END FUNCTION SYSTEMQQ
      END INTERFACE


! CHANGE WORKING DIRECTORY (AND POSSIBLY DEFAULT DRIVE)
      INTERFACE
        INTEGER(4) FUNCTION CHDIR(DIRECTORY_NAME)
          !DEC$ ATTRIBUTES DEFAULT :: CHDIR
            CHARACTER(LEN=*) DIRECTORY_NAME
        END FUNCTION CHDIR
      END INTERFACE
    
! -----------------------------------------------------------------
! Floating-Point Inquiry and Control Routines
! -----------------------------------------------------------------
      INTEGER(2), PARAMETER :: FPCW$MCW_EM        = Z'003F'  ! EXCEPTION MASK
      INTEGER(2), PARAMETER :: FPCW$INVALID       = Z'0001'  ! INVALID
      INTEGER(2), PARAMETER :: FPCW$DENORMAL      = Z'0002'  ! DENORMAL
      INTEGER(2), PARAMETER :: FPCW$ZERODIVIDE    = Z'0004'  ! ZERO DIVIDE
      INTEGER(2), PARAMETER :: FPCW$OVERFLOW      = Z'0008'  ! OVERFLOW
      INTEGER(2), PARAMETER :: FPCW$UNDERFLOW     = Z'0010'  ! UNDERFLOW
      INTEGER(2), PARAMETER :: FPCW$INEXACT       = Z'0020'  ! INEXACT (PRECISION)

      INTEGER(2), PARAMETER :: FPCW$MCW_PC        = Z'0300'  ! PRECISION CONTROL MASK
      INTEGER(2), PARAMETER :: FPCW$64            = Z'0300'  ! 64 BITS
      INTEGER(2), PARAMETER :: FPCW$53            = Z'0200'  ! 53 BITS
      INTEGER(2), PARAMETER :: FPCW$24            = Z'0000'  ! 24 BITS
                                                    
      INTEGER(2), PARAMETER :: FPCW$MCW_IC        = Z'1000'  ! INFINITY CONTROL MASK
      INTEGER(2), PARAMETER :: FPCW$AFFINE        = Z'1000'  ! AFFINE
      INTEGER(2), PARAMETER :: FPCW$PROJECTIVE    = Z'0000'  ! PROJECTIVE
                                                    
      INTEGER(2), PARAMETER :: FPCW$MCW_RC        = Z'0C00'  ! ROUNDING CONTROL MASK
      INTEGER(2), PARAMETER :: FPCW$CHOP          = Z'0C00'  ! CHOP
      INTEGER(2), PARAMETER :: FPCW$UP            = Z'0800'  ! UP
      INTEGER(2), PARAMETER :: FPCW$DOWN          = Z'0400'  ! DOWN
      INTEGER(2), PARAMETER :: FPCW$NEAR          = Z'0000'  ! NEAR
                                                    
      INTEGER(2), PARAMETER :: FPSW$MSW_EM        = Z'003F'  ! EXCEPTION MASK
      INTEGER(2), PARAMETER :: FPSW$INVALID       = Z'0001'  ! INVALID
      INTEGER(2), PARAMETER :: FPSW$DENORMAL      = Z'0002'  ! DENORMAL
      INTEGER(2), PARAMETER :: FPSW$ZERODIVIDE    = Z'0004'  ! ZERO DIVIDE
      INTEGER(2), PARAMETER :: FPSW$OVERFLOW      = Z'0008'  ! OVERFLOW
      INTEGER(2), PARAMETER :: FPSW$UNDERFLOW     = Z'0010'  ! UNDERFLOW
      INTEGER(2), PARAMETER :: FPSW$INEXACT       = Z'0020'  ! INEXACT (PRECISION)

! FPSR TRAPS
      INTEGER(4) FPSR_TRAP_MASK
      INTEGER(4) FPSR_TRAP_VD_MASK
      INTEGER(4) FPSR_TRAP_DD_MASK
      INTEGER(4) FPSR_TRAP_ZD_MASK
      INTEGER(4) FPSR_TRAP_OD_MASK
      INTEGER(4) FPSR_TRAP_UD_MASK
      INTEGER(4) FPSR_TRAP_ID_MASK
      INTEGER(4) FPSR_TRAP_VD_POS
      INTEGER(4) FPSR_TRAP_DD_POS
      INTEGER(4) FPSR_TRAP_ZD_POS
      INTEGER(4) FPSR_TRAP_OD_POS
      INTEGER(4) FPSR_TRAP_UD_POS
      INTEGER(4) FPSR_TRAP_ID_POS

      PARAMETER (FPSR_TRAP_MASK = Z'0000003F')
      PARAMETER (FPSR_TRAP_VD_MASK = Z'00000001')
      PARAMETER (FPSR_TRAP_DD_MASK = Z'00000002')
      PARAMETER (FPSR_TRAP_ZD_MASK = Z'00000004')
      PARAMETER (FPSR_TRAP_OD_MASK = Z'00000008')
      PARAMETER (FPSR_TRAP_UD_MASK = Z'00000010')
      PARAMETER (FPSR_TRAP_ID_MASK = Z'00000020')
      PARAMETER (FPSR_TRAP_VD_POS = Z'00000000')
      PARAMETER (FPSR_TRAP_DD_POS = Z'00000001')
      PARAMETER (FPSR_TRAP_ZD_POS = Z'00000002')
      PARAMETER (FPSR_TRAP_OD_POS = Z'00000003')
      PARAMETER (FPSR_TRAP_UD_POS = Z'00000004')
      PARAMETER (FPSR_TRAP_ID_POS = Z'00000005')
                 
                 

!FPE SIGNALS
      INTEGER(4), PARAMETER :: FPE$INVALID        = Z'81'
      INTEGER(4), PARAMETER :: FPE$DENORMAL       = Z'82'
      INTEGER(4), PARAMETER :: FPE$ZERODIVIDE     = Z'83'
      INTEGER(4), PARAMETER :: FPE$OVERFLOW       = Z'84'
      INTEGER(4), PARAMETER :: FPE$UNDERFLOW      = Z'85'
      INTEGER(4), PARAMETER :: FPE$INEXACT        = Z'86'
      INTEGER(4), PARAMETER :: FPE$UNEMULATED     = Z'87'
      INTEGER(4), PARAMETER :: FPE$SQRTNEG        = Z'88'
      INTEGER(4), PARAMETER :: FPE$STACKOVERFLOW  = Z'8A'
      INTEGER(4), PARAMETER :: FPE$STACKUNDERFLOW = Z'8B'
      INTEGER(4), PARAMETER :: FPE$EXPLICITGEN    = Z'8C'


      INTERFACE

        SUBROUTINE GETCONTROLFPQQ(CONTROL)
          !DEC$ ATTRIBUTES DEFAULT :: GETCONTROLFPQQ
         INTEGER(2) CONTROL
        END SUBROUTINE
  
        SUBROUTINE GETSTATUSFPQQ(STATUS)
          !DEC$ ATTRIBUTES DEFAULT :: GETSTATUSFPQQ
          INTEGER(2) STATUS
        END SUBROUTINE

        SUBROUTINE CLEARSTATUSFPQQ()
          !DEC$ ATTRIBUTES DEFAULT :: CLEARSTATUSFPQQ
        END SUBROUTINE

        SUBROUTINE LCWRQQ(CONTROL)
          !DEC$ ATTRIBUTES DEFAULT :: LCWRQQ
          INTEGER(2) CONTROL
        END SUBROUTINE
   
        SUBROUTINE SCWRQQ(CONTROL)
          !DEC$ ATTRIBUTES DEFAULT :: SCWRQQ
          INTEGER(2) CONTROL
        END SUBROUTINE
   
        SUBROUTINE SETCONTROLFPQQ(CONTROL)
          !DEC$ ATTRIBUTES DEFAULT :: SETCONTROLFPQQ
          INTEGER(2) CONTROL
        END SUBROUTINE

        SUBROUTINE SSWRQQ(STATUS)
          !DEC$ ATTRIBUTES DEFAULT :: SSWRQQ
          INTEGER(2) STATUS
        END SUBROUTINE

! SET PROCESSOR F.P. MASK FLAGS
        INTEGER FUNCTION IEEE_FLAGS(ACTION, MODE, IN, OUT)
          !DEC$ ATTRIBUTES DEFAULT :: IEEE_FLAGS
          CHARACTER(LEN=*), INTENT(IN) :: ACTION, MODE, IN
          CHARACTER(LEN=*), INTENT(OUT) :: OUT
        END FUNCTION
        
      END INTERFACE        

! ESTABLISH A HANDLER FOR IEEE EXCEPTIONS
      INTERFACE IEEE_HANDLER
        FUNCTION IEEE_HANDLER(ARG_ACTION,ARG_EXCEPTION,HNDLR)
!DEC$ IF  DEFINED(_WIN32) .OR. DEFINED(_WIN64)
          !DEC$ ATTRIBUTES DEFAULT,DECORATE,ALIAS:'IEEE_HANDLER' ::  IEEE_HANDLER
!DEC$ ELSEIF DEFINED(__APPLE__)
          !DEC$ ATTRIBUTES DEFAULT,ALIAS:'_ieee_handler_' ::  IEEE_HANDLER
!DEC$ ELSE
          !DEC$ ATTRIBUTES DEFAULT,ALIAS:'ieee_handler_' ::  IEEE_HANDLER
!DEC$ ENDIF
          USE IFPORT_TYPES
          INTEGER(SIZEOF_SIZE_T) :: IEEE_HANDLER
          CHARACTER(LEN=*) ARG_ACTION, ARG_EXCEPTION
          INTERFACE
            SUBROUTINE HNDLR(SIGNO, SIGINFO)
              INTEGER(4), INTENT(IN) :: SIGNO, SIGINFO
            END SUBROUTINE
          END INTERFACE
        END FUNCTION

        FUNCTION IEEE_INT_HANDLER(ARG_ACTION,ARG_EXCEPTION,HNDLR)          
!DEC$ IF  DEFINED(_WIN32) .OR. DEFINED(_WIN64)
          !DEC$ ATTRIBUTES DEFAULT,DECORATE,ALIAS:'IEEE_HANDLER' ::  IEEE_INT_HANDLER
!DEC$ ELSEIF DEFINED(__APPLE__)
          !DEC$ ATTRIBUTES DEFAULT,ALIAS:'_ieee_handler_' ::  IEEE_INT_HANDLER
!DEC$ ELSE 
          !DEC$ ATTRIBUTES DEFAULT,ALIAS:'ieee_handler_' ::  IEEE_INT_HANDLER
!DEC$ ENDIF
          USE IFPORT_TYPES
          INTEGER(SIZEOF_SIZE_T) :: IEEE_INT_HANDLER
          CHARACTER(LEN=*) ARG_ACTION, ARG_EXCEPTION
          INTEGER(SIZEOF_SIZE_T) HNDLR
          !DEC$ ATTRIBUTES VALUE :: HNDLR
        END FUNCTION          
      END INTERFACE IEEE_HANDLER

! -----------------------------------------------------------------
! File Management Routines
! -----------------------------------------------------------------
      INTERFACE
        INTEGER(4) FUNCTION DELFILESQQ(FILES)
          !DEC$ ATTRIBUTES DEFAULT :: DELFILESQQ
          CHARACTER(LEN=*) FILES
        END FUNCTION

        INTEGER(4) FUNCTION FINDFILEQQ(FILE, ENV, BUF)
          !DEC$ ATTRIBUTES DEFAULT :: FINDFILEQQ
          CHARACTER(LEN=*) FILE, ENV, BUF
        END FUNCTION

        INTEGER(4) FUNCTION FULLPATHQQ(NAME, FULLPATH)
          !DEC$ ATTRIBUTES DEFAULT :: FULLPATHQQ
            CHARACTER(LEN=*) NAME, FULLPATH
        END FUNCTION
       END INTERFACE

! GET INFORMATION ABOUT A FILE
       INTERFACE GETFILEINFOQQ
        INTEGER(4) FUNCTION GETFILEINFOQQ(FILES, BUFFER,DWHANDLE)
          !DEC$ ATTRIBUTES DEFAULT :: GETFILEINFOQQ
          use IFPORT_TYPES
          CHARACTER(LEN=*) FILES
          TYPE(FILE$INFO) :: BUFFER
          INTEGER(POINTER_LEN) DWHANDLE
        END FUNCTION GETFILEINFOQQ

        INTEGER(4) FUNCTION GETFILEINFOQQI8(FILES, BUFFER,DWHANDLE)
          !DEC$ ATTRIBUTES DEFAULT :: GETFILEINFOQQI8
          use IFPORT_TYPES
          CHARACTER(LEN=*) FILES
          TYPE(FILE$INFOI8) :: BUFFER
          INTEGER(POINTER_LEN) DWHANDLE
        END FUNCTION GETFILEINFOQQI8
       END INTERFACE 

       INTERFACE   
        SUBROUTINE PACKTIMEQQ(TIMEDATE,IYR,IMON,IDAY,IHR,IMIN,ISEC)
          !DEC$ ATTRIBUTES DEFAULT :: PACKTIMEQQ
          INTEGER(4) TIMEDATE
          INTEGER(2) IYR, IMON, IDAY, IHR, IMIN, ISEC
        END SUBROUTINE

        LOGICAL(4) FUNCTION RENAMEFILEQQ(OLDNAME, NEWNAME)
          !DEC$ ATTRIBUTES DEFAULT :: RENAMEFILEQQ
          CHARACTER(LEN=*) OLDNAME, NEWNAME
        END FUNCTION RENAMEFILEQQ

        LOGICAL(4) FUNCTION SETFILEACCESSQQ(NAME, ACCESS)
          !DEC$ ATTRIBUTES DEFAULT :: SETFILEACCESSQQ
          CHARACTER(LEN=*) NAME
          INTEGER(4) ACCESS
        END FUNCTION

        LOGICAL(4) FUNCTION SETFILETIMEQQ(NAME, TIMEDATE)
          !DEC$ ATTRIBUTES DEFAULT :: SETFILETIMEQQ
          CHARACTER(LEN=*) NAME
          INTEGER(4) TIMEDATE
        END FUNCTION

        INTEGER(4) FUNCTION SPLITPATHQQ(PATH, DRIVE, DIR, NAME, EXT)
          !DEC$ ATTRIBUTES DEFAULT :: SPLITPATHQQ
            CHARACTER(LEN=*) PATH, DRIVE, DIR, NAME, EXT
        END FUNCTION

        SUBROUTINE UNPACKTIMEQQ(TIMEDATE,IYR,IMON,IDAY,IHR,IMIN,ISEC)
          !DEC$ ATTRIBUTES DEFAULT :: UNPACKTIMEQQ
          INTEGER(4) TIMEDATE
          INTEGER(2) IYR, IMON, IDAY, IHR, IMIN, ISEC
        END SUBROUTINE
      END INTERFACE

!DEC$ IF  DEFINED(_WIN32) .OR. DEFINED(_WIN64)
!!!************************ SPORT defs *************************************
!
!**  TOSS = what to throw away on input
!**  OUT = what to terminate records with on output
!**  TERM = what to look for for termination on input
!--------------------------------------------

      integer, parameter :: DL_TOSS_CR    = '01'X
      integer, parameter :: DL_TOSS_LF    = '02'X
      integer, parameter :: DL_TERM_CR    = '04'X
      integer, parameter :: DL_TERM_LF    = '08'X
      integer, parameter :: DL_TERM_CRLF  = '0C'X
      integer, parameter :: DL_OUT_CR     = '10'X
      integer, parameter :: DL_OUT_LF     = '20'X

      !  istat = sport_connect( port, delim_options )

      interface
      integer*4 function SPORT_CONNECT ( port, options )
      !dec$ attributes default, decorate, alias:'SPORT_CONNECT' :: SPORT_CONNECT
      integer(4), intent(in) :: port
      integer(4), intent(in), optional :: options
      end function SPORT_CONNECT
      end interface

      !  istat = sport_connect_ex( port, delim_options, BufferSize )

      interface
      integer*4 function SPORT_CONNECT_EX ( port, options, BufferSize )
      !dec$ attributes default, decorate, alias:'SPORT_CONNECT_EX' :: SPORT_CONNECT_EX
      integer(4), intent(in) :: port
      integer(4), intent(in), optional :: options
      integer(4), intent(in), optional :: BufferSize
      end function SPORT_CONNECT_EX
      end interface

      !  istat = sport_release( port )

      interface
      integer*4 function SPORT_RELEASE ( port )
      !dec$ attributes default, decorate, alias:'SPORT_RELEASE' :: SPORT_RELEASE
      integer(4), intent(in) :: port
      end function SPORT_RELEASE
      end interface

      !  ihnd = sport_get_handle( port, handle )

      interface
      integer*4 function SPORT_GET_HANDLE ( port, handle )
      !dec$ attributes default, decorate, alias:'SPORT_GET_HANDLE' :: SPORT_GET_HANDLE
      integer(4), intent(in) :: port
      integer(INT_PTR_KIND()), intent(out) :: handle
      end function SPORT_GET_HANDLE
      end interface

      !   istat = sport_get_state( port, baud, parity, data, stop )

      interface
      integer*4 function SPORT_GET_STATE ( port, baud, parity, dbits, sbits )
      !dec$ attributes default, decorate, alias:'SPORT_GET_STATE' :: SPORT_GET_STATE
      integer(4), intent(in) :: port
      integer(4), intent(out), optional :: baud
      integer(4), intent(out), optional :: parity
      integer(4), intent(out), optional :: dbits
      integer(4), intent(out), optional :: sbits
      end function SPORT_GET_STATE
      end interface

      !   istat = sport_get_state_ex( port, baud, parity, data, stop, ... )

      interface
      integer*4 function SPORT_GET_STATE_EX ( port, baud, parity, dbits, sbits, &
                Binmode, DTRcntrl, RTScntrl, OutCTSFlow, OutDSRFlow, DSRSense, &
                OutXonOff, InXonOff, XonLim, XoffLim, TXContOnXoff, ErrAbort, &
                ErrCharEnbl, NullStrip, XonChar, XoffChar, ErrChar, EofChar, EvtChar )
      !dec$ attributes default, decorate, alias:'SPORT_GET_STATE_EX' :: SPORT_GET_STATE_EX
      integer(4), intent(in) :: port
      integer(4), intent(out), optional :: baud
      integer(4), intent(out), optional :: parity
      integer(4), intent(out), optional :: dbits
      integer(4), intent(out), optional :: sbits
      integer(4), intent(out), optional :: Binmode
      integer(4), intent(out), optional :: DTRcntrl
      integer(4), intent(out), optional :: RTScntrl
      integer(4), intent(out), optional :: OutCTSFlow
      integer(4), intent(out), optional :: OutDSRFlow
      integer(4), intent(out), optional :: DSRSense
      integer(4), intent(out), optional :: OutXonOff
      integer(4), intent(out), optional :: InXonOff
      integer(4), intent(out), optional :: XonLim
      integer(4), intent(out), optional :: XoffLim
      integer(4), intent(out), optional :: TXContOnXoff
      integer(4), intent(out), optional :: ErrAbort
      integer(4), intent(out), optional :: ErrCharEnbl
      integer(4), intent(out), optional :: NullStrip
      character,  intent(out), optional :: XonChar
      character,  intent(out), optional :: XoffChar
      character,  intent(out), optional :: ErrChar
      character,  intent(out), optional :: EofChar
      character,  intent(out), optional :: EvtChar
      end function SPORT_GET_STATE_EX
      end interface

      !   istat = sport_set_state( port, baud, parity, data, stop )

      interface
      integer*4 function SPORT_SET_STATE ( port, baud, parity, dbits, sbits )
      !dec$ attributes default, decorate, alias:'SPORT_SET_STATE' :: SPORT_SET_STATE
      integer(4), intent(in) :: port
      integer(4), intent(in), optional :: baud
      integer(4), intent(in), optional :: parity
      integer(4), intent(in), optional :: dbits
      integer(4), intent(in), optional :: sbits
      end function SPORT_SET_STATE
      end interface

      !   istat = sport_set_state_ex( port, baud, parity, data, stop, ... )

      interface
      integer*4 function SPORT_SET_STATE_EX ( port, baud, parity, dbits, sbits, &
                Binmode, DTRcntrl, RTScntrl, OutCTSFlow, OutDSRFlow, DSRSense, &
                OutXonOff, InXonOff, XonLim, XoffLim, TXContOnXoff, ErrAbort, &
                ErrCharEnbl, NullStrip, XonChar, XoffChar, ErrChar, EofChar, EvtChar, &
                fZeroDCB )
      !dec$ attributes default, decorate, alias:'SPORT_SET_STATE_EX' :: SPORT_SET_STATE_EX
      integer(4), intent(in) :: port
      integer(4), intent(in), optional :: baud
      integer(4), intent(in), optional :: parity
      integer(4), intent(in), optional :: dbits
      integer(4), intent(in), optional :: sbits
      integer(4), intent(in), optional :: Binmode
      integer(4), intent(in), optional :: DTRcntrl
      integer(4), intent(in), optional :: RTScntrl
      integer(4), intent(in), optional :: OutCTSFlow
      integer(4), intent(in), optional :: OutDSRFlow
      integer(4), intent(in), optional :: DSRSense
      integer(4), intent(in), optional :: OutXonOff
      integer(4), intent(in), optional :: InXonOff
      integer(4), intent(in), optional :: XonLim
      integer(4), intent(in), optional :: XoffLim
      integer(4), intent(in), optional :: TXContOnXoff
      integer(4), intent(in), optional :: ErrAbort
      integer(4), intent(in), optional :: ErrCharEnbl
      integer(4), intent(in), optional :: NullStrip
      character,  intent(in), optional :: XonChar
      character,  intent(in), optional :: XoffChar
      character,  intent(in), optional :: ErrChar
      character,  intent(in), optional :: EofChar
      character,  intent(in), optional :: EvtChar
      integer(4), intent(in), optional :: fZeroDCB
      end function SPORT_SET_STATE_EX
      end interface


      interface
      integer*4 function SPORT_SHOW_STATE ( port, level )
      !dec$ attributes default, decorate, alias:'SPORT_SHOW_STATE' :: SPORT_SHOW_STATE
      integer(4), intent(in) :: port
      integer(4), intent(in), optional :: level
      end function SPORT_SHOW_STATE
      end interface

      !   istat = sport_set/get_timeouts( port, ... )

      interface
      integer*4 function SPORT_GET_TIMEOUTS ( port, rx_int, tx_tot_mult, &
      			tx_tot_const )
      !dec$ attributes default, decorate, alias:'SPORT_GET_TIMEOUTS' :: SPORT_GET_TIMEOUTS
      integer(4), intent(in) :: port
      integer(4), intent(out), optional :: rx_int
      integer(4), intent(out), optional :: tx_tot_mult
      integer(4), intent(out), optional :: tx_tot_const
      end function SPORT_GET_TIMEOUTS
      end interface

      interface
      integer*4 function SPORT_SET_TIMEOUTS ( port, rx_int, tx_tot_mult, &
      			tx_tot_const )
      !dec$ attributes default, decorate, alias:'SPORT_SET_TIMEOUTS' :: SPORT_SET_TIMEOUTS
      integer(4), intent(in) :: port
      integer(4), intent(in), optional :: rx_int
      integer(4), intent(in), optional :: tx_tot_mult
      integer(4), intent(in), optional :: tx_tot_const
      end function SPORT_SET_TIMEOUTS
      end interface


      !   istat = sport_special_func( port, func )

      interface
      integer*4 function SPORT_SPECIAL_FUNC ( port, function )
      !dec$ attributes default, decorate, alias:'SPORT_SPECIAL_FUNC' :: SPORT_SPECIAL_FUNC
      integer(4), intent(in) :: port
      integer(4), intent(in) :: function
      end function SPORT_SPECIAL_FUNC
      end interface


      !    istat = sport_write_data( port, buff, hidden-len, count )

      interface
      integer*4 function SPORT_WRITE_DATA ( port, data, count )
      !dec$ attributes default, decorate, alias:'SPORT_WRITE_DATA' :: SPORT_WRITE_DATA
      integer(4), intent(in) :: port
      character*(*), intent(in) :: data
      integer(4), intent(in), optional :: count
      end function SPORT_WRITE_DATA
      end interface

      interface
      integer*4 function SPORT_WRITE_LINE ( port, data, count )
      !dec$ attributes default, decorate, alias:'SPORT_WRITE_LINE' :: SPORT_WRITE_LINE
      integer(4), intent(in) :: port
      character*(*), intent(in) :: data
      integer(4), intent(in), optional :: count
      end function SPORT_WRITE_LINE
      end interface

      !    istat = sport_read_data( port, buffer, len, count )

      interface
      integer*4 function SPORT_READ_DATA ( port, buffer, count )
      !dec$ attributes default, decorate, alias:'SPORT_READ_DATA' :: SPORT_READ_DATA
      integer(4), intent(in) :: port
      character*(*), intent(out) :: buffer
      integer(4), intent(out), optional :: count
      end function SPORT_READ_DATA
      end interface

      !    istat = sport_read_line( port, buffer, len, count )

      interface
      integer*4 function SPORT_READ_LINE ( port, buffer, count )
      !dec$ attributes default, decorate, alias:'SPORT_READ_LINE' :: SPORT_READ_LINE
      integer(4), intent(in) :: port
      character*(*), intent(out) :: buffer
      integer(4), intent(out), optional :: count
      end function SPORT_READ_LINE
      end interface

      !    istat = sport_peek_data( port, present, count )

      interface
      integer*4 function SPORT_PEEK_DATA ( port, present, count )
      !dec$ attributes default, decorate, alias:'SPORT_PEEK_DATA' :: SPORT_PEEK_DATA
      integer(4), intent(in) :: port
      integer(4), intent(out), optional :: present
      integer(4), intent(out), optional :: count
      end function SPORT_PEEK_DATA
      end interface


      !    istat = sport_peek_line( port, present, count )

      interface
      integer*4 function SPORT_PEEK_LINE ( port, present, count )
      !dec$ attributes default, decorate, alias:'SPORT_PEEK_LINE' :: SPORT_PEEK_LINE
      integer(4), intent(in) :: port
      integer(4), intent(out), optional :: present
      integer(4), intent(out), optional :: count
      end function SPORT_PEEK_LINE
      end interface


      !    istat = sport_purge( port, function )

      interface
      integer*4 function SPORT_PURGE ( port, function )
      !dec$ attributes default, decorate, alias:'SPORT_PURGE' :: SPORT_PURGE
      integer(4), intent(in) :: port
      integer(4), intent(in) :: function
      end function SPORT_PURGE
      end interface

      !    istat = sport_cancel_io( port )

      interface
      integer*4 function SPORT_CANCEL_IO ( port )
      !dec$ attributes default, decorate, alias:'SPORT_CANCEL_IO' :: SPORT_CANCEL_IO
      integer(4), intent(in) :: port
      end function SPORT_CANCEL_IO
      end interface

!!!********************* End SPORT defs *************************************


!DEC$ ENDIF

      CHARACTER(1), PARAMETER :: FILE$CURDRIVE  = ' '
      INTEGER(4), PARAMETER :: FILE$MAXNAME     = 255
      INTEGER(4), PARAMETER :: MAXPATH          = 260
      INTEGER(4), PARAMETER :: $MAXPATH         = 260

      INTEGER(4), PARAMETER :: FILE$NORMAL      = Z'0000'
      INTEGER(4), PARAMETER :: FILE$READONLY    = Z'0001'
      INTEGER(4), PARAMETER :: FILE$HIDDEN      = Z'0002'
      INTEGER(4), PARAMETER :: FILE$SYSTEM      = Z'0004'
      INTEGER(4), PARAMETER :: FILE$VOLUME      = Z'0008'
      INTEGER(4), PARAMETER :: FILE$DIR         = Z'0010'
      INTEGER(4), PARAMETER :: FILE$ARCHIVE     = Z'0020'

      INTEGER(4), PARAMETER :: FILE$FIRST       = -1
      INTEGER(4), PARAMETER :: FILE$LAST        = -2
      INTEGER(4), PARAMETER :: FILE$ERROR       = -3

! FOR PACKTIMEQQ AND UNPACKTIMEQQ
      INTEGER(4), PARAMETER :: FILE$INVALID     = -1
! FOR SETFILETIMEQQ
      INTEGER(4), PARAMETER :: FILE$CURTIME     = -1
      
! -----------------------------------------------------------------
! 
! -----------------------------------------------------------------

      INTEGER(4), PARAMETER :: SRT$REAL4          = Z'00010000'
      INTEGER(4), PARAMETER :: SRT$REAL8          = Z'00020000'
      INTEGER(4), PARAMETER :: SRT$INTEGER1       = Z'00030000'
      INTEGER(4), PARAMETER :: SRT$INTEGER2       = Z'00040000'
      INTEGER(4), PARAMETER :: SRT$INTEGER4       = Z'00050000'
      INTEGER(4), PARAMETER :: SRT$INTEGER8       = Z'00060000'
      INTEGER(4), PARAMETER :: SRT$REAL10         = Z'00070000'
      INTEGER(4), PARAMETER :: SRT$REAL16         = Z'00080000'

      END MODULE
!
!  End of module IFPORT


!! End of file IFPORT.F90!!!!!!!!!!!!
