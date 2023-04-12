#include "symbol.inc"

MODULE targetpot 

 USE prec
 USE solvation
 USE pot_k
 USE base
 USE lattice
 USE poscar
 USE main_mpi

 TYPE info_tpot
 LOGICAL :: LTPOT           = .FALSE.  ! Target potential on/off
 LOGICAL :: TRUEVACLEVEL    = .TRUE.   ! .TRUE. reevaluating vacuum level. .FALSE. = -FERMI_SHIFT
 LOGICAL :: READY           = .FALSE.  ! IF INCAR IS READ
 INTEGER :: METHOD          = 1        ! 1 electronics, 2 ionic
 REAL(q) :: VDIFF           = 0.01     ! Rotential Threshold
 REAL(q) :: VTARGET         = 4.44     ! Target potential, -chemical potential
 REAL(q) :: VRATE           = -1.0     ! Initial Rate of changing NELECT, electron/V
 REAL(q) :: VRATELIM        = 0.2      ! Limit for |TPOTVRATE|, electron/V
 REAL(q) :: VRATEDAMP       = 1.0      ! Damping factor for |TPOTVRATE|
 REAL(q) :: VEDIFF          = 1.d-4    ! Energy threshold of electronic iteration to start updating NELECT
 LOGICAL :: DYNVRATE        = .TRUE.   ! Updating NELECT with VRATE
 REAL(q) :: ELECTSTEP       = 0.01     ! (Maximum) Amount of electrons changed.
 REAL(q) :: NELECT0         = 0        ! Last Number of electron 
 REAL(q) :: V0              = 0        ! Last Potential 
 REAL(q) :: FERMI_SHIFT     = 0        ! Last FERMI SHIFT
 REAL(q) :: VACLEVEL        = 0        ! Last Vacuum level
 REAL(q) :: DFERMI_SHIFTLIM = 0.5      ! Limit for change in FERMI_SHIFT
 REAL(q) :: DNELECTLIM      = 1.d-4    ! Limit for amount of electron changed for updating VRATE
 LOGICAL :: GCENERGY        = .TRUE.   ! GC correction -(NELECT - ZVALS)*EFERMI  included in TOTEN
 LOGICAL :: GCIONIC         = .TRUE.   ! GC correction to the end of each ionic step IBRION = 0
 REAL(q) :: ZVALS           = 0.0      ! Total number of valence electron for neutral case
 INTEGER :: ITER            = 0        ! Iteration
 END TYPE

 TYPE info_penalty
 LOGICAL :: LPENPOT         = .FALSE.  ! Penalty potential is applied or not, 
                                       ! softplus = h * log( 1 + exp( ( z - zo ) / w ) )
 REAL(q) :: H               = 1.       ! Potential high
 REAL(q) :: W               = 1.       ! Potential width
 REAL(q) :: Z0              = 0.5      ! Z position (crystal coor) of the separation between explicit and implicit region.
 REAL(q) :: ZMIN            = 0.4      ! MIN Z position (crystal coor) to apply PENPOT

 END TYPE

 TYPE (info_tpot ) :: TPOT
 TYPE (info_penalty) :: PENPOT
 PUBLIC :: UPDATE_NELECT, TPOT, PENPOT
 REAL(q), PUBLIC :: VACLEVEL_TPOT

 CONTAINS

 SUBROUTINE UPDATE_NELECT( IO, INFO, EFERMI, DESUM_, DESUM1_)

      IMPLICIT NONE

      TYPE (in_struct), INTENT(IN) :: IO
      TYPE (info_struct), INTENT(INOUT) :: INFO
      REAL(q), INTENT(IN)    :: EFERMI, DESUM_, DESUM1_ 
      REAL(q) :: FERMI_SHIFT, VACLEVEL, V, NELECT, DNELECT


      INTEGER IDUM,N,IERR
      LOGICAL :: LOPEN,LDUM, TPOTCAR_EXISTS
      REAL(q) :: RDUM, NEB_NELECT
      COMPLEX(q) :: CDUM
      CHARACTER*1 :: CHARAC
      CHARACTER*20 :: METHOD
      CHARACTER*30 :: VFORMULA
 

      IF ( .NOT. TPOT%READY ) THEN
          LOPEN=.FALSE.
          OPEN(UNIT=IO%IU5,FILE='INCAR',STATUS='OLD')
          CALL RDATAB(LOPEN,'INCAR',IO%IU5,'LTPOT','=','#',';','L', &
                IDUM,RDUM,CDUM,TPOT%LTPOT,CHARAC,N,1,IERR)
          IF ((IERR/=0).OR.(N<1)) TPOT%LTPOT=.FALSE.
          IF ( TPOT%LTPOT ) THEN

               CALL RDATAB(LOPEN,'INCAR',IO%IU5,'TPOTMETHOD','=','#',';','I', &
                TPOT%METHOD,RDUM,CDUM,LDUM,CHARAC,N,1,IERR)
               IF ((IERR/=0).OR.(N<1)) TPOT%METHOD= 1
              
               CALL RDATAB(LOPEN,'INCAR',IO%IU5,'TPOTTRUEVACLEVEL','=','#',';','L', &
                IDUM,RDUM,CDUM,TPOT%TRUEVACLEVEL,CHARAC,N,1,IERR)
               IF ((IERR/=0).OR.(N<1)) TPOT%TRUEVACLEVEL= .TRUE.

               CALL RDATAB(LOPEN,'INCAR',IO%IU5,'TPOTVTARGET','=','#',';','F', &
                IDUM,TPOT%VTARGET,CDUM,LDUM,CHARAC,N,1,IERR)
               IF ((IERR/=0).OR.(N<1)) TPOT%VTARGET= 3.44

               CALL RDATAB(LOPEN,'INCAR',IO%IU5,'TPOTVDIFF','=','#',';','F', &
                IDUM,TPOT%VDIFF,CDUM,LDUM,CHARAC,N,1,IERR)
               IF ((IERR/=0).OR.(N<1)) TPOT%VDIFF= 0.01

               CALL RDATAB(LOPEN,'INCAR',IO%IU5,'TPOTVRATE','=','#',';','F', &
                IDUM,TPOT%VRATE,CDUM,LDUM,CHARAC,N,1,IERR)
               IF ((IERR/=0).OR.(N<1)) TPOT%VRATE= -1.0

               CALL RDATAB(LOPEN,'INCAR',IO%IU5,'TPOTVRATELIM','=','#',';','F', &
                IDUM,TPOT%VRATELIM,CDUM,LDUM,CHARAC,N,1,IERR)
               IF ((IERR/=0).OR.(N<1)) TPOT%VRATELIM= 0.2

               CALL RDATAB(LOPEN,'INCAR',IO%IU5,'TPOTVRATEDAMP','=','#',';','F',&
                IDUM,TPOT%VRATEDAMP,CDUM,LDUM,CHARAC,N,1,IERR)
               IF ((IERR/=0).OR.(N<1)) TPOT%VRATEDAMP = 1.

               CALL RDATAB(LOPEN,'INCAR',IO%IU5,'TPOTVEDIFF','=','#',';','F', &
                IDUM,TPOT%VEDIFF,CDUM,LDUM,CHARAC,N,1,IERR)
               IF ((IERR/=0).OR.(N<1)) TPOT%VEDIFF= 0.0001

               CALL RDATAB(LOPEN,'INCAR',IO%IU5,'TPOTDYNVRATE','=','#',';','L', &
                IDUM,RDUM,CDUM,TPOT%DYNVRATE,CHARAC,N,1,IERR)
               IF ((IERR/=0).OR.(N<1)) TPOT%DYNVRATE= .FALSE.

               CALL RDATAB(LOPEN,'INCAR',IO%IU5,'TPOTELECTSTEP','=','#',';','F', &
                IDUM,TPOT%ELECTSTEP,CDUM,LDUM,CHARAC,N,1,IERR)
               IF ((IERR/=0).OR.(N<1)) TPOT%ELECTSTEP= 0.01

               CALL RDATAB(LOPEN,'INCAR',IO%IU5,'TPOTDFERMI_SHIFTLIM','=','#',';','F',&
                IDUM,TPOT%DFERMI_SHIFTLIM,CDUM,LDUM,CHARAC,N,1,IERR)
               IF ((IERR/=0).OR.(N<1)) TPOT%DFERMI_SHIFTLIM= 0.5

               CALL RDATAB(LOPEN,'INCAR',IO%IU5,'TPOTDNELECTLIM','=','#',';','F',&
                IDUM,TPOT%DNELECTLIM,CDUM,LDUM,CHARAC,N,1,IERR)
               IF ((IERR/=0).OR.(N<1)) TPOT%DNELECTLIM = 1.d-4

               CALL RDATAB(LOPEN,'INCAR',IO%IU5,'TPOTGCENERGY','=','#',';','L', &
                IDUM,RDUM,CDUM,TPOT%GCENERGY,CHARAC,N,1,IERR)
               IF ((IERR/=0).OR.(N<1)) TPOT%GCENERGY = .TRUE.

               CALL RDATAB(LOPEN,'INCAR',IO%IU5,'TPOTGCIONIC','=','#',';','L', &
                IDUM,RDUM,CDUM,TPOT%GCIONIC,CHARAC,N,1,IERR)
               IF ((IERR/=0).OR.(N<1)) TPOT%GCIONIC = .TRUE.
               
          ENDIF

          ! Read information for penalty potential
          CALL RDATAB(LOPEN,'INCAR',IO%IU5,'LPENPOT','=','#',';','L', &
              IDUM,RDUM,CDUM,PENPOT%LPENPOT,CHARAC,N,1,IERR)
          IF ((IERR/=0).OR.(N<1)) PENPOT%LPENPOT = .FALSE.

          CALL RDATAB(LOPEN,'INCAR',IO%IU5,'PENPOT_H','=','#',';','F',&
           IDUM,PENPOT%H,CDUM,LDUM,CHARAC,N,1,IERR)
          IF ((IERR/=0).OR.(N<1)) PENPOT%H = 1.

          CALL RDATAB(LOPEN,'INCAR',IO%IU5,'PENPOT_W','=','#',';','F',&
           IDUM,PENPOT%W,CDUM,LDUM,CHARAC,N,1,IERR)
          IF ((IERR/=0).OR.(N<1)) PENPOT%W = 1.

          CALL RDATAB(LOPEN,'INCAR',IO%IU5,'PENPOT_Z0','=','#',';','F',&
           IDUM,PENPOT%Z0,CDUM,LDUM,CHARAC,N,1,IERR)
          IF ((IERR/=0).OR.(N<1)) PENPOT%Z0 = 0.5 

          CALL RDATAB(LOPEN,'INCAR',IO%IU5,'PENPOT_ZMIN','=','#',';','F',&
           IDUM,PENPOT%ZMIN,CDUM,LDUM,CHARAC,N,1,IERR)
          IF ((IERR/=0).OR.(N<1)) PENPOT%ZMIN = PENPOT%Z0 - 0.1

          TPOT%READY   = .TRUE.
          TPOT%NELECT0 = INFO%NELECT
          TPOT%V0      = 10000
      ENDIF

      IF ( .NOT. LSOL ) THEN
          IF (IO%IU6>=0) WRITE( IO%IU6, 9004 )
          TPOT%LTPOT = .FALSE.
          GOTO 9006 
      ENDIF

      IF ( LSOL .AND. .NOT. TPOT%LTPOT .AND. TPOT%ITER == 0 ) THEN
          TPOT%METHOD = 3
          IF (IO%IU6>=0) WRITE( IO%IU6, 9005 ) LSOL, TPOT%LTPOT, TPOT%METHOD
      ENDIF

      ! Update NELECT with TPOTCAR During TPOT%ITER == 0
      IF ( TPOT%ITER == 0 ) THEN
          INQUIRE(FILE = DIR_APP(1:DIR_LEN)//"TPOTCAR", EXIST = tpotcar_exists)
          IF ( tpotcar_exists ) THEN
              OPEN(UNIT=9101,FILE=DIR_APP(1:DIR_LEN)//"TPOTCAR",STATUS='OLD')
              READ(9101,*) NEB_NELECT 
              INFO%NELECT = NEB_NELECT
              CLOSE(9101)
          ENDIF
      ENDIF

      FERMI_SHIFT  = VACPOT
      ! Regulate FERMI_SHIFT: VASPSol sometime report ths number wrong.
      IF ( TPOT%ITER > 0 ) THEN
          IF ( abs( FERMI_SHIFT - TPOT%FERMI_SHIFT) > TPOT%DFERMI_SHIFTLIM .AND. abs( FERMI_SHIFT ) < 1.d-4 ) THEN
              IF (IO%IU6>=0) WRITE( IO%IU6, 9001 ) 'FERMI_SHIFT', FERMI_SHIFT, TPOT%FERMI_SHIFT, 'FERMI_SHIFT', TPOT%FERMI_SHIFT
              FERMI_SHIFT = TPOT%FERMI_SHIFT
          ENDIF
      ENDIF
      TPOT%FERMI_SHIFT = FERMI_SHIFT

      VACLEVEL = VACLEVEL_TPOT
      ! Regulate VACUUM LEVEL: VASPSol sometime report ths number wrong.
      IF ( TPOT%ITER > 0 ) THEN
          IF ( abs( VACLEVEL - TPOT%VACLEVEL) > TPOT%DFERMI_SHIFTLIM .AND. abs( VACLEVEL ) < 1.d-4 ) THEN
              IF (IO%IU6>=0) WRITE( IO%IU6, 9001 ) 'VACLEVEL', VACLEVEL, TPOT%VACLEVEL, 'VACLEVEL', TPOT%VACLEVEL
              VACLEVEL = TPOT%VACLEVEL
          ENDIF
      ENDIF

      TPOT%VACLEVEL    = VACLEVEL
      IF ( TPOT%TRUEVACLEVEL ) THEN
          V = -( EFERMI - VACLEVEL )
          VFORMULA = ' V=-(EFERMI-VACLEVEL)/e'
      ELSE
          V = -( EFERMI + FERMI_SHIFT )
          VFORMULA = ' V=-(EFERMI+FERMI_SHIFT)/e'
      ENDIF

      NELECT           = INFO%NELECT

      ! Calculate rate change
      !IF(IO%IU6>=0) WRITE( IO%IU6, * )'1', ABS( V - TPOT%VTARGET ), TPOT%VDIFF, TPOT%ITER, TPOT%VRATELIM 
      IF ( ABS( V - TPOT%VTARGET ) > TPOT%VDIFF ) THEN
          IF ( TPOT%ITER > 0 .AND. ABS( INFO%NELECT - TPOT%NELECT0) > TPOT%DNELECTLIM ) &
              TPOT%VRATE = ( V - TPOT%V0 ) / ( INFO%NELECT - TPOT%NELECT0) * TPOT%VRATEDAMP
          !IF(IO%IU6>=0) WRITE( IO%IU6, * ) '2', TPOT%VRATE
          IF ( ABS( TPOT%VRATE ) < TPOT%VRATELIM ) &
              TPOT%VRATE = TPOT%VRATE / ABS( TPOT%VRATE ) * TPOT%VRATELIM
          !IF(IO%IU6>=0) WRITE( IO%IU6, * ) '3', TPOT%VRATE
      ENDIF 
    
      IF ( TPOT%METHOD == 1 ) THEN
          METHOD = 'Electronic Steps'
          IF ( ABS( V - TPOT%VTARGET ) > TPOT%VDIFF ) INFO%LABORT = .FALSE.
          IF(ABS(DESUM_)<TPOT%VEDIFF.OR.ABS(DESUM1_)<TPOT%VEDIFF) THEN
              IF ( ABS( V - TPOT%VTARGET ) > TPOT%VDIFF ) THEN
                  ! Updated with dynamic rate
                  !IF(IO%IU6>=0) WRITE( IO%IU6, * ) '3', TPOT%DYNVRATE
                  IF ( TPOT%DYNVRATE ) THEN
                      DNELECT = ( TPOT%VTARGET - V ) / TPOT%VRATE 
                      IF ( ABS( DNELECT ) > ABS( TPOT%ELECTSTEP ) ) DNELECT = DNELECT / ABS( DNELECT ) * ABS( TPOT%ELECTSTEP )
                      NELECT  = DNELECT + INFO%NELECT
                  
                  ! Updated with fix step
                  ELSE IF ( V > TPOT%VTARGET ) THEN
                      NELECT = INFO%NELECT + TPOT%ELECTSTEP
                  ELSE
                      NELECT = INFO%NELECT - TPOT%ELECTSTEP
                  ENDIF
                  TPOT%V0  = V
                  TPOT%NELECT0 = INFO%NELECT
                  TPOT%ITER    = TPOT%ITER + 1
              ENDIF
          ENDIF
          IF (IO%IU6>=0) WRITE( IO%IU6, 9002 )  FERMI_SHIFT, EFERMI, INFO%NELECT, V, TPOT%VTARGET, TPOT%METHOD, METHOD, &
                                                  &TPOT%VEDIFF, TPOT%VDIFF, TPOT%VRATE, TPOT%VRATELIM, TPOT%VRATEDAMP, TPOT%DYNVRATE, TPOT%ELECTSTEP, &
                                                  &NELECT, TPOT%GCENERGY, TPOT%GCIONIC !, TPOT%V0, TPOT%NELECT0

          INFO%NELECT = NELECT

      ELSEIF ( TPOT%METHOD == 2 ) THEN
          METHOD = 'Ionic Steps'
          IF ( INFO%LABORT .AND. ABS( V - TPOT%VTARGET ) > TPOT%VDIFF ) THEN
              ! Updated with dynamic rate
              IF ( TPOT%DYNVRATE ) THEN
                      DNELECT = ( TPOT%VTARGET - V ) / TPOT%VRATE 
                      IF ( ABS( DNELECT ) > ABS( TPOT%ELECTSTEP ) ) DNELECT = DNELECT / ABS( DNELECT ) * ABS( TPOT%ELECTSTEP )
                      NELECT  = DNELECT + INFO%NELECT

              ! Updated with fix step
              ELSE IF ( V > TPOT%VTARGET ) THEN
                  NELECT = INFO%NELECT + TPOT%ELECTSTEP
              ELSE
                  NELECT = INFO%NELECT - TPOT%ELECTSTEP
              ENDIF
              TPOT%V0  = V
              TPOT%NELECT0 = INFO%NELECT
              TPOT%ITER    = TPOT%ITER + 1

          ENDIF
          IF ( INFO%LABORT ) THEN
              IF (IO%IU6>=0) WRITE( IO%IU6, 9002 ) FERMI_SHIFT, VACLEVEL, EFERMI, INFO%NELECT, TPOT%TRUEVACLEVEL, V, VFORMULA, TPOT%VTARGET,&
                                                  &TPOT%METHOD, METHOD, &
                                                  &TPOT%VEDIFF, TPOT%VDIFF, TPOT%VRATE, TPOT%VRATELIM, TPOT%VRATEDAMP, TPOT%DYNVRATE, TPOT%ELECTSTEP, &
                                                  &NELECT, TPOT%GCENERGY, TPOT%GCIONIC !, TPOT%V0, TPOT%NELECT0
          ENDIF
          INFO%NELECT = NELECT

      ELSE
          METHOD = 'NOT SUPORTED'
          IF (IO%IU6>=0) WRITE( IO%IU6, 9003 ) TPOT%METHOD
          NELECT = INFO%NELECT
          TPOT%V0  = V
          TPOT%NELECT0 = INFO%NELECT
          TPOT%ITER    = TPOT%ITER + 1

          IF ( INFO%LABORT ) THEN
              IF (IO%IU6>=0) WRITE( IO%IU6, 9002 ) FERMI_SHIFT, VACLEVEL, EFERMI, INFO%NELECT, TPOT%TRUEVACLEVEL, V, VFORMULA,TPOT%VTARGET,&
                                                  &TPOT%METHOD, METHOD, &
                                                  &TPOT%VEDIFF, TPOT%VDIFF, TPOT%VRATE, TPOT%VRATELIM, TPOT%VRATEDAMP, TPOT%DYNVRATE, TPOT%ELECTSTEP, &
                                                  &NELECT, TPOT%GCENERGY, TPOT%GCIONIC !, TPOT%V0, TPOT%NELECT0
          ENDIF
      ENDIF

9001 FORMAT('     TPOT:',A15,' IS VERY DIFFERENT FROM THE LAST ITERATION'/&
           &'     ', F10.5,' vs ', F10.5,' eV'/&
           &      'SETTING ',A15,' = ', F10.5, 'eV')

9002 FORMAT( '    ====================================='/ &
            &'    TARGET POTENTIAL ROUTINE'/ &
            &'    FERMI_SHIFT         =', F16.12,' eV'/ &
            &'    VACLEVEL            =', F16.12,' eV'/ &
            &'    EFERMI              =', F16.12,' eV'/ &
            &'    CURRENT NELECT      =', F16.5, ' electrons'/    &
            &'    TRUEVACLEVEL        =', L16,''/  &
            &'    CURRENT POTENTIAL   =', F16.12,' V',A30/  &
            &'    TARGET POT. VTARGET =', F16.12,' V'/  &
            &'    TARGET POT. METHOD  =', I16,' ',A20,''/  &
            &'    ENERGY THRES.VEDIFF =', F16.12,' eV'/  &
            &'    POT. THRES. VDIFF   =', F16.12,' V'/  &
            &'    VRATE               =', F16.5,' V/electron'/  &
            &'    VRATELIM            =', F16.5,' V/electron'/  &
            &'    VRATEDAMP           =', F16.5,' '/  &
            &'    DYNVRATE            =', L16,''/  &
            &'    ELECTSTEP           =', F16.12,' electrons/step'/  &
            &'    UPDATED NELECT      =', F16.5, ' electrons'/   &
            &'    GCENERGY            =', L16,''/  &
            &'    GCIONIC             =', L16,''/  &
!            &'    V0                  =', F16.5, ' V'/   &
!            &'    NELECT0             =', F16.5, ' electrons'/   &
            &'    =====================================') 

9003 FORMAT('     TPOT: METHOD = ',I5,' IS NOT SUPPORTED' )
9004 FORMAT('     TPOT: LSOL = .FALSE. IS NOT SUPPORTED' )
9005 FORMAT('     TPOT: LSOL = ',L1,' LTPOT = ',L1,' SETTING METHOD = ',I3 )

9006  CONTINUE

 END SUBROUTINE

 SUBROUTINE APPLY_PENPOT( IO, DYN, LATT_CUR, T_INFO, TIFOR )
     TYPE (in_struct)       :: IO
     TYPE(type_info)        :: T_INFO
     TYPE(dynamics)         :: DYN
     TYPE(latt)             :: LATT_CUR
     REAL(q), INTENT(INOUT) :: TIFOR(3,T_INFO%NIONS)
     INTEGER                :: I, J, K
     REAL(q)                :: EPENALTY, FPENALTY, DZ, DZW
     REAL(q), PARAMETER     :: log10e = 0.4342944819032518

     IF (IO%IU6>=0) WRITE(IO%IU6,9007)PENPOT%LPENPOT, PENPOT%H, PENPOT%W, PENPOT%Z0, PENPOT%ZMIN
     IF ( .NOT. PENPOT%LPENPOT ) THEN
         IF (IO%IU6>=0) WRITE(IO%IU6,9008)
         GOTO 9011
     ENDIF

     EPENALTY = 0.
     DO I=1,T_INFO%NIONS
         IF ( DYN%POSION(3,I) > PENPOT%ZMIN ) THEN
             DZ = DYN%POSION(3,I) - PENPOT%Z0
             DZW = DZ/PENPOT%W
             FPENALTY = -PENPOT%H*log10e/(PENPOT%W*(1+EXP(DZW)))*EXP(DZW)
             EPENALTY = EPENALTY + PENPOT%H * LOG10(1+EXP(DZW))
             TIFOR(3,I) = TIFOR(3,I) + FPENALTY
             IF (IO%IU6>=0) WRITE(IO%IU6,9009)I, FPENALTY
         END IF
     ENDDO
     IF (IO%IU6>=0) WRITE(IO%IU6,9010)EPENALTY

9007 FORMAT( '    ====================================='/ &
            &'    PENALTY POTENTIAL ROUTINE'/ &
            &'    LPENPOT             =', L16,''/ &
            &'    PENPOT_H            =', F16.12,' eV'/ &
            &'    PENPOT_W            =', F16.12,' Crystal Coordinate'/ &
            &'    PENPOT_Z0           =', F16.12,' Crystal Coordinate, Separation between IMPLICIT and EXPLICIT'/ &
            &'    PENPOT_ZMIN         =', F16.12,' Crystal Coordinate, Starting point for applying penalty potential'/ &
            &'    =====================================')

9008 FORMAT('     TPOT: NO PENALTY POTENTIAL' )
9009 FORMAT('     TPOT: PENALTY FORCE TO ATOM ',I5,':',F16.12,' eV/Angstrom (Included in total force)')
9010 FORMAT('     TPOT: PENALTY ENERGY = ',F16.12,' eV (Not yet included in TOTEN)')
    
9011 CONTINUE

 END SUBROUTINE

 SUBROUTINE CAL_GC_COOR( IO, P, INFO, T_INFO, GC_COOR, EFERMI, LGCIONIC )
   USE pseudo, ONLY: potcar
   IMPLICIT NONE
   ! Calculate -(NELECT-NELECT0)*EF

   TYPE (in_struct), INTENT(IN)   :: IO
   TYPE(type_info), INTENT(IN)    :: T_INFO
   TYPE (info_struct), INTENT(IN) :: INFO
   TYPE(potcar),INTENT(IN)        :: P(T_INFO%NTYP)
   REAL(q), INTENT(INOUT)         :: GC_COOR
   REAL(q), INTENT(IN)            :: EFERMI
   LOGICAL, INTENT(IN)            :: LGCIONIC

   INTEGER :: NI, NT, NIS


   IF (.NOT. TPOT%GCENERGY) GOTO 9012
   IF (TPOT%GCIONIC .AND. .NOT. LGCIONIC ) GOTO 9012
   IF (.NOT. TPOT%GCIONIC .AND. LGCIONIC ) GOTO 9012
   IF (TPOT%ZVALS < 1.d-4 ) THEN
     TPOT%ZVALS = 0.0
     NIS = 1
     type:DO NT=1,T_INFO%NTYP
       ions: DO NI=NIS,T_INFO%NITYP(NT)+NIS-1
         TPOT%ZVALS =  TPOT%ZVALS+ P(NT)%ZVALF
       ENDDO ions
     ENDDO type
   ENDIF

   GC_COOR = -( INFO%NELECT - TPOT%ZVALS ) * EFERMI
   !IF (IO%IU6>=0)  WRITE(IO%IU0,*) 'ZVALS', TPOT%ZVALS
   !IF (IO%IU6>=0)  WRITE(IO%IU0,*) 'NELECT', INFO%NELECT
   !IF (IO%IU6>=0)  WRITE(IO%IU0,*) 'EFERMI', EFERMI

9012 CONTINUE

 END SUBROUTINE

 
! vaclevel
!>==========================SUBROUTINE GET_VACUUMLEVEL=================
!!
!! compute the fermi shift
!!
!!=====================================================================
SUBROUTINE GET_VACUUMLEVEL(LATT_CUR, T_INFO, GRIDC, WDES, CVPSP, CHTOT)

  USE poscar, ONLY : type_info
  USE lattice, ONLY : latt, DIRKAR
  USE mgrid, ONLY : grid_3d
  USE pseudo, ONLY : potcar
  USE wave, ONLY : wavedes

  IMPLICIT NONE

  TYPE(type_info),INTENT(IN) :: T_INFO
  TYPE (latt),INTENT(IN) :: LATT_CUR
  TYPE (grid_3d) :: GRIDC
  TYPE (wavedes) :: WDES

  INTEGER(q) :: I
  COMPLEX(q) :: CVPSP(GRIDC%MPLWV,WDES%NCDIJ)
  COMPLEX(q) :: CHTOT(GRIDC%MPLWV, WDES%NCDIJ)

  RGRID, ALLOCATABLE :: RCHTOT(:), RV(:)
  COMPLEX(q), ALLOCATABLE::  CWORK(:), CWORK_V(:), CVHAR(:), CV(:)
!  REAL(q) :: VACPOT_PSP, VACPOT

  INTEGER :: NODE_ME, IONODE

  ALLOCATE(CWORK(GRIDC%MPLWV), CWORK_V(GRIDC%MPLWV) )
  ALLOCATE(CVHAR(GRIDC%MPLWV), CV(GRIDC%MPLWV))
  ALLOCATE(RCHTOT(DIMREAL(GRIDC%MPLWV)), RV(DIMREAL(GRIDC%MPLWV)))

  CWORK = 0._q
  CWORK_V = 0._q
  CVHAR = 0._q
  CV = 0._q
  RV = 0._q
  RCHTOT = 0._q

  !copy CHTOT to CWORK
  CALL RC_ADD(CHTOT, 1.0_q, CHTOT, 0.0_q, CWORK, GRIDC)
  !charge density to real space, CWORK --> RCHTOT
  CALL FFT3D(CWORK, GRIDC, 1)
  CALL UNPACK_C2R(CWORK, 1.0_q, CWORK, 0.0_q, RCHTOT, GRIDC)


  CV = 0._q
  RV = 0._q
  ! CVPSP --> real
  CALL RC_ADD(CVPSP, 1.0_q, CVPSP, 0.0_q, CV, GRIDC)
  CALL FFT3D(CV, GRIDC, 1)
  CALL UNPACK_C2R(CV, 1.0_q, CV, 0.0_q, RV, GRIDC)

  ! vacuum level
  CALL GET_VAC_LEVEL(GRIDC, RCHTOT, RV, VACLEVEL_TPOT)
  !IF (SP_IO%IU6>=0)  WRITE(SP_IO%IU0,*) ' VACUUM LEVEL (TPOT)= ', VACLEVEL_TPOT

  DEALLOCATE(RCHTOT, RV)
  DEALLOCATE(CWORK_V, CWORK, CVHAR, CV)

  RETURN

END SUBROUTINE GET_VACUUMLEVEL

END MODULE 
