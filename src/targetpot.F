MODULE targetpot 

 USE prec
 USE solvation
 USE pot_k

 TYPE info_tpot
 LOGICAL :: LTPOT           = .FALSE.  ! Target potential on/off
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
 REAL(q) :: DFERMI_SHIFTLIM = 0.5      ! Limit for change in FERMI_SHIFT
 REAL(q) :: DNELECTLIM      = 1.d-4    ! Limit for amount of electron changed for updating VRATE
 INTEGER :: ITER            = 0        ! Iteration
 END TYPE

 TYPE (info_tpot ) :: TPOT
 PUBLIC :: UPDATE_NELECT, TPOT
 
 CONTAINS

 SUBROUTINE UPDATE_NELECT( IO, INFO, EFERMI, DESUM_, DESUM1_)

      IMPLICIT NONE

      TYPE (in_struct), INTENT(IN) :: IO
      TYPE (info_struct), INTENT(INOUT) :: INFO
      REAL(q), INTENT(IN)    :: EFERMI, DESUM_, DESUM1_ 
      REAL(q) :: FERMI_SHIFT, V, NELECT, DNELECT


      INTEGER IDUM,N,IERR
      LOGICAL :: LOPEN,LDUM
      REAL(q) :: RDUM
      COMPLEX(q) :: CDUM
      CHARACTER*1 :: CHARAC
      CHARACTER*20 :: METHOD
 

      IF (TPOT%READY == .FALSE.) THEN
          LOPEN=.FALSE.
          OPEN(UNIT=IO%IU5,FILE='INCAR',STATUS='OLD')
          CALL RDATAB(LOPEN,'INCAR',IO%IU5,'LTPOT','=','#',';','L', &
                IDUM,RDUM,CDUM,TPOT%LTPOT,CHARAC,N,1,IERR)
          IF ((IERR/=0).OR.(N<1)) TPOT%LTPOT=.FALSE.
          IF (TPOT%LTPOT == .TRUE. ) THEN

               CALL RDATAB(LOPEN,'INCAR',IO%IU5,'TPOTMETHOD','=','#',';','I', &
                TPOT%METHOD,RDUM,CDUM,LDUM,CHARAC,N,1,IERR)
               IF ((IERR/=0).OR.(N<1)) TPOT%METHOD= 1

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

          ENDIF
          TPOT%READY   = .TRUE.
          TPOT%NELECT0 = INFO%NELECT
          TPOT%V0      = 10000
      ENDIF

      IF ( LSOL == .FALSE. ) THEN
          IF (IO%IU6>=0) WRITE( IO%IU6, 9004 )
          TPOT%LTPOT = .FALSE.
          GOTO 9006 
      ENDIF

      IF ( LSOL .AND. TPOT%LTPOT == .FALSE. .AND. TPOT%ITER == 0 ) THEN
          TPOT%METHOD = 3
          IF (IO%IU6>=0) WRITE( IO%IU6, 9005 ) LSOL, TPOT%LTPOT, TPOT%METHOD
      ENDIF

      FERMI_SHIFT  = VACPOT
      ! Regulate FERMI_SHIFT: VASPSol sometime report ths number wrong.
      IF ( TPOT%ITER > 0 ) THEN
          IF ( abs( FERMI_SHIFT - TPOT%FERMI_SHIFT) > TPOT%DFERMI_SHIFTLIM .AND. abs( FERMI_SHIFT ) < 1.d-4 ) THEN
              IF (IO%IU6>=0) WRITE( IO%IU6, 9001 ) FERMI_SHIFT, TPOT%FERMI_SHIFT, TPOT%FERMI_SHIFT
              FERMI_SHIFT = TPOT%FERMI_SHIFT
          ENDIF
      ENDIF
      TPOT%FERMI_SHIFT = FERMI_SHIFT
      V            = -EFERMI - FERMI_SHIFT
      NELECT       = INFO%NELECT

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
                                                  &NELECT !, TPOT%V0, TPOT%NELECT0

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
              IF (IO%IU6>=0) WRITE( IO%IU6, 9002 ) FERMI_SHIFT, EFERMI, INFO%NELECT, V, TPOT%VTARGET, TPOT%METHOD, METHOD, &
                                                  &TPOT%VEDIFF, TPOT%VDIFF, TPOT%VRATE, TPOT%VRATELIM, TPOT%VRATEDAMP, TPOT%DYNVRATE, TPOT%ELECTSTEP, &
                                                  &NELECT !, TPOT%V0, TPOT%NELECT0
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
              IF (IO%IU6>=0) WRITE( IO%IU6, 9002 ) FERMI_SHIFT, EFERMI, INFO%NELECT, V, TPOT%VTARGET, TPOT%METHOD, METHOD, &
                                                  &TPOT%VEDIFF, TPOT%VDIFF, TPOT%VRATE, TPOT%VRATELIM, TPOT%VRATEDAMP, TPOT%DYNVRATE, TPOT%ELECTSTEP, &
                                                  &NELECT !, TPOT%V0, TPOT%NELECT0
          ENDIF
      ENDIF

9001 FORMAT('     TPOT: FERMI_SHIFT IS VERY DIFFERENT FROM THE LAST ITERATION'/&
           &'     ', F10.5,' vs ', F10.5,' eV'/&
           &      'SETTING FERMI_SHIFT = ', F10.5, 'eV')

9002 FORMAT( '    ====================================='/ &
            &'    TARGET POTENTIAL ROUTINE'/ &
            &'    FERMI_SHIFT         =', F16.12,' eV'/ &
            &'    EFERMI              =', F16.12,' eV'/ &
            &'    CURRENT NELECT      =', F16.5, ' electrons'/    &
            &'    CURRENT POTENTIAL   =', F16.12,' V [= -(EFERMI + FERMI_SHIFT)] '/  &
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
!            &'    V0                  =', F16.5, ' V'/   &
!            &'    NELECT0             =', F16.5, ' electrons'/   &
            &'    =====================================') 

9003 FORMAT('     TPOT: METHOD = ',I5,' IS NOT SUPPORTED' )
9004 FORMAT('     TPOT: LSOL = .FALSE. IS NOT SUPPORTED' )
9005 FORMAT('     TPOT: LSOL = ',L1,' LTPOT = ',L1,' SETTING METHOD = ',I3 )

9006  CONTINUE

 END SUBROUTINE
END MODULE 
