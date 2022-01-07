MODULE targetpot 

 USE prec
 USE solvation
 USE pot_k

 TYPE info_tpot
 LOGICAL :: LTPOT   = .FALSE. ! Target potential on/off
 LOGICAL :: READY   = .FALSE. ! IF INCAR IS READ
 INTEGER :: METHOD  = 1       ! Method 1 == Scaling, 2 == Step
 REAL(q) :: VDIFF   = 0.25     ! Threshold for target potential
 REAL(q) :: VTARGET = 3.44    ! Target potential
 REAL(q) :: VRATE   = 2.     ! dV/dnelect
 REAL(q) :: EDIFF   = 0.0001     ! dV/dnelect
 LOGICAL :: DYNVRATE  = .TRUE.     ! Dynamic VRATE
 REAL(q) :: ELECTSTEP = 0.01    ! n electron change per step (METHOD = 2 )
 REAL(q) :: NELECT0   = 0     ! Last Number of electron 
 REAL(q) :: V0        = 0     ! Last Potential 

 END TYPE

 
 TYPE (info_tpot ) :: TPOT
 PUBLIC :: UPDATE_NELECT, TPOT
 
 CONTAINS

 SUBROUTINE UPDATE_NELECT( IO, INFO, EFERMI, DESUM_, DESUM1_)

      IMPLICIT NONE

      TYPE (in_struct), INTENT(IN) :: IO
      TYPE (info_struct), INTENT(INOUT) :: INFO
      REAL(q), INTENT(IN)    :: EFERMI, DESUM_, DESUM1_ 
      REAL(q) :: FERMI_SHIFT, V, NELECT


      INTEGER IDUM,N,IERR
      LOGICAL :: LOPEN,LDUM
      REAL(q) :: RDUM
      COMPLEX(q) :: CDUM
      CHARACTER*1 :: CHARAC

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
               IF ((IERR/=0).OR.(N<1)) TPOT%VDIFF= 0.25

               CALL RDATAB(LOPEN,'INCAR',IO%IU5,'TPOTVRATE','=','#',';','F', &
                IDUM,TPOT%VRATE,CDUM,LDUM,CHARAC,N,1,IERR)
               IF ((IERR/=0).OR.(N<1)) TPOT%VRATE= 2

               CALL RDATAB(LOPEN,'INCAR',IO%IU5,'TOPTVEDIFF','=','#',';','F', &
                IDUM,TPOT%EDIFF,CDUM,LDUM,CHARAC,N,1,IERR)
               IF ((IERR/=0).OR.(N<1)) TPOT%EDIFF= 0.0001

               CALL RDATAB(LOPEN,'INCAR',IO%IU5,'TPOTDYNVRATE','=','#',';','L', &
                IDUM,RDUM,CDUM,TPOT%DYNVRATE,CHARAC,N,1,IERR)
               IF ((IERR/=0).OR.(N<1)) TPOT%DYNVRATE= .FALSE.

               CALL RDATAB(LOPEN,'INCAR',IO%IU5,'TOPTELECTSTEP','=','#',';','F', &
                IDUM,TPOT%ELECTSTEP,CDUM,LDUM,CHARAC,N,1,IERR)
               IF ((IERR/=0).OR.(N<1)) TPOT%ELECTSTEP= 0.01

          ENDIF
          TPOT%READY   = .TRUE.
          TPOT%NELECT0 = INFO%NELECT
          TPOT%V0      = 10000
      ENDIF

      FERMI_SHIFT  = VACPOT
      V            = -EFERMI - FERMI_SHIFT
      NELECT       = INFO%NELECT

      IF ( TPOT%METHOD == 1 ) THEN
          IF(ABS(DESUM_)<TPOT%EDIFF.OR.ABS(DESUM1_)<TPOT%EDIFF) THEN
              IF ( ABS( V - TPOT%VTARGET ) > TPOT%VDIFF ) THEN
                  IF ( TPOT%V0 < 9999 .AND. ABS( INFO%NELECT - TPOT%NELECT0) > 0.1 .AND. TPOT%DYNVRATE) &
                      TPOT%VRATE = ( V - TPOT%V0 ) / ( INFO%NELECT - TPOT%NELECT0)

                  NELECT = ( V - TPOT%VTARGET ) / TPOT%VRATE + INFO%NELECT
                  TPOT%V0  = V
                  TPOT%NELECT0 = INFO%NELECT
              ENDIF
          ENDIF
          IF (IO%IU6>=0) WRITE( IO%IU6, 9001 ) FERMI_SHIFT, EFERMI, INFO%NELECT, V, TPOT%VTARGET, TPOT%METHOD, TPOT%EDIFF, TPOT%VDIFF, TPOT%VRATE, NELECT
          INFO%NELECT = NELECT

      ELSEIF ( TPOT%METHOD == 2 ) THEN
          IF ( INFO%LABORT .AND. ABS( V - TPOT%VTARGET ) > TPOT%VDIFF ) THEN
              IF ( V > TPOT%VTARGET ) THEN
                  NELECT = INFO%NELECT + TPOT%ELECTSTEP
              ELSE 
                  NELECT = INFO%NELECT - TPOT%ELECTSTEP
              ENDIF
          ENDIF
          IF ( INFO%LABORT ) THEN
          IF (IO%IU6>=0) WRITE( IO%IU6, 9002 ) FERMI_SHIFT, EFERMI, INFO%NELECT, V, TPOT%VTARGET, TPOT%METHOD, TPOT%VDIFF, TPOT%ELECTSTEP, NELECT
          ENDIF
          INFO%NELECT = NELECT

      ENDIF 


9001 FORMAT( '    ====================================='/ &
            &'    TARGET POTENTIAL ROUTINE'/ &
            &'    FERMI_SHIFT         =', F16.12,' eV'/ &
            &'    EFERMI              =', F16.12,' eV'/ &
            &'    CURRENT NELECT      =', F16.5, ' electrons'/    &
            &'    CURRENT POTENTIAL   =', F16.12,' V'/  &
            &'    TARGET POT. VTARGET =', F16.12,' V'/  &
            &'    TARGET POT. METHOD  =', I16,' Electronic steps'/  &
            &'    ENERGY THRES. EDIFF =', F16.12,' eV'/  &
            &'    POT. THRES. VDIFF   =', F16.12,' V'/  &
            &'    VRATE               =', F16.5,' V/electron'/  &
            &'    UPDATED NELECT      =', F16.5, ' electrons'/   &
            &'    =====================================') 

9002 FORMAT( '    ====================================='/ &
            &'    TARGET POTENTIAL ROUTINE'/ &
            &'    FERMI_SHIFT         =', F16.12,' eV'/ &
            &'    EFERMI              =', F16.12,' eV'/ &
            &'    CURRENT NELECT      =', F16.5, ' electrons'/    &
            &'    CURRENT POTENTIAL   =', F16.12,' V'/  &
            &'    TARGET POT. VTARGET =', F16.12,' V'/  &
            &'    TARGET POT. METHOD  =', I16,' Ionic Steps'/  &
            &'    POT. THRES. VDIFF   =', F16.12,' V'/  &
            &'    ELECTSTEP           =', F16.12,' electrons/step'/  &
            &'    UPDATED NELECT      =', F16.5, ' electrons'/   &
            &'    =====================================')


 END SUBROUTINE
END MODULE 
