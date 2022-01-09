# tpot

# Getting the code

The following is instruction for CPU code. The precedure is the same for GPU (i.e. files ended with _gpu.F)


:one: Admend electron.F

Add the following to electron.F, the code between ```Begin``` and ```End``` ```target potential```


```
! solvation__
      USE solvation
!------ Begin target potential
      USE targetpot
!------ End target potential
! solvation__
```

```Fortran
        IF(INFO%IHARMONIC==1)THEN
           CALL WRITE_EIGENVAL_RESIDUAL( WDES, W, IO%IU6)
        ELSE
           CALL WRITE_EIGENVAL( WDES, W, IO%IU6)
        END IF
      io_end
      ENDIF


!------ Begin target potential
      CALL UPDATE_NELECT( IO, INFO, EFERMI, DESUM(N), DESUM1 ) 
!------ End  target potential

      IF (((NSTEP==1 .OR. NSTEP==DYN%NSW).AND.INFO%LABORT).OR. &
     &     (IO%NWRITE>=1 .AND.INFO%LABORT).OR.IO%NWRITE>=3) THEN
      io_begin
!-----Charge-density along one line
```

:two: Admend .objects with ```targetpot.o```, right after ```solvation.o```
```
        solvation.o \
        targetpot.o \
        pot.o \
```

:three: Update VASPSol

There is a small modification to ```VASPSol/src/solvation.F```. 

Download from: https://github.com/zoowe/VASPsol/tree/tpot/src 

Or modify your current ```solvation.F```, at the begining of ```MODULE POT_K```
```
 LOGICAL, SAVE :: LDEFAULTPCM = .FALSE.
 LOGICAL, SAVE :: LJDFTX = .FALSE.
!------ Begin Target Potential
 REAL(q), PUBLIC, SAVE :: VACPOT_PSP, VACPOT
!------ End Target Potential
 CONTAINS
```
and in ```SUBROUTINE GET_FERMISHIFT```
```
  COMPLEX(q), ALLOCATABLE::  CWORK(:), CWORK_V(:), CVHAR(:), CV(:) 
!  REAL(q) :: VACPOT_PSP, VACPOT
  INTEGER :: NODE_ME, IONODE
```
:four: Copy main source code
Copy ```src/targetpot.F``` to vasp ```src``` folder

:four: Recompile your vasp code

# Running the code

The following are available keyword to add in ```INCAR```
```
LTPOT               = .FALSE.  # Turn on/off TPOT
TPOTMETHOD          = 1        # 1 electronics, 2 ionic
TPOTVTARGET         = 3.44     # Target potential
TPOTVDIFF           = 0.01     # Rotential Threshold
TPOTVRATE           = -1.0     # Initial Rate of changing NELECT, V/electron
TPOTVRATELIM        = 0.2      # Limit for |TPOTVRATE|, V/electron
TPOTVRATEDAMP       = 1.0      # Damping factor for |TPOTVRATE|
TPOTVEDIFF          = 1.d-4    # Energy threshold of electronic iteration to start updating NELECT
TPOTDYNVRATE        = .TRUE.   # Updating NELECT with VRATE
TPOTELECTSTEP       = 0.01     # (Maximum) Amount of electrons changed.
TPOTDFERMI_SHIFTLIM = 0.5      # Limit for change in FERMI_SHIFT
TPOTDNELECTLIM      = 1.d-4    # Limit for amount of electron changed for updating VRATE.

```

There are currently two methods for updating ```NELECT```. 

:one: Electronic step

```TPOTMETHOD    = 1```

Number of electrons is updated at every electronic step to reach target potential. This method could be expensive.

:two: Ionic step

```TPOTMETHOD    = 2```

Number of electrons is updated at the end of each ionic step if current potential is not close to the target potential. 


In both methods, it is benificial to start calculation with a reasonable ```NELECT``` for your target potential (Use ```ISTART = 1```). It can be estimated by doing a series of SCF calculations with different ```NELECT```. 

## Details of each keyword

### LPOT

LOGICAL, DEFAULT: .FALSE.

This keywork turns on and off target potential routine.

### TPOTMETHOD

INTEGER, DEFAULT: 2

1: Updating NELECT for each SCF cycle. 
2. Updating NELECT for each ionic iteration.

### TPOTVTARGET

REAL, DEFAULT: 3.44 (V)

This keywork defines the desire target potential. Note that, it is the potential of electrode with respect to vacuum level. 

### TPOTVDIFF

REAL, DEFAULT: 0.01

This keywork defines a threshold for optimized potential, i.e. current potential will be converged within ```TPOTVDIFF``` V from the target potential ```TPOTVTARGET```.

### TPOTVRATE

REAL, DEFAULT: -1.0 (V/electron)

This keywork controls the initial rate for changing ```NELECT``` to reach target potential. It will be updated after each iteration.

### TPOTVRATELIM

REAL, DEFAULT: 0.2 (V/electron)  

It defines the lower limit of ```TPOTVRATE```, i.e. if ```|TPOTVRATE| < TPOTVRATELIM, TPOTVRATE = TPOTVRATE/|TPOTVRATE| * TPOTVRATELIM```

### TPOTVRATEDAMP

REAL, DEFAULT: 1.0

Damping factor for TPOTVRATE. It changes how agressive ```NELECT``` to be updated, i.e. ```TPOTVRATE = TPOTVRATE * TPOTVRATEDAMP```

### TPOTVEDIFF

REAL, DEFAULT: 1.d-4 eV

For ```TPOTMETHOD = 1```, ```NELECT``` is updated only if energy converge to ```TPOTVEDIFF```. It should be set to a value smaller than ```EDIFF```.

### TPOTDYNVRATE

LOGICAL, DEFAULT: TRUE

It controls how ```NELECT``` is updated. If it is ```.TRUE.```, ```NELECT``` is update with ```TPOTVRATE```, otherwise, it is updated by an increasment ```TPOTELECTSTEP```.

### TPOTELECTSTEP

REAL, DEFAULT: 0.01 electrons

Increasement for updating ```NELECT``` when ```TPOTDYNVRATE = .FALSE.```. When ```TPOTDYNVRATE = .TRUE.```, this is the maximum amount of electron changed at every updating step.

### TDFERMI_SHIFTLIM

REAL, DEFAULT: 0.5

Sometime, VASPSol could not calculate FERMI_SHIFT (reported at 0.0 or any non-reasonable number). If FERMI_SHIFT differs with the value calculated in previous step by ```TDFERMI_SHIFTLIM```, ```TPOT``` will use the previous value.

### TPOTDNELECTLIM

REAL, DEFAULT: 1.d-4 electron

To avoid divergence, if the difference in ```NELECT``` between two consecutive steps is smaller than ```TPOTDNELECTLIM```, ```TPOTVRATE``` is not updated.

## Examples

### Charging electrode to reach a target potential

One can do a self consistent (SCF) cycle with updating ```NELECT``` to reach the target potential. The following keywords are needed to charge electrode to a potential of 3.44 V (i.e. -1 V vs. SHE) within 0.01 V accuracy.

```
#VASPSol keywords are omitted.
IBRION = -1
NSW    = 1
NELM   = 200    # Yes, it may need a lot of iterations

LTPOT = .TRUE.
TPOTMETHOD      = 1
TPOTVTARGET     = 3.44
TPOTVDIFF       = 0.01
TPOTVRATE       = -1.
TPOTVRATELIM    = 0.2
TPOTVRATEDAMP   = 2
TPOTVEDIFF      = 0.0001
TPOTDYNVRATE    = .TRUE.
TPOTELECTSTEP   = 0.05    
```

Or, one can do the following
```
#VASPSol keywords are omitted.
EDIFFG = 1.d-4
IBRION = -1
NSW    = 20
NELM   = 60 

LTPOT = .TRUE.
TPOTMETHOD      = 2
TPOTVTARGET     = 3.44
TPOTVDIFF       = 0.01
TPOTVRATE       = -1.
TPOTVRATELIM    = 0.2
TPOTVRATEDAMP   = 1        # Can be done agreesively 
TPOTVEDIFF      = 0.0001
TPOTDYNVRATE    = .TRUE.
TPOTELECTSTEP   = 0.10
```

### Molecular dynamics at a target potential

It is important to start with a reasonable ```NELECT```. Use the above exmaple to find ```NELECT``` for a given target potential before doing molecular dynamics.

```
#VASPSol keywords are omitted.
IBRION =   0
NSW    =   2000  
#Other molecular dynamicss keywords are omiited

LTPOT = .TRUE.
TPOTMETHOD      = 2        # Avoid 1 for MD
TPOTVTARGET     = 3.44
TPOTVDIFF       = 0.01
TPOTVRATE       = -1.
TPOTVRATELIM    = 0.2
TPOTVRATEDAMP   = 1        
TPOTVEDIFF      = 0.0001
TPOTDYNVRATE    = .TRUE.
TPOTELECTSTEP   = 0.05     # Keep changing in ```NELECT``` is small.
```
