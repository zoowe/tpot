# tpot

```tpot``` is a ```Fortran``` code that if it is added to the [VASP code](https://www.vasp.at/) allows to perform Grand Canonical ensemble simulations with Density Functional theory by controlling number of electrons in the system (slab) so that its work function reaches a target value. It is helpful for target potential electrode simulation. It requires [VASPSol](https://github.com/zoowe/VASPsol/tree/tpot/src).

# Getting the code

The following is instruction for CPU code. The precedure is the same for GPU (i.e. files ended with _gpu.F)


:one: Make nessesary changes to source of VASP code

Follow the instruction here: [MODIFICATION_TO_VASP.md](MODIFICATION_TO_VASP.md)

:two: Update VASPSol

Download and follow instruction at https://github.com/zoowe/VASPsol/

:three: Copy main source code

Copy ```src/targetpot.F``` to vasp ```src``` folder

:four: Recompile your vasp code

# Running the code

The following are available keyword to add in ```INCAR```
```
TPOTTRUEVACLEVEL    = .TRUE.   # Use true vacuum level or FERMI_SHIFT
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
TPOTGCENERGY        = .TRUE.   # Calculate grand canonical energy
TPOTGCIONIC         = .TRUE.   # Calculate grand canonical energy only at the end of ionic iteration
```

There are currently two methods for updating ```NELECT```. 

:one: Electronic step

```TPOTMETHOD    = 1```

Number of electrons is updated at every electronic step to reach target potential. This method could be expensive.

:two: Ionic step

```TPOTMETHOD    = 2```

Number of electrons is updated at the end of each ionic step if current potential is not close to the target potential. 


In both methods, it is benificial to start calculation with a reasonable ```NELECT``` for your target potential (Use ```ISTART = 1```). It can be estimated by doing a series of SCF calculations with different ```NELECT```. 

# Details of each keyword

### LTPOT

LOGICAL, DEFAULT: .FALSE.

This keywork turns on and off target potential routine.

### TPOTMETHOD

INTEGER, DEFAULT: 2

1: Updating ```NELECT``` for each SCF cycle.

2. Updating ```NELECT``` for each ionic iteration.

### TPOTTRUEVACLEVEL

LOGICAL, DEFAULT: .TRUE.

If .TRUE., electrode potential is calculated as -(EFERMI-VACLEVEL)/e 
If .FALSE., electrode potential is calculated as -(EFERMI+FERMI_SHIFT)/e 

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

### TPOTDFERMI_SHIFTLIM

REAL, DEFAULT: 0.5 eV

Sometime, VASPSol could not calculate FERMI_SHIFT (reported at 0.0 or any non-reasonable number). If FERMI_SHIFT differs with the value calculated in previous step by ```TPOTDFERMI_SHIFTLIM```, ```TPOT``` will use the previous value.

### TPOTDNELECTLIM

REAL, DEFAULT: 1.d-4 electron

To avoid divergence, if the difference in ```NELECT``` between two consecutive steps is smaller than ```TPOTDNELECTLIM```, ```TPOTVRATE``` is not updated.

### TPOTGCENERGY

LOGICAL, DEFAULT: .TRUE.

Calculate grand canonical energy

### TPOTGCIONIC

LOGICAL, DEFAULT: .TRUE.

Calculate grand canonical energy only at the end of ionic iteration


# Examples

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

It is important to start with a reasonable ```NELECT```. Use the above example to find ```NELECT``` for a given target potential before doing molecular dynamics.

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
TPOTELECTSTEP   = 0.05     # Keep changing in number of electron small for stable dynamics.
```
