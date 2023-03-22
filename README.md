# tpot

```tpot``` is a ```Fortran``` code that if it is added to the [VASP code](https://www.vasp.at/) allows to perform Grand Canonical ensemble simulations with Density Functional theory by controlling number of electrons in the system (slab) so that its work function reaches a target value. It is helpful for target potential electrode simulation. It requires [VASPSol](https://github.com/zoowe/VASPsol/tree/tpot/src).

## Installation

:one: Make nessesary changes to source of VASP code

Follow the instruction for [vasp 5.4.4](docs/vaspcode)

:two: Update VASPSol

Download and follow instruction at https://github.com/zoowe/VASPsol/

:three: Copy main source code

Copy ```src/targetpot.F``` to vasp ```src``` folder

:four: Recompile your vasp code

## Running the code

Detailed instructions and examples are available at [TPOT's mannual](docs/tpot) 

