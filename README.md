# tpot

```tpot``` is a ```Fortran``` code that enables Grand Canonical simulations of liquid/solid interface with Density Functional theory by controlling number of electrons in the simulation supercell so that the work function reaches a target value. It runs with the [VASP code](https://www.vasp.at/) and [VASPSol](https://github.com/zoowe/VASPsol/tree/solhybrid) package.

## Installation

:one: Make nessesary changes to source of VASP code

Follow the instruction for [vasp 5.4.4](docs/vaspcode)

:two: Update VASPSol

Download and follow instruction at https://github.com/zoowe/VASPsol/tree/solhybrid

:three: Copy main source code

Copy ```src/targetpot.F``` to vasp ```src``` folder

:four: Recompile your vasp code

## Running the code

Detailed instructions and examples are available at [TPOT's mannual](docs/tpot) 

## Citation

If you use ```TPOT``` for your work, please cite the following paper:

- An Explicit-Implicit Hybrid Solvent Model for Grand Canonical Simulations of the Electrochemical Environment.
D. Le, ChemRxiv (2023) [doi: 10.26434/chemrxiv-2023-z2n4n](https://doi.org/10.26434/chemrxiv-2023-z2n4n)

