# Modification to VASP code 

Since we can not provide a batch for VASP (according to its licence agreement), we provide a list of changes that we need to make to ```electron.F```, ```pot.F```, ```main.F```, and ```.objects``` files.

These instruction below are exlicitly for vasp 5.4.4. For other version, users need to make appropriate corretions (expected to be similar). This [pbz_patch_610](https://github.com/zoowe/VASPsol/blob/master/src/patches/pbz_patch_610) patch needs to be applied as well for vasp 6.1.0 or newer.

:one: electron.F

Compare the followng blocks of the codes with those in ```electron.F``` to make nessesary modifications. There are changes between ```Begin tpot``` and ```End tpot```

```Fortran 
! solvation__
      USE solvation
!------ Begin tpot
      USE targetpot
!------ End tpot
! solvation__
```

```Fortran
      REAL(q) :: TIFOR(3,T_INFO%NIONS)
!------ Begin tpot
      REAL(q) :: GC_COOR  ! correction for Grand canonical DFT
!------ End tpot

      IONODE=0
```

```Fortran
      ENDIF
      DWRITE0 "densta is ok"
!------ Begin tpot
!=======================================================================
!Calculate CG_COOR = -(NELECT - NELECT0) * EFERMI
!=======================================================================
      GC_COOR = 0
      CALL CAL_GC_COOR( IO, P, INFO, T_INFO, GC_COOR, EFERMI, .FALSE. )
!------ End tpot
!=======================================================================
! calculate free-energy and bandstructur-energy
```
For vasp 5.4.4
```Fortran
      E%EBANDSTR=BANDSTRUCTURE_ENERGY(WDES, W)
!------ Begin tpot
      TOTEN=E%EBANDSTR+E%DENC+E%XCENC+E%TEWEN+E%PSCENC+E%EENTROPY+E%PAWPS+E%PAWAE+INFO%EALLAT+E%EXHF+ECORE()+&
            & Ediel_sol + GC_COOR
!------ End tpot
!-MM- Added to accomodate constrained moment calculations
```
For vasp 6 
```Fortran
      E%EBANDSTR=BANDSTRUCTURE_ENERGY(WDES, W)
!------ Begin tpot
      TOTEN=E%EBANDSTR+E%DENC+E%XCENC+E%TEWEN+E%PSCENC+E%EENTROPY+E%PAWPS &
           +E%PAWAE+INFO%EALLAT+E%EXHF+ECORE()+Ediel_sol+E%ESCPC + GC_COOR
!------ End tpot
!-MM- Added to accomodate constrained moment calculations
```

```Fortran
        WRITE(17 ,*)
      ENDIF
!------ Begin tpot
 io1: IF (IO%NWRITE>=2 .OR. (NSTEP==1)) THEN
    ! energy
      IF (LCORREL()) THEN
         WRITE(IO%IU6,7241) E%PSCENC,E%TEWEN,E%DENC,E%EXHF,E%XCENC,E%PAWPS,E%PAWAE, &
                         E%EENTROPY,E%EBANDSTR,INFO%EALLAT+ECORE(),Ediel_sol,CG_COOR, TOTEN, &
                         TOTEN-E%EENTROPY,TOTEN-E%EENTROPY/(2+NORDER)   
      ELSE
         WRITE(IO%IU6,7240) E%PSCENC,E%TEWEN,E%DENC,E%EXHF,E%XCENC,E%PAWPS,E%PAWAE, &
                         E%EENTROPY,E%EBANDSTR,INFO%EALLAT,Ediel_sol,GC_COOR, TOTEN, &
                         TOTEN-E%EENTROPY,TOTEN-E%EENTROPY/(2+NORDER)
      ENDIF

!------ End tpot
      IF (LHFCALC) THEN 
```

```Fortran
     &        '  Solvation  Ediel_sol  = ',F18.8/ &
!------ Begin tpot
     &        '  GC corr.     GC_COOR  = ',F18.8/ &
!------ End tpot
     &        '  ---------------------------------------------------'/ &
```
Do the same for the block of code below.
```Fortran
     &        '  Solvation  Ediel_sol  = ',F18.8/ &
!------ Begin tpot
     &        '  GC corr.     GC_COOR  = ',F18.8/ &
!------ End tpot
     &        '  ---------------------------------------------------'/ &
```

```Fortran
           CALL WRITE_EIGENVAL( WDES, W, IO%IU6)
        END IF
      io_end
      ENDIF

!------ Begin tpot
      CALL UPDATE_NELECT( IO, INFO, EFERMI, DESUM(N), DESUM1 ) 
!------ End  tpot

      IF (((NSTEP==1 .OR. NSTEP==DYN%NSW).AND.INFO%LABORT).OR. &
```

:two: pot.F

Compare the followng blocks of the code with those in ```pot.F``` to make nessesary modifications. There are changes between ```Begin tpot``` and ```End tpot```

```Fortran
! solvation__
      USE solvation
!------ Begin tpot
      USE targetpot
!------ End tpot
! solvation__
! bexternal__
```

```Fortran
      COMPLEX(q), ALLOCATABLE::  CWORK1(:),CWORK(:,:)
!------ Begin tpot
      COMPLEX(q), ALLOCATABLE :: CVTOT_XC(:,:)
!------ End tpot
      REAL(q) ELECTROSTATIC
```

```Fortran
!-MM- end of addition
!------ Begin tpot
!-----------------------------------------------------------------------
! Save XC CVTOT to CVTOT_XC
!-----------------------------------------------------------------------      
      ALLOCATE(CVTOT_XC(GRIDC%MPLWV,WDES%NCDIJ))
      CVTOT_XC = CVTOT
!-----------------------------------------------------------------------
! FFT of the exchange-correlation potential CVTOT_XC to reciprocal space
!-----------------------------------------------------------------------
      RINPL=1._q/GRIDC%NPLWV
      DO  ISP=1,WDES%NCDIJ
         CALL RL_ADD(CVTOT_XC(1,ISP),RINPL,CVTOT_XC(1,ISP),0.0_q,CVTOT_XC(1,ISP),GRIDC)
         CALL FFT3D(CVTOT_XC(1,ISP),GRIDC,-1)
      ENDDO
!------ End tpot
!-----------------------------------------------------------------------
! calculate the total potential
!-----------------------------------------------------------------------
! add external electrostatic potential
```

```Fortran
      CALL POT_FLIP(CVTOT, GRIDC,WDES%NCDIJ )
!------ Begin tpot
      CALL GET_FERMISHIFT(LATT_CUR, T_INFO, GRIDC, WDES, CWORK, CHTOT)
      CALL GET_VACUUMLEVEL(LATT_CUR, T_INFO, GRIDC, WDES, CVTOT-CVTOT_XC, CHTOT)
      DEALLOCATE(CVTOT_XC)
!------ End tpot
!=======================================================================
! if overlap is used :
```

:three: main.F

Compare the followng blocks of the code with those in ```main.F``` to make nessesary modifications. There are changes between ```Begin tpot``` and ```End tpot```

```Fortran
! solvation__
      USE solvation
!------ Begin tpot
      USE targetpot
!------ End tpot
! solvation__
      USE locproj
```

```
#endif
!------ Begin tpot
      REAL(q) :: GC_COOR  ! correction for Grand canonical DFT
!------ End tpot
#ifdef PROFILING
```

```Fortran
!====================== FORCES+STRESS ==================================
!
!=======================================================================
!------ Begin tpot
!=======================================================================
!TPOT: Calculate GC_COOR = -(NELECT - NELECT0) * EFERMI
!=======================================================================
      GC_COOR = 0
      CALL CAL_GC_COOR( IO, P, INFO, T_INFO, GC_COOR, EFERMI, .TRUE. )
      TOTEN = TOTEN + GC_COOR

      NORDER=0
      IF (KPOINTS%ISMEAR>=0) NORDER=KPOINTS%ISMEAR

 7261 FORMAT(/ &
     &        A/ &
     &        '  ---------------------------------------------------'/ &
     &        '  free  energy   TOTEN  = ',F18.8,' eV' //&
     &        '  energy  without entropy=',F18.8, &
     &        '  energy(sigma->0) =',F18.8, //&
     &        '  GC Correction         = ',F18.8,' eV included in TOTEN')
!------ End tpot

      ! no forces for OEP methods (EXXOEP/=0)
```

```Fortran
         WRITE(TIU6,130)
!------ Begin tpot
         WRITE(TIU6,7261) '  FREE ENERGIE OF THE ION-ELECTRON SYSTEM (eV)', & 
            TOTEN,TOTEN-E%EENTROPY,TOTEN-E%EENTROPY/(2+NORDER),GC_COOR
!------ End tpot
      ENDIF
      IF (DYN%PSTRESS/=0) THEN
```

:four: Admend .objects with ```targetpot.o```, right after ```solvation.o```
```
        solvation.o \
        targetpot.o \
        pot.o \
```

