# Modification to VASP code 

Since we can not provide a batch for VASP (according to its licence agreement), we provide a list of changes that we need to make to ```electron.F```, ```pot.F```, ```main.F```, and ```.objects``` files.

These instruction below are exlicitly for vasp 5.4.4. For other version, users need to make appropriate corretions (expected to be similar). 

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
!=======================================================================
! calculate free-energy and bandstructur-energy
! EBANDSTR = sum of the energy eigenvalues of the electronic states
!         weighted by the relative weight of the special k point
! TOTEN = total free energy of the system
!=======================================================================
      E%EBANDSTR=BANDSTRUCTURE_ENERGY(WDES, W)
      TOTEN=E%EBANDSTR+E%DENC+E%XCENC+E%TEWEN+E%PSCENC+E%EENTROPY+E%PAWPS+E%PAWAE+INFO%EALLAT+E%EXHF+ECORE()+&
            & Ediel_sol + GC_COOR
!------ End tpot
!-MM- Added to accomodate constrained moment calculations
      IF (M_CONSTRAINED()) TOTEN=TOTEN+E_CONSTRAINT()
      io_begin
      CALL WRITE_CONSTRAINED_M(17,.FALSE.)
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

      IF (LHFCALC) THEN 
         WRITE(IO%IU6,'( "  exchange ACFDT corr.  = ",F18.8,"  see jH, gK, PRB 81, 115126")') E%EXHF_ACFDT
      ENDIF

7240  FORMAT(/ &
              ' Free energy of the ion-electron system (eV)'/ &
     &        '  ---------------------------------------------------'/ &
     &        '  alpha Z        PSCENC = ',F18.8/ &
     &        '  Ewald energy   TEWEN  = ',F18.8/ &
     &        '  -Hartree energ DENC   = ',F18.8/ &
     &        '  -exchange      EXHF   = ',F18.8/ &
     &        '  -V(xc)+E(xc)   XCENC  = ',F18.8/ &
     &        '  PAW double counting   = ',2F18.8/ &
     &        '  entropy T*S    EENTRO = ',F18.8/ &
     &        '  eigenvalues    EBANDS = ',F18.8/ &
     &        '  atomic energy  EATOM  = ',F18.8/ &
     &        '  Solvation  Ediel_sol  = ',F18.8/ &
     &        '  GC corr.     GC_COOR  = ',F18.8/ &
     &        '  ---------------------------------------------------'/ &
     &        '  free energy    TOTEN  = ',F18.8,' eV'// &
     &        '  energy without entropy =',F18.8, &
     &        '  energy(sigma->0) =',F18.8)
7241  FORMAT(/ &
              ' Free energy of the ion-electron system (eV)'/ &
     &        '  ---------------------------------------------------'/ &
     &        '  alpha Z        PSCENC = ',F18.8/ &
     &        '  Ewald energy   TEWEN  = ',F18.8/ &
     &        '  -Hartree energ DENC   = ',F18.8/ &
     &        '  -exchange      EXHF   = ',F18.8/ &
     &        '  -V(xc)+E(xc)   XCENC  = ',F18.8/ &
     &        '  PAW double counting   = ',2F18.8/ &
     &        '  entropy T*S    EENTRO = ',F18.8/ &
     &        '  eigenvalues    EBANDS = ',F18.8/ &
     &        '  core contrib.  ECORE  = ',F18.8/ &
     &        '  Solvation  Ediel_sol  = ',F18.8/ &
     &        '  GC corr.     GC_COOR  = ',F18.8/ &
     &        '  ---------------------------------------------------'/ &
     &        '  free energy    TOTEN  = ',F18.8,' eV'// &
     &        '  energy without entropy =',F18.8, &
     &        '  energy(sigma->0) =',F18.8)
!------ End tpot
72612 FORMAT(//&
     &        '  METAGGA EXCHANGE AND CORRELATION (eV)'/ &
     &        '  ---------------------------------------------------'/ &
     &        '  LDA+GGA E(xc)  EXCG   = ',F18.6/ &
```
```Fortran
        IF(INFO%IHARMONIC==1)THEN
           CALL WRITE_EIGENVAL_RESIDUAL( WDES, W, IO%IU6)
        ELSE
           CALL WRITE_EIGENVAL( WDES, W, IO%IU6)
        END IF
      io_end
      ENDIF

!------ Begin tpot
      CALL UPDATE_NELECT( IO, INFO, EFERMI, DESUM(N), DESUM1 ) 
!------ End  target potential

      IF (((NSTEP==1 .OR. NSTEP==DYN%NSW).AND.INFO%LABORT).OR. &
     &     (IO%NWRITE>=1 .AND.INFO%LABORT).OR.IO%NWRITE>=3) THEN
      io_begin
!-----Charge-density along one line
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
! work arrays (allocated after call to FEXCG)
      COMPLEX(q), ALLOCATABLE::  CWORK1(:),CWORK(:,:)
!------ Begin tpot
      COMPLEX(q), ALLOCATABLE :: CVTOT_XC(:,:)
!------ End tpot
      REAL(q) ELECTROSTATIC
      LOGICAL, EXTERNAL :: L_NO_LSDA_GLOBAL
```

```Fortran
#endif
#endif
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
! bexternal__
      IF (LBEXTERNAL()) CALL BEXT_ADDV(CVTOT,GRIDC,SIZE(CVTOT,2))
! bexternal__
      CALL POT_FLIP(CVTOT, GRIDC,WDES%NCDIJ )
!------ Begin tpot
      CALL GET_FERMISHIFT(LATT_CUR, T_INFO, GRIDC, WDES, CWORK, CHTOT)
      CALL GET_VACUUMLEVEL(LATT_CUR, T_INFO, GRIDC, WDES, CVTOT-CVTOT_XC, CHTOT)
      DEALLOCATE(CVTOT_XC)
!------ End tpot
!=======================================================================
! if overlap is used :
! copy CVTOT to SV and set contribution of unbalanced lattice-vectors
! to zero,  then  FFT of SV and CVTOT to real space
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
      ! no forces for IALGO=1-4
      ! no forces if potential was read in INFO%INICHG==4
```

```Fortran
#ifdef libbeef
      IF(LBEEFENS) LBEEFCALCBASIS = .TRUE.
#endif
!------ Begin tpot
      IF (IO%IU6>=0) THEN
         WRITE(TIU6,130)
         WRITE(TIU6,7261) '  FREE ENERGIE OF THE ION-ELECTRON SYSTEM (eV)', & 
            TOTEN,TOTEN-E%EENTROPY,TOTEN-E%EENTROPY/(2+NORDER),GC_COOR
      ENDIF
!------ End tpot
      IF (DYN%PSTRESS/=0) THEN
         TOTEN=TOTEN+DYN%PSTRESS/(EVTOJ*1E22_q)*LATT_CUR%OMEGA

```

:four: Admend .objects with ```targetpot.o```, right after ```solvation.o```
```
        solvation.o \
        targetpot.o \
        pot.o \
```

