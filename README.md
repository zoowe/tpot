# tpot

# Instruction

:one: Admend electrond.F

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

:three: Copy main source code
Copy ```src/targetpot.F``` to vasp ```src``` folder

:four: Recompile your vasp code



