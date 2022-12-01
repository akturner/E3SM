.SUFFIXES: .F .o

OBJS = 	mpas_seaice_column.o \
	mpas_seaice_error.o \
	mpas_seaice_constants.o \
	mpas_ocn_seaice_column.o

all: $(OBJS)
	ar -ru libseaice_driver.a $(OBJS)

mpas_seaice_error.o:

mpas_seaice_constants.o:

mpas_seaice_column.o: mpas_seaice_constants.o mpas_seaice_error.o

mpas_ocn_seaice_column.o: mpas_seaice_column.o

clean:
	$(RM) *.o *.i *.mod *.f90

FW = ../../../mpas-framework/src

.F.o:
	$(RM) $@ $*.mod
ifeq "$(GEN_F90)" "true"
	$(CPP) $(CPPFLAGS) $(CPPINCLUDES) $< > $*.f90

	$(FC) $(FFLAGS) -c $*.f90 $(FCINCLUDES) -I$(FW)/framework -I$(FW)/operators -I$(FW)/external/esmf_time_f90 -I../seaice_column
else
	$(FC) $(CPPFLAGS) $(FFLAGS) -c $*.F $(CPPINCLUDES) $(FCINCLUDES) -I$(FW)/framework -I$(FW)/operators -I$(FW)/external/esmf_time_f90 -I../seaice_column
endif
