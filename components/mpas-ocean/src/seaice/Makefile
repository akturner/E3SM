.SUFFIXES: .F .o

.PHONY: all column shared clean analysis_members

OBJS =	mpas_ocn_seaice_column.o

all: column shared $(OBJS)
	ar -ru libseaice_driver.a $(OBJS)

column:
	(cd column; $(MAKE) FFLAGS="${FFLAGS}" FCINCLUDES="$(FCINCLUDES) $(OCEAN_SHARED_INCLUDES)" CPPFLAGS="$(CPPFLAGS)" CPPINCLUDES="$(CPPINCLUDES)" all)

shared: column
	(cd shared; $(MAKE) FFLAGS="${FFLAGS}" FCINCLUDES="$(FCINCLUDES) $(OCEAN_SHARED_INCLUDES)" CPPFLAGS="$(CPPFLAGS)" CPPINCLUDES="$(CPPINCLUDES)" all)

analysis_members: column shared
	(cd analysis_members; $(MAKE) FFLAGS="${FFLAGS}" FCINCLUDES="$(FCINCLUDES) $(OCEAN_SHARED_INCLUDES)" CPPFLAGS="$(CPPFLAGS)" CPPINCLUDES="$(CPPINCLUDES)" all)

$(OBJS): column shared analysis_members

clean:
	$(RM) *.o *.i *.mod *.f90
	(cd column; $(MAKE) clean)
	(cd shared; $(MAKE) clean)
	(cd analysis_members; $(MAKE) clean)

FW = ../../../mpas-framework/src

.F.o:
	$(RM) $@ $*.mod
ifeq "$(GEN_F90)" "true"
	$(CPP) $(CPPFLAGS) $(CPPINCLUDES) $< > $*.f90

	$(FC) $(FFLAGS) -c $*.f90 $(FCINCLUDES) -I$(FW)/framework -I$(FW)/operators -I$(FW)/external/esmf_time_f90 -I./column -I./shared
else
	$(FC) $(CPPFLAGS) $(FFLAGS) -c $*.F $(CPPINCLUDES) $(FCINCLUDES) -I$(FW)/framework -I$(FW)/operators -I$(FW)/external/esmf_time_f90 -I./column -I./shared -I../shared
endif
