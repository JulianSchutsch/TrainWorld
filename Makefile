.PHONY: all
.PHONY: runtests

GPR_PROJECT_PATH=-aP$(CURDIR)/../cairoada -aP$(CURDIR)/../CairoAda -aP$(CURDIR)/gpr

all:
	gprbuild $(GPR_PROJECT_PATH) build/build.gpr
	cd build; ./build
	gprbuild $(GPR_PROJECT_PATH) tests/coretest.gpr
	gprbuild $(GPR_PROJECT_PATH) tests/nettest.gpr
	gprbuild $(GPR_PROJECT_PATH) tests/graphicstest.gpr
	gprbuild $(GPR_PROJECT_PATH) tests/guitest.gpr

clean:
	gprclean $(GPR_PROJECT_PATH) build/build.gpr
	gprclean $(GPR_PROJECT_PATH) tests/coretest.gpr
	gprclean $(GPR_PROJECT_PATH) tests/nettest.gpr
	gprclean $(GPR_PROJECT_PATH) tests/graphicstest.gpr
	gprclean $(GPR_PROJECT_PATH) tests/guitest.gpr

runtests:
	tests/coretest
	tests/nettest
	tests/graphicstest
