.PHONY: all
.PHONY: runtests

GPR_PROJECT_PATH=-aP./../cairoada -aP./../CairoAda -aP./gpr

all:
	gprbuild $(GPR_PROJECT_PATH) build/build.gpr
	cd build; ./build
	gprbuild $(GPR_PROJECT_PATH) tests/coretest.gpr
	gprbuild $(GPR_PROJECT_PATH) tests/nettest.gpr
	gprbuild $(GPR_PROJECT_PATH) tests/graphicstest.gpr
	gprbuild $(GPR_PROJECT_PATH) tests/guitest.gpr
	gprbuild $(GPR_PROJECT_PATH) tests/xtest.gpr
	gprbuild $(GPR_PROJECT_PATH) tests/pngtest.gpr

clean:
	gprclean $(GPR_PROJECT_PATH) build/build.gpr
	gprclean $(GPR_PROJECT_PATH) tests/coretest.gpr
	gprclean $(GPR_PROJECT_PATH) tests/nettest.gpr
	gprclean $(GPR_PROJECT_PATH) tests/graphicstest.gpr
	gprclean $(GPR_PROJECT_PATH) tests/guitest.gpr
	gprclean $(GPR_PROJECT_PATH) tests/xtest.gpr
	gprclean $(GPR_PROJECT_PATH) tests/pngtest.gpr

runtests:
	tests/coretest
	tests/nettest
	tests/graphicstest
