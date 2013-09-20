GHC_WORKING=/5playpen/t-jastol/ghc-validate/inplace/bin/ghc-stage2
GHC_BROKEN=/5playpen/t-jastol/ghc-validate-3/inplace/bin/ghc-stage2
HC_FLAGS=-fforce-recomp -dcmm-lint -O2 -c
MAIN=HeapStackCheck.cmm

.PHONY: clean

all: broken-short.cmm working-short.cmm

broken-short.cmm: broken.cmm
	sed -n '/==================== Post CPS Cmm ====================/,/^$$/p' broken.cmm > broken-short.cmm

working-short.cmm: working.cmm
	sed -n '/==================== Post CPS Cmm ====================/,/^$$/p' working.cmm > working-short.cmm

working.cmm: $(MAIN)
	$(GHC_WORKING) $(HC_FLAGS) $(MAIN) -ddump-cmm > working.cmm

broken.cmm: $(MAIN)
	$(GHC_BROKEN) $(HC_FLAGS) $(MAIN) -ddump-cmm > broken.cmm

clean:
	rm -f broken.cmm broken-short.cmm working-short.cmm working.cmm *.o
