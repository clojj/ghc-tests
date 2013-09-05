GHC_WORKING=/5playpen/t-jastol/ghc-validate-2/inplace/bin/ghc-stage2
GHC_BROKEN=/5playpen/t-jastol/ghc-validate-3/inplace/bin/ghc-stage2
HC_FLAGS=-fforce-recomp -dcmm-lint -O -c
MAIN=Exception.cmm

.PHONY: clean

all: broken-short.cmm working-short.cmm

broken-short.cmm: broken.cmm
	sed -n '/==================== Post CPS Cmm ====================/,/^$$/p' broken.cmm > broken-short.cmm

working-short.cmm: working.cmm
	sed -n '/==================== Post CPS Cmm ====================/,/^$$/p' working.cmm > working-short.cmm

working.cmm: $(MAIN)
	$(GHC_WORKING) $(HC_FLAGS) $(MAIN) -ddump-cmm > working.cmm 2>&1

broken.cmm: $(MAIN)
	$(GHC_BROKEN) $(HC_FLAGS) $(MAIN) -ddump-cmm > broken.cmm 2>&1

clean:
	rm -f broken.cmm broken-short.cmm working-short.cmm working.cmm Exception.o
