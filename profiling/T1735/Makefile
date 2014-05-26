GHC=/dane/uczelnia/projekty/ghc-build/inplace/bin/ghc-stage2
HC_FLAGS=-fforce-recomp -prof -auto-all -O2
MAIN=T1735.hs

.PHONY: all

all: T1735-input.cmm T1735-output.cmm

T1735-output.cmm: T1735-dump.cmm
	sed -n '/==================== Output Cmm ====================/,/^$$/p' T1735-dump.cmm > T1735-output.cmm

T1735-input.cmm: T1735-dump.cmm
	sed -n '/==================== Cmm produced by new codegen ====================/,/^$$/p' T1735-dump.cmm > T1735-input.cmm

T1735-dump.cmm: $(MAIN)
	$(GHC) $(HC_FLAGS) $(MAIN) -ddump-cmm > T1735-dump.cmm 2>&1

clean:
	rm -f *.hi *.o T1735 T1735-dump.cmm T1735-input.cmm T1735-output.cmm
