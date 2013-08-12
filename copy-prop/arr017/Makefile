GHC=/5playpen/t-jastol/build/inplace/bin/ghc-stage2
HC_FLAGS=-fforce-recomp -dcmm-lint -O
MAIN=arr017.hs

.PHONY: all

all: copy-prop-after.cmm copy-prop-before.cmm

copy-prop-after.cmm: copy-prop-dump.cmm
	sed -n '/==================== Copy propagation ====================/,/^$$/p' copy-prop-dump.cmm > copy-prop-after.cmm

copy-prop-before.cmm: copy-prop-dump.cmm
	sed -n '/==================== Before copy propagation ====================/,/^$$/p' copy-prop-dump.cmm > copy-prop-before.cmm

copy-prop-dump.cmm: $(MAIN)
	$(GHC) $(HC_FLAGS) $(MAIN) -fcmm-copy-propagation -ddump-cmm-copy-prop -ddump-cmm-before-cp > copy-prop-dump.cmm 2>&1

clean:
	rm *.hi *.o arr017 copy-prop-dump.cmm copy-prop-before.cmm copy-prop-after.cmm
