GHC=/5playpen/t-jastol/build/inplace/bin/ghc-stage2
HC_FLAGS=-fforce-recomp
MAIN=T2902_B.hs

.PHONY: all copyprop baseline clean

all: copyprop baseline fulldump

copyprop: copy-prop-dump.cmm

copy-prop-dump.cmm: $(MAIN)
	$(GHC) $(HC_FLAGS) $(MAIN) -O -fcmm-loopify -fcmm-copy-propagation -ddump-cmm-copy-prop > copy-prop-dump.cmm 2>&1

baseline: baseline-dump.cmm

baseline-dump.cmm: $(MAIN)
	$(GHC) $(HC_FLAGS) -O -fcmm-loopify -ddump-cmm-loopify $(MAIN) > baseline-dump.cmm 2>&1

fulldump: baseline-full-dump.cmm copy-prop-full-dump.cmm

baseline-full-dump.cmm: $(MAIN)
	$(GHC) $(HC_FLAGS) $(MAIN) -O -fcmm-loopify -ddump-cmm > baseline-full-dump.cmm 2>&1

copy-prop-full-dump.cmm: $(MAIN)
	$(GHC) $(HC_FLAGS) $(MAIN) -O -fcmm-loopify -fcmm-copy-propagation -ddump-cmm > copy-prop-full-dump.cmm 2>&1

clean:
	rm *.hi *.o T2902_B *.cmm
