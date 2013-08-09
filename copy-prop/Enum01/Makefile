GHC=/5playpen/t-jastol/build/inplace/bin/ghc-stage2
HC_FLAGS=-fforce-recomp
MAIN=Main.hs

.PHONY: all copyprop baseline clean

all: copyprop baseline fulldump

copyprop: copy-prop-dump.cmm

copy-prop-dump.cmm: Main.hs Enum01.hs
	$(GHC) $(HC_FLAGS) $(MAIN) -fcmm-loopify -fcmm-copy-propagation -ddump-cmm-copy-prop > copy-prop-dump.cmm 2>&1

baseline: baseline-dump.cmm

baseline-dump.cmm: Main.hs Enum01.hs
	$(GHC) $(HC_FLAGS) -fcmm-loopify -ddump-cmm-loopify $(MAIN) > baseline-dump.cmm 2>&1

fulldump: baseline-full-dump.cmm copy-prop-full-dump.cmm

baseline-full-dump.cmm: Main.hs Enum01.hs
	$(GHC) $(HC_FLAGS) $(MAIN) -fcmm-loopify -ddump-cmm > baseline-full-dump.cmm 2>&1

copy-prop-full-dump.cmm: Main.hs Enum01.hs
	$(GHC) $(HC_FLAGS) $(MAIN) -fcmm-copy-propagation -ddump-cmm > copy-prop-full-dump.cmm 2>&1

clean:
	rm *.hi *.o Main *.cmm
