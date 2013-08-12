GHC=/5playpen/t-jastol/build/inplace/bin/ghc-stage2
HC_FLAGS=-fforce-recomp -dcmm-lint
MAIN=B.hs

.PHONY: all copyprop baseline clean fulldump-asm

all: copyprop baseline fulldump fulldump-asm

copyprop: copy-prop-dump.cmm

copy-prop-dump.cmm: $(MAIN)
	$(GHC) $(HC_FLAGS) $(MAIN) -O -fcmm-loopify -fcmm-copy-propagation -ddump-cmm-copy-prop -ddump-cmm-loopify > copy-prop-dump.cmm 2>&1

baseline: baseline-dump.cmm

baseline-dump.cmm: $(MAIN)
	$(GHC) $(HC_FLAGS) -O -fcmm-loopify -ddump-cmm-loopify $(MAIN) > baseline-dump.cmm 2>&1

fulldump: baseline-full-dump.cmm copy-prop-full-dump.cmm

baseline-full-dump.cmm: $(MAIN)
	$(GHC) $(HC_FLAGS) $(MAIN) -O -fcmm-loopify -ddump-cmm > baseline-full-dump.cmm 2>&1

copy-prop-full-dump.cmm: $(MAIN)
	$(GHC) $(HC_FLAGS) $(MAIN) -O -fcmm-loopify -fcmm-copy-propagation -ddump-cmm > copy-prop-full-dump.cmm 2>&1

fulldump-asm: baseline-full-dump.asm copy-prop-full-dump.asm

baseline-full-dump.asm: $(MAIN)
	$(GHC) $(HC_FLAGS) $(MAIN) -O -fcmm-loopify -ddump-asm > baseline-full-dump.asm 2>&1

copy-prop-full-dump.asm: $(MAIN)
	$(GHC) $(HC_FLAGS) $(MAIN) -O -fcmm-loopify -fcmm-copy-propagation -ddump-asm > copy-prop-full-dump.asm 2>&1

clean:
	rm *.hi *.o B *.cmm *.asm
