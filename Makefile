PLATFORM ?= musl64
STRIP = strip
PKG ?= mqtt-to-periodic
COMPILER ?= ghc9141

ifeq ($(PLATFORM),aarch64-multiplatform-musl)
STRIP = aarch64-linux-gnu-strip
else
ifeq ($(PLATFORM),muslpi)
STRIP = armv6l-unknown-linux-musleabihf-strip
COMPILER = ghc884
else
ifeq ($(PLATFORM),aarch64-android)
STRIP = aarch64-unknown-linux-android-strip
else

endif

endif

endif

all: package

dist/$(PLATFORM):
	mkdir -p $@

dist/$(PLATFORM)/%: dist/$(PLATFORM)
	nix-build -A projectCross.$(PLATFORM).hsPkgs.$(PKG).components.exes.$(shell basename $@) --argstr compiler-nix-name $(COMPILER)
	cp -f result/bin/$(shell basename $@) $@
	chmod +w $@
	nix-shell --run "$(STRIP) -s $@" --argstr compiler-nix-name $(COMPILER) --arg crossPlatforms "ps: with ps; [$(PLATFORM)]"
	chmod -w $@

dist/mqtt-to-periodic:
	PKG=$(shell basename $@) make dist/$(PLATFORM)/$(shell basename $@)

package: dist/mqtt-to-periodic
	cd dist/$(PLATFORM) && tar cjvf ../mqtt-to-periodic-linux-$(PLATFORM).tar.bz2 *

update-sha256:
	gawk -f nix/update-sha256.awk cabal.project > nix/sha256map.nix

clean:
	rm -rf dist

help:
	@echo make PLATFORM=muslpi
	@echo make PLATFORM=musl64
	@echo make PLATFORM=aarch64-multiplatform-musl
	@echo make clean
	@echo make update-sha256
