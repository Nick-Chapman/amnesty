
top: reg diff

reg: .reg/smb1 .reg/dk1

.reg/smb1: .reg .stack carts/smb.nes
	stack run smb -- -nopic1 > $@

.reg/dk1: .reg .stack carts/dk.nes
	stack run dk -- -nopic1 > $@

.reg: ; @mkdir -p $@

.stack: src/*.hs Makefile
	stack build
	touch $@

.PHONEY diff:
	git diff .reg

