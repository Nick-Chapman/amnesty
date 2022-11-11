
top: reg diff

reg: .reg/smb1 .reg/dk1 .reg/dk50 .reg/dk400

.reg/smb1: .reg .stack carts/smb.nes
	stack run -- smb -reg > $@

.reg/dk1: .reg .stack carts/dk.nes
	stack run -- dk -reg > $@

.reg/dk50: .reg .stack carts/dk.nes
	stack run -- dk50 -reg > $@

.reg/dk400: .reg .stack carts/dk.nes
	stack run -- dk400 -reg > $@

.reg: ; @mkdir -p $@

.stack: src/*.hs Makefile
	stack build
	touch $@

.PHONEY diff:
	git diff .reg

