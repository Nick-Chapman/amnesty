
top: reg diff

dev: run_dk1

run_dk1: _build/dk1.exe
	./$^

_build/%.exe: _build/%.o _build/rt.o
	#@echo Linking
	g++ $^ -o $@

_build/rt.o: c/rt.c c/rt.h Makefile
	#@echo Building $<
	gcc -Wall -Werror $< -c -o $@

_build/%.o: _build/%.c c/rt.h Makefile
	#@echo Building $<
	gcc -Wall -Werror $< -c -o $@


_build/dk1.c: _build .stack carts/dk.nes
	stack run -- dk dump > $@

_build: ; @mkdir -p $@


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

diff:
	git diff .reg

