
exe = .stack-work/dist/x86_64-linux/Cabal-3.2.1.0/build/main.exe/main.exe

CFLAGS = -Winline -Wall -Werror -O1


top: reg diff

diff:
	git diff .reg

reg: .reg/smb1 .reg/dk1 .reg/dk50 .reg/dk400

# Run regessions from C
.reg/%: _build/%.exe
	./$^ > $@

# Link any Generated C
_build/%.exe: _build/%.o
	g++ $^ -o $@ -lSDL2

_build/%.o: _build/%.C main.C
	gcc $(CFLAGS) -I /usr/include/SDL2 -c $< -o $@


_build/dk400.C: carts/dk.nes
_build/dk50.C: carts/dk.nes
_build/dk1.C: carts/dk.nes
_build/smb1.C: carts/smb.nes


_build/dk400.C: $(exe)
	$(exe) -- dk400 dump > $@

_build/dk50.C: $(exe)
	$(exe) -- dk50 dump > $@

_build/dk1.C: $(exe)
	$(exe) -- dk dump > $@

_build/smb1.C: $(exe)
	$(exe) -- smb dump > $@

$(exe) : src/*.hs
	stack build



#
#top: reg diff
#
#run: run_dk400
#
#CFLAGS = -Winline -Wall -Werror -O1
#
#exe = .stack-work/dist/x86_64-linux/Cabal-3.2.1.0/build/main.exe/main.exe
#
##.stack: src/*.hs Makefile
##	stack build
##	touch $@
#
#$(exe) : src/*.hs
#	stack build
##	touch $(exe)
#
#
## Run regessions from C
#.reg/%: _build/%.exe
#	./$^ > $@
#
## Run executable for any Generated C
#run_%: _build/%.exe
#	./$^
#
## Link any Generated C
#_build/%.exe: _build/%.o
#	g++ $^ -o $@ -lSDL2
#
## Compile any Generated C
#_build/%.o: _build/%.C main.C
#	gcc $(CFLAGS) -I /usr/include/SDL2 -c $< -o $@
#
#
## Generate C for 4 examples: sm1, dk1, dk50, dk400
#_build/smb1.C: _build $(exe) carts/smb.nes
#	$(exe) -- smb dump > $@
#
#_build/dk1.C: _build $(exe) carts/dk.nes
#	$(exe) -- dk dump > $@
#
#_build/dk50.C: _build $(exe) carts/dk.nes
#	$(exe) -- dk50 dump > $@
#
#_build/dk400.C: _build $(exe) carts/dk.nes
#	$(exe) -- dk400 dump > $@
#
#
#_build: ; @mkdir -p $@
#
#
## Regressions...
#reg: .reg/smb1 .reg/dk1 .reg/dk50 .reg/dk400
#
## # show SMB character graphics
## .reg/smb1: .reg $(exe) carts/smb.nes
## 	$(exe) -- smb -reg > $@
#
## # show DK character graphics
## .reg/dk1: .reg $(exe) carts/dk.nes
## 	$(exe) -- dk -reg > $@
#
## # show DK at frame 50 (title screen)
## .reg/dk50: .reg $(exe) carts/dk.nes
## 	$(exe) -- dk50 -reg > $@
#
## # show DK at frame 400 (level 1 attract)
## .reg/dk400: .reg $(exe) carts/dk.nes
## 	$(exe) -- dk400 -reg > $@
#
#.reg: ; @mkdir -p $@
#
#diff:
#	git diff .reg
#
