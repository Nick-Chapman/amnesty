
top: reg diff

run: run_dk1

CFLAGS = -Wall -Werror

# Run the executable to display the character graphics from DK
run_dk1: _build/dk1.exe
	./$^

# Link objects from (Any) Generated, and Handcoded C
_build/%.exe: _build/%.o _build/rt.o
	@echo Linking
	g++ $^ -o $@ -lSDL2

# Compile the Handcoded C runtime
_build/rt.o: c/rt.C c/rt.h Makefile
	gcc $(CFLAGS) -I /usr/include/SDL2 -c $< -o $@

# Compile (Any) Generated C code
_build/%.o: _build/%.C c/rt.h Makefile
	gcc $(CFLAGS) -c $< -o $@

# Generate C to display the character graphics from DK
_build/dk1.C: _build .stack carts/dk.nes
	stack run -- dk dump > $@

_build: ; @mkdir -p $@


# Regressions...
reg: .reg/smb1 .reg/dk1 .reg/dk50 .reg/dk400

# show SMB character graphics
.reg/smb1: .reg .stack carts/smb.nes
	stack run -- smb -reg > $@

# show DK character graphics
.reg/dk1: .reg .stack carts/dk.nes
	stack run -- dk -reg > $@

# show DK at frame 50 (title screen)
.reg/dk50: .reg .stack carts/dk.nes
	stack run -- dk50 -reg > $@

# show DK at frame 400 (level 1 attract)
.reg/dk400: .reg .stack carts/dk.nes
	stack run -- dk400 -reg > $@

.reg: ; @mkdir -p $@

.stack: src/*.hs Makefile
	stack build
	touch $@

diff:
	git diff .reg
