
top: reg diff

run: run_dk1

CFLAGS = -Winline -Wall -Werror -O1

# Run the executable to display the character graphics from DK
run_dk1: _build/dk1.exe
	./$^

# Link
_build/%.exe: _build/%.o
	@echo Linking
	g++ $^ -o $@ -lSDL2

# Compile (Any) Generated C code
_build/%.o: _build/%.C main.C Makefile
	gcc $(CFLAGS) -I /usr/include/SDL2 -c $< -o $@

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
