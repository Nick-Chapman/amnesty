
#include <stdio.h>
#include "rt.h"

int main() {
  //printf("main..\n");
  ppu();
  //printf("main..done\n");
}


u1 is_pressed(Key key) {
  return 0;
}

u8 read_mem(u16 addr) {
  return 0;
}
u16 hilo(u8 hi,u8 lo) {
  return 0;
}
void emitPixel(u8 x,u8 y,u8 col) {
  printf("emitPixel: %d %d %d\n",x,y,col);
}
u1 testbit(u8 v,u8 n) {
  return 0;
}
