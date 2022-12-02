
#include <assert.h>
#include <stdio.h>
#include "rt.h"

int main() {
  //printf("main..\n");
  ppu();
  //printf("main..done\n");
}


u1 is_pressed(Key key) {
  //assert(0);
  return 0;
}

u8 read_mem(u16 addr) {
  if (addr < 0x2000) {
    u8 b = chr1[addr];
    printf("readMem: %04x --> %02x\n",addr,b);
    return b;
  }
  assert(0);
}
u16 hilo(u8 hi,u8 lo) {
  return (hi << 8) | lo;
}
void emitPixel(u8 x,u8 y,u8 col) {
  printf("emitPixel (%d,%d) %d\n",x,y,col);
}
u1 testbit(u8 v,u8 n) {
  //printf("testbit (%d,%d)\n",v,n);
  assert(0 <= n && n < 8);
  return v & (1<<n);
}
