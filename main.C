
#include <assert.h>
#include <stdio.h>
#include <sys/time.h> // gettimeofday()
#include "SDL.h"

typedef unsigned long u64;

typedef bool u1;
typedef unsigned char u8;
typedef unsigned short u16;

typedef enum { Key_x, Key_y, Key_z, Key_n, Key_p } Key;

#define repeat(n) for (int i = (n); i>0; i--)

static void ppu(void); // defined in generated code
extern u8 chr1[]; // deined in generated code

u64 wallclock_time() { //in micro-seconds
  struct timeval tv;
  gettimeofday(&tv,NULL);
  return tv.tv_sec*(u64)1000000+tv.tv_usec;
}

static u8 reg_RegX;
static u8 reg_RegY;
static u8 reg_RegZ;
static u8 reg_RegN;
static u8 reg_RegP;
static u8 reg_RegScanX;
static u8 reg_RegScanY;

const int scale = 3;
const int width = 256;
const int height = 240;

SDL_Window* create_window() {
  int x = 10000; //SDL_WINDOWPOS_CENTERED;
  int y = 0; //SDL_WINDOWPOS_CENTERED;
  int w = width * scale;
  int h = height * scale;
  int flags = 0;
  return SDL_CreateWindow ("amnesty", x, y, w, h, flags);
}

typedef struct { u8 r; u8 g; u8 b; } rgb;

rgb colours[] =
{
 {0x66, 0x66, 0x66}, {0x00, 0x2A, 0x88}, {0x14, 0x12, 0xA7}, {0x3B, 0x00, 0xA4},
 {0x5C, 0x00, 0x7E}, {0x6E, 0x00, 0x40}, {0x6C, 0x06, 0x00}, {0x56, 0x1D, 0x00},
 {0x33, 0x35, 0x00}, {0x0B, 0x48, 0x00}, {0x00, 0x52, 0x00}, {0x00, 0x4F, 0x08},
 {0x00, 0x40, 0x4D}, {0x00, 0x00, 0x00}, {0x00, 0x00, 0x00}, {0x00, 0x00, 0x00},
 {0xAD, 0xAD, 0xAD}, {0x15, 0x5F, 0xD9}, {0x42, 0x40, 0xFF}, {0x75, 0x27, 0xFE},
 {0xA0, 0x1A, 0xCC}, {0xB7, 0x1E, 0x7B}, {0xB5, 0x31, 0x20}, {0x99, 0x4E, 0x00},
 {0x6B, 0x6D, 0x00}, {0x38, 0x87, 0x00}, {0x0C, 0x93, 0x00}, {0x00, 0x8F, 0x32},
 {0x00, 0x7C, 0x8D}, {0x00, 0x00, 0x00}, {0x00, 0x00, 0x00}, {0x00, 0x00, 0x00},
 {0xFF, 0xFE, 0xFF}, {0x64, 0xB0, 0xFF}, {0x92, 0x90, 0xFF}, {0xC6, 0x76, 0xFF},
 {0xF3, 0x6A, 0xFF}, {0xFE, 0x6E, 0xCC}, {0xFE, 0x81, 0x70}, {0xEA, 0x9E, 0x22},
 {0xBC, 0xBE, 0x00}, {0x88, 0xD8, 0x00}, {0x5C, 0xE4, 0x30}, {0x45, 0xE0, 0x82},
 {0x48, 0xCD, 0xDE}, {0x4F, 0x4F, 0x4F}, {0x00, 0x00, 0x00}, {0x00, 0x00, 0x00},
 {0xFF, 0xFE, 0xFF}, {0xC0, 0xDF, 0xFF}, {0xD3, 0xD2, 0xFF}, {0xE8, 0xC8, 0xFF},
 {0xFB, 0xC2, 0xFF}, {0xFE, 0xC4, 0xEA}, {0xFE, 0xCC, 0xC5}, {0xF7, 0xD8, 0xA5},
 {0xE4, 0xE5, 0x94}, {0xCF, 0xEF, 0x96}, {0xBD, 0xF4, 0xAB}, {0xB3, 0xF3, 0xCC},
 {0xB5, 0xEB, 0xF2}, {0xB8, 0xB8, 0xB8}, {0x00, 0x00, 0x00}, {0x00, 0x00, 0x00}
};

u8 frame_buffer[width * height];

void render(SDL_Renderer* renderer) {
  for (int y = 0; y < height; y++) {
    for (int x = 0; x < width; x++) {
      rgb col = colours[frame_buffer[x + y * width]];
      SDL_SetRenderDrawColor(renderer, col.r, col.g, col.b, 255);
      SDL_Rect rect = { x * scale, y * scale, scale, scale };
      SDL_RenderFillRect(renderer, &rect);
    }
  }
  SDL_RenderPresent(renderer);
}

enum Keys
  {
   KEYS_QUIT = 1
  };

static u64 keystate;

#define KEY(x) (!!(keystate & (x)))

static void input() {
  SDL_Event event_buffer[64];
  size_t num = 0;
  while (num < 64) {
    int has = SDL_PollEvent(&event_buffer[num]);
    if (!has) break;
    num++;
  }
  for (size_t i = 0; i < num; ++i) {
    SDL_Event e = event_buffer[i];
    if (e.type == SDL_QUIT) {
      e.type = SDL_KEYDOWN;
      e.key.keysym.sym = SDLK_ESCAPE;
    }
    if (! (e.type == SDL_KEYDOWN || e.type == SDL_KEYUP)) continue;
    u64 mask = 0;
    u64 f = e.type == SDL_KEYDOWN;
    switch (e.key.keysym.sym) {
#define KEY_MAP(x, y) case x: mask = y; break;
      KEY_MAP(SDLK_ESCAPE, KEYS_QUIT);
    }
    keystate = (keystate & ~mask) | (-f & mask);
  }
}

void print_stats_maybe() {
  static bool first = true;
  if (first) {
    first = false;
    printf("   n : frame# :  fps\n");
  }
  static u64 last_print_time = wallclock_time();
  static int frames = 0;
  static int frames_last = 0;
  static int prints = 0;
  frames++;
  u64 now = wallclock_time();
  bool do_print = (now - last_print_time) > 1000000; // every second
  if (do_print) {
    prints++;
    int fps = frames - frames_last;
    printf("%4d :%7d :%5d\n" , prints, frames, fps);
    frames_last = frames;
    last_print_time = now;
  }
}

int main() {
  SDL_Window* window = create_window();
  int rflags = SDL_RENDERER_ACCELERATED; //| SDL_RENDERER_PRESENTVSYNC;
  SDL_Renderer* renderer = SDL_CreateRenderer(window, -1, rflags);
  while (!KEY(KEYS_QUIT)) {
    input();
    ppu();
    render(renderer); // most time here! no render: fps: 100 -> 4000
    print_stats_maybe();
  }
  SDL_DestroyRenderer(renderer);
  SDL_DestroyWindow(window);
}

// ----------------------------------------------------------------------
// called by generated code...

inline static
u1 is_pressed(Key key) {
  //assert(0);
  return 0;
}


u8 vram[0x4000]; //way too much! -- this is the vram address space

inline static
void write_mem(u16 addr, u8 value) {
  //printf("writeMem: %04x <- %02x\n",addr,value);
  vram[addr] = value;
}

inline static
u8 read_mem(u16 addr) {
  if (addr < 0x2000) {
    u8 value = chr1[addr];
    //printf("readMem: %04x --> %02x\n",addr,value);
    return value;
  }
  u8 value = vram[addr];
  //printf("readMem: %04x --> %02x\n",addr,value);
  return value;
}

inline static
u16 hilo(u8 hi,u8 lo) {
  return (hi << 8) | lo;
}

inline static
u1 testbit(u8 v,u8 n) {
  //printf("testbit (%d,%d)\n",v,n);
  assert(0 <= n && n < 8);
  return v & (1<<n);
}

inline static
void emitPixel(u8 x,u8 y,u8 col6) {
  //printf("emitPixel (%d,%d) %d\n",x,y,col6);
  assert (0 <= col6 && col6 <= 63);
  frame_buffer[x + y * width] = col6;
}

#define my_assert(a,b) {}
#define False false
#define True true

inline static
u8 make_b8(u1 a, u1 b, u1 c, u1 d, u1 e, u1 f, u1 g, u1 h) {
  return (a<<7) | (b<<6) | (c<<5) | (d<<4) | (e<<3) | (f<<2) | (g<<1) | (h<<0);
}
