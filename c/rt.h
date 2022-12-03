
typedef bool u1;
typedef unsigned char u8;
typedef unsigned short u16;

typedef enum { Key_x, Key_y, Key_z, Key_n, Key_p } Key;

extern u8 reg_RegX;
extern u8 reg_RegY;
extern u8 reg_RegZ;
extern u8 reg_RegN;
extern u8 reg_RegP;
extern u8 reg_RegScanX;
extern u8 reg_RegScanY;

#define repeat(n) for (int i = (n); i>0; i--)

u1 is_pressed(Key);
u8 read_mem(u16);
u16 hilo(u8,u8);
void emitPixel(u8,u8,u8);
u1 testbit(u8,u8);

void ppu(void);

extern u8 chr1[]; // [ 8192 ];
