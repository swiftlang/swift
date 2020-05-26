struct BitfieldOne {
  unsigned a;
  unsigned : 0;
  struct Nested {
    float x;
    unsigned y : 15;
    unsigned z : 8;
  } b;
  int c : 5;
  int d : 7;
  int e : 13;
  int f : 15;
  int g : 8;
  int h : 2;
  float i;
  int j : 3;
  int k : 4;
  unsigned long long l;
  unsigned m;
};

struct BitfieldOne createBitfieldOne(void);
void consumeBitfieldOne(struct BitfieldOne one);

