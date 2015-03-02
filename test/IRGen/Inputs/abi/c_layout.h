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

struct A {
  int x;
};
struct A createA(void);


struct BitfieldSeparatorReference {
  unsigned char a;
  unsigned : 0;
  unsigned char b;
};

typedef struct BitfieldSeparatorSameName {
  unsigned char a;
  unsigned : 0;
  unsigned char b;
} BitfieldSeparatorSameName;

typedef struct BitfieldSeparatorDifferentNameStruct {
  unsigned char a;
  unsigned : 0;
  unsigned char b;
} BitfieldSeparatorDifferentName;

typedef struct {
  unsigned char a;
  unsigned : 0;
  unsigned char b;
} BitfieldSeparatorAnon;

typedef float vector_float3 __attribute__((__ext_vector_type__(3)));

struct SIMDStruct {
  vector_float3 v;
};

void takesSIMDStruct(struct SIMDStruct);

struct HasRecursivePointers {
  struct HasRecursivePointers *next;
  void (*getNext)(struct HasRecursivePointers);
};

// Test sign extension behavior

char chareth(char a);
signed char signedChareth(signed char a);
unsigned char unsignedChareth(unsigned char a);

short eatMyShorts(short a);
unsigned short eatMyUnsignedShorts(unsigned short a);

int ints(int a);
unsigned unsigneds(unsigned a);
