typedef struct MyCStruct {
  int i;
  int *ip;
  char c;
} MyCStruct;

typedef enum MyCEnum {
  ONE,
  TWO,
  THREE,
} MyCEnum;

typedef union MyCUnion {
  char c;
  int i;
  float f;
  int *ip;
} MyCUnion;

typedef struct MyCStructWithBitfields {
  unsigned a : 1;
  unsigned b : 2;
  unsigned c : 4;
  unsigned d : 8;
} MyCStructWithBitfields;

