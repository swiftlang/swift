typedef union IntOrFloat {
  int i;
  float f;
} IntOrFloat;

typedef struct StructWithNamedUnion {
  int a;
  int b;
  IntOrFloat intfloat;
} NamedUnion;

