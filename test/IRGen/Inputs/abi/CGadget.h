struct MyRect {
  float x;
  float y;
  float width;
  float height;
};

struct Trio {
  double i;
  double j;
  double k;
};

struct IntPair {
  int a;
  int b;
};

struct NestedInts {
  struct A {
    int value;
  } a;
  struct B {
    int value;
  } b;
};

struct BigStruct {
  char a[32];
};

enum RawEnum {
  Intergalactic,
  Planetary
};

typedef struct One {
  float first;
  float second;
} One;

static inline One makeOne(float f, float s) {
  One one;
  one.first = f;
  one.second = s;

  return one;
}

static inline float MyRect_Area(struct MyRect rect) {
  return rect.width * rect.height;
}
