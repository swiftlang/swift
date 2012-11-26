typedef enum { red, green, blue} Color;

enum Tribool {
  True, False, Indeterminate
};

struct Point {
  float x;
  float y;
};

typedef struct {
  struct {
    int a;
    float b;
    struct {
      double c;
    };
  };
} AnonStructs;
