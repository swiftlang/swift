#define SWIFT_NAME(X) __attribute__((swift_name(#X)))

void drawString(const char *, int x, int y) SWIFT_NAME(drawString(_:x:y:));

enum SWIFT_NAME(ColorKind) ColorType {
  CT_red,
  CT_green,
  CT_blue,
};

typedef struct SWIFT_NAME(Point) {
  int X SWIFT_NAME(x);
  int Y SWIFT_NAME(y);
} PointType;

typedef int my_int_t SWIFT_NAME(MyInt);
