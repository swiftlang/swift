typedef enum { red, green, blue } Color;

enum Tribool {
  True, False, Indeterminate
};

enum {
  AnonConst1 = 0x700000000,
  AnonConst2
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

typedef struct __NSFastEnumerationState_s {
  unsigned long state;
  void *itemsPtr;
  unsigned long *mutationsPtr;
  unsigned long extra[5];
} NSFastEnumerationState;

typedef void *CFTypeRef;
typedef void const *HWND;
typedef struct __CFString *CFStringRef;

typedef struct {
  struct CGPoint {
    double x;
    double y;
  } origin;
  struct CGSize {
    double width;
    double height;
  } size;
} CGRect;

typedef CGRect NSRect;

typedef void MyVoid;
MyVoid returnsMyVoid(void);

// Function and struct with same name.
void funcOrStruct(void);
struct funcOrStruct { int i; };

