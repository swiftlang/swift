typedef enum { red, green, blue} Color;

enum Tribool {
  True, False, Indeterminate
};

enum {
  AnonConst1 = 7,
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

struct NSFastEnumerationState {
  unsigned long state;
  void *itemsPtr;
  unsigned long *mutationsPtr;
  unsigned long extra[5];
};

typedef void *CFTypeRef;
typedef void const *HWND;
typedef struct __CFString *CFStringRef;
