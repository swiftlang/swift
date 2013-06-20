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

// Names from MacTypes.h that conflict with swift's library types.
// rdar://14175675
typedef unsigned __INT8_TYPE__ UInt8;
typedef unsigned __INT16_TYPE__ UInt16;
typedef unsigned __INT32_TYPE__ UInt32;
typedef unsigned __INT64_TYPE__ UInt64;
typedef float Float32;
typedef double Float64;
typedef long double Float80;

// Other types from MacTypes.h.
typedef __INT8_TYPE__ SInt8;
typedef __INT16_TYPE__ SInt16;
typedef __INT32_TYPE__ SInt32;
typedef __INT64_TYPE__ SInt64;

// Types from stdint.h.
typedef unsigned __INT8_TYPE__ uint8_t;
typedef unsigned __INT16_TYPE__ uint16_t;
typedef unsigned __INT32_TYPE__ uint32_t;
typedef unsigned __INT64_TYPE__ uint64_t;
typedef __INT8_TYPE__ int8_t;
typedef __INT16_TYPE__ int16_t;
typedef __INT32_TYPE__ int32_t;
typedef __INT64_TYPE__ int64_t;

