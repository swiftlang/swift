#ifndef __CTYPES__
#define __CTYPES__

typedef enum { red, green, blue } Color;

enum Tribool {
  True, False, Indeterminate
};

enum {
  AnonConst1 = 0x700000000,
  AnonConst2
};

enum {
  AnonConstSmall1 = 16,
  AnonConstSmall2
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

typedef void const *HWND;

struct StructWithBitfields {
  unsigned First;
  unsigned Second : 17;
  unsigned Third : 5;
};

typedef struct {
  unsigned a;
  unsigned b[];
} StructWithFlexibleArray;

//===---
// Tag decls and typedefs.
//===---

struct FooStruct1 {
  int x;
  double y;
};

typedef struct FooStruct2 {
  int x;
  double y;
} FooStructTypedef1;

typedef struct {
  int x;
  double y;
} FooStructTypedef2;

typedef struct FooStruct3 {
  int x;
  double y;
} FooStruct3;

struct FooStruct4 {
  int x;
  double y;
};
typedef struct FooStruct4 FooStruct4;

struct FooStruct5;
typedef struct FooStruct5 FooStruct5;
struct FooStruct5 {
  int x;
  double y;
};

typedef struct FooStruct6 FooStruct6;
struct FooStruct6 {
  int x;
  double y;
};

//===---
// Typedefs.
//===---

typedef void MyVoid;
MyVoid returnsMyVoid(void);

// Function and struct with same name.
int funcOrStruct(void);
struct funcOrStruct { int i; };

// Names from MacTypes.h that conflict with swift's library types.
// rdar://14175675
#define STDLIB_TEST(TYPE, NAME) extern NAME NAME##_test
#define STDLIB_TYPEDEF(TYPE, NAME) \
  typedef TYPE NAME; \
  STDLIB_TEST(TYPE, NAME)
STDLIB_TYPEDEF(__UINT8_TYPE__, UInt8);
STDLIB_TYPEDEF(__UINT16_TYPE__, UInt16);
STDLIB_TYPEDEF(__UINT32_TYPE__, UInt32);
STDLIB_TYPEDEF(__UINT64_TYPE__, UInt64);
STDLIB_TYPEDEF(float, Float32);
STDLIB_TYPEDEF(double, Float64);
STDLIB_TYPEDEF(long double, Float80);

// Other types from MacTypes.h.
STDLIB_TYPEDEF(__INT8_TYPE__, SInt8);
STDLIB_TYPEDEF(__INT16_TYPE__, SInt16);
STDLIB_TYPEDEF(__INT32_TYPE__, SInt32);
STDLIB_TYPEDEF(__INT64_TYPE__, SInt64);

typedef SInt32 OSStatus;

// Types from stdint.h.
#include <stdint.h>
STDLIB_TEST(__UINT8_TYPE__, uint8_t);
STDLIB_TEST(__UINT16_TYPE__, uint16_t);
STDLIB_TEST(__UINT32_TYPE__, uint32_t);
STDLIB_TEST(__UINT64_TYPE__, uint64_t);
STDLIB_TEST(__INT8_TYPE__, int8_t);
STDLIB_TEST(__INT16_TYPE__, int16_t);
STDLIB_TEST(__INT32_TYPE__, int32_t);
STDLIB_TEST(__INT64_TYPE__, int64_t);
STDLIB_TEST(__INTPTR_TYPE__, intptr_t);
STDLIB_TEST(__UINTPTR_TYPE__, uintptr_t);

// Types from stddef.h.
STDLIB_TYPEDEF(__PTRDIFF_TYPE__, ptrdiff_t);
STDLIB_TYPEDEF(__SIZE_TYPE__, size_t);
STDLIB_TYPEDEF(__SIZE_TYPE__, rsize_t);

// Types from sys/types.h (POSIX).
STDLIB_TYPEDEF(long, ssize_t);
STDLIB_TYPEDEF(__UINT8_TYPE__, u_int8_t);
STDLIB_TYPEDEF(__UINT16_TYPE__, u_int16_t);
STDLIB_TYPEDEF(__UINT32_TYPE__, u_int32_t);
STDLIB_TYPEDEF(__UINT64_TYPE__, u_int64_t);

// Types from libkern/OSTypes.h.
STDLIB_TYPEDEF(signed int, SInt);
STDLIB_TYPEDEF(unsigned int, UInt);

void noreturnFunction() __attribute__((noreturn));
void couldReturnFunction() __attribute__((noreturn));


//===---
// Function pointers
//===---

typedef int (*fptr)(int);
fptr getFunctionPointer(void);
void useFunctionPointer(fptr);

struct FunctionPointerWrapper {
  fptr a;
  fptr b;
};

typedef void (*fptr2)(int, long, void *);
fptr2 getFunctionPointer2(void);
void useFunctionPointer2(fptr2);

//===---
// Unions
//===---

union _GLKVector4 {
    struct { float x, y, z, w; };
    struct { float r, g, b, a; };
    struct { float s, t, p, q; };
    float v[4];
} __attribute__((aligned(16)));
typedef union _GLKVector4 GLKVector4;

typedef struct AnonUnion {
  union {
    float a;
    float b;
    float c;
    float d;
  };
  int x;
} AnonUnion;

//===---
// Bitfields
//===---

typedef struct ModRM {
  unsigned rm: 3;
  unsigned reg: 3;
  unsigned mod: 2;
  unsigned opcode;
} ModRM;

#endif
