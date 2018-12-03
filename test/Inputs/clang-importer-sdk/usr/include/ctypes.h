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

typedef struct {
  struct {
    int a;
    float b;
  } x;
  struct {
    float a;
    int b;
    struct {
      int c;
    } z;
  } y;
} UnnamedStructs;

typedef void const *HWND;

typedef struct {
  unsigned a;
  unsigned b[];
} StructWithFlexibleArray;

#include <stdarg.h>
extern void hasVaList(va_list args);

//===---
// Tag decls and typedefs.
//===---
/*!
  @keyword Foo1, Struct1
*/
struct FooStruct1 {
  /*!
  @keyword x, Struct1
  @recommended y
  */
  int x;
  /*!
  @keyword y, Struct1
  @recommendedover x
  */
  double y;
};

/*!
  @keyword Foo2
*/
typedef struct FooStruct2 {
  int x;
  double y;
} FooStructTypedef1;

typedef struct {
  int x;
  double y;
} FooStructTypedef2;

/*!
  @recommended Foo2, Foo1
*/
typedef struct FooStruct3 {
  int x;
  double y;
} FooStruct3;

/*!
  @recommendedover Foo3, Foo2
*/
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
/*!
  @recommendedover ro1, ro2, ro3, ro4
  @recommended r1, r2, r3
  @keyword k1, k2, k3, k4
*/
struct FooStruct6 {
  int x;
  double y;
};

struct StructWithForwardDeclaredStruct {
  struct ForwardDeclaredStruct *ptr;
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

typedef union IntOrFloat {
  int i;
  float f;
} IntOrFloat;

typedef struct NamedUnion {
  int a;
  int b;
  IntOrFloat intfloat;
} NamedUnion;

typedef struct AnonUnion {
  union {
    float a;
    float b;
    float c;
    float d;
  };
  int x;
} AnonUnion;

struct UnnamedUnion {
  union {
    int i;
    float f;
  } u;
};

//===---
// Bitfields
//===---

struct StructWithBitfields {
  unsigned First;
  unsigned Second : 17;
  unsigned Third : 5;
  unsigned : 11;
};

typedef struct ModRM {
  unsigned rm: 3;
  unsigned reg: 3;
  unsigned mod: 2;
  unsigned opcode;
} ModRM;

//===---
// Arrays
//===---
void useArray(char x[4], char y[], char z[][8]);
void staticBoundsArray(const char x[static 4]);

void useBigArray(char max_size[4096], char max_size_plus_one[4097]);
void useBigArray2d(char max_size[][4096], char max_size_plus_one[][4097]);

typedef const int FourConstInts[4];
void nonnullArrayParameters(const char x[_Nonnull], void * const _Nullable y[_Nonnull], _Nonnull FourConstInts z);
void nullableArrayParameters(const char x[_Nullable], void * const _Nullable y[_Nullable], _Nullable FourConstInts z);

typedef double real_t __attribute__((availability(swift,unavailable,message="use double")));

extern real_t realSin(real_t value);

#endif
