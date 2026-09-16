void unsafe_c_function(void) __attribute__((swift_attr("unsafe")));

struct __attribute__((swift_attr("unsafe"))) UnsafeType {
  int field;
};

void print_ints(int *ptr, int count);

#define _CXX_INTEROP_STRINGIFY(_x) #_x

#define SWIFT_SHARED_REFERENCE(_retain, _release)                                \
  __attribute__((swift_attr("import_reference")))                          \
  __attribute__((swift_attr(_CXX_INTEROP_STRINGIFY(retain:_retain))))      \
  __attribute__((swift_attr(_CXX_INTEROP_STRINGIFY(release:_release))))

#define SWIFT_SAFE __attribute__((swift_attr("@safe")))
#define SWIFT_UNSAFE __attribute__((swift_attr("@unsafe")))

struct NoPointers {
  float x, y, z;
};

union NoPointersUnion {
  float x;
  double y;
};

struct NoPointersUnsafe {
  float x, y, z;
} SWIFT_UNSAFE;

// expected-note@+1 {{this type has unknown escapability: its member 'numbers' is a pointer or reference, and Swift cannot tell whether it owns what it points to}}
struct HasPointers {
  float *numbers;
};


// expected-note@+1 {{this type has unknown escapability: its member 'numbers' is a pointer or reference, and Swift cannot tell whether it owns what it points to}}
union HasPointersUnion {
  float *numbers;
  double x;
};

struct HasPointersSafe {
  float *numbers;
} SWIFT_SAFE;

struct RefCountedType {
  void *ptr;
} SWIFT_SHARED_REFERENCE(RCRetain, RCRelease);

struct RefCountedType *RCRetain(struct RefCountedType *object);
void RCRelease(struct RefCountedType *object);

struct HasRefCounted {
  struct RefCountedType *ref;
};

// expected-note@+1 {{this type has unknown escapability: its member 'next' is a pointer or reference, and Swift cannot tell whether it owns what it points to}}
struct ListNode {
  double data;
  struct ListNode *next;
};

enum SomeColor {
  SCRed,
  SCGreen,
  SCBlue
};
