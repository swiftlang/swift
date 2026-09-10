#ifndef TEST_INTEROP_CXX_FOREIGN_REFERENCE_INPUTS_CDECL_H
#define TEST_INTEROP_CXX_FOREIGN_REFERENCE_INPUTS_CDECL_H

struct Shared;

#ifdef __cplusplus
extern "C" {
#endif

void retainShared(struct Shared *_Nonnull);
void releaseShared(struct Shared *_Nonnull);

struct __attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:immortal")))
__attribute__((swift_attr("release:immortal"))) Immortal {
  int value;
};

struct __attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:retainShared")))
__attribute__((swift_attr("release:releaseShared"))) Shared {
  int value;
  int refCount;
};

struct Immortal *_Nonnull getImmortal(void);
struct Shared *_Nonnull makeShared(int value)
    __attribute__((swift_attr("returns_retained")));
int sharedRefCount(struct Shared *_Nonnull s);

// Implemented in Swift using '@c @implementation'.
void CImplTakesImmortal(struct Immortal *_Nonnull value);
struct Immortal *_Nonnull CImplReturnsImmortal(void);
int CImplGetSharedValue(struct Shared *_Nonnull s);

// Calls the Swift implementations above through the C ABI.
int callSwiftImplementations(struct Shared *_Nonnull s);

#ifdef __cplusplus
}
#endif

#endif // TEST_INTEROP_CXX_FOREIGN_REFERENCE_INPUTS_CDECL_H
