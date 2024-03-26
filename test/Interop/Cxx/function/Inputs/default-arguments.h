inline void unused(int x = 0) {}

inline bool isZero(int value = 0) { return value == 0; }
inline bool isNonZero(int value = 0) { return value != 0; }

inline bool isNil(int *ptr = nullptr) { return ptr == nullptr; }
inline bool isStrNil(const char *ptr = "") { return ptr == nullptr; }

static int *globalPtr = nullptr;
inline bool isGlobalNonNil(int *ptr = globalPtr) { return ptr; }

inline int sum(int a, int b = 1) { return a + b; }
inline int subtract(int a = 123, int b = 1) { return a - b; }

static int counter = 0;
inline bool isZeroCounter(int value = counter++) { return value == 0; }

struct ArgTy {
  int value;

  static ArgTy createNonZero() { return {1}; }
  static ArgTy createZero();
};
struct ArgTyNonPOD {
  int value;

  static ArgTyNonPOD createNonZero() { return {1}; }
  ~ArgTyNonPOD() {}
};

struct ArgTyView {
  ArgTy *ptr;
};
struct __attribute__((swift_attr("import_owned"))) ArgTyOwnedPtr {
  ArgTy *ptr;
};

struct __attribute__((swift_attr("import_reference"),
                      swift_attr("retain:immortal"),
                      swift_attr("release:immortal"))) ArgFRT {
  int value;
  ArgFRT(int value) : value(value) {}
};
static ArgFRT *globalFRT;
inline void createFRT() { globalFRT = new ArgFRT(123); }
inline void deleteFRT() { delete globalFRT; }

struct __attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:retainFn")))
__attribute__((swift_attr("release:releaseFn"))) ArgRefCounted {
  int value;
  int refCount = 1;
  ArgRefCounted(int value) : value(value) {}
  static ArgRefCounted *create() { return new ArgRefCounted(321); }
};
inline void retainFn(ArgRefCounted *a) { a->refCount++; }
inline void releaseFn(ArgRefCounted *a) { a->refCount--; }

inline bool isArgZero(ArgTy a = {0}) { return a.value == 0; }
inline bool isArgNonZero(ArgTy a = ArgTy::createNonZero()) { return a.value != 0; }
inline bool isArgZeroOutOfLine(ArgTy a = ArgTy::createZero()) { return a.value == 0; }

inline bool isArgZeroConstRef(const ArgTy &a = {0}) { return a.value == 0; }
inline bool isArgNonZeroConstRef(const ArgTy &a = ArgTy::createNonZero()) { return a.value == 0; }
inline bool isArgNonPODNonZeroConstRef(const ArgTyNonPOD &a = ArgTyNonPOD::createNonZero()) { return a.value == 0; }

inline bool isArgViewNull(ArgTyView a = {nullptr}) { return a.ptr == nullptr; }
inline bool isArgViewNullAnd(ArgTyView a = {nullptr}, bool second = true) { return a.ptr == nullptr && second; }
inline bool isArgViewNullAndReversed(bool second = true, ArgTyView a = {nullptr}) { return a.ptr == nullptr && second; }
inline bool isArgViewNullUnsafeParam(__attribute__((swift_attr("import_unsafe"))) ArgTyView a = {nullptr}) { return a.ptr == nullptr; }
inline bool isArgViewNullUnsafeFunc(ArgTyView a = {nullptr}) __attribute__((swift_attr("import_unsafe"))) { return a.ptr == nullptr; }
inline bool isArgOwnedPtrNull(ArgTyOwnedPtr a = {nullptr}) { return a.ptr == nullptr; }
inline bool isArgFRTNull(ArgFRT *a = nullptr) { return a == nullptr; }
inline int getArgFRTValue(ArgFRT *a = globalFRT) { return a->value; }
inline int getArgRefCountedValue(ArgRefCounted *a = ArgRefCounted::create()) { return a->value; }

struct HasMethodWithDefaultArg {
  bool isZero(int v = 0) const { return v == 0; }
  bool isNonZero(int v = sizeof(ArgTy)) const { return v != 0; }
  bool isNilPtr(int *v = nullptr) const { return v == nullptr; }
  bool isNilConstPtr(const int *v = nullptr) const { return v == nullptr; }
  bool isZeroConstRef(const int &v = 0) const { return v == 0; }
};

struct DerivedFromHasMethodWithDefaultArg : public HasMethodWithDefaultArg {};
struct DerivedFromDerivedFromHasMethodWithDefaultArg : public DerivedFromHasMethodWithDefaultArg {};

struct HasStaticMethodWithDefaultArg {
  static int value;
  static bool isNonZero(int v = value);

  static int counter;
  static bool isNonZeroCounter(int v = counter++);

private:
  static int privateCounter;
public:
  static bool isNonZeroPrivateCounter(int v = privateCounter++);

  // This should not get a default value in Swift:
  static ArgTy &customTy;
  static bool isArgZeroRef(ArgTy &a = customTy) { return a.value == 0; }
};

// TODO: support default arguments of constructors
// (https://github.com/apple/swift/issues/70124)
struct HasCtorWithDefaultArg {
  int value;

  HasCtorWithDefaultArg(int a, int b = 456, int c = 123) : value(a + b + c) {}
};

template <typename T>
struct TemplatedHasMethodWithDefaultArg {
  bool isZero(T v = 0) const { return v == 0; }
  bool isNonZero(T v = T()) const { return v != 0; }
};
typedef TemplatedHasMethodWithDefaultArg<int> TemplatedHasMethodWithDefaultArgInt;
typedef TemplatedHasMethodWithDefaultArg<float> TemplatedHasMethodWithDefaultArgFloat;

struct DerivedFromTemplatedHasMethodWithDefaultArgFloat : TemplatedHasMethodWithDefaultArgFloat {};

inline int ambiguous(int a, int b = 1) { return a; }
inline int ambiguous(int a) { return a; }

inline int nonTrailing(int a, int b = 2) { return a + b; }
inline int nonTrailing(int a = 1, int b);

// MARK: - Un-instantiatable default expressions

class NoDefinition;

template <typename A>
struct Base {};

template <typename A>
struct RequiresDef : Base<A> {
  A value;
};

template <typename T>
void invalidDefaultExpr(Base<T> x = RequiresDef<T>()) {}

template <typename T>
struct InvalidStruct {
  void invalidDefaultExprMethod(Base<T> x = RequiresDef<T>()) const {}
};
typedef InvalidStruct<NoDefinition> InvalidStructNoDef;
