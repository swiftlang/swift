#ifndef TEST_INTEROP_CXX_CLASS_INPUTS_CONSTRUCTORS_H
#define TEST_INTEROP_CXX_CLASS_INPUTS_CONSTRUCTORS_H

struct ExplicitDefaultConstructor {
  ExplicitDefaultConstructor() : x(42) {}
  int x;
};

struct ImplicitDefaultConstructor {
  int x = 42;
};

struct DefaultedDefaultConstructor {
  int x = 42;
  DefaultedDefaultConstructor() = default;
};

struct MemberOfClassType {
  ImplicitDefaultConstructor member;
};

struct DefaultConstructorDeleted {
  DefaultConstructorDeleted() = delete;
  int &a;
};

struct ConstructorWithParam {
  ConstructorWithParam(int val) : x(val + 42) {}
  int x;
};

struct CopyAndMoveConstructor {
  CopyAndMoveConstructor(const CopyAndMoveConstructor &) = default;
  CopyAndMoveConstructor(CopyAndMoveConstructor &&) = default;

  int value = 123;
  int *ptr = nullptr;
};

struct Base {};

struct ArgType {
  int i = 42;
};

struct HasVirtualBase : public virtual Base {
  HasVirtualBase() = delete;
  HasVirtualBase(ArgType Arg) {}
  int i;
};

struct EmptyStruct {};

struct IntWrapper {
  int x;
};

struct TemplatedConstructor {
  ArgType value;

  template<class T>
  TemplatedConstructor(T value) : value(value) { }
};

struct TemplatedConstructorWithExtraArg {
  template<class T>
  TemplatedConstructorWithExtraArg(int, T value) { }
  template<class T>
  TemplatedConstructorWithExtraArg(T value, int) { }
  template<class T, class U>
  TemplatedConstructorWithExtraArg(T value, U other) { }
};

struct TemplatedCopyConstructor {
  int x = 0;

  TemplatedCopyConstructor(int x) : x(x) {}

  template <class T>
  TemplatedCopyConstructor(const T &value) : x(value.x) {}
};

struct TemplatedCopyConstructorWithExtraArg {
  int x = 0;

  TemplatedCopyConstructorWithExtraArg(int x) : x(x) {}

  template <class T>
  TemplatedCopyConstructorWithExtraArg(const T &value, int add = 0)
      : x(value.x + add) {}
};

struct __attribute__((swift_attr("import_unsafe")))
HasUserProvidedCopyConstructor {
  int numCopies;
  HasUserProvidedCopyConstructor(int numCopies = 0) : numCopies(numCopies) {}
  HasUserProvidedCopyConstructor(const HasUserProvidedCopyConstructor &other)
      : numCopies(other.numCopies + 1) {}
};

struct DeletedCopyConstructor {
  DeletedCopyConstructor(const DeletedCopyConstructor &) = delete;
};

#ifdef ENABLE_PTRAUTH
struct HasPtrAuthMember {
  void (*__ptrauth(1, 1, 3) handler)();
};
#endif

struct MoveConstructorWithOneParameterWithDefaultArg {
  int value;

  MoveConstructorWithOneParameterWithDefaultArg(int value) : value(value) {}

  MoveConstructorWithOneParameterWithDefaultArg(
      const MoveConstructorWithOneParameterWithDefaultArg &) = delete;

  MoveConstructorWithOneParameterWithDefaultArg(
      MoveConstructorWithOneParameterWithDefaultArg &&other =
          MoveConstructorWithOneParameterWithDefaultArg{0})
      : value(other.value + 1) {}
};

struct __attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:retain")))
__attribute__((swift_attr("release:release"))) ImportWithCtor {
  int value = 0;
  int param1 = 0;
  int param2 = 0;

  __attribute__((swift_name("init()")))
  __attribute__((swift_attr("returns_retained")))
  static ImportWithCtor * _Nonnull create() {
    return new ImportWithCtor{1};
  }

  __attribute__((swift_name("init(_:)")))
  __attribute__((swift_attr("returns_retained")))
  static ImportWithCtor * _Nonnull create(int x) {
    return new ImportWithCtor{1, x};
  }

  __attribute__((swift_name("init(_:_:)")))
  __attribute__((swift_attr("returns_retained")))
  static ImportWithCtor * _Nonnull create(int x, int y) {
    return new ImportWithCtor{1, x, y};
  }

  __attribute__((swift_name("init(_:_:_:)")))
  __attribute__((swift_attr("returns_unretained")))
  static ImportWithCtor * _Nonnull create(int x, int y, int z) {
    return new ImportWithCtor{0, x, y};
  }
};

inline void retain(ImportWithCtor * _Nonnull x) {
  x->value++;
}

inline void release(ImportWithCtor * _Nonnull x) {
  if (!--x->value)
    delete x;
}

class Value {
public:
  __attribute__((swift_name("init(x:)")))
  static Value forFoo(int x) {
      Value ret;
      ret.x = x;
      return ret; 
  }
  int getX() const { return x; }
private:
  int x;
};

namespace TrivialCtor {
struct __attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:Retain1")))
__attribute__((swift_attr("release:Release1"))) CxxRefTy {
public:
  int val = 1;
};
} // namespace TrivialCtor

void Retain1(TrivialCtor::CxxRefTy *_Nonnull v) {};
void Release1(TrivialCtor::CxxRefTy *_Nonnull v) {};

namespace UserProvidedDefaultCtor {
struct __attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:Retain2")))
__attribute__((swift_attr("release:Release2"))) CxxRefTy {
public:
  int val = 1;
  CxxRefTy() { val = 2; }
};
} // namespace UserProvidedDefaultCtor

void Retain2(UserProvidedDefaultCtor::CxxRefTy *_Nonnull v) {};
void Release2(UserProvidedDefaultCtor::CxxRefTy *_Nonnull v) {};

namespace ExplicitCompilerGeneratedDefaultCtor {
struct __attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:Retain3")))
__attribute__((swift_attr("release:Release3"))) CxxRefTy {
public:
  int val = 1;
  CxxRefTy() = default;
};
} // namespace ExplicitCompilerGeneratedDefaultCtor

void Retain3(ExplicitCompilerGeneratedDefaultCtor::CxxRefTy *_Nonnull v) {};
void Release3(ExplicitCompilerGeneratedDefaultCtor::CxxRefTy *_Nonnull v) {};

namespace PlacementOperatorNew {
struct __attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:Retain4")))
__attribute__((swift_attr("release:Release4"))) CxxRefTy { // expected-error {{no matching function for call to 'operator new'}}
public:
  int val = 1;
  void *operator new(unsigned long, void *ptr) { return ptr; } // expected-note {{candidate function not viable: requires 2 arguments, but 1 was provided}}
};
} // namespace PlacementOperatorNew

void Retain4(PlacementOperatorNew::CxxRefTy *_Nonnull v) {};
void Release4(PlacementOperatorNew::CxxRefTy *_Nonnull v) {};

namespace PrivateOperatorNew {
struct __attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:Retain5")))
__attribute__((swift_attr("release:Release5"))) CxxRefTy { // expected-error {{'operator new' is a private member of 'PrivateOperatorNew::CxxRefTy}}
public:
  int val = 1;

private:
  void *operator new(unsigned long); // expected-note {{declared private here}}
};
} // namespace PrivateOperatorNew

void Retain5(PrivateOperatorNew::CxxRefTy *_Nonnull v) {};
void Release5(PrivateOperatorNew::CxxRefTy *_Nonnull v) {};

namespace ProtectedOperatorNew {
struct __attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:Retain6")))
__attribute__((swift_attr("release:Release6"))) CxxRefTy { // expected-error {{'operator new' is a protected member of 'ProtectedOperatorNew::CxxRefTy'}}
public:
  int val = 1;

protected:
  void *operator new(unsigned long); // expected-note {{declared protected here}}
};
} // namespace ProtectedOperatorNew

void Retain6(ProtectedOperatorNew::CxxRefTy *_Nonnull v) {};
void Release6(ProtectedOperatorNew::CxxRefTy *_Nonnull v) {};

namespace DeletedOperatorNew {
struct __attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:Retain7")))
__attribute__((swift_attr("release:Release7"))) CxxRefTy { // expected-error {{call to deleted function 'operator new'}}
public:
  int val = 1;
  void *operator new(unsigned long) = delete; // expected-note {{candidate function has been explicitly deleted}}
};
} // namespace DeletedOperatorNew

void Retain7(DeletedOperatorNew::CxxRefTy *_Nonnull v) {};
void Release7(DeletedOperatorNew::CxxRefTy *_Nonnull v) {};

namespace PrivateCtor {
struct __attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:Retain8")))
__attribute__((swift_attr("release:Release8"))) CxxRefTy {
private:
  int val = 1;
  CxxRefTy() {}
};
} // namespace PrivateCtor

void Retain8(PrivateCtor::CxxRefTy *_Nonnull v) {};
void Release8(PrivateCtor::CxxRefTy *_Nonnull v) {};

namespace ProtectedCtor {
struct __attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:Retain9")))
__attribute__((swift_attr("release:Release9"))) CxxRefTy {
protected:
  int val = 1;
  CxxRefTy() {}
};
} // namespace ProtectedCtor

void Retain9(ProtectedCtor::CxxRefTy *_Nonnull v) {};
void Release9(ProtectedCtor::CxxRefTy *_Nonnull v) {};

namespace DeletedCtor {
struct __attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:Retain10")))
__attribute__((swift_attr("release:Release10"))) CxxRefTy {
public:
  int val = 90;
  CxxRefTy() = delete;
};
} // namespace DeletedCtor

void Retain10(DeletedCtor::CxxRefTy *_Nonnull v) {};
void Release10(DeletedCtor::CxxRefTy *_Nonnull v) {};

namespace CtorWithDefaultArg {
struct __attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:Retain11")))
__attribute__((swift_attr("release:Release11"))) CxxRefTy {
public:
  int val = 1;
  CxxRefTy(int x = 2) : val(x) {}
};
} // namespace CtorWithDefaultArg

void Retain11(CtorWithDefaultArg::CxxRefTy *_Nonnull v) {};
void Release11(CtorWithDefaultArg::CxxRefTy *_Nonnull v) {};

namespace CtorWithDefaultAndNonDefaultArg {
struct __attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:Retain12")))
__attribute__((swift_attr("release:Release12"))) CxxRefTy {
public:
  int val;
  CxxRefTy(int x, int y = 42) : val(x + y) {}
};
} // namespace CtorWithDefaultAndNonDefaultArg

void Retain12(CtorWithDefaultAndNonDefaultArg::CxxRefTy *_Nonnull v) {};
void Release12(CtorWithDefaultAndNonDefaultArg::CxxRefTy *_Nonnull v) {};

namespace DefaulltAndNonDefaultCtors {
struct __attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:Retain13")))
__attribute__((swift_attr("release:Release13"))) CxxRefTy {
public:
  int val = 1;
  CxxRefTy() = default;
  CxxRefTy(int x) : val(x) {}
};
} // namespace DefaulltAndNonDefaultCtors

void Retain13(DefaulltAndNonDefaultCtors::CxxRefTy *_Nonnull v) {};
void Release13(DefaulltAndNonDefaultCtors::CxxRefTy *_Nonnull v) {};

namespace ParameterisedCtor {
struct __attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:Retain14")))
__attribute__((swift_attr("release:Release14"))) CxxRefTy {
public:
  int val = 1;
  CxxRefTy(int x) : val(x) {}
};
} // namespace ParameterisedCtor

void Retain14(ParameterisedCtor::CxxRefTy *_Nonnull v) {};
void Release14(ParameterisedCtor::CxxRefTy *_Nonnull v) {};

namespace UserProvidedStaticFactory {
struct __attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:Retain15")))
__attribute__((swift_attr("release:Release15"))) CxxRefTy {
public:
  int val = 1;

  CxxRefTy() = default;

  __attribute__((swift_name("init()")))
  __attribute__((swift_attr("returns_retained")))
  static CxxRefTy *_Nonnull create() {
    CxxRefTy *_Nonnull returnPtr = new CxxRefTy();
    returnPtr->val = 2;
    return returnPtr;
  }

  __attribute__((swift_name("init(_:)")))
  __attribute__((swift_attr("returns_retained")))
  static CxxRefTy *_Nonnull create(int x) {
    CxxRefTy *returnPtr = new CxxRefTy();
    returnPtr->val = x;
    return returnPtr;
  }
};
} // namespace UserProvidedStaticFactory

void Retain15(UserProvidedStaticFactory::CxxRefTy *_Nonnull v) {};
void Release15(UserProvidedStaticFactory::CxxRefTy *_Nonnull v) {};

// TODO: Avoid synthesizing factories for immortal references to prevent
// "returns_(un)retained" errors and ensure "cannot be constructed" error
// instead.
namespace ImmortalReference {
struct __attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:immortal")))
__attribute__((swift_attr("release:immortal")))
CxxRefTy { //  expected-error {{'__returns_CxxRefTy' cannot be annotated with either SWIFT_RETURNS_RETAINED or SWIFT_RETURNS_UNRETAINED because it is not returning a SWIFT_SHARED_REFERENCE type}}
public:
  int val = 1;
  };
  } // namespace ImmortalReference

#endif // TEST_INTEROP_CXX_CLASS_INPUTS_CONSTRUCTORS_H
