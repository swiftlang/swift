#ifndef TEST_INTEROP_CXX_CLASS_INPUTS_CONSTRUCTORS_H
#define TEST_INTEROP_CXX_CLASS_INPUTS_CONSTRUCTORS_H

#ifndef __SIZE_T_DEFINED_CUSTOM__
#define __SIZE_T_DEFINED_CUSTOM__

#if defined(_WIN32) || defined(_WIN64)
typedef unsigned long long size_t;
#else
typedef unsigned long size_t;
#endif

#endif // __SIZE_T_DEFINED_CUSTOM__

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

namespace UserFactoriesForCXXRefTypeInit {
struct __attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:retain")))
__attribute__((swift_attr("release:release"))) ImportWithCtor {
  int value = 0;
  int param1 = 0;
  int param2 = 0;

  __attribute__((swift_name("init()"))) __attribute__((swift_attr(
      "returns_retained"))) static ImportWithCtor *_Nonnull create() {
    return new ImportWithCtor{1};
  }

  __attribute__((swift_name("init(_:)"))) __attribute__((swift_attr(
      "returns_retained"))) static ImportWithCtor *_Nonnull create(int x) {
    return new ImportWithCtor{1, x};
  }

  __attribute__((swift_name("init(_:_:)")))
  __attribute__((swift_attr("returns_retained"))) static ImportWithCtor
      *_Nonnull create(int x, int y) {
    return new ImportWithCtor{1, x, y};
  }

  __attribute__((swift_name("init(_:_:_:)")))
  __attribute__((swift_attr("returns_unretained"))) static ImportWithCtor
      *_Nonnull create(int x, int y, int z) {
    return new ImportWithCtor{0, x, y};
  }
};

class Value {
public:
  __attribute__((swift_name("init(x:)"))) static Value forFoo(int x) {
    Value ret;
    ret.x = x;
    return ret;
  }
  int getX() const { return x; }

private:
  int x;
};
} // namespace UserFactoriesForCXXRefTypeInit

inline void retain(UserFactoriesForCXXRefTypeInit::ImportWithCtor *_Nonnull x) {
  x->value++;
}

inline void
release(UserFactoriesForCXXRefTypeInit::ImportWithCtor *_Nonnull x) {
  if (!--x->value)
    delete x;
}

namespace SwiftInitSynthesisForCXXRefTypes {
struct __attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:Retain1")))
__attribute__((swift_attr("release:Release1"))) CompilerGeneratedDefaultCtor {
public:
  int val = 1;
};

struct __attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:Retain3"))) __attribute__((
    swift_attr("release:Release3"))) ExplicitCompilerGeneratedDefaultCtor {
public:
  int val = 1;
  ExplicitCompilerGeneratedDefaultCtor() = default;
};

struct __attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:immortal")))
__attribute__((swift_attr("release:immortal"))) ImmortalReference {
public:
  int val = 1;
};

struct __attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:Retain2")))
__attribute__((swift_attr("release:Release2"))) UserProvidedDefaultCtor {
public:
  int val = 1;
  UserProvidedDefaultCtor() { val = 2; }
};

struct __attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:Retain15")))
__attribute__((swift_attr("release:Release15"))) UserProvidedStaticFactory {
public:
  int val = 1;

  UserProvidedStaticFactory() = default;

  __attribute__((swift_name("init()"))) __attribute__((
      swift_attr("returns_retained"))) static UserProvidedStaticFactory
      *_Nonnull create() {
    UserProvidedStaticFactory *_Nonnull returnPtr =
        new UserProvidedStaticFactory();
    returnPtr->val = 2;
    return returnPtr;
  }

  __attribute__((swift_name("init(_:)"))) __attribute__((
      swift_attr("returns_retained"))) static UserProvidedStaticFactory
      *_Nonnull create(int x) {
    UserProvidedStaticFactory *returnPtr = new UserProvidedStaticFactory();
    returnPtr->val = x;
    return returnPtr;
  }
};

struct __attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:Retain4")))
__attribute__((swift_attr("release:Release4"))) PlacementOperatorNew {
public:
  int val = 1;
  void *operator new(size_t, void *ptr) { return ptr; }
};

struct __attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:Retain5")))
__attribute__((swift_attr("release:Release5"))) PrivateOperatorNew {
public:
  int val = 1;

private:
  void *operator new(size_t);
};

struct __attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:Retain6")))
__attribute__((swift_attr("release:Release6"))) ProtectedOperatorNew {
public:
  int val = 1;

protected:
  void *operator new(size_t);
};

struct __attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:Retain7")))
__attribute__((swift_attr("release:Release7"))) DeletedOperatorNew {
public:
  int val = 1;
  void *operator new(size_t) = delete;
};

struct __attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:Retain8")))
__attribute__((swift_attr("release:Release8"))) PrivateCtor {
private:
  int val = 1;
  PrivateCtor() {}
};

struct __attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:Retain9")))
__attribute__((swift_attr("release:Release9"))) ProtectedCtor {
protected:
  int val = 1;
  ProtectedCtor() {}
};

struct __attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:Retain10")))
__attribute__((swift_attr("release:Release10"))) DeletedCtor {
public:
  int val = 90;
  DeletedCtor() = delete;
};

struct __attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:Retain11")))
__attribute__((swift_attr("release:Release11"))) CtorWithDefaultArg {
public:
  int val = 1;
  CtorWithDefaultArg(int x = 2) : val(x) {}
};

struct __attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:Retain12"))) __attribute__((
    swift_attr("release:Release12"))) CtorWithDefaultAndNonDefaultArg {
public:
  int val;
  CtorWithDefaultAndNonDefaultArg(int x, int y = 42) : val(x + y) {}
};

struct __attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:Retain13")))
__attribute__((swift_attr("release:Release13"))) DefaulltAndNonDefaultCtors {
public:
  int val = 1;
  DefaulltAndNonDefaultCtors() = default;
  DefaulltAndNonDefaultCtors(int x) : val(x) {}
};

struct __attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:Retain14")))
__attribute__((swift_attr("release:Release14"))) ParameterizedCtor {
public:
  int val = 1;
  ParameterizedCtor(int x) : val(x) {}
};

struct __attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:Retain16")))
__attribute__((swift_attr("release:Release16"))) ParameterizedCtor2 {
public:
  int val1 = 1;
  int val2 = 1;
  ParameterizedCtor2() {}
  ParameterizedCtor2(int x) : val1(x) {}
  ParameterizedCtor2(int x, int y) : val1(x), val2(y) {}
};

struct __attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:Retain17")))
__attribute__((swift_attr("release:Release17"))) VariadicCtors {
public:
  int val1 = 1;
  int val2 = 2;
  VariadicCtors(...) {}
};

struct __attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:Retain18")))
__attribute__((swift_attr("release:Release18"))) NoIdentifierInCtorParam {
public:
  int val = 10;
  NoIdentifierInCtorParam(int) {}
};

struct cxxValTy {
public:
  int val;
  cxxValTy() {}
  cxxValTy(int x) { val = x; }
  ~cxxValTy() {}
};

struct __attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:Retain20")))
__attribute__((swift_attr("release:Release20"))) RValRefCtor2 {
public:
  cxxValTy val;
  RValRefCtor2(cxxValTy &&x) { val = x; }
};

struct __attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:Retain21")))
__attribute__((swift_attr("release:Release21"))) UserDefinedCopyCtor {
public:
  int val = 1;
  UserDefinedCopyCtor(int x) { val = x; };

  // User defined copy ctor
  UserDefinedCopyCtor(const UserDefinedCopyCtor &other) { val = other.val; }
};

struct __attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:Retain22")))
__attribute__((swift_attr("release:Release22"))) UserDefinedMoveCtor {
public:
  int val = 1;
  UserDefinedMoveCtor(int x) { val = x; };

  // User defined move constructor
  UserDefinedMoveCtor(UserDefinedMoveCtor &&other) noexcept {
    val = other.val;
    other.val = -1;
  }
};

struct __attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:Retain23")))
__attribute__((swift_attr("release:Release23"))) UserDefinedMoveAndCopyCtor {
public:
  int val = 1;
  UserDefinedMoveAndCopyCtor(int x) { val = x; };

  // User defined copy ctor
  UserDefinedMoveAndCopyCtor(const UserDefinedMoveAndCopyCtor &other) {
    val = other.val;
  }

  // User defined move constructor
  UserDefinedMoveAndCopyCtor(UserDefinedMoveAndCopyCtor &&other) noexcept {
    val = other.val;
    other.val = -1;
  }
};
} // namespace SwiftInitSynthesisForCXXRefTypes

void Retain1(SwiftInitSynthesisForCXXRefTypes::CompilerGeneratedDefaultCtor
                 *_Nonnull v) {}
void Release1(SwiftInitSynthesisForCXXRefTypes::CompilerGeneratedDefaultCtor
                  *_Nonnull v) {}
void Retain2(
    SwiftInitSynthesisForCXXRefTypes::UserProvidedDefaultCtor *_Nonnull v) {}
void Release2(
    SwiftInitSynthesisForCXXRefTypes::UserProvidedDefaultCtor *_Nonnull v) {}
void Retain3(
    SwiftInitSynthesisForCXXRefTypes::ExplicitCompilerGeneratedDefaultCtor
        *_Nonnull v) {}
void Release3(
    SwiftInitSynthesisForCXXRefTypes::ExplicitCompilerGeneratedDefaultCtor
        *_Nonnull v) {}
void Retain4(
    SwiftInitSynthesisForCXXRefTypes::PlacementOperatorNew *_Nonnull v) {}
void Release4(
    SwiftInitSynthesisForCXXRefTypes::PlacementOperatorNew *_Nonnull v) {}
void Retain5(SwiftInitSynthesisForCXXRefTypes::PrivateOperatorNew *_Nonnull v) {
};
void Release5(
    SwiftInitSynthesisForCXXRefTypes::PrivateOperatorNew *_Nonnull v) {}
void Retain6(
    SwiftInitSynthesisForCXXRefTypes::ProtectedOperatorNew *_Nonnull v) {}
void Release6(
    SwiftInitSynthesisForCXXRefTypes::ProtectedOperatorNew *_Nonnull v) {}
void Retain7(SwiftInitSynthesisForCXXRefTypes::DeletedOperatorNew *_Nonnull v) {
};
void Release7(
    SwiftInitSynthesisForCXXRefTypes::DeletedOperatorNew *_Nonnull v) {}
void Retain8(SwiftInitSynthesisForCXXRefTypes::PrivateCtor *_Nonnull v) {}
void Release8(SwiftInitSynthesisForCXXRefTypes::PrivateCtor *_Nonnull v) {}
void Retain9(SwiftInitSynthesisForCXXRefTypes::ProtectedCtor *_Nonnull v) {}
void Release9(SwiftInitSynthesisForCXXRefTypes::ProtectedCtor *_Nonnull v) {}
void Retain10(SwiftInitSynthesisForCXXRefTypes::DeletedCtor *_Nonnull v) {}
void Release10(SwiftInitSynthesisForCXXRefTypes::DeletedCtor *_Nonnull v) {}
void Retain11(
    SwiftInitSynthesisForCXXRefTypes::CtorWithDefaultArg *_Nonnull v) {}
void Release11(
    SwiftInitSynthesisForCXXRefTypes::CtorWithDefaultArg *_Nonnull v) {}
void Retain12(SwiftInitSynthesisForCXXRefTypes::CtorWithDefaultAndNonDefaultArg
                  *_Nonnull v) {}
void Release12(SwiftInitSynthesisForCXXRefTypes::CtorWithDefaultAndNonDefaultArg
                   *_Nonnull v) {}
void Retain13(
    SwiftInitSynthesisForCXXRefTypes::DefaulltAndNonDefaultCtors *_Nonnull v) {
};
void Release13(
    SwiftInitSynthesisForCXXRefTypes::DefaulltAndNonDefaultCtors *_Nonnull v) {
};
void Retain14(SwiftInitSynthesisForCXXRefTypes::ParameterizedCtor *_Nonnull v) {
};
void Release14(
    SwiftInitSynthesisForCXXRefTypes::ParameterizedCtor *_Nonnull v) {}
void Retain15(
    SwiftInitSynthesisForCXXRefTypes::UserProvidedStaticFactory *_Nonnull v) {}
void Release15(
    SwiftInitSynthesisForCXXRefTypes::UserProvidedStaticFactory *_Nonnull v) {}
void Retain16(
    SwiftInitSynthesisForCXXRefTypes::ParameterizedCtor2 *_Nonnull v) {}
void Release16(
    SwiftInitSynthesisForCXXRefTypes::ParameterizedCtor2 *_Nonnull v) {}
void Retain17(SwiftInitSynthesisForCXXRefTypes::VariadicCtors *_Nonnull v) {}
void Release17(SwiftInitSynthesisForCXXRefTypes::VariadicCtors *_Nonnull v) {}
void Retain18(
    SwiftInitSynthesisForCXXRefTypes::NoIdentifierInCtorParam *_Nonnull v) {}
void Release18(
    SwiftInitSynthesisForCXXRefTypes::NoIdentifierInCtorParam *_Nonnull v) {}
void Retain20(SwiftInitSynthesisForCXXRefTypes::RValRefCtor2 *_Nonnull v) {}
void Release20(SwiftInitSynthesisForCXXRefTypes::RValRefCtor2 *_Nonnull v) {}
void Retain21(
    SwiftInitSynthesisForCXXRefTypes::UserDefinedCopyCtor *_Nonnull v) {}
void Release21(
    SwiftInitSynthesisForCXXRefTypes::UserDefinedCopyCtor *_Nonnull v) {}
void Retain22(
    SwiftInitSynthesisForCXXRefTypes::UserDefinedMoveCtor *_Nonnull v) {}
void Release22(
    SwiftInitSynthesisForCXXRefTypes::UserDefinedMoveCtor *_Nonnull v) {}
void Retain23(
    SwiftInitSynthesisForCXXRefTypes::UserDefinedMoveAndCopyCtor *_Nonnull v) {
};
void Release23(
    SwiftInitSynthesisForCXXRefTypes::UserDefinedMoveAndCopyCtor *_Nonnull v) {
};

#endif // TEST_INTEROP_CXX_CLASS_INPUTS_CONSTRUCTORS_H
