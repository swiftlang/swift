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

#endif // TEST_INTEROP_CXX_CLASS_INPUTS_CONSTRUCTORS_H
