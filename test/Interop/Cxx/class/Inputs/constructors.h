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

#endif // TEST_INTEROP_CXX_CLASS_INPUTS_CONSTRUCTORS_H
