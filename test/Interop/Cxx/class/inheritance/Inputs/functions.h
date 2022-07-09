struct NonTrivial {
  NonTrivial() {}
  ~NonTrivial() {}

  inline const char *inNonTrivial() const { return "NonTrivial::inNonTrivial"; }
  inline const char *inNonTrivialWithArgs(int a, int b) const {
    return "NonTrivial::inNonTrivialWithArgs";
  }
};

struct Base {
  inline const char *mutatingInBase() { return "Base::mutatingInBase"; }
  inline const char *constInBase() const { return "Base::constInBase"; }
  // TODO: if these are unnamed we hit an (unrelated) SILGen bug. Same for
  // subscripts.
  inline const char *takesArgsInBase(int a, int b, int c) const {
    return "Base::takesArgsInBase";
  }

  inline const char *takesNonTrivialInBase(NonTrivial a) const {
    return "Base::takesNonTrivialInBase";
  }
  inline NonTrivial returnsNonTrivialInBase() const { return NonTrivial{}; }

  template <class T>
  inline const char *templateInBase(T t) const {
    return "Base::templateInBase";
  }

  static const char *staticInBase() { return "Base::staticInBase"; }

  int renamed(int i) __attribute__((swift_name("swiftRenamed(input:)"))) {
    return i * 2;
  }

  void pure() const __attribute__((pure)) {}
};

struct OtherBase {
  inline const char *inOtherBase() const { return "OtherBase::inOtherBase"; }
  // TODO: test private access
};

struct Derived : Base, OtherBase {
  inline const char *inDerived() const { return "Derived::inDerived"; }
};

struct DerivedFromDerived : Derived {
  inline const char *topLevel() const { return "DerivedFromDerived::topLevel"; }
};

struct DerivedFromNonTrivial : NonTrivial {};

struct EmptyBaseClass {
  const char *inBase() const { return "EmptyBaseClass::inBase"; }
};

struct DerivedFromEmptyBaseClass : EmptyBaseClass {
  int a = 42;
  int b = 42;
};