struct __attribute__((swift_attr("import_unsafe"))) NonTrivial {
  NonTrivial() {}
  ~NonTrivial() {}

  inline const char *inNonTrivial() const
      __attribute__((swift_attr("import_unsafe"))) {
    return "NonTrivial::inNonTrivial";
  }
  inline const char *inNonTrivialWithArgs(int a, int b) const
      __attribute__((swift_attr("import_unsafe"))) {
    return "NonTrivial::inNonTrivialWithArgs";
  }
};

struct Base {
  inline const char *mutatingInBase()
      __attribute__((swift_attr("import_unsafe"))) {
    return "Base::mutatingInBase";
  }
  inline const char *constInBase() const
      __attribute__((swift_attr("import_unsafe"))) {
    return "Base::constInBase";
  }
  // TODO: if these are unnamed we hit an (unrelated) SILGen bug. Same for
  // subscripts.
  inline const char *takesArgsInBase(int a, int b, int c) const
      __attribute__((swift_attr("import_unsafe"))) {
    return "Base::takesArgsInBase";
  }

  inline const char *takesNonTrivialInBase(NonTrivial a) const
      __attribute__((swift_attr("import_unsafe"))) {
    return "Base::takesNonTrivialInBase";
  }
  inline NonTrivial returnsNonTrivialInBase() const { return NonTrivial{}; }

  template <class T>
  inline const char *templateInBase(T t) const
      __attribute__((swift_attr("import_unsafe"))) {
    return "Base::templateInBase";
  }

  static const char *staticInBase()
      __attribute__((swift_attr("import_unsafe"))) {
    return "Base::staticInBase";
  }

  int renamed(int i) __attribute__((swift_name("swiftRenamed(input:)"))) {
    return i * 2;
  }

  void pure() const __attribute__((pure)) {}
};

struct OtherBase {
  inline const char *inOtherBase() const
      __attribute__((swift_attr("import_unsafe"))) {
    return "OtherBase::inOtherBase";
  }
  // TODO: test private access
};

struct Derived : Base, OtherBase {
  inline const char *inDerived() const
      __attribute__((swift_attr("import_unsafe"))) {
    return "Derived::inDerived";
  }
};

struct DerivedFromDerived : Derived {
  inline const char *topLevel() const
      __attribute__((swift_attr("import_unsafe"))) {
    return "DerivedFromDerived::topLevel";
  }
};

struct __attribute__((swift_attr("import_unsafe"))) DerivedFromNonTrivial
    : NonTrivial {};

struct EmptyBaseClass {
  const char *inBase() const __attribute__((swift_attr("import_unsafe"))) {
    return "EmptyBaseClass::inBase";
  }
};

struct DerivedFromEmptyBaseClass : EmptyBaseClass {
  int a = 42;
  int b = 42;
};