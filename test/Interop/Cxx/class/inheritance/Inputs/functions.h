struct __attribute__((swift_attr("import_unsafe"))) NonTrivial {
  NonTrivial() {}
  NonTrivial(const NonTrivial &) {}
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
  inline const char *rvalueThisInBase() const&&
      __attribute__((swift_attr("import_unsafe"))) {
    return "Base::rvalueThisInBase";
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

  int pure() const __attribute__((pure)) { return 123; }

  inline int sameMethodNameSameSignature() const {
    return 42;
  }

  inline int sameMethodDifferentSignature() const {
    return 18;
  }
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

  inline int sameMethodNameSameSignature() const {
    return 21;
  }

  inline int sameMethodDifferentSignature(int x) const {
    return x + 1;
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

struct PrivatelyInherited : private Base {
};

struct ProtectedInherited : protected Base {
};

struct EmptyBaseClass {
  const char *inBase() const __attribute__((swift_attr("import_unsafe"))) {
    return "EmptyBaseClass::inBase";
  }
};

struct DerivedFromEmptyBaseClass : EmptyBaseClass {
  int a = 42;
  int b = 42;
};

int &getCopyCounter() {
    static int copyCounter = 0;
    return copyCounter;
}

class CopyTrackedBaseClass {
public:
    CopyTrackedBaseClass(int x) : x(x) {}
    CopyTrackedBaseClass(const CopyTrackedBaseClass &other) : x(other.x) {
        ++getCopyCounter();
    }

    int getX() const {
        return x;
    }
    int getXMut() {
        return x;
    }
private:
    int x;
};

class CopyTrackedDerivedClass: public CopyTrackedBaseClass {
public:
    CopyTrackedDerivedClass(int x) : CopyTrackedBaseClass(x) {}

    int getDerivedX() const {
        return getX();
    }
};

class NonEmptyBase {
public:
    int getY() const {
        return y;
    }
private:
    int y = 11;
};

class CopyTrackedDerivedDerivedClass: public NonEmptyBase, public CopyTrackedDerivedClass {
public:
    CopyTrackedDerivedDerivedClass(int x) : CopyTrackedDerivedClass(x) {}
};
