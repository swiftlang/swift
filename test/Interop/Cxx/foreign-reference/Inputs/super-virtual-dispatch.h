#define FRT_IMMORTAL                                                           \
  __attribute__((swift_attr("import_reference")))                              \
  __attribute__((swift_attr("retain:immortal")))                               \
  __attribute__((swift_attr("release:immortal")))

struct FRT_IMMORTAL Base {
  virtual int virtualMethod() const { return 100; }
  virtual ~Base() = default;

  static Base &create() {
    static Base instance;
    return instance;
  }
};

struct FRT_IMMORTAL Derived : Base {
  int virtualMethod() const override { return 200; }

  static Derived &create() {
    static Derived instance;
    return instance;
  }
};

struct FRT_IMMORTAL LeafDerived : Derived {
  int virtualMethod() const override { return 300; }

  static LeafDerived &create() {
    static LeafDerived instance;
    return instance;
  }
};

struct FRT_IMMORTAL DerivedNoOverride : Base {
  static DerivedNoOverride &create() {
    static DerivedNoOverride instance;
    return instance;
  }
};

struct FRT_IMMORTAL LeafOverNoOverride : DerivedNoOverride {
  int virtualMethod() const override { return 400; }

  static LeafOverNoOverride &create() {
    static LeafOverNoOverride instance;
    return instance;
  }
};

struct UnrelatedBase {
  int unrelatedMethod() const { return 999; }
};

struct FRT_IMMORTAL DerivedWithUnrelatedBase : Base, UnrelatedBase {
  int virtualMethod() const override { return 500; }

  static DerivedWithUnrelatedBase &create() {
    static DerivedWithUnrelatedBase instance;
    return instance;
  }
};

struct SecondBaseWithSameMethod {
  virtual int virtualMethod() const { return 700; }
  virtual ~SecondBaseWithSameMethod() = default;
};

struct FRT_IMMORTAL DerivedWithConflictingBase : Base, SecondBaseWithSameMethod {
  int virtualMethod() const override { return 600; }

  static DerivedWithConflictingBase &create() {
    static DerivedWithConflictingBase instance;
    return instance;
  }
};

struct FRT_IMMORTAL AbstractBase {
  virtual int pureMethod() const = 0;
  virtual ~AbstractBase() = default;
};

struct FRT_IMMORTAL ConcreteDerived : AbstractBase {
  int pureMethod() const override { return 42; }

  static ConcreteDerived &create() {
    static ConcreteDerived instance;
    return instance;
  }
};

struct FRT_IMMORTAL VirtuallyDerived : virtual Base {
  int virtualMethod() const override { return 800; }

  static VirtuallyDerived &create() {
    static VirtuallyDerived instance;
    return instance;
  }
};
