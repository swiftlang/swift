#define FRT_IMMORTAL                                                           \
  __attribute__((swift_attr("import_reference")))                              \
  __attribute__((swift_attr("retain:immortal")))                               \
  __attribute__((swift_attr("release:immortal")))

struct FRT_IMMORTAL Base {
  int baseValue;

  Base() : baseValue(1) {}
  Base(const Base &) = delete;

  int getBaseValue() const { return baseValue; }
  void setBaseValue(int v) { baseValue = v; }

  static Base &create() {
    static Base instance;
    return instance;
  }
};

struct FRT_IMMORTAL Derived : Base {
  int derivedValue;

  Derived() : derivedValue(2) {}
  Derived(const Derived &) = delete;

  int getDerivedValue() const { return derivedValue; }

  static Derived &create() {
    static Derived instance;
    return instance;
  }
};

struct FRT_IMMORTAL LeafDerived : Derived {
  int leafValue;

  LeafDerived() : leafValue(3) {}
  LeafDerived(const LeafDerived &) = delete;

  int getLeafValue() const { return leafValue; }

  static LeafDerived &create() {
    static LeafDerived instance;
    return instance;
  }
};

struct FRT_IMMORTAL Unrelated {
  int unrelatedValue;

  Unrelated() : unrelatedValue(99) {}
  Unrelated(const Unrelated &) = delete;

  static Unrelated &create() {
    static Unrelated instance;
    return instance;
  }
};

inline int getBaseValueFromBase(const Base &b) { return b.getBaseValue(); }

// Virtual inheritance: should NOT produce a Swift superclass relationship.
struct FRT_IMMORTAL VirtualDerived : virtual Base {
  int virtualDerivedValue;

  VirtualDerived() : virtualDerivedValue(4) {}
  VirtualDerived(const VirtualDerived &) = delete;

  int getVirtualDerivedValue() const { return virtualDerivedValue; }

  static VirtualDerived &create() {
    static VirtualDerived instance;
    return instance;
  }
};

struct RefCountedBase {
  int refCount = 1;
  int baseField = 10;

  RefCountedBase() = default;
  RefCountedBase(const RefCountedBase &) = delete;

  void retain() { refCount++; }
  void release() { refCount--; }

  int getBaseValue() const { return baseField; }

  static RefCountedBase &create() {
    static RefCountedBase instance;
    return instance;
  }
} __attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:.retain")))
__attribute__((swift_attr("release:.release")));

struct RefCountedDerived : RefCountedBase {
  int derivedField = 20;

  RefCountedDerived() = default;
  RefCountedDerived(const RefCountedDerived &) = delete;

  int getDerivedField() const { return derivedField; }

  static RefCountedDerived &create() {
    static RefCountedDerived instance;
    return instance;
  }
};

inline int getBaseValueFromRefCountedBase(const RefCountedBase &b) {
  return b.getBaseValue();
}

struct OverridesLifetimeOps : RefCountedDerived {
  int cVal = 3;
  void retainC() const {}
  void releaseC() const {}
} __attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:.retainC")))
__attribute__((swift_attr("release:.releaseC")));

struct OverridesLifetimeOpsDerived : OverridesLifetimeOps {
  int dVal = 4;
} __attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:.retainC")))
__attribute__((swift_attr("release:.releaseC")));
// FIXME: this redeclares the lifetime operations, since ClangImporter currently
// reports a conflict between annotations on base types otherwise

// Private inheritance: should NOT produce a Swift superclass relationship.
struct FRT_IMMORTAL PrivateDerived : private Base {
  int privateDerivedValue;

  PrivateDerived() : privateDerivedValue(5) {}
  PrivateDerived(const PrivateDerived &) = delete;

  int getPrivateDerivedValue() const { return privateDerivedValue; }

  static PrivateDerived &create() {
    static PrivateDerived instance;
    return instance;
  }
};

// Protected inheritance: should NOT produce a Swift superclass relationship.
struct FRT_IMMORTAL ProtectedDerived : protected Base {
  int protectedDerivedValue;

  ProtectedDerived() : protectedDerivedValue(6) {}
  ProtectedDerived(const ProtectedDerived &) = delete;

  int getProtectedDerivedValue() const { return protectedDerivedValue; }

  static ProtectedDerived &create() {
    static ProtectedDerived instance;
    return instance;
  }
};

struct EmptyTag {};

struct FRT_IMMORTAL DerivedFromEmptyAndBase : EmptyTag, Base {
  int extraValue;

  DerivedFromEmptyAndBase() : extraValue(7) {}
  DerivedFromEmptyAndBase(const DerivedFromEmptyAndBase &) = delete;

  int getExtraValue() const { return extraValue; }

  static DerivedFromEmptyAndBase &create() {
    static DerivedFromEmptyAndBase instance;
    return instance;
  }
};

template <typename Derived>
struct FRT_IMMORTAL CRTPBase {
  int crtpBaseValue;

  CRTPBase() : crtpBaseValue(10) {}

  Derived *derivedSelf() { return static_cast<Derived *>(this); }
};

struct FRT_IMMORTAL CRTPDerived : CRTPBase<CRTPDerived> {
  int crtpDerivedValue;

  CRTPDerived() : crtpDerivedValue(11) {}

  static CRTPDerived &create() {
    static CRTPDerived instance;
    return instance;
  }
};

using CRTPBaseOfDerived = CRTPBase<CRTPDerived>;
