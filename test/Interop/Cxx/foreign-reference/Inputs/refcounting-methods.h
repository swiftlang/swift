#include <swift/bridging>

struct RefCountedBox {
  int value;
  int refCount = 1;

  SWIFT_RETURNS_RETAINED
  RefCountedBox(int value) : value(value) {}

  void doRetain() { refCount++; }
  void doRelease() { refCount--; }
} SWIFT_SHARED_REFERENCE(.doRetain, .doRelease);

struct DerivedRefCountedBox : RefCountedBox {
  int secondValue = 1;
  SWIFT_RETURNS_RETAINED
  DerivedRefCountedBox(int value, int secondValue)
      : RefCountedBox(value), secondValue(secondValue) {}
};

// MARK: Retain in a base type, release in derived

struct BaseHasRetain {
  mutable int refCount = 1;
  void doRetainInBase() const { refCount++; }
};

struct DerivedHasRelease : BaseHasRetain {
  int value;
  SWIFT_RETURNS_RETAINED
  DerivedHasRelease(int value) : value(value) {}

  void doRelease() const { refCount--; }
} SWIFT_SHARED_REFERENCE(.doRetainInBase, .doRelease);

// MARK: Retain in a base type, release in templated derived

template <typename T>
struct TemplatedDerivedHasRelease : BaseHasRetain {
  T value;
  SWIFT_RETURNS_RETAINED
  TemplatedDerivedHasRelease(T value) : value(value) {}

  void doReleaseTemplated() const { refCount--; }
} SWIFT_SHARED_REFERENCE(.doRetainInBase, .doReleaseTemplated);

using TemplatedDerivedHasReleaseFloat = TemplatedDerivedHasRelease<float>;
using TemplatedDerivedHasReleaseInt = TemplatedDerivedHasRelease<int>;

// MARK: Retain/release in CRTP base type

template <typename Derived>
struct CRTPBase {
  mutable int refCount = 1;
  void crtpRetain() const { refCount++; }
  void crtpRelease() const { refCount--; }
} SWIFT_SHARED_REFERENCE(.crtpRetain, .crtpRelease);

struct CRTPDerived : CRTPBase<CRTPDerived> {
  int value;
  SWIFT_RETURNS_RETAINED
  CRTPDerived(int value) : value(value) {}
};

// MARK: Release in CRTP base type deletes the object

// The release operation is a member of a class template specialization that
// is only instantiated when Swift uses it.
template <typename Derived>
struct CRTPDeletingBase {
  void operator delete(void *ptr) {
    ++deleteCount;
    ::operator delete(ptr);
  }

  void crtpRetain() const { ++refCount; }
  void crtpRelease() const {
    if (!--refCount)
      delete static_cast<const Derived *>(this);
  }

  static inline int deleteCount = 0;

private:
  mutable int refCount = 1;
} SWIFT_SHARED_REFERENCE(.crtpRetain, .crtpRelease);

struct CRTPDeletingDerived : CRTPDeletingBase<CRTPDeletingDerived> {
  int value;

  // Use the global allocation function: a class-specific new-expression would
  // make Clang instantiate the class-specific `operator delete` on its own.
  static CRTPDeletingDerived *create(int value) SWIFT_RETURNS_RETAINED {
    return ::new CRTPDeletingDerived(value);
  }

  static int getDeleteCount() { return deleteCount; }

private:
  CRTPDeletingDerived(int value) : value(value) {}
};

// MARK: Virtual retain and release

struct VirtualRetainRelease {
  int value;
  mutable int refCount = 1;
  mutable bool calledBase = false;
  SWIFT_RETURNS_RETAINED
  VirtualRetainRelease(int value) : value(value) {}

  virtual void doRetainVirtual() const { refCount++; calledBase = true; }
  virtual void doReleaseVirtual() const { refCount--; calledBase = true; }
  virtual ~VirtualRetainRelease() = default;
} SWIFT_SHARED_REFERENCE(.doRetainVirtual, .doReleaseVirtual);

struct DerivedVirtualRetainRelease : VirtualRetainRelease {
  SWIFT_RETURNS_RETAINED
  DerivedVirtualRetainRelease(int value) : VirtualRetainRelease(value) {}

  void doRetainVirtual() const override { refCount++; }
  void doReleaseVirtual() const override { refCount--; }
};

// MARK: Pure virtual retain and release

struct PureVirtualRetainRelease {
  int value;
  SWIFT_RETURNS_RETAINED
  PureVirtualRetainRelease(int value) : value(value) {}

  virtual void doRetainPure() const = 0;
  virtual void doReleasePure() const = 0;
  virtual ~PureVirtualRetainRelease() = default;
} SWIFT_SHARED_REFERENCE(.doRetainPure, .doReleasePure);

struct DerivedPureVirtualRetainRelease : PureVirtualRetainRelease {
  mutable int refCount = 1;

  SWIFT_RETURNS_RETAINED
  DerivedPureVirtualRetainRelease(int value) : PureVirtualRetainRelease(value) {}
  void doRetainPure() const override { refCount++; }
  void doReleasePure() const override { refCount--; }
};

// MARK: Static retain/release
#ifdef INCORRECT
struct StaticRetainRelease {
// expected-error@-1 {{specified retain function '.staticRetain' is a static function; expected an instance function}}
// expected-error@-2 {{specified release function '.staticRelease' is a static function; expected an instance function}}
  int value;
  int refCount = 1;

  SWIFT_RETURNS_RETAINED
  StaticRetainRelease(int value) : value(value) {}

  static void staticRetain(StaticRetainRelease* o) { o->refCount++; }
  static void staticRelease(StaticRetainRelease* o) { o->refCount--; }
} SWIFT_SHARED_REFERENCE(.staticRetain, .staticRelease);

struct DerivedStaticRetainRelease : StaticRetainRelease {
// expected-error@-1 {{specified release function '.staticRelease' is a static function; expected an instance function}}
// expected-error@-2 {{specified retain function '.staticRetain' is a static function; expected an instance function}}
  int secondValue = 1;
  SWIFT_RETURNS_RETAINED
  DerivedStaticRetainRelease(int value, int secondValue)
      : StaticRetainRelease(value), secondValue(secondValue) {}
};

struct SharedA {
  void doRetain();
  void doRelease();
} SWIFT_SHARED_REFERENCE(.doRetain, .doRelease);

struct SharedB {
  void doRetain();
  void doRelease();
} SWIFT_SHARED_REFERENCE(.doRetain, .doRelease);

// expected-warning@+1 {{unable to infer SWIFT_SHARED_REFERENCE}}
struct SharedAB : SharedA, SharedB { SharedAB(int) {} };

// MARK: Mixed immortal / non-immortal retain/release
struct MixedImmortalRetainRelease {
// expected-error@-1 {{reference type 'MixedImmortalRetainRelease' must mark both or neither of its retain and release operations as immortal}}
  int value;
  void doRelease() const {}
} SWIFT_SHARED_REFERENCE(immortal, doRelease);
// expected-note@-1 {{retain and release functions specified on 'MixedImmortalRetainRelease'}}

// A type that inherits its FRT-ness from a base with a mixed annotation is
// itself invalid (in addition to the base's own diagnostic above).
// expected-warning@+1 {{unable to infer SWIFT_SHARED_REFERENCE for 'DerivedFromMixedImmortal', although one of its transitive base types is marked as SWIFT_SHARED_REFERENCE}}
struct DerivedFromMixedImmortal : MixedImmortalRetainRelease {
  DerivedFromMixedImmortal(int) {}
};
#endif

// Multiple retain/release operations,
// but we disambiguate them by arity.
struct AmbiguousReleaseMethods {
  int value;

  SWIFT_RETURNS_RETAINED
  AmbiguousReleaseMethods(int value) : value(value) {}

  virtual void doRetain();
  virtual void doRelease();
  virtual void doRelease(int argument);
} SWIFT_SHARED_REFERENCE(.doRetain, .doRelease);

struct AmbiguousFreeReleaseAndRetainMethods {
  int value;

  SWIFT_RETURNS_RETAINED
  AmbiguousFreeReleaseAndRetainMethods(int value) : value(value) {}
} SWIFT_SHARED_REFERENCE(retainAmbiguousFreeReleaseAndRetainMethods,
                         releaseAmbiguousFreeReleaseAndRetainMethods);

void retainAmbiguousFreeReleaseAndRetainMethods(
    AmbiguousFreeReleaseAndRetainMethods *v);
void retainAmbiguousFreeReleaseAndRetainMethods(); // wrong arity: 0
void releaseAmbiguousFreeReleaseAndRetainMethods(
    AmbiguousFreeReleaseAndRetainMethods *v);
void releaseAmbiguousFreeReleaseAndRetainMethods(
    AmbiguousFreeReleaseAndRetainMethods *v,
    int argument); // wrong arity: 2

struct OnlyViaFunctionBox {
  int refCount = 1;
} SWIFT_SHARED_REFERENCE(retainOnlyViaFunctionBox, releaseOnlyViaFunctionBox);

inline void retainOnlyViaFunctionBox(OnlyViaFunctionBox *v) { v->refCount++; }
inline void releaseOnlyViaFunctionBox(OnlyViaFunctionBox *v) {
  v->refCount--;
}

// MARK: Derived types whose own annotation replaces the base's.

struct BaseOfReannotated {
  mutable int refCount = 1;
  void baseRetain() const { refCount++; }
  void baseRelease() const { refCount--; }
} SWIFT_SHARED_REFERENCE(.baseRetain, .baseRelease);

struct ImmortalDerivedFromShared : BaseOfReannotated {
} SWIFT_IMMORTAL_REFERENCE;

struct UnsafeDerivedFromShared : BaseOfReannotated {} SWIFT_UNSAFE_REFERENCE;

struct OwnOpsDerivedFromShared : BaseOfReannotated {
  void ownRetain() const { refCount++; }
  void ownRelease() const { refCount--; }
} SWIFT_SHARED_REFERENCE(.ownRetain, .ownRelease);

// MARK: Overrides of virtual retain/release operations

struct VirtualOps {
  mutable int refCount = 1;
  virtual void virtualRetain() const { refCount++; }
  virtual void virtualRelease() const { refCount--; }
  virtual int unrelatedVirtual() const { return 0; }
  __attribute__((swift_attr("unsafe(always)")))
  virtual int explicitlyUnsafeVirtual() const { return 0; }
  virtual ~VirtualOps() = default;
} SWIFT_SHARED_REFERENCE(.virtualRetain, .virtualRelease);

struct OverridesVirtualOps : VirtualOps {
  void virtualRetain() const override { refCount++; }
  void virtualRelease() const override { refCount--; }
  int unrelatedVirtual() const override { return 1; }
  int explicitlyUnsafeVirtual() const override { return 1; }
};

struct DoesNotOverrideVirtualOps : VirtualOps {};

struct OverridesSkippingMiddle : DoesNotOverrideVirtualOps {
  void virtualRetain() const override { refCount++; }
  void virtualRelease() const override { refCount--; }
};

struct NonFRTPolymorphicBase {
  int value = 0;
  virtual ~NonFRTPolymorphicBase() = default;
};

// The FRT base is not at offset 0, so there is no Swift superclass.
struct OverridesWithNonPrimaryFRTBase : NonFRTPolymorphicBase, VirtualOps {
  void virtualRetain() const override { refCount++; }
  void virtualRelease() const override { refCount--; }
};

struct SameNamedVirtual {
  virtual void virtualRetain() const {}
  virtual ~SameNamedVirtual() = default;
};

struct OverridesInBothBases : VirtualOps, SameNamedVirtual {
  void virtualRetain() const override { refCount++; }
  void virtualRelease() const override { refCount--; }
};

struct OverridesThroughVirtualBase : virtual VirtualOps {
  void virtualRetain() const override { refCount++; }
  void virtualRelease() const override { refCount--; }
};

template <typename T>
struct TemplatedOverride : VirtualOps {
  T value;
  void virtualRetain() const override { refCount++; }
  void virtualRelease() const override { refCount--; }
};

using TemplatedOverrideInt = TemplatedOverride<int>;

struct ReannotatedOverride : VirtualOps {
  void virtualRetain() const override { refCount++; }
  void virtualRelease() const override { refCount--; }
  void ownRetain() const { refCount++; }
  void ownRelease() const { refCount--; }
} SWIFT_SHARED_REFERENCE(.ownRetain, .ownRelease);

struct ImmortalReannotatedOverride : VirtualOps {
  void virtualRetain() const override { refCount++; }
  void virtualRelease() const override { refCount--; }
} SWIFT_IMMORTAL_REFERENCE;

// The operations are declared in a base that is not a foreign reference type.
struct NonFRTWithVirtualOps {
  mutable int refCount = 1;
  virtual void inheritedRetain() const { refCount++; }
  virtual void inheritedRelease() const { refCount--; }
  virtual ~NonFRTWithVirtualOps() = default;
};

struct AnnotatedOverNonFRTBase : NonFRTWithVirtualOps {
} SWIFT_SHARED_REFERENCE(.inheritedRetain, .inheritedRelease);

struct OverridesOpsOfNonFRTBase : AnnotatedOverNonFRTBase {
  void inheritedRetain() const override { refCount++; }
  void inheritedRelease() const override { refCount--; }
};

// The annotation names the operations by their Swift names.
struct SwiftNamedVirtualOps {
  mutable int refCount = 1;
  SWIFT_NAME(swiftRetain()) virtual void cxxRetain() const { refCount++; }
  SWIFT_NAME(swiftRelease()) virtual void cxxRelease() const { refCount--; }
  virtual ~SwiftNamedVirtualOps() = default;
} SWIFT_SHARED_REFERENCE(.swiftRetain, .swiftRelease);

struct OverridesSwiftNamedOps : SwiftNamedVirtualOps {
  void cxxRetain() const override { refCount++; }
  void cxxRelease() const override { refCount--; }
};

// Only the derived type is ever named in Swift.
struct OnlyDerivedNamedBase {
  mutable int refCount = 1;
  virtual void onlyRetain() const { refCount++; }
  virtual void onlyRelease() const { refCount--; }
  virtual ~OnlyDerivedNamedBase() = default;
} SWIFT_SHARED_REFERENCE(.onlyRetain, .onlyRelease);

struct OnlyDerivedNamed : OnlyDerivedNamedBase {
  void onlyRetain() const override { refCount++; }
  void onlyRelease() const override { refCount--; }
};

struct DiamondLeft : virtual VirtualOps {};
struct DiamondRight : virtual VirtualOps {};

struct DiamondBottom : DiamondLeft, DiamondRight {
  void virtualRetain() const override { refCount++; }
  void virtualRelease() const override { refCount--; }
};

// MARK: Methods that hide non-virtual retain/release operations

struct NonVirtualOps {
  mutable int refCount = 1;
  void plainRetain() const { refCount++; }
  void plainRelease() const { refCount--; }
} SWIFT_SHARED_REFERENCE(.plainRetain, .plainRelease);

struct HidesNonVirtualOps : NonVirtualOps {
  void plainRetain() const { refCount += 2; }
  void plainRelease() const { refCount -= 2; }
};

struct HidesNonVirtualOpsWithVirtual : NonVirtualOps {
  virtual void plainRetain() const { refCount += 2; }
  virtual void plainRelease() const { refCount -= 2; }
  virtual ~HidesNonVirtualOpsWithVirtual() = default;
};

// Overrides the virtual methods introduced by HidesNonVirtualOpsWithVirtual,
// not the non-virtual operations.
struct OverridesVirtualHidingNonVirtualOps : HidesNonVirtualOpsWithVirtual {
  void plainRetain() const override { refCount += 3; }
  void plainRelease() const override { refCount -= 3; }
};

struct OtherPlainRetain {
  virtual void plainRetain() const {}
  virtual ~OtherPlainRetain() = default;
};

// Overrides the other base's virtual, not the non-virtual operation.
struct OverridesOnlyOtherBase : NonVirtualOps, OtherPlainRetain {
  void plainRetain() const override {}
};
