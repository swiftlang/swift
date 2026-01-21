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
  mutable int refCount = 1;
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
#endif
