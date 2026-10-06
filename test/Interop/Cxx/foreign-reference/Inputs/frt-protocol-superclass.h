#ifndef TEST_INTEROP_CXX_FOREIGN_REFERENCE_INPUTS_FRT_PROTOCOL_SUPERCLASS_H
#define TEST_INTEROP_CXX_FOREIGN_REFERENCE_INPUTS_FRT_PROTOCOL_SUPERCLASS_H

#define _CXX_INTEROP_STRINGIFY(_x) #_x

#define SWIFT_SHARED_REFERENCE(_retain, _release)                          \
  __attribute__((swift_attr("import_reference")))                          \
  __attribute__((swift_attr(_CXX_INTEROP_STRINGIFY(retain:_retain))))      \
  __attribute__((swift_attr(_CXX_INTEROP_STRINGIFY(release:_release))))

#define SWIFT_IMMORTAL_REFERENCE                     \
  __attribute__((swift_attr("import_reference")))    \
  __attribute__((swift_attr("retain:immortal")))     \
  __attribute__((swift_attr("release:immortal")))

#define SWIFT_RETURNS_RETAINED __attribute__((swift_attr("returns_retained")))

struct SharedBase {
  virtual int tag() const { return 42; }

  SWIFT_RETURNS_RETAINED
  static SharedBase &make() { return *new SharedBase(); }

  int currentRefCount() const { return refCount; }

  // Cumulative counts of ref()/deref() calls, used to confirm the custom
  // retain/release functions are actually invoked (at -Onone).
  static int numRefs() { return totalRefs; }
  static int numDerefs() { return totalDerefs; }

  void ref() { ++refCount; ++totalRefs; }
  void deref() {
    ++totalDerefs;
    if (--refCount == 0)
      delete this;
  }

private:
  SharedBase() : refCount(1) {}
  virtual ~SharedBase() = default;

  int refCount;
  static int totalRefs;
  static int totalDerefs;
} SWIFT_SHARED_REFERENCE(.ref, .deref);

// HACK: defining these in a header is bad practice; we only use them to observe
// retain/release activity at runtime.
int SharedBase::totalRefs = 0;
int SharedBase::totalDerefs = 0;

struct ImmortalBase {
  virtual int tag() const { return 1; }

  /// A canary sitting immediately after the vtable pointer, which is where a
  /// Swift heap object keeps its reference count. An immortal FRT must never be
  /// retained or released, so this value must never change.
  long long canary() const { return canaryValue; }

  static ImmortalBase &shared() {
    static ImmortalBase instance;
    return instance;
  }

private:
  long long canaryValue = 0xC0FFEE;
} SWIFT_IMMORTAL_REFERENCE;

#endif // TEST_INTEROP_CXX_FOREIGN_REFERENCE_INPUTS_FRT_PROTOCOL_SUPERCLASS_H
