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

struct BaseObj {
  virtual int tag() const { return 0; }
} SWIFT_IMMORTAL_REFERENCE;

struct DerivedObj1 : BaseObj {
  int tag() const override { return 1; }
  static DerivedObj1 &make() {
    static DerivedObj1 instance;
    return instance;
  }
};

struct DerivedObj2 : BaseObj {
  int tag() const override { return 2; }
  static DerivedObj2 &make() {
    static DerivedObj2 instance;
    return instance;
  }
};

struct SharedObj {
  virtual int tag() const { return 42; }

  SWIFT_RETURNS_RETAINED
  static SharedObj &make() {
    return *new SharedObj();
  }

  int currentRefCount() const { return refCount; }

  // Cumulative counts of ref()/deref() calls, used to confirm the custom
  // retain/release functions are actually invoked (holds only at -Onone).
  static int numRefs() { return totalRefs; }
  static int numDerefs() { return totalDerefs; }

  void ref() { ++refCount; ++totalRefs; }
  void deref() {
    ++totalDerefs;
    if (--refCount == 0)
      delete this;
  }

private:
  SharedObj() : refCount(1) {}
  virtual ~SharedObj() = default;

  int refCount;
  static int totalRefs;
  static int totalDerefs;
} SWIFT_SHARED_REFERENCE(.ref, .deref);

// HACK: defining these in a header is bad practice; we only use them to observe
// retain/release activity at runtime.
int SharedObj::totalRefs = 0;
int SharedObj::totalDerefs = 0;
