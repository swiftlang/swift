#if __has_feature(nullability)
_Pragma("clang assume_nonnull begin")
#endif

#define SWIFT_SHARED_REFERENCE(_retain, _release)                              \
  __attribute__((swift_attr("import_reference")))                              \
  __attribute__((swift_attr("retain:" #_retain)))                              \
  __attribute__((swift_attr("release:" #_release)))

#define SWIFT_RETURNS_RETAINED __attribute__((swift_attr("returns_retained")))

struct SubclassableShared {
  int refcount = 1;

  virtual ~SubclassableShared() {}
} SWIFT_SHARED_REFERENCE(retainSubclassableShared, releaseSubclassableShared);

inline void retainSubclassableShared(SubclassableShared *t) { ++t->refcount; }
inline void releaseSubclassableShared(SubclassableShared *t) {
  if (--t->refcount <= 0)
    delete t;
}

struct DerivedSubclassableShared : SubclassableShared {};

struct NonVirtualShared {
  int refcount = 1;
} SWIFT_SHARED_REFERENCE(retainNonVirtualShared, releaseNonVirtualShared);

inline void retainNonVirtualShared(NonVirtualShared *t) { ++t->refcount; }
inline void releaseNonVirtualShared(NonVirtualShared *t) {
  if (--t->refcount <= 0)
    (void)"DELETION PLACEHOLDER";
}

struct FinalShared final {
  int refcount = 1;

  virtual ~FinalShared() {}
} SWIFT_SHARED_REFERENCE(retainFinalShared, releaseFinalShared);

inline void retainFinalShared(FinalShared *t) { ++t->refcount; }
inline void releaseFinalShared(FinalShared *t) {
  if (--t->refcount <= 0)
    delete t;
}

struct PrivateDtorShared {
  int refcount = 1;

  friend void releasePrivateDtorShared(PrivateDtorShared *t);

private:
  virtual ~PrivateDtorShared() {}
} SWIFT_SHARED_REFERENCE(retainPrivateDtorShared, releasePrivateDtorShared);

inline void retainPrivateDtorShared(PrivateDtorShared *t) { ++t->refcount; }
inline void releasePrivateDtorShared(PrivateDtorShared *t) {
  if (--t->refcount <= 0)
    delete t;
}

struct ProtectedDtorShared {
  int refcount = 1;

  friend void releaseProtectedDtorShared(ProtectedDtorShared *t);

protected:
  virtual ~ProtectedDtorShared() {}
} SWIFT_SHARED_REFERENCE(retainProtectedDtorShared, releaseProtectedDtorShared);

inline void retainProtectedDtorShared(ProtectedDtorShared *t) { ++t->refcount; }
inline void releaseProtectedDtorShared(ProtectedDtorShared *t) {
  if (--t->refcount <= 0)
    delete t;
}

struct DeletedDtorShared {
  int refcount = 1;

  virtual ~DeletedDtorShared() = delete;
} SWIFT_SHARED_REFERENCE(retainDeletedDtorShared, releaseDeletedDtorShared);

inline void retainDeletedDtorShared(DeletedDtorShared *t) { ++t->refcount; }
inline void releaseDeletedDtorShared(DeletedDtorShared *t) {
  if (--t->refcount <= 0)
    (void)"DELETION PLACEHOLDER";
}

struct SharedConstructed {
  int refcount = 1;
  int a = 0;
  long b = 0;

  SWIFT_RETURNS_RETAINED SharedConstructed() {}
  SWIFT_RETURNS_RETAINED SharedConstructed(int a) : a(a) {}
  SWIFT_RETURNS_RETAINED SharedConstructed(int a, long b) : a(a), b(b) {}

  int getA() const { return a; }
  long getB() const { return b; }

  virtual ~SharedConstructed() {}
} SWIFT_SHARED_REFERENCE(retainSharedConstructed, releaseSharedConstructed);

inline void retainSharedConstructed(SharedConstructed *t) { ++t->refcount; }
inline void releaseSharedConstructed(SharedConstructed *t) {
  if (--t->refcount <= 0)
    delete t;
}

struct ArgOnlyConstructed {
  int refcount = 1;
  int a = 0;

  SWIFT_RETURNS_RETAINED ArgOnlyConstructed(int a) : a(a) {}

  virtual ~ArgOnlyConstructed() {}
} SWIFT_SHARED_REFERENCE(retainArgOnlyConstructed, releaseArgOnlyConstructed);

inline void retainArgOnlyConstructed(ArgOnlyConstructed *t) { ++t->refcount; }
inline void releaseArgOnlyConstructed(ArgOnlyConstructed *t) {
  if (--t->refcount <= 0)
    delete t;
}

inline int &liveNonTrivialArgs() {
  static int count = 0;
  return count;
}
inline int getLiveNonTrivialArgs() { return liveNonTrivialArgs(); }

struct NonTrivialArg {
  int value;

  NonTrivialArg(int value) : value(value) { ++liveNonTrivialArgs(); }
  NonTrivialArg(const NonTrivialArg &other) : value(other.value) {
    ++liveNonTrivialArgs();
  }
  ~NonTrivialArg() { --liveNonTrivialArgs(); }
};

struct RefcountedArg {
  int refcount = 1;
  int value = 0;

  SWIFT_RETURNS_RETAINED RefcountedArg(int value) : value(value) {}

  int getRefcount() const { return refcount; }

  virtual ~RefcountedArg() {}
} SWIFT_SHARED_REFERENCE(retainRefcountedArg, releaseRefcountedArg);

inline void retainRefcountedArg(RefcountedArg *t) { ++t->refcount; }
inline void releaseRefcountedArg(RefcountedArg *t) {
  if (--t->refcount <= 0)
    delete t;
}

struct ReferenceConstructed {
  int refcount = 1;
  int seen = 0;

  SWIFT_RETURNS_RETAINED ReferenceConstructed(const int &i) : seen(i) {}
  SWIFT_RETURNS_RETAINED ReferenceConstructed(int &i, int) : seen(i) {
    i = 100;
  }
  SWIFT_RETURNS_RETAINED ReferenceConstructed(NonTrivialArg arg, long)
      : seen(arg.value) {}
  SWIFT_RETURNS_RETAINED ReferenceConstructed(const NonTrivialArg &arg, char)
      : seen(arg.value) {}
  SWIFT_RETURNS_RETAINED ReferenceConstructed(RefcountedArg *arg, short)
      : seen(arg->value) {}

  int getSeen() const { return seen; }

  virtual ~ReferenceConstructed() {}
} SWIFT_SHARED_REFERENCE(retainReferenceConstructed,
                         releaseReferenceConstructed);

inline void retainReferenceConstructed(ReferenceConstructed *t) {
  ++t->refcount;
}
inline void releaseReferenceConstructed(ReferenceConstructed *t) {
  if (--t->refcount <= 0)
    delete t;
}

struct OverloadedConstructed {
  int refcount = 1;
  int which = 0;

  SWIFT_RETURNS_RETAINED OverloadedConstructed(int) : which(1) {}
  SWIFT_RETURNS_RETAINED OverloadedConstructed(double) : which(2) {}

  int getWhich() const { return which; }

  virtual ~OverloadedConstructed() {}
} SWIFT_SHARED_REFERENCE(retainOverloadedConstructed,
                         releaseOverloadedConstructed);

inline void retainOverloadedConstructed(OverloadedConstructed *t) {
  ++t->refcount;
}
inline void releaseOverloadedConstructed(OverloadedConstructed *t) {
  if (--t->refcount <= 0)
    delete t;
}

#if __has_feature(nullability)
_Pragma("clang assume_nonnull end")
#endif
