#if __has_feature(nullability)
_Pragma("clang assume_nonnull begin")
#endif

#define SWIFT_SHARED_REFERENCE(_retain, _release)                              \
  __attribute__((swift_attr("import_reference")))                              \
  __attribute__((swift_attr("retain:" #_retain)))                              \
  __attribute__((swift_attr("release:" #_release)))

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

#if __has_feature(nullability)
_Pragma("clang assume_nonnull end")
#endif
