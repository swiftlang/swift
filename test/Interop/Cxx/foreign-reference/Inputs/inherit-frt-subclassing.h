#if __has_feature(nullability)
_Pragma("clang assume_nonnull begin")
#endif

struct SubclassableShared {
  int refcount = 1;

  virtual ~SubclassableShared() {}
} __attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:retainSubclassableShared")))
__attribute__((swift_attr("release:releaseSubclassableShared")));

inline void retainSubclassableShared(SubclassableShared *t) { ++t->refcount; }
inline void releaseSubclassableShared(SubclassableShared *t) {
  if (--t->refcount <= 0)
    delete t;
}

struct DerivedSubclassableShared : SubclassableShared {};

struct NonVirtualShared {
  int refcount = 1;
} __attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:retainNonVirtualShared")))
__attribute__((swift_attr("release:releaseNonVirtualShared")));

inline void retainNonVirtualShared(NonVirtualShared *t) { ++t->refcount; }
inline void releaseNonVirtualShared(NonVirtualShared *t) {
  if (--t->refcount <= 0)
    (void)"DELETION PLACEHOLDER";
}

struct FinalShared final {
  int refcount = 1;

  virtual ~FinalShared() {}
} __attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:retainFinalShared")))
__attribute__((swift_attr("release:releaseFinalShared")));

inline void retainFinalShared(FinalShared *t) { ++t->refcount; }
inline void releaseFinalShared(FinalShared *t) {
  if (--t->refcount <= 0)
    delete t;
}

#if __has_feature(nullability)
_Pragma("clang assume_nonnull end")
#endif
