inline int &liveCount() {
  static int n = 0;
  return n;
}
inline int getLiveCount() { return liveCount(); }

struct BaseRef {
  int refCount = 1;
  int value;

  BaseRef(int value) : value(value) { liveCount()++; }
  BaseRef(const BaseRef &) = delete;

  virtual ~BaseRef() { liveCount()--; }
} __attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:retainShared")))
__attribute__((swift_attr("release:releaseShared")));

struct DerivedRef : BaseRef {
  int other;

  DerivedRef(int value, int other) : BaseRef(value), other(other) {}
};

inline void retainShared(BaseRef *_Nonnull r) { r->refCount++; }
inline void releaseShared(BaseRef *_Nonnull r) {
  if (--r->refCount == 0)
    delete r;
}

inline BaseRef *_Nonnull createBaseRef(int value)
    __attribute__((swift_attr("returns_retained"))) {
  return new BaseRef(value);
}

inline DerivedRef *_Nonnull createDerivedRef(int value, int other)
    __attribute__((swift_attr("returns_retained"))) {
  return new DerivedRef(value, other);
}
