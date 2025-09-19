#pragma once

#include <stdio.h>
#include <swift/bridging>

class SharedFRT {
public:
  SharedFRT() : _refCount(1) { logMsg("Ctor"); }

private:
  void logMsg(const char *s) const {
    printf("RefCount: %d, message: %s\n", _refCount, s);
  }

  ~SharedFRT() { logMsg("Dtor"); }
  SharedFRT(const SharedFRT &) = delete;
  SharedFRT &operator=(const SharedFRT &) = delete;
  SharedFRT(SharedFRT &&) = delete;
  SharedFRT &operator=(SharedFRT &&) = delete;

  int _refCount;

  friend void retainSharedFRT(SharedFRT *_Nonnull);
  friend void releaseSharedFRT(SharedFRT *_Nonnull);
} SWIFT_SHARED_REFERENCE(retainSharedFRT, releaseSharedFRT);

class MyToken {
public:
  MyToken() = default;
  MyToken(MyToken const &) {}
};

inline void retainSharedFRT(SharedFRT *_Nonnull x) {
  ++x->_refCount;
  x->logMsg("retain");
}

inline void releaseSharedFRT(SharedFRT *_Nonnull x) {
  --x->_refCount;
  x->logMsg("release");
  if (x->_refCount == 0)
    delete x;
}

struct LargeStructWithRefCountedField {
  void const *a;
  void const *b;
  unsigned long c;
  unsigned d;
  SharedFRT *e;
};

struct LargeStructWithRefCountedFieldNested {
  int a;
  LargeStructWithRefCountedField b;
};

inline LargeStructWithRefCountedField getStruct() {
  return {0, 0, 0, 0, new SharedFRT()};
}

inline LargeStructWithRefCountedFieldNested getNestedStruct() {
  return {0, {0, 0, 0, 0, new SharedFRT()}};
}
