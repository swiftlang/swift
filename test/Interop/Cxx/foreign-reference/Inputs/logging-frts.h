#pragma once

#include <stdio.h>
#include <swift/bridging>

class SharedFRT {
public:
  SWIFT_RETURNS_RETAINED
  SharedFRT() : _refCount(1) { logMsg("Ctor"); }

protected:
  virtual ~SharedFRT() { logMsg("Dtor"); }

private:
  void logMsg(const char *s) const {
    printf("RefCount: %d, message: %s\n", _refCount, s);
  }

  SharedFRT(const SharedFRT &) = delete;
  SharedFRT &operator=(const SharedFRT &) = delete;
  SharedFRT(SharedFRT &&) = delete;
  SharedFRT &operator=(SharedFRT &&) = delete;

  int _refCount;

public:
  void retainSharedFRT() {
    ++_refCount;
    logMsg("retain");
  }

  friend void releaseSharedFRT(SharedFRT *_Nonnull);
} SWIFT_SHARED_REFERENCE(.retainSharedFRT, releaseSharedFRT);

class MyToken {
public:
  MyToken() = default;
  MyToken(MyToken const &) {}
};

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

template <class T>
struct Ref {
  T *_Nonnull ptr() const { return ref; }
  T *ref;
};

class Payload final : public SharedFRT {
public:
  static Ref<Payload> create(int value) {
    Ref<Payload> ref;
    ref.ref = new Payload(value);
    return ref;
  }

  int value() const { return m_value; }

private:
  explicit Payload(int value) : m_value(value) {}
  int m_value;
};

struct FirstBase {
  int a, b, c;
  virtual ~FirstBase() {}
};

struct DerivedFRT : FirstBase, SharedFRT {
  SWIFT_RETURNS_RETAINED
  DerivedFRT() = default;
};
