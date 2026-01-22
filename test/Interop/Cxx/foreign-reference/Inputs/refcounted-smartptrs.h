#pragma once

#include <cstdio>
#include <swift/bridging>

class RefCountedBase {
public:
  SWIFT_RETURNS_RETAINED
  RefCountedBase() : _refCount(1) { printf("created\n"); }

protected:
  virtual ~RefCountedBase() { printf("destroyed"); }

private:

  RefCountedBase(const RefCountedBase &) = delete;
  RefCountedBase &operator=(const RefCountedBase &) = delete;
  RefCountedBase(RefCountedBase &&) = delete;
  RefCountedBase &operator=(RefCountedBase &&) = delete;

  int _refCount;

  int method() const { return 42; }

  friend void retainSharedFRT(RefCountedBase *_Nonnull);
  friend void releaseSharedFRT(RefCountedBase *_Nonnull);
} SWIFT_SHARED_REFERENCE(retainSharedFRT, releaseSharedFRT);

inline void retainSharedFRT(RefCountedBase *_Nonnull x) {
  ++x->_refCount;
}

inline void releaseSharedFRT(RefCountedBase *_Nonnull x) {
  --x->_refCount;
  if (x->_refCount == 0)
    delete x;
}

template <class T>
struct SWIFT_REFCOUNTED_PTR(.getPtr) Ref {
  Ref(T *_Nonnull ptr) : ptr(ptr) { retainSharedFRT(ptr); }
  ~Ref() { releaseSharedFRT(ptr); }
  Ref(const Ref &other) : ptr(other.ptr) { retainSharedFRT(ptr); }
  Ref(Ref &&other) = delete;
  Ref &operator=(const Ref &other) {
    releaseSharedFRT(ptr);
    ptr = other.ptr;
    retainSharedFRT(ptr);
    return *this;
  }
  Ref &operator=(Ref &&other) = delete;

  T &operator*() const { return *ptr; }
  T *_Nonnull getPtr() const { return ptr; }

  T *_Nonnull ptr;
};

template <class T>
struct SWIFT_REFCOUNTED_PTR(.get) Ptr {
  Ptr(T *_Nullable ptr) : ptr(ptr) { retainSharedFRT(ptr); }
  ~Ptr() { releaseSharedFRT(ptr); }
  Ptr(const Ptr &other) : ptr(other.ptr) { retainSharedFRT(ptr); }
  Ptr(Ptr &&other) : ptr(other.ptr) { other.ptr = nullptr; }
  Ptr &operator=(const Ptr &other) {
    releaseSharedFRT(ptr);
    ptr = other.ptr;
    retainSharedFRT(ptr);
    return *this;
  }
  Ptr &operator=(Ptr &&other) {
    releaseSharedFRT(ptr);
    ptr = other.ptr;
    other.ptr = nullptr;
    return *this;
  }
  T &operator*() const { return *ptr; }
  operator bool() const { return ptr; }
  SWIFT_NAME(get())
  T *_Nullable getPtr() const { return ptr; }

  T *_Nullable ptr;
};

template <class T>
struct DerivedRefBase {
  DerivedRefBase(T *_Nonnull ptr) : ptr(ptr) { retainSharedFRT(ptr); }
  ~DerivedRefBase() { releaseSharedFRT(ptr); }
  DerivedRefBase(const DerivedRefBase &other) : ptr(other.ptr) { retainSharedFRT(ptr); }
  DerivedRefBase(DerivedRefBase &&other) = delete;
  DerivedRefBase &operator=(const DerivedRefBase &other) {
    releaseSharedFRT(ptr);
    ptr = other.ptr;
    retainSharedFRT(ptr);
    return *this;
  }
  DerivedRefBase &operator=(DerivedRefBase &&other) = delete;
  T &operator*() const { return *ptr; }
  T *_Nonnull getPtr() const { return ptr; }

  T *_Nonnull ptr;
};

template <class T>
struct SWIFT_REFCOUNTED_PTR(.getPtr) DerivedRef : DerivedRefBase<T> {
  using DerivedRefBase<T>::DerivedRefBase;
};

using RefOfBase = Ref<RefCountedBase>;
using PtrOfBase = Ptr<RefCountedBase>;
using DerivedRefOfBase = DerivedRef<RefCountedBase>;

namespace errors { // expected-note * {{'errors' declared here}}
struct __attribute__((
    swift_attr("@_refCountedPtr(Nullability: \"Nullable\")"))) MissingToRawPtr {};
// expected-note@-1 * {{SWIFT_REFCOUNTED_PTR on 'MissingToRawPtr' is missing required '_toRawPointer' parameter}}

struct SWIFT_REFCOUNTED_PTR(.getPtr) MissingConversionFunction {};
// expected-note@-1 * {{cannot find function '.getPtr' specified in '_toRawPointer' parameter of SWIFT_REFCOUNTED_PTR 'MissingConversionFunction'}}

struct SWIFT_REFCOUNTED_PTR(.getPtr) MultipleConversionFunctions {
// expected-note@-1 * {{there are multiple candidates of function '.getPtr' specified in '_toRawPointer' parameter of SWIFT_REFCOUNTED_PTR 'MultipleConversionFunctions'}}
  void getPtr();
  void getPtr(int a);
};

struct SWIFT_REFCOUNTED_PTR(.getPtr) WrongConversionSignature {
// expected-note@-1 * {{function 'getPtr()' specified in '_toRawPointer' parameter of SWIFT_REFCOUNTED_PTR on 'WrongConversionSignature' has incorrect signature; expected a function that takes no arguments and returns a pointer to a foreign reference type}}
  void getPtr();
};

class SWIFT_REFCOUNTED_PTR(.getPtr) PrivateConversionFunction {
  // expected-note@-1 * {{cannot find function '.getPtr' specified in '_toRawPointer' parameter of SWIFT_REFCOUNTED_PTR 'PrivateConversionFunction'}}
  RefCountedBase *_Nonnull getPtr();
};
} // namespace errors
