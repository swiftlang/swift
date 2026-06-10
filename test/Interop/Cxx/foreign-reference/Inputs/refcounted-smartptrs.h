#pragma once

#include <cstdio>
#include <swift/bridging>

class RefCountedBase {
public:
  SWIFT_RETURNS_RETAINED
  RefCountedBase() : _refCount(1) { printf("created\n"); }

  static RefCountedBase * _Nonnull forSmartPtr() {
    auto p = new RefCountedBase();
    p->_refCount = 0;
    return p;
  }

  int method() const { return 42; }

protected:
  virtual ~RefCountedBase() { printf("destroyed\n"); }

private:

  RefCountedBase(const RefCountedBase &) = delete;
  RefCountedBase &operator=(const RefCountedBase &) = delete;
  RefCountedBase(RefCountedBase &&) = delete;
  RefCountedBase &operator=(RefCountedBase &&) = delete;

  int _refCount;

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
  Ptr(T *_Nullable ptr) : ptr(ptr) {
    if (ptr)
      retainSharedFRT(ptr);
  }
  ~Ptr() {
    if (ptr)
      releaseSharedFRT(ptr);
  }
  Ptr(const Ptr &other) : ptr(other.ptr) {
    if (ptr)
      retainSharedFRT(ptr);
  }
  Ptr(Ptr &&other) : ptr(other.ptr) { other.ptr = nullptr; }
  Ptr &operator=(const Ptr &other) {
    if (ptr)
      releaseSharedFRT(ptr);
    ptr = other.ptr;
    if (ptr)
      retainSharedFRT(ptr);
    return *this;
  }
  Ptr &operator=(Ptr &&other) {
    if (ptr)
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

inline void bridgedFunction(RefOfBase ptr) {}
inline void bridgedFunction2(PtrOfBase ptr) {}
inline RefOfBase bridgedFunction3() {
  return RefOfBase(RefCountedBase::forSmartPtr());
}
inline PtrOfBase bridgedFunction4() {
  return PtrOfBase(RefCountedBase::forSmartPtr());
}
inline void notBridgedFunction(RefOfBase &ptr) {}
inline void notBridgedFunction2(const RefOfBase &ptr) {}
inline void notBridgedFunction3(RefOfBase &&ptr) {}

struct PairWithPtr {
  PtrOfBase ptr;
  int otherValue;
  PairWithPtr(RefCountedBase *_Nullable p) : ptr(p), otherValue(42) {}

  PtrOfBase loadPtr() const { return ptr; }
  void storePtr(PtrOfBase newPtr) { ptr = newPtr; }
};

struct PairWithConstRef {
  const RefOfBase ref;
  int otherValue;
  PairWithConstRef(RefCountedBase *_Nonnull p) : ref(p), otherValue(42) {}

  RefOfBase loadRef() const { return ref; }
};

inline PairWithPtr makePairWithPtr() {
  return PairWithPtr(RefCountedBase::forSmartPtr());
}
inline PairWithPtr makePairWithNullPtr() { return PairWithPtr(nullptr); }
inline PairWithConstRef makePairWithConstRef() {
  return PairWithConstRef(RefCountedBase::forSmartPtr());
}
inline void takesPairWithPtr(PairWithPtr p) {}

// A non-nullable smart pointer that, in addition to the usual T*
// constructor, has a constructor taking a T&.
template <class T>
struct SWIFT_REFCOUNTED_PTR(.getPtr) RefR : Ref<T> {
  RefR(T *_Nonnull ptr) : Ref<T>(ptr) { printf("RefR ptr ctor\n"); }
  RefR(T &ref) : Ref<T>(&ref) { printf("RefR ref ctor\n"); }
};

using RefROfBase = RefR<RefCountedBase>;

inline void bridgedFunctionR(RefROfBase ptr) {}
inline RefROfBase bridgedFunctionR_returns() {
  return RefROfBase(RefCountedBase::forSmartPtr());
}

// A non-nullable smart pointer that has both a T* and a const T*
// constructor. The non-const T* ctor must be the one selected for
// implicit bridging.
template <class T>
struct SWIFT_REFCOUNTED_PTR(.getPtr) RefConstPtr : Ref<T> {
  RefConstPtr(T *_Nonnull ptr) : Ref<T>(ptr) {
    printf("RefConstPtr ptr ctor\n");
  }
  RefConstPtr(const T *_Nonnull ptr) : Ref<T>(const_cast<T *>(ptr)) {
    printf("RefConstPtr const-ptr ctor\n");
  }
};

using RefConstPtrOfBase = RefConstPtr<RefCountedBase>;

inline void bridgedFunctionConstPtr(RefConstPtrOfBase ptr) {}
inline RefConstPtrOfBase bridgedFunctionConstPtr_returns() {
  return RefConstPtrOfBase(RefCountedBase::forSmartPtr());
}

// A non-nullable smart pointer whose only constructor takes a const
// T& and whose raw-pointer accessor returns a T& (rather than a T*).
template <class T>
struct SWIFT_REFCOUNTED_PTR(.getRef) RefRefOnly : Ref<T> {
  RefRefOnly(const T &ref) : Ref<T>(const_cast<T *>(&ref)) {
    printf("RefRefOnly ref ctor\n");
  }
  __attribute__((swift_attr("returns_unretained"))) T &getRef() const {
    return *this->getPtr();
  }
};

using RefRefOnlyOfBase = RefRefOnly<RefCountedBase>;

inline void bridgedFunctionRefOnly(RefRefOnlyOfBase ptr) {}
inline RefRefOnlyOfBase bridgedFunctionRefOnly_returns() {
  return RefRefOnlyOfBase(*RefCountedBase::forSmartPtr());
}

// A derived smart pointer that itself is *not* annotated with
// SWIFT_REFCOUNTED_PTR. Inheriting from the annotated Ref<T> means the
// bridging `asReference` property is cloned onto this class via the
// inheritance lookup, so the user can convert to the FRT explicitly.
// No implicit bridging happens at function boundaries because this
// type itself doesn't carry the macro.
template <class T>
struct UnannotatedRef : Ref<T> {
  using Ref<T>::Ref;
};

using UnannotatedRefOfBase = UnannotatedRef<RefCountedBase>;

inline UnannotatedRefOfBase makeUnannotatedRef() {
  return UnannotatedRefOfBase(RefCountedBase::forSmartPtr());
}

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

struct SWIFT_REFCOUNTED_PTR(.getPtr) NoSuitableCtor {
  // expected-note@-1 * {{cannot find constructor to create smart pointer from a raw pointer to a shared reference 'NoSuitableCtor'}}
  RefCountedBase *_Nonnull getPtr();
};

struct RefCounted : RefCountedBase {};

struct SWIFT_REFCOUNTED_PTR(.getPtr) AmbiguousCtors {
  // expected-note@-1 * {{there are multiple constructors to create smart pointer from a raw pointer to a shared reference 'AmbiguousCtors'}}
  AmbiguousCtors(RefCountedBase* _Nullable);
  AmbiguousCtors(RefCounted* _Nullable);
  RefCountedBase *_Nonnull getPtr();
};

struct SWIFT_REFCOUNTED_PTR(.getPtr) MismatchingPointerTypes {
  // expected-note@-1 * {{the constructor's parameter to create smart pointer from a raw pointer does not match the return type of the raw pointer accessor in 'MismatchingPointerTypes'}}
  MismatchingPointerTypes(RefCounted* _Nullable);
  RefCountedBase *_Nonnull getPtr();
};

// Two T& ctors should still be ambiguous: per-kind ambiguity is
// preserved (we only fall back to the reference slot when there is no
// pointer slot).
struct SWIFT_REFCOUNTED_PTR(.getPtr) AmbiguousRefCtors {
  // expected-note@-1 * {{there are multiple constructors to create smart pointer from a raw pointer to a shared reference 'AmbiguousRefCtors'}}
  AmbiguousRefCtors(RefCountedBase &);
  AmbiguousRefCtors(RefCounted &);
  RefCountedBase *_Nonnull getPtr();
};

// An rvalue-reference ctor isn't a valid bridging ctor; if it's the
// only one we fail to find a suitable ctor.
struct SWIFT_REFCOUNTED_PTR(.getPtr) RvalueRefCtor {
  // expected-note@-1 * {{cannot find constructor to create smart pointer from a raw pointer to a shared reference 'RvalueRefCtor'}}
  RvalueRefCtor(RefCountedBase &&);
  RefCountedBase *_Nonnull getPtr();
};
} // namespace errors
