#pragma once

template <class T>
struct RemovePointerImpl {
  using type = T;
};
template <class T>
struct RemovePointerImpl<T *> {
  using type = T;
};
template <class T>
using RemovePointer = typename RemovePointerImpl<T>::type;

struct OSMetaClass {};

template <class T>
struct OSMetaClassFor {
  static const OSMetaClass meta;
};
template <class T>
const OSMetaClass OSMetaClassFor<T>::meta{};

struct OSMetaClassBase {
  static OSMetaClassBase *_Nullable safeMetaCast(
      const OSMetaClassBase *_Nullable inst, const OSMetaClass *_Nonnull meta) {
    if (!inst || inst->getMetaClass() != meta)
      return nullptr;
    return const_cast<OSMetaClassBase *>(inst);
  }
  virtual const OSMetaClass *_Nonnull getMetaClass() const = 0;
  virtual ~OSMetaClassBase() = default;
};

#define OSTypeID(type) (&OSMetaClassFor<type>::meta)

#define OSDynamicCast(type, inst)                                              \
  ((type *)OSMetaClassBase::safeMetaCast((inst), OSTypeID(type)))

struct __attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:immortal")))
__attribute__((swift_attr("release:immortal"))) FRTBase : OSMetaClassBase {
  int x = 1;
  const OSMetaClass *_Nonnull getMetaClass() const override {
    return OSTypeID(FRTBase);
  }
};

struct __attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:immortal")))
__attribute__((swift_attr("release:immortal"))) FRTDerived : FRTBase {
  int y = 2;
  const OSMetaClass *_Nonnull getMetaClass() const override {
    return OSTypeID(FRTDerived);
  }
};

inline FRTBase *_Nonnull makeBase() {
  static FRTBase base;
  return &base;
}

inline FRTBase *_Nonnull makeDerivedAsBase() {
  static FRTDerived derived;
  return &derived;
}

template <class T>
T passThrough(T value) {
  return value;
}

template <class R, class I>
R cast(I i) {
  return (R)i;
}

template <class BasePtr, class DerivedPtr>
DerivedPtr dynamicCast(BasePtr x) {
  return dynamic_cast<DerivedPtr>(x);
}

template <class BasePtr, class DerivedPtr>
DerivedPtr downcast(BasePtr x) {
  DerivedPtr d = OSDynamicCast(RemovePointer<DerivedPtr>, x);
  return d;
}

// The nullability specifier on the dependent result makes Swift import the
// template as returning `DerivedPtr?`, so callers have to check the result.
// expected-note@+3 {{in call to function 'nullableDowncast'}}
// expected-error@+1 {{could not substitute parameters for C++ function template 'nullableDowncast': optional type 'FRTDerived?' cannot replace template parameter 'DerivedPtr', which is declared with a nullability specifier}}
template <class BasePtr, class DerivedPtr>
DerivedPtr _Nullable nullableDowncast(BasePtr x) {
  DerivedPtr d = OSDynamicCast(RemovePointer<DerivedPtr>, x);
  return d;
}
