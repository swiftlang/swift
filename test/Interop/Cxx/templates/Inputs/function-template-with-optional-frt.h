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

struct OSMetaClass;

struct OSMetaClassBase {
  static OSMetaClassBase *safeMetaCast(const OSMetaClassBase *inst,
                                       const OSMetaClass *meta) {
    (void)meta;
    return const_cast<OSMetaClassBase *>(inst);
  }
  virtual ~OSMetaClassBase() = default;
};

#define OSTypeID(type) ((const OSMetaClass *)nullptr)

#define OSDynamicCast(type, inst)                                              \
  ((type *)OSMetaClassBase::safeMetaCast((inst), OSTypeID(type)))

struct __attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:immortal")))
__attribute__((swift_attr("release:immortal"))) FRTBase : OSMetaClassBase {
  int x;
};

struct __attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:immortal")))
__attribute__((swift_attr("release:immortal"))) FRTDerived : FRTBase {
  int y;
};

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
