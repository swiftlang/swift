//===--- ExistentialMetadataImpl.h - Existential metadata -------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Declarations used to implement value witnesses for Swift
// existential types.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_RUNTIME_EXISTENTIALMETADATAIMPL_H
#define SWIFT_RUNTIME_EXISTENTIALMETADATAIMPL_H

#include "MetadataImpl.h"

namespace swift {
namespace metadataimpl {

/// A common base class for opaque-existential and class-existential boxes.
template<typename Impl>
struct LLVM_LIBRARY_VISIBILITY ExistentialBoxBase {
  template <class Container, class... A>
  static void destroyArray(Container *array, size_t n, A... args) {
    size_t stride = Container::getContainerStride(args...);
    char *bytes = (char*)array;
    while (n--) {
      Impl::destroy((Container*)bytes, args...);
      bytes += stride;
    }
  }
  
  template <class Container, class... A>
  static Container *initializeArrayWithCopy(Container *dest,
                                            Container *src,
                                            size_t n,
                                            A... args) {
    size_t stride = Container::getContainerStride(args...);
    char *destBytes = (char*)dest, *srcBytes = (char*)src;
    while (n--) {
      Impl::initializeWithCopy((Container*)destBytes,
                               (Container*)srcBytes, args...);
      destBytes += stride; srcBytes += stride;
    }
    return dest;
  }
  
  template <class Container, class... A>
  static Container *initializeArrayWithTakeFrontToBack(Container *dest,
                                                       Container *src,
                                                       size_t n,
                                                       A... args) {
    size_t stride = Container::getContainerStride(args...);
    char *destBytes = (char*)dest, *srcBytes = (char*)src;
    while (n--) {
      Impl::initializeWithTake((Container*)destBytes,
                               (Container*)srcBytes, args...);
      destBytes += stride; srcBytes += stride;
    }
    return dest;
  }
  
  template <class Container, class... A>
  static Container *initializeArrayWithTakeBackToFront(Container *dest,
                                                       Container *src,
                                                       size_t n,
                                                       A... args) {
    size_t stride = Container::getContainerStride(args...);
    char *destBytes = (char*)dest + n * stride, *srcBytes = (char*)src + n * stride;
    while (n--) {
      destBytes -= stride; srcBytes -= stride;
      Impl::initializeWithTake((Container*)destBytes,
                               (Container*)srcBytes, args...);
    }
    return dest;
  }
};

/// A common base class for fixed and non-fixed opaque-existential box
/// implementations.
struct LLVM_LIBRARY_VISIBILITY OpaqueExistentialBoxBase
    : ExistentialBoxBase<OpaqueExistentialBoxBase> {
  template <class Container, class... A>
  static void destroy(Container *value, A... args) {
#ifdef SWIFT_RUNTIME_ENABLE_COW_EXISTENTIALS
    auto *type = value->getType();
    auto *vwt = type->getValueWitnesses();
    if (vwt->isValueInline()) {
      // destroy(&valueBuffer)
      type->vw_destroy(
          reinterpret_cast<OpaqueValue *>(value->getBuffer(args...)));
    } else {
      // release(valueBuffer[0])
      swift_release(
          *reinterpret_cast<HeapObject **>(value->getBuffer(args...)));
    }
#else
    value->getType()->vw_destroyBuffer(value->getBuffer(args...));
#endif
  }

  enum class Dest {
    Assign,
    Init,
  };
  enum class Source {
    Copy,
    Take
  };

  template <class Container, class... A>
  static void copyReference(Container *dest, Container *src, Dest d, Source s,
                            A... args) {
    auto *destRefAddr =
        reinterpret_cast<HeapObject **>(dest->getBuffer(args...));

    // Load the source reference.
    auto *srcRef = *reinterpret_cast<HeapObject **>(src->getBuffer(args...));

    // Load the old destination reference so we can release it later if this is
    // an assignment.
    HeapObject *destRef = d == Dest::Assign ? *destRefAddr : nullptr;

    // Do the assignment.
    *destRefAddr = srcRef;

    // If we copy the source retain the reference.
    if (s == Source::Copy)
      swift_retain(srcRef);

    // If we have an assignment release the old reference.
    if (d == Dest::Assign)
      swift_release(destRef);
  }

  template <class Container, class... A>
  static Container *initializeWithCopy(Container *dest, Container *src,
                                       A... args) {
    src->copyTypeInto(dest, args...);
#ifdef SWIFT_RUNTIME_ENABLE_COW_EXISTENTIALS
    auto *type = src->getType();
    auto *vwt = type->getValueWitnesses();

    if (vwt->isValueInline()) {
      auto *destValue =
          reinterpret_cast<OpaqueValue *>(dest->getBuffer(args...));
      auto *srcValue =
          reinterpret_cast<OpaqueValue *>(src->getBuffer(args...));

      type->vw_initializeWithCopy(destValue, srcValue);
    } else {
      // initWithCopy of the reference to the cow box.
      copyReference(dest, src, Dest::Init, Source::Copy, args...);
    }
#else
    src->getType()->vw_initializeBufferWithCopyOfBuffer(dest->getBuffer(args...),
                                                        src->getBuffer(args...));
#endif
    return dest;
  }
  
  template <class Container, class... A>
  static Container *initializeWithTake(Container *dest, Container *src,
                                       A... args) {
    src->copyTypeInto(dest, args...);
#ifdef SWIFT_RUNTIME_ENABLE_COW_EXISTENTIALS
    auto *type = src->getType();
    auto *vwt = type->getValueWitnesses();

    if (vwt->isValueInline()) {
      auto *destValue =
          reinterpret_cast<OpaqueValue *>(dest->getBuffer(args...));
      auto *srcValue =
          reinterpret_cast<OpaqueValue *>(src->getBuffer(args...));

      type->vw_initializeWithTake(destValue, srcValue);
    } else {
      // initWithTake of the reference to the cow box.
      copyReference(dest, src, Dest::Init, Source::Take, args...);
    }
#else
    src->getType()->vw_initializeBufferWithTakeOfBuffer(dest->getBuffer(args...),
                                                        src->getBuffer(args...));
#endif
    return dest;
  }

  template <class Container, class... A>
  static Container *assignWithCopy(Container *dest, Container *src,
                                   A... args) {
    auto srcType = src->getType();
    auto destType = dest->getType();
#ifdef SWIFT_RUNTIME_ENABLE_COW_EXISTENTIALS
    if (src == dest)
      return dest;
    if (srcType == destType) {
      // Types match.
      auto *vwt = srcType->getValueWitnesses();

      if (vwt->isValueInline()) {
        // Inline.
        auto *destValue =
            reinterpret_cast<OpaqueValue *>(dest->getBuffer(args...));
        auto *srcValue =
            reinterpret_cast<OpaqueValue *>(src->getBuffer(args...));
        // assignWithCopy.
        srcType->vw_assignWithCopy(destValue, srcValue);
      } else {
        // Outline (boxed value).
        // assignWithCopy.
        copyReference(dest, src, Dest::Assign, Source::Copy, args...);
      }
    } else {
      // Different types.
      auto *destVwt = destType->getValueWitnesses();
      auto *srcVwt = srcType->getValueWitnesses();
      if (destVwt->isValueInline()) {
        // Inline destination value.
        ValueBuffer tmpBuffer;
        auto *opaqueTmpBuffer = reinterpret_cast<OpaqueValue *>(&tmpBuffer);
        auto *destValue =
            reinterpret_cast<OpaqueValue *>(dest->getBuffer(args...));
        auto *srcValue =
            reinterpret_cast<OpaqueValue *>(src->getBuffer(args...));

        // Move dest value asside so we can destroy it later.
        destType->vw_initializeWithTake(opaqueTmpBuffer, destValue);

        if (srcVwt->isValueInline()) {
          // Inline src value.

          srcType->vw_initializeWithCopy(destValue, srcValue);
        } else {
          // Outline src value.

          // initWithCopy of reference to cow box.
          copyReference(dest, src, Dest::Init, Source::Copy, args...);
        }

        // Finally, destroy the old dest value.
        destType->vw_destroy(opaqueTmpBuffer);
      } else {
        // Outline destination value.

        // Get the dest reference so we can release it later.
        auto *destRef =
            *reinterpret_cast<HeapObject **>(dest->getBuffer(args...));

        if (srcVwt->isValueInline()) {

          // initWithCopy.
          auto *destValue =
              reinterpret_cast<OpaqueValue *>(dest->getBuffer(args...));
          auto *srcValue =
              reinterpret_cast<OpaqueValue *>(src->getBuffer(args...));
          srcType->vw_initializeWithCopy(destValue, srcValue);
        } else {

          // initWithCopy of reference to cow box.
          copyReference(dest, src, Dest::Init, Source::Copy, args...);
        }

        // Release dest reference.
        swift_release(destRef);
      }
    }
    return dest;
#else
    if (srcType == destType) {
      OpaqueValue *srcValue = srcType->vw_projectBuffer(src->getBuffer(args...));
      OpaqueValue *destValue = srcType->vw_projectBuffer(dest->getBuffer(args...));
      srcType->vw_assignWithCopy(destValue, srcValue);
      return dest;
    } else {
      destType->vw_destroyBuffer(dest->getBuffer(args...));
      return initializeWithCopy(dest, src, args...);
    }
#endif
  }

  template <class Container, class... A>
  static Container *assignWithTake(Container *dest, Container *src,
                                   A... args) {
    auto srcType = src->getType();
    auto destType = dest->getType();
#ifdef SWIFT_RUNTIME_ENABLE_COW_EXISTENTIALS
    if (src == dest)
      return dest;

    if (srcType == destType) {
      // Types match.

      auto *vwt = srcType->getValueWitnesses();
      if (vwt->isValueInline()) {
        // Inline.

        auto *destValue =
            reinterpret_cast<OpaqueValue *>(dest->getBuffer(args...));
        auto *srcValue =
            reinterpret_cast<OpaqueValue *>(src->getBuffer(args...));
        // assignWithTake.
        srcType->vw_assignWithTake(destValue, srcValue);
      } else {
        // Outline (boxed value).

        // assignWithTake of reference to cow box.
        copyReference(dest, src, Dest::Assign, Source::Take, args...);
      }
    } else {
      // Different types.

      auto *destVwt = destType->getValueWitnesses();
      auto *srcVwt = srcType->getValueWitnesses();
      if (destVwt->isValueInline()) {
        // Inline destination value.

        ValueBuffer tmpBuffer;
        auto *opaqueTmpBuffer = reinterpret_cast<OpaqueValue *>(&tmpBuffer);
        auto *destValue =
            reinterpret_cast<OpaqueValue *>(dest->getBuffer(args...));
        auto *srcValue =
            reinterpret_cast<OpaqueValue *>(src->getBuffer(args...));

        // Move dest value asside.
        destType->vw_initializeWithTake(opaqueTmpBuffer, destValue);
        if (srcVwt->isValueInline()) {
          // Inline src value.

          srcType->vw_initializeWithTake(destValue, srcValue);
        } else {
          // Outline src value.

          // initWithTake of reference to cow box.
          copyReference(dest, src, Dest::Init, Source::Take, args...);
        }

        // Destroy old dest value.
        destType->vw_destroy(opaqueTmpBuffer);
      } else {
        // Outline destination value.

        // Get the old dest reference.
        auto *destRef =
            *reinterpret_cast<HeapObject **>(dest->getBuffer(args...));

        if (srcVwt->isValueInline()) {
          // initWithCopy.

          auto *destValue =
              reinterpret_cast<OpaqueValue *>(dest->getBuffer(args...));
          auto *srcValue =
              reinterpret_cast<OpaqueValue *>(src->getBuffer(args...));
          // initWithTake.
          srcType->vw_initializeWithTake(destValue, srcValue);
        } else {

          // initWithTake of reference to cow box.
          copyReference(dest, src, Dest::Init, Source::Take, args...);
        }

        // Release old dest reference.
        swift_release(destRef);
      }
    }
    return dest;
#else
    if (srcType == destType) {
      OpaqueValue *srcValue = srcType->vw_projectBuffer(src->getBuffer(args...));
      OpaqueValue *destValue = srcType->vw_projectBuffer(dest->getBuffer(args...));
      srcType->vw_assignWithTake(destValue, srcValue);
      return dest;
    } else {
      destType->vw_destroyBuffer(dest->getBuffer(args...));
      return initializeWithTake(dest, src, args...);
    }
#endif
  }
};

/// The basic layout of an opaque existential with a fixed number of
/// witness tables.  Note that the WitnessTables field is accessed via
/// spooky action from Header.
template <unsigned NumWitnessTables>
struct LLVM_LIBRARY_VISIBILITY FixedOpaqueExistentialContainer {
  OpaqueExistentialContainer Header;
  const void *WitnessTables[NumWitnessTables];
};
// We need to be able to instantiate for NumWitnessTables==0, which
// requires an explicit specialization.
template <>
struct FixedOpaqueExistentialContainer<0> {
  OpaqueExistentialContainer Header;
};

/// A box implementation class for an opaque existential type with
/// a fixed number of witness tables.
template <unsigned NumWitnessTables>
struct LLVM_LIBRARY_VISIBILITY OpaqueExistentialBox
    : OpaqueExistentialBoxBase {
  struct Container : FixedOpaqueExistentialContainer<NumWitnessTables> {
    const Metadata *getType() const {
      return this->Header.Type;
    }
    ValueBuffer *getBuffer() {
      return &this->Header.Buffer;
    }
    void copyTypeInto(Container *dest) const {
      this->Header.copyTypeInto(&dest->Header, NumWitnessTables);
    }
    
    static size_t getContainerStride() {
      return sizeof(Container);
    }
  };
  using type = Container;

  static constexpr size_t size = sizeof(Container);
  static constexpr size_t alignment = alignof(Container);
  static constexpr size_t stride = sizeof(Container);
  static constexpr size_t isPOD = false;
  static constexpr bool isBitwiseTakable = false;
  static constexpr unsigned numExtraInhabitants = 0;
};

/// A non-fixed box implementation class for an opaque existential
/// type with a dynamic number of witness tables.
struct LLVM_LIBRARY_VISIBILITY NonFixedOpaqueExistentialBox
    : OpaqueExistentialBoxBase {
  struct Container {
    OpaqueExistentialContainer Header;

    const Metadata *getType() {
      return Header.Type;
    }
    ValueBuffer *getBuffer(const Metadata *self) {
      return &Header.Buffer;
    }
    void copyTypeInto(Container *dest, const Metadata *self) {
      Header.copyTypeInto(&dest->Header, getNumWitnessTables(self));
    }

    static unsigned getNumWitnessTables(const Metadata *self) {
      auto castSelf = static_cast<const ExistentialTypeMetadata*>(self);
      return castSelf->Flags.getNumWitnessTables();
    }

    static size_t getAlignment(unsigned numWitnessTables) {
      return std::max(alignof(void*), alignof(ValueBuffer));
    }
    static size_t getSize(unsigned numWitnessTables) {
      constexpr size_t base = sizeof(OpaqueExistentialContainer);
      static_assert(base > 0, "stride needs base size > 0");
      return base + numWitnessTables * sizeof(void*);
    }
    static size_t getStride(unsigned numWitnessTables) {
      return getSize(numWitnessTables);
    }
    
    static size_t getContainerStride(const Metadata *self) {
      return getStride(getNumWitnessTables(self));
    }
  };

  using type = Container;
  static constexpr unsigned numExtraInhabitants = 0;
};

/// A common base class for fixed and non-fixed class-existential box
/// implementations.
struct LLVM_LIBRARY_VISIBILITY ClassExistentialBoxBase
    : ExistentialBoxBase<ClassExistentialBoxBase> {
  static constexpr unsigned numExtraInhabitants =
    swift_getHeapObjectExtraInhabitantCount();

  template <class Container, class... A>
  static void destroy(Container *value, A... args) {
    swift_unknownRelease(*value->getValueSlot());
  }
  
  template <class Container, class... A>
  static Container *initializeWithCopy(Container *dest, Container *src,
                                       A... args) {
    src->copyTypeInto(dest, args...);
    auto newValue = *src->getValueSlot();
    *dest->getValueSlot() = newValue;
    swift_unknownRetain(newValue);
    return dest;  
  }

  template <class Container, class... A>
  static Container *initializeWithTake(Container *dest, Container *src,
                                       A... args) {
    src->copyTypeInto(dest, args...);
    *dest->getValueSlot() = *src->getValueSlot();
    return dest;
  }

  template <class Container, class... A>
  static Container *assignWithCopy(Container *dest, Container *src,
                                   A... args) {
    src->copyTypeInto(dest, args...);
    auto newValue = *src->getValueSlot();
    auto oldValue = *dest->getValueSlot();
    *dest->getValueSlot() = newValue;
    swift_unknownRetain(newValue);
    swift_unknownRelease(oldValue);
    return dest;
  }

  template <class Container, class... A>
  static Container *assignWithTake(Container *dest, Container *src,
                                   A... args) {
    src->copyTypeInto(dest, args...);
    auto newValue = *src->getValueSlot();
    auto oldValue = *dest->getValueSlot();
    *dest->getValueSlot() = newValue;
    swift_unknownRelease(oldValue);
    return dest;
  }

  template <class Container, class... A>
  static void storeExtraInhabitant(Container *dest, int index, A... args) {
    swift_storeHeapObjectExtraInhabitant((HeapObject**) dest->getValueSlot(),
                                         index);
  }

  template <class Container, class... A>
  static int getExtraInhabitantIndex(const Container *src, A... args) {
    return swift_getHeapObjectExtraInhabitantIndex(
                                  (HeapObject* const *) src->getValueSlot());
  }
  
};

/// A box implementation class for an existential container with
/// a class constraint and a fixed number of protocol witness tables.
template <unsigned NumWitnessTables>
struct LLVM_LIBRARY_VISIBILITY ClassExistentialBox
    : ClassExistentialBoxBase {
  struct Container {
    ClassExistentialContainer Header;
    const void *TypeInfo[NumWitnessTables];

    void copyTypeInto(Container *dest) const {
      for (unsigned i = 0; i != NumWitnessTables; ++i)
        dest->TypeInfo[i] = TypeInfo[i];
    }
    void **getValueSlot() { return &Header.Value; }
    void * const *getValueSlot() const { return &Header.Value; }
    
    static size_t getContainerStride() { return sizeof(Container); }
  };

  using type = Container;

  static constexpr size_t size = sizeof(Container);
  static constexpr size_t alignment = alignof(Container);
  static constexpr size_t stride = sizeof(Container);
  static constexpr size_t isPOD = false;
  static constexpr size_t isBitwiseTakable = true;
};

/// A non-fixed box implementation class for a class existential
/// type with a dynamic number of witness tables.
struct LLVM_LIBRARY_VISIBILITY NonFixedClassExistentialBox
    : ClassExistentialBoxBase {
  struct Container {
    ClassExistentialContainer Header;

    static unsigned getNumWitnessTables(const Metadata *self) {
      auto castSelf = static_cast<const ExistentialTypeMetadata*>(self); 
      return castSelf->Flags.getNumWitnessTables();
    }

    void copyTypeInto(Container *dest, const Metadata *self) {
      Header.copyTypeInto(&dest->Header, getNumWitnessTables(self));
    }

    void **getValueSlot() { return &Header.Value; }
    void * const *getValueSlot() const { return &Header.Value; }

    static size_t getAlignment(unsigned numWitnessTables) {
      return alignof(void*);
    }
    static size_t getSize(unsigned numWitnessTables) {
      constexpr size_t base = sizeof(ClassExistentialContainer);
      static_assert(base > 0, "stride needs base size > 0");
      return base + numWitnessTables * sizeof(void*);
    }
    static size_t getStride(unsigned numWitnessTables) {
      return getSize(numWitnessTables);
    }
    static size_t getContainerStride(const Metadata *self) {
      return getStride(getNumWitnessTables(self));
    }
  };
  using type = Container;
};

/// A common base class for fixed and non-fixed existential metatype box
/// implementations.
struct LLVM_LIBRARY_VISIBILITY ExistentialMetatypeBoxBase
    : ExistentialBoxBase<ExistentialMetatypeBoxBase> {
  static constexpr unsigned numExtraInhabitants =
    swift_getHeapObjectExtraInhabitantCount();

  template <class Container, class... A>
  static void destroy(Container *value, A... args) {
  }
  
  template <class Container, class... A>
  static Container *initializeWithCopy(Container *dest, Container *src,
                                       A... args) {
    src->copyTypeInto(dest, args...);
    *dest->getValueSlot() = *src->getValueSlot();
    return dest;  
  }

  template <class Container, class... A>
  static Container *initializeWithTake(Container *dest, Container *src,
                                       A... args) {
    src->copyTypeInto(dest, args...);
    *dest->getValueSlot() = *src->getValueSlot();
    return dest;
  }

  template <class Container, class... A>
  static Container *assignWithCopy(Container *dest, Container *src,
                                   A... args) {
    src->copyTypeInto(dest, args...);
    *dest->getValueSlot() = *src->getValueSlot();
    return dest;
  }

  template <class Container, class... A>
  static Container *assignWithTake(Container *dest, Container *src,
                                   A... args) {
    src->copyTypeInto(dest, args...);
    *dest->getValueSlot() = *src->getValueSlot();
    return dest;
  }

  template <class Container, class... A>
  static void storeExtraInhabitant(Container *dest, int index, A... args) {
    swift_storeHeapObjectExtraInhabitant((HeapObject**) dest->getValueSlot(),
                                         index);
  }

  template <class Container, class... A>
  static int getExtraInhabitantIndex(const Container *src, A... args) {
    return swift_getHeapObjectExtraInhabitantIndex(
                                  (HeapObject* const *) src->getValueSlot());
  }
  
};

/// A box implementation class for an existential metatype container
/// with a fixed number of protocol witness tables.
template <unsigned NumWitnessTables>
struct LLVM_LIBRARY_VISIBILITY ExistentialMetatypeBox
    : ExistentialMetatypeBoxBase {
  struct Container {
    ExistentialMetatypeContainer Header;
    const void *TypeInfo[NumWitnessTables];

    void copyTypeInto(Container *dest) const {
      for (unsigned i = 0; i != NumWitnessTables; ++i)
        dest->TypeInfo[i] = TypeInfo[i];
    }
    const Metadata **getValueSlot() { return &Header.Value; }
    const Metadata * const *getValueSlot() const { return &Header.Value; }
    
    static size_t getContainerStride() { return sizeof(Container); }
  };

  using type = Container;

  static constexpr size_t size = sizeof(Container);
  static constexpr size_t alignment = alignof(Container);
  static constexpr size_t stride = sizeof(Container);
  static constexpr size_t isPOD = true;
  static constexpr size_t isBitwiseTakable = true;
};

/// A non-fixed box implementation class for an existential metatype
/// type with a dynamic number of witness tables.
struct LLVM_LIBRARY_VISIBILITY NonFixedExistentialMetatypeBox
    : ExistentialMetatypeBoxBase {
  struct Container {
    ExistentialMetatypeContainer Header;

    static unsigned getNumWitnessTables(const Metadata *self) {
      auto castSelf = static_cast<const ExistentialTypeMetadata*>(self); 
      return castSelf->Flags.getNumWitnessTables();
    }

    void copyTypeInto(Container *dest, const Metadata *self) {
      Header.copyTypeInto(&dest->Header, getNumWitnessTables(self));
    }

    const Metadata **getValueSlot() { return &Header.Value; }
    const Metadata * const *getValueSlot() const { return &Header.Value; }

    static size_t getAlignment(unsigned numWitnessTables) {
      return alignof(void*);
    }
    static size_t getSize(unsigned numWitnessTables) {
      constexpr size_t base = sizeof(ExistentialMetatypeContainer);
      static_assert(base > 0, "stride needs base size > 0");
      return base + numWitnessTables * sizeof(void*);
    }
    static size_t getStride(unsigned numWitnessTables) {
      return getSize(numWitnessTables);
    }
    static size_t getContainerStride(const Metadata *self) {
      return getStride(getNumWitnessTables(self));
    }
  };
  using type = Container;
};

} // end namespace metadataimpl
} // end namespace swift

#endif /* SWIFT_RUNTIME_EXISTENTIALMETADATAIMPL_H */
