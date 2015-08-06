//===--- Private.h - Private runtime declarations --------------*- C++ -*--===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Private declarations of the Swift runtime.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_RUNTIME_PRIVATE_H
#define SWIFT_RUNTIME_PRIVATE_H

#include "swift/Runtime/Config.h"
#include "swift/Runtime/Metadata.h"
#include "llvm/Support/Compiler.h"

#if SWIFT_OBJC_INTEROP && defined(__OBJC2__)
#include <objc/objc.h>

// Redeclare APIs from the Objective-C runtime.
// These functions are not available through public headers, but are guaranteed
// to exist on OS X >= 10.9 and iOS >= 7.0.

extern "C" id objc_retain(id);
extern "C" void objc_release(id);
extern "C" id _objc_rootAutorelease(id);
extern "C" void objc_moveWeak(id*, id*);
extern "C" void objc_copyWeak(id*, id*);
extern "C" id objc_initWeak(id*, id);
extern "C" id objc_storeWeak(id*, id);
extern "C" void objc_destroyWeak(id*);
extern "C" id objc_loadWeakRetained(id*);

// Description of an Objective-C image.
// __DATA,__objc_imageinfo stores one of these.
typedef struct objc_image_info {
    uint32_t version; // currently 0
    uint32_t flags;
} objc_image_info;

// Class and metaclass construction from a compiler-generated memory image.
// cls and cls->isa must each be OBJC_MAX_CLASS_SIZE bytes.·
// Extra bytes not used the the metadata must be zero.
// info is the same objc_image_info that would be emitted by a static compiler.
// Returns nil if a class with the same name already exists.
// Returns nil if the superclass is nil and the class is not marked as a root.
// Returns nil if the superclass is under construction.
// Do not call objc_registerClassPair().
OBJC_EXPORT Class objc_readClassPair(Class cls,
                                     const struct objc_image_info *info)
    __OSX_AVAILABLE_STARTING(__MAC_10_10, __IPHONE_8_0);
#endif

namespace swift {
  struct ProtocolDescriptor;

#if SWIFT_HAS_ISA_MASKING
  extern "C" uintptr_t swift_isaMask;
#endif

#if SWIFT_OBJC_INTEROP
  extern "C" LLVM_LIBRARY_VISIBILITY
  bool _swift_objectConformsToObjCProtocol(const void *theObject,
                                    const ProtocolDescriptor *theProtocol);
  
  extern "C" LLVM_LIBRARY_VISIBILITY
  bool _swift_classConformsToObjCProtocol(const void *theClass,
                                    const ProtocolDescriptor *theProtocol);
#endif

  extern "C" LLVM_LIBRARY_VISIBILITY LLVM_ATTRIBUTE_NORETURN
  void _swift_abortRetainUnowned(const void *object);

  extern "C" LLVM_LIBRARY_VISIBILITY
  void _swift_deallocClassInstance(HeapObject *object);

  /// Is the given value a valid alignment mask?
  static inline bool isAlignmentMask(size_t mask) {
    // mask          == xyz01111...
    // mask+1        == xyz10000...
    // mask&(mask+1) == xyz00000...
    // So this is nonzero if and only if there any bits set
    // other than an arbitrarily long sequence of low bits.
    return (mask & (mask + 1)) == 0;
  }

  /// Return the class of an object which is known to be an allocated
  /// heap object.
  static inline const ClassMetadata *_swift_getClassOfAllocated(const void *object) {
    // Load the isa field.
    uintptr_t bits = *reinterpret_cast<const uintptr_t*>(object);

#if SWIFT_HAS_ISA_MASKING
    // Apply the mask.
    bits &= swift_isaMask;
#endif

    // The result is a class pointer.
    return reinterpret_cast<const ClassMetadata *>(bits);
  }

  /// Is the given value an Objective-C tagged pointer?
  static inline bool isObjCTaggedPointer(const void *object) {
#if SWIFT_OBJC_INTEROP
    return (((uintptr_t) object) & heap_object_abi::ObjCReservedBitsMask);
#else
    assert(!(((uintptr_t) object) & heap_object_abi::ObjCReservedBitsMask));
    return false;
#endif
  }

  static inline bool isObjCTaggedPointerOrNull(const void *object) {
    return object == nullptr || isObjCTaggedPointer(object);
  }

  LLVM_LIBRARY_VISIBILITY
  const ClassMetadata *_swift_getClass(const void *object);

  static inline
  const ClassMetadata *_swift_getSuperclass(const ClassMetadata *theClass) {
    return theClass->SuperClass;
  }

  LLVM_LIBRARY_VISIBILITY
  bool usesNativeSwiftReferenceCounting(const ClassMetadata *theClass);

  /// Get the superclass pointer value used for Swift root classes.
  /// Note that this function may return a nullptr on non-objc platforms,
  /// where there is no common root class. rdar://problem/18987058
  const ClassMetadata *getRootSuperclass();
  
  /// Replace entries of a freshly-instantiated value witness table with more
  /// efficient common implementations where applicable.
  ///
  /// For instance, if the value witness table represents a POD type, this will
  /// insert POD value witnesses into the table. The vwtable's flags must have
  /// been initialized before calling this function.
  ///
  /// Returns true if common value witnesses were used, false otherwise.
  void installCommonValueWitnesses(ValueWitnessTable *vwtable);


} // end namespace swift

#endif /* SWIFT_RUNTIME_PRIVATE_H */
