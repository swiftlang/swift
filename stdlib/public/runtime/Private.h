//===--- Private.h - Private runtime declarations ---------------*- C++ -*-===//
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
// Private declarations of the Swift runtime.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_RUNTIME_PRIVATE_H
#define SWIFT_RUNTIME_PRIVATE_H

#include "swift/Demangling/Demangle.h"
#include "swift/Runtime/Config.h"
#include "swift/Runtime/Metadata.h"
#include "llvm/Support/Compiler.h"

// Opaque ISAs need to use object_getClass which is in runtime.h
#if SWIFT_HAS_OPAQUE_ISAS
#include <objc/runtime.h>
#endif

namespace swift {

#if SWIFT_HAS_ISA_MASKING
  SWIFT_RUNTIME_EXPORT
  uintptr_t swift_isaMask;
#endif

#if SWIFT_OBJC_INTEROP
  bool objectConformsToObjCProtocol(const void *theObject,
                                    const ProtocolDescriptor *theProtocol);
  
  bool classConformsToObjCProtocol(const void *theClass,
                                    const ProtocolDescriptor *theProtocol);
#endif

  /// Is the given value a valid alignment mask?
  static inline bool isAlignmentMask(size_t mask) {
    // mask          == xyz01111...
    // mask+1        == xyz10000...
    // mask&(mask+1) == xyz00000...
    // So this is nonzero if and only if there any bits set
    // other than an arbitrarily long sequence of low bits.
    return (mask & (mask + 1)) == 0;
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

  /// Return the class of an object which is known to be an allocated
  /// heap object.
  /// Note, in this case, the object may or may not have a non-pointer ISA.
  /// Masking, or otherwise, may be required to get a class pointer.
  static inline const ClassMetadata *_swift_getClassOfAllocated(const void *object) {
#if SWIFT_HAS_OPAQUE_ISAS
    // The ISA is opaque so masking it will not return a pointer.  We instead
    // need to call the objc runtime to get the class.
    return reinterpret_cast<const ClassMetadata*>(object_getClass((id)object));
#else
    // Load the isa field.
    uintptr_t bits = *reinterpret_cast<const uintptr_t*>(object);

#if SWIFT_HAS_ISA_MASKING
    // Apply the mask.
    bits &= swift_isaMask;
#endif

    // The result is a class pointer.
    return reinterpret_cast<const ClassMetadata *>(bits);
#endif
  }

  /// Return the class of an object which is known to be an allocated
  /// heap object.
  /// Note, in this case, the object is known to have a pointer ISA, and no
  /// masking is required to convert from non-pointer to pointer ISA.
  static inline const ClassMetadata *
  _swift_getClassOfAllocatedFromPointer(const void *object) {
    // Load the isa field.
    uintptr_t bits = *reinterpret_cast<const uintptr_t*>(object);

    // The result is a class pointer.
    return reinterpret_cast<const ClassMetadata *>(bits);
  }

#if SWIFT_OBJC_INTEROP && SWIFT_HAS_OPAQUE_ISAS
  /// Return whether this object is of a class which uses non-pointer ISAs.
  static inline bool _swift_isNonPointerIsaObjCClass(const void *object) {
    // Load the isa field.
    uintptr_t bits = *reinterpret_cast<const uintptr_t*>(object);
    // If the low bit is set, then we are definitely an objc object.
    // FIXME: Use a variable for this.
    return bits & 1;
  }
#endif

  LLVM_LIBRARY_VISIBILITY
  const ClassMetadata *_swift_getClass(const void *object);

  static inline
  const ClassMetadata *_swift_getSuperclass(const ClassMetadata *theClass) {
    return theClass->SuperClass;
  }

  LLVM_LIBRARY_VISIBILITY
  bool usesNativeSwiftReferenceCounting(const ClassMetadata *theClass);

  static inline
  bool objectUsesNativeSwiftReferenceCounting(const void *object) {
    assert(!isObjCTaggedPointerOrNull(object));
#if SWIFT_HAS_OPAQUE_ISAS
    // Fast path for opaque ISAs.  We don't want to call
    // _swift_getClassOfAllocated as that will call object_getClass.
    // Instead we can look at the bits in the ISA and tell if its a
    // non-pointer opaque ISA which means it is definitely an ObjC
    // object and doesn't use native swift reference counting.
    if (_swift_isNonPointerIsaObjCClass(object))
      return false;
    return usesNativeSwiftReferenceCounting(_swift_getClassOfAllocatedFromPointer(object));
#else
    return usesNativeSwiftReferenceCounting(_swift_getClassOfAllocated(object));
#endif
  }

  /// Get the superclass pointer value used for Swift root classes.
  /// Note that this function may return a nullptr on non-objc platforms,
  /// where there is no common root class. rdar://problem/18987058
  const ClassMetadata *getRootSuperclass();

  /// Check if a class has a formal superclass in the AST.
  static inline
  bool classHasSuperclass(const ClassMetadata *c) {
    return (c->SuperClass && c->SuperClass != getRootSuperclass());
  }

  /// Replace entries of a freshly-instantiated value witness table with more
  /// efficient common implementations where applicable.
  ///
  /// For instance, if the value witness table represents a POD type, this will
  /// insert POD value witnesses into the table. The vwtable's flags must have
  /// been initialized before calling this function.
  ///
  /// Returns true if common value witnesses were used, false otherwise.
  void installCommonValueWitnesses(ValueWitnessTable *vwtable);

  const Metadata *
  _matchMetadataByMangledTypeName(const llvm::StringRef metadataNameRef,
                                  const Metadata *metadata,
                                  const NominalTypeDescriptor *ntd);

  const Metadata *
  _searchConformancesByMangledTypeName(const llvm::StringRef typeName);

  Demangle::NodePointer _swift_buildDemanglingForMetadata(const Metadata *type,
                                                      Demangle::Demangler &Dem);

  /// A helper function which avoids performing a store if the destination
  /// address already contains the source value.  This is useful when
  /// "initializing" memory that might have been initialized to the correct
  /// value statically.  In such a case, the compiler might have gone so far
  /// as to map the entire object readonly, or we might just want to avoid
  /// dirtying memory unnecessarily.
  template <class T>
  static void assignUnlessEqual(T &dest, T newValue) {
    if (dest != newValue)
      dest = newValue;
  }

#if defined(__CYGWIN__)
  void _swift_once_f(uintptr_t *predicate, void *context,
                     void (*function)(void *));
#endif

} // end namespace swift

#endif /* SWIFT_RUNTIME_PRIVATE_H */
