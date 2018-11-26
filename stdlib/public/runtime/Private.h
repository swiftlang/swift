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

#include "swift/Demangling/Demangler.h"
#include "swift/Runtime/Config.h"
#include "swift/Runtime/Metadata.h"
#include "llvm/Support/Compiler.h"

// Opaque ISAs need to use object_getClass which is in runtime.h
#if SWIFT_HAS_OPAQUE_ISAS
#include <objc/runtime.h>
#endif

namespace swift {

class TypeReferenceOwnership {
  enum : uint8_t {
    Weak = 1 << 0,
    Unowned = 1 << 1,
    Unmanaged = 1 << 2,
  };

  uint8_t Data;

  constexpr TypeReferenceOwnership(uint8_t Data) : Data(Data) {}

public:
  constexpr TypeReferenceOwnership() : Data(0) {}

#define REF_STORAGE(Name, ...) \
  void set##Name() { Data |= Name; } \
  bool is##Name() const { return Data & Name; }
#include "swift/AST/ReferenceStorage.def"
};

/// Type information consists of metadata and its ownership info,
/// such information is used by `_typeByMangledName` accessor
/// since we don't represent ownership attributes in the metadata
/// itself related info has to be bundled with it.
class TypeInfo {
  const Metadata *Type;
  TypeReferenceOwnership ReferenceOwnership;

public:
  TypeInfo() : Type(nullptr), ReferenceOwnership() {}

  TypeInfo(const Metadata *type, TypeReferenceOwnership ownership)
      : Type(type), ReferenceOwnership(ownership) {}

  operator const Metadata *() { return Type; }

  bool isWeak() const { return ReferenceOwnership.isWeak(); }
  bool isUnowned() const { return ReferenceOwnership.isUnowned(); }
  bool isUnmanaged() const { return ReferenceOwnership.isUnmanaged(); }
};

#if SWIFT_HAS_ISA_MASKING
  SWIFT_RUNTIME_EXPORT
  uintptr_t swift_isaMask;
#endif

#if SWIFT_OBJC_INTEROP
  bool objectConformsToObjCProtocol(const void *theObject,
                                    ProtocolDescriptorRef protocol);
  
  bool classConformsToObjCProtocol(const void *theClass,
                                   ProtocolDescriptorRef protocol);
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
    id idObject = reinterpret_cast<id>(const_cast<void *>(object));
    return reinterpret_cast<const ClassMetadata*>(object_getClass(idObject));
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
    return  (c->Superclass && c->Superclass != getRootSuperclass());
  }

  /// Replace entries of a freshly-instantiated value witness table with more
  /// efficient common implementations where applicable.
  ///
  /// All information is taken from the passed-in layout rather than the VWT.
  /// This is so that we can delay "publishing" the flags in the actual
  /// value witness table until all required changes have been made.
  ///
  /// For instance, if the value witness table represents a POD type, this will
  /// insert POD value witnesses into the table. The vwtable's flags must have
  /// been initialized before calling this function.
  ///
  /// Returns true if common value witnesses were used, false otherwise.
  void installCommonValueWitnesses(const TypeLayout &layout,
                                   ValueWitnessTable *vwtable);

  const Metadata *
  _matchMetadataByMangledTypeName(const llvm::StringRef metadataNameRef,
                                  const Metadata *metadata,
                                  const TypeContextDescriptor *ntd);

  bool
  _contextDescriptorMatchesMangling(const ContextDescriptor *context,
                                    Demangle::NodePointer node);
  
  const TypeContextDescriptor *
  _searchConformancesByMangledTypeName(Demangle::NodePointer node);

  Demangle::NodePointer _swift_buildDemanglingForMetadata(const Metadata *type,
                                                      Demangle::Demangler &Dem);

  /// Callback used to provide the substitution for a generic parameter
  /// referenced by a "flat" index (where all depths have been collapsed)
  /// to its metadata.
  using SubstFlatGenericParameterFn =
    llvm::function_ref<const Metadata *(unsigned flatIndex)>;

  /// Callback used to provide the substitution of a generic parameter
  /// (described by depth/index) to its metadata.
  using SubstGenericParameterFn =
    llvm::function_ref<const Metadata *(unsigned depth, unsigned index)>;

  /// Retrieve the type metadata described by the given type name.
  ///
  /// \p substGenericParam Function that provides generic argument metadata
  /// given a particular generic parameter specified by depth/index.
  TypeInfo _getTypeByMangledName(StringRef typeName,
                                 SubstGenericParameterFn substGenericParam);

  /// Gather generic parameter counts from a context descriptor.
  ///
  /// \returns true if the innermost descriptor is generic.
  bool _gatherGenericParameterCounts(const ContextDescriptor *descriptor,
                                     std::vector<unsigned> &genericParamCounts);

  /// Map depth/index to a flat index.
  llvm::Optional<unsigned> _depthIndexToFlatIndex(
                                          unsigned depth, unsigned index,
                                          llvm::ArrayRef<unsigned> paramCounts);

  /// Check the given generic requirements using the given set of generic
  /// arguments, collecting the key arguments (e.g., witness tables) for
  /// the caller.
  ///
  /// \param requirements The set of requirements to evaluate.
  ///
  /// \param extraArguments The extra arguments determined while checking
  /// generic requirements (e.g., those that need to be
  /// passed to an instantiation function) will be added to this vector.
  ///
  /// \returns true if an error occurred, false otherwise.
  bool _checkGenericRequirements(
                    llvm::ArrayRef<GenericRequirementDescriptor> requirements,
                    std::vector<const void *> &extraArguments,
                    SubstFlatGenericParameterFn substFlatGenericParam,
                    SubstGenericParameterFn substGenericParam);

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

  static inline const Metadata *getMetadataForClass(const ClassMetadata *c) {
#if SWIFT_OBJC_INTEROP
    return swift_getObjCClassMetadata(c);
#else
    return c;
#endif
  }

  void *allocateMetadata(size_t size, size_t align);

  Demangle::NodePointer
  _buildDemanglingForContext(const ContextDescriptor *context,
                             llvm::ArrayRef<NodePointer> demangledGenerics,
                             bool concretizedGenerics,
                             Demangle::Demangler &Dem);
  
  /// Symbolic reference resolver that produces the demangling tree for the
  /// referenced context.
  class ResolveToDemanglingForContext {
    Demangle::Demangler &Dem;
  public:
    explicit ResolveToDemanglingForContext(Demangle::Demangler &Dem)
      : Dem(Dem) {}
    
    Demangle::NodePointer operator()(int32_t offset, const void *base) {
      auto descriptor =
        (const ContextDescriptor *)detail::applyRelativeOffset(base, offset);
      
      return _buildDemanglingForContext(descriptor, {}, false, Dem);
    }
  };

  /// Is the given type imported from a C tag type?
  bool isCImportedTagType(const TypeContextDescriptor *type);

  /// Check whether a type conforms to a protocol.
  ///
  /// \param value - can be null, in which case the question should
  ///   be answered abstractly if possible
  /// \param conformance - if non-null, and the protocol requires a
  ///   witness table, and the type implements the protocol, the witness
  ///   table will be placed here
  bool _conformsToProtocol(const OpaqueValue *value,
                           const Metadata *type,
                           ProtocolDescriptorRef protocol,
                           const WitnessTable **conformance);

  void _swift_getFieldAt(
      const Metadata *type, unsigned index,
      std::function<void(llvm::StringRef name, FieldType type)> callback);

} // end namespace swift

#endif /* SWIFT_RUNTIME_PRIVATE_H */
