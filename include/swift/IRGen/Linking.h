//===--- Linking.h - Named declarations and how to link to them -*- C++ -*-===//
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

#ifndef SWIFT_IRGEN_LINKING_H
#define SWIFT_IRGEN_LINKING_H

#include "swift/AST/Decl.h"
#include "swift/AST/Module.h"
#include "swift/AST/ProtocolAssociations.h"
#include "swift/AST/ProtocolConformance.h"
#include "swift/AST/Types.h"
#include "swift/IRGen/ValueWitness.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILGlobalVariable.h"
#include "swift/SIL/SILModule.h"
#include "llvm/ADT/DenseMapInfo.h"
#include "llvm/IR/GlobalValue.h"

namespace llvm {
class Triple;
}

namespace swift {
namespace irgen {
class IRGenModule;
class Alignment;

/// Determine if the triple uses the DLL storage.
bool useDllStorage(const llvm::Triple &triple);

class UniversalLinkageInfo {
public:
  bool IsELFObject;
  bool UseDLLStorage;

  /// True iff are multiple llvm modules.
  bool HasMultipleIGMs;

  bool IsWholeModule;

  explicit UniversalLinkageInfo(IRGenModule &IGM);

  UniversalLinkageInfo(const llvm::Triple &triple, bool hasMultipleIGMs,
                       bool isWholeModule);

  /// In case of multiple llvm modules (in multi-threaded compilation) all
  /// private decls must be visible from other files.
  bool shouldAllPrivateDeclsBeVisibleFromOtherFiles() const {
    return HasMultipleIGMs;
  }
  /// In case of multipe llvm modules, private lazy protocol
  /// witness table accessors could be emitted by two different IGMs during
  /// IRGen into different object files and the linker would complain about
  /// duplicate symbols.
  bool needLinkerToMergeDuplicateSymbols() const { return HasMultipleIGMs; }
};

/// Selector for type metadata symbol kinds.
enum class TypeMetadataAddress {
  AddressPoint,
  FullMetadata,
};

/// A link entity is some sort of named declaration, combined with all
/// the information necessary to distinguish specific implementations
/// of the declaration from each other.
///
/// For example, functions may be uncurried at different levels, each of
/// which potentially creates a different top-level function.
class LinkEntity {
  /// ValueDecl*, SILFunction*, or TypeBase*, depending on Kind.
  void *Pointer;

  /// ProtocolConformance*, depending on Kind.
  void *SecondaryPointer;

  /// A hand-rolled bitfield with the following layout:
  unsigned Data;

  enum : unsigned {
    KindShift = 0, KindMask = 0xFF,

    // This field appears in the ValueWitness kind.
    ValueWitnessShift = 8, ValueWitnessMask = 0xFF00,

    // This field appears in the TypeMetadata kind.
    MetadataAddressShift = 8, MetadataAddressMask = 0x0300,

    // This field appears in associated type access functions.
    AssociatedTypeIndexShift = 8, AssociatedTypeIndexMask = ~KindMask,

    // This field appears in associated conformance access functions.
    AssociatedConformanceIndexShift = 8,
    AssociatedConformanceIndexMask = ~KindMask,
  };
#define LINKENTITY_SET_FIELD(field, value) (value << field##Shift)
#define LINKENTITY_GET_FIELD(value, field) ((value & field##Mask) >> field##Shift)

  enum class Kind {
    /// A method dispatch thunk.  The pointer is a FuncDecl* inside a protocol
    /// or a class.
    DispatchThunk,

    /// A method dispatch thunk for an initializing constructor.  The pointer
    /// is a ConstructorDecl* inside a class.
    DispatchThunkInitializer,

    /// A method dispatch thunk for an allocating constructor.  The pointer is a
    /// ConstructorDecl* inside a protocol or a class.
    DispatchThunkAllocator,

    /// A method descriptor.  The pointer is a FuncDecl* inside a protocol
    /// or a class.
    MethodDescriptor,

    /// A method descriptor for an initializing constructor.  The pointer
    /// is a ConstructorDecl* inside a class.
    MethodDescriptorInitializer,

    /// A method descriptor for an allocating constructor.  The pointer is a
    /// ConstructorDecl* inside a protocol or a class.
    MethodDescriptorAllocator,

    /// A method lookup function for a class.  The pointer is a ClassDecl*.
    MethodLookupFunction,

    /// A resilient enum tag index. The pointer is a EnumElementDecl*.
    EnumCase,

    /// A field offset.  The pointer is a VarDecl*.
    FieldOffset,

    /// An Objective-C class reference.  The pointer is a ClassDecl*.
    ObjCClass,

    /// An Objective-C class reference reference.  The pointer is a ClassDecl*.
    ObjCClassRef,

    /// An Objective-C metaclass reference.  The pointer is a ClassDecl*.
    ObjCMetaclass,

    /// A swift metaclass-stub reference.  The pointer is a ClassDecl*.
    SwiftMetaclassStub,

    /// A class metadata base offset global variable.  This stores the offset
    /// of the immediate members of a class (generic parameters, field offsets,
    /// vtable offsets) in the class's metadata.  The immediate members begin
    /// immediately after the superclass members end.
    ///
    /// The pointer is a ClassDecl*.
    ClassMetadataBaseOffset,

    /// The property descriptor for a public property or subscript.
    /// The pointer is an AbstractStorageDecl*.
    PropertyDescriptor,

    /// The nominal type descriptor for a nominal type.
    /// The pointer is a NominalTypeDecl*.
    NominalTypeDescriptor,

    /// The metadata pattern for a generic nominal type.
    /// The pointer is a NominalTypeDecl*.
    TypeMetadataPattern,

    /// The instantiation cache for a generic nominal type.
    /// The pointer is a NominalTypeDecl*.
    TypeMetadataInstantiationCache,

    /// The instantiation function for a generic nominal type.
    /// The pointer is a NominalTypeDecl*.
    TypeMetadataInstantiationFunction,

    /// The in-place initialization cache for a generic nominal type.
    /// The pointer is a NominalTypeDecl*.
    TypeMetadataSingletonInitializationCache,

    /// The completion function for a generic or resilient nominal type.
    /// The pointer is a NominalTypeDecl*.
    TypeMetadataCompletionFunction,

    /// The module descriptor for a module.
    /// The pointer is a ModuleDecl*.
    ModuleDescriptor,

    /// The protocol descriptor for a protocol type.
    /// The pointer is a ProtocolDecl*.
    ProtocolDescriptor,

    /// The alias referring to the base of the requirements within the
    /// protocol descriptor, which is used to determine the offset of a
    /// particular requirement in the witness table.
    /// The pointer is a ProtocolDecl*.
    ProtocolRequirementsBaseDescriptor,

    /// An descriptor for an associated type within a protocol, which
    /// will alias the TargetProtocolRequirement descripting this
    /// particular associated type.
    /// The pointer is an AssociatedTypeDecl*.
    AssociatedTypeDescriptor,

    /// An descriptor for an associated conformance within a protocol, which
    /// will alias the TargetProtocolRequirement descripting this
    /// particular associated conformance.
    /// The pointer is a ProtocolDecl*; the index of the associated conformance
    /// is stored in the data.
    AssociatedConformanceDescriptor,

    /// A default accessor for an associated conformance of a protocol.
    /// The pointer is a ProtocolDecl*; the index of the associated conformance
    /// is stored in the data.
    DefaultAssociatedConformanceAccessor,

    /// A SIL function. The pointer is a SILFunction*.
    SILFunction,

    /// The descriptor for an extension.
    /// The pointer is an ExtensionDecl*.
    ExtensionDescriptor,
    
    /// The descriptor for a runtime-anonymous context.
    /// The pointer is the DeclContext* of a child of the context that should
    /// be considered private.
    AnonymousDescriptor,
    
    /// A SIL global variable. The pointer is a SILGlobalVariable*.
    SILGlobalVariable,

    // These next few are protocol-conformance kinds.

    /// A direct protocol witness table. The secondary pointer is a
    /// ProtocolConformance*.
    DirectProtocolWitnessTable,

    /// A protocol witness table pattern. The secondary pointer is a
    /// ProtocolConformance*.
    ProtocolWitnessTablePattern,

    /// A witness accessor function. The secondary pointer is a
    /// ProtocolConformance*.
    ProtocolWitnessTableAccessFunction,

    /// The instantiation function for a generic protocol witness table.
    /// The secondary pointer is a ProtocolConformance*.
    GenericProtocolWitnessTableInstantiationFunction,

    /// A function which returns the witness table for a protocol-constrained
    /// associated type of a protocol.  The secondary pointer is a
    /// ProtocolConformance*.  The index of the associated conformance
    /// requirement is stored in the data.
    AssociatedTypeWitnessTableAccessFunction,

    /// A reflection metadata descriptor for the associated type witnesses of a
    /// nominal type in a protocol conformance.
    ReflectionAssociatedTypeDescriptor,

    /// The protocol conformance descriptor for a conformance.
    /// The pointer is a NormalProtocolConformance*.
    ProtocolConformanceDescriptor,

    // These are both type kinds and protocol-conformance kinds.

    /// A lazy protocol witness accessor function. The pointer is a
    /// canonical TypeBase*, and the secondary pointer is a
    /// ProtocolConformance*.
    ProtocolWitnessTableLazyAccessFunction,

    /// A lazy protocol witness cache variable. The pointer is a
    /// canonical TypeBase*, and the secondary pointer is a
    /// ProtocolConformance*.
    ProtocolWitnessTableLazyCacheVariable,

    // Everything following this is a type kind.

    /// A value witness for a type.
    /// The pointer is a canonical TypeBase*.
    ValueWitness,

    /// The value witness table for a type.
    /// The pointer is a canonical TypeBase*.
    ValueWitnessTable,

    /// The metadata or metadata template for a type.
    /// The pointer is a canonical TypeBase*.
    TypeMetadata,

    /// An access function for type metadata.
    /// The pointer is a canonical TypeBase*.
    TypeMetadataAccessFunction,

    /// A lazy cache variable for type metadata.
    /// The pointer is a canonical TypeBase*.
    TypeMetadataLazyCacheVariable,

    /// A foreign type metadata candidate.
    /// The pointer is a canonical TypeBase*.
    ForeignTypeMetadataCandidate,

    /// A reflection metadata descriptor for a builtin or imported type.
    ReflectionBuiltinDescriptor,

    /// A reflection metadata descriptor for a struct, enum, class or protocol.
    ReflectionFieldDescriptor,

    /// A coroutine continuation prototype function.
    CoroutineContinuationPrototype,
  };
  friend struct llvm::DenseMapInfo<LinkEntity>;

  Kind getKind() const {
    return Kind(LINKENTITY_GET_FIELD(Data, Kind));
  }

  static bool isDeclKind(Kind k) {
    return k <= Kind::DefaultAssociatedConformanceAccessor;
  }
  static bool isTypeKind(Kind k) {
    return k >= Kind::ProtocolWitnessTableLazyAccessFunction;
  }

  static bool isProtocolConformanceKind(Kind k) {
    return (k >= Kind::DirectProtocolWitnessTable &&
            k <= Kind::ProtocolWitnessTableLazyCacheVariable);
  }

  void setForDecl(Kind kind, const ValueDecl *decl) {
    assert(isDeclKind(kind));
    Pointer = const_cast<void*>(static_cast<const void*>(decl));
    SecondaryPointer = nullptr;
    Data = LINKENTITY_SET_FIELD(Kind, unsigned(kind));
  }

  void setForProtocolAndAssociatedConformance(Kind kind,
                                              const ProtocolDecl *proto,
                                              CanType associatedType,
                                              ProtocolDecl *associatedProtocol){
    assert(isDeclKind(kind));
    Pointer = static_cast<ValueDecl *>(const_cast<ProtocolDecl *>(proto));
    SecondaryPointer = nullptr;
    Data = LINKENTITY_SET_FIELD(Kind, unsigned(kind)) |
           LINKENTITY_SET_FIELD(AssociatedConformanceIndex,
                                getAssociatedConformanceIndex(
                                                          proto,
                                                          associatedType,
                                                          associatedProtocol));
  }

  void setForProtocolConformance(Kind kind, const ProtocolConformance *c) {
    assert(isProtocolConformanceKind(kind) && !isTypeKind(kind));
    Pointer = nullptr;
    SecondaryPointer = const_cast<void*>(static_cast<const void*>(c));
    Data = LINKENTITY_SET_FIELD(Kind, unsigned(kind));
  }

  void setForProtocolConformanceAndType(Kind kind, const ProtocolConformance *c,
                                        CanType type) {
    assert(isProtocolConformanceKind(kind) && isTypeKind(kind));
    Pointer = type.getPointer();
    SecondaryPointer = const_cast<void*>(static_cast<const void*>(c));
    Data = LINKENTITY_SET_FIELD(Kind, unsigned(kind));
  }

  void setForProtocolConformanceAndAssociatedType(Kind kind,
                                                  const ProtocolConformance *c,
                                                  AssociatedTypeDecl *associate) {
    assert(isProtocolConformanceKind(kind));
    Pointer = nullptr;
    SecondaryPointer = const_cast<void*>(static_cast<const void*>(c));
    Data = LINKENTITY_SET_FIELD(Kind, unsigned(kind)) |
           LINKENTITY_SET_FIELD(AssociatedTypeIndex,
                                getAssociatedTypeIndex(c, associate));
  }

  void setForProtocolConformanceAndAssociatedConformance(Kind kind,
                                                  const ProtocolConformance *c,
                                                  CanType associatedType,
                                            ProtocolDecl *associatedProtocol) {
    assert(isProtocolConformanceKind(kind));
    Pointer = associatedProtocol;
    SecondaryPointer = const_cast<void*>(static_cast<const void*>(c));
    Data = LINKENTITY_SET_FIELD(Kind, unsigned(kind)) |
           LINKENTITY_SET_FIELD(AssociatedConformanceIndex,
                                getAssociatedConformanceIndex(c, associatedType,
                                                          associatedProtocol));
  }

  // We store associated types using their index in their parent protocol
  // in order to avoid bloating LinkEntity out to three key pointers.
  static unsigned getAssociatedTypeIndex(const ProtocolConformance *conformance,
                                         AssociatedTypeDecl *associate) {
    assert(conformance->getProtocol() == associate->getProtocol());
    unsigned result = 0;
    for (auto requirement : associate->getProtocol()->getMembers()) {
      if (requirement == associate) return result;
      if (isa<AssociatedTypeDecl>(requirement)) result++;
    }
    llvm_unreachable("didn't find associated type in protocol?");
  }

  static AssociatedTypeDecl *
  getAssociatedTypeByIndex(const ProtocolConformance *conformance,
                           unsigned index) {
    for (auto requirement : conformance->getProtocol()->getMembers()) {
      if (auto associate = dyn_cast<AssociatedTypeDecl>(requirement)) {
        if (index == 0) return associate;
        index--;
      }
    }
    llvm_unreachable("didn't find associated type in protocol?");
  }

  // We store associated conformances using their index in the requirement
  // list of the requirement signature of the protocol.
  static unsigned getAssociatedConformanceIndex(const ProtocolDecl *proto,
                                                CanType associatedType,
                                                ProtocolDecl *requirement) {
    unsigned index = 0;
    for (const auto &reqt : proto->getRequirementSignature()) {
      if (reqt.getKind() == RequirementKind::Conformance &&
          reqt.getFirstType()->getCanonicalType() == associatedType &&
          reqt.getSecondType()->castTo<ProtocolType>()->getDecl() ==
                                                                requirement) {
        return index;
      }
      ++index;
    }
    llvm_unreachable("requirement not found in protocol");
  }

  // We store associated conformances using their index in the requirement
  // list of the requirement signature of the conformance's protocol.
  static unsigned getAssociatedConformanceIndex(
                                      const ProtocolConformance *conformance,
                                      CanType associatedType,
                                      ProtocolDecl *requirement) {
    return getAssociatedConformanceIndex(conformance->getProtocol(),
                                         associatedType, requirement);
  }

  static std::pair<CanType, ProtocolDecl*>
  getAssociatedConformanceByIndex(const ProtocolDecl *proto,
                                  unsigned index) {
    auto &reqt = proto->getRequirementSignature()[index];
    assert(reqt.getKind() == RequirementKind::Conformance);
    return { reqt.getFirstType()->getCanonicalType(),
             reqt.getSecondType()->castTo<ProtocolType>()->getDecl() };
  }

  static std::pair<CanType, ProtocolDecl*>
  getAssociatedConformanceByIndex(const ProtocolConformance *conformance,
                                  unsigned index) {
    return getAssociatedConformanceByIndex(conformance->getProtocol(), index);
  }

  void setForType(Kind kind, CanType type) {
    assert(isTypeKind(kind));
    Pointer = type.getPointer();
    SecondaryPointer = nullptr;
    Data = LINKENTITY_SET_FIELD(Kind, unsigned(kind));
  }

  LinkEntity() = default;

  static bool isValidResilientMethodRef(SILDeclRef declRef) {
    if (declRef.isForeign ||
        declRef.isDirectReference ||
        declRef.isCurried)
      return false;

    auto *decl = declRef.getDecl();
    return (isa<ClassDecl>(decl->getDeclContext()) ||
            isa<ProtocolDecl>(decl->getDeclContext()));
  }

public:
  static LinkEntity forDispatchThunk(SILDeclRef declRef) {
    assert(isValidResilientMethodRef(declRef));

    LinkEntity::Kind kind;
    switch (declRef.kind) {
    case SILDeclRef::Kind::Func:
      kind = Kind::DispatchThunk;
      break;
    case SILDeclRef::Kind::Initializer:
      kind = Kind::DispatchThunkInitializer;
      break;
    case SILDeclRef::Kind::Allocator:
      kind = Kind::DispatchThunkAllocator;
      break;
    default:
      llvm_unreachable("Bad SILDeclRef for dispatch thunk");
    }

    LinkEntity entity;
    entity.setForDecl(kind, declRef.getDecl());
    return entity;
  }

  static LinkEntity forMethodDescriptor(SILDeclRef declRef) {
    assert(isValidResilientMethodRef(declRef));

    LinkEntity::Kind kind;
    switch (declRef.kind) {
    case SILDeclRef::Kind::Func:
      kind = Kind::MethodDescriptor;
      break;
    case SILDeclRef::Kind::Initializer:
      kind = Kind::MethodDescriptorInitializer;
      break;
    case SILDeclRef::Kind::Allocator:
      kind = Kind::MethodDescriptorAllocator;
      break;
    default:
      llvm_unreachable("Bad SILDeclRef for method descriptor");
    }

    LinkEntity entity;
    entity.setForDecl(kind, declRef.getDecl());
    return entity;
  }

  static LinkEntity forMethodLookupFunction(ClassDecl *classDecl) {
    LinkEntity entity;
    entity.setForDecl(Kind::MethodLookupFunction, classDecl);
    return entity;
  }

  static LinkEntity forFieldOffset(VarDecl *decl) {
    LinkEntity entity;
    entity.setForDecl(Kind::FieldOffset, decl);
    return entity;
  }

  static LinkEntity forEnumCase(EnumElementDecl *decl) {
    LinkEntity entity;
    entity.setForDecl(Kind::EnumCase, decl);
    return entity;
  }

  static LinkEntity forObjCClassRef(ClassDecl *decl) {
    LinkEntity entity;
    entity.setForDecl(Kind::ObjCClassRef, decl);
    return entity;
  }

  static LinkEntity forObjCClass(ClassDecl *decl) {
    LinkEntity entity;
    entity.setForDecl(Kind::ObjCClass, decl);
    return entity;
  }

  static LinkEntity forObjCMetaclass(ClassDecl *decl) {
    LinkEntity entity;
    entity.setForDecl(Kind::ObjCMetaclass, decl);
    return entity;
  }

  static LinkEntity forSwiftMetaclassStub(ClassDecl *decl) {
    LinkEntity entity;
    entity.setForDecl(Kind::SwiftMetaclassStub, decl);
    return entity;
  }

  static LinkEntity forTypeMetadata(CanType concreteType,
                                    TypeMetadataAddress addr) {
    LinkEntity entity;
    entity.Pointer = concreteType.getPointer();
    entity.SecondaryPointer = nullptr;
    entity.Data = LINKENTITY_SET_FIELD(Kind, unsigned(Kind::TypeMetadata))
                | LINKENTITY_SET_FIELD(MetadataAddress, unsigned(addr));
    return entity;
  }

  static LinkEntity forTypeMetadataPattern(NominalTypeDecl *decl) {
    LinkEntity entity;
    entity.setForDecl(Kind::TypeMetadataPattern, decl);
    return entity;
  }

  static LinkEntity forTypeMetadataAccessFunction(CanType type) {
    LinkEntity entity;
    entity.setForType(Kind::TypeMetadataAccessFunction, type);
    return entity;
  }

  static LinkEntity forTypeMetadataInstantiationCache(NominalTypeDecl *decl) {
    LinkEntity entity;
    entity.setForDecl(Kind::TypeMetadataInstantiationCache, decl);
    return entity;
  }

  static LinkEntity forTypeMetadataInstantiationFunction(NominalTypeDecl *decl) {
    LinkEntity entity;
    entity.setForDecl(Kind::TypeMetadataInstantiationFunction, decl);
    return entity;
  }

  static LinkEntity forTypeMetadataSingletonInitializationCache(
                                                      NominalTypeDecl *decl) {
    LinkEntity entity;
    entity.setForDecl(Kind::TypeMetadataSingletonInitializationCache, decl);
    return entity;
  }

  static LinkEntity forTypeMetadataCompletionFunction(NominalTypeDecl *decl) {
    LinkEntity entity;
    entity.setForDecl(Kind::TypeMetadataCompletionFunction, decl);
    return entity;
  }

  static LinkEntity forTypeMetadataLazyCacheVariable(CanType type) {
    LinkEntity entity;
    entity.setForType(Kind::TypeMetadataLazyCacheVariable, type);
    return entity;
  }

  static LinkEntity forForeignTypeMetadataCandidate(CanType type) {
    LinkEntity entity;
    entity.setForType(Kind::ForeignTypeMetadataCandidate, type);
    return entity;
  }

  static LinkEntity forClassMetadataBaseOffset(ClassDecl *decl) {
    LinkEntity entity;
    entity.setForDecl(Kind::ClassMetadataBaseOffset, decl);
    return entity;
  }

  static LinkEntity forNominalTypeDescriptor(NominalTypeDecl *decl) {
    LinkEntity entity;
    entity.setForDecl(Kind::NominalTypeDescriptor, decl);
    return entity;
  }

  static LinkEntity forPropertyDescriptor(AbstractStorageDecl *decl) {
    LinkEntity entity;
    entity.setForDecl(Kind::PropertyDescriptor, decl);
    return entity;
  }

  static LinkEntity forModuleDescriptor(ModuleDecl *decl) {
    LinkEntity entity;
    entity.setForDecl(Kind::ModuleDescriptor, decl);
    return entity;
  }

  static LinkEntity forExtensionDescriptor(ExtensionDecl *decl) {
    LinkEntity entity;
    entity.Pointer = const_cast<void*>(static_cast<const void*>(decl));
    entity.SecondaryPointer = nullptr;
    entity.Data =
      LINKENTITY_SET_FIELD(Kind, unsigned(Kind::ExtensionDescriptor));
    return entity;
  }

  static LinkEntity forAnonymousDescriptor(DeclContext *dc) {
    LinkEntity entity;
    entity.Pointer = const_cast<void*>(static_cast<const void*>(dc));
    entity.SecondaryPointer = nullptr;
    entity.Data =
      LINKENTITY_SET_FIELD(Kind, unsigned(Kind::AnonymousDescriptor));
    return entity;
  }

  static LinkEntity forProtocolDescriptor(ProtocolDecl *decl) {
    LinkEntity entity;
    entity.setForDecl(Kind::ProtocolDescriptor, decl);
    return entity;
  }

  static LinkEntity forProtocolRequirementsBaseDescriptor(ProtocolDecl *decl) {
    LinkEntity entity;
    entity.setForDecl(Kind::ProtocolRequirementsBaseDescriptor, decl);
    return entity;
  }

  static LinkEntity forValueWitness(CanType concreteType, ValueWitness witness) {
    LinkEntity entity;
    entity.Pointer = concreteType.getPointer();
    entity.Data = LINKENTITY_SET_FIELD(Kind, unsigned(Kind::ValueWitness))
                | LINKENTITY_SET_FIELD(ValueWitness, unsigned(witness));
    return entity;
  }

  static LinkEntity forValueWitnessTable(CanType type) {
    LinkEntity entity;
    entity.setForType(Kind::ValueWitnessTable, type);
    return entity;
  }

  static LinkEntity forSILFunction(SILFunction *F)
  {
    LinkEntity entity;
    entity.Pointer = F;
    entity.SecondaryPointer = nullptr;
    entity.Data = LINKENTITY_SET_FIELD(Kind, unsigned(Kind::SILFunction));
    return entity;
  }

  static LinkEntity forSILGlobalVariable(SILGlobalVariable *G) {
    LinkEntity entity;
    entity.Pointer = G;
    entity.SecondaryPointer = nullptr;
    entity.Data = LINKENTITY_SET_FIELD(Kind, unsigned(Kind::SILGlobalVariable));
    return entity;
  }

  static LinkEntity
  forDirectProtocolWitnessTable(const ProtocolConformance *C) {
    LinkEntity entity;
    entity.setForProtocolConformance(Kind::DirectProtocolWitnessTable, C);
    return entity;
  }

  static LinkEntity
  forProtocolWitnessTablePattern(const ProtocolConformance *C) {
    LinkEntity entity;
    entity.setForProtocolConformance(Kind::ProtocolWitnessTablePattern, C);
    return entity;
  }

  static LinkEntity
  forProtocolWitnessTableAccessFunction(const ProtocolConformance *C) {
    LinkEntity entity;
    entity.setForProtocolConformance(Kind::ProtocolWitnessTableAccessFunction,
                                     C);
    return entity;
  }

  static LinkEntity
  forGenericProtocolWitnessTableInstantiationFunction(
                                      const ProtocolConformance *C) {
    LinkEntity entity;
    entity.setForProtocolConformance(
                     Kind::GenericProtocolWitnessTableInstantiationFunction, C);
    return entity;
  }

  static LinkEntity
  forProtocolWitnessTableLazyAccessFunction(const ProtocolConformance *C,
                                            CanType type) {
    LinkEntity entity;
    entity.setForProtocolConformanceAndType(
             Kind::ProtocolWitnessTableLazyAccessFunction, C, type);
    return entity;
  }

  static LinkEntity
  forProtocolWitnessTableLazyCacheVariable(const ProtocolConformance *C,
                                           CanType type) {
    LinkEntity entity;
    entity.setForProtocolConformanceAndType(
             Kind::ProtocolWitnessTableLazyCacheVariable, C, type);
    return entity;
  }

  static LinkEntity
  forAssociatedTypeDescriptor(AssociatedTypeDecl *assocType) {
    LinkEntity entity;
    entity.setForDecl(Kind::AssociatedTypeDescriptor, assocType);
    return entity;
  }

  static LinkEntity
  forAssociatedConformanceDescriptor(AssociatedConformance conformance) {
    LinkEntity entity;
    entity.setForProtocolAndAssociatedConformance(
        Kind::AssociatedConformanceDescriptor,
        conformance.getSourceProtocol(),
        conformance.getAssociation(),
        conformance.getAssociatedRequirement());
    return entity;
  }

  static LinkEntity
  forAssociatedTypeWitnessTableAccessFunction(const ProtocolConformance *C,
                                     const AssociatedConformance &association) {
    LinkEntity entity;
    entity.setForProtocolConformanceAndAssociatedConformance(
                     Kind::AssociatedTypeWitnessTableAccessFunction, C,
                     association.getAssociation(),
                     association.getAssociatedRequirement());
    return entity;
  }

  static LinkEntity
  forDefaultAssociatedConformanceAccessor(AssociatedConformance conformance) {
    LinkEntity entity;
    entity.setForProtocolAndAssociatedConformance(
        Kind::DefaultAssociatedConformanceAccessor,
        conformance.getSourceProtocol(),
        conformance.getAssociation(),
        conformance.getAssociatedRequirement());
    return entity;
  }

  static LinkEntity forReflectionBuiltinDescriptor(CanType type) {
    LinkEntity entity;
    entity.setForType(Kind::ReflectionBuiltinDescriptor, type);
    return entity;
  }

  static LinkEntity forReflectionFieldDescriptor(CanType type) {
    LinkEntity entity;
    entity.setForType(Kind::ReflectionFieldDescriptor, type);
    return entity;
  }

  static LinkEntity
  forReflectionAssociatedTypeDescriptor(const ProtocolConformance *C) {
    LinkEntity entity;
    entity.setForProtocolConformance(
        Kind::ReflectionAssociatedTypeDescriptor, C);
    return entity;
  }

  static LinkEntity
  forProtocolConformanceDescriptor(const NormalProtocolConformance *C) {
    LinkEntity entity;
    entity.setForProtocolConformance(
        Kind::ProtocolConformanceDescriptor, C);
    return entity;
  }

  static LinkEntity forCoroutineContinuationPrototype(CanSILFunctionType type) {
    LinkEntity entity;
    entity.setForType(Kind::CoroutineContinuationPrototype, type);
    return entity;
  }

  void mangle(llvm::raw_ostream &out) const;
  void mangle(SmallVectorImpl<char> &buffer) const;
  std::string mangleAsString() const;
  SILLinkage getLinkage(ForDefinition_t isDefinition) const;

  /// Returns true if this function or global variable is potentially defined
  /// in a different module.
  ///
  bool isAvailableExternally(IRGenModule &IGM) const;

  const ValueDecl *getDecl() const {
    assert(isDeclKind(getKind()));
    return reinterpret_cast<ValueDecl*>(Pointer);
  }
  
  const ExtensionDecl *getExtension() const {
    assert(getKind() == Kind::ExtensionDescriptor);
    return reinterpret_cast<ExtensionDecl*>(Pointer);
  }

  const DeclContext *getDeclContext() const {
    assert(getKind() == Kind::AnonymousDescriptor);
    return reinterpret_cast<DeclContext*>(Pointer);
  }

  SILFunction *getSILFunction() const {
    assert(getKind() == Kind::SILFunction);
    return reinterpret_cast<SILFunction*>(Pointer);
  }

  SILGlobalVariable *getSILGlobalVariable() const {
    assert(getKind() == Kind::SILGlobalVariable);
    return reinterpret_cast<SILGlobalVariable*>(Pointer);
  }
  
  const ProtocolConformance *getProtocolConformance() const {
    assert(isProtocolConformanceKind(getKind()));
    return reinterpret_cast<ProtocolConformance*>(SecondaryPointer);
  }

  AssociatedTypeDecl *getAssociatedType() const {
    assert(getKind() == Kind::AssociatedTypeDescriptor);
    return reinterpret_cast<AssociatedTypeDecl *>(Pointer);
  }

  std::pair<CanType, ProtocolDecl *> getAssociatedConformance() const {
    if (getKind() == Kind::AssociatedTypeWitnessTableAccessFunction) {
      return getAssociatedConformanceByIndex(getProtocolConformance(),
                       LINKENTITY_GET_FIELD(Data, AssociatedConformanceIndex));
    }

    assert(getKind() == Kind::AssociatedConformanceDescriptor ||
           getKind() == Kind::DefaultAssociatedConformanceAccessor);
    return getAssociatedConformanceByIndex(
             cast<ProtocolDecl>(getDecl()),
             LINKENTITY_GET_FIELD(Data, AssociatedConformanceIndex));
  }

  ProtocolDecl *getAssociatedProtocol() const {
    assert(getKind() == Kind::AssociatedTypeWitnessTableAccessFunction);
    return reinterpret_cast<ProtocolDecl*>(Pointer);
  }

  bool isValueWitness() const { return getKind() == Kind::ValueWitness; }
  CanType getType() const {
    assert(isTypeKind(getKind()));
    return CanType(reinterpret_cast<TypeBase*>(Pointer));
  }
  ValueWitness getValueWitness() const {
    assert(getKind() == Kind::ValueWitness);
    return ValueWitness(LINKENTITY_GET_FIELD(Data, ValueWitness));
  }
  TypeMetadataAddress getMetadataAddress() const {
    assert(getKind() == Kind::TypeMetadata);
    return (TypeMetadataAddress)LINKENTITY_GET_FIELD(Data, MetadataAddress);
  }
  bool isForeignTypeMetadataCandidate() const {
    return getKind() == Kind::ForeignTypeMetadataCandidate;
  }
  bool isObjCClassRef() const {
    return getKind() == Kind::ObjCClassRef;
  }

  /// Determine whether this entity will be weak-imported.
  bool isWeakImported(ModuleDecl *module) const {
    if (getKind() == Kind::SILGlobalVariable &&
        getSILGlobalVariable()->getDecl())
      return getSILGlobalVariable()->getDecl()->isWeakImported(module);

    if (getKind() == Kind::SILFunction) {
      if (auto clangOwner = getSILFunction()->getClangNodeOwner())
        return clangOwner->isWeakImported(module);
      if (getSILFunction()->isWeakLinked())
        return getSILFunction()->isAvailableExternally();
    }

    if (!isDeclKind(getKind()))
      return false;

    return getDecl()->isWeakImported(module);
  }
  
  /// Return the source file whose codegen should trigger emission of this
  /// link entity, if one can be identified.
  const SourceFile *getSourceFileForEmission() const;
  
  /// Get the preferred alignment for the definition of this entity.
  Alignment getAlignment(IRGenModule &IGM) const;
  
  /// Get the default LLVM type to use for forward declarations of this
  /// entity.
  llvm::Type *getDefaultDeclarationType(IRGenModule &IGM) const;
#undef LINKENTITY_GET_FIELD
#undef LINKENTITY_SET_FIELD
};

/// Encapsulated information about the linkage of an entity.
class LinkInfo {
  LinkInfo() = default;

  llvm::SmallString<32> Name;
  llvm::GlobalValue::LinkageTypes Linkage;
  llvm::GlobalValue::VisibilityTypes Visibility;
  llvm::GlobalValue::DLLStorageClassTypes DLLStorageClass;
  ForDefinition_t ForDefinition;

public:
  /// Compute linkage information for the given
  static LinkInfo get(IRGenModule &IGM, const LinkEntity &entity,
                      ForDefinition_t forDefinition);

  static LinkInfo get(const UniversalLinkageInfo &linkInfo,
                      ModuleDecl *swiftModule, const LinkEntity &entity,
                      ForDefinition_t forDefinition);

  static LinkInfo get(const UniversalLinkageInfo &linkInfo, StringRef name,
                      SILLinkage linkage, ForDefinition_t isDefinition,
                      bool isWeakImported);

  StringRef getName() const {
    return Name.str();
  }
  llvm::GlobalValue::LinkageTypes getLinkage() const {
    return Linkage;
  }
  llvm::GlobalValue::VisibilityTypes getVisibility() const {
    return Visibility;
  }
  llvm::GlobalValue::DLLStorageClassTypes getDLLStorage() const {
    return DLLStorageClass;
  }

  bool isForDefinition() const {
    return ForDefinition;
  }
  bool isUsed() const {
    return ForDefinition && isUsed(Linkage, Visibility, DLLStorageClass);
  }

  static bool isUsed(llvm::GlobalValue::LinkageTypes Linkage,
                     llvm::GlobalValue::VisibilityTypes Visibility,
                     llvm::GlobalValue::DLLStorageClassTypes DLLStorage);
};

StringRef encodeForceLoadSymbolName(llvm::SmallVectorImpl<char> &buf,
                                    StringRef name);
}
}

/// Allow LinkEntity to be used as a key for a DenseMap.
namespace llvm {
template <> struct DenseMapInfo<swift::irgen::LinkEntity> {
  using LinkEntity = swift::irgen::LinkEntity;
  static LinkEntity getEmptyKey() {
    LinkEntity entity;
    entity.Pointer = nullptr;
    entity.SecondaryPointer = nullptr;
    entity.Data = 0;
    return entity;
  }
  static LinkEntity getTombstoneKey() {
    LinkEntity entity;
    entity.Pointer = nullptr;
    entity.SecondaryPointer = nullptr;
    entity.Data = 1;
    return entity;
  }
  static unsigned getHashValue(const LinkEntity &entity) {
    return DenseMapInfo<void *>::getHashValue(entity.Pointer) ^
           DenseMapInfo<void *>::getHashValue(entity.SecondaryPointer) ^
           entity.Data;
  }
  static bool isEqual(const LinkEntity &LHS, const LinkEntity &RHS) {
    return LHS.Pointer == RHS.Pointer &&
           LHS.SecondaryPointer == RHS.SecondaryPointer && LHS.Data == RHS.Data;
  }
};
}
#endif
