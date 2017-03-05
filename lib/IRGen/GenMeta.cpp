//===--- GenMeta.cpp - IR generation for metadata constructs --------------===//
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
//  This file implements IR generation for type metadata constructs.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/ASTContext.h"
#include "swift/AST/CanTypeVisitor.h"
#include "swift/AST/Decl.h"
#include "swift/AST/IRGenOptions.h"
#include "swift/AST/Mangle.h"
#include "swift/AST/SubstitutionMap.h"
#include "swift/AST/Types.h"
#include "swift/SIL/FormalLinkage.h"
#include "swift/SIL/SILModule.h"
#include "swift/SIL/TypeLowering.h"
#include "swift/Runtime/Metadata.h"
#include "swift/ABI/MetadataValues.h"
#include "llvm/IR/InlineAsm.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/GlobalVariable.h"
#include "llvm/IR/Module.h"
#include "llvm/ADT/SmallString.h"

#include "Address.h"
#include "Callee.h"
#include "ClassMetadataLayout.h"
#include "ConstantBuilder.h"
#include "FixedTypeInfo.h"
#include "GenClass.h"
#include "GenPoly.h"
#include "GenValueWitness.h"
#include "GenArchetype.h"
#include "GenStruct.h"
#include "HeapTypeInfo.h"
#include "IRGenModule.h"
#include "IRGenDebugInfo.h"
#include "Linking.h"
#include "ScalarTypeInfo.h"
#include "StructMetadataLayout.h"
#include "StructLayout.h"
#include "EnumMetadataLayout.h"
#include "IRGenMangler.h"

#include "GenMeta.h"

using namespace swift;
using namespace irgen;

static llvm::Value *emitLoadOfObjCHeapMetadataRef(IRGenFunction &IGF,
                                                  llvm::Value *object);

static Address emitAddressOfMetadataSlotAtIndex(IRGenFunction &IGF,
                                                llvm::Value *metadata,
                                                int index,
                                                llvm::Type *objectTy) {
  // Require the metadata to be some type that we recognize as a
  // metadata pointer.
  assert(metadata->getType() == IGF.IGM.TypeMetadataPtrTy);

  // We require objectType to be a pointer type so that the GEP will
  // scale by the right amount.  We could load an arbitrary type using
  // some extra bitcasting.
  assert(IGF.IGM.DataLayout.getTypeStoreSize(objectTy) ==
         IGF.IGM.DataLayout.getTypeStoreSize(IGF.IGM.SizeTy));

  // Cast to T*.
  auto objectPtrTy = objectTy->getPointerTo();
  auto metadataWords = IGF.Builder.CreateBitCast(metadata, objectPtrTy);

  auto indexV = llvm::ConstantInt::getSigned(IGF.IGM.SizeTy, index);

  // GEP to the slot.
  Address slot(IGF.Builder.CreateInBoundsGEP(metadataWords, indexV),
               IGF.IGM.getPointerAlignment());

  return slot;
}

/// Emit a load from the given metadata at a constant index.
static llvm::LoadInst *emitLoadFromMetadataAtIndex(IRGenFunction &IGF,
                                                   llvm::Value *metadata,
                                                   int index,
                                                   llvm::Type *objectTy,
                                             const llvm::Twine &suffix = "") {
  Address slot =
    emitAddressOfMetadataSlotAtIndex(IGF, metadata, index, objectTy);

  // Load.
  return IGF.Builder.CreateLoad(slot, metadata->getName() + suffix);
}

static int getClassParentIndex(IRGenModule &IGM, ClassDecl *classDecl);

/// Produce a constant to place in a metatype's isa field
/// corresponding to the given metadata kind.
static llvm::ConstantInt *getMetadataKind(IRGenModule &IGM,
                                          MetadataKind kind) {
  return llvm::ConstantInt::get(IGM.MetadataKindTy, uint8_t(kind));
}

static Address createPointerSizedGEP(IRGenFunction &IGF,
                                     Address base,
                                     Size offset) {
  return IGF.Builder.CreateConstArrayGEP(base,
                                         IGF.IGM.getOffsetInWords(offset),
                                         offset);
}

// FIXME: willBeRelativelyAddressed is only needed to work around an ld64 bug
// resolving relative references to coalesceable symbols.
// It should be removed when fixed. rdar://problem/22674524
static llvm::Constant *getMangledTypeName(IRGenModule &IGM, CanType type,
                                      bool willBeRelativelyAddressed = false) {
  IRGenMangler Mangler;
  std::string Name = Mangler.mangleTypeForMetadata(type);
  return IGM.getAddrOfGlobalString(Name, willBeRelativelyAddressed);
}

llvm::Value *irgen::emitObjCMetadataRefForMetadata(IRGenFunction &IGF,
                                                   llvm::Value *classPtr) {
  classPtr = IGF.Builder.CreateBitCast(classPtr, IGF.IGM.ObjCClassPtrTy);
  
  // Fetch the metadata for that class.
  auto call = IGF.Builder.CreateCall(IGF.IGM.getGetObjCClassMetadataFn(),
                                     classPtr);
  call->setDoesNotThrow();
  call->setDoesNotAccessMemory();
  return call;
}

/// Emit a reference to the Swift metadata for an Objective-C class.
static llvm::Value *emitObjCMetadataRef(IRGenFunction &IGF,
                                        ClassDecl *theClass) {
  // Derive a pointer to the Objective-C class.
  auto classPtr = emitObjCHeapMetadataRef(IGF, theClass);
  
  return emitObjCMetadataRefForMetadata(IGF, classPtr);
}

namespace {
  /// A structure for collecting generic arguments for emitting a
  /// nominal metadata reference.  The structure produced here is
  /// consumed by swift_getGenericMetadata() and must correspond to
  /// the fill operations that the compiler emits for the bound decl.
  ///
  /// FIXME: Rework to use GenericSignature instead of AllArchetypes
  struct GenericArguments {
    /// The values to use to initialize the arguments structure.
    SmallVector<llvm::Value *, 8> Values;
    SmallVector<llvm::Type *, 8> Types;

    static unsigned getNumGenericArguments(IRGenModule &IGM,
                                           NominalTypeDecl *nominal) {
      GenericTypeRequirements requirements(IGM, nominal);
      return unsigned(requirements.hasParentType())
               + requirements.getNumTypeRequirements();
    }

    void collectTypes(IRGenModule &IGM, NominalTypeDecl *nominal) {
      GenericTypeRequirements requirements(IGM, nominal);
      collectTypes(IGM, requirements);
    }

    void collectTypes(IRGenModule &IGM,
                      const GenericTypeRequirements &requirements) {
      if (requirements.hasParentType()) {
        Types.push_back(IGM.TypeMetadataPtrTy);
      }

      for (auto &requirement : requirements.getRequirements()) {
        if (requirement.Protocol) {
          Types.push_back(IGM.WitnessTablePtrTy);
        } else {
          Types.push_back(IGM.TypeMetadataPtrTy);
        }
      }
    }

    void collect(IRGenFunction &IGF, CanType type) {
      NominalTypeDecl *decl;
      CanType parentType;

      if (auto nominalType = dyn_cast<NominalType>(type)) {
        decl = nominalType->getDecl();
        parentType = nominalType.getParent();
      } else {
        auto boundType = cast<BoundGenericType>(type);
        decl = boundType->getDecl();
        parentType = boundType.getParent();
      }

      GenericTypeRequirements requirements(IGF.IGM, decl);

      if (requirements.hasParentType()) {
        Values.push_back(IGF.emitTypeMetadataRef(parentType));
      }

      auto subs =
        type->getContextSubstitutionMap(IGF.IGM.getSwiftModule(), decl);
      requirements.enumerateFulfillments(IGF.IGM, subs,
                                [&](unsigned reqtIndex, CanType type,
                                    Optional<ProtocolConformanceRef> conf) {
        if (conf) {
          Values.push_back(emitWitnessTableRef(IGF, type, *conf));
        } else {
          Values.push_back(IGF.emitTypeMetadataRef(type));
        }
      });

      collectTypes(IGF.IGM, decl);
      assert(Types.size() == Values.size());
    }
  };
} // end anonymous namespace

/// Given an array of polymorphic arguments as might be set up by
/// GenericArguments, bind the polymorphic parameters.
static void emitPolymorphicParametersFromArray(IRGenFunction &IGF,
                                               NominalTypeDecl *typeDecl,
                                               Address array) {
  GenericTypeRequirements requirements(IGF.IGM, typeDecl);

  array = IGF.Builder.CreateElementBitCast(array, IGF.IGM.TypeMetadataPtrTy);

  auto getInContext = [&](CanType type) -> CanType {
    return typeDecl->mapTypeIntoContext(type)
             ->getCanonicalType();
  };

  // If we have a parent type, it's the first parameter.
  if (requirements.hasParentType()) {
    auto parentType = getInContext(requirements.getParentType());
    llvm::Value *parentMetadata = IGF.Builder.CreateLoad(array);
    array = IGF.Builder.CreateConstArrayGEP(array, 1, IGF.IGM.getPointerSize());
    IGF.bindLocalTypeDataFromTypeMetadata(parentType, IsExact, parentMetadata);
  }

  // Okay, bind everything else from the context.
  requirements.bindFromBuffer(IGF, array, getInContext);
}

static bool isTypeErasedGenericClass(NominalTypeDecl *ntd) {
  // ObjC classes are type erased.
  // TODO: Unless they have magic methods...
  if (auto clas = dyn_cast<ClassDecl>(ntd))
    return clas->hasClangNode() && clas->isGenericContext();
  return false;
}

static bool isTypeErasedGenericClassType(CanType type) {
  if (auto nom = type->getAnyNominal())
    return isTypeErasedGenericClass(nom);
  return false;
}

// Get the type that exists at runtime to represent a compile-time type.
CanType
irgen::getRuntimeReifiedType(IRGenModule &IGM, CanType type) {
  return CanType(type.transform([&](Type t) -> Type {
    if (isTypeErasedGenericClassType(CanType(t))) {
      return t->getAnyNominal()->getDeclaredType()->getCanonicalType();
    }
    return t;
  }));
}

/// Attempts to return a constant heap metadata reference for a
/// class type.  This is generally only valid for specific kinds of
/// ObjC reference, like superclasses or category references.
llvm::Constant *irgen::tryEmitConstantHeapMetadataRef(IRGenModule &IGM,
                                                      CanType type,
                                              bool allowDynamicUninitialized) {
  auto theDecl = type->getClassOrBoundGenericClass();
  assert(theDecl && "emitting constant heap metadata ref for non-class type?");

  // If the class must not require dynamic initialization --- e.g. if it
  // is a super reference --- then respect everything that might impose that.
  if (!allowDynamicUninitialized) {
    if (doesClassMetadataRequireDynamicInitialization(IGM, theDecl))
      return nullptr;

  // Otherwise, just respect genericity.
  } else if (theDecl->isGenericContext() && !isTypeErasedGenericClass(theDecl)){
    return nullptr;
  }

  // For imported classes, use the ObjC class symbol.
  // This incidentally cannot coincide with most of the awkward cases, like
  // having parent metadata.
  if (!hasKnownSwiftMetadata(IGM, theDecl))
    return IGM.getAddrOfObjCClass(theDecl, NotForDefinition);

  return IGM.getAddrOfTypeMetadata(type, false);
}

/// Attempts to return a constant type metadata reference for a
/// nominal type.
ConstantReference
irgen::tryEmitConstantTypeMetadataRef(IRGenModule &IGM, CanType type,
                                      SymbolReferenceKind refKind) {
  if (!isTypeMetadataAccessTrivial(IGM, type))
    return ConstantReference();

  return IGM.getAddrOfTypeMetadata(type, false, refKind);
}

/// Emit a reference to an ObjC class.  In general, the only things
/// you're allowed to do with the address of an ObjC class symbol are
/// (1) send ObjC messages to it (in which case the message will be
/// forwarded to the real class, if one exists) or (2) put it in
/// various data sections where the ObjC runtime will properly arrange
/// things.  Therefore, we must typically force the initialization of
/// a class when emitting a reference to it.
llvm::Value *irgen::emitObjCHeapMetadataRef(IRGenFunction &IGF,
                                            ClassDecl *theClass,
                                            bool allowUninitialized) {
  // If the class is visible only through the Objective-C runtime, form the
  // appropriate runtime call.
  if (theClass->getForeignClassKind() == ClassDecl::ForeignKind::RuntimeOnly) {
    SmallString<64> scratch;
    auto className =
        IGF.IGM.getAddrOfGlobalString(theClass->getObjCRuntimeName(scratch));
    return IGF.Builder.CreateCall(IGF.IGM.getLookUpClassFn(), className);
  }

  assert(!theClass->isForeign());

  Address classRef = IGF.IGM.getAddrOfObjCClassRef(theClass);
  auto classObject = IGF.Builder.CreateLoad(classRef);
  if (allowUninitialized) return classObject;

  // TODO: memoize this the same way that we memoize Swift type metadata?
  return IGF.Builder.CreateCall(IGF.IGM.getGetInitializedObjCClassFn(),
                                classObject);
}

/// Emit a reference to the type metadata for a foreign type.
static llvm::Value *emitForeignTypeMetadataRef(IRGenFunction &IGF,
                                               CanType type) {
  llvm::Value *candidate = IGF.IGM.getAddrOfForeignTypeMetadataCandidate(type);
  auto call = IGF.Builder.CreateCall(IGF.IGM.getGetForeignTypeMetadataFn(),
                                candidate);
  call->addAttribute(llvm::AttributeSet::FunctionIndex,
                     llvm::Attribute::NoUnwind);
  call->addAttribute(llvm::AttributeSet::FunctionIndex,
                     llvm::Attribute::ReadNone);
  return call;
}

/// Returns a metadata reference for a nominal type.
///
/// This is only valid in a couple of special cases:
/// 1) The nominal type is generic, in which case we emit a call to the
///    generic metadata accessor function, which must be defined separately.
/// 2) The nominal type is a value type with a fixed size from this
///    resilience domain, in which case we can reference the constant
///    metadata directly.
///
/// In any other case, a metadata accessor should be called instead.
static llvm::Value *emitNominalMetadataRef(IRGenFunction &IGF,
                                           NominalTypeDecl *theDecl,
                                           CanType theType) {
  assert(!isa<ProtocolDecl>(theDecl));

  if (!theDecl->isGenericContext()) {
    assert(!IGF.IGM.isResilient(theDecl, ResilienceExpansion::Maximal));
    // TODO: If Obj-C interop is off, we can relax this to allow referencing
    // class metadata too.
    assert(isa<StructDecl>(theDecl) || isa<EnumDecl>(theDecl));
    return IGF.IGM.getAddrOfTypeMetadata(theType, false);
  }

  // We are applying generic parameters to a generic type.
  assert(theType->isSpecialized() &&
         theType->getAnyNominal() == theDecl);

  // Check to see if we've maybe got a local reference already.
  if (auto cache = IGF.tryGetLocalTypeData(theType,
                                           LocalTypeDataKind::forTypeMetadata()))
    return cache;

  // Grab the substitutions.
  GenericArguments genericArgs;
  genericArgs.collect(IGF, theType);
  assert(genericArgs.Values.size() > 0 && "no generic args?!");

  // Call the generic metadata accessor function.
  llvm::Function *accessor =
      IGF.IGM.getAddrOfGenericTypeMetadataAccessFunction(theDecl,
                                                         genericArgs.Types,
                                                         NotForDefinition);

  auto result = IGF.Builder.CreateCall(accessor, genericArgs.Values);
  result->setDoesNotThrow();
  result->addAttribute(llvm::AttributeSet::FunctionIndex,
                       llvm::Attribute::ReadNone);

  IGF.setScopedLocalTypeData(theType, LocalTypeDataKind::forTypeMetadata(),
                             result);
  return result;
}


bool irgen::hasKnownSwiftMetadata(IRGenModule &IGM, CanType type) {
  if (ClassDecl *theClass = type.getClassOrBoundGenericClass()) {
    return hasKnownSwiftMetadata(IGM, theClass);
  }

  if (auto archetype = dyn_cast<ArchetypeType>(type)) {
    if (auto superclass = archetype->getSuperclass()) {
      return hasKnownSwiftMetadata(IGM, superclass->getCanonicalType());
    }
  }

  // Class existentials, etc.
  return false;
}

/// Is the given class known to have Swift-compatible metadata?
bool irgen::hasKnownSwiftMetadata(IRGenModule &IGM, ClassDecl *theClass) {
  // For now, the fact that a declaration was not implemented in Swift
  // is enough to conclusively force us into a slower path.
  // Eventually we might have an attribute here or something based on
  // the deployment target.
  return theClass->hasKnownSwiftImplementation();
}

/// Is the given method known to be callable by vtable lookup?
bool irgen::hasKnownVTableEntry(IRGenModule &IGM,
                                AbstractFunctionDecl *theMethod) {
  auto theClass = dyn_cast<ClassDecl>(theMethod->getDeclContext());
  // Extension methods don't get vtable entries.
  if (!theClass) {
    return false;
  }
  return theClass->hasKnownSwiftImplementation();
}

/// Is it basically trivial to access the given metadata?  If so, we don't
/// need a cache variable in its accessor.
bool irgen::isTypeMetadataAccessTrivial(IRGenModule &IGM, CanType type) {
  assert(!type->hasArchetype());

  // Value type metadata only requires dynamic initialization on first
  // access if it contains a resilient type.
  if (isa<StructType>(type) || isa<EnumType>(type)) {
    auto nominalType = cast<NominalType>(type);

    // Imported type metadata always requires an accessor.
    if (nominalType->getDecl()->hasClangNode())
      return false;

    // Metadata with a non-trivial parent node always requires an accessor.
    if (auto parent = nominalType.getParent())
      if (!isTypeMetadataAccessTrivial(IGM, parent))
        return false;

    // Resiliently-sized metadata access always requires an accessor.
    return (IGM.getTypeInfoForUnlowered(type).isFixedSize());
  }

  // The empty tuple type has a singleton metadata.
  if (auto tuple = dyn_cast<TupleType>(type))
    return tuple->getNumElements() == 0;

  // The builtin types generally don't require metadata, but some of them
  // have nodes in the runtime anyway.
  if (isa<BuiltinType>(type))
    return true;

  // SIL box types are artificial, but for the purposes of dynamic layout,
  // we use the NativeObject metadata.
  if (isa<SILBoxType>(type))
    return true;

  // DynamicSelfType is actually local.
  if (type->hasDynamicSelfType())
    return true;

  return false;
}

static bool hasRequiredTypeMetadataAccessFunction(IRGenModule &IGM,
                                                  NominalTypeDecl *typeDecl) {
  // This needs to be kept in sync with getTypeMetadataStrategy.

  if (isa<ProtocolDecl>(typeDecl))
    return false;

  switch (getDeclLinkage(typeDecl)) {
  case FormalLinkage::PublicUnique:
  case FormalLinkage::HiddenUnique:
  case FormalLinkage::Private:
    return true;

  case FormalLinkage::PublicNonUnique:
  case FormalLinkage::HiddenNonUnique:
    return false;
  }
  llvm_unreachable("bad formal linkage");

}

/// Return the standard access strategy for getting a non-dependent
/// type metadata object.
MetadataAccessStrategy
irgen::getTypeMetadataAccessStrategy(IRGenModule &IGM, CanType type) {
  // We should not be emitting accessors for partially-substituted
  // generic types.
  assert(!type->hasArchetype());

  // Non-generic structs, enums, and classes are special cases.
  //
  // Note that while protocol types don't have a metadata pattern,
  // we still require an accessor since we actually want to get
  // the metadata for the existential type.
  //
  // This needs to kept in sync with hasRequiredTypeMetadataAccessPattern.
  auto nominal = type->getAnyNominal();
  if (nominal && !isa<ProtocolDecl>(nominal)) {
    // Metadata accessors for fully-substituted generic types are
    // emitted with shared linkage.
    if (nominal->isGenericContext() && !nominal->isObjC()) {
      if (type->isSpecialized())
        return MetadataAccessStrategy::NonUniqueAccessor;
      assert(type->hasUnboundGenericType());
    }

    // If the type doesn't guarantee that it has an access function,
    // we might have to use a non-unique accessor.

    // Everything else requires accessors.
    switch (getDeclLinkage(nominal)) {
    case FormalLinkage::PublicUnique:
      return MetadataAccessStrategy::PublicUniqueAccessor;
    case FormalLinkage::HiddenUnique:
      return MetadataAccessStrategy::HiddenUniqueAccessor;
    case FormalLinkage::Private:
      return MetadataAccessStrategy::PrivateAccessor;

    case FormalLinkage::PublicNonUnique:
    case FormalLinkage::HiddenNonUnique:
      return MetadataAccessStrategy::NonUniqueAccessor;
    }
    llvm_unreachable("bad formal linkage");
  }

  // Everything else requires a shared accessor function.
  return MetadataAccessStrategy::NonUniqueAccessor;
}

/// Emit a string encoding the labels in the given tuple type.
static llvm::Constant *getTupleLabelsString(IRGenModule &IGM,
                                            CanTupleType type) {
  bool hasLabels = false;
  llvm::SmallString<128> buffer;
  for (auto &elt : type->getElements()) {
    if (elt.hasName()) {
      hasLabels = true;
      buffer.append(elt.getName().str());
    }

    // Each label is space-terminated.
    buffer += ' ';
  }

  // If there are no labels, use a null pointer.
  if (!hasLabels) {
    return llvm::ConstantPointerNull::get(IGM.Int8PtrTy);
  }

  // Otherwise, create a new string literal.
  // This method implicitly adds a null terminator.
  return IGM.getAddrOfGlobalString(buffer);
}

namespace {
  /// A visitor class for emitting a reference to a metatype object.
  /// This implements a "raw" access, useful for implementing cache
  /// functions or for implementing dependent accesses.
  ///
  /// If the access requires runtime initialization, that initialization
  /// must be dependency-ordered-before any load that carries a dependency
  /// from the resulting metadata pointer.
  class EmitTypeMetadataRef
    : public CanTypeVisitor<EmitTypeMetadataRef, llvm::Value *> {
  private:
    IRGenFunction &IGF;
  public:
    EmitTypeMetadataRef(IRGenFunction &IGF) : IGF(IGF) {}

#define TREAT_AS_OPAQUE(KIND)                          \
    llvm::Value *visit##KIND##Type(KIND##Type *type) { \
      return visitOpaqueType(CanType(type));           \
    }
    TREAT_AS_OPAQUE(BuiltinInteger)
    TREAT_AS_OPAQUE(BuiltinFloat)
    TREAT_AS_OPAQUE(BuiltinVector)
    TREAT_AS_OPAQUE(BuiltinRawPointer)
#undef TREAT_AS_OPAQUE

    llvm::Value *emitDirectMetadataRef(CanType type) {
      return IGF.IGM.getAddrOfTypeMetadata(type,
                                           /*pattern*/ false);
    }

    /// The given type should use opaque type info.  We assume that
    /// the runtime always provides an entry for such a type;  right
    /// now, that mapping is as one of the power-of-two integer types.
    llvm::Value *visitOpaqueType(CanType type) {
      auto &opaqueTI = cast<FixedTypeInfo>(IGF.IGM.getTypeInfoForLowered(type));
      unsigned numBits = opaqueTI.getFixedSize().getValueInBits();
      if (!llvm::isPowerOf2_32(numBits))
        numBits = llvm::NextPowerOf2(numBits);
      auto intTy = BuiltinIntegerType::get(numBits, IGF.IGM.Context);
      return emitDirectMetadataRef(CanType(intTy));
    }

    llvm::Value *visitBuiltinNativeObjectType(CanBuiltinNativeObjectType type) {
      return emitDirectMetadataRef(type);
    }

    llvm::Value *visitBuiltinBridgeObjectType(CanBuiltinBridgeObjectType type) {
      return emitDirectMetadataRef(type);
    }

    llvm::Value *visitBuiltinUnknownObjectType(CanBuiltinUnknownObjectType type) {
      return emitDirectMetadataRef(type);
    }

    llvm::Value *visitBuiltinUnsafeValueBufferType(
                                        CanBuiltinUnsafeValueBufferType type) {
      return emitDirectMetadataRef(type);
    }

    llvm::Value *visitNominalType(CanNominalType type) {
      assert(!type->isExistentialType());
      return emitNominalMetadataRef(IGF, type->getDecl(), type);
    }

    llvm::Value *visitBoundGenericType(CanBoundGenericType type) {
      assert(!type->isExistentialType());
      return emitNominalMetadataRef(IGF, type->getDecl(), type);
    }

    llvm::Value *visitTupleType(CanTupleType type) {
      if (auto cached = tryGetLocal(type))
        return cached;

      // I think the sanest thing to do here is drop labels, but maybe
      // that's not correct.  If so, that's really unfortunate in a
      // lot of ways.

      // Er, varargs bit?  Should that go in?


      switch (type->getNumElements()) {
      case 0: {// Special case the empty tuple, just use the global descriptor.
        llvm::Constant *fullMetadata = IGF.IGM.getEmptyTupleMetadata();
        llvm::Constant *indices[] = {
          llvm::ConstantInt::get(IGF.IGM.Int32Ty, 0),
          llvm::ConstantInt::get(IGF.IGM.Int32Ty, 1)
        };
        return llvm::ConstantExpr::getInBoundsGetElementPtr(
            /*Ty=*/nullptr, fullMetadata, indices);
      }

      case 1:
          // For metadata purposes, we consider a singleton tuple to be
          // isomorphic to its element type.
        return IGF.emitTypeMetadataRef(type.getElementType(0));

      case 2: {
        // Find the metadata pointer for this element.
        auto elt0Metadata = IGF.emitTypeMetadataRef(type.getElementType(0));
        auto elt1Metadata = IGF.emitTypeMetadataRef(type.getElementType(1));

        llvm::Value *args[] = {
          elt0Metadata, elt1Metadata,
          getTupleLabelsString(IGF.IGM, type),
          llvm::ConstantPointerNull::get(IGF.IGM.WitnessTablePtrTy) // proposed
        };

        auto call = IGF.Builder.CreateCall(IGF.IGM.getGetTupleMetadata2Fn(),
                                           args);
        call->setDoesNotThrow();
        return setLocal(CanType(type), call);
      }

      case 3: {
        // Find the metadata pointer for this element.
        auto elt0Metadata = IGF.emitTypeMetadataRef(type.getElementType(0));
        auto elt1Metadata = IGF.emitTypeMetadataRef(type.getElementType(1));
        auto elt2Metadata = IGF.emitTypeMetadataRef(type.getElementType(2));

        llvm::Value *args[] = {
          elt0Metadata, elt1Metadata, elt2Metadata,
          getTupleLabelsString(IGF.IGM, type),
          llvm::ConstantPointerNull::get(IGF.IGM.WitnessTablePtrTy) // proposed
        };

        auto call = IGF.Builder.CreateCall(IGF.IGM.getGetTupleMetadata3Fn(),
                                           args);
        call->setDoesNotThrow();
        return setLocal(CanType(type), call);
      }
      default:
        // TODO: use a caching entrypoint (with all information
        // out-of-line) for non-dependent tuples.

        llvm::Value *pointerToFirst = nullptr; // appease -Wuninitialized

        auto elements = type.getElementTypes();
        auto arrayTy = llvm::ArrayType::get(IGF.IGM.TypeMetadataPtrTy,
                                            elements.size());
        Address buffer = IGF.createAlloca(arrayTy,IGF.IGM.getPointerAlignment(),
                                          "tuple-elements");
        IGF.Builder.CreateLifetimeStart(buffer,
                                    IGF.IGM.getPointerSize() * elements.size());
        for (unsigned i = 0, e = elements.size(); i != e; ++i) {
          // Find the metadata pointer for this element.
          llvm::Value *eltMetadata = IGF.emitTypeMetadataRef(elements[i]);

          // GEP to the appropriate element and store.
          Address eltPtr = IGF.Builder.CreateStructGEP(buffer, i,
                                                     IGF.IGM.getPointerSize());
          IGF.Builder.CreateStore(eltMetadata, eltPtr);

          // Remember the GEP to the first element.
          if (i == 0) pointerToFirst = eltPtr.getAddress();
        }

        llvm::Value *args[] = {
          llvm::ConstantInt::get(IGF.IGM.SizeTy, elements.size()),
          pointerToFirst,
          getTupleLabelsString(IGF.IGM, type),
          llvm::ConstantPointerNull::get(IGF.IGM.WitnessTablePtrTy) // proposed
        };

        auto call = IGF.Builder.CreateCall(IGF.IGM.getGetTupleMetadataFn(),
                                           args);
        call->setDoesNotThrow();

        IGF.Builder.CreateLifetimeEnd(buffer,
                                    IGF.IGM.getPointerSize() * elements.size());

        return setLocal(type, call);
      }
    }

    llvm::Value *visitGenericFunctionType(CanGenericFunctionType type) {
      IGF.unimplemented(SourceLoc(),
                        "metadata ref for generic function type");
      return llvm::UndefValue::get(IGF.IGM.TypeMetadataPtrTy);
    }
      
    llvm::Value *extractAndMarkResultType(CanFunctionType type) {
      // If the function type throws, set the lower bit of the return type
      // address, so that we can carry this information over to the function
      // type metadata.
      auto metadata = IGF.emitTypeMetadataRef(type->getResult()->
                                              getCanonicalType());
      return metadata;
    }

    llvm::Value *extractAndMarkInOut(CanType type) {
      // If the type is inout, get the metadata for its inner object type
      // instead, and then set the lowest bit to help the runtime unique
      // the metadata type for this function.
      if (auto inoutType = dyn_cast<InOutType>(type)) {
        auto metadata = IGF.emitTypeMetadataRef(inoutType.getObjectType());
        auto metadataInt = IGF.Builder.CreatePtrToInt(metadata, IGF.IGM.SizeTy);
        auto inoutFlag = llvm::ConstantInt::get(IGF.IGM.SizeTy, 1);
        auto marked = IGF.Builder.CreateOr(metadataInt, inoutFlag);
        return IGF.Builder.CreateIntToPtr(marked, IGF.IGM.Int8PtrTy);
      }
      
      auto metadata = IGF.emitTypeMetadataRef(type);
      return IGF.Builder.CreateBitCast(metadata, IGF.IGM.Int8PtrTy);
    }

    llvm::Value *visitFunctionType(CanFunctionType type) {
      if (auto metatype = tryGetLocal(type))
        return metatype;

      auto resultMetadata = extractAndMarkResultType(type);
      
      CanTupleType inputTuple = dyn_cast<TupleType>(type.getInput());

      size_t numArguments = 1;

      if (inputTuple && !inputTuple->isMaterializable())
        numArguments = inputTuple->getNumElements();

      // Map the convention to a runtime metadata value.
      FunctionMetadataConvention metadataConvention;
      switch (type->getRepresentation()) {
      case FunctionTypeRepresentation::Swift:
        metadataConvention = FunctionMetadataConvention::Swift;
        break;
      case FunctionTypeRepresentation::Thin:
        metadataConvention = FunctionMetadataConvention::Thin;
        break;
      case FunctionTypeRepresentation::Block:
        metadataConvention = FunctionMetadataConvention::Block;
        break;
      case FunctionTypeRepresentation::CFunctionPointer:
        metadataConvention = FunctionMetadataConvention::CFunctionPointer;
        break;
      }
      
      auto flagsVal = FunctionTypeFlags()
        .withNumArguments(numArguments)
        .withConvention(metadataConvention)
        .withThrows(type->throws());
      
      auto flags = llvm::ConstantInt::get(IGF.IGM.SizeTy,
                                          flagsVal.getIntValue());

      switch (numArguments) {
        case 1: {
          auto arg0 = (inputTuple && !inputTuple->isMaterializable()) ?
            extractAndMarkInOut(inputTuple.getElementType(0))
          : extractAndMarkInOut(type.getInput());

          auto call = IGF.Builder.CreateCall(
                                            IGF.IGM.getGetFunctionMetadata1Fn(),
                                            {flags, arg0, resultMetadata});
          call->setDoesNotThrow();
          return setLocal(CanType(type), call);
        }

        case 2: {
          auto arg0 = extractAndMarkInOut(inputTuple.getElementType(0));
          auto arg1 = extractAndMarkInOut(inputTuple.getElementType(1));
          auto call = IGF.Builder.CreateCall(
                                            IGF.IGM.getGetFunctionMetadata2Fn(),
                                            {flags, arg0, arg1, resultMetadata});
          call->setDoesNotThrow();
          return setLocal(CanType(type), call);
        }

        case 3: {
          auto arg0 = extractAndMarkInOut(inputTuple.getElementType(0));
          auto arg1 = extractAndMarkInOut(inputTuple.getElementType(1));
          auto arg2 = extractAndMarkInOut(inputTuple.getElementType(2));
          auto call = IGF.Builder.CreateCall(
                                            IGF.IGM.getGetFunctionMetadata3Fn(),
                                            {flags, arg0, arg1, arg2,
                                             resultMetadata});
          call->setDoesNotThrow();
          return setLocal(CanType(type), call);
        }

        default:
          auto arguments = inputTuple.getElementTypes();
          auto arrayTy = llvm::ArrayType::get(IGF.IGM.Int8PtrTy,
                                              arguments.size() + 2);
          Address buffer = IGF.createAlloca(arrayTy,
                                            IGF.IGM.getPointerAlignment(),
                                            "function-arguments");
          IGF.Builder.CreateLifetimeStart(buffer,
                                  IGF.IGM.getPointerSize() * arguments.size());
          Address pointerToFirstArg = IGF.Builder.CreateStructGEP(buffer, 0,
                                                                   Size(0));
          Address flagsPtr = IGF.Builder.CreateBitCast(pointerToFirstArg,
                                               IGF.IGM.SizeTy->getPointerTo());
          IGF.Builder.CreateStore(flags, flagsPtr);
          
          for (size_t i = 0; i < arguments.size(); ++i) {
            auto argMetadata = extractAndMarkInOut(
                                                  inputTuple.getElementType(i));
            Address argPtr = IGF.Builder.CreateStructGEP(buffer, i + 1,
                                                      IGF.IGM.getPointerSize());
            IGF.Builder.CreateStore(argMetadata, argPtr);

          }
          Address resultPtr = IGF.Builder.CreateStructGEP(buffer,
                                                    arguments.size() + 1,
                                                    IGF.IGM.getPointerSize());
          resultPtr = IGF.Builder.CreateBitCast(resultPtr,
                                     IGF.IGM.TypeMetadataPtrTy->getPointerTo());
          IGF.Builder.CreateStore(resultMetadata, resultPtr);

          auto call = IGF.Builder.CreateCall(IGF.IGM.getGetFunctionMetadataFn(),
                                             pointerToFirstArg.getAddress());
          call->setDoesNotThrow();
          
          IGF.Builder.CreateLifetimeEnd(buffer,
                                   IGF.IGM.getPointerSize() * arguments.size());

          return setLocal(type, call);
      }
    }

    llvm::Value *visitAnyMetatypeType(CanAnyMetatypeType type) {
      // FIXME: We shouldn't accept a lowered metatype here, but we need to
      // represent Optional<@objc_metatype T.Type> as an AST type for ABI
      // reasons.
      
      // assert(!type->hasRepresentation()
      //       && "should not be asking for a representation-specific metatype "
      //          "metadata");
      
      if (auto metatype = tryGetLocal(type))
        return metatype;

      auto instMetadata = IGF.emitTypeMetadataRef(type.getInstanceType());
      auto fn = isa<MetatypeType>(type)
                  ? IGF.IGM.getGetMetatypeMetadataFn()
                  : IGF.IGM.getGetExistentialMetatypeMetadataFn();
      auto call = IGF.Builder.CreateCall(fn, instMetadata);
      call->setDoesNotThrow();

      return setLocal(type, call);
    }

    llvm::Value *visitModuleType(CanModuleType type) {
      IGF.unimplemented(SourceLoc(), "metadata ref for module type");
      return llvm::UndefValue::get(IGF.IGM.TypeMetadataPtrTy);
    }

    llvm::Value *visitDynamicSelfType(CanDynamicSelfType type) {
      return IGF.getLocalSelfMetadata();
    }
      
    llvm::Value *emitExistentialTypeMetadata(CanType type) {
      SmallVector<ProtocolDecl*, 2> protocols;
      type.getAnyExistentialTypeProtocols(protocols);
      
      // Collect references to the protocol descriptors.
      auto descriptorArrayTy
        = llvm::ArrayType::get(IGF.IGM.ProtocolDescriptorPtrTy,
                               protocols.size());
      Address descriptorArray = IGF.createAlloca(descriptorArrayTy,
                                                 IGF.IGM.getPointerAlignment(),
                                                 "protocols");
      IGF.Builder.CreateLifetimeStart(descriptorArray,
                                   IGF.IGM.getPointerSize() * protocols.size());
      descriptorArray = IGF.Builder.CreateBitCast(descriptorArray,
                               IGF.IGM.ProtocolDescriptorPtrTy->getPointerTo());
      
      unsigned index = 0;
      for (auto *p : protocols) {
        llvm::Value *ref = emitProtocolDescriptorRef(IGF, p);
        Address slot = IGF.Builder.CreateConstArrayGEP(descriptorArray,
                                               index, IGF.IGM.getPointerSize());
        IGF.Builder.CreateStore(ref, slot);
        ++index;
      }
      
      auto call = IGF.Builder.CreateCall(IGF.IGM.getGetExistentialMetadataFn(),
                                         {IGF.IGM.getSize(Size(protocols.size())),
                                          descriptorArray.getAddress()});
      call->setDoesNotThrow();
      IGF.Builder.CreateLifetimeEnd(descriptorArray,
                                   IGF.IGM.getPointerSize() * protocols.size());
      return setLocal(type, call);
    }

    llvm::Value *visitProtocolType(CanProtocolType type) {
      return emitExistentialTypeMetadata(type);
    }
      
    llvm::Value *visitProtocolCompositionType(CanProtocolCompositionType type) {
      return emitExistentialTypeMetadata(type);
    }

    llvm::Value *visitReferenceStorageType(CanReferenceStorageType type) {
      llvm_unreachable("reference storage type should have been converted by "
                       "SILGen");
    }
    llvm::Value *visitSILFunctionType(CanSILFunctionType type) {
      llvm_unreachable("should not be asking for metadata of a lowered SIL "
                       "function type--SILGen should have used the AST type");
    }

    llvm::Value *visitArchetypeType(CanArchetypeType type) {
      return emitArchetypeTypeMetadataRef(IGF, type);
    }

    llvm::Value *visitGenericTypeParamType(CanGenericTypeParamType type) {
      llvm_unreachable("dependent type should have been substituted by Sema or SILGen");
    }

    llvm::Value *visitDependentMemberType(CanDependentMemberType type) {
      llvm_unreachable("dependent type should have been substituted by Sema or SILGen");
    }

    llvm::Value *visitLValueType(CanLValueType type) {
      llvm_unreachable("lvalue type should have been lowered by SILGen");
    }
    llvm::Value *visitInOutType(CanInOutType type) {
      llvm_unreachable("inout type should have been lowered by SILGen");
    }
    llvm::Value *visitErrorType(CanErrorType type) {
      llvm_unreachable("error type should not appear in IRGen");
    }

    llvm::Value *visitSILBlockStorageType(CanSILBlockStorageType type) {
      llvm_unreachable("cannot ask for metadata of block storage");
    }

    llvm::Value *visitSILBoxType(CanSILBoxType type) {
      // The Builtin.NativeObject metadata can stand in for boxes.
      return emitDirectMetadataRef(type->getASTContext().TheNativeObjectType);
    }

    /// Try to find the metatype in local data.
    llvm::Value *tryGetLocal(CanType type) {
      return IGF.tryGetLocalTypeData(type, LocalTypeDataKind::forTypeMetadata());
    }

    /// Set the metatype in local data.
    llvm::Value *setLocal(CanType type, llvm::Instruction *metatype) {
      IGF.setScopedLocalTypeData(type, LocalTypeDataKind::forTypeMetadata(),
                                 metatype);
      return metatype;
    }
  };
} // end anonymous namespace

/// Emit a type metadata reference without using an accessor function.
static llvm::Value *emitDirectTypeMetadataRef(IRGenFunction &IGF,
                                              CanType type) {
  return EmitTypeMetadataRef(IGF).visit(type);
}

static Address emitAddressOfSuperclassRefInClassMetadata(IRGenFunction &IGF,
                                                  llvm::Value *metadata) {
  // The superclass field in a class type is the first field past the isa.
  unsigned index = 1;

  Address addr(metadata, IGF.IGM.getPointerAlignment());
  addr = IGF.Builder.CreateBitCast(addr,
                                   IGF.IGM.TypeMetadataPtrTy->getPointerTo());
  return IGF.Builder.CreateConstArrayGEP(addr, index, IGF.IGM.getPointerSize());
}

static bool isLoadFrom(llvm::Value *value, Address address) {
  if (auto load = dyn_cast<llvm::LoadInst>(value)) {
    return load->getOperand(0) == address.getAddress();
  }
  return false;
}

/// Emit the body of a lazy cache accessor.
///
/// If cacheVariable is null, we perform the direct access every time.
/// This is used for metadata accessors that come about due to resilience,
/// where the direct access is completely trivial.
void irgen::emitLazyCacheAccessFunction(IRGenModule &IGM,
                                        llvm::Function *accessor,
                                        llvm::GlobalVariable *cacheVariable,
         const llvm::function_ref<llvm::Value*(IRGenFunction &IGF)> &getValue) {
  accessor->setDoesNotThrow();

  // This function is logically 'readnone': the caller does not need
  // to reason about any side effects or stores it might perform.
  accessor->setDoesNotAccessMemory();

  IRGenFunction IGF(IGM, accessor);
  if (IGM.DebugInfo)
    IGM.DebugInfo->emitArtificialFunction(IGF, accessor);

  // If there's no cache variable, just perform the direct access.
  if (cacheVariable == nullptr) {
    IGF.Builder.CreateRet(getValue(IGF));
    return;
  }

  // Set up the cache variable.
  llvm::Constant *null =
    llvm::ConstantPointerNull::get(
                        cast<llvm::PointerType>(cacheVariable->getValueType()));

  cacheVariable->setInitializer(null);
  cacheVariable->setAlignment(IGM.getPointerAlignment().getValue());
  Address cache(cacheVariable, IGM.getPointerAlignment());

  // Okay, first thing, check the cache variable.
  //
  // Conceptually, this needs to establish memory ordering with the
  // store we do later in the function: if the metadata value is
  // non-null, we must be able to see any stores performed by the
  // initialization of the metadata.  However, any attempt to read
  // from the metadata will be address-dependent on the loaded
  // metadata pointer, which is sufficient to provide adequate
  // memory ordering guarantees on all the platforms we care about:
  // ARM has special rules about address dependencies, and x86's
  // memory ordering is strong enough to guarantee the visibility
  // even without the address dependency.
  //
  // And we do not need to worry about the compiler because the
  // address dependency naturally forces an order to the memory
  // accesses.
  //
  // Therefore, we can perform a completely naked load here.
  // FIXME: Technically should be "consume", but that introduces barriers in the
  // current LLVM ARM backend.
  auto load = IGF.Builder.CreateLoad(cache);
  // Make this barrier explicit when building for TSan to avoid false positives.
  if (IGM.IRGen.Opts.Sanitize == SanitizerKind::Thread)
    load->setOrdering(llvm::AtomicOrdering::Acquire);


  // Compare the load result against null.
  auto isNullBB = IGF.createBasicBlock("cacheIsNull");
  auto contBB = IGF.createBasicBlock("cont");
  llvm::Value *comparison = IGF.Builder.CreateICmpEQ(load, null);
  IGF.Builder.CreateCondBr(comparison, isNullBB, contBB);
  auto loadBB = IGF.Builder.GetInsertBlock();

  // If the load yielded null, emit the type metadata.
  IGF.Builder.emitBlock(isNullBB);
  llvm::Value *directResult = getValue(IGF);

  // Store it back to the cache variable.  This needs to be a store-release
  // because it needs to propagate memory visibility to the other threads
  // that can access the cache: the initializing stores might be visible
  // to this thread, but they aren't transitively guaranteed to be visible
  // to other threads unless this is a store-release.
  //
  // However, we can skip this if the value was actually loaded from the
  // cache.  This is a simple, if hacky, peephole that's useful for the
  // code in emitInPlaceTypeMetadataAccessFunctionBody.
  if (!isLoadFrom(directResult, cache)) {
    IGF.Builder.CreateStore(directResult, cache)
      ->setAtomic(llvm::AtomicOrdering::Release);
  }

  IGF.Builder.CreateBr(contBB);
  auto storeBB = IGF.Builder.GetInsertBlock();

  // Emit the continuation block.
  IGF.Builder.emitBlock(contBB);
  auto phi = IGF.Builder.CreatePHI(null->getType(), 2);
  phi->addIncoming(load, loadBB);
  phi->addIncoming(directResult, storeBB);

  IGF.Builder.CreateRet(phi);
}

static llvm::Value *emitGenericMetadataAccessFunction(IRGenFunction &IGF,
                                                      NominalTypeDecl *nominal,
                                                      GenericArguments &genericArgs) {
  CanType declaredType = nominal->getDeclaredType()->getCanonicalType();
  llvm::Value *metadata = IGF.IGM.getAddrOfTypeMetadata(declaredType, true);

  // Collect input arguments to the generic metadata accessor, as laid out
  // by the GenericArguments class.
  for (auto &arg : IGF.CurFn->args())
    genericArgs.Values.push_back(&arg);
  assert(genericArgs.Values.size() == genericArgs.Types.size());
  assert(genericArgs.Values.size() > 0 && "no generic args?!");

  // Slam that information directly into the generic arguments buffer.
  auto argsBufferTy =
    llvm::StructType::get(IGF.IGM.LLVMContext, genericArgs.Types);
  Address argsBuffer = IGF.createAlloca(argsBufferTy,
                                        IGF.IGM.getPointerAlignment(),
                                        "generic.arguments");
  IGF.Builder.CreateLifetimeStart(argsBuffer,
                          IGF.IGM.getPointerSize() * genericArgs.Values.size());
  for (unsigned i = 0, e = genericArgs.Values.size(); i != e; ++i) {
    Address elt = IGF.Builder.CreateStructGEP(argsBuffer, i,
                                              IGF.IGM.getPointerSize() * i);
    IGF.Builder.CreateStore(genericArgs.Values[i], elt);
  }

  // Cast to void*.
  llvm::Value *arguments =
    IGF.Builder.CreateBitCast(argsBuffer.getAddress(), IGF.IGM.Int8PtrTy);

  // Make the call.
  auto result = IGF.Builder.CreateCall(IGF.IGM.getGetGenericMetadataFn(),
                                       {metadata, arguments});
  result->setDoesNotThrow();
  result->addAttribute(llvm::AttributeSet::FunctionIndex,
                       llvm::Attribute::ReadOnly);

  IGF.Builder.CreateLifetimeEnd(argsBuffer,
                          IGF.IGM.getPointerSize() * genericArgs.Values.size());

  return result;

}

using InPlaceMetadataInitializer =
  llvm::function_ref<llvm::Value*(IRGenFunction &IGF, llvm::Value *metadata)>;

/// Emit a helper function for swift_once that performs in-place
/// initialization of the given nominal type.
static llvm::Constant *
createInPlaceMetadataInitializationFunction(IRGenModule &IGM, 
                                            CanNominalType type,
                                            llvm::Constant *metadata,
                                            llvm::Constant *cacheVariable,
                                     InPlaceMetadataInitializer &&initialize) {
  // There's an ignored i8* parameter.
  auto fnTy = llvm::FunctionType::get(IGM.VoidTy, {IGM.Int8PtrTy},
                                      /*variadic*/ false);
  llvm::Function *fn = llvm::Function::Create(fnTy,
                                       llvm::GlobalValue::PrivateLinkage,
                                       Twine("initialize_metadata_")
                                           + type->getDecl()->getName().str(),
                                       &IGM.Module);
  fn->setAttributes(IGM.constructInitialAttributes());
  
  // Set up the function.
  IRGenFunction IGF(IGM, fn);
  if (IGM.DebugInfo)
    IGM.DebugInfo->emitArtificialFunction(IGF, fn);

  // Skip instrumentation when building for TSan to avoid false positives.
  // The synchronization for this happens in the Runtime and we do not see it.
  if (IGM.IRGen.Opts.Sanitize == SanitizerKind::Thread)
    fn->removeFnAttr(llvm::Attribute::SanitizeThread);

  // Emit the initialization.
  llvm::Value *relocatedMetadata = initialize(IGF, metadata);

  // Store back to the cache variable.
  IGF.Builder.CreateStore(relocatedMetadata,
                          Address(cacheVariable, IGM.getPointerAlignment()))
    ->setAtomic(llvm::AtomicOrdering::Release);

  IGF.Builder.CreateRetVoid();
  return fn;
}

/// Emit the function body for the type metadata accessor of a nominal type
/// that might require in-place initialization.
static llvm::Value *
emitInPlaceTypeMetadataAccessFunctionBody(IRGenFunction &IGF,
                                          CanNominalType type,
                                          llvm::Constant *cacheVariable,
                                    InPlaceMetadataInitializer &&initializer) {
  llvm::Constant *metadata = IGF.IGM.getAddrOfTypeMetadata(type, false);

  // We might not have interesting initialization to do.
  assert((cacheVariable == nullptr) ==
            isTypeMetadataAccessTrivial(IGF.IGM, type));
  if (!cacheVariable)
    return metadata;

  // Okay, we have non-trivial initialization to do.
  // Ensure that we don't have multiple threads racing to do this.
  llvm::GlobalVariable *onceGuard =
    new llvm::GlobalVariable(IGF.IGM.Module, IGF.IGM.OnceTy, /*constant*/ false,
                             llvm::GlobalValue::PrivateLinkage,
                             llvm::Constant::getNullValue(IGF.IGM.OnceTy),
                             Twine(IGF.CurFn->getName()) + ".once_token");

  // There's no point in performing the fast-path token check here
  // because we've already checked the cache variable.  We're just using
  // swift_once to guarantee thread safety.
  assert(cacheVariable && "lazy initialization but no cache variable");

  // Create the protected function.  swift_once wants this as an i8*.
  llvm::Value *onceFn =
    createInPlaceMetadataInitializationFunction(IGF.IGM, type, metadata,
                                                cacheVariable,
                                                std::move(initializer));
  onceFn = IGF.Builder.CreateBitCast(onceFn, IGF.IGM.Int8PtrTy);

  auto onceCall = IGF.Builder.CreateCall(IGF.IGM.getOnceFn(),
                                         {onceGuard, onceFn});
  onceCall->setCallingConv(IGF.IGM.DefaultCC);

  // We can just load the cache now.
  // TODO: this should be consume-ordered when LLVM supports it.
  Address cacheAddr = Address(cacheVariable, IGF.IGM.getPointerAlignment());
  llvm::LoadInst *relocatedMetadata = IGF.Builder.CreateLoad(cacheAddr);
  // Make this barrier explicit when building for TSan to avoid false positives.
  if (IGF.IGM.IRGen.Opts.Sanitize == SanitizerKind::Thread)
    relocatedMetadata->setOrdering(llvm::AtomicOrdering::Acquire);

  // emitLazyCacheAccessFunction will see that the value was loaded from
  // the guard variable and skip the redundant store back.
  return relocatedMetadata;
}

/// Emit the body of a metadata accessor function for the given type.
///
/// This function is appropriate for ordinary situations where the
/// construction of the metadata value just involves calling idempotent
/// metadata-construction functions.  It is not used for the in-place
/// initialization of non-generic nominal type metadata.
static llvm::Value *emitTypeMetadataAccessFunctionBody(IRGenFunction &IGF,
                                                       CanType type) {
  assert(!type->hasArchetype() &&
         "cannot emit metadata accessor for context-dependent type");

  // We only take this path for That means
  // everything except non-generic nominal types.
  auto typeDecl = type->getAnyNominal();
  if (!typeDecl)
    return emitDirectTypeMetadataRef(IGF, type);

  if (typeDecl->isGenericContext() &&
      !(isa<ClassDecl>(typeDecl) && typeDecl->hasClangNode())) {
    // This is a metadata accessor for a fully substituted generic type.
    return emitDirectTypeMetadataRef(IGF, type);
  }

  // We should never be emitting a metadata accessor for resilient nominal
  // types outside of their defining module.  We'd only do that anyway for
  // types that don't guarantee the existence of a non-unique access
  // function, and that should never be true of a resilient type with
  // external availability.
  //
  // (The type might still not have a statically-known layout.  It just
  // can't be resilient at the top level: we have to know its immediate
  // members, or we can't even begin to approach the problem of emitting
  // metadata for it.)
  assert(!IGF.IGM.isResilient(typeDecl, ResilienceExpansion::Maximal));

  // Non-native types are just wrapped in various ways.
  if (auto classDecl = dyn_cast<ClassDecl>(typeDecl)) {
    // We emit a completely different pattern for foreign classes.
    if (classDecl->getForeignClassKind() == ClassDecl::ForeignKind::CFType) {
      return emitForeignTypeMetadataRef(IGF, type);
    }

    // Classes that might not have Swift metadata use a different
    // symbol name.
    if (!hasKnownSwiftMetadata(IGF.IGM, classDecl)) {
      return emitObjCMetadataRef(IGF, classDecl);
    }

  // Imported value types require foreign metadata uniquing.
  } else if (typeDecl->hasClangNode()) {
    return emitForeignTypeMetadataRef(IGF, type);
  }

  // Okay, everything else is built from a Swift metadata object.
  llvm::Constant *metadata = IGF.IGM.getAddrOfTypeMetadata(type, false);

  // We should not be doing more serious work along this path.
  assert(isTypeMetadataAccessTrivial(IGF.IGM, type));

  return metadata;
}

using MetadataAccessGenerator =
  llvm::function_ref<llvm::Value*(IRGenFunction &IGF, llvm::Constant *cache)>;

/// Get or create an accessor function to the given non-dependent type.
static llvm::Function *getTypeMetadataAccessFunction(IRGenModule &IGM,
                                        CanType type,
                                        ForDefinition_t shouldDefine,
                                        MetadataAccessGenerator &&generator) {
  assert(!type->hasArchetype());
  // Type should be bound unless it's type erased.
  assert(isTypeErasedGenericClassType(type)
           ? !isa<BoundGenericType>(type)
           : !isa<UnboundGenericType>(type));

  llvm::Function *accessor =
    IGM.getAddrOfTypeMetadataAccessFunction(type, shouldDefine);

  // If we're not supposed to define the accessor, or if we already
  // have defined it, just return the pointer.
  if (!shouldDefine || !accessor->empty())
    return accessor;

  // Okay, define the accessor.
  llvm::GlobalVariable *cacheVariable = nullptr;

  // If our preferred access method is to go via an accessor, it means
  // there is some non-trivial computation that needs to be cached.
  if (!isTypeMetadataAccessTrivial(IGM, type)) {
    cacheVariable = cast<llvm::GlobalVariable>(
        IGM.getAddrOfTypeMetadataLazyCacheVariable(type, ForDefinition));
  }

  emitLazyCacheAccessFunction(IGM, accessor, cacheVariable,
                              [&](IRGenFunction &IGF) -> llvm::Value* {
    return generator(IGF, cacheVariable);
  });

  return accessor;
}

/// Get or create an accessor function to the given non-dependent type.
static llvm::Function *getTypeMetadataAccessFunction(IRGenModule &IGM,
                                                 CanType type,
                                                 ForDefinition_t shouldDefine) {
  return getTypeMetadataAccessFunction(IGM, type, shouldDefine,
                                       [&](IRGenFunction &IGF,
                                           llvm::Constant *cacheVariable) {
    // We should not be called with ForDefinition for nominal types
    // that require in-place initialization.
    return emitTypeMetadataAccessFunctionBody(IGF, type);
  });
}

/// Get or create an accessor function to the given generic type.
static llvm::Function *getGenericTypeMetadataAccessFunction(IRGenModule &IGM,
                                                 NominalTypeDecl *nominal,
                                                 ForDefinition_t shouldDefine) {
  assert(nominal->isGenericContext());
  assert(!isTypeErasedGenericClass(nominal));

  GenericArguments genericArgs;
  genericArgs.collectTypes(IGM, nominal);

  llvm::Function *accessor =
    IGM.getAddrOfGenericTypeMetadataAccessFunction(
        nominal, genericArgs.Types, shouldDefine);

  // If we're not supposed to define the accessor, or if we already
  // have defined it, just return the pointer.
  if (!shouldDefine || !accessor->empty())
    return accessor;

  emitLazyCacheAccessFunction(IGM, accessor, /*cacheVariable=*/nullptr,
                              [&](IRGenFunction &IGF) -> llvm::Value* {
    return emitGenericMetadataAccessFunction(IGF, nominal, genericArgs);
  });

  return accessor;
}

/// Return the type metadata access function for the given type, if it
/// is guaranteed to exist.
static llvm::Constant *
getRequiredTypeMetadataAccessFunction(IRGenModule &IGM,
                                      NominalTypeDecl *theDecl,
                                      ForDefinition_t shouldDefine) {
  if (!hasRequiredTypeMetadataAccessFunction(IGM, theDecl))
    return nullptr;

  if (theDecl->isGenericContext()) {
    return getGenericTypeMetadataAccessFunction(IGM, theDecl, shouldDefine);
  }

  CanType declaredType = theDecl->getDeclaredType()->getCanonicalType();
  return getTypeMetadataAccessFunction(IGM, declaredType, shouldDefine);
}

/// Force a public metadata access function into existence if necessary
/// for the given type.
template <class BuilderTy>
static void maybeEmitNominalTypeMetadataAccessFunction(IRGenModule &IGM,
                                                NominalTypeDecl *theDecl,
                                                       BuilderTy &builder) {
  if (!hasRequiredTypeMetadataAccessFunction(IGM, theDecl))
    return;

  builder.createMetadataAccessFunction();
}

/// Emit a call to the type metadata accessor for the given function.
static llvm::Value *emitCallToTypeMetadataAccessFunction(IRGenFunction &IGF,
                                                         CanType type,
                                                 ForDefinition_t shouldDefine) {
  // If we already cached the metadata, use it.
  if (auto local =
        IGF.tryGetLocalTypeData(type, LocalTypeDataKind::forTypeMetadata()))
    return local;
  
  llvm::Constant *accessor =
    getTypeMetadataAccessFunction(IGF.IGM, type, shouldDefine);
  llvm::CallInst *call = IGF.Builder.CreateCall(accessor, {});
  call->setCallingConv(IGF.IGM.DefaultCC);
  call->setDoesNotAccessMemory();
  call->setDoesNotThrow();
  
  // Save the metadata for future lookups.
  IGF.setScopedLocalTypeData(type, LocalTypeDataKind::forTypeMetadata(), call);
  
  return call;
}

/// Produce the type metadata pointer for the given type.
llvm::Value *IRGenFunction::emitTypeMetadataRef(CanType type) {
  type = getRuntimeReifiedType(IGM, type);

  if (type->hasArchetype() ||
      isTypeMetadataAccessTrivial(IGM, type)) {
    return emitDirectTypeMetadataRef(*this, type);
  }

  switch (getTypeMetadataAccessStrategy(IGM, type)) {
  case MetadataAccessStrategy::PublicUniqueAccessor:
  case MetadataAccessStrategy::HiddenUniqueAccessor:
  case MetadataAccessStrategy::PrivateAccessor:
    return emitCallToTypeMetadataAccessFunction(*this, type, NotForDefinition);
  case MetadataAccessStrategy::NonUniqueAccessor:
    return emitCallToTypeMetadataAccessFunction(*this, type, ForDefinition);
  }
  llvm_unreachable("bad type metadata access strategy");
}

/// Return the address of a function that will return type metadata 
/// for the given non-dependent type.
llvm::Function *irgen::getOrCreateTypeMetadataAccessFunction(IRGenModule &IGM,
                                                             CanType type) {
  type = getRuntimeReifiedType(IGM, type);

  assert(!type->hasArchetype() &&
         "cannot create global function to return dependent type metadata");

  switch (getTypeMetadataAccessStrategy(IGM, type)) {
  case MetadataAccessStrategy::PublicUniqueAccessor:
  case MetadataAccessStrategy::HiddenUniqueAccessor:
  case MetadataAccessStrategy::PrivateAccessor:
    return getTypeMetadataAccessFunction(IGM, type, NotForDefinition);
  case MetadataAccessStrategy::NonUniqueAccessor:
    return getTypeMetadataAccessFunction(IGM, type, ForDefinition);
  }
  llvm_unreachable("bad type metadata access strategy");
}

namespace {
  /// A visitor class for emitting a reference to a metatype object.
  /// This implements a "raw" access, useful for implementing cache
  /// functions or for implementing dependent accesses.
  class EmitTypeMetadataRefForLayout
    : public CanTypeVisitor<EmitTypeMetadataRefForLayout, llvm::Value *> {
  private:
    IRGenFunction &IGF;
  public:
    EmitTypeMetadataRefForLayout(IRGenFunction &IGF) : IGF(IGF) {}

    llvm::Value *emitDirectMetadataRef(CanType type) {
      return IGF.IGM.getAddrOfTypeMetadata(type, /*pattern*/ false);
    }

    /// For most types, we can just emit the usual metadata.
    llvm::Value *visitType(CanType t) {
      return IGF.emitTypeMetadataRef(t);
    }

    llvm::Value *visitBoundGenericEnumType(CanBoundGenericEnumType type) {
      // Optionals have a lowered payload type, so we recurse here.
      if (auto objectTy = CanType(type).getAnyOptionalObjectType()) {
        auto payloadMetadata = visit(objectTy);
        llvm::Value *args[] = { payloadMetadata };
        llvm::Type *types[] = { IGF.IGM.TypeMetadataPtrTy };

        // Call the generic metadata accessor function.
        llvm::Function *accessor =
            IGF.IGM.getAddrOfGenericTypeMetadataAccessFunction(
                type->getDecl(), types, NotForDefinition);

        auto result = IGF.Builder.CreateCall(accessor, args);
        result->setDoesNotThrow();
        result->addAttribute(llvm::AttributeSet::FunctionIndex,
                             llvm::Attribute::ReadNone);

        return result;
      }

      // Otherwise, generic arguments are not lowered.
      return visitType(type);
    }

    llvm::Value *visitTupleType(CanTupleType type) {
      if (auto cached = tryGetLocal(type))
        return cached;

      switch (type->getNumElements()) {
      case 0: {// Special case the empty tuple, just use the global descriptor.
        llvm::Constant *fullMetadata = IGF.IGM.getEmptyTupleMetadata();
        llvm::Constant *indices[] = {
          llvm::ConstantInt::get(IGF.IGM.Int32Ty, 0),
          llvm::ConstantInt::get(IGF.IGM.Int32Ty, 1)
        };
        return llvm::ConstantExpr::getInBoundsGetElementPtr(
            /*Ty=*/nullptr, fullMetadata, indices);
      }

      case 1:
          // For layout purposes, we consider a singleton tuple to be
          // isomorphic to its element type.
        return visit(type.getElementType(0));

      case 2: {
        // Find the layout metadata pointers for these elements.
        auto elt0Metadata = visit(type.getElementType(0));
        auto elt1Metadata = visit(type.getElementType(1));

        llvm::Value *args[] = {
          elt0Metadata, elt1Metadata,
          // labels don't matter for layout
          llvm::ConstantPointerNull::get(IGF.IGM.Int8PtrTy),
          llvm::ConstantPointerNull::get(IGF.IGM.WitnessTablePtrTy) // proposed
        };

        auto call = IGF.Builder.CreateCall(IGF.IGM.getGetTupleMetadata2Fn(),
                                           args);
        call->setDoesNotThrow();
        return setLocal(CanType(type), call);
      }

      case 3: {
        // Find the layout metadata pointers for these elements.
        auto elt0Metadata = visit(type.getElementType(0));
        auto elt1Metadata = visit(type.getElementType(1));
        auto elt2Metadata = visit(type.getElementType(2));

        llvm::Value *args[] = {
          elt0Metadata, elt1Metadata, elt2Metadata,
          // labels don't matter for layout
          llvm::ConstantPointerNull::get(IGF.IGM.Int8PtrTy),
          llvm::ConstantPointerNull::get(IGF.IGM.WitnessTablePtrTy) // proposed
        };

        auto call = IGF.Builder.CreateCall(IGF.IGM.getGetTupleMetadata3Fn(),
                                           args);
        call->setDoesNotThrow();
        return setLocal(CanType(type), call);
      }
      default:
        // TODO: use a caching entrypoint (with all information
        // out-of-line) for non-dependent tuples.

        llvm::Value *pointerToFirst = nullptr; // appease -Wuninitialized

        auto elements = type.getElementTypes();
        auto arrayTy = llvm::ArrayType::get(IGF.IGM.TypeMetadataPtrTy,
                                            elements.size());
        Address buffer = IGF.createAlloca(arrayTy,IGF.IGM.getPointerAlignment(),
                                          "tuple-elements");
        IGF.Builder.CreateLifetimeStart(buffer,
                                    IGF.IGM.getPointerSize() * elements.size());
        for (unsigned i = 0, e = elements.size(); i != e; ++i) {
          // Find the metadata pointer for this element.
          llvm::Value *eltMetadata = visit(elements[i]);

          // GEP to the appropriate element and store.
          Address eltPtr = IGF.Builder.CreateStructGEP(buffer, i,
                                                     IGF.IGM.getPointerSize());
          IGF.Builder.CreateStore(eltMetadata, eltPtr);

          // Remember the GEP to the first element.
          if (i == 0) pointerToFirst = eltPtr.getAddress();
        }

        llvm::Value *args[] = {
          llvm::ConstantInt::get(IGF.IGM.SizeTy, elements.size()),
          pointerToFirst,
          // labels don't matter for layout
          llvm::ConstantPointerNull::get(IGF.IGM.Int8PtrTy),
          llvm::ConstantPointerNull::get(IGF.IGM.WitnessTablePtrTy) // proposed
        };

        auto call = IGF.Builder.CreateCall(IGF.IGM.getGetTupleMetadataFn(),
                                           args);
        call->setDoesNotThrow();

        IGF.Builder.CreateLifetimeEnd(buffer,
                                    IGF.IGM.getPointerSize() * elements.size());

        return setLocal(type, call);
      }
    }

    llvm::Value *visitAnyFunctionType(CanAnyFunctionType type) {
      llvm_unreachable("not a SIL type");
    }
      
    llvm::Value *visitSILFunctionType(CanSILFunctionType type) {
      // All function types have the same layout regardless of arguments or
      // abstraction level. Use the metadata for () -> () for thick functions,
      // or Builtin.UnknownObject for block functions.
      auto &C = type->getASTContext();
      switch (type->getRepresentation()) {
      case SILFunctionType::Representation::Thin:
      case SILFunctionType::Representation::Method:
      case SILFunctionType::Representation::WitnessMethod:
      case SILFunctionType::Representation::ObjCMethod:
      case SILFunctionType::Representation::CFunctionPointer:
      case SILFunctionType::Representation::Closure:
        // A thin function looks like a plain pointer.
        // FIXME: Except for extra inhabitants?
        return emitDirectMetadataRef(C.TheRawPointerType);
      case SILFunctionType::Representation::Thick:
        // All function types look like () -> ().
        // FIXME: It'd be nice not to have to call through the runtime here.
        return IGF.emitTypeMetadataRef(CanFunctionType::get(C.TheEmptyTupleType,
                                                          C.TheEmptyTupleType));
      case SILFunctionType::Representation::Block:
        // All block types look like Builtin.UnknownObject.
        return emitDirectMetadataRef(C.TheUnknownObjectType);
      }

      llvm_unreachable("Not a valid SILFunctionType.");
    }

    llvm::Value *visitAnyMetatypeType(CanAnyMetatypeType type) {
      
      assert(type->hasRepresentation()
             && "not a lowered metatype");

      switch (type->getRepresentation()) {
      case MetatypeRepresentation::Thin: {
        // Thin metatypes are empty, so they look like the empty tuple type.
        llvm::Constant *fullMetadata = IGF.IGM.getEmptyTupleMetadata();
        llvm::Constant *indices[] = {
          llvm::ConstantInt::get(IGF.IGM.Int32Ty, 0),
          llvm::ConstantInt::get(IGF.IGM.Int32Ty, 1)
        };
        return llvm::ConstantExpr::getInBoundsGetElementPtr(
            /*Ty=*/nullptr, fullMetadata, indices);
      }
      case MetatypeRepresentation::Thick:
      case MetatypeRepresentation::ObjC:
        // Thick and ObjC metatypes look like pointers with extra inhabitants.
        // Get the metatype metadata from the runtime.
        // FIXME: It'd be nice not to need a runtime call here.
        return IGF.emitTypeMetadataRef(type);
      }

      llvm_unreachable("Not a valid MetatypeRepresentation.");
    }

    /// Try to find the metatype in local data.
    llvm::Value *tryGetLocal(CanType type) {
      return IGF.tryGetLocalTypeDataForLayout(
                                          IGF.IGM.getLoweredType(type),
                                          LocalTypeDataKind::forTypeMetadata());
    }

    /// Set the metatype in local data.
    llvm::Value *setLocal(CanType type, llvm::Instruction *metatype) {
      IGF.setScopedLocalTypeDataForLayout(IGF.IGM.getLoweredType(type),
                                          LocalTypeDataKind::forTypeMetadata(),
                                          metatype);
      return metatype;
    }
  };
} // end anonymous namespace

llvm::Value *IRGenFunction::emitTypeMetadataRefForLayout(SILType type) {
  return EmitTypeMetadataRefForLayout(*this).visit(type.getSwiftRValueType());
}

namespace {

  /// A visitor class for emitting a reference to a type layout struct.
  /// There are a few ways we can emit it:
  ///
  /// - If the type is fixed-layout and we have visibility of its value
  ///   witness table (or one close enough), we can project the layout struct
  ///   from it.
  /// - If the type is fixed layout, we can emit our own copy of the layout
  ///   struct.
  /// - If the type is dynamic-layout, we have to instantiate its metadata
  ///   and project out its metadata. (FIXME: This leads to deadlocks in
  ///   recursive cases, though we can avoid many deadlocks because most
  ///   valid recursive types bottom out in fixed-sized types like classes
  ///   or pointers.)
  class EmitTypeLayoutRef
    : public CanTypeVisitor<EmitTypeLayoutRef, llvm::Value *> {
  private:
    IRGenFunction &IGF;
  public:
    EmitTypeLayoutRef(IRGenFunction &IGF) : IGF(IGF) {}

    llvm::Value *emitFromValueWitnessTablePointer(llvm::Value *vwtable) {
      llvm::Value *indexConstant = llvm::ConstantInt::get(IGF.IGM.Int32Ty,
                               (unsigned)ValueWitness::First_TypeLayoutWitness);
      return IGF.Builder.CreateInBoundsGEP(IGF.IGM.Int8PtrTy, vwtable,
                                           indexConstant);
    }

    /// Emit the type layout by projecting it from a value witness table to
    /// which we have linkage.
    llvm::Value *emitFromValueWitnessTable(CanType t) {
      auto *vwtable = IGF.IGM.getAddrOfValueWitnessTable(t);
      return emitFromValueWitnessTablePointer(vwtable);
    }

    /// Emit the type layout by projecting it from dynamic type metadata.
    llvm::Value *emitFromTypeMetadata(CanType t) {
      auto *vwtable = IGF.emitValueWitnessTableRefForLayout(
                                                    IGF.IGM.getLoweredType(t));
      return emitFromValueWitnessTablePointer(vwtable);
    }

    bool hasVisibleValueWitnessTable(CanType t) const {
      // Some builtin and structural types have value witnesses exported from
      // the runtime.
      auto &C = IGF.IGM.Context;
      if (t == C.TheEmptyTupleType
          || t == C.TheNativeObjectType
          || t == C.TheUnknownObjectType
          || t == C.TheBridgeObjectType
          || t == C.TheRawPointerType)
        return true;
      if (auto intTy = dyn_cast<BuiltinIntegerType>(t)) {
        auto width = intTy->getWidth();
        if (width.isPointerWidth())
          return true;
        if (width.isFixedWidth()) {
          switch (width.getFixedWidth()) {
          case 8:
          case 16:
          case 32:
          case 64:
          case 128:
          case 256:
            return true;
          default:
            return false;
          }
        }
        return false;
      }

      // TODO: If a nominal type is in the same source file as we're currently
      // emitting, we would be able to see its value witness table.
      return false;
    }

    /// Fallback default implementation.
    llvm::Value *visitType(CanType t) {
      auto silTy = IGF.IGM.getLoweredType(t);
      auto &ti = IGF.getTypeInfo(silTy);

      // If the type is in the same source file, or has a common value
      // witness table exported from the runtime, we can project from the
      // value witness table instead of emitting a new record.
      if (hasVisibleValueWitnessTable(t))
        return emitFromValueWitnessTable(t);

      // If the type is a singleton aggregate, the field's layout is equivalent
      // to the aggregate's.
      if (SILType singletonFieldTy = getSingletonAggregateFieldType(IGF.IGM,
                                             silTy, ResilienceExpansion::Maximal))
        return visit(singletonFieldTy.getSwiftRValueType());

      // If the type is fixed-layout, emit a copy of its layout.
      if (auto fixed = dyn_cast<FixedTypeInfo>(&ti)) {

        return IGF.IGM.emitFixedTypeLayout(t, *fixed);
      }

      return emitFromTypeMetadata(t);
    }
      
    llvm::Value *visitAnyFunctionType(CanAnyFunctionType type) {
      llvm_unreachable("not a SIL type");
    }
      
    llvm::Value *visitSILFunctionType(CanSILFunctionType type) {
      // All function types have the same layout regardless of arguments or
      // abstraction level. Use the value witness table for
      // @convention(blah) () -> () from the runtime.
      auto &C = type->getASTContext();
      switch (type->getRepresentation()) {
      case SILFunctionType::Representation::Thin:
      case SILFunctionType::Representation::Method:
      case SILFunctionType::Representation::WitnessMethod:
      case SILFunctionType::Representation::ObjCMethod:
      case SILFunctionType::Representation::CFunctionPointer:
      case SILFunctionType::Representation::Closure:
        // A thin function looks like a plain pointer.
        // FIXME: Except for extra inhabitants?
        return emitFromValueWitnessTable(C.TheRawPointerType);
      case SILFunctionType::Representation::Thick:
        // All function types look like () -> ().
        return emitFromValueWitnessTable(
                CanFunctionType::get(C.TheEmptyTupleType, C.TheEmptyTupleType));
      case SILFunctionType::Representation::Block:
        // All block types look like Builtin.UnknownObject.
        return emitFromValueWitnessTable(C.TheUnknownObjectType);
      }

      llvm_unreachable("Not a valid SILFunctionType.");
    }

    llvm::Value *visitAnyMetatypeType(CanAnyMetatypeType type) {
      
      assert(type->hasRepresentation()
             && "not a lowered metatype");

      switch (type->getRepresentation()) {
      case MetatypeRepresentation::Thin: {
        // Thin metatypes are empty, so they look like the empty tuple type.
        return emitFromValueWitnessTable(IGF.IGM.Context.TheEmptyTupleType);
      }
      case MetatypeRepresentation::Thick:
      case MetatypeRepresentation::ObjC:
        // Thick metatypes look like pointers with spare bits.
        return emitFromValueWitnessTable(
                     CanMetatypeType::get(IGF.IGM.Context.TheNativeObjectType));
      }

      llvm_unreachable("Not a valid MetatypeRepresentation.");
    }

    llvm::Value *visitAnyClassType(ClassDecl *classDecl) {
      // All class types have the same layout.
      switch (getReferenceCountingForClass(IGF.IGM, classDecl)) {
      case ReferenceCounting::Native:
        return emitFromValueWitnessTable(IGF.IGM.Context.TheNativeObjectType);

      case ReferenceCounting::ObjC:
      case ReferenceCounting::Block:
      case ReferenceCounting::Unknown:
        return emitFromValueWitnessTable(IGF.IGM.Context.TheUnknownObjectType);

      case ReferenceCounting::Bridge:
      case ReferenceCounting::Error:
        llvm_unreachable("classes shouldn't have this kind of refcounting");
      }

      llvm_unreachable("Not a valid ReferenceCounting.");
    }

    llvm::Value *visitClassType(CanClassType type) {
      return visitAnyClassType(type->getClassOrBoundGenericClass());
    }

    llvm::Value *visitBoundGenericClassType(CanBoundGenericClassType type) {
      return visitAnyClassType(type->getClassOrBoundGenericClass());
    }

    llvm::Value *visitReferenceStorageType(CanReferenceStorageType type) {
      // Other reference storage types all have the same layout for their
      // storage qualification and the reference counting of their underlying
      // object.

      auto &C = IGF.IGM.Context;
      CanType referent;
      switch (type->getOwnership()) {
      case Ownership::Strong:
        llvm_unreachable("shouldn't be a ReferenceStorageType");
      case Ownership::Weak:
        referent = type.getReferentType().getAnyOptionalObjectType();
        break;
      case Ownership::Unmanaged:
      case Ownership::Unowned:
        referent = type.getReferentType();
        break;
      }

      // Reference storage types with witness tables need open-coded layouts.
      // TODO: Maybe we could provide prefabs for 1 witness table.
      SmallVector<ProtocolDecl*, 2> protocols;
      if (referent.isAnyExistentialType(protocols))
        for (auto *proto : protocols)
          if (IGF.getSILTypes().protocolRequiresWitnessTable(proto))
            return visitType(type);

      // Unmanaged references are plain pointers with extra inhabitants,
      // which look like thick metatypes.
      if (type->getOwnership() == Ownership::Unmanaged) {
        auto metatype = CanMetatypeType::get(C.TheNativeObjectType);
        return emitFromValueWitnessTable(metatype);
      }

      auto getReferenceCountingForReferent
        = [&](CanType referent) -> ReferenceCounting {
          // If Objective-C interop is enabled, generic types might contain
          // Objective-C references, so we have to use unknown reference
          // counting.
          if (isa<ArchetypeType>(referent) ||
              referent->isExistentialType())
            return (IGF.IGM.ObjCInterop ?
                    ReferenceCounting::Unknown :
                    ReferenceCounting::Native);

          if (auto classDecl = referent->getClassOrBoundGenericClass())
            return getReferenceCountingForClass(IGF.IGM, classDecl);

          llvm_unreachable("unexpected referent for ref storage type");
        };

      CanType valueWitnessReferent;
      switch (getReferenceCountingForReferent(referent)) {
      case ReferenceCounting::Unknown:
      case ReferenceCounting::Block:
      case ReferenceCounting::ObjC:
        valueWitnessReferent = C.TheUnknownObjectType;
        break;

      case ReferenceCounting::Native:
        valueWitnessReferent = C.TheNativeObjectType;
        break;

      case ReferenceCounting::Bridge:
        valueWitnessReferent = C.TheBridgeObjectType;
        break;

      case ReferenceCounting::Error:
        llvm_unreachable("shouldn't be possible");
      }

      // Get the reference storage type of the builtin object whose value
      // witness we can borrow.
      if (type->getOwnership() == Ownership::Weak)
        valueWitnessReferent = OptionalType::get(valueWitnessReferent)
          ->getCanonicalType();

      auto valueWitnessType = CanReferenceStorageType::get(valueWitnessReferent,
                                                       type->getOwnership());
      return emitFromValueWitnessTable(valueWitnessType);
    }
  };

} // end anonymous namespace

llvm::Value *IRGenFunction::emitTypeLayoutRef(SILType type) {
  return EmitTypeLayoutRef(*this).visit(type.getSwiftRValueType());
}

void IRGenModule::setTrueConstGlobal(llvm::GlobalVariable *var) {
  switch (TargetInfo.OutputObjectFormat) {
  case llvm::Triple::UnknownObjectFormat:
    llvm_unreachable("unknown object format");
  case llvm::Triple::MachO:
    var->setSection("__TEXT,__const");
    break;
  case llvm::Triple::ELF:
    var->setSection(".rodata");
    break;
  case llvm::Triple::COFF:
    var->setSection(".rdata");
    break;
  case llvm::Triple::Wasm:
    llvm_unreachable("web assembly object format is not supported.");
    break;
  }
}

/// Produce the heap metadata pointer for the given class type.  For
/// Swift-defined types, this is equivalent to the metatype for the
/// class, but for Objective-C-defined types, this is the class
/// object.
llvm::Value *irgen::emitClassHeapMetadataRef(IRGenFunction &IGF, CanType type,
                                             MetadataValueType desiredType,
                                             bool allowUninitialized) {
  assert(type->mayHaveSuperclass());

  // Archetypes may or may not be ObjC classes and need unwrapping to get at
  // the class object.
  if (auto archetype = dyn_cast<ArchetypeType>(type)) {
    // Look up the Swift metadata from context.
    llvm::Value *archetypeMeta = IGF.emitTypeMetadataRef(type);
    // Get the class pointer.
    auto classPtr = emitClassHeapMetadataRefForMetatype(IGF, archetypeMeta,
                                                        archetype);
    if (desiredType == MetadataValueType::ObjCClass)
      classPtr = IGF.Builder.CreateBitCast(classPtr, IGF.IGM.ObjCClassPtrTy);
    return classPtr;
  }
  
  if (ClassDecl *theClass = type->getClassOrBoundGenericClass()) {
    if (!hasKnownSwiftMetadata(IGF.IGM, theClass)) {
      llvm::Value *result =
        emitObjCHeapMetadataRef(IGF, theClass, allowUninitialized);
      if (desiredType == MetadataValueType::TypeMetadata)
        result = IGF.Builder.CreateBitCast(result, IGF.IGM.TypeMetadataPtrTy);
      return result;
    }
  }

  llvm::Value *result = IGF.emitTypeMetadataRef(type);
  if (desiredType == MetadataValueType::ObjCClass)
    result = IGF.Builder.CreateBitCast(result, IGF.IGM.ObjCClassPtrTy);
  return result;
}

/// Emit a metatype value for a known type.
void irgen::emitMetatypeRef(IRGenFunction &IGF, CanMetatypeType type,
                            Explosion &explosion) {
  switch (type->getRepresentation()) {
  case MetatypeRepresentation::Thin:
    // Thin types have a trivial representation.
    break;

  case MetatypeRepresentation::Thick:
    explosion.add(IGF.emitTypeMetadataRef(type.getInstanceType()));
    break;

  case MetatypeRepresentation::ObjC:
    explosion.add(emitClassHeapMetadataRef(IGF, type.getInstanceType(),
                                           MetadataValueType::ObjCClass));
    break;
  }
}

/*****************************************************************************/
/** Nominal Type Descriptor Emission *****************************************/
/*****************************************************************************/

namespace {  
  /// Helper to produce a dummy value that can stand in for a global constant
  /// for us to build relative references against before the constant is
  /// instantiated.
  static std::unique_ptr<llvm::GlobalVariable>
  createTemporaryRelativeAddressBase(IRGenModule &IGM) {
    return std::unique_ptr<llvm::GlobalVariable>(
      new llvm::GlobalVariable(IGM.Int8Ty, true,
                               llvm::GlobalValue::PrivateLinkage));
  }
  
  static void
  replaceTemporaryRelativeAddressBase(IRGenModule &IGM,
            std::unique_ptr<llvm::GlobalVariable> temp, llvm::Constant *real) {
    real = llvm::ConstantExpr::getBitCast(real, IGM.Int8PtrTy);
    temp->replaceAllUsesWith(real);
  }

  template<class Impl>
  class NominalTypeDescriptorBuilderBase : public ConstantBuilder<> {
    Impl &asImpl() { return *static_cast<Impl*>(this); }

  public:
    NominalTypeDescriptorBuilderBase(IRGenModule &IGM) : ConstantBuilder(IGM) {}
    
    void layout() {
      asImpl().addName();
      asImpl().addKindDependentFields();
      asImpl().addGenericMetadataPatternAndKind();
      asImpl().addAccessFunction();
      asImpl().addGenericParams();
    }

    CanType getAbstractType() {
      return asImpl().getTarget()->getDeclaredType()->getCanonicalType();
    }

    void addName() {
      addRelativeAddress(getMangledTypeName(IGM, getAbstractType(),
                                 /*willBeRelativelyAddressed*/ true));
    }
    
    void addGenericMetadataPatternAndKind() {
      NominalTypeDecl *ntd = asImpl().getTarget();
      auto kind = asImpl().getKind();
      if (!ntd->isGenericContext()) {
        // There's no pattern to link.
        addConstantInt32(kind);
        return;
      }

      addRelativeAddressWithTag(
        IGM.getAddrOfTypeMetadata(getAbstractType(), /*pattern*/ true),
        kind);
    }

    void addAccessFunction() {
      NominalTypeDecl *typeDecl = asImpl().getTarget();
      llvm::Constant *accessFn =
        getRequiredTypeMetadataAccessFunction(IGM, typeDecl, NotForDefinition);
      addRelativeAddressOrNull(accessFn);
    }
    
    void addGenericParams() {
      NominalTypeDecl *ntd = asImpl().getTarget();

      // uint32_t GenericParameterVectorOffset;
      addConstantInt32InWords(asImpl().getGenericParamsOffset());

      // The archetype order here needs to be consistent with
      // MetadataLayout::addGenericFields.
      
      GenericTypeRequirements requirements(IGM, ntd);
      
      // uint32_t NumGenericRequirements;
      addConstantInt32(requirements.getStorageSizeInWords());

      // uint32_t NumPrimaryGenericParameters;
      addConstantInt32(requirements.getNumTypeRequirements());

      // GenericParameterDescriptorFlags Flags;
      GenericParameterDescriptorFlags flags;
      if (ntd->getDeclContext()->isTypeContext())
        flags = flags.withHasParent(true);
      if (requirements.hasParentType())
        flags = flags.withHasGenericParent(true);
      addConstantInt32(flags.getIntValue());

      // TODO: provide reflective descriptions of the type and
      // conformance requirements stored here.

      // };
    }
    
    llvm::Constant *emit() {
      // Set up a dummy global to stand in for the constant.
      auto tempBase = createTemporaryRelativeAddressBase(IGM);
      setRelativeAddressBase(tempBase.get());
      asImpl().layout();
      auto init = getInit();
      
      auto var = cast<llvm::GlobalVariable>(
                      IGM.getAddrOfNominalTypeDescriptor(asImpl().getTarget(),
                                                         init->getType()));
      var->setConstant(true);
      var->setInitializer(init);
      IGM.setTrueConstGlobal(var);

      replaceTemporaryRelativeAddressBase(IGM, std::move(tempBase), var);
      return var;
    }
    
    // Derived class must provide:
    //   NominalTypeDecl *getTarget();
    //   unsigned getKind();
    //   unsigned getGenericParamsOffset();
    //   void addKindDependentFields();
  };

  /// A CRTP helper for classes which are simply searching for a
  /// specific index within the metadata.
  ///
  /// The pattern is that subclasses should override an 'add' method
  /// from the appropriate layout class and ensure that they call
  /// setTargetOffset() when the appropriate location is reached.  The
  /// subclass user then just calls getTargetOffset(), which performs
  /// the layout and returns the found index.
  ///
  /// \tparam Base the base class, which should generally be a CRTP
  ///   class template applied to the most-derived class
  template <class Base> class MetadataSearcher : public Base {
    Size TargetOffset = Size::invalid();
    Size AddressPoint = Size::invalid();

  protected:
    void setTargetOffset() {
      assert(TargetOffset.isInvalid() && "setting twice");
      TargetOffset = this->NextOffset;
    }

  public:
    template <class... T> MetadataSearcher(T &&...args)
      : Base(std::forward<T>(args)...) {}

    void noteAddressPoint() { AddressPoint = this->NextOffset; }

    Size getTargetOffset() {
      assert(TargetOffset.isInvalid() && "computing twice");
      this->layout();
      assert(!TargetOffset.isInvalid() && "target not found!");
      assert(!AddressPoint.isInvalid() && "address point not set");
      return TargetOffset - AddressPoint;
    }

    Size::int_type getTargetIndex() {
      return this->IGM.getOffsetInWords(getTargetOffset());
    }
  };

  // A bunch of ugly macros to make it easy to declare certain
  // common kinds of searcher.
#define BEGIN_METADATA_SEARCHER_0(SEARCHER, DECLKIND)                   \
  struct SEARCHER                                                       \
    : MetadataSearcher<DECLKIND##MetadataScanner<SEARCHER>> {           \
    using super = MetadataSearcher;                                     \
    SEARCHER(IRGenModule &IGM, DECLKIND##Decl *target)                  \
      : super(IGM, target) {}
#define BEGIN_METADATA_SEARCHER_1(SEARCHER, DECLKIND, TYPE_1, NAME_1)   \
  struct SEARCHER                                                       \
      : MetadataSearcher<DECLKIND##MetadataScanner<SEARCHER>> {         \
    using super = MetadataSearcher;                                     \
    TYPE_1 NAME_1;                                                      \
    SEARCHER(IRGenModule &IGM, DECLKIND##Decl *target, TYPE_1 NAME_1)   \
      : super(IGM, target), NAME_1(NAME_1) {}
#define BEGIN_METADATA_SEARCHER_2(SEARCHER, DECLKIND, TYPE_1, NAME_1,   \
                                  TYPE_2, NAME_2)                       \
  struct SEARCHER                                                       \
      : MetadataSearcher<DECLKIND##MetadataScanner<SEARCHER>> {         \
    using super = MetadataSearcher;                                     \
    TYPE_1 NAME_1;                                                      \
    TYPE_2 NAME_2;                                                      \
    SEARCHER(IRGenModule &IGM, DECLKIND##Decl *target, TYPE_1 NAME_1,   \
             TYPE_2 NAME_2)                                             \
      : super(IGM, target), NAME_1(NAME_1), NAME_2(NAME_2) {}
#define END_METADATA_SEARCHER()                                         \
  };

#define BEGIN_GENERIC_METADATA_SEARCHER_0(SEARCHER)                     \
  template <template <class Impl> class Scanner>                        \
  struct SEARCHER : MetadataSearcher<Scanner<SEARCHER<Scanner>>> {      \
    using super = MetadataSearcher<Scanner<SEARCHER<Scanner>>>;         \
    using super::Target;                                                \
    using TargetType = decltype(Target);                                \
    SEARCHER(IRGenModule &IGM, TargetType target)                       \
      : super(IGM, target) {}
#define BEGIN_GENERIC_METADATA_SEARCHER_1(SEARCHER, TYPE_1, NAME_1)     \
  template <template <class Impl> class Scanner>                        \
  struct SEARCHER : MetadataSearcher<Scanner<SEARCHER<Scanner>>> {      \
    using super = MetadataSearcher<Scanner<SEARCHER<Scanner>>>;         \
    using super::Target;                                                \
    using TargetType = decltype(Target);                                \
    TYPE_1 NAME_1;                                                      \
    SEARCHER(IRGenModule &IGM, TargetType target, TYPE_1 NAME_1)        \
      : super(IGM, target), NAME_1(NAME_1) {}
#define BEGIN_GENERIC_METADATA_SEARCHER_2(SEARCHER, TYPE_1, NAME_1,     \
                                          TYPE_2, NAME_2)               \
  template <template <class Impl> class Scanner>                        \
  struct SEARCHER : MetadataSearcher<Scanner<SEARCHER<Scanner>>> {      \
    using super = MetadataSearcher<Scanner<SEARCHER<Scanner>>>;         \
    using super::Target;                                                \
    using TargetType = decltype(Target);                                \
    TYPE_1 NAME_1;                                                      \
    TYPE_2 NAME_2;                                                      \
    SEARCHER(IRGenModule &IGM, TargetType target,                       \
             TYPE_1 NAME_1, TYPE_2 NAME_2)                              \
      : super(IGM, target), NAME_1(NAME_1), NAME_2(NAME_2) {}
#define END_GENERIC_METADATA_SEARCHER(SOUGHT)                           \
  };                                                                    \
  using FindClass##SOUGHT = FindType##SOUGHT<ClassMetadataScanner>;     \
  using FindStruct##SOUGHT = FindType##SOUGHT<StructMetadataScanner>;   \
  using FindEnum##SOUGHT = FindType##SOUGHT<EnumMetadataScanner>;

  /// The total size and address point of a metadata object.
  struct MetadataSize {
    Size FullSize;
    Size AddressPoint;

    /// Return the offset from the address point to the end of the
    /// metadata object.
    Size getOffsetToEnd() const {
      return FullSize - AddressPoint;
    }
  };

  /// A template for computing the size of a metadata record.
  template <template <class T> class Scanner>
  class MetadataSizer : public Scanner<MetadataSizer<Scanner>> {
    typedef Scanner<MetadataSizer<Scanner>> super;
    using super::Target;
    using TargetType = decltype(Target);

    Size AddressPoint = Size::invalid();
  public:
    MetadataSizer(IRGenModule &IGM, TargetType target)
      : super(IGM, target) {}

    void noteAddressPoint() {
      AddressPoint = super::NextOffset;
      super::noteAddressPoint();
    }

    static MetadataSize compute(IRGenModule &IGM, TargetType target) {
      MetadataSizer sizer(IGM, target);
      sizer.layout();

      assert(!sizer.AddressPoint.isInvalid()
             && "did not find address point?!");
      assert(sizer.AddressPoint < sizer.NextOffset
             && "address point is after end?!");
      return { sizer.NextOffset, sizer.AddressPoint };
    }
  };

  static MetadataSize getSizeOfMetadata(IRGenModule &IGM, StructDecl *decl) {
    return MetadataSizer<StructMetadataScanner>::compute(IGM, decl);
  }

  static MetadataSize getSizeOfMetadata(IRGenModule &IGM, ClassDecl *decl) {
    return MetadataSizer<ClassMetadataScanner>::compute(IGM, decl);
  }

  static MetadataSize getSizeOfMetadata(IRGenModule &IGM, EnumDecl *decl) {
    return MetadataSizer<EnumMetadataScanner>::compute(IGM, decl);
  }

  /// Return the total size and address point of a metadata record.
  static MetadataSize getSizeOfMetadata(IRGenModule &IGM,
                                        NominalTypeDecl *decl) {
    if (auto theStruct = dyn_cast<StructDecl>(decl)) {
      return getSizeOfMetadata(IGM, theStruct);
    } else if (auto theClass = dyn_cast<ClassDecl>(decl)) {
      return getSizeOfMetadata(IGM, theClass);
    } else if (auto theEnum = dyn_cast<EnumDecl>(decl)) {
      return getSizeOfMetadata(IGM, theEnum);
    } else {
      llvm_unreachable("not implemented for other nominal types");
    }
  }
  
  /// Build a doubly-null-terminated list of field names.
  template<typename ValueDeclRange>
  unsigned getFieldNameString(const ValueDeclRange &fields,
                              llvm::SmallVectorImpl<char> &out) {
    unsigned numFields = 0;

    {
      llvm::raw_svector_ostream os(out);
      
      for (ValueDecl *prop : fields) {
        os << prop->getBaseName() << '\0';
        ++numFields;
      }
      // The final null terminator is provided by getAddrOfGlobalString.
    }
    return numFields;
  }
  
  /// Build the field type vector accessor for a nominal type. This is a
  /// function that lazily instantiates the type metadata for all of the
  /// types of the stored properties of an instance of a nominal type.
  static llvm::Function *
  getFieldTypeAccessorFn(IRGenModule &IGM,
                         NominalTypeDecl *type,
                         ArrayRef<FieldTypeInfo> fieldTypes) {
    // The accessor function has the following signature:
    // const Metadata * const *(*GetFieldTypes)(const Metadata *T);
    auto metadataArrayPtrTy = IGM.TypeMetadataPtrTy->getPointerTo();
    auto fnTy = llvm::FunctionType::get(metadataArrayPtrTy,
                                        IGM.TypeMetadataPtrTy,
                                        /*vararg*/ false);
    auto fn = llvm::Function::Create(fnTy, llvm::GlobalValue::PrivateLinkage,
                                     llvm::Twine("get_field_types_")
                                       + type->getName().str(),
                                     IGM.getModule());
    fn->setAttributes(IGM.constructInitialAttributes());

    // Emit the body of the field type accessor later. We need to access
    // the type metadata for the fields, which could lead to infinite recursion
    // in recursive types if we build the field type accessor during metadata
    // generation.
    IGM.addLazyFieldTypeAccessor(type, fieldTypes, fn);
    
    return fn;
  }
  
  /// Build a field type accessor for stored properties.
  static llvm::Function *
  getFieldTypeAccessorFn(IRGenModule &IGM,
                         NominalTypeDecl *type,
                         NominalTypeDecl::StoredPropertyRange storedProperties){
    SmallVector<FieldTypeInfo, 4> types;
    for (VarDecl *prop : storedProperties) {
      auto propertyType = type->mapTypeIntoContext(prop->getInterfaceType())
                              ->getCanonicalType();
      types.push_back(FieldTypeInfo(propertyType,
                                    /*indirect*/ false,
                                    propertyType->is<WeakStorageType>()));
    }
    return getFieldTypeAccessorFn(IGM, type, types);
  }
  
  /// Build a case type accessor for enum payloads.
  static llvm::Function *
  getFieldTypeAccessorFn(IRGenModule &IGM,
                         NominalTypeDecl *type,
                         ArrayRef<EnumImplStrategy::Element> enumElements) {
    SmallVector<FieldTypeInfo, 4> types;

    // This is a terrible special case, but otherwise the archetypes
    // aren't mapped correctly because the EnumImplStrategy ends up
    // using the lowered cases, i.e. the cases for Optional<>.
    if (type->classifyAsOptionalType() == OTK_ImplicitlyUnwrappedOptional) {
      assert(enumElements.size() == 1);
      auto decl = IGM.Context.getImplicitlyUnwrappedOptionalSomeDecl();
      auto caseType = decl->getParentEnum()->mapTypeIntoContext(
        decl->getArgumentInterfaceType())
          ->getCanonicalType();
      types.push_back(FieldTypeInfo(caseType, false, false));
      return getFieldTypeAccessorFn(IGM, type, types);
    }

    for (auto &elt : enumElements) {
      auto caseType = elt.decl->getParentEnum()->mapTypeIntoContext(
        elt.decl->getArgumentInterfaceType())
          ->getCanonicalType();
      bool isIndirect = elt.decl->isIndirect()
        || elt.decl->getParentEnum()->isIndirect();
      types.push_back(FieldTypeInfo(caseType, isIndirect, /*weak*/ false));
    }
    return getFieldTypeAccessorFn(IGM, type, types);
  }
  
  class StructNominalTypeDescriptorBuilder
    : public NominalTypeDescriptorBuilderBase<StructNominalTypeDescriptorBuilder>
  {
    using super
      = NominalTypeDescriptorBuilderBase<StructNominalTypeDescriptorBuilder>;
    
    // Offsets of key fields in the metadata records.
    Size FieldVectorOffset, GenericParamsOffset;
    
    StructDecl *Target;
    
  public:
    StructNominalTypeDescriptorBuilder(IRGenModule &IGM,
                                       StructDecl *s)
      : super(IGM), Target(s)
    {
      struct ScanForDescriptorOffsets
        : StructMetadataScanner<ScanForDescriptorOffsets>
      {
        ScanForDescriptorOffsets(IRGenModule &IGM, StructDecl *Target)
          : StructMetadataScanner(IGM, Target) {}

        Size AddressPoint = Size::invalid();
        Size FieldVectorOffset = Size::invalid();
        Size GenericParamsOffset = Size::invalid();
        
        void noteAddressPoint() { AddressPoint = NextOffset; }
        void noteStartOfFieldOffsets() { FieldVectorOffset = NextOffset; }
        void noteStartOfGenericRequirements() {
          GenericParamsOffset = NextOffset;
        }
      };
      
      ScanForDescriptorOffsets scanner(IGM, Target);
      scanner.layout();
      assert(!scanner.AddressPoint.isInvalid()
             && !scanner.FieldVectorOffset.isInvalid()
             && "did not find required fields in struct metadata?!");
      assert(scanner.FieldVectorOffset >= scanner.AddressPoint
             && "found field offset vector after address point?!");
      assert(scanner.GenericParamsOffset >= scanner.AddressPoint
             && "found generic param vector after address point?!");
      FieldVectorOffset = scanner.FieldVectorOffset - scanner.AddressPoint;
      GenericParamsOffset = scanner.GenericParamsOffset.isInvalid()
        ? Size(0) : scanner.GenericParamsOffset - scanner.AddressPoint;
    }
    
    StructDecl *getTarget() { return Target; }
    
    unsigned getKind() {
      return unsigned(NominalTypeKind::Struct);
    }
    
    Size getGenericParamsOffset() {
      return GenericParamsOffset;
    }
    
    void addKindDependentFields() {
      // Build the field name list.
      llvm::SmallString<64> fieldNames;
      unsigned numFields = getFieldNameString(Target->getStoredProperties(),
                                              fieldNames);
      
      addConstantInt32(numFields);
      addConstantInt32InWords(FieldVectorOffset);
      addRelativeAddress(IGM.getAddrOfGlobalString(fieldNames,
                                           /*willBeRelativelyAddressed*/ true));
      
      // Build the field type accessor function.
      llvm::Function *fieldTypeVectorAccessor
        = getFieldTypeAccessorFn(IGM, Target,
                                   Target->getStoredProperties());
      
      addRelativeAddress(fieldTypeVectorAccessor);
    }
  };
  
  class ClassNominalTypeDescriptorBuilder
    : public NominalTypeDescriptorBuilderBase<ClassNominalTypeDescriptorBuilder>
  {
    using super
      = NominalTypeDescriptorBuilderBase<ClassNominalTypeDescriptorBuilder>;
    
    // Offsets of key fields in the metadata records.
    Size FieldVectorOffset, GenericParamsOffset;
    
    ClassDecl *Target;
    
  public:
    ClassNominalTypeDescriptorBuilder(IRGenModule &IGM,
                                       ClassDecl *c)
      : super(IGM), Target(c)
    {
      // Scan the metadata layout for the class to find the key offsets to
      // put in our descriptor.
      struct ScanForDescriptorOffsets
        : ClassMetadataScanner<ScanForDescriptorOffsets>
      {
        ScanForDescriptorOffsets(IRGenModule &IGM, ClassDecl *Target)
          : ClassMetadataScanner(IGM, Target) {}
        
        Size AddressPoint = Size::invalid();
        Size FieldVectorOffset = Size::invalid();
        Size GenericParamsOffset = Size::invalid();
        
        void noteAddressPoint() { AddressPoint = NextOffset; }
        void noteStartOfFieldOffsets(ClassDecl *c) {
          if (c == Target) {
            FieldVectorOffset = NextOffset;
          }
        }
        void noteStartOfGenericRequirements(ClassDecl *c) {
          if (c == Target) {
            GenericParamsOffset = NextOffset;
          }
        }
      };
      
      ScanForDescriptorOffsets scanner(IGM, Target);
      scanner.layout();
      assert(!scanner.AddressPoint.isInvalid()
             && !scanner.FieldVectorOffset.isInvalid()
             && "did not find required fields in struct metadata?!");
      assert(scanner.FieldVectorOffset >= scanner.AddressPoint
             && "found field offset vector after address point?!");
      assert(scanner.GenericParamsOffset >= scanner.AddressPoint
             && "found generic param vector after address point?!");
      FieldVectorOffset = scanner.FieldVectorOffset - scanner.AddressPoint;
      GenericParamsOffset = scanner.GenericParamsOffset - scanner.AddressPoint;
    }
    
    ClassDecl *getTarget() { return Target; }
    
    unsigned getKind() {
      return unsigned(NominalTypeKind::Class);
    }
    
    Size getGenericParamsOffset() {
      return GenericParamsOffset;
    }
    
    void addKindDependentFields() {
      // Build the field name list.
      llvm::SmallString<64> fieldNames;
      unsigned numFields = getFieldNameString(Target->getStoredProperties(),
                                              fieldNames);
      
      addConstantInt32(numFields);
      addConstantInt32InWords(FieldVectorOffset);
      addRelativeAddress(IGM.getAddrOfGlobalString(fieldNames,
                                           /*willBeRelativelyAddressed*/ true));
      
      // Build the field type accessor function.
      llvm::Function *fieldTypeVectorAccessor
        = getFieldTypeAccessorFn(IGM, Target,
                                   Target->getStoredProperties());
      
      addRelativeAddress(fieldTypeVectorAccessor);
    }
  };
  
  class EnumNominalTypeDescriptorBuilder
    : public NominalTypeDescriptorBuilderBase<EnumNominalTypeDescriptorBuilder>
  {
    using super
      = NominalTypeDescriptorBuilderBase<EnumNominalTypeDescriptorBuilder>;
    
    // Offsets of key fields in the metadata records.
    Size GenericParamsOffset;
    Size PayloadSizeOffset;
    
    EnumDecl *Target;
    
  public:
    EnumNominalTypeDescriptorBuilder(IRGenModule &IGM, EnumDecl *c)
      : super(IGM), Target(c)
    {
      // Scan the metadata layout for the class to find the key offsets to
      // put in our descriptor.
      struct ScanForDescriptorOffsets
        : EnumMetadataScanner<ScanForDescriptorOffsets>
      {
        ScanForDescriptorOffsets(IRGenModule &IGM, EnumDecl *Target)
          : EnumMetadataScanner(IGM, Target) {}
        
        Size AddressPoint = Size::invalid();
        Size GenericParamsOffset = Size::invalid();
        Size PayloadSizeOffset = Size::invalid();
        
        void noteAddressPoint() { AddressPoint = NextOffset; }
        void addPayloadSize() {
          PayloadSizeOffset = NextOffset;
          EnumMetadataScanner::addPayloadSize();
        }
        void noteStartOfGenericRequirements() {
          GenericParamsOffset = NextOffset;
        }
      };
      
      ScanForDescriptorOffsets scanner(IGM, Target);
      scanner.layout();
      assert(!scanner.AddressPoint.isInvalid()
             && "did not find fields in Enum metadata?!");
      assert(scanner.GenericParamsOffset >= scanner.AddressPoint
             && "found generic param vector after address point?!");
      GenericParamsOffset = scanner.GenericParamsOffset.isInvalid()
        ? Size(0) : scanner.GenericParamsOffset - scanner.AddressPoint;
      PayloadSizeOffset = scanner.PayloadSizeOffset.isInvalid()
        ? Size(0) : scanner.PayloadSizeOffset - scanner.AddressPoint;
    }
    
    EnumDecl *getTarget() { return Target; }
    
    unsigned getKind() {
      return unsigned(NominalTypeKind::Enum);
    }
    
    Size getGenericParamsOffset() {
      return GenericParamsOffset;
    }
    
    void addKindDependentFields() {
      auto &strategy = getEnumImplStrategy(IGM,
                        Target->getDeclaredTypeInContext()->getCanonicalType());
      
      
      // # payload cases in the low 24 bits, payload size offset in the high 8.
      unsigned numPayloads = strategy.getElementsWithPayload().size();
      assert(numPayloads < (1<<24) && "too many payload elements for runtime");
      assert(PayloadSizeOffset % IGM.getPointerAlignment() == Size(0)
             && "payload size not word-aligned");
      unsigned PayloadSizeOffsetInWords
        = PayloadSizeOffset / IGM.getPointerSize();
      assert(PayloadSizeOffsetInWords < 0x100 &&
             "payload size offset too far from address point for runtime");
      addConstantInt32(numPayloads | (PayloadSizeOffsetInWords << 24));
      // # empty cases
      addConstantInt32(strategy.getElementsWithNoPayload().size());

      addRelativeAddressOrNull(strategy.emitCaseNames());

      // Build the case type accessor.
      llvm::Function *caseTypeVectorAccessor
        = getFieldTypeAccessorFn(IGM, Target,
                                 strategy.getElementsWithPayload());
      
      addRelativeAddress(caseTypeVectorAccessor);
    }
  };

} // end anonymous namespace

void
IRGenModule::addLazyFieldTypeAccessor(NominalTypeDecl *type,
                                      ArrayRef<FieldTypeInfo> fieldTypes,
                                      llvm::Function *fn) {
  IRGen.addLazyFieldTypeAccessor(type, fieldTypes, fn, this);
}

void
irgen::emitFieldTypeAccessor(IRGenModule &IGM,
                             NominalTypeDecl *type,
                             llvm::Function *fn,
                             ArrayRef<FieldTypeInfo> fieldTypes)
{
  IRGenFunction IGF(IGM, fn);
  if (IGM.DebugInfo)
    IGM.DebugInfo->emitArtificialFunction(IGF, fn);

  auto metadataArrayPtrTy = IGM.TypeMetadataPtrTy->getPointerTo();

  CanType formalType = type->getDeclaredTypeInContext()->getCanonicalType();
  llvm::Value *metadata = IGF.collectParameters().claimNext();
  setTypeMetadataName(IGM, metadata, formalType);
  
  // Get the address at which the field type vector reference should be
  // cached.
  llvm::Value *vectorPtr;
  auto nullVector = llvm::ConstantPointerNull::get(metadataArrayPtrTy);
  
  // If the type is not generic, we can use a global variable to cache the
  // address of the field type vector for the single instance.
  if (!type->isGenericContext()) {
    vectorPtr = new llvm::GlobalVariable(*IGM.getModule(),
                                         metadataArrayPtrTy,
                                         /*constant*/ false,
                                         llvm::GlobalValue::PrivateLinkage,
                                         nullVector,
                                         llvm::Twine("field_type_vector_")
                                           + type->getName().str());
  // For a generic type, use a slot we saved in the generic metadata pattern
  // immediately after the metadata object itself, which should be
  // instantiated with every generic metadata instance.
  } else {
    Size offset = getSizeOfMetadata(IGM, type).getOffsetToEnd();
    vectorPtr = IGF.Builder.CreateBitCast(metadata,
                                          metadataArrayPtrTy->getPointerTo());
    vectorPtr = IGF.Builder.CreateConstInBoundsGEP1_32(
        /*Ty=*/nullptr, vectorPtr, IGM.getOffsetInWords(offset));
  }
  
  // First, see if the field type vector has already been populated. This
  // load can be nonatomic; if we race to build the field offset vector, we
  // will detect so when we try to commit our pointer and simply discard the
  // redundant work.
  llvm::Value *initialVector
    = IGF.Builder.CreateLoad(vectorPtr, IGM.getPointerAlignment());
  
  auto entryBB = IGF.Builder.GetInsertBlock();
  auto buildBB = IGF.createBasicBlock("build_field_types");
  auto raceLostBB = IGF.createBasicBlock("race_lost");
  auto doneBB = IGF.createBasicBlock("done");
  
  llvm::Value *isNull
    = IGF.Builder.CreateICmpEQ(initialVector, nullVector);
  IGF.Builder.CreateCondBr(isNull, buildBB, doneBB);
  
  // Build the field type vector if we didn't already.
  IGF.Builder.emitBlock(buildBB);
  
  // Bind the metadata instance to our local type data so we
  // use it to provide metadata for generic parameters in field types.
  IGF.bindLocalTypeDataFromTypeMetadata(formalType, IsExact, metadata);
  
  // Bind archetype access paths if the type is generic.
  if (type->isGenericContext()) {
    auto declCtxt = type;
    if (auto generics = declCtxt->getGenericSignatureOfContext()) {
      auto getInContext = [&](CanType type) -> CanType {
        return declCtxt->mapTypeIntoContext(type)
            ->getCanonicalType();
      };
      bindArchetypeAccessPaths(IGF, generics, getInContext);
    }
  }

  // Allocate storage for the field vector.
  unsigned allocSize = fieldTypes.size() * IGM.getPointerSize().getValue();
  auto allocSizeVal = llvm::ConstantInt::get(IGM.IntPtrTy, allocSize);
  auto allocAlignMaskVal =
    IGM.getSize(IGM.getPointerAlignment().asSize() - Size(1));
  llvm::Value *builtVectorAlloc
    = IGF.emitAllocRawCall(allocSizeVal, allocAlignMaskVal);
  
  llvm::Value *builtVector
    = IGF.Builder.CreateBitCast(builtVectorAlloc, metadataArrayPtrTy);
  
  // Emit type metadata for the fields into the vector.
  for (unsigned i : indices(fieldTypes)) {
    auto fieldTy = fieldTypes[i].getType();
    auto slot = IGF.Builder.CreateInBoundsGEP(builtVector,
                      llvm::ConstantInt::get(IGM.Int32Ty, i));
    
    // Strip reference storage qualifiers like unowned and weak.
    // FIXME: Some clients probably care about them.
    if (auto refStorTy = dyn_cast<ReferenceStorageType>(fieldTy))
      fieldTy = refStorTy.getReferentType();
    
    auto metadata = IGF.emitTypeMetadataRef(fieldTy);

    auto fieldTypeInfo = fieldTypes[i];

    // Mix in flag bits.
    if (fieldTypeInfo.hasFlags()) {
      auto flags = FieldType()
        .withIndirect(fieldTypeInfo.isIndirect())
        .withWeak(fieldTypeInfo.isWeak());
      auto metadataBits = IGF.Builder.CreatePtrToInt(metadata, IGF.IGM.SizeTy);
      metadataBits = IGF.Builder.CreateOr(metadataBits,
                   llvm::ConstantInt::get(IGF.IGM.SizeTy, flags.getIntValue()));
      metadata = IGF.Builder.CreateIntToPtr(metadataBits, metadata->getType());
    }

    IGF.Builder.CreateStore(metadata, slot, IGM.getPointerAlignment());
  }
  
  // Atomically compare-exchange a pointer to our vector into the slot.
  auto vectorIntPtr = IGF.Builder.CreateBitCast(vectorPtr,
                                                IGM.IntPtrTy->getPointerTo());
  auto builtVectorInt = IGF.Builder.CreatePtrToInt(builtVector,
                                                   IGM.IntPtrTy);
  auto zero = llvm::ConstantInt::get(IGM.IntPtrTy, 0);
  
  llvm::Value *raceVectorInt = IGF.Builder.CreateAtomicCmpXchg(vectorIntPtr,
                               zero, builtVectorInt,
                               llvm::AtomicOrdering::SequentiallyConsistent,
                               llvm::AtomicOrdering::SequentiallyConsistent);

  // We might have added internal control flow above.
  buildBB = IGF.Builder.GetInsertBlock();

  // The pointer in the slot should still have been null.
  auto didStore = IGF.Builder.CreateExtractValue(raceVectorInt, 1);
  raceVectorInt = IGF.Builder.CreateExtractValue(raceVectorInt, 0);
  IGF.Builder.CreateCondBr(didStore, doneBB, raceLostBB);
  
  // If the cmpxchg failed, someone beat us to landing their field type
  // vector. Deallocate ours and return the winner.
  IGF.Builder.emitBlock(raceLostBB);
  IGF.emitDeallocRawCall(builtVectorAlloc, allocSizeVal, allocAlignMaskVal);
  auto raceVector = IGF.Builder.CreateIntToPtr(raceVectorInt,
                                               metadataArrayPtrTy);
  IGF.Builder.CreateBr(doneBB);
  
  // Return the result.
  IGF.Builder.emitBlock(doneBB);
  auto phi = IGF.Builder.CreatePHI(metadataArrayPtrTy, 3);
  phi->addIncoming(initialVector, entryBB);
  phi->addIncoming(builtVector, buildBB);
  phi->addIncoming(raceVector, raceLostBB);
  
  IGF.Builder.CreateRet(phi);
}

/*****************************************************************************/
/** Metadata Emission ********************************************************/
/*****************************************************************************/

namespace {
  /// An adapter class which turns a metadata layout class into a
  /// generic metadata layout class.
  template <class Impl, class Base>
  class GenericMetadataBuilderBase : public Base {
    typedef Base super;

    /// The number of generic witnesses in the type we're emitting.
    /// This is not really something we need to track.
    unsigned NumGenericWitnesses = 0;

    struct FillOp {
      CanType Type;
      Optional<ProtocolConformanceRef> Conformance;
      Size ToOffset;
      bool IsRelative;
    };

    SmallVector<FillOp, 8> FillOps;

    enum { TemplateHeaderFieldCount = 5 };
    enum { NumPrivateDataWords = swift::NumGenericMetadataPrivateDataWords };
    Size TemplateHeaderSize;

  protected:
    /// The offset of the address point in the type we're emitting.
    Size AddressPoint = Size::invalid();
    
    IRGenModule &IGM = super::IGM;
    using super::asImpl;
    using super::Target;
    
    /// Set to true if the metadata record for the generic type has fields
    /// outside of the generic parameter vector.
    bool HasDependentMetadata = false;
    
    /// Set to true if the value witness table for the generic type is dependent
    /// on its generic parameters. If true, the value witness will be
    /// tail-emplaced inside the metadata pattern and initialized by the fill
    /// function. Implies HasDependentMetadata.
    bool HasDependentVWT = false;
    
    /// The offset of the tail-allocated dependent VWT, if any.
    Size DependentVWTPoint = Size::invalid();

    template <class... T>
    GenericMetadataBuilderBase(IRGenModule &IGM, T &&...args)
      : super(IGM, std::forward<T>(args)...) {}

    /// Emit the create function for the template.
    llvm::Function *emitCreateFunction() {
      // Metadata *(*CreateFunction)(GenericMetadata*, const void * const *)
      llvm::Type *argTys[] = {IGM.TypeMetadataPatternPtrTy, IGM.Int8PtrPtrTy};
      auto ty = llvm::FunctionType::get(IGM.TypeMetadataPtrTy,
                                        argTys, /*isVarArg*/ false);
      llvm::Function *f = llvm::Function::Create(ty,
                                           llvm::GlobalValue::PrivateLinkage,
                                           llvm::Twine("create_generic_metadata_")
                                               + Target->getName().str(),
                                           &IGM.Module);
      f->setAttributes(IGM.constructInitialAttributes());
      
      IRGenFunction IGF(IGM, f);

      // Skip instrumentation when building for TSan to avoid false positives.
      // The synchronization for this happens in the Runtime and we do not see it.
      if (IGM.IRGen.Opts.Sanitize == SanitizerKind::Thread)
        f->removeFnAttr(llvm::Attribute::SanitizeThread);

      if (IGM.DebugInfo)
        IGM.DebugInfo->emitArtificialFunction(IGF, f);

      Explosion params = IGF.collectParameters();
      llvm::Value *metadataPattern = params.claimNext();
      llvm::Value *args = params.claimNext();

      // Bind the generic arguments.
      if (Target->isGenericContext()) {
        Address argsArray(args, IGM.getPointerAlignment());
        emitPolymorphicParametersFromArray(IGF, Target, argsArray);
      }

      // Allocate the metadata.
      llvm::Value *metadataValue =
        asImpl().emitAllocateMetadata(IGF, metadataPattern, args);
      
      // Execute the fill ops. Cast the parameters to word pointers because the
      // fill indexes are word-indexed.
      Address metadataWords(IGF.Builder.CreateBitCast(metadataValue, IGM.Int8PtrPtrTy),
                            IGM.getPointerAlignment());
      
      for (auto &fillOp : FillOps) {
        llvm::Value *value;
        if (fillOp.Conformance) {
          value = emitWitnessTableRef(IGF, fillOp.Type, *fillOp.Conformance);
        } else {
          value = IGF.emitTypeMetadataRef(fillOp.Type);
        }

        auto dest = createPointerSizedGEP(IGF, metadataWords,
                                          fillOp.ToOffset - AddressPoint);

        // A far relative indirectable pointer.
        if (fillOp.IsRelative) {
          dest = IGF.Builder.CreateElementBitCast(dest,
                                                  IGM.FarRelativeAddressTy);
          IGF.emitStoreOfRelativeIndirectablePointer(value, dest,
                                                     /*isFar*/ true);

        // A direct pointer.
        } else {
          value = IGF.Builder.CreateBitCast(value, IGM.Int8PtrTy);
          IGF.Builder.CreateStore(value, dest);
        }
      }
      
      // Initialize the instantiated dependent value witness table, if we have
      // one.
      llvm::Value *vwtableValue = nullptr;
      if (HasDependentVWT) {
        assert(!AddressPoint.isInvalid() && "did not set valid address point!");
        assert(!DependentVWTPoint.isInvalid() && "did not set dependent VWT point!");
        
        // Fill in the pointer from the metadata to the VWT. The VWT pointer
        // always immediately precedes the address point.
        auto vwtAddr = createPointerSizedGEP(IGF, metadataWords,
                                             DependentVWTPoint - AddressPoint);
        vwtableValue = IGF.Builder.CreateBitCast(vwtAddr.getAddress(),
                                                 IGF.IGM.WitnessTablePtrTy);

        auto vwtAddrVal = IGF.Builder.CreateBitCast(vwtableValue, IGM.Int8PtrTy);
        auto vwtRefAddr = createPointerSizedGEP(IGF, metadataWords,
                                                Size(0) - IGM.getPointerSize());
        IGF.Builder.CreateStore(vwtAddrVal, vwtRefAddr);
        
        HasDependentMetadata = true;
      }

      if (HasDependentMetadata) {
        asImpl().emitInitializeMetadata(IGF, metadataValue, vwtableValue);
      }
      
      // The metadata is now complete.
      IGF.Builder.CreateRet(metadataValue);
      
      return f;
    }

    void addFillOp(CanType type, Optional<ProtocolConformanceRef> conf,
                   bool isRelative) {
      FillOps.push_back({type, conf, getNextOffset(), isRelative });
    }
    
  public:
    void createMetadataAccessFunction() {
      (void) getGenericTypeMetadataAccessFunction(IGM, Target, ForDefinition);
    }

    void layout() {
      TemplateHeaderSize =
        ((NumPrivateDataWords + 1) * IGM.getPointerSize()) + Size(8);

      // Leave room for the header.
      auto header = this->reserveFields(TemplateHeaderFieldCount,
                                        TemplateHeaderSize);

      // Lay out the template data.
      super::layout();
      
      // Save a slot for the field type vector address to be instantiated into.
      asImpl().addFieldTypeVectorReferenceSlot();
      
      // If we have a dependent value witness table, emit its template.
      if (HasDependentVWT) {
        // Note the dependent VWT offset.
        DependentVWTPoint = getNextOffset();
        asImpl().addDependentValueWitnessTablePattern();
      }
      
      asImpl().addDependentData();

      // Fill in the header:
      unsigned Field = 0;
      auto headerFields =
        this->claimReservation(header, TemplateHeaderFieldCount);

      //   Metadata *(*CreateFunction)(GenericMetadata *, const void*);
      headerFields[Field++] = emitCreateFunction();
      
      //   uint32_t MetadataSize;
      // We compute this assuming that every entry in the metadata table
      // is a pointer in size.
      Size size = getNextOffset();
      headerFields[Field++] =
        llvm::ConstantInt::get(IGM.Int32Ty, size.getValue());
      
      //   uint16_t NumArguments;
      // TODO: ultimately, this should be the number of actual template
      // arguments, not the number of witness tables required.
      unsigned numGenericArguments =
        GenericArguments::getNumGenericArguments(IGM, Target);
      headerFields[Field++]
        = llvm::ConstantInt::get(IGM.Int16Ty, numGenericArguments);

      //   uint16_t AddressPoint;
      assert(!AddressPoint.isInvalid() && "address point not noted!");
      headerFields[Field++]
        = llvm::ConstantInt::get(IGM.Int16Ty, AddressPoint.getValue());

      //   void *PrivateData[NumPrivateDataWords];
      headerFields[Field++] = getPrivateDataInit();

      assert(TemplateHeaderFieldCount == Field);
    }

    /// Write down the index of the address point.
    void noteAddressPoint() {
      AddressPoint = getNextOffset();
      super::noteAddressPoint();
    }

    /// Ignore the preallocated header.
    Size getNextOffset() const {
      // Note that the header fields are all pointer-sized.
      return super::getNextOffset() - TemplateHeaderSize;
    }

    template <class... T>
    void addGenericArgument(CanType type, T &&...args) {
      NumGenericWitnesses++;
      addFillOp(type, None, /*relative*/ false);
      super::addGenericArgument(type, std::forward<T>(args)...);
    }

    template <class... T>
    void addGenericWitnessTable(CanType type, ProtocolConformanceRef conf,
                                T &&...args) {
      NumGenericWitnesses++;
      addFillOp(type, conf, /*relative*/ false);
      super::addGenericWitnessTable(type, conf, std::forward<T>(args)...);
    }
    
    void addFieldTypeVectorReferenceSlot() {
      this->addWord(
         llvm::ConstantPointerNull::get(IGM.TypeMetadataPtrTy->getPointerTo()));
    }
    
    // Can be overridden by subclassers to emit other dependent metadata.
    void addDependentData() {}
    
  private:
    static llvm::Constant *makeArray(llvm::Type *eltTy,
                                     ArrayRef<llvm::Constant*> elts) {
      auto arrayTy = llvm::ArrayType::get(eltTy, elts.size());
      return llvm::ConstantArray::get(arrayTy, elts);
    }

    /// Produce the initializer for the private-data field of the
    /// template header.
    llvm::Constant *getPrivateDataInit() {
      auto null = llvm::ConstantPointerNull::get(IGM.Int8PtrTy);
      
      llvm::Constant *privateData[NumPrivateDataWords];
      
      for (auto &element : privateData)
        element = null;

      return makeArray(IGM.Int8PtrTy, privateData);
    }
  };
} // end anonymous namespace

// Classes

static Address
emitAddressOfFieldOffsetVectorInClassMetadata(IRGenFunction &IGF,
                                              ClassDecl *theClass,
                                              llvm::Value *metadata) {
  BEGIN_METADATA_SEARCHER_0(GetOffsetToFieldOffsetVector, Class)
    void noteStartOfFieldOffsets(ClassDecl *whichClass) {
      if (whichClass == Target)
        setTargetOffset();
    }
  END_METADATA_SEARCHER()

  auto offset =
    GetOffsetToFieldOffsetVector(IGF.IGM, theClass).getTargetOffset();
  
  Address addr(metadata, IGF.IGM.getPointerAlignment());
  addr = IGF.Builder.CreateBitCast(addr,
                                   IGF.IGM.SizeTy->getPointerTo());
  return createPointerSizedGEP(IGF, addr, offset);
}

static llvm::Value *emitInitializeFieldOffsetVector(IRGenFunction &IGF,
                                                    ClassDecl *target,
                                                    llvm::Value *metadata) {
  llvm::Value *fieldVector
    = emitAddressOfFieldOffsetVectorInClassMetadata(IGF, target, metadata)
        .getAddress();
  
  // Collect the stored properties of the type.
  llvm::SmallVector<VarDecl*, 4> storedProperties;
  for (auto prop : target->getStoredProperties()) {
    storedProperties.push_back(prop);
  }

  // Fill out an array with the field type metadata records.
  Address fields = IGF.createAlloca(
                   llvm::ArrayType::get(IGF.IGM.SizeTy,
                                        storedProperties.size() * 2),
                   IGF.IGM.getPointerAlignment(), "classFields");
  IGF.Builder.CreateLifetimeStart(fields,
                  IGF.IGM.getPointerSize() * storedProperties.size() * 2);
  
  Address firstField;
  unsigned index = 0;
  for (auto prop : storedProperties) {
    auto propFormalTy = target->mapTypeIntoContext(prop->getInterfaceType())
                            ->getCanonicalType();
    SILType propLoweredTy = IGF.IGM.getLoweredType(propFormalTy);
    auto &propTI = IGF.getTypeInfo(propLoweredTy);
    auto sizeAndAlignMask
      = propTI.getSizeAndAlignmentMask(IGF, propLoweredTy);

    llvm::Value *size = sizeAndAlignMask.first;
    Address sizeAddr =
      IGF.Builder.CreateStructGEP(fields, index, IGF.IGM.getPointerSize());
    IGF.Builder.CreateStore(size, sizeAddr);
    if (index == 0) firstField = sizeAddr;

    llvm::Value *alignMask = sizeAndAlignMask.second;
    Address alignMaskAddr =
      IGF.Builder.CreateStructGEP(fields, index + 1,
                                  IGF.IGM.getPointerSize());
    IGF.Builder.CreateStore(alignMask, alignMaskAddr);

    index += 2;
  }

  if (storedProperties.empty()) {
    firstField = IGF.Builder.CreateStructGEP(fields, 0, Size(0));
  }

  // Ask the runtime to lay out the class.  This can relocate it if it
  // wasn't allocated with swift_allocateGenericClassMetadata.
  auto numFields = IGF.IGM.getSize(Size(storedProperties.size()));
  metadata = IGF.Builder.CreateCall(IGF.IGM.getInitClassMetadataUniversalFn(),
                                    {metadata, numFields,
                                     firstField.getAddress(), fieldVector});
  IGF.Builder.CreateLifetimeEnd(fields,
                  IGF.IGM.getPointerSize() * storedProperties.size() * 2);

  return metadata;
}

namespace {
  /// An adapter for laying out class metadata.
  template <class Impl>
  class ClassMetadataBuilderBase
         : public ConstantBuilder<ClassMetadataLayout<Impl>> {
    using super = ConstantBuilder<ClassMetadataLayout<Impl>>;

    Optional<MetadataSize> ClassObjectExtents;

  protected:
    using super::IGM;
    using super::Target;
    using super::setRelativeAddressBase;
    using super::addWord;
    using super::addConstantWord;
    using super::addInt16;
    using super::addConstantInt16;
    using super::addInt32;
    using super::addConstantInt32;
    using super::addStruct;
    using super::getNextOffset;
    using super::addFarRelativeAddress;
    using super::addFarRelativeAddressOrNull;
    const StructLayout &Layout;
    const ClassLayout &FieldLayout;
    SILVTable *VTable;

    ClassMetadataBuilderBase(IRGenModule &IGM, ClassDecl *theClass,
                             const StructLayout &layout,
                             const ClassLayout &fieldLayout,
                             llvm::GlobalVariable *relativeAddressBase)
      : super(IGM, theClass), Layout(layout), FieldLayout(fieldLayout) {
      setRelativeAddressBase(relativeAddressBase);
      VTable = IGM.getSILModule().lookUpVTable(Target);
    }

    void computeClassObjectExtents() {
      if (ClassObjectExtents.hasValue()) return;
      ClassObjectExtents = getSizeOfMetadata(IGM, Target);
    }

  public:
    /// The 'metadata flags' field in a class is actually a pointer to
    /// the metaclass object for the class.
    ///
    /// NONAPPLE: This is only really required for ObjC interop; maybe
    /// suppress this for classes that don't need to be exposed to
    /// ObjC, e.g. for non-Apple platforms?
    void addMetadataFlags() {
      static_assert(unsigned(MetadataKind::Class) == 0,
                    "class metadata kind is non-zero?");

      if (IGM.ObjCInterop) {
        // Get the metaclass pointer as an intptr_t.
        auto metaclass = IGM.getAddrOfMetaclassObject(Target,
                                                      NotForDefinition);
        auto flags = llvm::ConstantExpr::getPtrToInt(metaclass, IGM.IntPtrTy);
        addWord(flags);
      } else {
        // On non-objc platforms just fill it with a null, there
        // is no Objective-C metaclass.
        // FIXME: Remove this to save metadata space.
        // rdar://problem/18801263
        addWord(llvm::ConstantExpr::getNullValue(IGM.IntPtrTy));
      }
    }

    /// The runtime provides a value witness table for Builtin.NativeObject.
    void addValueWitnessTable() {
      ClassDecl *cls = Target;
      
      auto type = (cls->checkObjCAncestry() != ObjCClassKind::NonObjC
                   ? this->IGM.Context.TheUnknownObjectType
                   : this->IGM.Context.TheNativeObjectType);
      auto wtable = this->IGM.getAddrOfValueWitnessTable(type);
      addWord(wtable);
    }

    void addDestructorFunction() {
      auto expansion = ResilienceExpansion::Minimal;
      auto dtorRef = SILDeclRef(Target->getDestructor(),
                                SILDeclRef::Kind::Deallocator,
                                expansion);
      SILFunction *dtorFunc = IGM.getSILModule().lookUpFunction(dtorRef);
      if (dtorFunc) {
        addWord(IGM.getAddrOfSILFunction(dtorFunc, NotForDefinition));
      } else {
        // In case the optimizer removed the function. See comment in
        // addMethod().
        addWord(llvm::ConstantPointerNull::get(IGM.FunctionPtrTy));
      }
    }
    
    void addNominalTypeDescriptor() {
      auto descriptor =
        ClassNominalTypeDescriptorBuilder(IGM, Target).emit();
      addFarRelativeAddress(descriptor);
    }
    
    void addIVarDestroyer() {
      auto dtorFunc = IGM.getAddrOfIVarInitDestroy(Target,
                                                   /*isDestroyer=*/ true,
                                                   /*isForeign=*/ false,
                                                   NotForDefinition);
      if (dtorFunc) {
        addWord(*dtorFunc);
      } else {
        addWord(llvm::ConstantPointerNull::get(IGM.FunctionPtrTy));
      }
    }

    bool addReferenceToHeapMetadata(CanType type, bool allowUninitialized) {
      if (llvm::Constant *metadata
            = tryEmitConstantHeapMetadataRef(IGM, type, allowUninitialized)) {
        addWord(metadata);
        return true;
      } else {
        // Leave a null pointer placeholder to be filled at runtime
        addWord(llvm::ConstantPointerNull::get(IGM.TypeMetadataPtrTy));
        return false;
      }
    }

    void addClassFlags() {
      // Always set a flag saying that this is a Swift 1.0 class.
      ClassFlags flags = ClassFlags::IsSwift1;

      // Set a flag if the class uses Swift 1.0 refcounting.
      if (getReferenceCountingForClass(IGM, Target)
            == ReferenceCounting::Native) {
        flags |= ClassFlags::UsesSwift1Refcounting;
      }

      addConstantInt32((uint32_t) flags);
    }

    void addInstanceAddressPoint() {
      // Right now, we never allocate fields before the address point.
      addConstantInt32(0);
    }

    void addInstanceSize() {
      if (llvm::Constant *size
            = tryEmitClassConstantFragileInstanceSize(IGM, Target)) {
        // We only support a maximum 32-bit instance size.
        if (IGM.SizeTy != IGM.Int32Ty)
          size = llvm::ConstantExpr::getTrunc(size, IGM.Int32Ty);
        addInt32(size);
      } else {
        // Leave a zero placeholder to be filled at runtime
        addConstantInt32(0);
      }
    }
    
    void addInstanceAlignMask() {
      if (llvm::Constant *align
            = tryEmitClassConstantFragileInstanceAlignMask(IGM, Target)) {
        if (IGM.SizeTy != IGM.Int16Ty)
          align = llvm::ConstantExpr::getTrunc(align, IGM.Int16Ty);
        addInt16(align);
      } else {
        // Leave a zero placeholder to be filled at runtime
        addConstantInt16(0);
      }
    }

    void addRuntimeReservedBits() {
      addConstantInt16(0);
    }

    void addClassSize() {
      computeClassObjectExtents();
      addConstantInt32(ClassObjectExtents->FullSize.getValue());
    }

    void addClassAddressPoint() {
      computeClassObjectExtents();
      addConstantInt32(ClassObjectExtents->AddressPoint.getValue());
    }
    
    void addClassCacheData() {
      // We initially fill in these fields with addresses taken from
      // the ObjC runtime.
      // FIXME: Remove null data altogether rdar://problem/18801263
      addWord(IGM.getObjCEmptyCachePtr());
      addWord(IGM.getObjCEmptyVTablePtr());
    }

    void addClassDataPointer() {
      if (!IGM.ObjCInterop) {
        // with no Objective-C runtime, just give an empty pointer with the
        // swift bit set.
        addWord(llvm::ConstantInt::get(IGM.IntPtrTy, 1));
        return;
      }
      // Derive the RO-data.
      llvm::Constant *data = emitClassPrivateData(IGM, Target);

      // We always set the low bit to indicate this is a Swift class.
      data = llvm::ConstantExpr::getPtrToInt(data, IGM.IntPtrTy);
      data = llvm::ConstantExpr::getAdd(data,
                                    llvm::ConstantInt::get(IGM.IntPtrTy, 1));

      addWord(data);
    }

    void addFieldOffset(VarDecl *var) {
      assert(var->hasStorage());
      
      unsigned fieldIndex = FieldLayout.getFieldIndex(var);
      llvm::Constant *fieldOffsetOrZero;
      auto &element = Layout.getElement(fieldIndex);

      if (element.getKind() == ElementLayout::Kind::Fixed) {
        // Use a fixed offset if we have one.
        fieldOffsetOrZero = IGM.getSize(element.getByteOffset());
      } else {
        // Otherwise, leave a placeholder for the runtime to populate at runtime.
        fieldOffsetOrZero = llvm::ConstantInt::get(IGM.IntPtrTy, 0);
      }
      addWord(fieldOffsetOrZero);

      if (var->getDeclContext() == Target) {
        auto access = FieldLayout.AllFieldAccesses[fieldIndex];
        switch (access) {
        case FieldAccess::ConstantDirect:
        case FieldAccess::NonConstantDirect: {
          // Emit a global variable storing the constant field offset.
          // If the superclass was imported from Objective-C, the offset
          // does not include the superclass size; we rely on the
          // Objective-C runtime sliding it down.
          //
          // TODO: Don't emit the symbol if field has a fixed offset and size
          // in all resilience domains
          auto offsetAddr = IGM.getAddrOfFieldOffset(var, /*indirect*/ false,
                                                     ForDefinition);
          auto offsetVar = cast<llvm::GlobalVariable>(offsetAddr.getAddress());
          offsetVar->setInitializer(fieldOffsetOrZero);

          // If we know the offset won't change, make it a constant.
          offsetVar->setConstant(access == FieldAccess::ConstantDirect);

          break;
        }

        case FieldAccess::ConstantIndirect:
          // No global variable is needed.
          break;

        case FieldAccess::NonConstantIndirect:
          // Emit a global variable storing an offset into the field offset
          // vector within the class metadata. This access pattern is used
          // when the field offset depends on generic parameters. As above,
          // the Objective-C runtime will slide the field offsets within the
          // class metadata to adjust for the superclass size.
          //
          // TODO: This isn't plumbed through all the way yet.
          auto offsetAddr = IGM.getAddrOfFieldOffset(var, /*indirect*/ true,
                                                     ForDefinition);
          auto offsetVar = cast<llvm::GlobalVariable>(offsetAddr.getAddress());
          offsetVar->setConstant(false);
          auto offset = getClassFieldOffset(IGM, Target, var).getValue();
          auto offsetVal = llvm::ConstantInt::get(IGM.IntPtrTy, offset);
          offsetVar->setInitializer(offsetVal);

          break;
        }
      }
    }

    void addMethod(SILDeclRef fn) {
      // If this function is associated with the target class, go
      // ahead and emit the witness offset variable.
      if (fn.getDecl()->getDeclContext() == Target) {
        Address offsetVar = IGM.getAddrOfWitnessTableOffset(fn, ForDefinition);
        auto global = cast<llvm::GlobalVariable>(offsetVar.getAddress());

        auto offset = getNextOffset();
        auto offsetV = llvm::ConstantInt::get(IGM.SizeTy, offset.getValue());
        global->setInitializer(offsetV);
      }

      // Find the vtable entry.
      assert(VTable && "no vtable?!");
      if (SILFunction *func =
            VTable->getImplementation(IGM.getSILModule(), fn)) {
        addWord(IGM.getAddrOfSILFunction(func, NotForDefinition));
      } else {
        // The method is removed by dead method elimination.
        // It should be never called. We add a pointer to an error function.
        addWord(llvm::ConstantExpr::getBitCast(IGM.getDeletedMethodErrorFn(),
                                               IGM.FunctionPtrTy));
      }
    }

    void addGenericArgument(CanType argTy, ClassDecl *forClass) {
      addWord(llvm::Constant::getNullValue(IGM.TypeMetadataPtrTy));
    }

    void addGenericWitnessTable(CanType argTy, ProtocolConformanceRef conf,
                                ClassDecl *forClass) {
      addWord(llvm::Constant::getNullValue(IGM.WitnessTablePtrTy));
    }

  protected:
    bool isFinishInitializationIdempotent() {
      if (!Layout.isFixedLayout())
        return false;

      if (doesClassMetadataRequireDynamicInitialization(IGM, Target))
        return false;

      return true;
    }

    llvm::Value *emitFinishIdempotentInitialization(IRGenFunction &IGF,
                                                    llvm::Value *metadata) {
      if (IGF.IGM.ObjCInterop) {
        metadata =
          IGF.Builder.CreateBitCast(metadata, IGF.IGM.ObjCClassPtrTy);
        metadata =
          IGF.Builder.CreateCall(IGF.IGM.getGetInitializedObjCClassFn(),
                                 metadata);
        metadata =
           IGF.Builder.CreateBitCast(metadata, IGF.IGM.TypeMetadataPtrTy);
      }
      return metadata;
    }

    llvm::Value *emitFinishInitializationOfClassMetadata(IRGenFunction &IGF,
                                                      llvm::Value *metadata) {
      // We assume that we've already filled in the class's generic arguments.
      // We need to:
      //   - relocate the metadata to accommodate the superclass,
      //     if something in our hierarchy is resilient to us;
      //   - fill out the subclass's field offset vector, if its layout
      //     wasn't fixed;
      //   - copy field offsets and generic arguments from higher in the
      //     class hierarchy, if 
      //   - copy the superclass data, if there are generic arguments
      //     or field offset vectors there that weren't filled in;
      //   - populate the field offset vector, if layout isn't fixed, and
      //   - register the class with the ObjC runtime, if ObjC interop is
      //     enabled.
      //
      // emitInitializeFieldOffsetVector will do everything in the full case.
      if (doesClassMetadataRequireDynamicInitialization(IGF.IGM, Target)) {
        metadata = emitInitializeFieldOffsetVector(IGF, Target, metadata);

      // TODO: do something intermediate when e.g. all we needed to do was
      // set parent metadata pointers.

      // Otherwise, all we need to do is register with the ObjC runtime.
      } else {
        metadata = emitFinishIdempotentInitialization(IGF, metadata);
      }

      // Realizing the class with the ObjC runtime will copy back to the
      // field offset globals for us; but if ObjC interop is disabled, we
      // have to do that ourselves, assuming we didn't just emit them all
      // correctly in the first place.
      if (!Layout.isFixedLayout() && !IGF.IGM.ObjCInterop)
        emitInitializeFieldOffsets(IGF, metadata);

      return metadata;
    }

    // The Objective-C runtime will copy field offsets from the field offset
    // vector into field offset globals for us, if present. If there's no
    // Objective-C runtime, we have to do this ourselves.
    void emitInitializeFieldOffsets(IRGenFunction &IGF,
                                    llvm::Value *metadata) {
      unsigned index = FieldLayout.InheritedStoredProperties.size();

      for (auto prop : Target->getStoredProperties()) {
        auto access = FieldLayout.AllFieldAccesses[index];
        if (access == FieldAccess::NonConstantDirect) {
          Address offsetA = IGF.IGM.getAddrOfFieldOffset(prop,
                                                         /*indirect*/ false,
                                                         ForDefinition);

          // We can't use emitClassFieldOffset() here because that creates
          // an invariant load, which could be hoisted above the point
          // where the metadata becomes fully initialized
          Size offset = getClassFieldOffset(IGF.IGM, Target, prop);
          int index = IGF.IGM.getOffsetInWords(offset);
          auto offsetVal = emitLoadFromMetadataAtIndex(IGF, metadata, index,
                                                       IGF.IGM.SizeTy);
          IGF.Builder.CreateStore(offsetVal, offsetA);
        }

        index++;
      }
    }
  };

  class ClassMetadataBuilder :
    public ClassMetadataBuilderBase<ClassMetadataBuilder> {

    bool HasUnfilledSuperclass = false;
    bool HasUnfilledParent = false;
  public:
    ClassMetadataBuilder(IRGenModule &IGM, ClassDecl *theClass,
                         const StructLayout &layout,
                         const ClassLayout &fieldLayout,
                         llvm::GlobalVariable *relativeAddressBase)
      : ClassMetadataBuilderBase(IGM, theClass, layout, fieldLayout,
                                 relativeAddressBase) {
    }

    llvm::Constant *getInit() {
      return getInitWithSuggestedType(NumHeapMetadataFields,
                                      IGM.FullHeapMetadataStructTy);
    }

    void addSuperClass() {
      // If this is a root class, use SwiftObject as our formal parent.
      if (!Target->hasSuperclass()) {
        // This is only required for ObjC interoperation.
        if (!IGM.ObjCInterop) {
          addWord(llvm::ConstantPointerNull::get(IGM.TypeMetadataPtrTy));
          return;
        }

        // We have to do getAddrOfObjCClass ourselves here because
        // the ObjC runtime base needs to be ObjC-mangled but isn't
        // actually imported from a clang module.
        addWord(IGM.getAddrOfObjCClass(
                               IGM.getObjCRuntimeBaseForSwiftRootClass(Target),
                               NotForDefinition));
        return;
      }

      Type superclassTy = Target->mapTypeIntoContext(Target->getSuperclass());

      if (!addReferenceToHeapMetadata(superclassTy->getCanonicalType(),
                                      /*allowUninit*/ false)) {
        HasUnfilledSuperclass = true;
      }
    }

    void addParentMetadataRef(ClassDecl *forClass, Type classType) {
      CanType parentType = classType->getCanonicalType().getNominalParent();

      if (auto metadata =
            tryEmitConstantTypeMetadataRef(IGM, parentType,
                                           SymbolReferenceKind::Absolute)) {
        addWord(metadata.getValue());
      } else {
        // Leave a null pointer placeholder to be filled by in-place
        // initialization.
        addWord(llvm::ConstantPointerNull::get(IGM.TypeMetadataPtrTy));
        if (forClass == Target)
          HasUnfilledParent = true;
      }
    }

    bool canBeConstant() {
      // TODO: the metadata global can actually be constant in a very
      // special case: it's not a pattern, ObjC interoperation isn't
      // required, there are no class fields, and there is nothing that
      // needs to be runtime-adjusted.
      return false;
    }

    void createMetadataAccessFunction() {
      assert(!Target->isGenericContext());
      auto type =cast<ClassType>(Target->getDeclaredType()->getCanonicalType());

      (void) getTypeMetadataAccessFunction(IGM, type, ForDefinition,
      [&](IRGenFunction &IGF, llvm::Constant *cacheVar) -> llvm::Value* {
        // There's an interesting special case where we can do the
        // initialization idempotently and thus avoid the need for a lock.
        if (!HasUnfilledSuperclass && !HasUnfilledParent &&
            isFinishInitializationIdempotent()) {
          auto type = Target->getDeclaredType()->getCanonicalType();
          auto metadata =
            IGF.IGM.getAddrOfTypeMetadata(type, /*pattern*/ false);
          return emitFinishIdempotentInitialization(IGF, metadata);
        }

        // Otherwise, use the generic path.
        return emitInPlaceTypeMetadataAccessFunctionBody(IGF, type, cacheVar,
          [&](IRGenFunction &IGF, llvm::Value *metadata) {
            return emitInPlaceMetadataInitialization(IGF, type, metadata);
          });
      });
    }

  private:
    llvm::Value *emitInPlaceMetadataInitialization(IRGenFunction &IGF,
                                                   CanClassType type,
                                                   llvm::Value *metadata) {
      // Many of the things done by generic instantiation are unnecessary here:
      //   initializing the metaclass pointer
      //   initializing the ro-data pointer

      // Initialize the superclass if we didn't do so as a constant.
      if (HasUnfilledSuperclass) {
        auto superclass = type->getSuperclass(nullptr)->getCanonicalType();
        llvm::Value *superclassMetadata =
          emitClassHeapMetadataRef(IGF, superclass,
                                   MetadataValueType::TypeMetadata,
                                   /*allowUninit*/ false);
        Address superField =
          emitAddressOfSuperclassRefInClassMetadata(IGF, metadata);
        superField = IGF.Builder.CreateElementBitCast(superField,
                                                     IGF.IGM.TypeMetadataPtrTy);
        IGF.Builder.CreateStore(superclassMetadata, superField);
      }

      // Initialize the class's own parent pointer if it has one and it
      // wasn't emitted as a constant.
      if (HasUnfilledParent) {
        auto parentType = type.getParent();
        assert(parentType);
        llvm::Value *parentMetadata = IGF.emitTypeMetadataRef(parentType);

        int index = getClassParentIndex(IGF.IGM, Target);
        Address slot = emitAddressOfMetadataSlotAtIndex(IGF, metadata, index,
                                                    IGF.IGM.TypeMetadataPtrTy);
        IGF.Builder.CreateStore(parentMetadata, slot);
      }

      metadata = emitFinishInitializationOfClassMetadata(IGF, metadata);

      return metadata;
    }
  };
  
  /// A builder for metadata templates.
  class GenericClassMetadataBuilder :
    public GenericMetadataBuilderBase<GenericClassMetadataBuilder,
                      ClassMetadataBuilderBase<GenericClassMetadataBuilder>>
  {
    typedef GenericMetadataBuilderBase super;

    Size MetaclassPtrOffset = Size::invalid();
    Size ClassRODataPtrOffset = Size::invalid();
    Size MetaclassRODataPtrOffset = Size::invalid();
    Size DependentMetaclassPoint = Size::invalid();
    Size DependentClassRODataPoint = Size::invalid();
    Size DependentMetaclassRODataPoint = Size::invalid();
  public:
    GenericClassMetadataBuilder(IRGenModule &IGM, ClassDecl *theClass,
                                const StructLayout &layout,
                                const ClassLayout &fieldLayout,
                                llvm::GlobalVariable *relativeAddressBase)
      : super(IGM, theClass, layout, fieldLayout, relativeAddressBase)
    {
      // We need special initialization of metadata objects to trick the ObjC
      // runtime into initializing them.
      HasDependentMetadata = true;
    }

    void addParentMetadataRef(ClassDecl *forClass, Type classType) {
      CanType parentType = classType->getCanonicalType().getNominalParent();
      this->addFillOp(parentType, None, /*relative*/ false);
      addWord(llvm::ConstantPointerNull::get(IGM.TypeMetadataPtrTy));
    }

    void addSuperClass() {
      // Filled in by the runtime.
      addWord(llvm::ConstantPointerNull::get(IGM.TypeMetadataPtrTy));
    }

    llvm::Value *emitAllocateMetadata(IRGenFunction &IGF,
                                      llvm::Value *metadataPattern,
                                      llvm::Value *arguments) {
      llvm::Value *superMetadata;
      if (Target->hasSuperclass()) {
        Type superclass = Target->getSuperclass();
        superclass = Target->mapTypeIntoContext(superclass);
        superMetadata =
          emitClassHeapMetadataRef(IGF, superclass->getCanonicalType(),
                                   MetadataValueType::ObjCClass);
      } else if (IGM.ObjCInterop) {
        superMetadata = emitObjCHeapMetadataRef(IGF,
                               IGM.getObjCRuntimeBaseForSwiftRootClass(Target));
      } else {
        superMetadata
          = llvm::ConstantPointerNull::get(IGF.IGM.ObjCClassPtrTy);
      }

      return IGF.Builder.CreateCall(IGM.getAllocateGenericClassMetadataFn(),
                                    {metadataPattern, arguments, superMetadata});
    }
    
    void addMetadataFlags() {
      // The metaclass pointer will be instantiated here.
      MetaclassPtrOffset = getNextOffset();
      addWord(llvm::ConstantInt::get(IGM.IntPtrTy, 0));
    }
    
    void addClassDataPointer() {
      // The rodata pointer will be instantiated here.
      // Make sure we at least set the 'is Swift class' bit, though.
      ClassRODataPtrOffset = getNextOffset();
      addWord(llvm::ConstantInt::get(IGM.IntPtrTy, 1));
    }
    
    void addDependentData() {
      if (!IGM.ObjCInterop) {
        // Every piece of data in the dependent data appears to be related to
        // Objective-C information. If we're not doing Objective-C interop, we
        // can just skip adding it to the class.
        return;
      }
      // Emit space for the dependent metaclass.
      DependentMetaclassPoint = getNextOffset();
      // isa
      ClassDecl *rootClass = getRootClassForMetaclass(IGM, Target);
      auto isa = IGM.getAddrOfMetaclassObject(rootClass, NotForDefinition);
      addWord(isa);
      // super, which is dependent if the superclass is generic
      addWord(llvm::ConstantPointerNull::get(IGM.ObjCClassPtrTy));
      // cache
      addWord(IGM.getObjCEmptyCachePtr());
      // vtable
      addWord(IGM.getObjCEmptyVTablePtr());
      // rodata, which is always dependent
      MetaclassRODataPtrOffset = getNextOffset();
      addConstantWord(0);
      
      // Emit the dependent rodata.
      llvm::Constant *classData, *metaclassData;
      Size rodataSize;
      std::tie(classData, metaclassData, rodataSize)
        = emitClassPrivateDataFields(IGM, Target);
      
      DependentClassRODataPoint = getNextOffset();
      addStruct(classData, rodataSize);
      DependentMetaclassRODataPoint = getNextOffset();
      addStruct(metaclassData, rodataSize);
    }
                            
    void addDependentValueWitnessTablePattern() {
      llvm_unreachable("classes should never have dependent vwtables");
    }

    void noteStartOfFieldOffsets(ClassDecl *whichClass) {
      HasDependentMetadata = true;
    }
    
    void noteEndOfFieldOffsets(ClassDecl *whichClass) {}
    
    // Suppress GenericMetadataBuilderBase's default behavior of introducing
    // fill ops for generic arguments unless they belong directly to the target
    // class and not its ancestors.

    void addGenericArgument(CanType type, ClassDecl *forClass) {
      if (forClass == Target) {
        // Introduce the fill op.
        GenericMetadataBuilderBase::addGenericArgument(type, forClass);
      } else {
        // Lay out the field, but don't fill it in, we will copy it from
        // the superclass.
        HasDependentMetadata = true;
        ClassMetadataBuilderBase::addGenericArgument(type, forClass);
      }
    }
    
    void addGenericWitnessTable(CanType type, ProtocolConformanceRef conf,
                                ClassDecl *forClass) {
      if (forClass == Target) {
        // Introduce the fill op.
        GenericMetadataBuilderBase::addGenericWitnessTable(type, conf,forClass);
      } else {
        // Lay out the field, but don't provide the fill op, which we'll get
        // from the superclass.
        HasDependentMetadata = true;
        ClassMetadataBuilderBase::addGenericWitnessTable(type, conf, forClass);
      }
    }

    void emitInitializeMetadata(IRGenFunction &IGF,
                                llvm::Value *metadata,
                                llvm::Value *vwtable) {
      assert(!HasDependentVWT && "class should never have dependent VWT");

      // Fill in the metaclass pointer.
      Address metadataPtr(IGF.Builder.CreateBitCast(metadata, IGF.IGM.Int8PtrPtrTy),
                          IGF.IGM.getPointerAlignment());
      
      llvm::Value *metaclass;
      if (IGF.IGM.ObjCInterop) {
        assert(!DependentMetaclassPoint.isInvalid());
        assert(!MetaclassPtrOffset.isInvalid());

        Address metaclassPtrSlot = createPointerSizedGEP(IGF, metadataPtr,
                                            MetaclassPtrOffset - AddressPoint);
        metaclassPtrSlot = IGF.Builder.CreateBitCast(metaclassPtrSlot,
                                        IGF.IGM.ObjCClassPtrTy->getPointerTo());
        Address metaclassRawPtr = createPointerSizedGEP(IGF, metadataPtr,
                                        DependentMetaclassPoint - AddressPoint);
        metaclass = IGF.Builder.CreateBitCast(metaclassRawPtr,
                                              IGF.IGM.ObjCClassPtrTy)
          .getAddress();
        IGF.Builder.CreateStore(metaclass, metaclassPtrSlot);
      } else {
        // FIXME: Remove altogether rather than injecting a NULL value.
        // rdar://problem/18801263
        assert(!MetaclassPtrOffset.isInvalid());
        Address metaclassPtrSlot = createPointerSizedGEP(IGF, metadataPtr,
                                            MetaclassPtrOffset - AddressPoint);
        metaclassPtrSlot = IGF.Builder.CreateBitCast(metaclassPtrSlot,
                                        IGF.IGM.ObjCClassPtrTy->getPointerTo());
        IGF.Builder.CreateStore(
          llvm::ConstantPointerNull::get(IGF.IGM.ObjCClassPtrTy), 
          metaclassPtrSlot);
      }
      
      // Fill in the rodata reference in the class.
      Address classRODataPtr;
      if (IGF.IGM.ObjCInterop) {
        assert(!DependentClassRODataPoint.isInvalid());
        assert(!ClassRODataPtrOffset.isInvalid());
        Address rodataPtrSlot = createPointerSizedGEP(IGF, metadataPtr,
                                           ClassRODataPtrOffset - AddressPoint);
        rodataPtrSlot = IGF.Builder.CreateBitCast(rodataPtrSlot,
                                              IGF.IGM.IntPtrTy->getPointerTo());
        
        classRODataPtr = createPointerSizedGEP(IGF, metadataPtr,
                                      DependentClassRODataPoint - AddressPoint);
        // Set the low bit of the value to indicate "compiled by Swift".
        llvm::Value *rodata = IGF.Builder.CreatePtrToInt(
                                classRODataPtr.getAddress(), IGF.IGM.IntPtrTy);
        rodata = IGF.Builder.CreateOr(rodata, 1);
        IGF.Builder.CreateStore(rodata, rodataPtrSlot);
      } else {
        // NOTE: Unlike other bits of the metadata that should later be removed,
        // this one is important because things check this value's flags to
        // determine what kind of object it is. That said, if those checks
        // are determined to be removable, we can remove this as well per
        // rdar://problem/18801263
        assert(!ClassRODataPtrOffset.isInvalid());
        Address rodataPtrSlot = createPointerSizedGEP(IGF, metadataPtr,
                                           ClassRODataPtrOffset - AddressPoint);
        rodataPtrSlot = IGF.Builder.CreateBitCast(rodataPtrSlot,
                                              IGF.IGM.IntPtrTy->getPointerTo());
        
        IGF.Builder.CreateStore(llvm::ConstantInt::get(IGF.IGM.IntPtrTy, 1), 
                                                                rodataPtrSlot);
      }

      // Fill in the rodata reference in the metaclass.
      Address metaclassRODataPtr;
      if (IGF.IGM.ObjCInterop) {
        assert(!DependentMetaclassRODataPoint.isInvalid());
        assert(!MetaclassRODataPtrOffset.isInvalid());
        Address rodataPtrSlot = createPointerSizedGEP(IGF, metadataPtr,
                                      MetaclassRODataPtrOffset - AddressPoint);
        rodataPtrSlot = IGF.Builder.CreateBitCast(rodataPtrSlot,
                                              IGF.IGM.IntPtrTy->getPointerTo());
        
        metaclassRODataPtr = createPointerSizedGEP(IGF, metadataPtr,
                                 DependentMetaclassRODataPoint - AddressPoint);
        llvm::Value *rodata = IGF.Builder.CreatePtrToInt(
                            metaclassRODataPtr.getAddress(), IGF.IGM.IntPtrTy);
        IGF.Builder.CreateStore(rodata, rodataPtrSlot);
      }

      // We can assume that this never relocates the metadata because
      // it should have been allocated properly for the class.
      (void) emitFinishInitializationOfClassMetadata(IGF, metadata);
    }
  };
} // end anonymous namespace

/// Emit the ObjC-compatible class symbol for a class.
/// Since LLVM and many system linkers do not have a notion of relative symbol
/// references, we emit the symbol as a global asm block.
static void emitObjCClassSymbol(IRGenModule &IGM,
                                ClassDecl *classDecl,
                                llvm::GlobalValue *metadata) {
  llvm::SmallString<32> classSymbol;
  LinkEntity::forObjCClass(classDecl).mangle(classSymbol);
  
  // Create the alias.
  auto *metadataTy = cast<llvm::PointerType>(metadata->getType());

  // Create the alias.
  auto *alias = llvm::GlobalAlias::create(metadataTy->getElementType(),
                                          metadataTy->getAddressSpace(),
                                          metadata->getLinkage(),
                                          classSymbol.str(), metadata,
                                          IGM.getModule());
  if (IGM.useDllStorage())
    alias->setDLLStorageClass(metadata->getDLLStorageClass());
}

/// Emit the type metadata or metadata template for a class.
void irgen::emitClassMetadata(IRGenModule &IGM, ClassDecl *classDecl,
                              const StructLayout &layout,
                              const ClassLayout &fieldLayout) {
  assert(!classDecl->isForeign());

  // Set up a dummy global to stand in for the metadata object while we produce
  // relative references.
  auto tempBase = createTemporaryRelativeAddressBase(IGM);

  // TODO: classes nested within generic types
  llvm::Constant *init;
  bool isPattern;
  bool canBeConstant;
  if (classDecl->isGenericContext()) {
    GenericClassMetadataBuilder builder(IGM, classDecl, layout, fieldLayout,
                                        tempBase.get());
    builder.layout();
    init = builder.getInit();
    isPattern = true;
    canBeConstant = false;

    maybeEmitNominalTypeMetadataAccessFunction(IGM, classDecl, builder);
  } else {
    ClassMetadataBuilder builder(IGM, classDecl, layout, fieldLayout,
                                 tempBase.get());
    builder.layout();
    init = builder.getInit();
    isPattern = false;
    canBeConstant = builder.canBeConstant();

    maybeEmitNominalTypeMetadataAccessFunction(IGM, classDecl, builder);
  }

  CanType declaredType = classDecl->getDeclaredType()->getCanonicalType();

  // For now, all type metadata is directly stored.
  bool isIndirect = false;

  StringRef section{};
  if (classDecl->isObjC())
    section = "__DATA,__objc_data, regular";

  auto var = IGM.defineTypeMetadata(declaredType, isIndirect, isPattern,
               canBeConstant, init, std::move(tempBase), section);

  // Add classes that don't require dynamic initialization to the
  // ObjC class list.
  if (IGM.ObjCInterop && !isPattern && !isIndirect &&
      !doesClassMetadataRequireDynamicInitialization(IGM, classDecl)) {
    // Emit the ObjC class symbol to make the class visible to ObjC.
    if (classDecl->isObjC()) {
      emitObjCClassSymbol(IGM, classDecl, var);
    }

    IGM.addObjCClass(var,
              classDecl->getAttrs().hasAttribute<ObjCNonLazyRealizationAttr>());
  }
}

void IRGenFunction::setInvariantLoad(llvm::LoadInst *load) {
  load->setMetadata(IGM.InvariantMetadataID, IGM.InvariantNode);
}

void IRGenFunction::setDereferenceableLoad(llvm::LoadInst *load,
                                           unsigned size) {
  auto sizeConstant = llvm::ConstantInt::get(IGM.Int64Ty, size);
  auto sizeNode = llvm::MDNode::get(IGM.LLVMContext,
                                  llvm::ConstantAsMetadata::get(sizeConstant));
  load->setMetadata(IGM.DereferenceableID, sizeNode);
}

/// Emit a load from the given metadata at a constant index.
///
/// The load is marked invariant. This function should not be called
/// on metadata objects that are in the process of being initialized.
static llvm::LoadInst *
emitInvariantLoadFromMetadataAtIndex(IRGenFunction &IGF,
                                     llvm::Value *metadata,
                                     int index,
                                     llvm::Type *objectTy,
                               const Twine &suffix = Twine::createNull()) {
  auto result = emitLoadFromMetadataAtIndex(IGF, metadata, index, objectTy,
                                            suffix);
  IGF.setInvariantLoad(result);
  return result;
}

/// Given an AST type, load its value witness table.
llvm::Value *
IRGenFunction::emitValueWitnessTableRef(CanType type) {
  // See if we have a cached projection we can use.
  if (auto cached = tryGetLocalTypeData(type,
                                  LocalTypeDataKind::forValueWitnessTable())) {
    return cached;
  }
  
  auto metadata = emitTypeMetadataRef(type);
  auto vwtable = emitValueWitnessTableRefForMetadata(metadata);
  setScopedLocalTypeData(type, LocalTypeDataKind::forValueWitnessTable(),
                         vwtable);
  return vwtable;
}

/// Given a type metadata pointer, load its value witness table.
llvm::Value *
IRGenFunction::emitValueWitnessTableRefForMetadata(llvm::Value *metadata) {
  auto witness = emitInvariantLoadFromMetadataAtIndex(*this, metadata, -1,
                                                      IGM.WitnessTablePtrTy,
                                                      ".valueWitnesses");
  // A value witness table is dereferenceable to the number of value witness
  // pointers.
  
  // TODO: If we know the type statically has extra inhabitants, we know
  // there are more witnesses.
  auto numValueWitnesses
    = unsigned(ValueWitness::Last_RequiredValueWitness) + 1;
  setDereferenceableLoad(witness,
                         IGM.getPointerSize().getValue() * numValueWitnesses);
  return witness;
}

/// Given a lowered SIL type, load a value witness table that represents its
/// layout.
llvm::Value *
IRGenFunction::emitValueWitnessTableRefForLayout(SILType type) {
  // See if we have a cached projection we can use.
  if (auto cached = tryGetLocalTypeDataForLayout(type,
                                  LocalTypeDataKind::forValueWitnessTable())) {
    return cached;
  }
  
  auto metadata = emitTypeMetadataRefForLayout(type);
  auto vwtable = emitValueWitnessTableRefForMetadata(metadata);
  setScopedLocalTypeDataForLayout(type,
                                  LocalTypeDataKind::forValueWitnessTable(),
                                  vwtable);
  return vwtable;
}

/// Load the metadata reference at the given index.
static llvm::Value *emitLoadOfMetadataRefAtIndex(IRGenFunction &IGF,
                                                 llvm::Value *metadata,
                                                 int index) {
  return emitInvariantLoadFromMetadataAtIndex(IGF, metadata, index,
                                              IGF.IGM.TypeMetadataPtrTy);
}

/// Load the protocol witness table reference at the given index.
static llvm::Value *emitLoadOfWitnessTableRefAtIndex(IRGenFunction &IGF,
                                                     llvm::Value *metadata,
                                                     int index) {
  return emitInvariantLoadFromMetadataAtIndex(IGF, metadata, index,
                                              IGF.IGM.WitnessTablePtrTy);
}

namespace {
  /// A class for finding the 'parent' index in a class metadata object.
  BEGIN_METADATA_SEARCHER_0(FindClassParentIndex, Class)
    void addParentMetadataRef(ClassDecl *forClass, Type classType) {
      if (forClass == Target) setTargetOffset();
      super::addParentMetadataRef(forClass, classType);
    }
  END_METADATA_SEARCHER()
} // end anonymous namespace

/// Return the index of the parent metadata pointer for the given class.
static int getClassParentIndex(IRGenModule &IGM, ClassDecl *classDecl) {
  assert(classDecl->getDeclContext()->isTypeContext());
  return FindClassParentIndex(IGM, classDecl).getTargetIndex();
}

/// In both enums and structs, the parent index is at index 2.
static constexpr int ValueTypeParentIndex = 2;

/// Given a reference to some metadata, derive a reference to the
/// type's parent type.
llvm::Value *irgen::emitParentMetadataRef(IRGenFunction &IGF,
                                          NominalTypeDecl *decl,
                                          llvm::Value *metadata) {
  assert(decl->getDeclContext()->isTypeContext());

  switch (decl->getKind()) {
#define NOMINAL_TYPE_DECL(id, parent)
#define DECL(id, parent) \
  case DeclKind::id:
#include "swift/AST/DeclNodes.def"
    llvm_unreachable("not a nominal type");

  case DeclKind::Protocol:
    llvm_unreachable("protocols never have parent types!");

  case DeclKind::Class: {
    int index = getClassParentIndex(IGF.IGM, cast<ClassDecl>(decl));
    return emitLoadOfMetadataRefAtIndex(IGF, metadata, index);
  }

  case DeclKind::Enum:
  case DeclKind::Struct:
    return emitLoadOfMetadataRefAtIndex(IGF, metadata, ValueTypeParentIndex);
  }
  llvm_unreachable("bad decl kind!");
}

namespace {
  /// A class for finding the start of the generic requirements section
  /// in a type metadata object.
  BEGIN_GENERIC_METADATA_SEARCHER_0(FindTypeGenericRequirements)
    template <class... T>
    void noteStartOfGenericRequirements() {
      this->setTargetOffset();
    }

    template <class... T>
    void noteStartOfGenericRequirements(ClassDecl *forClass) {
      if (forClass == Target)
        this->setTargetOffset();
    }
  END_GENERIC_METADATA_SEARCHER(GenericRequirements)
} // end anonymous namespace

static int getIndexOfGenericRequirement(IRGenModule &IGM,
                                        NominalTypeDecl *decl,
                                        unsigned reqtIndex) {
  switch (decl->getKind()) {
#define NOMINAL_TYPE_DECL(id, parent)
#define DECL(id, parent) \
  case DeclKind::id:
#include "swift/AST/DeclNodes.def"
    llvm_unreachable("not a nominal type");

  case DeclKind::Protocol:
    llvm_unreachable("protocols are never generic!");

  case DeclKind::Class: {
    int index = FindClassGenericRequirements(IGM, cast<ClassDecl>(decl))
                  .getTargetIndex() + (int) reqtIndex;
    return index;
  }

  case DeclKind::Enum: {
    int index = FindEnumGenericRequirements(IGM, cast<EnumDecl>(decl))
                  .getTargetIndex() + (int) reqtIndex;
    return index;
  }
        
  case DeclKind::Struct: {
    int index = FindStructGenericRequirements(IGM, cast<StructDecl>(decl))
                  .getTargetIndex() + (int) reqtIndex;
    return index;
  }
  }
  llvm_unreachable("bad decl kind!");
}

/// Given a reference to nominal type metadata of the given type,
/// derive a reference to the nth argument metadata.  The type must
/// have generic arguments.
llvm::Value *irgen::emitArgumentMetadataRef(IRGenFunction &IGF,
                                            NominalTypeDecl *decl,
                                      const GenericTypeRequirements &reqts,
                                            unsigned reqtIndex,
                                            llvm::Value *metadata) {
  assert(reqts.getRequirements()[reqtIndex].Protocol == nullptr);
  int index = getIndexOfGenericRequirement(IGF.IGM, decl, reqtIndex);
  return emitLoadOfMetadataRefAtIndex(IGF, metadata, index);
}

/// Given a reference to nominal type metadata of the given type,
/// derive a reference to a protocol witness table for the nth
/// argument metadata.  The type must have generic arguments.
llvm::Value *irgen::emitArgumentWitnessTableRef(IRGenFunction &IGF,
                                                NominalTypeDecl *decl,
                                          const GenericTypeRequirements &reqts,
                                                unsigned reqtIndex,
                                                llvm::Value *metadata) {
  assert(reqts.getRequirements()[reqtIndex].Protocol != nullptr);
  int index = getIndexOfGenericRequirement(IGF.IGM, decl, reqtIndex);
  return emitLoadOfWitnessTableRefAtIndex(IGF, metadata, index);
}

irgen::Size irgen::getClassFieldOffset(IRGenModule &IGM,
                                       ClassDecl *theClass,
                                       VarDecl *field) {
  /// A class for finding a field offset in a class metadata object.
  BEGIN_METADATA_SEARCHER_1(FindClassFieldOffset, Class,
                            VarDecl *, TargetField)
    void addFieldOffset(VarDecl *field) {
      if (field == TargetField)
        setTargetOffset();
      super::addFieldOffset(field);
    }
  END_METADATA_SEARCHER()

  return FindClassFieldOffset(IGM, theClass, field).getTargetOffset();
}

/// Given a reference to class metadata of the given type,
/// derive a reference to the field offset for a stored property.
/// The type must have dependent generic layout.
llvm::Value *irgen::emitClassFieldOffset(IRGenFunction &IGF,
                                         ClassDecl *theClass,
                                         VarDecl *field,
                                         llvm::Value *metadata) {
  irgen::Size offset = getClassFieldOffset(IGF.IGM, theClass, field);
  int index = IGF.IGM.getOffsetInWords(offset);
  return emitInvariantLoadFromMetadataAtIndex(IGF, metadata, index,
                                              IGF.IGM.SizeTy);
}

/// Given a reference to class metadata of the given type,
/// load the fragile instance size and alignment of the class.
std::pair<llvm::Value *, llvm::Value *>
irgen::emitClassFragileInstanceSizeAndAlignMask(IRGenFunction &IGF,
                                                ClassDecl *theClass,
                                                llvm::Value *metadata) {  
  // If the class has fragile fixed layout, return the constant size and
  // alignment.
  if (llvm::Constant *size
        = tryEmitClassConstantFragileInstanceSize(IGF.IGM, theClass)) {
    llvm::Constant *alignMask
      = tryEmitClassConstantFragileInstanceAlignMask(IGF.IGM, theClass);
    assert(alignMask && "static size without static align");
    return {size, alignMask};
  }
 
  // Otherwise, load it from the metadata.
  return emitClassResilientInstanceSizeAndAlignMask(IGF, theClass, metadata);
}

std::pair<llvm::Value *, llvm::Value *>
irgen::emitClassResilientInstanceSizeAndAlignMask(IRGenFunction &IGF,
                                                  ClassDecl *theClass,
                                                  llvm::Value *metadata) {
  class FindClassSize
         : public ClassMetadataScanner<FindClassSize> {
    using super = ClassMetadataScanner<FindClassSize>;
  public:
    FindClassSize(IRGenModule &IGM, ClassDecl *theClass)
      : ClassMetadataScanner(IGM, theClass) {}

    Size InstanceSize = Size::invalid();
    Size InstanceAlignMask = Size::invalid();
        
    void noteAddressPoint() {
      assert(InstanceSize.isInvalid() && InstanceAlignMask.isInvalid()
             && "found size or alignment before address point?!");
      NextOffset = Size(0);
    }
        
    void addInstanceSize() {
      InstanceSize = NextOffset;
      super::addInstanceSize();
    }
      
    void addInstanceAlignMask() {
      InstanceAlignMask = NextOffset;
      super::addInstanceAlignMask();
    }
  };

  FindClassSize scanner(IGF.IGM, theClass);
  scanner.layout();
  assert(!scanner.InstanceSize.isInvalid()
         && !scanner.InstanceAlignMask.isInvalid()
         && "didn't find size or alignment in metadata?!");
  Address metadataAsBytes(IGF.Builder.CreateBitCast(metadata, IGF.IGM.Int8PtrTy),
                          IGF.IGM.getPointerAlignment());

  Address slot = IGF.Builder.CreateConstByteArrayGEP(metadataAsBytes,
                                                     scanner.InstanceSize);
  slot = IGF.Builder.CreateBitCast(slot, IGF.IGM.Int32Ty->getPointerTo());
  llvm::Value *size = IGF.Builder.CreateLoad(slot);
  if (IGF.IGM.SizeTy != IGF.IGM.Int32Ty)
    size = IGF.Builder.CreateZExt(size, IGF.IGM.SizeTy);

  slot = IGF.Builder.CreateConstByteArrayGEP(metadataAsBytes,
                                             scanner.InstanceAlignMask);
  slot = IGF.Builder.CreateBitCast(slot, IGF.IGM.Int16Ty->getPointerTo());
  llvm::Value *alignMask = IGF.Builder.CreateLoad(slot);
  alignMask = IGF.Builder.CreateZExt(alignMask, IGF.IGM.SizeTy);

  return {size, alignMask};
}

/// Given a non-tagged object pointer, load a pointer to its class object.
static llvm::Value *emitLoadOfObjCHeapMetadataRef(IRGenFunction &IGF,
                                                  llvm::Value *object) {
  if (IGF.IGM.TargetInfo.hasISAMasking()) {
    object = IGF.Builder.CreateBitCast(object,
                                       IGF.IGM.IntPtrTy->getPointerTo());
    llvm::Value *metadata =
      IGF.Builder.CreateLoad(Address(object, IGF.IGM.getPointerAlignment()));
    llvm::Value *mask = IGF.Builder.CreateLoad(IGF.IGM.getAddrOfObjCISAMask());
    metadata = IGF.Builder.CreateAnd(metadata, mask);
    metadata = IGF.Builder.CreateIntToPtr(metadata, IGF.IGM.TypeMetadataPtrTy);
    return metadata;
  } else if (IGF.IGM.TargetInfo.hasOpaqueISAs()) {
    return emitHeapMetadataRefForUnknownHeapObject(IGF, object);
  } else {
    object = IGF.Builder.CreateBitCast(object,
                                  IGF.IGM.TypeMetadataPtrTy->getPointerTo());
    llvm::Value *metadata =
      IGF.Builder.CreateLoad(Address(object, IGF.IGM.getPointerAlignment()));
    return metadata;
  }
}

/// Given a pointer to a heap object (i.e. definitely not a tagged
/// pointer), load its heap metadata pointer.
static llvm::Value *emitLoadOfHeapMetadataRef(IRGenFunction &IGF,
                                              llvm::Value *object,
                                              IsaEncoding isaEncoding,
                                              bool suppressCast) {
  switch (isaEncoding) {
  case IsaEncoding::Pointer: {
    // Drill into the object pointer.  Rather than bitcasting, we make
    // an effort to do something that should explode if we get something
    // mistyped.
    llvm::StructType *structTy =
      cast<llvm::StructType>(
        cast<llvm::PointerType>(object->getType())->getElementType());

    llvm::Value *slot;

    // We need a bitcast if we're dealing with an opaque class.
    if (structTy->isOpaque()) {
      auto metadataPtrPtrTy = IGF.IGM.TypeMetadataPtrTy->getPointerTo();
      slot = IGF.Builder.CreateBitCast(object, metadataPtrPtrTy);

    // Otherwise, make a GEP.
    } else {
      auto zero = llvm::ConstantInt::get(IGF.IGM.Int32Ty, 0);

      SmallVector<llvm::Value*, 4> indexes;
      indexes.push_back(zero);
      do {
        indexes.push_back(zero);

        // Keep drilling down to the first element type.
        auto eltTy = structTy->getElementType(0);
        assert(isa<llvm::StructType>(eltTy) || eltTy == IGF.IGM.TypeMetadataPtrTy);
        structTy = dyn_cast<llvm::StructType>(eltTy);
      } while (structTy != nullptr);

      slot = IGF.Builder.CreateInBoundsGEP(object, indexes);

      if (!suppressCast) {
        slot = IGF.Builder.CreateBitCast(slot,
                                    IGF.IGM.TypeMetadataPtrTy->getPointerTo());
      }
    }

    auto metadata = IGF.Builder.CreateLoad(Address(slot,
                                               IGF.IGM.getPointerAlignment()));
    if (IGF.IGM.EnableValueNames && object->hasName())
      metadata->setName(llvm::Twine(object->getName()) + ".metadata");
    return metadata;
  }
      
  case IsaEncoding::ObjC: {
    // Feed the object pointer to object_getClass.
    llvm::Value *objcClass = emitLoadOfObjCHeapMetadataRef(IGF, object);
    objcClass = IGF.Builder.CreateBitCast(objcClass, IGF.IGM.TypeMetadataPtrTy);
    return objcClass;
  }
  }

  llvm_unreachable("Not a valid IsaEncoding.");
}

/// Given an object of class type, produce the heap metadata reference
/// as an %objc_class*.
llvm::Value *irgen::emitHeapMetadataRefForHeapObject(IRGenFunction &IGF,
                                                     llvm::Value *object,
                                                     CanType objectType,
                                                     bool suppressCast) {
  ClassDecl *theClass = objectType.getClassOrBoundGenericClass();
  if (theClass && isKnownNotTaggedPointer(IGF.IGM, theClass))
    return emitLoadOfHeapMetadataRef(IGF, object,
                                     getIsaEncodingForType(IGF.IGM, objectType),
                                     suppressCast);

  // OK, ask the runtime for the class pointer of this potentially-ObjC object.
  return emitHeapMetadataRefForUnknownHeapObject(IGF, object);
}

llvm::Value *irgen::emitHeapMetadataRefForHeapObject(IRGenFunction &IGF,
                                                     llvm::Value *object,
                                                     SILType objectType,
                                                     bool suppressCast) {
  return emitHeapMetadataRefForHeapObject(IGF, object,
                                          objectType.getSwiftRValueType(),
                                          suppressCast);
}

/// Given an opaque class instance pointer, produce the type metadata reference
/// as a %type*.
llvm::Value *irgen::emitDynamicTypeOfOpaqueHeapObject(IRGenFunction &IGF,
                                                      llvm::Value *object) {
  object = IGF.Builder.CreateBitCast(object, IGF.IGM.ObjCPtrTy);
  auto metadata = IGF.Builder.CreateCall(IGF.IGM.getGetObjectTypeFn(),
                                         object,
                                         object->getName() + ".Type");
  metadata->setDoesNotThrow();
  metadata->setDoesNotAccessMemory();
  return metadata;
}

llvm::Value *irgen::
emitHeapMetadataRefForUnknownHeapObject(IRGenFunction &IGF,
                                        llvm::Value *object) {
  object = IGF.Builder.CreateBitCast(object, IGF.IGM.ObjCPtrTy);
  auto metadata = IGF.Builder.CreateCall(IGF.IGM.getGetObjectClassFn(),
                                         object,
                                         object->getName() + ".Type");
  metadata->setCallingConv(llvm::CallingConv::C);
  metadata->setDoesNotThrow();
  metadata->addAttribute(llvm::AttributeSet::FunctionIndex,
                         llvm::Attribute::ReadOnly);
  return metadata;
}

/// Given an object of class type, produce the type metadata reference
/// as a %type*.
llvm::Value *irgen::emitDynamicTypeOfHeapObject(IRGenFunction &IGF,
                                                llvm::Value *object,
                                                SILType objectType,
                                                bool suppressCast) {
  // If it is known to have swift metadata, just load.
  if (hasKnownSwiftMetadata(IGF.IGM, objectType.getSwiftRValueType())) {
    return emitLoadOfHeapMetadataRef(IGF, object,
                getIsaEncodingForType(IGF.IGM, objectType.getSwiftRValueType()),
                suppressCast);
  }

  // Okay, ask the runtime for the type metadata of this
  // potentially-ObjC object.
  return emitDynamicTypeOfOpaqueHeapObject(IGF, object);
}

/// Given a class metatype, produce the necessary heap metadata
/// reference.  This is generally the metatype pointer, but may
/// instead be a reference type.
llvm::Value *irgen::emitClassHeapMetadataRefForMetatype(IRGenFunction &IGF,
                                                        llvm::Value *metatype,
                                                        CanType type) {
  // If the type is known to have Swift metadata, this is trivial.
  if (hasKnownSwiftMetadata(IGF.IGM, type))
    return metatype;

  // Otherwise, we inline a little operation here.

  // Load the metatype kind.
  auto metatypeKindAddr =
    Address(IGF.Builder.CreateStructGEP(/*Ty=*/nullptr, metatype, 0),
            IGF.IGM.getPointerAlignment());
  auto metatypeKind =
    IGF.Builder.CreateLoad(metatypeKindAddr, metatype->getName() + ".kind");

  // Compare it with the class wrapper kind.
  auto classWrapperKind =
    llvm::ConstantInt::get(IGF.IGM.MetadataKindTy,
                           unsigned(MetadataKind::ObjCClassWrapper));
  auto isObjCClassWrapper =
    IGF.Builder.CreateICmpEQ(metatypeKind, classWrapperKind,
                             "isObjCClassWrapper");

  // Branch based on that.
  llvm::BasicBlock *contBB = IGF.createBasicBlock("metadataForClass.cont");
  llvm::BasicBlock *wrapBB = IGF.createBasicBlock("isWrapper");
  IGF.Builder.CreateCondBr(isObjCClassWrapper, wrapBB, contBB);
  llvm::BasicBlock *origBB = IGF.Builder.GetInsertBlock();

  // If it's a wrapper, load from the 'Class' field, which is at index 1.
  // TODO: if we guaranteed that this load couldn't crash, we could use
  // a select here instead, which might be profitable.
  IGF.Builder.emitBlock(wrapBB);
  auto classFromWrapper = 
    emitInvariantLoadFromMetadataAtIndex(IGF, metatype, 1,
                                         IGF.IGM.TypeMetadataPtrTy);
  IGF.Builder.CreateBr(contBB);

  // Continuation block.
  IGF.Builder.emitBlock(contBB);
  auto phi = IGF.Builder.CreatePHI(IGF.IGM.TypeMetadataPtrTy, 2,
                                   metatype->getName() + ".class");
  phi->addIncoming(metatype, origBB);
  phi->addIncoming(classFromWrapper, wrapBB);

  return phi;
}

namespace {
  /// A class for finding a vtable entry offset for a method argument
  /// in a class metadata object.
  BEGIN_METADATA_SEARCHER_1(FindClassMethodIndex, Class,
                            SILDeclRef, TargetMethod)
    void addMethod(SILDeclRef fn) {
      if (TargetMethod == fn)
        setTargetOffset();
      super::addMethod(fn);
    }
  END_METADATA_SEARCHER()
} // end anonymous namespace

/// Load the correct virtual function for the given class method.
llvm::Value *irgen::emitVirtualMethodValue(IRGenFunction &IGF,
                                           llvm::Value *base,
                                           SILType baseType,
                                           SILDeclRef method,
                                           CanSILFunctionType methodType,
                                           bool useSuperVTable) {
  AbstractFunctionDecl *methodDecl
    = cast<AbstractFunctionDecl>(method.getDecl());

  // Find the function that's actually got an entry in the metadata.
  SILDeclRef overridden = method.getBaseOverriddenVTableEntry();

  // Find the metadata.
  llvm::Value *metadata;
  if (useSuperVTable) {
    auto instanceTy = baseType;
    if (auto metaTy = dyn_cast<MetatypeType>(baseType.getSwiftRValueType()))
      instanceTy = SILType::getPrimitiveObjectType(metaTy.getInstanceType());

    if (IGF.IGM.isResilient(instanceTy.getClassOrBoundGenericClass(),
                            ResilienceExpansion::Maximal)) {
      // The derived type that is making the super call is resilient,
      // for example we may be in an extension of a class outside of our
      // resilience domain. So, we need to load the superclass metadata
      // dynamically.

      metadata = emitClassHeapMetadataRef(IGF, instanceTy.getSwiftRValueType(),
                                          MetadataValueType::TypeMetadata);
      auto superField = emitAddressOfSuperclassRefInClassMetadata(IGF, metadata);
      metadata = IGF.Builder.CreateLoad(superField);
    } else {
      // Otherwise, we can directly load the statically known superclass's
      // metadata.
      auto superTy = instanceTy.getSuperclass(/*resolver=*/nullptr);
      metadata = emitClassHeapMetadataRef(IGF, superTy.getSwiftRValueType(),
                                          MetadataValueType::TypeMetadata);
    }
  } else {
    if ((isa<FuncDecl>(methodDecl) && cast<FuncDecl>(methodDecl)->isStatic()) ||
        (isa<ConstructorDecl>(methodDecl) &&
         method.kind == SILDeclRef::Kind::Allocator)) {
      metadata = base;
    } else {
      metadata = emitHeapMetadataRefForHeapObject(IGF, base, baseType,
                                                  /*suppress cast*/ true);
    }
  }

  // Use the type of the method we were type-checked against, not the
  // type of the overridden method.
  llvm::AttributeSet attrs;
  auto fnTy = IGF.IGM.getFunctionType(methodType, attrs)->getPointerTo();

  auto declaringClass = cast<ClassDecl>(overridden.getDecl()->getDeclContext());
  auto index = FindClassMethodIndex(IGF.IGM, declaringClass, overridden)
                 .getTargetIndex();

  return emitInvariantLoadFromMetadataAtIndex(IGF, metadata, index, fnTy);
}

//===----------------------------------------------------------------------===//
// Value types (structs and enums)
//===----------------------------------------------------------------------===//

namespace {
  template <class Impl, class Base>
    class ValueTypeMetadataBuilderBase : public ConstantBuilder<Base> {
    using super = ConstantBuilder<Base>;

  protected:
    using super::asImpl;
    using super::IGM;
    using super::Target;
    using super::addWord;

    template <class DeclTy>
    ValueTypeMetadataBuilderBase(IRGenModule &IGM, DeclTy *theDecl)
      : super(IGM, theDecl) {}

    CanType getParentType() const {
      Type type = Target->getDeclaredTypeInContext();
      Type parentType = type->getNominalParent();
      if (parentType)
        return parentType->getCanonicalType();
      return CanType();
    }

  public:
    void addParentMetadataRef() {
      llvm::Constant *parentMetadata = nullptr;
      if (auto parentType = getParentType()) {
        parentMetadata =
          tryEmitConstantTypeMetadataRef(IGM, parentType,
                                         SymbolReferenceKind::Absolute)
            .getDirectValue();
        if (!parentMetadata) {
          asImpl().flagUnfilledParent();
        }
      }

      if (!parentMetadata)
        parentMetadata = llvm::ConstantPointerNull::get(IGM.TypeMetadataPtrTy);
      addWord(parentMetadata);
    }
  };
} // end anonymous namespace

static llvm::Value *
emitInPlaceValueTypeMetadataInitialization(IRGenFunction &IGF,
                                           CanNominalType type,
                                           llvm::Value *metadata,
                                           bool hasUnfilledParent) {
  // All the value types are basically similar.
  assert(isa<StructType>(type) || isa<EnumType>(type));

  // Initialize the parent-metadata field if it wasn't done statically.
  if (hasUnfilledParent) {
    CanType parentType = type.getParent();
    assert(parentType);

    // Value types hold the parent metadata as a far relative
    // indirectable pointer.
    llvm::Value *parentMetadata = IGF.emitTypeMetadataRef(parentType);
    Address addr =
      emitAddressOfMetadataSlotAtIndex(IGF, metadata, ValueTypeParentIndex,
                                       IGF.IGM.TypeMetadataPtrTy);
    IGF.Builder.CreateStore(parentMetadata, addr);
  }

  // Set up the value witness table if it's dependent.
  SILType loweredType = IGF.IGM.getLoweredType(AbstractionPattern(type), type);
  auto &ti = IGF.IGM.getTypeInfo(loweredType);
  if (!ti.isFixedSize()) {
    // We assume that that value witness table will already have been written
    // into the metadata; just load it.
    llvm::Value *vwtable = IGF.emitValueWitnessTableRefForMetadata(metadata);

    // Initialize the metadata.
    ti.initializeMetadata(IGF, metadata, vwtable, loweredType.getAddressType());
  }

  return metadata;
}

/// Create an access function for the type metadata of the given
/// non-generic nominal type.
static void createInPlaceValueTypeMetadataAccessFunction(IRGenModule &IGM,
                                                      NominalTypeDecl *typeDecl,
                                                      bool hasUnfilledParent) {
  assert(!typeDecl->isGenericContext());
  auto type =
    cast<NominalType>(typeDecl->getDeclaredType()->getCanonicalType());

  (void) getTypeMetadataAccessFunction(IGM, type, ForDefinition,
                                       [&](IRGenFunction &IGF,
                                           llvm::Constant *cacheVariable) {
    return emitInPlaceTypeMetadataAccessFunctionBody(IGF, type, cacheVariable,
      [&](IRGenFunction &IGF, llvm::Value *metadata) {
        return emitInPlaceValueTypeMetadataInitialization(IGF, type, metadata,
                                                          hasUnfilledParent);
      });
  });
}

//===----------------------------------------------------------------------===//
// Structs
//===----------------------------------------------------------------------===//

namespace {
  /// An adapter for laying out struct metadata.
  template <class Impl>
  class StructMetadataBuilderBase
         : public ValueTypeMetadataBuilderBase<Impl,StructMetadataLayout<Impl>>{
    using super = ValueTypeMetadataBuilderBase<Impl,StructMetadataLayout<Impl>>;

  protected:
    using super::IGM;
    using super::Target;
    using super::asImpl;
    using super::addConstantWord;
    using super::addWord;
    using super::addFarRelativeAddress;
    using super::addFarRelativeAddressOrNull;
    using super::setRelativeAddressBase;

    StructMetadataBuilderBase(IRGenModule &IGM, StructDecl *theStruct,
                              llvm::GlobalVariable *relativeAddressBase)
    : super(IGM, theStruct) {
      setRelativeAddressBase(relativeAddressBase);
    }

  public:
    void addMetadataFlags() {
      addWord(getMetadataKind(IGM, MetadataKind::Struct));
    }

    void addNominalTypeDescriptor() {
      llvm::Constant *descriptor =
        StructNominalTypeDescriptorBuilder(IGM, Target).emit();
      addFarRelativeAddress(descriptor);
    }

    void addFieldOffset(VarDecl *var) {
      assert(var->hasStorage() &&
             "storing field offset for computed property?!");
      SILType structType =
        IGM.getLoweredType(Target->getDeclaredTypeInContext());

      llvm::Constant *offset =
        emitPhysicalStructMemberFixedOffset(IGM, structType, var);
      // If we have a fixed offset, add it. Otherwise, leave zero as a
      // placeholder.
      if (offset) {
        addWord(offset);
      } else {
        asImpl().flagUnfilledFieldOffset();
        addConstantWord(0);
      }
    }

    void addGenericArgument(CanType type) {
      addWord(llvm::Constant::getNullValue(IGM.TypeMetadataPtrTy));
    }

    void addGenericWitnessTable(CanType type, ProtocolConformanceRef conf) {
      addWord(llvm::Constant::getNullValue(IGM.WitnessTablePtrTy));
    }

    llvm::Constant *getInit() {
      return this->getInitWithSuggestedType(NumHeapMetadataFields,
                                            IGM.FullHeapMetadataStructTy);
    }
  };

  class StructMetadataBuilder :
    public StructMetadataBuilderBase<StructMetadataBuilder> {

    bool HasUnfilledParent = false;
    bool HasUnfilledFieldOffset = false;
  public:
    StructMetadataBuilder(IRGenModule &IGM, StructDecl *theStruct,
                          llvm::GlobalVariable *relativeAddressBase)
      : StructMetadataBuilderBase(IGM, theStruct, relativeAddressBase) {}

    void flagUnfilledParent() {
      HasUnfilledParent = true;
    }

    void flagUnfilledFieldOffset() {
      HasUnfilledFieldOffset = true;
    }

    bool canBeConstant() {
      return !HasUnfilledParent && !HasUnfilledFieldOffset;
    }

    void addValueWitnessTable() {
      auto type = this->Target->getDeclaredType()->getCanonicalType();
      addWord(emitValueWitnessTable(IGM, type));
    }

    void createMetadataAccessFunction() {
      createInPlaceValueTypeMetadataAccessFunction(IGM, Target,
                                                   HasUnfilledParent);
    }
  };
  
  /// Emit a value witness table for a fixed-layout generic type, or a null
  /// placeholder if the value witness table is dependent on generic parameters.
  /// Returns nullptr if the value witness table is dependent.
  static llvm::Constant *
  getValueWitnessTableForGenericValueType(IRGenModule &IGM,
                                          NominalTypeDecl *decl,
                                          bool &dependent) {
    CanType unboundType
      = decl->getDeclaredTypeOfContext()->getCanonicalType();
    
    dependent = hasDependentValueWitnessTable(IGM, unboundType);    
    if (dependent)
      return llvm::ConstantPointerNull::get(IGM.Int8PtrTy);
    else
      return emitValueWitnessTable(IGM, unboundType);
  }
  
  /// A builder for metadata templates.
  class GenericStructMetadataBuilder :
    public GenericMetadataBuilderBase<GenericStructMetadataBuilder,
                      StructMetadataBuilderBase<GenericStructMetadataBuilder>> {

    typedef GenericMetadataBuilderBase super;
                        
  public:
    GenericStructMetadataBuilder(IRGenModule &IGM, StructDecl *theStruct,
                                 llvm::GlobalVariable *relativeAddressBase)
      : super(IGM, theStruct, relativeAddressBase) {}

    llvm::Value *emitAllocateMetadata(IRGenFunction &IGF,
                                      llvm::Value *metadataPattern,
                                      llvm::Value *arguments) {
      return IGF.Builder.CreateCall(IGM.getAllocateGenericValueMetadataFn(),
                                    {metadataPattern, arguments});
    }

    void addParentMetadataRef() {
      // Override to always use a fill op instead of a relocation.
      if (CanType parentType = getParentType()) {
        addFillOp(parentType, None, /*relative*/ false);
      }

      addConstantWord(0);
    }

    void flagUnfilledFieldOffset() {
      // We just assume this might happen.
    }
    
    void addValueWitnessTable() {
      addWord(getValueWitnessTableForGenericValueType(IGM, Target,
                                                      HasDependentVWT));
    }
                        
    void addDependentValueWitnessTablePattern() {
      SmallVector<llvm::Constant*, 20> pattern;
      emitDependentValueWitnessTablePattern(IGM,
                        Target->getDeclaredTypeOfContext()->getCanonicalType(),
                                            pattern);
      for (auto witness: pattern)
        addWord(witness);
    }
                        
    void emitInitializeMetadata(IRGenFunction &IGF,
                                llvm::Value *metadata,
                                llvm::Value *vwtable) {
      // Nominal types are always preserved through SIL lowering.
      auto structTy = Target->getDeclaredTypeInContext()->getCanonicalType();
      IGM.getTypeInfoForUnlowered(structTy)
        .initializeMetadata(IGF, metadata, vwtable,
                            IGF.IGM.getLoweredType(structTy));
    }
  };
} // end anonymous namespace

/// Emit the type metadata or metadata template for a struct.
void irgen::emitStructMetadata(IRGenModule &IGM, StructDecl *structDecl) {
  // Set up a dummy global to stand in for the metadata object while we produce
  // relative references.
  auto tempBase = createTemporaryRelativeAddressBase(IGM);

  // TODO: structs nested within generic types
  llvm::Constant *init;
  bool isPattern;
  bool canBeConstant;
  if (structDecl->isGenericContext()) {
    GenericStructMetadataBuilder builder(IGM, structDecl, tempBase.get());
    builder.layout();
    init = builder.getInit();
    isPattern = true;
    canBeConstant = false;

    maybeEmitNominalTypeMetadataAccessFunction(IGM, structDecl, builder);
  } else {
    StructMetadataBuilder builder(IGM, structDecl, tempBase.get());
    builder.layout();
    init = builder.getInit();
    isPattern = false;
    canBeConstant = builder.canBeConstant();

    maybeEmitNominalTypeMetadataAccessFunction(IGM, structDecl, builder);
  }

  CanType declaredType = structDecl->getDeclaredType()->getCanonicalType();

  // For now, all type metadata is directly stored.
  bool isIndirect = false;

  IGM.defineTypeMetadata(declaredType, isIndirect, isPattern,
                         canBeConstant, init,
                         std::move(tempBase));
}

// Enums

namespace {

template<class Impl>
class EnumMetadataBuilderBase
       : public ValueTypeMetadataBuilderBase<Impl, EnumMetadataLayout<Impl>> {
  using super = ValueTypeMetadataBuilderBase<Impl, EnumMetadataLayout<Impl>>;

protected:
  using super::IGM;
  using super::Target;
  using super::addWord;
  using super::addFarRelativeAddress;
  using super::addFarRelativeAddressOrNull;
  using super::setRelativeAddressBase;

public:
  EnumMetadataBuilderBase(IRGenModule &IGM, EnumDecl *theEnum,
                          llvm::GlobalVariable *relativeAddressBase)
  : super(IGM, theEnum) {
    setRelativeAddressBase(relativeAddressBase);
  }
  
  void addMetadataFlags() {
    addWord(getMetadataKind(IGM, Target->classifyAsOptionalType()
                            ? MetadataKind::Optional : MetadataKind::Enum));
  }
  
  void addNominalTypeDescriptor() {
    auto descriptor =
      EnumNominalTypeDescriptorBuilder(IGM, Target).emit();
    
    addFarRelativeAddress(descriptor);
  }

  void addGenericArgument(CanType type) {
    addWord(llvm::Constant::getNullValue(IGM.TypeMetadataPtrTy));
  }
  
  void addGenericWitnessTable(CanType type, ProtocolConformanceRef conf) {
    addWord(llvm::Constant::getNullValue(IGM.WitnessTablePtrTy));
  }
};
  
class EnumMetadataBuilder
  : public EnumMetadataBuilderBase<EnumMetadataBuilder> {
  bool HasUnfilledParent = false;
  bool HasUnfilledPayloadSize = false;

public:
  EnumMetadataBuilder(IRGenModule &IGM, EnumDecl *theEnum,
                      llvm::GlobalVariable *relativeAddressBase)
    : EnumMetadataBuilderBase(IGM, theEnum, relativeAddressBase) {}
  
  void addValueWitnessTable() {
    auto type = Target->getDeclaredType()->getCanonicalType();
    addWord(emitValueWitnessTable(IGM, type));
  }
  
  void addPayloadSize() {
    auto enumTy = Target->getDeclaredTypeInContext()->getCanonicalType();
    auto &enumTI = IGM.getTypeInfoForUnlowered(enumTy);
    if (!enumTI.isFixedSize(ResilienceExpansion::Maximal)) {
      addConstantWord(0);
      HasUnfilledPayloadSize = true;
      return;
    }

    assert(!enumTI.isFixedSize(ResilienceExpansion::Minimal) &&
           "non-generic, non-resilient enums don't need payload size in metadata");
    auto &strategy = getEnumImplStrategy(IGM, enumTy);
    addConstantWord(strategy.getPayloadSizeForMetadata());
  }

  void flagUnfilledParent() {
    HasUnfilledParent = true;
  }

  bool canBeConstant() {
    return !HasUnfilledParent && !HasUnfilledPayloadSize;
  }

  void createMetadataAccessFunction() {
    createInPlaceValueTypeMetadataAccessFunction(IGM, Target,
                                                 HasUnfilledParent);
  }
};
  
class GenericEnumMetadataBuilder
  : public GenericMetadataBuilderBase<GenericEnumMetadataBuilder,
                        EnumMetadataBuilderBase<GenericEnumMetadataBuilder>>
{
public:
  GenericEnumMetadataBuilder(IRGenModule &IGM, EnumDecl *theEnum,
                             llvm::GlobalVariable *relativeAddressBase)
    : GenericMetadataBuilderBase(IGM, theEnum, relativeAddressBase) {}

  llvm::Value *emitAllocateMetadata(IRGenFunction &IGF,
                                    llvm::Value *metadataPattern,
                                    llvm::Value *arguments) {
    return IGF.Builder.CreateCall(IGM.getAllocateGenericValueMetadataFn(),
                                  {metadataPattern, arguments});
  }

  void addParentMetadataRef() {
    // Override to always use a fill op instead of a relocation.
    if (CanType parentType = getParentType()) {
      addFillOp(parentType, None, /*relative*/ false);
    }

    addConstantWord(0);
  }
  
  void addValueWitnessTable() {
    addWord(getValueWitnessTableForGenericValueType(IGM, Target,
                                                    HasDependentVWT));
  }
  
  void addDependentValueWitnessTablePattern() {
    SmallVector<llvm::Constant*, 20> pattern;
    emitDependentValueWitnessTablePattern(IGM,
                        Target->getDeclaredTypeOfContext()->getCanonicalType(),
                                          pattern);
    for (auto witness: pattern)
      addWord(witness);
  }
  
  void addPayloadSize() {
    // In all cases where a payload size is demanded in the metadata, it's
    // runtime-dependent, so fill in a zero here.
    auto enumTy = Target->getDeclaredTypeInContext()->getCanonicalType();
    auto &enumTI = IGM.getTypeInfoForUnlowered(enumTy);
    (void) enumTI;
    assert(!enumTI.isFixedSize(ResilienceExpansion::Minimal) &&
           "non-generic, non-resilient enums don't need payload size in metadata");
    addConstantWord(0);
  }
  
  void emitInitializeMetadata(IRGenFunction &IGF,
                              llvm::Value *metadata,
                              llvm::Value *vwtable) {
    // Nominal types are always preserved through SIL lowering.
    auto enumTy = Target->getDeclaredTypeInContext()->getCanonicalType();
    IGM.getTypeInfoForUnlowered(enumTy)
      .initializeMetadata(IGF, metadata, vwtable,
                          IGF.IGM.getLoweredType(enumTy));
  }
};
  
} // end anonymous namespace

void irgen::emitEnumMetadata(IRGenModule &IGM, EnumDecl *theEnum) {
  // Set up a dummy global to stand in for the metadata object while we produce
  // relative references.
  auto tempBase = createTemporaryRelativeAddressBase(IGM);

  // TODO: enums nested inside generic types
  llvm::Constant *init;
  
  bool isPattern;
  bool canBeConstant;
  if (theEnum->isGenericContext()) {
    GenericEnumMetadataBuilder builder(IGM, theEnum, tempBase.get());
    builder.layout();
    init = builder.getInit();
    isPattern = true;
    canBeConstant = false;

    maybeEmitNominalTypeMetadataAccessFunction(IGM, theEnum, builder);
  } else {
    EnumMetadataBuilder builder(IGM, theEnum, tempBase.get());
    builder.layout();
    init = builder.getInit();
    isPattern = false;
    canBeConstant = builder.canBeConstant();

    maybeEmitNominalTypeMetadataAccessFunction(IGM, theEnum, builder);
  }

  CanType declaredType = theEnum->getDeclaredType()->getCanonicalType();

  // For now, all type metadata is directly stored.
  bool isIndirect = false;
  
  IGM.defineTypeMetadata(declaredType, isIndirect, isPattern,
                         canBeConstant, init, std::move(tempBase));
}

llvm::Value *IRGenFunction::emitObjCSelectorRefLoad(StringRef selector) {
  llvm::Constant *loadSelRef = IGM.getAddrOfObjCSelectorRef(selector);
  llvm::Value *loadSel =
    Builder.CreateLoad(Address(loadSelRef, IGM.getPointerAlignment()));

  // When generating JIT'd code, we need to call sel_registerName() to force
  // the runtime to unique the selector. For non-JIT'd code, the linker will
  // do it for us.
  if (IGM.IRGen.Opts.UseJIT) {
    loadSel = Builder.CreateCall(IGM.getObjCSelRegisterNameFn(), loadSel);
  }

  return loadSel;
}

//===----------------------------------------------------------------------===//
// Foreign types
//===----------------------------------------------------------------------===//

namespace {
  /// A CRTP layout class for foreign class metadata.
  template <class Impl>
  class ForeignClassMetadataLayout
         : public MetadataLayout<Impl> {
    using super = MetadataLayout<Impl>;
  protected:
    ClassDecl *Target;
    using super::asImpl;
  public:
    ForeignClassMetadataLayout(IRGenModule &IGM, ClassDecl *target)
      : super(IGM), Target(target) {}

    void layout() {
      super::layout();
      asImpl().addSuperClass();
      asImpl().addReservedWord();
      asImpl().addReservedWord();
      asImpl().addReservedWord();
    }

    bool requiresInitializationFunction() {
      // TODO: superclasses?
      return false;
    }
           
    CanType getTargetType() const {
      return Target->getDeclaredType()->getCanonicalType();
    }
  };
  
  /// An adapter that turns a metadata layout class into a foreign metadata
  /// layout class. Foreign metadata has an additional header that
  template<typename Impl, typename Base>
  class ForeignMetadataBuilderBase : public Base {
    typedef Base super;
    
  protected:
    IRGenModule &IGM = super::IGM;
    using super::asImpl;

    template <class... T>
    ForeignMetadataBuilderBase(IRGenModule &IGM,
                               T &&...args)
      : super(IGM, std::forward<T>(args)...) {}

    Size AddressPoint = Size::invalid();

    bool computeUnfilledParent() {
      if (auto parentType = asImpl().getTargetType().getNominalParent()) {
        return !tryEmitConstantTypeMetadataRef(IGM, parentType,
                                               SymbolReferenceKind::Absolute);
      }
      return false;
    }

  public:
    void layout() {
      if (asImpl().requiresInitializationFunction())
        asImpl().addInitializationFunction();
      asImpl().addForeignName();
      asImpl().addUniquePointer();
      asImpl().addForeignFlags();
      super::layout();
    }
    
    void addForeignFlags() {
      int64_t flags = 0;
      if (asImpl().requiresInitializationFunction()) flags |= 1;
      asImpl().addConstantWord(flags);
    }

    void addForeignName() {
      CanType targetType = asImpl().getTargetType();
      asImpl().addWord(getMangledTypeName(IGM, targetType));
    }

    void addUniquePointer() {
      asImpl().addWord(llvm::ConstantPointerNull::get(IGM.TypeMetadataPtrTy));
    }

    void addInitializationFunction() {
      auto type = cast<NominalType>(asImpl().getTargetType());

      auto fnTy = llvm::FunctionType::get(IGM.VoidTy, {IGM.TypeMetadataPtrTy},
                                          /*variadic*/ false);
      llvm::Function *fn = llvm::Function::Create(fnTy,
                                           llvm::GlobalValue::PrivateLinkage,
                                           Twine("initialize_metadata_")
                                             + type->getDecl()->getName().str(),
                                           &IGM.Module);
      fn->setAttributes(IGM.constructInitialAttributes());
      
      // Set up the function.
      IRGenFunction IGF(IGM, fn);
      if (IGM.DebugInfo)
        IGM.DebugInfo->emitArtificialFunction(IGF, fn);

      // Emit the initialization.
      llvm::Value *metadata = IGF.collectParameters().claimNext();
      asImpl().emitInitialization(IGF, metadata);

      IGF.Builder.CreateRetVoid();

      asImpl().addWord(fn);
    }

    void noteAddressPoint() {
      AddressPoint = asImpl().getNextOffset();
    }

    Size getOffsetOfAddressPoint() const { return AddressPoint; }
  };

  /// A builder for ForeignClassMetadata.
  class ForeignClassMetadataBuilder :
    public ForeignMetadataBuilderBase<ForeignClassMetadataBuilder,
      ConstantBuilder<ForeignClassMetadataLayout<ForeignClassMetadataBuilder>>>{
  public:
    ForeignClassMetadataBuilder(IRGenModule &IGM, ClassDecl *target)
      : ForeignMetadataBuilderBase(IGM, target) {}

    void emitInitialization(IRGenFunction &IGF, llvm::Value *metadata) {
      // TODO: superclasses?
      llvm_unreachable("no supported forms of initialization");
    }

    // Visitor methods.

    void addValueWitnessTable() {
      // Without Objective-C interop, foreign classes must still use
      // Swift native reference counting.
      auto type = (IGM.ObjCInterop
                   ? IGM.Context.TheUnknownObjectType
                   : IGM.Context.TheNativeObjectType);
      auto wtable = IGM.getAddrOfValueWitnessTable(type);
      addWord(wtable);
    }

    void addMetadataFlags() {
      addConstantWord((unsigned) MetadataKind::ForeignClass);
    }

    void addSuperClass() {
      // TODO: superclasses
      addWord(llvm::ConstantPointerNull::get(IGM.TypeMetadataPtrTy));
    }

    void addReservedWord() {
      addWord(llvm::ConstantPointerNull::get(IGM.Int8PtrTy));
    }
  };
  
  /// A builder for ForeignStructMetadata.
  class ForeignStructMetadataBuilder :
    public ForeignMetadataBuilderBase<ForeignStructMetadataBuilder,
                      StructMetadataBuilderBase<ForeignStructMetadataBuilder>>
  {
    bool HasUnfilledParent = false;
  public:
    ForeignStructMetadataBuilder(IRGenModule &IGM, StructDecl *target,
                                 llvm::GlobalVariable *relativeAddressBase)
      : ForeignMetadataBuilderBase(IGM, target, relativeAddressBase) {
      HasUnfilledParent = computeUnfilledParent();
    }
    
    CanType getTargetType() const {
      return Target->getDeclaredType()->getCanonicalType();
    }

    bool requiresInitializationFunction() const {
      return HasUnfilledParent;
    }
    void emitInitialization(IRGenFunction &IGF, llvm::Value *metadata) {
      if (HasUnfilledParent) {
        auto parentType = getTargetType().getNominalParent();
        auto parentMetadata = IGF.emitTypeMetadataRef(parentType);

        int index = ValueTypeParentIndex;
        Address slot = emitAddressOfMetadataSlotAtIndex(IGF, metadata, index,
                                                    IGF.IGM.TypeMetadataPtrTy);
        IGF.Builder.CreateStore(parentMetadata, slot);
      }
    }

    void addValueWitnessTable() {
      auto type = this->Target->getDeclaredType()->getCanonicalType();
      addWord(emitValueWitnessTable(IGM, type));
    }

    void flagUnfilledParent() {
      assert(HasUnfilledParent);
    }

    void flagUnfilledFieldOffset() {
      llvm_unreachable("foreign type with non-fixed layout?");
    }
  };
  
  /// A builder for ForeignEnumMetadata.
  class ForeignEnumMetadataBuilder :
    public ForeignMetadataBuilderBase<ForeignEnumMetadataBuilder,
                      EnumMetadataBuilderBase<ForeignEnumMetadataBuilder>>
  {
    bool HasUnfilledParent = false;
  public:
    ForeignEnumMetadataBuilder(IRGenModule &IGM, EnumDecl *target,
                               llvm::GlobalVariable *relativeAddressBase)
      : ForeignMetadataBuilderBase(IGM, target, relativeAddressBase) {
      HasUnfilledParent = computeUnfilledParent();
    }
    
    CanType getTargetType() const {
      return Target->getDeclaredType()->getCanonicalType();
    }

    bool requiresInitializationFunction() const {
      return HasUnfilledParent;
    }
    void emitInitialization(IRGenFunction &IGF, llvm::Value *metadata) {
      if (HasUnfilledParent) {
        auto parentType = getTargetType().getNominalParent();
        auto parentMetadata = IGF.emitTypeMetadataRef(parentType);

        int index = ValueTypeParentIndex;
        Address slot = emitAddressOfMetadataSlotAtIndex(IGF, metadata, index,
                                                    IGF.IGM.TypeMetadataPtrTy);
        IGF.Builder.CreateStore(parentMetadata, slot);
      }
    }

    void addValueWitnessTable() {
      auto type = this->Target->getDeclaredType()->getCanonicalType();
      addWord(emitValueWitnessTable(IGM, type));
    }
    
    void addPayloadSize() const {
      llvm_unreachable("nongeneric enums shouldn't need payload size in metadata");
    }

    void flagUnfilledParent() {
      assert(HasUnfilledParent);
    }
  };
} // end anonymous namespace

llvm::Constant *
IRGenModule::getAddrOfForeignTypeMetadataCandidate(CanType type) {
  // What we save in GlobalVars is actually the offsetted value.
  auto entity = LinkEntity::forForeignTypeMetadataCandidate(type);
  if (auto entry = GlobalVars[entity])
    return entry;

  // Create a temporary base for relative references.
  auto tempBase = createTemporaryRelativeAddressBase(*this);
  
  // Compute the constant initializer and the offset of the type
  // metadata candidate within it.
  Size addressPoint;
  llvm::Constant *init;
  if (auto classType = dyn_cast<ClassType>(type)) {
    assert(!classType.getParent());
    auto classDecl = classType->getDecl();
    assert(classDecl->isForeign());

    ForeignClassMetadataBuilder builder(*this, classDecl);
    builder.layout();
    addressPoint = builder.getOffsetOfAddressPoint();
    init = builder.getInit();
  } else if (auto structType = dyn_cast<StructType>(type)) {
    auto structDecl = structType->getDecl();
    assert(structDecl->hasClangNode());
    
    ForeignStructMetadataBuilder builder(*this, structDecl, tempBase.get());
    builder.layout();
    addressPoint = builder.getOffsetOfAddressPoint();
    init = builder.getInit();
  } else if (auto enumType = dyn_cast<EnumType>(type)) {
    auto enumDecl = enumType->getDecl();
    assert(enumDecl->hasClangNode());
    
    ForeignEnumMetadataBuilder builder(*this, enumDecl, tempBase.get());
    builder.layout();
    addressPoint = builder.getOffsetOfAddressPoint();
    init = builder.getInit();
  } else {
    llvm_unreachable("foreign metadata for unexpected type?!");
  }
  
  // Create the global variable.
  LinkInfo link = LinkInfo::get(*this, entity, ForDefinition);
  auto var = link.createVariable(*this, init->getType(),
                                 getPointerAlignment());
  var->setInitializer(init);
  
  // Close the loop on relative references.
  replaceTemporaryRelativeAddressBase(*this, std::move(tempBase), var);

  // Apply the offset.
  llvm::Constant *result = var;
  result = llvm::ConstantExpr::getBitCast(result, Int8PtrTy);
  result = llvm::ConstantExpr::getInBoundsGetElementPtr(
      Int8Ty, result, getSize(addressPoint));
  result = llvm::ConstantExpr::getBitCast(result, TypeMetadataPtrTy);

  // Only remember the offset.
  GlobalVars[entity] = result;

  return result;
}

// Protocols

/// Get the runtime identifier for a special protocol, if any.
SpecialProtocol irgen::getSpecialProtocolID(ProtocolDecl *P) {
  auto known = P->getKnownProtocolKind();
  if (!known)
    return SpecialProtocol::None;
  switch (*known) {
  case KnownProtocolKind::AnyObject:
    return SpecialProtocol::AnyObject;
  case KnownProtocolKind::Error:
    return SpecialProtocol::Error;
    
  // The other known protocols aren't special at runtime.
  case KnownProtocolKind::Sequence:
  case KnownProtocolKind::IteratorProtocol:
  case KnownProtocolKind::RawRepresentable:
  case KnownProtocolKind::Equatable:
  case KnownProtocolKind::Hashable:
  case KnownProtocolKind::Comparable:
  case KnownProtocolKind::ObjectiveCBridgeable:
  case KnownProtocolKind::DestructorSafeContainer:
  case KnownProtocolKind::SwiftNewtypeWrapper:
  case KnownProtocolKind::ExpressibleByArrayLiteral:
  case KnownProtocolKind::ExpressibleByBooleanLiteral:
  case KnownProtocolKind::ExpressibleByDictionaryLiteral:
  case KnownProtocolKind::ExpressibleByExtendedGraphemeClusterLiteral:
  case KnownProtocolKind::ExpressibleByFloatLiteral:
  case KnownProtocolKind::ExpressibleByIntegerLiteral:
  case KnownProtocolKind::ExpressibleByStringInterpolation:
  case KnownProtocolKind::ExpressibleByStringLiteral:
  case KnownProtocolKind::ExpressibleByNilLiteral:
  case KnownProtocolKind::ExpressibleByUnicodeScalarLiteral:
  case KnownProtocolKind::ExpressibleByColorLiteral:
  case KnownProtocolKind::ExpressibleByImageLiteral:
  case KnownProtocolKind::ExpressibleByFileReferenceLiteral:
  case KnownProtocolKind::ExpressibleByBuiltinBooleanLiteral:
  case KnownProtocolKind::ExpressibleByBuiltinExtendedGraphemeClusterLiteral:
  case KnownProtocolKind::ExpressibleByBuiltinFloatLiteral:
  case KnownProtocolKind::ExpressibleByBuiltinIntegerLiteral:
  case KnownProtocolKind::ExpressibleByBuiltinStringLiteral:
  case KnownProtocolKind::ExpressibleByBuiltinUTF16StringLiteral:
  case KnownProtocolKind::ExpressibleByBuiltinUnicodeScalarLiteral:
  case KnownProtocolKind::OptionSet:
  case KnownProtocolKind::BridgedNSError:
  case KnownProtocolKind::BridgedStoredNSError:
  case KnownProtocolKind::ErrorCodeProtocol:
    return SpecialProtocol::None;
  }

  llvm_unreachable("Not a valid KnownProtocolKind.");
}

namespace {
  const unsigned NumProtocolDescriptorFields = 13;

  class ProtocolDescriptorBuilder : public ConstantBuilder<> {
    ProtocolDecl *Protocol;
    SILDefaultWitnessTable *DefaultWitnesses;

  public:
    ProtocolDescriptorBuilder(IRGenModule &IGM, ProtocolDecl *protocol,
                              SILDefaultWitnessTable *defaultWitnesses)
      : ConstantBuilder(IGM), Protocol(protocol),
        DefaultWitnesses(defaultWitnesses) {}

    void layout() {
      addObjCCompatibilityIsa();
      addName();
      addInherited();
      addObjCCompatibilityTables();
      addSize();
      addFlags();
      addDefaultWitnessTable();
    }

    void addObjCCompatibilityIsa() {
      // The ObjC runtime will drop a reference to its magic Protocol class
      // here.
      addWord(llvm::ConstantPointerNull::get(IGM.Int8PtrTy));
    }
    
    void addName() {
      // Include the _Tt prefix. Since Swift protocol descriptors are laid
      // out to look like ObjC Protocol* objects, the name has to clearly be
      // a Swift mangled name.

      IRGenMangler mangler;
      std::string Name =
        mangler.mangleForProtocolDescriptor(Protocol->getDeclaredType());

      auto global = IGM.getAddrOfGlobalString(Name);
      addWord(global);
    }
    
    void addInherited() {
      // If there are no inherited protocols, produce null.
      auto inherited = Protocol->getInheritedProtocols();
      if (inherited.empty()) {
        addWord(llvm::ConstantPointerNull::get(IGM.Int8PtrTy));
        return;
      }
      
      // Otherwise, collect references to all of the inherited protocol
      // descriptors.
      SmallVector<llvm::Constant*, 4> inheritedDescriptors;
      inheritedDescriptors.push_back(IGM.getSize(Size(inherited.size())));
      
      for (ProtocolDecl *p : inherited) {
        auto descriptor = IGM.getAddrOfProtocolDescriptor(p, NotForDefinition,
                                                          nullptr);
        inheritedDescriptors.push_back(descriptor);
      }
      
      auto inheritedInit = llvm::ConstantStruct::getAnon(inheritedDescriptors);
      auto inheritedVar = new llvm::GlobalVariable(IGM.Module,
                                           inheritedInit->getType(),
                                           /*isConstant*/ true,
                                           llvm::GlobalValue::PrivateLinkage,
                                           inheritedInit);
      
      llvm::Constant *inheritedVarPtr
        = llvm::ConstantExpr::getBitCast(inheritedVar, IGM.Int8PtrTy);
      addWord(inheritedVarPtr);
    }
    
    void addObjCCompatibilityTables() {
      // Required instance methods
      addWord(llvm::ConstantPointerNull::get(IGM.Int8PtrTy));
      // Required class methods
      addWord(llvm::ConstantPointerNull::get(IGM.Int8PtrTy));
      // Optional instance methods
      addWord(llvm::ConstantPointerNull::get(IGM.Int8PtrTy));
      // Optional class methods
      addWord(llvm::ConstantPointerNull::get(IGM.Int8PtrTy));
      // Properties
      addWord(llvm::ConstantPointerNull::get(IGM.Int8PtrTy));
    }
    
    void addSize() {
      // The number of fields so far in words, plus 4 bytes for size and
      // 4 bytes for flags.
      addConstantInt32(getNextOffset().getValue() + 4 + 4);
    }
    
    void addFlags() {
      auto flags = ProtocolDescriptorFlags()
        .withSwift(true)
        .withClassConstraint(Protocol->requiresClass()
                               ? ProtocolClassConstraint::Class
                               : ProtocolClassConstraint::Any)
        .withDispatchStrategy(
                Lowering::TypeConverter::getProtocolDispatchStrategy(Protocol))
        .withSpecialProtocol(getSpecialProtocolID(Protocol));

      if (DefaultWitnesses)
        flags = flags.withResilient(true);

      addConstantInt32(flags.getIntValue());
    }

    void addDefaultWitnessTable() {
      // The runtime ignores these fields if the IsResilient flag is not set.
      if (DefaultWitnesses) {
        addConstantInt16(DefaultWitnesses->getMinimumWitnessTableSize());
        addConstantInt16(DefaultWitnesses->getDefaultWitnessTableSize());

        // Unused padding
        addConstantInt32(0);

        for (auto entry : DefaultWitnesses->getResilientDefaultEntries()) {
          addWord(IGM.getAddrOfSILFunction(entry.getWitness(), NotForDefinition));
        }
      } else {
        addConstantInt16(0);
        addConstantInt16(0);

        // Unused padding
        addConstantInt32(0);
      }
    }

    llvm::Constant *getInit() {
      return getInitWithSuggestedType(NumProtocolDescriptorFields,
                                      IGM.ProtocolDescriptorStructTy);
    }
  };
} // end anonymous namespace

/// Emit global structures associated with the given protocol. This comprises
/// the protocol descriptor, and for ObjC interop, references to the descriptor
/// that the ObjC runtime uses for uniquing.
void IRGenModule::emitProtocolDecl(ProtocolDecl *protocol) {
  // Emit remote reflection metadata for the protocol.
  emitFieldMetadataRecord(protocol);

  // If the protocol is Objective-C-compatible, go through the path that
  // produces an ObjC-compatible protocol_t.
  if (protocol->isObjC()) {
    // In JIT mode, we need to create protocol descriptors using the ObjC
    // runtime in JITted code.
    if (IRGen.Opts.UseJIT)
      return;
    
    // Native ObjC protocols are emitted on-demand in ObjC and uniqued by the
    // runtime; we don't need to try to emit a unique descriptor symbol for them.
    if (protocol->hasClangNode())
      return;
    
    getObjCProtocolGlobalVars(protocol);
    return;
  }

  SILDefaultWitnessTable *defaultWitnesses = nullptr;
  if (!protocol->hasFixedLayout())
    defaultWitnesses = getSILModule().lookUpDefaultWitnessTable(protocol);
  ProtocolDescriptorBuilder builder(*this, protocol, defaultWitnesses);
  builder.layout();

  auto init = builder.getInit();
  auto var = cast<llvm::GlobalVariable>(
                       getAddrOfProtocolDescriptor(protocol, ForDefinition,
                                                   init->getType()));
  var->setConstant(true);
  var->setInitializer(init);
}

/// \brief Load a reference to the protocol descriptor for the given protocol.
///
/// For Swift protocols, this is a constant reference to the protocol descriptor
/// symbol.
/// For ObjC protocols, descriptors are uniqued at runtime by the ObjC runtime.
/// We need to load the unique reference from a global variable fixed up at
/// startup.
llvm::Value *irgen::emitProtocolDescriptorRef(IRGenFunction &IGF,
                                              ProtocolDecl *protocol) {
  if (!protocol->isObjC())
    return IGF.IGM.getAddrOfProtocolDescriptor(protocol, NotForDefinition,
                                               nullptr);
  
  auto refVar = IGF.IGM.getAddrOfObjCProtocolRef(protocol, NotForDefinition);
  llvm::Value *val
    = IGF.Builder.CreateLoad(refVar, IGF.IGM.getPointerAlignment());
  val = IGF.Builder.CreateBitCast(val,
                          IGF.IGM.ProtocolDescriptorStructTy->getPointerTo());
  return val;
}

//===----------------------------------------------------------------------===//
// Other metadata.
//===----------------------------------------------------------------------===//

llvm::Value *irgen::emitMetatypeInstanceType(IRGenFunction &IGF,
                                             llvm::Value *metatypeMetadata) {
  // The instance type field of MetatypeMetadata is immediately after
  // the isa field.
  return emitInvariantLoadFromMetadataAtIndex(IGF, metatypeMetadata, 1,
                                     IGF.IGM.TypeMetadataPtrTy);
}
