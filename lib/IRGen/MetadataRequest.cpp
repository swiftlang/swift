//===--- MetadataRequest.cpp - IR generation for metadata requests --------===//
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
//  This file implements IR generation for accessing metadata.
//
//===----------------------------------------------------------------------===//

#include "MetadataRequest.h"

#include "ConstantBuilder.h"
#include "Explosion.h"
#include "FixedTypeInfo.h"
#include "GenericRequirement.h"
#include "GenArchetype.h"
#include "GenClass.h"
#include "GenMeta.h"
#include "GenProto.h"
#include "GenType.h"
#include "IRGenDebugInfo.h"
#include "IRGenFunction.h"
#include "IRGenMangler.h"
#include "IRGenModule.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/ExistentialLayout.h"
#include "swift/AST/IRGenOptions.h"
#include "swift/SIL/TypeLowering.h"

using namespace swift;
using namespace irgen;

llvm::Value *DynamicMetadataRequest::get(IRGenFunction &IGF) const {
  if (isStatic()) {
    return IGF.IGM.getSize(Size(StaticRequest.getOpaqueValue()));
  } else {
    return DynamicRequest;
  }
}

MetadataResponse
MetadataResponse::split(IRGenFunction &IGF, DynamicMetadataRequest request,
                        llvm::Value *response) {
  if (request.isStaticallyBlockingComplete()) {
    assert(response->getType() == IGF.IGM.TypeMetadataResponseTy);
    auto value = IGF.Builder.CreateExtractValue(response, 0);
    return MetadataResponse(value);
  } else {
    return split(IGF, response);
  }
}

MetadataResponse
MetadataResponse::split(IRGenFunction &IGF, llvm::Value *response) {
  assert(response->getType() == IGF.IGM.TypeMetadataResponseTy);
  auto value = IGF.Builder.CreateExtractValue(response, 0);
  auto state = IGF.Builder.CreateExtractValue(response, 1);
  return MetadataResponse(value, state);
}

llvm::Value *MetadataResponse::combine(IRGenFunction &IGF) const {
  assert(isValid());
  llvm::Value *pair =
    llvm::UndefValue::get(IGF.IGM.TypeMetadataResponseTy);
  pair = IGF.Builder.CreateInsertValue(pair, Metadata, 0);
  pair = IGF.Builder.CreateInsertValue(pair, getDynamicState(IGF), 1);
  return pair;
}

llvm::Value *MetadataResponse::getDynamicState(IRGenFunction &IGF) const {
  assert(isValid());
  return State ? State : getCompletedState(IGF.IGM);
}

llvm::Constant *MetadataResponse::getCompletedState(IRGenModule &IGM) {
  return IGM.getSize(Size(MetadataRequest::Complete));
}

llvm::Constant *IRGenModule::getAddrOfStringForTypeRef(StringRef str) {
  return getAddrOfStringForTypeRef(SymbolicMangling{str,{}});
}

llvm::Constant *IRGenModule::getAddrOfStringForTypeRef(
                                             const SymbolicMangling &mangling) {
  // Create a symbol name for the symbolic mangling. This is used as the
  // uniquing key both for ODR coalescing and within this TU.
  IRGenMangler mangler;
  std::string symbolName =
    mangler.mangleSymbolNameForSymbolicMangling(mangling);

  // See if we emitted the constant already.
  auto &entry = StringsForTypeRef[symbolName];
  if (entry.second)
    return entry.second;
  
  ConstantInitBuilder B(*this);
  auto S = B.beginStruct();
  S.setPacked(true);
  
  unsigned pos = 0;
  for (auto &symbolic : mangling.SymbolicReferences) {
    assert(symbolic.second >= pos
           && "references should be ordered");
    if (symbolic.second != pos) {
      // Emit the preceding literal chunk.
      auto literalChunk = StringRef(mangling.String.data() + pos,
                                    symbolic.second - pos);
      assert(literalChunk.back() == '\1' && "should be prefixed with \\1");
      auto literal = llvm::ConstantDataArray::getString(getLLVMContext(),
                                                        literalChunk,
                                                        /*null*/ false);
      S.add(literal);
    }
    
    // The symbolic reference is to the type context descriptor of the
    // referenced type.
    // We currently only allow symbolic references to nominal type contexts.
    auto nominal = cast<NominalTypeDecl>(symbolic.first);
    S.addRelativeAddress(
         getAddrOfTypeContextDescriptor(const_cast<NominalTypeDecl*>(nominal),
                                        DontRequireMetadata));
    
    pos = symbolic.second + 4;
  }
  
  // Add the last literal bit, if any.
  if (pos != mangling.String.size()) {
    auto literalChunk = StringRef(mangling.String.data() + pos,
                                  mangling.String.size() - pos);
    auto literal = llvm::ConstantDataArray::getString(getLLVMContext(),
                                                      literalChunk,
                                                      /*null*/ false);
    S.add(literal);
  }
  
  // And a null terminator!
  S.addInt(Int8Ty, 0);
  
  auto finished = S.finishAndCreateFuture();
  auto var = new llvm::GlobalVariable(Module, finished.getType(),
                                      /*constant*/ true,
                                      llvm::GlobalValue::LinkOnceODRLinkage,
                                      nullptr,
                                      symbolName);
  var->setVisibility(llvm::GlobalValue::HiddenVisibility);
  var->setAlignment(1);
  setTrueConstGlobal(var);
  var->setSection(getReflectionTypeRefSectionName());
  
  finished.installInGlobal(var);
  
  // Drill down to the i8* at the beginning of the constant.
  auto addr = llvm::ConstantExpr::getBitCast(var, Int8PtrTy);
  entry = {var, addr};
  
  return addr;
}

// FIXME: willBeRelativelyAddressed is only needed to work around an ld64 bug
// resolving relative references to coalesceable symbols.
// It should be removed when fixed. rdar://problem/22674524
llvm::Constant *irgen::getTypeRef(IRGenModule &IGM, CanType type) {
  IRGenMangler Mangler;
  auto SymbolicName = Mangler.mangleTypeForReflection(IGM, type,
                                                    IGM.getSwiftModule(),
                                                    /*single-field box*/ false);
  
  return IGM.getAddrOfStringForTypeRef(SymbolicName);
}

llvm::Value *irgen::emitObjCMetadataRefForMetadata(IRGenFunction &IGF,
                                                   llvm::Value *classPtr) {
  assert(IGF.IGM.Context.LangOpts.EnableObjCInterop);
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
  struct GenericArguments {
    /// The values to use to initialize the arguments structure.
    SmallVector<llvm::Value *, 8> Values;
    SmallVector<llvm::Type *, 8> Types;

    static unsigned getNumGenericArguments(IRGenModule &IGM,
                                           NominalTypeDecl *nominal) {
      GenericTypeRequirements requirements(IGM, nominal);
      return requirements.getNumTypeRequirements();
    }

    void collectTypes(IRGenModule &IGM, NominalTypeDecl *nominal) {
      GenericTypeRequirements requirements(IGM, nominal);
      collectTypes(IGM, requirements);
    }

    void collectTypes(IRGenModule &IGM,
                      const GenericTypeRequirements &requirements) {
      for (auto &requirement : requirements.getRequirements()) {
        if (requirement.Protocol) {
          Types.push_back(IGM.WitnessTablePtrTy);
        } else {
          Types.push_back(IGM.TypeMetadataPtrTy);
        }
      }
    }

    void collect(IRGenFunction &IGF, CanType type) {
      auto *decl = type.getNominalOrBoundGenericNominal();
      GenericTypeRequirements requirements(IGF.IGM, decl);

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

  return IGM.getAddrOfTypeMetadata(type);
}

/// Attempts to return a constant type metadata reference for a
/// nominal type.
ConstantReference
irgen::tryEmitConstantTypeMetadataRef(IRGenModule &IGM, CanType type,
                                      SymbolReferenceKind refKind) {
  if (!isTypeMetadataAccessTrivial(IGM, type))
    return ConstantReference();

  return IGM.getAddrOfTypeMetadata(type, refKind);
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
llvm::Value *irgen::uniqueForeignTypeMetadataRef(IRGenFunction &IGF,
                                                 llvm::Value *candidate) {
  auto call = IGF.Builder.CreateCall(IGF.IGM.getGetForeignTypeMetadataFn(),
                                     candidate);
  call->addAttribute(llvm::AttributeList::FunctionIndex,
                     llvm::Attribute::NoUnwind);
  call->addAttribute(llvm::AttributeList::FunctionIndex,
                     llvm::Attribute::ReadNone);
  return call;
}

/// Emit a reference to the type metadata for a foreign type.
static llvm::Value *emitForeignTypeMetadataRef(IRGenFunction &IGF,
                                               CanType type) {
  llvm::Value *candidate = IGF.IGM.getAddrOfForeignTypeMetadataCandidate(type);
  return uniqueForeignTypeMetadataRef(IGF, candidate);
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
    return IGF.IGM.getAddrOfTypeMetadata(theType);
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
  assert((!genericArgs.Values.empty() ||
          theDecl->getGenericSignature()->areAllParamsConcrete()) &&
         "no generic args?!");

  // Call the generic metadata accessor function.
  llvm::Function *accessor =
      IGF.IGM.getAddrOfGenericTypeMetadataAccessFunction(theDecl,
                                                         genericArgs.Types,
                                                         NotForDefinition);

  DynamicMetadataRequest request = MetadataRequest::Complete;

  auto result =
    IGF.emitGenericTypeMetadataAccessFunctionCall(accessor, genericArgs.Values,
                                                  request);

  IGF.setScopedLocalTypeMetadata(theType, result);

  // FIXME: propagate response
  return result.getMetadata();
}

/// Is it basically trivial to access the given metadata?  If so, we don't
/// need a cache variable in its accessor.
bool irgen::isTypeMetadataAccessTrivial(IRGenModule &IGM, CanType type) {
  assert(!type->hasArchetype());

  // Value type metadata only requires dynamic initialization on first
  // access if it contains a resilient type.
  if (isa<StructType>(type) || isa<EnumType>(type)) {
    auto nominalType = cast<NominalType>(type);
    auto *nominalDecl = nominalType->getDecl();

    // Imported type metadata always requires an accessor.
    if (isa<ClangModuleUnit>(nominalDecl->getModuleScopeContext()))
      return false;

    // Generic type metadata always requires an accessor.
    if (nominalDecl->isGenericContext())
      return false;

    // Resiliently-sized metadata access always requires an accessor.
    return (IGM.getTypeInfoForUnlowered(type).isFixedSize());
  }

  // The empty tuple type has a singleton metadata.
  if (auto tuple = dyn_cast<TupleType>(type))
    return tuple->getNumElements() == 0;
  
  // Any and AnyObject have singleton metadata.
  if (type->isAny() || type->isAnyObject())
    return true;

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

/// Return the standard access strategy for getting a non-dependent
/// type metadata object.
MetadataAccessStrategy irgen::getTypeMetadataAccessStrategy(CanType type) {
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
      return IGF.IGM.getAddrOfTypeMetadata(type);
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

        TupleTypeFlags flags =
          TupleTypeFlags().withNumElements(elements.size());
        llvm::Value *args[] = {
          llvm::ConstantInt::get(IGF.IGM.SizeTy, flags.getIntValue()),
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

    llvm::Value *getFunctionParameterRef(AnyFunctionType::CanParam &param) {
      auto type = param.getType();
      if (param.getParameterFlags().isInOut())
        type = type->getInOutObjectType()->getCanonicalType();
      return IGF.emitTypeMetadataRef(type);
    }

    llvm::Value *visitFunctionType(CanFunctionType type) {
      if (auto metatype = tryGetLocal(type))
        return metatype;

      auto result =
          IGF.emitTypeMetadataRef(type->getResult()->getCanonicalType());

      auto params = type.getParams();
      auto numParams = params.size();

      bool hasFlags = false;
      for (auto param : params) {
        if (!param.getParameterFlags().isNone()) {
          hasFlags = true;
          break;
        }
      }

      // Map the convention to a runtime metadata value.
      FunctionMetadataConvention metadataConvention;
      bool isEscaping = false;
      switch (type->getRepresentation()) {
      case FunctionTypeRepresentation::Swift:
        metadataConvention = FunctionMetadataConvention::Swift;
        isEscaping = !type->isNoEscape();
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
                          .withNumParameters(numParams)
                          .withConvention(metadataConvention)
                          .withThrows(type->throws())
                          .withParameterFlags(hasFlags)
                          .withEscaping(isEscaping);

      auto flags = llvm::ConstantInt::get(IGF.IGM.SizeTy,
                                          flagsVal.getIntValue());

      auto collectParameters =
          [&](llvm::function_ref<void(unsigned, llvm::Value *,
                                      ParameterFlags flags)>
                  processor) {
            for (auto index : indices(params)) {
              auto param = params[index];
              auto flags = param.getParameterFlags();

              auto parameterFlags =
                  ParameterFlags()
                      .withValueOwnership(flags.getValueOwnership())
                      .withVariadic(flags.isVariadic());

              processor(index, getFunctionParameterRef(param), parameterFlags);
            }
          };

      auto constructSimpleCall =
          [&](llvm::SmallVectorImpl<llvm::Value *> &arguments)
          -> llvm::Constant * {
        arguments.push_back(flags);

        collectParameters([&](unsigned i, llvm::Value *typeRef,
                              ParameterFlags flags) {
          arguments.push_back(typeRef);
          if (hasFlags)
            arguments.push_back(
                llvm::ConstantInt::get(IGF.IGM.Int32Ty, flags.getIntValue()));
        });

        arguments.push_back(result);

        switch (params.size()) {
        case 0:
          return IGF.IGM.getGetFunctionMetadata0Fn();

        case 1:
          return IGF.IGM.getGetFunctionMetadata1Fn();

        case 2:
          return IGF.IGM.getGetFunctionMetadata2Fn();

        case 3:
          return IGF.IGM.getGetFunctionMetadata3Fn();

        default:
          llvm_unreachable("supports only 1/2/3 parameter functions");
        }
      };

      switch (numParams) {
      case 0:
      case 1:
      case 2:
      case 3: {
        if (!hasFlags) {
          llvm::SmallVector<llvm::Value *, 8> arguments;
          auto *metadataFn = constructSimpleCall(arguments);
          auto *call = IGF.Builder.CreateCall(metadataFn, arguments);
          call->setDoesNotThrow();
          return setLocal(CanType(type), call);
        }

        // If function type has parameter flags, let's emit
        // the most general function to retrieve them.
        LLVM_FALLTHROUGH;
      }

      default:
        assert(!params.empty() && "0 parameter case is specialized!");

        auto *const Int32Ptr = IGF.IGM.Int32Ty->getPointerTo();
        llvm::SmallVector<llvm::Value *, 8> arguments;

        arguments.push_back(flags);

        ConstantInitBuilder paramFlags(IGF.IGM);
        auto flagsArr = paramFlags.beginArray();

        auto arrayTy =
            llvm::ArrayType::get(IGF.IGM.TypeMetadataPtrTy, numParams);
        Address parameters = IGF.createAlloca(
            arrayTy, IGF.IGM.getTypeMetadataAlignment(), "function-parameters");

        IGF.Builder.CreateLifetimeStart(parameters,
                                        IGF.IGM.getPointerSize() * numParams);

        collectParameters([&](unsigned i, llvm::Value *typeRef,
                              ParameterFlags flags) {
          auto argPtr = IGF.Builder.CreateStructGEP(parameters, i,
                                                    IGF.IGM.getPointerSize());
          IGF.Builder.CreateStore(typeRef, argPtr);
          if (i == 0)
            arguments.push_back(argPtr.getAddress());

          if (hasFlags)
            flagsArr.addInt32(flags.getIntValue());
        });

        if (hasFlags) {
          auto *flagsVar = flagsArr.finishAndCreateGlobal(
              "parameter-flags", IGF.IGM.getPointerAlignment(),
              /* constant */ true);
          arguments.push_back(IGF.Builder.CreateBitCast(flagsVar, Int32Ptr));
        } else {
          flagsArr.abandon();
          arguments.push_back(llvm::ConstantPointerNull::get(Int32Ptr));
        }

        arguments.push_back(result);

        auto call = IGF.Builder.CreateCall(IGF.IGM.getGetFunctionMetadataFn(),
                                           arguments);
        call->setDoesNotThrow();

        if (parameters.isValid())
          IGF.Builder.CreateLifetimeEnd(parameters,
                                        IGF.IGM.getPointerSize() * numParams);

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
      if (auto metatype = tryGetLocal(type))
        return metatype;

      // Any and AnyObject have singleton metadata in the runtime.
      llvm::Constant *singletonMetadata = nullptr;
      if (type->isAny())
        singletonMetadata = IGF.IGM.getAnyExistentialMetadata();
      if (type->isAnyObject())
        singletonMetadata = IGF.IGM.getAnyObjectExistentialMetadata();
      
      if (singletonMetadata) {
        llvm::Constant *indices[] = {
          llvm::ConstantInt::get(IGF.IGM.Int32Ty, 0),
          llvm::ConstantInt::get(IGF.IGM.Int32Ty, 1)
        };
        return llvm::ConstantExpr::getInBoundsGetElementPtr(
            /*Ty=*/nullptr, singletonMetadata, indices);
      }

      auto layout = type.getExistentialLayout();
      
      auto protocols = layout.getProtocols();

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
      for (auto *protoTy : protocols) {
        auto *protoDecl = protoTy->getDecl();
        llvm::Value *ref = emitProtocolDescriptorRef(IGF, protoDecl);
        Address slot = IGF.Builder.CreateConstArrayGEP(descriptorArray,
                                               index, IGF.IGM.getPointerSize());
        IGF.Builder.CreateStore(ref, slot);
        ++index;
      }

      // Note: ProtocolClassConstraint::Class is 0, ::Any is 1.
      auto classConstraint =
        llvm::ConstantInt::get(IGF.IGM.Int1Ty,
                               !layout.requiresClass());
      llvm::Value *superclassConstraint =
        llvm::ConstantPointerNull::get(IGF.IGM.TypeMetadataPtrTy);
      if (layout.superclass) {
        superclassConstraint = IGF.emitTypeMetadataRef(
          CanType(layout.superclass));
      }

      auto call = IGF.Builder.CreateCall(IGF.IGM.getGetExistentialMetadataFn(),
                                         {classConstraint,
                                          superclassConstraint,
                                          IGF.IGM.getSize(Size(protocols.size())),
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
    llvm::Value *visitSILTokenType(CanSILTokenType type) {
      llvm_unreachable("should not be asking for metadata of a SILToken type");
    }

    llvm::Value *visitArchetypeType(CanArchetypeType type) {
      return emitArchetypeTypeMetadataRef(IGF, type, MetadataRequest::Complete)
               .getMetadata();
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
                                        LazyCacheEmitter getValue,
                                        bool isReadNone) {
  accessor->setDoesNotThrow();

  // This function is logically 'readnone': the caller does not need
  // to reason about any side effects or stores it might perform.
  if (isReadNone)
    accessor->setDoesNotAccessMemory();

  IRGenFunction IGF(IGM, accessor);
  if (IGM.DebugInfo)
    IGM.DebugInfo->emitArtificialFunction(IGF, accessor);

  auto parameters = IGF.collectParameters();

  bool returnsResponse =
    (accessor->getReturnType() == IGM.TypeMetadataResponseTy);

  // If there's no cache variable, just perform the direct access.
  if (cacheVariable == nullptr) {
    auto response = getValue(IGF, parameters);
    llvm::Value *ret;
    if (returnsResponse) {
      ret = response.combine(IGF);
    } else {
      assert(response.isStaticallyKnownComplete());
      ret = response.getMetadata();
    }
    IGF.Builder.CreateRet(ret);
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
  if (IGM.IRGen.Opts.Sanitizers & SanitizerKind::Thread)
    load->setOrdering(llvm::AtomicOrdering::Acquire);

  // Compare the load result against null.
  auto isNullBB = IGF.createBasicBlock("cacheIsNull");
  auto contBB = IGF.createBasicBlock("cont");
  llvm::Value *comparison = IGF.Builder.CreateICmpEQ(load, null);
  IGF.Builder.CreateCondBr(comparison, isNullBB, contBB);
  auto loadBB = IGF.Builder.GetInsertBlock();

  // If the load yielded null, emit the type metadata.
  IGF.Builder.emitBlock(isNullBB);
  MetadataResponse response = getValue(IGF, parameters);
  auto directResult = response.getMetadata();

  llvm::Constant *completedState =
    (returnsResponse ? MetadataResponse::getCompletedState(IGM) : nullptr);

  // Skip caching if we're working with responses and the fetched result
  // is statically known to be complete.
  llvm::BasicBlock *completionCheckBB = nullptr;
  llvm::Value *directState = nullptr;
  if (returnsResponse && !response.isStaticallyKnownComplete()) {
    completionCheckBB = IGF.Builder.GetInsertBlock();
    directState = response.getDynamicState(IGF);

    auto isCompleteBB = IGF.createBasicBlock("is_complete");
    auto isComplete =
      IGF.Builder.CreateICmpEQ(directState, completedState);

    IGF.Builder.CreateCondBr(isComplete, isCompleteBB, contBB);
    IGF.Builder.emitBlock(isCompleteBB);
  } 

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

  // Add a phi for the metadata value.
  auto phi = IGF.Builder.CreatePHI(null->getType(), 3);
  phi->addIncoming(load, loadBB);
  phi->addIncoming(directResult, storeBB);

  // Add a phi for the metadata state if we're returning a response.
  llvm::Value *stateToReturn = nullptr;
  if (directState) {
    phi->addIncoming(directResult, completionCheckBB);

    auto completionStatePHI = IGF.Builder.CreatePHI(IGM.SizeTy, 3);
    completionStatePHI->addIncoming(completedState, loadBB);
    completionStatePHI->addIncoming(directState, completionCheckBB);
    completionStatePHI->addIncoming(completedState, storeBB);
    stateToReturn = completionStatePHI;
  } else if (returnsResponse) {
    stateToReturn = completedState;
  }

  // Build the return value.
  llvm::Value *ret;
  if (returnsResponse) {
    ret = MetadataResponse(phi, stateToReturn).combine(IGF);
  } else {
    ret = phi;
  }

  IGF.Builder.CreateRet(ret);
}

MetadataResponse
IRGenFunction::emitGenericTypeMetadataAccessFunctionCall(
                                              llvm::Function *accessFunction,
                                              ArrayRef<llvm::Value *> args,
                                              DynamicMetadataRequest request) {

  SmallVector<llvm::Value *, 8> callArgs;

  // Add the metadata request argument.
  callArgs.push_back(request.get(*this));

  Address argsBuffer;
  bool allocatedArgsBuffer = false;
  if (args.size() > NumDirectGenericTypeMetadataAccessFunctionArgs) {
    // Allocate an array to pass the arguments.
    auto argsBufferTy = llvm::ArrayType::get(IGM.Int8PtrTy, args.size());
    argsBuffer = createAlloca(argsBufferTy, IGM.getPointerAlignment());

    // Mark the beginning of the array lifetime.
    Builder.CreateLifetimeStart(argsBuffer,
                                IGM.getPointerSize() * args.size());
    allocatedArgsBuffer = true;

    // Fill in the buffer.
    for (unsigned i : indices(args)) {
      Address elt = Builder.CreateStructGEP(argsBuffer, i,
                                            IGM.getPointerSize() * i);
      auto *arg =
        Builder.CreateBitCast(args[i], elt.getType()->getPointerElementType());
      Builder.CreateStore(arg, elt);
    }

    // Add the buffer to the call arguments.
    callArgs.push_back(
      Builder.CreateBitCast(argsBuffer.getAddress(), IGM.Int8PtrPtrTy));
  } else {
    callArgs.append(args.begin(), args.end());
  }

  auto call = Builder.CreateCall(accessFunction, callArgs);
  call->setDoesNotThrow();
  call->setCallingConv(IGM.SwiftCC);
  call->addAttribute(llvm::AttributeList::FunctionIndex,
                     allocatedArgsBuffer
                       ? llvm::Attribute::InaccessibleMemOrArgMemOnly
                       : llvm::Attribute::ReadNone);

  // If we allocated a buffer for the arguments, end its lifetime.
  if (allocatedArgsBuffer)
    Builder.CreateLifetimeEnd(argsBuffer, IGM.getPointerSize() * args.size());

  return MetadataResponse::split(*this, request, call);
}

static MetadataResponse
emitGenericTypeMetadataAccessFunction(IRGenFunction &IGF,
                                      Explosion &params,
                                      NominalTypeDecl *nominal,
                                      GenericArguments &genericArgs) {
  llvm::Value *descriptor =
    IGF.IGM.getAddrOfTypeContextDescriptor(nominal, RequireMetadata);

  auto request = params.claimNext();

  auto numArguments = genericArgs.Types.size();

  bool allocatedBuffer = false;
  Address argsBuffer;
  if (numArguments > NumDirectGenericTypeMetadataAccessFunctionArgs) {
    // The caller provided a buffer with enough space for all of the arguments;
    // use that.
    argsBuffer = Address(params.claimNext(), IGF.IGM.getPointerAlignment());
  } else {
    // Allocate a buffer with enough storage for the arguments.
    auto argsBufferTy =
      llvm::StructType::get(IGF.IGM.LLVMContext, genericArgs.Types);
    argsBuffer = IGF.createAlloca(argsBufferTy,
                                  IGF.IGM.getPointerAlignment(),
                                  "generic.arguments");
    IGF.Builder.CreateLifetimeStart(argsBuffer,
                            IGF.IGM.getPointerSize() * genericArgs.Values.size());
    allocatedBuffer = true;

    // Store direct arguments into the buffer.
    for (auto i : range(numArguments)) {
      Address elt = IGF.Builder.CreateStructGEP(argsBuffer, i,
                                                IGF.IGM.getPointerSize() * i);

      auto *arg =
        IGF.Builder.CreateBitCast(params.claimNext(),
                                  elt.getType()->getPointerElementType());
      IGF.Builder.CreateStore(arg, elt);
    }
  }

  llvm::Value *arguments =
    IGF.Builder.CreateBitCast(argsBuffer.getAddress(), IGF.IGM.Int8PtrTy);

  // Make the call.
  auto result = IGF.Builder.CreateCall(IGF.IGM.getGetGenericMetadataFn(),
                                       {request, arguments, descriptor});
  result->setDoesNotThrow();
  result->setCallingConv(IGF.IGM.SwiftCC);
  result->addAttribute(llvm::AttributeList::FunctionIndex,
                       llvm::Attribute::ReadOnly);

  // If we allocated the array ourselves, end its lifetime.
  if (allocatedBuffer) {
    IGF.Builder.CreateLifetimeEnd(argsBuffer,
                          IGF.IGM.getPointerSize() * genericArgs.Values.size());
  }

  return MetadataResponse::split(IGF, result);
}

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
  if (IGM.IRGen.Opts.Sanitizers & SanitizerKind::Thread)
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
MetadataResponse
irgen::emitInPlaceTypeMetadataAccessFunctionBody(IRGenFunction &IGF,
                                                 CanNominalType type,
                                                 llvm::Constant *cacheVariable,
                                    InPlaceMetadataInitializer &&initializer) {
  llvm::Constant *metadata =
    IGF.IGM.requiresForeignTypeMetadata(type)
      ? IGF.IGM.getAddrOfForeignTypeMetadataCandidate(type)
      : IGF.IGM.getAddrOfTypeMetadata(type);

  // We might not have interesting initialization to do.
  assert((cacheVariable == nullptr) ==
            isTypeMetadataAccessTrivial(IGF.IGM, type));
  if (!cacheVariable)
    return MetadataResponse(metadata);

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
  auto context = llvm::UndefValue::get(IGF.IGM.Int8PtrTy);

  auto onceCall = IGF.Builder.CreateCall(IGF.IGM.getOnceFn(),
                                         {onceGuard, onceFn, context});
  onceCall->setCallingConv(IGF.IGM.DefaultCC);

  // We can just load the cache now.
  // TODO: this should be consume-ordered when LLVM supports it.
  Address cacheAddr = Address(cacheVariable, IGF.IGM.getPointerAlignment());
  llvm::LoadInst *relocatedMetadata = IGF.Builder.CreateLoad(cacheAddr);
  // Make this barrier explicit when building for TSan to avoid false positives.
  if (IGF.IGM.IRGen.Opts.Sanitizers & SanitizerKind::Thread)
    relocatedMetadata->setOrdering(llvm::AtomicOrdering::Acquire);

  // emitLazyCacheAccessFunction will see that the value was loaded from
  // the guard variable and skip the redundant store back.
  return MetadataResponse(relocatedMetadata);
}

/// Emit the body of a metadata accessor function for the given type.
///
/// This function is appropriate for ordinary situations where the
/// construction of the metadata value just involves calling idempotent
/// metadata-construction functions.  It is not used for the in-place
/// initialization of non-generic nominal type metadata.
static llvm::Value *
emitTypeMetadataAccessFunctionBody(IRGenFunction &IGF,
                                   CanType type) {
  assert(!type->hasArchetype() &&
         "cannot emit metadata accessor for context-dependent type");

  // We only take this path for non-generic nominal types.
  auto typeDecl = type->getAnyNominal();
  if (!typeDecl)
    return emitDirectTypeMetadataRef(IGF, type);

  if (typeDecl->isGenericContext() &&
      !(isa<ClassDecl>(typeDecl) &&
        isa<ClangModuleUnit>(typeDecl->getModuleScopeContext()))) {
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
  } else if (isa<ClangModuleUnit>(typeDecl->getModuleScopeContext())) {
    return emitForeignTypeMetadataRef(IGF, type);
  }

  // Okay, everything else is built from a Swift metadata object.
  llvm::Constant *metadata = IGF.IGM.getAddrOfTypeMetadata(type);

  // We should not be doing more serious work along this path.
  assert(isTypeMetadataAccessTrivial(IGF.IGM, type));

  return metadata;
}

/// Get or create an accessor function to the given non-dependent type.
llvm::Function *
irgen::getTypeMetadataAccessFunction(IRGenModule &IGM,
                                     CanType type,
                                     ForDefinition_t shouldDefine,
                                     MetadataAccessGenerator generator) {
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

    if (IGM.getOptions().optimizeForSize())
      accessor->addFnAttr(llvm::Attribute::NoInline);
  }

  emitLazyCacheAccessFunction(IGM, accessor, cacheVariable,
                              [&](IRGenFunction &IGF, Explosion &params) {
    auto request = DynamicMetadataRequest(params.claimNext());
    return generator(IGF, request, cacheVariable);
  });

  return accessor;
}

/// Get or create an accessor function to the given non-dependent type.
llvm::Function *irgen::getTypeMetadataAccessFunction(IRGenModule &IGM,
                                                     CanType type,
                                                 ForDefinition_t shouldDefine) {
  return getTypeMetadataAccessFunction(IGM, type, shouldDefine,
                                       [&](IRGenFunction &IGF,
                                           DynamicMetadataRequest request,
                                           llvm::Constant *cacheVariable) {
    // We should not be called with ForDefinition for nominal types
    // that require in-place initialization.
    // We should also not be called for types that require more interesting
    // initialization that really requires the request/response machinery.
    return MetadataResponse(emitTypeMetadataAccessFunctionBody(IGF, type));
  });
}

/// Get or create an accessor function to the given generic type.
llvm::Function *
irgen::getGenericTypeMetadataAccessFunction(IRGenModule &IGM,
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

  if (IGM.getOptions().optimizeForSize())
    accessor->addFnAttr(llvm::Attribute::NoInline);

  bool isReadNone =
      (genericArgs.Types.size() <= NumDirectGenericTypeMetadataAccessFunctionArgs);

  emitLazyCacheAccessFunction(IGM, accessor, /*cacheVariable=*/nullptr,
                              [&](IRGenFunction &IGF, Explosion &params) {
                                return emitGenericTypeMetadataAccessFunction(
                                    IGF, params, nominal, genericArgs);
                              },
                              isReadNone);

  return accessor;
}

/// Return the type metadata access function for the given type, if it
/// is guaranteed to exist.
llvm::Constant *
irgen::getRequiredTypeMetadataAccessFunction(IRGenModule &IGM,
                                             NominalTypeDecl *theDecl,
                                             ForDefinition_t shouldDefine) {
  if (theDecl->isGenericContext()) {
    return getGenericTypeMetadataAccessFunction(IGM, theDecl, shouldDefine);
  }

  CanType declaredType = theDecl->getDeclaredType()->getCanonicalType();
  return getTypeMetadataAccessFunction(IGM, declaredType, shouldDefine);
}

/// Emit a call to the type metadata accessor for the given function.
static MetadataResponse
emitCallToTypeMetadataAccessFunction(IRGenFunction &IGF,
                                     CanType type,
                                     DynamicMetadataRequest request,
                                     ForDefinition_t shouldDefine) {
  // If we already cached the metadata, use it.
  if (auto local = IGF.tryGetLocalTypeMetadata(type, request))
    return local;

  llvm::Constant *accessor =
    getTypeMetadataAccessFunction(IGF.IGM, type, shouldDefine);
  llvm::CallInst *call = IGF.Builder.CreateCall(accessor, { request.get(IGF) });
  call->setCallingConv(IGF.IGM.SwiftCC);
  call->setDoesNotAccessMemory();
  call->setDoesNotThrow();

  MetadataResponse result = MetadataResponse::split(IGF, request, call);
  
  // Save the metadata for future lookups.
  IGF.setScopedLocalTypeMetadata(type, result);
  
  return result;
}

/// Produce the type metadata pointer for the given type.
llvm::Value *IRGenFunction::emitTypeMetadataRef(CanType type) {
  return emitTypeMetadataRef(type, MetadataRequest::Complete).getMetadata();
}

/// Produce the type metadata pointer for the given type.
MetadataResponse
IRGenFunction::emitTypeMetadataRef(CanType type,
                                   DynamicMetadataRequest request) {
  type = getRuntimeReifiedType(IGM, type);

  if (type->hasArchetype() ||
      isTypeMetadataAccessTrivial(IGM, type)) {
    // FIXME: propagate metadata request!
    return MetadataResponse(emitDirectTypeMetadataRef(*this, type));
  }

  switch (getTypeMetadataAccessStrategy(type)) {
  case MetadataAccessStrategy::PublicUniqueAccessor:
  case MetadataAccessStrategy::HiddenUniqueAccessor:
  case MetadataAccessStrategy::PrivateAccessor:
    return emitCallToTypeMetadataAccessFunction(*this, type, request,
                                                NotForDefinition);
  case MetadataAccessStrategy::NonUniqueAccessor:
    return emitCallToTypeMetadataAccessFunction(*this, type, request,
                                                ForDefinition);
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

  switch (getTypeMetadataAccessStrategy(type)) {
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
      return IGF.IGM.getAddrOfTypeMetadata(type);
    }

    /// For most types, we can just emit the usual metadata.
    llvm::Value *visitType(CanType t) {
      return IGF.emitTypeMetadataRef(t);
    }

    llvm::Value *visitBoundGenericEnumType(CanBoundGenericEnumType type) {
      // Optionals have a lowered payload type, so we recurse here.
      if (auto objectTy = CanType(type).getOptionalObjectType()) {
        auto payloadMetadata = visit(objectTy);
        llvm::Value *args[] = { payloadMetadata };
        llvm::Type *types[] = { IGF.IGM.TypeMetadataPtrTy };

        // Call the generic metadata accessor function.
        llvm::Function *accessor =
            IGF.IGM.getAddrOfGenericTypeMetadataAccessFunction(
                type->getDecl(), types, NotForDefinition);

        return IGF.emitGenericTypeMetadataAccessFunctionCall(accessor, args,
                                                      MetadataRequest::Complete)
                 .getMetadata();
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

        TupleTypeFlags flags =
          TupleTypeFlags().withNumElements(elements.size());

        llvm::Value *args[] = {
          llvm::ConstantInt::get(IGF.IGM.SizeTy, flags.getIntValue()),
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
        return IGF.emitTypeMetadataRef(
                 CanFunctionType::get(AnyFunctionType::CanParamArrayRef(),
                                      C.TheEmptyTupleType,
                                      AnyFunctionType::ExtInfo()));
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
      auto *vwtable = IGF.emitValueWitnessTableRef(IGF.IGM.getLoweredType(t));
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
      if (auto fixed = dyn_cast<FixedTypeInfo>(&ti))
        return IGF.IGM.emitFixedTypeLayout(t, *fixed);

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
                 CanFunctionType::get(AnyFunctionType::CanParamArrayRef(),
                                      C.TheEmptyTupleType,
                                      AnyFunctionType::ExtInfo()));
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
        if (isa<ExistentialMetatypeType>(type)) {
          return emitFromTypeMetadata(type);
        }
        // Otherwise, this is a metatype that looks like a pointer.
        LLVM_FALLTHROUGH;
      case MetatypeRepresentation::ObjC:
        // Thick metatypes look like pointers with spare bits.
        return emitFromValueWitnessTable(
                     CanMetatypeType::get(IGF.IGM.Context.TheNativeObjectType));
      }

      llvm_unreachable("Not a valid MetatypeRepresentation.");
    }

    llvm::Value *visitAnyClassType(ClassDecl *classDecl) {
      // All class types have the same layout.
      auto type = classDecl->getDeclaredType()->getCanonicalType();
      switch (getReferenceCountingForType(IGF.IGM, type)) {
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
      case ReferenceOwnership::Strong:
        llvm_unreachable("shouldn't be a ReferenceStorageType");
      case ReferenceOwnership::Weak:
        referent = type.getReferentType().getOptionalObjectType();
        break;
      case ReferenceOwnership::Unmanaged:
      case ReferenceOwnership::Unowned:
        referent = type.getReferentType();
        break;
      }

      // Reference storage types with witness tables need open-coded layouts.
      // TODO: Maybe we could provide prefabs for 1 witness table.
      if (referent.isExistentialType()) {
        auto layout = referent.getExistentialLayout();
        for (auto *protoTy : layout.getProtocols()) {
          auto *protoDecl = protoTy->getDecl();
          if (IGF.getSILTypes().protocolRequiresWitnessTable(protoDecl))
            return visitType(type);
        }
      }

      // Unmanaged references are plain pointers with extra inhabitants,
      // which look like thick metatypes.
      //
      // FIXME: This sounds wrong, an Objective-C tagged pointer could be
      // stored in an unmanaged reference for instance.
      if (type->getOwnership() == ReferenceOwnership::Unmanaged) {
        auto metatype = CanMetatypeType::get(C.TheNativeObjectType);
        return emitFromValueWitnessTable(metatype);
      }

      CanType valueWitnessReferent;
      switch (getReferenceCountingForType(IGF.IGM, referent)) {
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
      if (type->getOwnership() == ReferenceOwnership::Weak)
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

/// Given a class metatype, produce the necessary heap metadata
/// reference.  This is generally the metatype pointer, but may
/// instead be a reference type.
llvm::Value *irgen::emitClassHeapMetadataRefForMetatype(IRGenFunction &IGF,
                                                        llvm::Value *metatype,
                                                        CanType type) {
  // If the type is known to have Swift metadata, this is trivial.
  if (hasKnownSwiftMetadata(IGF.IGM, type))
    return metatype;

  // Otherwise, we may have to unwrap an ObjC class wrapper.
  assert(IGF.IGM.Context.LangOpts.EnableObjCInterop);
  metatype = IGF.Builder.CreateBitCast(metatype, IGF.IGM.TypeMetadataPtrTy);
  
  // Fetch the metadata for that class.
  auto call = IGF.Builder.CreateCall(IGF.IGM.getGetObjCClassFromMetadataFn(),
                                     metatype);
  call->setDoesNotThrow();
  call->setDoesNotAccessMemory();
  return call;
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
