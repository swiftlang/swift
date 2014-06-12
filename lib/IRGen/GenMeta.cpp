//===--- GenMeta.cpp - IR generation for metadata constructs --------------===//
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
//  This file implements IR generation for metadata constructs like
//  metatypes and modules.  These is presently always trivial, but in
//  the future we will likely have some sort of physical
//  representation for at least some metatypes.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/ArchetypeBuilder.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/CanTypeVisitor.h"
#include "swift/AST/Decl.h"
#include "swift/AST/IRGenOptions.h"
#include "swift/AST/Substitution.h"
#include "swift/AST/Types.h"
#include "swift/ABI/MetadataValues.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/GlobalVariable.h"
#include "llvm/IR/Module.h"
#include "llvm/ADT/SmallString.h"

#include "Address.h"
#include "Callee.h"
#include "ClassMetadataLayout.h"
#include "FixedTypeInfo.h"
#include "GenClass.h"
#include "GenPoly.h"
#include "GenProto.h"
#include "GenStruct.h"
#include "HeapTypeInfo.h"
#include "IRGenModule.h"
#include "IRGenDebugInfo.h"
#include "Linking.h"
#include "ScalarTypeInfo.h"
#include "StructMetadataLayout.h"
#include "StructLayout.h"
#include "EnumMetadataLayout.h"

#include "GenMeta.h"

using namespace swift;
using namespace irgen;

/// Produce a constant to place in a metatype's isa field
/// corresponding to the given metadata kind.
static llvm::ConstantInt *getMetadataKind(IRGenModule &IGM,
                                          MetadataKind kind) {
  return llvm::ConstantInt::get(IGM.MetadataKindTy, uint8_t(kind));
}

static Size::int_type getOffsetInWords(IRGenModule &IGM, Size offset) {
  assert(offset.isMultipleOf(IGM.getPointerSize()));
  return offset / IGM.getPointerSize();
}
static Address createPointerSizedGEP(IRGenFunction &IGF,
                                     Address base,
                                     Size offset) {
  return IGF.Builder.CreateConstArrayGEP(base,
                                         getOffsetInWords(IGF.IGM, offset),
                                         offset);
}

static llvm::Constant *getMangledTypeName(IRGenModule &IGM, CanType type) {
  auto name = LinkEntity::forTypeMangling(type);
  llvm::SmallString<32> mangling;
  name.mangle(mangling);
  return IGM.getAddrOfGlobalString(mangling);
}

llvm::Value *irgen::emitObjCMetadataRefForMetadata(IRGenFunction &IGF,
                                                   llvm::Value *classPtr) {
  classPtr = IGF.Builder.CreateBitCast(classPtr, IGF.IGM.ObjCClassPtrTy);
  
  // Fetch the metadata for that class.
  auto call = IGF.Builder.CreateCall(IGF.IGM.getGetObjCClassMetadataFn(),
                                     classPtr);
  call->setDoesNotThrow();
  call->setDoesNotAccessMemory();
  call->setCallingConv(IGF.IGM.RuntimeCC);
  return call;
}

/// Emit a reference to the Swift metadata for an Objective-C class.
static llvm::Value *emitObjCMetadataRef(IRGenFunction &IGF,
                                        ClassDecl *theClass) {
  // Derive a pointer to the Objective-C class.
  auto classPtr = IGF.IGM.getAddrOfObjCClass(theClass, NotForDefinition);
  
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

    void collect(IRGenFunction &IGF, BoundGenericType *type) {
      // Add all the argument archetypes.
      // TODO: only the *primary* archetypes
      // TODO: not archetypes from outer contexts
      // TODO: but we are partially determined by the outer context!
      for (auto &sub : type->getSubstitutions(/*FIXME:*/nullptr, nullptr)) {
        CanType subbed = sub.Replacement->getCanonicalType();
        Values.push_back(IGF.emitTypeMetadataRef(subbed));
      }

      // All of those values are metadata pointers.
      Types.append(Values.size(), IGF.IGM.TypeMetadataPtrTy);

      // Add protocol witness tables for all those archetypes.
      for (auto &sub : type->getSubstitutions(/*FIXME:*/nullptr, nullptr))
        emitWitnessTableRefs(IGF, sub, Values);

      // All of those values are witness table pointers.
      Types.append(Values.size() - Types.size(), IGF.IGM.WitnessTablePtrTy);
    }
  };
}

static bool isMetadataIndirect(IRGenModule &IGM, NominalTypeDecl *theDecl) {
  // FIXME
  return false;
}

/// Attempts to return a constant heap metadata reference for a
/// nominal type.
llvm::Constant *irgen::tryEmitConstantHeapMetadataRef(IRGenModule &IGM,
                                                      CanType type) {
  assert(isa<NominalType>(type) || isa<BoundGenericType>(type));

  // We can't do this for any types with generic parameters, either
  // directly or inherited from the context.
  // FIXME: Should be an isSpecialized check here.
  if (isa<BoundGenericType>(type))
    return nullptr;
  auto theDecl = cast<NominalType>(type)->getDecl();
  if (theDecl->getGenericParamsOfContext())
    return nullptr;

  if (auto theClass = dyn_cast<ClassDecl>(theDecl))
    if (!hasKnownSwiftMetadata(IGM, theClass))
      return IGM.getAddrOfObjCClass(theClass, NotForDefinition);

  if (isMetadataIndirect(IGM, theDecl))
    return nullptr;

  return IGM.getAddrOfTypeMetadata(type, false, false);
}

/// Emit a reference to the type metadata for a foreign type.
static llvm::Value *emitForeignTypeMetadataRef(IRGenFunction &IGF,
                                               CanType type) {
  llvm::Value *candidate = IGF.IGM.getAddrOfForeignTypeMetadataCandidate(type);
  return IGF.Builder.CreateCall(IGF.IGM.getGetForeignTypeMetadataFn(),
                                candidate);
}

/// Returns a metadata reference for a class type.
static llvm::Value *emitNominalMetadataRef(IRGenFunction &IGF,
                                           NominalTypeDecl *theDecl,
                                           CanType theType) {
  // Non-native Swift classes need to be handled differently.
  if (auto theClass = dyn_cast<ClassDecl>(theDecl)) {
    // We emit a completely different pattern for foreign classes.
    if (theClass->isForeign()) {
      return emitForeignTypeMetadataRef(IGF, theType);
    }

    // Classes that might not have Swift metadata use a different
    // symbol name.
    if (!hasKnownSwiftMetadata(IGF.IGM, theClass)) {
      assert(!theDecl->getGenericParamsOfContext() &&
             "ObjC class cannot be generic");
      return emitObjCMetadataRef(IGF, theClass);
    }
  }

  auto generics = isa<ProtocolDecl>(theDecl)
                    ? nullptr
                    : theDecl->getGenericParamsOfContext();

  bool isPattern = (generics != nullptr);
  assert(!isPattern || isa<BoundGenericType>(theType));
  assert(isPattern || isa<NominalType>(theType));

  // If this is generic, check to see if we've maybe got a local
  // reference already.
  if (isPattern) {
    if (auto cache = IGF.tryGetLocalTypeData(theType, LocalTypeData::Metatype))
      return cache;
  }

  bool isIndirect = isMetadataIndirect(IGF.IGM, theDecl);

  // Grab a reference to the metadata or metadata template.
  CanType declaredType = theDecl->getDeclaredType()->getCanonicalType();
  llvm::Value *metadata = IGF.IGM.getAddrOfTypeMetadata(declaredType,
                                                        isIndirect, isPattern);

  // If it's indirected, go ahead and load the true value to use.
  // TODO: startup performance might force this to be some sort of
  // lazy check.
  if (isIndirect) {
    auto addr = Address(metadata, IGF.IGM.getPointerAlignment());
    metadata = IGF.Builder.CreateLoad(addr, "metadata.direct");
  }

  // If we don't have generic parameters, that's all we need.
  if (!generics) {
    assert(metadata->getType() == IGF.IGM.TypeMetadataPtrTy);
    return metadata;
  }

  // Okay, we need to call swift_getGenericMetadata.
  assert(metadata->getType() == IGF.IGM.TypeMetadataPatternPtrTy);

  // Grab the substitutions.
  auto boundGeneric = cast<BoundGenericType>(theType);
  assert(boundGeneric->getDecl() == theDecl);

  GenericArguments genericArgs;
  genericArgs.collect(IGF, boundGeneric);
  
  // If we have less than four arguments, use a fast entry point.
  assert(genericArgs.Values.size() > 0 && "no generic args?!");
  if (genericArgs.Values.size() <= 4) {
    llvm::Constant *fastMetadataGetters[] = {
      nullptr,
      IGF.IGM.getGetGenericMetadata1Fn(),
      IGF.IGM.getGetGenericMetadata2Fn(),
      IGF.IGM.getGetGenericMetadata3Fn(),
      IGF.IGM.getGetGenericMetadata4Fn(),
    };
    auto fastGetter = fastMetadataGetters[genericArgs.Values.size()];
    
    SmallVector<llvm::Value *, 5> args;
    args.push_back(metadata);
    for (auto value : genericArgs.Values)
      args.push_back(IGF.Builder.CreateBitCast(value, IGF.IGM.Int8PtrTy));
    auto result = IGF.Builder.CreateCall(fastGetter, args);
    result->setDoesNotThrow();
    result->addAttribute(llvm::AttributeSet::FunctionIndex,
                         llvm::Attribute::ReadNone);
    // FIXME: Save scope type metadata.
    return result;
  }

  // Slam that information directly into the generic arguments buffer.
  auto argsBufferTy =
    llvm::StructType::get(IGF.IGM.LLVMContext, genericArgs.Types);
  Address argsBuffer = IGF.createAlloca(argsBufferTy,
                                        IGF.IGM.getPointerAlignment(),
                                        "generic.arguments");
  for (unsigned i = 0, e = genericArgs.Values.size(); i != e; ++i) {
    Address elt = IGF.Builder.CreateStructGEP(argsBuffer, i,
                                              IGF.IGM.getPointerSize() * i);
    IGF.Builder.CreateStore(genericArgs.Values[i], elt);
  }

  // Cast to void*.
  llvm::Value *arguments =
    IGF.Builder.CreateBitCast(argsBuffer.getAddress(), IGF.IGM.Int8PtrTy);

  // Make the call.
  auto result = IGF.Builder.CreateCall2(IGF.IGM.getGetGenericMetadataFn(),
                                        metadata, arguments);
  result->setDoesNotThrow();
  result->addAttribute(llvm::AttributeSet::FunctionIndex,
                       llvm::Attribute::ReadOnly);

  // FIXME: Save scope type metadata.
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
  return hasKnownSwiftImplementation(IGM, theClass);
}

/// Is the given class known to have an implementation in Swift?
bool irgen::hasKnownSwiftImplementation(IRGenModule &IGM, ClassDecl *theClass) {
  return !theClass->hasClangNode();
}

/// Is the given method known to be callable by vtable lookup?
bool irgen::hasKnownVTableEntry(IRGenModule &IGM,
                                AbstractFunctionDecl *theMethod) {
  auto theClass = dyn_cast<ClassDecl>(theMethod->getDeclContext());
  if (!theClass) {
    assert(theMethod->hasClangNode() && "overriding a non-imported method");
    return false;
  }
  return hasKnownSwiftImplementation(IGM, theClass);
}

/// Emit a string encoding the labels in the given tuple type.
static llvm::Constant *getTupleLabelsString(IRGenModule &IGM,
                                            CanTupleType type) {
  bool hasLabels = false;
  llvm::SmallString<128> buffer;
  for (auto &elt : type->getFields()) {
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
    TREAT_AS_OPAQUE(BuiltinRawPointer)
#undef TREAT_AS_OPAQUE

    llvm::Value *emitDirectMetadataRef(CanType type) {
      return IGF.IGM.getAddrOfTypeMetadata(type,
                                           /*indirect*/ false,
                                           /*pattern*/ false);
    }

    /// The given type should use opaque type info.  We assume that
    /// the runtime always provides an entry for such a type;  right
    /// now, that mapping is as one of the integer types.
    llvm::Value *visitOpaqueType(CanType type) {
      auto &opaqueTI = cast<FixedTypeInfo>(IGF.IGM.getTypeInfoForLowered(type));
      assert(opaqueTI.getFixedSize() ==
             Size(opaqueTI.getFixedAlignment().getValue()));
      assert(opaqueTI.getFixedSize().isPowerOf2());
      auto numBits = 8 * opaqueTI.getFixedSize().getValue();
      auto intTy = BuiltinIntegerType::get(numBits, IGF.IGM.Context);
      return emitDirectMetadataRef(CanType(intTy));
    }

    llvm::Value *visitBuiltinNativeObjectType(CanBuiltinNativeObjectType type) {
      return emitDirectMetadataRef(type);
    }

    llvm::Value *visitBuiltinUnknownObjectType(CanBuiltinUnknownObjectType type) {
      return emitDirectMetadataRef(type);
    }

    llvm::Value *visitBuiltinVectorType(CanBuiltinVectorType type) {
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
        return llvm::ConstantExpr::getInBoundsGetElementPtr(fullMetadata,
                                                            indices);
      }

      case 1:
          // For metadata purposes, we consider a singleton tuple to be
          // isomorphic to its element type.
        return visit(type.getElementType(0));

      case 2: {
        // Find the metadata pointer for this element.
        llvm::Value *elt0Metadata = visit(type.getElementType(0));
        llvm::Value *elt1Metadata = visit(type.getElementType(1));

        llvm::Value *args[] = {
          elt0Metadata, elt1Metadata,
          getTupleLabelsString(IGF.IGM, type),
          llvm::ConstantPointerNull::get(IGF.IGM.WitnessTablePtrTy) // proposed
        };

        auto call = IGF.Builder.CreateCall(IGF.IGM.getGetTupleMetadata2Fn(),
                                           args);
        call->setDoesNotThrow();
        call->setCallingConv(IGF.IGM.RuntimeCC);
        return setLocal(CanType(type), call);
      }

      case 3: {
        // Find the metadata pointer for this element.
        llvm::Value *elt0Metadata = visit(type.getElementType(0));
        llvm::Value *elt1Metadata = visit(type.getElementType(1));
        llvm::Value *elt2Metadata = visit(type.getElementType(2));

        llvm::Value *args[] = {
          elt0Metadata, elt1Metadata, elt2Metadata,
          getTupleLabelsString(IGF.IGM, type),
          llvm::ConstantPointerNull::get(IGF.IGM.WitnessTablePtrTy) // proposed
        };

        auto call = IGF.Builder.CreateCall(IGF.IGM.getGetTupleMetadata3Fn(),
                                           args);
        call->setDoesNotThrow();
        call->setCallingConv(IGF.IGM.RuntimeCC);
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
          getTupleLabelsString(IGF.IGM, type),
          llvm::ConstantPointerNull::get(IGF.IGM.WitnessTablePtrTy) // proposed
        };

        auto call = IGF.Builder.CreateCall(IGF.IGM.getGetTupleMetadataFn(),
                                           args);
        call->setDoesNotThrow();
        call->setCallingConv(IGF.IGM.RuntimeCC);

        return setLocal(type, call);
      }
    }

    llvm::Value *visitPolymorphicFunctionType(CanPolymorphicFunctionType type) {
      IGF.unimplemented(SourceLoc(),
                        "metadata ref for polymorphic function type");
      return llvm::UndefValue::get(IGF.IGM.TypeMetadataPtrTy);
    }

    llvm::Value *visitGenericFunctionType(CanGenericFunctionType type) {
      IGF.unimplemented(SourceLoc(),
                        "metadata ref for generic function type");
      return llvm::UndefValue::get(IGF.IGM.TypeMetadataPtrTy);
    }

    llvm::Value *visitFunctionType(CanFunctionType type) {
      if (auto metatype = tryGetLocal(type))
        return metatype;

      // TODO: use a caching entrypoint (with all information
      // out-of-line) for non-dependent functions.

      auto argMetadata = visit(type.getInput());
      auto resultMetadata = visit(type.getResult());

      llvm::Constant *getMetadataFn;
      switch (type->getRepresentation()) {
      case AnyFunctionType::Representation::Thin:
        // TODO: Provide metadata for thin function types. This should not come
        // up yet because thin types aren't allowed in the AST.
        llvm_unreachable("thin function types aren't allowed in the AST");
      case AnyFunctionType::Representation::Thick:
        getMetadataFn = IGF.IGM.getGetFunctionMetadataFn();
        break;
      case AnyFunctionType::Representation::Block:
        getMetadataFn = IGF.IGM.getGetBlockMetadataFn();
        break;
      }
      
      auto call = IGF.Builder.CreateCall2(getMetadataFn,
                                          argMetadata, resultMetadata);
      call->setDoesNotThrow();
      call->setCallingConv(IGF.IGM.RuntimeCC);

      return setLocal(CanType(type), call);
    }

    llvm::Value *visitAnyMetatypeType(CanAnyMetatypeType type) {
      if (auto metatype = tryGetLocal(type))
        return metatype;

      auto instMetadata = visit(type.getInstanceType());
      auto fn = isa<MetatypeType>(type)
                  ? IGF.IGM.getGetMetatypeMetadataFn()
                  : IGF.IGM.getGetExistentialMetatypeMetadataFn();
      auto call = IGF.Builder.CreateCall(fn, instMetadata);
      call->setDoesNotThrow();
      call->setCallingConv(IGF.IGM.RuntimeCC);

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
      
      auto call = IGF.Builder.CreateCall2(IGF.IGM.getGetExistentialMetadataFn(),
                                        IGF.IGM.getSize(Size(protocols.size())),
                                        descriptorArray.getAddress());
      call->setDoesNotThrow();
      call->setCallingConv(IGF.IGM.RuntimeCC);
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
      return IGF.getLocalTypeData(type, LocalTypeData::Metatype);
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
      
    llvm::Value *visitSILBlockStorageType(CanSILBlockStorageType type) {
      llvm_unreachable("cannot ask for metadata of block storage");
    }

    /// Try to find the metatype in local data.
    llvm::Value *tryGetLocal(CanType type) {
      return IGF.tryGetLocalTypeData(type, LocalTypeData::Metatype);
    }

    /// Set the metatype in local data.
    llvm::Value *setLocal(CanType type, llvm::Value *metatype) {
      // FIXME: Save scope type metadata.
      return metatype;
    }
  };
}

/// Produce the type metadata pointer for the given type.
llvm::Value *IRGenFunction::emitTypeMetadataRef(CanType type) {
  return EmitTypeMetadataRef(*this).visit(type);
}

llvm::Value *IRGenFunction::emitTypeMetadataRef(SILType type) {
  return emitTypeMetadataRef(type.getSwiftRValueType());
}

/// Produce the heap metadata pointer for the given class type.  For
/// Swift-defined types, this is equivalent to the metatype for the
/// class, but for Objective-C-defined types, this is the class
/// object.
llvm::Value *irgen::emitClassHeapMetadataRef(IRGenFunction &IGF, CanType type) {
  assert(isa<ClassType>(type) || isa<BoundGenericClassType>(type));

  // ObjC-defined classes will always be top-level non-generic classes.

  if (auto classType = dyn_cast<ClassType>(type)) {
    auto theClass = classType->getDecl();
    if (hasKnownSwiftMetadata(IGF.IGM, theClass))
      return EmitTypeMetadataRef(IGF).visitClassType(classType);
    return IGF.IGM.getAddrOfObjCClass(theClass, NotForDefinition);
  }

  auto classType = cast<BoundGenericClassType>(type);
  assert(hasKnownSwiftMetadata(IGF.IGM, classType->getDecl()));
  return EmitTypeMetadataRef(IGF).visitBoundGenericClassType(classType);
}

llvm::Value *irgen::emitClassHeapMetadataRef(IRGenFunction &IGF, SILType type) {
  return emitClassHeapMetadataRef(IGF, type.getSwiftRValueType());
}

namespace {
  /// A CRTP type visitor for deciding whether the metatype for a type
  /// has trivial representation.
  struct HasTrivialMetatype : CanTypeVisitor<HasTrivialMetatype, bool> {
    /// Class metatypes have non-trivial representation due to the
    /// possibility of subclassing.
    bool visitClassType(CanClassType type) {
      return false;
    }
    bool visitBoundGenericClassType(CanBoundGenericClassType type) {
      return false;
    }

    /// Archetype metatypes have non-trivial representation in case
    /// they instantiate to a class metatype.
    bool visitArchetypeType(CanArchetypeType type) {
      return false;
    }
    
    /// All levels of class metatypes support subtyping.
    bool visitMetatypeType(CanMetatypeType type) {
      return visit(type.getInstanceType());
    }

    /// Everything else is trivial.
    bool visitType(CanType type) {
      return false;
    }
  };
}

/// Does the metatype for the given type have a trivial representation?
bool IRGenModule::isTrivialMetatype(CanMetatypeType metaTy) {
  // FIXME: We still need to handle unlowered metatypes from the AST for
  // IRGen protocol witnesses. This can go away (with the HasTrivialMetatype
  // visitor) when we enable SIL witnesses.
  if (!metaTy->hasRepresentation())
    return HasTrivialMetatype().visit(metaTy.getInstanceType());
  return metaTy->getRepresentation() == MetatypeRepresentation::Thin;
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
    explosion.add(emitClassHeapMetadataRef(IGF, type.getInstanceType()));
    break;
  }
}

/*****************************************************************************/
/** Nominal Type Descriptor Emission *****************************************/
/*****************************************************************************/

namespace {
  class ConstantBuilderBase {
  protected:
    IRGenModule &IGM;
    ConstantBuilderBase(IRGenModule &IGM) : IGM(IGM) {}
  };

  template <class Base = ConstantBuilderBase>
  class ConstantBuilder : public Base {
  protected:
    template <class... T>
    ConstantBuilder(T &&...args) : Base(std::forward<T>(args)...) {}

    IRGenModule &IGM = Base::IGM;

  private:
    llvm::SmallVector<llvm::Constant*, 16> Fields;
    Size NextOffset = Size(0);

  protected:
    Size getNextOffset() const { return NextOffset; }

    /// Add a uintptr_t value that represents the given offset, but
    /// scaled to a number of words.
    void addConstantWordInWords(Size value) {
      addConstantWord(getOffsetInWords(IGM, value));
    }

    /// Add a constant word-sized value.
    void addConstantWord(int64_t value) {
      addWord(llvm::ConstantInt::get(IGM.SizeTy, value));
    }

    /// Add a word-sized value.
    void addWord(llvm::Constant *value) {
      assert(value->getType() == IGM.IntPtrTy ||
             value->getType()->isPointerTy());
      assert(NextOffset.isMultipleOf(IGM.getPointerSize()));
      Fields.push_back(value);
      NextOffset += IGM.getPointerSize();
    }

    /// Add a uint32_t value that represents the given offset, but
    /// scaled to a number of words.
    void addConstantInt32InWords(Size value) {
      addConstantInt32(getOffsetInWords(IGM, value));
    }

    /// Add a constant 32-bit value.
    void addConstantInt32(int32_t value) {
      addInt32(llvm::ConstantInt::get(IGM.Int32Ty, value));
    }

    /// Add a 32-bit value.
    void addInt32(llvm::Constant *value) {
      assert(value->getType() == IGM.Int32Ty);
      assert(NextOffset.isMultipleOf(Size(4)));
      Fields.push_back(value);
      NextOffset += Size(4);
    }

    /// Add a constant of the given size.
    void addStruct(llvm::Constant *value, Size size) {
      assert(size.getValue()
               == IGM.DataLayout.getTypeStoreSize(value->getType()));
      assert(NextOffset.isMultipleOf(
                  Size(IGM.DataLayout.getABITypeAlignment(value->getType()))));
      Fields.push_back(value);
      NextOffset += size;
    }
    
    class ReservationToken {
      size_t Index;
      ReservationToken(size_t index) : Index(index) {}
      friend ConstantBuilder<Base>;
    };
    ReservationToken reserveFields(unsigned numFields, Size size) {
      unsigned index = Fields.size();
      Fields.append(numFields, nullptr);
      NextOffset += size;
      return ReservationToken(index);
    }
    MutableArrayRef<llvm::Constant*> claimReservation(ReservationToken token,
                                                      unsigned numFields) {
      return MutableArrayRef<llvm::Constant*>(&Fields[0] + token.Index,
                                              numFields);
    }

  public:
    llvm::Constant *getInit() const {
      return llvm::ConstantStruct::getAnon(Fields);
    }

    /// An optimization of getInit for when we have a known type we
    /// can use when there aren't any extra fields.
    llvm::Constant *getInitWithSuggestedType(unsigned numFields,
                                             llvm::StructType *type) {
      if (Fields.size() == numFields) {
        return llvm::ConstantStruct::get(type, Fields);
      } else {
        return getInit();
      }
    }
  };

  template<class Impl>
  class NominalTypeDescriptorBuilderBase : public ConstantBuilder<> {
    Impl &asImpl() { return *static_cast<Impl*>(this); }

  public:
    NominalTypeDescriptorBuilderBase(IRGenModule &IGM) : ConstantBuilder(IGM) {}
    
    void layout() {
      asImpl().addKind();
      asImpl().addName();
      asImpl().addKindDependentFields();
      asImpl().addGenericParams();
    }

    void addKind() {
      addConstantWord(asImpl().getKind());
    }
    
    void addName() {
      NominalTypeDecl *ntd = asImpl().getTarget();
      addWord(getMangledTypeName(IGM,
                                 ntd->getDeclaredType()->getCanonicalType()));
    }
    
    void addGenericParams() {
      NominalTypeDecl *ntd = asImpl().getTarget();
      if (!ntd->getGenericParams()) {
        // If there are no generic parameters, there is no generic parameter
        // vector.
        addConstantInt32(0);
        addConstantInt32(0);
        return;
      }
      
      // uint32_t GenericParameterVectorOffset;
      addConstantInt32InWords(asImpl().getGenericParamsOffset());

      // The archetype order here needs to be consistent with
      // MetadataLayout::addGenericFields.
      
      // Note that we intentionally don't forward the generic arguments.
      
      // Add all the primary archetypes.
      // TODO: only the *primary* archetypes.
      // TODO: not archetypes from outer contexts.
      auto allArchetypes = ntd->getGenericParams()->getAllArchetypes();
      
      // uint32_t NumGenericParameters;
      addConstantInt32(allArchetypes.size());
      
      // GenericParameter Parameters[NumGenericParameters];
      // struct GenericParameter {
      for (auto archetype : allArchetypes) {
        //   uint32_t NumWitnessTables;
        // Count the protocol conformances that require witness tables.
        unsigned count = std::count_if(archetype->getConformsTo().begin(),
                                       archetype->getConformsTo().end(),
               [](ProtocolDecl *p) { return requiresProtocolWitnessTable(p); });
        addConstantInt32(count);
      }
      // };
    }
    
    llvm::Constant *emit() {
      asImpl().layout();
      auto init = getInit();
      
      auto var = cast<llvm::GlobalVariable>(
                      IGM.getAddrOfNominalTypeDescriptor(asImpl().getTarget(),
                                                         init->getType()));
      var->setConstant(true);
      var->setInitializer(init);
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
      return getOffsetInWords(this->IGM, getTargetOffset());
    }
  };

  // A bunch of ugly macros to make it easy to declare certain
  // common kinds of searcher.
#define BEGIN_METADATA_SEARCHER_0(SEARCHER, DECLKIND)                   \
  struct SEARCHER                                                       \
    : MetadataSearcher<DECLKIND##MetadataScanner<SEARCHER>> {           \
    using super = MetadataSearcher;                                     \
    SEARCHER(IRGenModule &IGM, DECLKIND##Decl *target)                  \
      : MetadataSearcher(IGM, target) {}
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
  
  /// Build the field type vector accessor for a nominal type. This is a
  /// function that lazily instantiates the type metadata for all of the
  /// types of the stored properties of an instance of a nominal type.
  static llvm::Function *
  buildFieldTypeAccessorFn(IRGenModule &IGM,
                         NominalTypeDecl *type,
                         NominalTypeDecl::StoredPropertyRange storedProperties){
    // The accessor function has the following signature:
    // const Metadata * const *getFieldTypes(const Metadata *T);
    auto metadataArrayPtrTy = IGM.TypeMetadataPtrTy->getPointerTo();
    auto fnTy = llvm::FunctionType::get(metadataArrayPtrTy,
                                        IGM.TypeMetadataPtrTy,
                                        /*vararg*/ false);
    auto fn = llvm::Function::Create(fnTy, llvm::GlobalValue::InternalLinkage,
                                     llvm::Twine("get_field_types_")
                                       + type->getName().str(),
                                     IGM.getModule());
    IRGenFunction IGF(IGM, fn);
    
    llvm::Value *metadata = IGF.collectParameters(ResilienceExpansion::Minimal)
      .claimNext();
    
    // Get the address at which the field type vector reference should be
    // cached.
    llvm::Value *vectorPtr;
    auto nullVector = llvm::ConstantPointerNull::get(metadataArrayPtrTy);
    
    // If the type is not generic, we can use a global variable to cache the
    // address of the field type vector for the single instance.
    if (!type->getGenericParamsOfContext()) {
      vectorPtr = new llvm::GlobalVariable(*IGM.getModule(),
                                           metadataArrayPtrTy,
                                           /*constant*/ false,
                                           llvm::GlobalValue::InternalLinkage,
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
      vectorPtr = IGF.Builder.CreateConstInBoundsGEP1_32(vectorPtr,
                                              getOffsetInWords(IGM, offset));
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
    emitPolymorphicParametersForGenericValueWitness(IGF, type, metadata);
    
    // Allocate storage for the field vector.
    SmallVector<VarDecl*, 4> fields(storedProperties.begin(),
                                    storedProperties.end());
    unsigned allocSize = fields.size() * IGM.getPointerSize().getValue();
    auto allocSizeVal = llvm::ConstantInt::get(IGM.IntPtrTy, allocSize);
    auto allocAlignMaskVal =
      IGM.getSize(IGM.getPointerAlignment().asSize() - Size(1));
    llvm::Value *builtVectorAlloc
      = IGF.emitAllocRawCall(allocSizeVal, allocAlignMaskVal);
    
    llvm::Value *builtVector
      = IGF.Builder.CreateBitCast(builtVectorAlloc, metadataArrayPtrTy);
    
    // Emit type metadata for the fields into the vector.
    for (unsigned i : indices(fields)) {
      auto field = fields[i];
      auto slot = IGF.Builder.CreateInBoundsGEP(builtVector,
                        llvm::ConstantInt::get(IGM.Int32Ty, i));
      auto fieldTy = field->getType()->getCanonicalType();
      
      // Strip reference storage qualifiers like unowned and weak.
      // FIXME: Some clients probably care about them.
      if (auto refStorTy = dyn_cast<ReferenceStorageType>(fieldTy))
        fieldTy = refStorTy.getReferentType();
      
      auto metadata = IGF.emitTypeMetadataRef(fieldTy);
      IGF.Builder.CreateStore(metadata, slot, IGM.getPointerAlignment());
    }
    
    // Atomically compare-exchange a pointer to our vector into the slot.
    auto vectorIntPtr = IGF.Builder.CreateBitCast(vectorPtr,
                                                  IGM.IntPtrTy->getPointerTo());
    auto builtVectorInt = IGF.Builder.CreatePtrToInt(builtVector,
                                                     IGM.IntPtrTy);
    auto zero = llvm::ConstantInt::get(IGM.IntPtrTy, 0);
    
    auto raceVectorInt = IGF.Builder.CreateAtomicCmpXchg(vectorIntPtr,
                                 zero, builtVectorInt,
                                 llvm::AtomicOrdering::SequentiallyConsistent,
                                 llvm::AtomicOrdering::SequentiallyConsistent);
    
    // The pointer in the slot should still have been null.
    auto didStore = IGF.Builder.CreateICmpEQ(raceVectorInt, zero);
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
    
    return fn;
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
        void addGenericFields(const GenericParamList &g) {
          GenericParamsOffset = NextOffset;
          StructMetadataScanner::addGenericFields(g);
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
      unsigned numFields = 0;
      
      for (auto prop : Target->getStoredProperties()) {
        fieldNames.append(prop->getName().str());
        fieldNames.push_back('\0');
        ++numFields;
      }
      // The final null terminator is provided by getAddrOfGlobalString.
      
      addConstantInt32(numFields);
      addConstantInt32InWords(FieldVectorOffset);
      addWord(IGM.getAddrOfGlobalString(fieldNames));
      
      // Build the field type accessor function.
      llvm::Function *fieldTypeVectorAccessor
        = buildFieldTypeAccessorFn(IGM, Target,
                                   Target->getStoredProperties());
      
      addWord(fieldTypeVectorAccessor);
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
        void addGenericFields(const GenericParamList &g, ClassDecl *c) {
          if (c == Target) {
            GenericParamsOffset = NextOffset;
          }
          ClassMetadataScanner::addGenericFields(g, c);
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
      unsigned numFields = 0;
      
      for (auto prop : Target->getStoredProperties()) {
        fieldNames.append(prop->getName().str());
        fieldNames.push_back('\0');
        ++numFields;
      }
      // The final null terminator is provided by getAddrOfGlobalString.
      
      addConstantInt32(numFields);
      addConstantInt32InWords(FieldVectorOffset);
      addWord(IGM.getAddrOfGlobalString(fieldNames));
      
      // Build the field type accessor function.
      llvm::Function *fieldTypeVectorAccessor
        = buildFieldTypeAccessorFn(IGM, Target,
                                   Target->getStoredProperties());
      
      addWord(fieldTypeVectorAccessor);
    }
  };
  
  class EnumNominalTypeDescriptorBuilder
    : public NominalTypeDescriptorBuilderBase<EnumNominalTypeDescriptorBuilder>
  {
    using super
      = NominalTypeDescriptorBuilderBase<EnumNominalTypeDescriptorBuilder>;
    
    // Offsets of key fields in the metadata records.
    Size GenericParamsOffset;
    
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
        
        void noteAddressPoint() { AddressPoint = NextOffset; }
        void addGenericFields(const GenericParamList &g) {
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
    }
    
    EnumDecl *getTarget() { return Target; }
    
    unsigned getKind() {
      return unsigned(NominalTypeKind::Enum);
    }
    
    Size getGenericParamsOffset() {
      return GenericParamsOffset;
    }
    
    void addKindDependentFields() {
      // FIXME: Populate.
      addConstantInt32(0);
      addConstantInt32(0);
      addConstantWord(0);
      addConstantWord(0);
    }
  };
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

    /// The generics clause for the type we're emitting.
    const GenericParamList &ClassGenerics;
    
    /// The number of generic witnesses in the type we're emitting.
    /// This is not really something we need to track.
    unsigned NumGenericWitnesses = 0;

    struct FillOp {
      Size FromOffset;
      Size ToOffset;

      FillOp() = default;
      FillOp(Size from, Size to) : FromOffset(from), ToOffset(to) {}
    };

    SmallVector<FillOp, 8> FillOps;

    enum { TemplateHeaderFieldCount = 5 };
    enum { NumPrivateDataWords = 8 };
    Size TemplateHeaderSize;

  protected:
    /// The offset of the address point in the type we're emitting.
    Size AddressPoint = Size::invalid();
    
    IRGenModule &IGM = super::IGM;
    using super::asImpl;
    
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
    GenericMetadataBuilderBase(IRGenModule &IGM, 
                               const GenericParamList &generics,
                               T &&...args)
      : super(IGM, std::forward<T>(args)...), ClassGenerics(generics) {}

    /// Emit the fill function for the template.
    llvm::Function *emitFillFunction() {
      // void (*FillFunction)(void*, const void*)
      llvm::Type *argTys[] = {IGM.Int8PtrTy, IGM.Int8PtrTy};
      auto ty = llvm::FunctionType::get(IGM.VoidTy, argTys, /*isVarArg*/ false);
      llvm::Function *f = llvm::Function::Create(ty,
                                           llvm::GlobalValue::InternalLinkage,
                                           "fill_generic_metadata",
                                           &IGM.Module);
      
      IRGenFunction IGF(IGM, f);
      if (IGM.DebugInfo)
        IGM.DebugInfo->emitArtificialFunction(IGF, f);
      
      // Execute the fill ops. Cast the parameters to word pointers because the
      // fill indexes are word-indexed.
      Explosion params = IGF.collectParameters(ResilienceExpansion::Minimal);
      llvm::Value *fullMeta = params.claimNext();
      llvm::Value *args = params.claimNext();
      
      Address fullMetaWords(IGF.Builder.CreateBitCast(fullMeta,
                                                   IGM.SizeTy->getPointerTo()),
                            Alignment(IGM.getPointerAlignment()));
      Address argWords(IGF.Builder.CreateBitCast(args,
                                                 IGM.SizeTy->getPointerTo()),
                       Alignment(IGM.getPointerAlignment()));
      
      for (auto &fillOp : FillOps) {
        auto dest = createPointerSizedGEP(IGF, fullMetaWords, fillOp.ToOffset);
        auto src = createPointerSizedGEP(IGF, argWords, fillOp.FromOffset);
        IGF.Builder.CreateStore(IGF.Builder.CreateLoad(src), dest);
      }
      
      // Derive the metadata value.
      auto addressPointAddr =
        createPointerSizedGEP(IGF, fullMetaWords, AddressPoint);

      llvm::Value *metadataValue
        = IGF.Builder.CreateBitCast(addressPointAddr.getAddress(),
                                    IGF.IGM.TypeMetadataPtrTy);
      
      // Initialize the instantiated dependent value witness table, if we have
      // one.
      llvm::Value *vwtableValue = nullptr;
      if (HasDependentVWT) {
        assert(!AddressPoint.isInvalid() && "did not set valid address point!");
        assert(!DependentVWTPoint.isInvalid() && "did not set dependent VWT point!");
        
        // Fill in the pointer from the metadata to the VWT. The VWT pointer
        // always immediately precedes the address point.
        auto vwtAddr = createPointerSizedGEP(IGF, fullMetaWords,
                                             DependentVWTPoint);
        auto vwtAddrVal = IGF.Builder.CreatePtrToInt(vwtAddr.getAddress(),
                                                     IGM.SizeTy);
        auto vwtRefAddr = createPointerSizedGEP(IGF, fullMetaWords,
                                        AddressPoint - IGM.getPointerSize());
        IGF.Builder.CreateStore(vwtAddrVal, vwtRefAddr);

        vwtableValue = IGF.Builder.CreateBitCast(vwtAddr.getAddress(),
                                                 IGF.IGM.WitnessTablePtrTy);
        
        HasDependentMetadata = true;
      }

      if (HasDependentMetadata) {
        asImpl().emitInitializeMetadata(IGF, metadataValue, vwtableValue);
      }
      
      // The metadata is now complete.
      IGF.Builder.CreateRetVoid();
      
      return f;
    }
    
  public:
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

      //   void (*FillFunction)(void *, const void*);
      headerFields[Field++] = emitFillFunction();
      
      //   uint32_t MetadataSize;
      // We compute this assuming that every entry in the metadata table
      // is a pointer in size.
      Size size = getNextOffset();
      headerFields[Field++] =
        llvm::ConstantInt::get(IGM.Int32Ty, size.getValue());
      
      //   uint16_t NumArguments;
      // TODO: ultimately, this should be the number of actual template
      // arguments, not the number of witness tables required.
      headerFields[Field++]
        = llvm::ConstantInt::get(IGM.Int16Ty, NumGenericWitnesses);

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
    void addGenericArgument(ArchetypeType *type, T &&...args) {
      FillOps.push_back(FillOp(NumGenericWitnesses++ * IGM.getPointerSize(),
                               getNextOffset()));
      super::addGenericArgument(type, std::forward<T>(args)...);
    }

    template <class... T>
    void addGenericWitnessTable(ArchetypeType *type, ProtocolDecl *protocol,
                                T &&...args) {
      FillOps.push_back(FillOp(NumGenericWitnesses++ * IGM.getPointerSize(),
                               getNextOffset()));
      super::addGenericWitnessTable(type, protocol, std::forward<T>(args)...);
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
      // Spec'ed to be 8 pointers wide.  An arbitrary choice; should
      // work out an ideal size with the runtime folks.
      auto null = llvm::ConstantPointerNull::get(IGM.Int8PtrTy);
      
      llvm::Constant *privateData[NumPrivateDataWords] = {
        null, null, null, null, null, null, null, null
      };
      return makeArray(IGM.Int8PtrTy, privateData);
    }
  };
}

// Classes

namespace {
  /// An adapter for laying out class metadata.
  template <class Impl>
  class ClassMetadataBuilderBase
         : public ConstantBuilder<ClassMetadataLayout<Impl>> {
    using super = ConstantBuilder<ClassMetadataLayout<Impl>>;

    /// A mapping from functions to their final overriders.
    llvm::DenseMap<AbstractFunctionDecl*,AbstractFunctionDecl*> FinalOverriders;

    Optional<MetadataSize> ClassObjectExtents;

  protected:
    using super::IGM;
    using super::Target;
    using super::addWord;
    using super::addConstantWord;
    using super::addInt32;
    using super::addConstantInt32;
    using super::addStruct;
    using super::getNextOffset;
    const StructLayout &Layout;

    ClassMetadataBuilderBase(IRGenModule &IGM, ClassDecl *theClass,
                             const StructLayout &layout)
      : super(IGM, theClass), Layout(layout) {

      computeFinalOverriders();
    }

    /// Compute a map of all the final overriders for the class.
    void computeFinalOverriders() {
      // Walk up the whole class hierarchy.
      ClassDecl *cls = Target;
      do {
        // Make sure that each function has its final overrider set.
        for (auto member : cls->getMembers()) {
          auto fn = dyn_cast<AbstractFunctionDecl>(member);
          if (!fn) continue;

          // Check whether we already have an entry for this function.
          auto &finalOverrider = FinalOverriders[fn];

          // If not, the function is its own final overrider.
          if (!finalOverrider) finalOverrider = fn;

          // If the function directly overrides something, update the
          // overridden function's entry.
          if (auto overridden = fn->getOverriddenDecl())
            FinalOverriders.insert(std::make_pair(overridden, finalOverrider));

        }

        
      } while (cls->hasSuperclass() &&
               (cls = cls->getSuperclass()->getClassOrBoundGenericClass()));
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

      // Get the metaclass pointer as an intptr_t.
      auto metaclass = IGM.getAddrOfMetaclassObject(Target,
                                                    NotForDefinition);
      auto flags = llvm::ConstantExpr::getPtrToInt(metaclass, IGM.IntPtrTy);
      addWord(flags);
    }

    /// The runtime provides a value witness table for Builtin.NativeObject.
    void addValueWitnessTable() {
      ClassDecl *cls = Target;
      
      auto type = cls->isObjC()
        ? CanType(this->IGM.Context.TheUnknownObjectType)
        : CanType(this->IGM.Context.TheNativeObjectType);
      auto wtable = this->IGM.getAddrOfValueWitnessTable(type);
      addWord(wtable);
    }

    void addDestructorFunction() {
      auto expansion = ResilienceExpansion::Minimal;
      auto dtorRef = SILDeclRef(Target->getDestructor(),
                                SILDeclRef::Kind::Deallocator,
                                expansion);
      addWord(IGM.getAddrOfSILFunction(dtorRef, NotForDefinition));
    }
    
    void addNominalTypeDescriptor() {
      addWord(ClassNominalTypeDescriptorBuilder(IGM, Target).emit());
    }

    void addParentMetadataRef(ClassDecl *forClass) {
      // FIXME: this is wrong for multiple levels of generics; we need
      // to apply substitutions through.
      Type parentType =
        forClass->getDeclContext()->getDeclaredTypeInContext();
      addReferenceToType(parentType->getCanonicalType());
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
        // getSwiftRootClass needs to be ObjC-mangled but isn't
        // actually imported from a clang module.
        addWord(IGM.getAddrOfObjCClass(IGM.getSwiftRootClass(),
                                       NotForDefinition));
        return;
      }

      Type superclassTy
        = ArchetypeBuilder::mapTypeIntoContext(Target,
                                               Target->getSuperclass());
      addReferenceToType(superclassTy->getCanonicalType());
    }
    
    void addReferenceToType(CanType type) {
      if (llvm::Constant *metadata
            = tryEmitConstantHeapMetadataRef(IGM, type)) {
        addWord(metadata);
      } else {
        // Leave a null pointer placeholder to be filled at runtime
        addWord(llvm::ConstantPointerNull::get(IGM.TypeMetadataPtrTy));
      }
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
        if (IGM.SizeTy != IGM.Int32Ty)
          align = llvm::ConstantExpr::getTrunc(align, IGM.Int32Ty);
        addInt32(align);
      } else {
        // Leave a zero placeholder to be filled at runtime
        addConstantInt32(0);
      }
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
      addWord(IGM.getObjCEmptyCachePtr());
      addWord(IGM.getObjCEmptyVTablePtr());
    }

    void addClassDataPointer() {
      // Derive the RO-data.
      llvm::Constant *data = emitClassPrivateData(IGM, Target);

      // We always set the low bit to indicate this is a Swift class.
      data = llvm::ConstantExpr::getPtrToInt(data, IGM.IntPtrTy);
      data = llvm::ConstantExpr::getAdd(data,
                                    llvm::ConstantInt::get(IGM.IntPtrTy, 1));

      addWord(data);
    }

    void addFieldOffset(VarDecl *var) {
      // Use a fixed offset if we have one.
      if (auto offset = tryEmitClassConstantFragileFieldOffset(IGM, Target,
                                                               var))
        addWord(offset);
      // Otherwise, leave a placeholder for the runtime to populate at runtime.
      else
        addWord(llvm::ConstantInt::get(IGM.IntPtrTy, 0));
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

      // Find the final overrider, which we should already have computed.
      auto it = FinalOverriders.find(cast<AbstractFunctionDecl>(fn.getDecl()));
      assert(it != FinalOverriders.end());
      AbstractFunctionDecl *finalOverrider = it->second;

      fn = SILDeclRef(finalOverrider, fn.kind, fn.getResilienceExpansion(),
                      fn.uncurryLevel);

      // Add the appropriate method to the module.
      addWord(IGM.getAddrOfSILFunction(fn, NotForDefinition));
    }

    void addGenericArgument(ArchetypeType *archetype, ClassDecl *forClass) {
      addWord(llvm::Constant::getNullValue(IGM.TypeMetadataPtrTy));
    }

    void addGenericWitnessTable(ArchetypeType *archetype,
                                ProtocolDecl *protocol, ClassDecl *forClass) {
      addWord(llvm::Constant::getNullValue(IGM.WitnessTablePtrTy));
    }
  };

  class ClassMetadataBuilder :
    public ClassMetadataBuilderBase<ClassMetadataBuilder> {
  public:
    ClassMetadataBuilder(IRGenModule &IGM, ClassDecl *theClass,
                         const StructLayout &layout)
      : ClassMetadataBuilderBase(IGM, theClass, layout) {}

    llvm::Constant *getInit() {
      return getInitWithSuggestedType(NumHeapMetadataFields,
                                      IGM.FullHeapMetadataStructTy);
    }
  };
  
  Address emitAddressOfSuperclassRefInClassMetadata(IRGenFunction &IGF,
                                                    ClassDecl *theClass,
                                                    llvm::Value *metadata) {
    // The superclass field in a class type is the first field past the isa.
    unsigned index = 1;

    Address addr(metadata, IGF.IGM.getPointerAlignment());
    addr = IGF.Builder.CreateBitCast(addr,
                                     IGF.IGM.TypeMetadataPtrTy->getPointerTo());
    return IGF.Builder.CreateConstArrayGEP(addr, index, IGF.IGM.getPointerSize());
  }
  
  Address emitAddressOfFieldOffsetVectorInClassMetadata(IRGenFunction &IGF,
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

  /// A builder for metadata templates.
  class GenericClassMetadataBuilder :
    public GenericMetadataBuilderBase<GenericClassMetadataBuilder,
                      ClassMetadataBuilderBase<GenericClassMetadataBuilder>>
  {
    typedef GenericMetadataBuilderBase super;

    bool HasDependentSuperclass = false;
    bool HasDependentFieldOffsetVector = false;
    
    std::vector<std::tuple<ClassDecl*, Size, Size>>
      AncestorFieldOffsetVectors;
    
    std::vector<Size> AncestorFillOps;
    
    Size MetaclassPtrOffset = Size::invalid();
    Size ClassRODataPtrOffset = Size::invalid();
    Size MetaclassRODataPtrOffset = Size::invalid();
    Size DependentMetaclassPoint = Size::invalid();
    Size DependentClassRODataPoint = Size::invalid();
    Size DependentMetaclassRODataPoint = Size::invalid();
  public:
    GenericClassMetadataBuilder(IRGenModule &IGM, ClassDecl *theClass,
                                const StructLayout &layout,
                                const GenericParamList &classGenerics)
      : super(IGM, classGenerics, theClass, layout)
    {
      // We need special initialization of metadata objects to trick the ObjC
      // runtime into initializing them.
      HasDependentMetadata = true;
      
      // If the superclass is generic, we'll need to initialize the superclass
      // reference at runtime.
      if (theClass->hasSuperclass() &&
          theClass->getSuperclass()->is<BoundGenericClassType>()) {
        HasDependentSuperclass = true;
      }
    }
    
    void addMetadataFlags() {
      // The metaclass pointer will be instantiated here.
      MetaclassPtrOffset = getNextOffset();
      addWord(llvm::ConstantInt::get(IGM.IntPtrTy, 0));
    }
    
    void addClassDataPointer() {
      // The rodata pointer will be instantiated here.
      ClassRODataPtrOffset = getNextOffset();
      addWord(llvm::ConstantInt::get(IGM.IntPtrTy, 0));
    }
    
    void addDependentData() {
      // Emit space for the dependent metaclass.
      DependentMetaclassPoint = getNextOffset();
      // isa
      ClassDecl *rootClass = getRootClassForMetaclass(IGM, Target);
      auto isa = IGM.getAddrOfMetaclassObject(rootClass, NotForDefinition);
      addWord(isa);
      // super, which is dependent if the superclass is generic
      llvm::Constant *super;
      if (HasDependentSuperclass)
        super = llvm::ConstantPointerNull::get(IGM.ObjCClassPtrTy);
      else if (Target->hasSuperclass())
        super = IGM.getAddrOfMetaclassObject(
                         Target->getSuperclass()->getClassOrBoundGenericClass(),
                         NotForDefinition);
      else
        super = isa;
      addWord(super);
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

      if (whichClass == Target) {
        // If the metadata contains a field offset vector for the class itself,
        // then we need to initialize it at runtime.
        HasDependentFieldOffsetVector = true;
        return;
      }
      
      // If we have a field offset vector for an ancestor class, we will copy
      // it from our superclass metadata at instantiation time.
      AncestorFieldOffsetVectors.emplace_back(whichClass,
                                              asImpl().getNextOffset(),
                                              Size::invalid());
    }
    
    void noteEndOfFieldOffsets(ClassDecl *whichClass) {
      if (whichClass == Target)
        return;
      
      // Mark the end of the ancestor field offset vector.
      assert(!AncestorFieldOffsetVectors.empty()
             && "no start of ancestor field offsets?!");
      assert(std::get<0>(AncestorFieldOffsetVectors.back()) == whichClass
             && "mismatched start of ancestor field offsets?!");
      std::get<2>(AncestorFieldOffsetVectors.back()) = asImpl().getNextOffset();
    }
    
    // Suppress GenericMetadataBuilderBase's default behavior of introducing
    // fill ops for generic arguments unless they belong directly to the target
    // class and not its ancestors.

    void addGenericArgument(ArchetypeType *type, ClassDecl *forClass) {
      if (forClass == Target) {
        // Introduce the fill op.
        GenericMetadataBuilderBase::addGenericArgument(type, forClass);
      } else {
        // Lay out the field, but don't provide the fill op, which we'll get
        // from the superclass.
        HasDependentMetadata = true;
        AncestorFillOps.push_back(getNextOffset());
        ClassMetadataBuilderBase::addGenericArgument(type, forClass);
      }
    }
    
    void addGenericWitnessTable(ArchetypeType *type, ProtocolDecl *protocol,
                                ClassDecl *forClass) {
      if (forClass == Target) {
        // Introduce the fill op.
        GenericMetadataBuilderBase::addGenericWitnessTable(type, protocol,
                                                           forClass);
      } else {
        // Lay out the field, but don't provide the fill op, which we'll get
        // from the superclass.

        HasDependentMetadata = true;
        AncestorFillOps.push_back(getNextOffset());
        ClassMetadataBuilderBase::addGenericWitnessTable(type, protocol,
                                                         forClass);
      }
    }

    void emitInitializeMetadata(IRGenFunction &IGF,
                                llvm::Value *metadata,
                                llvm::Value *vwtable) {
      emitPolymorphicParametersForGenericValueWitness(IGF,
                                                      Target,
                                                      metadata);
      
      assert(!HasDependentVWT && "class should never have dependent VWT");
      
      // Fill in the metaclass pointer.
      Address metadataPtr(IGF.Builder.CreateBitCast(metadata, IGF.IGM.Int8PtrPtrTy),
                          IGF.IGM.getPointerAlignment());
      
      llvm::Value *metaclass;
      {
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
      }
      
      // Fill in the rodata reference in the class.
      Address classRODataPtr;
      {
        assert(!DependentClassRODataPoint.isInvalid());
        assert(!ClassRODataPtrOffset.isInvalid());
        Address rodataPtrSlot = createPointerSizedGEP(IGF, metadataPtr,
                                           ClassRODataPtrOffset - AddressPoint);
        rodataPtrSlot = IGF.Builder.CreateBitCast(rodataPtrSlot,
                                              IGF.IGM.IntPtrTy->getPointerTo());
        
        classRODataPtr = createPointerSizedGEP(IGF, metadataPtr,
                                      DependentClassRODataPoint - AddressPoint);
        // Set the low bit of the value to indicate "compiled by Swift".
        llvm::Value *rodata = IGF.Builder.CreatePtrToInt(classRODataPtr.getAddress(),
                                                         IGF.IGM.IntPtrTy);
        rodata = IGF.Builder.CreateOr(rodata, 1);
        IGF.Builder.CreateStore(rodata, rodataPtrSlot);
      }

      // Fill in the rodata reference in the metaclass.
      Address metaclassRODataPtr;
      {
        assert(!DependentMetaclassRODataPoint.isInvalid());
        assert(!MetaclassRODataPtrOffset.isInvalid());
        Address rodataPtrSlot = createPointerSizedGEP(IGF, metadataPtr,
                                      MetaclassRODataPtrOffset - AddressPoint);
        rodataPtrSlot = IGF.Builder.CreateBitCast(rodataPtrSlot,
                                                IGF.IGM.IntPtrTy->getPointerTo());
        
        metaclassRODataPtr = createPointerSizedGEP(IGF, metadataPtr,
                                 DependentMetaclassRODataPoint - AddressPoint);
        llvm::Value *rodata = IGF.Builder.CreatePtrToInt(metaclassRODataPtr.getAddress(),
                                                         IGF.IGM.IntPtrTy);
        IGF.Builder.CreateStore(rodata, rodataPtrSlot);
      }
      
      // Generate the runtime name for the class and poke it into the rodata.
      llvm::SmallString<32> buf;
      auto basename = IGM.getAddrOfGlobalString(Target->getObjCRuntimeName(buf));
      auto name = IGF.Builder.CreateCall2(IGM.getGetGenericClassObjCNameFn(),
                                          metadata, basename);
      name->setDoesNotThrow();
      Size nameOffset(IGM.getPointerAlignment().getValue() > 4 ? 24 : 16);
      for (Address rodataPtr : {classRODataPtr, metaclassRODataPtr}) {
        auto namePtr = createPointerSizedGEP(IGF, rodataPtr, nameOffset);
        namePtr = IGF.Builder.CreateBitCast(namePtr, IGM.Int8PtrPtrTy);
        IGF.Builder.CreateStore(name, namePtr);
      }
      
      // Get the superclass metadata.
      llvm::Value *superMetadata;
      if (Target->hasSuperclass()) {
        Type superclassTy
          = ArchetypeBuilder::mapTypeIntoContext(Target,
                                                 Target->getSuperclass());

        superMetadata = IGF.emitTypeMetadataRef(
                          superclassTy->getCanonicalType());
      } else {
        assert(!HasDependentSuperclass
               && "dependent superclass without superclass?!");
        superMetadata
          = llvm::ConstantPointerNull::get(IGF.IGM.TypeMetadataPtrTy);
      }
      
      // If the superclass is generic, populate the superclass fields of the
      // class and metaclass.
      if (HasDependentSuperclass) {
        Address superField
          = emitAddressOfSuperclassRefInClassMetadata(IGF,Target,metadata);
        IGF.Builder.CreateStore(superMetadata, superField);
        
        // The superclass of the metaclass is the metaclass of the superclass.
        Address metaSuperField
          = emitAddressOfSuperclassRefInClassMetadata(IGF,Target,metaclass);
        metaSuperField = IGF.Builder.CreateBitCast(metaSuperField,
                                           IGM.ObjCClassPtrTy->getPointerTo());
        llvm::Value *superMetaMetadata
          = IGF.Builder.CreateStructGEP(metaclass, 0);
        superMetaMetadata = IGF.Builder.CreateLoad(superMetaMetadata,
                                                   IGM.getPointerAlignment());
        IGF.Builder.CreateStore(superMetaMetadata, metaSuperField);
      }
      
      // If we have any ancestor generic parameters or field offset vectors,
      // copy them from the superclass metadata.
      if (!AncestorFieldOffsetVectors.empty() || !AncestorFillOps.empty()) {
        Address superBase(superMetadata, IGF.IGM.getPointerAlignment());
        Address selfBase(metadata, IGF.IGM.getPointerAlignment());
        superBase = IGF.Builder.CreateBitCast(superBase,
                                              IGF.IGM.SizeTy->getPointerTo());
        selfBase = IGF.Builder.CreateBitCast(selfBase,
                                             IGF.IGM.SizeTy->getPointerTo());
        
        for (Size ancestorOp : AncestorFillOps) {
          ancestorOp -= AddressPoint;
          Address superOp = createPointerSizedGEP(IGF, superBase, ancestorOp);
          Address selfOp = createPointerSizedGEP(IGF, selfBase, ancestorOp);
          IGF.Builder.CreateStore(IGF.Builder.CreateLoad(superOp), selfOp);
        }
        
        for (auto &ancestorFields : AncestorFieldOffsetVectors) {
          ClassDecl *ancestor;
          Size startIndex, endIndex;
          std::tie(ancestor, startIndex, endIndex) = ancestorFields;
          assert(startIndex <= endIndex);
          if (startIndex == endIndex)
            continue;
          Size size = endIndex - startIndex;
          startIndex -= AddressPoint;
          
          Address superVec = createPointerSizedGEP(IGF, superBase, startIndex);
          Address selfVec = createPointerSizedGEP(IGF, selfBase, startIndex);
          
          IGF.Builder.CreateMemCpy(selfVec, superVec, size);
        }
      }
      
      // If the field layout is dependent, ask the runtime to populate the
      // offset vector.
      if (HasDependentFieldOffsetVector) {
        llvm::Value *fieldVector
          = emitAddressOfFieldOffsetVectorInClassMetadata(IGF,
                                                          Target, metadata)
              .getAddress();
        
        // Collect the stored properties of the type.
        llvm::SmallVector<VarDecl*, 4> storedProperties;
        for (auto prop : Target->getStoredProperties()) {
          storedProperties.push_back(prop);
        }
        // Fill out an array with the field type metadata records.
        Address fields = IGF.createAlloca(
                         llvm::ArrayType::get(IGF.IGM.TypeMetadataPtrTy,
                                              storedProperties.size()),
                         IGF.IGM.getPointerAlignment(), "classFields");
        fields = IGF.Builder.CreateBitCast(fields,
                                     IGF.IGM.TypeMetadataPtrTy->getPointerTo());
        unsigned index = 0;
        for (auto prop : storedProperties) {
          auto propTy = prop->getType()->getCanonicalType();
          // Strip reference storage qualifiers like unowned and weak.
          // FIXME: Some clients probably care about them.
          if (auto refStorTy = dyn_cast<ReferenceStorageType>(propTy))
            propTy = refStorTy.getReferentType();
          
          llvm::Value *metadata = IGF.emitTypeMetadataRef(propTy);
          Address field = IGF.Builder.CreateConstArrayGEP(fields, index,
                                                      IGF.IGM.getPointerSize());
          IGF.Builder.CreateStore(metadata, field);
          ++index;
        }
        
        // Ask the runtime to lay out the class.
        auto numFields = llvm::ConstantInt::get(IGF.IGM.SizeTy,
                                                storedProperties.size());
        IGF.Builder.CreateCall5(IGF.IGM.getInitClassMetadataUniversalFn(),
                                metadata, superMetadata, numFields,
                                fields.getAddress(), fieldVector);
      }

      // Register the class with the ObjC runtime.
      llvm::Value *instantiateObjC = IGF.IGM.getInstantiateObjCClassFn();
      IGF.Builder.CreateCall(instantiateObjC, metadata);
    }
    
  };
}

/// Emit the ObjC-compatible class symbol for a class.
/// Since LLVM and many system linkers do not have a notion of relative symbol
/// references, we emit the symbol as a global asm block.
static void emitObjCClassSymbol(IRGenModule &IGM,
                                ClassDecl *classDecl,
                                llvm::GlobalVariable *fullMetadata) {
  llvm::SmallString<128> asmString;
  llvm::raw_svector_ostream os(asmString);
  
  llvm::SmallString<32> classSymbol;
  LinkEntity::forObjCClass(classDecl).mangle(classSymbol);
  
  // Get the address point offset into the full metadata.
  auto addrPointOffset
    = llvm::ConstantInt::get(IGM.Int32Ty, MetadataAdjustmentIndex::Class);
  llvm::Constant *gepIndexes[] = {
    llvm::ConstantInt::get(IGM.Int32Ty, 0),
    addrPointOffset,
  };
  auto addressPoint
    = llvm::ConstantExpr::getGetElementPtr(fullMetadata, gepIndexes);
  auto addressPointTy = cast<llvm::PointerType>(addressPoint->getType());
  
  // Create the alias.
  llvm::GlobalAlias::create(addressPointTy->getElementType(),
                            addressPointTy->getAddressSpace(),
                            fullMetadata->getLinkage(), classSymbol.str(),
                            addressPoint, IGM.getModule());
}

/// Emit the type metadata or metadata template for a class.
void irgen::emitClassMetadata(IRGenModule &IGM, ClassDecl *classDecl,
                              const StructLayout &layout) {
  assert(!classDecl->isForeign());

  // TODO: classes nested within generic types
  llvm::Constant *init;
  bool isPattern;
  if (auto *generics = classDecl->getGenericParamsOfContext()) {
    GenericClassMetadataBuilder builder(IGM, classDecl, layout, *generics);
    builder.layout();
    init = builder.getInit();
    isPattern = true;
  } else {
    ClassMetadataBuilder builder(IGM, classDecl, layout);
    builder.layout();
    init = builder.getInit();
    isPattern = false;
  }

  // For now, all type metadata is directly stored.
  bool isIndirect = false;

  CanType declaredType = classDecl->getDeclaredType()->getCanonicalType();
  auto var = cast<llvm::GlobalVariable>(
                     IGM.getAddrOfTypeMetadata(declaredType,
                                               isIndirect, isPattern,
                                               init->getType()));
  var->setInitializer(init);

  // TODO: the metadata global can actually be constant in a very
  // special case: it's not a pattern, ObjC interoperation isn't
  // required, there are no class fields, and there is nothing that
  // needs to be runtime-adjusted.
  var->setConstant(false);

  // Add non-generic classes to the ObjC class list.
  if (IGM.ObjCInterop && !isPattern && !isIndirect) {
    
    // We can't just use 'var' here because it's unadjusted.  Instead
    // of re-implementing the adjustment logic, just pull the metadata
    // pointer again.
    auto metadata =
      IGM.getAddrOfTypeMetadata(declaredType, isIndirect, isPattern);

    // Emit the ObjC class symbol to make the class visible to ObjC.
    if (classDecl->isObjC()) {
      // FIXME: Put the variable in a no_dead_strip section, as a workaround to
      // avoid linker transformations that may break up the symbol.
      var->setSection("__DATA,__objc_data, regular, no_dead_strip");
      
      emitObjCClassSymbol(IGM, classDecl, var);
    }

    IGM.addObjCClass(metadata);
  }
}

/// Does the given method require an override entry in the class v-table?
bool irgen::doesMethodRequireOverrideEntry(IRGenModule &IGM,
                                           AbstractFunctionDecl *fn,
                                           ResilienceExpansion explosionLevel,
                                           unsigned uncurryLevel) {
  // Check each of the overridden declarations in turn.
  AbstractFunctionDecl *overridden = fn->getOverriddenDecl();
  do {
    assert(overridden);
    
    // ObjC methods never get vtable entries, so overrides always need a new
    // entry.
    if (!hasKnownVTableEntry(IGM, overridden))
      continue;

    // TODO: eventually we'll need to handle stuff like abstraction
    // differences due to overrides of methods of polymorphic classes, e.g.
    //   class A<T> { func foo() -> T { ... } }
    //   class B : A<Int> { func foo() -> Int { ... } }
    // But that really ought to be handled by SIL-gen.
    return false;

  } while ((overridden = overridden->getOverriddenDecl()));

  // Otherwise, we need a new entry.
  return true;
}

/// Emit a load from the given metadata at a constant index.
static llvm::Value *emitLoadFromMetadataAtIndex(IRGenFunction &IGF,
                                                llvm::Value *metadata,
                                                int index,
                                                llvm::PointerType *objectTy) {
  // Require the metadata to be some type that we recognize as a
  // metadata pointer.
  assert(metadata->getType() == IGF.IGM.TypeMetadataPtrTy);

  // We require objectType to be a pointer type so that the GEP will
  // scale by the right amount.  We could load an arbitrary type using
  // some extra bitcasting.

  // Cast to T*.
  auto objectPtrTy = objectTy->getPointerTo();
  metadata = IGF.Builder.CreateBitCast(metadata, objectPtrTy);

  auto indexV = llvm::ConstantInt::getSigned(IGF.IGM.SizeTy, index);

  // GEP to the slot.
  Address slot(IGF.Builder.CreateInBoundsGEP(metadata, indexV),
               IGF.IGM.getPointerAlignment());

  // Load.
  auto result = IGF.Builder.CreateLoad(slot);
  return result;
}

/// Given a type metadata pointer, load its value witness table.
llvm::Value *
IRGenFunction::emitValueWitnessTableRefForMetadata(llvm::Value *metadata) {
  return emitLoadFromMetadataAtIndex(*this, metadata, -1,
                                     IGM.WitnessTablePtrTy);
}

/// Load the metadata reference at the given index.
static llvm::Value *emitLoadOfMetadataRefAtIndex(IRGenFunction &IGF,
                                                 llvm::Value *metadata,
                                                 int index) {
  return emitLoadFromMetadataAtIndex(IGF, metadata, index,
                                     IGF.IGM.TypeMetadataPtrTy);
}

/// Load the protocol witness table reference at the given index.
static llvm::Value *emitLoadOfWitnessTableRefAtIndex(IRGenFunction &IGF,
                                                     llvm::Value *metadata,
                                                     int index) {
  return emitLoadFromMetadataAtIndex(IGF, metadata, index,
                                     IGF.IGM.WitnessTablePtrTy);
}

namespace {
  /// A class for finding the 'parent' index in a class metadata object.
  BEGIN_METADATA_SEARCHER_0(FindClassParentIndex, Class)
    void addParentMetadataRef(ClassDecl *forClass) {
      if (forClass == Target) setTargetOffset();
      addParentMetadataRef(forClass);
    }
  END_METADATA_SEARCHER()
}

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
    int index =
      FindClassParentIndex(IGF.IGM, cast<ClassDecl>(decl)).getTargetIndex();
    return emitLoadOfMetadataRefAtIndex(IGF, metadata, index);
  }

  case DeclKind::Enum:
  case DeclKind::Struct:
    // In both of these cases, 'Parent' is always the third field.
    return emitLoadOfMetadataRefAtIndex(IGF, metadata, 2);
  }
  llvm_unreachable("bad decl kind!");
}

namespace {
  /// A class for finding a type argument in a type metadata object.
  BEGIN_GENERIC_METADATA_SEARCHER_1(FindTypeArgumentIndex,
                                    ArchetypeType *, TargetArchetype)
    template <class... T>
    void addGenericArgument(ArchetypeType *argument, T &&...args) {
      if (argument == TargetArchetype)
        this->setTargetOffset();
      super::addGenericArgument(argument, std::forward<T>(args)...);
    }
  END_GENERIC_METADATA_SEARCHER(ArgumentIndex)
}

/// Given a reference to nominal type metadata of the given type,
/// derive a reference to the nth argument metadata.  The type must
/// have generic arguments.
llvm::Value *irgen::emitArgumentMetadataRef(IRGenFunction &IGF,
                                            NominalTypeDecl *decl,
                                            unsigned argumentIndex,
                                            llvm::Value *metadata) {
  assert(decl->getGenericParams() != nullptr);
  auto targetArchetype =
    decl->getGenericParams()->getAllArchetypes()[argumentIndex];

  switch (decl->getKind()) {
#define NOMINAL_TYPE_DECL(id, parent)
#define DECL(id, parent) \
  case DeclKind::id:
#include "swift/AST/DeclNodes.def"
    llvm_unreachable("not a nominal type");

  case DeclKind::Protocol:
    llvm_unreachable("protocols are never generic!");

  case DeclKind::Class: {
    int index =
      FindClassArgumentIndex(IGF.IGM, cast<ClassDecl>(decl), targetArchetype)
        .getTargetIndex();
    return emitLoadOfMetadataRefAtIndex(IGF, metadata, index);
  }

  case DeclKind::Struct: {
    int index =
      FindStructArgumentIndex(IGF.IGM, cast<StructDecl>(decl), targetArchetype)
        .getTargetIndex();
    return emitLoadOfMetadataRefAtIndex(IGF, metadata, index);
  }

  case DeclKind::Enum: {
    int index =
      FindEnumArgumentIndex(IGF.IGM, cast<EnumDecl>(decl), targetArchetype)
        .getTargetIndex();
    return emitLoadOfMetadataRefAtIndex(IGF, metadata, index);
  }
  }
  llvm_unreachable("bad decl kind!");
}

namespace {
  /// A class for finding a protocol witness table for a type argument
  /// in a value type metadata object.
  BEGIN_GENERIC_METADATA_SEARCHER_2(FindTypeWitnessTableIndex,
                                    ArchetypeType *, TargetArchetype,
                                    ProtocolDecl *, TargetProtocol)
    template <class... T>
    void addGenericWitnessTable(ArchetypeType *argument,
                                ProtocolDecl *protocol,
                                T &&...args) {
      if (argument == TargetArchetype && protocol == TargetProtocol)
        this->setTargetOffset();
      super::addGenericWitnessTable(argument, protocol,
                                    std::forward<T>(args)...);
    }
  END_GENERIC_METADATA_SEARCHER(WitnessTableIndex)
}

/// Given a reference to nominal type metadata of the given type,
/// derive a reference to a protocol witness table for the nth
/// argument metadata.  The type must have generic arguments.
llvm::Value *irgen::emitArgumentWitnessTableRef(IRGenFunction &IGF,
                                                NominalTypeDecl *decl,
                                                unsigned argumentIndex,
                                                ProtocolDecl *targetProtocol,
                                                llvm::Value *metadata) {
  assert(decl->getGenericParams() != nullptr);
  auto targetArchetype =
    decl->getGenericParams()->getAllArchetypes()[argumentIndex];

  switch (decl->getKind()) {
#define NOMINAL_TYPE_DECL(id, parent)
#define DECL(id, parent) \
  case DeclKind::id:
#include "swift/AST/DeclNodes.def"
    llvm_unreachable("not a nominal type");

  case DeclKind::Protocol:
    llvm_unreachable("protocols are never generic!");

  case DeclKind::Class: {
    int index =
      FindClassWitnessTableIndex(IGF.IGM, cast<ClassDecl>(decl),
                                 targetArchetype, targetProtocol)
        .getTargetIndex();
    return emitLoadOfWitnessTableRefAtIndex(IGF, metadata, index);
  }

  case DeclKind::Enum: {
    int index =
      FindEnumWitnessTableIndex(IGF.IGM, cast<EnumDecl>(decl),
                                 targetArchetype, targetProtocol)
        .getTargetIndex();
    return emitLoadOfWitnessTableRefAtIndex(IGF, metadata, index);
  }
      
  case DeclKind::Struct: {
    int index =
      FindStructWitnessTableIndex(IGF.IGM, cast<StructDecl>(decl),
                                  targetArchetype, targetProtocol)
        .getTargetIndex();
    return emitLoadOfWitnessTableRefAtIndex(IGF, metadata, index);
  }
  }
  llvm_unreachable("bad decl kind!");
}

/// Given a reference to class metadata of the given type,
/// derive a reference to the field offset for a stored property.
/// The type must have dependent generic layout.
llvm::Value *irgen::emitClassFieldOffset(IRGenFunction &IGF,
                                         ClassDecl *theClass,
                                         VarDecl *field,
                                         llvm::Value *metadata) {
  /// A class for finding a field offset in a class metadata object.
  BEGIN_METADATA_SEARCHER_1(FindClassFieldOffset, Class,
                            VarDecl *, TargetField)
    void addFieldOffset(VarDecl *field) {
      if (field == TargetField)
        setTargetOffset();
      super::addFieldOffset(field);
    }
  END_METADATA_SEARCHER()

  int index = FindClassFieldOffset(IGF.IGM, theClass, field).getTargetIndex();
  llvm::Value *val = emitLoadOfWitnessTableRefAtIndex(IGF, metadata, index);
  return IGF.Builder.CreatePtrToInt(val, IGF.IGM.SizeTy);
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
  auto loadZExtInt32AtOffset = [&](Size offset) {
    Address slot = IGF.Builder.CreateConstByteArrayGEP(metadataAsBytes, offset);
    slot = IGF.Builder.CreateBitCast(slot, IGF.IGM.Int32Ty->getPointerTo());
    llvm::Value *result = IGF.Builder.CreateLoad(slot);
    if (IGF.IGM.SizeTy != IGF.IGM.Int32Ty)
      result = IGF.Builder.CreateZExt(result, IGF.IGM.SizeTy);
    return result;
  };
  llvm::Value *size = loadZExtInt32AtOffset(scanner.InstanceSize);
  llvm::Value *alignMask = loadZExtInt32AtOffset(scanner.InstanceAlignMask);
  return {size, alignMask};
}

/// Given a pointer to a heap object, load its heap metadata pointer using the
/// ObjC runtime.
static llvm::Value *emitLoadOfObjCHeapMetadataRef(IRGenFunction &IGF,
                                                  llvm::Value *object) {
  object = IGF.Builder.CreateBitCast(object, IGF.IGM.ObjCPtrTy);
  auto metadata = IGF.Builder.CreateCall(IGF.IGM.getGetObjectClassFn(),
                                         object,
                                         object->getName() + ".class");
  metadata->setCallingConv(IGF.IGM.RuntimeCC);
  metadata->setDoesNotThrow();
  metadata->setDoesNotAccessMemory();
  return metadata;
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
}

static bool isKnownNotTaggedPointer(IRGenModule &IGM, ClassDecl *theClass) {
  // For now, assume any class type defined in Clang might be tagged.
  return hasKnownSwiftMetadata(IGM, theClass);
}

/// Given an object of class type, produce the heap metadata reference
/// as an %objc_class*.
llvm::Value *irgen::emitHeapMetadataRefForHeapObject(IRGenFunction &IGF,
                                                     llvm::Value *object,
                                                     CanType objectType,
                                                     bool suppressCast) {
  ClassDecl *theClass = objectType.getClassOrBoundGenericClass();
  if (isKnownNotTaggedPointer(IGF.IGM, theClass))
    return emitLoadOfHeapMetadataRef(IGF, object,
                                     getIsaEncodingForType(IGF.IGM, objectType),
                                     suppressCast);

  // OK, ask the runtime for the class pointer of this
  // potentially-ObjC object.
  return emitLoadOfObjCHeapMetadataRef(IGF, object);
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
llvm::Value *irgen::emitTypeMetadataRefForOpaqueHeapObject(IRGenFunction &IGF,
                                                           llvm::Value *object)
{
  object = IGF.Builder.CreateBitCast(object, IGF.IGM.ObjCPtrTy);
  auto metadata = IGF.Builder.CreateCall(IGF.IGM.getGetObjectTypeFn(),
                                         object,
                                         object->getName() + ".Type");
  metadata->setCallingConv(IGF.IGM.RuntimeCC);
  metadata->setDoesNotThrow();
  metadata->setDoesNotAccessMemory();
  return metadata;
}

/// Given an object of class type, produce the type metadata reference
/// as a %type*.
llvm::Value *irgen::emitTypeMetadataRefForHeapObject(IRGenFunction &IGF,
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
  return emitTypeMetadataRefForOpaqueHeapObject(IGF, object);
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
    Address(IGF.Builder.CreateStructGEP(metatype, 0),
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
    emitLoadFromMetadataAtIndex(IGF, metatype, 1, IGF.IGM.TypeMetadataPtrTy);
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
  /// A class for finding a protocol witness table for a type argument
  /// in a class metadata object.
  BEGIN_METADATA_SEARCHER_1(FindClassMethodIndex, Class,
                            SILDeclRef, TargetMethod)
    void addMethod(SILDeclRef fn) {
      if (TargetMethod == fn)
        setTargetOffset();
      super::addMethod(fn);
    }
  END_METADATA_SEARCHER()
}

/// Provide the abstract parameters for virtual calls to the given method.
AbstractCallee irgen::getAbstractVirtualCallee(IRGenFunction &IGF,
                                               FuncDecl *method) {
  // TODO: maybe use better versions in the v-table sometimes?
  ResilienceExpansion bestExplosion = ResilienceExpansion::Minimal;
  unsigned naturalUncurry = method->getNaturalArgumentCount() - 1;

  return AbstractCallee(AbstractCC::Method, bestExplosion,
                        naturalUncurry, naturalUncurry, ExtraData::None);
}

/// Find the function which will actually appear in the virtual table.
static AbstractFunctionDecl *findOverriddenFunction(
                                        IRGenModule &IGM,
                                        AbstractFunctionDecl *method,
                                        ResilienceExpansion explosionLevel,
                                        unsigned uncurryLevel) {
  // 'method' is the most final method in the hierarchy which we
  // haven't yet found a compatible override for.  'cur' is the method
  // we're currently looking at.  Compatibility is transitive,
  // so we can forget our original method and just keep going up.

  AbstractFunctionDecl *cur = method;
  while ((cur = cur->getOverriddenDecl())) {
    if (!hasKnownVTableEntry(IGM, cur))
      break;
    method = cur;
  }
  return method;
}

/// Load the correct virtual function for the given class method.
llvm::Value *irgen::emitVirtualMethodValue(IRGenFunction &IGF,
                                           llvm::Value *base,
                                           SILType baseType,
                                           SILDeclRef method,
                                           CanSILFunctionType methodType,
                                           ResilienceExpansion maxExplosion) {
  // FIXME: Support property accessors.
  AbstractFunctionDecl *methodDecl
    = cast<AbstractFunctionDecl>(method.getDecl());

  // Find the function that's actually got an entry in the metadata.
  AbstractFunctionDecl *overridden =
    findOverriddenFunction(IGF.IGM, methodDecl,
                           method.getResilienceExpansion(), method.uncurryLevel);

  // Find the metadata.
  llvm::Value *metadata;
  if ((isa<FuncDecl>(methodDecl) && cast<FuncDecl>(methodDecl)->isStatic()) ||
      (isa<ConstructorDecl>(methodDecl) &&
       method.kind == SILDeclRef::Kind::Allocator)) {
    metadata = base;
  } else {
    metadata = emitHeapMetadataRefForHeapObject(IGF, base, baseType,
                                                /*suppress cast*/ true);
  }

  // Use the type of the method we were type-checked against, not the
  // type of the overridden method.
  llvm::AttributeSet attrs;
  auto fnTy = IGF.IGM.getFunctionType(methodType, method.getResilienceExpansion(),
                                      ExtraData::None, attrs)->getPointerTo();

  SILDeclRef fnRef(overridden, method.kind, method.getResilienceExpansion(),
                   method.uncurryLevel);
  auto declaringClass = cast<ClassDecl>(overridden->getDeclContext());
  auto index = FindClassMethodIndex(IGF.IGM, declaringClass, fnRef)
                 .getTargetIndex();

  return emitLoadFromMetadataAtIndex(IGF, metadata, index, fnTy);
}

//===----------------------------------------------------------------------===//
// Foreign types
//===----------------------------------------------------------------------===//

namespace {
  /// A CRTP layout class for foreign type metadata.
  template <class Impl>
  class ForeignTypeMetadataLayout {
  protected:
    IRGenModule &IGM;
    Impl &asImpl() { return *static_cast<Impl*>(this); }

    ForeignTypeMetadataLayout(IRGenModule &IGM) : IGM(IGM) {}

  public:
    void layout() {
      if (asImpl().requiresInitializationFunction())
        asImpl().addInitializationFunction();
      asImpl().addValueWitnessTable();
      asImpl().noteAddressPoint();
      asImpl().addMetadataFlags();
      asImpl().addForeignName();
      asImpl().addUniquePointer();
      asImpl().addForeignFlags();
    }

    void addInitializationFunction() {
      llvm_unreachable("should have overridden this method if "
                       "you need an initialization function");
    }
  };

  /// A CRTP layout class for foreign class metadata.
  template <class Impl>
  class ForeignClassMetadataLayout
         : public ForeignTypeMetadataLayout<Impl> {
    using super = ForeignTypeMetadataLayout<Impl>;
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
  };

  /// A builder for ForeignClassMetadata.
  class ForeignClassMetadataBuilder : public ConstantBuilder<
                  ForeignClassMetadataLayout<ForeignClassMetadataBuilder>> {
    Size AddressPoint = Size::invalid();
  public:
    ForeignClassMetadataBuilder(IRGenModule &IGM, ClassDecl *target)
      : ConstantBuilder(IGM, target) {}

    Size getOffsetOfAddressPoint() const { return AddressPoint; }

    // Visitor methods.

    void addValueWitnessTable() {
      auto type = IGM.Context.TheUnknownObjectType;
      auto wtable = IGM.getAddrOfValueWitnessTable(type);
      addWord(wtable);
    }

    void noteAddressPoint() {
      AddressPoint = getNextOffset();
    }

    void addMetadataFlags() {
      addConstantWord((unsigned) MetadataKind::ForeignClass);
    }

    void addForeignName() {
      addWord(getMangledTypeName(IGM,
                             Target->getDeclaredType()->getCanonicalType()));
    }

    void addUniquePointer() {
      addWord(llvm::ConstantPointerNull::get(IGM.TypeMetadataPtrTy));
    }

    void addForeignFlags() {
      int64_t flags = 0;
      if (requiresInitializationFunction()) flags |= 1;
      addConstantWord(flags);
    }

    void addSuperClass() {
      // TODO: superclasses
      addWord(llvm::ConstantPointerNull::get(IGM.TypeMetadataPtrTy));
    }

    void addReservedWord() {
      addWord(llvm::ConstantPointerNull::get(IGM.Int8PtrTy));
    }
  };
}

llvm::Constant *
irgen::emitForeignTypeMetadataInitializer(IRGenModule &IGM, CanType type,
                                          Size &offsetOfAddressPoint) {
  if (auto classType = dyn_cast<ClassType>(type)) {
    assert(!classType.getParent());
    auto classDecl = classType->getDecl();
    assert(classDecl->isForeign());

    ForeignClassMetadataBuilder builder(IGM, classDecl);
    builder.layout();
    offsetOfAddressPoint = builder.getOffsetOfAddressPoint();
    return builder.getInit();
  } else {
    llvm_unreachable("foreign type metadata layout for non-class!");
  }
}

//===----------------------------------------------------------------------===//
// Structs
//===----------------------------------------------------------------------===//

namespace {
  /// An adapter for laying out struct metadata.
  template <class Impl>
  class StructMetadataBuilderBase
         : public ConstantBuilder<StructMetadataLayout<Impl>> {
    using super = ConstantBuilder<StructMetadataLayout<Impl>>;

  protected:
    using super::IGM;
    using super::Target;
    using super::addConstantWord;
    using super::addWord;

    StructMetadataBuilderBase(IRGenModule &IGM, StructDecl *theStruct)
      : super(IGM, theStruct) {}

  public:
    void addMetadataFlags() {
      addWord(getMetadataKind(IGM, MetadataKind::Struct));
    }

    void addNominalTypeDescriptor() {
      addWord(StructNominalTypeDescriptorBuilder(IGM, Target).emit());
    }

    void addParentMetadataRef() {
      // FIXME!
      addWord(llvm::ConstantPointerNull::get(IGM.TypeMetadataPtrTy));
    }
    
    void addFieldOffset(VarDecl *var) {
      assert(var->hasStorage() &&
             "storing field offset for computed property?!");
      SILType structType =
        SILType::getPrimitiveAddressType(
                       Target->getDeclaredTypeInContext()->getCanonicalType());

      llvm::Constant *offset =
        emitPhysicalStructMemberFixedOffset(IGM, structType, var);
      // If we have a fixed offset, add it. Otherwise, leave zero as a
      // placeholder.
      if (offset)
        addWord(offset);
      else
        addConstantWord(0);
    }

    void addGenericArgument(ArchetypeType *type) {
      addWord(llvm::Constant::getNullValue(IGM.TypeMetadataPtrTy));
    }

    void addGenericWitnessTable(ArchetypeType *type, ProtocolDecl *protocol) {
      addWord(llvm::Constant::getNullValue(IGM.WitnessTablePtrTy));
    }

    llvm::Constant *getInit() {
      return this->getInitWithSuggestedType(NumHeapMetadataFields,
                                            IGM.FullHeapMetadataStructTy);
    }
  };

  class StructMetadataBuilder :
    public StructMetadataBuilderBase<StructMetadataBuilder> {
  public:
    StructMetadataBuilder(IRGenModule &IGM, StructDecl *theStruct)
      : StructMetadataBuilderBase(IGM, theStruct) {}

    void addValueWitnessTable() {
      auto type = this->Target->getDeclaredType()->getCanonicalType();
      addWord(emitValueWitnessTable(IGM, type));
    }
  };
  
  /// Emit a value witness table for a fixed-layout generic type, or a null
  /// placeholder if the value witness table is dependent on generic parameters.
  /// Returns true if the value witness table is dependent.
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
                                const GenericParamList &structGenerics)
      : super(IGM, structGenerics, theStruct) {}

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
      emitPolymorphicParametersForGenericValueWitness(IGF, Target, metadata);
      IGM.getTypeInfoForLowered(CanType(Target->getDeclaredTypeInContext()))
        .initializeMetadata(IGF, metadata, vwtable,
                            Target->getDeclaredTypeInContext()
                              ->getCanonicalType());
    }
  };
}

/// Emit the type metadata or metadata template for a struct.
void irgen::emitStructMetadata(IRGenModule &IGM, StructDecl *structDecl) {
  // TODO: structs nested within generic types
  llvm::Constant *init;
  bool isPattern;
  if (auto *generics = structDecl->getGenericParamsOfContext()) {
    GenericStructMetadataBuilder builder(IGM, structDecl, *generics);
    builder.layout();
    init = builder.getInit();
    isPattern = true;
  } else {
    StructMetadataBuilder builder(IGM, structDecl);
    builder.layout();
    init = builder.getInit();
    isPattern = false;
  }

  // For now, all type metadata is directly stored.
  bool isIndirect = false;

  CanType declaredType = structDecl->getDeclaredType()->getCanonicalType();
  auto var = cast<llvm::GlobalVariable>(
                     IGM.getAddrOfTypeMetadata(declaredType,
                                               isIndirect, isPattern,
                                               init->getType()));
  var->setConstant(!isPattern);
  var->setInitializer(init);
}

// Enums

namespace {

template<class Impl>
class EnumMetadataBuilderBase
       : public ConstantBuilder<EnumMetadataLayout<Impl>> {
  using super = ConstantBuilder<EnumMetadataLayout<Impl>>;

protected:
  using super::IGM;
  using super::Target;
  using super::addWord;

public:
  EnumMetadataBuilderBase(IRGenModule &IGM, EnumDecl *theEnum)
    : super(IGM, theEnum) {}
  
  void addMetadataFlags() {
    addWord(getMetadataKind(IGM, MetadataKind::Enum));
  }
  
  void addNominalTypeDescriptor() {
    // FIXME!
    addWord(EnumNominalTypeDescriptorBuilder(IGM, Target).emit());
  }
  
  void addParentMetadataRef() {
    // FIXME!
    addWord(llvm::ConstantPointerNull::get(IGM.TypeMetadataPtrTy));
  }
  
  void addGenericArgument(ArchetypeType *type) {
    addWord(llvm::Constant::getNullValue(IGM.TypeMetadataPtrTy));
  }
  
  void addGenericWitnessTable(ArchetypeType *type, ProtocolDecl *protocol) {
    addWord(llvm::Constant::getNullValue(IGM.WitnessTablePtrTy));
  }
};
  
class EnumMetadataBuilder
  : public EnumMetadataBuilderBase<EnumMetadataBuilder>
{
public:
  EnumMetadataBuilder(IRGenModule &IGM, EnumDecl *theEnum)
    : EnumMetadataBuilderBase(IGM, theEnum) {}
  
  void addValueWitnessTable() {
    auto type = Target->getDeclaredType()->getCanonicalType();
    addWord(emitValueWitnessTable(IGM, type));
  }
};
  
class GenericEnumMetadataBuilder
  : public GenericMetadataBuilderBase<GenericEnumMetadataBuilder,
                        EnumMetadataBuilderBase<GenericEnumMetadataBuilder>>
{
public:
  GenericEnumMetadataBuilder(IRGenModule &IGM, EnumDecl *theEnum,
                              const GenericParamList &enumGenerics)
    : GenericMetadataBuilderBase(IGM, enumGenerics, theEnum) {}
  
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
    emitPolymorphicParametersForGenericValueWitness(IGF, Target, metadata);
    IGM.getTypeInfoForLowered(CanType(Target->getDeclaredTypeInContext()))
      .initializeMetadata(IGF, metadata, vwtable,
                          Target->getDeclaredTypeInContext()
                            ->getCanonicalType());
  }
};
  
}

void irgen::emitEnumMetadata(IRGenModule &IGM, EnumDecl *theEnum) {
  // TODO: enums nested inside generic types
  llvm::Constant *init;
  
  bool isPattern;
  if (auto *generics = theEnum->getGenericParamsOfContext()) {
    GenericEnumMetadataBuilder builder(IGM, theEnum, *generics);
    builder.layout();
    init = builder.getInit();
    isPattern = true;
  } else {
    EnumMetadataBuilder builder(IGM, theEnum);
    builder.layout();
    init = builder.getInit();
    isPattern = false;
  }
  
  // For now, all type metadata is directly stored.
  bool isIndirect = false;
  
  CanType declaredType = theEnum->getDeclaredType()->getCanonicalType();
  auto var = cast<llvm::GlobalVariable>(
                              IGM.getAddrOfTypeMetadata(declaredType,
                                                        isIndirect, isPattern,
                                                        init->getType()));
  var->setConstant(!isPattern);
  var->setInitializer(init);
}

llvm::Value *IRGenFunction::emitObjCSelectorRefLoad(StringRef selector) {
  llvm::Constant *loadSelRef = IGM.getAddrOfObjCSelectorRef(selector);
  llvm::Value *loadSel =
    Builder.CreateLoad(Address(loadSelRef, IGM.getPointerAlignment()));

  // When generating JIT'd code, we need to call sel_registerName() to force
  // the runtime to unique the selector. For non-JIT'd code, the linker will
  // do it for us.
  if (IGM.Opts.UseJIT) {
    loadSel = Builder.CreateCall(IGM.getObjCSelRegisterNameFn(), loadSel);
  }

  return loadSel;
}

// Protocols

namespace {
  class ProtocolDescriptorBuilder {
    IRGenModule &IGM;
    ProtocolDecl *Protocol;

    SmallVector<llvm::Constant*, 8> Fields;

  public:
    ProtocolDescriptorBuilder(IRGenModule &IGM, ProtocolDecl *protocol)
      : IGM(IGM), Protocol(protocol) {}

    void layout() {
      addObjCCompatibilityIsa();
      addName();
      addInherited();
      addObjCCompatibilityTables();
      addSize();
      addFlags();
    }

    llvm::Constant *null() {
      return llvm::ConstantPointerNull::get(IGM.Int8PtrTy);
    }
    
    void addObjCCompatibilityIsa() {
      // The ObjC runtime will drop a reference to its magic Protocol class
      // here.
      Fields.push_back(null());
    }
    
    void addName() {
      Fields.push_back(getMangledTypeName(IGM,
                             Protocol->getDeclaredType()->getCanonicalType()));
    }
    
    void addInherited() {
      // If there are no inherited protocols, produce null.
      auto inherited = Protocol->getProtocols();
      if (inherited.empty()) {
        Fields.push_back(null());
        return;
      }
      
      // Otherwise, collect references to all of the inherited protocol
      // descriptors.
      SmallVector<llvm::Constant*, 4> inheritedDescriptors;
      inheritedDescriptors.push_back(IGM.getSize(Size(inherited.size())));
      
      for (ProtocolDecl *p : inherited) {
        auto descriptor = IGM.getAddrOfProtocolDescriptor(p, NotForDefinition);
        inheritedDescriptors.push_back(descriptor);
      }
      
      auto inheritedInit = llvm::ConstantStruct::getAnon(inheritedDescriptors);
      auto inheritedVar = new llvm::GlobalVariable(IGM.Module,
                                           inheritedInit->getType(),
                                           /*isConstant*/ true,
                                           llvm::GlobalValue::InternalLinkage,
                                           inheritedInit);
      
      llvm::Constant *inheritedVarPtr
        = llvm::ConstantExpr::getBitCast(inheritedVar, IGM.Int8PtrTy);
      Fields.push_back(inheritedVarPtr);
    }
    
    void addObjCCompatibilityTables() {
      // Required instance methods
      Fields.push_back(null());
      // Required class methods
      Fields.push_back(null());
      // Optional instance methods
      Fields.push_back(null());
      // Optional class methods
      Fields.push_back(null());
      // Properties
      Fields.push_back(null());
    }
    
    void addSize() {
      // The number of fields so far in words, plus 4 bytes for size and
      // 4 bytes for flags.
      unsigned sz = (Fields.size() * IGM.getPointerSize()).getValue() + 4 + 4;
      Fields.push_back(llvm::ConstantInt::get(IGM.Int32Ty, sz));
    }
    
    void addFlags() {
      // enum : uint32_t {
      //   IsSwift           = 1U <<  0U,
      unsigned flags = 1;
      
      //   ClassConstraint   = 1U <<  1U,
      // Set if the protocol is *not* class constrained.
      if (!Protocol->requiresClass())
        flags |= (1U << 1U);
      
      //   NeedsWitnessTable = 1U <<  2U,
      if (requiresProtocolWitnessTable(Protocol))
        flags |= (1U << 2U);

      // };
      
      Fields.push_back(llvm::ConstantInt::get(IGM.Int32Ty, flags));
    }

    void addValueWitnessTable() {
      // Build a fresh value witness table.  FIXME: this is actually
      // unnecessary --- every existential type will have the exact
      // same value witness table.
      CanType type = CanType(Protocol->getDeclaredType());
      Fields.push_back(emitValueWitnessTable(IGM, type));
    }

    llvm::Constant *getInit() {
      return llvm::ConstantStruct::get(IGM.ProtocolDescriptorStructTy,
                                       Fields);
    }
  };
} // end anonymous namespace

/// Emit global structures associated with the given protocol. This comprises
/// the protocol descriptor, and for ObjC interop, references to the descriptor
/// that the ObjC runtime uses for uniquing.
void IRGenModule::emitProtocolDecl(ProtocolDecl *protocol) {
  // If the protocol is Objective-C-compatible, go through the path that
  // produces an ObjC-compatible protocol_t.
  if (protocol->isObjC()) {
    // Native ObjC protocols are emitted on-demand in ObjC and uniqued by the
    // runtime; we don't need to try to emit a unique descriptor symbol for them.
    if (protocol->hasClangNode())
      return;
    
    getObjCProtocolGlobalVars(protocol);
    return;
  }
  
  ProtocolDescriptorBuilder builder(*this, protocol);
  builder.layout();
  auto init = builder.getInit();

  auto var = cast<llvm::GlobalVariable>(
                       getAddrOfProtocolDescriptor(protocol, ForDefinition));
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
    return IGF.IGM.getAddrOfProtocolDescriptor(protocol, NotForDefinition);
  
  auto refVar = IGF.IGM.getAddrOfObjCProtocolRef(protocol, NotForDefinition);
  llvm::Value *val
    = IGF.Builder.CreateLoad(refVar, IGF.IGM.getPointerAlignment());
  val = IGF.Builder.CreateBitCast(val,
                          IGF.IGM.ProtocolDescriptorStructTy->getPointerTo());
  return val;
}
