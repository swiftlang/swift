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

#include "swift/AST/Decl.h"
#include "swift/AST/Types.h"
#include "llvm/DerivedTypes.h"
#include "llvm/ADT/SmallString.h"

#include "Address.h"
#include "FixedTypeInfo.h"
#include "GenProto.h"
#include "GenType.h"
#include "IRGenModule.h"
#include "ScalarTypeInfo.h"
#include "TypeVisitor.h"

#include "GenMeta.h"

using namespace swift;
using namespace irgen;

namespace {
  struct EmptyTypeInfo : ScalarTypeInfo<EmptyTypeInfo, FixedTypeInfo> {
    EmptyTypeInfo(llvm::Type *ty)
      : ScalarTypeInfo(ty, Size(0), Alignment(1), IsPOD) {}
    unsigned getExplosionSize(ExplosionKind kind) const { return 0; }
    void getSchema(ExplosionSchema &schema) const {}
    void load(IRGenFunction &IGF, Address addr, Explosion &e) const {}
    void loadAsTake(IRGenFunction &IGF, Address addr, Explosion &e) const {}
    void assign(IRGenFunction &IGF, Explosion &e, Address addr) const {}
    void initialize(IRGenFunction &IGF, Explosion &e, Address addr) const {}
    void copy(IRGenFunction &IGF, Explosion &src, Explosion &dest) const {}
    void manage(IRGenFunction &IGF, Explosion &src, Explosion &dest) const {}
    void destroy(IRGenFunction &IGF, Address addr) const {}
  };
}

const TypeInfo *TypeConverter::convertMetaTypeType(MetaTypeType *T) {
  return new EmptyTypeInfo(IGM.Int8Ty);
}

const TypeInfo *TypeConverter::convertModuleType(ModuleType *T) {
  return new EmptyTypeInfo(IGM.Int8Ty);
}

/// Returns a metadata reference for a class type.
llvm::Value *irgen::emitNominalMetadataRef(IRGenFunction &IGF,
                                           NominalTypeDecl *theDecl,
                                           CanType theType) {
  auto generics = theDecl->getGenericParamsOfContext();

  bool isPattern = (generics != nullptr);
  assert(!isPattern || isa<BoundGenericType>(theType));
  assert(isPattern || isa<NominalType>(theType));

  bool isIndirect = false; // FIXME

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

  assert(metadata->getType() == IGF.IGM.TypeMetadataPtrTy);

  // If we don't have generic parameters, that's all we need.
  if (!generics) {
    return metadata;
  }

  // Okay, we need to call swift_getGenericMetadata.

  // Grab the substitutions.
  auto boundGeneric = cast<BoundGenericType>(theType);
  assert(boundGeneric->getDecl() == theDecl);
  auto subs = boundGeneric->getSubstitutions();

  // Compile all the generic arguments we need.
  Explosion genericArgs(ExplosionKind::Maximal);
  emitPolymorphicArguments(IGF, *generics, subs, genericArgs);

  // Slam that information directly into the generic arguments buffer.
  // TODO: sort actual arguments to the front.
  auto wtableArrayTy = llvm::ArrayType::get(IGF.IGM.WitnessTablePtrTy,
                                            genericArgs.size());
  Address argumentsBuffer = IGF.createAlloca(wtableArrayTy,
                                             IGF.IGM.getPointerAlignment(),
                                             "generic.arguments");
  for (unsigned i = 0, e = genericArgs.size(); i != e; ++i) {
    Address elt = IGF.Builder.CreateStructGEP(argumentsBuffer, i,
                                              IGF.IGM.getPointerSize() * i);
    IGF.Builder.CreateStore(genericArgs.claimUnmanagedNext(), elt);
  }

  // Cast to void*.
  llvm::Value *arguments =
    IGF.Builder.CreateBitCast(argumentsBuffer.getAddress(),
                              IGF.IGM.Int8PtrTy);

  // Make the call.
  auto result = IGF.Builder.CreateCall2(IGF.IGM.getGetGenericMetadataFn(),
                                        metadata, arguments);
  result->setDoesNotThrow();

  return result;
}

/// Emit a string encoding the labels in the given tuple type.
static llvm::Constant *getTupleLabelsString(IRGenModule &IGM,
                                            TupleType *type) {
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
    : public irgen::TypeVisitor<EmitTypeMetadataRef, llvm::Value *> {
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
      IRGenModule &IGM = IGF.IGM;
      const TypeInfo &opaqueTI = IGM.getFragileTypeInfo(type);
      assert(opaqueTI.StorageSize ==
             Size(opaqueTI.StorageAlignment.getValue()));
      assert(opaqueTI.StorageSize.isPowerOf2());
      auto numBits = 8 * opaqueTI.StorageSize.getValue();
      auto intTy = BuiltinIntegerType::get(numBits, IGM.Context);
      return emitDirectMetadataRef(CanType(intTy));
    }

    llvm::Value *visitBuiltinObjectPointerType(BuiltinObjectPointerType *type) {
      return emitDirectMetadataRef(CanType(type));
    }

    llvm::Value *visitBuiltinObjCPointerType(BuiltinObjCPointerType *type) {
      return emitDirectMetadataRef(CanType(type));
    }

    llvm::Value *visitNominalType(NominalType *type) {
      return emitNominalMetadataRef(IGF, type->getDecl(), CanType(type));
    }

    llvm::Value *visitBoundGenericType(BoundGenericType *type) {
      return emitNominalMetadataRef(IGF, type->getDecl(), CanType(type));
    }

    llvm::Value *visitTupleType(TupleType *type) {
      auto elements = type->getFields();

      // I think the sanest thing to do here is drop labels, but maybe
      // that's not correct.  If so, that's really unfortunate in a
      // lot of ways.

      // Er, varargs bit?  Should that go in?

      // For metadata purposes, we consider a singleton tuple to be
      // isomorphic to its element type.
      if (elements.size() == 1)
        return visit(CanType(elements[0].getType()));

      // TODO: use a caching entrypoint (with all information
      // out-of-line) for non-dependent tuples.

      // TODO: make a standard metadata for the empty tuple?

      llvm::Value *pointerToFirst;

      // If there are no elements, we can pass a bogus array pointer.
      if (elements.empty()) {
        auto metadataPtrPtrTy = IGF.IGM.TypeMetadataPtrTy->getPointerTo();
        pointerToFirst = llvm::UndefValue::get(metadataPtrPtrTy);

      // Otherwise, we need to fill out a temporary array to pass.
      } else {
        pointerToFirst = nullptr; // appease -Wuninitialized

        auto arrayTy = llvm::ArrayType::get(IGF.IGM.TypeMetadataPtrTy,
                                            elements.size());
        Address buffer = IGF.createAlloca(arrayTy, IGF.IGM.getPointerAlignment(),
                                          "tuple-elements");
        for (unsigned i = 0, e = elements.size(); i != e; ++i) {
          // Find the metadata pointer for this element.
          llvm::Value *eltMetadata = visit(CanType(elements[i].getType()));

          // GEP to the appropriate element and store.
          Address eltPtr = IGF.Builder.CreateStructGEP(buffer, i,
                                                       IGF.IGM.getPointerSize());
          IGF.Builder.CreateStore(eltMetadata, eltPtr);

          // Remember the GEP to the first element.
          if (i == 0) pointerToFirst = eltPtr.getAddress();
        }
      }

      llvm::Value *args[] = {
        llvm::ConstantInt::get(IGF.IGM.SizeTy, elements.size()),
        pointerToFirst,
        getTupleLabelsString(IGF.IGM, type),
        llvm::ConstantPointerNull::get(IGF.IGM.WitnessTablePtrTy) // proposed
      };

      auto call = IGF.Builder.CreateCall(IGF.IGM.getGetTupleMetadataFn(), args);
      call->setDoesNotThrow();
      call->setCallingConv(IGF.IGM.RuntimeCC);

      return call;
    }

    llvm::Value *visitPolymorphicFunctionType(PolymorphicFunctionType *type) {
      IGF.unimplemented(SourceLoc(),
                        "metadata ref for polymorphic function type");
      return llvm::UndefValue::get(IGF.IGM.TypeMetadataPtrTy);
    }

    llvm::Value *visitFunctionType(FunctionType *type) {
      // TODO: use a caching entrypoint (with all information
      // out-of-line) for non-dependent functions.

      auto argMetadata = visit(CanType(type->getInput()));
      auto resultMetadata = visit(CanType(type->getResult()));

      auto call = IGF.Builder.CreateCall2(IGF.IGM.getGetFunctionMetadataFn(),
                                          argMetadata, resultMetadata);
      call->setDoesNotThrow();
      call->setCallingConv(IGF.IGM.RuntimeCC);

      return call;
    }

    llvm::Value *visitArrayType(ArrayType *type) {
      IGF.unimplemented(SourceLoc(), "metadata ref for array type");
      return llvm::UndefValue::get(IGF.IGM.TypeMetadataPtrTy);
    }

    llvm::Value *visitMetaTypeType(MetaTypeType *type) {
      IGF.unimplemented(SourceLoc(), "metadata ref for metatype type");
      return llvm::UndefValue::get(IGF.IGM.TypeMetadataPtrTy);
    }

    llvm::Value *visitModuleType(ModuleType *type) {
      IGF.unimplemented(SourceLoc(), "metadata ref for module type");
      return llvm::UndefValue::get(IGF.IGM.TypeMetadataPtrTy);
    }

    llvm::Value *visitProtocolCompositionType(ProtocolCompositionType *type) {
      IGF.unimplemented(SourceLoc(), "metadata ref for protocol comp type");
      return llvm::UndefValue::get(IGF.IGM.TypeMetadataPtrTy);
    }

    llvm::Value *visitArchetypeType(ArchetypeType *type) {
      return emitArchetypeMetadataRef(IGF, type);
    }

    llvm::Value *visitLValueType(LValueType *type) {
      IGF.unimplemented(SourceLoc(), "metadata ref for l-value type");
      return llvm::UndefValue::get(IGF.IGM.TypeMetadataPtrTy);
    }
  };
}

/// Produce the type metadata pointer for the given type.
llvm::Value *irgen::emitTypeMetadataRef(IRGenFunction &IGF, Type type) {
  return EmitTypeMetadataRef(IGF).visit(type->getCanonicalType());
}

/// Emit a DeclRefExpr which refers to a metatype.
void irgen::emitMetaTypeRef(IRGenFunction &IGF, Type type,
                            Explosion &explosion) {
  // For now, all metatype types are empty.
}
