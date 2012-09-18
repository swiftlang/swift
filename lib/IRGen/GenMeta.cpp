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
/// Emit a DeclRefExpr which refers to a metatype.
void irgen::emitMetaTypeRef(IRGenFunction &IGF, Type type,
                            Explosion &explosion) {
  // For now, all metatype types are empty.
}
