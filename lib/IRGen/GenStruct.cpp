//===--- GenStruct.cpp - Swift IR Generation For 'struct' Types -----------===//
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
//  This file implements IR generation for struct types.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/Types.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Expr.h"
#include "swift/Basic/Optional.h"
#include "llvm/DerivedTypes.h"
#include "llvm/Function.h"

#include "GenType.h"
#include "IRGenFunction.h"
#include "IRGenModule.h"
#include "LValue.h"
#include "Explosion.h"

using namespace swift;
using namespace irgen;

namespace {
  /// The TypeInfo implementation for structs.
  class StructTypeInfo : public TypeInfo {
  public:
    static Address getSingletonAddress(IRGenFunction &IGF, Address addr) {
      llvm::Value *singletonAddr =
        IGF.Builder.CreateStructGEP(addr.getAddress(), 0);
      return Address(singletonAddr, addr.getAlignment());
    }

    /// The type info of the singleton member, or null if it carries no data.
    const TypeInfo *Singleton;

    StructTypeInfo(llvm::StructType *T, Size S, Alignment A,
                           IsPOD_t isPOD)
      : TypeInfo(T, S, A, isPOD), Singleton(nullptr) {}

    void getSchema(ExplosionSchema &schema) const {
      assert(isComplete());
      if (Singleton) Singleton->getSchema(schema);
    }

    unsigned getExplosionSize(ExplosionKind kind) const {
      assert(isComplete());
      if (!Singleton) return 0;
      return Singleton->getExplosionSize(kind);
    }

    void load(IRGenFunction &IGF, Address addr, Explosion &e) const {
      if (!Singleton) return;
      Singleton->load(IGF, getSingletonAddress(IGF, addr), e);
    }

    void loadAsTake(IRGenFunction &IGF, Address addr, Explosion &e) const {
      if (!Singleton) return;
      Singleton->loadAsTake(IGF, getSingletonAddress(IGF, addr), e);
    }

    void assign(IRGenFunction &IGF, Explosion &e, Address addr) const {
      if (!Singleton) return;
      Singleton->assign(IGF, e, getSingletonAddress(IGF, addr));
    }

    void initialize(IRGenFunction &IGF, Explosion &e, Address addr) const {
      if (!Singleton) return;
      Singleton->initialize(IGF, e, getSingletonAddress(IGF, addr));
    }

    void reexplode(IRGenFunction &IGF, Explosion &src, Explosion &dest) const {
      if (Singleton) Singleton->reexplode(IGF, src, dest);
    }

    void copy(IRGenFunction &IGF, Explosion &src, Explosion &dest) const {
      if (Singleton) Singleton->copy(IGF, src, dest);
    }

    void manage(IRGenFunction &IGF, Explosion &src, Explosion &dest) const {
      if (Singleton) Singleton->manage(IGF, src, dest);
    }

    void destroy(IRGenFunction &IGF, Address addr) const {
      if (Singleton && !isPOD(ResilienceScope::Local))
        Singleton->destroy(IGF, getSingletonAddress(IGF, addr));
    }

    void emitInjectionFunctionBody(IRGenFunction &IGF,
                                   OneOfElementDecl *elt,
                                   Explosion &params) const {
      // If this oneof carries no data, the function must take no
      // arguments and return void.
      if (!Singleton) {
        IGF.Builder.CreateRetVoid();
        return;
      }

      // Otherwise, package up the result.
      ExplosionSchema schema(params.getKind());
      Singleton->getSchema(schema);
      if (schema.requiresIndirectResult()) {
        Address returnSlot(params.claimUnmanagedNext(),
                           Singleton->StorageAlignment);
        initialize(IGF, params, returnSlot);
        IGF.Builder.CreateRetVoid();
      } else {
        IGF.emitScalarReturn(params);
      }
    }
  };
}  // end anonymous namespace.

static void emitInjectionFunction(IRGenModule &IGM,
                                  llvm::Function *fn,
                                  const StructTypeInfo &structTI,
                                  OneOfElementDecl *elt) {
  ExplosionKind explosionKind = ExplosionKind::Minimal;
  IRGenFunction IGF(IGM, Type(), ArrayRef<Pattern*>(), explosionKind,
                    /*uncurry level*/ 0, fn, Prologue::Bare);

  Explosion explosion = IGF.collectParameters();
  structTI.emitInjectionFunctionBody(IGF, elt, explosion);
}

/// emitStructType - Emit all the declarations associated with this oneof type.
void IRGenModule::emitStructType(StructType *oneof) {
  const StructTypeInfo &typeInfo = getFragileTypeInfo(oneof).as<StructTypeInfo>();
  OneOfElementDecl *elt = oneof->getDecl()->getElement();
  llvm::Function *fn = getAddrOfGlobalInjectionFunction(elt);
  emitInjectionFunction(*this, fn, typeInfo, elt);
}

void IRGenFunction::emitStructType(StructType *oneof) {
  const StructTypeInfo &typeInfo = getFragileTypeInfo(oneof).as<StructTypeInfo>();
  OneOfElementDecl *elt = oneof->getDecl()->getElement();
  llvm::Function *fn = getAddrOfLocalInjectionFunction(elt);
  emitInjectionFunction(IGM, fn, typeInfo, elt);
}

const TypeInfo *
TypeConverter::convertStructType(IRGenModule &IGM, StructType *T) {
  StructDecl *D = T->getDecl();
  llvm::StructType *convertedStruct = IGM.createNominalType(D);

  StructTypeInfo *convertedTInfo =
    new StructTypeInfo(convertedStruct, Size(0), Alignment(0), IsPOD);
  assert(!IGM.Types.Converted.count(T));
  IGM.Types.Converted.insert(std::make_pair(T, convertedTInfo));

  Type eltType = D->getUnderlyingType();

  llvm::Type *storageType;
  const TypeInfo &eltTInfo = getFragileTypeInfo(IGM, eltType);
  assert(eltTInfo.isComplete());
  storageType = eltTInfo.StorageType;
  convertedTInfo->StorageSize = eltTInfo.StorageSize;
  convertedTInfo->StorageAlignment = eltTInfo.StorageAlignment;
  convertedTInfo->Singleton = &eltTInfo;
  convertedTInfo->setPOD(eltTInfo.isPOD(ResilienceScope::Local));

  llvm::Type *body[] = { storageType };
  convertedStruct->setBody(body);

  return convertedTInfo;
}
