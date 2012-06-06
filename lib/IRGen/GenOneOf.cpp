//===--- GenOneOf.cpp - Swift IR Generation For 'oneof' Types -------------===//
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
//  This file implements IR generation for algebraic data types (ADTs,
//  or 'oneof' types) in Swift.  This includes creating the IR type as
//  well as emitting the basic access operations.
//
//  The current scheme is that all such types with are represented
//  with an initial word indicating the variant, followed by a union
//  of all the possibilities.  This is obviously completely acceptable
//  to everyone and will not benefit from further refinement.
//
//  As a completely unimportant premature optimization, we do emit
//  types with only a single variant as simple structs wrapping that
//  variant.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/Types.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Expr.h"
#include "swift/Basic/Optional.h"
#include "llvm/DerivedTypes.h"
#include "llvm/Function.h"

#include "ScalarTypeInfo.h"
#include "IRGenModule.h"
#include "LValue.h"

#include "GenOneOf.h"

using namespace swift;
using namespace irgen;

namespace {
  /// An abstract base class for TypeInfo implementations of oneof types.
  class OneofTypeInfo : public TypeInfo {
  public:
    OneofTypeInfo(llvm::StructType *T, Size S, Alignment A, IsPOD_t isPOD)
      : TypeInfo(T, S, A, isPOD) {}

    llvm::StructType *getStorageType() const {
      return cast<llvm::StructType>(TypeInfo::getStorageType());
    }

    llvm::IntegerType *getDiscriminatorType() const {
      llvm::StructType *Struct = getStorageType();
      return cast<llvm::IntegerType>(Struct->getElementType(0));
    }

    /// Map the given element to the appropriate value in the
    /// discriminator type.
    llvm::ConstantInt *getDiscriminatorIndex(OneOfElementDecl *target) const {
      // FIXME: using a linear search here is fairly ridiculous.
      unsigned index = 0;
      for (auto elt : cast<OneOfDecl>(target->getDeclContext())->getMembers()) {
        if (!isa<OneOfElementDecl>(elt)) continue;
        if (elt == target) break;
        index++;
      }
      return llvm::ConstantInt::get(getDiscriminatorType(), index);
    }

    virtual void emitInjectionFunctionBody(IRGenFunction &IGF,
                                           OneOfElementDecl *elt,
                                           Explosion &params) const = 0;
  };

  /// A TypeInfo implementation which uses an aggregate.
  class AggregateOneofTypeInfo : public OneofTypeInfo {
  public:
    AggregateOneofTypeInfo(llvm::StructType *T, Size S, Alignment A,
                           IsPOD_t isPOD)
      : OneofTypeInfo(T, S, A, isPOD) {}

    void getSchema(ExplosionSchema &schema) const {
      schema.add(ExplosionSchema::Element::forAggregate(getStorageType(),
                                                        StorageAlignment));
    }

    unsigned getExplosionSize(ExplosionKind kind) const {
      return 1;
    }

    void load(IRGenFunction &IGF, Address addr, Explosion &e) const {
      // FIXME
    }

    void loadAsTake(IRGenFunction &IGF, Address addr, Explosion &e) const {
      // FIXME
    }

    void assign(IRGenFunction &IGF, Explosion &e, Address addr) const {
      // FIXME
    }

    void assignWithCopy(IRGenFunction &IGF, Address dest, Address src) const {
      // FIXME
    }

    void assignWithTake(IRGenFunction &IGF, Address dest, Address src) const {
      // FIXME
    }

    void initialize(IRGenFunction &IGF, Explosion &e, Address addr) const {
      // FIXME
    }

    void reexplode(IRGenFunction &IGF, Explosion &src, Explosion &dest) const {
      // FIXME
    }

    void copy(IRGenFunction &IGF, Explosion &src, Explosion &dest) const {
      // FIXME
    }

    void manage(IRGenFunction &IGF, Explosion &src, Explosion &dest) const {
      // FIXME
    }

    void destroy(IRGenFunction &IGF, Address addr) const {
      // FIXME
    }

    void emitInjectionFunctionBody(IRGenFunction &IGF,
                                   OneOfElementDecl *elt,
                                   Explosion &params) const {
      // FIXME
      params.ignoreAndDestroy(IGF, params.size());
      IGF.Builder.CreateRetVoid();
    }
  };

  /// A TypeInfo implementation for singleton oneofs.
  class SingletonOneofTypeInfo : public OneofTypeInfo {
  public:
    static Address getSingletonAddress(IRGenFunction &IGF, Address addr) {
      llvm::Value *singletonAddr =
        IGF.Builder.CreateStructGEP(addr.getAddress(), 0);
      return Address(singletonAddr, addr.getAlignment());
    }

    /// The type info of the singleton member, or null if it carries no data.
    const TypeInfo *Singleton;

    SingletonOneofTypeInfo(llvm::StructType *T, Size S, Alignment A,
                           IsPOD_t isPOD)
      : OneofTypeInfo(T, S, A, isPOD), Singleton(nullptr) {}

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

    void assignWithCopy(IRGenFunction &IGF, Address dest, Address src) const {
      if (!Singleton) return;
      dest = getSingletonAddress(IGF, dest);
      src = getSingletonAddress(IGF, src);
      Singleton->assignWithCopy(IGF, dest, src);
    }

    void assignWithTake(IRGenFunction &IGF, Address dest, Address src) const {
      if (!Singleton) return;
      dest = getSingletonAddress(IGF, dest);
      src = getSingletonAddress(IGF, src);
      Singleton->assignWithTake(IGF, dest, src);
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

  /// A TypeInfo implementation for oneofs with no payload.
  class EnumTypeInfo :
    public PODSingleScalarTypeInfo<EnumTypeInfo,OneofTypeInfo> {
  public:
    EnumTypeInfo(llvm::StructType *T, Size S, Alignment A)
      : PODSingleScalarTypeInfo(T, S, A) {}

    llvm::Type *getScalarType() const {
      assert(isComplete());
      return getDiscriminatorType();
    }

    static Address projectScalar(IRGenFunction &IGF, Address addr) {
      return IGF.Builder.CreateStructGEP(addr, 0, Size(0));
    }

    void emitInjectionFunctionBody(IRGenFunction &IGF,
                                   OneOfElementDecl *elt,
                                   Explosion &params) const {
      IGF.Builder.CreateRet(getDiscriminatorIndex(elt));
    }
  };
}

const TypeInfo *
TypeConverter::convertOneOfType(IRGenModule &IGM, OneOfType *T) {
  OneOfDecl *D = T->getDecl();
  llvm::StructType *convertedStruct = IGM.createNominalType(D);

  unsigned numElements = 0;
  for (auto elt : D->getMembers())
    if (isa<OneOfElementDecl>(elt))
      ++numElements;

  // We don't need a discriminator if this is a singleton ADT.
  if (numElements == 1) {
    SingletonOneofTypeInfo *convertedTInfo =
      new SingletonOneofTypeInfo(convertedStruct, Size(0), Alignment(0), IsPOD);
    assert(!IGM.Types.Converted.count(T));
    IGM.Types.Converted.insert(std::make_pair(T, convertedTInfo));

    Type eltType =
        cast<OneOfElementDecl>(D->getMembers()[0])->getArgumentType();

    llvm::Type *storageType;
    if (eltType.isNull()) {
      storageType = IGM.Int8Ty;
      convertedTInfo->StorageAlignment = Alignment(1);
      convertedTInfo->Singleton = nullptr;
    } else {
      const TypeInfo &eltTInfo = getFragileTypeInfo(IGM, eltType);
      assert(eltTInfo.isComplete());
      storageType = eltTInfo.StorageType;
      convertedTInfo->StorageSize = eltTInfo.StorageSize;
      convertedTInfo->StorageAlignment = eltTInfo.StorageAlignment;
      convertedTInfo->Singleton = &eltTInfo;
      convertedTInfo->setPOD(eltTInfo.isPOD(ResilienceScope::Local));
    }

    llvm::Type *body[] = { storageType };
    convertedStruct->setBody(body);

    return convertedTInfo;
  }

  // Otherwise, we need a discriminator.
  llvm::Type *discriminatorType;
  Size discriminatorSize;
  if (numElements == 2) {
    discriminatorType = IGM.Int1Ty;
    discriminatorSize = Size(1);
  } else if (numElements <= (1 << 8)) {
    discriminatorType = IGM.Int8Ty;
    discriminatorSize = Size(1);
  } else if (numElements <= (1 << 16)) {
    discriminatorType = IGM.Int16Ty;
    discriminatorSize = Size(2);
  } else {
    discriminatorType = IGM.Int32Ty;
    discriminatorSize = Size(4);
  }

  SmallVector<llvm::Type*, 2> body;
  body.push_back(discriminatorType);

  Size payloadSize = Size(0);
  Alignment storageAlignment = Alignment(1);
  IsPOD_t isPOD = IsPOD;

  // Figure out how much storage we need for the union.
  for (Decl *member : D->getMembers()) {
    OneOfElementDecl *elt = dyn_cast<OneOfElementDecl>(member);
    if (!elt)
      continue;

    // Ignore variants that carry no data.
    Type eltType = elt->getArgumentType();
    if (eltType.isNull()) continue;

    // Compute layout for the type, and ignore variants with
    // zero-size data.
    const TypeInfo &eltTInfo = getFragileTypeInfo(IGM, eltType);
    assert(eltTInfo.isComplete());
    if (eltTInfo.isEmpty(ResilienceScope::Local)) continue;

    // The required payload size is the amount of padding needed to
    // get up to the element's alignment, plus the actual size.
    Size eltPayloadSize = eltTInfo.StorageSize;
    if (eltTInfo.StorageAlignment.getValue() > discriminatorSize.getValue())
      eltPayloadSize += Size(eltTInfo.StorageAlignment.getValue()
                               - discriminatorSize.getValue());

    payloadSize = std::max(payloadSize, eltPayloadSize);
    storageAlignment = std::max(storageAlignment, eltTInfo.StorageAlignment);
    isPOD &= eltTInfo.isPOD(ResilienceScope::Local);
  }

  Size storageSize = discriminatorSize + payloadSize;

  OneofTypeInfo *convertedTInfo;

  // If there's no payload at all, use the enum TypeInfo.
  if (!payloadSize) {
    assert(isPOD);
    convertedTInfo = new EnumTypeInfo(convertedStruct, storageSize,
                                      storageAlignment);

  // Otherwise, add the payload array to the storage.
  } else {
    body.push_back(llvm::ArrayType::get(IGM.Int8Ty, payloadSize.getValue()));

    // TODO: don't always use an aggregate representation.
    convertedTInfo = new AggregateOneofTypeInfo(convertedStruct, storageSize,
                                                storageAlignment, isPOD);
  }

  convertedStruct->setBody(body);
  return convertedTInfo;
}
/// Emit a reference to a oneof element decl.
void irgen::emitOneOfElementRef(IRGenFunction &IGF,
                                OneOfElementDecl *elt,
                                Explosion &result) {
  // Get the injection function.
  llvm::Function *injection = IGF.IGM.getAddrOfInjectionFunction(elt);

  // If the element is of function type, just emit this as a function
  // reference.  It will always literally be of function type when
  // written this way.
  if (isa<FunctionType>(elt->getType())) {
    result.addUnmanaged(
               llvm::ConstantExpr::getBitCast(injection, IGF.IGM.Int8PtrTy));
    result.addUnmanaged(IGF.IGM.RefCountedNull);
    return;
  }

  // Otherwise, we need to call the injection function (with no
  // arguments, except maybe a temporary result) and expand the result
  // into the explosion.
  IGF.emitNullaryCall(injection, elt->getType(), result);
}

/// Emit the injection function for the given element.
static void emitInjectionFunction(IRGenModule &IGM,
                                  llvm::Function *fn,
                                  const OneofTypeInfo &oneofTI,
                                  OneOfElementDecl *elt) {
  ExplosionKind explosionKind = ExplosionKind::Minimal;
  IRGenFunction IGF(IGM, Type(), ArrayRef<Pattern*>(), explosionKind,
                    /*uncurry level*/ 0, fn, Prologue::Bare);

  Explosion explosion = IGF.collectParameters();
  oneofTI.emitInjectionFunctionBody(IGF, elt, explosion);
}

/// emitOneOfType - Emit all the declarations associated with this oneof type.
void IRGenModule::emitOneOfType(OneOfType *oneof) {
  // FIXME: This is mostly copy-paste from emitExtension;
  // figure out how to refactor! 
  for (Decl *member : oneof->getDecl()->getMembers()) {
    switch (member->getKind()) {
    case DeclKind::Import:
    case DeclKind::TopLevelCode:
    case DeclKind::Protocol:
    case DeclKind::Extension:
      llvm_unreachable("decl not allowed in struct!");

    // We can't have meaningful initializers for variables; these just show
    // up as part of parsing properties.
    case DeclKind::PatternBinding:
      continue;

    case DeclKind::Subscript:
      // Getter/setter will be handled separately.
      continue;
    case DeclKind::TypeAlias:
      continue;
    case DeclKind::OneOf:
      emitOneOfType(cast<OneOfDecl>(member)->getDeclaredType());
      continue;
    case DeclKind::Struct:
      emitStructType(cast<StructDecl>(member)->getDeclaredType());
      continue;
    case DeclKind::Class:
      emitClassType(cast<ClassDecl>(member)->getDeclaredType());
      continue;
    case DeclKind::Var:
      if (cast<VarDecl>(member)->isProperty())
        // Getter/setter will be handled separately.
        continue;
      // FIXME: Will need an implementation here for resilience
      continue;
    case DeclKind::Func: {
      FuncDecl *func = cast<FuncDecl>(member);
      if (func->isStatic()) {
        // Eventually this won't always be the right thing.
        emitStaticMethod(func);
      } else {
        emitInstanceMethod(func);
      }
      continue;
    }
    case DeclKind::OneOfElement: {
      const OneofTypeInfo &typeInfo =
          getFragileTypeInfo(oneof).as<OneofTypeInfo>();
      OneOfElementDecl *elt = cast<OneOfElementDecl>(member);
      llvm::Function *fn = getAddrOfInjectionFunction(elt);
      emitInjectionFunction(*this, fn, typeInfo, elt);
      continue;
    }
    case DeclKind::Constructor: {
      emitConstructor(cast<ConstructorDecl>(member));
      continue;
    }
    }
    llvm_unreachable("bad extension member kind");
  }
}
