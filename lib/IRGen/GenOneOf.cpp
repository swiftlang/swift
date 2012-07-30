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

#include "FixedTypeInfo.h"
#include "GenProto.h"
#include "GenType.h"
#include "IRGenModule.h"
#include "LValue.h"
#include "ScalarTypeInfo.h"

#include "GenOneOf.h"

using namespace swift;
using namespace irgen;

namespace {
  /// An abstract base class for TypeInfo implementations of oneof types.
  class OneofTypeInfo : public FixedTypeInfo { // FIXME: not always fixed!
  public:
    OneofTypeInfo(llvm::StructType *T, Size S, Alignment A, IsPOD_t isPOD)
      : FixedTypeInfo(T, S, A, isPOD) {}

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

  bool isObviouslyEmptyType(CanType type) {
    if (auto tuple = dyn_cast<TupleType>(type)) {
      for (auto &field : tuple->getFields())
        if (!isObviouslyEmptyType(CanType(field.getType())))
          return false;
      return true;
    }
    // Add more cases here?  Meh.
    return false;
  }

  /// An implementation strategy for a oneof.
  class OneofImplStrategy {
  public:
    enum Kind {
      Singleton,
      Enum,
      Aggregate
    };

  private:
    unsigned NumElements;
    Kind TheKind;

  public:
    explicit OneofImplStrategy(OneOfDecl *oneof) {
      NumElements = 0;

      bool hasApparentPayload = false;
      for (auto member : oneof->getMembers()) {
        auto elt = dyn_cast<OneOfElementDecl>(member);
        if (!elt) continue;

        NumElements++;

        // Compute whether this gives us an apparent payload.
        if (hasApparentPayload) continue;

        Type argType = elt->getArgumentType();
        if (!argType.isNull() &&
            !isObviouslyEmptyType(argType->getCanonicalType()))
          hasApparentPayload = true;
      }

      assert(NumElements != 0);
      if (NumElements == 1) {
        TheKind = Singleton;
      } else if (hasApparentPayload) {
        TheKind = Aggregate;
      } else {
        TheKind = Enum;
      }
    }

    Kind getKind() const { return TheKind; }
    unsigned getNumElements() const { return NumElements; }

    /// Create a forward declaration for the oneof.
    OneofTypeInfo *create(llvm::StructType *convertedStruct) const {
      switch (getKind()) {
      case Singleton:
        return new SingletonOneofTypeInfo(convertedStruct,
                                          Size(0), Alignment(0), IsPOD);
      case Enum:
        return new EnumTypeInfo(convertedStruct,
                                Size(0), Alignment(0));
      case Aggregate:
        return new AggregateOneofTypeInfo(convertedStruct,
                                          Size(0), Alignment(0), IsPOD);
      }
      llvm_unreachable("bad strategy kind");
    }
  };
}

const TypeInfo *TypeConverter::convertOneOfType(OneOfDecl *oneof) {
  llvm::StructType *convertedStruct = IGM.createNominalType(oneof);

  // Compute the implementation strategy.
  OneofImplStrategy strategy(oneof);

  // Create the TI as a forward declaration and map it in the table.
  OneofTypeInfo *convertedTI = strategy.create(convertedStruct);
  auto typesMapKey = oneof->getDeclaredType().getPointer();
  assert(!Types.count(typesMapKey));
  Types.insert(std::make_pair(typesMapKey, convertedTI));

  // We don't need a discriminator if this is a singleton ADT.
  if (strategy.getKind() == OneofImplStrategy::Singleton) {
    auto oneofTI = static_cast<SingletonOneofTypeInfo*>(convertedTI);

    Type eltType =
      cast<OneOfElementDecl>(oneof->getMembers()[0])->getArgumentType();

    llvm::Type *storageType;
    if (eltType.isNull()) {
      storageType = IGM.Int8Ty;
      oneofTI->StorageAlignment = Alignment(1);
      oneofTI->Singleton = nullptr;
    } else {
      const TypeInfo &eltTI = getFragileTypeInfo(eltType);
      assert(eltTI.isComplete());
      storageType = eltTI.StorageType;
      oneofTI->StorageSize = eltTI.StorageSize;
      oneofTI->StorageAlignment = eltTI.StorageAlignment;
      oneofTI->Singleton = &eltTI;
      oneofTI->setPOD(eltTI.isPOD(ResilienceScope::Local));
    }

    llvm::Type *body[] = { storageType };
    convertedStruct->setBody(body);

    return oneofTI;
  }

  // Otherwise, we need a discriminator.

  // Compute the discriminator type.
  llvm::Type *discriminatorType;
  Size discriminatorSize;
  if (strategy.getNumElements() == 2) {
    discriminatorType = IGM.Int1Ty;
    discriminatorSize = Size(1);
  } else if (strategy.getNumElements() <= (1 << 8)) {
    discriminatorType = IGM.Int8Ty;
    discriminatorSize = Size(1);
  } else if (strategy.getNumElements() <= (1 << 16)) {
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
  for (Decl *member : oneof->getMembers()) {
    OneOfElementDecl *elt = dyn_cast<OneOfElementDecl>(member);
    if (!elt)
      continue;

    // Ignore variants that carry no data.
    Type eltType = elt->getArgumentType();
    if (eltType.isNull()) continue;

    // Compute layout for the type, and ignore variants with
    // zero-size data.
    const TypeInfo &eltTInfo = getFragileTypeInfo(eltType);
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

  convertedTI->StorageSize = discriminatorSize + payloadSize;
  convertedTI->StorageAlignment = storageAlignment;
  convertedTI->setPOD(isPOD);

  // Add the payload to the body if necessary.
  if (payloadSize) {
    body.push_back(llvm::ArrayType::get(IGM.Int8Ty, payloadSize.getValue()));
  }

  // TODO: remember element layout information above and stash it in
  // convertedTI.

  convertedStruct->setBody(body);
  return convertedTI;
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
                                  OneOfElementDecl *elt) {
  ExplosionKind explosionKind = ExplosionKind::Minimal;
  IRGenFunction IGF(IGM, Type(), ArrayRef<Pattern*>(), explosionKind,
                    /*uncurry level*/ 0, fn, Prologue::Bare);
  if (elt->hasArgumentType()) {
    // FIXME: Implement!
    IGF.Builder.CreateUnreachable();
    return;
  }

  Explosion explosion = IGF.collectParameters();
  OneOfDecl *ood = cast<OneOfDecl>(elt->getDeclContext());
  if (ood->getGenericParams())
    emitPolymorphicParameters(IGF, *ood->getGenericParams(), explosion);
  const OneofTypeInfo &oneofTI =
      IGM.getFragileTypeInfo(ood->getDeclaredTypeInContext()).as<OneofTypeInfo>();
  oneofTI.emitInjectionFunctionBody(IGF, elt, explosion);
}

/// emitOneOfType - Emit all the declarations associated with this oneof type.
void IRGenModule::emitOneOfDecl(OneOfDecl *oneof) {
  // FIXME: This is mostly copy-paste from emitExtension;
  // figure out how to refactor! 
  for (Decl *member : oneof->getMembers()) {
    switch (member->getKind()) {
    case DeclKind::Import:
    case DeclKind::TopLevelCode:
    case DeclKind::Protocol:
    case DeclKind::Extension:
    case DeclKind::Destructor:
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
      emitOneOfDecl(cast<OneOfDecl>(member));
      continue;
    case DeclKind::Struct:
      emitStructDecl(cast<StructDecl>(member));
      continue;
    case DeclKind::Class:
      emitClassDecl(cast<ClassDecl>(member));
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
      OneOfElementDecl *elt = cast<OneOfElementDecl>(member);
      llvm::Function *fn = getAddrOfInjectionFunction(elt);
      emitInjectionFunction(*this, fn, elt);
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
