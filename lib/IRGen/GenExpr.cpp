//===--- GenExpr.cpp - Miscellaneous IR Generation for Expressions --------===//
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
//  This file implements general IR generation for Swift expressions.
//  Expressions which naturally belong to a specific type kind, such
//  as TupleExpr, are generally implemented in the type-specific file.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/ExprHandle.h"
#include "swift/AST/Pattern.h"
#include "swift/AST/Types.h"
#include "swift/Basic/Optional.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"

#include "ASTVisitor.h"
#include "GenArray.h"
#include "GenClass.h"
#include "GenClosure.h"
#include "GenFunc.h"
#include "GenInit.h"
#include "GenMeta.h"
#include "GenProto.h"
#include "GenTuple.h"
#include "IRGenFunction.h"
#include "IRGenModule.h"
#include "LValue.h"
#include "Explosion.h"
#include "TypeInfo.h"

using namespace swift;
using namespace irgen;

/// Is the given l-value type heap or non-heap?
static OnHeap_t isOnHeap(Type type) {
  return (type->castTo<LValueType>()->isHeap() ? OnHeap : NotOnHeap);
}

/// Emit an integer literal expression.
static llvm::Value *emitIntegerLiteralExpr(IRGenFunction &IGF,
                                           IntegerLiteralExpr *E) {
  assert(E->getType()->is<BuiltinIntegerType>());
  return llvm::ConstantInt::get(IGF.IGM.LLVMContext, E->getValue());
}

/// Emit an float literal expression.
static llvm::Value *emitFloatLiteralExpr(IRGenFunction &IGF,
                                         FloatLiteralExpr *E) {
  assert(E->getType()->is<BuiltinFloatType>());
  return llvm::ConstantFP::get(IGF.IGM.LLVMContext, E->getValue());
}

/// Emit an string literal expression.
static llvm::Value *emitCharacterLiteralExpr(IRGenFunction &IGF,
                                             CharacterLiteralExpr *E) {
  assert(E->getType()->is<BuiltinIntegerType>());
  return llvm::ConstantInt::get(IGF.IGM.Int32Ty, E->getValue());
}

static LValue emitDeclRefLValue(IRGenFunction &IGF, ValueDecl *D, Type Ty) {
  switch (D->getKind()) {
#define VALUE_DECL(id, parent)
#define DECL(id, parent) case DeclKind::id:
#include "swift/AST/DeclNodes.def"
    llvm_unreachable("decl is not a value decl");

  case DeclKind::TypeAlias:
  case DeclKind::OneOf:
  case DeclKind::Struct:
  case DeclKind::Class:
  case DeclKind::Protocol:
  case DeclKind::Func:
  case DeclKind::OneOfElement:
    llvm_unreachable("decl cannot be emitted as an l-value");

  case DeclKind::Var:

  case DeclKind::Subscript:
    llvm_unreachable("subscript decl cannot be referenced");

  case DeclKind::Constructor:
    llvm_unreachable("constructor decl cannot be referenced");

  case DeclKind::Destructor:
    llvm_unreachable("destructor decl cannot be referenced");
  }
  llvm_unreachable("bad decl kind");
}

/// Emit a declaration reference as an exploded r-value.
static void emitDeclRef(IRGenFunction &IGF, ValueDecl *D, Type Ty, SourceLoc Loc,
                        Explosion &explosion) {
  switch (D->getKind()) {
#define VALUE_DECL(id, parent)
#define DECL(id, parent) case DeclKind::id:
#include "swift/AST/DeclNodes.def"
    llvm_unreachable("decl is not a value decl");

  case DeclKind::TypeAlias:
  case DeclKind::OneOf:
  case DeclKind::Struct:
  case DeclKind::Class:
  case DeclKind::Protocol: {
    CanType instanceTy = Ty->castTo<MetaTypeType>()
      ->getInstanceType()->getCanonicalType();
    emitMetaTypeRef(IGF, instanceTy, explosion);
    return;
  }

  case DeclKind::Var:
 
  case DeclKind::Func:
    return emitRValueForFunction(IGF, cast<FuncDecl>(D), explosion);

  case DeclKind::OneOfElement:
  case DeclKind::Subscript:
    llvm_unreachable("subscript decl cannot be referenced");

  case DeclKind::Constructor:
    case DeclKind::Destructor:
    llvm_unreachable("destructor decl cannot be referenced");
  }
  llvm_unreachable("bad decl kind");
}

/// Emit a rvalue-to-lvalue conversion.
static OwnedAddress emitMaterializeExpr(IRGenFunction &IGF,
                                        MaterializeExpr *E) {
  // Do we need a heap object?
  OnHeap_t onHeap = isOnHeap(E->getType());

  // Compute the object type.
  Expr *subExpr = E->getSubExpr();
  const TypeInfo &objectTI = IGF.getFragileTypeInfo(subExpr->getType());

  // Begin the initialization.
  Initialization I;
  InitializedObject object = I.getObjectForTemporary();
  I.registerObject(IGF, object, onHeap, objectTI);

  // Allocate.
  OwnedAddress addr =
    I.emitLocalAllocation(IGF, object, onHeap, objectTI,
                          "materialized-temporary");

  // Emit the initializer.
  I.emitInit(IGF, object, addr, subExpr, objectTI);

  // We're done.
  return addr;
}

/// Emit a get-metatype operation for the given base expression.
static void emitGetMetatype(IRGenFunction &IGF, Expr *base, Explosion &out) {
  auto type = base->getType()->getCanonicalType();

  // If the expression has class type, evaluate to an object and
  // pull the metatype out of that.
  if (type->getClassOrBoundGenericClass()) {
    Explosion temp(ExplosionKind::Maximal);
    IGF.emitRValue(base, temp);
    auto value = temp.claimNext().getValue(); // let the cleanup happen
    auto baseType = base->getType()->getCanonicalType();
    out.addUnmanaged(emitTypeMetadataRefForHeapObject(IGF, value, baseType));
    return;
  }

  // Otherwise, ignore and use the static type.
  emitMetaTypeRef(IGF, type, out);
}

/// Emit a checked unconditional downcast.
llvm::Value *IRGenFunction::emitUnconditionalDowncast(llvm::Value *from,
                                                      CanType toType) {
  // Emit the value we're casting from.
  if (from->getType() != IGM.Int8PtrTy)
    from = Builder.CreateBitCast(from, IGM.Int8PtrTy);
  
  // Emit a reference to the metadata.
  llvm::Value *metadataRef
    = IGM.getAddrOfTypeMetadata(toType, false, false);
  if (metadataRef->getType() != IGM.Int8PtrTy)
    metadataRef = Builder.CreateBitCast(metadataRef, IGM.Int8PtrTy);
  
  // Call the (unconditional) dynamic cast.
  auto call
    = Builder.CreateCall2(IGM.getDynamicCastClassUnconditionalFn(),
                              from, metadataRef);
  // FIXME: Eventually, we may want to throw.
  call->setDoesNotThrow();
  
  llvm::Type *subTy = getFragileTypeInfo(toType).StorageType;
  return Builder.CreateBitCast(call, subTy);
  
}

/// Emit an is-subtype check.
llvm::Value *IRGenFunction::emitIsSubtype(llvm::Value *from,
                                          CanType toType) {
  // Emit the value we're casting from.
  if (from->getType() != IGM.Int8PtrTy)
    from = Builder.CreateBitCast(from, IGM.Int8PtrTy);
  
  // Emit a reference to the metadata.
  llvm::Value *metadataRef
  = IGM.getAddrOfTypeMetadata(toType, false, false);
  if (metadataRef->getType() != IGM.Int8PtrTy)
    metadataRef = Builder.CreateBitCast(metadataRef, IGM.Int8PtrTy);

  // Call the checked dynamic cast.
  auto call
    = Builder.CreateCall2(IGM.getDynamicCastClassFn(),
                          from, metadataRef);
  call->setDoesNotThrow();
  
  // Compare the result to null.
  return Builder.CreateICmp(llvm::CmpInst::Predicate::ICMP_NE,
                            call,
                            llvm::ConstantPointerNull::get(IGM.Int8PtrTy));
}

namespace {
  /// A visitor for emitting a value into an explosion.  We call this
  /// r-value emission, but do note that it's valid to emit an
  /// expression of l-value type in this way; the effect is that of
  /// emitLValueAsScalar.
  class RValueEmitter : public irgen::ExprVisitor<RValueEmitter> {
    IRGenFunction &IGF;
    Explosion &Out;

  public:
    RValueEmitter(IRGenFunction &IGF, Explosion &out) : IGF(IGF), Out(out) {}

    void visitExpr(Expr *) {}
    
    void visitLoadExpr(LoadExpr *E) {
      abort();
    }

    void visitMaterializeExpr(MaterializeExpr *E) {
      OwnedAddress addr = emitMaterializeExpr(IGF, E);
      Out.addUnmanaged(addr.getAddressPointer());
    }

    void visitRequalifyExpr(RequalifyExpr *E) {
    }

    void visitTupleExpr(TupleExpr *E) {
     }

    void visitSubscriptExpr(SubscriptExpr *E) {
     }
    
    void visitTupleShuffleExpr(TupleShuffleExpr *E) {
     }

    /// Metatypes are always compatible up the inheritance hierarchy.
    void visitMetatypeConversionExpr(MetatypeConversionExpr *E) {
      IGF.emitRValue(E->getSubExpr(), Out);
    }
    void visitFunctionConversionExpr(FunctionConversionExpr *E) {
      IGF.emitRValue(E->getSubExpr(), Out);
    }
    void visitErasureExpr(ErasureExpr *E) {
     }
    void visitSpecializeExpr(SpecializeExpr *E) {
    }
    void visitDerivedToBaseExpr(DerivedToBaseExpr *E) {
      Explosion subResult(ExplosionKind::Maximal);
      IGF.emitRValue(E->getSubExpr(), subResult);
      ManagedValue val = subResult.claimNext();
      llvm::Type *baseTy = IGF.getFragileTypeInfo(E->getType()).StorageType;
      llvm::Value *castVal = IGF.Builder.CreateBitCast(val.getValue(), baseTy);
      Out.add({castVal, val.getCleanup()});
    }
    void visitArchetypeToSuperExpr(ArchetypeToSuperExpr *E) {
      // Emit the expression with archetype type as an rvalue.
      Explosion subResult(ExplosionKind::Maximal);
      IGF.emitRValue(E->getSubExpr(), subResult);

      // The data associated with the archetype is simply a pointer; grab it
      // and cast it to the superclass type.
      ManagedValue val = subResult.claimNext();
      const TypeInfo &baseTypeInfo = IGF.getFragileTypeInfo(E->getType());
      llvm::Type *baseTy = baseTypeInfo.StorageType;
      llvm::Type *basePtrTy = baseTy->getPointerTo();
      llvm::Value *castPtrVal = IGF.Builder.CreateBitCast(val.getValue(),
                                                          basePtrTy);
      llvm::Value *castVal
        = IGF.Builder.CreateLoad(castPtrVal, IGF.IGM.getPointerAlignment());
      Out.add({castVal, val.getCleanup()});
    }
    
    void visitIsSubtypeExpr(IsSubtypeExpr *E) {
      /// Emit the value we're testing.
      Explosion subResult(ExplosionKind::Maximal);
      IGF.emitRValue(E->getSubExpr(), subResult);
      ManagedValue val = subResult.claimNext();
      llvm::Value *object = val.getValue();
      Out.addUnmanaged(IGF.emitIsSubtype(object,
                               E->getTypeLoc().getType()->getCanonicalType()));
    }
    
    void visitSuperIsArchetypeExpr(SuperIsArchetypeExpr *E) {
      IGF.emitSuperIsArchetype(E->getSubExpr(),
                               E->getTypeLoc().getType()->getCanonicalType(),
                               Out);
    }

    void visitScalarToTupleExpr(ScalarToTupleExpr *E) {
    }
    void visitTupleElementExpr(TupleElementExpr *E) {
     }

    void visitDotSyntaxBaseIgnoredExpr(DotSyntaxBaseIgnoredExpr *E) {
      IGF.emitRValue(E->getRHS(), Out);
    }
    
    void visitCoerceExpr(CoerceExpr *E) {
      IGF.emitRValue(E->getSubExpr(), Out);
    }
    
    void visitUncheckedDowncastExpr(UncheckedDowncastExpr *E) {
      // Emit the value we're casting from.
      Explosion subResult(ExplosionKind::Maximal);
      IGF.emitRValue(E->getSubExpr(), subResult);
      ManagedValue val = subResult.claimNext();
      llvm::Value *object = val.getValue();
      llvm::Value *castVal = IGF.emitUnconditionalDowncast(
                                             object,
                                             E->getType()->getCanonicalType());
      Out.add({castVal, val.getCleanup()});
    }

    void visitUncheckedSuperToArchetypeExpr(UncheckedSuperToArchetypeExpr *E) {
      IGF.emitSupertoArchetypeConversion(E->getSubExpr(),
                                         E->getType()->getCanonicalType(),
                                         Out);
    }

    void visitNewArrayExpr(NewArrayExpr *E) {
      emitNewArrayExpr(IGF, E, Out);
    }

    void visitMetatypeExpr(MetatypeExpr *E) {
      // If we have a base, we have to evaluate it.
      if (auto base = E->getBase())
        return emitGetMetatype(IGF, base, Out);

      // Otherwise, just use the static type of the expression.
      auto type = E->getType()->getCanonicalType();
      type = CanType(cast<MetaTypeType>(type)->getInstanceType());
      emitMetaTypeRef(IGF, type, Out);
    }

    void visitApplyExpr(ApplyExpr *E) {
      emitApplyExpr(IGF, E, Out);
    }

    void visitIntegerLiteralExpr(IntegerLiteralExpr *E) {
      Out.addUnmanaged(emitIntegerLiteralExpr(IGF, E));
    }

    void visitFloatLiteralExpr(FloatLiteralExpr *E) {
      Out.addUnmanaged(emitFloatLiteralExpr(IGF, E));
    }

    void visitCharacterLiteralExpr(CharacterLiteralExpr *E) {
      Out.addUnmanaged(emitCharacterLiteralExpr(IGF, E));
    }
    void visitStringLiteralExpr(StringLiteralExpr *E) {
     }
    void visitInterpolatedStringLiteralExpr(InterpolatedStringLiteralExpr *E) {
      visit(E->getSemanticExpr());
    }
    void visitCollectionExpr(CollectionExpr *E) {
      visit(E->getSemanticExpr());
    }

    void visitDeclRefExpr(DeclRefExpr *E) {
      emitDeclRef(IGF, E->getDecl(), E->getType(), E->getLoc(), Out);
    }
    void visitSuperRefExpr(SuperRefExpr *E) {
      emitDeclRef(IGF, E->getThis(), E->getType(), E->getLoc(), Out);
    }
    void visitOtherConstructorDeclRefExpr(OtherConstructorDeclRefExpr *E) {
      return;
    }
    
    void visitRebindThisInConstructorExpr(RebindThisInConstructorExpr *E) {
      // FIXME: For delegating value constructors, we want to emitRValueAsInit
      // FIXME: We should conditionalize the 'this' rebinding below on the
      // ObjC-ness of the called constructor. Swift constructors should never
      // rebind 'this'.
      
      Explosion sub(IGF.CurExplosionLevel);
      IGF.emitRValue(E->getSubExpr(), sub);
      
      // Coerce the result to 'this' type.
      CanType thisTy = E->getThis()->getType()->getCanonicalType();
      llvm::Value *newThis = sub.forwardNext(IGF);
      if (!E->getSubExpr()->getType()->isEqual(E->getThis()->getType()))
        newThis = IGF.emitUnconditionalDowncast(newThis, thisTy);

      // Reassign 'this' with the result.
      LValue thisLV = emitDeclRefLValue(IGF, E->getThis(),
                                        E->getThis()->getTypeOfReference());
      OwnedAddress addr = IGF.emitAddressForPhysicalLValue(thisLV);
      
      Explosion newThisE(IGF.CurExplosionLevel);
      newThisE.addUnmanaged(newThis);
      
      IGF.getFragileTypeInfo(thisTy).assign(IGF, newThisE, addr);
    }

    void visitMemberRefExpr(MemberRefExpr *E) {
    }
    
    bool isLValueMember(ValueDecl *D) {
      return isa<VarDecl>(D) || isa<SubscriptDecl>(D);
    }

    bool isTypeMember(ValueDecl *D) {
      return isa<TypeDecl>(D);
    }

#define FOR_MEMBER_KIND(KIND)                                              \
    void visit##KIND##MemberRefExpr(KIND##MemberRefExpr *E) {              \
    }                                                                      \
    void visit##KIND##SubscriptExpr(KIND##SubscriptExpr *E) {              \
     }

    FOR_MEMBER_KIND(Existential)
    FOR_MEMBER_KIND(Archetype)
#undef FOR_MEMBER_KIND

    void visitCapturingExpr(CapturingExpr *E) {
      emitClosure(IGF, E, Out);
    }

    void visitModuleExpr(ModuleExpr *E) {
      // Nothing to do: modules have no runtime representation.
    }
    
    void visitBridgeToBlockExpr(BridgeToBlockExpr *E) {
      Explosion closure(ExplosionKind::Minimal);
      IGF.emitRValue(E->getSubExpr(), closure);
      emitBridgeToBlock(IGF, E->getType()->getCanonicalType(), closure, Out);
    }
    
    void visitIfExpr(IfExpr *E) {
      abort();
    }
  };
}

void IRGenFunction::emitRValue(Expr *E, Explosion &explosion) {
  assert(0);
  RValueEmitter(*this, explosion).visit(E);
}



void IRGenFunction::emitFakeExplosion(const TypeInfo &type, Explosion &explosion) {
  ExplosionSchema schema(explosion.getKind());
  type.getSchema(schema);
  for (auto &element : schema) {
    llvm::Type *elementType;
    if (element.isAggregate()) {
      elementType = element.getAggregateType()->getPointerTo();
    } else {
      elementType = element.getScalarType();
    }
    
    explosion.addUnmanaged(llvm::UndefValue::get(elementType));
  }
}
