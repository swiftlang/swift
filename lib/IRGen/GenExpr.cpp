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
#include "Condition.h"
#include "GenArray.h"
#include "GenClass.h"
#include "GenClosure.h"
#include "GenFunc.h"
#include "GenInit.h"
#include "GenLValue.h"
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

/// Emit an string literal expression.
static void emitStringLiteralExpr(IRGenFunction &IGF,
                                  StringLiteralExpr *E, 
                                  Explosion &out) {
  // Type of the expression should be either a lone RawPointer or a
  // (RawPointer, Int64) tuple.
  bool includeSize = !E->getType()->is<BuiltinRawPointerType>();
  if (includeSize)
    assert(E->getType()->is<TupleType>());

  emitStringLiteral(IGF, E->getValue(), includeSize, out);
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
    if (D->getDeclContext()->isLocalContext())
      return IGF.emitAddressLValue(IGF.getLocalVar(cast<VarDecl>(D)));
    return IGF.getGlobal(cast<VarDecl>(D));
      
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
    return IGF.emitLValueAsScalar(emitDeclRefLValue(IGF, D, Ty),
                                  OnHeap, explosion);

  case DeclKind::Func:
    return emitRValueForFunction(IGF, cast<FuncDecl>(D), explosion);

  case DeclKind::OneOfElement: {
    IGF.unimplemented(Loc, "uncurried reference to oneof");
    IGF.emitFakeExplosion(IGF.getFragileTypeInfo(Ty), explosion);
    return;
  }

  case DeclKind::Subscript:
    llvm_unreachable("subscript decl cannot be referenced");

  case DeclKind::Constructor: {
    IGF.unimplemented(Loc, "uncurried reference to constructor");
    IGF.emitFakeExplosion(IGF.getFragileTypeInfo(Ty), explosion);
    return;
  }
  case DeclKind::Destructor:
    llvm_unreachable("destructor decl cannot be referenced");
  }
  llvm_unreachable("bad decl kind");
}

/// Emit the given expression, which must have primitive scalar type,
/// as that primitive scalar value.  This is just a convenience method
/// for not needing to construct and destroy an Explosion.
llvm::Value *IRGenFunction::emitAsPrimitiveScalar(Expr *E) {
  Explosion explosion(ExplosionKind::Minimal);
  emitRValue(E, explosion);

  llvm::Value *result = explosion.claimUnmanagedNext();
  assert(explosion.empty());
  return result;
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
  IGF.emitIgnored(base);
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

    void visitLoadExpr(LoadExpr *E) {
      const TypeInfo &type = IGF.getFragileTypeInfo(E->getType());
      return IGF.emitLoad(IGF.emitLValue(E->getSubExpr()),
                          type, Out);
    }

    void visitMaterializeExpr(MaterializeExpr *E) {
      OwnedAddress addr = emitMaterializeExpr(IGF, E);
      Out.addUnmanaged(addr.getAddressPointer());
    }

    void visitRequalifyExpr(RequalifyExpr *E) {
      emitRequalify(IGF, E, Out);
    }

    void visitTupleExpr(TupleExpr *E) {
      emitTupleLiteral(IGF, E, Out);
    }

    void visitSubscriptExpr(SubscriptExpr *E) {
      IGF.emitLValueAsScalar(emitSubscriptLValue(IGF, E),
                             isOnHeap(E->getType()), Out);
    }
    
    void visitTupleShuffleExpr(TupleShuffleExpr *E) {
      emitTupleShuffle(IGF, E, Out);
    }

    /// Metatypes are always compatible up the inheritance hierarchy.
    void visitMetatypeConversionExpr(MetatypeConversionExpr *E) {
      IGF.emitRValue(E->getSubExpr(), Out);
    }
    void visitFunctionConversionExpr(FunctionConversionExpr *E) {
      IGF.emitRValue(E->getSubExpr(), Out);
    }
    void visitErasureExpr(ErasureExpr *E) {
      emitErasure(IGF, E, Out);
    }
    void visitSpecializeExpr(SpecializeExpr *E) {
      IGF.unimplemented(E->getLoc(), "specialize expressions");
      IGF.emitFakeExplosion(IGF.getFragileTypeInfo(E->getType()), Out);
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
      llvm_unreachable("not implemented");      
    }

    void visitScalarToTupleExpr(ScalarToTupleExpr *E) {
      emitScalarToTuple(IGF, E, Out);
    }
    void visitTupleElementExpr(TupleElementExpr *E) {
      emitTupleElement(IGF, E, Out);
    }

    void visitDotSyntaxBaseIgnoredExpr(DotSyntaxBaseIgnoredExpr *E) {
      IGF.emitIgnored(E->getLHS());
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
      // FIXME: We should check whether the dynamic type of the RHS is
      // actually the same as the archetype. Right now, this is an
      // unchecked cast.
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
      emitStringLiteralExpr(IGF, E, Out);
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
      IGF.unimplemented(E->getLoc(), "uncurried reference to constructor");
      IGF.emitFakeExplosion(IGF.getFragileTypeInfo(E->getType()), Out);
      return;
    }
    
    void visitRebindThisInConstructorExpr(RebindThisInConstructorExpr *E) {
      // FIXME: For delegating value constructors, we want to emitRValueAsInit
      // 'this' in place with the result of the subexpression.
      if (!E->getThis()->getType()->hasReferenceSemantics()) {
        IGF.unimplemented(E->getLoc(), "delegating value constructor");
        IGF.emitFakeExplosion(IGF.getFragileTypeInfo(E->getType()), Out);
        return;
      }
      
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
      IGF.emitLValueAsScalar(emitMemberRefLValue(IGF, E),
                             isOnHeap(E->getType()), Out);
    }
    
    bool isLValueMember(ValueDecl *D) {
      return isa<VarDecl>(D) || isa<SubscriptDecl>(D);
    }

    bool isTypeMember(ValueDecl *D) {
      return isa<TypeDecl>(D);
    }

#define FOR_MEMBER_KIND(KIND)                                              \
    void visit##KIND##MemberRefExpr(KIND##MemberRefExpr *E) {              \
      if (isLValueMember(E->getDecl())) {                                  \
        assert(E->getType()->is<LValueType>());                            \
        return IGF.emitLValueAsScalar(emit##KIND##MemberRefLValue(IGF, E), \
                                      isOnHeap(E->getType()), Out);        \
      }                                                                    \
      if (isTypeMember(E->getDecl())) {                                    \
        IGF.emitIgnored(E->getBase());                                     \
        auto type = cast<TypeDecl>(E->getDecl())->getDeclaredType()        \
          ->getCanonicalType();                                            \
        return emitMetaTypeRef(IGF, type, Out);                            \
      }                                                                    \
                                                                           \
      assert(!E->getType()->is<LValueType>());                             \
      emit##KIND##MemberRef(IGF, E, Out);                                  \
    }                                                                      \
    void visit##KIND##SubscriptExpr(KIND##SubscriptExpr *E) {              \
      assert(E->getType()->is<LValueType>());                              \
      return IGF.emitLValueAsScalar(emit##KIND##SubscriptLValue(IGF, E),   \
                                    isOnHeap(E->getType()), Out);          \
    }

    FOR_MEMBER_KIND(Existential)
    FOR_MEMBER_KIND(Archetype)
    FOR_MEMBER_KIND(Generic)
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
      // Emit the condition.
      Condition cond = IGF.emitCondition(E->getCondExpr(),
                                         /*hasFalse=*/ true);
      
      // Emit the branches.
      Explosion thenResult(Out.getKind()),
                elseResult(Out.getKind());
      const TypeInfo &ti = IGF.getFragileTypeInfo(E->getType());
      
      llvm::BasicBlock *thenPred, *elsePred;
      
      if (cond.hasTrue()) {
        cond.enterTrue(IGF);
        IGF.emitRValue(E->getThenExpr(), thenResult);
        thenPred = IGF.Builder.GetInsertBlock();
        cond.exitTrue(IGF);
      }
      
      if (cond.hasFalse()) {
        cond.enterFalse(IGF);
        IGF.emitRValue(E->getElseExpr(), elseResult);
        elsePred = IGF.Builder.GetInsertBlock();
        cond.exitFalse(IGF);
      }
      
      cond.complete(IGF);
      
      // Join the results of the branches.
      
      // If only one branch was valid, use the result from that branch.
      if (cond.hasTrue() && !cond.hasFalse()) {
        ti.reexplode(IGF, thenResult, Out);
        return;
      }
      
      if (cond.hasFalse() && !cond.hasTrue()) {
        ti.reexplode(IGF, elseResult, Out);
        return;
      }
      
      // Otherwise, phi the results from both branches.
      assert(thenResult.size() == elseResult.size()
             && "mismatched explosion sizes in branches of if expr");
      
      Explosion phiResult(Out.getKind());
      while (!thenResult.empty()) {
        // Strip cleanups from the incoming values. We'll introduce new cleanups
        // on the phi-ed values by TypeInfo::manage-ing them.
        llvm::Value *thenVal = thenResult.forwardNext(IGF);
        llvm::Value *elseVal = elseResult.forwardNext(IGF);
        assert(thenVal->getType() == elseVal->getType()
               && "mismatched explosion types in branches of if expr");
        llvm::PHINode *phi = IGF.Builder.CreatePHI(thenVal->getType(), 2);
        phi->addIncoming(thenVal, thenPred);
        phi->addIncoming(elseVal, elsePred);
        phiResult.addUnmanaged(phi);
      }
      ti.manage(IGF, phiResult, Out);
    }
  };
}

void IRGenFunction::emitRValue(Expr *E, Explosion &explosion) {
  RValueEmitter(*this, explosion).visit(E);
}

namespace {
  /// A visitor for emitting a value into memory.  Like r-value
  /// emission, this can actually emit an l-value, with the result
  /// that the address (and possibly the owner) of the l-value are
  /// stored.
  class RValueInitEmitter : public irgen::ExprVisitor<RValueInitEmitter> {
    IRGenFunction &IGF;
    const TypeInfo &AddrTI;
    Address Addr;

  public:
    RValueInitEmitter(IRGenFunction &IGF, const TypeInfo &addrTI, Address addr)
      : IGF(IGF), AddrTI(addrTI), Addr(addr) {}

    void visitExpr(Expr *E) {
      // The default behavior is to emit as an explosion and then
      // initialize from that.
      Explosion explosion(ExplosionKind::Maximal);
      IGF.emitRValue(E, explosion);
      AddrTI.initialize(IGF, explosion, Addr);
    }

    void visitApplyExpr(ApplyExpr *E) {
      emitApplyExprToMemory(IGF, E, Addr, AddrTI);
    }

    void visitLoadExpr(LoadExpr *E) {
      return emitLoadAsInit(IGF, IGF.emitLValue(E->getSubExpr()),
                            Addr, AddrTI);
    }

    void visitErasureExpr(ErasureExpr *E) {
      emitErasureAsInit(IGF, E, Addr, AddrTI);
    }

    // TODO: Implement some other interesting cases that could
    // benefit from this:
    //   TupleExpr
    //   TupleShuffleExpr
  };
}

/// Emit the given expression as the initializer for an object at the
/// given address.  A FullExpr has already been pushed, and a cleanup
/// for the address will be activated immediately after completion.
void IRGenFunction::emitRValueAsInit(Expr *E, Address addr,
                                     const TypeInfo &addrTI) {
  RValueInitEmitter(*this, addrTI, addr).visit(E);
}

namespace {
  class LValueEmitter : public irgen::ExprVisitor<LValueEmitter, LValue> {
    IRGenFunction &IGF;

  public:
    LValueEmitter(IRGenFunction &IGF) : IGF(IGF) {}

#define NOT_LVALUE_EXPR(Id) \
    LValue visit##Id##Expr(Id##Expr *E) { \
      llvm_unreachable("these expression kinds should never be l-values"); \
    }
    NOT_LVALUE_EXPR(Apply)
    NOT_LVALUE_EXPR(IntegerLiteral)
    NOT_LVALUE_EXPR(FloatLiteral)
    NOT_LVALUE_EXPR(CharacterLiteral)
    NOT_LVALUE_EXPR(StringLiteral)
    NOT_LVALUE_EXPR(InterpolatedStringLiteral)
    NOT_LVALUE_EXPR(TupleShuffle)
    NOT_LVALUE_EXPR(FunctionConversion)
    NOT_LVALUE_EXPR(MetatypeConversion)
    NOT_LVALUE_EXPR(Erasure)
    NOT_LVALUE_EXPR(Specialize) // FIXME: Generic subscripts?
    NOT_LVALUE_EXPR(DerivedToBase)
    NOT_LVALUE_EXPR(ArchetypeToSuper)
    NOT_LVALUE_EXPR(ScalarToTuple)
    NOT_LVALUE_EXPR(Func)
    NOT_LVALUE_EXPR(Closure)
    NOT_LVALUE_EXPR(Load)
    NOT_LVALUE_EXPR(Tuple)
    NOT_LVALUE_EXPR(Array)
    NOT_LVALUE_EXPR(Collection)
    NOT_LVALUE_EXPR(NewArray)
    NOT_LVALUE_EXPR(NewReference)
    NOT_LVALUE_EXPR(Metatype)
    NOT_LVALUE_EXPR(DotSyntaxBaseIgnored)
    NOT_LVALUE_EXPR(Coerce)
    NOT_LVALUE_EXPR(UncheckedDowncast)
    NOT_LVALUE_EXPR(UncheckedSuperToArchetype)
    NOT_LVALUE_EXPR(IsSubtype)
    NOT_LVALUE_EXPR(SuperIsArchetype)
    NOT_LVALUE_EXPR(Module)
    NOT_LVALUE_EXPR(BridgeToBlock)
    NOT_LVALUE_EXPR(OtherConstructorDeclRef)
    NOT_LVALUE_EXPR(RebindThisInConstructor)
    NOT_LVALUE_EXPR(If)
#undef NOT_LVALUE_EXPR

    LValue visitTupleElementExpr(TupleElementExpr *E) {
      return emitTupleElementLValue(IGF, E);
    }

    // Qualification never affects emission as an l-value.
    LValue visitRequalifyExpr(RequalifyExpr *E) {
      return visit(E->getSubExpr());
    }

    LValue visitMaterializeExpr(MaterializeExpr *E) {
      OwnedAddress addr = emitMaterializeExpr(IGF, E);
      return IGF.emitAddressLValue(addr);
    }

    LValue visitDeclRefExpr(DeclRefExpr *E) {
      return emitDeclRefLValue(IGF, E->getDecl(), E->getType());
    }
    
    LValue visitSuperRefExpr(SuperRefExpr *E) {
      return emitDeclRefLValue(IGF, E->getThis(), E->getType());
    }
    
    LValue visitMemberRefExpr(MemberRefExpr *E) {
      return emitMemberRefLValue(IGF, E);
    }
    
    LValue visitSubscriptExpr(SubscriptExpr *E) {
      return emitSubscriptLValue(IGF, E);
    }

#define FOR_MEMBER_KIND(KIND)                                              \
    LValue visit##KIND##MemberRefExpr(KIND##MemberRefExpr *E) {            \
      return emit##KIND##MemberRefLValue(IGF, E);                          \
    }                                                                      \
    LValue visit##KIND##SubscriptExpr(KIND##SubscriptExpr *E) {            \
      return emit##KIND##SubscriptLValue(IGF, E);                          \
    }

    FOR_MEMBER_KIND(Existential)
    FOR_MEMBER_KIND(Archetype)
    FOR_MEMBER_KIND(Generic)
#undef FOR_MEMBER_KIND
  };
}

/// Emit the given expression as an l-value.  The expression must
/// actually have l-value kind; to try to find an address for an
/// expression as an aggressive local optimization, use
/// tryEmitAsAddress.
LValue IRGenFunction::emitLValue(Expr *E) {
  assert(E->getType()->is<LValueType>());
  return LValueEmitter(*this).visit(E);
}

namespace {
  class AddressEmitter : public irgen::ASTVisitor<AddressEmitter,
                                                  Optional<Address>,
                                                  void,
                                                  Optional<Address> >
  {
    IRGenFunction &IGF;
    const TypeInfo &ObjectType;

  public:
    AddressEmitter(IRGenFunction &IGF, const TypeInfo &objectType)
      : IGF(IGF), ObjectType(objectType) {}

#define NON_LOCATEABLE(T) \
    Optional<Address> visit##T(T *D) { return Nothing; }

    // Look through loads without further ado.
    Optional<Address> visitLoadExpr(LoadExpr *E) {
      return visit(E->getSubExpr());
    }

    // We can find addresses for some locals.
    Optional<Address> visitDeclRefExpr(DeclRefExpr *E) {
      return visit(E->getDecl());
    }
    Optional<Address> visitSuperRefExpr(SuperRefExpr *E) {
      return visit(E->getThis());
    }

    // Visiting a decl is equivalent to visiting a reference to it.

    // Ignore non-value decls.
#define DECL(Id, Parent) \
    Optional<Address> visit##Id##Decl(Id##Decl *D) { \
      llvm_unreachable("not a value decl!"); \
    }
#define VALUE_DECL(Id, Parent)
#include "swift/AST/DeclNodes.def"

    // These are r-values.
    NON_LOCATEABLE(FuncDecl)
    NON_LOCATEABLE(OneOfElementDecl)

    // These are potentially supportable.
    NON_LOCATEABLE(TypeAliasDecl)
    NON_LOCATEABLE(OneOfDecl)
    NON_LOCATEABLE(StructDecl)
    NON_LOCATEABLE(ClassDecl)
    NON_LOCATEABLE(ProtocolDecl)
                                                    
    // FIXME: Not really a ValueDecl.
    NON_LOCATEABLE(SubscriptDecl)
    NON_LOCATEABLE(ConstructorDecl)
    NON_LOCATEABLE(DestructorDecl)
                                                    
    // These we support.
    Optional<Address> visitVarDecl(VarDecl *D) {
      // For now, only bother with locals.
      if (!D->getDeclContext()->isLocalContext())
        return Nothing;

      return IGF.getLocalVar(D);
    }

    // Some call results will naturally come back in memory.
    Optional<Address> visitApplyExpr(ApplyExpr *E) {
      return tryEmitApplyAsAddress(IGF, E, ObjectType);
    }

    // Changes in qualification are unimportant for this.
    Optional<Address> visitRequalifyExpr(RequalifyExpr *E) {
      return visit(E->getSubExpr());
    }
    Optional<Address> visitAddressOfExpr(AddressOfExpr *E) {
      return visit(E->getSubExpr());
    }

    // We can locate a tuple element if we can locate the tuple.
    Optional<Address> visitTupleElementExpr(TupleElementExpr *E) {
      return tryEmitTupleElementAsAddress(IGF, E);
    }

    // Materializations are always in memory.
    Optional<Address> visitMaterializeExpr(MaterializeExpr *E) {
      return emitMaterializeExpr(IGF, cast<MaterializeExpr>(E));
    }

    Optional<Address> visitMemberRefExpr(MemberRefExpr *E) {
      return tryEmitMemberRefAsAddress(IGF, E);
    }

    // These expressions aren't naturally already in memory.
    NON_LOCATEABLE(TupleExpr)
    NON_LOCATEABLE(ArrayExpr)
    NON_LOCATEABLE(CollectionExpr)
    NON_LOCATEABLE(IntegerLiteralExpr)
    NON_LOCATEABLE(FloatLiteralExpr)
    NON_LOCATEABLE(CharacterLiteralExpr)
    NON_LOCATEABLE(StringLiteralExpr)
    NON_LOCATEABLE(InterpolatedStringLiteralExpr)
    NON_LOCATEABLE(TupleShuffleExpr)
    NON_LOCATEABLE(ErasureExpr)
    NON_LOCATEABLE(SpecializeExpr) // FIXME: Generic subscripts?
    NON_LOCATEABLE(DerivedToBaseExpr)
    NON_LOCATEABLE(ArchetypeToSuperExpr)
    NON_LOCATEABLE(ScalarToTupleExpr)
    NON_LOCATEABLE(FunctionConversionExpr)
    NON_LOCATEABLE(MetatypeConversionExpr)
    NON_LOCATEABLE(CapturingExpr)
    NON_LOCATEABLE(ModuleExpr)
    NON_LOCATEABLE(DotSyntaxBaseIgnoredExpr)
    NON_LOCATEABLE(NewReferenceExpr)
    NON_LOCATEABLE(NewArrayExpr)
    NON_LOCATEABLE(MetatypeExpr)
    NON_LOCATEABLE(CoerceExpr)
    NON_LOCATEABLE(UncheckedDowncastExpr)
    NON_LOCATEABLE(UncheckedSuperToArchetypeExpr)
    NON_LOCATEABLE(IsSubtypeExpr)
    NON_LOCATEABLE(SuperIsArchetypeExpr)
    NON_LOCATEABLE(ExistentialMemberRefExpr)
    NON_LOCATEABLE(ArchetypeMemberRefExpr)
    NON_LOCATEABLE(GenericMemberRefExpr)
    NON_LOCATEABLE(BridgeToBlockExpr)
    NON_LOCATEABLE(OtherConstructorDeclRefExpr)
    NON_LOCATEABLE(RebindThisInConstructorExpr)
    NON_LOCATEABLE(IfExpr)

    // FIXME: We may want to specialize IR generation for array subscripts.
    NON_LOCATEABLE(SubscriptExpr)
    NON_LOCATEABLE(ExistentialSubscriptExpr)
    NON_LOCATEABLE(ArchetypeSubscriptExpr)
    NON_LOCATEABLE(GenericSubscriptExpr)

#undef NON_LOCATEABLE
  };
}

/// Try to emit the given expression as an entity with an address.
/// This is useful for local optimizations.
Optional<Address>
IRGenFunction::tryEmitAsAddress(Expr *E, const TypeInfo &type) {
  return AddressEmitter(*this, type).visit(E);
}

namespace {
  struct IgnoredExprEmitter : irgen::ASTVisitor<IgnoredExprEmitter> {
    IRGenFunction &IGF;
    IgnoredExprEmitter(IRGenFunction &IGF) : IGF(IGF) {}

#define EXPR(Id, Parent)
#define UNCHECKED_EXPR(Id, Parent) \
    void visit##Id##Expr(Id##Expr *E) { \
      llvm_unreachable("expression should not have survived to IR-gen"); \
    }
#include "swift/AST/ExprNodes.def"
    void visitErrorExpr(ErrorExpr *E) {
      llvm_unreachable("expression should not have survived to IR-gen");
    }
    void visitIntegerLiteralExpr(IntegerLiteralExpr *E) {}
    void visitFloatLiteralExpr(FloatLiteralExpr *E) {}
    void visitDeclRefExpr(DeclRefExpr *E) {}
    void visitDotSyntaxBaseIgnoredExpr(DotSyntaxBaseIgnoredExpr *E) {
      visit(E->getLHS());
      visit(E->getRHS());
    }
    void visitTupleExpr(TupleExpr *E) {
      for (auto elt : E->getElements())
        visit(elt);
    }
    void visitTupleElementExpr(TupleElementExpr *E) {
      visit(E->getBase());
    }
    void visitFuncExpr(FuncExpr *E) {}
    void visitClosureExpr(ClosureExpr *E) {}
    void visitModuleExpr(ModuleExpr *E) {}

#define USING_SUBEXPR(Id) \
    void visit##Id##Expr(Id##Expr *E) { \
      return visit(E->getSubExpr()); \
    }
    USING_SUBEXPR(Paren)
    USING_SUBEXPR(AddressOf)
    USING_SUBEXPR(Requalify)
    USING_SUBEXPR(Materialize)

    void visitTupleShuffleExpr(TupleShuffleExpr *E) {
      // First, evaluate the base expression.
      visit(E->getSubExpr());

      // Then evaluate any defaulted elements.
      TupleType *TT = E->getType()->castTo<TupleType>();
      for (unsigned i = 0, e = TT->getFields().size(); i != e; ++i) {
        if (E->getElementMapping()[i] == -1)
          visit(TT->getFields()[i].getInit()->getExpr());
      }
    }

    void visitExpr(Expr *E) {
      // If all else fails, emit it as an r-value.
      Explosion explosion(ExplosionKind::Maximal);
      IGF.emitRValue(E, explosion);

      // Ignore all the values.
      explosion.ignoreAndDestroy(IGF, explosion.size());
    }
  };
}

/// Emit an expression whose value is being ignored.
void IRGenFunction::emitIgnored(Expr *E) {
  IgnoredExprEmitter(*this).visit(E);
}

/// Emit a fake l-value which obeys the given specification.  This
/// should only ever be used for error recovery.
LValue IRGenFunction::emitFakeLValue(Type type) {
  Type objTy = type->castTo<LValueType>()->getObjectType();
  const TypeInfo &lvalueInfo = getFragileTypeInfo(objTy);
  llvm::Value *fakeAddr =
    llvm::UndefValue::get(lvalueInfo.getStorageType()->getPointerTo());
  return emitAddressLValue(OwnedAddress(Address(fakeAddr,
                                                lvalueInfo.StorageAlignment),
                                        IGM.RefCountedNull));
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
