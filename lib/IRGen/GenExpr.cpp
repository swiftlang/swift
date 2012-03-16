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

#include "swift/AST/Types.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Expr.h"
#include "swift/Basic/Optional.h"
#include "llvm/DerivedTypes.h"
#include "llvm/Function.h"
#include "llvm/Target/TargetData.h"

#include "GenType.h"
#include "IRGenFunction.h"
#include "IRGenModule.h"
#include "LValue.h"
#include "Explosion.h"

using namespace swift;
using namespace irgen;

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

static LValue emitDeclRefLValue(IRGenFunction &IGF, DeclRefExpr *E) {
  ValueDecl *D = E->getDecl();
  switch (D->getKind()) {
  case DeclKind::Extension:
  case DeclKind::Import:
  case DeclKind::TypeAlias:
    llvm_unreachable("decl is not a value decl");

  case DeclKind::Func:
  case DeclKind::OneOfElement:
    llvm_unreachable("decl cannot be emitted as an l-value");

  case DeclKind::Var:
    if (D->getDeclContext()->isLocalContext())
      return IGF.emitAddressLValue(IGF.getLocal(D));
    return IGF.getGlobal(cast<VarDecl>(D));

  case DeclKind::ElementRef:
    IGF.unimplemented(E->getLoc(), "emitting this decl as an l-value");
    return IGF.emitFakeLValue(IGF.getFragileTypeInfo(D->getType()));
  }
  llvm_unreachable("bad decl kind");
}

/// Emit a declaration reference as an exploded r-value.
void IRGenFunction::emitExplodedDeclRef(DeclRefExpr *E, Explosion &explosion) {
  ValueDecl *D = E->getDecl();
  switch (D->getKind()) {
  case DeclKind::Extension:
  case DeclKind::Import:
  case DeclKind::TypeAlias:
    llvm_unreachable("decl is not a value decl");

  case DeclKind::Var:
    return emitLValueAsScalar(emitDeclRefLValue(*this, E), explosion);

  case DeclKind::Func:
    emitExplodedRValueForFunction(cast<FuncDecl>(D), explosion);
    return;

  case DeclKind::OneOfElement:
    return emitOneOfElementRef(cast<OneOfElementDecl>(D), explosion);

  case DeclKind::ElementRef:
    unimplemented(E->getLoc(), "emitting this decl as an r-value");
    return emitFakeExplosion(getFragileTypeInfo(E->getType()), explosion);
  }
  llvm_unreachable("bad decl kind");
}

/// Emit the given expression, which must have primitive scalar type,
/// as that primitive scalar value.  This is just a convenience method
/// for not needing to construct and destroy an Explosion.
llvm::Value *IRGenFunction::emitAsPrimitiveScalar(Expr *E) {
  Explosion explosion(ExplosionKind::Minimal);
  emitExplodedRValue(E, explosion);

  llvm::Value *result = explosion.claimNext();
  assert(explosion.empty());
  return result;
}

/// Emit a rvalue-to-lvalue conversion.
static Address emitMaterializeExpr(IRGenFunction &IGF, MaterializeExpr *E) {
  Expr *subExpr = E->getSubExpr();
  const TypeInfo &valueTI = IGF.getFragileTypeInfo(subExpr->getType());
  Address temporary = IGF.createFullExprAlloca(valueTI.getStorageType(),
                                               valueTI.StorageAlignment,
                                               "materialized-temporary");
  IGF.emitRValueToMemory(subExpr, temporary, valueTI);
  return temporary;
}

void IRGenFunction::emitExplodedRValue(Expr *E, Explosion &explosion) {
  switch (E->getKind()) {
#define EXPR(Id, Parent)
#define UNCHECKED_EXPR(Id, Parent) case ExprKind::Id:
#include "swift/AST/ExprNodes.def"
  case ExprKind::Error:
    llvm_unreachable("these expression kinds should not survive to IR-gen");

  case ExprKind::AnonClosureArg:
    llvm_unreachable("these expression kinds should never be rvalues");

  case ExprKind::Load: {
    const TypeInfo &type = getFragileTypeInfo(E->getType());
    return emitExplodedLoad(emitLValue(cast<LoadExpr>(E)->getSubExpr()),
                            type, explosion);
  }

  case ExprKind::Materialize: {
    Address addr = emitMaterializeExpr(*this, cast<MaterializeExpr>(E));
    explosion.add(addr.getAddress());
    return;
  }

  case ExprKind::Paren:
    return emitExplodedRValue(cast<ParenExpr>(E)->getSubExpr(), explosion);

  case ExprKind::AddressOf:
    return emitExplodedRValue(cast<AddressOfExpr>(E)->getSubExpr(),
                              explosion);    

  case ExprKind::Tuple:
    return emitExplodedTupleLiteral(cast<TupleExpr>(E), explosion);

  case ExprKind::TupleShuffle:
    return emitExplodedTupleShuffle(cast<TupleShuffleExpr>(E), explosion);

  case ExprKind::SyntacticTupleElement:
  case ExprKind::ImplicitThisTupleElement:
    return emitExplodedTupleElement(cast<TupleElementExpr>(E), explosion);

  case ExprKind::DotSyntaxPlusFuncUse: {
    DotSyntaxPlusFuncUseExpr *DE = cast<DotSyntaxPlusFuncUseExpr>(E);
    emitIgnored(DE->getBaseExpr());
    return emitExplodedRValue(DE->getPlusFuncExpr(), explosion);
  }

  case ExprKind::Call:
  case ExprKind::Unary:
  case ExprKind::Binary:
  case ExprKind::ConstructorCall:
  case ExprKind::DotSyntaxCall:
    return emitExplodedApplyExpr(cast<ApplyExpr>(E), explosion);

  case ExprKind::IntegerLiteral:
    return explosion.add(emitIntegerLiteralExpr(*this, cast<IntegerLiteralExpr>(E)));
  case ExprKind::FloatLiteral:
    return explosion.add(emitFloatLiteralExpr(*this, cast<FloatLiteralExpr>(E)));

  case ExprKind::LookThroughOneof:
    return emitExplodedRValue(cast<LookThroughOneofExpr>(E)->getSubExpr(),
                              explosion);

  case ExprKind::DeclRef:
    return emitExplodedDeclRef(cast<DeclRefExpr>(E), explosion);

  case ExprKind::ImplicitClosure:
  case ExprKind::ExplicitClosure:
    return emitExplodedClosure(cast<ClosureExpr>(E), explosion);

  case ExprKind::Func:
  case ExprKind::Module:
    IGM.unimplemented(E->getLoc(),
                      "cannot explode r-values for this expression yet");
    return emitFakeExplosion(getFragileTypeInfo(E->getType()), explosion);
  }
  llvm_unreachable("bad expression kind!");
}

/// Emit the given expression as an l-value.  The expression must
/// actually have l-value kind; to try to find an address for an
/// expression as an aggressive local optimization, use
/// tryEmitAsAddress.
LValue IRGenFunction::emitLValue(Expr *E) {
  assert(E->getType()->is<LValueType>());

  switch (E->getKind()) {
#define EXPR(Id, Parent)
#define UNCHECKED_EXPR(Id, Parent) case ExprKind::Id:
#include "swift/AST/ExprNodes.def"
  case ExprKind::Error:
    llvm_unreachable("these expression kinds should not survive to IR-gen");

  case ExprKind::Call:
  case ExprKind::Unary:
  case ExprKind::Binary:
  case ExprKind::IntegerLiteral:
  case ExprKind::FloatLiteral:
  case ExprKind::TupleShuffle:
  case ExprKind::Func:
  case ExprKind::ExplicitClosure:
  case ExprKind::ImplicitClosure:
  case ExprKind::Load:
  case ExprKind::Tuple:
  case ExprKind::DotSyntaxPlusFuncUse:
  case ExprKind::Module:
  case ExprKind::ConstructorCall:
  case ExprKind::DotSyntaxCall:
    llvm_unreachable("these expression kinds should never be l-values");

  case ExprKind::Paren:
    return emitLValue(cast<ParenExpr>(E)->getSubExpr());

  case ExprKind::AddressOf:
    return emitLValue(cast<AddressOfExpr>(E)->getSubExpr());

  case ExprKind::SyntacticTupleElement:
  case ExprKind::ImplicitThisTupleElement:
    return emitTupleElementLValue(cast<TupleElementExpr>(E));

  case ExprKind::LookThroughOneof:
    return emitLookThroughOneofLValue(cast<LookThroughOneofExpr>(E));

  case ExprKind::Materialize: {
    Address addr = emitMaterializeExpr(*this, cast<MaterializeExpr>(E));
    return emitAddressLValue(addr);
  }

  case ExprKind::DeclRef:
    return emitDeclRefLValue(*this, cast<DeclRefExpr>(E));

  case ExprKind::AnonClosureArg: {
    Address addr = ClosureParams[cast<AnonClosureArgExpr>(E)->getArgNumber()];
    return emitAddressLValue(addr);
  }
  }
  llvm_unreachable("bad expression kind!");
}

/// Try to emit the given expression as an entity with an address.
/// This is useful for local optimizations.
Optional<Address>
IRGenFunction::tryEmitAsAddress(Expr *E, const TypeInfo &type) {
  switch (E->getKind()) {
#define EXPR(Id, Parent)
#define UNCHECKED_EXPR(Id, Parent) case ExprKind::Id:
#include "swift/AST/ExprNodes.def"
  case ExprKind::Error:
    llvm_unreachable("these expression kinds should not survive to IR-gen");

  // Look through loads without further ado.
  case ExprKind::Load:
    return tryEmitAsAddress(cast<LoadExpr>(E)->getSubExpr(), type);

  // We can find addresses for some locals.
  case ExprKind::DeclRef: {
    ValueDecl *D = cast<DeclRefExpr>(E)->getDecl();
    switch (D->getKind()) {
#define DECL(Id, Parent) case DeclKind::Id:
#define VALUE_DECL(Id, Parent)
#include "swift/AST/DeclNodes.def"
      llvm_unreachable("not a value decl!");

    // These are r-values.
    case DeclKind::Func:
    case DeclKind::OneOfElement:
      return Nothing;

    // These are potentially supportable.
    case DeclKind::TypeAlias:
    case DeclKind::ElementRef:
      return Nothing;

    // These we support.
    case DeclKind::Var:
      // For now, only bother with locals.
      if (!D->getDeclContext()->isLocalContext())
        return Nothing;

      return getLocal(D);
    }
    llvm_unreachable("bad declaration kind!");
  }

  // Some call results will naturally come back in memory.
  case ExprKind::Call:
  case ExprKind::Unary:
  case ExprKind::Binary:
  case ExprKind::DotSyntaxCall:
  case ExprKind::ConstructorCall:
    return tryEmitApplyAsAddress(cast<ApplyExpr>(E), type);

  // Look through parens.
  case ExprKind::Paren:
    return tryEmitAsAddress(cast<ParenExpr>(E)->getSubExpr(), type);

  // We can locate a oneof payload if we can locate the oneof.
  case ExprKind::LookThroughOneof:
    return tryEmitLookThroughOneofAsAddress(cast<LookThroughOneofExpr>(E));

  // We can locate a tuple element if we can locate the tuple.
  case ExprKind::SyntacticTupleElement:
  case ExprKind::ImplicitThisTupleElement:
    return tryEmitTupleElementAsAddress(cast<TupleElementExpr>(E));

  // &x is in memory if x is.
  case ExprKind::AddressOf:
    // The difference in type between explicit and implicit is
    // currently not relevant.
    return tryEmitAsAddress(cast<AddressOfExpr>(E)->getSubExpr(), type);

  // Materializations are always in memory.
  case ExprKind::Materialize:
    return emitMaterializeExpr(*this, cast<MaterializeExpr>(E));

  // Closure arguments are always in memory.
  case ExprKind::AnonClosureArg:
    return ClosureParams[cast<AnonClosureArgExpr>(E)->getArgNumber()];

  // These expressions aren't naturally already in memory.
  case ExprKind::Tuple:
  case ExprKind::IntegerLiteral:
  case ExprKind::FloatLiteral:
  case ExprKind::TupleShuffle:
  case ExprKind::Func:
  case ExprKind::ExplicitClosure:
  case ExprKind::ImplicitClosure:
  case ExprKind::DotSyntaxPlusFuncUse:
  case ExprKind::Module:
    return Nothing;
  }
  llvm_unreachable("bad expression kind!");
}

/// Emit an expression as an initializer for the given l-value.
void IRGenFunction::emitInit(Address addr, Expr *E, const TypeInfo &type) {
  emitRValueToMemory(E, addr, type);
}

/// Emit an r-value directly into memory.
void IRGenFunction::emitRValueToMemory(Expr *E, Address addr,
                                       const TypeInfo &type) {
  Explosion explosion(ExplosionKind::Maximal);
  emitExplodedRValue(E, explosion);
  type.storeExplosion(*this, explosion, addr);
}

/// Zero-initializer the given l-value.
void IRGenFunction::emitZeroInit(Address addr, const TypeInfo &type) {
  ExplosionSchema schema(ExplosionKind::Maximal);
  type.getExplosionSchema(schema);

  // Try to fill the value in with stores if that doesn't make for a
  // ridiculous amount of IR.  This is impossible if the schema
  // contains an aggregate;  otherwise, 4 is just a number.
  if (!schema.containsAggregate() && schema.size() <= 4) {
    Explosion explosion(schema.getKind());
    for (auto elt : schema) {
      explosion.add(llvm::Constant::getNullValue(elt.getScalarType()));
    }
    type.initWithExplosion(*this, explosion, addr);
    return;
  }

  // Otherwise, just do a memset.
  Builder.CreateMemSet(Builder.CreateBitCast(addr.getAddress(), IGM.Int8PtrTy),
                       Builder.getInt8(0),
                       Builder.getInt64(type.StorageSize.getValue()),
                       addr.getAlignment().getValue(),
                       /*volatile*/ false);
}

/// Emit an expression whose value is being ignored.
void IRGenFunction::emitIgnored(Expr *E) {
  // For now, just emit it as an r-value.
  Explosion explosion(ExplosionKind::Maximal);
  emitExplodedRValue(E, explosion);
}

/// Emit a fake l-value which obeys the given specification.  This
/// should only ever be used for error recovery.
LValue IRGenFunction::emitFakeLValue(const TypeInfo &type) {
  llvm::Value *fakeAddr =
    llvm::UndefValue::get(type.getStorageType()->getPointerTo());
  return emitAddressLValue(Address(fakeAddr, type.StorageAlignment));
}

void IRGenFunction::emitFakeExplosion(const TypeInfo &type, Explosion &explosion) {
  ExplosionSchema schema(explosion.getKind());
  type.getExplosionSchema(schema);
  for (auto &element : schema) {
    llvm::Type *elementType;
    if (element.isAggregate()) {
      elementType = element.getAggregateType()->getPointerTo();
    } else {
      elementType = element.getScalarType();
    }

    explosion.add(llvm::UndefValue::get(elementType));
  }
}
