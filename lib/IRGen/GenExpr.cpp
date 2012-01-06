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
#include "RValue.h"

using namespace swift;
using namespace irgen;

/// Emit an integer literal expression.
static RValue emitIntegerLiteralExpr(IRGenFunction &IGF, IntegerLiteralExpr *E,
                                     const TypeInfo &TInfo) {
  // We know that the integer literal type has a BuiltinIntegerType.
  assert(TInfo.getSchema().isScalar(1));
  assert(isa<llvm::IntegerType>(TInfo.getSchema().getScalarTypes()[0]));
  return RValue::forScalars(llvm::ConstantInt::get(IGF.IGM.LLVMContext,
                                                   E->getValue()));
}

/// Emit an float literal expression.
static RValue emitFloatLiteralExpr(IRGenFunction &IGF, FloatLiteralExpr *E,
                                   const TypeInfo &TInfo) {
  // We know that the integer literal type has a BuiltinFloatingPointType.
  assert(TInfo.getSchema().isScalar(1));
  assert(TInfo.getSchema().getScalarTypes()[0]->isFloatingPointTy());
  
  return RValue::forScalars(llvm::ConstantFP::get(IGF.IGM.LLVMContext,
                                                  E->getValue()));
}

static LValue emitDeclRefLValue(IRGenFunction &IGF, DeclRefExpr *E,
                                const TypeInfo &TInfo) {
  ValueDecl *D = E->getDecl();
  switch (D->getKind()) {
  case DeclKind::Extension:
  case DeclKind::Import:
  case DeclKind::TypeAlias:
    llvm_unreachable("decl is not a value decl");

  case DeclKind::Func:
  case DeclKind::MetaType:
    llvm_unreachable("decl cannot be emitted as an l-value");

  case DeclKind::Var:
    if (D->getDeclContext()->isLocalContext())
      return IGF.emitAddressLValue(IGF.getLocal(D));
    return IGF.getGlobal(cast<VarDecl>(D), TInfo);

  case DeclKind::Arg:
    return IGF.emitAddressLValue(IGF.getLocal(D));

  case DeclKind::ElementRef:
  case DeclKind::OneOfElement:
    IGF.unimplemented(E->getLoc(), "emitting this decl as an l-value");
    return IGF.emitFakeLValue(TInfo);
  }
  llvm_unreachable("bad decl kind");
}

/// Emit a declaration reference as an r-value.
RValue IRGenFunction::emitDeclRefRValue(DeclRefExpr *E, const TypeInfo &TInfo) {
  ValueDecl *D = E->getDecl();
  switch (D->getKind()) {
  case DeclKind::Extension:
  case DeclKind::Import:
  case DeclKind::TypeAlias:
    llvm_unreachable("decl is not a value decl");

  case DeclKind::MetaType:
    llvm_unreachable("metatype has no run-time representation");

  case DeclKind::Arg:
  case DeclKind::Var:
    return emitLoad(emitDeclRefLValue(*this, E, TInfo), TInfo);

  case DeclKind::Func:
    return emitRValueForFunction(cast<FuncDecl>(D));

  case DeclKind::OneOfElement: {
    llvm::Value *fn =
      IGM.getAddrOfInjectionFunction(cast<OneOfElementDecl>(D));
    llvm::Value *data = llvm::UndefValue::get(IGM.Int8PtrTy);
    return RValue::forScalars(fn, data);
  }

  case DeclKind::ElementRef:
    unimplemented(E->getLoc(), "emitting this decl as an r-value");
    return emitFakeRValue(TInfo);
  }
  llvm_unreachable("bad decl kind");
}

RValue IRGenFunction::emitRValue(Expr *E) {
  const TypeInfo &TInfo = IGM.getFragileTypeInfo(E->getType());
  return emitRValue(E, TInfo);
}

/// Emit the given expression as an r-value.  The expression need not
/// actually have r-value kind.
RValue IRGenFunction::emitRValue(Expr *E, const TypeInfo &TInfo) {
  switch (E->getKind()) {
#define EXPR(Id, Parent)
#define UNCHECKED_EXPR(Id, Parent) case ExprKind::Id:
#include "swift/AST/ExprNodes.def"
    llvm_unreachable("these expression kinds should not survive to IR-gen");

  case ExprKind::Load:
    return emitRValue(cast<LoadExpr>(E)->getSubExpr(), TInfo);

  case ExprKind::Call:
  case ExprKind::Unary:
  case ExprKind::Binary:
    return emitApplyExpr(cast<ApplyExpr>(E), TInfo);

  case ExprKind::IntegerLiteral:
    return emitIntegerLiteralExpr(*this, cast<IntegerLiteralExpr>(E), TInfo);
  case ExprKind::FloatLiteral:
    return emitFloatLiteralExpr(*this, cast<FloatLiteralExpr>(E), TInfo);

  case ExprKind::Tuple:
    return emitTupleExpr(cast<TupleExpr>(E), TInfo);
  case ExprKind::TupleElement:
    return emitTupleElementRValue(cast<TupleElementExpr>(E), TInfo);
  case ExprKind::TupleShuffle:
    return emitTupleShuffleExpr(cast<TupleShuffleExpr>(E), TInfo);

  case ExprKind::LookThroughOneof:
    return emitLookThroughOneofRValue(cast<LookThroughOneofExpr>(E));

  case ExprKind::DeclRef:
    return emitDeclRefRValue(cast<DeclRefExpr>(E), TInfo);

  case ExprKind::Func:
  case ExprKind::Closure:
  case ExprKind::AnonClosureArg:
  case ExprKind::DotSyntaxCall:
    IGM.unimplemented(E->getLoc(),
                      "cannot generate r-values for this expression yet");
    return emitFakeRValue(TInfo);
  }
  llvm_unreachable("bad expression kind!");
}

LValue IRGenFunction::emitLValue(Expr *E) {
  const TypeInfo &TInfo = IGM.getFragileTypeInfo(E->getType());
  return emitLValue(E, TInfo);
}

/// Emit the given expression as an l-value.  The expression must
/// actually have l-value kind; to try to emit an expression as an
/// l-value as an aggressive local optimization, use tryEmitAsLValue.
LValue IRGenFunction::emitLValue(Expr *E, const TypeInfo &TInfo) {
  assert(E->getValueKind() == ValueKind::LValue);

  switch (E->getKind()) {
#define EXPR(Id, Parent)
#define UNCHECKED_EXPR(Id, Parent) case ExprKind::Id:
#include "swift/AST/ExprNodes.def"
    llvm_unreachable("these expression kinds should not survive to IR-gen");

  case ExprKind::Call:
  case ExprKind::Unary:
  case ExprKind::Binary:
  case ExprKind::IntegerLiteral:
  case ExprKind::FloatLiteral:
  case ExprKind::TupleShuffle:
  case ExprKind::Func:
  case ExprKind::Closure:
  case ExprKind::AnonClosureArg:
  case ExprKind::Load:
    llvm_unreachable("these expression kinds should never be l-values");

  case ExprKind::DotSyntaxCall:
    IGM.unimplemented(E->getLoc(),
                      "cannot generate l-values for this expression yet");
    return emitFakeLValue(TInfo);

  case ExprKind::Tuple: {
    TupleExpr *TE = cast<TupleExpr>(E);
    assert(TE->isGroupingParen() && "emitting non-grouping tuple as l-value");
    return emitLValue(TE->getElement(0), TInfo);
  }

  case ExprKind::TupleElement:
    return emitTupleElementLValue(cast<TupleElementExpr>(E), TInfo);

  case ExprKind::LookThroughOneof:
    return emitLookThroughOneofLValue(cast<LookThroughOneofExpr>(E));

  case ExprKind::DeclRef:
    return emitDeclRefLValue(*this, cast<DeclRefExpr>(E), TInfo);
  }
  llvm_unreachable("bad expression kind!");
}

/// Try to emit the given expression as an underlying l-value.
Optional<LValue> IRGenFunction::tryEmitAsLValue(Expr *E,
                                                const TypeInfo &type) {
  // If it *is* an l-value, then go ahead.
  if (E->getValueKind() == ValueKind::LValue)
    return emitLValue(E, type);

  switch (E->getKind()) {
#define EXPR(Id, Parent)
#define UNCHECKED_EXPR(Id, Parent) case ExprKind::Id:
#include "swift/AST/ExprNodes.def"
    llvm_unreachable("these expression kinds should not survive to IR-gen");

  case ExprKind::Load:
    return emitLValue(cast<LoadExpr>(E)->getSubExpr(), type);

  case ExprKind::Call:
  case ExprKind::Unary:
  case ExprKind::Binary:
  case ExprKind::IntegerLiteral:
  case ExprKind::FloatLiteral:
  case ExprKind::DeclRef:
  case ExprKind::Func:
  case ExprKind::Closure:
  case ExprKind::AnonClosureArg:
  case ExprKind::DotSyntaxCall:
    // These can never be usefully emitted as l-values, if they
    // weren't l-values before.
    return Nothing;

  case ExprKind::Tuple: {
    TupleExpr *tuple = cast<TupleExpr>(E);
    if (tuple->isGroupingParen())
      return tryEmitAsLValue(tuple->getElement(0), type);
    return Nothing;
  }

  case ExprKind::TupleElement:
  case ExprKind::TupleShuffle:
  case ExprKind::LookThroughOneof:
    // These could all be usefully emitted as l-values in some cases,
    // but we haven't bothered implementing that yet.
    return Nothing;
  }
  llvm_unreachable("bad expression kind!");
}

/// Emit an expression as an initializer for the given l-value.
void IRGenFunction::emitInit(Address addr, Expr *E, const TypeInfo &TInfo) {
  // TODO: we can do better than this.
  RValue RV = emitRValue(E);
  TInfo.store(*this, RV, addr);
}

/// Zero-initializer the given l-value.
void IRGenFunction::emitZeroInit(Address addr, const TypeInfo &type) {
  RValueSchema schema = type.getSchema();

  // If the schema is scalar, just store a bunch of values into it.
  // This makes for better IR than a memset.
  if (schema.isScalar()) {
    SmallVector<llvm::Value*, RValue::MaxScalars> scalars;
    for (llvm::Type *ty : schema.getScalarTypes()) {
      scalars.push_back(llvm::Constant::getNullValue(ty));
    }
    type.store(*this, RValue::forScalars(scalars), addr);
    return;
  }

  // Otherwise, since the schema is aggregate, do a memset.
  Builder.CreateMemSet(Builder.CreateBitCast(addr.getAddress(), IGM.Int8PtrTy),
                       Builder.getInt8(0),
                       Builder.getInt64(type.StorageSize.getValue()),
                       addr.getAlignment().getValue(),
                       /*volatile*/ false);
}

/// Emit an expression whose value is being ignored.
void IRGenFunction::emitIgnored(Expr *E) {
  // For now, just emit it as an r-value.
  emitRValue(E);
}

/// Emit a fake l-value which obeys the given specification.  This
/// should only ever be used for error recovery.
LValue IRGenFunction::emitFakeLValue(const TypeInfo &type) {
  llvm::Value *fakeAddr =
    llvm::UndefValue::get(type.getStorageType()->getPointerTo());
  return emitAddressLValue(Address(fakeAddr, type.StorageAlignment));
}

/// Emit a fake r-value which obeys the given specification.  This
/// should only ever be used for error recovery.
RValue IRGenFunction::emitFakeRValue(const TypeInfo &TInfo) {
  RValueSchema Schema = TInfo.getSchema();
  if (Schema.isScalar()) {
    llvm::SmallVector<llvm::Value*, RValue::MaxScalars> Scalars;
    for (llvm::Type *T : Schema.getScalarTypes()) {
      Scalars.push_back(llvm::UndefValue::get(T));
    }
    return RValue::forScalars(Scalars);
  } else {
    llvm::Value *Addr =
      llvm::UndefValue::get(Schema.getAggregateType()->getPointerTo());
    return RValue::forAggregate(Addr);
  }
}
