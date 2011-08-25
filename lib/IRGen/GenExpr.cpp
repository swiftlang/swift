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
  // We make this work by making some pretty awesome assumptions about
  // how the type is represented.  Probably there ought to be
  // something slightly less awesome.
  RValueSchema Schema = TInfo.getSchema();
  assert(Schema.isScalar(1));
  llvm::IntegerType *IntTy =
    cast<llvm::IntegerType>(Schema.getScalarTypes()[0]);

  llvm::Value *Value = llvm::ConstantInt::get(IntTy, E->getValue());
  return RValue::forScalars(Value);
}

RValue IRGenFunction::emitRValue(Expr *E) {
  const TypeInfo &TInfo = IGM.getFragileTypeInfo(E->Ty);
  return emitRValue(E, TInfo);
}

RValue IRGenFunction::emitRValue(Expr *E, const TypeInfo &TInfo) {
  switch (E->Kind) {
  case ExprKind::OverloadSetRef:
  case ExprKind::Sequence:
  case ExprKind::UnresolvedDeclRef:
  case ExprKind::UnresolvedDot:
  case ExprKind::UnresolvedMember:
  case ExprKind::UnresolvedScopedIdentifier:
    llvm_unreachable("these expression kinds should not survive to IR-gen");

  case ExprKind::Call:
  case ExprKind::Unary:
  case ExprKind::Binary:
    return emitApplyExpr(cast<ApplyExpr>(E), TInfo);

  case ExprKind::IntegerLiteral:
    return emitIntegerLiteralExpr(*this, cast<IntegerLiteralExpr>(E), TInfo);

  case ExprKind::Tuple:
    return emitTupleExpr(cast<TupleExpr>(E), TInfo);
  case ExprKind::TupleElement:
    return emitTupleElementExpr(cast<TupleElementExpr>(E), TInfo);
  case ExprKind::TupleShuffle:
    return emitTupleShuffleExpr(cast<TupleShuffleExpr>(E), TInfo);

  case ExprKind::DeclRef:
  case ExprKind::Func:
  case ExprKind::Closure:
  case ExprKind::AnonClosureArg:
    IGM.unimplemented(E->getLocStart(),
                      "cannot generate r-values for this expression yet");
    return emitFakeRValue(TInfo);
  }
  llvm_unreachable("bad expression kind!");
}

LValue IRGenFunction::emitLValue(Expr *E) {
  const TypeInfo &TInfo = IGM.getFragileTypeInfo(E->Ty);
  return emitLValue(E, TInfo);
}

LValue IRGenFunction::emitLValue(Expr *E, const TypeInfo &TInfo) {
  // FIXME: implement
  return LValue();
}

/// Emit an expression whose value is being ignored.
void IRGenFunction::emitIgnored(Expr *E) {
  // For now, just emit it as an r-value.
  emitRValue(E);
}

/// Emit a fake l-value which obeys the given specification.  This
/// should only ever be used for error recovery.
LValue IRGenFunction::emitFakeLValue(const TypeInfo &TInfo) {
  llvm::Value *FakeAddr =
    llvm::UndefValue::get(TInfo.getStorageType()->getPointerTo());
  return LValue::forAddress(FakeAddr, TInfo.StorageAlignment);
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
