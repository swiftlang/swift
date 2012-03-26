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
#include "swift/AST/ASTVisitor.h"
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
void IRGenFunction::emitDeclRef(DeclRefExpr *E, Explosion &explosion) {
  ValueDecl *D = E->getDecl();
  switch (D->getKind()) {
  case DeclKind::Extension:
  case DeclKind::Import:
  case DeclKind::TypeAlias:
    llvm_unreachable("decl is not a value decl");

  case DeclKind::Var:
    return emitLValueAsScalar(emitDeclRefLValue(*this, E),
                              OnHeap, explosion);

  case DeclKind::Func:
    emitRValueForFunction(cast<FuncDecl>(D), explosion);
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
  emitRValue(E, explosion);

  llvm::Value *result = explosion.claimNext();
  assert(explosion.empty());
  return result;
}

/// Emit a rvalue-to-lvalue conversion.
static OwnedAddress emitMaterializeExpr(IRGenFunction &IGF,
                                        MaterializeExpr *E) {
  Expr *subExpr = E->getSubExpr();
  const TypeInfo &valueTI = IGF.getFragileTypeInfo(subExpr->getType());

  bool onHeap = E->getType()->castTo<LValueType>()->isHeap();
  OwnedAddress addr = IGF.createFullExprAlloca(valueTI,
                                               onHeap ? OnHeap : NotOnHeap,
                                               "materialized-temporary");

  IGF.emitRValueToMemory(subExpr, addr.getAddress(), valueTI);
  return addr;
}

void IRGenFunction::emitRValue(Expr *E, Explosion &explosion) {
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
    return emitLoad(emitLValue(cast<LoadExpr>(E)->getSubExpr()),
                            type, explosion);
  }

  case ExprKind::Materialize: {
    OwnedAddress addr = emitMaterializeExpr(*this, cast<MaterializeExpr>(E));
    explosion.add(addr.getAddressPointer());
    return;
  }

  case ExprKind::Requalify:
    return emitRequalify(cast<RequalifyExpr>(E), explosion);

  case ExprKind::Paren:
    return emitRValue(cast<ParenExpr>(E)->getSubExpr(), explosion);

  case ExprKind::AddressOf:
    return emitRValue(cast<AddressOfExpr>(E)->getSubExpr(),
                              explosion);    

  case ExprKind::Tuple:
    return emitTupleLiteral(cast<TupleExpr>(E), explosion);

  case ExprKind::TupleShuffle:
    return emitTupleShuffle(cast<TupleShuffleExpr>(E), explosion);

  case ExprKind::SyntacticTupleElement:
  case ExprKind::ImplicitThisTupleElement:
    return emitTupleElement(cast<TupleElementExpr>(E), explosion);

  case ExprKind::DotSyntaxBaseIgnored: {
    DotSyntaxBaseIgnoredExpr *DE = cast<DotSyntaxBaseIgnoredExpr>(E);
    emitIgnored(DE->getLHS());
    return emitRValue(DE->getRHS(), explosion);
  }

  case ExprKind::Call:
  case ExprKind::Unary:
  case ExprKind::Binary:
  case ExprKind::ConstructorCall:
  case ExprKind::DotSyntaxCall:
    return emitApplyExpr(cast<ApplyExpr>(E), explosion);

  case ExprKind::IntegerLiteral:
    return explosion.add(emitIntegerLiteralExpr(*this,
                                                cast<IntegerLiteralExpr>(E)));
  case ExprKind::FloatLiteral:
    return explosion.add(emitFloatLiteralExpr(*this,
                                              cast<FloatLiteralExpr>(E)));

  case ExprKind::LookThroughOneof:
    return emitRValue(cast<LookThroughOneofExpr>(E)->getSubExpr(),
                      explosion);

  case ExprKind::DeclRef:
    return emitDeclRef(cast<DeclRefExpr>(E), explosion);

  case ExprKind::ImplicitClosure:
  case ExprKind::ExplicitClosure:
    return emitClosure(cast<ClosureExpr>(E), explosion);

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
  case ExprKind::DotSyntaxBaseIgnored:
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

  // Qualification never affects emission as an l-value.
  case ExprKind::Requalify:
    return emitLValue(cast<RequalifyExpr>(E)->getSubExpr());

  case ExprKind::Materialize: {
    OwnedAddress addr = emitMaterializeExpr(*this, cast<MaterializeExpr>(E));
    return emitAddressLValue(addr);
  }

  case ExprKind::DeclRef:
    return emitDeclRefLValue(*this, cast<DeclRefExpr>(E));

  case ExprKind::AnonClosureArg: {
    OwnedAddress addr = getLocal(cast<AnonClosureArgExpr>(E)->getDecl());
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

  // We can locate a requalified l-value if we can locate the original.
  case ExprKind::Requalify:
    return tryEmitAsAddress(cast<RequalifyExpr>(E)->getSubExpr(), type);

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
    return getLocal(cast<AnonClosureArgExpr>(E)->getDecl());

  // These expressions aren't naturally already in memory.
  case ExprKind::Tuple:
  case ExprKind::IntegerLiteral:
  case ExprKind::FloatLiteral:
  case ExprKind::TupleShuffle:
  case ExprKind::Func:
  case ExprKind::ExplicitClosure:
  case ExprKind::ImplicitClosure:
  case ExprKind::DotSyntaxBaseIgnored:
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
  emitRValue(E, explosion);
  type.initialize(*this, explosion, addr);
}

/// Zero-initialize the given memory location.
void IRGenFunction::emitZeroInit(Address addr, const TypeInfo &type) {
  ExplosionSchema schema(ExplosionKind::Maximal);
  type.getSchema(schema);

  // Try to fill the value in with stores if that doesn't make for a
  // ridiculous amount of IR.  This is impossible if the schema
  // contains an aggregate;  otherwise, 4 is just a number.
  if (!schema.containsAggregate() && schema.size() <= 4) {
    Explosion explosion(schema.getKind());
    for (auto elt : schema) {
      explosion.add(llvm::Constant::getNullValue(elt.getScalarType()));
    }
    type.initialize(*this, explosion, addr);
    return;
  }

  // Otherwise, just do a memset.
  Builder.CreateMemSet(Builder.CreateBitCast(addr.getAddress(), IGM.Int8PtrTy),
                       Builder.getInt8(0),
                       Builder.getInt64(type.StorageSize.getValue()),
                       addr.getAlignment().getValue(),
                       /*volatile*/ false);
}

namespace {
  struct IgnoredExprEmitter : ASTVisitor<IgnoredExprEmitter> {
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
    void visitAnonClosureArgExpr(AnonClosureArgExpr *E) {}
    void visitModuleExpr(ModuleExpr *E) {}

#define USING_SUBEXPR(Id) \
    void visit##Id##Expr(Id##Expr *E) { \
      return visit(E->getSubExpr()); \
    }
    USING_SUBEXPR(Paren)
    USING_SUBEXPR(AddressOf)
    USING_SUBEXPR(LookThroughOneof)
    USING_SUBEXPR(Requalify)
    USING_SUBEXPR(Materialize)

    void visitTupleShuffleExpr(TupleShuffleExpr *E) {
      // First, evaluate the base expression.
      visit(E->getSubExpr());

      // Then evaluate any defaulted elements.
      TupleType *TT = E->getType()->castTo<TupleType>();
      for (unsigned i = 0, e = TT->getFields().size(); i != e; ++i) {
        if (E->getElementMapping()[i] == -1)
          visit(TT->getFields()[i].getInit());
      }
    }

    void visitExpr(Expr *E) {
      // If all else fails, emit it as an r-value.
      Explosion explosion(ExplosionKind::Maximal);
      IGF.emitRValue(E, explosion);
    }


  };
}

/// Emit an expression whose value is being ignored.
void IRGenFunction::emitIgnored(Expr *E) {
  IgnoredExprEmitter(*this).visit(E);
}

/// Emit a fake l-value which obeys the given specification.  This
/// should only ever be used for error recovery.
LValue IRGenFunction::emitFakeLValue(const TypeInfo &type) {
  llvm::Value *fakeAddr =
    llvm::UndefValue::get(type.getStorageType()->getPointerTo());
  return emitAddressLValue(OwnedAddress(Address(fakeAddr, type.StorageAlignment),
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

    explosion.add(llvm::UndefValue::get(elementType));
  }
}
