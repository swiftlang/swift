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
#include "swift/Basic/Optional.h"
#include "llvm/DerivedTypes.h"
#include "llvm/Function.h"
#include "llvm/Target/TargetData.h"

#include "ASTVisitor.h"
#include "GenClosure.h"
#include "GenFunc.h"
#include "GenLValue.h"
#include "GenOneOf.h"
#include "GenTuple.h"
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
static void emitDeclRef(IRGenFunction &IGF, DeclRefExpr *E,
                        Explosion &explosion) {
  ValueDecl *D = E->getDecl();
  switch (D->getKind()) {
  case DeclKind::Extension:
  case DeclKind::Import:
  case DeclKind::TypeAlias:
    llvm_unreachable("decl is not a value decl");

  case DeclKind::Var:
    return IGF.emitLValueAsScalar(emitDeclRefLValue(IGF, E),
                                  OnHeap, explosion);

  case DeclKind::Func:
    return emitRValueForFunction(IGF, cast<FuncDecl>(D), explosion);

  case DeclKind::OneOfElement:
    return emitOneOfElementRef(IGF, cast<OneOfElementDecl>(D), explosion);

  case DeclKind::ElementRef:
    IGF.unimplemented(E->getLoc(), "emitting this decl as an r-value");
    return IGF.emitFakeExplosion(IGF.getFragileTypeInfo(E->getType()),
                                 explosion);
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
      Out.add(addr.getAddressPointer());
    }

    void visitRequalifyExpr(RequalifyExpr *E) {
      emitRequalify(IGF, E, Out);
    }

    void visitTupleExpr(TupleExpr *E) {
      emitTupleLiteral(IGF, E, Out);
    }

    void visitTupleShuffleExpr(TupleShuffleExpr *E) {
      emitTupleShuffle(IGF, E, Out);
    }

    void visitTupleElementExpr(TupleElementExpr *E) {
      emitTupleElement(IGF, E, Out);
    }

    void visitDotSyntaxBaseIgnoredExpr(DotSyntaxBaseIgnoredExpr *E) {
      IGF.emitIgnored(E->getLHS());
      IGF.emitRValue(E->getRHS(), Out);
    }

    void visitApplyExpr(ApplyExpr *E) {
      emitApplyExpr(IGF, E, Out);
    }

    void visitIntegerLiteralExpr(IntegerLiteralExpr *E) {
      Out.add(emitIntegerLiteralExpr(IGF, E));
    }

    void visitFloatLiteralExpr(FloatLiteralExpr *E) {
      Out.add(emitFloatLiteralExpr(IGF, E));
    }

    void visitLookThroughOneofExpr(LookThroughOneofExpr *E) {
      emitLookThroughOneof(IGF, E, Out);
    }

    void visitDeclRefExpr(DeclRefExpr *E) {
      emitDeclRef(IGF, E, Out);
    }

    void visitClosureExpr(ClosureExpr *E) {
      emitClosure(IGF, E, Out);
    }

    void visitFuncExpr(FuncExpr *E) {
      IGF.IGM.unimplemented(E->getLoc(),
                            "cannot explode r-values for this expression yet");
      IGF.emitFakeExplosion(IGF.getFragileTypeInfo(E->getType()), Out);
    }

    void visitModuleExpr(ModuleExpr *E) {
      // Nothing to do: modules have no runtime representation.
    }
  };
}

void IRGenFunction::emitRValue(Expr *E, Explosion &explosion) {
  RValueEmitter(*this, explosion).visit(E);
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
    NOT_LVALUE_EXPR(TupleShuffle)
    NOT_LVALUE_EXPR(Func)
    NOT_LVALUE_EXPR(Closure)
    NOT_LVALUE_EXPR(Load)
    NOT_LVALUE_EXPR(Tuple)
    NOT_LVALUE_EXPR(DotSyntaxBaseIgnored)
    NOT_LVALUE_EXPR(Module)
#undef NOT_LVALUE_EXPR

    LValue visitTupleElementExpr(TupleElementExpr *E) {
      return emitTupleElementLValue(IGF, E);
    }

    LValue visitLookThroughOneofExpr(LookThroughOneofExpr *E) {
      return emitLookThroughOneofLValue(IGF, E);
    }

    // Qualification never affects emission as an l-value.
    LValue visitRequalifyExpr(RequalifyExpr *E) {
      return visit(E);
    }

    LValue visitMaterializeExpr(MaterializeExpr *E) {
      OwnedAddress addr = emitMaterializeExpr(IGF, E);
      return IGF.emitAddressLValue(addr);
    }

    LValue visitDeclRefExpr(DeclRefExpr *E) {
      return emitDeclRefLValue(IGF, E);
    }
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
                                                  Optional<Address> > {
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
    NON_LOCATEABLE(ElementRefDecl)

    // These we support.
    Optional<Address> visitVarDecl(VarDecl *D) {
      // For now, only bother with locals.
      if (!D->getDeclContext()->isLocalContext())
        return Nothing;

      return IGF.getLocal(D);
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

    // We can locate a oneof payload if we can locate the oneof.
    Optional<Address> visitLookThroughOneofExpr(LookThroughOneofExpr *E) {
      return tryEmitLookThroughOneofAsAddress(IGF, E);
    }

    // We can locate a tuple element if we can locate the tuple.
    Optional<Address> visitTupleElementExpr(TupleElementExpr *E) {
      return tryEmitTupleElementAsAddress(IGF, E);
    }

    // Materializations are always in memory.
    Optional<Address> visitMaterializeExpr(MaterializeExpr *E) {
      return emitMaterializeExpr(IGF, cast<MaterializeExpr>(E));
    }

    // These expressions aren't naturally already in memory.
    NON_LOCATEABLE(TupleExpr)
    NON_LOCATEABLE(IntegerLiteralExpr)
    NON_LOCATEABLE(FloatLiteralExpr)
    NON_LOCATEABLE(TupleShuffleExpr)
    NON_LOCATEABLE(CapturingExpr)
    NON_LOCATEABLE(ModuleExpr)
    NON_LOCATEABLE(DotSyntaxBaseIgnoredExpr)

#undef NON_LOCATEABLE
  };
}

/// Try to emit the given expression as an entity with an address.
/// This is useful for local optimizations.
Optional<Address>
IRGenFunction::tryEmitAsAddress(Expr *E, const TypeInfo &type) {
  return AddressEmitter(*this, type).visit(E);
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
