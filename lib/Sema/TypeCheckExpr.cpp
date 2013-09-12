//===--- TypeCheckExpr.cpp - Type Checking for Expressions ----------------===//
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
// This file implements semantic analysis for expressions, analysing an
// expression tree in post-order, bottom-up, from leaves up to the root.
//
//===----------------------------------------------------------------------===//

#include "TypeChecker.h"
#include "swift/AST/NameLookup.h"
#include "swift/AST/ASTVisitor.h"
#include "swift/AST/Attr.h"
#include "swift/AST/ASTWalker.h"
#include "llvm/ADT/APInt.h"
#include "llvm/ADT/PointerUnion.h"
#include "llvm/Support/SaveAndRestore.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/STLExtras.h"
using namespace swift;

//===----------------------------------------------------------------------===//
// Expression Semantic Analysis Routines
//===----------------------------------------------------------------------===//

void substituteInputSugarArgumentType(Type argTy,
                                      CanType resultTy,
                                      Type &resultSugarTy,
                                      bool &uniqueSugarTy) {
  // If we already failed finding a unique sugar, bail out.
  if (!uniqueSugarTy)
    return;
    
  if (TupleType *argTupleTy = argTy->getAs<TupleType>()) {
    // Recursively walk tuple arguments.
    for (auto &field : argTupleTy->getFields()) {
      substituteInputSugarArgumentType(field.getType(), resultTy,
                                       resultSugarTy, uniqueSugarTy);
      if (!uniqueSugarTy)
        return;
    }
  } else {
    if (argTy->getCanonicalType() == resultTy) {
      if (resultSugarTy) {
        // Make sure this argument's sugar is consistent with the sugar we
        // already found.
        if (argTy->isSpelledLike(resultSugarTy))
          return;
        uniqueSugarTy = false;
        return;
      } else {
        resultSugarTy = argTy;
      }
    }
  }
}

/// If the inputs to an apply expression use a consistent "sugar" type
/// (that is, a typealias or shorthand syntax) equivalent to the result type of
/// the function, set the result type of the expression to that sugar type.
Expr *TypeChecker::substituteInputSugarTypeForResult(ApplyExpr *E) {
  if (!E->getType() || E->getType()->is<ErrorType>())
    return E;
  
  Type argTy = E->getArg()->getType();
  
  CanType resultTy = E->getFn()->getType()->castTo<FunctionType>()->getResult()
    ->getCanonicalType();
  
  Type resultSugarTy; // null if no sugar found, set when sugar found
  bool uniqueSugarTy = true; // true if a unique sugar mapping found
  
  substituteInputSugarArgumentType(argTy, resultTy,
                                   resultSugarTy, uniqueSugarTy);
  
  if (resultSugarTy && uniqueSugarTy)
    E->setType(resultSugarTy);

  return E;
}

/// Is the given expression a valid thing to use as the injection
/// function from the data for a newly-allocated array into the
/// given slice type?
Expr *TypeChecker::buildArrayInjectionFnRef(DeclContext *dc,
                                            ArraySliceType *sliceType,
                                            Type lenTy, SourceLoc Loc) {
  // Build the expression "Slice<T>".
  Expr *sliceTypeRef =
    new (Context) MetatypeExpr(nullptr, Loc,
                               MetaTypeType::get(sliceType, Context));

  // Build the expression "Slice<T>.convertFromHeapArray".
  Expr *injectionFn = new (Context) UnresolvedDotExpr(
                                      sliceTypeRef, Loc,
                                      Context.getIdentifier("convertFromHeapArray"),
                                      Loc);
  if (typeCheckExpressionShallow(injectionFn, dc))
    return nullptr;

  // The input is a tuple type:
  TupleTypeElt argTypes[3] = {
    // The first element is Builtin.RawPointer.
    // FIXME: this should probably be either UnsafePointer<T> or the
    // first two arguments should be combined into a byref(heap).
    Context.TheRawPointerType,

    // The second element is the owner pointer, Builtin.ObjectPointer.
    Context.TheObjectPointerType,

    // The third element is the bound type.  Maybe this should be a
    // target-specific size_t type?
    lenTy
  };

  Type input = TupleType::get(argTypes, Context);

  // The result is just the slice type.
  Type result = sliceType;

  FunctionType *fnTy = FunctionType::get(input, result, Context);

  // FIXME: this produces terrible diagnostics.
  if (convertToType(injectionFn, fnTy, dc))
    return nullptr;

  return injectionFn;
}

/// getInfixData - If the specified expression is an infix binary
/// operator, return its infix operator attributes.
static InfixData getInfixData(TypeChecker &TC, Expr *E) {
  if (auto *ifExpr = dyn_cast<IfExpr>(E)) {
    // Ternary has fixed precedence.
    assert(!ifExpr->isFolded() && "already folded if expr in sequence?!");
    (void)ifExpr;
    return InfixData(100, Associativity::Right);
  } else if (auto *assign = dyn_cast<AssignExpr>(E)) {
    // Assignment has fixed precedence.
    assert(!assign->isFolded() && "already folded assign expr in sequence?!");
    (void)assign;
    return InfixData(90, Associativity::Right);
  } else if (auto *as = dyn_cast<ExplicitCastExpr>(E)) {
    // 'as' and 'is' casts have fixed precedence.
    assert(!as->isFolded() && "already folded 'as' expr in sequence?!");
    (void)as;
    return InfixData(95, Associativity::None);
  } else if (DeclRefExpr *DRE = dyn_cast<DeclRefExpr>(E)) {
    if (Optional<InfixOperatorDecl*> maybeOp
      = TC.TU.lookupInfixOperator(DRE->getDecl()->getName(), E->getLoc())) {
      if (auto *op = *maybeOp)
        return op->getInfixData();
      TC.diagnose(DRE->getLoc(), diag::unknown_binop);
    }
  } else if (OverloadedDeclRefExpr *OO = dyn_cast<OverloadedDeclRefExpr>(E)) {
    Identifier name = OO->getDecls()[0]->getName();
    if (Optional<InfixOperatorDecl*> maybeOp
        = TC.TU.lookupInfixOperator(name, E->getLoc())) {
      if (auto *op = *maybeOp)
        return op->getInfixData();
      TC.diagnose(OO->getLoc(), diag::unknown_binop);
    }
  // Otherwise, complain.
  } else {
    TC.diagnose(E->getLoc(), diag::unknown_binop);
  }
  
  // Recover with an infinite-precedence left-associative operator.
  return InfixData((unsigned char)~0U, Associativity::Left);
}

static Expr *makeBinOp(TypeChecker &TC, Expr *Op, Expr *LHS, Expr *RHS) {
  if (!LHS || !RHS)
    return nullptr;
  
  if (auto *ifExpr = dyn_cast<IfExpr>(Op)) {
    // Resolve the ternary expression.
    assert(!ifExpr->isFolded() && "already folded if expr in sequence?!");
    ifExpr->setCondExpr(LHS);
    ifExpr->setElseExpr(RHS);
    return ifExpr;
  }

  if (auto *assign = dyn_cast<AssignExpr>(Op)) {
    // Resolve the assignment expression.
    assert(!assign->isFolded() && "already folded assign expr in sequence?!");
    assign->setDest(LHS);
    assign->setSrc(RHS);
    return assign;
  }
  
  if (auto *as = dyn_cast<ExplicitCastExpr>(Op)) {
    // Resolve the 'as' or 'is' expression.
    assert(!as->isFolded() && "already folded 'as' expr in sequence?!");
    assert(RHS == as && "'as' with non-type RHS?!");
    as->setSubExpr(LHS);
    return as;
  }
  
  // Build the argument to the operation.
  Expr *ArgElts[] = { LHS, RHS };
  auto ArgElts2 = TC.Context.AllocateCopy(MutableArrayRef<Expr*>(ArgElts));
  TupleExpr *Arg = new (TC.Context) TupleExpr(SourceLoc(), 
                                              ArgElts2, 0, SourceLoc(),
                                              /*hasTrailingClosure=*/false);

  // Build the operation.
  return new (TC.Context) BinaryExpr(Op, Arg);
}

/// foldSequence - Take a sequence of expressions and fold a prefix of
/// it into a tree of BinaryExprs using precedence parsing.
static Expr *foldSequence(TypeChecker &TC,
                          Expr *LHS,
                          ArrayRef<Expr*> &S,
                          unsigned MinPrecedence) {
  // Invariant: S is even-sized.
  // Invariant: All elements at even indices are operator references.
  assert(!S.empty());
  assert((S.size() & 1) == 0);
  
  struct Op {
    Expr *op;
    InfixData infixData;
    
    explicit operator bool() const { return op; }
  };
  
  /// Get the operator, if appropriate to this pass.
  auto getNextOperator = [&]() -> Op {
    Expr *op = S[0];

    // If the operator's precedence is lower than the minimum, stop here.
    InfixData opInfo = getInfixData(TC, op);
    if (opInfo.getPrecedence() < MinPrecedence) return {nullptr, {}};
    return {op, opInfo};
  };

  // Extract out the first operator.
  Op Op1 = getNextOperator();
  if (!Op1) return LHS;
  
  // We will definitely be consuming at least one operator.
  // Pull out the prospective RHS and slice off the first two elements.
  Expr *RHS = S[1];
  S = S.slice(2);

  while (!S.empty()) {
    assert(!S.empty());
    assert((S.size() & 1) == 0);
    assert(Op1.infixData.getPrecedence() >= MinPrecedence);

    // Pull out the next binary operator.
    Expr *Op2 = S[0];
  
    InfixData Op2Info = getInfixData(TC, Op2);
    // If the second operator's precedence is lower than the min
    // precedence, break out of the loop.
    if (Op2Info.getPrecedence() < MinPrecedence) break;
    
    // If the first operator's precedence is higher than the second
    // operator's precedence, or they have matching precedence and are
    // both left-associative, fold LHS and RHS immediately.
    if (Op1.infixData.getPrecedence() > Op2Info.getPrecedence() ||
        (Op1.infixData == Op2Info && Op1.infixData.isLeftAssociative())) {
      LHS = makeBinOp(TC, Op1.op, LHS, RHS);
      Op1 = getNextOperator();
      assert(Op1 && "should get a valid operator here");
      RHS = S[1];
      S = S.slice(2);
      continue;
    }

    // If the first operator's precedence is lower than the second
    // operator's precedence, recursively fold all such
    // higher-precedence operators starting from this point, then
    // repeat.
    if (Op1.infixData.getPrecedence() < Op2Info.getPrecedence()) {
      RHS = foldSequence(TC, RHS, S, Op1.infixData.getPrecedence() + 1);
      continue;
    }

    // If the first operator's precedence is the same as the second
    // operator's precedence, and they're both right-associative,
    // recursively fold operators starting from this point, then
    // immediately fold LHS and RHS.
    if (Op1.infixData == Op2Info && Op1.infixData.isRightAssociative()) {
      RHS = foldSequence(TC, RHS, S, Op1.infixData.getPrecedence());
      LHS = makeBinOp(TC, Op1.op, LHS, RHS);

      // If we've drained the entire sequence, we're done.
      if (S.empty()) return LHS;

      // Otherwise, start all over with our new LHS.
      return foldSequence(TC, LHS, S, MinPrecedence);
    }

    // If we ended up here, it's because we have two operators
    // with mismatched or no associativity.
    assert(Op1.infixData.getPrecedence() == Op2Info.getPrecedence());
    assert(Op1.infixData.getAssociativity() != Op2Info.getAssociativity()
           || Op1.infixData.isNonAssociative());

    if (Op1.infixData.isNonAssociative()) {
      // FIXME: QoI ranges
      TC.diagnose(Op1.op->getLoc(), diag::non_assoc_adjacent);
    } else if (Op2Info.isNonAssociative()) {
      TC.diagnose(Op2->getLoc(), diag::non_assoc_adjacent);
    } else {
      TC.diagnose(Op1.op->getLoc(), diag::incompatible_assoc);
    }
    
    // Recover by arbitrarily binding the first two.
    LHS = makeBinOp(TC, Op1.op, LHS, RHS);
    return foldSequence(TC, LHS, S, MinPrecedence);
  }

  // Fold LHS and RHS together and declare completion.
  return makeBinOp(TC, Op1.op, LHS, RHS);
}

Type TypeChecker::getTypeOfRValue(ValueDecl *value) {
  Type type = value->getType();
  if (!value->isReferencedAsLValue())
    return type;

  // Look at the canonical type just for efficiency.  We won't
  // use this as the source of the result.
  CanType canType = type->getCanonicalType();

  // Strip l-value-ness.
  if (isa<LValueType>(canType)) {
    return type->castTo<LValueType>()->getObjectType();

  // Turn [weak] T into Optional<T>.
  } else if (isa<WeakStorageType>(canType)) {
    // On the one hand, we should probably use a better location than
    // the declaration's.  On the other hand, all these diagnostics
    // are "broken library" errors, so it should really never matter.

    auto refTy = type->castTo<ReferenceStorageType>()->getReferentType();
    auto optTy = getOptionalType(value->getLoc(), refTy);

    // If we can't create Optional<T>, use T instead of returning null.
    if (!optTy) return refTy;

    // Check that we can do intrinsic operations on Optional<T> before
    // returning.
    if (!Context.hasOptionalIntrinsics()) {
      diagnose(value->getLoc(), diag::optional_intrinsics_not_found);
    }

    return refTy; // FIXME: optTy

  // Ignore [unowned] qualification.
  } else if (isa<UnownedStorageType>(canType)) {
    return type->castTo<UnownedStorageType>()->getReferentType();

  // No other transforms necessary.
  } else {
    return type;
  }
}

Type TypeChecker::getUnopenedTypeOfReference(ValueDecl *value, Type baseType) {
  if (value->isReferencedAsLValue()) {
    // Determine the qualifiers we want.
    LValueType::Qual quals =
      (baseType ? LValueType::Qual::DefaultForMemberAccess
                : LValueType::Qual::DefaultForVar);
    if (!value->isSettableOnBase(baseType))
      quals |= LValueType::Qual::NonSettable;

    return LValueType::get(getTypeOfRValue(value), quals, Context);
  }

  return value->getType();
}

Expr *TypeChecker::buildCheckedRefExpr(ValueDecl *value, SourceLoc loc) {
  auto type = getUnopenedTypeOfReference(value);
  return new (Context) DeclRefExpr(value, loc, type);
}

Expr *TypeChecker::buildRefExpr(ArrayRef<ValueDecl *> Decls, SourceLoc NameLoc,
                                bool isSpecialized) {
  assert(!Decls.empty() && "Must have at least one declaration");

  if (Decls.size() == 1 && !isa<ProtocolDecl>(Decls[0]->getDeclContext())) {
    auto result = new (Context) DeclRefExpr(Decls[0], NameLoc);
    result->setSpecialized(isSpecialized);
    return result;
  }

  Decls = Context.AllocateCopy(Decls);
  auto result = new (Context) OverloadedDeclRefExpr(Decls, NameLoc);
  result->setSpecialized(isSpecialized);
  return result;
}

static Type lookupGlobalType(TypeChecker &TC, StringRef name) {
  UnqualifiedLookup lookup(TC.Context.getIdentifier(name), &TC.TU);
  TypeDecl *TD = lookup.getSingleTypeResult();
  if (!TD)
    return Type();
  return TD->getDeclaredType();
}


Type TypeChecker::getDefaultType(ProtocolDecl *protocol) {
  Type *type = nullptr;
  const char *name = nullptr;

  // CharacterLiteralConvertible -> CharacterLiteralType
  if (protocol == getProtocol(SourceLoc(),
                              KnownProtocolKind::CharacterLiteralConvertible)) {
    type = &CharacterLiteralType;
    name = "CharacterLiteralType";
  }
  // StringLiteralConvertible -> StringLiteralType
  // StringInterpolationConvertible -> StringLiteralType
  else if (protocol == getProtocol(
                         SourceLoc(),
                         KnownProtocolKind::StringLiteralConvertible) ||
           protocol == getProtocol(
                         SourceLoc(),
                         KnownProtocolKind::StringInterpolationConvertible)) {
    type = &StringLiteralType;
    name = "StringLiteralType";
  }
  // IntegerLiteralConvertible -> IntegerLiteralType
  else if (protocol == getProtocol(
                         SourceLoc(),
                         KnownProtocolKind::IntegerLiteralConvertible)) {
    type = &IntLiteralType;
    name = "IntegerLiteralType";
  }
  // FloatLiteralConvertible -> FloatLiteralType
  else if (protocol == getProtocol(SourceLoc(),
                                   KnownProtocolKind::FloatLiteralConvertible)){
    type = &FloatLiteralType;
    name = "FloatLiteralType";
  }
  // ArrayLiteralConvertible -> Slice
  else if (protocol == getProtocol(SourceLoc(),
                                   KnownProtocolKind::ArrayLiteralConvertible)){
    type = &ArrayLiteralType;
    name = "Slice";
  }
  // DictionaryLiteralConvertible -> Dictionary
  else if (protocol == getProtocol(
                         SourceLoc(),
                         KnownProtocolKind::DictionaryLiteralConvertible)) {
    type = &DictionaryLiteralType;
    name = "Dictionary";
  }

  if (!type)
    return nullptr;

  // If we haven't found the type yet, look for it now.
  if (!*type) {
    *type = lookupGlobalType(*this, name);

    // Strip off one level of sugar; we don't actually want to print
    // the name of the typealias itself anywhere.
    if (type && *type) {
      if (auto typeAlias = dyn_cast<NameAliasType>(type->getPointer()))
        *type = typeAlias->getDecl()->getUnderlyingType();
    }
  }

  return *type;
}

Expr *TypeChecker::foldSequence(SequenceExpr *expr) {
  ArrayRef<Expr*> Elts = expr->getElements();
  assert(Elts.size() > 1 && "inadequate number of elements in sequence");
  assert((Elts.size() & 1) == 1 && "even number of elements in sequence");

  Expr *LHS = Elts[0];
  Elts = Elts.slice(1);

  Expr *Result = ::foldSequence(*this, LHS, Elts, /*min precedence*/ 0);
  assert(Elts.empty());
  return Result;
}

namespace {
  class FindCapturedVars : public ASTWalker {
    TypeChecker &tc;
    llvm::SetVector<ValueDecl*> &captures;
    DeclContext *CurExprAsDC;

  public:
    FindCapturedVars(TypeChecker &tc, llvm::SetVector<ValueDecl*> &captures,
                     CapturingExpr *curExpr)
        : tc(tc), captures(captures) {
      if (auto *FE = dyn_cast<FuncExpr>(curExpr))
        CurExprAsDC = FE;
      else if (auto *CE = dyn_cast<PipeClosureExpr>(curExpr))
        CurExprAsDC = CE;
      else
        CurExprAsDC = cast<ClosureExpr>(curExpr);
    }

    void doWalk(Expr *E) {
      E->walk(*this);
    }
    void doWalk(Stmt *S) {
      S->walk(*this);
    }

    std::pair<bool, Expr *> walkToExprPre(Expr *E) override {
      if (DeclRefExpr *DRE = dyn_cast<DeclRefExpr>(E))
        return walkToDeclRefExpr(DRE);

      // Don't recurse into child closures, they should already have a capture
      // list computed, we just propagate it (filtering out stuff that they
      // capture from us).
      if (CapturingExpr *SubCE = dyn_cast<CapturingExpr>(E)) {
        for (auto D : SubCE->getCaptures())
          if (D->getDeclContext() != CurExprAsDC)
            captures.insert(D);
        return { false, E };
      }
      return { true, E };
    }

    std::pair<bool, Expr *> walkToDeclRefExpr(DeclRefExpr *DRE) {
      auto *D = DRE->getDecl();

      // Decl references that are within the Capture are local references, ones
      // from parent context are captures.
      if (!CurExprAsDC->isChildContextOf(D->getDeclContext()))
        return { false, DRE };

      // Only capture var decls at global scope.  Other things can be captured
      // if they are local.
      if (!isa<VarDecl>(D) &&
          !DRE->getDecl()->getDeclContext()->isLocalContext())
        return { false, DRE };


      // A [byref] parameter cannot be captured.
      // FIXME: As a temporary hack, ignore 'self', which is an implicit
      // [byref] parameter for instance methods of structs.
      if (D->getType()->is<LValueType>() &&
          !D->getName().str().equals("self"))
        tc.diagnose(DRE->getLoc(), diag::byref_capture, D->getName());

      captures.insert(D);
      return { false, DRE };
    }

  };
}

void TypeChecker::computeCaptures(CapturingExpr *capturing) {
  llvm::SetVector<ValueDecl*> Captures;
  FindCapturedVars finder(*this, Captures, capturing);
  if (auto closure = dyn_cast<ClosureExpr>(capturing))
    finder.doWalk(closure->getBody());
  else if (auto closure = dyn_cast<PipeClosureExpr>(capturing))
    finder.doWalk(closure->getBody());
  else {
    auto func = cast<FuncExpr>(capturing);
    finder.doWalk(func->getDecl()->getBody());
  }
  ValueDecl** CaptureCopy
    = Context.AllocateCopy<ValueDecl*>(Captures.begin(), Captures.end());
  capturing->setCaptures(llvm::makeArrayRef(CaptureCopy, Captures.size()));
}

