//===--- TypeCheckExpr.cpp - Type Checking for Expressions ----------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
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
#include "swift/AST/Decl.h"
#include "swift/Basic/Defer.h"
#include "swift/Parse/Lexer.h"
#include "llvm/ADT/APInt.h"
#include "llvm/ADT/PointerUnion.h"
#include "llvm/Support/SaveAndRestore.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/STLExtras.h"
using namespace swift;

//===----------------------------------------------------------------------===//
// Expression Semantic Analysis Routines
//===----------------------------------------------------------------------===//

static void substituteInputSugarArgumentType(Type argTy, CanType resultTy,
                                             Type &resultSugarTy,
                                             bool &uniqueSugarTy) {
  // If we already failed finding a unique sugar, bail out.
  if (!uniqueSugarTy)
    return;
    
  if (TupleType *argTupleTy = argTy->getAs<TupleType>()) {
    // Recursively walk tuple arguments.
    for (auto &field : argTupleTy->getElements()) {
      substituteInputSugarArgumentType(field.getType(), resultTy,
                                       resultSugarTy, uniqueSugarTy);
      if (!uniqueSugarTy)
        return;
    }
    return;
  }
  
  if (argTy->getCanonicalType() != resultTy) {
    // If the argument is a metatype of what we're looking for, propagate that.
    if (auto MTT = argTy->getAs<MetatypeType>())
      argTy = MTT->getInstanceType();

    if (argTy->getCanonicalType() != resultTy)
      return;
  }

  // If this type is parenthesized, remove the parens.  We don't want to
  // propagate parens from arguments to the result type.
  if (auto *PT = dyn_cast<ParenType>(argTy.getPointer()))
    argTy = PT->getUnderlyingType();
  
  // If this is the first match against the sugar type we found, use it.
  if (!resultSugarTy) {
    resultSugarTy = argTy;
    return;
  }
  
  // Make sure this argument's sugar is consistent with the sugar we
  // already found.
  if (argTy->isSpelledLike(resultSugarTy))
    return;
  uniqueSugarTy = false;
}

/// If we can propagate type sugar from input arguments types to the result of
/// an apply, do so.
///
Expr *TypeChecker::substituteInputSugarTypeForResult(ApplyExpr *E) {
  if (!E->getType() || E->getType()->is<ErrorType>())
    return E;
  
  Type resultTy = E->getFn()->getType()->castTo<FunctionType>()->getResult();

  /// Check to see if you have "x+y" (where x and y are type aliases) that match
  // the canonical result type.  If so, propagate the sugar.
  Type resultSugarTy; // null if no sugar found, set when sugar found
  bool uniqueSugarTy = true; // true if a unique sugar mapping found
  substituteInputSugarArgumentType(E->getArg()->getType(),
                                   resultTy->getCanonicalType(),
                                   resultSugarTy, uniqueSugarTy);
  
  if (resultSugarTy && uniqueSugarTy && E->getType()->isCanonical()) {
    E->setType(resultSugarTy);
    return E;
  }

  // Otherwise check to see if this is a ConstructorRefExpr on a TypeExpr with
  // sugar on it.  If so, propagate the sugar to the curried result function
  // type.
  if (isa<ConstructorRefCallExpr>(E) && isa<TypeExpr>(E->getArg())) {
    auto resultSugar =
      E->getArg()->getType()->castTo<MetatypeType>()->getInstanceType();

    // The result of this apply is "(args) -> T" where T is the type being
    // constructed.  Apply the sugar onto it.
    if (auto FT = E->getType()->getAs<FunctionType>())
      if (FT->getResult()->isEqual(resultSugar) && !resultSugar->isCanonical()){
        auto NFT = FunctionType::get(FT->getInput(), resultSugar,
                                     FT->getExtInfo());
        E->setType(NFT);
        return E;
      }
  }

  // Otherwise, if the callee function had sugar on the result type, but it got
  // dropped, make sure to propagate it along.
  if (!resultTy->isCanonical() && E->getType()->isCanonical() &&
      resultTy->isEqual(E->getType())) {
    E->setType(resultTy);
    return E;
  }


  return E;
}

/// getInfixData - If the specified expression is an infix binary
/// operator, return its infix operator attributes.
static InfixData getInfixData(TypeChecker &TC, DeclContext *DC, Expr *E) {
  if (auto *ifExpr = dyn_cast<IfExpr>(E)) {
    // Ternary has fixed precedence.
    assert(!ifExpr->isFolded() && "already folded if expr in sequence?!");
    (void)ifExpr;
    return InfixData(IntrinsicPrecedences::IfExpr,
                     Associativity::Right,
                     /*assignment*/ false);

  }

  if (auto *assign = dyn_cast<AssignExpr>(E)) {
    // Assignment has fixed precedence.
    assert(!assign->isFolded() && "already folded assign expr in sequence?!");
    (void)assign;
    return InfixData(IntrinsicPrecedences::AssignExpr,
                     Associativity::Right,
                     /*assignment*/ true);

  }

  if (auto *as = dyn_cast<ExplicitCastExpr>(E)) {
    // 'as' and 'is' casts have fixed precedence.
    assert(!as->isFolded() && "already folded 'as' expr in sequence?!");
    (void)as;
    return InfixData(IntrinsicPrecedences::ExplicitCastExpr,
                     Associativity::None,
                     /*assignment*/ false);

  }

  if (DeclRefExpr *DRE = dyn_cast<DeclRefExpr>(E)) {
    SourceFile *SF = DC->getParentSourceFile();
    Identifier name = DRE->getDecl()->getName();
    bool isCascading = DC->isCascadingContextForLookup(true);
    if (InfixOperatorDecl *op = SF->lookupInfixOperator(name, isCascading,
                                                        E->getLoc()))
      return op->getInfixData();

  }

  if (OverloadedDeclRefExpr *OO = dyn_cast<OverloadedDeclRefExpr>(E)) {
    SourceFile *SF = DC->getParentSourceFile();
    Identifier name = OO->getDecls()[0]->getName();
    bool isCascading = DC->isCascadingContextForLookup(true);
    if (InfixOperatorDecl *op = SF->lookupInfixOperator(name, isCascading,
                                                        E->getLoc()))
      return op->getInfixData();
  }

  // If E is already an ErrorExpr, then we've diagnosed it as invalid already,
  // otherwise emit an error.
  if (!isa<ErrorExpr>(E))
    TC.diagnose(E->getLoc(), diag::unknown_binop);

  // Recover with an infinite-precedence left-associative operator.
  return InfixData((unsigned char)~0U, Associativity::Left,
                   /*assignment*/ false);
}

// The way we compute isEndOfSequence relies on the assumption that
// the sequence-folding algorithm never recurses with a prefix of the
// entire sequence.
static Expr *makeBinOp(TypeChecker &TC, Expr *Op, Expr *LHS, Expr *RHS,
                       const InfixData &infixData, bool isEndOfSequence) {
  if (!LHS || !RHS)
    return nullptr;

  // If the left-hand-side is a 'try', hoist it up.
  AnyTryExpr *tryEval = dyn_cast<AnyTryExpr>(LHS);
  if (tryEval) {
    LHS = tryEval->getSubExpr();
  }
  
  // If this is an assignment operator, and the left operand is an optional
  // evaluation, pull the operator into the chain.
  OptionalEvaluationExpr *optEval = nullptr;
  if (infixData.isAssignment()) {
    if ((optEval = dyn_cast<OptionalEvaluationExpr>(LHS))) {
      LHS = optEval->getSubExpr();
    }
  }

  // If the right operand is a try, it's an error unless the operator
  // is an assignment or conditional operator and there's nothing to
  // the right that didn't parse as part of the right operand.
  //
  // Generally, nothing to the right will fail to parse as part of the
  // right operand because there are no standard operators that have
  // lower precedence than assignment operators or the conditional
  // operator.
  //
  // We allow the right operand of the conditional operator to begin
  // with 'try' for consistency with the middle operand.  This allows:
  //   x ? try foo() : try bar()
  // but not:
  //   x ? try foo() : try bar() $#! 1
  // assuming $#! is some crazy operator with lower precedence
  // than the conditional operator.
  if (isa<AnyTryExpr>(RHS)) {
    // If you change this, also change TRY_KIND_SELECT in diagnostics.
    enum class TryKindForDiagnostics : unsigned {
      Try,
      ForceTry,
      OptionalTry
    };
    TryKindForDiagnostics tryKind;
    switch (RHS->getKind()) {
    case ExprKind::Try:
      tryKind = TryKindForDiagnostics::Try;
      break;
    case ExprKind::ForceTry:
      tryKind = TryKindForDiagnostics::ForceTry;
      break;
    case ExprKind::OptionalTry:
      tryKind = TryKindForDiagnostics::OptionalTry;
      break;
    default:
      llvm_unreachable("unknown try-like expression");
    }

    if (isa<IfExpr>(Op) || infixData.isAssignment()) {
      if (!isEndOfSequence) {
        if (isa<IfExpr>(Op)) {
          TC.diagnose(RHS->getStartLoc(), diag::try_if_rhs_noncovering,
                      static_cast<unsigned>(tryKind));
        } else {
          TC.diagnose(RHS->getStartLoc(), diag::try_assign_rhs_noncovering,
                      static_cast<unsigned>(tryKind));
        }
      }
    } else {
      TC.diagnose(RHS->getStartLoc(), diag::try_rhs,
                  static_cast<unsigned>(tryKind));
    }
  }

  // Fold the result into the optional evaluation or try.
  auto makeResultExpr = [&](Expr *result) -> Expr * {
    if (optEval) {
      optEval->setSubExpr(result);
      result = optEval;
    }
    if (tryEval) {
      tryEval->setSubExpr(result);
      result = tryEval;
    }
    return result;
  };
  
  if (auto *ifExpr = dyn_cast<IfExpr>(Op)) {
    // Resolve the ternary expression.
    assert(!ifExpr->isFolded() && "already folded if expr in sequence?!");
    ifExpr->setCondExpr(LHS);
    ifExpr->setElseExpr(RHS);
    return makeResultExpr(ifExpr);
  }

  if (auto *assign = dyn_cast<AssignExpr>(Op)) {
    // Resolve the assignment expression.
    assert(!assign->isFolded() && "already folded assign expr in sequence?!");
    assign->setDest(LHS);
    assign->setSrc(RHS);
    return makeResultExpr(assign);
  }
  
  if (auto *as = dyn_cast<ExplicitCastExpr>(Op)) {
    // Resolve the 'as' or 'is' expression.
    assert(!as->isFolded() && "already folded 'as' expr in sequence?!");
    assert(RHS == as && "'as' with non-type RHS?!");
    as->setSubExpr(LHS);    
    return makeResultExpr(as);
  }
  
  // Build the argument to the operation.
  Expr *ArgElts[] = { LHS, RHS };
  auto ArgElts2 = TC.Context.AllocateCopy(MutableArrayRef<Expr*>(ArgElts));
  TupleExpr *Arg = TupleExpr::create(TC.Context,
                                     SourceLoc(), 
                                     ArgElts2, { }, { }, SourceLoc(),
                                     /*hasTrailingClosure=*/false,
                                     LHS->isImplicit() && RHS->isImplicit());

  
  
  // Build the operation.
  return makeResultExpr(new (TC.Context) BinaryExpr(Op, Arg, Op->isImplicit()));
}

/// foldSequence - Take a sequence of expressions and fold a prefix of
/// it into a tree of BinaryExprs using precedence parsing.
static Expr *foldSequence(TypeChecker &TC, DeclContext *DC,
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
    InfixData opInfo = getInfixData(TC, DC, op);
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
    assert((S.size() & 1) == 0);
    assert(Op1.infixData.isValid() && "Not a valid operator to fold");
    assert(Op1.infixData.getPrecedence() >= MinPrecedence);

    // If the operator is a cast operator, the RHS can't extend past the type
    // that's part of the cast production.
    if (isa<ExplicitCastExpr>(Op1.op)) {
      LHS = makeBinOp(TC, Op1.op, LHS, RHS, Op1.infixData, S.empty());
      Op1 = getNextOperator();
      if (!Op1) return LHS;
      RHS = S[1];
      S = S.slice(2);
      continue;
    }
    
    // Pull out the next binary operator.
    Expr *Op2 = S[0];
  
    InfixData Op2Info = getInfixData(TC, DC, Op2);
    // If the second operator's precedence is lower than the min
    // precedence, break out of the loop.
    if (Op2Info.getPrecedence() < MinPrecedence) break;
    
    // If the first operator's precedence is higher than the second
    // operator's precedence, or they have matching precedence and are
    // both left-associative, fold LHS and RHS immediately.
    if (Op1.infixData.getPrecedence() > Op2Info.getPrecedence() ||
        (Op1.infixData == Op2Info && Op1.infixData.isLeftAssociative())) {
      LHS = makeBinOp(TC, Op1.op, LHS, RHS, Op1.infixData, S.empty());
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
      RHS = foldSequence(TC, DC, RHS, S, Op1.infixData.getPrecedence() + 1);
      continue;
    }

    // If the first operator's precedence is the same as the second
    // operator's precedence, and they're both right-associative,
    // recursively fold operators starting from this point, then
    // immediately fold LHS and RHS.
    if (Op1.infixData == Op2Info && Op1.infixData.isRightAssociative()) {
      RHS = foldSequence(TC, DC, RHS, S, Op1.infixData.getPrecedence());
      LHS = makeBinOp(TC, Op1.op, LHS, RHS, Op1.infixData, S.empty());

      // If we've drained the entire sequence, we're done.
      if (S.empty()) return LHS;

      // Otherwise, start all over with our new LHS.
      return foldSequence(TC, DC, LHS, S, MinPrecedence);
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
    LHS = makeBinOp(TC, Op1.op, LHS, RHS, Op1.infixData, S.empty());
    return foldSequence(TC, DC, LHS, S, MinPrecedence);
  }

  // Fold LHS and RHS together and declare completion.
  return makeBinOp(TC, Op1.op, LHS, RHS, Op1.infixData, S.empty());
}

Type TypeChecker::getTypeOfRValue(ValueDecl *value, bool wantInterfaceType) {
  validateDecl(value);

  Type type;
  if (wantInterfaceType)
    type = value->getInterfaceType();
  else
    type = value->getType();

  // Uses of inout argument values are lvalues.
  if (auto iot = type->getAs<InOutType>())
    return iot->getObjectType();
  
  // Uses of values with lvalue type produce their rvalue.
  if (auto LV = type->getAs<LValueType>())
    return LV->getObjectType();

  // Ignore 'unowned', 'weak' and @unmanaged qualification.
  if (type->is<ReferenceStorageType>())
    return type->getReferenceStorageReferent();

  // No other transforms necessary.
  return type;
}

bool TypeChecker::requireOptionalIntrinsics(SourceLoc loc) {
  if (Context.hasOptionalIntrinsics(this)) return false;

  diagnose(loc, diag::optional_intrinsics_not_found);
  return true;
}

bool TypeChecker::requirePointerArgumentIntrinsics(SourceLoc loc) {
  if (Context.hasPointerArgumentIntrinsics(this)) return false;

  diagnose(loc, diag::pointer_argument_intrinsics_not_found);
  return true;
}

bool TypeChecker::requireArrayLiteralIntrinsics(SourceLoc loc) {
  if (Context.hasArrayLiteralIntrinsics(this)) return false;
  
  diagnose(loc, diag::array_literal_intrinsics_not_found);
  return true;
}

/// Does a var or subscript produce an l-value?
///
/// \param baseType - the type of the base on which this object
///   is being accessed; must be null if and only if this is not
///   a type member
static bool doesStorageProduceLValue(TypeChecker &TC,
                                     AbstractStorageDecl *storage,
                                     Type baseType, DeclContext *useDC,
                                     const DeclRefExpr *base = nullptr) {
  // Unsettable storage decls always produce rvalues.
  if (!storage->isSettable(useDC, base))
    return false;
  
  if (TC.Context.LangOpts.EnableAccessControl &&
      !storage->isSetterAccessibleFrom(useDC))
    return false;

  // If there is no base, or if the base isn't being used, it is settable.
  // This is only possible for vars.
  if (auto var = dyn_cast<VarDecl>(storage)) {
    if (!baseType || var->isStatic())
      return true;
  }

  // If the base is an lvalue, then a reference produces an lvalue.
  if (baseType->is<LValueType>())
    return true;

  // Stored properties of reference types produce lvalues.
  if (baseType->hasReferenceSemantics() && storage->hasStorage())
    return true;

  // So the base is an rvalue type. The only way an accessor can
  // produce an lvalue is if we have a property where both the
  // getter and setter are nonmutating.
  return !storage->hasStorage() &&
      !storage->isGetterMutating() &&
      storage->isSetterNonMutating();
}

Type TypeChecker::getUnopenedTypeOfReference(ValueDecl *value, Type baseType,
                                             DeclContext *UseDC,
                                             const DeclRefExpr *base,
                                             bool wantInterfaceType) {
  validateDecl(value);
  if (value->isInvalid())
    return ErrorType::get(Context);

  Type requestedType = getTypeOfRValue(value, wantInterfaceType);

  // Qualify storage declarations with an lvalue when appropriate.
  // Otherwise, they yield rvalues (and the access must be a load).
  if (auto *storage = dyn_cast<AbstractStorageDecl>(value)) {
    if (doesStorageProduceLValue(*this, storage, baseType, UseDC, base)) {
      // Vars are simply lvalues of their rvalue type.
      if (isa<VarDecl>(storage))
        return LValueType::get(requestedType);

      // Subscript decls have function type.  For the purposes of later type
      // checker consumption, model this as returning an lvalue.
      assert(isa<SubscriptDecl>(storage));
      auto *RFT = requestedType->castTo<FunctionType>();
      return FunctionType::get(RFT->getInput(),
                               LValueType::get(RFT->getResult()),
                               RFT->getExtInfo());
    }
  }

  return requestedType;
}

Expr *TypeChecker::buildCheckedRefExpr(ValueDecl *value, DeclContext *UseDC,
                                       SourceLoc loc, bool Implicit) {
  auto type = getUnopenedTypeOfReference(value, Type(), UseDC);
  AccessSemantics semantics = value->getAccessSemanticsFromContext(UseDC);
  return new (Context) DeclRefExpr(value, loc, Implicit, semantics, type);
}

Expr *TypeChecker::buildRefExpr(ArrayRef<ValueDecl *> Decls,
                                DeclContext *UseDC, SourceLoc NameLoc,
                                bool Implicit, bool isSpecialized) {
  assert(!Decls.empty() && "Must have at least one declaration");

  if (Decls.size() == 1 && !isa<ProtocolDecl>(Decls[0]->getDeclContext())) {
    AccessSemantics semantics = Decls[0]->getAccessSemanticsFromContext(UseDC);
    auto result = new (Context) DeclRefExpr(Decls[0], NameLoc, Implicit,
                                            semantics);
    if (isSpecialized)
      result->setSpecialized();
    return result;
  }

  Decls = Context.AllocateCopy(Decls);
  auto result = new (Context) OverloadedDeclRefExpr(Decls, NameLoc, Implicit);
  result->setSpecialized(isSpecialized);
  return result;
}

static Type lookupDefaultLiteralType(TypeChecker &TC, DeclContext *dc,
                                     StringRef name) {
  auto lookupOptions = defaultUnqualifiedLookupOptions;
  if (isa<AbstractFunctionDecl>(dc))
    lookupOptions |= NameLookupFlags::KnownPrivate;
  auto lookup = TC.lookupUnqualified(dc->getModuleScopeContext(),
                                     TC.Context.getIdentifier(name),
                                     SourceLoc(),
                                     lookupOptions);
  TypeDecl *TD = lookup.getSingleTypeResult();
  if (!TD)
    return Type();
  TC.validateDecl(TD);
  return TD->getDeclaredType();
}

Type TypeChecker::getDefaultType(ProtocolDecl *protocol, DeclContext *dc) {
  Type *type = nullptr;
  const char *name = nullptr;

  // UnicodeScalarLiteralConvertible -> UnicodeScalarType
  if (protocol ==
           getProtocol(
               SourceLoc(),
               KnownProtocolKind::UnicodeScalarLiteralConvertible)) {
    type = &UnicodeScalarType;
    name = "UnicodeScalarType";
  }
  // ExtendedGraphemeClusterLiteralConvertible -> ExtendedGraphemeClusterType
  else if (protocol ==
           getProtocol(
               SourceLoc(),
               KnownProtocolKind::ExtendedGraphemeClusterLiteralConvertible)) {
    type = &ExtendedGraphemeClusterType;
    name = "ExtendedGraphemeClusterType";
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
  // BooleanLiteralConvertible -> BoolLiteralType
  else if (protocol == getProtocol(
                         SourceLoc(),
                         KnownProtocolKind::BooleanLiteralConvertible)){
    type = &BooleanLiteralType;
    name = "BooleanLiteralType";
  }
  // ArrayLiteralConvertible -> Array
  else if (protocol == getProtocol(SourceLoc(),
                                   KnownProtocolKind::ArrayLiteralConvertible)){
    type = &ArrayLiteralType;
    name = "Array";
  }
  // DictionaryLiteralConvertible -> Dictionary
  else if (protocol == getProtocol(
                         SourceLoc(),
                         KnownProtocolKind::DictionaryLiteralConvertible)) {
    type = &DictionaryLiteralType;
    name = "Dictionary";
  }
  // _ColorLiteralConvertible -> _ColorLiteralType
  else if (protocol == getProtocol(
                         SourceLoc(),
                         KnownProtocolKind::ColorLiteralConvertible)) {
    type = &ColorLiteralType;
    name = "_ColorLiteralType";
  }
  // _ImageLiteralConvertible -> _ImageLiteralType
  else if (protocol == getProtocol(
                         SourceLoc(),
                         KnownProtocolKind::ImageLiteralConvertible)) {
    type = &ImageLiteralType;
    name = "_ImageLiteralType";
  }
  // _FileReferenceLiteralConvertible -> _FileReferenceLiteralType
  else if (protocol == getProtocol(
                         SourceLoc(),
                         KnownProtocolKind::FileReferenceLiteralConvertible)) {
    type = &FileReferenceLiteralType;
    name = "_FileReferenceLiteralType";
  }

  if (!type)
    return nullptr;

  // If we haven't found the type yet, look for it now.
  if (!*type) {
    *type = lookupDefaultLiteralType(*this, dc, name);

    if (!*type)
      *type = lookupDefaultLiteralType(*this, getStdlibModule(dc), name);

    // Strip off one level of sugar; we don't actually want to print
    // the name of the typealias itself anywhere.
    if (type && *type) {
      if (auto typeAlias = dyn_cast<NameAliasType>(type->getPointer()))
        *type = typeAlias->getDecl()->getUnderlyingType();
    }
  }

  return *type;
}

Expr *TypeChecker::foldSequence(SequenceExpr *expr, DeclContext *dc) {
  ArrayRef<Expr*> Elts = expr->getElements();
  assert(Elts.size() > 1 && "inadequate number of elements in sequence");
  assert((Elts.size() & 1) == 1 && "even number of elements in sequence");

  Expr *LHS = Elts[0];
  Elts = Elts.slice(1);

  Expr *Result = ::foldSequence(*this, dc, LHS, Elts, /*min precedence*/ 0);
  assert(Elts.empty());
  return Result;
}

namespace {
  class FindCapturedVars : public ASTWalker {
    TypeChecker &TC;
    SmallVectorImpl<CapturedValue> &captureList;
    llvm::SmallDenseMap<ValueDecl*, unsigned, 4> captureEntryNumber;
    SourceLoc CaptureLoc;
    llvm::SmallPtrSet<ValueDecl *, 2> Diagnosed;
    /// The AbstractClosureExpr or AbstractFunctionDecl being analyzed.
    AnyFunctionRef AFR;
    bool &capturesTypes;
  public:
    FindCapturedVars(TypeChecker &tc,
                     SmallVectorImpl<CapturedValue> &captureList,
                     bool &capturesTypes,
                     AnyFunctionRef AFR)
        : TC(tc), captureList(captureList), AFR(AFR),
          capturesTypes(capturesTypes) {
      if (auto AFD = AFR.getAbstractFunctionDecl())
        CaptureLoc = AFD->getLoc();
      else {
        auto ACE = AFR.getAbstractClosureExpr();
        if (auto closure = dyn_cast<ClosureExpr>(ACE))
          CaptureLoc = closure->getInLoc();

        if (CaptureLoc.isInvalid())
          CaptureLoc = ACE->getLoc();
      }
    }

    /// \brief Check if the type of an expression references any generic
    /// type parameters.
    ///
    /// FIXME: SILGen doesn't currently allow local generic functions to
    /// capture generic parameters from an outer context. Once it does, we
    /// will need to distinguish outer and inner type parameters here.
    void checkType(Type type) {
      // Nothing to do if the type is concrete.
      if (!type || !type->hasArchetype())
        return;

      // Easy case.
      if (!type->hasOpenedExistential()) {
        capturesTypes = true;
        return;
      }

      // This type contains both an archetype and an open existential. Walk the
      // type to see if we have any archetypes that are *not* open existentials.
      if (type.findIf([](Type t) -> bool {
            return (t->is<ArchetypeType>() && !t->isOpenedExistential());
          }))
        capturesTypes = true;
    }

    /// Add the specified capture to the closure's capture list, diagnosing it
    /// if invalid.
    void addCapture(CapturedValue capture, SourceLoc Loc) {
      auto VD = capture.getDecl();

      // Check to see if we already have an entry for this decl.
      unsigned &entryNumber = captureEntryNumber[VD];
      if (entryNumber == 0) {
        captureList.push_back(capture);
        entryNumber = captureList.size();
      } else {
        // If this already had an entry in the capture list, make sure to merge
        // the information together.  If one is noescape but the other isn't,
        // then the result is escaping.
        unsigned Flags =
          captureList[entryNumber-1].getFlags() & capture.getFlags();
        capture = CapturedValue(VD, Flags);
        captureList[entryNumber-1] = capture;
      }

      // If VD is a noescape decl, then the closure we're computing this for
      // must also be noescape.
      if (VD->getAttrs().hasAttribute<NoEscapeAttr>() &&
          !capture.isNoEscape() &&
          // Don't repeatedly diagnose the same thing.
          Diagnosed.insert(VD).second) {

        // Otherwise, diagnose this as an invalid capture.
        bool isDecl = AFR.getAbstractFunctionDecl() != nullptr;

        TC.diagnose(Loc, isDecl ? diag::decl_closure_noescape_use :
                    diag::closure_noescape_use, VD->getName());

        if (VD->getAttrs().hasAttribute<AutoClosureAttr>() &&
            VD->getAttrs().getAttribute<NoEscapeAttr>()->isImplicit())
          TC.diagnose(VD->getLoc(), diag::noescape_autoclosure,
                      VD->getName());
      }
    }

    std::pair<bool, Expr *> walkToDeclRefExpr(DeclRefExpr *DRE) {
      auto *D = DRE->getDecl();

      // DC is the DeclContext where D was defined
      // CurDC is the DeclContext where D was referenced
      auto DC = D->getDeclContext();
      auto CurDC = AFR.getAsDeclContext();

      // A local reference is not a capture.
      if (CurDC == DC)
        return { false, DRE };

      auto TmpDC = CurDC;

      if (!isa<TopLevelCodeDecl>(DC)) {
        while (TmpDC != nullptr) {
          if (TmpDC == DC)
            break;

          // We have an intervening nominal type context that is not the
          // declaration context, and the declaration context is not global.
          // This is not supported since nominal types cannot capture values.
          if (auto NTD = dyn_cast<NominalTypeDecl>(TmpDC)) {
            if (DC->isLocalContext()) {
              TC.diagnose(DRE->getLoc(), diag::capture_across_type_decl,
                          NTD->getDescriptiveKind(),
                          D->getName());

              TC.diagnose(NTD->getLoc(), diag::type_declared_here);

              TC.diagnose(D->getLoc(), diag::decl_declared_here,
                          D->getName());

              return { false, DRE };
            }
          }

          TmpDC = TmpDC->getParent();
        }

        // We walked all the way up to the root without finding the declaration,
        // so this is not a capture.
        if (TmpDC == nullptr)
          return { false, DRE };
      }

      // Only capture var decls at global scope.  Other things can be captured
      // if they are local.
      if (!isa<VarDecl>(D) && !DC->isLocalContext())
        return { false, DRE };

      // Can only capture a variable that is declared before the capturing
      // entity.
      llvm::DenseSet<ValueDecl *> checkedCaptures;
      llvm::SmallVector<FuncDecl *, 2> capturePath;
      
      std::function<bool (ValueDecl *)>
      validateForwardCapture = [&](ValueDecl *capturedDecl) -> bool {
        if (!checkedCaptures.insert(capturedDecl).second)
          return true;
      
        // Captures at nonlocal scope are order-invariant.
        if (!capturedDecl->getDeclContext()->isLocalContext())
          return true;
        
        // Assume implicit decl captures are OK.
        if (!CaptureLoc.isValid() || !capturedDecl->getLoc().isValid())
          return true;
        
        // Check the order of the declarations.
        if (!TC.Context.SourceMgr.isBeforeInBuffer(CaptureLoc,
                                                   capturedDecl->getLoc()))
          return true;
        
        // Forward captures of functions are OK, if the function doesn't
        // transitively capture variables ahead of the original function.
        if (auto func = dyn_cast<FuncDecl>(capturedDecl)) {
          if (!func->getCaptureInfo().hasBeenComputed()) {
            // Check later.
            TC.ForwardCapturedFuncs[func].push_back(AFR);
            return true;
          }
          // Recursively check the transitive captures.
          capturePath.push_back(func);
          defer { capturePath.pop_back(); };
          for (auto capture : func->getCaptureInfo().getCaptures())
            if (!validateForwardCapture(capture.getDecl()))
              return false;
          return true;
        }
        
        // Diagnose the improper forward capture.
        if (Diagnosed.insert(capturedDecl).second) {
          if (capturedDecl == DRE->getDecl()) {
            TC.diagnose(DRE->getLoc(), diag::capture_before_declaration,
                        capturedDecl->getName());
          } else {
            TC.diagnose(DRE->getLoc(),
                        diag::transitive_capture_before_declaration,
                        DRE->getDecl()->getName(),
                        capturedDecl->getName());
            ValueDecl *prevDecl = capturedDecl;
            for (auto path : reversed(capturePath)) {
              TC.diagnose(path->getLoc(),
                          diag::transitive_capture_through_here,
                          path->getName(),
                          prevDecl->getName());
              prevDecl = path;
            }
          }
          TC.diagnose(capturedDecl->getLoc(), diag::decl_declared_here,
                      capturedDecl->getName());
        }
        return false;
      };
      
      if (!validateForwardCapture(DRE->getDecl()))
        return { false, DRE };

      // We're going to capture this, compute flags for the capture.
      unsigned Flags = 0;

      // If this is a direct reference to underlying storage, then this is a
      // capture of the storage address - not a capture of the getter/setter.
      if (DRE->getAccessSemantics() == AccessSemantics::DirectToStorage)
        Flags |= CapturedValue::IsDirect;

      // If the closure is noescape, then we can capture the decl as noescape.
      if (AFR.isKnownNoEscape())
        Flags |= CapturedValue::IsNoEscape;

      addCapture(CapturedValue(D, Flags), DRE->getStartLoc());
      return { false, DRE };
    }

    void propagateCaptures(AnyFunctionRef innerClosure, SourceLoc captureLoc) {
      TC.computeCaptures(innerClosure);

      auto CurDC = AFR.getAsDeclContext();
      bool isNoEscapeClosure = AFR.isKnownNoEscape();

      for (auto capture : innerClosure.getCaptureInfo().getCaptures()) {
        // If the decl was captured from us, it isn't captured *by* us.
        if (capture.getDecl()->getDeclContext() == CurDC)
          continue;

        // Compute adjusted flags.
        unsigned Flags = capture.getFlags();

        // The decl is captured normally, even if it was captured directly
        // in the subclosure.
        Flags &= ~CapturedValue::IsDirect;

        // If this is an escaping closure, then any captured decls are also
        // escaping, even if they are coming from an inner noescape closure.
        if (!isNoEscapeClosure)
          Flags &= ~CapturedValue::IsNoEscape;

        addCapture(CapturedValue(capture.getDecl(), Flags), captureLoc);
      }

      if (innerClosure.getCaptureInfo().hasGenericParamCaptures())
        capturesTypes = true;
    }

    bool walkToDeclPre(Decl *D) override {
      if (auto *AFD = dyn_cast<AbstractFunctionDecl>(D)) {
        propagateCaptures(AFD, AFD->getLoc());
        
        // Can default parameter initializers capture state?  That seems like
        // a really bad idea.
        for (auto *paramList : AFD->getParameterLists())
          for (auto param : *paramList) {
            if (auto E = param->getDefaultValue())
              E->getExpr()->walk(*this);
          }
        return false;
      }

      return true;
    }

    std::pair<bool, Expr *> walkToExprPre(Expr *E) override {
      checkType(E->getType());

      if (auto *ECE = dyn_cast<ExplicitCastExpr>(E)) {
        checkType(ECE->getCastTypeLoc().getType());
        return { true, E };
      }

      if (auto *DRE = dyn_cast<DeclRefExpr>(E))
        return walkToDeclRefExpr(DRE);

      // When we see a reference to the 'super' expression, capture 'self' decl.
      if (auto *superE = dyn_cast<SuperRefExpr>(E)) {
        auto CurDC = AFR.getAsDeclContext();
        if (CurDC->isChildContextOf(superE->getSelf()->getDeclContext()))
          addCapture(CapturedValue(superE->getSelf(), 0), superE->getLoc());
        return { false, superE };
      }

      // Don't recurse into child closures. They should already have a capture
      // list computed; we just propagate it, filtering out stuff that they
      // capture from us.
      if (auto *SubCE = dyn_cast<AbstractClosureExpr>(E)) {
        propagateCaptures(SubCE, SubCE->getStartLoc());
        return { false, E };
      }

      return { true, E };
    }
  };
}

void TypeChecker::maybeDiagnoseCaptures(Expr *E, AnyFunctionRef AFR) {
  if (!AFR.getCaptureInfo().hasBeenComputed()) {
    // The capture list is not always initialized by the point we reference
    // it. Remember we formed a C function pointer so we can diagnose later
    // if necessary.
    LocalCFunctionPointers[AFR].push_back(E);
    return;
  }

  if (AFR.getCaptureInfo().hasGenericParamCaptures() ||
      AFR.getCaptureInfo().hasLocalCaptures()) {
    diagnose(E->getLoc(),
             diag::c_function_pointer_from_function_with_context,
             /*closure*/ AFR.getAbstractClosureExpr() != nullptr,
             /*generic params*/ AFR.getCaptureInfo().hasGenericParamCaptures());
  }
}

void TypeChecker::computeCaptures(AnyFunctionRef AFR) {
  if (AFR.getCaptureInfo().hasBeenComputed())
    return;

  SmallVector<CapturedValue, 4> Captures;
  bool GenericParamCaptures = false;
  FindCapturedVars finder(*this, Captures, GenericParamCaptures, AFR);
  AFR.getBody()->walk(finder);

  if (AFR.hasType())
    finder.checkType(AFR.getType());

  // If this is an init(), explicitly walk the initializer values for members of
  // the type.  They will be implicitly emitted by SILGen into the generated
  // initializer.
  if (auto CD =
        dyn_cast_or_null<ConstructorDecl>(AFR.getAbstractFunctionDecl())) {
    auto *typeDecl = dyn_cast<NominalTypeDecl>(CD->getDeclContext());
    if (typeDecl && CD->isDesignatedInit()) {
      for (auto member : typeDecl->getMembers()) {
        // Ignore everything other than PBDs.
        auto *PBD = dyn_cast<PatternBindingDecl>(member);
        if (!PBD) continue;
        // Walk the initializers for all properties declared in the type with
        // an initializer.
        for (auto &elt : PBD->getPatternList())
          if (auto *init = elt.getInit())
            init->walk(finder);
      }
    }
  }

  // Since nested generic functions are not supported yet, the only case where
  // generic parameters can be captured is by closures and non-generic local
  // functions.
  //
  // So we only set GenericParamCaptures if we have a closure, or a
  // non-generic function defined inside a local context.
  auto *AFD = AFR.getAbstractFunctionDecl();
  if (!AFD ||
      (!AFD->getGenericParams() &&
       AFD->getDeclContext()->isLocalContext())) {
    AFR.getCaptureInfo().setGenericParamCaptures(GenericParamCaptures);
  }

  if (Captures.empty())
    AFR.getCaptureInfo().setCaptures(None);
  else
    AFR.getCaptureInfo().setCaptures(Context.AllocateCopy(Captures));

  // Diagnose if we have local captures and there were C pointers formed to
  // this function before we computed captures.
  auto cFunctionPointers = LocalCFunctionPointers.find(AFR);
  if (cFunctionPointers != LocalCFunctionPointers.end())
    for (auto *expr : cFunctionPointers->second)
      maybeDiagnoseCaptures(expr, AFR);
}

