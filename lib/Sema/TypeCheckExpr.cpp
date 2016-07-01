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
#include "swift/AST/Decl.h"
#include "swift/Parse/Lexer.h"
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
    auto resultSugar = cast<TypeExpr>(E->getArg())->getInstanceType();

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

  if (isa<ArrowExpr>(E)) {
    return InfixData(IntrinsicPrecedences::ArrowExpr,
                     Associativity::Right,
                     /*assignment*/ false);
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

  if (auto *arrow = dyn_cast<ArrowExpr>(Op)) {
    // Resolve the '->' expression.
    assert(!arrow->isFolded() && "already folded '->' expr in sequence?!");
    arrow->setArgsExpr(LHS);
    arrow->setResultExpr(RHS);
    return makeResultExpr(arrow);
  }
  
  // Build the argument to the operation.
  Expr *ArgElts[] = { LHS, RHS };
  auto ArgElts2 = TC.Context.AllocateCopy(MutableArrayRef<Expr*>(ArgElts));
  TupleExpr *Arg = TupleExpr::create(TC.Context,
                                     SourceLoc(), 
                                     ArgElts2, { }, { }, SourceLoc(),
                                     /*hasTrailingClosure=*/false,
                                     /*Implicit=*/true);

  
  
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
                                       DeclNameLoc loc, bool Implicit) {
  auto type = getUnopenedTypeOfReference(value, Type(), UseDC);
  AccessSemantics semantics = value->getAccessSemanticsFromContext(UseDC);
  return new (Context) DeclRefExpr(value, loc, Implicit, semantics, type);
}

Expr *TypeChecker::buildRefExpr(ArrayRef<ValueDecl *> Decls,
                                DeclContext *UseDC, DeclNameLoc NameLoc,
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
