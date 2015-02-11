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
#include "swift/AST/Decl.h"
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

  } else if (auto *assign = dyn_cast<AssignExpr>(E)) {
    // Assignment has fixed precedence.
    assert(!assign->isFolded() && "already folded assign expr in sequence?!");
    (void)assign;
    return InfixData(IntrinsicPrecedences::AssignExpr,
                     Associativity::Right,
                     /*assignment*/ true);

  } else if (auto *as = dyn_cast<ExplicitCastExpr>(E)) {
    // 'as' and 'is' casts have fixed precedence.
    assert(!as->isFolded() && "already folded 'as' expr in sequence?!");
    (void)as;
    return InfixData(IntrinsicPrecedences::ExplicitCastExpr,
                     Associativity::None,
                     /*assignment*/ false);

  } else if (DeclRefExpr *DRE = dyn_cast<DeclRefExpr>(E)) {
    SourceFile *SF = DC->getParentSourceFile();
    Identifier name = DRE->getDecl()->getName();
    bool isCascading = DC->isCascadingContextForLookup(true);
    if (InfixOperatorDecl *op = SF->lookupInfixOperator(name, isCascading,
                                                        E->getLoc()))
      return op->getInfixData();

  } else if (OverloadedDeclRefExpr *OO = dyn_cast<OverloadedDeclRefExpr>(E)) {
    SourceFile *SF = DC->getParentSourceFile();
    Identifier name = OO->getDecls()[0]->getName();
    bool isCascading = DC->isCascadingContextForLookup(true);
    if (InfixOperatorDecl *op = SF->lookupInfixOperator(name, isCascading,
                                                        E->getLoc()))
      return op->getInfixData();
  }
  
  TC.diagnose(E->getLoc(), diag::unknown_binop);
  // Recover with an infinite-precedence left-associative operator.
  return InfixData((unsigned char)~0U, Associativity::Left,
                   /*assignment*/ false);
}

static Expr *makeBinOp(TypeChecker &TC, Expr *Op, Expr *LHS, Expr *RHS,
                       const InfixData &infixData) {
  if (!LHS || !RHS)
    return nullptr;
  
  // If this is an assignment operator, and the left operand is an optional
  // evaluation, pull the operator into the chain.
  OptionalEvaluationExpr *optEval = nullptr;
  if (infixData.isAssignment()) {
    if ((optEval = dyn_cast<OptionalEvaluationExpr>(LHS))) {
      LHS = optEval->getSubExpr();
    }
  }
  
  // Fold the result into the optional evaluation, if we have one.
  auto makeResultExpr = [&](Expr *result) -> Expr * {
    if (optEval) {
      optEval->setSubExpr(result);
      return optEval;
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
      LHS = makeBinOp(TC, Op1.op, LHS, RHS, Op1.infixData);
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
      LHS = makeBinOp(TC, Op1.op, LHS, RHS, Op1.infixData);
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
      LHS = makeBinOp(TC, Op1.op, LHS, RHS, Op1.infixData);

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
    LHS = makeBinOp(TC, Op1.op, LHS, RHS, Op1.infixData);
    return foldSequence(TC, DC, LHS, S, MinPrecedence);
  }

  // Fold LHS and RHS together and declare completion.
  return makeBinOp(TC, Op1.op, LHS, RHS, Op1.infixData);
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
                                     Type baseType, DeclContext *useDC) {
  // Unsettable storage decls always produce rvalues.
  if (!storage->isSettable(useDC))
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

  // If the base is a reference type, or if the base is mutable, then a
  // reference produces an lvalue.
  if (baseType->hasReferenceSemantics() || baseType->is<LValueType>())
    return true;

  // If the base is an rvalue, then we only produce an lvalue if both
  // the getter and setter are nonmutating.

  switch (storage->getStorageKind()) {
  // The setter of a stored decl is implicitly mutating.
  case AbstractStorageDecl::Stored:
    return false;

  // For pure-addressed storage, just consider the addressors.
  case AbstractStorageDecl::Addressed:
    return !storage->getAddressor()->isMutating() &&
           !storage->getMutableAddressor()->isMutating();

  // Otherwise, consider the getter and setter.
  case AbstractStorageDecl::StoredWithObservers:
  case AbstractStorageDecl::StoredWithTrivialAccessors:
  case AbstractStorageDecl::InheritedWithObservers:
  case AbstractStorageDecl::AddressedWithTrivialAccessors:
  case AbstractStorageDecl::AddressedWithObservers:
  case AbstractStorageDecl::ComputedWithMutableAddress:
  case AbstractStorageDecl::Computed:
    return !storage->getGetter()->isMutating() &&
           (!storage->getSetter() || !storage->getSetter()->isMutating());
  }
  llvm_unreachable("bad storage kind");
}

Type TypeChecker::getUnopenedTypeOfReference(ValueDecl *value, Type baseType,
                                             DeclContext *UseDC,
                                             bool wantInterfaceType) {
  validateDecl(value);
  if (value->isInvalid())
    return ErrorType::get(Context);

  Type requestedType = getTypeOfRValue(value, wantInterfaceType);

  // Qualify storage declarations with an lvalue when appropriate.
  // Otherwise, they yield rvalues (and the access must be a load).
  if (auto *storage = dyn_cast<AbstractStorageDecl>(value)) {
    if (doesStorageProduceLValue(*this, storage, baseType, UseDC)) {
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
  UnqualifiedLookup lookup(TC.Context.getIdentifier(name),
                           dc->getModuleScopeContext(),
                           nullptr,
                           /*non-cascading=*/isa<AbstractFunctionDecl>(dc));
  TypeDecl *TD = lookup.getSingleTypeResult();
  if (!TD)
    return Type();
  TC.validateDecl(TD);
  return TD->getDeclaredType();
}

Type TypeChecker::getDefaultType(ProtocolDecl *protocol, DeclContext *dc) {
  Type *type = nullptr;
  const char *name = nullptr;

  // CharacterLiteralConvertible -> CharacterLiteralType
  if (protocol == getProtocol(SourceLoc(),
                              KnownProtocolKind::CharacterLiteralConvertible)) {
    type = &CharacterLiteralType;
    name = "CharacterLiteralType";
  }
  // UnicodeScalarLiteralConvertible -> UnicodeScalarType
  else if (protocol ==
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
    llvm::SetVector<ValueDecl*> &captures;
    DeclContext *CurDC;
    SourceLoc CaptureLoc;
    llvm::SmallPtrSet<ValueDecl *, 2> Diagnosed;

  public:
    FindCapturedVars(TypeChecker &tc, llvm::SetVector<ValueDecl*> &captures,
                     AnyFunctionRef AFR)
        : TC(tc), captures(captures), CurDC(AFR.getAsDeclContext()) {
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

    void doWalk(Expr *E) {
      E->walk(*this);
    }
    void doWalk(Stmt *S) {
      S->walk(*this);
    }

    /// Add the specified capture to the closure's capture list, diagnosing it
    /// if invalid.
    void addCapture(ValueDecl *VD, SourceLoc Loc) {
      captures.insert(VD);

      // If VD is a noescape decl, then the closure we're computing this for
      // must also be noescape.
      if (!VD->getAttrs().hasAttribute<NoEscapeAttr>())
        return;

      // Closure expressions are processed as part of expression checking.
      if (isa<AbstractClosureExpr>(CurDC))
        return;

      // Don't repeatedly diagnose the same thing.
      if (!Diagnosed.insert(VD).second)
        return;

      // Otherwise, diagnose this as an invalid capture.
      TC.diagnose(Loc, diag::decl_closure_noescape_use, VD->getName());

      if (VD->getAttrs().hasAttribute<AutoClosureAttr>() &&
          VD->getAttrs().getAttribute<NoEscapeAttr>()->isImplicit())
        TC.diagnose(VD->getLoc(), diag::noescape_autoclosure,
                    VD->getName());

    }

    std::pair<bool, Expr *> walkToDeclRefExpr(DeclRefExpr *DRE) {
      auto *D = DRE->getDecl();

      // Decl references that are within the Capture are local references, ones
      // from parent context are captures.
      if (!CurDC->isChildContextOf(D->getDeclContext()))
        return { false, DRE };

      // Only capture var decls at global scope.  Other things can be captured
      // if they are local.
      if (!isa<VarDecl>(D) && !D->getDeclContext()->isLocalContext())
        return { false, DRE };

      // Can only capture a local that is declared before the capturing entity.
      if (DRE->getDecl()->getDeclContext()->isLocalContext() &&
          CaptureLoc.isValid() && DRE->getDecl()->getLoc().isValid() &&
          TC.Context.SourceMgr.isBeforeInBuffer(CaptureLoc,
                                                DRE->getDecl()->getLoc())) {
        if (Diagnosed.insert(DRE->getDecl()).second) {
          TC.diagnose(DRE->getLoc(), diag::capture_before_declaration,
                      DRE->getDecl()->getName());
          TC.diagnose(DRE->getDecl()->getLoc(), diag::decl_declared_here,
                      DRE->getDecl()->getName());
        }
        return { false, DRE };
      }

      if (auto FD = dyn_cast<FuncDecl>(D)) {
        // TODO: Local functions cannot be recursive, because SILGen
        // cannot handle it yet.
        if (CurDC == FD) {
          TC.diagnose(DRE->getLoc(), 
                      diag::unsupported_recursive_local_function);
          return { false, DRE };
        }

        // TODO: Local function references aren't implemented in
        // SILGen yet. However, if there are no local captures, it will work.
        // Keep track of these local function captures so we can check them
        // later.
        // Observing accessors appear to harmlessly capture each
        // other, however, so give them an exception.
        if (!FD->getAccessorStorageDecl())
          TC.LocalFunctionCaptures.push_back({FD, DRE->getLoc()});
      }
      
      addCapture(D, DRE->getStartLoc());
      return { false, DRE };
    }

    std::pair<bool, Expr *> walkToExprPre(Expr *E) override {
      if (auto *DRE = dyn_cast<DeclRefExpr>(E))
        return walkToDeclRefExpr(DRE);

      if (auto *superE = dyn_cast<SuperRefExpr>(E)) {
        if (CurDC->isChildContextOf(superE->getSelf()->getDeclContext()))
          addCapture(superE->getSelf(), superE->getLoc());
        return { false, superE };
      }

      // Don't recurse into child closures. They should already have a capture
      // list computed; we just propagate it, filtering out stuff that they
      // capture from us.
      if (auto *SubCE = dyn_cast<AbstractClosureExpr>(E)) {
        for (auto D : SubCE->getCaptureInfo().getCaptures())
          if (D->getDeclContext() != CurDC)
            addCapture(D, E->getStartLoc());
        return { false, E };
      }
      return { true, E };
    }
  };
}

void TypeChecker::computeCaptures(AnyFunctionRef AFR) {
  llvm::SetVector<ValueDecl *> Captures;
  FindCapturedVars finder(*this, Captures, AFR);
  finder.doWalk(AFR.getBody());
  ValueDecl **CaptureCopy =
      Context.AllocateCopy<ValueDecl *>(Captures.begin(), Captures.end());
  AFR.getCaptureInfo().setCaptures(
      llvm::makeArrayRef(CaptureCopy, Captures.size()));
}

