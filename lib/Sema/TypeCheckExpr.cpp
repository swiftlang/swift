//===--- TypeCheckExpr.cpp - Type Checking for Expressions ----------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file implements semantic analysis for expressions, analyzing an
// expression tree in post-order, bottom-up, from leaves up to the root.
//
//===----------------------------------------------------------------------===//

#include "TypeChecker.h"
#include "swift/AST/NameLookup.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Initializer.h"
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
  argTy = argTy->getWithoutParens();
  
  // If this is the first match against the sugar type we found, use it.
  if (!resultSugarTy) {
    resultSugarTy = argTy;
    return;
  }
  
  // Make sure this argument's sugar is consistent with the sugar we
  // already found.
  if (argTy.getPointer() == resultSugarTy.getPointer())
    return;
  uniqueSugarTy = false;
}

/// If we can propagate type sugar from input arguments types to the result of
/// an apply, do so.
///
Expr *TypeChecker::substituteInputSugarTypeForResult(ApplyExpr *E) {
  if (!E->getType() || E->getType()->hasError())
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
        auto NFT = FunctionType::get(FT->getParams(), resultSugar,
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

/// Look up the builtin precedence group with the given name.
static PrecedenceGroupDecl *
getBuiltinPrecedenceGroup(TypeChecker &TC, DeclContext *DC, Identifier name,
                          SourceLoc loc) {
  auto group = TC.lookupPrecedenceGroup(DC, name,
                                        /*suppress diags*/ SourceLoc());
  if (!group) {
    TC.diagnose(loc, diag::missing_builtin_precedence_group, name);
  }
  return group;
}

static PrecedenceGroupDecl *
lookupPrecedenceGroupForOperator(TypeChecker &TC, DeclContext *DC,
                                 Identifier name, SourceLoc loc) {
  SourceFile *SF = DC->getParentSourceFile();
  bool isCascading = DC->isCascadingContextForLookup(true);
  if (auto op = SF->lookupInfixOperator(name, isCascading, loc)) {
    TC.validateDecl(op);
    return op->getPrecedenceGroup();
  } else {
    TC.diagnose(loc, diag::unknown_binop);
  }
  return nullptr;
}

PrecedenceGroupDecl *
TypeChecker::lookupPrecedenceGroupForInfixOperator(DeclContext *DC, Expr *E) {
  if (auto ifExpr = dyn_cast<IfExpr>(E)) {
    // Ternary has fixed precedence.
    return getBuiltinPrecedenceGroup(*this, DC, Context.Id_TernaryPrecedence,
                                     ifExpr->getQuestionLoc());
  }

  if (auto assignExpr = dyn_cast<AssignExpr>(E)) {
    // Assignment has fixed precedence.
    return getBuiltinPrecedenceGroup(*this, DC, Context.Id_AssignmentPrecedence,
                                     assignExpr->getEqualLoc());
  }

  if (auto castExpr = dyn_cast<ExplicitCastExpr>(E)) {
    // 'as' and 'is' casts have fixed precedence.
    return getBuiltinPrecedenceGroup(*this, DC, Context.Id_CastingPrecedence,
                                     castExpr->getAsLoc());
  }

  if (auto *DRE = dyn_cast<DeclRefExpr>(E)) {
    Identifier name = DRE->getDecl()->getBaseName().getIdentifier();
    return lookupPrecedenceGroupForOperator(*this, DC, name, DRE->getLoc());
  }

  if (auto *OO = dyn_cast<OverloadedDeclRefExpr>(E)) {
    Identifier name = OO->getDecls()[0]->getBaseName().getIdentifier();
    return lookupPrecedenceGroupForOperator(*this, DC, name, OO->getLoc());
  }

  if (auto arrowExpr = dyn_cast<ArrowExpr>(E)) {
    return getBuiltinPrecedenceGroup(*this, DC,
                                     Context.Id_FunctionArrowPrecedence,
                                     arrowExpr->getArrowLoc());
  }

  // An already-folded binary operator comes up for non-primary use cases
  // of this function.
  if (auto binaryExpr = dyn_cast<BinaryExpr>(E)) {
    return lookupPrecedenceGroupForInfixOperator(DC, binaryExpr->getFn());
  }

  // If E is already an ErrorExpr, then we've diagnosed it as invalid already,
  // otherwise emit an error.
  if (!isa<ErrorExpr>(E))
    diagnose(E->getLoc(), diag::unknown_binop);

  return nullptr;
}

// The way we compute isEndOfSequence relies on the assumption that
// the sequence-folding algorithm never recurses with a prefix of the
// entire sequence.
static Expr *makeBinOp(TypeChecker &TC, Expr *Op, Expr *LHS, Expr *RHS,
                       PrecedenceGroupDecl *opPrecedence,
                       bool isEndOfSequence) {
  if (!LHS || !RHS)
    return nullptr;

  // If the left-hand-side is a 'try', hoist it up.
  auto *tryEval = dyn_cast<AnyTryExpr>(LHS);
  if (tryEval) {
    LHS = tryEval->getSubExpr();
  }
  
  // If this is an assignment operator, and the left operand is an optional
  // evaluation, pull the operator into the chain.
  OptionalEvaluationExpr *optEval = nullptr;
  if (opPrecedence && opPrecedence->isAssignment()) {
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

    if (isa<IfExpr>(Op) ||
        (opPrecedence && opPrecedence->isAssignment())) {
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
                                     /*HasTrailingClosure=*/false,
                                     /*Implicit=*/true);

  
  
  // Build the operation.
  return makeResultExpr(new (TC.Context) BinaryExpr(Op, Arg, Op->isImplicit()));
}

namespace {
  class PrecedenceBound {
    llvm::PointerIntPair<PrecedenceGroupDecl*,1,bool> GroupAndIsStrict;
  public:
    PrecedenceBound() {}
    PrecedenceBound(PrecedenceGroupDecl *decl, bool isStrict)
      : GroupAndIsStrict(decl, isStrict) {}

    bool shouldConsider(TypeChecker &TC, PrecedenceGroupDecl *group) {
      auto storedGroup = GroupAndIsStrict.getPointer();
      if (!storedGroup) return true;
      if (!group) return false;
      if (storedGroup == group) return !GroupAndIsStrict.getInt();
      return TC.Context.associateInfixOperators(group, storedGroup)
               == Associativity::Left;
    }
  };
} // end anonymous namespace

/// foldSequence - Take a sequence of expressions and fold a prefix of
/// it into a tree of BinaryExprs using precedence parsing.
static Expr *foldSequence(TypeChecker &TC, DeclContext *DC,
                          Expr *LHS,
                          ArrayRef<Expr*> &S,
                          PrecedenceBound precedenceBound) {
  // Invariant: S is even-sized.
  // Invariant: All elements at even indices are operator references.
  assert(!S.empty());
  assert((S.size() & 1) == 0);
  
  struct Op {
    Expr *op;
    PrecedenceGroupDecl *precedence;
    
    explicit operator bool() const { return op != nullptr; }
  };
  
  /// Get the operator, if appropriate to this pass.
  auto getNextOperator = [&]() -> Op {
    Expr *op = S[0];

    // If the operator's precedence is lower than the minimum, stop here.
    auto opPrecedence = TC.lookupPrecedenceGroupForInfixOperator(DC, op);
    if (!precedenceBound.shouldConsider(TC, opPrecedence))
      return {nullptr, nullptr};
    return {op, opPrecedence};
  };

  // Extract out the first operator.
  Op op1 = getNextOperator();
  if (!op1) return LHS;
  
  // We will definitely be consuming at least one operator.
  // Pull out the prospective RHS and slice off the first two elements.
  Expr *RHS = S[1];
  S = S.slice(2);
  
  while (!S.empty()) {
    assert((S.size() & 1) == 0);
    assert(precedenceBound.shouldConsider(TC, op1.precedence));

    // If the operator is a cast operator, the RHS can't extend past the type
    // that's part of the cast production.
    if (isa<ExplicitCastExpr>(op1.op)) {
      LHS = makeBinOp(TC, op1.op, LHS, RHS, op1.precedence, S.empty());
      op1 = getNextOperator();
      if (!op1) return LHS;
      RHS = S[1];
      S = S.slice(2);
      continue;
    }
    
    // Pull out the next binary operator.
    Op op2 = { S[0], TC.lookupPrecedenceGroupForInfixOperator(DC, S[0]) };

    // If the second operator's precedence is lower than the
    // precedence bound, break out of the loop.
    if (!precedenceBound.shouldConsider(TC, op2.precedence)) break;

    // If we're missing precedence info for either operator, treat them
    // as non-associative.
    Associativity associativity;
    if (!op1.precedence || !op2.precedence) {
      associativity = Associativity::None;
    } else {
      associativity =
        TC.Context.associateInfixOperators(op1.precedence, op2.precedence);
    }

    // Apply left-associativity immediately by folding the first two
    // operands.
    if (associativity == Associativity::Left) {
      LHS = makeBinOp(TC, op1.op, LHS, RHS, op1.precedence, S.empty());
      op1 = op2;
      RHS = S[1];
      S = S.slice(2);
      continue;
    }

    // If the first operator's precedence is lower than the second
    // operator's precedence, recursively fold all such
    // higher-precedence operators starting from this point, then
    // repeat.
    if (associativity == Associativity::Right &&
        op1.precedence != op2.precedence) {
      RHS = foldSequence(TC, DC, RHS, S,
                         PrecedenceBound(op1.precedence, /*strict*/ true));
      continue;
    }

    // Apply right-associativity by recursively folding operators
    // starting from this point, then immediately folding the LHS and RHS.
    if (associativity == Associativity::Right) {
      RHS = foldSequence(TC, DC, RHS, S,
                         PrecedenceBound(op1.precedence, /*strict*/ false));
      LHS = makeBinOp(TC, op1.op, LHS, RHS, op1.precedence, S.empty());

      // If we've drained the entire sequence, we're done.
      if (S.empty()) return LHS;

      // Otherwise, start all over with our new LHS.
      return foldSequence(TC, DC, LHS, S, precedenceBound);
    }

    // If we ended up here, it's because we're either:
    //   - missing precedence groups,
    //   - have unordered precedence groups, or
    //   - have the same precedence group with no associativity.
    assert(associativity == Associativity::None);

    // Don't diagnose if we're missing a precedence group; this is
    // an invalid-code situation.
    if (!op1.precedence || !op2.precedence) {
      // do nothing
    } else if (op1.precedence == op2.precedence) {
      assert(op1.precedence->isNonAssociative());
      // FIXME: QoI ranges
      TC.diagnose(op1.op->getLoc(), diag::non_associative_adjacent_operators,
                  op1.precedence->getName())
        .highlight(SourceRange(op2.op->getLoc(), op2.op->getLoc()));

    } else {
      TC.diagnose(op1.op->getLoc(), diag::unordered_adjacent_operators,
                  op1.precedence->getName(), op2.precedence->getName())
        .highlight(SourceRange(op2.op->getLoc(), op2.op->getLoc()));      
    }
    
    // Recover by arbitrarily binding the first two.
    LHS = makeBinOp(TC, op1.op, LHS, RHS, op1.precedence, S.empty());
    return foldSequence(TC, DC, LHS, S, precedenceBound);
  }

  // Fold LHS and RHS together and declare completion.
  return makeBinOp(TC, op1.op, LHS, RHS, op1.precedence, S.empty());
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

Expr *TypeChecker::buildCheckedRefExpr(VarDecl *value, DeclContext *UseDC,
                                       DeclNameLoc loc, bool Implicit) {
  auto type = getUnopenedTypeOfReference(value, Type(), UseDC);
  auto semantics = value->getAccessSemanticsFromContext(UseDC,
                                                       /*isAccessOnSelf*/false);
  return new (Context) DeclRefExpr(value, loc, Implicit, semantics, type);
}

Expr *TypeChecker::buildRefExpr(ArrayRef<ValueDecl *> Decls,
                                DeclContext *UseDC, DeclNameLoc NameLoc,
                                bool Implicit, FunctionRefKind functionRefKind) {
  assert(!Decls.empty() && "Must have at least one declaration");

  if (Decls.size() == 1 && !isa<ProtocolDecl>(Decls[0]->getDeclContext())) {
    auto semantics = Decls[0]->getAccessSemanticsFromContext(UseDC,
                                                       /*isAccessOnSelf*/false);
    return new (Context) DeclRefExpr(Decls[0], NameLoc, Implicit, semantics);
  }

  Decls = Context.AllocateCopy(Decls);
  auto result = new (Context) OverloadedDeclRefExpr(Decls, NameLoc, 
                                                    functionRefKind,
                                                    Implicit);
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

  if (TD->isInvalid())
    return Type();

  if (auto *NTD = dyn_cast<NominalTypeDecl>(TD))
    return NTD->getDeclaredType();
  return cast<TypeAliasDecl>(TD)->getDeclaredInterfaceType();
}

Type TypeChecker::getDefaultType(ProtocolDecl *protocol, DeclContext *dc) {
  Type *type = nullptr;
  const char *name = nullptr;

  // LegacyExpressibleByUnicodeScalarLiteral -> UnicodeScalarType
  if (protocol ==
           getProtocol(
               SourceLoc(),
               KnownProtocolKind::LegacyExpressibleByUnicodeScalarLiteral)) {
    type = &UnicodeScalarType;
    name = "UnicodeScalarType";
  }
  // ExpressibleByExtendedGraphemeClusterLiteral -> ExtendedGraphemeClusterType
  else if (protocol ==
           getProtocol(
               SourceLoc(),
               KnownProtocolKind::ExpressibleByExtendedGraphemeClusterLiteral)) {
    type = &ExtendedGraphemeClusterType;
    name = "ExtendedGraphemeClusterType";
  }
  // ExpressibleByUnicodeScalarLiteral -> UnicodeScalarLiteralType
  else if (protocol ==
           getProtocol(
               SourceLoc(),
               KnownProtocolKind::ExpressibleByUnicodeScalarLiteral)) {
    type = &UnicodeScalarLiteralType;
    name = "UnicodeScalarLiteralType";
  }
  // ExpressibleByCharacterLiteral -> CharacterLiteralType
  else if (protocol ==
           getProtocol(
               SourceLoc(),
               KnownProtocolKind::ExpressibleByCharacterLiteral)) {
    type = &CharacterLiteralType;
    name = "CharacterLiteralType";
  }
  // ExpressibleByStringLiteral -> StringLiteralType
  // ExpressibleByStringInterpolation -> StringLiteralType
  else if (protocol == getProtocol(
                         SourceLoc(),
                         KnownProtocolKind::ExpressibleByStringLiteral) ||
           protocol == getProtocol(
                         SourceLoc(),
                         KnownProtocolKind::ExpressibleByStringInterpolation)) {
    type = &StringLiteralType;
    name = "StringLiteralType";
  }
  // ExpressibleByIntegerLiteral -> IntegerLiteralType
  else if (protocol == getProtocol(
                         SourceLoc(),
                         KnownProtocolKind::ExpressibleByIntegerLiteral)) {
    type = &IntLiteralType;
    name = "IntegerLiteralType";
  }
  // ExpressibleByFloatLiteral -> FloatLiteralType
  else if (protocol == getProtocol(SourceLoc(),
                                   KnownProtocolKind::ExpressibleByFloatLiteral)){
    type = &FloatLiteralType;
    name = "FloatLiteralType";
  }
  // ExpressibleByBooleanLiteral -> BoolLiteralType
  else if (protocol == getProtocol(
                         SourceLoc(),
                         KnownProtocolKind::ExpressibleByBooleanLiteral)){
    type = &BooleanLiteralType;
    name = "BooleanLiteralType";
  }
  // ExpressibleByArrayLiteral -> Array
  else if (protocol == getProtocol(SourceLoc(),
                                   KnownProtocolKind::ExpressibleByArrayLiteral)){
    type = &ArrayLiteralType;
    name = "Array";
  }
  // ExpressibleByDictionaryLiteral -> Dictionary
  else if (protocol == getProtocol(
                         SourceLoc(),
                         KnownProtocolKind::ExpressibleByDictionaryLiteral)) {
    type = &DictionaryLiteralType;
    name = "Dictionary";
  }
  // _ExpressibleByColorLiteral -> _ColorLiteralType
  else if (protocol == getProtocol(
                         SourceLoc(),
                         KnownProtocolKind::ExpressibleByColorLiteral)) {
    type = &ColorLiteralType;
    name = "_ColorLiteralType";
  }
  // _ExpressibleByImageLiteral -> _ImageLiteralType
  else if (protocol == getProtocol(
                         SourceLoc(),
                         KnownProtocolKind::ExpressibleByImageLiteral)) {
    type = &ImageLiteralType;
    name = "_ImageLiteralType";
  }
  // _ExpressibleByFileReferenceLiteral -> _FileReferenceLiteralType
  else if (protocol == getProtocol(
                         SourceLoc(),
                         KnownProtocolKind::ExpressibleByFileReferenceLiteral)) {
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
      if (auto boundTypeAlias =
                 dyn_cast<NameAliasType>(type->getPointer()))
        *type = boundTypeAlias->getSinglyDesugaredType();
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

  Expr *Result = ::foldSequence(*this, dc, LHS, Elts, PrecedenceBound());
  assert(Elts.empty());

  return Result;
}
