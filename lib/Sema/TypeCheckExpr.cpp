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
    return InfixData(100, Associativity::Right);
  } else if (auto *assign = dyn_cast<AssignExpr>(E)) {
    // Assignment has fixed precedence.
    assert(!assign->isFolded() && "already folded assign expr in sequence?!");
    return InfixData(90, Associativity::Right);
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
  return InfixData(~0U, Associativity::Left);
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

Expr *TypeChecker::buildRefExpr(ArrayRef<ValueDecl *> Decls, SourceLoc NameLoc) {
  assert(!Decls.empty() && "Must have at least one declaration");

  if (Decls.size() == 1 && !isa<ProtocolDecl>(Decls[0]->getDeclContext())) {
    return new (Context) DeclRefExpr(Decls[0], NameLoc,
                                     Decls[0]->getTypeOfReference());
  }

  Decls = Context.AllocateCopy(Decls);
  return new (Context) OverloadedDeclRefExpr(Decls, NameLoc,
                         UnstructuredUnresolvedType::get(Context));
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

static bool semaFuncParamPatterns(TypeChecker *checker,
                                  DeclContext *dc,
                                  ArrayRef<Pattern*> paramPatterns,
                                  bool isFirstPass,
                                  bool allowUnknownTypes) {
  bool badType = false;
  for (Pattern *P : paramPatterns) {
    if (P->hasType())
      continue;
    if (checker->typeCheckPattern(P, dc, isFirstPass, allowUnknownTypes)) {
      badType = true;
      continue;
    }
  }
  return badType;
}

void TypeChecker::semaFuncExpr(FuncExpr *FE, bool isFirstPass,
                               bool allowUnknownTypes) {
  if (FE->getType() && !FE->getType()->isUnresolvedType())
    return;

  bool badType = false;
  if (FE->getBodyResultTypeLoc().getType()) {
    if (validateType(FE->getBodyResultTypeLoc())) {
      FE->getBodyResultTypeLoc().setInvalidType(Context);
      badType = true;
    }
  }
  
  badType = badType || semaFuncParamPatterns(this, FE, FE->getArgParamPatterns(),
                                             isFirstPass, allowUnknownTypes);
  badType = badType || semaFuncParamPatterns(this, FE, FE->getBodyParamPatterns(),
                                             isFirstPass, allowUnknownTypes);

  if (badType) {
    FE->setType(ErrorType::get(Context));
    return;
  }

  Type funcTy = FE->getBodyResultTypeLoc().getType();
  if (!funcTy) {
    if (allowUnknownTypes)
      funcTy = UnstructuredUnresolvedType::get(Context);
    else
      funcTy = TupleType::getEmpty(Context);
  }

  // FIXME: it would be nice to have comments explaining what this is all about.
  auto patterns = FE->getArgParamPatterns();
  bool isInstanceFunc = false;
  GenericParamList *genericParams = nullptr;
  GenericParamList *outerGenericParams = nullptr;
  if (FuncDecl *FD = FE->getDecl()) {
    isInstanceFunc = (bool)FD->computeThisType(&outerGenericParams);
    genericParams = FD->getGenericParams();
  }

  for (unsigned i = 0, e = patterns.size(); i != e; ++i) {
    Type argTy = patterns[e - i - 1]->getType();
    if (e - i - 1 == isInstanceFunc && genericParams) {
      funcTy = PolymorphicFunctionType::get(argTy, funcTy,
                                            genericParams,
                                            Context);
    } else if (e - i - 1 == 0 && outerGenericParams) {
      funcTy = PolymorphicFunctionType::get(argTy, funcTy,
                                            outerGenericParams,
                                            Context);
    } else {
      funcTy = FunctionType::get(argTy, funcTy, Context);
    }
  }
  FE->setType(funcTy);
}

namespace {
  class FindCapturedVars : public ASTWalker {
    TypeChecker &tc;
    llvm::SetVector<ValueDecl*> &captures;
    CapturingExpr *curExpr;

  public:
    std::pair<bool, Expr *> walkToExprPre(Expr *E) override {
      if (DeclRefExpr *DRE = dyn_cast<DeclRefExpr>(E)) {
        if (DRE->getDecl()->getDeclContext()->isLocalContext() &&
            DRE->getDecl()->getDeclContext() != curExpr) {
          // A [byref] parameter cannot be captured.
          // FIXME: As a temporary hack, ignore 'this', which is an implicit
          // [byref] parameter for instance methods of structs.
          if (DRE->getDecl()->getType()->is<LValueType>() && !
              DRE->getDecl()->getName().str().equals("this")) {
            tc.diagnose(DRE->getLoc(), diag::byref_capture,
                        DRE->getDecl()->getName());
          }
          captures.insert(DRE->getDecl());
        }
        return { false, E };
      }
      if (CapturingExpr *SubCE = dyn_cast<CapturingExpr>(E)) {
        for (auto D : SubCE->getCaptures())
          if (D->getDeclContext() != curExpr)
            captures.insert(D);
        return { false, E };
      }
      return { true, E };
    }

    FindCapturedVars(TypeChecker &tc,
                     llvm::SetVector<ValueDecl*> &captures,
                     CapturingExpr *curExpr)
      : tc(tc), captures(captures), curExpr(curExpr) {}

    void doWalk(Expr *E) {
      E->walk(*this);
    }
    void doWalk(Stmt *S) {
      S->walk(*this);
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
    if (auto body = func->getBody()) {
      finder.doWalk(body);
    }
  }
  ValueDecl** CaptureCopy
    = Context.AllocateCopy<ValueDecl*>(Captures.begin(), Captures.end());
  capturing->setCaptures(llvm::makeArrayRef(CaptureCopy, Captures.size()));
}

