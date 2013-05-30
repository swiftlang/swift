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
Expr *TypeChecker::buildArrayInjectionFnRef(ArraySliceType *sliceType,
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
  if (typeCheckExpressionShallow(injectionFn))
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
  return coerceToType(injectionFn, fnTy);
}

/// getInfixData - If the specified expression is an infix binary
/// operator, return its infix operator attributes.
static InfixData getInfixData(TypeChecker &TC, Expr *E) {
  assert(!isa<UnresolvedElseExpr>(E) &&
         "should fold ':' as part of ternary folding");
  if (isa<UnresolvedIfExpr>(E)) {
    // Ternary has fixed precedence.
    return InfixData(100, Associativity::Right);
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
  
  // An "operator" for our purposes can be either a binary op, or the MHS of a
  // ternary.
  struct Op {
    enum { Null, Binary, Ternary } kind;
    union {
      Expr *binary;
      struct {
        SourceLoc question;
        swift::Expr *mhs;
        SourceLoc colon;
      } ternary;
    };
    InfixData infixData;
    
    Op() : kind(Null) {}
    
    Op(Expr *binary, InfixData data)
      : kind(Binary), binary(binary), infixData(data) {}
    Op(SourceLoc question, swift::Expr *mhs, SourceLoc colon, InfixData data)
      : kind(Ternary), ternary{question, mhs, colon}, infixData(data) {}
    
    SourceLoc getLoc() const {
      switch (kind) {
      case Null:
        llvm_unreachable("null op");
      case Binary:
        return binary->getLoc();
      case Ternary:
        return ternary.question;
      }
    }
    
    explicit operator bool() const { return kind != Null; }
  };
  
  /// Get the next binary or ternary operator, if appropriate to this pass.
  auto getNextOperator = [&]() -> Op {
    Expr *op = S[0];
    // If this is a ternary ':', stop here.
    // The outer parse will match it to a '?'.
    if (isa<UnresolvedElseExpr>(op)) {
      return {};
    }
    
    // If the operator's precedence is lower than the minimum, stop here.
    InfixData opInfo = getInfixData(TC, op);
    if (opInfo.getPrecedence() < MinPrecedence) return {};
    // If this is a ternary '?', do a maximal-munch parse of the middle operand
    // up to the matching ':'.
    if (isa<UnresolvedIfExpr>(op)) {
      Expr *MHS = S[1];
      assert(S.size() >= 4 &&
             "SequenceExpr doesn't have enough elts to complete ternary");
      S = S.slice(2);
      MHS = foldSequence(TC, MHS, S, 0);
      assert(S.size() >= 2 &&
             "folding MHS of ternary did not leave enough elts to complete ternary");
      assert(isa<UnresolvedElseExpr>(S[0]) &&
             "folding MHS of ternary did not end at ':'");
      return Op(op->getLoc(), MHS, S[0]->getLoc(), opInfo);
    }
    return Op(op, opInfo);
  };
  
  /// Finalize an operator expression.
  auto makeOperatorExpr = [&](Expr *LHS, Op Operator, Expr *RHS) -> Expr* {
    switch (Operator.kind) {
    case Op::Null:
      llvm_unreachable("should have break'ed on null Op");
        
    case Op::Binary:
      return makeBinOp(TC, Operator.binary, LHS, RHS);
    
    case Op::Ternary:
      return new (TC.Context) IfExpr(LHS,
                                     Operator.ternary.question,
                                     Operator.ternary.mhs,
                                     Operator.ternary.colon,
                                     RHS);
    }
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
    // If this is a ternary ':', break out of the loop.
    if (isa<UnresolvedElseExpr>(Op2)) break;
  
    InfixData Op2Info = getInfixData(TC, Op2);
    // If the second operator's precedence is lower than the min
    // precedence, break out of the loop.
    if (Op2Info.getPrecedence() < MinPrecedence) break;
    
    // If the first operator's precedence is higher than the second
    // operator's precedence, or they have matching precedence and are
    // both left-associative, fold LHS and RHS immediately.
    if (Op1.infixData.getPrecedence() > Op2Info.getPrecedence() ||
        (Op1.infixData == Op2Info && Op1.infixData.isLeftAssociative())) {
      LHS = makeOperatorExpr(LHS, Op1, RHS);
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
      LHS = makeOperatorExpr(LHS, Op1, RHS);

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
      TC.diagnose(Op1.getLoc(), diag::non_assoc_adjacent);
    } else if (Op2Info.isNonAssociative()) {
      TC.diagnose(Op2->getLoc(), diag::non_assoc_adjacent);
    } else {
      TC.diagnose(Op1.getLoc(), diag::incompatible_assoc);
    }
    
    // Recover by arbitrarily binding the first two.
    LHS = makeOperatorExpr(LHS, Op1, RHS);
    return foldSequence(TC, LHS, S, MinPrecedence);
  }

  // Fold LHS (and MHS, if ternary) and RHS together and declare completion.

  return makeOperatorExpr(LHS, Op1, RHS);
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

/// \brief Determine whether the given type is a suitable argument type for
/// a string literal conversion function.
static bool isStringLiteralArg(Type argType) {
  if (argType->is<BuiltinRawPointerType>())
    return true;

  TupleType *tt = argType->getAs<TupleType>();
  if (!tt)
    return false;
  if (tt->getFields().size() != 2)
    return false;
  if (!tt->getElementType(0)->is<BuiltinRawPointerType>())
    return false;
  BuiltinIntegerType *intTy
  = tt->getElementType(1)->getAs<BuiltinIntegerType>();
  if (!intTy)
    return false;
  if (intTy->getBitWidth() != 64)
    return false;
  return true;
}

std::pair<FuncDecl*, Type>
TypeChecker::isLiteralCompatibleType(Type Ty, SourceLoc Loc, LiteralKind LitTy,
                                     bool Complain, bool RequiresBuiltinArg) {
  if (Ty->is<LValueType>()) {
    if (Complain)
      diagnose(Loc, diag::type_not_compatible_literal, Ty);
    return std::pair<FuncDecl*, Type>();
  }

  // Look up the convertFrom*Literal method on the type.  If it is missing,
  // then the type isn't compatible with literals.  If it is present, it must
  // have a single argument.
  const char *MethodName = 0;
  const char *AltMethodName = 0;
  switch (LitTy) {
  case LiteralKind::Int:    MethodName = "convertFromIntegerLiteral"; break;
  case LiteralKind::Float:  MethodName = "convertFromFloatLiteral"; break;
  case LiteralKind::Char:   MethodName = "convertFromCharacterLiteral"; break;
  case LiteralKind::UTFString: MethodName = "convertFromStringLiteral"; break;
  case LiteralKind::ASCIIString:
    MethodName = "convertFromASCIIStringLiteral";
    AltMethodName = "convertFromStringLiteral";
    break;
  case LiteralKind::Array:
    llvm_unreachable("Cannot handle array literals here");
  case LiteralKind::Dictionary:
    llvm_unreachable("Cannot handle dictionary literals here");
  }
  assert(MethodName && "Didn't know LitTy");

  auto metaTy = MetaTypeType::get(Ty, Context);
  MemberLookup PrimaryLookup(metaTy, Context.getIdentifier(MethodName), TU);
  Optional<MemberLookup> AltLookup;
  if (AltMethodName && !PrimaryLookup.isSuccess())
    AltLookup.emplace(metaTy, Context.getIdentifier(AltMethodName), TU);
  MemberLookup &Lookup = AltLookup ? AltLookup.getValue() : PrimaryLookup;

  if (!Lookup.isSuccess()) {
    if (Complain)
      diagnose(Loc, diag::type_not_compatible_literal, Ty);
    return std::pair<FuncDecl*, Type>();
  }

  if (Lookup.Results.size() != 1) {
    if (Complain) {
      diagnose(Loc, diag::type_ambiguous_literal_conversion, Ty, MethodName);
      for (MemberLookupResult Res : Lookup.Results)
        diagnose(Res.D, diag::found_candidate);
    }
    return std::pair<FuncDecl*, Type>();
  }
  
  // Verify that the implementation is a metatype 'static' func.
  MemberLookupResult LookupResult = Lookup.Results[0];
  if (LookupResult.Kind != MemberLookupResult::MetatypeMember ||
      !isa<FuncDecl>(LookupResult.D)) {
    if (Complain)
      diagnose(LookupResult.D->getLoc(),
               diag::type_literal_conversion_not_static, Ty, MethodName);
    return std::pair<FuncDecl*, Type>();
  }
  FuncDecl *Method = cast<FuncDecl>(LookupResult.D);
  if (Method->getType()->is<ErrorType>())
    return std::pair<FuncDecl *, Type>();

  // Check that the type of the 'convertFrom*Literal' method makes
  // sense.  We want a type of "S -> DestTy" where S is the expected type.
  AnyFunctionType *FT = Method->getType()->castTo<AnyFunctionType>();
  FT = FT->getResult()->castTo<AnyFunctionType>();
  
  // The result of the convert function must be the destination type.
  if (!FT->getResult()->isEqual(Ty)) {
    if (Loc.isValid()) {
      diagnose(Method->getLoc(),
               diag::literal_conversion_wrong_return_type, Ty, MethodName);
      diagnose(Loc, diag::while_converting_literal, Ty);
    }
    return std::pair<FuncDecl*, Type>();
  }
  
  // Get the argument type, ignoring single element tuples.
  Type ArgType = FT->getInput();
  
  // Look through single element tuples.
  if (TupleType *TT = ArgType->getAs<TupleType>())
    if (TT->getFields().size() == 1)
      ArgType = TT->getFields()[0].getType();

  // FIXME: This duplicated code will be unnecessary when we switch
  // literals over to formal protocols.
  if (!((LitTy == LiteralKind::Int && ArgType->is<BuiltinIntegerType>()) ||
        (LitTy == LiteralKind::Float && ArgType->is<BuiltinFloatType>()) ||
        (LitTy == LiteralKind::Char &&
         (ArgType->is<BuiltinIntegerType>() &&
          ArgType->castTo<BuiltinIntegerType>()->getBitWidth() == 32)) ||
        ((LitTy == LiteralKind::ASCIIString ||
          LitTy == LiteralKind::UTFString) &&
         isStringLiteralArg(ArgType))) &&
      (RequiresBuiltinArg ||
       !isLiteralCompatibleType(ArgType, Loc, LitTy, Complain, true).first)) {
    return { nullptr, nullptr };
  }

  return std::pair<FuncDecl*, Type>(Method, ArgType);
}

/// \brief Retrieve the default literal type for the given literal kind.
Type TypeChecker::getDefaultLiteralType(LiteralKind kind) {
  Type *type;
  const char *name;
  switch (kind) {
  case LiteralKind::Char:
    type = &CharacterLiteralType;
    name = "CharacterLiteralType";
    break;

  case LiteralKind::ASCIIString:
  case LiteralKind::UTFString:
    type = &StringLiteralType;
    name = "StringLiteralType";
      break;

  case LiteralKind::Float:
    type = &FloatLiteralType;
    name = "FloatLiteralType";
    break;

  case LiteralKind::Int:
    type = &IntLiteralType;
    name = "IntegerLiteralType";
    break;

  case LiteralKind::Array:
    type = &ArrayLiteralType;
    name = "Slice";
    break;

  case LiteralKind::Dictionary:
    type = &DictionaryLiteralType;
    name = "Dictionary";
    break;
  }

  // If we haven't found the type yet, look for it now.
  if (!*type) {
    *type = lookupGlobalType(*this, name);

    // Strip off one level of sugar; we don't actually want to print
    // IntegerLiteralType anywhere.
    if (type && *type) {
      if (auto typeAlias = dyn_cast<NameAliasType>(type->getPointer()))
        *type = typeAlias->getDecl()->getUnderlyingType();
    }
  }

  return *type;
}

Type TypeChecker::getDefaultLiteralType(LiteralExpr *E) {
  if (isa<IntegerLiteralExpr>(E)) {
    if (!IntLiteralType) {
      IntLiteralType = getDefaultLiteralType(LiteralKind::Int);
      if (!IntLiteralType) {
        diagnose(E->getLoc(), diag::no_IntegerLiteralType_found);
        IntLiteralType = BuiltinIntegerType::get(32, Context);
      }
    }
    return IntLiteralType;
  }
  
  if (isa<FloatLiteralExpr>(E)) {
    if (!FloatLiteralType) {
      FloatLiteralType = getDefaultLiteralType(LiteralKind::Float);
      if (!FloatLiteralType) {
        diagnose(E->getLoc(), diag::no_FloatLiteralType_found);
        FloatLiteralType = Context.TheIEEE64Type;
      }
    }
    return FloatLiteralType;
  }
  
  if (isa<CharacterLiteralExpr>(E)) {
    if (!CharacterLiteralType) {
      CharacterLiteralType = getDefaultLiteralType(LiteralKind::Char);
      if (!CharacterLiteralType) {
        diagnose(E->getLoc(), diag::no_CharacterLiteralType_found);
        CharacterLiteralType = BuiltinIntegerType::get(32, Context);
      }
    }
    return CharacterLiteralType;
  }
  
  assert((isa<StringLiteralExpr>(E) || isa<InterpolatedStringLiteralExpr>(E)) &&
         "Unknown literal type");
  if (!StringLiteralType) {
    StringLiteralType = getDefaultLiteralType(LiteralKind::UTFString);
    if (!StringLiteralType) {
      diagnose(E->getLoc(), diag::no_StringLiteralType_found);
      StringLiteralType = Context.TheRawPointerType;
    }
  }
  return StringLiteralType;
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
                                  ArrayRef<Pattern*> paramPatterns,
                                  bool isFirstPass,
                                  bool allowUnknownTypes) {
  bool badType = false;
  for (Pattern *P : paramPatterns) {
    if (P->hasType())
      continue;
    if (checker->typeCheckPattern(P, isFirstPass, allowUnknownTypes)) {
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
  
  badType = badType || semaFuncParamPatterns(this, FE->getArgParamPatterns(),
                                             isFirstPass, allowUnknownTypes);
  badType = badType || semaFuncParamPatterns(this, FE->getBodyParamPatterns(),
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

