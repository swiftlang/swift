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
#include "swift/AST/ASTVisitor.h"
#include "swift/AST/Attr.h"
#include "llvm/ADT/APInt.h"
#include "llvm/ADT/PointerUnion.h"
#include "llvm/ADT/Twine.h"
using namespace swift;

/// isIntLiteralCompatibleType - Check to see if the specified type has a
/// properly defined integer literal conversion function, emiting an error and
/// returning null if not.  If everything looks kosher, return the conversion
/// function and the argument type that it expects.
static std::pair<FuncDecl*, Type> isIntLiteralCompatibleType(Type IntTy,
                                                             SourceLoc Loc,
                                                             TypeChecker *TC) {
  // Look up the convert_from_integer_literal method on the type.  If it is
  // missing, then the type isn't compatible with integer literals.  If it is
  // present, it must have a single argument of builtin integer type specifying
  // the bitwidth of the allowed value.
  SmallVector<ValueDecl*, 8> Methods;
  TC->TU.lookupGlobalExtensionMethods(IntTy, 
                      TC->Context.getIdentifier("convert_from_integer_literal"),
                                      Methods);
  
  if (Methods.empty()) {
    TC->diagnose(Loc, diag::type_not_compatible_int_literal, IntTy);
    return std::pair<FuncDecl*, Type>();
  }
  
  if (Methods.size() != 1) {
    TC->diagnose(Loc, diag::type_ambiguous_int_literal_conversion, IntTy);
    for (ValueDecl *D : Methods)
      TC->diagnose(D->getLocStart(), diag::found_candidate);
    return std::pair<FuncDecl*, Type>();
  }

  // Verify that the implementation is a metatype 'plus' func.
  FuncDecl *Method = dyn_cast<FuncDecl>(Methods[0]);
  if (Method == 0 || !Method->isPlus()) {
    TC->diagnose(Method->getLocStart(), diag::type_integer_conversion_not_plus,
                 IntTy);
    return std::pair<FuncDecl*, Type>();
  }
  
  // Check that the type of the 'convert_from_integer_literal' method makes
  // sense.  We want a type of "S -> DestTy" where S is an integer type.
  FunctionType *FT = Method->getType()->castTo<FunctionType>();
  
  // The result of the convert function must be the destination type.
  if (!FT->Result->isEqual(IntTy)) {
    TC->diagnose(Method->getLocStart(), 
                 diag::integer_conversion_wrong_return_type, IntTy);
    TC->diagnose(Loc, diag::while_converting_int_literal, IntTy);
    return std::pair<FuncDecl*, Type>();
  }
  
  // Get the argument type, ignoring single element tuples.
  Type ArgType = FT->Input;
    
  // Look through single element tuples.
  if (TupleType *TT = ArgType->getAs<TupleType>())
    if (TT->Fields.size() == 1)
      ArgType = TT->Fields[0].Ty;
  
  return std::pair<FuncDecl*, Type>(Method, ArgType);
}

/// applyTypeToInteger - Apply the specified type to the integer literal
/// expression (which is known to have dependent type), performing semantic
/// analysis and returning null on a semantic error or the new AST to use on
/// success.
Expr *TypeChecker::applyTypeToInteger(IntegerLiteralExpr *E, Type DestTy) {
  assert(E->getType()->is<DependentType>() &&
         "should only be called on dependent integers");

   // Check the destination type to see if it is compatible with integer
  // literals, diagnosing the failure if not.
  std::pair<FuncDecl*, Type> IntInfo =
    isIntLiteralCompatibleType(DestTy, E->getLoc(), this);
  FuncDecl *Method = IntInfo.first;
  Type ArgType = IntInfo.second;
  if (Method == 0) return 0;
  
  // The argument type must either be a Builtin:: integer type (in which case
  // this is an integer type in the standard library) or some other type that
  // itself has a conversion function from a builtin integer type (in which case
  // we have "chaining", and an implicit conversion through that type).
  Expr *Intermediate;  
  if (BuiltinIntegerType *BIT = ArgType->getAs<BuiltinIntegerType>()) {
    // If this is a direct use of the builtin integer type, use the integer size
    // to diagnose excess precision issues.
    llvm::APInt Value(1, 0);
    bool Failure = E->getText().getAsInteger(0, Value);  (void)Failure;
    assert(!Failure && "Lexer should have verified a reasonable type!");
    
    if (Value.getActiveBits() > BIT->getBitWidth()) {
      diagnose(E->getLoc(), diag::int_literal_too_large, Value.getBitWidth(),
               DestTy);
      return 0;
    }
    
    // Give the integer literal the builtin integer type.
    E->setType(ArgType, ValueKind::RValue);
    Intermediate = E;
  } else {
    // Check to see if this is the chaining case, where ArgType itself has a
    // conversion from a Builtin integer type.
    IntInfo = isIntLiteralCompatibleType(ArgType, E->getLoc(), this);
    if (IntInfo.first == 0) {
      diagnose(Method->getLocStart(),
               diag::while_processing_literal_conversion_function, DestTy);
      return 0;
    }
    
    if (!IntInfo.second->is<BuiltinIntegerType>()) {
      diagnose(Method->getLocStart(),
               diag::type_integer_conversion_defined_wrong, DestTy);
      diagnose(E->getLoc(), diag::while_converting_int_literal, DestTy);
      return 0;
    }
    
    // If this a 'chaining' case, recursively convert the integer literal to the
    // intermediate type, then use our conversion function to finish the
    // translation.
    Intermediate = applyTypeToInteger(E, ArgType);
    if (Intermediate == 0) return 0;
    
    // Okay, now Intermediate is known to have type 'ArgType' so we can use a
    // call to our conversion function to finish things off.
  }
      
  DeclRefExpr *DRE =
    new (Context) DeclRefExpr(Method,
                              // FIXME: This location is a hack!
                              Intermediate->getStartLoc(),
                              Method->getTypeJudgement());
  
  // Return a new call of the conversion function, passing in the integer
  // literal.
  return new (Context) CallExpr(DRE, Intermediate,
                                TypeJudgement(DestTy, ValueKind::RValue));
}

//===----------------------------------------------------------------------===//
// Expression Semantic Analysis Routines
//===----------------------------------------------------------------------===//

Expr *TypeChecker::convertToRValue(Expr *E) {
  assert(E && "no expression to load!");

  // Fast path: already an r-value.
  if (E->getValueKind() == ValueKind::RValue) return E;

  return new (Context) LoadExpr(E);
}

bool TypeChecker::semaTupleExpr(TupleExpr *TE) {
  // A tuple expr with a single subexpression and no name is just a grouping
  // paren.
  if (TE->isGroupingParen()) {
    TE->setType(TE->getElement(0)->getTypeJudgement());
    return false;
  }
  
  // Compute the result type.
  SmallVector<TupleTypeElt, 8> ResultTyElts(TE->getNumElements());

  // A tuple is an r-value if any sub-expression is an r-value or
  // if there are no sub-expressions at all.
  bool ResultIsRValue = (TE->getNumElements() == 0);
  
  for (unsigned i = 0, e = TE->getNumElements(); i != e; ++i) {
    bool EltIsRValue;
    
    // If the element value is missing, it has the value of the default
    // expression of the result type, which must be known.
    Expr *Elt = TE->getElement(i);
    if (Elt == 0) {
      assert(TE->getType() && isa<TupleType>(TE->getType()) && 
             "Can't have default value without a result type");
      
      ResultTyElts[i].Ty =
        cast<TupleType>(TE->getType())->getElementType(i);

      // Default values are always r-values.
      EltIsRValue = true;
      
      // FIXME: What about a default value that is dependent?
      if (ResultTyElts[i].Ty->is<DependentType>()) {
        TE->setDependentType(ResultTyElts[i].Ty);
        return false;
      }
    } else {
      // If any of the tuple element types is dependent, the whole tuple should
      // have dependent type.
      if (Elt->getType()->is<DependentType>()) {
        TE->setDependentType(Elt->getType());
        return false;
      }
      
      ResultTyElts[i].Ty = Elt->getType();
      EltIsRValue = (Elt->getValueKind() == ValueKind::RValue);
    }

    // If we're known to be building an r-value, and the element is an
    // l-value, convert it.
    if (ResultIsRValue && !EltIsRValue) {
      TE->setElement(i, convertToRValue(Elt));

    // If we thought we were building an l-value, and the element is
    // an r-value, convert all the previous elements.
    } else if (!ResultIsRValue && EltIsRValue) {
      ResultIsRValue = true;
      for (unsigned j = 0; j != i; ++j) {
        Expr *E = TE->getElement(j);
        if (E) TE->setElement(j, convertToRValue(E));
      }
    }

    // If a name was specified for this element, use it.
    ResultTyElts[i].Name = TE->getElementName(i);
  }
  
  TE->setType(TupleType::get(ResultTyElts, Context),
              ResultIsRValue ? ValueKind::RValue : ValueKind::LValue);
  return false;
}

bool TypeChecker::semaApplyExpr(ApplyExpr *E) {
  Expr *E1 = E->getFn();
  Expr *E2 = E->getArg();
  
  // If we have a concrete function type, then we win.
  if (FunctionType *FT = E1->getType()->getAs<FunctionType>()) {
    // If this is an operator, make sure that the declaration found was declared
    // as such.
    if (isa<UnaryExpr>(E) &&
        !cast<DeclRefExpr>(E1)->getDecl()->isOperator()) {
      diagnose(E1->getLoc(), diag::unary_op_without_attribute);
      return true;
    }
    
    if (isa<BinaryExpr>(E) &&
        !cast<DeclRefExpr>(E1)->getDecl()->getAttrs().isInfix()) {
      diagnose(E1->getLoc(), diag::binary_op_without_attribute);
      return true;
    }
    
    // We have a function application.  Check that the argument type matches the
    // expected type of the function.
    E2 = convertToType(E2, FT->Input);
    if (E2 == 0) {
      diagnose(E1->getLoc(), diag::while_converting_function_argument,
               FT->Input);
      return true;
    }
    
    E->setArg(E2);
    E->setType(FT->Result, ValueKind::RValue);
    return false;
  }
  
  // Otherwise, the function's type must be dependent.  If it is something else,
  // we have a type error.
  if (!E1->getType()->is<DependentType>()) {
    diagnose(E1->getLoc(), diag::called_expr_isnt_function);
    return true;
  }
  
  // Okay, if the argument is also dependent, we can't do anything here.  Just
  // wait until something gets resolved.
  if (E2->getType()->is<DependentType>()) {
    E->setDependentType(E2->getType());
    return false;
  }
  
  // Okay, we have a typed argument and untyped function.  See if we can infer
  // a type for that function.
  
  // Otherwise, we must have an application to an overload set.  See if we can
  // resolve which overload member is based on the argument type.
  OverloadSetRefExpr *OS = dyn_cast<OverloadSetRefExpr>(E1);
  if (!OS) {
    // If not, just use the dependent type.
    E->setType(E1->getType(), ValueKind::RValue);
    return false;
  }
  
  ValueDecl *BestCandidateFound = 0;
  Expr::ConversionRank BestRank = Expr::CR_Invalid;
  
  for (ValueDecl *Fn : OS->Decls) {
    Type ArgTy = Fn->getType()->castTo<FunctionType>()->Input;
    // If we found an exact match, disambiguate the overload set.
    Expr::ConversionRank Rank = E2->getRankOfConversionTo(ArgTy);
    
    // If this conversion is worst than our best candidate, ignore it.
    if (Rank > BestRank)
      continue;
    
    // If this is better than our previous candidate, use it!
    if (Rank < BestRank) {
      BestRank = Rank;
      BestCandidateFound = Fn;
      continue;
    }
    
    // Otherwise, this is a repeat of an existing candidate with the same
    // rank.  This means that the candidates at this rank are ambiguous with
    // respect to each other, so none can be used.  If something comes along
    // with a lower rank we can use it though.
    BestCandidateFound = 0;
  }
  
  // If we found a successful match, resolve the overload set to it and continue
  // type checking.
  if (BestCandidateFound) {
    E1 = new (Context) DeclRefExpr(BestCandidateFound, OS->getLoc(),
                                   BestCandidateFound->getTypeJudgement());
    E->setFn(E1);
    return semaApplyExpr(E);
  }
  
  // Otherwise we have either an ambiguity between multiple possible candidates
  // or not candidate at all.
  if (BestRank != Expr::CR_Invalid)
    diagnose(E1->getLoc(), diag::overloading_ambiguity);
  else if (isa<BinaryExpr>(E))
    diagnose(E1->getLoc(), diag::no_candidates, 0);
  else if (isa<UnaryExpr>(E))
    diagnose(E1->getLoc(), diag::no_candidates, 1);
  else
    diagnose(E1->getLoc(), diag::no_candidates, 2);
  
  // Print out the candidate set.
  for (auto TheDecl : OS->Decls) {
    Type ArgTy = TheDecl->getType()->castTo<FunctionType>()->Input;
    if (E2->getRankOfConversionTo(ArgTy) != BestRank)
      continue;
    diagnose(TheDecl->getLocStart(), diag::found_candidate);
  }
  return true;
}

//===----------------------------------------------------------------------===//
// Expression Reanalysis - SemaExpressionTree
//===----------------------------------------------------------------------===//

namespace {
/// SemaExpressionTree - This class implements bottom-up (aka "leaf to root",
/// analyzing 1 and 4 before the + in "1+4") semantic analysis of an
/// already-existing expression tree.  This is performed when a closure is
/// formed and anonymous decls like "$4" get a concrete type associated with
/// them.  During the initial parse, these decls get a 'dependent' type, which
/// disables most semantic analysis associated with them.
///
/// When the expression tree is bound to a context, the anonymous decls get a
/// concrete type and we have to rescan the tree to assign types to
/// intermediate nodes, introduce type coercion etc.  This visitor does this
/// job.  Each visit method reanalyzes the children of a node, then reanalyzes
/// the node, and returns true on error.
class SemaExpressionTree : public ExprVisitor<SemaExpressionTree, Expr*> {
public:
  TypeChecker &TC;
  
  Expr *visitIntegerLiteralExpr(IntegerLiteralExpr *E) {
    if (E->getType().isNull())
      E->setDependentType(DependentType::get(TC.Context));
    return E;
  }
  Expr *visitFloatLiteralExpr(FloatLiteralExpr *E) {
    return E;
  }
  Expr *visitDeclRefExpr(DeclRefExpr *E) {
    if (E->getDecl() == 0) {
      TC.diagnose(E->getLoc(), diag::use_undeclared_identifier);
      return 0;
    }
    
    // If the decl had an invalid type, then an error has already been emitted,
    // just propagate it up.
    if (E->getDecl()->getType()->is<ErrorType>())
      return 0;
    
    // TODO: QOI: If the decl had an "invalid" bit set, then return the error
    // object to improve error recovery.
    E->setType(E->getDecl()->getTypeJudgement());
    return E;
  }
  Expr *visitOverloadSetRefExpr(OverloadSetRefExpr *E) {
    E->setDependentType(DependentType::get(TC.Context));
    return E;
  }
  Expr *visitUnresolvedDeclRefExpr(UnresolvedDeclRefExpr *E) {
    llvm_unreachable("name binding should resolve all UnresolvedDeclRefExprs!");
    return 0;
  }
  Expr *visitUnresolvedMemberExpr(UnresolvedMemberExpr *E) {
    E->setDependentType(DependentType::get(TC.Context));
    return E;
  }
  
  Expr *visitTupleExpr(TupleExpr *E) {
    if (TC.semaTupleExpr(E))
      return 0;
    return E;
  }
  Expr *visitUnresolvedDotExpr(UnresolvedDotExpr *E);
  
  Expr *visitUnresolvedScopedIdentifierExpr
  (UnresolvedScopedIdentifierExpr *E) {
    llvm_unreachable("UnresolvedScopedIdentifierExpr should be resolved "
                     "by name binding!");
  }

  Expr *visitLookThroughOneofExpr(LookThroughOneofExpr *E) {
    llvm_unreachable("type-checking LookThroughOneofExpr?");
  }

  Expr *visitTupleElementExpr(TupleElementExpr *E) {
    // TupleElementExpr is fully resolved.
    assert(!E->getType()->is<DependentType>());
    return E;
  }
  
  Expr *visitTupleShuffleExpr(TupleShuffleExpr *E) {
    // TupleShuffleExpr is fully resolved.
    assert(!E->getType()->is<DependentType>());
    return E;
  }
  
  Expr *visitDotSyntaxPlusFuncUseExpr(DotSyntaxPlusFuncUseExpr *E) {
    // DotSyntaxPlusFuncUseExpr is fully type checked.
    return E;
  }

  Expr *visitLoadExpr(LoadExpr *E) {
    // LoadExpr is fully checked.
    return E;
  }
  
  Expr *visitSequenceExpr(SequenceExpr *E);
  
  void PreProcessBraceStmt(BraceStmt *BS);
  
  Expr *visitFuncExpr(FuncExpr *E) {
    return E;
  }
  
  Expr *visitClosureExpr(ClosureExpr *E) {
    assert(0 && "Should not walk into ClosureExprs!");
    return 0;
  }
  
  Expr *visitAnonClosureArgExpr(AnonClosureArgExpr *E) {
    // Nothing we can do here.  These remain as resolved or unresolved as they
    // always were.  If no type is assigned, we give them a dependent type so
    // that we get resolution later.
    if (E->getType().isNull())
      E->setDependentType(DependentType::get(TC.Context));
    return E;
  }
  
  
  Expr *visitApplyExpr(ApplyExpr *E) {
    return TC.semaApplyExpr(E) ? 0 : E;
  }
  Expr *visitCallExpr(CallExpr *E) { return visitApplyExpr(E); }
  Expr *visitUnaryExpr(UnaryExpr *E) { return visitApplyExpr(E); }
  Expr *visitBinaryExpr(BinaryExpr *E) {
    // This is necessary because ExprWalker doesn't visit the
    // TupleExpr right now.
    if (TC.semaTupleExpr(E->getArgTuple()))
      return 0;
    return visitApplyExpr(E);
  }
  Expr *visitDotSyntaxCallExpr(DotSyntaxCallExpr *E) {
    return visitApplyExpr(E);
  }

  Expr *lookThroughOneofs(Expr *E) {
    OneOfType *oneof = E->getType()->castTo<OneOfType>();
    assert(oneof->isTransparentType());
    TypeJudgement TJ(oneof->getTransparentType(), E->getValueKind());
    return new (TC.Context) LookThroughOneofExpr(E, TJ);
  }
  
  SemaExpressionTree(TypeChecker &tc) : TC(tc) {}
  

  Expr *doIt(Expr *E) {
    return E->walk(^Expr*(Expr *E, WalkOrder Order, WalkContext const&) {
      // This is implemented as a postorder walk.
      if (Order == WalkOrder::PreOrder) {
        // Do not walk into ClosureExpr.  Anonexprs within a nested closures
        // will have already been resolved, so we don't need to recurse into it.
        // This also prevents N^2 re-sema activity with lots of nested closures.
        if (isa<ClosureExpr>(E) || isa<FuncExpr>(E)) return 0;
        
        return E;
      }
      
      // Dispatch to the right visitor case in the post-order walk.  We know
      // that the operands have already been processed and are valid.
      return this->visit(E);
    }, ^Stmt*(Stmt *S, WalkOrder Order, WalkContext const& WalkCtx) {
      // Never recurse into statements.
      return 0;
    });
  }
};
} // end anonymous namespace.

Expr *SemaExpressionTree::visitUnresolvedDotExpr(UnresolvedDotExpr *E) {
  Expr *Base = E->getBase();
  Type SubExprTy = Base->getType();
   
  // Check in the context of a protocol.
  if (ProtocolType *PT = SubExprTy->getAs<ProtocolType>()) {
    for (ValueDecl *VD : PT->Elements) {
      if (VD->getName() != E->getName()) continue;
      
      // The protocol value is applied via a DeclRefExpr.
      DeclRefExpr *Fn = new (TC.Context) DeclRefExpr(VD, E->getNameLoc(),
                                                     VD->getTypeJudgement());
      
      if (FuncDecl *FD = dyn_cast<FuncDecl>(VD))
        if (FD->isPlus())
          return new (TC.Context) DotSyntaxPlusFuncUseExpr(Base, E->getDotLoc(),
                                                           Fn);
      
      ApplyExpr *Call = new (TC.Context) 
        DotSyntaxCallExpr(Fn, E->getDotLoc(), Base);
      if (TC.semaApplyExpr(Call))
        return 0;
      return Call;
    }
  }
  
  // Look in any extensions that add methods to the base type.
  SmallVector<ValueDecl*, 8> ExtensionMethods;
  TC.TU.lookupGlobalExtensionMethods(SubExprTy, E->getName(),ExtensionMethods);
  
  Expr *FnRef = 0;
  switch (ExtensionMethods.size()) {
  case 0: break;
  case 1:
    // Apply the base value to the function there is a single candidate in the
    // set then this is directly resolved.
    FnRef = new (TC.Context) DeclRefExpr(ExtensionMethods[0],
                                         E->getNameLoc(),
                                       ExtensionMethods[0]->getTypeJudgement());
      
    if (FuncDecl *FD = dyn_cast<FuncDecl>(ExtensionMethods[0]))
      if (FD->isPlus())
        return new (TC.Context) DotSyntaxPlusFuncUseExpr(Base, E->getDotLoc(),
                                                      cast<DeclRefExpr>(FnRef));
    break;
  default:
    // Otherwise it is an overload case.  
    FnRef = new (TC.Context) OverloadSetRefExpr(
                                    TC.Context.AllocateCopy(ExtensionMethods),
                                                E->getNameLoc());
    FnRef->setDependentType(DependentType::get(TC.Context));
    break;
  }
  
  if (FnRef) {
    ApplyExpr *Call = new (TC.Context) DotSyntaxCallExpr(FnRef, E->getDotLoc(),
                                                         Base);
    if (TC.semaApplyExpr(Call))
      return 0;
    
    return Call;
  }
  
  // If this is a member access to a oneof with a single element constructor
  // (e.g. a struct), allow direct access to the type underlying the single
  // element.
  bool LookedThroughOneofs = false;
  if (OneOfType *OneOf = SubExprTy->getAs<OneOfType>())
    if (OneOf->isTransparentType()) {
      SubExprTy = OneOf->getTransparentType();
      LookedThroughOneofs = true;
    }
  
  // Check to see if this is a reference to a tuple field.
  if (TupleType *TT = SubExprTy->getAs<TupleType>()) {
    // If the field name exists, we win.
    int FieldNo = TT->getNamedElementId(E->getName());
    if (FieldNo != -1) {
      if (LookedThroughOneofs)
        Base = lookThroughOneofs(Base);
      
      return new (TC.Context) 
        TupleElementExpr(Base, E->getDotLoc(), (unsigned) FieldNo,
                         E->getNameLoc(),
                         TypeJudgement(TT->getElementType(FieldNo),
                                       Base->getValueKind()));
    }
    
    // Okay, the field name was invalid.  If this is a dollarident like $4,
    // process it as a field index.
    if (E->getName().str().startswith("$")) {
      unsigned Value = 0;
      if (!E->getName().str().substr(1).getAsInteger(10, Value)) {
        if (Value >= TT->Fields.size()) {
          TC.diagnose(E->getNameLoc(), diag::field_number_too_large);
          return 0;
        }
        
        if (LookedThroughOneofs)
          Base = lookThroughOneofs(Base);
        
        return new (TC.Context) 
          TupleElementExpr(Base, E->getDotLoc(), Value,
                           E->getNameLoc(),
                           TypeJudgement(TT->getElementType(Value),
                                         Base->getValueKind()));
      }
    }
  }
  
  if (SubExprTy->is<DependentType>()) {
    E->setDependentType(SubExprTy);
    return E;
  }

  // FIXME: This diagnostic is a bit painful. Plus, fix the source range when
  // expressions actually have source ranges.
  TC.diagnose(E->getNameLoc(), diag::no_valid_dot_expression, SubExprTy)
    << Base->getSourceRange();
  return 0;
}


/// getInfixData - If the specified expression is an infix binary
/// operator, return its precedence.
static InfixData getInfixData(TypeChecker &TC, Expr *E) {
  if (DeclRefExpr *DRE = dyn_cast<DeclRefExpr>(E)) {
    if (DRE->getDecl()->getAttrs().isInfix())
      return DRE->getDecl()->getAttrs().getInfixData();

    TC.diagnose(DRE->getLoc(), diag::binop_not_infix);

  // If this is an overload set, the entire overload set is required
  // to have the same infix data.
  } else if (OverloadSetRefExpr *OO = dyn_cast<OverloadSetRefExpr>(E)) {

    ValueDecl *FirstDecl = nullptr;
    InfixData Infix;
    for (auto D : OO->Decls) {
      // It is possible some unary operators got mixed into the overload set.
      if (!D->getAttrs().isInfix())
        continue;
      
      if (Infix.isValid() && Infix != D->getAttrs().getInfixData()) {
        TC.diagnose(OO->getLoc(), diag::binop_mismatched_infix);
        TC.diagnose(FirstDecl->getLocStart(), diag::first_declaration);
        TC.diagnose(D->getLocStart(), diag::second_declaration);
        return Infix;
      }
      
      Infix = D->getAttrs().getInfixData();
      FirstDecl = D;
    }

    if (Infix.isValid())
      return Infix;

    TC.diagnose(OO->getLoc(), diag::binop_not_overloaded);

  // Otherwise, complain.
  } else {
    TC.diagnose(E->getLoc(), diag::unknown_binop);
  }
  
  // Recover with an infinite-precedence left-associative operator.
  return InfixData(~0U, Associativity::Left);
}

static Expr *makeBinOp(TypeChecker &TC, Expr *Op, Expr *LHS, Expr *RHS) {
  Expr *ArgElts[] = { LHS, RHS };
  Expr **ArgElts2 = TC.Context.AllocateCopy<Expr*>(ArgElts, ArgElts+2);
  TupleExpr *Arg = new (TC.Context) TupleExpr(SourceLoc(), 
                                              ArgElts2, 0, 2, SourceLoc(),
                                              false);
  return new (TC.Context) BinaryExpr(Op, Arg);
}

/// foldSequence - Take a sequence of expressions and fold a prefix of
/// it into a tree of BinaryExprs using precedence parsing.
static Expr *foldSequence(TypeChecker &TC, Expr *LHS, ArrayRef<Expr*> &S,
                          unsigned MinPrecedence) {
  // Invariant: S is even-sized.
  // Invariant: All elements at even indices are operator references.
  assert(!S.empty());
  assert((S.size() & 1) == 0);

  // Extract out the first operator.  If its precedence is lower
  // than the minimum, stop here.
  Expr *Op1 = S[0];
  InfixData Op1Info = getInfixData(TC, Op1);
  if (Op1Info.getPrecedence() < MinPrecedence) return LHS;

  // We will definitely be consuming at least one operator.
  // Pull out the prospective RHS and slice off the first two elements.
  Expr *RHS = S[1];
  S = S.slice(2);

  while (!S.empty()) {
    assert(!S.empty());
    assert((S.size() & 1) == 0);
    assert(Op1Info.getPrecedence() >= MinPrecedence);

    // Pull out the next binary operator.
    Expr *Op2 = S[0];
    InfixData Op2Info = getInfixData(TC, Op2);

    // If the second operator's precedence is lower than the min
    // precedence, break out of the loop.
    if (Op2Info.getPrecedence() < MinPrecedence) break;

    // If the first operator's precedence is higher than the second
    // operator's precedence, or they have matching precedence and are
    // both left-associative, fold LHS and RHS immediately.
    if (Op1Info.getPrecedence() > Op2Info.getPrecedence() ||
        (Op1Info == Op2Info && Op1Info.isLeftAssociative())) {
      LHS = makeBinOp(TC, Op1, LHS, RHS);
      RHS = S[1];
      Op1 = Op2;
      Op1Info = Op2Info;
      S = S.slice(2);
      continue;
    }

    // If the first operator's precedence is lower than the second
    // operator's precedence, recursively fold all such
    // higher-precedence operators starting from this point, then
    // repeat.
    if (Op1Info.getPrecedence() < Op2Info.getPrecedence()) {
      RHS = foldSequence(TC, RHS, S, Op1Info.getPrecedence() + 1);
      continue;
    }

    // If the first operator's precedence is the same as the second
    // operator's precedence, and they're both right-associative,
    // recursively fold operators starting from this point, then
    // immediately fold LHS and RHS.
    if (Op1Info == Op2Info && Op1Info.isRightAssociative()) {
      RHS = foldSequence(TC, RHS, S, Op1Info.getPrecedence());
      LHS = makeBinOp(TC, Op1, LHS, RHS);

      // If we've drained the entire sequence, we're done.
      if (S.empty()) return LHS;

      // Otherwise, start all over with our new LHS.
      return foldSequence(TC, LHS, S, MinPrecedence);
    }

    // If we ended up here, it's because we have two operators
    // with mismatched or no associativity.
    assert(Op1Info.getPrecedence() == Op2Info.getPrecedence());
    assert(Op1Info.getAssociativity() != Op2Info.getAssociativity() ||
           Op1Info.isNonAssociative());

    if (Op1Info.isNonAssociative()) {
      // FIXME: QoI ranges
      TC.diagnose(Op1->getLoc(), diag::non_assoc_adjacent);
    } else if (Op2Info.isNonAssociative()) {
      TC.diagnose(Op2->getLoc(), diag::non_assoc_adjacent);
    } else {
      TC.diagnose(Op1->getLoc(), diag::incompatible_assoc);
    }
    
    // Recover by arbitrarily binding the first two.
    LHS = makeBinOp(TC, Op1, LHS, RHS);
    return foldSequence(TC, LHS, S, MinPrecedence);
  }

  // Fold LHS and RHS together and declare completion.
  return makeBinOp(TC, Op1, LHS, RHS);
}

/// foldSequence - Take a SequenceExpr and fold it into a tree of
/// BinaryExprs using precedence parsing.
Expr *TypeChecker::foldSequence(SequenceExpr *E) {
  ArrayRef<Expr*> Elts = E->getElements();
  assert(Elts.size() > 1 && "inadequate number of elements in sequence");
  assert((Elts.size() & 1) == 1 && "even number of elements in sequence");

  Expr *LHS = Elts[0];
  Elts = Elts.slice(1);

  Expr *Result = ::foldSequence(*this, LHS, Elts, /*min precedence*/ 0);
  assert(Elts.empty());
  return Result;
}

Expr *SemaExpressionTree::visitSequenceExpr(SequenceExpr *E) {
  llvm_unreachable("visiting sequence expression during normal type-checking!");
}

bool TypeChecker::typeCheckExpression(Expr *&E, Type ConvertType) {
  SemaExpressionTree SET(*this);
  E = SET.doIt(E);

  if (E == 0) return true;

  // If our context specifies a type, apply it to the expression.
  if (ConvertType) {
    E = convertToType(E, ConvertType);
    if (E == 0) return true;
  }
  
  // Check the initializer/body to make sure that we succeeded in resolving
  // all of the types contained within it.  If we've resolved everything, then
  // we're done processing the expression.  While we're doing the walk, keep
  // track of whether we have any literals without a resolved type.
  __block Expr *OneDependentExpr = 0;
  __block SourceLoc HasDependentLiterals;
  
  E = E->walk(^(Expr *E, WalkOrder Order, WalkContext const&) {
    // Ignore the preorder walk.  We'd rather diagnose use of unresolved types
    // during the postorder walk so that the inner most expressions are 
    // diagnosed before the outermost ones.
    if (Order == WalkOrder::PreOrder)
      return E;
    
    assert(!isa<SequenceExpr>(E) && "Should have resolved this");
    
    if (E->getType()->is<DependentType>()) {
      // Remember the first dependent expression we come across.
      if (OneDependentExpr == 0)
        OneDependentExpr = E;

      // If we have literals with dependent types, remember this.
      if (isa<IntegerLiteralExpr>(E) && !HasDependentLiterals.isValid())
        HasDependentLiterals = E->getStartLoc();
    }
    return E;
  }, ^Stmt*(Stmt *S, WalkOrder Order, WalkContext const&) {
    // Never recurse into statements.
    return 0;
  });

  // If we found any dependent literals, then force them to the library
  // specified default integer type (usually int64).
  if (HasDependentLiterals.isValid()) {
    // Lookup the "integer_literal_type" which is what all the unresolved
    // literals get implicitly coerced to.
    TypeAliasDecl *TAD = TU.lookupGlobalType(
                                  Context.getIdentifier("integer_literal_type"),
                                              NLKind::UnqualifiedLookup);
    if (TAD == 0) {
      diagnose(HasDependentLiterals, diag::no_integer_literal_type_found);
      E = 0;
      return true;
    }

    Type IntLiteralType = TAD->getAliasType();

    // Walk the tree again to update all the entries.
    E = E->walk(^(Expr *E, WalkOrder Order, WalkContext const&) {
      // Ignore the preorder walk.
      if (Order == WalkOrder::PreOrder)
        return E;

      // Process dependent IntegerLiteralExprs.
      if (IntegerLiteralExpr *ILE = dyn_cast<IntegerLiteralExpr>(E))
        if (E->getType()->is<DependentType>())
          return applyTypeToInteger(ILE, IntLiteralType);
      return E;
    }, ^Stmt*(Stmt *S, WalkOrder Order, WalkContext const&) {
      // Never recurse into statements.
      return 0;
    });

    if (E == 0)
      return true;
    
    // Now that we've added some types to the mix, re-type-check the expression
    // tree and recheck for dependent types.
    OneDependentExpr = 0;
    E = SET.doIt(E);
    if (E == 0) return true;
    
    E = E->walk(^(Expr *E, WalkOrder Order, WalkContext const&) {
      // Remember the first dependent expression we come across.
      if (Order != WalkOrder::PreOrder && E->getType()->is<DependentType>() &&
          OneDependentExpr == 0)
        OneDependentExpr = E;
      return E;
    }, ^Stmt*(Stmt *S, WalkOrder Order, WalkContext const& WalkCtx) {
      // Never recurse into statements.
      return 0;
    });
  }
  
  // If there are no dependent expressions, then we're done.
  if (OneDependentExpr == 0) return false;

  // Otherwise, emit an error about the ambiguity.
  // FIXME: QoI ranges.
  diagnose(OneDependentExpr->getStartLoc(),
           diag::ambiguous_expression_unresolved);
  E = 0;
  return true;
}
