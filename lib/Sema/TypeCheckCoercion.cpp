//===--- TypeCheckCoercion.cpp - Expression Coercion ---------------------------------===//
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
// This file implements semantic analysis for expression when its context
// implies a type returned by the expression.  This coerces the expression to
// that type.
//
//===----------------------------------------------------------------------===//

#include "TypeChecker.h"
#include "swift/AST/ASTVisitor.h"
#include "llvm/ADT/Twine.h"
using namespace swift;

namespace {
/// SemaCoerce - This class implements top-down semantic analysis (aka "root to
/// leaf", using the type of "+" to infer the type of "a" in "a+1") of an
/// already-existing expression tree.  This is performed when an expression with
/// dependent type is used in a context that forces a specific type.  
///
/// Each visit method reanalyzes the node to see if the type can be propagated
/// into it.  If not, it returns it.  If so it checks to see if the type
/// is contradictory (in which case it returns NULL) otherwise it applies the
/// type (possibly recursively) and returns the new/updated expression.
class SemaCoerce : public ExprVisitor<SemaCoerce, Expr*> {
public:
  TypeChecker &TC;
  Type DestTy;
  
  Expr *visitErrorExpr(ErrorExpr *E) {
    return E;
  }
  
  Expr *visitIntegerLiteralExpr(IntegerLiteralExpr *E) {
    return TC.applyTypeToLiteral(E, DestTy);
  }
  Expr *visitFloatLiteralExpr(FloatLiteralExpr *E) {
    return TC.applyTypeToLiteral(E, DestTy);
  }
  Expr *visitDeclRefExpr(DeclRefExpr *E) {
    return E;
  }

  static bool isMatchForLValue(ValueDecl *val, LValueType *lv) {
    return val->isReferencedAsLValue()
        && val->getType()->isEqual(lv->getObjectType());
  }

  Expr *coerceOverloadToLValue(OverloadSetRefExpr *E, LValueType *lv) {
    for (ValueDecl *val : E->getDecls()) {
      if (isMatchForLValue(val, lv))
        return new (TC.Context) DeclRefExpr(val, E->getLoc(), lv);
    }
    return E;
  }
  
  Expr *visitOverloadSetRefExpr(OverloadSetRefExpr *E) {
    // If we're looking for an lvalue type, we need an exact match
    // to the target object type.
    if (LValueType *lv = DestTy->getAs<LValueType>())
      return coerceOverloadToLValue(E, lv);

    // Otherwise, we can at the very least do lvalue-to-rvalue conversions
    // on the value.
    // FIXME: and other conversions as well; really this should consider
    //   conversion rank
    // FIXME: diagnose ambiguity
    for (ValueDecl *val : E->getDecls()) {
      Type srcTy = val->getType();
      if (!srcTy->isEqual(DestTy)) continue;
      return TC.buildDeclRefRValue(val, E->getLoc());
    }
    return E;
  }
  
  // If this is an UnresolvedMemberExpr, then this provides the type we've
  // been looking for!
  Expr *visitUnresolvedMemberExpr(UnresolvedMemberExpr *UME) {
    // The only valid type for an UME is a OneOfType.
    OneOfType *DT = DestTy->getAs<OneOfType>();
    if (DT == 0) {
      TC.diagnose(UME->getLoc(), diag::cannot_convert_dependent_reference,
                  UME->getName(), DestTy);
      return 0;
    }
    
    // The oneof type must have an element of the specified name.
    OneOfElementDecl *DED = DT->getElement(UME->getName());
    if (DED == 0) {
      TC.diagnose(UME->getLoc(), diag::invalid_member_in_type,
                  DestTy, UME->getName());
      TC.diagnose(DT->getOneOfLoc(), diag::type_declared_here);
      return 0;
    }
    
    // If it does, then everything is good, resolve the reference.
    return new (TC.Context) DeclRefExpr(DED, UME->getColonLoc(),
                                        DED->getType());
  }  
  
  Expr *visitParenExpr(ParenExpr *E) {
    assert(0 && "Already special cased in SemaCoerce::convertToType");
  }
    
  Expr *visitTupleExpr(TupleExpr *E) {
    return E;
  }
  
  Expr *visitUnresolvedDeclRefExpr(UnresolvedDeclRefExpr *E) {
    return E;
  }
  Expr *visitUnresolvedDotExpr(UnresolvedDotExpr *E) {
    return E;
  }

  Expr *visitTupleElementExpr(TupleElementExpr *E) {
    // TupleElementExpr is fully resolved.
    llvm_unreachable("This node doesn't exist for dependent types");
  }
  
  
  Expr *visitCallExpr(CallExpr *E) {
    // If we have ":f(x)" and the result type of the call is a OneOfType, then
    // :f must be an element constructor for the oneof value.  Note that
    // handling this syntactically causes us to reject "(:f)(x)" as ambiguous.
    if (UnresolvedMemberExpr *UME =
          dyn_cast<UnresolvedMemberExpr>(E->getFn())) {
      if (OneOfType *DT = DestTy->getAs<OneOfType>()) {
        // The oneof type must have an element of the specified name.
        OneOfElementDecl *DED = DT->getElement(UME->getName());
        if (DED == 0 || !DED->getType()->is<FunctionType>()) {
          TC.diagnose(UME->getLoc(), diag::invalid_type_to_initialize_member,
                      DestTy);
          return 0;
        }

        // FIXME: Preserve source locations.
        E->setFn(new (TC.Context) DeclRefExpr(DED, UME->getColonLoc(),
                                              DED->getType()));
        return TC.semaApplyExpr(E);
      }
    }
    
    // FIXME: Given a CallExpr a(b) where "a" is an overloaded value, we
    // may be able to prune the overload set based on the known result type.
    // Doing this may allow the ambiguity to resolve by removing candidates
    // that caused the ambiguity.  For example if we know that the destination
    // type is 'int', and we had "int -> int" and "SomeTy -> float", we can
    // prune the second one, and then recursively apply 'int' to b.
    return E;
  }
  Expr *visitSequenceExpr(SequenceExpr *E) {
    llvm_unreachable("SequenceExprs should all be resolved by this pass");
  }

  Expr *visitFuncExpr(FuncExpr *E) {
    return E;      
  }

  Expr *visitExplicitClosureExpr(ExplicitClosureExpr *E);

  Expr *visitImplicitClosureExpr(ImplicitClosureExpr *E) {
    return E;      
  }
  
  Expr *visitModuleExpr(ModuleExpr *E) {
    return E;
  }
  
  Expr *visitAnonClosureArgExpr(AnonClosureArgExpr *E) {
    return E;
  }

  Expr *visitUnaryExpr(UnaryExpr *E) {
    // TODO: If the function is an overload set and the result type that we're
    // coercing onto the binop is completely incompatible with some elements
    // of the overload set, trim them out.      
    return E;
  }

  Expr *visitBinaryExpr(BinaryExpr *E) {
    // TODO: If the function is an overload set and the result type that we're
    // coercing onto the binop is completely incompatible with some elements
    // of the overload set, trim them out.      
    return E;
  }

  Expr *visitConstructorCallExpr(ConstructorCallExpr *E) {
    return E;
  }

  Expr *visitDotSyntaxCallExpr(DotSyntaxCallExpr *E) {
    return E;
  }
  
  Expr *visitDotSyntaxBaseIgnoredExpr(DotSyntaxBaseIgnoredExpr *E) {
    // FIXME: Coerces the RHS.
    return E;
  }

  Expr *visitImplicitConversionExpr(ImplicitConversionExpr *E) {
    return E;
  }

  Expr *visitAddressOfExpr(AddressOfExpr *E) {
    LValueType *DestLT = DestTy->getAs<LValueType>();
    if (!DestLT) {
      TC.diagnose(E->getLoc(), diag::load_of_explicit_lvalue,
                  E->getType()->castTo<LValueType>()->getObjectType())
        << E->getSourceRange();
      return nullptr;
    }

    // Really minor optimization.
    if (!DestLT->isExplicit())
      return visit(E->getSubExpr());

    LValueType::Qual qs = DestLT->getQualifiers();
    qs |= LValueType::Qual::Implicit;

    Type NewDestTy = LValueType::get(DestLT->getObjectType(), qs, TC.Context);
    return convertToType(E, NewDestTy, TC);
  }

  SemaCoerce(TypeChecker &TC, Type DestTy) : TC(TC), DestTy(DestTy) {
    assert(!DestTy->is<DependentType>());
  }
  Expr *doIt(Expr *E) {
    return visit(E);
  }
  
  /// convertToType - This is the main entrypoint to SemaCoerce.
  static Expr *convertToType(Expr *E, Type DestTy, TypeChecker &TC);

  static Expr *convertScalarToTupleType(Expr *E, TupleType *DestTy,
                                        unsigned FieldNo, TypeChecker &TC);
  static Expr *
  convertTupleToTupleType(Expr *E, unsigned NumExprElements,
                          TupleType *DestTy, TypeChecker &TC);
};
} // end anonymous namespace.


Expr *SemaCoerce::visitExplicitClosureExpr(ExplicitClosureExpr *E) {
  // Make sure that we're converting the closure to a function type.  If not,
  // diagnose the error.
  FunctionType *FT = DestTy->getAs<FunctionType>();
  if (FT == 0) {
    TC.diagnose(E->getStartLoc(), diag::closure_not_function_type, DestTy)
    << E->getSourceRange();
    return 0;
  }

  // Now that we have a FunctionType for the closure, we can know how many
  // arguments are allowed.
  E->setType(FT);
  
  // If the input to the function is a non-tuple, only $0 is valid, if it is a
  // tuple, then $0..$N are valid depending on the number of inputs to the
  // tuple.
  unsigned NumInputArgs = 1;
  TupleType *FuncInputTT = dyn_cast<TupleType>(FT->getInput().getPointer());
  if (FuncInputTT)
    NumInputArgs = FuncInputTT->getFields().size();
    
  // Bind any anonymous closure arguments, validating them and resolving
  // their types.
  for (AnonClosureArgExpr *Arg = E->getClosureArgList(); Arg;
       Arg = Arg->getNextInClosure()) {
    assert((Arg->getType().isNull() || Arg->getType()->is<DependentType>()) &&
           "Anon arg already has a type?");
    
    // Verify that the argument number isn't too large, e.g. using $4 when the
    // bound function only has 2 inputs.
    if (Arg->getArgNumber() >= NumInputArgs) {
      TC.diagnose(Arg->getLoc(), diag::invalid_anonymous_argument,
                  Arg->getArgNumber(), NumInputArgs);
      Arg->setType(ErrorType::get(TC.Context));
      // TODO: We could turn this into error expr or something else to preserve
      // the AST.
      return 0;
    }
    
    Type NewArgType;
    if (FuncInputTT) {
      NewArgType = FuncInputTT->getElementType(Arg->getArgNumber());
    } else {
      assert(NumInputArgs == 1 && "Must have unary case");
      NewArgType = FT->getInput();
    }

    Arg->setType(LValueType::get(NewArgType, LValueType::Qual::DefaultForVar,
                                 TC.Context));
  }
  
  // Now that the AnonClosureArgExpr's have a type, redo semantic analysis of
  // the closure from the leaves of the expression tree up.
  Expr *Result = E->getBody();
  
  // Make sure that the body agrees with the result type of the closure.
  Result = TC.convertToType(Result, FT->getResult());
  if (Result == 0) {
    TC.diagnose(E->getStartLoc(), 
                diag::while_converting_closure_body_to_inferred_return_type,
                FT->getResult())
    << E->getBody()->getSourceRange();
    return 0;
  }

  // Type check the full expression, verifying that it is fully typed.
  if (TC.typeCheckExpression(Result, FT->getResult()))
    return 0;
  
  E->setBody(Result);
  return E;
}


/// convertTupleToTupleType - Given an expression that has tuple type, convert
/// it to have some other tuple type.
///
/// The caller gives us a list of the expressions named arguments and a count of
/// tuple elements for E in the IdentList+NumIdents array.  DestTy specifies the
/// type to convert to, which is known to be a TupleType.
Expr *
SemaCoerce::convertTupleToTupleType(Expr *E, unsigned NumExprElements,
                                    TupleType *DestTy, TypeChecker &TC){
  
  // If the tuple expression or destination type have named elements, we
  // have to match them up to handle the swizzle case for when:
  //   (.y = 4, .x = 3)
  // is converted to type:
  //   (.x = int, .y = int)
  SmallVector<Identifier, 8> IdentList(NumExprElements);
  
  // Check to see if this conversion is ok by looping over all the destination
  // elements and seeing if they are provided by the input.
  
  // Keep track of which input elements are used.
  // TODO: Record where the destination elements came from in the AST.
  SmallVector<bool, 16> UsedElements(NumExprElements);
  SmallVector<int, 16>  DestElementSources(DestTy->getFields().size(), -1);

  if (TupleType *ETy = E->getType()->getAs<TupleType>()) {
    assert(ETy->getFields().size() == NumExprElements && "#elements mismatch!");
    for (unsigned i = 0, e = ETy->getFields().size(); i != e; ++i)
      IdentList[i] = ETy->getFields()[i].getName();
    
    // First off, see if we can resolve any named values from matching named
    // inputs.
    for (unsigned i = 0, e = DestTy->getFields().size(); i != e; ++i) {
      const TupleTypeElt &DestElt = DestTy->getFields()[i];
      // If this destination field is named, first check for a matching named
      // element in the input, from any position.
      if (!DestElt.hasName()) continue;

      int InputElement = -1;
      for (unsigned j = 0; j != NumExprElements; ++j)
        if (IdentList[j] == DestElt.getName()) {
          InputElement = j;
          break;
        }
      if (InputElement == -1) continue;
      
      DestElementSources[i] = InputElement;
      UsedElements[InputElement] = true;
    }
  }
  
  // Next step, resolve (in order) unmatched named results and unnamed results
  // to any left-over unnamed input.
  unsigned NextInputValue = 0;
  for (unsigned i = 0, e = DestTy->getFields().size(); i != e; ++i) {
    // If we already found an input to satisfy this output, we're done.
    if (DestElementSources[i] != -1) continue;
    
    // Scan for an unmatched unnamed input value.
    while (1) {
      // If we didn't find any input values, we ran out of inputs to use.
      if (NextInputValue == NumExprElements)
        break;
      
      // If this input value is unnamed and unused, use it!
      if (!UsedElements[NextInputValue] && IdentList[NextInputValue].empty())
        break;
      
      ++NextInputValue;
    }
    
    // If we ran out of input values, we either don't have enough sources to
    // fill the dest (as in when assigning (1,2) to (int,int,int), or we ran out
    // and default values should be used.
    if (NextInputValue == NumExprElements) {
      if (DestTy->getFields()[i].hasInit()) {
        // If the default initializer should be used, leave the
        // DestElementSources field set to -2.
        DestElementSources[i] = -2;
        continue;
      }        
     
      // If this is a TupleExpr (common case) get a more precise location for
      // the element we care about.
      SourceLoc ErrorLoc = E->getStartLoc();
      if (TupleExpr *TE = dyn_cast<TupleExpr>(E))
        ErrorLoc = TE->getRParenLoc();
      
      if (!DestTy->getFields()[i].hasName())
        TC.diagnose(ErrorLoc, diag::not_initialized_tuple_element, i,
                    E->getType());
      else
        TC.diagnose(ErrorLoc, diag::not_initialized_named_tuple_element,
                    DestTy->getFields()[i].getName(), i, E->getType());
      return 0;
    }
    
    // Okay, we found an input value to use.
    DestElementSources[i] = NextInputValue;
    UsedElements[NextInputValue] = true;
  }
  
  // If there were any unused input values, we fail.
  for (unsigned i = 0, e = UsedElements.size(); i != e; ++i)
    if (!UsedElements[i]) {
      // If this is a TupleExpr (common case) get a more precise location for
      // the element we care about.
      SourceLoc ErrorLoc = E->getLoc();
      if (TupleExpr *TE = dyn_cast<TupleExpr>(E))
        if (Expr *SubExp = TE->getElement(i))
          ErrorLoc = SubExp->getLoc();
      
    if (IdentList[i].empty())
      TC.diagnose(ErrorLoc, diag::tuple_element_not_used, i, DestTy);
    else
      TC.diagnose(ErrorLoc, diag::named_tuple_element_not_used, IdentList[i],
                  i, DestTy);
      return 0;
    }
  
  // It looks like the elements line up, walk through them and see if the types
  // either agree or can be converted.  If the expression is a TupleExpr, we do
  // this conversion in place.
  TupleExpr *TE = dyn_cast<TupleExpr>(E);
  if (TE && TE->getNumElements() != 1 &&
      TE->getNumElements() == DestTy->getFields().size()) {
    SmallVector<Expr*, 8> OrigElts(TE->getElements().begin(),
                                   TE->getElements().end());
    
    for (unsigned i = 0, e = DestTy->getFields().size(); i != e; ++i) {
      // Extract the input element corresponding to this destination element.
      unsigned SrcField = DestElementSources[i];
      assert(SrcField != ~0U && "dest field not found?");
      
      // If SrcField is -2, then the destination element should use its default
      // value.
      if (SrcField == -2U) {
        TE->setElement(i, 0);
        continue;
      }
      
      // Check to see if the src value can be converted to the destination
      // element type.
      Expr *Elt = OrigElts[SrcField];
      
      Elt = convertToType(Elt, DestTy->getElementType(i), TC);
      // TODO: QOI: Include a note about this failure!
      if (Elt == 0) return 0;
      TE->setElement(i, Elt);
    }
    
    // Okay, we updated the tuple in place.
    E->setType(DestTy);
    return E;
  }
  
  // Otherwise, if it isn't a tuple literal, we unpack the source elementwise so
  // we can do elementwise conversions as needed, then rebuild a new TupleExpr
  // of the right destination type.
  TupleType *ETy = E->getType()->getAs<TupleType>();
  SmallVector<int, 16> NewElements(DestTy->getFields().size());
  
  for (unsigned i = 0, e = DestTy->getFields().size(); i != e; ++i) {
    // Extract the input element corresponding to this destination element.
    unsigned SrcField = DestElementSources[i];
    assert(SrcField != ~0U && "dest field not found?");
    
    if (SrcField == -2U) {
      // Use the default element for the tuple.
      NewElements[i] = -1;
      continue;
    }
    
    Type DestEltTy = DestTy->getElementType(i);
    
    if (ETy && !ETy->getElementType(SrcField)->isEqual(DestEltTy)) {
      TC.diagnose(E->getLoc(), diag::tuple_element_type_mismatch, i,
                  ETy->getElementType(SrcField),
                  DestTy->getElementType(i));
      return 0;
    }
    
    NewElements[i] = SrcField;
  }
  
  // If we got here, the type conversion is successful, create a new TupleExpr.  
  ArrayRef<int> Mapping = TC.Context.AllocateCopy(NewElements);
  
  return new (TC.Context) TupleShuffleExpr(E, Mapping, DestTy);
}

/// convertScalarToTupleType - Convert the specified expression to the specified
/// tuple type, which is known to be initializable with one element.
Expr *SemaCoerce::convertScalarToTupleType(Expr *E, TupleType *DestTy,
                                           unsigned ScalarField,
                                           TypeChecker &TC) {
  // If the destination is a tuple type with at most one element that has no
  // default value, see if the expression's type is convertable to the
  // element type.  This handles assigning 4 to "(a = 4, b : int)".
  Type ScalarType = DestTy->getElementType(ScalarField);
  Expr *ERes = convertToType(E, ScalarType, TC);
  if (ERes == 0) return 0;
  
  unsigned NumFields = DestTy->getFields().size();
  
  // Must allocate space for the AST node.
  MutableArrayRef<Expr*> NewSE(TC.Context.Allocate<Expr*>(NumFields),NumFields);
  
  bool NeedsNames = false;
  for (unsigned i = 0, e = NumFields; i != e; ++i) {
    if (i == (unsigned)ScalarField)
      NewSE[i] = ERes;
    else
      NewSE[i] = 0;
    
    NeedsNames |= DestTy->getFields()[i].hasName();
  }
  
  // Handle the name if the element is named.
  Identifier *NewName = 0;
  if (NeedsNames) {
    NewName = TC.Context.Allocate<Identifier>(NumFields);
    for (unsigned i = 0, e = NumFields; i != e; ++i)
      NewName[i] = DestTy->getFields()[i].getName();
  }
  
  return new (TC.Context) TupleExpr(SourceLoc(), NewSE, NewName, SourceLoc(),
                                    DestTy);
}

/// Would the source lvalue type be coercible to the dest lvalue type
/// if it were explicit?
static bool isSubtypeExceptImplicit(LValueType *SrcTy, LValueType *DestTy) {
  return DestTy->getObjectType()->isEqual(SrcTy->getObjectType())
      && SrcTy->getQualifiers().withoutImplicit() <= DestTy->getQualifiers();
}

/// convertToType - This is the recursive implementation of
/// ConvertToType.  It produces diagnostics and returns null on failure.
///
/// NOTE: This needs to be kept in synch with getConversionRank in Expr.cpp.
///
Expr *SemaCoerce::convertToType(Expr *E, Type DestTy, TypeChecker &TC) {
  assert(!DestTy->is<DependentType>() &&
         "Result of conversion can't be dependent");

  // If the destination is a AutoClosing FunctionType, we have special rules.
  if (FunctionType *FT = DestTy->getAs<FunctionType>())
    if (FT->isAutoClosure()) {
      // We require the expression to be an ImplicitClosureExpr that produces
      // DestTy.
      if (E->getType()->isEqual(DestTy) && isa<ImplicitClosureExpr>(E))
        return E;
      
      // If we don't have it yet, force the input to the result of the closure
      // and build the implicit closure.
      E = convertToType(E, FT->getResult(), TC);
      if (E == 0) return 0;
      return new (TC.Context) ImplicitClosureExpr(E, DestTy);
    }
  
  // If we have an exact match, we're done.
  if (E->getType()->isEqual(DestTy))
    return E;
  
  // If the expression is a grouping parenthesis and it has a dependent type,
  // just force the type through it, regardless of what DestTy is.
  if (ParenExpr *PE = dyn_cast<ParenExpr>(E)) {
    PE->setSubExpr(convertToType(PE->getSubExpr(), DestTy, TC));
    if (PE->getSubExpr() == 0) return 0;
    PE->setType(PE->getSubExpr()->getType());
    return PE;
  }

  if (LValueType *DestLT = DestTy->getAs<LValueType>()) {
    LValueType *SrcLT = E->getType()->getAs<LValueType>();

    // Qualification conversion.
    if (SrcLT && DestLT->getObjectType()->isEqual(SrcLT->getObjectType()) &&
        SrcLT->getQualifiers() <= DestLT->getQualifiers()) {
      assert(SrcLT->getQualifiers() < DestLT->getQualifiers() &&
             "qualifiers match exactly but types are different?");
      return new (TC.Context) RequalifyExpr(E, DestTy);
    }

    // Materialization.
    if (!DestLT->isExplicit()) {
      E = convertToType(E, DestLT->getObjectType(), TC);
      if (!E) return nullptr;

      return new (TC.Context) MaterializeExpr(E, DestTy);
    }

    // Just ignore dependent types for now.
    if (E->getType()->is<DependentType>()) return E;

    // Failure.

    // Use a special diagnostic if the coercion would have worked
    // except we needed an explicit marker.
    if (SrcLT && isSubtypeExceptImplicit(SrcLT, DestLT)) {
      TC.diagnose(E->getLoc(), diag::implicit_use_of_lvalue,
                  SrcLT->getObjectType())
        << E->getSourceRange();
      return nullptr;
    }

    // Use a special diagnostic for mismatched l-values.
    if (SrcLT) {
      TC.diagnose(E->getLoc(), diag::invalid_conversion_of_lvalue,
                  SrcLT->getObjectType(), DestLT->getObjectType())
        << E->getSourceRange();
      return nullptr;
    }

    TC.diagnose(E->getLoc(), diag::invalid_conversion_to_lvalue,
                E->getType(), DestLT->getObjectType());
    return nullptr;
  }

  if (TupleType *TT = DestTy->getAs<TupleType>()) {
    // Type conversions are carefully ranked so that they "do the right thing",
    // because they can be highly ambiguous.  For example, consider something
    // like foo(4, 5) when foo is declared to take ((int,int=3), int=6).  This
    // could be parsed as either ((4,5), 6) or ((4,3),5), but the later one is
    // the "right" answer.
    
    // If the element of the tuple has dependent type and is a TupleExpr, try to
    // convert it.
    if (TupleExpr *TE = dyn_cast<TupleExpr>(E))
      return convertTupleToTupleType(TE, TE->getNumElements(), TT,TC);

    // If the is a scalar to tuple conversion, form the tuple and return it.
    int ScalarFieldNo = TT->getFieldForScalarInit();
    if (ScalarFieldNo != -1)
      return convertScalarToTupleType(E, TT, ScalarFieldNo, TC);
    
    // If the input is a tuple and the output is a tuple, see if we can convert
    // each element.
    if (TupleType *ETy = E->getType()->getAs<TupleType>())
      return convertTupleToTupleType(E, ETy->getFields().size(), TT, TC);
  }

  // If the source is an l-value, load from it.  We intentionally do
  // this before checking for certain destination types below.
  if (LValueType *srcLV = E->getType()->getAs<LValueType>()) {
    E = TC.convertLValueToRValue(srcLV, E);
    if (!E) return nullptr;

    return convertToType(E, DestTy, TC);
  }

  // If the input expression has a dependent type, then there are two cases:
  // first this could be an AnonDecl whose type will be specified by a larger
  // context, second, this could be a context sensitive expression value like
  // :foo.  If this is a context sensitive expression, propagate the type down
  // into the subexpression.
  if (E->getType()->is<DependentType>())
    return SemaCoerce(TC, DestTy).doIt(E);

  // Could not do the conversion.

  // When diagnosing a failed conversion, ignore l-values on the source type.
  TC.diagnose(E->getLoc(), diag::invalid_conversion, E->getType(), DestTy)
    << E->getSourceRange();
  return nullptr;
}



Expr *TypeChecker::convertToType(Expr *E, Type DestTy) {
  return SemaCoerce::convertToType(E, DestTy, *this);
}
