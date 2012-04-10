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
#include "swift/AST/ASTWalker.h"
#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/APInt.h"
#include "llvm/ADT/SetVector.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/Twine.h"
#include <utility>
using namespace swift;

namespace {

/// CoercedResult - The result of coercing a given expression to a given
/// type, which is either an expression (if the coercion is being applied) or
/// simply a type (if the coercion is not being applied). NULL indicates that
class CoercedResult {
  llvm::PointerUnion<Expr *, TypeBase *> Stored;
  
public:
  CoercedResult(std::nullptr_t) : Stored() { }
  
  explicit CoercedResult(Expr *E) : Stored(E) {assert(E && "Null expression");}
  
  CoercedResult(Type Ty) : Stored(Ty.getPointer()) {
    assert(!Ty.isNull() && "Null type");
  }
  
  explicit operator bool() const {
    return !Stored.isNull();
  }
  
  Type getType() const {
    assert(!Stored.isNull() && "No type for an invalid coerced result");
    return Stored.is<Expr *>()? Stored.get<Expr*>()->getType() 
                              : Type(Stored.get<TypeBase *>());
    
  }
  
  Expr *getExpr() const {
    assert(Stored.is<Expr *>() && "Coerced result does not have an expression");
    return Stored.get<Expr *>();
  }
};
  
/// SemaCoerce - This class implements top-down semantic analysis (aka "root to
/// leaf", using the type of "+" to infer the type of "a" in "a+1") of an
/// already-existing expression tree.  This is performed when an expression with
/// dependent type is used in a context that forces a specific type.  
///
/// Each visit method reanalyzes the node to see if the type can be propagated
/// into it.  If not, it returns it.  If so it checks to see if the type
/// is contradictory (in which case it returns NULL) otherwise it applies the
/// type (possibly recursively) and (optionally) returns the new/updated 
/// expression.
class SemaCoerce : public ExprVisitor<SemaCoerce, CoercedResult> {
  enum class LiteralType {
    Int, Float, String
  };
  
  /// \brief Determine whether the given type is compatible with an integer
  /// or floating-point literal and what function would perform the conversion.
  ///
  /// \returns The function that will perform the conversion, along with the
  /// type of the argument of this function.
  std::pair<FuncDecl*, Type> isLiteralCompatibleType(Type Ty, SourceLoc Loc, 
                                                     LiteralType LitTy);

  /// coerceLiteralToType - Coerce the given literal to the destination type.
  /// The literal is assumed to have dependent type.
  CoercedResult coerceLiteral(Expr *E);
  
  TypeChecker &TC;
  Type DestTy;
  
  /// Apply - Whether to apply the results of this coercion to the AST, 
  /// returning new or updating ASTs and emitting any diagnostics.
  bool Apply;
  
  template<typename ...ArgTypes>
  InFlightDiagnostic diagnose(ArgTypes &&...Args) {
    InFlightDiagnostic Diag = TC.diagnose(std::forward<ArgTypes>(Args)...);
    if (!Apply)
      Diag.suppress();
    
    return std::move(Diag);
  }

  /// unchanged - Return an unchanged expressions as a coerced result. 
  ///
  /// This routine takes care to produce the correct kind of result both when
  /// we are applying an operation (returning an expression) and when checking
  /// whether coercion will succeed (returning a type).
  CoercedResult unchanged(Expr *E) {
    return unchanged(E, Apply);
  }

  /// unchanged - Return an unchanged expressions as a coerced result. 
  ///
  /// This routine takes care to produce the correct kind of result both when
  /// we are applying an operation (returning an expression) and when checking
  /// whether coercion will succeed (returning a type).
  static CoercedResult unchanged(Expr *E, bool Apply) {
    if (!Apply)
      return CoercedResult(E->getType());
    
    return CoercedResult(E);
  }

  /// coerce - Return a newly-coerced expression.
  CoercedResult coerced(Expr *E) {
    return coerced(E, Apply);
  }

  /// coerce - Return a newly-coerced expression.
  static CoercedResult coerced(Expr *E, bool Apply) {
    assert(Apply && "Cannot return a coerced expression when not applying");
    return CoercedResult(E);
  }  
  
public:
  CoercedResult visitErrorExpr(ErrorExpr *E) {
    return unchanged(E);
  }
  
  CoercedResult visitIntegerLiteralExpr(IntegerLiteralExpr *E) {
    return coerceLiteral(E);
  }
  CoercedResult visitFloatLiteralExpr(FloatLiteralExpr *E) {
    return coerceLiteral(E);
  }
  CoercedResult visitStringLiteralExpr(StringLiteralExpr *E) {
    return coerceLiteral(E);
  }
  CoercedResult visitDeclRefExpr(DeclRefExpr *E) {
    return unchanged(E);
  }

  static Type matchLValueType(ValueDecl *val, LValueType *lv) {
    if (val->isReferencedAsLValue() && 
        val->getType()->isEqual(lv->getObjectType()))
      return lv;
    
    return Type();
  }

  CoercedResult coerceOverloadToLValue(OverloadSetRefExpr *E, LValueType *lv) {
    for (ValueDecl *val : E->getDecls()) {
      if (Type Matched = matchLValueType(val, lv)) {
        if (!Apply)
          return Matched;
        
        // FIXME: We should be handling this like overload resolution, because
        // ambiguities are possible.
        return coerced(
                 E->createFilteredWithCopy(ArrayRef<ValueDecl *>(&val, 1)));
      }
    }

    // FIXME: We should be able to materialize values here, unless
    // implicit(byref) goes away.
    if (Apply) {
      diagnose(E->getLoc(), diag::no_candidates_ref,
               E->getDecls()[0]->getName());
      TC.printOverloadSetCandidates(E->getDecls());
    }

    return nullptr;
  }
  
  CoercedResult visitOverloadSetRefExpr(OverloadSetRefExpr *E) {
    // If we're looking for an lvalue type, we need an exact match
    // to the target object type.
    // FIXME: What about qualifiers?
    if (LValueType *lv = DestTy->getAs<LValueType>())
      return coerceOverloadToLValue(E, lv);

    // Determine which declarations are viable.
    SmallVector<ValueDecl *, 4> Viable;
    for (ValueDecl *Val : E->getDecls()) {
      Type srcTy = Val->getType();
      
      DeclRefExpr DRE(Val, E->getLoc(), srcTy);
      if (!TC.isCoercibleToType(&DRE, DestTy))
        continue;
      
      Viable.push_back(Val);
    }
    
    if (Viable.size() == 1) {
      if (!Apply)
        return DestTy;
      
      return coerced(TC.convertToRValue(E->createFilteredWithCopy(Viable)));
    }
      
    if (Apply) {
      if (Viable.empty()) {
        diagnose(E->getLoc(), diag::no_candidates_ref, 
                 E->getDecls()[0]->getName())
          << E->getSourceRange();
        TC.printOverloadSetCandidates(E->getDecls());
      } else {
        diagnose(E->getLoc(), diag::overloading_ambiguity)
          << E->getSourceRange();
        TC.printOverloadSetCandidates(Viable);
      }
    }
    
    return nullptr;
  }
  
  // If this is an UnresolvedMemberExpr, then this provides the type we've
  // been looking for!
  CoercedResult visitUnresolvedMemberExpr(UnresolvedMemberExpr *UME) {
    // The only valid type for an UME is a OneOfType.
    OneOfType *DT = DestTy->getAs<OneOfType>();
    if (DT == 0) {
      diagnose(UME->getLoc(), diag::cannot_convert_dependent_reference,
               UME->getName(), DestTy);
      return nullptr;
    }
    
    // The oneof type must have an element of the specified name.
    OneOfElementDecl *DED = DT->getElement(UME->getName());
    if (DED == 0) {
      diagnose(UME->getLoc(), diag::invalid_member_in_type,
               DestTy, UME->getName());
      diagnose(DT->getOneOfLoc(), diag::type_declared_here);
      return nullptr;
    }

    if (DED->getType()->is<FunctionType>()) {
      diagnose(UME->getLoc(), diag::call_element_function_type,
               DestTy, UME->getName());
      return nullptr;
    }

    // If it does, then everything is good, resolve the reference.
    if (!Apply)
      return DED->getType();
    
    return coerced(new (TC.Context) DeclRefExpr(DED, UME->getColonLoc(),
                                                DED->getType()));
  }  
  
  CoercedResult visitParenExpr(ParenExpr *E) {
    assert(0 && "Already special cased in SemaCoerce::coerceToType");
  }
    
  CoercedResult visitTupleExpr(TupleExpr *E) {
    return unchanged(E);
  }
  
  CoercedResult visitUnresolvedDeclRefExpr(UnresolvedDeclRefExpr *E) {
    // FIXME: Is this an error-recovery case?
    return unchanged(E);
  }
  CoercedResult visitUnresolvedDotExpr(UnresolvedDotExpr *E) {
    // FIXME: Is this an error-recovery case?
    return unchanged(E);
  }

  CoercedResult visitTupleElementExpr(TupleElementExpr *E) {
    // TupleElementExpr is fully resolved.
    llvm_unreachable("This node doesn't exist for dependent types");
  }
  
  
  CoercedResult visitApplyExpr(ApplyExpr *E);
  
  CoercedResult visitSequenceExpr(SequenceExpr *E) {
    llvm_unreachable("SequenceExprs should all be resolved by this pass");
  }

  CoercedResult visitFuncExpr(FuncExpr *E) {
    return unchanged(E);      
  }

  CoercedResult visitExplicitClosureExpr(ExplicitClosureExpr *E);

  CoercedResult visitImplicitClosureExpr(ImplicitClosureExpr *E) {
    return unchanged(E);      
  }
  
  CoercedResult visitModuleExpr(ModuleExpr *E) {
    return unchanged(E);
  }

  CoercedResult visitDotSyntaxBaseIgnoredExpr(DotSyntaxBaseIgnoredExpr *E) {
    // FIXME: Coerces the RHS.
    return unchanged(E);
  }

  CoercedResult visitImplicitConversionExpr(ImplicitConversionExpr *E) {
    return unchanged(E);
  }

  CoercedResult visitAddressOfExpr(AddressOfExpr *E) {
    LValueType *DestLT = DestTy->getAs<LValueType>();
    if (!DestLT) {
      if (!Apply)
        return nullptr;
      
      // The '&' is incorrect. Customize our diagnostic based on whether
      // removing the '&' would make the code type-check or not.
      if (coerceToType(E->getSubExpr(), DestTy, TC, false)) {
        // The code would have type-checked without '&'; tell the user that the
        // '&' is extraneous and type-check as if it weren't there.
        diagnose(E->getLoc(), diag::explicit_lvalue)
          << E->getSubExpr()->getSourceRange();
        return coerceToType(E->getSubExpr(), DestTy, TC, true);
      }

      // The '&' wouldn't fix it, either; produce diagnostics based on coercing
      // the subexpression directly, and the user can deal with the '&'
      // afterward.
      return coerceToType(E->getSubExpr(), DestTy, TC, true);
    }
    
    
    if (CoercedResult Sub = coerceToType(E->getSubExpr(), DestTy, TC, Apply)) {
      if (!Apply)
        return DestTy;
      
      E->setSubExpr(Sub.getExpr());
      E->setType(DestTy);
      return unchanged(E);
    }
    
    return nullptr;
  }

  SemaCoerce(TypeChecker &TC, Type DestTy, bool Apply) 
    : TC(TC), DestTy(DestTy), Apply(Apply) 
  {
    assert(!DestTy->isDependentType());
  }
  
  CoercedResult doIt(Expr *E) {
    return visit(E);
  }
  
  /// coerceToType - This is the main entrypoint to SemaCoerce.
  static CoercedResult coerceToType(Expr *E, Type DestTy, TypeChecker &TC,
                                    bool Apply);

  static CoercedResult convertScalarToTupleType(Expr *E, TupleType *DestTy,
                                                unsigned FieldNo, 
                                                TypeChecker &TC, bool Apply);
  static CoercedResult
  convertTupleToTupleType(Expr *E, unsigned NumExprElements,
                          TupleType *DestTy, TypeChecker &TC, bool Apply);
  
  /// loadLValue - Load the given lvalue expression.
  static CoercedResult loadLValue(Expr *E, LValueType *LValue, TypeChecker &TC,
                                  bool Apply) {
    assert(E->getType()->isEqual(LValue));
    
    // Can't load from an explicit lvalue.
    if (LValue->isExplicit()) {
      if (Apply)
        TC.diagnose(E->getLoc(), diag::load_of_explicit_lvalue,
                    LValue->getObjectType())
          << E->getSourceRange();
      
      return nullptr;
    }
    
    if (!Apply) {
      // If we're not going to apply the result anyway, just return the
      // appropriate type.
      return LValue->getObjectType();
    }
    
    return coerced(new (TC.Context) LoadExpr(E, LValue->getObjectType()),
                   Apply);
  }
};
  
} // end anonymous namespace.

/// isLiteralCompatibleType - Check to see if the specified type has a properly
/// defined literal conversion function, emiting an error and returning null if
/// not.  If everything looks kosher, return the conversion function and the
/// argument type that it expects.
std::pair<FuncDecl*, Type> 
SemaCoerce::isLiteralCompatibleType(Type Ty, SourceLoc Loc, LiteralType LitTy) {
  // Look up the convertFrom*Literal method on the type.  If it is missing,
  // then the type isn't compatible with literals.  If it is present, it must
  // have a single argument.
  SmallVector<ValueDecl*, 8> Methods;
  const char *MethodName = 0;
  switch (LitTy) {
  case LiteralType::Int:    MethodName = "convertFromIntegerLiteral"; break;
  case LiteralType::Float:  MethodName = "convertFromFloatLiteral"; break;
  case LiteralType::String: MethodName = "convertFromStringLiteral"; break;
  }
  assert(MethodName && "Didn't know LitTy");
  TC.TU.lookupGlobalExtensionMethods(Ty, TC.Context.getIdentifier(MethodName),
                                     Methods);
  
  if (Methods.empty()) {
    diagnose(Loc, diag::type_not_compatible_literal, Ty);
    return std::pair<FuncDecl*, Type>();
  }
  
  if (Methods.size() != 1) {
    diagnose(Loc, diag::type_ambiguous_literal_conversion, Ty, MethodName);
    if (Apply)
      for (ValueDecl *D : Methods)
        diagnose(D->getLocStart(), diag::found_candidate);
    return std::pair<FuncDecl*, Type>();
  }
  
  // Verify that the implementation is a metatype 'static' func.
  FuncDecl *Method = dyn_cast<FuncDecl>(Methods[0]);
  if (Method == 0 || !Method->isStatic()) {
    diagnose(Method->getLocStart(), diag::type_literal_conversion_not_static,
             Ty, MethodName);
    return std::pair<FuncDecl*, Type>();
  }
  
  // Check that the type of the 'convertFrom*Literal' method makes
  // sense.  We want a type of "S -> DestTy" where S is the expected type.
  FunctionType *FT = Method->getType()->castTo<FunctionType>();
  
  // The result of the convert function must be the destination type.
  if (!FT->getResult()->isEqual(Ty)) {
    diagnose(Method->getLocStart(), 
             diag::literal_conversion_wrong_return_type, Ty, MethodName);
    diagnose(Loc, diag::while_converting_literal, Ty);
    return std::pair<FuncDecl*, Type>();
  }
  
  // Get the argument type, ignoring single element tuples.
  Type ArgType = FT->getInput();
  
  // Look through single element tuples.
  if (TupleType *TT = ArgType->getAs<TupleType>())
    if (TT->getFields().size() == 1)
      ArgType = TT->getFields()[0].getType();
  
  return std::pair<FuncDecl*, Type>(Method, ArgType);
}

CoercedResult SemaCoerce::coerceLiteral(Expr *E) {
  assert(E->getType()->isDependentType() && "only accepts dependent types");
  LiteralType LitTy;
  if (isa<IntegerLiteralExpr>(E))
    LitTy = LiteralType::Int;
  else if (isa<FloatLiteralExpr>(E))
    LitTy = LiteralType::Float;
  else {
    assert(isa<StringLiteralExpr>(E));
    LitTy = LiteralType::String;
  }
  
  // Check the destination type to see if it is compatible with literals,
  // diagnosing the failure if not.
  std::pair<FuncDecl*, Type> LiteralInfo 
    = isLiteralCompatibleType(DestTy, E->getLoc(), LitTy);
  FuncDecl *Method = LiteralInfo.first;
  Type ArgType = LiteralInfo.second;
  if (!Method)
    return nullptr;
  
  // The argument type must either be a Builtin:: integer/fp type (in which case
  // this is a type in the standard library) or some other type that itself has
  // a conversion function from a builtin type (in which case we have
  // "chaining", and an implicit conversion through that type).
  Expr *Intermediate;
  BuiltinIntegerType *BIT;
  BuiltinFloatType *BFT;
  if (LitTy == LiteralType::Int &&
      (BIT = ArgType->getAs<BuiltinIntegerType>())) {
    // If this is a direct use of the builtin integer type, use the integer size
    // to diagnose excess precision issues.
    llvm::APInt Value(1, 0);
    StringRef IntText = cast<IntegerLiteralExpr>(E)->getText();
    unsigned Radix;
    if (IntText.startswith("0x")) {
      IntText = IntText.substr(2);
      Radix = 16;
    } else if (IntText.startswith("0o")) {
      IntText = IntText.substr(2);
      Radix = 8;
    } else if (IntText.startswith("0b")) {
      IntText = IntText.substr(2);
      Radix = 2;
    } else {
      Radix = 10;
    }
    bool Failure = IntText.getAsInteger(Radix, Value);
    assert(!Failure && "Lexer should have verified a reasonable type!");
    (void)Failure;
    
    if (Value.getActiveBits() > BIT->getBitWidth())
      diagnose(E->getLoc(), diag::int_literal_too_large, Value.getBitWidth(),
               DestTy);
    
    // Give the integer literal the builtin integer type.
    if (Apply)
      E->setType(ArgType);
    Intermediate = E;
  } else if (LitTy == LiteralType::Float &&
             (BFT = ArgType->getAs<BuiltinFloatType>())) {
    // If this is a direct use of a builtin floating point type, use the
    // floating point type to do the syntax verification.
    llvm::APFloat Val(BFT->getAPFloatSemantics());
    switch (Val.convertFromString(cast<FloatLiteralExpr>(E)->getText(),
                                  llvm::APFloat::rmNearestTiesToEven)) {
    default: break;
    case llvm::APFloat::opOverflow: 
      if (Apply) {
        llvm::SmallString<20> Buffer;
        llvm::APFloat::getLargest(Val.getSemantics()).toString(Buffer);
        diagnose(E->getLoc(), diag::float_literal_overflow, Buffer);
      }
      break;
    case llvm::APFloat::opUnderflow: 
      if (Apply) {
        // Denormals are ok, but reported as underflow by APFloat.
        if (!Val.isZero()) break;
        llvm::SmallString<20> Buffer;
        llvm::APFloat::getSmallest(Val.getSemantics()).toString(Buffer);
        diagnose(E->getLoc(), diag::float_literal_underflow, Buffer);
      }
      break;
    }
    
    if (Apply)
      E->setType(ArgType);
    Intermediate = E;
  } else if (LitTy == LiteralType::String &&
             ArgType->is<BuiltinRawPointerType>()) {
    // Nothing to do.
    E->setType(ArgType);
    Intermediate = E;
  } else {
    // Check to see if this is the chaining case, where ArgType itself has a
    // conversion from a Builtin type.
    LiteralInfo = isLiteralCompatibleType(ArgType, E->getLoc(), LitTy);
    if (LiteralInfo.first == 0) {
      diagnose(Method->getLocStart(),
               diag::while_processing_literal_conversion_function, DestTy);
      return nullptr;
    }
    
    if (LitTy == LiteralType::Int &&
        LiteralInfo.second->is<BuiltinIntegerType>()) {
      // ok.
    } else if (LitTy == LiteralType::Float &&
               LiteralInfo.second->is<BuiltinFloatType>()) {
      // ok.
    } else if (LitTy == LiteralType::String &&
               LiteralInfo.second->is<BuiltinRawPointerType>()) {
      // ok.
    } else {
      diagnose(Method->getLocStart(),
               diag::type_literal_conversion_defined_wrong, DestTy);
      diagnose(E->getLoc(), diag::while_converting_literal, DestTy);
      return nullptr;
    }
    
    // If this a 'chaining' case, recursively convert the literal to the
    // intermediate type, then use our conversion function to finish the
    // translation.
    if (CoercedResult IntermediateRes = coerceToType(E, ArgType, TC, Apply)) {
      if (!Apply)
        return DestTy;
      
      Intermediate = IntermediateRes.getExpr();
    } else {
      return nullptr;
    }
    
    // Okay, now Intermediate is known to have type 'ArgType' so we can use a
    // call to our conversion function to finish things off.
  }
  
  if (!Apply)
    return DestTy;
  
  DeclRefExpr *DRE
    = new (TC.Context) DeclRefExpr(Method,
                                   // FIXME: This location is a hack!
                                   Intermediate->getStartLoc(),
                                   Method->getType());
  
  // Return a new call of the conversion function, passing in the integer
  // literal.
  return coerced(new (TC.Context) CallExpr(DRE, Intermediate, DestTy));
}

CoercedResult SemaCoerce::visitApplyExpr(ApplyExpr *E) {
  // TODO: We would really like to propagate something like
  // "DependentTy->DestTy" up into the Fn argument, eliminating these special
  // cases.  See the 'syntactic' FIXME's below.
  
  // Given a CallExpr a(b) where "a" is an overloaded value, we may be able to
  // prune the overload set based on the known result type.  Doing this may
  // allow the ambiguity to resolve by removing candidates
  // that caused the ambiguity.  For example if we know that the destination
  // type is 'int', and we had "(int) -> int" and "(SomeTy) -> float", we can
  // prune the second one, and then recursively apply 'int' to b.
  //
  // FIXME: Handling this syntactically causes us to reject "(:f)(x)" as
  // ambiguous.
  if (OverloadSetRefExpr *OSE = dyn_cast<OverloadSetRefExpr>(E->getFn())) {
    SmallVector<ValueDecl*, 4> Viable;
    if (ValueDecl *Best = TC.filterOverloadSet(OSE->getDecls(), E->getArg(),
                                               DestTy, Viable)) {
      if (!Apply) {
        // Determine the type of the resulting call expression.
        Type Ty = Best->getType();
        if (LValueType *LValue = Ty->getAs<LValueType>())
          Ty = LValue->getObjectType();
        
        if (FunctionType *FnTy = Ty->getAs<FunctionType>())
          return FnTy->getResult();
        
        return Ty;
      }
      
      Expr *Fn = TC.buildDeclRefRValue(Best, OSE->getLoc());
      E->setFn(Fn);
      
      if (Expr *Result = TC.semaApplyExpr(E))
        return coerced(Result);
      
      return nullptr;
    }
    
    if (Apply) {
      if (Viable.empty())
        TC.diagnoseEmptyOverloadSet(E, OSE->getDecls());
      else {
        diagnose(E->getFn()->getLoc(), diag::overloading_ambiguity)
          << E->getSourceRange();
        TC.printOverloadSetCandidates(Viable);
      }
    }
    
    return nullptr;
  }
  
  // If we have ".f(x)" and the result type of the call is a OneOfType, then
  // .f must be an element constructor for the oneof value.
  //
  // FIXME: Handling this syntactically causes us to reject "(:f)(x)" as
  // ambiguous.
  if (UnresolvedMemberExpr *UME =
      dyn_cast<UnresolvedMemberExpr>(E->getFn())) {
    if (OneOfType *DT = DestTy->getAs<OneOfType>()) {
      // The oneof type must have an element of the specified name.
      OneOfElementDecl *DED = DT->getElement(UME->getName());
      if (DED == 0) {
        diagnose(UME->getLoc(), diag::invalid_member_in_type,
                    DestTy, UME->getName());
        diagnose(DT->getOneOfLoc(), diag::type_declared_here);
        return 0;
      }
      
      if (!DED->getType()->is<FunctionType>()) {
        diagnose(UME->getLoc(), diag::call_element_not_function_type,
                 DestTy, UME->getName());
        return 0;
      }
      
      // FIXME: Preserve source locations.
      E->setFn(new (TC.Context) DeclRefExpr(DED, UME->getColonLoc(),
                                            DED->getType()));
      // FIXME: We need to be able to perform type-checking here without
      // emitting diagnostics in the failure case.
      Expr *Result = TC.semaApplyExpr(E);
      if (!Result)
        return nullptr;
      return unchanged(Result);
    }
  }
  
  return unchanged(E);
}


CoercedResult SemaCoerce::visitExplicitClosureExpr(ExplicitClosureExpr *E) {
  // Make sure that we're converting the closure to a function type.  If not,
  // diagnose the error.
  FunctionType *FT = DestTy->getAs<FunctionType>();
  if (FT == 0) {
    diagnose(E->getStartLoc(), diag::closure_not_function_type, DestTy)
      << E->getSourceRange();
    return nullptr;
  }

  // Now that we have a FunctionType for the closure, we can know how many
  // arguments are allowed.
  if (Apply)
    E->setType(FT);
  
  // If the input to the function is a non-tuple, only $0 is valid, if it is a
  // tuple, then $0..$N are valid depending on the number of inputs to the
  // tuple.
  unsigned NumInputArgs = 1;
  TupleType *FuncInputTT = dyn_cast<TupleType>(FT->getInput().getPointer());
  if (FuncInputTT)
    NumInputArgs = FuncInputTT->getFields().size();

  if (NumInputArgs < E->getParserVarDecls().size()) {
    diagnose(E->getLoc(), diag::invalid_anonymous_argument,
             E->getParserVarDecls().size() - 1, NumInputArgs);
    return nullptr;
  }

  // FIXME: We actually do want to perform type-checking again, to make sure
  // that the closure expression type-checks with the given function type.
  // For now, we just assume that it does type-check, since we don't have a
  // way to silence the errors (yet).
  if (!Apply)
    return DestTy;
  
  // Build pattern for parameters.
  // FIXME: This pattern is currently unused!
  std::vector<VarDecl*> ArgVars(E->getParserVarDecls().begin(),
                                E->getParserVarDecls().end());
  Pattern *ArgPat;
  SourceLoc loc = E->getLoc();

  E->GenerateVarDecls(NumInputArgs, ArgVars, TC.Context);

  if (FuncInputTT) {
    std::vector<TuplePatternElt> ArgElts;
    for (unsigned i = 0; i < NumInputArgs; ++i) {
      ArgVars[i]->setType(FuncInputTT->getElementType(i));
      ArgElts.emplace_back(new (TC.Context) NamedPattern(ArgVars[i]));
    }
    ArgPat = TuplePattern::create(TC.Context, loc, ArgElts, loc);
  } else {
    ArgVars[0]->setType(FT->getInput());
    ArgPat = new (TC.Context) NamedPattern(ArgVars[0]);
  }
  E->setPattern(ArgPat);

  Expr *Result = E->getBody();

  // Type check the full expression, verifying that it is fully typed.
  if (TC.typeCheckExpression(Result, FT->getResult()))
    return 0;
  
  E->setBody(Result);
  return unchanged(E);
}


/// convertTupleToTupleType - Given an expression that has tuple type, convert
/// it to have some other tuple type.
///
/// The caller gives us a list of the expressions named arguments and a count of
/// tuple elements for E in the IdentList+NumIdents array.  DestTy specifies the
/// type to convert to, which is known to be a TupleType.
CoercedResult
SemaCoerce::convertTupleToTupleType(Expr *E, unsigned NumExprElements,
                                    TupleType *DestTy, TypeChecker &TC,
                                    bool Apply){
  
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
      
      if (Apply) {
        if (!DestTy->getFields()[i].hasName())
          TC.diagnose(ErrorLoc, diag::not_initialized_tuple_element, i,
                      E->getType());
        else
          TC.diagnose(ErrorLoc, diag::not_initialized_named_tuple_element,
                      DestTy->getFields()[i].getName(), i, E->getType());
      }
      return nullptr;
    }
    
    // Okay, we found an input value to use.
    DestElementSources[i] = NextInputValue;
    UsedElements[NextInputValue] = true;
  }
  
  // If there were any unused input values, we fail.
  for (unsigned i = 0, e = UsedElements.size(); i != e; ++i) {
    if (!UsedElements[i]) {
      // If this is a TupleExpr (common case) get a more precise location for
      // the element we care about.
      SourceLoc ErrorLoc = E->getLoc();
      if (TupleExpr *TE = dyn_cast<TupleExpr>(E))
        if (Expr *SubExp = TE->getElement(i))
          ErrorLoc = SubExp->getLoc();
      
      if (Apply) {
        if (IdentList[i].empty())
          TC.diagnose(ErrorLoc, diag::tuple_element_not_used, i, DestTy);
        else
          TC.diagnose(ErrorLoc, diag::named_tuple_element_not_used, 
                      IdentList[i], i, DestTy);
      }
    
      return nullptr;
    }
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
        if (Apply)
          TE->setElement(i, 0);
        continue;
      }
      
      // Check to see if the src value can be converted to the destination
      // element type.
      if (CoercedResult Elt = coerceToType(OrigElts[SrcField], 
                                            DestTy->getElementType(i),
                                            TC, Apply)) {
        if (Apply)
          TE->setElement(i, Elt.getExpr());
      } else {
        // TODO: QOI: Include a note about this failure!
        return nullptr;
      }
    }
    
    if (!Apply)
      return Type(DestTy);
    
    // Okay, we updated the tuple in place.
    E->setType(DestTy);
    return unchanged(E, Apply);
  }
  
  // Otherwise, if it isn't a tuple literal, we unpack the source elementwise so
  // we can do elementwise conversions as needed, then rebuild a new TupleExpr
  // of the right destination type.
  TupleType *ETy = E->getType()->getAs<TupleType>();
  SmallVector<int, 16> NewElements(DestTy->getFields().size());
  
  bool RebuildSourceType = false;
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
    
    if (ETy) {
      Type ElementTy = ETy->getElementType(SrcField);
      if (TE && ElementTy->isDependentType()) {
        // FIXME: We shouldn't need a TupleExpr to handle this coercion.
        // Check to see if the src value can be converted to the destination
        // element type.
        if (CoercedResult Elt = coerceToType(TE->getElement(SrcField), 
                                              DestEltTy, TC, Apply)) {
          if (Apply)
            TE->setElement(SrcField, Elt.getExpr());
        } else {
          // FIXME: QOI: Include a note about this failure!
          return nullptr;
        }
        
        // Because we have coerced something in the source tuple, we need to
        // rebuild the type of that tuple.
        RebuildSourceType = true;
      } else if (ETy && !ElementTy->isEqual(DestEltTy)) {
        // FIXME: Allow conversions when we don't have a tuple expression?
        if (Apply)
          TC.diagnose(E->getLoc(), diag::tuple_element_type_mismatch, i,
                      ETy->getElementType(SrcField),
                      DestTy->getElementType(i));
        return nullptr;
      }
    }
    
    NewElements[i] = SrcField;
  }
  
  if (!Apply)
    return Type(DestTy);

  // If we need to rebuild the type of the source due to coercion, do so now.
  if (RebuildSourceType) {
    SmallVector<TupleTypeElt, 4> NewTypeElts;
    NewTypeElts.reserve(ETy->getFields().size());

    unsigned I = 0;
    for (const auto &Elt : ETy->getFields())
      NewTypeElts.push_back(Elt.getWithType(TE->getElement(I++)->getType()));
    
    E->setType(TupleType::get(NewTypeElts, TC.Context));
  }
    
  // If we got here, the type conversion is successful, create a new TupleExpr.  
  ArrayRef<int> Mapping = TC.Context.AllocateCopy(NewElements);
  return coerced(new (TC.Context) TupleShuffleExpr(E, Mapping, DestTy), Apply);
}

/// convertScalarToTupleType - Convert the specified expression to the specified
/// tuple type, which is known to be initializable with one element.
CoercedResult SemaCoerce::convertScalarToTupleType(Expr *E, TupleType *DestTy,
                                                   unsigned ScalarField,
                                                   TypeChecker &TC,
                                                   bool Apply) {
  // If the destination is a tuple type with at most one element that has no
  // default value, see if the expression's type is convertable to the
  // element type.  This handles assigning 4 to "(a = 4, b : int)".
  Type ScalarType = DestTy->getElementType(ScalarField);
  CoercedResult ERes = coerceToType(E, ScalarType, TC, Apply);
  if (!ERes)
    return nullptr;

  if (!Apply)
    return Type(DestTy);
  
  unsigned NumFields = DestTy->getFields().size();
  
  // Must allocate space for the AST node.
  MutableArrayRef<Expr*> NewSE(TC.Context.Allocate<Expr*>(NumFields),NumFields);
  
  bool NeedsNames = false;
  for (unsigned i = 0, e = NumFields; i != e; ++i) {
    if (i == (unsigned)ScalarField)
      NewSE[i] = ERes.getExpr();
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
  
  return CoercedResult(new (TC.Context) TupleExpr(SourceLoc(), NewSE, NewName, 
                                                  SourceLoc(), DestTy));
}

/// Would the source lvalue type be coercible to the dest lvalue type
/// if it were explicit?
static bool isSubtypeExceptImplicit(LValueType *SrcTy, LValueType *DestTy) {
  return DestTy->getObjectType()->isEqual(SrcTy->getObjectType())
      && SrcTy->getQualifiers().withoutImplicit() <= DestTy->getQualifiers();
}

namespace {
  class FindCapturedVars : public ASTWalker {
    llvm::SetVector<ValueDecl*> &Captures;

  public:
    bool walkToExprPre(Expr *E) {
      if (DeclRefExpr *DRE = dyn_cast<DeclRefExpr>(E))
        if (DRE->getDecl()->getDeclContext()->isLocalContext())
          Captures.insert(DRE->getDecl());
      return true;
    }

    FindCapturedVars(llvm::SetVector<ValueDecl*> &captures)
      : Captures(captures) {}

    void doWalk(Expr *E) {
      E->walk(*this);
    }
  };
}

/// coerceToType - This is the recursive implementation of
/// coerceToType.  It produces diagnostics and returns null on failure.
CoercedResult SemaCoerce::coerceToType(Expr *E, Type DestTy, TypeChecker &TC,
                                        bool Apply) {
  assert(!DestTy->isDependentType() &&
         "Result of conversion can't be dependent");

  // If the destination is a AutoClosing FunctionType, we have special rules.
  if (FunctionType *FT = DestTy->getAs<FunctionType>())
    if (FT->isAutoClosure()) {
      // We require the expression to be an ImplicitClosureExpr that produces
      // DestTy.
      if (E->getType()->isEqual(DestTy) && isa<ImplicitClosureExpr>(E))
        return unchanged(E, Apply);
      
      // If we don't have it yet, force the input to the result of the closure
      // and build the implicit closure.
      if (CoercedResult CoercedE = coerceToType(E, FT->getResult(), TC, Apply)) {
        if (!Apply)
          return DestTy;
        
        E = CoercedE.getExpr();
      } else {
        // QOI: Add an note?
        return nullptr;
      }
      
      // FIXME: Need to figure out correct parent DeclContext; fortunately,
      // it doesn't matter much for the moment because nothing actually needs
      // to use the ImplicitClosureExpr as its context.
      ImplicitClosureExpr *ICE =
          new (TC.Context) ImplicitClosureExpr(E, &TC.TU, DestTy);
      Pattern *Pat = TuplePattern::create(TC.Context, E->getLoc(),
                                          ArrayRef<TuplePatternElt>(),
                                          E->getLoc());
      ICE->setPattern(Pat);

      // Perform a recursive walk to compute the capture list; this is quite
      // different from the way this is done for explicit closures because
      // the closure doesn't exist until type-checking.
      llvm::SetVector<ValueDecl*> Captures;
      FindCapturedVars(Captures).doWalk(E);
      ValueDecl** CaptureCopy
        = TC.Context.AllocateCopy<ValueDecl*>(Captures.begin(), Captures.end());
      ICE->setCaptures(llvm::makeArrayRef(CaptureCopy, Captures.size()));

      return coerced(ICE, Apply);
    }
  
  // If we have an exact match, we're done.
  if (E->getType()->isEqual(DestTy))
    return unchanged(E, Apply);
  
  // If the expression is a grouping parenthesis and it has a dependent type,
  // just force the type through it, regardless of what DestTy is.
  if (ParenExpr *PE = dyn_cast<ParenExpr>(E)) {
    CoercedResult Sub = coerceToType(PE->getSubExpr(), DestTy, TC, Apply);
    if (!Sub)
      return nullptr;
    
    if (!Apply)
       return DestTy;
      
    PE->setSubExpr(Sub.getExpr());
    PE->setType(Sub.getType());
    return coerced(PE, Apply);
  }

  if (LValueType *DestLT = DestTy->getAs<LValueType>()) {
    LValueType *SrcLT = E->getType()->getAs<LValueType>();

    // Qualification conversion.
    if (SrcLT && DestLT->getObjectType()->isEqual(SrcLT->getObjectType()) &&
        SrcLT->getQualifiers() <= DestLT->getQualifiers()) {
      assert(SrcLT->getQualifiers() < DestLT->getQualifiers() &&
             "qualifiers match exactly but types are different?");

      if (!Apply)
        return DestTy;
      
      return coerced(new (TC.Context) RequalifyExpr(E, DestTy), Apply);
    }

    // If the input expression has a dependent type, try to coerce it to an
    // appropriate type.
    if (E->getType()->isDependentType())
      return SemaCoerce(TC, DestTy, Apply).doIt(E);

    // Materialization.
    if (!DestLT->isExplicit()) {
      CoercedResult CoercedE = coerceToType(E, DestLT->getObjectType(), TC,
                                             Apply);
      if (!CoercedE)
        return nullptr;
      
      if (!Apply)
        return DestTy;
      
      return coerced(new (TC.Context) MaterializeExpr(E, DestTy), Apply);
    }

    // Failure.

    // Use a special diagnostic if the coercion would have worked
    // except we needed an explicit marker.
    if (SrcLT && isSubtypeExceptImplicit(SrcLT, DestLT)) {
      if (Apply)
        TC.diagnose(E->getLoc(), diag::implicit_use_of_lvalue,
                    SrcLT->getObjectType())
          << E->getSourceRange();
      return nullptr;
    }

    // Use a special diagnostic for mismatched l-values.
    if (SrcLT) {
      if (Apply)
        TC.diagnose(E->getLoc(), diag::invalid_conversion_of_lvalue,
                    SrcLT->getObjectType(), DestLT->getObjectType())
          << E->getSourceRange();
      return nullptr;
    }

    if (Apply)
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
      return convertTupleToTupleType(TE, TE->getNumElements(), TT, TC, Apply);

    // If the is a scalar to tuple conversion, form the tuple and return it.
    int ScalarFieldNo = TT->getFieldForScalarInit();
    if (ScalarFieldNo != -1)
      return convertScalarToTupleType(E, TT, ScalarFieldNo, TC, Apply);
    
    // If the input is a tuple and the output is a tuple, see if we can convert
    // each element.
    if (TupleType *ETy = E->getType()->getAs<TupleType>())
      return convertTupleToTupleType(E, ETy->getFields().size(), TT, TC, Apply);
  }

  // If the input expression has a dependent type, try to coerce it to an
  // appropriate type.
  if (E->getType()->isDependentType())
    return SemaCoerce(TC, DestTy, Apply).doIt(E);

  // A function type can converted to a function type with the same input/result
  // types, ignoring labels.
  // FIXME: We will probably want to allow a contravariant return type and
  // covariant parameter types, but for now we don't have anything that compels
  // us to do so.
  if (FunctionType *DestFuncTy = DestTy->getAs<FunctionType>()) {
    if (FunctionType *SrcFuncTy = E->getType()->getAs<FunctionType>()) {
      if (DestFuncTy->getUnlabeledType(TC.Context)->isEqual(
            SrcFuncTy->getUnlabeledType(TC.Context))) {
        if (!Apply)
          return DestTy;
            
        return coerced(new (TC.Context) ParameterRenameExpr(E, DestTy), Apply);
      }
    }
  }

  // If the source is an l-value, load from it.
  if (LValueType *LValue = E->getType()->getAs<LValueType>()) {
    if (CoercedResult Loaded = loadLValue(E, LValue, TC, Apply)) {
      if (!Apply) {
        // Since we aren't applying the expression anyway, 
        LoadExpr Load(E, Loaded.getType());
        return coerceToType(&Load, DestTy, TC, Apply);
      }
      
      return coerceToType(Loaded.getExpr(), DestTy, TC, Apply);
    }
    
    return nullptr;
  }

  // Could not do the conversion.

  // When diagnosing a failed conversion, ignore l-values on the source type.
  if (Apply)
    TC.diagnose(E->getLoc(), diag::invalid_conversion, E->getType(), DestTy)
      << E->getSourceRange();
  return nullptr;
}

Expr *TypeChecker::coerceToType(Expr *E, Type DestTy) {
  if (CoercedResult Res = SemaCoerce::coerceToType(E, DestTy, *this, 
                                                    /*Apply=*/true))
    return Res.getExpr();
  
  return nullptr;
}

bool TypeChecker::isCoercibleToType(Expr *E, Type Ty) {
  return (bool)SemaCoerce::coerceToType(E, Ty, *this, /*Apply=*/false);
}

Expr *TypeChecker::convertLValueToRValue(LValueType *srcLV, Expr *E) {
  assert(E && "no expression to load!");
  assert(E->getType()->isEqual(srcLV));
  
  if (CoercedResult Result = SemaCoerce::loadLValue(E, srcLV, *this, true))
    return Result.getExpr();

  return nullptr;
}

