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
#include "swift/AST/NameLookup.h"
#include "swift/AST/ASTVisitor.h"
#include "swift/AST/ASTWalker.h"
#include "swift/AST/Attr.h"
#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/APInt.h"
#include "llvm/ADT/SetVector.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/Twine.h"
#include <utility>
using namespace swift;

namespace {

/// CoercedResult - The result of coercing a given expression to a given
/// type, which is either an expression (if the coercion is being applied) or
/// simply a type (if the coercion is not being applied). 
class CoercedResult {
  enum StoredKind {
    SK_Failed,
    SK_Unknowable,
    SK_Expr,
    SK_Type
  };

  llvm::PointerIntPair<void *, 2, StoredKind> Stored;
  
public:
  CoercedResult(std::nullptr_t) : Stored(0, SK_Failed) { }

  CoercedResult(CoercionResult Kind) {
    switch (Kind) {
    case CoercionResult::Succeeded:
      llvm_unreachable("Must provide expression or type");

    case CoercionResult::Failed:
      Stored.setInt(SK_Failed);
      break;

    case CoercionResult::Unknowable:
      Stored.setInt(SK_Unknowable);
      break;
    }
  }

  explicit CoercedResult(Expr *E) : Stored(E, SK_Expr) {
    assert(E && "Null expression");
  }
  
  CoercedResult(Type Ty) : Stored(Ty.getPointer(), SK_Type) {
    assert(!Ty.isNull() && "Null type");
  }
  
  explicit operator bool() const {
    return Stored.getInt() == SK_Expr ||
           Stored.getInt() == SK_Type;
  }

  CoercionResult getKind() const {
    switch (Stored.getInt()) {
    case SK_Failed:
      return CoercionResult::Failed;

    case SK_Unknowable:
      return CoercionResult::Unknowable;

    case SK_Expr:
    case SK_Type:
      return CoercionResult::Succeeded;
    }
  }

  Type getType() const {
    switch (Stored.getInt()) {
    case SK_Failed:
    case SK_Unknowable:
      llvm_unreachable("No type for an invalid/unknowable coerced result");

    case SK_Expr:
      return getExpr()->getType();

    case SK_Type:
      return Type(static_cast<TypeBase *>(Stored.getPointer()));
    }
  }
  
  Expr *getExpr() const {
    assert(Stored.getInt() == SK_Expr &&
           "Coerced result does not have an expression");
    return static_cast<Expr *>(Stored.getPointer());
  }
};

namespace {
  /// \brief Flags that control how type coercion is performed.
  enum CoercionFlags {
    /// \brief Apply the coercion, producing a coerced expression of the
    /// requested type (if successful) or emitting diagnostics (if a failure
    /// occurs). There is no way to back out of a coercion that has been
    /// applied, because it destructively updates the AST.
    CF_Apply = 0x01,
    /// \brief This coercion is part of an assignment operation, which permits
    /// the first argument to be treated as an implicit lvalue.
    CF_Assignment = 0x02,
    /// \brief The argument will be treated as an implicit lvalue.
    CF_ImplicitLValue = 0x04,
    /// \brief The argument allows user-defined conversions.
    CF_UserConversions = 0x08,
    
    /// \brief The basic set of "non-propagated" flags.
    CF_NotPropagated = CF_Assignment|CF_ImplicitLValue
  };
}
  
/// SemaCoerce - This class implements top-down semantic analysis (aka "root to
/// leaf", using the type of "+" to infer the type of "a" in "a+1") of an
/// already-existing expression tree.  This is performed when an expression with
/// unresolved type is used in a context that forces a specific type.
///
/// Each visit method reanalyzes the node to see if the type can be propagated
/// into it.  If not, it returns it.  If so it checks to see if the type
/// is contradictory (in which case it returns NULL) otherwise it applies the
/// type (possibly recursively) and (optionally) returns the new/updated 
/// expression.
class SemaCoerce : public ExprVisitor<SemaCoerce, CoercedResult> {
  enum class LiteralType {
    Int, Float, Char, String
  };
  
  /// \brief Determine whether the given type is compatible with an integer
  /// or floating-point literal and what function would perform the conversion.
  ///
  /// \returns The function that will perform the conversion, along with the
  /// type of the argument of this function.
  std::pair<FuncDecl*, Type> isLiteralCompatibleType(Type Ty, SourceLoc Loc, 
                                                     LiteralType LitTy);

  TypeChecker &TC;
  Type DestTy;
  
  /// \brief Flags that control how the coercion is performed, e.g.,
  /// if we are returning new/updated ASTs and emitted diagnostics.
  unsigned Flags;
  
  /// \brief A wrapper around an optional in-flight diagnostic, which
  /// acts like a diagnostic if it is initialized with a diagnostic but is
  /// otherwise inert.
  class CoerceDiagnostic {
    Optional<InFlightDiagnostic> Diag;
    
  public:
    CoerceDiagnostic() : Diag() { }
    CoerceDiagnostic(CoerceDiagnostic &&Other) : Diag(std::move(Other.Diag)) { }
    CoerceDiagnostic(InFlightDiagnostic &&Diag) : Diag(std::move(Diag)) { }
    
    /// \brief Add a source range to the diagnostic.
    CoerceDiagnostic &operator<<(SourceRange R) {
      if (Diag)
        *Diag << R;
      return *this;
    }
  };
  
  /// diagnose - Diagnose a problem encountered during type coercion.
  template<typename ...ArgTypes>
  CoerceDiagnostic diagnose(ArgTypes &&...Args) {
    if (Flags & CF_Apply)
      return TC.diagnose(std::forward<ArgTypes>(Args)...);
    
    return CoerceDiagnostic();
  }

  /// unchanged - Return an unchanged expressions as a coerced result.
  ///
  /// This routine takes care to produce the correct kind of result both when
  /// we are applying an operation (returning an expression) and when checking
  /// whether coercion will succeed (returning a type).
  CoercedResult unchanged(Expr *E) {
    return unchanged(E, Flags);
  }

  /// unchanged - Return an unchanged expressions as a coerced result. 
  ///
  /// This routine takes care to produce the correct kind of result both when
  /// we are applying an operation (returning an expression) and when checking
  /// whether coercion will succeed (returning a type).
  static CoercedResult unchanged(Expr *E, unsigned Flags) {
    if (!(Flags & CF_Apply))
      return CoercedResult(E->getType());
    
    return CoercedResult(E);
  }

  /// coerce - Return a newly-coerced expression.
  CoercedResult coerced(Expr *E) {
    return coerced(E, Flags);
  }

  /// coerce - Return a newly-coerced expression.
  static CoercedResult coerced(Expr *E, unsigned Flags) {
    assert((Flags & CF_Apply) &&
           "Cannot return a coerced expression when not applying");
    return CoercedResult(E);
  }  

  /// \brief Try to perform a user-defined conversion.
  CoercedResult tryUserConversion(Expr *E);
  
public:
  CoercedResult visitErrorExpr(ErrorExpr *E) {
    return unchanged(E);
  }
  
  CoercedResult visitLiteralExpr(LiteralExpr *E);
  CoercedResult 
  visitInterpolatedStringLiteralExpr(InterpolatedStringLiteralExpr *E);

  CoercedResult visitDeclRefExpr(DeclRefExpr *E) {
    return unchanged(E);
  }
  CoercedResult visitMemberRefExpr(MemberRefExpr *E) {
    return unchanged(E);
  }
  CoercedResult visitExistentialMemberRefExpr(ExistentialMemberRefExpr *E) {
    return unchanged(E);
  }
  CoercedResult visitArchetypeMemberRefExpr(ArchetypeMemberRefExpr *E) {
    return unchanged(E);
  }
  CoercedResult visitNewArrayExpr(NewArrayExpr *E) {
    return unchanged(E);
  }
  CoercedResult visitNewReferenceExpr(NewReferenceExpr *E) {
    return unchanged(E);
  }
  CoercedResult visitOpaqueValueExpr(OpaqueValueExpr *E) {
    return unchanged(E);
  }
  CoercedResult visitSubscriptExpr(SubscriptExpr *E) {
    return unchanged(E);
  }
  CoercedResult visitExistentialSubscriptExpr(ExistentialSubscriptExpr *E) {
    return unchanged(E);
  }
  CoercedResult visitArchetypeSubscriptExpr(ArchetypeSubscriptExpr *E) {
    return unchanged(E);
  }
  CoercedResult visitOverloadedSubscriptExpr(OverloadedSubscriptExpr *E) {
    Type BaseTy = E->getBase()->getType()->getRValueType();

    Type DestElementTy = DestTy->getRValueType();

    llvm::SmallVector<ValueDecl *, 2> Viable;
    ValueDecl *Best = TC.filterOverloadSet(E->getDecls(),
                                           /*OperatorSyntax=*/true,
                                           BaseTy, E->getIndex(),
                                           DestElementTy, Viable);
    
    if (Best) {
      SubscriptDecl *BestSub = cast<SubscriptDecl>(Best);      
      Type ResultTy = LValueType::get(BestSub->getElementType(),
                                      LValueType::Qual::NonHeap,
                                      TC.Context);
      if (!(Flags & CF_Apply))
        return ResultTy;

      Type ContainerTy = Best->getDeclContext()->getDeclaredTypeOfContext();
      if (ContainerTy->isExistentialType()) {
        ExistentialSubscriptExpr *Result
          = new (TC.Context) ExistentialSubscriptExpr(E->getBase(),
                                                      E->getLBracketLoc(),
                                                      E->getIndex(),
                                                      E->getRBracketLoc(),
                                                      BestSub);
        return coerced(TC.semaSubscriptExpr(Result));
      }
      
      if (ContainerTy->is<ArchetypeType>()) {
        ArchetypeSubscriptExpr *Result
          = new (TC.Context) ArchetypeSubscriptExpr(E->getBase(),
                                                    E->getLBracketLoc(),
                                                    E->getIndex(),
                                                    E->getRBracketLoc(),
                                                    BestSub);
        return coerced(TC.semaSubscriptExpr(Result));
      }

      SubscriptExpr *Result
        = new (TC.Context) SubscriptExpr(E->getBase(), E->getLBracketLoc(),
                                         E->getIndex(), E->getRBracketLoc(),
                                         BestSub);
      return coerced(TC.semaSubscriptExpr(Result));
    }
    
    if (!(Flags & CF_Apply))
      return nullptr;
    
    diagnose(E->getLBracketLoc(), diag::subscript_overload_fail,
             !Viable.empty(), BaseTy, E->getIndex()->getType())
      << E->getBase()->getSourceRange() << E->getIndex()->getSourceRange();
    TC.printOverloadSetCandidates(Viable);
    E->setType(ErrorType::get(TC.Context));
    return nullptr;

  }
  
  Type matchLValueType(ValueDecl *val, LValueType *lv, Type BaseTy) {
    if (!val->isReferencedAsLValue())
      return Type();

    Type valTy = val->getType();
    valTy = TC.substMemberTypeWithBase(valTy, BaseTy);
    if (!valTy)
      return Type();

    if (valTy->isEqual(lv->getObjectType()))
      return valTy;

    return Type();
  }

  CoercedResult coerceOverloadToLValue(OverloadSetRefExpr *E, LValueType *lv) {
    Type BaseTy = E->getBaseType();
    for (ValueDecl *val : E->getDecls()) {
      if (Type Matched = matchLValueType(val, lv, BaseTy)) {
        if (!(Flags & CF_Apply))
          return Matched;
        
        // FIXME: We should be handling this like overload resolution, because
        // ambiguities are possible.
        return coerced(TC.buildFilteredOverloadSet(E, val));
      }
    }

    if (Flags & CF_Apply) {
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

    // If DestFT is a FunctionType, get it as one.
    FunctionType *DestFT = DestTy->getAs<FunctionType>();
    
    // Determine which declarations are viable.
    SmallVector<ValueDecl *, 4> Viable;
    for (ValueDecl *Val : E->getDecls()) {
      Type srcTy = Val->getType();
      
      // If we have a non-static method along with an object argument,
      // look past the 'this'.
      if (FuncDecl *Func = dyn_cast<FuncDecl>(Val))
        if (!Func->isStatic() && E->hasBaseObject())
          srcTy = srcTy->getAs<FunctionType>()->getResult();

      // If this overloaded set refers to a member of an archetype, substitute
      // the associated types that depend on that archetype through the
      // source type.
      srcTy = TC.substMemberTypeWithBase(srcTy, E->getBaseType());
      if (!srcTy)
        continue;

      // If we're trying to coerce the overload set to function type that is
      // partially specified, filter based on what we know.
      if (DestFT && DestFT->getInput()->isUnresolvedType()) {
        if (FunctionType *SFT = srcTy->getAs<FunctionType>())
          if (TC.isSubtypeOf(SFT->getResult(), DestFT->getResult())) {
            Viable.push_back(Val);
            continue;
          }
      }
      
      
      DeclRefExpr DRE(Val, E->getLoc(), srcTy);
      if (TC.isCoercibleToType(&DRE, DestTy) == CoercionResult::Failed)
        continue;
      
      Viable.push_back(Val);
    }
    
    if (Viable.size() == 1) {
      if (!(Flags & CF_Apply))
        return DestTy;
      
      return coerced(TC.convertToRValue(TC.buildFilteredOverloadSet(E,Viable)));
    }
      
    if (Flags & CF_Apply) {
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
    // The only valid type for an UME is a OneOfType or function producing one.
    OneOfType *DT = DestTy->getAs<OneOfType>();
    if (DT == 0)
      if (FunctionType *FT = DestTy->getAs<FunctionType>())
        DT = FT->getResult()->getAs<OneOfType>();
      
    if (DT == 0) {
      diagnose(UME->getLoc(), diag::cannot_convert_unresolved_reference,
               UME->getName(), DestTy);
      return nullptr;
    }

    OneOfDecl *D = DT->getDecl();
    // The oneof type must have an element of the specified name.
    OneOfElementDecl *DED = D->getElement(UME->getName());
    if (DED == 0) {
      diagnose(UME->getLoc(), diag::invalid_member_in_type,
               DT, UME->getName());
      diagnose(D->getLoc(), diag::type_declared_here);
      return nullptr;
    }

    if (DED->getType()->is<FunctionType>() != DestTy->is<FunctionType>()) {
      if (DED->getType()->is<FunctionType>())
        diagnose(UME->getLoc(), diag::call_element_function_type,
                 DestTy, UME->getName());
      else
        diagnose(UME->getLoc(), diag::call_element_not_function_type,
                 DED->getType(), UME->getName());

      return nullptr;
    }

    // If it does, then everything is good, resolve the reference.
    if (!(Flags & CF_Apply))
      return DED->getType();
    
    return coerced(new (TC.Context) DeclRefExpr(DED, UME->getColonLoc(),
                                                DED->getType()));
  }  
  
  CoercedResult visitParenExpr(ParenExpr *E) {
    llvm_unreachable("Already special cased in SemaCoerce::coerceToType");
  }
    
  CoercedResult visitTupleExpr(TupleExpr *E) {
    if (DestTy->isEqual(E->getType()))
      return unchanged(E);
      
    return tryUserConversion(E);
  }
  
  CoercedResult visitUnresolvedDeclRefExpr(UnresolvedDeclRefExpr *E) {
    // FIXME: Is this an error-recovery case?
    if (!(Flags & CF_Apply) && !DestTy->isEqual(E->getType()))
      return nullptr;
    
    return unchanged(E);
  }
  CoercedResult visitUnresolvedDotExpr(UnresolvedDotExpr *E) {
    // We can't do much with the destination type here. The expression
    // might end up being valid, if later the base expression gets a type
    // (e.g., if it has literals of unresolved type), or it could always be
    // ill-formed.
    return CoercionResult::Unknowable;
  }

  CoercedResult visitTupleElementExpr(TupleElementExpr *E) {
    // TupleElementExpr is fully resolved.
    llvm_unreachable("This node doesn't exist for unresolved types");
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

  CoercedResult visitCoerceExpr(CoerceExpr *E) {
    return unchanged(E);
  }

  CoercedResult visitConstructExpr(ConstructExpr *E) {
    return unchanged(E);
  }

  CoercedResult visitImplicitConversionExpr(ImplicitConversionExpr *E) {
    return unchanged(E);
  }

  CoercedResult visitAddressOfExpr(AddressOfExpr *E) {
    LValueType *DestLT = DestTy->getAs<LValueType>();
    if (!DestLT) {
      if (!(Flags & CF_Apply))
        return nullptr;
      
      // The '&' is incorrect. Customize our diagnostic based on whether
      // removing the '&' would make the code type-check or not.
      if (coerceToType(E->getSubExpr(), DestTy, TC, Flags & ~CF_Apply)) {
        // The code would have type-checked without '&'; tell the user that the
        // '&' is extraneous and type-check as if it weren't there.
        diagnose(E->getLoc(), diag::explicit_lvalue)
          << E->getSubExpr()->getSourceRange();
        return coerceToType(E->getSubExpr(), DestTy, TC, Flags);
      }

      // The '&' wouldn't fix it, either; produce diagnostics based on coercing
      // the subexpression directly, and the user can deal with the '&'
      // afterward.
      return coerceToType(E->getSubExpr(), DestTy, TC, Flags);
    }
    
    // FIXME: Note that this was an explicit lvalue.
    if (CoercedResult Sub = coerceToType(E->getSubExpr(), DestTy, TC,
                                         Flags & ~CF_UserConversions)) {
      if (!(Flags & CF_Apply))
        return DestTy;
      
      E->setSubExpr(Sub.getExpr());
      E->setType(DestTy);
      return unchanged(E);
    } else {
      return Sub;
    }
  }

  SemaCoerce(TypeChecker &TC, Type DestTy, unsigned Flags)
    : TC(TC), DestTy(DestTy), Flags(Flags) {
    assert(!isa<UnstructuredUnresolvedType>(DestTy));
  }
  
  CoercedResult doIt(Expr *E) {
    return visit(E);
  }
  
  /// coerceToType - This is the main entrypoint to SemaCoerce.
  static CoercedResult coerceToType(Expr *E, Type DestTy, TypeChecker &TC,
                                    unsigned Flags);

  static CoercedResult convertScalarToTupleType(Expr *E, TupleType *DestTy,
                                                unsigned FieldNo, 
                                                TypeChecker &TC,
                                                unsigned Flags);
  static CoercedResult
  convertTupleToTupleType(Expr *E, unsigned NumExprElements,
                          TupleType *DestTy, TypeChecker &TC, unsigned Flags);
  
  /// loadLValue - Load the given lvalue expression.
  static CoercedResult loadLValue(Expr *E, LValueType *LValue, TypeChecker &TC,
                                  unsigned Flags) {
    assert(E->getType()->isEqual(LValue));
    
    // Can't load from an explicit lvalue.
    if (auto AddrOf = dyn_cast<AddressOfExpr>(E->getSemanticsProvidingExpr())) {
      if (Flags & CF_Apply) {
        TC.diagnose(E->getLoc(), diag::load_of_explicit_lvalue,
                    LValue->getObjectType())
          << AddrOf->getSourceRange();
        E->setType(ErrorType::get(TC.Context));
      }
      
      return nullptr;
    }
    
    if (!(Flags & CF_Apply)) {
      // If we're not going to apply the result anyway, just return the
      // appropriate type.
      return LValue->getObjectType();
    }
    
    return coerced(new (TC.Context) LoadExpr(E, LValue->getObjectType()),Flags);
  }
  
  static CoercedResult
  coerceObjectArgument(Expr *E, Type ContainerTy, TypeChecker &TC,
                       unsigned Flags);

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
  const char *MethodName = 0;
  switch (LitTy) {
  case LiteralType::Int:    MethodName = "convertFromIntegerLiteral"; break;
  case LiteralType::Float:  MethodName = "convertFromFloatLiteral"; break;
  case LiteralType::Char:   MethodName = "convertFromCharacterLiteral"; break;
  case LiteralType::String: MethodName = "convertFromStringLiteral"; break;
  }
  assert(MethodName && "Didn't know LitTy");
  MemberLookup Lookup(Ty, TC.Context.getIdentifier(MethodName), TC.TU);
  
  if (!Lookup.isSuccess() || Ty->is<TupleType>() || Ty->is<LValueType>()) {
    diagnose(Loc, diag::type_not_compatible_literal, Ty);
    return std::pair<FuncDecl*, Type>();
  }

  if (Lookup.Results.size() != 1) {
    diagnose(Loc, diag::type_ambiguous_literal_conversion, Ty, MethodName);
    for (MemberLookupResult Res : Lookup.Results) {
      assert(Res.Kind != MemberLookupResult::TupleElement &&
             "Unexpected lookup result in non-tuple");
      diagnose(Res.D->getLoc(), diag::found_candidate);
    }
    return std::pair<FuncDecl*, Type>();
  }
  
  // Verify that the implementation is a metatype 'static' func.
  MemberLookupResult LookupResult = Lookup.Results[0];
  assert(LookupResult.Kind != MemberLookupResult::TupleElement &&
         "Unexpected lookup result in non-tuple");
  if (LookupResult.Kind != MemberLookupResult::MetatypeMember ||
      !isa<FuncDecl>(LookupResult.D)) {
    diagnose(LookupResult.D->getLoc(), diag::type_literal_conversion_not_static,
             Ty, MethodName);
    return std::pair<FuncDecl*, Type>();
  }
  FuncDecl *Method = cast<FuncDecl>(LookupResult.D);
  
  // Check that the type of the 'convertFrom*Literal' method makes
  // sense.  We want a type of "S -> DestTy" where S is the expected type.
  FunctionType *FT = Method->getType()->castTo<FunctionType>();
  
  // The result of the convert function must be the destination type.
  if (!FT->getResult()->isEqual(Ty)) {
    diagnose(Method->getLoc(), 
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

CoercedResult SemaCoerce::visitLiteralExpr(LiteralExpr *E) {
  assert(E->getType()->isUnresolvedType() && "only accepts unresolved types");

  // We can't know if a literal of unresolved type coerces to an existential
  // type, because we haven't picked a literal type yet.
  if (DestTy->isExistentialType())
    return CoercionResult::Unknowable;

  LiteralType LitTy;
  if (isa<IntegerLiteralExpr>(E))
    LitTy = LiteralType::Int;
  else if (isa<FloatLiteralExpr>(E))
    LitTy = LiteralType::Float;
  else if (isa<CharacterLiteralExpr>(E))
    LitTy = LiteralType::Char;
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
    if (Flags & CF_Apply)
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
      if (Flags & CF_Apply) {
        llvm::SmallString<20> Buffer;
        llvm::APFloat::getLargest(Val.getSemantics()).toString(Buffer);
        diagnose(E->getLoc(), diag::float_literal_overflow, Buffer);
      }
      break;
    case llvm::APFloat::opUnderflow: 
      if (Flags & CF_Apply) {
        // Denormals are ok, but reported as underflow by APFloat.
        if (!Val.isZero()) break;
        llvm::SmallString<20> Buffer;
        llvm::APFloat::getSmallest(Val.getSemantics()).toString(Buffer);
        diagnose(E->getLoc(), diag::float_literal_underflow, Buffer);
      }
      break;
    }
    
    if (Flags & CF_Apply)
      E->setType(ArgType);
    Intermediate = E;
  } else if (LitTy == LiteralType::String &&
             ArgType->is<BuiltinRawPointerType>()) {
    // Nothing to do.
    if (Flags & CF_Apply)
      E->setType(ArgType);
    Intermediate = E;
  } else if (LitTy == LiteralType::Char &&
             ArgType->is<BuiltinIntegerType>() &&
             ArgType->getAs<BuiltinIntegerType>()->getBitWidth() == 32) {
    // Nothing to do.
    if (Flags & CF_Apply)
      E->setType(ArgType);
    Intermediate = E;
  } else {
    // Check to see if this is the chaining case, where ArgType itself has a
    // conversion from a Builtin type.
    LiteralInfo = isLiteralCompatibleType(ArgType, E->getLoc(), LitTy);
    if (LiteralInfo.first == 0) {
      diagnose(Method->getLoc(),
               diag::while_processing_literal_conversion_function, DestTy);
      return nullptr;
    }
    
    if (LitTy == LiteralType::Int &&
        LiteralInfo.second->is<BuiltinIntegerType>()) {
      // ok.
    } else if (LitTy == LiteralType::Float &&
               LiteralInfo.second->is<BuiltinFloatType>()) {
      // ok.
    } else if (LitTy == LiteralType::Char &&
               LiteralInfo.second->is<BuiltinIntegerType>() &&
         LiteralInfo.second->getAs<BuiltinIntegerType>()->getBitWidth() == 32) {
      // ok.

    } else if (LitTy == LiteralType::String &&
               LiteralInfo.second->is<BuiltinRawPointerType>()) {
      // ok.
    } else {
      diagnose(Method->getLoc(),
               diag::type_literal_conversion_defined_wrong, DestTy);
      diagnose(E->getLoc(), diag::while_converting_literal, DestTy);
      return nullptr;
    }
    
    // If this a 'chaining' case, recursively convert the literal to the
    // intermediate type, then use our conversion function to finish the
    // translation.
    if (CoercedResult IntermediateRes = coerceToType(E, ArgType, TC, Flags)) {
      if (!(Flags & CF_Apply))
        return DestTy;
      
      Intermediate = IntermediateRes.getExpr();
    } else {
      return IntermediateRes;
    }
    
    // Okay, now Intermediate is known to have type 'ArgType' so we can use a
    // call to our conversion function to finish things off.
  }
  
  if (!(Flags & CF_Apply))
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

CoercedResult SemaCoerce::tryUserConversion(Expr *E) {
  assert((Flags & CF_UserConversions)
         && "Not allowed to perform user conversions!");
  
  // The source type may already be an lvalue; just look into the underlying
  // object type.
  Type SourceTy = E->getType()->getRValueType();

  // We can only perform implicit conversions from nominal types.
  if (!SourceTy->is<NominalType>())
    return nullptr;
  
  MemberLookup Lookup(SourceTy, TC.Context.getIdentifier("__conversion"),
                      TC.TU);
  if (!Lookup.isSuccess())
    return nullptr;
  
  SmallVector<ValueDecl *, 4> Viable;
  for (auto R : Lookup.Results) {
    switch (R.Kind) {
    case MemberLookupResult::MemberProperty:
    case MemberLookupResult::MetatypeMember:
    case MemberLookupResult::MetaArchetypeMember:
    case MemberLookupResult::TupleElement:
    case MemberLookupResult::ExistentialMember:  // FIXME: Should work?
    case MemberLookupResult::ArchetypeMember:    // FIXME: Should work?
      continue;
    
    case MemberLookupResult::MemberFunction:
      if (!R.D->getAttrs().isConversion())
        continue;
      break;
    }
    
    Type ResultTy
      = R.D->getType()->getAs<FunctionType>()->getResult()->
          getAs<FunctionType>()->getResult();
    if (ResultTy->isEqual(DestTy))
      Viable.push_back(R.D);
  }

  // FIXME: Terrible diagnostics!
  if (Viable.size() != 1)
    return nullptr;
  
  if (!(Flags & CF_Apply))
    return DestTy;
  
  // FIXME: Provide a specialized AST node for these conversions.
  Expr *FnRef
    = new (TC.Context) DeclRefExpr(Viable.front(), E->getStartLoc(),
                                   Viable.front()->getTypeOfReference());
  // FIXME: Terrible, terrible hack to get the source ranges to work for
  // these implicit nodes.
  FnRef = new (TC.Context) ParenExpr(E->getStartLoc(), FnRef, E->getEndLoc(),
                                     FnRef->getType());
  ApplyExpr *ApplyThis = new (TC.Context) DotSyntaxCallExpr(FnRef,
                                                            SourceLoc(),
                                                            E);
  Expr *BoundFn = TC.semaApplyExpr(ApplyThis);
  if (!BoundFn)
    return nullptr;
  
  TupleExpr *Args = new (TC.Context) TupleExpr(E->getStartLoc(),
                                               MutableArrayRef<Expr *>(),
                                               nullptr, E->getEndLoc());
  TC.semaTupleExpr(Args);

  ApplyExpr *Call = new (TC.Context) CallExpr(BoundFn, Args);
  return coerced(TC.semaApplyExpr(Call));
}

CoercedResult SemaCoerce::visitInterpolatedStringLiteralExpr(
                            InterpolatedStringLiteralExpr *E) {
  TypeDecl *DestTyDecl = 0;
  if (NominalType *Nominal = DestTy->getAs<NominalType>()) {
    DestTyDecl = Nominal->getDecl();
    if (!isa<StructDecl>(DestTyDecl) && !isa<OneOfDecl>(DestTyDecl))
      DestTyDecl = 0;
  }
  if (!DestTyDecl) {
    if (Flags & CF_Apply)
      diagnose(E->getLoc(), diag::nonstring_interpolation_type, DestTy);
    return nullptr;
  }

  // Find all of the constructors of the string type we're coercing to.
  ConstructorLookup Ctors(DestTy, TC.TU);

  for (auto &Segment : E->getSegments()) {
    // First, try coercing to the string type.
    if (auto CoercedSegment = coerceToType(Segment, DestTy, TC,
                                           Flags & ~CF_Apply)) {
      if (!(Flags & CF_Apply))
        continue;
      
      // Perform the coercion.
      if (CoercedResult R = coerceToType(Segment, DestTy, TC, Flags)) {
        Segment = R.getExpr();
        continue;
      } else {
        return R;
      }
    } else if (CoercedSegment.getKind() == CoercionResult::Unknowable) {
      return CoercedSegment;
    }

    // Second, try to find a constructor to explicitly perform the conversion.
    SmallVector<ValueDecl *, 4> Viable;
    ValueDecl *Best = TC.filterOverloadSet(Ctors.Results,
                                           /*OperatorSyntax=*/true,
                                           DestTy, Segment, Type(),
                                           Viable);
    if (Best) {
      if (!(Flags & CF_Apply))
        continue;

      if (ConstructorDecl *CD = dyn_cast<ConstructorDecl>(Best)) {
        Type ArgTy = CD->getArgumentType();
        if (CoercedResult CoercedS = coerceToType(Segment, ArgTy, TC, Flags)) {
          Segment = new (TC.Context) ConstructExpr(
                                       DestTy,
                                       CoercedS.getExpr()->getStartLoc(),
                                       CD, CoercedS.getExpr());
          continue;
        } else {
          assert(CoercedS.getKind() == CoercionResult::Unknowable);
          return CoercedS;
        }
      }

      Expr *CtorRef = new (TC.Context) DeclRefExpr(Best, Segment->getStartLoc(),
                                                   Best->getTypeOfReference());
      ApplyExpr *Call = new (TC.Context) CallExpr(CtorRef, Segment);
      Expr *Checked = TC.semaApplyExpr(Call);
      if (!Checked)
        return nullptr;
      
      Segment = Checked;
      continue;
    }

    if (Flags & CF_Apply) {
      // FIXME: We want range information here.
      diagnose(Segment->getLoc(), diag::string_interpolation_overload_fail,
               Segment->getType(), DestTy);
      TC.printOverloadSetCandidates(Viable.empty()? Ctors.Results : Viable);
    }
    return nullptr;
  }

  // FIXME: Broken! We should be using some special kind of 'build from string
  // fragments' (informal) protocol here, which is guaranteed to work, rather
  // than going through so many '+' operations.
  if (!(Flags & CF_Apply))
    return DestTy;

  SmallVector<ValueDecl*, 8> plusResults;
  UnqualifiedLookup plus(TC.Context.getIdentifier("+"), &TC.TU);
  for (auto result : plus.Results)
    plusResults.push_back(result.getValueDecl());

  Expr *Result = nullptr;
  for (auto Segment : E->getSegments()) {
    if (!Result) {
      Result = Segment;
      continue;
    }

    // FIXME: Leaks in !Apply mode!
    Expr *Args[] = {Result, Segment};
    TupleExpr *Arg = new (TC.Context) TupleExpr(SourceLoc(),
                      TC.Context.AllocateCopy(MutableArrayRef<Expr *>(Args, 2)),
                                                nullptr, SourceLoc());
    if (TC.semaTupleExpr(Arg))
      return nullptr;
    
    // Perform overload resolution.
    SmallVector<ValueDecl *, 16> Viable;
    ValueDecl *Best = TC.filterOverloadSet(plusResults,
                                           /*OperatorSyntax=*/true,
                                           Type(), Arg, Type(),
                                           Viable);
    if (!Best) {
      if (Flags & CF_Apply) {
        // FIXME: We want range information here.
        diagnose(E->getStartLoc(), diag::string_interpolation_overload_fail,
                 Result->getType(), Segment->getType());
        if (Viable.empty())
          TC.printOverloadSetCandidates(plusResults);
        else
          TC.printOverloadSetCandidates(Viable);
      }
      
      return nullptr;
    }
    
    Expr *Fn = new (TC.Context) DeclRefExpr(Best, Segment->getStartLoc(),
                                            Best->getTypeOfReference());
    Result = TC.semaApplyExpr(new (TC.Context) BinaryExpr(Fn, Arg));
    if (!Result)
      return nullptr;
  }
  
  if (!(Flags & CF_Apply))
    return Result->getType();
  
  E->setSemanticExpr(Result);
  E->setType(Result->getType());
  return coerced(E);
}

CoercedResult SemaCoerce::visitApplyExpr(ApplyExpr *E) {
  // Given a CallExpr a(b) where "a" is an overloaded value, we may be able to
  // prune the overload set based on the known result type.  Doing this may
  // allow the ambiguity to resolve by removing candidates
  // that caused the ambiguity.  For example if we know that the destination
  // type is 'int', and we had "(int) -> int" and "(SomeTy) -> float", we can
  // prune the second one, and then recursively apply 'int' to b.
  //

  if (OverloadSetRefExpr *OSE = dyn_cast<OverloadSetRefExpr>(E->getFn())) {
    SmallVector<ValueDecl*, 4> Viable;
    if (ValueDecl *Best = TC.filterOverloadSet(OSE->getDecls(),
                                               (isa<PrefixUnaryExpr>(E) ||
                                                isa<PostfixUnaryExpr>(E) ||
                                                isa<BinaryExpr>(E)),
                                               OSE->getBaseType(),
                                               E->getArg(),
                                               DestTy, Viable)) {
      if (!(Flags & CF_Apply)) {
        // Determine the type of the resulting call expression.
        Type Ty = Best->getType()->getRValueType();

        if (FunctionType *FnTy = Ty->getAs<FunctionType>())
          return FnTy->getResult();
        
        return Ty;
      }
      
      Expr *Fn = TC.buildFilteredOverloadSet(OSE, Best);
      Fn = TC.convertToRValue(Fn);
      E->setFn(Fn);

      if (Expr *Result = TC.semaApplyExpr(E))
        return coerceToType(Result, DestTy, TC, Flags);
      
      return nullptr;
    }
    
    if (Flags & CF_Apply) {
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
  
  Type FnTy = FunctionType::get(E->getArg()->getType(), DestTy, TC.Context);
  
  if (auto CoRes = SemaCoerce::coerceToType(E->getFn(), FnTy, TC,
                                            Flags & CF_Apply)) {
    if (!(Flags & CF_Apply))
      return DestTy;
    
    E->setFn(CoRes.getExpr());
    
    // FIXME: is this needed?
    if (Expr *Result = TC.semaApplyExpr(E))
      return coerced(Result);

    return nullptr;
  } else {
    return CoRes;
  }
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
  if (Flags & CF_Apply)
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
  if (!(Flags & CF_Apply))
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
                                    unsigned Flags){
  assert(E->getType()->is<TupleType>() && "Unexpected expression");

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
  SmallVector<int, 16>  VarargElementSources;
  bool hasVarargDest = false;

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

    if (DestTy->getFields()[i].isVararg()) {
      // This is a varargs field; eat all the rest of the unnamed elements.
      while (NextInputValue != NumExprElements) {
        if (!UsedElements[NextInputValue] &&
            IdentList[NextInputValue].empty()) {
          VarargElementSources.push_back(NextInputValue);
          UsedElements[NextInputValue] = true;
        }
        ++NextInputValue;
      }
      hasVarargDest = true;
      DestElementSources[i] = -3;
      break;
    }

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
      
      if (Flags & CF_Apply) {
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
      
      if (Flags & CF_Apply) {
        if (IdentList[i].empty())
          TC.diagnose(ErrorLoc, diag::tuple_element_not_used, i, DestTy);
        else
          TC.diagnose(ErrorLoc, diag::named_tuple_element_not_used, 
                      IdentList[i], i, DestTy);
      }
    
      return nullptr;
    }
  }

  // We coerce the source elements to the correct types, then build a shuffle
  // if necessary.
  TupleExpr *TE = dyn_cast<TupleExpr>(E);
  TupleType *ETy = E->getType()->getAs<TupleType>();
  SmallVector<int, 16> NewElements;
  
  bool RebuildSourceType = false;
  for (unsigned i = 0, e = DestTy->getFields().size(); i != e; ++i) {
    // Extract the input element corresponding to this destination element.
    int SrcField = DestElementSources[i];
    assert(SrcField != -1 && "dest field not found?");
    
    if (SrcField == -2) {
      // Use the default element for the tuple.
      NewElements.push_back(-1);
      continue;
    }

    if (SrcField == -3) {
      NewElements.push_back(-2);
      break;
    }

    Type DestEltTy = DestTy->getElementType(i);
    
    // If we are performing coercion for an assignment, and this is the
    // first argument, make it an implicit lvalue.
    unsigned SubFlags = Flags;
    if (SubFlags & CF_Assignment) {
      SubFlags &= ~CF_NotPropagated;
      
      if (i == 0)
        SubFlags |= CF_ImplicitLValue;
    }

    if (ETy) {
      Type ElementTy = ETy->getElementType(SrcField);
      if (TE) {
        // FIXME: We shouldn't need a TupleExpr to handle this coercion.
        // Check to see if the src value can be converted to the destination
        // element type.
        if (CoercedResult Elt = coerceToType(TE->getElement(SrcField), 
                                             DestEltTy, TC, SubFlags)) {
          if (Flags & CF_Apply)
            TE->setElement(SrcField, Elt.getExpr());
        } else {
          // FIXME: QOI: Include a note about this failure!
          return Elt;
        }
        
        // Because we have coerced something in the source tuple, we need to
        // rebuild the type of that tuple.
        RebuildSourceType = true;
      } else if (ETy && !ElementTy->isEqual(DestEltTy)) {
        // FIXME: Allow conversions when we don't have a tuple expression?
        if (Flags & CF_Apply)
          TC.diagnose(E->getLoc(), diag::tuple_element_type_mismatch, SrcField,
                      ElementTy, DestEltTy);
        return nullptr;
      }
    }
    
    NewElements.push_back(SrcField);
  }

  Expr *injectionFn = 0;
  if (hasVarargDest) {
    Type DestEltTy = DestTy->getFields().back().getVarargBaseTy();
    unsigned SubFlags = Flags & ~CF_NotPropagated;
    for (int SrcField : VarargElementSources) {
      Type ElementTy = ETy->getElementType(SrcField);
      if (TE) {
        // FIXME: We shouldn't need a TupleExpr to handle this coercion.
        // Check to see if the src value can be converted to the destination
        // element type.
        if (CoercedResult Elt = coerceToType(TE->getElement(SrcField), 
                                             DestEltTy, TC, SubFlags)) {
          if (Flags & CF_Apply)
            TE->setElement(SrcField, Elt.getExpr());
        } else {
          // FIXME: QOI: Include a note about this failure!
          return Elt;
        }

        // Because we have coerced something in the source tuple, we need to
        // rebuild the type of that tuple.
        RebuildSourceType = true;
      } else if (ETy && !ElementTy->isEqual(DestEltTy)) {
        // FIXME: Allow conversions when we don't have a tuple expression?
        if (Flags & CF_Apply)
          TC.diagnose(E->getLoc(), diag::tuple_element_type_mismatch, SrcField,
                      ElementTy, DestEltTy);
        return nullptr;
      }

      NewElements.push_back(SrcField);
    }

    // Find the appropriate injection function.
    ArraySliceType *sliceType =
        cast<ArraySliceType>(DestTy->getFields().back().getType());
    Type boundType = BuiltinIntegerType::get(64, TC.Context);
    injectionFn = TC.buildArrayInjectionFnRef(sliceType, boundType,
                                              E->getStartLoc());
    if (!injectionFn)
      return nullptr;
  }

  if (!(Flags & CF_Apply))
    return Type(DestTy);

  // If we don't actually need to shuffle, skip building a shuffle.
  bool NullShuffle = TE != 0;
  if (NullShuffle) {
    for (int i = 0, e = NewElements.size(); i != e; i++) {
      if (i != NewElements[i]) {
        NullShuffle = false;
        break;
      }
    }
  }
  
  if (NullShuffle) {
    // Force the type of the expression to match the destination to make
    // the AST consistent.
    E->setType(DestTy);
    return unchanged(E, Flags);
  }

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
  TupleShuffleExpr *TSE = new (TC.Context) TupleShuffleExpr(E, Mapping, DestTy);
  TSE->setVarargsInjectionFunction(injectionFn);
  return coerced(TSE, Flags);
}

/// convertScalarToTupleType - Convert the specified expression to the specified
/// tuple type, which is known to be initializable with one element.
CoercedResult SemaCoerce::convertScalarToTupleType(Expr *E, TupleType *DestTy,
                                                   unsigned ScalarField,
                                                   TypeChecker &TC,
                                                   unsigned Flags) {
  // If the destination is a tuple type with at most one element that has no
  // default value, see if the expression's type is convertable to the
  // element type.  This handles assigning 4 to "(a = 4, b : int)".

  TupleTypeElt Field = DestTy->getFields()[ScalarField];

  // If we are performing coercion for an assignment, and this is the
  // first argument, make it an implicit lvalue.
  unsigned SubFlags = Flags;
  if (SubFlags & CF_Assignment) {
    SubFlags &= ~CF_NotPropagated;
    
    if (ScalarField == 0)
      SubFlags |= CF_ImplicitLValue;
  }

  Type ScalarType = Field.getType();
  if (Field.isVararg())
    ScalarType = Field.getVarargBaseTy();
  CoercedResult ERes = coerceToType(E, ScalarType, TC, SubFlags);
  if (!ERes)
    return ERes;

  if (!(Flags & CF_Apply))
    return Type(DestTy);

  // At this point, we know the conversion is going to succeed;
  // convertTupleToTupleType is just being reused for convenience.
  Expr * Exprs[] = { ERes.getExpr() };
  MutableArrayRef<Expr *> TupleSubExprs =
      TC.Context.AllocateCopy(MutableArrayRef<Expr*>(Exprs));
  TupleExpr *TE = new (TC.Context) TupleExpr(SourceLoc(), TupleSubExprs,
                                             nullptr, SourceLoc());
  TupleType *SrcTT = TupleType::get(TupleTypeElt(ScalarType, Identifier()),
                                    TC.Context);
  TE->setType(SrcTT);
  return convertTupleToTupleType(TE, 1, DestTy, TC, Flags);
}

/// \brief Determine whether the protocol set X is a subset of (or equivalent
/// to) the protocol set Y.
static bool
isProtocolSubset(ArrayRef<ProtocolDecl *> X, ArrayRef<ProtocolDecl *> Y) {
  if (X.empty())
    return true;
  
  if (Y.empty())
    return false;
  
  // Gather the set of protocols in Y and anything they inherit.
  llvm::SmallPtrSet<ProtocolDecl *, 4> YProtos;
  for (auto YProto : Y) {
    if (YProtos.insert(YProto))
      YProto->collectInherited(YProtos);
  }
  
  // Check whether each of the Xs has been found in Y.
  for (auto XProto : X) {
    if (!YProtos.count(XProto))
      return false;
  }
  
  return true;
}

/// \brief Coerce the object argument for a member reference (.) or function
/// application.
CoercedResult
SemaCoerce::coerceObjectArgument(Expr *E, Type ContainerTy, TypeChecker &TC,
                                 unsigned Flags) {
  // Silently propagate errors.
  if (ContainerTy->is<ErrorType>() || E->getType()->is<ErrorType>())
    return nullptr;

  // The type we're converting to is always an lvalue of the given container
  // type.
  ContainerTy = ContainerTy->getRValueType();

  // Determine the type we're converting from, and whether we need to
  // materialize the source.
  Type SrcTy = E->getType();
  LValueType *SrcLV = SrcTy->getAs<LValueType>();
  Type SrcObjectTy = SrcLV ? SrcLV->getObjectType() : SrcTy;

  // Check whether the source object is the same as or a subtype of the
  // container type.
  bool Convertible = false;
  if (SrcObjectTy->isEqual(ContainerTy)) {
    Convertible = true;
  } else {
    SmallVector<ProtocolDecl *, 4> DestProtocols;
    SmallVector<ProtocolDecl *, 4> SrcProtocols;
    if (ContainerTy->isExistentialType(DestProtocols) &&
        SrcObjectTy->isExistentialType(SrcProtocols) &&
        isProtocolSubset(DestProtocols, SrcProtocols))
      Convertible = true;
  }
  
  // Complain if no conversion is possible.
  if (!Convertible) {
    if (Flags & CF_Apply)
      TC.diagnose(E->getLoc(), diag::no_convert_object_arg, SrcObjectTy,
                  ContainerTy);
    return nullptr;
  }

  if (SrcObjectTy->hasReferenceSemantics()) {
    // The destination type is just the source object type.
    if (!(Flags & CF_Apply))
      return SrcObjectTy;

    // If the source is an lvalue, perform an lvalue-to-rvalue conversion.
    if (SrcLV) {
      E = TC.convertLValueToRValue(SrcLV, E);
      return coerced(E, Flags);
    }

    return unchanged(E, Flags);
  }

  // Compute the destination type, which is simply an lvalue of the source
  // object type.
  Type DestTy = LValueType::get(SrcObjectTy,
                                LValueType::Qual::DefaultForMemberAccess,
                                TC.Context);

  // If we're not applying the results, we're done.
  if (!(Flags & CF_Apply))
    return DestTy;
  
  // If we have to materialize the object, do so now.
  if (!SrcLV) {
    E = new (TC.Context) MaterializeExpr(E, DestTy);
    return coerced(E, Flags);
  }
  
  // The source is an lvalue, but we may need to change the qualifiers.
  if (SrcTy->isEqual(DestTy))
    return unchanged(E, Flags);
  
  return coerced(new (TC.Context) RequalifyExpr(E, DestTy), Flags);
}

/// Would the source lvalue type be coercible to the dest lvalue type
/// if it were explicit?
static bool isSubtypeExceptImplicit(LValueType *SrcTy, LValueType *DestTy) {
  return DestTy->getObjectType()->isEqual(SrcTy->getObjectType())
      && SrcTy->getQualifiers() <= DestTy->getQualifiers();
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
                                       unsigned Flags) {
  assert(!isa<UnstructuredUnresolvedType>(DestTy) &&
         "Result of conversion can't be unresolved");

  // Don't bother trying to perform a conversion to or from error type.
  if (DestTy->is<ErrorType>() || E->getType()->is<ErrorType>())
    return nullptr;
  
  // If the destination is a AutoClosing FunctionType, we have special rules.
  if (FunctionType *FT = DestTy->getAs<FunctionType>())
    if (FT->isAutoClosure()) {
      // We require the expression to be an ImplicitClosureExpr that produces
      // DestTy.
      if (E->getType()->isEqual(DestTy) && isa<ImplicitClosureExpr>(E))
        return unchanged(E, Flags);
      
      // If we don't have it yet, force the input to the result of the closure
      // and build the implicit closure.
      if (CoercedResult CoercedE = coerceToType(E, FT->getResult(), TC,
                                                Flags & ~CF_NotPropagated)){
        if (!(Flags & CF_Apply))
          return DestTy;
        
        E = CoercedE.getExpr();
      } else {
        // QOI: Add an note?
        return CoercedE;
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

      return coerced(ICE, Flags);
    }
  
  // If we have an exact match, we're done.
  if (E->getType()->isEqual(DestTy))
    return unchanged(E, Flags);
  
  // If the expression is a grouping parenthesis and it has an unresolved type,
  // just force the type through it, regardless of what DestTy is.
  if (ParenExpr *PE = dyn_cast<ParenExpr>(E)) {
    CoercedResult Sub = coerceToType(PE->getSubExpr(), DestTy, TC, Flags);
    if (!Sub)
      return Sub;
    
    if (!(Flags & CF_Apply))
       return DestTy;
      
    PE->setSubExpr(Sub.getExpr());
    PE->setType(Sub.getType());
    return coerced(PE, Flags);
  }

  if (LValueType *DestLT = DestTy->getAs<LValueType>()) {
    Type SrcTy = E->getType();
    LValueType *SrcLT = SrcTy->getAs<LValueType>();

    // If the input expression has an unresolved type, try to coerce it to an
    // appropriate type.
    if (SrcTy->isUnresolvedType())
      return SemaCoerce(TC, DestTy, Flags).doIt(E);

    if ((Flags & CF_ImplicitLValue) ||
        isa<AddressOfExpr>(E->getSemanticsProvidingExpr())) {
      // Qualification conversion.
      if (SrcLT && DestLT->getObjectType()->isEqual(SrcLT->getObjectType()) &&
          SrcLT->getQualifiers() < DestLT->getQualifiers()) {
        assert(SrcLT->getQualifiers() < DestLT->getQualifiers() &&
               "qualifiers match exactly but types are different?");
        if (!(Flags & CF_Apply))
          return DestTy;
        
        return coerced(new (TC.Context) RequalifyExpr(E, DestTy), Flags);
      }
    }
    
    // Failure.

    // Use a special diagnostic if the coercion would have worked
    // except we needed an explicit marker.
    if (SrcLT && isSubtypeExceptImplicit(SrcLT, DestLT)) {
      if (Flags & CF_Apply)
        TC.diagnose(E->getLoc(), diag::implicit_use_of_lvalue,
                    SrcLT->getObjectType())
          << E->getSourceRange();
      return nullptr;
    }

    // Use a special diagnostic for mismatched l-values.
    if (SrcLT) {
      if (Flags & CF_Apply)
        TC.diagnose(E->getLoc(), diag::invalid_conversion_of_lvalue,
                    SrcLT->getObjectType(), DestLT->getObjectType())
          << E->getSourceRange();
      return nullptr;
    }

    if (Flags & CF_Apply)
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
    
    // If the element of the tuple has unresolved type and is a TupleExpr, try
    // to convert it.
    if (TupleExpr *TE = dyn_cast<TupleExpr>(E))
      return convertTupleToTupleType(TE, TE->getNumElements(), TT, TC, Flags);

    // If the is a scalar to tuple conversion, form the tuple and return it.
    int ScalarFieldNo = TT->getFieldForScalarInit();
    if (ScalarFieldNo != -1)
      return convertScalarToTupleType(E, TT, ScalarFieldNo, TC, Flags);
    
    // If the input is a tuple and the output is a tuple, see if we can convert
    // each element.
    if (TupleType *ETy = E->getType()->getAs<TupleType>())
      return convertTupleToTupleType(E, ETy->getFields().size(), TT, TC, Flags);
  }
  
  // If the input expression has an unresolved type, try to coerce it to an
  // appropriate type.
  if (E->getType()->isUnresolvedType())
    return SemaCoerce(TC, DestTy, Flags).doIt(E);

  // If there is an implicit conversion from the source to the destination
  // type, use it.
  if (Flags & CF_UserConversions) {
    CoercedResult Coerced = SemaCoerce(TC, DestTy, Flags).tryUserConversion(E);
    if (Coerced || Coerced.getKind() == CoercionResult::Unknowable)
      return Coerced;
  }
  
  // If the source is an l-value, load from it.
  if (LValueType *LValue = E->getType()->getAs<LValueType>()) {
    if (CoercedResult Loaded = loadLValue(E, LValue, TC,
                                          Flags & ~CF_NotPropagated)) {
      if (!(Flags & CF_Apply)) {
        // Since we aren't applying the expression anyway, 
        LoadExpr Load(E, Loaded.getType());
        return coerceToType(&Load, DestTy, TC, Flags);
      }
      
      return coerceToType(Loaded.getExpr(), DestTy, TC,
                          Flags & ~CF_NotPropagated);
    } else {
      return Loaded;
    }
  }

  {
    // A value of any given type can be converted to a value of existential if
    // the value's type conforms to each of the protocols.
    SmallVector<ProtocolDecl *, 4> DestProtocols;
    
    if (DestTy->isExistentialType(DestProtocols)) {
      // Check whether the source type conforms to each of the destination
      // protocols.
      SmallVector<ProtocolConformance *, 4> Conformances;
      for (auto DestProto : DestProtocols) {
        ProtocolConformance *Conformance = nullptr;
        if (TC.conformsToProtocol(E->getType(), DestProto, &Conformance)) {
          Conformances.push_back(Conformance);
          continue;
        }
        
        if (Flags & CF_Apply) // FIXME: Say *why* it doesn't conform.
          TC.diagnose(E->getLoc(), diag::invalid_implicit_protocol_conformance,
                      E->getType(), DestProto->getDeclaredType());

        return nullptr;
      }
      
      if (!(Flags & CF_Apply))
        return DestTy;

      return coerced(
               new (TC.Context) ErasureExpr(E, DestTy,
                                  TC.Context.AllocateCopy(Conformances)),
                    Flags);
    }
  }

  // A value of function type can be converted to a value of another function
  // type, so long as the source is a subtype of the destination.
  if (E->getType()->is<FunctionType>() && DestTy->is<FunctionType>()) {
    bool Trivial;
    if (TC.isSubtypeOf(E->getType(), DestTy, Trivial)) {
      if (!(Flags & CF_Apply))
        return DestTy;
      
      return coerced(new (TC.Context) FunctionConversionExpr(E, DestTy,
                                                             Trivial),
                     Flags);
    }
  }

  // Could not do the conversion.
  bool Trivial;
  assert(!TC.isSubtypeOf(E->getType(), DestTy, Trivial) &&
         "subtype relationship not handled by type coercion");
  (void)Trivial;

  // When diagnosing a failed conversion, ignore l-values on the source type.
  if (Flags & CF_Apply)
    TC.diagnose(E->getLoc(), diag::invalid_conversion, E->getType(), DestTy)
      << E->getSourceRange();
  return nullptr;
}

void CoercedExpr::diagnose() const {
  TC.diagnose(E->getLoc(), diag::invalid_conversion, E->getType(), DestTy)
    << E->getSourceRange();
}

CoercedExpr TypeChecker::coerceToType(Expr *E, Type DestTy, bool Assignment) {
  unsigned Flags = CF_Apply | CF_UserConversions;
  if (Assignment)
    Flags |= CF_Assignment;

  CoercedResult Res = SemaCoerce::coerceToType(E, DestTy, *this, Flags);
  return CoercedExpr(*this, Res.getKind(),
                     Res.getKind() == CoercionResult::Succeeded? Res.getExpr()
                                                               : E,
                     DestTy);
}

CoercionResult TypeChecker::isCoercibleToType(Expr *E, Type Ty,
                                              bool Assignment) {
  unsigned Flags = CF_UserConversions;
  if (Assignment)
    Flags |= CF_Assignment;

  return SemaCoerce::coerceToType(E, Ty, *this, Flags).getKind();
}

Expr *TypeChecker::coerceObjectArgument(Expr *E, Type ContainerTy) {
  if (CoercedResult Res = SemaCoerce::coerceObjectArgument(E, ContainerTy,
                                                           *this, CF_Apply))
    return Res.getExpr();
  else if (Res.getKind() == CoercionResult::Unknowable) {
    ContainerTy = ContainerTy->getRValueType();

    Type SrcObjectTy = E->getType()->getRValueType();
    diagnose(E->getLoc(), diag::no_convert_object_arg, SrcObjectTy,
             ContainerTy);
  }

  return nullptr;
}

bool TypeChecker::isCoercibleObjectArgument(Expr *E, Type ContainerTy) {
  // FIXME: Propagate Unknowable to caller.
  return (bool)SemaCoerce::coerceObjectArgument(E, ContainerTy, *this,
                                                /*Flags=*/0);
}

Expr *TypeChecker::convertLValueToRValue(LValueType *srcLV, Expr *E) {
  assert(E && "no expression to load!");
  assert(E->getType()->isEqual(srcLV));
  
  if (CoercedResult Result = SemaCoerce::loadLValue(E, srcLV, *this, CF_Apply))
    return Result.getExpr();
  else {
    assert(Result.getKind() != CoercionResult::Unknowable);
  }

  return nullptr;
}

