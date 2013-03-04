//===--- TypeCheckCoercion.cpp - Expression Coercion ----------------------===//
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

static bool recordDeduction(CoercionContext &CC, SourceLoc Loc,
                            DeducibleGenericParamType *Deducible,
                            Type DeducedTy, unsigned Flags);

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
  static bool isStringLiteral(LiteralKind LitTy) {
    return LitTy == LiteralKind::UTFString ||
           LitTy == LiteralKind::ASCIIString;
  }

  CoercionContext &CC;
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

  CoercedResult failed(Expr *E) {
    if (Flags & CF_Apply)
      TC.diagnose(E->getLoc(), diag::invalid_conversion, E->getType(), DestTy)
        << E->getSourceRange();
    return nullptr;
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
    // An unresolved DeclRefExpr is a closure argument; we don't know what it
    // will be deduced to.
    return CoercionResult::Unknowable;
  }
  CoercedResult visitMemberRefExpr(MemberRefExpr *E) {
    if (E->getType()->isUnresolvedType())
      return CoercionResult::Unknowable;

    return failed(E); // FIXME: Is this reachable?
  }
  CoercedResult visitSuperMemberRefExpr(SuperMemberRefExpr *E) {
    llvm_unreachable("super member ref not implemented");
  }
  CoercedResult visitExistentialMemberRefExpr(ExistentialMemberRefExpr *E) {
    return failed(E); // FIXME: Is this reachable?
  }
  CoercedResult visitArchetypeMemberRefExpr(ArchetypeMemberRefExpr *E) {
    return failed(E); // FIXME: Is this reachable?
  }
  CoercedResult visitGenericMemberRefExpr(GenericMemberRefExpr *E) {
    return failed(E); // FIXME: Is this reachable?
  }
  CoercedResult visitNewArrayExpr(NewArrayExpr *E) {
    return failed(E); // FIXME: Is this reachable?
  }
  CoercedResult visitNewReferenceExpr(NewReferenceExpr *E) {
    return failed(E); // FIXME: Is this reachable?
  }
  CoercedResult visitMetatypeExpr(MetatypeExpr *E) {
    return failed(E); // We actually could potentially do a coercion here.
  }
  CoercedResult visitOpaqueValueExpr(OpaqueValueExpr *E) {
    // An OpaqueValueExpr can be unresolved if deducible types are involved.
    // If it could be resolved, though, we would have already resolved it.
    return failed(E);
  }
  CoercedResult visitSubscriptExpr(SubscriptExpr *E) {
    if (E->getType()->isUnresolvedType())
      return CoercionResult::Unknowable;

    return failed(E);
  }
  CoercedResult visitSuperSubscriptExpr(SuperSubscriptExpr *E) {
    llvm_unreachable("super subscript not implemented");
  }
  CoercedResult visitExistentialSubscriptExpr(ExistentialSubscriptExpr *E) {
    return failed(E); // FIXME: Is this reachable?
  }
  CoercedResult visitArchetypeSubscriptExpr(ArchetypeSubscriptExpr *E) {
    return failed(E); // FIXME: Is this reachable?
  }
  CoercedResult visitGenericSubscriptExpr(GenericSubscriptExpr *E) {
    return failed(E); // FIXME: Is this reachable?
  }
  CoercedResult visitOverloadedSubscriptExpr(OverloadedSubscriptExpr *E) {
    Type BaseTy = E->getBase()->getType()->getRValueType();

    Type DestElementTy = DestTy->getRValueType();

    llvm::SmallVector<ValueDecl *, 2> Viable;
    auto Best = TC.filterOverloadSet(E->getDecls(), /*OperatorSyntax=*/true,
                                     BaseTy, E->getIndex(), DestElementTy,
                                     Viable);
    
    if (Best) {
      // FIXME: Deal with substitution here!
      SubscriptDecl *BestSub = cast<SubscriptDecl>(Best.getDecl());
      Type ResultTy = LValueType::get(BestSub->getElementType(),
                                      LValueType::Qual::NonHeap,
                                      TC.Context);
      if (!(Flags & CF_Apply))
        return ResultTy;

      Type ContainerTy
        = Best.getDecl()->getDeclContext()->getDeclaredTypeOfContext();
      if (ContainerTy->isExistentialType()) {
        ExistentialSubscriptExpr *Result
          = new (TC.Context) ExistentialSubscriptExpr(E->getBase(),
                                                      E->getIndex(),
                                                      BestSub);
        return coerced(TC.semaSubscriptExpr(Result));
      }
      
      if (ContainerTy->is<ArchetypeType>()) {
        ArchetypeSubscriptExpr *Result
          = new (TC.Context) ArchetypeSubscriptExpr(E->getBase(),
                                                    E->getIndex(),
                                                    BestSub);
        return coerced(TC.semaSubscriptExpr(Result));
      }

      SubscriptExpr *Result
        = new (TC.Context) SubscriptExpr(E->getBase(), E->getIndex(), BestSub);
      return coerced(TC.semaSubscriptExpr(Result));
    }
    
    if (!(Flags & CF_Apply))
      return nullptr;
    
    diagnose(E->getIndex()->getStartLoc(), diag::subscript_overload_fail,
             !Viable.empty(), BaseTy, E->getIndex()->getType())
      << E->getBase()->getSourceRange() << E->getIndex()->getSourceRange();
    TC.printOverloadSetCandidates(Viable);
    E->setType(ErrorType::get(TC.Context));
    return nullptr;

  }
  
  CoercedResult visitOverloadedSuperSubscriptExpr(
                                              OverloadedSuperSubscriptExpr *E) {
    llvm_unreachable("not implemented");
  }

  CoercedResult visitOverloadedExpr(OverloadedExpr Ovl) {
    SourceLoc Loc = Ovl.getExpr()->getLoc();
    SmallVector<ValueDecl *, 4> Viable;
    if (OverloadCandidate Best
          = TC.filterOverloadSetForValue(Ovl.getCandidates(), Loc,
                                         Ovl.getBaseType(), DestTy,
                                         Viable, &CC)) {

      // We can only perform this coercion in a context where we can
      // implicitly treat the expression as an lvalue.
      if (DestTy->is<LValueType>() && !(Flags & CF_ImplicitLValue)) {
        if (Flags & CF_Apply) {
          TC.diagnose(Loc, diag::implicit_use_of_lvalue,
                      Best.getType()->getRValueType())
          << Ovl.getExpr()->getSourceRange();
        }

        return nullptr;
      }

      unsigned SubFlags = Flags;
      if (DestTy->is<LValueType>())
        SubFlags |= CF_ImplicitLValue;

      if (!(Flags & CF_Apply)) {
        // Check the coercion of the (potentially-specialized) best candidate
        // to the destination type succeeds. This also deduced generic
        // arguments from the type of the best candidate.
        Type ResultTy = Best.getType();
        if (!DestTy->is<LValueType>()) {
          ResultTy = ResultTy->getRValueType();
        }
        OpaqueValueExpr OVE(Loc, ResultTy);
        return coerceToType(&OVE, DestTy, CC, SubFlags);
      }

      Expr *Result = TC.buildFilteredOverloadSet(Ovl, Best);
      if (!DestTy->is<LValueType>())
        Result = TC.convertToRValue(Result);
      return coerceToType(Result, DestTy, CC, SubFlags);
    }

    if (Flags & CF_Apply) {
      if (Viable.empty()) {
        diagnose(Loc, diag::no_candidates_ref,
                 Ovl.getCandidates()[0]->getName())
          << Ovl.getExpr()->getSourceRange();
        TC.printOverloadSetCandidates(Ovl.getCandidates());
      } else {
        diagnose(Loc, diag::overloading_ambiguity)
          << Ovl.getExpr()->getSourceRange();
        TC.printOverloadSetCandidates(Viable);
      }
    }

    // If we're not applying and we have more than one candidate, the result
    // is unknowable thus far.
    if (!(Flags & CF_Apply) && !Viable.empty())
      return CoercionResult::Unknowable;
    
    return nullptr;
  }

  CoercedResult visitOverloadSetRefExpr(OverloadSetRefExpr *E) {
    return visitOverloadedExpr(TC.getOverloadedExpr(E));
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
      diagnose(D, diag::type_declared_here);
      return nullptr;
    }

    Type ElemTy = DED->getType()->getAs<AnyFunctionType>()->getResult();
    if (ElemTy->is<FunctionType>() != DestTy->is<FunctionType>()) {
      if (ElemTy->is<FunctionType>())
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

    Expr *E = new (TC.Context) MetatypeExpr(nullptr, UME->getDotLoc(),
                                            MetaTypeType::get(DT, TC.Context));
    E = TC.buildMemberRefExpr(E, SourceLoc(), DED, UME->getDotLoc());
    return coerced(TC.recheckTypes(E));
  }  
  
  CoercedResult visitParenExpr(ParenExpr *E) {
    llvm_unreachable("Already special cased in SemaCoerce::coerceToType");
  }
    
  CoercedResult visitTupleExpr(TupleExpr *E) {
    // We can't coerce a TupleExpr to anything other than a tuple.
    assert(!DestTy->is<TupleType>() &&
           "Already special cased in SemaCoerce::coerceToType");
    return failed(E);
  }
  
  CoercedResult visitCollectionExpr(CollectionExpr *E) {
    llvm_unreachable("not implemented");
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
  CoercedResult visitUnresolvedSpecializeExpr(UnresolvedSpecializeExpr *E) {
    TC.diagnose(E->getLoc(), diag::requires_constraint_checker);
    return nullptr;
  }
  CoercedResult visitUnresolvedSuperMemberExpr(UnresolvedSuperMemberExpr *E) {
    llvm_unreachable("not implemented");
  }

  CoercedResult visitTupleElementExpr(TupleElementExpr *E) {
    // TupleElementExpr is fully resolved.
    llvm_unreachable("This node doesn't exist for unresolved types");
  }
  
  
  CoercedResult visitApplyExpr(ApplyExpr *E);
  
  CoercedResult visitSequenceExpr(SequenceExpr *E) {
    llvm_unreachable("SequenceExprs should all be resolved by this pass");
  }

  CoercedResult visitFuncExpr(FuncExpr *E);

  CoercedResult visitExplicitClosureExpr(ExplicitClosureExpr *E);

  CoercedResult visitImplicitClosureExpr(ImplicitClosureExpr *E) {
    llvm_unreachable("This node doesn't exist for unresolved types");
  }
  
  CoercedResult visitModuleExpr(ModuleExpr *E) {
    llvm_unreachable("This node doesn't exist for unresolved types");
  }

  CoercedResult visitDotSyntaxBaseIgnoredExpr(DotSyntaxBaseIgnoredExpr *E) {
    // FIXME: Coerces the RHS.
    return failed(E);
  }

  CoercedResult visitCoerceExpr(CoerceExpr *E) {
    llvm_unreachable("This node doesn't exist for unresolved types");
  }

  CoercedResult visitDowncastExpr(DowncastExpr *E) {
    llvm_unreachable("This node doesn't exist for unresolved types");
  }

  CoercedResult visitSuperToArchetypeExpr(SuperToArchetypeExpr *E) {
    llvm_unreachable("This node doesn't exist for unresolved types");
  }

  CoercedResult visitImplicitConversionExpr(ImplicitConversionExpr *E) {
    llvm_unreachable("This node doesn't exist for unresolved types");
  }

  CoercedResult visitAddressOfExpr(AddressOfExpr *E) {
    LValueType *DestLT = DestTy->getAs<LValueType>();
    if (!DestLT) {
      if (!(Flags & CF_Apply))
        return nullptr;
      
      // The '&' is incorrect. Customize our diagnostic based on whether
      // removing the '&' would make the code type-check or not.
      if (coerceToType(E->getSubExpr(), DestTy, CC, Flags & ~CF_Apply)) {
        // The code would have type-checked without '&'; tell the user that the
        // '&' is extraneous and type-check as if it weren't there.
        diagnose(E->getLoc(), diag::explicit_lvalue)
          << E->getSubExpr()->getSourceRange();
        return coerceToType(E->getSubExpr(), DestTy, CC, Flags);
      }

      // The '&' wouldn't fix it, either; produce diagnostics based on coercing
      // the subexpression directly, and the user can deal with the '&'
      // afterward.
      return coerceToType(E->getSubExpr(), DestTy, CC, Flags);
    }
    
    // Treat the subexpression as an implicit lvalue.
    unsigned SubFlags = (Flags & ~CF_UserConversions) | CF_ImplicitLValue;
    if (CoercedResult Sub = coerceToType(E->getSubExpr(), DestTy, CC,
                                         SubFlags)) {
      if (!(Flags & CF_Apply))
        return DestTy;

      E->setSubExpr(Sub.getExpr());
      E->setType(DestTy);
      return unchanged(E);
    } else {
      return Sub;
    }
  }

  SemaCoerce(CoercionContext &CC, Type DestTy, unsigned Flags)
    : CC(CC), TC(CC.TC), DestTy(DestTy), Flags(Flags) {
    assert(!DestTy->is<UnstructuredUnresolvedType>());
  }
  
  CoercedResult doIt(Expr *E) {
    assert(E->getType()->isUnresolvedType() && "Unexpected expr");
    return visit(E);
  }
  
  /// coerceToType - This is the main entrypoint to SemaCoerce.
  static CoercedResult coerceToType(Expr *E, Type DestTy, CoercionContext &CC,
                                    unsigned Flags);

  static CoercedResult convertScalarToTupleType(Expr *E, TupleType *DestTy,
                                                unsigned FieldNo, 
                                                CoercionContext &CC,
                                                unsigned Flags);
  static CoercedResult
  convertTupleToTupleType(Expr *E, unsigned NumExprElements, TupleType *DestTy,
                          CoercionContext &CC, unsigned Flags);
  
  /// loadLValue - Load the given lvalue expression.
  static CoercedResult loadLValue(Expr *E, LValueType *LValue,
                                  CoercionContext &CC, unsigned Flags) {
    TypeChecker &TC = CC.TC;
    
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
  coerceObjectArgument(Expr *E, Type ContainerTy, CoercionContext &CC,
                       unsigned Flags);

};
  
} // end anonymous namespace.

std::pair<FuncDecl*, Type>
TypeChecker::isLiteralCompatibleType(Type Ty, SourceLoc Loc, LiteralKind LitTy,
                                     bool Complain) {
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
  
  return std::pair<FuncDecl*, Type>(Method, ArgType);
}

// Check for (Builtin.RawPointer, Builtin.Int64).
static bool isRawPtrAndInt64(Type ty) {
  TupleType *tt = ty->getAs<TupleType>();
  if (!tt)
    return false;
  if (tt->getFields().size() != 2)
    return false;
  if (!tt->getElementType(0)->is<BuiltinRawPointerType>())
    return false;
  BuiltinIntegerType *IntTy =
    tt->getElementType(1)->getAs<BuiltinIntegerType>();
  if (!IntTy)
    return false;
  if (IntTy->getBitWidth() != 64)
    return false;
  return true;
}

CoercedResult SemaCoerce::visitLiteralExpr(LiteralExpr *E) {
  assert(E->getType()->isUnresolvedType() && "only accepts unresolved types");

  // We can't know if a literal of unresolved type coerces to an existential
  // type, because we haven't picked a literal type yet.
  if (DestTy->isExistentialType())
    return CoercionResult::Unknowable;
  
  // If the destination type is equivalent to the default literal type, use
  // the default literal type as the sugared type of the literal.
  Type defaultLiteralTy = TC.getDefaultLiteralType(E);
  if (DestTy->isEqual(defaultLiteralTy))
    DestTy = defaultLiteralTy;

  LiteralKind LitTy;
  if (isa<IntegerLiteralExpr>(E))
    LitTy = LiteralKind::Int;
  else if (isa<FloatLiteralExpr>(E))
    LitTy = LiteralKind::Float;
  else if (isa<CharacterLiteralExpr>(E))
    LitTy = LiteralKind::Char;
  else {
    StringLiteralExpr *StringE = cast<StringLiteralExpr>(E);
    LitTy = LiteralKind::ASCIIString;
    for (unsigned char c : StringE->getValue()) {
      if (c > 127) {
        LitTy = LiteralKind::UTFString;
        break;
      }
    }
  }
  
  // Check the destination type to see if it is compatible with literals,
  // diagnosing the failure if not.
  std::pair<FuncDecl*, Type> LiteralInfo 
    = TC.isLiteralCompatibleType(DestTy, E->getLoc(), LitTy, Flags & CF_Apply);
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
  if (LitTy == LiteralKind::Int &&
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
  } else if (LitTy == LiteralKind::Float &&
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
  } else if (isStringLiteral(LitTy) &&
             ArgType->is<BuiltinRawPointerType>()) {
    // Nothing to do.
    if (Flags & CF_Apply)
      E->setType(ArgType);
    Intermediate = E;
  } else if (isStringLiteral(LitTy) && isRawPtrAndInt64(ArgType)) {
    // Nothing to do.
    if (Flags & CF_Apply)
      E->setType(ArgType);
    Intermediate = E;
  } else if (LitTy == LiteralKind::Char &&
             ArgType->is<BuiltinIntegerType>() &&
             ArgType->getAs<BuiltinIntegerType>()->getBitWidth() == 32) {
    // Nothing to do.
    if (Flags & CF_Apply)
      E->setType(ArgType);
    Intermediate = E;
  } else {
    // Check to see if this is the chaining case, where ArgType itself has a
    // conversion from a Builtin type.
    LiteralInfo = TC.isLiteralCompatibleType(ArgType, E->getLoc(), LitTy,
                                             Flags & CF_Apply);
    if (LiteralInfo.first == 0) {
      diagnose(Method->getLoc(),
               diag::while_processing_literal_conversion_function, DestTy);
      return nullptr;
    }
    
    if (LitTy == LiteralKind::Int &&
        LiteralInfo.second->is<BuiltinIntegerType>()) {
      // ok.
    } else if (LitTy == LiteralKind::Float &&
               LiteralInfo.second->is<BuiltinFloatType>()) {
      // ok.
    } else if (LitTy == LiteralKind::Char &&
               LiteralInfo.second->is<BuiltinIntegerType>() &&
         LiteralInfo.second->getAs<BuiltinIntegerType>()->getBitWidth() == 32) {
      // ok.

    } else if (isStringLiteral(LitTy) &&
               LiteralInfo.second->is<BuiltinRawPointerType>()) {
      // ok.
    } else if (isStringLiteral(LitTy) && isRawPtrAndInt64(LiteralInfo.second)) {
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
    if (CoercedResult IntermediateRes = coerceToType(E, ArgType, CC, Flags)) {
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

  Expr *DRE = new (TC.Context) MetatypeExpr(nullptr, Intermediate->getStartLoc(),
                                            Method->computeThisType());
  DRE = TC.recheckTypes(TC.buildMemberRefExpr(DRE, SourceLoc(), Method,
                                              Intermediate->getStartLoc()));

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
    case MemberLookupResult::ExistentialMember:  // FIXME: Should work?
    case MemberLookupResult::ArchetypeMember:    // FIXME: Should work?
    case MemberLookupResult::GenericParameter:
      continue;
    
    case MemberLookupResult::MemberFunction:
      if (!R.D->getAttrs().isConversion())
        continue;
      break;
    }
    
    // FIXME: PolymorphicFunctionType
    if (R.D->getType()->is<ErrorType>())
      continue;
    Type ResultTy
      = R.D->getType()->castTo<FunctionType>()->getResult()->
          castTo<AnyFunctionType>()->getResult();
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
    if (auto CoercedSegment = coerceToType(Segment, DestTy, CC,
                                           Flags & ~CF_Apply)) {
      if (!(Flags & CF_Apply))
        continue;
      
      // Perform the coercion.
      if (CoercedResult R = coerceToType(Segment, DestTy, CC, Flags)) {
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
    auto Best = TC.filterOverloadSet(Ctors.Results, /*OperatorSyntax=*/true,
                                     DestTy, Segment, Type(), Viable);
    if (Best) {
      if (!(Flags & CF_Apply))
        continue;

      Type DestMetaTy = MetaTypeType::get(DestTy, TC.Context);
      Expr *TypeBase = new (TC.Context) MetatypeExpr(nullptr,
                                                     Segment->getStartLoc(),
                                                     DestMetaTy);
      Expr *CtorRef = new (TC.Context) DeclRefExpr(Best.getDecl(),
                                                   Segment->getStartLoc(),
                                                   Best.getDecl()->getType());
      CtorRef = new (TC.Context) ConstructorRefCallExpr(CtorRef, TypeBase);
      CtorRef = TC.recheckTypes(CtorRef);
      if (!CtorRef)
        return nullptr;

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
    auto Best = TC.filterOverloadSet(plusResults, /*OperatorSyntax=*/true,
                                     Type(), Arg, Type(), Viable);
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

    Expr *Fn = TC.buildRefExpr(Best, Segment->getStartLoc());
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

  if (OverloadedExpr Ovl = TC.getOverloadedExpr(E->getFn())) {
    SmallVector<ValueDecl*, 4> Viable;
    if (auto Best = TC.filterOverloadSet(Ovl.getCandidates(),
                                         (isa<PrefixUnaryExpr>(E) ||
                                          isa<PostfixUnaryExpr>(E) ||
                                          isa<BinaryExpr>(E)),
                                          Ovl.getBaseType(),
                                          E->getArg(),
                                          DestTy, Viable)) {
      if (!(Flags & CF_Apply)) {
        // Determine the type of the resulting call expression.
        Type Ty = Best.getType()->getRValueType();

        if (AnyFunctionType *FnTy = Ty->getAs<AnyFunctionType>())
          return FnTy->getResult();
        
        return Ty;
      }
      
      Expr *Fn = TC.buildFilteredOverloadSet(Ovl, Best);
      Fn = TC.convertToRValue(Fn);
      E->setFn(Fn);

      if (Expr *Result = TC.semaApplyExpr(E))
        return coerceToType(Result, DestTy, CC, Flags);
      
      return CoercionResult::Failed;
    }
    
    if (Flags & CF_Apply) {
      if (Viable.empty()) {
        TC.diagnoseEmptyOverloadSet(E, Ovl.getCandidates());
      } else if (E->getArg()->getType()->isUnresolvedType()) {
        return CoercionResult::Unknowable;
      } else {
        diagnose(E->getFn()->getLoc(), diag::overloading_ambiguity)
          << E->getSourceRange();
        TC.printOverloadSetCandidates(Viable);
      }
      E->setType(ErrorType::get(TC.Context));

      // FIXME: Recursively set the 'error' type down to the semantics-providing
      // expression?
      Ovl.getExpr()->setType(ErrorType::get(TC.Context));
      return CoercionResult::Failed;
    }

    if (!Viable.empty() && E->getArg()->getType()->isUnresolvedType())
      return CoercionResult::Unknowable;
    return CoercionResult::Failed;
  }

  // Handle polymorphic function types.
  if (auto PolyFn = E->getFn()->getType()->getAs<PolymorphicFunctionType>()) {
    if (OverloadCandidate Ovl = TC.checkPolymorphicApply(PolyFn,
                                                         CoercionKind::Normal,
                                                         E->getArg(), DestTy)) {
      if (!(Flags & CF_Apply)) {
        return DestTy;
      }

      Expr *Fn = TC.buildSpecializeExpr(E->getFn(), Ovl.getType(),
                                        Ovl.getSubstitutions(),
                                        Ovl.getConformances(),
                                        /*OnlyInnermostParams=*/true);
      E->setFn(Fn);
      return coerceToType(TC.semaApplyExpr(E), DestTy, CC, Flags);
    }
  }

  Type FnTy = FunctionType::get(E->getArg()->getType(), DestTy, TC.Context);
  
  if (auto CoRes = SemaCoerce::coerceToType(E->getFn(), FnTy, CC,
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
      auto p = new (TC.Context) NamedPattern(ArgVars[i]);
      p->setType(FuncInputTT->getElementType(i));
      ArgElts.emplace_back(p);
    }
    ArgPat = TuplePattern::create(TC.Context, loc, ArgElts, loc);
  } else {
    ArgVars[0]->setType(FT->getInput());
    ArgPat = new (TC.Context) NamedPattern(ArgVars[0]);
  }
  ArgPat->setType(FT->getInput());
  E->setPattern(ArgPat);

  Expr *Result = E->getBody();

  // Type check the full expression, verifying that it is fully typed.
  if (TC.typeCheckExpression(Result, FT->getResult()))
    return 0;
  
  E->setBody(Result);

  E->computeCaptures(TC.Context);

  return unchanged(E);
}

CoercedResult SemaCoerce::visitFuncExpr(FuncExpr *E) {
  // Make sure that we're converting the closure to a function type.  If not,
  // diagnose the error.
  FunctionType *FT = DestTy->getAs<FunctionType>();
  if (FT == 0 || E->getNumParamPatterns() != 1) {
    diagnose(E->getStartLoc(), diag::funcexpr_not_function_type, DestTy)
      << E->getSourceRange();
    return nullptr;
  }
  
  // If we're inferring an incompatible result type, diagnose it.
  if (!FT->getResult()->is<UnstructuredUnresolvedType>()) {
    // FIXME: isEquals is almost certainly the wrong predicate here.
    Type ExplicitResultTy = E->getBodyResultTypeLoc().getType();
    if (ExplicitResultTy &&
        !ExplicitResultTy->is<ErrorType>() &&
        !ExplicitResultTy->isEqual(FT->getResult())) {
      diagnose(E->getStartLoc(), diag::funcexpr_incompatible_result,
               ExplicitResultTy, FT->getResult())
        << E->getSourceRange();
      return nullptr;
    }
  }

  // The pattern that specifies the arguments of the FuncExpr must be missing
  // some type information.  e.g. 'a' in "func(a,b : Int) {}".  Try to resolve
  // something useful from DestTy.
  if (!(Flags & CF_Apply))
    return DestTy;

  // Now that we have a FunctionType for the closure, we can know how many
  // arguments are allowed.
  E->setType(FT);

  // Apply inferred argument type information to the argument patterns.
  if (TC.coerceToType(E->getArgParamPatterns()[0], FT->getInput(), false))
    return nullptr;
  if (TC.coerceToType(E->getBodyParamPatterns()[0], FT->getInput(), false))
    return nullptr;
  
  // FIXME: Result type too!
  
  // Now that we potentially resolved something in the FuncExpr, reanalyze it.
  TC.semaFuncExpr(E, false, /*allowUnknownTypes*/true);
  return coerced(E);
}



/// convertTupleToTupleType - Given an expression that has tuple type, convert
/// it to have some other tuple type.
///
/// The caller gives us a list of the expressions named arguments and a count of
/// tuple elements for E in the IdentList+NumIdents array.  DestTy specifies the
/// type to convert to, which is known to be a TupleType.
CoercedResult
SemaCoerce::convertTupleToTupleType(Expr *E, unsigned NumExprElements,
                                    TupleType *DestTy, CoercionContext &CC,
                                    unsigned Flags){
  TypeChecker &TC = CC.TC;
  
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
        if (IdentList[j] == DestElt.getName() && !UsedElements[j]) {
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
  bool Unknowable = false;
  for (unsigned i = 0, e = DestTy->getFields().size(); i != e; ++i) {
    // Extract the input element corresponding to this destination element.
    int SrcField = DestElementSources[i];
    assert(SrcField != -1 && "dest field not found?");
    
    if (SrcField == -2) {
      // Use the default element for the tuple.
      NewElements.push_back(TupleShuffleExpr::DefaultInitialize);
      continue;
    }

    if (SrcField == -3) {
      NewElements.push_back(TupleShuffleExpr::FirstVariadic);
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
        CoercedResult Elt = coerceToType(TE->getElement(SrcField),
                                         DestEltTy, CC, SubFlags);
        switch (Elt.getKind()) {
        case CoercionResult::Succeeded:
          if (Flags & CF_Apply)
            TE->setElement(SrcField, Elt.getExpr());
          break;
            
        case CoercionResult::Failed:
          // FIXME: QOI: Include a note about this failure!
          return Elt;

        case CoercionResult::Unknowable:
          Unknowable = true;
          break;
        }
        
        // Because we have coerced something in the source tuple, we need to
        // rebuild the type of that tuple.
        RebuildSourceType = true;
      } else if (ETy && !TC.isSameType(ElementTy, DestEltTy, &CC)) {
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
        CoercedResult Elt = coerceToType(TE->getElement(SrcField),
                                         DestEltTy, CC, SubFlags);
        switch (Elt.getKind()) {
        case CoercionResult::Succeeded:
          if (Flags & CF_Apply)
            TE->setElement(SrcField, Elt.getExpr());
          break;
          
        case CoercionResult::Failed:
          // FIXME: QOI: Include a note about this failure!
          return Elt;
          
        case CoercionResult::Unknowable:
          Unknowable = true;
          break;
        }

        // Because we have coerced something in the source tuple, we need to
        // rebuild the type of that tuple.
        RebuildSourceType = true;
      } else if (ETy && !TC.isSameType(ElementTy, DestEltTy, &CC)) {
        // FIXME: Allow conversions when we don't have a tuple expression?
        if (Flags & CF_Apply)
          TC.diagnose(E->getLoc(), diag::tuple_element_type_mismatch, SrcField,
                      ElementTy, DestEltTy);
        return nullptr;
      }

      NewElements.push_back(SrcField);
    }

    if (Unknowable) {
      return CoercionResult::Unknowable;
    }
    
    // Find the appropriate injection function.
    ArraySliceType *sliceType =
        cast<ArraySliceType>(DestTy->getFields().back().getType().getPointer());
    Type boundType = BuiltinIntegerType::get(64, TC.Context);
    injectionFn = TC.buildArrayInjectionFnRef(sliceType, boundType,
                                              E->getStartLoc());
    if (!injectionFn)
      return nullptr;
  } else if (Unknowable) {
    return CoercionResult::Unknowable;
  }
  
  if (!(Flags & CF_Apply))
    return Type(DestTy);

  // If we don't actually need to shuffle, skip building a shuffle.
  bool NullShuffle = TE != 0;
  if (NullShuffle) {
    for (int i = 0, e = NewElements.size(); i != e; ++i) {
      if (i != NewElements[i]) {
        NullShuffle = false;
        break;
      }
    }
  }
  
  if (NullShuffle) {
    // Build a tuple type equivalent to the destination type but preserving the
    // sugar of the source elements.
    // FIXME: This doesn't work if the type has default values because they fail
    // to canonicalize.
    bool hasInits = false;
    SmallVector<TupleTypeElt, 4> destSugarFields;
    
    if (TE) {
      for (int i = 0, e = NewElements.size(); i != e; ++i) {
        TupleTypeElt const &destField = DestTy->getFields()[i];
        assert((destField.getType()->isUnresolvedType() ||
                TE->getElement(i)->getType()->isEqual(destField.getType()))
               && "tuple subexpr type is not equivalent to coerced tuple field");
        destSugarFields.push_back(TupleTypeElt(TE->getElement(i)->getType(),
                                               destField.getName(),
                                               destField.getInit(),
                                               destField.getVarargBaseTy()));
        hasInits |= destField.hasInit();
          
      }
    } else {
      for (int i = 0, e = NewElements.size(); i != e; ++i) {
        TupleTypeElt const &destField = DestTy->getFields()[i];
        assert((destField.getType()->isUnresolvedType() ||
                ETy->getFields()[i].getType()->isEqual(destField.getType()))
               && "subtuple type is not equivalent to coerced tuple field");
        destSugarFields.push_back(TupleTypeElt(ETy->getFields()[i].getType(),
                                               destField.getName(),
                                               destField.getInit(),
                                               destField.getVarargBaseTy()));
        hasInits |= destField.hasInit();
      }
    }
    
    Type destSugarTy = hasInits
      ? DestTy
      : TupleType::get(destSugarFields, DestTy->getASTContext());

    E->setType(destSugarTy);
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
  
  ETy = E->getType()->castTo<TupleType>();
    
  // If we got here, the type conversion is successful, create a new TupleExpr.
  
  // Build a tuple type equivalent to the destination type but preserving the
  // sugar of the shuffled source elements.
  // FIXME: This doesn't work if the type has default values because they fail
  // to canonicalize.
  bool hasInits = false;
  SmallVector<TupleTypeElt, 4> destSugarFields;
  for (int i = 0, e = NewElements.size(); i != e; ++i) {
    TupleTypeElt const &destField = DestTy->getFields()[i];
    if (NewElements[i] == TupleShuffleExpr::DefaultInitialize) {
      hasInits = true;
      destSugarFields.push_back(destField);
      continue;
    }
    
    // FIXME: Transfer sugar from variadic arguments.
    if (NewElements[i] == TupleShuffleExpr::FirstVariadic) {
      destSugarFields.push_back(destField);
      break;
    }
    
    assert(NewElements[i] >= 0);
    
    TupleTypeElt const &srcField = ETy->getFields()[NewElements[i]];
    
    assert((destField.getType()->isUnresolvedType() ||
            srcField.getType()->isEqual(destField.getType()))
           && "shuffled tuple type is not equivalent to result tuple field");
    destSugarFields.push_back(TupleTypeElt(srcField.getType(),
                                           destField.getName(),
                                           destField.getInit(),
                                           destField.getVarargBaseTy()));
    hasInits |= destField.hasInit();
  }
  
  Type destSugarTy = hasInits
    ? DestTy
    : TupleType::get(destSugarFields, DestTy->getASTContext());
  
  ArrayRef<int> Mapping = TC.Context.AllocateCopy(NewElements);
  TupleShuffleExpr *TSE = new (TC.Context) TupleShuffleExpr(E, Mapping,
                                                            destSugarTy);
  TSE->setVarargsInjectionFunction(injectionFn);
  return coerced(TSE, Flags);
}

/// convertScalarToTupleType - Convert the specified expression to the specified
/// tuple type, which is known to be initializable with one element.
CoercedResult SemaCoerce::convertScalarToTupleType(Expr *E, TupleType *DestTy,
                                                   unsigned ScalarField,
                                                   CoercionContext &CC,
                                                   unsigned Flags) {
  TypeChecker &TC = CC.TC;
  
  // If the destination is a tuple type with at most one element that has no
  // default value, see if the expression's type is convertable to the
  // element type.  This handles assigning 4 to "(a = 4, b : int)".

  const TupleTypeElt &Field = DestTy->getFields()[ScalarField];

  // If we are performing coercion for an assignment, and this is the
  // first argument, make it an implicit lvalue.
  unsigned SubFlags = Flags;
  if (SubFlags & CF_Assignment) {
    SubFlags &= ~CF_NotPropagated;
    
    if (ScalarField == 0)
      SubFlags |= CF_ImplicitLValue;
  }

  // If the destination type is variadic, compute the injection function to use.
  Expr *injectionFn = nullptr;
  const TupleTypeElt &LastField = DestTy->getFields().back();

  if (LastField.isVararg()) {
    // Find the appropriate injection function.
    ArraySliceType *sliceType =
      cast<ArraySliceType>(LastField.getType().getPointer());
    Type boundType = BuiltinIntegerType::get(64, TC.Context);
    injectionFn = TC.buildArrayInjectionFnRef(sliceType, boundType,
                                              E->getStartLoc());
    if (!injectionFn)
      return nullptr;
  }

  // If we're initializing the varargs list, use its base type.
  Type ScalarType = Field.getType();
  if (Field.isVararg())
    ScalarType = Field.getVarargBaseTy();

  CoercedResult ERes = coerceToType(E, ScalarType, CC, SubFlags);
  if (!ERes)
    return ERes;
  
  // Preserve the sugar of the scalar field.
  // FIXME: This doesn't work if the type has default values because they fail
  // to canonicalize.
  SmallVector<TupleTypeElt, 4> sugarFields;
  bool hasInit = false;
  size_t i = 0;
  for (auto &field : DestTy->getFields()) {
    if (field.hasInit()) {
      hasInit = true;
      break;
    }
    if (i == ScalarField) {
      if (field.isVararg()) {
        assert((field.getVarargBaseTy()->isUnresolvedType() ||
                ERes.getType()->isEqual(field.getVarargBaseTy())) &&
               "scalar field is not equivalent to dest vararg field?!");

        sugarFields.push_back(TupleTypeElt(field.getType(),
                                           field.getName(),
                                           field.getInit(),
                                           ERes.getType()));
      }
      else {
        assert((field.getType()->isUnresolvedType() ||
                ERes.getType()->isEqual(field.getType())) &&
               "scalar field is not equivalent to dest tuple field?!");
        sugarFields.push_back(TupleTypeElt(ERes.getType(),
                                           field.getName()));
      }
    } else {
      sugarFields.push_back(field);
    }
    ++i;
  }
  
  Type destSugarTy = hasInit ? DestTy : TupleType::get(sugarFields,
                                                       DestTy->getASTContext());

  if (!(Flags & CF_Apply))
    return Type(destSugarTy);

  return CoercedResult(new (TC.Context) ScalarToTupleExpr(ERes.getExpr(),
                                                          destSugarTy,
                                                          ScalarField,
                                                          injectionFn));
}

/// \brief Coerce the object argument for a member reference (.) or function
/// application.
CoercedResult
SemaCoerce::coerceObjectArgument(Expr *E, Type ContainerTy, CoercionContext &CC,
                                 unsigned Flags) {
  // Silently propagate errors.
  if (ContainerTy->is<ErrorType>() || E->getType()->is<ErrorType>())
    return nullptr;

  TypeChecker &TC = CC.TC;
  
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
  bool Trivial;
  if (!TC.isSubtypeOf(SrcObjectTy, ContainerTy, Trivial, &CC)) {
    if (Flags & CF_Apply)
      TC.diagnose(E->getLoc(), diag::no_convert_object_arg, SrcObjectTy,
                  ContainerTy);
    return nullptr;
  }

  if (SrcObjectTy->hasReferenceSemantics() || SrcObjectTy->is<MetaTypeType>()) {
    // The destination type is just the source object type.
    if (!(Flags & CF_Apply))
      return SrcObjectTy;

    return coerceToType(E, ContainerTy, CC, Flags);
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

/// \brief Record that the given archetype was deduced to have the given
/// type within a coercion context. If this deduction conflicts with a
/// prior deduction, complain (if allowed) and return true to indicate failure.
static bool recordDeduction(CoercionContext &CC, SourceLoc Loc,
                            DeducibleGenericParamType *Deducible, Type DeducedTy,
                            unsigned Flags) {
  if (Type ExistingTy = CC.getSubstitution(Deducible)) {
    // Check that the previously-deduced type matches the given type.
    if (!ExistingTy->isEqual(DeducedTy)) {
      if (Flags & CF_Apply) {
        CC.TC.diagnose(Loc, diag::inconsistent_deduction,
                       Deducible->getName().str(),
                       ExistingTy, DeducedTy);
      }
      
      return true;
    }

    return false;
  }

  // Record this new deduction.
  CC.Substitutions[Deducible] = DeducedTy;

  // Check that the deduced type meets all of the requirements placed on the
  // archetype.
  TypeChecker &TC = CC.TC;
  SmallVectorImpl<ProtocolConformance *> &MyConformances
    = CC.Conformance[Deducible];
  for (auto Proto : Deducible->getConformsTo()) {
    ProtocolConformance *Conformance = nullptr;
    if (TC.conformsToProtocol(DeducedTy, Proto, &Conformance)) {
      MyConformances.push_back(Conformance);
      continue;
    }

    if (Flags & CF_Apply) {
      TC.diagnose(Loc, diag::type_does_not_conform, DeducedTy,
                  Proto->getDeclaredType());
      TC.diagnose(Proto, diag::protocol_here, Proto->getName());
    }

    return true;
  }

  return false;
}

/// coerceToType - This is the recursive implementation of
/// coerceToType.  It produces diagnostics and returns null on failure.
CoercedResult SemaCoerce::coerceToType(Expr *E, Type DestTy,
                                       CoercionContext &CC, unsigned Flags) {
  assert(!DestTy->is<UnstructuredUnresolvedType>() &&
         "Result of conversion can't be unresolved");

  // Don't bother trying to perform a conversion to or from error type.
  if (DestTy->is<ErrorType>() || E->getType()->is<ErrorType>())
    return nullptr;

  TypeChecker &TC = CC.TC;
  
  if (FunctionType *FT = DestTy->getAs<FunctionType>()) {
    // If the destination is a AutoClosing FunctionType, we have special rules.
    if (FT->isAutoClosure()) {
      // We require the expression to be an ImplicitClosureExpr that produces
      // DestTy.
      if (E->getType()->isEqual(DestTy) && isa<ImplicitClosureExpr>(E))
        return unchanged(E, Flags);
      
      // If we don't have it yet, force the input to the result of the closure
      // and build the implicit closure.
      if (CoercedResult CoercedE = coerceToType(E, FT->getResult(), CC,
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
      Pat->setType(TupleType::getEmpty(TC.Context));
      ICE->setPattern(Pat);

      // Compute the capture list, now that we have analyzed the expression.
      ICE->computeCaptures(TC.Context);

      return coerced(ICE, Flags);
    }
    
    // FIXME: If the destination is an [objc_block] function type, insert a
    // BridgeToBlockExpr.
    if (FT->isBlock()) {
      if (E->getType()->isEqual(DestTy))
        return unchanged(E, Flags);
      
      FunctionType *NonBlockTy = FunctionType::get(FT->getInput(),
                                                   FT->getResult(),
                                                   TC.Context);
      
      if (CoercedResult CoercedE = coerceToType(E, NonBlockTy, CC,
                                                Flags & ~CF_NotPropagated)) {
        if (!(Flags & CF_Apply))
          return DestTy;
        
        E = CoercedE.getExpr();
        if (E->getType()->isEqual(DestTy))
          return CoercedE;
      } else {
        return CoercedE;
      }
      
      BridgeToBlockExpr *BBE = new (TC.Context) BridgeToBlockExpr(E, FT);
      return coerced(BBE, Flags);
    }
  }

  // If we have an exact match, we're done.
  if (E->getType()->isEqual(DestTy))
    return unchanged(E, Flags);

  // If the destination is deducible, deduce it now (or check it against a
  // prior deduction).
  if (auto Deducible = DestTy->getAs<DeducibleGenericParamType>()) {
    if (!Deducible->isPrimary())
      return CoercionResult::Unknowable;
    
    Type ExistingTy = CC.getSubstitution(Deducible);

    if (E->getType()->isUnresolvedType()) {
      // If we already have a binding for this archetype, try to use it.
      if (ExistingTy)
        return coerceToType(E, ExistingTy, CC, Flags);

      // We don't have a binding, and cannot deduce anything from an
      // unresolved type. Whether coercion will succeed is unknowable.
      return CoercionResult::Unknowable;
    }

    // We always deduce an object type (never a reference type).
    Type DeducedTy = E->getType()->getRValueType();

    if (recordDeduction(CC, E->getLoc(), Deducible, DeducedTy, Flags))
      return nullptr;

    if (!(Flags & CF_Apply))
      return DeducedTy;

    // If the source is an lvalue, load from it.
    if (auto SrcLV = E->getType()->getAs<LValueType>())
      return loadLValue(E, SrcLV, CC, Flags & ~CF_NotPropagated);

    return unchanged(E, Flags);
  }
  
  // If the source is deducible, deduce it now (or check it against a
  // prior deduction).
  if (auto DeducibleSrc = E->getType()->getAs<DeducibleGenericParamType>()) {
    if (!DeducibleSrc->isPrimary())
      return CoercionResult::Unknowable;

    // We always deduce an object type (never a reference type).
    Type DeducedTy = DestTy->getRValueType();

    // Record and check the deduction.
    if (recordDeduction(CC, E->getLoc(), DeducibleSrc, DeducedTy, Flags))
      return nullptr;

    if (!(Flags & CF_Apply))
      return DestTy;

    return unchanged(E, Flags);
  }

  // If the expression is a grouping parenthesis and it has an unresolved type,
  // just force the type through it, regardless of what DestTy is.
  if (ParenExpr *PE = dyn_cast<ParenExpr>(E)) {
    CoercedResult Sub = coerceToType(PE->getSubExpr(), DestTy, CC, Flags);
    if (!Sub)
      return Sub;
    
    if (!(Flags & CF_Apply))
       return DestTy;
      
    PE->setSubExpr(Sub.getExpr());
    PE->setType(Sub.getType());
    return coerced(PE, Flags);
  }

  // If our expression has polymorphic function type, perform deduction for it.
  if (auto polyFn = E->getType()->getAs<PolymorphicFunctionType>()) {
    if (OverloadCandidate Cand = TC.checkPolymorphicUse(polyFn, DestTy,
                                                        E->getLoc(), &CC)) {
      if (!(Flags & CF_Apply)) {
        return Cand.getType();
      }

      Expr *Result = TC.buildSpecializeExpr(E, Cand.getType(),
                                            Cand.getSubstitutions(),
                                            Cand.getConformances(),
                                            /*OnlyInnermostParams=*/true);
      return coerced(Result, Flags);
    }
  }

  if (LValueType *DestLT = DestTy->getAs<LValueType>()) {
    Type SrcTy = E->getType();
    LValueType *SrcLT = SrcTy->getAs<LValueType>();

    // If the input expression has an unresolved type, try to coerce it to an
    // appropriate type, then continue with type checking.
    if (SrcTy->isUnresolvedType()) {
      if (CoercedResult Res = SemaCoerce(CC, DestTy, Flags).doIt(E)) {
        if (Flags & CF_Apply) {
          E = Res.getExpr();
          SrcTy = E->getType();
        } else {
          SrcTy = Res.getType();
        }
        SrcLT = SrcTy->getAs<LValueType>();
      } else {
        return Res;
      }
    }

    if (SrcLT &&
        TC.isSameType(DestLT->getObjectType(), SrcLT->getObjectType(), &CC)) {
      bool AddressAllowed = (Flags & CF_ImplicitLValue)
                         || isa<AddressOfExpr>(E->getSemanticsProvidingExpr());
      bool QualifiersOkay = SrcLT->getQualifiers() <= DestLT->getQualifiers();
      
      // Both references with the same object type (after deduction).
      if (!(Flags & CF_Apply)) {
        return QualifiersOkay && AddressAllowed? CoercedResult(DestTy)
                                               : CoercedResult(nullptr);
      }

      if (QualifiersOkay) {
        // If we weren't actually allowed to make this binding, complain
        // (build build the ASTs anyway).
        if (!AddressAllowed) {
          TC.diagnose(E->getLoc(), diag::implicit_use_of_lvalue,
                      SrcLT->getObjectType())
            << E->getSourceRange();
        }

        if (SrcLT->getQualifiers() < DestLT->getQualifiers())
          return coerced(new (TC.Context) RequalifyExpr(E, DestTy), Flags);
        else
          return unchanged(E, Flags);
      }
    }
    
    // Failure.

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
      return convertTupleToTupleType(TE, TE->getNumElements(), TT, CC, Flags);

    // If the is a scalar to tuple conversion, form the tuple and return it.
    int ScalarFieldNo = TT->getFieldForScalarInit();
    if (ScalarFieldNo != -1)
      return convertScalarToTupleType(E, TT, ScalarFieldNo, CC, Flags);
    
    // If the input is a tuple and the output is a tuple, see if we can convert
    // each element.
    if (TupleType *ETy = E->getType()->getAs<TupleType>())
      return convertTupleToTupleType(E, ETy->getFields().size(), TT, CC, Flags);
  }

  // Try to match up the types, deducing any generic arguments along the way.
  // We do this now, rather than in the earlier isEqual type check, because
  // lvalues and tuple types need to be decomposed first.
  if (TC.isSameType(E->getType(), DestTy, &CC))
    return unchanged(E, Flags);

  // If there is an implicit conversion from the source to the destination
  // type, use it.
  if (Flags & CF_UserConversions) {
    CoercedResult Coerced = SemaCoerce(CC, DestTy, Flags).tryUserConversion(E);
    if (Coerced || Coerced.getKind() == CoercionResult::Unknowable)
      return Coerced;
  }

  // If the source is an l-value, load from it.
  if (LValueType *LValue = E->getType()->getAs<LValueType>()) {
    if (CoercedResult Loaded = loadLValue(E, LValue, CC,
                                          Flags & ~CF_NotPropagated)) {
      if (!(Flags & CF_Apply)) {
        // Since we aren't applying the expression anyway, 
        LoadExpr Load(E, Loaded.getType());
        return coerceToType(&Load, DestTy, CC, Flags);
      }
      
      return coerceToType(Loaded.getExpr(), DestTy, CC,
                          Flags & ~CF_NotPropagated);
    } else {
      return Loaded;
    }
  }

  // If the source and destination types are metatypes, that
  // conversion might be okay.
  if (auto destMetatype = DestTy->getAs<MetaTypeType>()) {
    if (auto srcMetatype = E->getType()->getAs<MetaTypeType>()) {
      if (TC.isTrivialSubtypeOf(srcMetatype->getInstanceType(),
                                destMetatype->getInstanceType())) {
        return coerced(new (TC.Context) MetatypeConversionExpr(E, DestTy),
                       Flags);
      }
    }
  }

  // A value of function type can be converted to a value of another function
  // type, so long as the source is a subtype of the destination.
  if (E->getType()->is<AnyFunctionType>() && DestTy->is<AnyFunctionType>()) {
    bool Trivial;
    if (TC.isSubtypeOf(E->getType(), DestTy, Trivial, &CC)) {
      if (!(Flags & CF_Apply))
        return DestTy;
      
      return coerced(new (TC.Context) FunctionConversionExpr(E, DestTy,
                                                             Trivial),
                     Flags);
    }
  }

  // A value of class type can be converted to a value of the type of its
  // base class.
  if (E->getType()->mayHaveSuperclass() &&
      DestTy->getClassOrBoundGenericClass()) {
    bool Trivial;
    if (TC.isSubtypeOf(E->getType(), DestTy, Trivial, &CC)) {
      if (!(Flags & CF_Apply))
        return DestTy;

      // If we're coming from an archetype, retrieve the superclass from the
      // archetype and convert to it.
      if (auto srcArchetype = E->getType()->getAs<ArchetypeType>()) {
        E = new (TC.Context)ArchetypeToSuperExpr(E,
                                                 srcArchetype->getSuperclass());

        // If the superclass of the archetype is what we were looking for,
        // were done.
        if (E->getType()->isEqual(DestTy))
          return coerced(E, Flags);
      }

      return coerced(new (TC.Context) DerivedToBaseExpr(E, DestTy),
                     Flags);
    }
  }

  // If the input expression has an unresolved type, try to coerce it to an
  // appropriate type.
  if (E->getType()->isUnresolvedType())
    return SemaCoerce(CC, DestTy, Flags).doIt(E);

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

  // Could not do the conversion.
  bool Trivial;
  assert(!TC.isSubtypeOf(E->getType(), DestTy, Trivial, &CC) &&
         "subtype relationship not handled by type coercion");
  (void)Trivial;

  // When diagnosing a failed conversion, ignore l-values on the source type.
  if (Flags & CF_Apply) {
    TC.diagnose(E->getLoc(), diag::invalid_conversion, E->getType(), DestTy)
      << E->getSourceRange();
    }
  return nullptr;
}

void CoercedExpr::diagnose() const {
  TC.diagnose(E->getLoc(), diag::invalid_conversion, E->getType(), DestTy)
    << E->getSourceRange();
}

void CoercionContext::requestSubstitutionsFor(
                                 ArrayRef<DeducibleGenericParamType *> Params) {
  for (auto GP : Params) {
    Substitutions[GP] = Type();
  }
}

bool CoercionContext::hasCompleteSubstitutions() const {
  for (auto S : Substitutions) {
    if (!S.second)
      return false;
  }

  return true;
}

/// \brief "Finalize" a complete set of substitutions by computing
static bool finalizeSubstitutions(CoercionContext &CC, SourceLoc ComplainLoc) {
  TypeChecker &TC = CC.TC;

  // Seed the archetype stack with the set of substitutions we asked for.
  TypeSubstitutionMap TwoStepSubstitutions; // FIXME: Inefficient!
  for (auto &Sub : CC.Substitutions) {
    auto Archetype = Sub.first->getArchetype();
    TwoStepSubstitutions[Archetype] = Sub.second;
  }

  return TC.checkSubstitutions(TwoStepSubstitutions, CC.Conformance,
                               ComplainLoc, &CC.Substitutions);
}

CoercedExpr TypeChecker::coerceToType(Expr *E, Type DestTy, CoercionKind Kind,
                                      CoercionContext *CC) {
  // Preserve the input as-is if it's already of the appropriate type.
  if (E->getType()->isEqual(DestTy))
    return CoercedExpr(*this, CoercionResult::Succeeded, E, DestTy);
  
  unsigned Flags = CF_Apply | CF_UserConversions;
  switch (Kind) {
  case CoercionKind::Normal:
    break;

  case CoercionKind::Assignment:
    Flags |= CF_Assignment;
    break;

  case CoercionKind::ImplicitLValue:
    Flags |= CF_ImplicitLValue;
    break;
  }

  CoercionContext MyCC(*this);
  if (!CC)
    CC = &MyCC;

  // If we require substitution, perform initial deduction/substitution.
  if (CC->requiresSubstitution()) {
    // If we don't already have a complete set of substitutions, attempt
    // coercion without applying the results. This will deduce any generic
    // arguments that can be deduced.
    if (!CC->hasCompleteSubstitutions()) {
      CoercedResult Res = SemaCoerce::coerceToType(E, DestTy, *CC,
                                                   Flags & ~CF_Apply);

      // If we failed here, we're done.
      if (Res.getKind() == CoercionResult::Failed) {
        return CoercedExpr(*this, Res.getKind(), E, DestTy);
      }
    }

    // Substitute the deduced arguments into the type.
    DestTy = substType(DestTy, CC->Substitutions);
    validateTypeSimple(DestTy);
    if (!DestTy)
      return CoercedExpr(*this, CoercionResult::Failed, E, DestTy);
  }

  // If we've completed our substitutions, finalize them by gathering
  // protocol-conformance information for all of the associated types.
  if (CC->requiresSubstitution() && CC->hasCompleteSubstitutions() &&
      finalizeSubstitutions(*CC, E->getLoc()))
    return CoercedExpr(*this, CoercionResult::Failed, E, DestTy);

  CoercedResult Res = SemaCoerce::coerceToType(E, DestTy, *CC, Flags);
  return CoercedExpr(*this, Res.getKind(),
                     Res.getKind() == CoercionResult::Succeeded? Res.getExpr()
                                                               : E,
                     DestTy);
}

CoercionResult TypeChecker::isCoercibleToType(Expr *E, Type Ty,
                                              CoercionKind Kind,
                                              CoercionContext *CC) {
  unsigned Flags = CF_UserConversions;
  switch (Kind) {
  case CoercionKind::Normal:
    break;

  case CoercionKind::Assignment:
    Flags |= CF_Assignment;
    break;

  case CoercionKind::ImplicitLValue:
    Flags |= CF_ImplicitLValue;
    break;
  }

  CoercionContext MyCC(*this);
  if (!CC)
    CC = &MyCC;

  // If we require substitution, perform initial deduction/substitution.
  if (CC->requiresSubstitution()) {
    // If we don't already have a complete set of substitutions, perform
    // coercion first to deduce any generic arguments. If this coercion fails,
    // we're done.
    if (!CC->hasCompleteSubstitutions() &&
        (SemaCoerce::coerceToType(E, Ty, *CC, Flags).getKind()
           == CoercionResult::Failed)) {
        return CoercionResult::Failed;
    }

    // Substitute the deduced arguments into the type.
    Ty = substType(Ty, CC->Substitutions);
    if (!Ty)
      return CoercionResult::Failed;

    validateTypeSimple(Ty);
  }

  // If we've completed our substitutions, finalize them by gathering
  // protocol-conformance information for all of the associated types.
  if (CC->requiresSubstitution() && CC->hasCompleteSubstitutions() &&
      finalizeSubstitutions(*CC, /*ComplainLoc=*/SourceLoc()))
    return CoercionResult::Failed;

  return SemaCoerce::coerceToType(E, Ty, *CC, Flags).getKind();
}

Expr *TypeChecker::coerceObjectArgument(Expr *E, Type ContainerTy,
                                        CoercionContext *CC) {
  CoercionContext MyCC(*this);
  if (!CC)
    CC = &MyCC;

  if (CoercedResult Res = SemaCoerce::coerceObjectArgument(E, ContainerTy,
                                                           *CC, CF_Apply))
    return Res.getExpr();
  else if (Res.getKind() == CoercionResult::Unknowable) {
    ContainerTy = ContainerTy->getRValueType();

    Type SrcObjectTy = E->getType()->getRValueType();
    diagnose(E->getLoc(), diag::no_convert_object_arg, SrcObjectTy,
             ContainerTy);
  }

  return nullptr;
}

Expr *TypeChecker::convertLValueToRValue(LValueType *srcLV, Expr *E) {
  assert(E && "no expression to load!");
  assert(E->getType()->isEqual(srcLV));
  
  CoercionContext CC(*this);
  if (CoercedResult Result = SemaCoerce::loadLValue(E, srcLV, CC, CF_Apply))
    return Result.getExpr();
  else {
    assert(Result.getKind() != CoercionResult::Unknowable);
  }

  return nullptr;
}

namespace {
  /// \brief Flags that control how type matching is performed.
  enum TypeMatchingFlags {
    /// \brief Only allow exact matches.
    ST_None = 0x00,
    /// \brief Whether we are comparing the unlabeled types or not.
    ST_Unlabeled = 0x01,
    /// \brief Whether to allow the left-hand type (T1) to be a subtype of
    /// the right-hand type (T2).
    ST_AllowSubtype = 0x02,
    /// \brief Whether to allow 'non-trivial' subtyping relationships, e.g.,
    /// ones that change the representation of the object.
    ST_AllowNonTrivialSubtype = 0x04,
    /// \brief Whether to allow 'non-trivial' subtyping relationships in the
    /// input/result of function types.
    ST_AllowNonTrivialFunctionSubtype = 0x08
  };
}

/// \brief Helper routine for TypeChecker::isSubtypeOf and
/// TypeChecker::isSameType that performs the actual comparison between types.
static bool matchTypes(TypeChecker &TC, Type T1, Type T2, unsigned Flags,
                       bool &Trivial, CoercionContext *CC) {
  assert((!(Flags & ST_AllowNonTrivialFunctionSubtype) ||
          Flags & ST_AllowNonTrivialSubtype) &&
         "Non-trivial function input/result without non-trivial subtyping?");
  
  // If the types are equivalent, we're done.
  if (T1->isEqual(T2))
    return true;

  // If T1 or T2 is a deducible parameter, it gets deduced to T2 or T1,
  // respectively.
  if (CC) {
    if (auto Deducible1 = T1->getAs<DeducibleGenericParamType>()) {
      if (!Deducible1->isPrimary())
        return true;

      if (!T2->isUnresolvedType()) {
        // Record this deduction.
        // FIXME: Would be useful to get the diagnostic back out, if
        // deduction conflicts.
        return !recordDeduction(*CC, SourceLoc(), Deducible1, T2, /*Flags=*/0);
      }
    }
    
    if (auto Deducible2 = T2->getAs<DeducibleGenericParamType>()) {
      if (!Deducible2->isPrimary())
        return true;

      if (!T1->isUnresolvedType()) {
        // Record this deduction.
        // FIXME: Would be useful to get the diagnostic back out, if
        // deduction conflicts.
        return !recordDeduction(*CC, SourceLoc(), Deducible2, T1, /*Flags=*/0);
      }
    }
  }

  // A value of a given type is a subtype of a protocol type if it conforms
  // to that protocol type. We always consider this a non-trivial conversion,
  // because it may require the introduction of a new protocol mapping.
  // FIXME: Depending on our implementation model for protocols, some cases
  // (such as selecting a subset of protocols in an existential type, or
  // selecting a protocol from which the protocol T1 inherits) might actually
  // be considered trivial.
  if (Flags & ST_AllowNonTrivialSubtype) {
    SmallVector<ProtocolDecl *, 4> T2Protos;
    if (T2->isExistentialType(T2Protos)) {
      for (auto Proto2 : T2Protos) {
        if (!TC.conformsToProtocol(T1, Proto2)) {
          return false;
        }
      }
      
      Trivial = false;
      return true;
    }
  }

  if (Flags & ST_AllowSubtype) {
    // Check for a base-class/super-class relationship.
    if (T1->mayHaveSuperclass() && T2->getClassOrBoundGenericClass()) {
      auto classDecl2 = T2->getClassOrBoundGenericClass();
      for (auto super1 = TC.getSuperClassOf(T1); super1;
           super1 = TC.getSuperClassOf(super1)) {
        if (super1->getClassOrBoundGenericClass() != classDecl2)
          continue;
        
        return matchTypes(TC, super1, T2, ST_None, Trivial, CC);
      }
    }
  }

  // From here on, we require the types to have equivalent kind.
  if (T1->getCanonicalType()->getKind() !=
      T2->getCanonicalType()->getKind()) {
    if (Flags & ST_Unlabeled) {
      // If removing the labels changes anything, try again with the unlabeled
      // types.
      Type UnlabeledT1 = T1->getUnlabeledType(TC.Context);
      Type UnlabeledT2 = T2->getUnlabeledType(TC.Context);
      if (UnlabeledT1.getPointer() != T1.getPointer() ||
          UnlabeledT2.getPointer() != T2.getPointer())
        return matchTypes(TC, UnlabeledT1, UnlabeledT2, Flags, Trivial, CC);
    }

    return false;
  }

  // An lvalue type is a subtype of another lvalue type if their object types
  // are the same and its qualifiers are a subset of the qualifiers of the
  // other.
  if (auto LV1 = T1->getAs<LValueType>()) {
    auto LV2 = T2->getAs<LValueType>();

    return (LV1->getQualifiers() == LV2->getQualifiers() ||
            (Flags & ST_AllowSubtype &&
             LV1->getQualifiers() < LV2->getQualifiers())) &&
           matchTypes(TC, LV1->getObjectType(), LV2->getObjectType(),
                      ST_None, Trivial, CC);
  }
  
  // Function types allow covariant result types and contravariant argument
  // types, ignoring labels.
  if (auto Func1 = T1->getAs<FunctionType>()) {
    auto Func2 = T2->castTo<FunctionType>();

    // Compute the flags to be used for the subtyping checks of the input
    // and result types.
    unsigned SubFlags = Flags;
    if (Flags & ST_AllowNonTrivialFunctionSubtype)
      SubFlags = SubFlags & ~ST_AllowNonTrivialFunctionSubtype;
    else if (Flags & ST_AllowNonTrivialSubtype)
      SubFlags = SubFlags & ~ST_AllowNonTrivialSubtype;

    if (Func1->isAutoClosure() != Func2->isAutoClosure()) {
      // [auto_closure] types are subtypes of the corresponding
      // non-[auto_closure] types.
      if (!(Flags & ST_AllowSubtype) || Func2->isAutoClosure())
        return false;
    }

    // Result type can be covariant (or equal), ignoring labels.
    if (!matchTypes(TC,
                     Func1->getResult(),
                     Func2->getResult(),
                     SubFlags, Trivial, CC))
      return false;
    
    // Input types can be contravariant (or equal), ignoring labels.
    return matchTypes(TC,
                      Func2->getInput(),
                      Func1->getInput(),
                      SubFlags, Trivial, CC);
  }
  
  // Tuple types. The subtyping relationship for tuples is based on subtyping
  // of the elements, ignoring field names and default arguments.
  if (auto Tuple1 = T1->getAs<TupleType>()) {
    auto Tuple2 = T2->castTo<TupleType>();
    
    if (Tuple1->getFields().size() != Tuple2->getFields().size())
      return false;
    
    for (unsigned I = 0, N = Tuple1->getFields().size(); I != N; ++I) {
      // Unless we're ignoring labels, names must match.
      const auto &Field1 = Tuple1->getFields()[I];
      const auto &Field2 = Tuple2->getFields()[I];
      if (!(Flags & ST_Unlabeled) && Field1.getName() != Field2.getName())
        return false;

      // Varargs must match.
      if (Field1.isVararg() != Field2.isVararg())
        return false;
      
      if (!matchTypes(TC,
                      Tuple1->getElementType(I),
                      Tuple2->getElementType(I),
                      Flags,
                      Trivial, CC))
        return false;
    }
    
    return true;
  }

  // Bound generic types.
  if (auto Bound1 = T1->getAs<BoundGenericType>()) {
    auto Bound2 = T2->castTo<BoundGenericType>();
    if (Bound1->getDecl() != Bound2->getDecl())
      return false;

    // Match up the parents.
    if (Bound1->getParent() && Bound2->getParent()) {
      if (!matchTypes(TC, Bound1->getParent(), Bound2->getParent(),
                      ST_None, Trivial, CC))
        return false;
    } else {
      assert(!Bound1->getParent() && !Bound2->getParent() &&
             "Parent mismatch with bound generic types");
    }

    auto Args1 = Bound1->getGenericArgs();
    auto Args2 = Bound2->getGenericArgs();

    // Match up each of the argument types.
    // FIXME: Should we ignore labels here?
    assert(Args1.size() == Args2.size() && "Bound generic size mismatch");
    for (unsigned I = 0, N = Args1.size(); I != N; ++I) {
      if (!matchTypes(TC, Args1[I], Args2[I], ST_None, Trivial, CC))
        return false;
    }

    return true;
  }

  // Metatypes.
  if (auto Meta1 = T1->getAs<MetaTypeType>()) {
    auto Meta2 = T2->castTo<MetaTypeType>();
    return matchTypes(TC, Meta1->getInstanceType(), Meta2->getInstanceType(),
                      Flags & ST_AllowSubtype, Trivial, CC);
  }

  // Nominal types.
  if (auto Nominal1 = T1->getAs<NominalType>()) {
    auto Nominal2 = T2->castTo<NominalType>();

    // If we're pointing at different declarations, this can't be the same
    // type.
    if (Nominal1->getDecl() != Nominal2->getDecl())
      return false;

    // If one of the nominal types is missing a parent, the types are
    // only equivalent if both are parentless.
    if (!Nominal1->getParent() || !Nominal2->getParent()) {
      return (bool)Nominal1->getParent() == (bool)Nominal2->getParent();
    }

    // Match up the parents.
    return matchTypes(TC, Nominal1->getParent(), Nominal2->getParent(),
                      ST_None, Trivial, CC);
  }
  return false;  
}

/// \brief Determine whether T1 is a subtype of (or equivalent to) T2.
///
/// \param Trivial Will be set to 'false' if there are any subtyping
/// relationships that aren't 'trivial', in the sense that they require some
/// representation change (such as introducing a protocol-conformance mapping).
///
/// This checks for a non-strict subtyping relationship T1 <= T2.
bool TypeChecker::isSubtypeOf(Type T1, Type T2, bool &Trivial,
                              CoercionContext *CC) {
  Trivial = true;
  unsigned Flags = ST_Unlabeled | ST_AllowSubtype | ST_AllowNonTrivialSubtype
                 |  ST_AllowNonTrivialFunctionSubtype;
  return matchTypes(*this, T1, T2, Flags, Trivial, CC);
}

bool TypeChecker::isTrivialSubtypeOf(Type T1, Type T2, CoercionContext *CC) {
  bool trivial = false;
  unsigned flags = ST_AllowSubtype;
  return matchTypes(*this, T1, T2, flags, trivial, CC);
}

bool TypeChecker::isSameType(Type T1, Type T2, CoercionContext *CC,
                             bool Labeled) {
  bool Trivial = true;
  unsigned Flags = ST_None;
  if (!Labeled)
    Flags |= ST_Unlabeled;
  return matchTypes(*this, T1, T2, Flags, Trivial, CC);
}
