//===--- TypeChecker.h - Type Checking Class --------------------*- C++ -*-===//
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
//  This file defines the TypeChecking class.
//
//===----------------------------------------------------------------------===//

#ifndef TYPECHECKING_H
#define TYPECHECKING_H

#include "swift/AST/AST.h"
#include "swift/AST/Diagnostics.h"

namespace swift {

class TypeChecker {
public:
  TranslationUnit &TU;
  ASTContext &Context;
    
private:
  
  /// \brief The 'Any' protocol, used when reinfying existential member
  /// references.
  ProtocolDecl *AnyProto;
  
  /// \brief The 'Range' protocol, used by the for-each loop.
  ProtocolDecl *RangeProto;
  
  Type IntLiteralType;
  Type FloatLiteralType;
  Type CharacterLiteralType;
  Type StringLiteralType;

public:
  TypeChecker(TranslationUnit &TU)
    : TU(TU), Context(TU.Ctx), AnyProto(0), RangeProto(0) {}
  
  template<typename ...ArgTypes>
  InFlightDiagnostic diagnose(ArgTypes... Args) {
    return Context.Diags.diagnose(Args...);
  }

  Type getArraySliceType(SourceLoc loc, Type elementType);
  Expr *buildArrayInjectionFnRef(ArraySliceType *sliceType,
                                 Type lenTy, SourceLoc Loc);

  bool validateType(ValueDecl *VD, bool isFirstPass);
  bool validateType(Type T, bool isFirstPass);

  Type substType(Type T, TypeSubstitutionMap &Substitutions);
  
  bool isSubtypeOf(Type T1, Type T2, bool &Trivial);
  bool isSubtypeOf(Type T1, Type T2) {
    bool Trivial = false;
    return isSubtypeOf(T1, T2, Trivial);
  }
  
  bool semaFunctionSignature(FuncExpr *FE);
  bool semaTupleExpr(TupleExpr *TE);
  Expr *semaSubscriptExpr(SubscriptExpr *SE);
  Expr *semaApplyExpr(ApplyExpr *E);
  Expr *semaUnresolvedDotExpr(UnresolvedDotExpr *E);
  void typeCheckIgnoredExpr(Expr *E);
  void typeCheckFunctionBody(FuncExpr *FE);

  void typeCheckTopLevelReplExpr(Expr *&E, TopLevelCodeDecl *TLCD);
  void REPLCheckPatternBinding(PatternBindingDecl *D);

  /// \brief Perform a shallow recheck of the given newly-built AST node.
  ///
  /// Rechecking typically occurs when one has resolved name lookup and built a
  /// new AST node that then needs to be type-checked.
  Expr *recheckTypes(Expr *E);

  void typeCheckDecl(Decl *D, bool isFirstPass);

  bool typeCheckExpression(Expr *&E, Type ConvertType = Type());
  bool typeCheckPattern(Pattern *P, bool isFirstPass);
  bool coerceToType(Pattern *P, Type Ty, bool isFirstPass);
  bool typeCheckCondition(Expr *&E);
  bool typeCheckArrayBound(Expr *&E, bool requireConstant);
  bool typeCheckAssignment(Expr *&Dest, SourceLoc EqualLoc, Expr *&Src);
  
  /// \brief Retrieve the default literal type for the given literal
  /// expression.
  Type getDefaultLiteralType(LiteralExpr *E);
  
  /// resolveUnresolvedLiterals - Given an expression containing unresolved
  /// literals, resolve those unresolved literals to their default types and
  /// type-check the expression again.
  bool resolveUnresolvedLiterals(Expr *&E);
  
  /// coerceToType - Do semantic analysis of an expression in a context that
  /// expects a particular type.  This performs a conversion to that type if
  /// the types don't match and diagnoses cases where the conversion cannot be
  /// performed.
  ///
  ///
  /// \param Assignment When true, treat this as an assignment.
  ///
  /// This emits a diagnostic and returns null on error.
  Expr *coerceToType(Expr *E, Type Ty, bool Assignment = false);
  
  /// isCoercibleToType - Determine whether the given expression can be 
  /// coerced to the given type.
  bool isCoercibleToType(Expr *E, Type Ty, bool Assignment = false);
  
  /// coerceObjectArgument - Coerce the given expression to an object argument
  /// of the given container type.
  ///
  /// The resulting expression will always be an lvalue, but may be an lvalue
  /// to a subtype of the requested container type.
  Expr *coerceObjectArgument(Expr *E, Type ContainerTy);
  
  /// isCoercibleObjectArgument - Determine whether the given expression can
  /// be coerced to an object argument for a member of the given type.
  bool isCoercibleObjectArgument(Expr *E, Type ContainerTy);
  
  Expr *convertToRValue(Expr *E);
  Expr *convertLValueToRValue(LValueType *SrcLT, Expr *E);
  Expr *convertToMaterializable(Expr *E);

  /// conformsToProtocol - Determine whether the given type conforms to the
  /// given protocol.
  ///
  /// \param ComplainLoc If valid, then this function will emit diagnostics if
  /// T does not conform to the given protocol. The primary diagnostic will
  /// be placed at this location, with notes for each of the protocol
  /// requirements not satisfied.
  ProtocolConformance *conformsToProtocol(Type T, ProtocolDecl *Proto,
                                          SourceLoc ComplainLoc = SourceLoc());
  
  /// \name Overload resolution
  ///
  /// Routines that perform overload resolution or provide diagnostics related
  /// to overload resolution.
  /// @{
  
  /// diagnoseEmptyOverloadSet - Diagnose a case where we disproved all of the
  /// possible candidates in an overload set of a call.
  void diagnoseEmptyOverloadSet(Expr *E, ArrayRef<ValueDecl *> Candidates);
  void printOverloadSetCandidates(ArrayRef<ValueDecl *> Candidates);
  
  /// filterOverloadSet - Filter a set of overload candidates based on the 
  /// the given argument type (for a call) or result type (if the context 
  /// provides such a type). 
  /// 
  /// This routine is used in both type-checking directions (root-to-leaf and
  /// leaf-to-root). The call argument allows leaf-to-root type checking
  /// to filter out candidates that can't be called as functions or have
  /// incompatible arguments. Conversely, when a destination type is provided,
  /// root-to-leaf type checking filters out candidates that cannot produce a
  /// result of an acceptable type. Both argument and destination type may be
  /// provided, which provides overload resolution based on both the call
  /// arguments and the expected result of the call.
  ///
  /// \param Candidates The set of overloaded candidates that should be 
  /// considered
  ///
  /// \param OperatorSyntax Whether this the overloaded function is being
  /// applied with operator syntax (e.g., a + b) rather than call syntax
  /// (e.g., +(a, b)).
  ///
  /// \param BaseTy The type of the object that will become the 'this' pointer
  /// for a call to a method. If not provided, then there is no 'this' object.
  ///
  /// \param Arg The call argument, to be passed to the function that is
  /// eventually selected by overload resolution.
  ///
  /// \param DestTy The type to which the result should be coerced, or null if
  /// not known.
  ///
  /// \param Viable Output vector to which all of the viable candidates will be
  /// added.
  ///
  /// \returns The best candidate, if there is one.
  ValueDecl *filterOverloadSet(ArrayRef<ValueDecl *> Candidates,
                               bool OperatorSyntax,
                               Type BaseTy,
                               Expr *Arg,
                               Type DestTy,
                               SmallVectorImpl<ValueDecl *> &Viable);
  
  /// buildFilteredOverloadSet - Given an overload set that has already been
  /// filtered, produce a new overload set with just the given set of
  /// declarations in it.
  Expr *buildFilteredOverloadSet(OverloadSetRefExpr *OSE,
                                 ArrayRef<ValueDecl *> Remaining);

  /// buildFilteredOverloadSet - Given an overload set for which we have
  /// chosen a best candidate,return an expression that refers to that
  /// candidate.
  Expr *buildFilteredOverloadSet(OverloadSetRefExpr *OSE, ValueDecl *Best);
  /// @}

  /// \brief Retrieve the Any protocol declaration, or build one if none
  /// exists.
  ProtocolDecl *getAnyProtocol(SourceLoc Loc);

  /// \brief Retrieve the Range protocol declaration, if it exists.
  ProtocolDecl *getRangeProtocol();
};

  
  
} // end namespace swift

#endif
