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

class MemberLookup;
class TypeChecker;

/// CoercionResult - Describes the result of attempting to coerce an
/// expression to a given type.
enum class CoercionResult {
  Succeeded,
  Failed,
  Unknowable
};

/// CoercedExpr - Describes the result of coercing an expression to a given
/// type.
class CoercedExpr {
  TypeChecker &TC;
  CoercionResult Kind;
  Expr *E;
  Type DestTy;

  void diagnose() const;

public:
  CoercedExpr(TypeChecker &TC, CoercionResult Kind, Expr *E, Type DestTy)
    : TC(TC), Kind(Kind), E(E), DestTy(DestTy) {}

  CoercionResult getKind() const { return Kind; }
  Expr *getExpr() const { return E; }

  explicit operator bool() const { return Kind == CoercionResult::Succeeded; }

  operator Expr*() const {
    switch (Kind) {
    case CoercionResult::Succeeded:
      return E;

    case CoercionResult::Unknowable:
      diagnose();
      // Fall-through

    case CoercionResult::Failed:
      return 0;
    }
  }
};

/// \brief A mapping from archetypes to the protocol-conformance 
typedef llvm::DenseMap<ArchetypeType *, SmallVector<ProtocolConformance *, 2>>
  ConformanceMap;

/// \brief An overload candidate.
/// FIXME: Encode the actual substitutions here in some efficient manner.
class OverloadCandidate {
public:
  /// SubstitutionInfoType - Information about the substitution of generic
  /// parameters required to produce this overload candidate.
  struct SubstitutionInfoType {
    TypeSubstitutionMap Substitutions;
    ConformanceMap Conformances;
  };

private:
  llvm::PointerIntPair<ValueDecl *, 1, bool> DeclAndComplete;
  Type Ty;
  std::unique_ptr<SubstitutionInfoType> SubstitutionInfo;

public:
  OverloadCandidate() : DeclAndComplete(0, false), Ty() { }
  OverloadCandidate(ValueDecl *Value, Type Ty, bool Complete)
    : DeclAndComplete(Value, Complete), Ty(Ty) { }
  OverloadCandidate(ValueDecl *Value, Type Ty,
                    SubstitutionInfoType &&SubstitutionInfo)
    : DeclAndComplete(Value, true), Ty(Ty),
      SubstitutionInfo(new SubstitutionInfoType(std::move(SubstitutionInfo))) {}

  /// \brief Whether this overload candidate is 'complete', meaning that we
  /// can use the declaration (possibly by applying the given substitutions)
  /// immediately.
  bool isComplete() const { return DeclAndComplete.getInt(); }

  /// \brief Retrieve the declaration 
  ValueDecl *getDecl() const { return DeclAndComplete.getPointer(); }

  /// \brief Retrieve the type of a reference to this overload candidate,
  /// after substitution.
  Type getType() const { return Ty; }

  /// \brief Evaluates true if the selected candidate is complete.
  explicit operator bool() const { return isComplete(); }

  /// \brief Determine whether this overload candidate has any substitutions.
  bool hasSubstitutions() const { return SubstitutionInfo != nullptr; }

  /// \brief Retrieve the substitutions for this overload candidate.
  TypeSubstitutionMap &getSubstitutions() const {
    assert(hasSubstitutions() && "Candidate does not have substitutions");
    return SubstitutionInfo->Substitutions;
  }

  /// \brief Retrieve the set of protocol conformance records that go with the
  /// substitutions.
  ConformanceMap &getConformances() const {
    assert(hasSubstitutions() && "Candidate does not have substitutions");
    return SubstitutionInfo->Conformances;
  }
};

/// \brief Captures information about the context of a type coercion, including
/// the type checker instance and the type substitutions deduced by the
/// coercion.
struct CoercionContext {
  CoercionContext(TypeChecker &TC) : TC(TC) { }
  
  TypeChecker &TC;
  TypeSubstitutionMap Substitutions;

  /// \brief Mapping from each of the archetypes to the set of protocol-
  /// conformance mappings for each of the requirements on the archetype.
  llvm::DenseMap<ArchetypeType *, SmallVector<ProtocolConformance *, 2>>
    Conformance;

  /// \brief Identify the set of generic parameters for which we want to
  /// compute substitutions.
  void requestSubstitutionsFor(ArrayRef<GenericParam> Params);
  
  /// \brief Retrieve the substitution for the given deduced archetype, if
  /// known.
  Type getSubstitution(ArchetypeType *Archetype) const {
    TypeSubstitutionMap::const_iterator Pos = Substitutions.find(Archetype);
    assert(Pos != Substitutions.end() && "Not deducible");
    return Pos->second;
  }

  /// \brief Determine whether the given archetype is deducible in this
  /// context.
  bool isDeducible(ArchetypeType *Archetype) const {
    return Substitutions.find(Archetype) != Substitutions.end();
  }

  /// \brief Determine whether the given coercion context requires
  /// substitutions.
  bool requiresSubstitution() const { return !Substitutions.empty(); }

  /// \brief Determine whether this coercion context has complete substitution
  /// information.
  bool hasCompleteSubstitutions() const;
};

class TypeChecker {
public:
  TranslationUnit &TU;
  ASTContext &Context;
    
private:  
  /// \brief The 'Enumerable' protocol, used by the for-each loop.
  ProtocolDecl *EnumerableProto;

  /// \brief The 'Range' protocol, used by the for-each loop.
  ProtocolDecl *RangeProto;
  
  Type IntLiteralType;
  Type FloatLiteralType;
  Type CharacterLiteralType;
  Type StringLiteralType;

public:
  TypeChecker(TranslationUnit &TU)
    : TU(TU), Context(TU.Ctx), EnumerableProto(0), RangeProto(0) {}
  
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

  /// \brief Replace the type \c T of a protocol member given the type of the
  /// base of a member access, \c BaseTy.
  Type substMemberTypeWithBase(Type T, Type BaseTy);

  bool isSubtypeOf(Type T1, Type T2, bool &Trivial);
  bool isSubtypeOf(Type T1, Type T2) {
    bool Trivial = false;
    return isSubtypeOf(T1, T2, Trivial);
  }
  
  bool semaFunctionSignature(FuncExpr *FE);
  bool semaTupleExpr(TupleExpr *TE);
  Expr *semaSubscriptExpr(SubscriptExpr *SE);
  Expr *semaSubscriptExpr(ExistentialSubscriptExpr *SE);
  Expr *semaSubscriptExpr(ArchetypeSubscriptExpr *SE);
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
  /// \param Assignment When true, treat this as an assignment.
  ///
  /// On success, returns the coerced expression. Otherwise, returns either
  /// failure (in which case a diagnostic was produced) or 'unknowable', if
  /// it is unknown whether the coercion can occur (e.g., due to literals that
  /// have not been coerced to any specific type).
  CoercedExpr coerceToType(Expr *E, Type Ty, bool Assignment = false,
                           CoercionContext *CC = nullptr);
  
  /// isCoercibleToType - Determine whether the given expression can be 
  /// coerced to the given type.
  ///
  /// If a non-NULL coercion context is provided, that coercion context
  /// will be used (and updated) based on this coercion.
  ///
  /// The result is a three-state value: the coercion may succeed, may fail, or
  /// it may be unknowable whether it can ever succeed (for example, if
  /// unresolved literals are involved).
  CoercionResult isCoercibleToType(Expr *E, Type Ty, bool Assignment = false,
                                   CoercionContext *CC = nullptr);
  
  /// coerceObjectArgument - Coerce the given expression to an object argument
  /// of the given container type.
  ///
  /// The resulting expression will always be an lvalue, but may be an lvalue
  /// to a subtype of the requested container type.
  Expr *coerceObjectArgument(Expr *E, Type ContainerTy,
                             CoercionContext *CC = nullptr);

  /// isCoercibleObjectArgument - Determine whether the given expression can
  /// be coerced to an object argument for a member of the given type.
  ///
  /// If a non-NULL coercion context is provided, that coercion context
  /// will be used (and updated) based on this coercion.
  bool isCoercibleObjectArgument(Expr *E, Type ContainerTy,
                                 CoercionContext *CC = nullptr);
  
  Expr *convertToRValue(Expr *E);
  Expr *convertLValueToRValue(LValueType *SrcLT, Expr *E);
  Expr *convertToMaterializable(Expr *E);

  /// conformsToProtocol - Determine whether the given type conforms to the
  /// given protocol.
  ///
  /// \param Conformance If non-NULL, and the type does conform to the given
  /// protocol, this will be set to the protocol conformance mapping that
  /// maps the given type \c T to the protocol \c Proto. The mapping may be
  /// NULL, if the mapping is trivial due to T being either an archetype or
  /// an existential type that directly implies conformance to \c Proto.
  ///
  /// \param ComplainLoc If valid, then this function will emit diagnostics if
  /// T does not conform to the given protocol. The primary diagnostic will
  /// be placed at this location, with notes for each of the protocol
  /// requirements not satisfied.
  ///
  /// \returns true if T conforms to the protocol Proto, false otherwise.
  bool conformsToProtocol(Type T, ProtocolDecl *Proto,
                          ProtocolConformance **Conformance = 0,
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
  /// \param Viable If there is no single, complete vialbe candidate, output
  /// vector to which all of the viable candidates will be added.
  ///
  /// \returns The best candidate, if there is one.
  OverloadCandidate filterOverloadSet(ArrayRef<ValueDecl *> Candidates,
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
  /// chosen a best candidate, return an expression that refers to that
  /// candidate.
  Expr *buildFilteredOverloadSet(OverloadSetRefExpr *OSE,
                                 const OverloadCandidate &Candidate);

  /// \brief Build a reference to a declaration, where name lookup returned
  /// the given set of declarations.
  Expr *buildRefExpr(ArrayRef<ValueDecl *> Decls, SourceLoc NameLoc);

  /// \brief Build a reference to a (non-member) overload candidate.
  Expr *buildRefExpr(const OverloadCandidate &Candidate, SourceLoc NameLoc);

  /// \brief Build a reference to a member of the given base expression, where
  /// name lookup for the member returned the given set of declarations. 
  Expr *buildMemberRefExpr(Expr *Base, SourceLoc DotLoc,
                           ArrayRef<ValueDecl *> Decls,
                           SourceLoc MemberLoc);

  /// \brief Build a reference to a member of the given base expression,
  /// given the results of a successful member lookup.
  Expr *buildMemberRefExpr(Expr *Base, SourceLoc DotLoc,
                           MemberLookup &Results,
                           SourceLoc NameLoc);
  /// @}

  /// \brief Retrieve the Enumerable protocol declaration, if it exists.
  ProtocolDecl *getEnumerableProtocol();

  /// \brief Retrieve the Range protocol declaration, if it exists.
  ProtocolDecl *getRangeProtocol();
};

  
  
} // end namespace swift

#endif
