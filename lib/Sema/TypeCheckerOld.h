//===--- TypeCheckerOld.h - Old Type Checker Helpers ------------*- C++ -*-===//
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
//  This file defines types used only by the old type checker.
//
//===----------------------------------------------------------------------===//
#ifndef SWIFT_SEMA_TYPE_CHECKER_OLD_H
#define SWIFT_SEMA_TYPE_CHECKER_OLD_H
#include "TypeChecker.h"
using namespace swift;

/// \brief Wraps an expression that refers to an overloaded set of declarations,
/// which may have various syntactic forms (normal reference, member reference,
/// operator name) and may also refer to a single, generic declaration.
///
/// This class is used when decomposing an expression into the pieces used
/// by overload resolution.
class OverloadedExpr {
  /// \brief The actual expression, along with a bit that indicates whether
  /// this is an overloaded member operation
  llvm::PointerIntPair<Expr *, 1, bool> E;

  union {
    struct {
      ValueDecl * const *Start;
      unsigned Size;
    } Overloaded;

    struct {
      ValueDecl *Single;
    } Generic;
  };

  /// \brief The type of the base of a member access, if in fact this is a
  /// member access.
  Type BaseTy;

public:
  /// \brief Creates an empty overloaded expression.
  OverloadedExpr() : E(nullptr, false) { }

  /// \brief Creates an overloaded expression that refers to a single (generic)
  /// entity.
  OverloadedExpr(Expr *E, Type BaseTy, ValueDecl *Single)
    : E(E, false), BaseTy(BaseTy)
  {
    Generic.Single = Single;
  }

  /// \brief Creates an overloaded expression that refers to a set of
  /// declarations.
  OverloadedExpr(Expr *E, Type BaseTy, ArrayRef<ValueDecl *> Candidates)
    : E(E, true), BaseTy(BaseTy)
  {
    Overloaded.Start = Candidates.data();
    Overloaded.Size = Candidates.size();
  }

  /// \brief Creates an overloaded expression from an overload set expression.
  /// FIXME: This constructor is meant to be temporary; client should go
  /// through TypeChecker::getOverloadExpr() instead.
  OverloadedExpr(OverloadSetRefExpr *OSE)
    : E(OSE, true), BaseTy(OSE->getBaseType())
  {
    Overloaded.Start = OSE->getDecls().data();
    Overloaded.Size = OSE->getDecls().size();
  }

  /// \brief Determine if this overloaded expression stores a valid expression.
  explicit operator bool() const { return E.getPointer(); }

  /// \brief Retrieve the overloaded expression.
  Expr *getExpr() const { return E.getPointer(); }

  /// \brief Retrieve the type of the base object, or a null type if there is
  /// no base.
  Type getBaseType() const { return BaseTy; }

  /// \brief Retrieve the set of overload candidates, which may contain a
  /// single declaration (if it is generic).
  ArrayRef<ValueDecl *> getCandidates() const {
    if (E.getInt()) {
      return ArrayRef<ValueDecl *>(Overloaded.Start, Overloaded.Size);
    }

    return ArrayRef<ValueDecl *>(&Generic.Single, 1);
  }
};

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
  Type InferredBaseTy;
  
public:
  OverloadCandidate() : DeclAndComplete(0, false), Ty() { }
  OverloadCandidate(ValueDecl *Value, Type Ty, bool Complete)
    : DeclAndComplete(Value, Complete), Ty(Ty) { }
  OverloadCandidate(ValueDecl *Value, Type Ty, Type InferredBaseTy)
    : DeclAndComplete(Value, true), Ty(Ty), InferredBaseTy(InferredBaseTy){}
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

  /// \brief Set the declaration.
  void setDecl(ValueDecl *D) { DeclAndComplete.setPointer(D); }

  /// \brief Retrieve the type of a reference to this overload candidate,
  /// after substitution.
  Type getType() const { return Ty; }

  /// \brief Retrieve the inferred base type to be used when referencing
  /// this declaration.
  ///
  /// This inferred base type is used when we're performing operator lookup
  /// into, e.g., protocols, and we need to record the inferred type of 'This'.
  Type getInferredBaseType() const { return InferredBaseTy; }

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

Expr *buildFilteredOverloadSet(TypeChecker &TC,
                               OverloadedExpr Ovl,
                               ArrayRef<ValueDecl *> Remaining);
Expr *buildFilteredOverloadSet(TypeChecker &TC, OverloadedExpr Ovl,
                               const OverloadCandidate &Candidate);

Expr *buildCandidateRefExpr(TypeChecker &TC, const OverloadCandidate &Candidate,
                            SourceLoc NameLoc);

/// \brief Build a reference to a member of the given base expression, where
/// name lookup for the member returned the given set of declarations. 
Expr *buildMemberRefExpr(TypeChecker &TC, Expr *Base, SourceLoc DotLoc,
                         ArrayRef<ValueDecl *> Decls,
                         SourceLoc MemberLoc);

/// \brief Build a reference to a member of the given base expression,
/// given the results of a successful member lookup.
Expr *buildMemberRefExpr(TypeChecker &TC, Expr *Base, SourceLoc DotLoc,
                         MemberLookup &Results,
                         SourceLoc NameLoc);

#endif
