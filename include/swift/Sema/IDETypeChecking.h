//===--- IDETypeChecking.h - Type-check entry points ------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
/// \file
/// \brief Provides extra type-checking entry points for use during code
/// completion, which happens *without* type-checking an entire file at once.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SEMA_IDETYPECHECKING_H
#define SWIFT_SEMA_IDETYPECHECKING_H

#include "swift/Basic/SourceLoc.h"
#include <memory>

namespace swift {
  class AbstractFunctionDecl;
  class Decl;
  class Expr;
  class LazyResolver;
  class ExtensionDecl;
  class ProtocolDecl;

  /// \brief Typecheck a declaration parsed during code completion.
  ///
  /// \returns true on success, false on error.
  bool typeCheckCompletionDecl(Decl *D);

  /// \brief Check if T1 is convertible to T2.
  ///
  /// \returns true on convertible, false on not.
  bool isConvertibleTo(Type T1, Type T2, DeclContext &DC);

  bool isEqual(Type T1, Type T2, DeclContext &DC);

  bool canPossiblyEqual(Type T1, Type T2, DeclContext &DC);

  bool canPossiblyConvertTo(Type T1, Type T2, DeclContext &DC);

  void collectDefaultImplementationForProtocolMembers(ProtocolDecl *PD,
                        llvm::SmallDenseMap<ValueDecl*, ValueDecl*> &DefaultMap);

  /// \brief Given an unresolved member E and its parent P, this function tries
  /// to infer the type of E.
  /// \returns true on success, false on error.
  bool typeCheckUnresolvedExpr(DeclContext &DC, Expr* E,
                               Expr *P, SmallVectorImpl<Type> &PossibleTypes);

  enum InterestedMemberKind : uint8_t {
    Viable,
    Unviable,
    All,
  };

  struct ResolvedMemberResult {
    struct Implementation;
    Implementation &Impl;

    ResolvedMemberResult();
    ~ResolvedMemberResult();
    operator bool() const;
    bool hasBestOverload() const;
    ValueDecl* getBestOverload() const;
    ArrayRef<ValueDecl*> getMemberDecls(InterestedMemberKind Kind);
  };

  ResolvedMemberResult resolveValueMember(DeclContext &DC, Type BaseTy,
                                         DeclName Name);

  /// \brief Given a type and an extension to the original type decl of that type,
  /// decide if the extension has been applied, i.e. if the requirements of the
  /// extension have been fulfilled.
  /// \returns True on applied, false on not applied.
  bool isExtensionApplied(DeclContext &DC, Type Ty, const ExtensionDecl *ED);

/// The kind of type checking to perform for code completion.
  enum class CompletionTypeCheckKind {
    /// Type check the expression as normal.
    Normal,

    /// Type check the argument to an Objective-C #keyPath.
    ObjCKeyPath,
  };

  /// \brief Return the type of an expression parsed during code completion, or
  /// None on error.
  Optional<Type> getTypeOfCompletionContextExpr(
                   ASTContext &Ctx,
                   DeclContext *DC,
                   CompletionTypeCheckKind kind,
                   Expr *&parsedExpr,
                   ConcreteDeclRef &referencedDecl);

  /// Typecheck the sequence expression \p parsedExpr for code completion.
  ///
  /// This requires that \p parsedExpr is a SequenceExpr and that it contains:
  ///   * ... leading sequence  LHS
  ///   * UnresolvedDeclRefExpr operator
  ///   * CodeCompletionExpr    RHS
  ///
  /// On success, returns false, and replaces parsedExpr with the binary
  /// expression corresponding to the operator.  The type of the operator and
  /// RHS are also set, but the rest of the expression may not be typed
  ///
  /// The LHS should already be type-checked or this will be very slow.
  bool typeCheckCompletionSequence(DeclContext *DC, Expr *&parsedExpr);

  /// Typecheck the given expression.
  bool typeCheckExpression(DeclContext *DC, Expr *&parsedExpr);

  /// Partially typecheck the specified function body.
  bool typeCheckAbstractFunctionBodyUntil(AbstractFunctionDecl *AFD,
                                          SourceLoc EndTypeCheckLoc);

  /// \brief Typecheck top-level code parsed during code completion.
  ///
  /// \returns true on success, false on error.
  bool typeCheckTopLevelCodeDecl(TopLevelCodeDecl *TLCD);

  /// A unique_ptr for LazyResolver that can perform additional cleanup.
  using OwnedResolver = std::unique_ptr<LazyResolver, void(*)(LazyResolver*)>;

  /// Creates a lazy type resolver for use in lookups.
  OwnedResolver createLazyResolver(ASTContext &Ctx);
}

#endif
