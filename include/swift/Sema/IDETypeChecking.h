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
/// Provides extra type-checking entry points for use during code
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
  class Type;
  class TypeChecker;
  class DeclContext;
  class ConcreteDeclRef;
  class ValueDecl;
  class DeclName;

  /// Typecheck a declaration parsed during code completion.
  void typeCheckCompletionDecl(Decl *D);

  /// Check if T1 is convertible to T2.
  ///
  /// \returns true on convertible, false on not.
  bool isConvertibleTo(Type T1, Type T2, DeclContext &DC);

  bool isEqual(Type T1, Type T2, DeclContext &DC);

  bool canPossiblyEqual(Type T1, Type T2, DeclContext &DC);

  bool canPossiblyConvertTo(Type T1, Type T2, DeclContext &DC);

  void collectDefaultImplementationForProtocolMembers(ProtocolDecl *PD,
                        llvm::SmallDenseMap<ValueDecl*, ValueDecl*> &DefaultMap);

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

  /// Given a type and an extension to the original type decl of that type,
  /// decide if the extension has been applied, i.e. if the requirements of the
  /// extension have been fulfilled.
  /// \returns True on applied, false on not applied.
  bool isExtensionApplied(DeclContext &DC, Type Ty, const ExtensionDecl *ED);

/// The kind of type checking to perform for code completion.
  enum class CompletionTypeCheckKind {
    /// Type check the expression as normal.
    Normal,

    /// Type check the argument to an Objective-C #keyPath.
    KeyPath,
  };

  /// Return the type of an expression parsed during code completion, or
  /// None on error.
  Optional<Type> getTypeOfCompletionContextExpr(
                   ASTContext &Ctx,
                   DeclContext *DC,
                   CompletionTypeCheckKind kind,
                   Expr *&parsedExpr,
                   ConcreteDeclRef &referencedDecl);

  /// Resolve type of operator function with \c opName appending it to \c LHS.
  ///
  /// For \p refKind, use \c DeclRefKind::PostfixOperator for postfix operator,
  /// or \c DeclRefKind::BinaryOperator for infix operator.
  /// On success, returns resolved function type of the operator. The LHS should
  /// already be type-checked. This function guarantees LHS not to be modified.
  FunctionType *getTypeOfCompletionOperator(DeclContext *DC, Expr *LHS,
                                            Identifier opName,
                                            DeclRefKind refKind,
                                            ConcreteDeclRef &referencedDecl);

  /// Typecheck the given expression.
  bool typeCheckExpression(DeclContext *DC, Expr *&parsedExpr);

  /// Partially typecheck the specified function body.
  bool typeCheckAbstractFunctionBodyUntil(AbstractFunctionDecl *AFD,
                                          SourceLoc EndTypeCheckLoc);

  /// Typecheck top-level code parsed during code completion.
  ///
  /// \returns true on success, false on error.
  bool typeCheckTopLevelCodeDecl(TopLevelCodeDecl *TLCD);

  /// Creates a type checker instance on the given AST context, if it
  /// doesn't already have one.
  ///
  /// \returns a reference to the type checker instance.
  TypeChecker &createTypeChecker(ASTContext &Ctx);

  struct ExtensionInfo {
    // The extension with the declarations to apply.
    ExtensionDecl *Ext;
    // The extension that enables the former to apply, if any (i.e. a
    // conditional
    // conformance to Foo enables 'extension Foo').
    ExtensionDecl *EnablingExt;
    bool IsSynthesized;
  };

  using ExtensionGroupOperation =
      llvm::function_ref<void(ArrayRef<ExtensionInfo>)>;

  class SynthesizedExtensionAnalyzer {
    struct Implementation;
    Implementation &Impl;
  public:
    SynthesizedExtensionAnalyzer(NominalTypeDecl *Target,
                                 PrintOptions Options,
                                 bool IncludeUnconditional = true);
    ~SynthesizedExtensionAnalyzer();

    enum class MergeGroupKind : char {
      All,
      MergeableWithTypeDef,
      UnmergeableWithTypeDef,
    };

    void forEachExtensionMergeGroup(MergeGroupKind Kind,
                                    ExtensionGroupOperation Fn);
    bool isInSynthesizedExtension(const ValueDecl *VD);
    bool shouldPrintRequirement(ExtensionDecl *ED, StringRef Req);
    bool hasMergeGroup(MergeGroupKind Kind);
  };
}

#endif
