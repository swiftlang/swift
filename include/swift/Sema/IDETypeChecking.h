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

#include "llvm/ADT/MapVector.h"
#include "swift/Basic/SourceLoc.h"
#include <memory>

namespace swift {
  class AbstractFunctionDecl;
  class Decl;
  class Expr;
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

  /// Typecheck binding initializer at \p bindingIndex.
  void typeCheckPatternBinding(PatternBindingDecl *PBD, unsigned bindingIndex);

  /// Check if T1 is convertible to T2.
  ///
  /// \returns true on convertible, false on not.
  bool isConvertibleTo(Type T1, Type T2, bool openArchetypes, DeclContext &DC);

  void collectDefaultImplementationForProtocolMembers(ProtocolDecl *PD,
                        llvm::SmallDenseMap<ValueDecl*, ValueDecl*> &DefaultMap);

  enum InterestedMemberKind : uint8_t {
    Viable,
    Unviable,
    All,
  };

  struct ResolvedMemberResult {
    struct Implementation;
    Implementation *Impl;

    ResolvedMemberResult();
    ~ResolvedMemberResult();
    ResolvedMemberResult(const ResolvedMemberResult &) = delete;
    ResolvedMemberResult & operator=(ResolvedMemberResult &) = delete;
    ResolvedMemberResult(ResolvedMemberResult &&other) {
      Impl = other.Impl;
      other.Impl = nullptr;
    }
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
  bool isExtensionApplied(const DeclContext *DC, Type Ty,
                          const ExtensionDecl *ED);

  /// Given a type and an member value decl , decide if the decl is applied,
  /// i.e. if the \c where requirements of the decl have been fulfilled.
  /// \returns True on applied, false on not applied.
  bool isMemberDeclApplied(const DeclContext *DC, Type Ty, const ValueDecl *VD);

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

  /// Reported type for an expression. This expression is represented by offset
  /// length in the source buffer;
  struct ExpressionTypeInfo {

    /// The start of the expression;
    uint32_t offset;

    /// The length of the expression;
    uint32_t length;

    /// The start of the printed type in a separately given string buffer.
    uint32_t typeOffset;

    /// The length of the printed type
    uint32_t typeLength;

    /// The offsets and lengths of all protocols the type conforms to
    std::vector<std::pair<uint32_t, uint32_t>> protocols;
  };

  /// Collect type information for every expression in \c SF; all types will
  /// be printed to \c OS.
  ArrayRef<ExpressionTypeInfo> collectExpressionType(SourceFile &SF,
    ArrayRef<const char *> ExpectedProtocols,
    std::vector<ExpressionTypeInfo> &scratch,
    bool CanonicalType,
    llvm::raw_ostream &OS);

  /// Resolve a list of mangled names to accessible protocol decls from
  /// the decl context.
  ProtocolDecl *resolveProtocolName(DeclContext *dc, StringRef Name);

  /// FIXME: All of the below goes away once CallExpr directly stores its
  /// arguments.

  /// Return value for getOriginalArgumentList().
  struct OriginalArgumentList {
    SmallVector<Expr *, 4> args;
    SmallVector<Identifier, 4> labels;
    SmallVector<SourceLoc, 4> labelLocs;
    SourceLoc lParenLoc;
    SourceLoc rParenLoc;
    bool hasTrailingClosure = false;
  };

  /// When applying a solution to a constraint system, the type checker rewrites
  /// argument lists of calls to insert default arguments and collect varargs.
  /// Sometimes for diagnostics we want to work on the original argument list as
  /// written by the user; this performs the reverse transformation.
  OriginalArgumentList getOriginalArgumentList(Expr *expr);

  /// Return true if the specified type or a super-class/super-protocol has the
  /// @dynamicMemberLookup attribute on it.
  bool hasDynamicMemberLookupAttribute(Type type);

  /// Returns the root type and result type of the keypath type in a keypath
  /// dynamic member lookup subscript, or \c None if it cannot be determined.
  ///
  /// \param subscript The potential keypath dynamic member lookup subscript.
  Type getRootTypeOfKeypathDynamicMember(SubscriptDecl *subscript);

  Type getResultTypeOfKeypathDynamicMember(SubscriptDecl *subscript);

  /// Collect all the protocol requirements that a given declaration can
  ///   provide default implementations for. VD is a declaration in extension
  ///   declaration. Scratch is the buffer to collect those protocol
  ///   requirements.
  ///
  /// \returns the slice of Scratch
  ArrayRef<ValueDecl*>
  canDeclProvideDefaultImplementationFor(ValueDecl* VD);

  /// Get decls that the given decl overrides, protocol requirements that
  ///   it serves as a default implementation of, and optionally protocol
  ///   requirements it satisfies in a conforming class
  ArrayRef<ValueDecl*>
  collectAllOverriddenDecls(ValueDecl *VD,
                            bool IncludeProtocolRequirements = true,
                            bool Transitive = false);

}

#endif
