//===--- ASTScope.h - Swift AST Scope ---------------------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file defines the ASTScope class and related functionality, which
// describes the scopes that exist within a Swift AST.
//
//===----------------------------------------------------------------------===//
#ifndef SWIFT_AST_SCOPE_MAP_H
#define SWIFT_AST_SCOPE_MAP_H

#include "swift/AST/ASTNode.h"
#include "swift/Basic/LLVM.h"
#include "swift/Basic/SourceManager.h"
#include "llvm/ADT/PointerIntPair.h"
#include "llvm/ADT/SmallVector.h"

namespace swift {

class AbstractFunctionDecl;
class ASTContext;
class BraceStmt;
class Decl;
class Expr;
class GenericParamList;
class BraceStmt;
class IfStmt;
class IterableDeclContext;
class GenericParamList;
class GuardStmt;
class WhileStmt;
class DoCatchStmt;
class ForStmt;
class ForEachStmt;
class PatternBindingDecl;
class SourceFile;
class Stmt;
class StmtConditionElement;

/// Describes kind of scope that occurs within the AST.
enum class ASTScopeKind : uint8_t {
  /// A source file, which is the root of a scope.
  SourceFile,
  /// The body of a type or extension thereof.
  TypeOrExtensionBody,
  /// The generic parameters of a declaration.
  GenericParams,
  /// The parameters of a function/initializer/deinitializer.
  AbstractFunctionParams,
  /// The scope introduced by a particular clause in a pattern binding
  /// declaration.
  AfterPatternBinding,
  /// The scope introduced by a brace statement.
  BraceStmt,
  /// The scope introduced by an element in a brace statement.
  BraceStmtElement,
  /*
  BraceStmt,
  IfThenBranch,
  IfElseBranch,
  GuardElseBranch,
  GuardFollowingScope,
  WhileBody,
  DoCatchStmt,
  ForEachBody,
  FollowingStmtConditionElement,
   */
};

class ASTScope {
  /// The kind of scope this represents.
  ASTScopeKind kind;

  /// The parent scope of this particular scope along with a bit indicating
  /// whether the children of this node have already been expanded.
  mutable llvm::PointerIntPair<const ASTScope *, 1, bool> parentAndExpanded;

  /// Union describing the various kinds of AST nodes that can introduce
  /// scopes.
  union {
    /// For \c kind == ASTScopeKind::SourceFile.
    struct {
      // The actual source file.
      SourceFile *file;

      /// The next element that should be considered in the source file.
      ///
      /// This accomodates the expansion of source files.
      mutable unsigned nextElement;
    } sourceFile;

    /// An iterable declaration context, which covers nominal type declarations
    /// and extensions.
    ///
    /// For \c kind == ASTScopeKind::TypeOrExtensionBody.
    IterableDeclContext *iterableDeclContext;

    /// For \c kind == ASTScopeKind::GenericParams.
    struct {
      /// The generic parameters themselves.
      GenericParamList *params;

      /// The declaration that has generic parameters.
      Decl *decl;

      /// The index of the current parameter.
      unsigned index;
    } genericParams;

    /// An abstract function (init/func/deinit).
    ///
    /// For \c kind == ASTScopeKind::AbstractFunctionParams.
    struct {
      /// The function declaration.
      AbstractFunctionDecl *decl;

      /// The index into the function parameter lists.
      unsigned listIndex;

      /// The parameter index into the current function parameter list.
      unsigned paramIndex;
    } abstractFunctionParams;

    /// For \c kind == ASTScopeKind::AfterPatternBinding.
    struct {
      PatternBindingDecl *decl;
      unsigned entry;
    } patternBinding;

    /// For \c kind == ASTScopeKind::BraceStmt.
    BraceStmt *braceStmt;

    /// For \c kind == ASTScopeKind::BraceStmtElement.
    struct {
      BraceStmt *braceStmt;
      mutable unsigned element;
    } braceStmtElement;
};

  /// Child scopes, sorted by source range.
  mutable SmallVector<ASTScope *, 4> storedChildren;

  ASTScope(SourceFile *sourceFile, unsigned nextElement)
      : kind(ASTScopeKind::SourceFile), parentAndExpanded(nullptr, false) {
    this->sourceFile.file = sourceFile;
    this->sourceFile.nextElement = nextElement;
  }

  ASTScope(const ASTScope *parent, IterableDeclContext *idc)
      : kind(ASTScopeKind::TypeOrExtensionBody),
        parentAndExpanded(parent, false) {
    this->iterableDeclContext = idc;
  }

  ASTScope(const ASTScope *parent, GenericParamList *genericParams,
           Decl *decl, unsigned index)
      : kind(ASTScopeKind::GenericParams), parentAndExpanded(parent, false) {
    this->genericParams.params = genericParams;
    this->genericParams.decl = decl;
    this->genericParams.index = index;
  }

  ASTScope(const ASTScope *parent, AbstractFunctionDecl *abstractFunction,
           unsigned listIndex, unsigned paramIndex)
      : kind(ASTScopeKind::AbstractFunctionParams),
        parentAndExpanded(parent, false) {
    this->abstractFunctionParams.decl = abstractFunction;
    this->abstractFunctionParams.listIndex = listIndex;
    this->abstractFunctionParams.paramIndex = paramIndex;
  }

  ASTScope(const ASTScope *parent, PatternBindingDecl *decl, unsigned entry)
      : kind(ASTScopeKind::AfterPatternBinding),
        parentAndExpanded(parent, false) {
    this->patternBinding.decl = decl;
    this->patternBinding.entry = entry;
  }
  
  ASTScope(const ASTScope *parent, BraceStmt *braceStmt)
      : kind(ASTScopeKind::BraceStmt), parentAndExpanded(parent, false) {
    this->braceStmt = braceStmt;
  }

  ASTScope(const ASTScope *parent, BraceStmt *braceStmt, unsigned element)
      : kind(ASTScopeKind::BraceStmtElement), parentAndExpanded(parent, false) {
    this->braceStmtElement.braceStmt = braceStmt;
    this->braceStmtElement.element = element;
  }

  ~ASTScope();

  ASTScope(ASTScope &&) = delete;
  ASTScope &operator=(ASTScope &&) = delete;
  ASTScope(const ASTScope &) = delete;
  ASTScope &operator=(const ASTScope &) = delete;

  /// Expand the children of this AST scope so they can be queried.
  void expand() const;

  /// Determine whether the given scope has already been expanded.
  bool isExpanded() const;

  /// Create a new AST scope if one is needed for the given declaration.
  ///
  /// \returns the newly-created AST scope, or \c null if there is no scope
  /// introduced by this declaration.
  static ASTScope *createIfNeeded(const ASTScope *parent, Decl *decl);

  /// Create a new AST scope if one is needed for the given statement.
  ///
  /// \returns the newly-created AST scope, or \c null if there is no scope
  /// introduced by this statement.
  static ASTScope *createIfNeeded(const ASTScope *parent, Stmt *stmt);

  /// Create a new AST scope if one is needed for the given expression.
  ///
  /// \returns the newly-created AST scope, or \c null if there is no scope
  /// introduced by this expression.
  static ASTScope *createIfNeeded(const ASTScope *parent, Expr *Expr);

  /// Create a new AST scope if one is needed for the given AST node.
  ///
  /// \returns the newly-created AST scope, or \c null if there is no scope
  /// introduced by this AST node.
  static ASTScope *createIfNeeded(const ASTScope *parent, ASTNode node);

  /// Create a new AST scope that describes the continuation within the same
  /// parent scope.
  ///
  /// Statements, such as 'guard' and local declarations, introduce scopes
  /// that extend to the end of an enclosing brace-stmt. This
  /// operation finds the "continuation" in the nearest enclosing brace
  /// statement, which is the next statement following.
  static ASTScope *createContinuationScope(const ASTScope *parent);

  /// Retrieve the ASTContext in which this scope exists.
  ASTContext &getASTContext() const;

  /// Retrieve the source file in which this scope exists.
  SourceFile &getSourceFile() const;

public:
  /// Create the AST scope for a source file, which is the root of the scope
  /// tree.
  static ASTScope *createRoot(SourceFile *sourceFile);

  /// Determine the kind of AST scope we have.
  ASTScopeKind getKind() const { return kind; }

  /// Retrieve the parent scope that encloses this one.
  const ASTScope *getParent() const { return parentAndExpanded.getPointer(); }

  /// Retrieve the children of this AST scope, expanding if necessary.
  ArrayRef<ASTScope *> children() const {
    if (!isExpanded()) expand();
    return storedChildren;
  }

  /// Determine the source range covered by this scope.
  SourceRange getSourceRange() const;

  /// Expand the entire scope map.
  ///
  /// Normally, the scope map will be expanded only as needed by its queries,
  /// but complete expansion can be useful for debugging.
  void expandAll() const;

  /// Print out this scope for debugging/reporting purposes.
  void print(llvm::raw_ostream &out, unsigned level = 0,
             bool lastChild = false) const;

  LLVM_ATTRIBUTE_DEPRECATED(void dump() const LLVM_ATTRIBUTE_USED,
                            "only for use within the debugger");

  // Make vanilla new/delete illegal for Decls.
  void *operator new(size_t bytes) = delete;
  void operator delete(void *data) = delete;

  // Only allow allocation of scopes using the allocator of a particular source
  // file.
  void *operator new(size_t bytes, const ASTContext &ctx,
                     unsigned alignment = alignof(ASTScope));
  void *operator new(size_t Bytes, void *Mem) {
    assert(Mem);
    return Mem;
  }
};

}

#endif // SWIFT_BASIC_SOURCE_RANGE_MAP_H
