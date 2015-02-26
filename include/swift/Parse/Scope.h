//===--- Scope.h - Scope Abstraction ----------------------------*- C++ -*-===//
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
// This file defines the Sema interface which implement hooks invoked by the 
// parser to build the AST.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SEMA_SCOPE_H
#define SWIFT_SEMA_SCOPE_H

#include "swift/AST/Identifier.h"
#include "swift/Basic/TreeScopedHashTable.h"
#include "llvm/ADT/SmallVector.h"

namespace swift {
  class ValueDecl;
  class Parser;
  class Scope;
  class SavedScope;

/// ScopeInfo - A single instance of this class is maintained by the Parser to
/// track the current scope.
class ScopeInfo {
  friend class Scope;
public:
  typedef std::pair<unsigned, ValueDecl*> ValueScopeEntry;
  
  typedef TreeScopedHashTable<Identifier, ValueScopeEntry> ScopedHTTy;
  typedef ScopedHTTy::ScopeTy ScopedHTScopeTy;
  typedef ScopedHTTy::DetachedScopeTy ScopedHTDetachedScopeTy;

private:
  ScopedHTTy HT;

  Scope *CurScope = nullptr;
  unsigned ResolvableDepth = 0;

public:
  ValueDecl *lookupValueName(Identifier Name);

  /// addToScope - Register the specified decl as being in the current lexical
  /// scope.
  void addToScope(ValueDecl *D, Parser &TheParser);

  bool isInactiveConfigBlock() const;

  SavedScope saveCurrentScope();
};

enum class ScopeKind {
  Extension,
  FunctionBody,
  Generics,
  EnumBody,
  StructBody,
  ClassBody,
  ProtocolBody,
  ConstructorBody,
  DestructorBody,

  Brace,
  TopLevel,
  ForVars,
  ForeachVars,
  CaseVars,
  IfVars,
  WhileVars,
  ActiveConfigBlock,

  ClosureParams,
};

/// \brief An opaque object that owns the scope frame.  The scope frame can be
/// re-entered later.
class SavedScope {
  friend class Scope;

  ScopeInfo::ScopedHTDetachedScopeTy HTDetachedScope;
  unsigned Depth;
  ScopeKind Kind;
  bool IsInactiveConfigBlock;

  SavedScope() = delete;
  SavedScope(const SavedScope &) = delete;
  void operator=(const SavedScope &) = delete;

public:
  SavedScope(SavedScope &&Other) = default;
  SavedScope &operator=(SavedScope &&) = default;
  ~SavedScope() = default;

  SavedScope(ScopeInfo::ScopedHTDetachedScopeTy &&HTDetachedScope,
             unsigned Depth, ScopeKind Kind, bool IsInactiveConfigBlock)
    : HTDetachedScope(std::move(HTDetachedScope)), Depth(Depth), Kind(Kind),
      IsInactiveConfigBlock(IsInactiveConfigBlock) {}
};

/// Scope - This class represents lexical scopes.  These objects are created
/// and destroyed as the parser is running, and name lookup happens relative
/// to them.
///
class Scope {
  friend class ScopeInfo;

  Scope(const Scope&) = delete;
  void operator=(const Scope&) = delete;

  ScopeInfo &SI;
  ScopeInfo::ScopedHTScopeTy HTScope;

  Scope *PrevScope;
  unsigned PrevResolvableDepth;
  unsigned Depth;
  ScopeKind Kind;
  bool IsInactiveConfigBlock;

  /// \brief Save this scope so that it can be re-entered later.  Transfers the
  /// ownership of the scope frame to returned object.
  SavedScope saveScope() {
    return SavedScope(HTScope.detach(), Depth, Kind, IsInactiveConfigBlock);
  }

  unsigned getDepth() const {
    return Depth;
  }

  bool isResolvable() const;

public:
  /// \brief Create a lexical scope of the specified kind.
  Scope(Parser *P, ScopeKind SC, bool IsInactiveConfigBlock = false);

  /// \brief Re-enter the specified scope, transferring the ownership of the
  /// scope frame to the new object.
  Scope(Parser *P, SavedScope &&SS);
  ~Scope() {
    // Active config blocks delegate to the enclosing scope, so there's nothing
    // to pop off.
    assert(SI.CurScope == this && "Scope mismatch");
    SI.CurScope = PrevScope;
    SI.ResolvableDepth = PrevResolvableDepth;
  }
};

inline ValueDecl *ScopeInfo::lookupValueName(Identifier Name) {
  // FIXME: this check can go away when SIL parser parses everything in
  // a toplevel scope.
  if (!CurScope)
    return nullptr;

  assert(CurScope && "no scope");
  // If we found nothing, or we found a decl at the top-level, return nothing.
  // We ignore results at the top-level because we may have overloading that
  // will be resolved properly by name binding.
  std::pair<unsigned, ValueDecl *> Res = HT.lookup(CurScope->HTScope, Name);
  if (Res.first < ResolvableDepth)
    return 0;
  return Res.second;
}

inline bool ScopeInfo::isInactiveConfigBlock() const {
  if (!CurScope)
    return false;
  return CurScope->IsInactiveConfigBlock;
}

inline SavedScope ScopeInfo::saveCurrentScope() {
  return CurScope->saveScope();
}

} // end namespace swift

#endif
