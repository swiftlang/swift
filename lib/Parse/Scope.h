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
#include "llvm/ADT/ScopedHashTable.h"
#include "llvm/ADT/SmallVector.h"

namespace swift {
  class ValueDecl;
  class TypeAliasDecl;
  class Parser;
  class Scope;
  class Type;

/// ScopeInfo - A single instance of this class is maintained by the Parser to
/// track the current scope.
class ScopeInfo {
  friend class Scope;
public:
  typedef std::pair<unsigned, ValueDecl*> ValueScopeEntry;
  typedef std::pair<unsigned, TypeAliasDecl*> TypeScopeEntry;
  
  typedef llvm::ScopedHashTable<Identifier, ValueScopeEntry> ValueScopeHTTy;
  typedef llvm::ScopedHashTable<Identifier, TypeScopeEntry> TypeScopeHTTy;
private:
  Parser &TheParser;
  ValueScopeHTTy ValueScopeHT;
  TypeScopeHTTy TypeScopeHT;
  Scope *CurScope;
  
  /// UnresolvedTypes - This keeps track of all of the unresolved types in the
  /// AST.
  void *const UnresolvedTypes; // DenseMap<Identifier, TypeAliasDecl *>
  SmallVector<TypeAliasDecl*, 8> UnresolvedTypeList;
public:
  ScopeInfo(Parser &TheParser);
  ~ScopeInfo();
  
  const SmallVectorImpl<TypeAliasDecl*> &getUnresolvedTypeList() const { 
    return UnresolvedTypeList;
  }

  ValueDecl *lookupValueName(Identifier Name) {
    // If we found nothing, or we found a decl at the top-level, return nothing.
    // We ignore results at the top-level because we may have overloading that
    // will be resolved properly by name binding.
    std::pair<unsigned, ValueDecl*> Res = ValueScopeHT.lookup(Name);
    if (Res.first == 0) return 0;
    return Res.second;
  }
  
  /// lookupOrInsertTypeNameDecl - Perform a lexical scope lookup for the
  /// specified name in a type context, returning the decl if found or
  /// installing and returning a new Unresolved one if not.
  TypeAliasDecl *lookupOrInsertTypeNameDecl(Identifier Name, SMLoc Loc);

  /// lookupOrInsertTypeName - This is the same as lookupOrInsertTypeNameDecl,
  /// but returns the alias as a type.
  Type lookupOrInsertTypeName(Identifier Name, SMLoc Loc);

  /// lookupTypeNameAndLevel - Lookup the specified type name, returning the
  /// level it is at as well.
  TypeAliasDecl *lookupTypeNameAndLevel(Identifier Name, unsigned &Level) {
    std::pair<unsigned, TypeAliasDecl*> Entry = TypeScopeHT.lookup(Name);
    if (Entry.second == 0)
      return 0;
    Level = Entry.first;
    return Entry.second;
  }

  /// addToScope - Register the specified decl as being in the current lexical
  /// scope.
  void addToScope(ValueDecl *D);  
  
  /// addTypeAliasToScope - Add a type alias to the current scope, diagnosing
  /// redefinitions as required.
  TypeAliasDecl *addTypeAliasToScope(SMLoc TypeAliasLoc, Identifier Name,
                                     Type Ty);
};


/// Scope - This class represents lexical scopes.  These objects are created
/// and destroyed as the parser is running, and name lookup happens relative
/// to them.
///
class Scope {
  Scope(const Scope&) = delete;
  void operator=(const Scope&) = delete;
  ScopeInfo &SI;
  llvm::ScopedHashTableScope<Identifier, 
                             ScopeInfo::ValueScopeEntry> ValueHTScope;
  llvm::ScopedHashTableScope<Identifier, ScopeInfo::TypeScopeEntry> TypeHTScope;

  Scope *PrevScope;
  unsigned Depth;
public:
  explicit Scope(Parser *P);
  ~Scope() {
    assert(SI.CurScope == this && "Scope mismatch");
    SI.CurScope = PrevScope;
  }

  unsigned getDepth() const {
    return Depth;
  }
};
  
} // end namespace swift

#endif
