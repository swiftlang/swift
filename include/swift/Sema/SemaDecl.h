//===--- SemaDecl.h - Swift Semantic Analysis for Declarations --*- C++ -*-===//
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
// parser to build the AST for declarations.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SEMA_DECL_H
#define SWIFT_SEMA_DECL_H

#include "swift/Sema/SemaBase.h"
#include "swift/AST/Identifier.h"

namespace llvm {
  template <typename K, typename V, typename KInfo>
  class ScopedHashTable;
}

namespace swift {
  class Expr;
  class Type;
  class VarDecl;
  class NamedDecl;
  class Scope;
  class DeclAttributes;
  
/// SemaDecl - Semantic analysis support for Swift declarations.
class SemaDecl : public SemaBase {
  typedef std::pair<unsigned, NamedDecl*> ScopeEntry;
  typedef llvm::ScopedHashTable<Identifier, ScopeEntry,
                                llvm::DenseMapInfo<Identifier> > ScopeHTType;
  ScopeHTType *const ScopeHT;
  Scope *CurScope;
  friend class Scope;
public:
  explicit SemaDecl(Sema &S);
  ~SemaDecl();
  
  //===--------------------------------------------------------------------===//
  // Name lookup.
  //===--------------------------------------------------------------------===//
  
  /// AddToScope - Register the specified decl as being in the current lexical
  /// scope.
  void AddToScope(NamedDecl *D);
  
  /// LookupName - Perform a lexical scope lookup for the specified name,
  /// returning the active decl if found or null if not.
  NamedDecl *LookupName(Identifier Name);
  
  //===--------------------------------------------------------------------===//
  // Declaration handling.
  //===--------------------------------------------------------------------===//
  
  VarDecl *ActOnVarDecl(llvm::SMLoc VarLoc, llvm::StringRef Name, Type *Ty,
                        Expr *Init, DeclAttributes &Attrs);
};
  
} // end namespace swift

#endif
