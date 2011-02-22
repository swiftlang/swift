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
#include "llvm/ADT/NullablePtr.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/SMLoc.h"

namespace swift {
  class Expr;
  class Type;
  class TypeAliasDecl;
  class OneOfDecl;
  class VarDecl;
  class FuncDecl;
  class AnonDecl;
  class ValueDecl;
  class ElementRefDecl;
  class Scope;
  class DeclAttributes;
  
/// SemaDecl - Semantic analysis support for Swift declarations.
class SemaDecl : public SemaBase {
  void *const ScopeHT; // ScopedHashTable<Identifier, ScopeEntry>
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
  void AddToScope(ValueDecl *D);
  
  /// LookupValueName - Perform a lexical scope lookup for the specified name in
  /// a value context, returning the active decl if found or null if not.
  ValueDecl *LookupValueName(Identifier Name);
  

  /// AnonClosureArgs - These are the current active set of anonymous closure
  /// arguments, named $0 ... $9.  These are added to the list when first
  /// referenced in a context and cleared out when something uses them (binding
  /// them to a ClosureExpr).
  llvm::SmallVector<llvm::NullablePtr<AnonDecl>, 8> AnonClosureArgs;
  
  /// GetAnonDecl - Get the anondecl for the specified anonymous closure
  /// argument reference.  This occurs for use of $0 .. $9.  This returns null
  /// on an invalid name.
  AnonDecl *GetAnonDecl(llvm::StringRef Text, llvm::SMLoc RefLoc);
  
  //===--------------------------------------------------------------------===//
  // Declaration handling.
  //===--------------------------------------------------------------------===//
  
  /// ActOnTopLevelDecl - This is called after parsing a new top-level decl.
  void ActOnTopLevelDecl(ValueDecl *D);

  /// ActOnTopLevelDeclError - This is called after an error parsing a top-level
  /// decl.
  void ActOnTopLevelDeclError();

  TypeAliasDecl *ActOnTypeAlias(llvm::SMLoc TypeAliasLoc, Identifier Name,
                                Type *Ty);
  
  VarDecl *ActOnVarDecl(llvm::SMLoc VarLoc, Identifier Name, Type *Ty,
                        Expr *Init, DeclAttributes &Attrs);
  FuncDecl *ActOnFuncDecl(llvm::SMLoc FuncLoc, llvm::StringRef Name,
                          Type *Ty, DeclAttributes &Attrs);
  void CreateArgumentDeclsForFunc(FuncDecl *FD);
  FuncDecl *ActOnFuncBody(FuncDecl *FD, Expr *Body);

  OneOfDecl *ActOnOneOfDecl(llvm::SMLoc OneOfLoc, Identifier Name,
                          DeclAttributes &Attrs);
  
  struct OneOfElementInfo {
    llvm::SMLoc NameLoc;
    llvm::StringRef Name;
    Type *EltType;
  };
  
  void ActOnCompleteOneOfDecl(OneOfDecl *DD, const OneOfElementInfo *Elements,
                             unsigned NumElements);
  
  // Name processing.
  
  /// ActOnElementName - Assign a name to an element of D specified by Path.
  ElementRefDecl *ActOnElementName(Identifier Name, llvm::SMLoc NameLoc,
                                   VarDecl *D, const unsigned *Path,
                                   unsigned PathLen);
  bool CheckAccessPathArity(unsigned NumChildren, llvm::SMLoc LPLoc, VarDecl *D,
                            const unsigned *Path, unsigned PathLen);
};
  
} // end namespace swift

#endif
