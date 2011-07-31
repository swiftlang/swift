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

namespace llvm {
  template <typename PT1, typename PT2, typename PT3> class PointerUnion3;
}
namespace swift {
  class Decl;
  class Expr;
  class Stmt;
  class BraceStmt;
  class Type;
  class TypeAliasDecl;
  class VarDecl;
  class FuncDecl;
  class ValueDecl;
  class ElementRefDecl;
  class TranslationUnitDecl;
  class Scope;
  class DeclAttributes;
  class DeclVarName;
  
/// SemaDecl - Semantic analysis support for Swift declarations.
class SemaDecl : public SemaBase {
  void *const ValueScopeHT; // ScopedHashTable<Identifier, ValueScopeEntry>
  void *const TypeScopeHT;  // ScopedHashTable<Identifier, TypeScopeEntry>
  Scope *CurScope;
  friend class Scope;
  
  /// UnresolvedTypes - This keeps track of all of the unresolved types in the
  /// AST.
  void *const UnresolvedTypes; // DenseMap<Identifier, TypeAliasDecl *>
  
  SmallVector<TypeAliasDecl*, 8> UnresolvedTypeList;
public:

  explicit SemaDecl(Sema &S);
  ~SemaDecl();
  
  typedef llvm::PointerUnion3<Expr*, Stmt*, Decl*> ExprStmtOrDecl;

  /// handleEndOfTranslationUnit - This is invoked at the end of the translation
  /// unit.
  void handleEndOfTranslationUnit(TranslationUnitDecl *TU,
                                  SMLoc FileStart,
                                  ArrayRef<ExprStmtOrDecl> Items,
                                  SMLoc FileEnd);

  //===--------------------------------------------------------------------===//
  // Name lookup.
  //===--------------------------------------------------------------------===//
  
  /// AddToScope - Register the specified decl as being in the current lexical
  /// scope.
  void AddToScope(ValueDecl *D);
  
  /// LookupValueName - Perform a lexical scope lookup for the specified name in
  /// a value context, returning the active decl if found or null if not.
  ValueDecl *LookupValueName(Identifier Name);

  /// LookupTypeName - Perform a lexical scope lookup for the specified name in
  /// a type context, returning the decl if found or installing and returning a
  /// new Unresolved one if not.
  TypeAliasDecl *LookupTypeName(Identifier Name, SMLoc Loc);
  
  //===--------------------------------------------------------------------===//
  // Declaration handling.
  //===--------------------------------------------------------------------===//
  
  TypeAliasDecl *ActOnTypeAlias(SMLoc TypeAliasLoc, Identifier Name,
                                Type Ty);
  
  Decl *ActOnImportDecl(SMLoc ImportLoc,
                        ArrayRef<std::pair<Identifier,SMLoc>> Path,
                        DeclAttributes &Attrs);
  
  VarDecl *ActOnVarDecl(SMLoc VarLoc, DeclVarName &Name, Type Ty,
                        Expr *Init, DeclAttributes &Attrs);

  void ActOnStructDecl(SMLoc StructLoc, DeclAttributes &Attrs,
                       Identifier Name, Type Ty,
                       SmallVectorImpl<ExprStmtOrDecl> &Decls);
  
  // Name processing.
  
  /// ActOnElementName - Assign a name to an element of D specified by Path.
  ElementRefDecl *ActOnElementName(Identifier Name, SMLoc NameLoc,
                                   VarDecl *D, ArrayRef<unsigned> Path);
  bool CheckAccessPathArity(unsigned NumChildren, SMLoc LPLoc, VarDecl *D,
                            ArrayRef<unsigned> Path);
  
};
  
} // end namespace swift

#endif
