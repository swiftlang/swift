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
  template <typename T> class ArrayRef;
  template <typename PT1, typename PT2> class PointerUnion;
}
namespace swift {
  class Expr;
  class Type;
  class Decl;
  class TypeAliasDecl;
  class VarDecl;
  class FuncDecl;
  class MethDecl;
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
  
  llvm::SmallVector<TypeAliasDecl*, 8> UnresolvedTypeList;
public:

  explicit SemaDecl(Sema &S);
  ~SemaDecl();
  
  typedef llvm::PointerUnion<Expr*, Decl*> ExprOrDecl;

  /// handleEndOfTranslationUnit - This is invoked at the end of the translation
  /// unit.
  void handleEndOfTranslationUnit(TranslationUnitDecl *TU,
                                  SMLoc FileStart,
                                  llvm::ArrayRef<ExprOrDecl> Items,
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
                        llvm::ArrayRef<std::pair<Identifier,SMLoc> > Path,
                        DeclAttributes &Attrs);
  
  VarDecl *ActOnVarDecl(SMLoc VarLoc, DeclVarName &Name, Type Ty,
                        Expr *Init, DeclAttributes &Attrs);
  FuncDecl *ActOnFuncDecl(SMLoc FuncLoc, Identifier Name,
                          Type Ty, DeclAttributes &Attrs);
  MethDecl *ActOnMethDecl(SMLoc MethLoc, Type ReceiverType,
                          Identifier FuncName, Type Ty, DeclAttributes &Attrs);

  void CreateArgumentDeclsForFunc(ValueDecl *D);
  void ActOnFuncBody(ValueDecl *FD, Expr *Body);

  void ActOnStructDecl(SMLoc StructLoc, DeclAttributes &Attrs,
                       Identifier Name, Type Ty,
                       llvm::SmallVectorImpl<ExprOrDecl> &Decls);
  
  // Name processing.
  
  /// ActOnElementName - Assign a name to an element of D specified by Path.
  ElementRefDecl *ActOnElementName(Identifier Name, SMLoc NameLoc,
                                   VarDecl *D, llvm::ArrayRef<unsigned> Path);
  bool CheckAccessPathArity(unsigned NumChildren, SMLoc LPLoc, VarDecl *D,
                            llvm::ArrayRef<unsigned> Path);
  
};
  
} // end namespace swift

#endif
