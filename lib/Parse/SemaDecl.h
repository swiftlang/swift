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

#include "SemaBase.h"
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
  class Parser;
  
/// SemaDecl - Semantic analysis support for Swift declarations.
class SemaDecl : public SemaBase {
public:

  explicit SemaDecl(Sema &S) : SemaBase(S) {}
  ~SemaDecl() {}
  
  typedef llvm::PointerUnion3<Expr*, Stmt*, Decl*> ExprStmtOrDecl;

  /// handleEndOfTranslationUnit - This is invoked at the end of the translation
  /// unit.
  void handleEndOfTranslationUnit(TranslationUnitDecl *TU,
                                  SMLoc FileStart,
                                  ArrayRef<ExprStmtOrDecl> Items,
                                  SMLoc FileEnd, Parser &P);

};
  
} // end namespace swift

#endif
