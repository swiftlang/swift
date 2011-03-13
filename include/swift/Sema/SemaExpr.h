//===--- SemaExpr.h - Swift Expression Semantic Analysis --------*- C++ -*-===//
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
// parser to build the AST for expressions.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SEMA_EXPR_H
#define SWIFT_SEMA_EXPR_H

#include "swift/Sema/SemaBase.h"
#include "llvm/ADT/PointerIntPair.h"

namespace llvm {
  template <typename PT1, typename PT2> class PointerUnion;
  template<class T> class NullablePtr;
  template<class T> class ArrayRef;
}
namespace swift {
  class Decl;
  class Sema;
  class Expr;
  class Type;
  class ValueDecl;
  class Identifier;
  
/// SemaExpr - Semantic analysis support for Swift expressions.
class SemaExpr : public SemaBase {
public:
  explicit SemaExpr(Sema &S) : SemaBase(S) {}

  
  // Action Implementations
  llvm::NullablePtr<Expr>
  ActOnNumericConstant(llvm::StringRef Text, llvm::SMLoc Loc);
  llvm::NullablePtr<Expr>
  ActOnDollarIdentExpr(llvm::StringRef Text, llvm::SMLoc Loc);
  llvm::NullablePtr<Expr>
  ActOnIdentifierExpr(Identifier Text, llvm::SMLoc Loc);
  llvm::NullablePtr<Expr>
  ActOnScopedIdentifierExpr(Identifier ScopeName, llvm::SMLoc ScopeLoc,
                            llvm::SMLoc ColonColonLoc,
                            Identifier Name, llvm::SMLoc NameLoc);
  llvm::NullablePtr<Expr>
  ActOnUnresolvedMemberExpr(llvm::SMLoc ColonLoc, llvm::SMLoc NameLoc,
                            Identifier Name);
  
  llvm::NullablePtr<Expr>
  ActOnTupleExpr(llvm::SMLoc LPLoc, Expr *const *SubExprs,
                 const Identifier *SubExprNames,
                 unsigned NumSubExprs, llvm::SMLoc RPLoc);
  llvm::NullablePtr<Expr>
  ActOnBraceExpr(llvm::SMLoc LBLoc,
                 llvm::ArrayRef<llvm::PointerUnion<Expr*, Decl*> > Elements,
                 bool HasMissingSemi, llvm::SMLoc RBLoc);
  
  llvm::NullablePtr<Expr>
  ActOnDotIdentifier(Expr *E, llvm::SMLoc DotLoc, llvm::StringRef Identifier,
                     llvm::SMLoc IdentifierLoc);
  
  llvm::NullablePtr<Expr>
  ActOnArraySubscript(Expr *Base,llvm::SMLoc LLoc, Expr *Idx, llvm::SMLoc RLoc);
  
  llvm::NullablePtr<Expr> ActOnSequence(llvm::ArrayRef<Expr *> Exprs); 
};
  
} // end namespace swift

#endif
