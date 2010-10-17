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
  template <typename PT1, typename PT2>
  class PointerUnion;
  template<class T>
  class NullablePtr;
}
namespace swift {
  class Sema;
  class Expr;
  class Type;
  class ValueDecl;
  class Identifier;
  
/// SemaExpr - Semantic analysis support for Swift expressions.
class SemaExpr : public SemaBase {
public:
  explicit SemaExpr(Sema &S) : SemaBase(S) {}

  
  // Utility Functions

  enum ConversionReason {
    CR_BinOpLHS,  // Left side of binary operator.
    CR_BinOpRHS,  // Right side of binary operator.
    CR_FuncApply, // Application of function argument.
    CR_VarInit,   // Var initializer
    CR_FuncBody   // Function body specification.
  };
  
  /// ConvertToType - Do semantic analysis of an expression in a context that
  /// expects a particular type.  This does conversion to that type if the types
  /// don't match and diagnoses cases where the conversion cannot be performed.
  /// The Reason specifies why this conversion is happening, for diagnostic
  /// purposes.
  ///
  /// This emits a diagnostic and returns null on error.
  Expr *ConvertToType(Expr *E, Type *Ty, bool IgnoreAnonDecls,
                      ConversionReason Reason);

  enum JuxtapositionGreediness {
    JG_NonGreedy,       // Is not a function
    JG_LocallyGreedy,   // Is a tightly binding function.
    JG_Greedy           // Is a loosely binding function.
  };

  /// getJuxtapositionGreediness - This returns an enum that specifies how
  /// tightly juxtaposition binds for a subexpression.  This allows functions
  /// to bind to their arguments tightly.  When this returns something other
  /// than NonGreedy, Sema is guaranteeing that juxtaposition will result in an
  /// ApplyExpr.
  JuxtapositionGreediness getJuxtapositionGreediness(Expr *E) const;
  
  // Action Implementations
  llvm::NullablePtr<Expr>
  ActOnNumericConstant(llvm::StringRef Text, llvm::SMLoc Loc);
  llvm::NullablePtr<Expr>
  ActOnIdentifierExpr(llvm::StringRef Text, llvm::SMLoc Loc);
  llvm::NullablePtr<Expr>
  ActOnScopedIdentifierExpr(llvm::StringRef ScopeName, llvm::SMLoc ScopeLoc,
                            llvm::SMLoc ColonColonLoc,
                            llvm::StringRef Name, llvm::SMLoc NameLoc);
  llvm::NullablePtr<Expr>
  ActOnUnresolvedMemberExpr(llvm::SMLoc ColonLoc, llvm::SMLoc NameLoc,
                            llvm::StringRef Name);
  
  llvm::NullablePtr<Expr>
  ActOnTupleExpr(llvm::SMLoc LPLoc, Expr *const *SubExprs,
                 const Identifier *SubExprNames,
                 unsigned NumSubExprs, llvm::SMLoc RPLoc);
  llvm::NullablePtr<Expr>
  ActOnBraceExpr(llvm::SMLoc LBLoc,
                 const llvm::PointerUnion<Expr*, ValueDecl*> *Elements,
                 unsigned NumElements, bool HasMissingSemi, llvm::SMLoc RBLoc);
  
  llvm::NullablePtr<Expr>
  ActOnDotIdentifier(Expr *E, llvm::SMLoc DotLoc, llvm::StringRef Identifier,
                     llvm::SMLoc IdentifierLoc);
  
  llvm::NullablePtr<Expr>
  ActOnArraySubscript(Expr *Base,llvm::SMLoc LLoc, Expr *Idx, llvm::SMLoc RLoc);
  
  llvm::PointerIntPair<Expr*, 1, bool> ActOnJuxtaposition(Expr *E1, Expr *E2);
  llvm::NullablePtr<Expr> ActOnSequence(Expr **Exprs, unsigned NumExprs); 
  
  llvm::NullablePtr<Expr> ActOnBinaryExpr(Expr *LHS, ValueDecl *OpFn,
                                          llvm::SMLoc OpLoc, Expr *RHS);
};
  
} // end namespace swift

#endif
