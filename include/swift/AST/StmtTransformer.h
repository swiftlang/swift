//===--- StmtTransformer.h - Class for transforming Stmts -------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file defines the StmtTransformer class.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_AST_STMTTRANSFORMER_H
#define SWIFT_AST_STMTTRANSFORMER_H

#include "swift/AST/ASTWalker.h"
#include "swift/AST/Stmt.h"
#include "swift/AST/Expr.h"

#include <forward_list>

namespace swift {
class StmtTransformer {
protected:
  class ClosureFinder : public ASTWalker {
    StmtTransformer &StmtTrans;
  public:
    ClosureFinder(StmtTransformer &CCR) : StmtTrans(CCR) { }
    virtual std::pair<bool, Stmt *> walkToStmtPre(Stmt *S) {
      if (isa<BraceStmt>(S)) {
        // To respect nesting, don't walk into brace statements.
        return { false, S };
      } else {
        return { true, S };
      }
    }
    virtual std::pair<bool, Expr *> walkToExprPre(Expr *E) {
      if (ClosureExpr *CE = dyn_cast<ClosureExpr>(E)) {
        BraceStmt *B = CE->getBody();
        if (B) {
          BraceStmt *NB = StmtTrans.transformBraceStmt(B, false);
          CE->setBody(NB, CE->hasSingleExpressionBody());
        }
      }
      return { true, E };
    }
  };
  ClosureFinder CF;
  
public:
  StmtTransformer() : CF(*this) {}
  virtual ~StmtTransformer() {}

  const ASTWalker &getClosureFinder() const {
    return CF;
  }

  virtual Stmt *transformStmt(Stmt *S);
  virtual BraceStmt *transformBraceStmt(BraceStmt *BS, bool TopLevel);
#define STMT(CLASS, PARENT) \
  virtual CLASS##Stmt *transform##CLASS##Stmt(CLASS##Stmt *S);
#include "swift/AST/StmtNodes.def"
};
};

#endif /* SWIFT_AST_STMTTRANSFORMER_H */
