//===--- InstrumenterSupport.h - Instrumenter Support -----------*- C++ -*-===//
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
//  This file implements the supporting functions for writing instrumenters of
//  the Swift AST.
//
//===----------------------------------------------------------------------===//

#include "TypeChecker.h"

namespace swift {
namespace instrumenter_support {

template <class E> class Added {
private:
  E Contents;

public:
  Added() {}
  Added(E NewContents) { Contents = NewContents; }
  const Added<E> &operator=(const Added<E> &rhs) {
    Contents = rhs.Contents;
    return *this;
  }
  E &operator*() { return Contents; }
  E &operator->() { return Contents; }
};

class InstrumenterBase {

protected:
  InstrumenterBase() : CF(*this) {}
  virtual ~InstrumenterBase() = default;
  virtual void anchor();
  virtual BraceStmt *transformBraceStmt(BraceStmt *BS,
                                        bool TopLevel = false) = 0;

  class ClosureFinder : public ASTWalker {
  private:
    InstrumenterBase &I;

  public:
    ClosureFinder(InstrumenterBase &Inst) : I(Inst) {}
    std::pair<bool, Stmt *> walkToStmtPre(Stmt *S) override {
      if (isa<BraceStmt>(S)) {
        return {false, S}; // don't walk into brace statements; we
                           // need to respect nesting!
      } else {
        return {true, S};
      }
    }
    std::pair<bool, Expr *> walkToExprPre(Expr *E) override {
      if (ClosureExpr *CE = dyn_cast<ClosureExpr>(E)) {
        BraceStmt *B = CE->getBody();
        if (B) {
          BraceStmt *NB = I.transformBraceStmt(B);
          CE->setBody(NB, false);
          // just with the entry and exit logging this is going to
          // be more than a single expression!
        }
      }
      return {true, E};
    }
  };

  ClosureFinder CF;

  template <class T>
  bool doTypeCheck(ASTContext &Ctx, DeclContext *DC, Added<T *> &parsedExpr) {
    Expr *E = *parsedExpr;
    bool result = doTypeCheckImpl(Ctx, DC, E);
    parsedExpr = Added<T *>(dyn_cast<T>(E));
    return result;
  }
  
private:
  bool doTypeCheckImpl(ASTContext &Ctx, DeclContext *DC, Expr * &parsedExpr);
};
}
}
