//===--- InstrumenterSupport.h - Instrumenter Support -----------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
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

using namespace swift;


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

  class ErrorGatherer : public DiagnosticConsumer {
  private:
    bool error = false;
    DiagnosticEngine &diags;

  public:
    ErrorGatherer(DiagnosticEngine &diags) : diags(diags) {
      diags.addConsumer(*this);
    }
    ~ErrorGatherer() override { diags.takeConsumers(); }
    void handleDiagnostic(SourceManager &SM, SourceLoc Loc,
                          DiagnosticKind Kind, StringRef Text,
                          const DiagnosticInfo &Info) override {
      if (Kind == swift::DiagnosticKind::Error) {
        error = true;
      }
      llvm::errs() << Text << "\n";
    }
    bool hadError() { return error; }
  };

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

  class ErrorFinder : public ASTWalker {
    bool error = false;

  public:
    ErrorFinder() {}
    std::pair<bool, Expr *> walkToExprPre(Expr *E) override {
      if (isa<ErrorExpr>(E) || !E->getType() || E->getType()->hasError()) {
        error = true;
        return {false, E};
      }
      return {true, E};
    }
    bool walkToDeclPre(Decl *D) override {
      if (ValueDecl *VD = dyn_cast<ValueDecl>(D)) {
        if (!VD->hasInterfaceType() || VD->getInterfaceType()->hasError()) {
          error = true;
          return false;
        }
      }
      return true;
    }
    bool hadError() { return error; }
  };

  template <class T>
  bool doTypeCheck(ASTContext &Ctx, DeclContext *DC, Added<T *> &parsedExpr) {
    DiagnosticEngine diags(Ctx.SourceMgr);
    ErrorGatherer errorGatherer(diags);

    TypeChecker TC(Ctx, diags);

    Expr *E = *parsedExpr;
    TC.typeCheckExpression(E, DC);
    parsedExpr = Added<T *>(dyn_cast<T>(E));

    if (*parsedExpr) {
      ErrorFinder errorFinder;
      parsedExpr->walk(errorFinder);
      if (!errorFinder.hadError() && !errorGatherer.hadError()) {
        return true;
      }
    }

    return false;
  }
};
}
}
