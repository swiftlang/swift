//===--- PlaygroundTransform.cpp - Playground Transform -------------------===//
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
//  This file implements the playground transform for Swift.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/ASTContext.h"
#include "swift/AST/ASTNode.h"
#include "swift/AST/ASTWalker.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Expr.h"
#include "swift/AST/Module.h"
#include "swift/AST/NameLookup.h"
#include "swift/AST/Stmt.h"
#include "swift/AST/Types.h"
#include "swift/Sema/CodeCompletionTypeChecking.h"
#include "swift/Subsystems.h"

using namespace swift;

//===----------------------------------------------------------------------===//
// performPlaygroundTransform
//===----------------------------------------------------------------------===//

class Instrumenter {
private:
  ASTContext &Context;
  DeclContext *TypeCheckDC;
public:
  Instrumenter (ASTContext &C, DeclContext *DC) :
    Context(C), TypeCheckDC(DC) { }
    
  Stmt *transformStmt(Stmt *S) { 
    switch (S->getKind()) {
    default:
      return S;
    case StmtKind::Brace:
      return transformBraceStmt(llvm::cast<BraceStmt>(S));
    case StmtKind::If:
      return transformIfStmt(llvm::cast<IfStmt>(S));
    }
  }
    
  // transform*() return their input if it's unmodified,
  // or a modified copy of their input otherwise.
  IfStmt *transformIfStmt(IfStmt *IS) {
    if (Stmt *TS = IS->getThenStmt()) {
      Stmt *NTS = transformStmt(TS);
      if (NTS != TS) {
        IS->setThenStmt(NTS);
      }
    }

    if (Stmt *ES = IS->getElseStmt()) {
      Stmt *NES = transformStmt(ES);
      if (NES != ES) {
        IS->setElseStmt(NES);
      }
    }

    return IS;
  }

  BraceStmt *transformBraceStmt(BraceStmt *BS) {
    llvm::ArrayRef<ASTNode> OriginalElements = BS->getElements();
    typedef llvm::SmallVector<swift::ASTNode, 3> ElementVector;
    ElementVector Elements(OriginalElements.begin(),
                           OriginalElements.end());

    bool Modified = false;

    for (size_t EI = 0;
         EI != Elements.size();
         ++EI) {
      swift::ASTNode &Element = Elements[EI];
      if (Expr *E = Element.dyn_cast<Expr*>()) {
        if (AssignExpr *AE = llvm::dyn_cast<AssignExpr>(E)) {
          Expr *Log = logAssignExpr(AE);
          if (Log) {
            Modified = true;
            Elements.insert(Elements.begin() + (EI + 1), Log);
            ++EI;
          }
        }
      } else if (Stmt *S = Element.dyn_cast<Stmt*>()) {
        Stmt *NS = transformStmt(S);
        if (NS != S) {
          Modified = true;
          Elements[EI] = NS;
        }
      } else if (Decl *D = Element.dyn_cast<Decl*>()) {
        (void)D;
      }
    }

    if (Modified) {
      return swift::BraceStmt::create(Context, BS->getLBraceLoc(),
                                      Context.AllocateCopy(Elements),
                                      BS->getRBraceLoc());
    } else {
      return BS;
    }
  }

  // log*() functions return a newly-created log expression to be inserted
  // after the expression they're looking at.
  Expr *logAssignExpr(AssignExpr *AE) {
    Expr *Dest = AE->getDest();
    if (DeclRefExpr *DRE = llvm::dyn_cast<DeclRefExpr>(Dest)) {
      return buildLoggerCall(DRE->getDecl());
    }
    return nullptr;
  }

  Expr *buildLoggerCall(ValueDecl *VD) {
    Expr *Name = new (Context) StringLiteralExpr("", SourceRange());
    Expr *Header = new (Context) StringLiteralExpr("", SourceRange());
    Header->setImplicit(true);

    Expr *LoggerArgExprs[] = {
        new (Context) DeclRefExpr(ConcreteDeclRef(VD),
                                  SourceLoc(),
                                  true, // implicit
                                  false, // uses direct property access
                                  Type()),
        Name,
        Header
      };

    TupleExpr *LoggerArgs = new (Context) TupleExpr(
      SourceLoc(),
      Context.AllocateCopy(LoggerArgExprs),
      (Identifier*)nullptr,
      SourceLoc(),
      false, // hasTrailingClosure
      true, // implicit
      Type());

    UnresolvedDeclRefExpr *LoggerRef =
      new (Context) UnresolvedDeclRefExpr(
        Context.getIdentifier("playground_log"),
        DeclRefKind::Ordinary,
        SourceLoc());

    LoggerRef->setImplicit(true);

    Expr *LoggerCall = new (Context) CallExpr(LoggerRef, LoggerArgs, true,
                                              Type());

    Expr *SendDataArgExprs[] = {
        LoggerCall
      };

    TupleExpr *SendDataArgs = new (Context) TupleExpr(
      SourceLoc(),
      Context.AllocateCopy(SendDataArgExprs),
      (Identifier*)nullptr,
      SourceLoc(),
      false, // hasTrailingClosure
      true, // implicit
      Type());

    UnresolvedDeclRefExpr *SendDataRef = 
      new (Context) UnresolvedDeclRefExpr(
        Context.getIdentifier("DVTSendPlaygroundLogDataToHost"),
        DeclRefKind::Ordinary,
        SourceLoc());

    SendDataRef->setImplicit(true);

    Expr *SendDataCall = new (Context) CallExpr(SendDataRef, SendDataArgs, true,
                                                Type());

    if (!typeCheckCompletionContextExpr(Context, TypeCheckDC, SendDataCall)) {
      return nullptr;
    }

    return SendDataCall;
  }
};

void swift::performPlaygroundTransform(SourceFile &SF) {
  class ExpressionFinder : public ASTWalker {
    FuncDecl *WrappedDecl = nullptr;
    FuncDecl *ExpressionDecl = nullptr;
    StringRef WrappedName = "__lldb_wrapped_expr";
    StringRef ExpressionName = "__lldb_expr";
  public:
    virtual bool walkToDeclPre(Decl *D) {
      if (FuncDecl *FD = llvm::dyn_cast<FuncDecl>(D)) {
        StringRef FuncName = FD->getName().str();
        if (!FuncName.endswith(WrappedName)) {
          WrappedDecl = FD;
          return false;
        } else if (!FuncName.endswith(ExpressionName)) {
          ExpressionDecl = FD;
          return false;
        }
      }
      return true;
    }
    FuncDecl *getFunctionToTransform() {
      return WrappedDecl ? WrappedDecl : ExpressionDecl;
    }
  };

  ExpressionFinder EF;
  for (Decl* D : SF.Decls) {
    D->walk(EF);
  }

  if (FuncDecl *FuncToTransform = EF.getFunctionToTransform()) {
    if (BraceStmt *Body = FuncToTransform->getBody()) {
      ASTContext &Context(SF.getASTContext());

      Module *LoggerModule = Context.getModule(
        std::make_pair(Context.getIdentifier("PlaygroundLoggerLibrary"),
                       SourceLoc()));

      Module *CommModule = Context.getModule(
        std::make_pair(Context.getIdentifier("DVTPlaygroundCommunication"),
                       SourceLoc()));

      if (LoggerModule && CommModule)
      {
        SmallVector<ValueDecl*, 1> Decls;

        bool OK = true;

        LoggerModule->lookupValue(Module::AccessPathTy(),
                                  Context.getIdentifier("playground_log"),
                                  NLKind::UnqualifiedLookup,
                                  Decls);

        if (Decls.size() != 1)
          OK = false;

        Decls.clear();

        CommModule->lookupValue(
          Module::AccessPathTy(),
          Context.getIdentifier("DVTSendPlaygroundLogDataToHost"),
          NLKind::UnqualifiedLookup,
          Decls);

        if (Decls.size() != 1)
          OK = false;

        if (OK) {
          Instrumenter I(Context, FuncToTransform);
          BraceStmt *NewBody = I.transformBraceStmt(Body);
          if (NewBody != Body) {
            FuncToTransform->setBody(NewBody);
          }
        }
      }
    }
  }
}
