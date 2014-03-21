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
#include "swift/AST/Pattern.h"
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
  unsigned TmpNameIndex = 0;
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
          std::pair<PatternBindingDecl *, VarDecl *> PV =
            buildPatternAndVariable(AE->getSrc());
          DeclRefExpr *DRE =
            new (Context) DeclRefExpr(ConcreteDeclRef(PV.second),
                                      SourceLoc(),
                                      true, // implicit
                                      false, // uses direct property access
                                      AE->getSrc()->getType());
          AssignExpr *NAE = new (Context) AssignExpr(AE->getDest(),
                                                     SourceLoc(),
                                                     DRE,
                                                     true); // implicit
          AE->setImplicit(true);
          Expr *Log = buildLoggerCall(
            new (Context) DeclRefExpr(ConcreteDeclRef(PV.second),
                                      SourceLoc(),
                                      true, // implicit
                                      false, // uses direct property access
                                      AE->getSrc()->getType()),
            AE->getSrc()->getSourceRange());
          Modified = true;
          Elements[EI] = PV.first;
          Elements.insert(Elements.begin() + (EI + 1), PV.second);
          Elements.insert(Elements.begin() + (EI + 2), Log);
          Elements.insert(Elements.begin() + (EI + 3), NAE);
          EI += 3;
        }
        else {
          Expr *Log = logExpr(E);
          if (Log) {
            Modified = true;
            Elements[EI] = Log;
          }
        }
      } else if (Stmt *S = Element.dyn_cast<Stmt*>()) {
        Stmt *NS = transformStmt(S);
        if (NS != S) {
          Modified = true;
          Elements[EI] = NS;
        }
      } else if (Decl *D = Element.dyn_cast<Decl*>()) {
        if (VarDecl *VD = llvm::dyn_cast<VarDecl>(D)) {
          Expr *Log = logVarDecl(VD);
          if (Log) {
            Modified = true;
            Elements.insert(Elements.begin() + (EI + 1), Log);
            ++EI;
          }
        }
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
  // after or instead of the expression they're looking at.
  Expr *logExpr(Expr *E) {
    return buildLoggerCall(E, E->getSourceRange());
  }

  Expr *logVarDecl(VarDecl *VD) {
    return buildLoggerCall(
      new (Context) DeclRefExpr(ConcreteDeclRef(VD),
                                SourceLoc(),
                                true, // implicit
                                false, // uses direct property access
                                Type()),
      VD->getSourceRange());
  }

  std::pair<PatternBindingDecl*, VarDecl*>
    buildPatternAndVariable(Expr *InitExpr) {
    char NameBuf[11] = { 0 };
    snprintf(NameBuf, 11, "tmp%u", TmpNameIndex);
    TmpNameIndex++;

    VarDecl *VD = new (Context) VarDecl(false, // static
                                        true, // let
                                        SourceLoc(),
                                        Context.getIdentifier(NameBuf),
                                        InitExpr->getType(),
                                        TypeCheckDC);

    VD->setImplicit();

    NamedPattern *NP = new (Context) NamedPattern(VD,
                                                  true); // implicit

    PatternBindingDecl *PBD =
      new (Context) PatternBindingDecl(SourceLoc(),
                                       StaticSpellingKind::None,
                                       SourceLoc(),
                                       NP,
                                       InitExpr,
                                       false, // is conditional
                                       TypeCheckDC);

    PBD->setImplicit();

    return std::make_pair(PBD, VD);
  }

  Expr *buildLoggerCall(Expr *E, SourceRange SR) {
    Expr *Name = new (Context) StringLiteralExpr("", SourceRange());
    Expr *Header = new (Context) StringLiteralExpr("", SourceRange());
    Name->setImplicit(true);
    Header->setImplicit(true);

    Expr *LoggerArgExprs[] = {
        E,
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

    std::pair<unsigned, unsigned> StartLC =
      Context.SourceMgr.getLineAndColumn(SR.Start);

    std::pair<unsigned, unsigned> EndLC =
      Context.SourceMgr.getLineAndColumn(SR.End);

    const size_t buf_size = 8;

    char *start_line_buf = (char*)Context.Allocate(buf_size, 1);
    char *end_line_buf = (char*)Context.Allocate(buf_size, 1);
    char *start_column_buf = (char*)Context.Allocate(buf_size, 1);
    char *end_column_buf = (char*)Context.Allocate(buf_size, 1);

    ::snprintf(start_line_buf, buf_size, "%d", StartLC.first - 5);
    ::snprintf(start_column_buf, buf_size, "%d", StartLC.second - 1);
    ::snprintf(end_line_buf, buf_size, "%d", EndLC.first - 5);
    ::snprintf(end_column_buf, buf_size, "%d", EndLC.second - 1);

    Expr *StartLine = new (Context) IntegerLiteralExpr(start_line_buf, 
                                                       SourceLoc(), true);
    Expr *EndLine = new (Context) IntegerLiteralExpr(end_line_buf,
                                                     SourceLoc(), true);
    Expr *StartColumn = new (Context) IntegerLiteralExpr(start_column_buf, 
                                                         SourceLoc(), true);
    Expr *EndColumn = new (Context) IntegerLiteralExpr(end_column_buf, 
                                                       SourceLoc(), true);

    Expr *SendDataArgExprs[] = {
        LoggerCall,
        StartLine,
        EndLine,
        StartColumn,
        EndColumn
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
        std::make_pair(Context.getIdentifier("PlaygroundLogger"),
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
