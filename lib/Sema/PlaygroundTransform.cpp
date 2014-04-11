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
    case StmtKind::While:
      return transformWhileStmt(llvm::cast<WhileStmt>(S));
    case StmtKind::DoWhile:
      return transformDoWhileStmt(llvm::cast<DoWhileStmt>(S));
    case StmtKind::For:
      return transformForStmt(llvm::cast<ForStmt>(S));
    case StmtKind::ForEach:
      return transformForEachStmt(llvm::cast<ForEachStmt>(S));
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

  WhileStmt *transformWhileStmt(WhileStmt *WS) {
    if (Stmt *B = WS->getBody()) {
      Stmt *NB = transformStmt(B);
      if (NB != B) {
        WS->setBody(NB);
      }
    }
      
    return WS;
  }

  DoWhileStmt *transformDoWhileStmt(DoWhileStmt *DWS) {
    if (Stmt *B = DWS->getBody()) {
      Stmt *NB = transformStmt(B);
      if (NB != B) {
        DWS->setBody(NB);
      }
    }
      
    return DWS;
  }

  ForStmt *transformForStmt(ForStmt *FS) {
    if (Stmt *B = FS->getBody()) {
      Stmt *NB = transformStmt(B);
      if (NB != B) {
        FS->setBody(NB);
      }
    }
      
    return FS;
  }

  ForEachStmt *transformForEachStmt(ForEachStmt *FES) {
    if (BraceStmt *B = FES->getBody()) {
      BraceStmt *NB = transformBraceStmt(B);
      if (NB != B) {
        FES->setBody(NB);
      }
    }
      
    return FES;
  }

  std::pair<DeclRefExpr *, VarDecl *> digForVariable(Expr *E) {
    if (DeclRefExpr *DRE = llvm::dyn_cast<DeclRefExpr>(E)) {
      return std::make_pair(DRE, llvm::dyn_cast<VarDecl>(DRE->getDecl()));
    } else if (LoadExpr *LE = llvm::dyn_cast<LoadExpr>(E)) {
      return digForVariable(LE->getSubExpr());
    } else if (ForceValueExpr *FVE = llvm::dyn_cast<ForceValueExpr>(E)) {
      return digForVariable(FVE->getSubExpr());
    } else {
      return std::make_pair(nullptr, nullptr);
    }
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
        else if (CallExpr *CE = llvm::dyn_cast<CallExpr>(E)) {
          if (CE->getType()->getCanonicalType() ==
              Context.TheEmptyTupleType) {
            if (DotSyntaxCallExpr *DSCE =
                llvm::dyn_cast<DotSyntaxCallExpr>(CE->getFn())) {
              Expr *TargetExpr = DSCE->getArg();
              DeclRefExpr *TargetDRE = nullptr;
              VarDecl *TargetVD = nullptr;

              std::tie(TargetDRE, TargetVD) = digForVariable(TargetExpr);

              if (TargetVD) {
                Expr *Log = logDeclRef(TargetDRE);
                if (Log) {
                  Modified = true;
                  Elements.insert(Elements.begin() + (EI + 1), Log);
                  ++EI;
                }
              }
            }
          } else {
            // do the same as for all other expressions
            std::pair<PatternBindingDecl *, VarDecl *> PV =
              buildPatternAndVariable(E);
            Expr *Log = buildLoggerCall(
              new (Context) DeclRefExpr(ConcreteDeclRef(PV.second),
                                        SourceLoc(),
                                        true, // implicit
                                        false, // uses direct property access
                                        E->getType()),
              E->getSourceRange());
            Modified = true;
            Elements[EI] = PV.first;
            Elements.insert(Elements.begin() + (EI + 1), PV.second);
            Elements.insert(Elements.begin() + (EI + 2), Log);
            EI += 2;
          }
        }
        else {
          if (E->getType()->getCanonicalType() !=
              Context.TheEmptyTupleType) {
            std::pair<PatternBindingDecl *, VarDecl *> PV =
              buildPatternAndVariable(E);
            Expr *Log = buildLoggerCall(
              new (Context) DeclRefExpr(ConcreteDeclRef(PV.second),
                                        SourceLoc(),
                                        true, // implicit
                                        false, // uses direct property access
                                        E->getType()),
              E->getSourceRange());
            Modified = true;
            Elements[EI] = PV.first;
            Elements.insert(Elements.begin() + (EI + 1), PV.second);
            Elements.insert(Elements.begin() + (EI + 2), Log);
            EI += 2;
          }
        }
      } else if (Stmt *S = Element.dyn_cast<Stmt*>()) {
        if (ReturnStmt *RS = llvm::dyn_cast<ReturnStmt>(S)) {
          if (RS->hasResult()) {
            std::pair<PatternBindingDecl *, VarDecl *> PV =
              buildPatternAndVariable(RS->getResult());
            DeclRefExpr *DRE =
              new (Context) DeclRefExpr(ConcreteDeclRef(PV.second),
                                        SourceLoc(),
                                        true, // implicit
                                        false, // uses direct property access
                                        RS->getResult()->getType());
            ReturnStmt *NRS = new (Context) ReturnStmt(SourceLoc(),
                                                       DRE,
                                                       true); // implicit
            Expr *Log = buildLoggerCall(
              new (Context) DeclRefExpr(ConcreteDeclRef(PV.second),
                                        SourceLoc(),
                                        true, // implicit
                                        false, // uses direct property access
                                        RS->getResult()->getType()),
              RS->getResult()->getSourceRange());
            Modified = true;
            Elements[EI] = PV.first;
            Elements.insert(Elements.begin() + (EI + 1), PV.second);
            Elements.insert(Elements.begin() + (EI + 2), Log);
            Elements.insert(Elements.begin() + (EI + 3), NRS);
            EI += 3;
          }
        } else {
          Stmt *NS = transformStmt(S);
          if (NS != S) {
            Modified = true;
            Elements[EI] = NS;
          }
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
        else if (FuncDecl *FD = llvm::dyn_cast<FuncDecl>(D)) {
          if (BraceStmt *B = FD->getBody()) {
            BraceStmt *NB = transformBraceStmt(B);
            if (NB != B) {
              FD->setBody(NB);
            }
          }
        }
      }
    }

    Modified = true;

    Elements.insert(Elements.begin(), buildScopeEntry(BS->getSourceRange()));
    Elements.insert(Elements.end(), buildScopeExit(BS->getSourceRange()));

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

  Expr *logDeclRef(DeclRefExpr *DRE) {
    VarDecl *VD = llvm::cast<VarDecl>(DRE->getDecl());
    return buildLoggerCall(
      new (Context) DeclRefExpr(ConcreteDeclRef(VD),
                                SourceLoc(),
                                true, // implicit
                                false, // uses direct property access
                                Type()),
      DRE->getSourceRange());
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
    Name->setImplicit(true);
      
    Expr *LoggerArgExprs[] = {
        E,
        Name
      };

    return buildLoggerCallWithArgs("playground_log", 
                                   MutableArrayRef<Expr *>(LoggerArgExprs),
                                   SR);
  }

  Expr *buildScopeEntry(SourceRange SR) {
    return buildScopeCall(SR, false);
  }

  Expr *buildScopeExit(SourceRange SR) {
    return buildScopeCall(SR, true);
  }

  Expr *buildScopeCall(SourceRange SR, bool IsExit) {
    const char *LoggerName = IsExit ? "playground_log_scope_exit"
                                    : "playground_log_scope_entry";

    return buildLoggerCallWithArgs(LoggerName,
                                   MutableArrayRef<Expr *>(),
                                   SR);
  }

  Expr *buildLoggerCallWithArgs(const char *LoggerName,
                                MutableArrayRef<Expr *> Args,
                                SourceRange SR) {
    TupleExpr *LoggerArgs = new (Context) TupleExpr(
      SourceLoc(),
      Context.AllocateCopy(Args),
      (Identifier*)nullptr,
      SourceLoc(),
      false, // hasTrailingClosure
      true, // implicit
      Type());

    UnresolvedDeclRefExpr *LoggerRef =
      new (Context) UnresolvedDeclRefExpr(
        Context.getIdentifier(LoggerName),
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

    ::snprintf(start_line_buf, buf_size, "%d",
               (StartLC.first < 5) ? 0 : (StartLC.first - 5));
    ::snprintf(start_column_buf, buf_size, "%d", StartLC.second - 1);
    ::snprintf(end_line_buf, buf_size, "%d",
               (EndLC.first < 5) ?  0 : (EndLC.first - 5));
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

static bool moduleHasFunction(Module *M, const char *N)
{
  ASTContext &Context(M->getASTContext());

  SmallVector<ValueDecl*, 1> Decls;

  M->lookupValue(Module::AccessPathTy(),
                 Context.getIdentifier(N),
                 NLKind::UnqualifiedLookup,
                 Decls);

  return (Decls.size() != 0);
}

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

        if (!moduleHasFunction(LoggerModule, "playground_log") ||
            !moduleHasFunction(LoggerModule, "playground_log_scope_entry") ||
            !moduleHasFunction(LoggerModule, "playground_log_scope_exit") ||
            !moduleHasFunction(CommModule, "DVTSendPlaygroundLogDataToHost")) {
          OK = false;
        }

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
