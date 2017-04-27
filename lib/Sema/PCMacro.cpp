//===--- PCMacro.cpp - PCMacro --------------------------------------------===//
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
//  This file implements the 'program counter simulation' for Swift.
//  Based off the PlaygroundTransform, PCMacro instruments code to call
//  functions at times that a debugger would show the program counter move.
//
//===----------------------------------------------------------------------===//

#include "InstrumenterSupport.h"

#include "swift/Subsystems.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Expr.h"
#include "swift/AST/ParameterList.h"
#include "swift/AST/Module.h"
#include "swift/AST/Pattern.h"
#include "swift/AST/Stmt.h"

using namespace swift;
using namespace swift::instrumenter_support;

//===----------------------------------------------------------------------===//
// performPCMacro
//===----------------------------------------------------------------------===//

namespace {

class Instrumenter : InstrumenterBase {
private:
  ASTContext &Context;
  DeclContext *TypeCheckDC;
  unsigned &TmpNameIndex;

public:
  Instrumenter(ASTContext &C, DeclContext *DC, unsigned &TmpNameIndex)
      : Context(C), TypeCheckDC(DC), TmpNameIndex(TmpNameIndex) {}

  Stmt *transformStmt(Stmt *S) {
    switch (S->getKind()) {
    default:
      return S;
    case StmtKind::Brace:
      return transformBraceStmt(cast<BraceStmt>(S));
    case StmtKind::If:
      return transformIfStmt(cast<IfStmt>(S));
    case StmtKind::Guard:
      return transformGuardStmt(cast<GuardStmt>(S));
    case StmtKind::While: {
      return transformWhileStmt(cast<WhileStmt>(S));
    }
    case StmtKind::RepeatWhile: {
      return transformRepeatWhileStmt(cast<RepeatWhileStmt>(S));
    }
    case StmtKind::For: {
      return transformForStmt(cast<ForStmt>(S));
    }
    case StmtKind::ForEach: {
      return transformForEachStmt(cast<ForEachStmt>(S));
    }
    case StmtKind::Switch: {
      return transformSwitchStmt(cast<SwitchStmt>(S));
    }
    case StmtKind::Do:
      return transformDoStmt(llvm::cast<DoStmt>(S));
    case StmtKind::DoCatch:
      return transformDoCatchStmt(cast<DoCatchStmt>(S));
    }
  }

  void transformStmtCondition(StmtCondition SC, SourceLoc StartLoc) {
    // Right now, only handle if statements with one condition
    if (SC.size() == 1) {
      StmtConditionElement *SCE = SC.begin();
      switch (SCE->getKind()) {
      case StmtConditionElement::ConditionKind::CK_Boolean: {
        Expr *E = SCE->getBoolean();
        SourceLoc EndLoc = E->getEndLoc();
        if (StartLoc.isValid() && EndLoc.isValid()) {
          Expr *NE = buildInlineLoggerCall({StartLoc, EndLoc}, E);
          SCE->setBoolean(NE);
        }
      } break;
      case StmtConditionElement::ConditionKind::CK_PatternBinding: {
        Expr *E = SCE->getInitializer();
        SourceLoc EndLoc = E->getEndLoc();
        if (StartLoc.isValid() && EndLoc.isValid()) {
          Expr *NE = buildInlineLoggerCall({StartLoc, EndLoc}, E);
          SCE->setInitializer(NE);
        }
      } break;
      default:;
      }
    }
  }

  // transform*() return their input if it's unmodified,
  // or a modified copy of their input otherwise.
  IfStmt *transformIfStmt(IfStmt *IS) {
    StmtCondition SC = IS->getCond();
    transformStmtCondition(SC, IS->getStartLoc());
    IS->setCond(SC); // FIXME: is setting required?..

    if (Stmt *TS = IS->getThenStmt()) {
      Stmt *NTS = transformStmt(TS);
      if (NTS != TS) {
        IS->setThenStmt(NTS);
      }
    }

    if (Stmt *ES = IS->getElseStmt()) {
      SourceLoc ElseLoc = IS->getElseLoc(); // FIXME: got to pass this back into
                                            // transformStmt if the else stmt is
                                            // an IfStmt. Then we prepend this
                                            // range to the ifstmt highlight.
                                            // See the elseif.swift test.
      Stmt *NES = transformStmt(ES);
      if (ElseLoc.isValid()) {
        if (BraceStmt *BS = dyn_cast<BraceStmt>(NES)) {
          BraceStmt *NBS = prependLoggerCall(BS, ElseLoc);
          if (NBS != ES) {
            IS->setElseStmt(NBS);
          }
        } else if (IfStmt *EIS = dyn_cast<IfStmt>(NES)) {
          // FIXME: here we should use the old range to show a better highlight
          // (including the previous else)
          if (EIS != ES) {
            IS->setElseStmt(EIS);
          }
        } else {
          llvm_unreachable(
              "IfStmt else stmts must be either IfStmt or BraceStmt");
        }
      } else {
        if (NES != ES) {
          IS->setElseStmt(NES);
        }
      }
    }

    return IS;
  }

  GuardStmt *transformGuardStmt(GuardStmt *GS) {
    StmtCondition SC = GS->getCond();
    transformStmtCondition(SC, GS->getStartLoc());
    GS->setCond(SC);

    if (Stmt *BS = GS->getBody())
      GS->setBody(transformStmt(BS));
    return GS;
  }

  WhileStmt *transformWhileStmt(WhileStmt *WS) {
    StmtCondition SC = WS->getCond();
    transformStmtCondition(SC, WS->getStartLoc());
    WS->setCond(SC);

    if (Stmt *B = WS->getBody()) {
      Stmt *NB = transformStmt(B);
      if (NB != B) {
        WS->setBody(NB);
      }
    }

    return WS;
  }

  RepeatWhileStmt *transformRepeatWhileStmt(RepeatWhileStmt *RWS) {
    if (Stmt *B = RWS->getBody()) {
      Stmt *NB = transformStmt(B);
      if (NB != B) {
        RWS->setBody(NB);
      }
    }

    return RWS;
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

      // point at the for stmt, to look nice
      SourceLoc StartLoc = FES->getStartLoc();
      SourceLoc EndLoc = FES->getIterator()->getEndLoc();
      // FIXME: get the 'end' of the for stmt
      // if (FD->getBodyResultTypeLoc().hasLocation()) {
      //   EndLoc = FD->getBodyResultTypeLoc().getSourceRange().End;
      // } else {
      //   EndLoc = FD->getParameterLists().back()->getSourceRange().End;
      // }

      if (StartLoc.isValid() && EndLoc.isValid()) {
        BraceStmt *NNB = prependLoggerCall(NB, {StartLoc, EndLoc});
        if (NNB != B) {
          FES->setBody(NNB);
        }
      } else {
        if (NB != B) {
          FES->setBody(NB);
        }
      }
    }

    return FES;
  }

  SwitchStmt *transformSwitchStmt(SwitchStmt *SS) {
    // Get the subject range (and switch keyword) and begin by pointing at that
    // range. Then stop pointing at it (for now, until we can replace the
    // switch subject expr).
    // Insert both these stmts before the SwitchStmt.
    SourceLoc StartLoc = SS->getStartLoc();
    SourceLoc EndLoc = SS->getSubjectExpr()->getEndLoc();

    for (CaseStmt *CS : SS->getCases()) {
      if (Stmt *S = CS->getBody()) {
        if (auto *B = dyn_cast<BraceStmt>(S)) {
          BraceStmt *NB = transformBraceStmt(B);

          // Lets insert a before and after log pointing at the case statement
          // at the start of the body (just like in for loops.
          BraceStmt *NNB = nullptr;
          SourceRange CaseRange = CS->getLabelItemsRange();
          if (CaseRange.isValid()) {
            NNB = prependLoggerCall(NB, CaseRange);
          } else {
            NNB = NB;
          }

          // Now we prepend the switch log, so that it looks like switch came
          // before case
          BraceStmt *NNNB = nullptr;
          if (StartLoc.isValid() && EndLoc.isValid()) {
            NNNB = prependLoggerCall(NNB, {StartLoc, EndLoc});
          } else {
            NNNB = NNB;
          }

          if (NNNB != B) {
            CS->setBody(NNNB);
          }
        }
      }
    }

    return SS;
  }

  DoStmt *transformDoStmt(DoStmt *DS) {
    if (BraceStmt *B = dyn_cast_or_null<BraceStmt>(DS->getBody())) {
      BraceStmt *NB = transformBraceStmt(B);
      if (NB != B) {
        DS->setBody(NB);
      }
    }
    return DS;
  }

  DoCatchStmt *transformDoCatchStmt(DoCatchStmt *DCS) {
    if (BraceStmt *B = dyn_cast_or_null<BraceStmt>(DCS->getBody())) {
      BraceStmt *NB = transformBraceStmt(B);
      if (NB != B) {
        DCS->setBody(NB);
      }
    }
    for (CatchStmt *C : DCS->getCatches()) {
      if (BraceStmt *CB = dyn_cast_or_null<BraceStmt>(C->getBody())) {
        BraceStmt *NCB = transformBraceStmt(CB);
        if (NCB != CB) {
          C->setBody(NCB);
        }
      }
    }
    return DCS;
  }

  Decl *transformDecl(Decl *D) {
    if (D->isImplicit())
      return D;
    if (FuncDecl *FD = dyn_cast<FuncDecl>(D)) {
      if (BraceStmt *B = FD->getBody()) {
        BraceStmt *NB = transformBraceStmt(B);
        // Since it would look strange going straight to the first line in a
        // function body, we throw in a before/after pointing at the function
        // decl at the start of the transformed body
        SourceLoc StartLoc = FD->getStartLoc();
        SourceLoc EndLoc = SourceLoc();
        if (FD->getBodyResultTypeLoc().hasLocation()) {
          EndLoc = FD->getBodyResultTypeLoc().getSourceRange().End;
        } else {
          EndLoc = FD->getParameterLists().back()->getSourceRange().End;
        }

        if (EndLoc.isValid()) {
          BraceStmt *NNB = prependLoggerCall(NB, {StartLoc, EndLoc});
          if (NNB != B) {
            FD->setBody(NNB);
          }
        } else {
          if (NB != B) {
            FD->setBody(NB);
          }
        }
      }
    } else if (auto *NTD = dyn_cast<NominalTypeDecl>(D)) {
      for (Decl *Member : NTD->getMembers()) {
        transformDecl(Member);
      }
    }

    return D;
  }

  BraceStmt *transformBraceStmt(BraceStmt *BS, bool TopLevel = false) override {
    ArrayRef<ASTNode> OriginalElements = BS->getElements();
    SmallVector<swift::ASTNode, 3> Elements(OriginalElements.begin(),
                                            OriginalElements.end());

    for (size_t EI = 0; EI != Elements.size(); ++EI) {
      swift::ASTNode &Element = Elements[EI];
      if (Expr *E = Element.dyn_cast<Expr *>()) {
        E->walk(CF);

        Added<Stmt *> LogBefore = buildLoggerCall(E->getSourceRange(), true);
        Added<Stmt *> LogAfter = buildLoggerCall(E->getSourceRange(), false);

        if (*LogBefore && *LogAfter) {
          Elements[EI] = *LogBefore;
          Elements.insert(Elements.begin() + (EI + 1), E);
          Elements.insert(Elements.begin() + (EI + 2), *LogAfter);
          EI += 2;
        }
      } else if (Stmt *S = Element.dyn_cast<Stmt *>()) {
        S->walk(CF);
        if (ReturnStmt *RS = dyn_cast<ReturnStmt>(S)) {
          if (RS->hasResult()) {
            std::pair<PatternBindingDecl *, VarDecl *> PV =
                buildPatternAndVariable(RS->getResult());
            SourceLoc ResultStartLoc = RS->getResult()->getStartLoc();
            DeclRefExpr *DRE = new (Context) DeclRefExpr(
                ConcreteDeclRef(PV.second),
                ResultStartLoc.isValid() ? DeclNameLoc(ResultStartLoc)
                                         : DeclNameLoc(),
                true, // implicit
                AccessSemantics::Ordinary, RS->getResult()->getType());
            ReturnStmt *NRS = new (Context) ReturnStmt(SourceLoc(), DRE,
                                                       true); // implicit
            Added<Stmt *> LogBefore =
                buildLoggerCall(RS->getSourceRange(), true);
            Added<Stmt *> LogAfter =
                buildLoggerCall(RS->getSourceRange(), false);
            if (*LogBefore && *LogAfter) {
              Elements[EI] = *LogBefore;
              Elements.insert(Elements.begin() + (EI + 1), PV.first);
              Elements.insert(Elements.begin() + (EI + 2), PV.second);
              Elements.insert(Elements.begin() + (EI + 3), *LogAfter);
              Elements.insert(Elements.begin() + (EI + 4), NRS);
              EI += 4;
            }
          } else {
            Added<Stmt *> LogBefore =
                buildLoggerCall(RS->getSourceRange(), true);
            Added<Stmt *> LogAfter =
                buildLoggerCall(RS->getSourceRange(), false);
            if (*LogBefore && *LogAfter) {
              Elements[EI] = *LogBefore;
              Elements.insert(Elements.begin() + (EI + 1), *LogAfter);
              Elements.insert(Elements.begin() + (EI + 2), RS);
              EI += 2;
            }
          }
        } else if (ContinueStmt *CS = dyn_cast<ContinueStmt>(S)) {
          Added<Stmt *> LogBefore = buildLoggerCall(CS->getSourceRange(), true);
          Added<Stmt *> LogAfter = buildLoggerCall(CS->getSourceRange(), false);
          if (*LogBefore && *LogAfter) {
            Elements[EI] = *LogBefore;
            Elements.insert(Elements.begin() + (EI + 1), *LogAfter);
            Elements.insert(Elements.begin() + (EI + 2), CS);
            EI += 2;
          }

        } else if (BreakStmt *BS = dyn_cast<BreakStmt>(S)) {
          Added<Stmt *> LogBefore = buildLoggerCall(BS->getSourceRange(), true);
          Added<Stmt *> LogAfter = buildLoggerCall(BS->getSourceRange(), false);
          if (*LogBefore && *LogAfter) {
            Elements[EI] = *LogBefore;
            Elements.insert(Elements.begin() + (EI + 1), *LogAfter);
            Elements.insert(Elements.begin() + (EI + 2), BS);
            EI += 2;
          }

        } else if (FallthroughStmt *FS = dyn_cast<FallthroughStmt>(S)) {
          Added<Stmt *> LogBefore = buildLoggerCall(FS->getSourceRange(), true);
          Added<Stmt *> LogAfter = buildLoggerCall(FS->getSourceRange(), false);
          if (*LogBefore && *LogAfter) {
            Elements[EI] = *LogBefore;
            Elements.insert(Elements.begin() + (EI + 1), *LogAfter);
            Elements.insert(Elements.begin() + (EI + 2), FS);
            EI += 2;
          }

        } else {
          Stmt *NS = transformStmt(S);
          if (NS != S) {
            Elements[EI] = NS;
          }
        }
      } else if (Decl *D = Element.dyn_cast<Decl *>()) {
        D->walk(CF);
        if (auto *PBD = dyn_cast<PatternBindingDecl>(D)) {
          // FIXME: Should iterate all var decls
          if (VarDecl *VD = PBD->getSingleVar()) {
            if (VD->getParentInitializer()) {

              SourceRange SR = PBD->getSourceRange();
              if (!SR.isValid()) {
                SR = PBD->getOrigInitRange(0);
              }

              Added<Stmt *> LogBefore = buildLoggerCall(SR, true);
              Added<Stmt *> LogAfter = buildLoggerCall(SR, false);

              if (*LogBefore && *LogAfter) {
                Elements[EI] = *LogBefore;
                Elements.insert(Elements.begin() + (EI + 1), D);
                Elements.insert(Elements.begin() + (EI + 2), *LogAfter);
                EI += 2;
              }
            }
          }
        } else {
          transformDecl(D);
        }
      }
    }

    return swift::BraceStmt::create(Context, BS->getLBraceLoc(), Elements,
                                    BS->getRBraceLoc());
  }

  std::pair<PatternBindingDecl *, VarDecl *>
  buildPatternAndVariable(Expr *InitExpr) {
    // This is 16 because "pctmp" is 5 chars, %u is at most 10 digits long plus
    // a null terminator.
    char NameBuf[16] = {0};
    snprintf(NameBuf, sizeof(NameBuf), "pctmp%u", TmpNameIndex);
    TmpNameIndex++;

    Expr *MaybeLoadInitExpr = nullptr;

    if (LValueType *LVT = InitExpr->getType()->getAs<LValueType>()) {
      MaybeLoadInitExpr =
          new (Context) LoadExpr(InitExpr, LVT->getObjectType());
    } else {
      MaybeLoadInitExpr = InitExpr;
    }

    VarDecl *VD =
        new (Context) VarDecl(/*IsStatic*/false, /*IsLet*/true,
                              /*IsCaptureList*/false, SourceLoc(),
                              Context.getIdentifier(NameBuf),
                              MaybeLoadInitExpr->getType(), TypeCheckDC);

    VD->setImplicit();

    NamedPattern *NP = new (Context) NamedPattern(VD, /*implicit*/ true);
    PatternBindingDecl *PBD = PatternBindingDecl::create(
        Context, SourceLoc(), StaticSpellingKind::None, SourceLoc(), NP,
        MaybeLoadInitExpr, TypeCheckDC);
    PBD->setImplicit();

    return std::make_pair(PBD, VD);
  }

  Added<Stmt *> buildLoggerCall(SourceRange SR, bool isBefore) {
    if (isBefore) {
      return buildLoggerCallWithArgs("__builtin_pc_before", SR);
    } else {
      return buildLoggerCallWithArgs("__builtin_pc_after", SR);
    }
  }

  // Puts a pair of before/after calls at the start of the body, pointing at
  // that range.
  BraceStmt *prependLoggerCall(BraceStmt *BS, SourceRange SR) {
    Added<Stmt *> Before = buildLoggerCall(SR, true);
    Added<Stmt *> After = buildLoggerCall(SR, false);

    ArrayRef<ASTNode> OriginalElements = BS->getElements();
    SmallVector<swift::ASTNode, 3> Elements(OriginalElements.begin(),
                                            OriginalElements.end());

    Elements.insert(Elements.begin(), {*Before, *After});
    return swift::BraceStmt::create(Context, BS->getLBraceLoc(), Elements,
                                    BS->getRBraceLoc());
  }

  // Takes an existing Expr and builds an expr that calls before, stores the
  // return value of the expr, calls after, then returns that return value.
  Expr *buildInlineLoggerCall(SourceRange SR, Expr *E) {

    if (!SR.isValid()) {
      return E;
    }

    std::pair<unsigned, unsigned> StartLC =
        Context.SourceMgr.getLineAndColumn(SR.Start);

    std::pair<unsigned, unsigned> EndLC = Context.SourceMgr.getLineAndColumn(
        Lexer::getLocForEndOfToken(Context.SourceMgr, SR.End));

    const size_t buf_size = 8;

    char *start_line_buf = (char *)Context.Allocate(buf_size, 1);
    char *end_line_buf = (char *)Context.Allocate(buf_size, 1);
    char *start_column_buf = (char *)Context.Allocate(buf_size, 1);
    char *end_column_buf = (char *)Context.Allocate(buf_size, 1);

    ::snprintf(start_line_buf, buf_size, "%u", StartLC.first);
    ::snprintf(start_column_buf, buf_size, "%u", StartLC.second);
    ::snprintf(end_line_buf, buf_size, "%u", EndLC.first);
    ::snprintf(end_column_buf, buf_size, "%u", EndLC.second);

    Expr *StartLine =
        new (Context) IntegerLiteralExpr(start_line_buf, SR.End, true);
    Expr *EndLine =
        new (Context) IntegerLiteralExpr(end_line_buf, SR.End, true);
    Expr *StartColumn =
        new (Context) IntegerLiteralExpr(start_column_buf, SR.End, true);
    Expr *EndColumn =
        new (Context) IntegerLiteralExpr(end_column_buf, SR.End, true);

    llvm::SmallVector<Expr *, 5> ArgsWithSourceRange{};

    ArgsWithSourceRange.append({StartLine, EndLine, StartColumn, EndColumn});

    UnresolvedDeclRefExpr *BeforeLoggerRef = new (Context)
        UnresolvedDeclRefExpr(Context.getIdentifier("__builtin_pc_before"),
                              DeclRefKind::Ordinary, DeclNameLoc(SR.End));
    BeforeLoggerRef->setImplicit(true);
    SmallVector<Identifier, 4> ArgLabels(ArgsWithSourceRange.size(),
                                         Identifier());
    ApplyExpr *BeforeLoggerCall = CallExpr::createImplicit(
        Context, BeforeLoggerRef, ArgsWithSourceRange, ArgLabels);
    Added<ApplyExpr *> AddedBeforeLogger(BeforeLoggerCall);
    if (!doTypeCheck(Context, TypeCheckDC, AddedBeforeLogger)) {
      // typically due to 'use of unresolved identifier '__builtin_pc_before''
      return E; // return E, it will be used in recovering from TC failure
    }

    UnresolvedDeclRefExpr *AfterLoggerRef = new (Context)
        UnresolvedDeclRefExpr(Context.getIdentifier("__builtin_pc_after"),
                              DeclRefKind::Ordinary, DeclNameLoc(SR.End));
    AfterLoggerRef->setImplicit(true);
    ApplyExpr *AfterLoggerCall = CallExpr::createImplicit(
        Context, AfterLoggerRef, ArgsWithSourceRange, ArgLabels);
    Added<ApplyExpr *> AddedAfterLogger(AfterLoggerCall);
    if (!doTypeCheck(Context, TypeCheckDC, AddedAfterLogger)) {
      // typically due to 'use of unresolved identifier '__builtin_pc_after''
      return E; // return E, it will be used in recovering from TC failure
    }

    llvm::SmallVector<Expr *, 3> TupleArgs{};
    TupleArgs.append({*AddedBeforeLogger, E, *AddedAfterLogger});
    SmallVector<Identifier, 3> ThreeArgLabels(TupleArgs.size(), Identifier());
    TupleExpr *Tup =
        TupleExpr::createImplicit(Context, TupleArgs, ThreeArgLabels);
    SmallVector<TupleTypeElt, 3> TupleTypes{};
    TupleTypes.append({TupleTypeElt(TupleType::getEmpty(Context)),
                       TupleTypeElt(E->getType()),
                       TupleTypeElt(TupleType::getEmpty(Context))});
    Tup->setType(TupleType::get(TupleTypes, Context));
    TupleElementExpr *GetOne = new (Context)
        TupleElementExpr(Tup, SourceLoc(), 1, SourceLoc(), E->getType());

    GetOne->setImplicit(true);

    Added<Expr *> AddedGet(GetOne);

    return *AddedGet;
  }

  Added<Stmt *> buildLoggerCallWithArgs(const char *LoggerName,
                                        SourceRange SR) {
    if (!SR.isValid()) {
      return nullptr;
    }

    std::pair<unsigned, unsigned> StartLC =
        Context.SourceMgr.getLineAndColumn(SR.Start);

    std::pair<unsigned, unsigned> EndLC = Context.SourceMgr.getLineAndColumn(
        Lexer::getLocForEndOfToken(Context.SourceMgr, SR.End));

    const size_t buf_size = 8;

    char *start_line_buf = (char *)Context.Allocate(buf_size, 1);
    char *end_line_buf = (char *)Context.Allocate(buf_size, 1);
    char *start_column_buf = (char *)Context.Allocate(buf_size, 1);
    char *end_column_buf = (char *)Context.Allocate(buf_size, 1);

    ::snprintf(start_line_buf, buf_size, "%u", StartLC.first);
    ::snprintf(start_column_buf, buf_size, "%u", StartLC.second);
    ::snprintf(end_line_buf, buf_size, "%u", EndLC.first);
    ::snprintf(end_column_buf, buf_size, "%u", EndLC.second);

    Expr *StartLine =
        new (Context) IntegerLiteralExpr(start_line_buf, SR.End, true);
    Expr *EndLine =
        new (Context) IntegerLiteralExpr(end_line_buf, SR.End, true);
    Expr *StartColumn =
        new (Context) IntegerLiteralExpr(start_column_buf, SR.End, true);
    Expr *EndColumn =
        new (Context) IntegerLiteralExpr(end_column_buf, SR.End, true);

    llvm::SmallVector<Expr *, 4> ArgsWithSourceRange{};

    ArgsWithSourceRange.append({StartLine, EndLine, StartColumn, EndColumn});

    UnresolvedDeclRefExpr *LoggerRef = new (Context)
        UnresolvedDeclRefExpr(Context.getIdentifier(LoggerName),
                              DeclRefKind::Ordinary, DeclNameLoc(SR.End));

    LoggerRef->setImplicit(true);

    SmallVector<Identifier, 4> ArgLabels(ArgsWithSourceRange.size(),
                                         Identifier());
    ApplyExpr *LoggerCall = CallExpr::createImplicit(
        Context, LoggerRef, ArgsWithSourceRange, ArgLabels);
    Added<ApplyExpr *> AddedLogger(LoggerCall);

    if (!doTypeCheck(Context, TypeCheckDC, AddedLogger)) {
      return nullptr;
    }

    return buildLoggerCallWithApply(AddedLogger, SR);
  }

  // Assumes Apply has already been type-checked.
  Added<Stmt *> buildLoggerCallWithApply(Added<ApplyExpr *> Apply,
                                         SourceRange SR) {

    ASTNode Elements[] = {*Apply};

    BraceStmt *BS =
        BraceStmt::create(Context, SourceLoc(), Elements, SourceLoc(), true);

    return BS;
  }
};

} // end anonymous namespace

void swift::performPCMacro(SourceFile &SF, TopLevelContext &TLC) {
  class ExpressionFinder : public ASTWalker {
  private:
    unsigned TmpNameIndex = 0;
    TopLevelContext &TLC;

  public:
    ExpressionFinder(TopLevelContext &TLC) : TLC(TLC) {}

    bool walkToDeclPre(Decl *D) override {
      if (AbstractFunctionDecl *FD = dyn_cast<AbstractFunctionDecl>(D)) {
        if (!FD->isImplicit()) {
          if (FD->getBody()) {
            ASTContext &ctx = FD->getASTContext();
            Instrumenter I(ctx, FD, TmpNameIndex);
            Decl *NewDecl = I.transformDecl(FD);
            if (AbstractFunctionDecl *NFD =
                    dyn_cast<AbstractFunctionDecl>(NewDecl)) {
              TypeChecker TC(ctx);
              TC.checkFunctionErrorHandling(NFD);
            }
            return false;
          }
        }
      } else if (TopLevelCodeDecl *TLCD = dyn_cast<TopLevelCodeDecl>(D)) {
        if (!TLCD->isImplicit()) {
          if (BraceStmt *Body = TLCD->getBody()) {
            ASTContext &ctx = static_cast<Decl *>(TLCD)->getASTContext();
            Instrumenter I(ctx, TLCD, TmpNameIndex);
            BraceStmt *NewBody = I.transformBraceStmt(Body, true);
            if (NewBody != Body) {
              TLCD->setBody(NewBody);
              TypeChecker TC(ctx);
              TC.checkTopLevelErrorHandling(TLCD);
              TC.contextualizeTopLevelCode(TLC,
                                           SmallVector<Decl *, 1>(1, TLCD));
            }
            return false;
          }
        }
      }
      return true;
    }
  };

  ExpressionFinder EF(TLC);
  for (Decl *D : SF.Decls) {
    D->walk(EF);
  }
}
