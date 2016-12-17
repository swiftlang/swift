//===--- StmtTransformer.cpp - Swift Language Statement ASTs --------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
//  This file implements the StmtTransformer class.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/StmtTransformer.h"

using namespace swift;

Stmt *StmtTransformer::transformStmt(Stmt *S) {
  switch (S->getKind()) {
#define STMT(CLASS, PARENT) \
  case StmtKind::CLASS: \
    return transform##CLASS##Stmt(static_cast<CLASS##Stmt *>(S));
#include "swift/AST/StmtNodes.def"
  }
  llvm_unreachable("Not reachable, all cases handled");
}

// transform*() return their input if it's unmodified,
// or a modified copy of their input otherwise.
IfStmt *StmtTransformer::transformIfStmt(IfStmt *IS) {
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

GuardStmt *StmtTransformer::transformGuardStmt(GuardStmt *GS) {
  if (Stmt *BS = GS->getBody()) {
    auto NBS = transformStmt(BS);
    if (NBS != BS) {
      GS->setBody(NBS);
    }
  }
  return GS;
}

WhileStmt *StmtTransformer::transformWhileStmt(WhileStmt *WS) {
  if (Stmt *B = WS->getBody()) {
    Stmt *NB = transformStmt(B);
    if (NB != B) {
      WS->setBody(NB);
    }
  }

  return WS;
}

RepeatWhileStmt *StmtTransformer::transformRepeatWhileStmt(RepeatWhileStmt *RWS) {
  if (Stmt *B = RWS->getBody()) {
    Stmt *NB = transformStmt(B);
    if (NB != B) {
      RWS->setBody(NB);
    }
  }

  return RWS;
}

ForStmt *StmtTransformer::transformForStmt(ForStmt *FS) {
  if (Stmt *B = FS->getBody()) {
    Stmt *NB = transformStmt(B);
    if (NB != B) {
      FS->setBody(NB);
    }
  }

  return FS;
}

ForEachStmt *StmtTransformer::transformForEachStmt(ForEachStmt *FES) {
  if (BraceStmt *B = FES->getBody()) {
    BraceStmt *NB = transformBraceStmt(B);
    if (NB != B) {
      FES->setBody(NB);
    }
  }

  return FES;
}

SwitchStmt *StmtTransformer::transformSwitchStmt(SwitchStmt *SS) {
  for (CaseStmt *CS : SS->getCases()) {
    if (Stmt *S = CS->getBody()) {
      if (auto *B = dyn_cast<BraceStmt>(S)) {
        BraceStmt *NB = transformBraceStmt(B);
        if (NB != B) {
          CS->setBody(NB);
        }
      }
    }
  }

  return SS;
}

DoStmt *StmtTransformer::transformDoStmt(DoStmt *DS) {
  if (BraceStmt *B = dyn_cast_or_null<BraceStmt>(DS->getBody())) {
    BraceStmt *NB = transformBraceStmt(B);
    if (NB != B) {
      DS->setBody(NB);
    }
  }
  return DS;
}

DoCatchStmt *StmtTransformer::transformDoCatchStmt(DoCatchStmt *DCS) {
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

BraceStmt *StmtTransformer::transformBraceStmt(BraceStmt *BS) {
  return transformBraceStmt(BS, false);
}

BraceStmt *StmtTransformer::transformBraceStmt(BraceStmt *BS, bool TopLevel) {
  return BS;
}
CaseStmt *StmtTransformer::transformCaseStmt(CaseStmt *CS) { return CS; }
ReturnStmt *StmtTransformer::transformReturnStmt(ReturnStmt *RS) { return RS; }
DeferStmt *StmtTransformer::transformDeferStmt(DeferStmt *DS) { return DS; }
FailStmt *StmtTransformer::transformFailStmt(FailStmt *ST) { return ST; }
BreakStmt *StmtTransformer::transformBreakStmt(BreakStmt *ST) { return ST; }
CatchStmt *StmtTransformer::transformCatchStmt(CatchStmt *ST) { return ST; }
ThrowStmt *StmtTransformer::transformThrowStmt(ThrowStmt *ST) { return ST; }
ContinueStmt *StmtTransformer::transformContinueStmt(ContinueStmt *ST) {
  return ST;
}
IfConfigStmt *StmtTransformer::transformIfConfigStmt(IfConfigStmt *ST) {
  return ST;
}
FallthroughStmt *StmtTransformer::transformFallthroughStmt(FallthroughStmt *ST)
{
  return ST;
}
