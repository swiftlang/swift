//===--- SILGenProfiling.cpp - Instrumentation based profiling ------------===//
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

#include "SILGenProfiling.h"
#include "SILGen.h"
#include "swift/AST/ASTNode.h"
#include "swift/AST/ASTWalker.h"
#include "llvm/IR/Intrinsics.h"

using namespace swift;
using namespace Lowering;

namespace {

/// An ASTWalker that maps ASTNodes to profiling counters.
struct MapRegionCounters : public ASTWalker {
  /// The next counter value to assign.
  unsigned NextCounter;

  /// The map of statements to counters.
  llvm::DenseMap<ASTNode, unsigned> &CounterMap;

  MapRegionCounters(llvm::DenseMap<ASTNode, unsigned> &CounterMap)
      : NextCounter(0), CounterMap(CounterMap) {}

  bool walkToDeclPre(Decl *D) override {
    if (auto *AFD = dyn_cast<AbstractFunctionDecl>(D))
      CounterMap[AFD->getBody()] = NextCounter++;
    return true;
  }

  std::pair<bool, Stmt *> walkToStmtPre(Stmt *S) override {
    if (auto *IS = dyn_cast<IfStmt>(S)) {
      CounterMap[IS->getThenStmt()] = NextCounter++;
    } else if (auto *WS = dyn_cast<WhileStmt>(S)) {
      CounterMap[WS->getBody()] = NextCounter++;
    } else if (auto *DWS = dyn_cast<DoWhileStmt>(S)) {
      CounterMap[DWS->getBody()] = NextCounter++;
    } else if (auto *FS = dyn_cast<ForStmt>(S)) {
      CounterMap[FS->getBody()] = NextCounter++;
    } else if (auto *FES = dyn_cast<ForEachStmt>(S)) {
      CounterMap[FES->getBody()] = NextCounter++;
    }
    return {true, S};
  }

  std::pair<bool, Expr *> walkToExprPre(Expr *E) override {
    switch (E->getKind()) {
    default:
      break;
    case ExprKind::AutoClosure:
    case ExprKind::Closure:
    case ExprKind::If:
      CounterMap[E] = NextCounter++;
      break;
    }
    return {true, E};
  }
};

} // end anonymous namespace

void SILGenProfiling::assignRegionCounters(ASTNode Root, SILFunction &Fn) {
  setFuncName(Fn);

  MapRegionCounters Mapper(RegionCounterMap);
  Root.walk(Mapper);

  NumRegionCounters = Mapper.NextCounter;
  // TODO: Mapper needs to calculate a function hash as it goes.
  FunctionHash = 0x0;
}

void SILGenProfiling::setFuncName(SILFunction &Fn) {
  // TODO: Are SIL's mangled names sufficiently unique for the profile?
  CurrentFuncName = Fn.getName();
}

static SILLocation getLocation(ASTNode Node) {
  if (Expr *E = Node.dyn_cast<Expr *>())
    return E;
  else if (Stmt *S = Node.dyn_cast<Stmt *>())
    return S;
  else if (Decl *D = Node.dyn_cast<Decl *>())
    return D;
  else
    llvm_unreachable("unsupported ASTNode");
}

void SILGenProfiling::emitCounterIncrement(SILBuilder &Builder, ASTNode Node) {
  auto &C = Builder.getASTContext();

  auto CounterIt = RegionCounterMap.find(Node);
  assert(CounterIt != RegionCounterMap.end() &&
         "cannot increment non-existent counter");

  auto Int32Ty = SGM.Types.getLoweredType(BuiltinIntegerType::get(32, C));
  auto Int64Ty = SGM.Types.getLoweredType(BuiltinIntegerType::get(64, C));

  SILLocation Loc = getLocation(Node);
  SILValue Args[] = {
      // TODO: In C++ we give this string linkage that matches the functions, so
      // that it's uniqued appropriately across TUs.
      Builder.createStringLiteral(Loc, StringRef(CurrentFuncName),
                                  StringLiteralInst::Encoding::UTF8),
      Builder.createIntegerLiteral(Loc, Int64Ty, FunctionHash),
      Builder.createIntegerLiteral(Loc, Int32Ty, NumRegionCounters),
      // TODO: Should we take care to emit only one copy of each of the above
      // three literals per function?
      Builder.createIntegerLiteral(Loc, Int32Ty, CounterIt->second)};
  Builder.createBuiltin(Loc, C.getIdentifier("int_instrprof_increment"),
                        SGM.Types.getEmptyTupleType(), {}, Args);
}
