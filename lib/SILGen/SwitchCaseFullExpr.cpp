//===--- SwitchCaseFullExpr.cpp -------------------------------------------===//
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

#include "SwitchCaseFullExpr.h"
#include "SILGenFunction.h"
#include "Scope.h"
#include "swift/SIL/SILLocation.h"

using namespace swift;
using namespace Lowering;

SwitchCaseFullExpr::SwitchCaseFullExpr(SILGenFunction &SGF, CleanupLocation loc,
                                       SILBasicBlock *contBlock)
    : SGF(SGF), scope(SGF.Cleanups, loc), loc(loc),
      contBlock(contBlock ? *contBlock : *SGF.B.splitBlockForFallthrough()) {}

SwitchCaseFullExpr::~SwitchCaseFullExpr() {
  assert(!scope.isValid() && "Switch Case Full Expr was not popped?!");
}

void SwitchCaseFullExpr::exit(SILLocation loc, ArrayRef<SILValue> branchArgs) {
  assert(SGF.B.hasValidInsertionPoint());
  scope.pop();
  SGF.B.createBranch(loc, &contBlock, branchArgs);
}
