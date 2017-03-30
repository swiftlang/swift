//===--- SwitchCaseFullExpr.h -----------------------------------*- C++ -*-===//
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

#ifndef SWIFT_SILGEN_SWITCHCASEFULLEXPR_H
#define SWIFT_SILGEN_SWITCHCASEFULLEXPR_H

#include "Scope.h"

namespace swift {
namespace Lowering {

class SILGenFunction;

/// A cleanup scope RAII object, like FullExpr, that comes with a JumpDest for a
/// continuation block. It is intended to be used to handle switch cases.
///
/// You *must* call exit() at some point.
///
/// This scope is also exposed to the debug info.
class SwitchCaseFullExpr {
  SILGenFunction &SGF;
  Scope scope;
  CleanupLocation loc;
  NullablePtr<SILBasicBlock> contBlock;

public:
  SwitchCaseFullExpr(SILGenFunction &SGF, CleanupLocation loc);
  SwitchCaseFullExpr(SILGenFunction &SGF, CleanupLocation loc,
                     SILBasicBlock *contBlock);

  ~SwitchCaseFullExpr() = default;

  SwitchCaseFullExpr(const SwitchCaseFullExpr &) = delete;
  SwitchCaseFullExpr &operator=(const SwitchCaseFullExpr &) = delete;

  /// Pop the scope and branch to the cont block.
  void exitAndBranch(SILLocation loc, ArrayRef<SILValue> result = {});

  /// Pop the scope and do not branch to the cont block.
  void exit();
};

} // namespace Lowering
} // namespace swift

#endif
