//===-- ConstantFolding.h - Utilities for SIL constant folding --*- C++ -*-===//
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
// This file defines utility functions for constant folding.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SIL_CONSTANTFOLDING_H
#define SWIFT_SIL_CONSTANTFOLDING_H

#include "swift/SIL/SILInstruction.h"

namespace swift {

/// Evaluates the constant result of a binary bit-operation.
///
/// The \p ID must be the ID of a binary bit-operation builtin.
APInt constantFoldBitOperation(APInt lhs, APInt rhs, BuiltinValueKind ID);

/// Evaluates the constant result of an integer comparison.
///
/// The \p ID must be the ID of an integer builtin operation.
APInt constantFoldComparison(APInt lhs, APInt rhs, BuiltinValueKind ID);

/// Evaluates the constant result of a binary operation with overflow.
///
/// The \p ID must be the ID of a binary operation with overflow.
APInt constantFoldBinaryWithOverflow(APInt lhs, APInt rhs, bool &Overflow,
                                     llvm::Intrinsic::ID ID);

/// Evaluates the constant result of a division operation.
///
/// The \p ID must be the ID of a division operation.
APInt constantFoldDiv(APInt lhs, APInt rhs, bool &Overflow, BuiltinValueKind ID);

  /// Evaluates the constant result of an integer cast operation.
  ///
  /// The \p ID must be the ID of a trunc/sext/zext builtin.
APInt constantFoldCast(APInt val, const BuiltinInfo &BI);

} // End namespace swift.

#endif
