//===--- SimplifyDebugReconstruction.swift --------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import SIL

/// Instructions whose only reconstruction block simplification is folding an `undef` operand.
///
/// These are all salvageable into a debug reconstruction block (see `salvageDebugInfo`), but
/// have nothing to simplify in a regular block.
protocol UndefFoldingSimplifiable : DebugReconstructionBlockSimplifiable, SingleValueInstruction {
}

extension UndefFoldingSimplifiable {
  func simplifyForDebugReconstructionBlock(_ context: SimplifyContext) {
    foldUndefOperands(context)
  }
}

// Note: `enum` is deliberately not listed here, as folding away a known case loses information.

// Casts and conversions.
extension UpcastInst : UndefFoldingSimplifiable {}
extension ConvertFunctionInst : UndefFoldingSimplifiable {}
extension AddressToPointerInst : UndefFoldingSimplifiable {}

// Address projections.
extension StructElementAddrInst : UndefFoldingSimplifiable {}
extension TupleElementAddrInst : UndefFoldingSimplifiable {}
extension RefElementAddrInst : UndefFoldingSimplifiable {}
extension RefTailAddrInst : UndefFoldingSimplifiable {}
extension VectorBaseAddrInst : UndefFoldingSimplifiable {}
