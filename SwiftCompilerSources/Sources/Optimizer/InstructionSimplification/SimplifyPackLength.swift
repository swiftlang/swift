//===--- SimplifyPackLength.swift -----------------------------------------===//
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

import AST
import SIL

// Simplify `pack_length` with constant-length pack.
//
// Before:
// %len = pack_length $Pack{Int, String, Float}
//
// After:
// %len = integer_literal Builtin.Word, 3
//
extension PackLengthInst : SILCombineSimplifiable, OnoneSimplifiable {
  func simplify(_ context: SimplifyContext) {
    let packElements = self.packType.rawType.elementTypesOfPackType
    if packElements.contains(where: { $0.isPackExpansion }) {
      return
    }

    let builder = Builder(before: self, context)
    let integerLiteral = builder.createIntegerLiteral(packElements.count, type: self.type)
    self.replace(with: integerLiteral, context)
  }
}
