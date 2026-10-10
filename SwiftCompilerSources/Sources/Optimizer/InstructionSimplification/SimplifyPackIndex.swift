//===--- SimplifyPackIndex.swift ------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import SIL

/// Replace a dynamic_pack_index with a scalar_pack_index when the equivalent
/// scalar index can be determined.
///
/// Before:
///
///   %0 = integer_literal $Builtin.Word, 1
///   %idx = dynamic_pack_index %0 of $Pack{Int, Float, repeat each T}
///
/// After:
///
///   %idx = scalar_pack_index 1 of $Pack{Int, Float, repeat each T}
///
extension DynamicPackIndexInst: Simplifiable, SILCombineSimplifiable {
  func simplify(_ context: SimplifyContext) {
    guard let index = self.structuralIndex
    else {
      return
    }

    let builder = Builder(before: self, context)
    let scalarPackIndex = builder.createScalarPackIndex(
      componentIndex: index, indexedPackType: self.indexedPackType)
    self.replace(with: scalarPackIndex, context)
    return
  }
}

/// Replace a pack_pack_index with a scalar_pack_index when the equivalent
/// scalar index can be determined.
///
/// Before:
///
///   %0 = integer_literal $Builtin.Word, 1
///   %base = dynamic_pack_index %0 of $Pack{Float, Double, repeat each T}
///   %idx = pack_pack_index 1, %base of $Pack{Int, Float, Double, repeat each T}
///
/// After:
///
///   %idx = scalar_pack_index 2 of $Pack{Int, Float, Double, repeat each T}
///
extension PackPackIndexInst: Simplifiable, SILCombineSimplifiable {
  func simplify(_ context: SimplifyContext) {
    guard let index = self.structuralIndex
    else {
      return
    }

    let builder = Builder(before: self, context)
    let scalarPackIndex = builder.createScalarPackIndex(
      componentIndex: index, indexedPackType: self.indexedPackType)
    self.replace(with: scalarPackIndex, context)
    return
  }
}
