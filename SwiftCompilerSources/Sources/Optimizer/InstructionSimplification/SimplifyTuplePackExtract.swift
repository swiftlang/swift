//===--- SimplifyTuplePackExtract.swift -----------------------------------===//
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

// Replace a tuple_pack_extract with a scalar_pack_index index operand
// with tuple_element_addr, if its tuple type contains no pack expansions.
//
// %tuple = ... : $(Int, Int)
// %idx = scalar_pack_index 0 of $Pack{Int, Int}
// %addr = tuple_pack_exttract %idx of %tuple as $Int
//
// is transformed to:
//
// %tuple = ... : $(Int, Int)
// %addr = tuple_extract 0 of %tuple as $Int
//
extension TuplePackExtractInst : OnoneSimplifiable {
  func simplify(_ context: SimplifyContext) {
    guard !self.tupleOperand.value.type.tupleContainsPackExpansionType,
          let scalarIndex = self.indexOperand.value as? ScalarPackIndexInst
    else {
      return
    }

    let builder = Builder(before: self, context)
    let tupleExtract = builder.createTupleExtract(tuple: self.tupleOperand.value, elementIndex: scalarIndex.componentIndex)
    self.replace(with: tupleExtract, context)
  }
}
