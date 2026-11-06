//===--- SimplifyTuplePackElementAddr.swift -------------------------------===//
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

// Replace a tuple_pack_element_addr with tuple_element_addr, if its tuple type
// contains no pack expansions and the structural index of its pack index
// operand is statically known.
//
// %tuple = alloc_stack $(Int, Int)
// %idx = scalar_pack_index 0 of $Pack{Int, Int}
// %addr = tuple_pack_element_addr %idx of %tuple as $*Int
//
// is transformed to:
//
// %tuple = alloc_stack $(Int, Int)
// %addr = tuple_element_addr %tuple, 0
//
// If the result type of the tuple_pack_element_addr differs from the type of
// the tuple element, e.g. because it is an opened pack element archetype, an
// unchecked_addr_cast to the original result type is inserted:
//
// %i = integer_literal $Builtin.Word, 1
// %idx = dynamic_pack_index %i of $Pack{Int, String}
// %addr = tuple_pack_element_addr %idx of %tuple as $*@pack_element("...") T
//
// is transformed to:
//
// %elt = tuple_element_addr %tuple, 1
// %addr = unchecked_addr_cast %elt to $*@pack_element("...") T
//
extension TuplePackElementAddrInst : OnoneSimplifiable, SILCombineSimplifiable {
  func simplify(_ context: SimplifyContext) {
    guard !self.tupleOperand.value.type.tupleContainsPackExpansionType,
          let packIndex = self.indexOperand.value as? AnyPackIndexInst,
          let index = packIndex.structuralIndex
    else {
      return
    }

    let builder = Builder(before: self, context)
    let tupleElementAddr = builder.createTupleElementAddr(tupleAddress: self.tupleOperand.value, elementIndex: index)
    if tupleElementAddr.type == self.type {
      self.replace(with: tupleElementAddr, context)
    } else {
      let cast = builder.createUncheckedAddrCast(from: tupleElementAddr, to: self.type)
      self.replace(with: cast, context)
    }
  }
}
