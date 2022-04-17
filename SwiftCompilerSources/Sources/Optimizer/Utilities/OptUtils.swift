//===--- OptUtils.swift - Utilities for optimizations ----------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2021 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import SIL

extension Value {
  var nonDebugUses: LazyFilterSequence<UseList> {
    uses.lazy.filter { !($0.instruction is DebugValueInst) }
  }
}

extension Builder {
  static func insert(after inst: Instruction, location: Location,
                     _ context: PassContext, insertFunc: (Builder) -> ()) {
    if inst is TermInst {
      for succ in inst.block.successors {
        assert(succ.hasSinglePredecessor,
               "the terminator instruction must not have critical successors")
        let builder = Builder(at: succ.instructions.first!, location: location,
                              context)
        insertFunc(builder)
      }
    } else {
      let builder = Builder(at: inst.next!, location: location, context)
      insertFunc(builder)
    }
  }
}
