//===--- SimplifyDynamicPackIndex.swift -----------------------------------===//
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

// A dynamic_pack_index into an empty pack has undefined behavior, because no
// index is in range for the pack. Code reaching one is therefore unreachable.
//
// This removes the body of a loop over an empty pack, such as one in a
// specialization of a variadic generic function called with no arguments.
// Otherwise, its @pack_element types can never be eliminated.
//
// Before:
// bb1:
//   %a = ...
//   %idx = dynamic_pack_index %i of $Pack{}
//   ...
//
// After:
// bb1:
//   %a = ...
//   unreachable
//
extension DynamicPackIndexInst : OnoneSimplifiable {
  func simplify(_ context: SimplifyContext) {
    guard self.indexedPackType.rawType.elementTypesOfPackType.isEmpty else {
      return
    }

    let builder = Builder(before: self, context)
    let unreachable = builder.createUnreachable()
    // The instructions after the unreachable, starting with this one, are
    // moved to a new block without predecessors, which is removed later.
    _ = context.splitBlock(after: unreachable)
  }
}
