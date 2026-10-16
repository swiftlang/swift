//===--- PostOrder.swift - the post-order of a function's blocks ----------===//
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
import OptimizerBridging

/// The reachable blocks of a function in post-order, i.e. each block comes after all of its
/// successors, ignoring back-edges. Use `reversed()` for reverse post-order.
///
/// This is a view of the cached PostOrderAnalysis, which is only invalidated at the end of a pass or
/// by `context.updateAnalysis()`. Accessing it after invalidation is a fatal error.
///
/// Blocks created in the meantime are not included, and erased blocks must not be visited.
struct PostOrder: RandomAccessCollection {
  private let bridged: BridgedPostOrder

  init(bridged: BridgedPostOrder) {
    self.bridged = bridged
  }

  var startIndex: Int { 0 }
  var endIndex: Int { bridged.count }

  subscript(_ index: Int) -> BasicBlock {
    assert(index >= 0 && index < endIndex, "post-order index out of range")
    return bridged.getBlock(index).block
  }
}

//===--------------------------------------------------------------------===//
//                              Tests
//===--------------------------------------------------------------------===//

let postOrderTest = FunctionTest("post_order") {
  function, arguments, context in

  let postOrder = context.postOrder
  print("post-order: \(postOrder.map { $0.index })")
  print("reverse post-order: \(postOrder.reversed().map { $0.index })")
}

let postOrderInvalidationTest = FunctionTest("post_order_invalidation") {
  function, arguments, context in

  let postOrder = context.postOrder
  let copy = Array(postOrder)

  _ = context.splitBlock(before: function.entryBlock.terminator)
  context.updateAnalysis()

  print("copy: \(copy.map { $0.index })")
  print("view: \(postOrder[0].index)")
}
