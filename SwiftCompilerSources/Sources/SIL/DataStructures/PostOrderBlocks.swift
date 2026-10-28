//===--- PostOrderBlocks.swift - the post-order of a function's blocks ----===//
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

/// The blocks reachable from the entry block in post-order, i.e. each block comes after all of its
/// successors, ignoring back-edges. Use `reversed()` for reverse post-order.
///
/// The post-order is computed lazily, and each iteration recomputes it from scratch.
///
/// The CFG may be changed while iterating. Changing blocks which were already returned doesn't affect
/// the rest of the iteration, e.g. blocks created as their successors are not returned. Changing the
/// terminator of any other block may cause blocks to be skipped or newly created blocks to be returned,
/// and such a block must not be erased.
public struct PostOrderBlocks<Ctxt: Context> : CollectionLikeSequence {
  let function: Function
  let context: Ctxt

  public func makeIterator() -> PostOrderBlockIterator {
    PostOrderBlockIterator(function, context)
  }
}

// This iterator must be a class because we need a deinit.
// TODO: Make it a struct once this is possible with non-copyable types.
public final class PostOrderBlockIterator : IteratorProtocol {
  private var visited: BasicBlockSet

  /// The current path of the depth-first walk, with the index of the next successor to visit.
  private var path: Stack<(block: BasicBlock, nextSuccessor: Int)>

  init(_ function: Function, _ context: some Context) {
    self.visited = BasicBlockSet(context)
    self.path = Stack(context)
    if let entryBlock = function.blocks.first {
      visited.insert(entryBlock)
      path.push((entryBlock, 0))
    }
  }

  deinit {
    path.deinitialize()
    visited.deinitialize()
  }

  public func next() -> BasicBlock? {
    while let top = path.pop() {
      let successors = top.block.successors
      if top.nextSuccessor >= successors.count {
        return top.block
      }
      path.push((top.block, top.nextSuccessor + 1))
      let successor = successors[top.nextSuccessor]
      if visited.insert(successor) {
        path.push((successor, 0))
      }
    }
    return nil
  }
}

extension Function {
  /// The blocks reachable from the entry block in post-order.
  public func postOrderBlocks<Ctxt: Context>(_ context: Ctxt) -> PostOrderBlocks<Ctxt> {
    PostOrderBlocks(function: self, context: context)
  }
}

//===--------------------------------------------------------------------===//
//                              Tests
//===--------------------------------------------------------------------===//

let postOrderBlocksTest = Test("post_order_blocks") {
  function, arguments, context in

  let postOrder = function.postOrderBlocks(context)
  print("post-order: \(postOrder.map { $0.index })")
  print("reverse post-order: \(postOrder.reversed().map { $0.index })")
  for block in postOrder {
    print("first: \(block.index)")
    break
  }
}
