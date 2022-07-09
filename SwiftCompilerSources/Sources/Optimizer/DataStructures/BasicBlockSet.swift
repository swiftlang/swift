//===--- BasicBlockSet.swift - a set of basic blocks ----------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import SIL
import OptimizerBridging

/// A set of basic blocks.
///
/// This is an extremely efficient implementation which does not need memory
/// allocations or hash lookups.
///
/// This type should be a move-only type, but unfortunately we don't have move-only
/// types yet. Therefore it's needed to call `deinitialize()` explicitly to
/// destruct this data structure, e.g. in a `defer {}` block.
struct BasicBlockSet : CustomStringConvertible, CustomReflectable {

  private let context: PassContext
  private let bridged: BridgedBasicBlockSet
    
  init(_ context: PassContext) {
    self.context = context
    self.bridged = PassContext_allocBasicBlockSet(context._bridged)
  }

  func contains(_ block: BasicBlock) -> Bool {
    BasicBlockSet_contains(bridged, block.bridged) != 0
  }

  mutating func insert(_ block: BasicBlock) {
    BasicBlockSet_insert(bridged, block.bridged)
  }

  mutating func erase(_ block: BasicBlock) {
    BasicBlockSet_erase(bridged, block.bridged)
  }

  var description: String {
    let function = BasicBlockSet_getFunction(bridged).function
    let blockNames = function.blocks.enumerated().filter { contains($0.1) }
                                                 .map { "bb\($0.0)"}
    return "{" + blockNames.joined(separator: ", ") + "}"
  }

  var customMirror: Mirror { Mirror(self, children: []) }

  /// TODO: once we have move-only types, make this a real deinit.
  mutating func deinitialize() {
    PassContext_freeBasicBlockSet(context._bridged, bridged)
  }
}
