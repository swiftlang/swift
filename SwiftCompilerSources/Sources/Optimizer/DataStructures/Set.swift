//===--- Set.swift - sets for basic blocks, values and instructions -------===//
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

  /// Returns true if `block` was not contained in the set before inserting.
  @discardableResult
  mutating func insert(_ block: BasicBlock) -> Bool {
    BasicBlockSet_insert(bridged, block.bridged) != 0
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

/// A set of values.
///
/// This is an extremely efficient implementation which does not need memory
/// allocations or hash lookups.
///
/// This type should be a move-only type, but unfortunately we don't have move-only
/// types yet. Therefore it's needed to call `deinitialize()` explicitly to
/// destruct this data structure, e.g. in a `defer {}` block.
struct ValueSet : CustomStringConvertible, CustomReflectable {

  private let context: PassContext
  private let bridged: BridgedNodeSet
    
  init(_ context: PassContext) {
    self.context = context
    self.bridged = PassContext_allocNodeSet(context._bridged)
  }

  func contains(_ value: Value) -> Bool {
    NodeSet_containsValue(bridged, value.bridged) != 0
  }

  /// Returns true if `value` was not contained in the set before inserting.
  @discardableResult
  mutating func insert(_ value: Value) -> Bool {
    NodeSet_insertValue(bridged, value.bridged) != 0
  }

  mutating func erase(_ value: Value) {
    NodeSet_eraseValue(bridged, value.bridged)
  }

  var description: String {
    let function = NodeSet_getFunction(bridged).function
    var d = "{\n"
    for block in function.blocks {
      for arg in block.arguments {
        if contains(arg) {
          d += arg.description
        }
      }
      for inst in block.instructions {
        for result in inst.results {
          if contains(result) {
            d += result.description
          }
        }
      }
    }
    d += "}\n"
    return d
  }

  var customMirror: Mirror { Mirror(self, children: []) }

  /// TODO: once we have move-only types, make this a real deinit.
  mutating func deinitialize() {
    PassContext_freeNodeSet(context._bridged, bridged)
  }
}

/// A set of instructions.
///
/// This is an extremely efficient implementation which does not need memory
/// allocations or hash lookups.
///
/// This type should be a move-only type, but unfortunately we don't have move-only
/// types yet. Therefore it's needed to call `deinitialize()` explicitly to
/// destruct this data structure, e.g. in a `defer {}` block.
struct InstructionSet : CustomStringConvertible, CustomReflectable {

  private let context: PassContext
  private let bridged: BridgedNodeSet
    
  init(_ context: PassContext) {
    self.context = context
    self.bridged = PassContext_allocNodeSet(context._bridged)
  }

  func contains(_ inst: Instruction) -> Bool {
    NodeSet_containsInstruction(bridged, inst.bridged) != 0
  }

  /// Returns true if `inst` was not contained in the set before inserting.
  @discardableResult
  mutating func insert(_ inst: Instruction) -> Bool {
    NodeSet_insertInstruction(bridged, inst.bridged) != 0
  }

  mutating func erase(_ inst: Instruction) {
    NodeSet_eraseInstruction(bridged, inst.bridged)
  }

  var description: String {
    let function = NodeSet_getFunction(bridged).function
    var d = "{\n"
    for inst in function.instructions {
      if contains(inst) {
        d += inst.description
      }
    }
    d += "}\n"
    return d
  }

  var customMirror: Mirror { Mirror(self, children: []) }

  /// TODO: once we have move-only types, make this a real deinit.
  mutating func deinitialize() {
    PassContext_freeNodeSet(context._bridged, bridged)
  }
}
