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

protocol IntrusiveSet : CustomStringConvertible, NoReflectionChildren {
  associatedtype Element

  init(_ context: some Context)
  mutating func insert(_ element: Element) -> Bool
  mutating func erase(_ element: Element)
  func contains(_ element: Element) -> Bool
  mutating func deinitialize()
}

/// A set of basic blocks.
///
/// This is an extremely efficient implementation which does not need memory
/// allocations or hash lookups.
///
/// This type should be a move-only type, but unfortunately we don't have move-only
/// types yet. Therefore it's needed to call `deinitialize()` explicitly to
/// destruct this data structure, e.g. in a `defer {}` block.
struct BasicBlockSet : IntrusiveSet {

  private let context: BridgedPassContext
  private let bridged: BridgedBasicBlockSet
    
  init(_ context: some Context) {
    self.context = context._bridged
    self.bridged = PassContext_allocBasicBlockSet(self.context)
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

  /// TODO: once we have move-only types, make this a real deinit.
  mutating func deinitialize() {
    PassContext_freeBasicBlockSet(context, bridged)
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
struct ValueSet : IntrusiveSet {

  private let context: BridgedPassContext
  private let bridged: BridgedNodeSet
    
  init(_ context: some Context) {
    self.context = context._bridged
    self.bridged = PassContext_allocNodeSet(self.context)
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

  /// TODO: once we have move-only types, make this a real deinit.
  mutating func deinitialize() {
    PassContext_freeNodeSet(context, bridged)
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
struct InstructionSet : IntrusiveSet {

  private let context: BridgedPassContext
  private let bridged: BridgedNodeSet
    
  init(_ context: some Context) {
    self.context = context._bridged
    self.bridged = PassContext_allocNodeSet(self.context)
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

  /// TODO: once we have move-only types, make this a real deinit.
  mutating func deinitialize() {
    PassContext_freeNodeSet(context, bridged)
  }
}
