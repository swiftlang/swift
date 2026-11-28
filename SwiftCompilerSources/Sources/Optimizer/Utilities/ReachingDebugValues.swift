//===--- ReachingDebugValues.swift - Debug values describing variables ----===//
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

/// A set of variables, indexed in the order of insertion.
struct TrackedVariables {
  /// The complete info of each variable, as described by its first inserted debug value.
  private(set) var debugVariables = [DebugVariableInstruction.DebugVariable]()

  /// Maps each variable to its index in the array.
  private var indices = [DebugVariableInstruction.DebugVariable: Int]()

  var count: Int { debugVariables.count }

  /// Inserts the variable which `debugValue` describes, if it is not contained yet.
  mutating func insert(variableOf debugValue: DebugValueInst) {
    guard let variable = debugValue.debugVariable, indices[variable] == nil else {
      return
    }
    indices[variable] = debugVariables.count
    debugVariables.append(variable)
  }

  /// The index of the variable which `debugValue` describes, or nil if it is untracked.
  func index(ofVariableOf debugValue: DebugValueInst) -> Int? {
    debugValue.debugVariable.flatMap { indices[$0] }
  }
}

/// What describes a tracked variable at a point of the function.
enum ReachingDebugValue {
  /// The value of the variable is directly described by a single debug value instruction.
  case debugValue(DebugValueInst)

  /// The value of the variable is unknown. The value may be defined on a path, and killed or never
  /// described on another path: the value cannot be salvaged with the given information.
  case undescribed

  /// Different debug values reach the point on different paths, which join at the beginning of
  /// the block. The value of the variable is known to be one of the incoming debug values,
  /// but it cannot be speculated which one.
  case join(BasicBlock, debugValues: [DebugValueInst])
}

/// Customizes `ReachingDebugValues`.
protocol ReachingDebugValuesClient {
  /// Updates `states`, the list of variable states, with the side effects of a non-debug instruction.
  ///
  /// This models the debug values which would be inserted after `instruction`, e.g. a kill
  /// changes the state to `.undescribed`.
  ///
  /// For the dataflow to terminate, `transfer` must preserve the order of states: if a state moves
  /// from a debug value to a join, a join gains debug values, or a state becomes `.undescribed`, the
  /// transferred state may only stay the same or move in the same direction.
  func transfer(after instruction: Instruction, states: inout [ReachingDebugValue])
}

/// Computes, for each point of a region of a function, what describes each tracked variable at the
/// point.
///
/// A debug value describes its variable until the next debug value of the variable. Optimizations
/// use this to describe variables at new points:
/// * Optimizations which replace memory by SSA values, like DeadObjectElimination, insert a clone of
///   the reaching debug value after each store.
/// * Optimizations which merge control flow paths, like SimplifyCFG, join the debug values of the
///   paths.
/// * The propagation of debug values into async funclets clones the reaching debug values after
///   each suspension point.
///
/// The region starts at `begin`, whose block dominates the other blocks of the region. The variables
/// are undescribed at `begin`, and along the edges which enter the region elsewhere.
///
/// Complexity: the blocks are visited in reverse post-order, and a block is walked again only if
/// the state at its beginning changed, which only a loop back edge can cause. A block is
/// therefore typically walked once, or twice in a loop.
struct ReachingDebugValues<Client: ReachingDebugValuesClient> {
  /// The list of tracked variables.
  /// The analysis ignores any variable not part of this list.
  let variables: TrackedVariables

  /// The client may customize part of this analysis, for example, to model a side effect
  /// of an instruction.
  private let client: Client

  /// The instruction at which the analysis starts.
  private let begin: Instruction

  /// The blocks of the region which are reachable from the function entry, in reverse post-order.
  private let blocks: [BasicBlock]

  /// A map of the information for each `debug_value` instruction: its variable index in `TrackedVariables`,
  /// and its identifier, which allows grouping identical instructions.
  private var debugValueInfo = [DebugValueInst: (variableIndex: Int, id: DebugValueID)]()

  /// A map of block, to the list of states for each variable at the beginning of this block
  /// (using the index from TrackedVariables)
  private var entryStates = [BasicBlock: [ReachingDebugValue]]()

  /// A map of block, to the list of states for each variable at the end of this block
  private var exitStates = [BasicBlock: [ReachingDebugValue]]()

  /// `region` must contain the block of `begin`.
  init(of variables: TrackedVariables, in region: BasicBlockSet, startingAt begin: Instruction,
       client: Client, _ context: some Context) {
    self.variables = variables
    self.client = client
    self.begin = begin
    self.blocks = begin.parentFunction.postOrderBlocks(context).reversed().filter { region.contains($0) }

    // Any predecessor outside of the region is considered undescribed.
    for block in blocks {
      for predecessor in block.predecessors where !region.contains(predecessor) {
        exitStates[predecessor] = undescribedState
      }
    }
    assignDebugValueIDs()
    solve()
  }

  /// What describes each variable at the beginning of `block`, or at `begin` if it is in `block`.
  /// Nil if `block` is not in the region, or is unreachable.
  func reachingDebugValues(atBeginOf block: BasicBlock) -> [ReachingDebugValue]? {
    entryStates[block]
  }

  /// What describes variable `index` at the end of each predecessor of `block` which is not
  /// unreachable.
  func incoming(ofVariable index: Int,
                atBeginOf block: BasicBlock) -> [(predecessor: BasicBlock, reaching: ReachingDebugValue)] {
    // `compactMap` will only skip over unreachable predecessors.
    block.predecessors.compactMap { pred in exitStates[pred].map { (predecessor: pred, reaching: $0[index]) } }
  }

  /// Calls `body` with each instruction in the region, and what describes each variable before the
  /// instruction.
  ///
  /// `body` must not change the blocks of the region.
  func forEachInstruction(body: (Instruction, [ReachingDebugValue]) -> ()) {
    for block in blocks {
      if let entryState = entryStates[block] {
        _ = walk(block: block, from: entryState, body: body)
      }
    }
  }

  /// Computes `debugValueInfo` for the debug values in `blocks`.
  private mutating func assignDebugValueIDs() {
    var assignedIDMap = [DebugValueIDKey: DebugValueID]()
    var numIDs = 0
    func newID() -> DebugValueID {
      defer { numIDs += 1 }
      return DebugValueID(value: numIDs)
    }

    for block in blocks {
      for inst in instructions(of: block) {
        // Skip over untracked variables.
        guard let debugValue = inst as? DebugValueInst,
              let variableIndex = variables.index(ofVariableOf: debugValue) else {
          continue
        }
        let id: DebugValueID
        if debugValue.debugReconstructionBlock == nil && debugValue.operands.count == 1 {
          // Debug values with no reconstruction block are considered identical if they have the same
          // variable and operand.
          let key = DebugValueIDKey(variableIndex: variableIndex, operand: debugValue.operands[0].value.hashable)
          if let existingID = assignedIDMap[key] {
            id = existingID
          } else {
            id = newID()
            assignedIDMap[key] = id
          }
        } else {
          id = newID()
        }
        debugValueInfo[debugValue] = (variableIndex, id)
      }
    }
  }

  /// Computes `entryStates` and `exitStates` of `blocks`.
  private mutating func solve() {
    var positions = [BasicBlock: Int]()
    for (position, block) in blocks.enumerated() {
      positions[block] = position
    }
    // Visits the blocks in reverse post-order, and a block again when the exit state of a
    // predecessor changed. A change along a back edge needs another sweep.
    var needsUpdate = [Bool](repeating: true, count: blocks.count)
    var needsAnotherSweep: Bool
    repeat {
      needsAnotherSweep = false
      for (position, block) in blocks.enumerated() where needsUpdate[position] {
        needsUpdate[position] = false
        guard let entryState = mergedEntryState(of: block) else {
          // No predecessor has been visited yet.
          continue
        }
        // If the entry state hasn't changed, skip the walk, as it would produce the same result.
        if let previousEntryState = entryStates[block], areStatesIdentical(entryState, previousEntryState) {
          continue
        }
        entryStates[block] = entryState
        let exitState = walk(block: block, from: entryState) { _, _ in }
        if let previousExitState = exitStates[block], areStatesIdentical(exitState, previousExitState) {
          continue
        }
        exitStates[block] = exitState
        for successor in block.successors {
          if let successorPosition = positions[successor] {
            needsUpdate[successorPosition] = true
            // A back edge: the successor was already visited in this sweep.
            needsAnotherSweep = needsAnotherSweep || successorPosition <= position
          }
        }
      }
    } while needsAnotherSweep
  }

  /// Merges the exit states of the visited predecessors of `block` with its previously computed entry state.
  /// Returns nil if no predecessor has been visited yet.
  private func mergedEntryState(of block: BasicBlock) -> [ReachingDebugValue]? {
    if block == begin.parentBlock {
      return undescribedState
    }
    let predecessorStates = block.predecessors.compactMap { exitStates[$0] }
    if predecessorStates.isEmpty {
      return nil
    }
    let previous = entryStates[block]
    return (0..<variables.count).map { index in
      merge(incoming: predecessorStates.map { $0[index] }, at: block, previous: previous?[index])
    }
  }

  /// Merges the states for one variable. `incoming` contains the state of the variable for each predecessor,
  /// and `previous` contains the previously computed state, if it has been computed.
  private func merge(incoming: [ReachingDebugValue], at block: BasicBlock,
                     previous: ReachingDebugValue?) -> ReachingDebugValue {
    // A previously undescribed variable cannot become described.
    if case .undescribed? = previous {
      return .undescribed
    }
    // A variable undescribed on any path is undescribed.
    if incoming.contains(where: { areStatesEquivalent($0, .undescribed) }) {
      return .undescribed
    }
    // Collect each incoming debug_value, deduplicating identical ones.
    var debugValues = [DebugValueInst]()
    var collectedIDs = Set<DebugValueID>()
    for value in incoming {
      for debugValue in value.debugValues where collectedIDs.insert(id(of: debugValue)).inserted {
        debugValues.append(debugValue)
      }
    }
    // A join at this block stays a join, so that the dataflow terminates.
    if case .join(block, _)? = previous {
      return .join(block, debugValues: debugValues)
    }
    let first = incoming[0]
    // If the incoming states differ, there is a join at this block.
    if !incoming.allSatisfy({ areStatesEquivalent($0, first) }) {
      return .join(block, debugValues: debugValues)
    }
    // `areStatesEquivalent` allows debugValues in a join to differ, unite them.
    if case .join(let joinBlock, _) = first {
      return .join(joinBlock, debugValues: debugValues)
    }
    return first
  }

  /// Walks the instructions of `block` in the region, updating the state, and returns the state at
  /// its end. Calls `body` for each instruction, with the state before it.
  private func walk(block: BasicBlock, from entryState: [ReachingDebugValue],
                    body: (Instruction, [ReachingDebugValue]) -> ()) -> [ReachingDebugValue] {
    var state = entryState
    for inst in instructions(of: block) {
      body(inst, state)
      if let debugValue = inst as? DebugValueInst, let info = debugValueInfo[debugValue] {
        state[info.variableIndex] = .debugValue(debugValue)
      } else {
        client.transfer(after: inst, states: &state)
      }
    }
    return state
  }

  /// The instructions of `block` in the region. Instructions before `begin` are not included.
  private func instructions(of block: BasicBlock) -> InstructionList {
    InstructionList(first: block == begin.parentBlock ? begin : block.instructions.first)
  }

  private var undescribedState: [ReachingDebugValue] {
    [ReachingDebugValue](repeating: .undescribed, count: variables.count)
  }

  private func id(of debugValue: DebugValueInst) -> DebugValueID {
    debugValueInfo[debugValue]!.id
  }

  /// True if both describe the variable the same way: joins at the same block are equivalent, even
  /// if they have collected different debug values so far.
  private func areStatesEquivalent(_ lhs: ReachingDebugValue, _ rhs: ReachingDebugValue) -> Bool {
    switch (lhs, rhs) {
    case (.debugValue(let lhsDebugValue), .debugValue(let rhsDebugValue)):
      return id(of: lhsDebugValue) == id(of: rhsDebugValue)
    case (.undescribed, .undescribed):
      return true
    case (.join(let lhsBlock, _), .join(let rhsBlock, _)):
      return lhsBlock == rhsBlock
    default:
      return false
    }
  }

  /// True if both are equivalent and, for joins, have the same debug values.
  private func areStatesIdentical(_ lhs: ReachingDebugValue, _ rhs: ReachingDebugValue) -> Bool {
    guard areStatesEquivalent(lhs, rhs) else {
      return false
    }
    let lhsDebugValues = lhs.debugValues
    let rhsDebugValues = rhs.debugValues
    return lhsDebugValues.count == rhsDebugValues.count &&
           Set(lhsDebugValues.map { id(of: $0) }) == Set(rhsDebugValues.map { id(of: $0) })
  }

  private func areStatesIdentical(_ lhs: [ReachingDebugValue], _ rhs: [ReachingDebugValue]) -> Bool {
    zip(lhs, rhs).allSatisfy { areStatesIdentical($0, $1) }
  }
}

/// An identifier for a `debug_value` instruction. Identical debug values have the same identifier.
private struct DebugValueID: Hashable {
  let value: Int
}

/// The key which gives identical debug values without a reconstruction block the same `DebugValueID`.
private struct DebugValueIDKey: Hashable {
  let variableIndex: Int
  let operand: HashableValue
}

private extension ReachingDebugValue {
  var debugValues: [DebugValueInst] {
    switch self {
    case .debugValue(let debugValue):
      return [debugValue]
    case .undescribed:
      return []
    case .join(_, let debugValues):
      return debugValues
    }
  }
}
