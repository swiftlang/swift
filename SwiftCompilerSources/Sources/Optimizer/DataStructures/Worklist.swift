//===--- Worklist.swift ---------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2023 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import SIL

/// A utility for processing entities in a worklist.
///
/// A `Worklist` is basically a combination of a stack and a set.
/// It can be used for typical worklist-processing algorithms.
///
/// This type should be a move-only type, but unfortunately we don't have move-only
/// types yet. Therefore it's needed to call `deinitialize()` explicitly to
/// destruct this data structure, e.g. in a `defer {}` block.
struct Worklist<Set: IntrusiveSet> : CustomStringConvertible, NoReflectionChildren {
  typealias Element = Set.Element
  private var worklist: Stack<Element>
  private var pushedElements: Set

  init(_ context: some Context) {
    self.worklist = Stack(context)
    self.pushedElements = Set(context)
  }
  
  mutating func pop() -> Element? { return worklist.pop() }

  /// Pop and allow the popped element to be pushed again to the worklist.
  mutating func popAndForget() -> Element? {
    if let element = worklist.pop() {
      pushedElements.erase(element)
      return element
    }
    return nil
  }

  mutating func pushIfNotVisited(_ element: Element) {
    if pushedElements.insert(element) {
      worklist.append(element)
    }
  }

  mutating func pushIfNotVisited<S: Sequence>(contentsOf other: S) where S.Element == Element {
    for element in other {
      pushIfNotVisited(element)
    }
  }

  /// Returns true if \p element was pushed to the worklist, regardless if it's already popped or not.
  func hasBeenPushed(_ element: Element) -> Bool { pushedElements.contains(element) }

  var isEmpty: Bool { worklist.isEmpty }

  var description: String {
    """
    worklist: \(worklist)
    pushed:   \(pushedElements)
    """
  }

  /// TODO: once we have move-only types, make this a real deinit.
  mutating func deinitialize() {
    pushedElements.deinitialize()
    worklist.deinitialize()
  }
}

typealias BasicBlockWorklist = Worklist<BasicBlockSet>
typealias InstructionWorklist = Worklist<InstructionSet>
typealias SpecificInstructionWorklist<InstType: Instruction> = Worklist<SpecificInstructionSet<InstType>>
typealias ValueWorklist = Worklist<ValueSet>
typealias OperandWorklist = Worklist<OperandSet>

extension InstructionWorklist {
  mutating func pushPredecessors(of inst: Instruction, ignoring ignoreInst: Instruction) {
    if let prev = inst.previous {
      if prev != ignoreInst {
        pushIfNotVisited(prev)
      }
    } else {
      for predBlock in inst.parentBlock.predecessors {
        let termInst = predBlock.terminator
        if termInst != ignoreInst {
          pushIfNotVisited(termInst)
        }
      }
    }
  }

  mutating func pushSuccessors(of inst: Instruction, ignoring ignoreInst: Instruction) {
    if let succ = inst.next {
      if succ != ignoreInst {
        pushIfNotVisited(succ)
      }
    } else {
      for succBlock in inst.parentBlock.successors {
        let firstInst = succBlock.instructions.first!
        if firstInst != ignoreInst {
          pushIfNotVisited(firstInst)
        }
      }
    }
  }
}

/// A worklist for `Function`s.
struct FunctionWorklist {

  // The current functions in the worklist.
  private(set) var functions = Array<Function>()

  // All functions which were ever pushed to the worklist.
  private var pushedFunctions = Set<Function>()

  mutating func pushIfNotVisited(_ function: Function) {
    if pushedFunctions.insert(function).inserted {
      functions.append(function)
    }
  }

  mutating func pushIfNotVisited<S: Sequence>(contentsOf functions: S) where S.Element == Function {
    for f in functions {
      pushIfNotVisited(f)
    }
  }

  mutating func pop() -> Function? {
    return functions.popLast()
  }
}

/// Like `ValueWorklist`, but allows pushing `Value`s from different functions -
/// at the cost of a less efficient implementation.
struct CrossFunctionValueWorklist {

  // The current values in the worklist.
  private(set) var values = Array<Value>()

  // All values which were ever pushed to the worklist.
  private var pushedValues = Set<ObjectIdentifier>(minimumCapacity: 8)

  init() {
    values.reserveCapacity(8)
  }

  mutating func pop() -> Value? {
    return values.popLast()
  }

  mutating func pushIfNotVisited(_ value: Value) {
    if pushedValues.insert(ObjectIdentifier(value)).inserted {
      values.append(value)
    }
  }

  mutating func pushIfNotVisited<S: Sequence>(contentsOf values: S) where S.Element == Value {
    for value in values {
      pushIfNotVisited(value)
    }
  }

  func hasBeenPushed(_ value: Value) -> Bool {
    return pushedValues.contains(ObjectIdentifier(value))
  }
}
