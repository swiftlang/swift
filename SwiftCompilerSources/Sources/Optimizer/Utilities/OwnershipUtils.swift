//===--- OwnershipUtils.swift - Utilities for ownership -------------------===//
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

// Visited the introducers of a forwarded lifetime.
//
// This is used to query lifetime attributes on values, such as "escaping" or "lexical". It must be precise for correctness and is performance critical.
//
// TODO: Ideally, this would be defined alongside ForwardingInstruction, but basic data structures and utilities are not available in the SIL module.
protocol ForwardingUseDefWalker {
  mutating func introducer(_ value: Value) -> WalkResult

  // Minimally, check a ValueSet. This walker may traverse chains of aggregation and destructuring by default. Implementations may traverse phis.
  mutating func needWalk(for value: Value) -> Bool

  mutating func walkUp(value: Value) -> WalkResult
}

extension ForwardingUseDefWalker {
  mutating func walkUp(value: Value) -> WalkResult {
    walkUpDefault(value: value)
  }
  mutating func walkUpDefault(value: Value) -> WalkResult {
    if let inst = value.definingInstruction as? ForwardingInstruction
    {
      return walkUp(instruction: inst)
    }
    return introducer(value)
  }
  mutating func walkUp(instruction: ForwardingInstruction) -> WalkResult {
    for operand in instruction.forwardedOperands {
      if needWalk(for: operand.value) {
        if walkUp(value: operand.value) == .abortWalk {
          return .abortWalk
        }
      }
    }
    return .continueWalk
  }
}

// The underlying value is either an operation that summarizes lifetime attributes, such as a phi or move_value, or it's lifetime has no special attributes and can be freely optimized.
//
// TODO: put lifetime queries (escaping, lexical) here.
struct ForwardingIntroducer {
  var value: Value
}

// Value -> ForwardingIntroducer
extension ForwardingIntroducer {
  private struct GatherForwardingIntroducers : ForwardingUseDefWalker {
    var visitedValues: ValueSet
    var introducers: [ForwardingIntroducer] = []
    
    init(_ context: Context) {
      self.visitedValues = ValueSet(context)
    }
    
    mutating func needWalk(for value: Value) -> Bool {
      visitedValues.insert(value)
    }
    
    mutating func introducer(_ value: Value) -> WalkResult {
      introducers.append(ForwardingIntroducer(value: value))
      return .continueWalk
    }
  }
  
  // Gather all introducers and deinitialize visitedValues before the caller has a chance to recurse.
  static func gather(for value: Value, _ context: Context) -> [ForwardingIntroducer] {
    var gather = GatherForwardingIntroducers(context)
    defer { gather.visitedValues.deinitialize() }
    let result = gather.walkUp(value: value)
    assert(result == .continueWalk)
    return gather.introducers
  }
}

// ForwardingIntroducer -> Value
protocol ForwardingDefUseWalker {
  // Minimally, check a ValueSet. This walker may traverse chains of aggregation and destructuring by default. Implementations may handle phis.
  mutating func needWalk(for value: Value) -> Bool

  mutating func leafUse(_ operand: Operand) -> WalkResult
  
  mutating func walkDownUses(of value: Value) -> WalkResult
    
  mutating func walkDown(operand: Operand) -> WalkResult
}

extension ForwardingDefUseWalker {
  mutating func walkDownUses(of value: Value) -> WalkResult {
    return walkDownUsesDefault(of: value)
  }

  mutating func walkDownUsesDefault(of value: Value) -> WalkResult {
    if !needWalk(for: value) { return .continueWalk }

    for operand in value.uses where !operand.isTypeDependent {
      if walkDown(operand: operand) == .abortWalk {
        return .abortWalk
      }
    }
    return .continueWalk
  }

  mutating func walkDown(operand: Operand) -> WalkResult {
    walkDownDefault(operand: operand)
  }

  mutating func walkDownDefault(operand: Operand) -> WalkResult {
    if let inst = operand.instruction as? ForwardingInstruction {
      return walkDownAllResults(of: inst)
    }
    return leafUse(operand)
  }

  private mutating func walkDownAllResults(of inst: ForwardingInstruction)
  -> WalkResult {
    let result = inst.visitForwardedResults {
      walkDownUses(of: $0) == .continueWalk
    }
    return result ? .continueWalk : .abortWalk
  }
}

extension ForwardingIntroducer {
  private struct UseVisitor : ForwardingDefUseWalker {
    var visitedValues: ValueSet
    var visitor: (Operand) -> WalkResult
    
    init(visitor: @escaping (Operand) -> WalkResult, _ context: Context) {
      self.visitedValues = ValueSet(context)
      self.visitor = visitor
    }

    mutating func needWalk(for value: Value) -> Bool {
      visitedValues.insert(value)
    }
    
    mutating func leafUse(_ operand: Operand) -> WalkResult {
      return visitor(operand)
    }
  }

  // TODO: make the visitor non-escaping once Swift supports stored
  // non-escaping closues.
  func visitForwardedUses(visitor: @escaping (Operand) -> WalkResult,
                          _ context: Context)
  -> WalkResult {
    var useVisitor = UseVisitor(visitor: visitor, context)
    defer { useVisitor.visitedValues.deinitialize() }
    return useVisitor.walkDownUses(of: value)
  }
}
