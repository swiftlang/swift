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

// Visit the introducers of a forwarded lifetime (Value -> LifetimeIntroducer).
//
// A lifetime introducer produces an initial OSSA lifetime which may be extended by forwarding instructions. The introducer is never itself the result of a ForwardingInstruction. Example:
//
//   # lifetime introducer
//   %1 = apply               -+                  -+
//   ...                       | OSSA lifetime     |
//   # forwarding instruction  |                   |
//   %2 = struct $S (%1)      -+ -+                | forward-extended lifetime
//                                | OSSA lifetime  |
//   # non-forwarding consumer    |                |
//   destroy_value %2            -+               -+
//
// The lifetime of a single owned value ends when it is forwarded, but certain lifetime properties are relevant for the entire forward-extended lifetime. For example, if an owned lifetime has a pointer-escaping use, then all values in the forward-extended lifetime are also considered pointer-escaping. Certain propeties, like lexical lifetimes, only exist on the forward introducer and apply to all forwarded values.
//
// Note: Although move_value conceptually forwards an owned value, it also summarizes lifetime attributes; therefore, it is not formally a ForwardingInstruction.
//
// TODO: when phi lifetime flags are implemented, phis will introduce a lifetime in the same way as move_value.
//
// The lifetime introducer of a guaranteed value is the borrow introducer:
//
//   # lifetime introducer / borrow introducer
//   %1 = begin_borrow        -+
//   ...                       | OSSA lifetime == forwarded lifetime
//   # forwarding instruction  |
//   %2 = struct $S (%1)       | - forwarded uses are within the OSSA lifetime
//                             |
//   end_borrow %1            -+
//
// TODO: When a begin_borrow has no lifetime flags, it can be ignored as a lifetime introducer. In that case, an owned value may introduce guaranteed OSSA lifetimes.
//
// This walker is used to query basic lifetime attributes on values, such as "escaping" or "lexical". It must be precise for correctness and is performance critical.
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

// This conveniently gathers all forward introducers and deinitializes visitedValues before the caller has a chance to recurse.
func gatherLifetimeIntroducers(for value: Value, _ context: Context) -> [Value] {
  var gather = GatherLifetimeIntroducers(context)
  defer { gather.visitedValues.deinitialize() }
  let result = gather.walkUp(value: value)
  assert(result == .continueWalk)
  return gather.introducers
}

private struct GatherLifetimeIntroducers : ForwardingUseDefWalker {
  var visitedValues: ValueSet
  var introducers: [Value] = []
  
  init(_ context: Context) {
    self.visitedValues = ValueSet(context)
  }
  
  mutating func needWalk(for value: Value) -> Bool {
    visitedValues.insert(value)
  }
  
  mutating func introducer(_ value: Value) -> WalkResult {
    introducers.append(value)
    return .continueWalk
  }
}

// Visit all the uses in a forward-extended lifetime (LifetimeIntroducer -> Operand).
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
    for result in inst.forwardedResults {
      if walkDownUses(of: result) == .abortWalk {
        return .abortWalk
      }
    }
    return .continueWalk
  }
}

// This conveniently allows a closure to be called for each leaf use of a forward-extended lifetime. It should be called on a forward introducer provided by ForwardingDefUseWalker.introducer() or gatherLifetimeIntroducers().
//
// TODO: make the visitor non-escaping once Swift supports stored non-escaping closues.
func visitForwardedUses(introducer: Value,
  visitor: @escaping (Operand) -> WalkResult,
  _ context: Context)
-> WalkResult {
  var useVisitor = VisitForwardedUses(visitor: visitor, context)
  defer { useVisitor.visitedValues.deinitialize() }
  return useVisitor.walkDownUses(of: introducer)
}

private struct VisitForwardedUses : ForwardingDefUseWalker {
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
