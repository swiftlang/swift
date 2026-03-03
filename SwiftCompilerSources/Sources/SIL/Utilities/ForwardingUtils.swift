//===--- ForwardingUtils.swift - Utilities for ownership forwarding -------===//
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
///
/// TODO: Once the hasPointerEscape flags are implemented for
/// BeginBorrowInst, MoveValueInst, and Allocation,
/// ForwardingUseDefWalker should be used to check whether any value
/// is part of a forward-extended lifetime that has a pointer escape.
//===----------------------------------------------------------------------===//

private let verbose = false

private func log(_ message: @autoclosure () -> String) {
  if verbose {
    print("### \(message())")
  }
}

/// Return true if any use in `value`s forward-extend lifetime has
/// .pointerEscape operand ownership.
///
/// TODO: the C++ implementation does not yet gather all
/// forward-extended lifetime introducers.
///
/// TODO: Add [pointer_escape] flags to MoveValue, BeginBorrow, and
/// Allocation. Then add a `Value.hasPointerEscapingUse` property that
/// performs the use-def walk to pickup the flags. Then only call into
/// this def-use walk to initially set the flags.
public func findPointerEscapingUse(of value: Value) -> Bool {
  value.bridged.findPointerEscape()
}

/// Visit the introducers of a forwarded lifetime (Value -> LifetimeIntroducer).
///
/// A lifetime introducer produces an initial OSSA lifetime which may
/// be extended by forwarding instructions. The introducer is never
/// itself the result of a ForwardingInstruction. Example:
///
///   # lifetime introducer
///   %1 = apply               -+                  -+
///   ...                       | OSSA lifetime     |
///   # forwarding instruction  |                   |
///   %2 = struct $S (%1)      -+ -+                | forward-extended lifetime
///                                | OSSA lifetime  |
///   # non-forwarding consumer    |                |
///   destroy_value %2            -+               -+
///
/// The lifetime of a single owned value ends when it is forwarded,
/// but certain lifetime properties are relevant for the entire
/// forward-extended lifetime. For example, if an owned lifetime has a
/// pointer-escaping use, then all values in the forward-extended
/// lifetime are also considered pointer-escaping. Certain properties,
/// like lexical lifetimes, only exist on the forward introducer and
/// apply to all forwarded values.
///
/// Note: Although move_value conceptually forwards an owned value, it
/// also summarizes lifetime attributes; therefore, it is not formally
/// a ForwardingInstruction.
///
/// The lifetime introducer of a guaranteed value is the borrow introducer:
///
///   # lifetime introducer / borrow introducer
///   %1 = begin_borrow        -+
///   ...                       | OSSA lifetime == forwarded lifetime
///   # forwarding instruction  |
///   %2 = struct $S (%1)       | - forwarded uses are within the OSSA lifetime
///                             |
///   end_borrow %1            -+
///
/// TODO: When a begin_borrow has no lifetime flags, it can be ignored
/// as a lifetime introducer. In that case, an owned value may
/// introduce guaranteed OSSA lifetimes.
///
/// Forwarded lifetimes also extend through phis. In this case,
/// however, there is no ForwardingInstruction.
///
///   # lifetime introducer
///   %1 = apply             -+               -+
///   ...                     | OSSA lifetime  |
///   # phi operand           |                |
///   br bbContinue(%1: $S)  -+                | forward-extended lifetime
///                                            |
///   bbContinue(%phi : $S): -+ OSSA lifetime  |
///   ...                     |                |
///   destroy_value %phi     -+               -+
///
/// TODO: when phi lifetime flags are implemented, phis will introduce
/// a lifetime in the same way as move_value.
///
/// This walker is used to query basic lifetime attributes on values,
/// such as "escaping" or "lexical". It must be precise for
/// correctness and is performance critical.
public protocol ForwardingUseDefWalker {
  associatedtype PathContext

  mutating func introducer(_ value: Value, _ path: PathContext) -> WalkResult

  // Minimally, check a ValueSet. This walker may traverse chains of
  // aggregation and destructuring along with phis.
  mutating func needWalk(for value: Value, _ path: PathContext) -> Bool

  mutating func walkUp(value: Value, _ path: PathContext) -> WalkResult
}

extension ForwardingUseDefWalker {
  public mutating func walkUp(value: Value, _ path: PathContext) -> WalkResult {
    walkUpDefault(forwarded: value, path)
  }
  public mutating func walkUpDefault(forwarded value: Value, _ path: PathContext)
    -> WalkResult {
    if let inst = value.forwardingInstruction, !inst.forwardedOperands.isEmpty {
      return walkUp(instruction: inst, path)
    }
    if let phi = Phi(value) {
      return walkUp(phi: phi, path)
    }
    return introducer(value, path)
  }
  public mutating func walkUp(instruction: ForwardingInstruction, _ path: PathContext)
    -> WalkResult {
    for operand in instruction.forwardedOperands {
      if needWalk(for: operand.value, path) {
        if walkUp(value: operand.value, path) == .abortWalk {
          return .abortWalk
        }
      }
    }
    return .continueWalk
  }
  public mutating func walkUp(phi: Phi, _ path: PathContext) -> WalkResult {
    for operand in phi.incomingOperands {
      if needWalk(for: operand.value, path) {
        if walkUp(value: operand.value, path) == .abortWalk {
          return .abortWalk
        }
      }
    }
    return .continueWalk
  }
}

// This conveniently gathers all forward introducers and deinitializes
// visitedValues before the caller has a chance to recurse.
public func gatherLifetimeIntroducers(for value: Value, _ context: Context) -> [Value] {
  var introducers: [Value] = []
  var walker = VisitLifetimeIntroducers(context) {
    introducers.append($0)
    return .continueWalk
  }
  defer { walker.deinitialize() }
  _ = walker.walkUp(value: value, ())
  return introducers
}

// TODO: visitor can be nonescaping when we have borrowed properties.
public func visitLifetimeIntroducers(for value: Value, _ context: Context,
                                     visitor: @escaping (Value) -> WalkResult)
  -> WalkResult {
  var walker = VisitLifetimeIntroducers(context, visitor: visitor)
  defer { walker.visitedValues.deinitialize() }
  return walker.walkUp(value: value, ())
}

private struct VisitLifetimeIntroducers : ForwardingUseDefWalker {
  var visitor: (Value) -> WalkResult
  var visitedValues: ValueSet
  
  init(_ context: Context, visitor: @escaping (Value) -> WalkResult) {
    self.visitor = visitor
    self.visitedValues = ValueSet(context)
  }
  
  mutating func deinitialize() { visitedValues.deinitialize() }

  mutating func needWalk(for value: Value, _: Void) -> Bool {
    visitedValues.insert(value)
  }

  mutating func introducer(_ value: Value, _: Void) -> WalkResult {
    visitor(value)
  }
}

public enum ForwardingUseResult: CustomStringConvertible {
  case operand(Operand)
  case deadValue(Value, Operand?)

  public var description: String {
    switch self {
    case .operand(let operand):
      return operand.description
    case .deadValue(let deadValue, let operand):
      var desc = "dead value: \(deadValue.description)"
      if let operand = operand {
        desc += "from: \(operand)"
      }
      return desc
    }
  }
}

/// Visit all the non-forwarding uses in a forward-extended lifetime
/// (LifetimeIntroducer -> Operand).
///
/// Minimal requirements:
///   needWalk(for value: Value) -> Bool
///   nonForwardingUse(of operand: Operand) -> WalkResult
///   deadValue(_ value: Value, using operand: Operand?) -> WalkResult
///
/// Start walking:
///   walkDown(root: Value)
///
public protocol ForwardingDefUseWalker {
  /// Minimally, check a ValueSet. This walker may traverse chains of
  /// aggregation and destructuring by default. Implementations may
  /// handle phis.
  mutating func needWalk(for value: Value) -> Bool

  /// A nonForwarding use does not forward ownership, but may
  /// propagate the lifetime in other ways, such as an interior
  /// pointer.
  mutating func nonForwardingUse(of operand: Operand) -> WalkResult

  /// Report any initial or forwarded value with no uses. Only relevant for
  /// guaranteed values or incomplete OSSA. This could be a dead
  /// instruction, a terminator in which the result is dead on one
  /// path, or a dead phi.
  ///
  /// \p operand is nil if \p value is the root.
  mutating func deadValue(_ value: Value, using operand: Operand?) -> WalkResult

  /// This is called for every forwarded value. If the root was an
  /// owned value, then this identifies all OSSA lifetimes in the
  /// forward-extendd lifetime.
  mutating func walkDownUses(of: Value, using: Operand?) -> WalkResult
    
  mutating func walkDown(operand: Operand) -> WalkResult
}

extension ForwardingDefUseWalker {
  /// Start walking
  public mutating func walkDown(root: Value) -> WalkResult {
    walkDownUses(of: root, using: nil)
  }

  public mutating func walkDownUses(of value: Value, using operand: Operand?)
    -> WalkResult {
    return walkDownUsesDefault(forwarding: value, using: operand)
  }

  public mutating func walkDownUsesDefault(forwarding value: Value,
    using operand: Operand?)
  -> WalkResult {
    if !needWalk(for: value) {
      return .continueWalk
    }
    var hasUse = false
    for use in value.uses where !use.isTypeDependent {
      if walkDown(operand: use) == .abortWalk {
        return .abortWalk
      }
      hasUse = true
    }
    if !hasUse {
      return deadValue(value, using: operand)
    }
    return .continueWalk
  }

  public mutating func walkDown(operand: Operand) -> WalkResult {
    walkDownDefault(forwarding: operand)
  }

  public mutating func walkDownDefault(forwarding operand: Operand) -> WalkResult {
    if let inst = operand.instruction as? ForwardingInstruction {
      let singleOper = inst.singleForwardedOperand
      if singleOper == nil || singleOper! == operand {
        return inst.forwardedResults.walk {
          walkDownUses(of: $0, using: operand)
        }
      }
    }
    if let phi = Phi(using: operand) {
      return walkDownUses(of: phi.value, using: operand)
    }
    return nonForwardingUse(of: operand)
  }
}

/// This conveniently allows a closure to be called for each leaf use
/// of a forward-extended lifetime. It should be called on a forward
/// introducer provided by ForwardingDefUseWalker.introducer() or
/// gatherLifetimeIntroducers().
///
/// TODO: make the visitor non-escaping once Swift supports stored
/// non-escaping closures.
public func visitForwardedUses(introducer: Value, _ context: Context,
  visitor: @escaping (ForwardingUseResult) -> WalkResult)
-> WalkResult {
  var useVisitor = VisitForwardedUses(visitor: visitor, context)
  defer { useVisitor.visitedValues.deinitialize() }
  return useVisitor.walkDown(root: introducer)
}

private struct VisitForwardedUses : ForwardingDefUseWalker {
  var visitedValues: ValueSet
  var visitor: (ForwardingUseResult) -> WalkResult
  
  init(visitor: @escaping (ForwardingUseResult) -> WalkResult,
    _ context: Context) {
    self.visitedValues = ValueSet(context)
    self.visitor = visitor
  }

  mutating func needWalk(for value: Value) -> Bool {
    visitedValues.insert(value)
  }
  
  mutating func nonForwardingUse(of operand: Operand) -> WalkResult {
    return visitor(.operand(operand))
  }

  mutating func deadValue(_ value: Value, using operand: Operand?)
  -> WalkResult {
    return visitor(.deadValue(value, operand))
  }
}

/// Walk all uses of partial_apply [on_stack] that may propagate the closure context. Gather each FullApplySite that
/// either invokes this closure as its callee, or passes the closure as an argument to another function.
///
/// Start walk:
///   walkDown(closure:)
///
/// This is a subset of the functionality in LifetimeDependenceDefUseWalker, but significantly simpler. This avoids
/// traversing lifetime dependencies that do not propagate context. For example, a mark_dependence on a closure extends
/// its lifetime but cannot introduce any new uses of the closure context.
public struct NonEscapingClosureDefUseWalker {
  let context: Context
  var visitedValues: ValueSet
  public var applyOperandStack: Stack<Operand>

  /// `visitor` takes an operand whose instruction is always a FullApplySite.
  public init(_ context: Context) {
    self.context = context
    self.visitedValues = ValueSet(context)
    self.applyOperandStack = Stack(context)
  }

  public mutating func deinitialize() {
    visitedValues.deinitialize()
    applyOperandStack.deinitialize()
  }

  public mutating func walkDown(closure: PartialApplyInst) -> WalkResult {
    assert(!closure.mayEscape)
    return walkDownUses(of: closure, using: nil)
  }

  public mutating func closureContextLeafUse(of operand: Operand) -> WalkResult {
    switch operand.instruction {
    case is FullApplySite:
      applyOperandStack.push(operand)
      return .continueWalk
    case is MarkDependenceInst, is FixLifetimeInst, is DestroyValueInst:
      return .continueWalk
    default:
      if operand.instruction.isIncidentalUse {
        return .continueWalk
      }
      log(">>> Unexpected closure use \(operand)")
      // Escaping or unexpected closure use. Expected escaping uses include ReturnInst with a lifetime-dependent result.
      //
      // TODO: Check in the SIL verifier that all uses are expected.
      return .abortWalk
    }
  }
}

extension NonEscapingClosureDefUseWalker: ForwardingDefUseWalker {
  public mutating func needWalk(for value: Value) -> Bool {
    visitedValues.insert(value)
  }

  public mutating func nonForwardingUse(of operand: Operand) -> WalkResult {
    // Nonescaping closures may be moved, copied, or borrowed.
    switch operand.instruction {
    case let transition as OwnershipTransitionInstruction:
      return walkDownUses(of: transition.ownershipResult, using: operand)
    case let convert as ConvertEscapeToNoEscapeInst:
      return walkDownUses(of: convert, using: operand)
    default:
      // Otherwise, assume the use cannot propagate the closure context.
      return closureContextLeafUse(of: operand)
    }
  }

  public mutating func deadValue(_ value: Value, using operand: Operand?) -> WalkResult {
    return .continueWalk
  }
}

let forwardingUseDefTest = Test("forwarding_use_def_test") {
  function, arguments, context in
  let value = arguments.takeValue()
  for introducer in gatherLifetimeIntroducers(for: value, context) {
    print("INTRODUCER: \(introducer)")
  }
}

let forwardingDefUseTest = Test("forwarding_def_use_test") {
  function, arguments, context in
  let value = arguments.takeValue()
  _ = visitForwardedUses(introducer: value, context) { useResult in
    print("USE: \(useResult)")
    return .continueWalk
  }
}
