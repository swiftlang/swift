//===--- RedundantOverflowCheckRemoval.swift ------------------------------===//
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

/// Removes overflow checks (`cond_fail`s of `*_with_overflow` builtins) that are guarded by
/// control flow or by other overflow checks.
///
/// The pass collects range constraints from conditional branches and from overflow checks it
/// cannot remove, and drops a `cond_fail` whenever a dominating constraint proves the operation
/// can't trap, e.g.
///
///   if x > 2 {
///     _ = x - 2   // the `x > 2` branch proves `x - 2` can't underflow
///   }
///
let redundantOverflowCheckRemoval = FunctionPass(name: "remove-redundant-overflow-checks") {
  (function: Function, context: FunctionPassContext) in

  var remover = OverflowCheckRemover(function, context)
  remover.run()
}

/// A relationship between two values. The arithmetic relations express that the operation is known
/// to not have trapped at that point in the program.
///
///   if x > 2 { x }            -> slt(2, x)
///   if x > 2 {} else { x }    -> sle(x, 2)
///   x - 2                     -> sSub(x, 2)
///   x + y                     -> sAdd(x, y)
private enum ValueRelation {
  case eq, ult, ule, uAdd, uSub, uMul, slt, sle, sAdd, sSub, sMul
}

/// A constraint on the range of some values, valid in all blocks dominated by `dominatingBlock`.
private struct Constraint {
  let dominatingBlock: BasicBlock
  let left: Value
  let right: Value
  let relationship: ValueRelation
}

private struct OverflowCheckRemover {
  let function: Function
  let context: FunctionPassContext
  let domTree: DominatorTree

  /// The value relationships collected so far.
  var constraints: [Constraint] = []
  /// The `cond_fail`s marked for removal.
  var toRemove: [CondFailInst] = []

  init(_ function: Function, _ context: FunctionPassContext) {
    self.function = function
    self.context = context
    self.domTree = context.dominatorTree
  }

  mutating func run() {
    let reversePostOrder = computeReversePostOrder()

    // Forward scan: use control flow and previously seen overflow checks to remove overflow checks.
    // A reverse-post-order walk guarantees a dominating block (and its constraints) is seen before
    // any block it dominates.
    for block in reversePostOrder {
      for inst in block.instructions {
        if let condBranch = inst as? CondBranchInst {
          registerBranchFormula(condBranch)
        }
        if let condFail = inst as? CondFailInst {
          if tryToRemoveCondFail(condFail) {
            toRemove.append(condFail)
            continue
          }
          // Couldn't remove it: use it as a constraint for later checks.
          registerCondFailFormula(condFail)
        }
      }
    }
    var changed = removeCollected()

    // Reverse scan: use future overflow checks that must execute to remove earlier ones. This is
    // block-local because we don't have post-dominators here.
    for block in reversePostOrder {
      constraints.removeAll(keepingCapacity: true)
      for inst in block.instructions.reversed() {
        if let condFail = inst as? CondFailInst {
          if tryToRemoveCondFail(condFail) {
            toRemove.append(condFail)
            continue
          }
          registerCondFailFormula(condFail)
          continue
        }
        // Don't move an overflow check past a side effect: it would delay the trap past a
        // user-visible change.
        if inst.mayHaveSideEffects {
          constraints.removeAll(keepingCapacity: true)
        }
      }
    }
    changed = removeCollected() || changed
    _ = changed
  }

  private mutating func removeCollected() -> Bool {
    if toRemove.isEmpty {
      return false
    }
    for condFail in toRemove {
      context.erase(instruction: condFail)
    }
    toRemove.removeAll(keepingCapacity: true)
    return true
  }

  /// Returns the blocks of `function` in reverse post order.
  private func computeReversePostOrder() -> [BasicBlock] {
    var visited = BasicBlockSet(context)
    defer { visited.deinitialize() }

    var postOrder: [BasicBlock] = []
    var worklist: [(block: BasicBlock, nextSuccessor: Int)] = []

    visited.insert(function.entryBlock)
    worklist.append((function.entryBlock, 0))
    while let top = worklist.last {
      let successors = top.block.successors
      if top.nextSuccessor < successors.count {
        worklist[worklist.count - 1].nextSuccessor += 1
        let successor = successors[top.nextSuccessor]
        if visited.insert(successor) {
          worklist.append((successor, 0))
        }
      } else {
        postOrder.append(top.block)
        worklist.removeLast()
      }
    }
    return postOrder.reversed()
  }

  private mutating func tryToRemoveCondFail(_ condFail: CondFailInst) -> Bool {
    guard let tupleExtract = condFail.condition as? TupleExtractInst,
          let builtin = tupleExtract.tuple as? BuiltinInst
    else {
      return false
    }
    // `x - min(a, x)` can't trap.
    if isRedundantMinSubtraction(builtin, at: condFail.parentBlock) {
      return true
    }
    for constraint in constraints {
      if constraint.dominatingBlock.dominates(condFail.parentBlock, domTree),
         isOverflowCheckRemoved(by: constraint, builtin) {
        return true
      }
    }
    return false
  }

  /// Whether `builtin` computes `x - min(a, x)`, which can't trap: `min(a, x) <= x` keeps the
  /// result in `[0, x]`. A signed subtraction also needs `a >= 0`, else `x - a` can overflow.
  private func isRedundantMinSubtraction(_ builtin: BuiltinInst, at block: BasicBlock) -> Bool {
    let isSigned: Bool
    switch builtin.id {
    case .SSubOver: isSigned = true
    case .USubOver: isSigned = false
    default:        return false
    }
    let x = builtin.operands[0].value
    guard let (a, b) = matchMinDiamond(builtin.operands[1].value, wantSigned: isSigned) else {
      return false
    }
    // The minuend must be one of the compared operands, so that the subtrahend is `<= x`.
    let other: Value
    if x == a {
      other = b
    } else if x == b {
      other = a
    } else {
      return false
    }
    // Unsigned subtraction only traps on underflow, which `min(a, x) <= x` rules out.
    if !isSigned {
      return true
    }
    // Signed: also rule out overflow above.
    return isKnownNonNegative(other, at: block)
  }

  /// Whether `value` is non-negative at `block`: a non-negative literal, or covered by a dominating
  /// `0 <= value` constraint.
  private func isKnownNonNegative(_ value: Value, at block: BasicBlock) -> Bool {
    if let literal = literalValue(value) {
      return literal >= 0
    }
    for constraint in constraints {
      if constraint.relationship == .sle,
         let left = literalValue(constraint.left), left >= 0,
         constraint.right == value,
         constraint.dominatingBlock.dominates(block, domTree) {
        return true
      }
    }
    return false
  }

  private mutating func registerCondFailFormula(_ condFail: CondFailInst) {
    if let tupleExtract = condFail.condition as? TupleExtractInst,
       let builtin = tupleExtract.tuple as? BuiltinInst,
       let relation = arithOpRelation(builtin) {
      constraints.append(Constraint(dominatingBlock: condFail.parentBlock,
                                    left: builtin.operands[0].value,
                                    right: builtin.operands[1].value,
                                    relationship: relation))
    }

    // A `cond_fail` directly on a comparison tells us the comparison is false on the fall-through,
    // e.g. `cond_fail %cmp_ult(x, y)` proves `x >= y` afterwards.
    if let comparison = condFail.condition as? BuiltinInst {
      addComparisonRelation(comparison, trueBlock: nil, falseBlock: comparison.parentBlock)
    }
  }

  private mutating func registerBranchFormula(_ condBranch: CondBranchInst) {
    guard let comparison = condBranch.condition as? BuiltinInst else {
      return
    }
    // Only a block with a single predecessor (and the blocks it dominates) can rely on the branch
    // condition; otherwise it may be reached from elsewhere.
    let trueBlock = condBranch.trueBlock.singlePredecessor != nil ? condBranch.trueBlock : nil
    let falseBlock = condBranch.falseBlock.singlePredecessor != nil ? condBranch.falseBlock : nil
    addComparisonRelation(comparison, trueBlock: trueBlock, falseBlock: falseBlock)
  }

  private mutating func addComparisonRelation(_ comparison: BuiltinInst,
                                              trueBlock: BasicBlock?,
                                              falseBlock: BasicBlock?) {
    // Match the comparison kind before touching the operands: the branch condition can be any
    // builtin (e.g. a unary `trunc`), not just a two-operand comparison.
    let trueRelation: ValueRelation
    let falseRelation: ValueRelation
    var swap = false

    switch comparison.id {
    case .ICMP_NE:
      if let falseBlock {
        constraints.append(Constraint(dominatingBlock: falseBlock,
                                      left: comparison.operands[0].value,
                                      right: comparison.operands[1].value,
                                      relationship: .eq))
      }
      return
    case .ICMP_EQ:
      if let trueBlock {
        constraints.append(Constraint(dominatingBlock: trueBlock,
                                      left: comparison.operands[0].value,
                                      right: comparison.operands[1].value,
                                      relationship: .eq))
      }
      return
    case .ICMP_SLE:
      trueRelation = .sle; falseRelation = .slt
    case .ICMP_SLT:
      trueRelation = .slt; falseRelation = .sle
    case .ICMP_SGE:
      trueRelation = .slt; falseRelation = .sle; swap = true
    case .ICMP_SGT:
      trueRelation = .sle; falseRelation = .slt; swap = true
    case .ICMP_ULE:
      trueRelation = .ule; falseRelation = .ult
    case .ICMP_ULT:
      trueRelation = .ult; falseRelation = .ule
    case .ICMP_UGT:
      trueRelation = .ule; falseRelation = .ult; swap = true
    case .ICMP_UGE:
      trueRelation = .ult; falseRelation = .ule; swap = true
    default:
      return
    }

    let left = comparison.operands[0].value
    let right = comparison.operands[1].value
    let l = swap ? right : left
    let r = swap ? left : right
    if let trueBlock {
      constraints.append(Constraint(dominatingBlock: trueBlock, left: l, right: r,
                                    relationship: trueRelation))
    }
    if let falseBlock {
      constraints.append(Constraint(dominatingBlock: falseBlock, left: r, right: l,
                                    relationship: falseRelation))
    }
  }
}

/// Matches a `min` lowered to a diamond and returns the two compared operands (the merged value is
/// `<=` both). The comparison's signedness must match `wantSigned`, e.g.
///
///   %c = builtin "cmp_[su]l[te]"(%a, %b)
///   cond_br %c, trueBB, falseBB
/// trueBB:  br mergeBB(%a)   // the true edge carries %a, the smaller
/// falseBB: br mergeBB(%b)
/// mergeBB(%min):
///
private func matchMinDiamond(_ value: Value, wantSigned: Bool) -> (Value, Value)? {
  guard let phi = Phi(value) else {
    return nil
  }
  // Exactly two incoming edges, both from a single common conditional block.
  var predecessors = phi.predecessors
  guard let pred0 = predecessors.next(), let pred1 = predecessors.next(),
        predecessors.next() == nil,
        let condBlock = pred0.singlePredecessor, condBlock == pred1.singlePredecessor,
        let condBranch = condBlock.terminator as? CondBranchInst
  else {
    return nil
  }
  let trueBlock = condBranch.trueBlock
  let falseBlock = condBranch.falseBlock
  guard (pred0 == trueBlock && pred1 == falseBlock) || (pred1 == trueBlock && pred0 == falseBlock),
        let comparison = condBranch.condition as? BuiltinInst
  else {
    return nil
  }
  switch comparison.id {
  case .ICMP_SLT, .ICMP_SLE: if !wantSigned { return nil }
  case .ICMP_ULT, .ICMP_ULE: if wantSigned { return nil }
  default: return nil
  }
  // For `min`, the true edge (where op0 <(=) op1) carries op0, the smaller one.
  let op0 = comparison.operands[0].value
  let op1 = comparison.operands[1].value
  if phi.incomingOperand(inPredecessor: trueBlock).value == op0,
     phi.incomingOperand(inPredecessor: falseBlock).value == op1 {
    return (op0, op1)
  }
  return nil
}

/// The arithmetic-overflow builtins and the relation each one expresses when it doesn't trap.
private func arithOpRelation(_ builtin: BuiltinInst) -> ValueRelation? {
  switch builtin.id {
  case .SAddOver: return .sAdd
  case .UAddOver: return .uAdd
  case .SSubOver: return .sSub
  case .USubOver: return .uSub
  case .SMulOver: return .sMul
  case .UMulOver: return .uMul
  default:        return nil
  }
}

/// Whether `constraint` proves the overflow check of `builtin` is unnecessary.
private func isOverflowCheckRemoved(by constraint: Constraint, _ builtin: BuiltinInst) -> Bool {
  let l = constraint.left
  let r = constraint.right

  // The constraint's operands must have the same type as the arithmetic operation.
  switch builtin.id {
  case .SAddOver, .UAddOver, .SMulOver, .UMulOver, .USubOver, .SSubOver:
    if l.type != builtin.operands[0].value.type {
      return false
    }
  default:
    return false
  }

  let a = builtin.operands[0].value
  let b = builtin.operands[1].value

  switch builtin.id {
  case .SAddOver:
    // `l + r` doesn't trap and `l >= a, r >= b` (or commutatively) => `a + b` doesn't trap.
    if constraint.relationship == .sAdd {
      if knownRelation(a, l, .sle) && knownRelation(b, r, .sle) { return true }
      if knownRelation(b, l, .sle) && knownRelation(a, r, .sle) { return true }
    }
    // `a + 1` doesn't trap if `a` is smaller than anything.
    if constraint.relationship == .slt {
      if l == a, isLiteral(b, 1) { return true }
      if l == b, isLiteral(a, 1) { return true }
    }
    // `x + c` (c > 0) bounds-checked `< 0` afterwards: an overflow makes the sum
    // negative, so that check traps anyway.
    if constraint.relationship == .sle, isLiteral(l, 0),
       let sum = r as? TupleExtractInst, sum.fieldIndex == 0, sum.tuple == builtin,
       isKnownPositive(a) || isKnownPositive(b) {
      return true
    }
    return false

  case .UAddOver:
    if constraint.relationship == .uAdd {
      if knownRelation(a, l, .ule) && knownRelation(b, r, .ule) { return true }
      if knownRelation(b, l, .ule) && knownRelation(a, r, .ule) { return true }
    }
    if constraint.relationship == .ult {
      if l == a, isLiteral(b, 1) { return true }
      if l == b, isLiteral(a, 1) { return true }
    }
    return false

  case .SMulOver:
    // `l * r` doesn't trap and (`|a| < |l|` and `b == r`) or (`a == l` and `|b| < |r|`) =>
    // `a * b` doesn't trap. We disallow a pure sign flip (`|l| == |a|`) because `-MIN * -1` traps.
    if constraint.relationship == .sMul {
      if isKnownAbsLess(a, l) && knownRelation(b, r, .eq) { return true }
      if knownRelation(a, l, .eq) && isKnownAbsLess(b, r) { return true }
      if isKnownAbsLess(b, l) && knownRelation(a, r, .eq) { return true }
      if knownRelation(b, l, .eq) && isKnownAbsLess(a, r) { return true }
    }
    return false

  case .UMulOver:
    if constraint.relationship == .uMul {
      if knownRelation(a, l, .ule) && knownRelation(b, r, .ule) { return true }
      if knownRelation(b, l, .ule) && knownRelation(a, r, .ule) { return true }
    }
    return false

  case .USubOver:
    // Given `l < r` (or `l <= r`), `a - b` doesn't trap if `r == a` and `b <= l`.
    if constraint.relationship == .ule || constraint.relationship == .ult {
      if knownRelation(r, a, .eq) && knownRelation(b, l, .ule) { return true }
    }
    if constraint.relationship == .eq {
      if knownRelation(r, b, .eq) && knownRelation(a, l, .ule) { return true }
      if knownRelation(l, b, .eq) && knownRelation(a, r, .ule) { return true }
    }
    if constraint.relationship == .uSub {
      if knownRelation(l, a, .ule) && knownRelation(b, r, .ule) { return true }
    }
    // `a - 1` doesn't trap if `a` is greater than some other number.
    if constraint.relationship == .ult {
      if r == a, isLiteral(b, 1) { return true }
    }
    return false

  case .SSubOver:
    // Given `l < r`, `a - b` doesn't trap if `l` is positive (a double negative can overflow),
    // `r == a` and `b <= l`.
    if constraint.relationship == .sle || constraint.relationship == .slt {
      if isKnownPositive(l) && knownRelation(r, a, .eq) && knownRelation(b, l, .sle) { return true }
    }
    if constraint.relationship == .sSub {
      if knownRelation(l, a, .sle) && knownRelation(b, r, .sle) { return true }
    }
    if constraint.relationship == .eq {
      if knownRelation(r, b, .eq) && knownRelation(a, l, .sle) { return true }
      if knownRelation(l, b, .eq) && knownRelation(a, r, .sle) { return true }
    }
    if constraint.relationship == .slt {
      if r == a, isLiteral(b, 1) { return true }
    }
    return false

  default:
    return false
  }
}

/// Whether `relation` is a known relation between `a` and `b`.
private func knownRelation(_ a: Value, _ b: Value, _ relation: ValueRelation) -> Bool {
  // Identical values are equal and thus also `<=`.
  if a == b, relation == .eq || relation == .sle || relation == .ule {
    return true
  }
  guard let av = literalValue(a), let bv = literalValue(b) else {
    return false
  }
  switch relation {
  case .eq:  return av == bv
  case .sle: return av <= bv
  case .slt: return av < bv
  // Only compare unsigned when both are non-negative `Int`s. A literal with the top bit set
  // could be wrong here, so we bail instead.
  case .ule: return av >= 0 && bv >= 0 && av <= bv
  case .ult: return av >= 0 && bv >= 0 && av < bv
  default:   return false
  }
}

/// Whether `n` is a literal known to be strictly positive.
private func isKnownPositive(_ n: Value) -> Bool {
  if let value = literalValue(n) {
    return value > 0
  }
  return false
}

/// Whether `|a| < |b|`, i.e. `a` is closer to zero than `b`.
private func isKnownAbsLess(_ a: Value, _ b: Value) -> Bool {
  if let av = literalValue(a), let bv = literalValue(b) {
    return av.magnitude < bv.magnitude
  }
  return false
}

private func isLiteral(_ value: Value, _ constant: Int) -> Bool {
  return literalValue(value) == constant
}

private func literalValue(_ value: Value) -> Int? {
  return (value as? IntegerLiteralInst)?.value
}
