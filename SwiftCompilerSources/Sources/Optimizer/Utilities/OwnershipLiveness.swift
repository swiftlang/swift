//===--- OwnershipLiveness.swift - Utilities for ownership liveness -------===//
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
//
// Utilities that specify ownership SSA (OSSA) lifetimes.
//
// TODO: Implement ExtendedLinearLiveness. This requires
// MultiDefPrunedLiveness, which is not supported by InstructionRange.
//
// TODO: Move this all into SIL, along with DominatorTree. OSSA
// lifetimes and dominance are part of SIL semantics, and need to be
// verified. Remove uses of FunctionPassContext.
//
//===----------------------------------------------------------------------===//

import SIL

private let verbose = false

private func log(_ message: @autoclosure () -> String) {
  if verbose {
    print("### \(message())")
  }
}

/// Compute liveness and return a range, which the caller must deinitialize.
///
/// `definingValue` must introduce an OSSA lifetime. It may be either
/// an owned value or introduce a borrowed value (BeginBorrowValue),
/// including:
///
/// 1. Owned non-phi values
/// 2. Owned phi values
/// 3. Borrow scope introducers: begin_borrow/load_borrow
/// 4. Reborrows: guaranteed phi that ends its operand's borrow scope and
///    requires post-dominating scope-ending uses (end_borrow or reborrow)
///
/// `definingValue`'s lifetime must already complete on all paths
/// (a.k.a linear). Only lifetime-ending operations generate liveness.
///
/// `definingValue` dominates the range. Forwarding and phi uses do
/// not extend the lifetime.
///
/// This is the simplest OSSA liveness analysis. It assumes that OSSA
/// lifetime completion has already run on `definingValue`, and it
/// cannot fix OSSA lifetimes after a transformation.
func computeLinearLiveness(for definingValue: Value, _ context: Context)
  -> InstructionRange {

  assert(definingValue.ownership == .owned || BeginBorrowValue(definingValue) != nil,
         "value must define an OSSA lifetime")

  // InstructionRange cannot directly represent the beginning of the block
  // so we fake it with getRepresentativeInstruction().
  var range = InstructionRange(for: definingValue, context)

  // Compute liveness.
 for use in definingValue.lookThroughBorrowedFromUser.uses {
    let instruction = use.instruction
    if use.endsLifetime || instruction is ExtendLifetimeInst {
      range.insert(instruction)
    }
  }
  return range
}

/// Compute known liveness and return a range, which the caller must deinitialize.
///
/// This computes a minimal liveness, ignoring pointer escaping uses.
///
/// The caller must call deinitialize() on the result.
func computeKnownLiveness(for definingValue: Value, visitInnerUses: Bool = false, _ context: FunctionPassContext)
  -> InstructionRange {
  // Ignore pointer escapes and other failures.
  return InteriorLivenessResult.compute(for: definingValue, ignoreEscape: true,
                                        visitInnerUses: visitInnerUses, context).acquireRange
}

/// Compute the live range for the borrow scopes of a guaranteed value. This returns a separate instruction range for
/// each of the value's borrow introducers.
///
/// TODO: This should return a single multiply-defined instruction range.
func computeBorrowLiveRange(for value: Value, _ context: FunctionPassContext)
  -> SingleInlineArray<(BeginBorrowValue, InstructionRange)> {
  assert(value.ownership == .guaranteed)

  var ranges = SingleInlineArray<(BeginBorrowValue, InstructionRange)>()
  // If introducers is empty, then the dependence is on a trivial value, so
  // there is no ownership range.
  for beginBorrow in value.getBorrowIntroducers(context) {
    /// FIXME: Remove calls to computeKnownLiveness() as soon as lifetime completion runs immediately after
    /// SILGen. Instead, this should compute linear liveness for borrowed value by switching over BeginBorrowValue, just
    /// like LifetimeDependence.Scope.computeRange().
    ranges.push((beginBorrow, computeKnownLiveness(for: beginBorrow.value, context)))
  }
  return ranges
}

/// If any interior pointer may escape, then record the first instance here. If 'ignoreEscape' is true, this
/// immediately aborts the walk, so further instances are unavailable.
///
/// .escaping may either be a non-address operand with
/// .pointerEscape ownership, or and address operand that escapes
/// the address (address_to_pointer).
///
/// .unknown is an address operand whose user is unrecognized.
enum InteriorPointerStatus: CustomDebugStringConvertible {
  case nonescaping
  case escaping(SingleInlineArray<Operand>)
  case unknown(Operand)

  mutating func setEscaping(operand: Operand) {
    switch self {
    case .nonescaping:
      self = .escaping(SingleInlineArray(element: operand))
    case let .escaping(oldOperands):
      var newOperands = SingleInlineArray<Operand>()
      newOperands.append(contentsOf: oldOperands)
      newOperands.append(operand)
      self = .escaping(newOperands)
    case .unknown:
      break
    }
  }

  var debugDescription: String {
    switch self {
    case .nonescaping:
      return "No pointer escape"
    case let .escaping(operands):
      return "Pointer escapes: " + operands.map({ "\($0)" }).joined(separator: "\n                 ")
    case let .unknown(operand):
      return "Unknown use: \(operand)"
    }
  }
}

typealias InnerScopeHandler = (Value) -> WalkResult

/// An OSSA lifetime begins with a single "defining" value, which must be owned, or must begin a borrow scope. A
/// complete OSSA lifetime has a linear lifetime, meaning that it has a lifetime-ending use on all paths. Interior
/// liveness computes liveness without assuming the lifetime is complete. To do this, it must find all "use points" and
/// prove that the defining value is never propagated beyond those points. This is used to initially complete OSSA
/// lifetimes and fix them after transformations that's don't preserve OSSA.
///
/// Invariants:
///
/// - The definition dominates all use points (hence the result is a single InstructionRange).
///
/// - Liveness does not extend beyond lifetime-ending operations (a.k.a. affine lifetimes).
///
/// - All inner scopes are complete. (Use `innerScopeHandler` to either complete them or to recursively compute their
/// liveness and either bail-out on or propagate inner pointer escapes outward).
struct InteriorLivenessResult: CustomDebugStringConvertible {
  // 'success' may only be set to .abortWalk if pointerStatus != .nonescaping or visitInnerUses returned false.
  // The client can therefore ensure success if it has already checked for pointer escapes.
  let success: WalkResult
  var range: InstructionRange
  let pointerStatus: InteriorPointerStatus

  /// Compute liveness for a single OSSA value without assuming a complete lifetime.
  ///
  /// The caller must call acquireRange or deinitialize() on the result.
  static func compute(for definingValue: Value, ignoreEscape: Bool = false, visitInnerUses: Bool,
                      _ context: FunctionPassContext,
                      innerScopeHandler: InnerScopeHandler? = nil) -> InteriorLivenessResult {

    assert(definingValue.ownership == .owned || BeginBorrowValue(definingValue) != nil,
           "value must define an OSSA lifetime")

    var range = InstructionRange(for: definingValue, context)

    var visitor = InteriorUseWalker(definingValue: definingValue, ignoreEscape: ignoreEscape,
                                    visitInnerUses: visitInnerUses, context) {
      range.insert($0.instruction)
      return .continueWalk
    }
    defer { visitor.deinitialize() }
    visitor.innerScopeHandler = innerScopeHandler
    let success = visitor.visitUses()
    assert(visitor.unenclosedPhis.isEmpty, "missing adjacent phis")
    let result = InteriorLivenessResult(success: success, range: range, pointerStatus: visitor.pointerStatus)
    log("Interior liveness for: \(definingValue)\n\(result)")
    return result
  }

  /// Client must call deinitialize() on the result.
  var acquireRange: InstructionRange { consuming get { range } }

  mutating func deinitialize() {
    range.deinitialize()
  }

  var debugDescription: String {
    "\(success)\n\(range)\n\(pointerStatus)"
  }
}

/// Classify ownership uses. This reduces operand ownership to a
/// visitor API that can be used by def-use walkers to ensure complete
/// handling of all legal SIL patterns.
///
/// Code that relies on the ownership effect of a use should conform
/// to this visitor. This facilitates correct handling of special cases
/// involving borrow scopes and interior pointers.
///
/// The top-level entry points are:
/// - `classify(operand:)`
/// - `visitOwnershipUses(of:)`
///
/// The implementation may recursively call back to the top-level
/// entry points. Additionally, the implementation may recurse into inner
/// borrow scopes, skipping over the uses within inner scopes using:
/// - `visitInnerBorrowUses(of: BorrowingInstruction, operand:)`
/// - `visitOwnedDependentUses(of: Value)`
///
/// Visitors implement:
///
/// - ownershipLeafUse(of:isInnerlifetime:)
/// - forwardingUse(of:isInnerlifetime:)
/// - interiorPointerUse(of:into:)
/// - pointerEscapingUse(of:)
/// - dependentUse(of:dependentValue:)
/// - dependentUse(of:dependentAddress:)
/// - borrowingUse(of:by:)
///
/// This only visits the first level of uses. The implementation may transitively visit forwarded, borrowed, dependent,
/// or address values in the overrides listed above.
///
/// `isInnerlifetime` indicates whether the value being used is
/// defined by the "outer" OSSA lifetime or an inner borrow scope.
/// When the OwnershipUseVisitor is invoked on an outer value
/// (visitOwnershipUses(of:)), it visits all the uses of that value
/// and also visits the lifetime-ending uses of any inner borrow
/// scopes. This provides a complete set of liveness "use points":
///
///   %0 = begin_borrow %outerValue
///   %1 = begin_borrow %0
///   end_borrow %1        // inner "use point" of %0
///   end_borrow %0        // outer use of %0
///
/// This becomes more complicated with reborrows and closures. The
/// implementation can simply rely on isInnerLifetime to know whether
/// the value being used is part of the outer lifetimes vs. its inner
/// lifetimes. This is important, for example, if the implementation
/// wants to know if the use ends the lifetime of the outer value.
///
/// Visitor implementations treat inner and outer uses differently. It
/// may, for example, assume that inner lifetimes are complete
/// and therefore only care about the lifetime-ending uses.
protocol OwnershipUseVisitor {
  var context: Context { get }

  /// A non-forwarding use.
  ///
  /// `isInnerLifetime` indicates whether `operand` uses the original
  /// OSSA lifetime. This use ends the original lifetime if
  /// (!isInnerLifetime && use.endsLifetime).
  mutating func ownershipLeafUse(of operand: Operand, isInnerLifetime: Bool) -> WalkResult

  /// A forwarding operand.
  ///
  /// Use ForwardingInstruction or ForwardingDefUseWalker to handle
  /// downstream uses.
  ///
  /// If `isInnerLifetime` is true, then the value depends on an inner borrow.
  mutating func forwardingUse(of operand: Operand, isInnerLifetime: Bool) -> WalkResult

  /// A use that projects an address.
  mutating func interiorPointerUse(of: Operand, into address: Value) -> WalkResult

  /// A use that escapes information from its operand's value.
  ///
  /// Note: this may not find all relevant pointer escapes, such as
  /// from owned forwarded values. Clients should generally check
  /// findPointerEscape() before relying on a liveness result and
  /// implement this as a fatalError.
  mutating func pointerEscapingUse(of operand: Operand) -> WalkResult

  /// A use that creates an implicit borrow scope over the lifetime of
  /// an owned dependent value. The operand ownership is .borrow, but
  /// there are no explicit scope-ending operations. Instead
  /// BorrowingInstruction.scopeEndingOperands will return the final
  /// consumes in the dependent value's forwarding chain.
  mutating func dependentUse(of operand: Operand, dependentValue value: Value) -> WalkResult

  /// A use that creates an implicit borrow scope over all reachable uses of a value stored in
  /// `dependentAddress`. This could conservatively be handled has `pointerEscapingUse`, but note that accessing the
  /// `dependentAddress` only keeps the original owner alive, it cannot modify the original (modifying a dependent
  /// address is still just a "read" of the dependence source.
  mutating func dependentUse(of operand: Operand, dependentAddress address: Value) -> WalkResult

  /// A use that is scoped to an inner borrow scope.
  mutating func borrowingUse(of operand: Operand, by borrowInst: BorrowingInstruction) -> WalkResult
}

extension OwnershipUseVisitor {
  /// Classify a non-address type operand, dispatching to one of the
  /// protocol methods below.
  mutating func classify(operand: Operand) -> WalkResult {
    switch operand.value.ownership {
    case .owned:
      return classifyOwned(operand: operand)
    case .guaranteed:
      return classifyGuaranteed(operand: operand)
    case .none, .unowned:
      return .continueWalk
    }
  }

  /// Visit all uses that contribute to the ownership live
  /// range of `value`. This does not assume that `value` has a
  /// complete lifetime, and non-lifetime-ending uses are visited.
  ///
  /// If `value` is a phi (owned or reborrowed), then find its inner
  /// adjacent phis and treat them like inner borrows.
  ///
  /// This is only called for uses in the outer lifetime.
  mutating func visitOwnershipUses(of value: Value) -> WalkResult {
    switch value.ownership {
    case .owned:
      return value.uses.ignoreTypeDependence.walk { classifyOwned(operand: $0) }
    case .guaranteed:
      return value.uses.ignoreTypeDependence.walk {
        classifyGuaranteed(operand: $0) }
    case .none, .unowned:
      return .continueWalk
    }
  }

  /// Handle an owned dependent value, such as a closure capture or owned mark_dependence.
  ///
  /// Called by walkDownUses(of:) for owned values.
  ///
  /// When a borrow introduces an owned value, each OSSA lifetime is effectively a separate borrow scope. A destroy or
  /// consumes ends that borrow scope, while a forwarding consume effectively "reborrows".
  ///   
  ///   %dependent = mark_dependence [nonescaping] %owned on %base // borrow 'owned'
  ///                                                              // visit uses of owned 'dependent' value
  ///   %forwarded = move_value %dependent
  ///   destroy_value %forwarded // ends the inner borrow scope
  ///
  /// Preconditions:
  /// - value.ownership == .owned
  /// - value.type.isEscapable
  mutating func visitOwnedDependentUses(of value: Value) -> WalkResult {
    assert(value.ownership == .owned, "inner value must be a reborrow or owned forward")
    assert(value.type.isEscapable(in: value.parentFunction), "cannot handle non-escapable dependent values")
    return value.uses.endingLifetime.walk {
      switch $0.ownership {
      case .forwardingConsume:
        return forwardingUse(of: $0, isInnerLifetime: true)
      case .destroyingConsume:
        return ownershipLeafUse(of: $0, isInnerLifetime: true)
      default:
        fatalError("invalid owned lifetime ending operand ownership")
      }
    }
  }

  /// Visit uses of borrowing instruction (operandOwnerhip == .borrow),
  /// skipping uses within the borrow scope.
  ///
  ///   %borrow = begin_borrow %def         // visitInnerBorrowUses is called on this BorrowingInstruction
  ///   %address = ref_element_addr %borrow // ignored
  ///   end_borrow %borrow                  // visited as a leaf use of as inner lifetime.
  ///
  mutating func visitInnerBorrowUses(of borrowInst: BorrowingInstruction, operand: Operand) -> WalkResult {
    if let dependent = borrowInst.dependentValue {
      if dependent.ownership == .guaranteed {
        return visitOwnershipUses(of: dependent)
      }
      return pointerEscapingUse(of: operand)
    }
    // Otherwise, directly visit the scope ending uses as leaf uses.
    //
    // TODO: remove this stack by changing visitScopeEndingOperands to take a non-escaping closure.
    var stack = Stack<Operand>(context)
    defer { stack.deinitialize() }
    let result = borrowInst.visitScopeEndingOperands(context) {
      stack.push($0)
      return .continueWalk
    }
    guard result == .continueWalk else {
      // If the dependent value is not scoped then consider it a pointer escape.
      return pointerEscapingUse(of: operand)
    }
    return stack.walk { ownershipLeafUse(of: $0, isInnerLifetime: true) }
  }
}

extension OwnershipUseVisitor {
  // This is only called for uses in the outer lifetime.
  private mutating func classifyOwned(operand: Operand) -> WalkResult {
    switch operand.ownership {
    case .nonUse:
      return .continueWalk

    case .destroyingConsume:
      return ownershipLeafUse(of: operand, isInnerLifetime: false)

    case .forwardingConsume:
      return forwardingUse(of: operand, isInnerLifetime: false)

    case .pointerEscape:
      if let mdai = operand.instruction as? MarkDependenceAddrInst, operand == mdai.baseOperand {
        return dependentUse(of: operand, dependentAddress: mdai.address)
      }
      return pointerEscapingUse(of: operand)

    case .instantaneousUse, .forwardingUnowned, .unownedInstantaneousUse, .bitwiseEscape:
      return ownershipLeafUse(of: operand, isInnerLifetime: false)

    case .borrow:
      return visitBorrowingUse(of: operand)

    case .anyInteriorPointer:
      return visitInteriorPointerUse(of: operand)

    // TODO: .interiorPointer should instead be handled like .anyInteriorPointer.
    case .interiorPointer, .trivialUse, .endBorrow, .reborrow, .guaranteedForwarding:
      fatalError("ownership incompatible with an owned value");
    }
  }

  // This is only called for uses in the outer lifetime.
  private mutating func classifyGuaranteed(operand: Operand)
    -> WalkResult {
    switch operand.ownership {
    case .nonUse:
      return .continueWalk

    case .pointerEscape:
      // TODO: Change ProjectBox ownership to InteriorPointer and allow them to take owned values.
      if operand.instruction is ProjectBoxInst {
        return visitInteriorPointerUse(of: operand)
      }
      if let mdai = operand.instruction as? MarkDependenceAddrInst, operand == mdai.baseOperand {
        return dependentUse(of: operand, dependentAddress: mdai.address)
      }
      return pointerEscapingUse(of: operand)

    case .instantaneousUse, .forwardingUnowned, .unownedInstantaneousUse, .bitwiseEscape, .endBorrow, .reborrow:
      return ownershipLeafUse(of: operand, isInnerLifetime: false)

    case .guaranteedForwarding:
      return forwardingUse(of: operand, isInnerLifetime: false)

    case .borrow:
      return visitBorrowingUse(of: operand)

    case .interiorPointer, .anyInteriorPointer:
      return visitInteriorPointerUse(of: operand)

    case .trivialUse, .forwardingConsume, .destroyingConsume:
      fatalError("ownership incompatible with a guaranteed value")
    }
  }

  private mutating func visitBorrowingUse(of operand: Operand) -> WalkResult {
    switch operand.instruction {
    case let pai as PartialApplyInst:
      assert(!pai.mayEscape)
      return dependentUse(of: operand, dependentValue: pai)
    case let mdi as MarkDependenceInst:
      // .borrow operand ownership only applies to the base operand of a non-escaping markdep that forwards a
      // non-address value.
      assert(operand == mdi.baseOperand && mdi.isNonEscaping)
      return dependentUse(of: operand, dependentValue: mdi)
    case let bfi as BorrowedFromInst where !bfi.borrowedPhi.isReborrow:
      return dependentUse(of: operand, dependentValue: bfi)
    default:
      return borrowingUse(of: operand,
                          by: BorrowingInstruction(operand.instruction)!)
    }
  }

  private mutating func visitInteriorPointerUse(of operand: Operand) -> WalkResult {
    switch operand.instruction {
    case is RefTailAddrInst, is RefElementAddrInst, is ProjectBoxInst,
         is OpenExistentialBoxInst:
      let svi = operand.instruction as! SingleValueInstruction
      return interiorPointerUse(of: operand, into: svi)
    default:
      return pointerEscapingUse(of: operand)
    }
  }
}

/// Visit all interior uses of an OSSA lifetime.
///
/// - `definingValue` dominates all uses. Only dominated phis extend
/// the lifetime. All other phis must have a lifetime-ending outer
/// adjacent phi; otherwise they will be recorded as `unenclosedPhis`.
///
/// - Does not assume the current lifetime is linear. Transitively
/// follows guaranteed forwarding and address uses within the current
/// scope. Phis that are not dominated by definingValue or an outer
/// adjacent phi are marked "unenclosed" to signal an incomplete
/// lifetime.
///
/// - Assumes inner scopes *are* linear, including borrow and address
/// scopes (e.g. begin_borrow, load_borrow, begin_apply, store_borrow,
/// begin_access) A `innerScopeHandler` callback may be used to
/// complete inner scopes before updating liveness.
///
/// InteriorUseWalker can be used to complete (linearize) an OSSA
/// lifetime after transformation that invalidates OSSA.
///
/// Example:
///
///     %s = struct ...
///     %f = struct_extract %s     // defines a guaranteed value (%f)
///     %b = begin_borrow %f
///     %a = ref_element_addr %b
///     _  = address_to_pointer %a
///     end_borrow %b              // the only interior use of %f
///
/// When computing interior liveness for %f, %b is an inner
/// scope. Because inner scopes are complete, the only relevant use is
/// end_borrow %b. Despite the address_to_pointer instruction, %f does
/// not escape any dependent address. 
///
/// TODO: Implement the hasPointerEscape flags on BeginBorrowInst,
/// MoveValueInst, and Allocation. Then this visitor should assert
/// that the forward-extended lifetime introducer has no pointer
/// escaping uses.
struct InteriorUseWalker {
  let functionContext: FunctionPassContext
  var context: Context { functionContext }

  let definingValue: Value
  let ignoreEscape: Bool

  // If true, it's not assumed that inner scopes are linear. It forces to visit
  // all interior uses if inner scopes.
  let visitInnerUses: Bool

  let useVisitor: (Operand) -> WalkResult

  var innerScopeHandler: InnerScopeHandler? = nil

  private func handleInner(borrowed value: Value) -> WalkResult  {
    guard let innerScopeHandler else {
      return .continueWalk
    }
    return innerScopeHandler(value)
  }

  var unenclosedPhis: [Phi] = []

  var function: Function { definingValue.parentFunction }

  var pointerStatus: InteriorPointerStatus = .nonescaping

  private var visited: ValueSet

  mutating func deinitialize() {
    visited.deinitialize()
  }

  init(definingValue: Value, ignoreEscape: Bool, visitInnerUses: Bool, _ context: FunctionPassContext,
    visitor: @escaping (Operand) -> WalkResult) {
    assert(!definingValue.type.isAddress, "address values have no ownership")
    self.functionContext = context
    self.definingValue = definingValue
    self.ignoreEscape = ignoreEscape
    self.visitInnerUses = visitInnerUses
    self.useVisitor = visitor
    self.visited = ValueSet(context)
  }

  mutating func visitUses() -> WalkResult {
    return visitOwnershipUses(of: definingValue)
  }
}

extension InteriorUseWalker: OwnershipUseVisitor {
  /// This is invoked for all non-address uses of the outer lifetime,
  /// even if the use forwards a value or produces an interior
  /// pointer. This is only invoked for uses of an inner lifetime
  /// if it ends the lifetime.
  mutating func ownershipLeafUse(of operand: Operand, isInnerLifetime: Bool)
    -> WalkResult {
    useVisitor(operand)
  }

  // Visit owned and guaranteed forwarding operands.
  //
  // Guaranteed forwarding operands extend the outer lifetime.
  //
  // Owned forwarding operands end the outer lifetime but extend the
  // inner lifetime (e.g. from a PartialApply or MarkDependence).
  mutating func forwardingUse(of operand: Operand, isInnerLifetime: Bool)
    -> WalkResult {
    switch operand.value.ownership {
    case .guaranteed:
      assert(!isInnerLifetime, "inner guaranteed forwards are not walked")
      return walkDown(operand: operand)
    case .owned:
      return isInnerLifetime ? walkDown(operand: operand) : useVisitor(operand)
    default:
      fatalError("forwarded values must have a lifetime")
    }
  }

  mutating func interiorPointerUse(of operand: Operand, into address: Value)
    -> WalkResult {
    if useVisitor(operand) == .abortWalk {
      return .abortWalk
    }
    return walkDownAddressUses(of: address)
  }

  // Handle partial_apply [on_stack] and mark_dependence [nonescaping].
  mutating func dependentUse(of operand: Operand, dependentValue value: Value) -> WalkResult {
    // OSSA lifetime ignores trivial types.
    if value.type.isTrivial(in: function) {
      return .continueWalk
    }
    guard value.type.isEscapable(in: function) else {
      // Non-escapable dependent values can be lifetime-extended by copying, which is not handled by
      // InteriorUseWalker. LifetimeDependenceDefUseWalker does this.
      return pointerEscapingUse(of: operand)
    }
    if useVisitor(operand) == .abortWalk {
      return .abortWalk
    }
    return walkDownUses(of: value)
  }

  mutating func dependentUse(of operand: Operand, dependentAddress address: Value) -> WalkResult {
    // An mutable local variable depends a value that depends on the original interior pointer. This would require data
    // flow to find local uses. InteriorUseWalker only walks the SSA uses.
    pointerEscapingUse(of: operand)
  }

  mutating func pointerEscapingUse(of operand: Operand) -> WalkResult {
    if useVisitor(operand) == .abortWalk {
      return .abortWalk
    }
    return setPointerEscape(of: operand)
  }

  mutating func setPointerEscape(of operand: Operand) -> WalkResult {
    pointerStatus.setEscaping(operand: operand)
    return ignoreEscape ? .continueWalk : .abortWalk
  }

  // Call the innerScopeHandler before visiting the scope-ending uses.
  mutating func borrowingUse(of operand: Operand, by borrowInst: BorrowingInstruction) -> WalkResult {
    if let beginBorrow = BeginBorrowValue(resultOf: borrowInst) {
      if handleInner(borrowed: beginBorrow.value) == .abortWalk {
        return .abortWalk
      }
    }
    if useVisitor(operand) == .abortWalk {
      return .abortWalk
    }
    if visitInnerBorrowUses(of: borrowInst, operand: operand) == .abortWalk {
      return .abortWalk
    }
    if !visitInnerUses {
      return .continueWalk
    }
    guard let innerValue = borrowInst.innerValue else {
      return setPointerEscape(of: operand)
    }
    // Call visitInnerBorrowUses before visitOwnershipUses because it will visit uses of tokens, such as
    // the begin_apply token, which don't have ownership.
    if innerValue.type.isAddress {
      return interiorPointerUse(of: operand, into: innerValue)
    }
    return visitOwnershipUses(of: innerValue)
  }
}

extension InteriorUseWalker: AddressUseVisitor {
  /// An address projection produces a single address result and does not
  /// escape its address operand in any other way.
  mutating func projectedAddressUse(of operand: Operand, into value: Value)
    -> WalkResult {
    return walkDownAddressUses(of: value)
  }

  mutating func appliedAddressUse(of operand: Operand, by apply: FullApplySite)
    -> WalkResult {
    if apply is BeginApplyInst {
      return scopedAddressUse(of: operand)
    }
    return leafAddressUse(of: operand)
  }

  mutating func scopedAddressUse(of operand: Operand) -> WalkResult {
    switch operand.instruction {
    case let ba as BeginAccessInst:
      if handleInner(borrowed: ba) == .abortWalk {
        return .abortWalk
      }
      return ba.scopeEndingOperands.walk { useVisitor($0) }
    case let ba as BeginApplyInst:
      if handleInner(borrowed: ba.token) == .abortWalk {
        return .abortWalk
      }
      return ba.token.uses.walk {
        useVisitor($0)
      }
    case let sb as StoreBorrowInst:
      if handleInner(borrowed: sb) == .abortWalk {
        return .abortWalk
      }
      return sb.uses.filterUsers(ofType: EndBorrowInst.self).walk {
        useVisitor($0)
      }
    case let load as LoadBorrowInst:
      if handleInner(borrowed: load) == .abortWalk {
        return .abortWalk
      }
      return load.uses.endingLifetime.walk {
        useVisitor($0)
      }
    default:
      fatalError("Unrecognized scoped address use: \(operand.instruction)")
    }
  }

  mutating func scopeEndingAddressUse(of operand: Operand) -> WalkResult {
    return .continueWalk
  }

  mutating func leafAddressUse(of operand: Operand) -> WalkResult {
    return .continueWalk
  }

  mutating func loadedAddressUse(of operand: Operand, intoValue value: Value)
    -> WalkResult {
    return .continueWalk
  }    

  mutating func loadedAddressUse(of operand: Operand, intoAddress address: Operand)
    -> WalkResult {
    return .continueWalk
  }
  
  mutating func yieldedAddressUse(of operand: Operand) -> WalkResult {
    return .continueWalk
  }

  mutating func dependentAddressUse(of operand: Operand, dependentValue value: Value)
    -> WalkResult {
    // For Escapable values, simply continue the walk.
    if value.mayEscape {
      return walkDownUses(of: value)
    }
    // TODO: Handle non-escapable values by walking through copies as done by LifetimeDependenceDefUseWalker or
    // NonEscapingClosureDefUseWalker. But this code path also handles non-escaping closures that have not been promoted
    // to [on_stack] (and still have an escapable function type). Such closures may be incorrectly destroyed after their
    // captures. To avoid this problem, either rewrite ClosureLifetimeFixup to produce correct OSSA lifetimes, or check
    // for that special case and continue to bailout here.
    return escapingAddressUse(of: operand)
  }

  mutating func dependentAddressUse(of operand: Operand, dependentAddress address: Value) -> WalkResult {
    // TODO: consider data flow that finds reachable uses of `dependentAddress`.
    return escapingAddressUse(of: operand)
  }    

  mutating func escapingAddressUse(of operand: Operand) -> WalkResult {
    pointerStatus.setEscaping(operand: operand)
    return ignoreEscape ? .continueWalk : .abortWalk
  }

  mutating func unknownAddressUse(of operand: Operand) -> WalkResult {
    pointerStatus = .unknown(operand)
    return .abortWalk
  }

  private mutating func walkDownAddressUses(of address: Value) -> WalkResult {
    assert(address.type.isAddress)
    return address.uses.ignoreTypeDependence.walk {
      // Record all uses
      if useVisitor($0) == .abortWalk {
        return .abortWalk
      }
      return classifyAddress(operand: $0)
    }
  }
}

// Helpers to walk down forwarding operations.
extension InteriorUseWalker {
  // Walk down forwarding operands
  private mutating func walkDown(operand: Operand) -> WalkResult {
    // Record all uses
    if useVisitor(operand) == .abortWalk {
      return .abortWalk
    }
    if let inst = operand.instruction as? ForwardingInstruction {
      return inst.forwardedResults.walk { walkDownUses(of: $0) }
    }
    // TODO: verify that ForwardInstruction handles all .forward operand ownership and assert that only phis can be
    // reached: assert(Phi(using: operand) != nil)
    return .continueWalk
  }

  private mutating func walkDownUses(of value: Value) -> WalkResult {
    guard value.ownership.hasLifetime else {
      return .continueWalk
    }
    guard visited.insert(value) else {
      return .continueWalk
    }
    switch value.ownership {
    case .owned:
      // Each owned lifetime is an inner scope.
      if handleInner(borrowed: value) == .abortWalk {
        return .abortWalk
      }
      return visitOwnedDependentUses(of: value)
    case .guaranteed:
      // Handle a forwarded guaranteed value exactly like the outer borrow.
      return visitOwnershipUses(of: value)
    default:
      fatalError("ownership requires a lifetime")
    }
  }
}

/// Cache the liveness boundary by taking a snapshot of its InstructionRange.
struct LivenessBoundary : CustomStringConvertible {
  var lastUsers : Stack<Instruction>
  var boundaryEdges : Stack<BasicBlock>
  var deadDefs : Stack<Value>

  // Compute the boundary of a singly-defined range.
  init(value: Value, range: InstructionRange, _ context: Context) {
    assert(range.blockRange.isValid)

    lastUsers = Stack<Instruction>(context)
    boundaryEdges = Stack<BasicBlock>(context)
    deadDefs = Stack<Value>(context)

    lastUsers.append(contentsOf: range.ends)
    boundaryEdges.append(contentsOf: range.exitBlocks)
    if lastUsers.isEmpty {
      deadDefs.push(value)
      assert(boundaryEdges.isEmpty)
    }
  }

  var description: String {
    (lastUsers.map { "last user: \($0.description)" }
    + boundaryEdges.map { "boundary edge: \($0.description)" }
    + deadDefs.map { "dead def: \($0.description)" }).joined(separator: "\n")
  }

  mutating func deinitialize() {
    lastUsers.deinitialize()
    boundaryEdges.deinitialize()
    deadDefs.deinitialize()
  }
}

let linearLivenessTest = FunctionTest("linear_liveness_swift") {
  function, arguments, context in
  let value = arguments.takeValue()
  print("Linear liveness: \(value)")
  var range = computeLinearLiveness(for: value, context)
  defer { range.deinitialize() }
  print("Live blocks:")
  print(range)
  var boundary = LivenessBoundary(value: value, range: range, context)
  defer { boundary.deinitialize() }
  print(boundary)
}

let interiorLivenessTest = FunctionTest("interior_liveness_swift") {
  function, arguments, context in
  let value = arguments.takeValue()
  let visitInnerUses = arguments.hasUntaken ? arguments.takeBool() : false

  print("Interior liveness\(visitInnerUses ? " with inner uses" : ""): \(value)")

  var range = InstructionRange(for: value, context)
  defer { range.deinitialize() }

  var visitor = InteriorUseWalker(definingValue: value, ignoreEscape: true, visitInnerUses: visitInnerUses, context) {
    range.insert($0.instruction)
    return .continueWalk
  }
  defer { visitor.deinitialize() }

  visitor.innerScopeHandler = {
    print("Inner scope: \($0)")
    return .continueWalk
  }

  let success = visitor.visitUses()

  switch visitor.pointerStatus {
  case .nonescaping:
    break
  case let .escaping(operands):
    for operand in operands {
      print("Pointer escape: \(operand.instruction)")
    }
  case let .unknown(operand):
    print("Unrecognized SIL address user \(operand.instruction)")
  }
  if success == .abortWalk {
    print("Incomplete liveness")
  }
  print(range)

  var boundary = LivenessBoundary(value: value, range: range, context)
  defer { boundary.deinitialize() }
  print(boundary)
}

//
// TODO: Move this to InstructionRange.swift when computeLinearLiveness is in the SIL module.
//
let rangeOverlapsPathTest = FunctionTest("range_overlaps_path") {
  function, arguments, context in
  let rangeValue = arguments.takeValue()
  print("Range of: \(rangeValue)")
  var range = computeLinearLiveness(for: rangeValue, context)
  defer { range.deinitialize() }
  let pathInst = arguments.takeInstruction()
  print("Path begin: \(pathInst)")
  if let pathBegin = pathInst as? ScopedInstruction {
    for end in pathBegin.endInstructions {
      print("Overlap kind:", range.overlaps(pathBegin: pathInst, pathEnd: end, context))
    }
    return
  }
  if let pathValue = pathInst as? SingleValueInstruction, pathValue.ownership == .owned {
    for end in pathValue.uses.endingLifetime {
      print("Overlap kind:", range.overlaps(pathBegin: pathInst, pathEnd: end.instruction, context))
    }
    return
  }
  print("Test specification error: not a scoped or owned instruction: \(pathInst)")
}
