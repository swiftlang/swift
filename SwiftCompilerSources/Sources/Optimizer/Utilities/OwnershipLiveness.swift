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

  assert(definingValue.ownership == .owned
    || BeginBorrowValue(definingValue) != nil,
    "value must define an OSSA lifetime")

  // InstructionRange cannot directly represent the beginning of the block
  // so we fake it with getRepresentativeInstruction().
  var range = InstructionRange(for: definingValue, context)

  // Compute liveness.
  definingValue.uses.endingLifetime.forEach {
    range.insert($0.instruction)
  }
  return range
}

typealias InnerScopeHandler = (Value) -> WalkResult

/// Compute liveness and return a range, which the caller must deinitialize.
///
/// An OSSA lifetime begins with a single "defining" value, which must
/// be owned, or must begin a borrow scope. A complete OSSA lifetime
/// has a linear lifetime, meaning that it has a lifetime-ending use
/// on all paths. Interior liveness computes liveness without assuming
/// the lifetime is complete. To do this, it must find all "use
/// points" and prove that the defining value is never propagated
/// beyond those points. This is used to initially complete OSSA
/// lifetimes and fix them after transformations that's don't preserve
/// OSSA.
///
/// Invariants:
///
/// - The definition dominates all use points.
///
/// - Liveness does not extend beyond lifetime-ending operations
/// (a.k.a. affine lifetimes).
///
/// - All inner scopes are complete. (Use `innerScopeHandler` to
/// complete them or bail-out).
func computeInteriorLiveness(for definingValue: Value,
  _ context: FunctionPassContext,
  innerScopeHandler: InnerScopeHandler? = nil) -> InstructionRange {

  assert(definingValue.ownership == .owned
    || BeginBorrowValue(definingValue) != nil,
    "value must define an OSSA lifetime")

  var range = InstructionRange(for: definingValue, context)

  var visitor = InteriorUseWalker(definingValue: definingValue, context) {
    range.insert($0.instruction)
    return .continueWalk
  }
  defer { visitor.deinitialize() }
  visitor.innerScopeHandler = innerScopeHandler
  let success = visitor.visitUses()
  switch visitor.pointerStatus {
  case .nonEscaping:
    break
  case let .escaping(operand):
    fatalError("""
                 check findPointerEscape() before computing interior liveness.
                 Pointer escape: \(operand.instruction)
                 """)
  case let .unknown(operand):
    fatalError("Unrecognized SIL address user \(operand.instruction)")
  }
  assert(success == .continueWalk, "our visitor never fails")
  assert(visitor.unenclosedPhis.isEmpty, "missing adjacent phis")
  return range
}


/// Visit all uses of an interior address that keep the parent object alive.
///
/// See C++ TransitiveAddressWalker.
///
/// Requires:
///   mutating func leafUse(address:) -> WalkResult
///   mutating func dependent(value:dependsOn:) -> WalkResult
///   mutating func pointerEscape(address:) -> WalkResult
///   mutating func unknown(address:) -> WalkResult
///
/// TODO: Verify that pointerEscape is only called for live ranges in which
/// `findPointerEscape()` returns true.
protocol AddressLifetimeDefUseWalker {
  var _context: Context { get }

  /// Start a def-use walk from and address.
  mutating func walkDownUses(ofAddress: Value) -> WalkResult 

  /// Start a def-use walk from an address operand.
  mutating func walkDown(address: Operand) -> WalkResult

  /// An address projection produces a single address result and does not
  /// escape its address operand in any other way.
  ///
  /// If this returns .continueWalk then the uses of the address
  /// result will be transitively visited.
  mutating func projection(of address: Operand) -> WalkResult

  /// A address leaf use cannot propagate the address bits beyond the
  /// instruction.
  ///
  /// An apply or builtin, which propagates an address into the
  /// callee, can be a leaf use if the argument does not escape.
  mutating func leafUse(address: Operand) -> WalkResult

  /// A non-address `value` whose ownership depends on the in-memory
  /// value at `address`, such as `mark_dependence %value on %address`.
  mutating func dependent(value: Value, dependsOn address: Operand)
    -> WalkResult

  /// A pointer escape may propagate the address beyond the current instruction.
  mutating func pointerEscape(address: Operand) -> WalkResult

  /// A unknown address use. This should never be called in valid SIL.
  mutating func unknown(address: Operand) -> WalkResult

  /// Handle begin_access.
  ///
  /// If this returns .continueWalk, then leafUse(address:) will be called
  /// on the scope ending operands.
  ///
  /// Allows the implementation to complete inner scopes before considering
  /// their scope ending operations as uses of the outer scope.
  ///
  /// This may add uses to the inner scope, but it may not modify the use-list
  /// containing \p address or in any outer scopes.
  mutating func handle(access: BeginAccessInst) -> WalkResult

  /// Handle load_borrow
  ///
  /// If this returns .continueWalk, then leafUse(address:) will be called
  /// on the scope ending operands.
  ///
  /// Allows the implementation to complete inner scopes before considering
  /// their scope ending operations as uses of the outer scope.
  ///
  /// This may add uses to the inner scope, but it may not modify the use-list
  /// containing \p address or in any outer scopes.
  mutating func handle(borrow: LoadBorrowInst) -> WalkResult
}

extension AddressLifetimeDefUseWalker {
  mutating func projection(of address: Operand) -> WalkResult {
    return .continueWalk
  }

  mutating func handle(access: BeginAccessInst) -> WalkResult {
    return .continueWalk
  }

  mutating func handle(borrow: LoadBorrowInst) -> WalkResult {
    return .continueWalk
  }

  mutating func walkDownUses(ofAddress address: Value) -> WalkResult {
    address.uses.ignoreTypeDependence.walk { walkDown(address: $0) }
  }

  private mutating func walkDown(projection value: Value, of operand: Operand)
    -> WalkResult {
    if projection(of: operand) == .abortWalk {
      return .abortWalk
    }
    return walkDownUses(ofAddress: value)
  }

  mutating func walkDown(address: Operand) -> WalkResult {
    switch address.instruction {
    case let ba as BeginAccessInst:
      if handle(access: ba) == .abortWalk {
        return .abortWalk
      }
      return ba.endOperands.walk { leafUse(address: $0) }

    case let load as LoadBorrowInst:
      if handle(borrow: load) == .abortWalk {
        return .abortWalk
      }
      return BeginBorrowValue(load)!.scopeEndingOperands.walk {
        leafUse(address: $0)
      }
    case let markDep as MarkDependenceInst:
      if markDep.valueOperand == address {
        return walkDown(projection: markDep, of: address)
      }
      assert(markDep.baseOperand == address)
      // If another address depends on the current address,
      // optimisticaly handle it like a projection.
      if markDep.type.isAddress {
        return walkDown(projection: markDep, of: address)
      }
      /* TODO: Check LifetimeDependence
      if LifetimeDependence(markDependenceInst: markDep, _context) != nil {
        return dependent(value: markDep, dependsOn: address)
      }
      */
      // A potentially escaping value depends on this address.
      return pointerEscape(address: address)

    case let pai as PartialApplyInst where pai.isOnStack:
      return dependent(value: pai, dependsOn: address)

    case is StructElementAddrInst, is TupleElementAddrInst,
         is InitEnumDataAddrInst, is UncheckedTakeEnumDataAddrInst,
         is InitExistentialAddrInst, is OpenExistentialAddrInst,
         is IndexAddrInst, is MarkUnresolvedNonCopyableValueInst,
         is ProjectBlockStorageInst, is TailAddrInst, is StoreBorrowInst,
         is UncheckedAddrCastInst, is MarkUninitializedInst, is DropDeinitInst,
         is TuplePackElementAddrInst, is CopyableToMoveOnlyWrapperAddrInst,
         is MoveOnlyWrapperToCopyableAddrInst:
      let svi = address.instruction as! SingleValueInstruction
      return walkDown(projection: svi, of: address)

    case is ReturnInst, is ThrowInst, is YieldInst, is TryApplyInst,
         is SwitchEnumAddrInst, is CheckedCastAddrBranchInst,
         is SelectEnumAddrInst, is InjectEnumAddrInst,
         is LoadInst, is CopyAddrInst, is StoreInst, is DestroyAddrInst,
         is LoadUnownedInst, is StoreUnownedInst,
         is LoadWeakInst, is StoreWeakInst,
         is AssignInst, is AssignByWrapperInst, is AssignOrInitInst,
         is MarkUnresolvedMoveAddrInst, is ExplicitCopyAddrInst,
         is RetainValueAddrInst, is ReleaseValueAddrInst,
         is UncheckedRefCastAddrInst, is UnconditionalCheckedCastAddrInst,
         is TupleAddrConstructorInst, is InitBlockStorageHeaderInst,
         is DeallocStackInst, is EndApplyInst,
         is IsUniqueInst, is MarkFunctionEscapeInst,
         is WitnessMethodInst, is ValueMetatypeInst, is ExistentialMetatypeInst,
         is DeinitExistentialAddrInst,
         is GetAsyncContinuationAddrInst, is KeyPathInst,
         is PackElementSetInst, is PackElementGetInst:
      return leafUse(address: address)

    case let builtin as BuiltinInst:
      switch builtin.id {
      case .TSanInoutAccess, .ResumeThrowingContinuationReturning,
           .ResumeNonThrowingContinuationReturning, .Copy, .GenericAdd,
           .GenericFAdd, .GenericAnd, .GenericAShr, .GenericLShr, .GenericOr,
           .GenericFDiv, .GenericMul, .GenericFMul, .GenericSDiv,
           .GenericExactSDiv, .GenericShl, .GenericSRem, .GenericSub,
           .GenericFSub, .GenericUDiv, .GenericExactUDiv, .GenericURem,
           .GenericFRem, .GenericXor, .TaskRunInline, .ZeroInitializer,
           .GetEnumTag, .InjectEnumTag:
        return leafUse(address: address)
      default:
        // TODO: SIL verification should check that this exhaustively
        // recognizes all builtin address uses.
        return .abortWalk
      }

    case is AddressToPointerInst:
      return pointerEscape(address: address)

    case let pai as PartialApplyInst where !pai.isOnStack:
      return pointerEscape(address: address)

    case is BranchInst, is CondBranchInst:
      fatalError("address phi is not allowed")

    default:
      if address.instruction.isIncidentalUse {
        return leafUse(address: address)
      }
      // Unkown instruction.
      return unknown(address: address)
    }
  }
}

/// For OwnershipUseVisitor API entry points that operate on a
/// use, Isinnerlifetime indicates whether the value being used is
/// defined by the "outer" OSSA lifetime or an inner borrow scope.
///
/// When the OwnershipUseVisitor is invoked on an outer value
/// (visitUsesOfOuter(value:)), it visits all the uses of that value
/// and also visits the lifetime-ending uses of any inner borrow
/// scopes. This provides a complete set of liveness "use points":
///
///   %0 = begin_borrow %outerValue
///   %1 = begin_borrow %0
///   end_borrow %1        // inner "use point" of %0
///   end_borrow %0        // outer use of %0
///
/// This becomes more complicated with reborrows and closures. The
/// implementation can simply rely on IsInnerLifetime to know whether
/// the value being used is part of the outer lifetimes vs. its inner
/// lifetimes. This is important, for example, if the implementation
/// wants to know if the use ends the lifetime of the outer value.
///
/// Visitor implementations treat inner and outer uses differently. It
/// may, for example, assume that inner lifetimes are complete
/// and therefore only care about the lifetime-ending uses.
enum IsInnerLifetime {
case outerLifetime
case innerLifetime
}

/// Package operand ownership into a visitor API.
///
/// Code that relies on effect of a use on ownership of its value
/// should conform to this visitor. This ensures correct handling for
/// special cases involving borrow scopes and interior pointers.
///
/// `visitUsesOfOuter(value:)` is the main entry point.
///
/// `visitUsesOfInner(value:)` is called back for each inner borrow
/// scope. The implementation may or may not decide to recurse through
/// reborrows and nested borrow scopes, calling back to
/// `visitUsesOfInner(value:)` as needed.
///
/// Visitors need to implement:
///
/// - visit(use:_),
/// - visitForwarding(operand:_)
/// - visitReborrow(operand:_)
/// - visitPointerEscape(use:)
///
/// This only visits the first level of uses. The implementation may
/// transitively visit forwarding operations in its implementation of
/// `visitForwarding(operand:_)` and `visitReborrow(operand:_)`, which
/// can recursively call back to `visitUsesOfOuter(value:)` or
/// `visitUsesOfInner(value:)`.
///
/// For uses that begin a borrow or access scope, this correctly
/// "skips ahead" to the end of the scope. To record incomplete or
/// dead inner scopes (no scope-ending use on some path), the
/// implementation must override `handleInner(borrow:)`.
protocol OwnershipUseVisitor {
  var _context: Context { get }

  /// Visit a non-forwarding use. For uses that are guarded by a
  /// handler, this is only called if the handler returns .continueWalk.
  ///
  /// IsInnerLifetime indicates whether `operand` uses the original
  /// OSSA lifetime. This use ends the original lifetime if
  /// (IsInnerLifetime == .outerLifetime && use.endsLifetime).
  func visit(use: Operand, _: IsInnerLifetime) -> WalkResult

  /// Visit a forwarding operand. For uses that are guarded by a
  /// handler, this is only called if the handler returns .continueWalk.
  ///
  /// Use ForwardingInstruction or ForwardingDefUseWalker to handle
  /// downstream uses.
  mutating func visitForwarding(operand: Operand, _: IsInnerLifetime)
  -> WalkResult

  /// Visit a reborrow operand. For uses that are guarded by a
  /// handler, this is only called if the handler returns .continueWalk.
  ///
  /// Use visitUsesOfInner to visit downstream borrow scopes.
  mutating func visitReborrow(operand: Operand, _: IsInnerLifetime)
  -> WalkResult

  /// Visit a use that propagates its operand to some trivial
  /// value such as an address that depends on the operand's value.
  mutating func visitInteriorPointer(use: Operand) -> WalkResult

  /// Visit a use that escapes information from its operand's value.
  ///
  /// Note: visitPointerEscape may not find all relevant pointer
  /// escapes, such as from owned forwarded values. Clients should
  /// generally check findPointerEscape() before relying on a liveness
  /// result and implement this as a fatalError.
  mutating func visitPointerEscape(use: Operand) -> WalkResult

  /// Handles any of:
  ///
  /// - begin_borrow, load_borrow, store_borrow, begin_apply.
  ///
  /// - an inner adjacent phi (where the value currently being visited is an
  /// outer adjacent phi in the same block).
  ///
  /// - a reborrow of an inner borrow scope or a reborrow of an inner
  /// adjacent phi.
  ///
  /// If this returns .continueWalk, then visit(use:) will be called
  /// on the scope-ending operands.
  ///
  /// Allows the implementation to complete an inner scope before
  /// visiting its scope-ending operations as uses of the outer
  /// scope. The implementation may add uses to the inner scope, but
  /// it may not modify the use-list containing \p address or in any
  /// outer scopes.
  mutating func handleInner(borrow: BeginBorrowValue) -> WalkResult

  /// Handle begin_access.
  ///
  /// If this returns .continueWalk, then visit(use:) will be called
  /// on the scope ending operands.
  ///
  /// Allows the implementation to complete an inner scope before
  /// visiting its scope-ending operations as uses of the outer
  /// scope. The implementation may add uses to the inner scope, but
  /// it may not modify the use-list containing \p address or in any
  /// outer scopes.
  mutating func handleAccess(address: BeginAccessInst) -> WalkResult
}

// Default requirements: Visit everything as a regular use except escapes.
extension OwnershipUseVisitor {
  mutating func handleInner(borrow: BeginBorrowValue) -> WalkResult {
    return .continueWalk
  }

  mutating func handleAccess(address: BeginAccessInst) -> WalkResult {
    return .continueWalk
  }
}

extension OwnershipUseVisitor {
  /// Visit all uses that contribute to the ownership live
  /// range of `value`. This does not assume that `value` has a
  /// complete lifetime, and non-lifetime-ending uses are visited.
  ///
  /// If `value` is a phi (owned or reborrowed), then find its inner
  /// adjacent phis and treat them like inner borrows.
  ///
  /// This is only called for uses in the outer lifetime.
  mutating func visitUsesOfOuter(value: Value) -> WalkResult {
    // If the outer value is an owned phi or reborrow, consider inner
    // adjacent phis part of its lifetime.
    if let phi = Phi(value), phi.endsLifetime {
      var innerPhis = Stack<Phi>(_context)
      defer { innerPhis.deinitialize() }
      gatherInnerAdjacentPhis(for: phi, in: &innerPhis, _context)
      // Inner adjacent reborrows are considered inner borrow scopes.
      // Inner adjacent guaranteed phis are consider part of the outer lifetime.
      return innerPhis.walk { innerPhi in
        innerPhi.isReborrow ? visitUsesOfInner(value: innerPhi.value)
          : innerPhi.value.uses.ignoreTypeDependence.walk {
            visitGuaranteed(use: $0)
          }
      }
    }
    switch value.ownership {
    case .owned:
      return value.uses.ignoreTypeDependence.walk { visitOwned(use: $0) }
    case .guaranteed:
      return value.uses.ignoreTypeDependence.walk { visitGuaranteed(use: $0) }
    case .none, .unowned:
      return .continueWalk
    }
  }

  /// Visit only those uses of a value within an inner borrow scope
  /// that may affect the outer lifetime. An inner borrow scope is one
  /// in which the borrowing operand is itself a use of the outer
  /// lifetime, including: begin_borrow, reborrow, partial_apply,
  /// mark_dependence, or an inner adjacent phi (where original SSA
  /// def is a phi in the same block).
  mutating func visitUsesOfInner(value: Value) -> WalkResult {
    if let beginBorrow = BeginBorrowValue(value) {
      if handleInner(borrow: beginBorrow) == .abortWalk {
        return .abortWalk
      }
      return beginBorrow.scopeEndingOperands.walk {
        switch $0.ownership {
        case .endBorrow:
          return visit(use: $0, .innerLifetime)
        case .reborrow:
          return visitReborrow(operand: $0, .innerLifetime)
        default:
          fatalError("invalid borrow scope ending operand ownership")
        }
      }
    }
    // When a borrow introduces an owned value, each OSSA lifetime is
    // effectively a separate borrow scope. A destroy ends the borrow
    // scope, while a forwarding consume effectively "reborrows".
    assert(value.ownership == .owned,
      "inner value must be a reborrow or owned forward")
    return value.uses.endingLifetime.walk {
      switch $0.ownership {
      case .forwardingConsume:
        return visitForwarding(operand: $0, .innerLifetime)
      case .destroyingConsume:
        return visit(use: $0, .innerLifetime)
      default:
        fatalError("invalid owned lifetime ending operand ownership")
      }
    }
  }
}

extension OwnershipUseVisitor {
  // Visit a borrowing operand (operandOwnerhip == .borrow).
  private mutating func visitInnerBorrow(operand: Operand) -> WalkResult {
    // If a borrowed value is introduced, then handle the inner scope.
    if let beginBorrow = BeginBorrowValue(using: operand) {
      return visitUsesOfInner(value: beginBorrow.value)
    }
    // Otherwise, directly visit the scope ending uses.
    let borrowInst = BorrowingInstruction(operand.instruction)!
    return borrowInst.visitScopeEndingOperands(_context) { [self] in 
      visit(use: $0, .innerLifetime)
    }
  }

  // This is only called for uses in the outer lifetime.
  private mutating func visitOwned(use: Operand) -> WalkResult {
    switch use.ownership {
    case .nonUse:
      return .continueWalk

    case .destroyingConsume:
      return visit(use: use, .outerLifetime)

    case .forwardingConsume:
      return visitForwarding(operand: use, .outerLifetime)

    case .pointerEscape:
      return visitPointerEscape(use: use)

    case .instantaneousUse, .forwardingUnowned, .unownedInstantaneousUse,
      .bitwiseEscape:
      return visit(use: use, .outerLifetime)

    case .borrow:
      return visitInnerBorrow(operand: use)

    // TODO: Eventually, visit owned InteriorPointers as implicit borrows.
    case .interiorPointer, .trivialUse, .endBorrow, .reborrow,
      .guaranteedForwarding:
      fatalError("ownership incompatible with an owned value");
    }
  }

  // This is only called for uses in the outer lifetime.
  private mutating func visitGuaranteed(use: Operand)
  -> WalkResult {
    switch use.ownership {
    case .nonUse:
      return .continueWalk

    case .pointerEscape:
      return visitPointerEscape(use: use)

    case .instantaneousUse, .forwardingUnowned, .unownedInstantaneousUse,
      .bitwiseEscape:
      return visit(use: use, .outerLifetime)

    case .endBorrow:
      return visit(use: use, .outerLifetime)

    case .reborrow:
      return visitReborrow(operand: use, .outerLifetime)

    case .guaranteedForwarding:
      return visitForwarding(operand: use, .outerLifetime)

    case .borrow:
      return visitInnerBorrow(operand: use)

    case .interiorPointer:
      return visitInteriorPointer(use: use)

    case .trivialUse, .forwardingConsume, .destroyingConsume:
      fatalError("ownership incompatible with a guaranteed value")
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
/// scope. Phis that are not dominanted by definingValue or an outer
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
///
/// TODO: Change the operandOwnership of MarkDependenceInst base operand.
/// It should be a borrowing operand, not a pointer escape.
struct InteriorUseWalker: OwnershipUseVisitor, AddressLifetimeDefUseWalker {
  let context: FunctionPassContext
  var _context: Context { context }

  let definingValue: Value
  let useVisitor: (Operand) -> WalkResult

  var innerScopeHandler: InnerScopeHandler? = nil

  var unenclosedPhis: [Phi] = []

  var function: Function { definingValue.parentFunction }

  /// If any interior pointer may escape, then record the first instance
  /// here. This immediately aborts the walk, so further instances are
  /// unavailable.
  ///
  /// .escaping may either be a non-address operand with
  /// .pointerEscape ownership, or and address operand that escapes
  /// the adderss (address_to_pointer).
  ///
  /// .unknown is an address operand whose user is unrecognized.
  enum InteriorPointerStatus {
    case nonEscaping
    case escaping(Operand)
    case unknown(Operand)
  }
  var pointerStatus: InteriorPointerStatus = .nonEscaping

  private var visited: ValueSet

  mutating func deinitialize() {
    visited.deinitialize()
  }

  init(definingValue: Value, _ context: FunctionPassContext,
    visitor: @escaping (Operand) -> WalkResult) {
    assert(!definingValue.type.isAddress, "address values have no ownership")
    self.context = context
    self.definingValue = definingValue
    self.useVisitor = visitor
    self.visited = ValueSet(context)
  }

  mutating func visitUses() -> WalkResult {
    visitUsesOfOuter(value: definingValue)
  }

  /// This is invoked for all uses of the outer lifetime, even if the
  /// use forwards a value or produces an interior pointer. This is
  /// not invoked for address-to-address projections. This is only
  /// invoked for uses of an inner lifetime if `use` ends the
  /// lifetime.
  func visit(use: Operand, _ isInnerLifetime: IsInnerLifetime)
  -> WalkResult {
    useVisitor(use)
  }

  // Visit owned and guaranteed forwarding operands.
  //
  // Guaranteed forwarding operands extend the outer lifetime.
  //
  // Owned forwarding operands end the outer lifetime but extend the
  // inner lifetime (e.g. from a PartialApply or MarkDependence).
  mutating func visitForwarding(operand: Operand,
    _ isInnerLifetime: IsInnerLifetime) -> WalkResult {
    switch operand.value.ownership {
    case .guaranteed:
      assert(isInnerLifetime == .outerLifetime,
        "inner guaranteed forwards are not walked")
      return walkDown(operand: operand)
    case .owned:
      switch isInnerLifetime {
      case .outerLifetime:
        return visit(use: operand, .outerLifetime)
      case .innerLifetime:
        return walkDown(operand: operand)
      }
    default:
      fatalError("forwarded values must have a lifetime")
    }
  }

  // Visit a reborrow operand. This ends an outer lifetime and extends
  // an inner lifetime.
  mutating func visitReborrow(operand: Operand,
    _ isInnerLifetime: IsInnerLifetime) -> WalkResult {
    switch isInnerLifetime {
    case .outerLifetime:
      return visit(use: operand, .outerLifetime)
    case .innerLifetime:
      return walkDown(operand: operand)
    }
  }

  mutating func visitPointerEscape(use: Operand) -> WalkResult {
    if useVisitor(use) == .abortWalk {
      return .abortWalk
    }
    pointerStatus = .escaping(use)
    return .abortWalk
  }

  // TODO: Change ProjectBox ownership to InteriorPointer
  mutating func visitInteriorPointer(use: Operand) -> WalkResult {
    // OSSA lifetime ignores trivial types.
    if use.value.type.isTrivial(in: function) {
      return .continueWalk
    }
    if useVisitor(use) == .abortWalk {
      return .abortWalk
    }
    let instruction = use.instruction
    switch instruction {
    case let rta as RefTailAddrInst:
      return walkDownUses(ofAddress: rta)
    case let rea as RefElementAddrInst:
      return walkDownUses(ofAddress: rea)
    case let pb as ProjectBoxInst:
      return walkDownUses(ofAddress: pb)
    default:
      return walkDown(address: use)
    }
  }

  mutating func projection(of address: Operand) -> WalkResult {
    visit(use: address, .outerLifetime)
  }

  mutating func leafUse(address: Operand) -> WalkResult {
    visit(use: address, .outerLifetime)
  }

  mutating func pointerEscape(address: Operand) -> WalkResult {
    visitPointerEscape(use: address)
  }

  mutating func unknown(address: Operand) -> WalkResult {
    pointerStatus = .unknown(address)
    return .abortWalk
  }

  mutating func dependent(value: Value, dependsOn address: Operand)
    -> WalkResult {
    if visit(use: address, .outerLifetime) == .abortWalk {
      return .abortWalk
    }
    // If `value` is owned, then `visit(use:, .innerLifetime)` will be
    // invoked for each leaf use.
    return walkDownUses(of: value)
  }

  func handleInner(borrow: BeginBorrowValue) -> WalkResult  {
    guard let innerScopeHandler else {
      return .continueWalk
    }
    return innerScopeHandler(borrow.value)
  }

  func handleAccess(address: BeginAccessInst) -> WalkResult {
    guard let innerScopeHandler else {
      return .continueWalk
    }
    return innerScopeHandler(address)
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
    if let phi = Phi(using: operand) {
      if phi.value.ownership == .guaranteed {
        return walkDown(guaranteedPhi: phi)
      }
      return walkDownUses(of: phi.value)
    }
    // TODO: verify that ForwardInstruction handles all .forward
    // operand ownership and change this to a fatalError.
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
      return visitUsesOfInner(value: value)
    case .guaranteed:
      return visitUsesOfOuter(value: value)
    default:
      fatalError("ownership requires a lifetime")
    }
  }

  // Dominating definingValue example: walkDown must continue visiting
  // uses of a reborrow in the inner borrow scope:
  //
  // bb0:
  //  d1 = ...
  //  cond_br bb1, bb2
  // bb1:
  //   b1 = borrow d1
  //   br bb3(b1)
  // bb2:
  //   b2 = borrow d1
  //   br bb3(b2)
  // bb3(reborrow):
  //   u1 = d1
  //   u2 = reborrow
  //   // can't move destroy above u2
  //   destroy_value d1
  //
  // Dominating definingValue example: walkDown must continue visiting
  // uses of a guaranteed phi in the outer lifetime:
  //
  // bb0:
  //  b1 = borrow d1
  //  cond_br bb1, bb2
  // bb1:
  //   p1 = projection b1
  //   br bb3(p1)
  // bb2:
  //   p1 = projection b1
  //   br bb3(p2)
  // bb3(forwardingPhi):
  //   u1 = b1
  //   u2 = forwardingPhi
  //   // can't move end_borrow above u2
  //   end_borrow b1
  private mutating func walkDown(guaranteedPhi: Phi) -> WalkResult {
    guard visited.insert(guaranteedPhi.value) else {
      return .continueWalk
    }
    var enclosingValues = Stack<Value>(context)
    defer { enclosingValues.deinitialize() }
    gatherEnclosingValues(for: guaranteedPhi.value, in: &enclosingValues,
                          context)
    guard enclosingValues.contains(definingValue) else {
      // Since definingValue is not an enclosing value, it must be
      // consumed or reborrowed by some outer adjacent phi in this
      // block. An outer adjacent phi's uses do not contribute to the
      // outer liveness. Instead, guaranteedPhi will be recorded as a
      // regular lifetime-ending use by the visitor.
      return .continueWalk
    }
    // definingValue is not consumed or reborrowed by an outer
    // adjacent phi in guaranteedPhi's block. Therefore this
    // guaranteedPhi's uses contribute to the liveness of
    // definingValue.
    //
    // TODO: instead of relying on Dominance, we can reformulate
    // this algorithm to detect redundant phis, similar to the
    // SSAUpdater.
    if !definingValue.parentBlock.dominates(guaranteedPhi.successor,
      context.dominatorTree) {
      // definingValue does not dominate guaranteedPhi. Record this
      // unenclosed phi so the liveness client can insert the missing
      // outer adjacent phi.
      unenclosedPhis.append(guaranteedPhi);
      return .continueWalk
    }
    // Since definingValue dominates guaranteedPhi, this is a well-formed linear
    // lifetime, and liveness can proceed.
    if guaranteedPhi.isReborrow {
      return visitUsesOfInner(value: guaranteedPhi.value)
    } else {
      return visitUsesOfOuter(value: guaranteedPhi.value)
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
    assert(range.isValid)

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
  print(function)
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
  print(function)
  print("Interior liveness: \(value)")

  var range = InstructionRange(for: value, context)
  defer { range.deinitialize() }

  var visitor = InteriorUseWalker(definingValue: value, context) {
    range.insert($0.instruction)
    return .continueWalk
  }
  defer { visitor.deinitialize() }

  let success = visitor.visitUses()

  switch visitor.pointerStatus {
  case .nonEscaping:
    break
  case let .escaping(operand):
    print("Pointer escape: \(operand.instruction)")
  case let .unknown(operand):
    print("Unrecognized SIL address user \(operand.instruction)")
  }
  if success == .abortWalk {
    print("Incomplete liveness")
  }
  print(range)
  print("Unenclosed phis {")
  visitor.unenclosedPhis.forEach { print("  \($0)") } 
  print("}")

  var boundary = LivenessBoundary(value: value, range: range, context)
  defer { boundary.deinitialize() }
  print(boundary)
}

// Print the uses of an address.
struct AddressLifetimeUsePrinter: AddressLifetimeDefUseWalker {
  let _context: Context

  mutating func leafUse(address: Operand) -> WalkResult {
    print("Leaf use: \(address)")
    return .continueWalk
  }

  mutating func pointerEscape(address: Operand) -> WalkResult {
    print("Pointer escape: \(address)")
    return .continueWalk
  }

  mutating func unknown(address: Operand) -> WalkResult {
    print("Unknown: \(address)")
    return .continueWalk
  }

  mutating func dependent(value: Value, dependsOn address: Operand)
    -> WalkResult {
    print("Dependent: \(value) on \(address)")
    return .continueWalk
  }
}

let addressUseTest = FunctionTest("address_use_test") {
    function, arguments, context in
  let address = arguments.takeValue()
  assert(address.type.isAddress)
  print(function)
  print("Uses of address: \(address)")
  var printer = AddressLifetimeUsePrinter(_context: context)
  _ = printer.walkDownUses(ofAddress: address)
}
