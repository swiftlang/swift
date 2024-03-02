//===--- AddressUtils.swift - Utilities for handling SIL addresses -------===//
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

/// Classify address uses. This can be used by def-use walkers to
/// ensure complete handling of all legal SIL patterns.
///
/// TODO: Integrate this with SIL verification to ensure completeness.
///
/// TODO: Convert AddressDefUseWalker to conform to AddressUtils after
/// checking that the additional instructions are handled correctly by
/// escape analysis.
///
/// TODO: Verify that pointerEscape is only called for live ranges in which
/// `findPointerEscape()` returns true.
protocol AddressUseVisitor {
  var context: Context { get }

  /// An address projection produces a single address result and does not
  /// escape its address operand in any other way.
  mutating func projectedAddressUse(of operand: Operand, into value: Value)
    -> WalkResult

  /// An access scope: begin_access, begin_apply, load_borrow.
  mutating func scopedAddressUse(of operand: Operand) -> WalkResult

  /// end_access, end_apply, abort_apply, end_borrow.
  mutating func scopeEndingAddressUse(of operand: Operand) -> WalkResult

  /// An address leaf use propagates neither the address bits, nor the
  /// in-memory value beyond the instruction.
  ///
  /// StoringInstructions are leaf uses.
  mutating func leafAddressUse(of operand: Operand) -> WalkResult

  /// An address used by an apply.
  mutating func appliedAddressUse(of operand: Operand, by apply: FullApplySite)
    -> WalkResult

  /// A loaded address use propagates the value at the address.
  mutating func loadedAddressUse(of operand: Operand, into value: Value)
    -> WalkResult

  /// A loaded address use propagates the value at the address to the
  /// destination address operand.
  mutating func loadedAddressUse(of operand: Operand, into address: Operand)
    -> WalkResult

  /// A non-address owned `value` whose ownership depends on the in-memory
  /// value at `address`, such as `mark_dependence %value on %address`.
  mutating func dependentAddressUse(of operand: Operand, into value: Value)
    -> WalkResult

  /// A pointer escape may propagate the address beyond the current instruction.
  mutating func escapingAddressUse(of operand: Operand) -> WalkResult

  /// A unknown address use. This should never be called in valid SIL.
  mutating func unknownAddressUse(of operand: Operand) -> WalkResult
}

extension AddressUseVisitor {
  /// Classify an address-type operand, dispatching to one of the
  /// protocol methods above.
  mutating func classifyAddress(operand: Operand) -> WalkResult {
    switch operand.instruction {
    case is BeginAccessInst, is LoadBorrowInst, is StoreBorrowInst:
      return scopedAddressUse(of: operand)

    case is EndAccessInst, is EndApplyInst, is AbortApplyInst, is EndBorrowInst:
      return scopeEndingAddressUse(of: operand)

    case let markDep as MarkDependenceInst:
      if markDep.valueOperand == operand {
        return projectedAddressUse(of: operand, into: markDep)
      }
      assert(markDep.baseOperand == operand)
      // If another address depends on the current address,
      // handle it like a projection.
      if markDep.type.isAddress {
        return projectedAddressUse(of: operand, into: markDep)
      }
      if LifetimeDependence(markDep, context) != nil {
        // This is unreachable from InteriorUseVisitor because the
        // base address of a `mark_dependence [nonescaping]` must be a
        // `begin_access`, and interior liveness does not check uses of
        // the accessed address.
        return dependentAddressUse(of: operand, into: markDep)
      }
      // A potentially escaping value depends on this address.
      return escapingAddressUse(of: operand)

    case let pai as PartialApplyInst where pai.isOnStack:
      return dependentAddressUse(of: operand, into: pai)

    case let pai as PartialApplyInst where !pai.isOnStack:
      return escapingAddressUse(of: operand)

    case is ReturnInst, is ThrowInst, is YieldInst, is AddressToPointerInst:
      return escapingAddressUse(of: operand)

    case is StructElementAddrInst, is TupleElementAddrInst,
         is IndexAddrInst, is TailAddrInst, is TuplePackElementAddrInst, 
         is InitEnumDataAddrInst, is UncheckedTakeEnumDataAddrInst,
         is InitExistentialAddrInst, is OpenExistentialAddrInst,
         is ProjectBlockStorageInst, is UncheckedAddrCastInst,
         is UnconditionalCheckedCastAddrInst,
         is MarkUninitializedInst, is DropDeinitInst,
         is CopyableToMoveOnlyWrapperAddrInst,
         is MoveOnlyWrapperToCopyableAddrInst,
         is MarkUnresolvedNonCopyableValueInst:
      let svi = operand.instruction as! SingleValueInstruction
      return projectedAddressUse(of: operand, into: svi)

    case let apply as FullApplySite:
      return appliedAddressUse(of: operand, by: apply)

    case is SwitchEnumAddrInst, is CheckedCastAddrBranchInst,
         is SelectEnumAddrInst, is InjectEnumAddrInst,
         is StoreInst, is StoreUnownedInst, is StoreWeakInst,
         is AssignInst, is AssignByWrapperInst, is AssignOrInitInst,
         is TupleAddrConstructorInst, is InitBlockStorageHeaderInst,
         is RetainValueAddrInst, is ReleaseValueAddrInst,
         is DestroyAddrInst, is DeallocStackInst, 
         is DeinitExistentialAddrInst,
         is IsUniqueInst, is MarkFunctionEscapeInst,
         is PackElementSetInst:
      return leafAddressUse(of: operand)

    case is LoadInst, is LoadUnownedInst,  is LoadWeakInst, 
         is ValueMetatypeInst, is ExistentialMetatypeInst,
         is PackElementGetInst:
      let svi = operand.instruction as! SingleValueInstruction
      return loadedAddressUse(of: operand, into: svi)

    case let sdai as SourceDestAddrInstruction
           where sdai.sourceOperand == operand:
      return loadedAddressUse(of: operand, into: sdai.destinationOperand)

    case let sdai as SourceDestAddrInstruction
           where sdai.destinationOperand == operand:
      return leafAddressUse(of: operand)

    case let builtin as BuiltinInst:
      switch builtin.id {
      case .Copy where builtin.operands[1] == operand: // source
        return loadedAddressUse(of: operand, into: builtin.operands[0])

      case .Copy where builtin.operands[0] == operand: // dest
        return leafAddressUse(of: operand)

      // Builtins that cannot load a nontrivial value.
      case .TSanInoutAccess, .ResumeThrowingContinuationReturning,
           .ResumeNonThrowingContinuationReturning, .GenericAdd,
           .GenericFAdd, .GenericAnd, .GenericAShr, .GenericLShr, .GenericOr,
           .GenericFDiv, .GenericMul, .GenericFMul, .GenericSDiv,
           .GenericExactSDiv, .GenericShl, .GenericSRem, .GenericSub,
           .GenericFSub, .GenericUDiv, .GenericExactUDiv, .GenericURem,
           .GenericFRem, .GenericXor, .TaskRunInline, .ZeroInitializer,
           .GetEnumTag, .InjectEnumTag:
        return leafAddressUse(of: operand)
      default:
        // TODO: SIL verification should check that this exhaustively
        // recognizes all builtin address uses.
        return .abortWalk
      }

    case is BranchInst, is CondBranchInst:
      fatalError("address phi is not allowed")

    default:
      if operand.instruction.isIncidentalUse {
        return leafAddressUse(of: operand)
      }
      // Unkown instruction.
      return unknownAddressUse(of: operand)
    }
  }
}

extension AccessBase {
  /// If this access base has a single initializer, return it, along
  /// with the initialized address. This does not guarantee that all
  /// uses of that address are dominated by the store or even that the
  /// store is a direct use of `address`.
  func findSingleInitializer(_ context: some Context) -> (initialAddress: Value, initializingStore: Instruction)? {
    let baseAddr: Value
    switch self {
    case let .stack(allocStack):
      baseAddr = allocStack
    case let .argument(arg):
      guard arg.convention.isIndirectOut else {
        return nil
      }
      baseAddr = arg
    default:
      return nil
    }
    return AddressInitializationWalker.findSingleInitializer(ofAddress: baseAddr, context: context)
  }
}

// Walk the address def-use paths to find a single initialization.
//
// Implements AddressUseVisitor to guarantee that we can't miss any
// stores. This separates escapingAddressUse from leafAddressUse.
//
// Main entry point:
//  static func findSingleInitializer(ofAddress: Value, context: some Context)
//
// TODO: Make AddressDefUseWalker always conform to AddressUseVisitor once we're
// ready to debug changes to escape analysis etc...
//
// Future:
// AddressUseVisitor
//    (how to transitively follow uses, complete classification)
// -> AddressPathDefUseWalker
//    (follow projections and track Path,
//     client handles all other uses, such as access scopes)
// -> AddressProjectionDefUseWalker
//    (follow projections, track Path, ignore access scopes,
//     merge all other callbacks into only two:
//     instantaneousAddressUse vs. escapingAddressUse)
//
// FIXME: This currently assumes that isAddressInitialization catches
// writes to the memory address. We need a complete abstraction that
// distinguishes between `mayWriteToMemory` for dependence vs. actual
// modification of memory.
struct AddressInitializationWalker: AddressDefUseWalker, AddressUseVisitor {
  let context: any Context

  var walkDownCache = WalkerCache<SmallProjectionPath>()

  var isProjected = false
  var initializingStore: Instruction?

  static func findSingleInitializer(ofAddress baseAddr: Value, context: some Context)
    -> (initialAddress: Value, initializingStore: Instruction)? {

    var walker = AddressInitializationWalker(context: context)
    if walker.walkDownUses(ofAddress: baseAddr, path: SmallProjectionPath()) == .abortWalk {
      return nil
    }
    guard let initializingStore = walker.initializingStore else {
      return nil
    }
    return (initialAddress: baseAddr, initializingStore: initializingStore)
  }

  private mutating func setInitializer(instruction: Instruction) -> WalkResult {
    // An initializer must be unique and store the full value.
    if initializingStore != nil || isProjected {
      initializingStore = nil
      return .abortWalk
    }
    initializingStore = instruction
    return .continueWalk
  }
}

// Implement AddressDefUseWalker
extension AddressInitializationWalker {
  mutating func leafUse(address: Operand, path: SmallProjectionPath)
    -> WalkResult {
    isProjected = !path.isEmpty
    return classifyAddress(operand: address)
  }
}

// Implement AddresUseVisitor
extension AddressInitializationWalker {
  /// An address projection produces a single address result and does not
  /// escape its address operand in any other way.
  mutating func projectedAddressUse(of operand: Operand, into value: Value)
    -> WalkResult {
    // AddressDefUseWalker should catch most of these.
    return .abortWalk
  }

  mutating func scopedAddressUse(of operand: Operand) -> WalkResult {
    // AddressDefUseWalker currently skips most of these.
    return .abortWalk
  }

  mutating func scopeEndingAddressUse(of operand: Operand) -> WalkResult {
    // AddressDefUseWalker currently skips most of these.
    return .continueWalk
  }

  mutating func leafAddressUse(of operand: Operand) -> WalkResult {
    if operand.isAddressInitialization {
      return setInitializer(instruction: operand.instruction)
    }
    // FIXME: check mayWriteToMemory but ignore non-stores. Currently,
    // stores should all be checked my isAddressInitialization, but
    // this is not robust.
    return .continueWalk
  }

  mutating func appliedAddressUse(of operand: Operand, by apply: FullApplySite)
    -> WalkResult {
    if operand.isAddressInitialization {
      return setInitializer(instruction: operand.instruction)
    }
    guard let convention = apply.convention(of: operand) else {
      return .continueWalk
    }
    return convention.isIndirectIn ? .continueWalk : .abortWalk
  }

  mutating func loadedAddressUse(of operand: Operand, into value: Value)
    -> WalkResult {
    return .continueWalk
  }

  mutating func loadedAddressUse(of operand: Operand, into address: Operand)
    -> WalkResult {
    return .continueWalk
  }

  mutating func dependentAddressUse(of operand: Operand, into value: Value)
    -> WalkResult {
    return .continueWalk
  }

  mutating func escapingAddressUse(of operand: Operand) -> WalkResult {
    return .abortWalk
  }

  mutating func unknownAddressUse(of operand: Operand) -> WalkResult {
    return .abortWalk
  }
}
