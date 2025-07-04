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

private let verbose = false

private func log(_ message: @autoclosure () -> String) {
  if verbose {
    print("### \(message())")
  }
}

/// Classify address uses. This can be used by def-use walkers to
/// ensure complete handling of all legal SIL patterns.
///
/// TODO: Integrate this with SIL verification to ensure completeness.
///
/// TODO: Convert AddressDefUseWalker to use AddressUseVisitor after
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
  mutating func loadedAddressUse(of operand: Operand, intoValue value: Value)
    -> WalkResult

  /// A loaded address use propagates the value at the address to the
  /// destination address operand.
  mutating func loadedAddressUse(of operand: Operand, intoAddress address: Operand)
    -> WalkResult

  /// Yielding an address may modify the value at the yield, but not past the yield. The yielded value may escape, but
  /// only if its type is escapable.
  mutating func yieldedAddressUse(of operand: Operand) -> WalkResult

  /// A forwarded `value` whose ownership depends on the in-memory value at the address `operand`.
  /// `%value` may be an address type, but only uses of this address value are dependent.
  /// e.g. `%value = mark_dependence %_ on %operand`
  mutating func dependentAddressUse(of operand: Operand, dependentValue value: Value) -> WalkResult

  /// A dependent `address` whose lifetime depends on the in-memory value at `address`.
  /// e.g. `mark_dependence_addr %address on %operand`
  mutating func dependentAddressUse(of operand: Operand, dependentAddress address: Value) -> WalkResult

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

    case is MarkDependenceInstruction:
      return classifyMarkDependence(operand: operand)

    case let pai as PartialApplyInst where !pai.mayEscape:
      return dependentAddressUse(of: operand, dependentValue: pai)

    case let pai as PartialApplyInst where pai.mayEscape:
      return escapingAddressUse(of: operand)

    case is ThrowInst, is AddressToPointerInst:
      return escapingAddressUse(of: operand)

    case is StructElementAddrInst, is TupleElementAddrInst,
         is IndexAddrInst, is TailAddrInst, is TuplePackElementAddrInst, 
         is InitEnumDataAddrInst, is UncheckedTakeEnumDataAddrInst,
         is InitExistentialAddrInst, is OpenExistentialAddrInst,
         is ProjectBlockStorageInst, is UncheckedAddrCastInst,
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
         is PackElementSetInst, is EndCOWMutationAddrInst:
      return leafAddressUse(of: operand)

    case is LoadInst, is LoadUnownedInst,  is LoadWeakInst, is ValueMetatypeInst, is ExistentialMetatypeInst,
         is PackElementGetInst:
      let svi = operand.instruction as! SingleValueInstruction
      return loadedAddressUse(of: operand, intoValue: svi)

    case is YieldInst:
      return yieldedAddressUse(of: operand)

    case let sdai as SourceDestAddrInstruction
           where sdai.sourceOperand == operand:
      return loadedAddressUse(of: operand, intoAddress: sdai.destinationOperand)

    case let sdai as SourceDestAddrInstruction
           where sdai.destinationOperand == operand:
      return leafAddressUse(of: operand)

    case let builtin as BuiltinInst:
      switch builtin.id {
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
      // Unknown instruction.
      return unknownAddressUse(of: operand)
    }
  }

  private mutating func classifyMarkDependence(operand: Operand) -> WalkResult {
    switch operand.instruction {
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
      switch markDep.dependenceKind {
      case .Unresolved:
        if LifetimeDependence(markDep, context) == nil {
          break
        }
        fallthrough
      case .NonEscaping:
        // Note: This is unreachable from InteriorUseVisitor because the base address of a `mark_dependence
        // [nonescaping]` must be a `begin_access`, and interior liveness does not check uses of the accessed address.
        return dependentAddressUse(of: operand, dependentValue: markDep)
      case .Escaping:
        break
      }
      // A potentially escaping value depends on this address.
      return escapingAddressUse(of: operand)
      
    case let markDep as MarkDependenceAddrInst:
      if markDep.addressOperand == operand {
        return leafAddressUse(of: operand)
      }
      switch markDep.dependenceKind {
      case .Unresolved:
        if LifetimeDependence(markDep, context) == nil {
          break
        }
        fallthrough
      case .NonEscaping:
        return dependentAddressUse(of: operand, dependentAddress: markDep.address)
      case .Escaping:
        break
      }
      // A potentially escaping value depends on this address.
      return escapingAddressUse(of: operand)

    default:
      fatalError("Unexpected MarkDependenceInstruction")
    }
  }
}

extension AccessBase {
  enum Initializer {
    // An @in or @inout argument that is never modified inside the function. Handling an unmodified @inout like an @in
    // allows clients to ignore access scopes for the purpose of reaching definitions and lifetimes.
    case argument(FunctionArgument)
    // A yielded @in argument.
    case yield(MultipleValueInstructionResult)
    // A local variable or @out argument that is assigned once.
    // 'initialAddress' is the first point at which address may be used. Typically the allocation.
    case store(initializingStore: Instruction, initialAddress: Value)

    var initialAddress: Value {
      let address: Value
      switch self {
      case let .argument(arg):
        address = arg
      case let .yield(result):
        address = result
      case let .store(_, initialAddress):
        address = initialAddress
      }
      assert(address.type.isAddress)
      return address
    }
  }

  /// If this access base has a single initializer, return it, along with the initialized address. This does not
  /// guarantee that all uses of that address are dominated by the store or even that the store is a direct use of
  /// `address`.
  func findSingleInitializer(_ context: some Context) -> Initializer? {
    let baseAddr: Value
    switch self {
    case let .stack(allocStack):
      baseAddr = allocStack
    case let .argument(arg):
      if arg.convention.isIndirectIn {
        return .argument(arg)
      }
      baseAddr = arg
    case let .storeBorrow(sbi):
      guard case let .stack(allocStack) = sbi.destinationOperand.value.accessBase else {
        return nil
      }
      return .store(initializingStore: sbi, initialAddress: allocStack)
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
  let baseAddress: Value
  let context: any Context

  var walkDownCache = WalkerCache<SmallProjectionPath>()

  var isProjected = false
  var initializer: AccessBase.Initializer?

  static func findSingleInitializer(ofAddress baseAddr: Value, context: some Context)
    -> AccessBase.Initializer? {

    var walker = AddressInitializationWalker(baseAddress: baseAddr, context)
    if walker.walkDownUses(ofAddress: baseAddr, path: SmallProjectionPath()) == .abortWalk {
      return nil
    }
    return walker.initializer
  }

  private init(baseAddress: Value, _ context: some Context) {
    self.baseAddress = baseAddress
    self.context = context
    if let arg = baseAddress as? FunctionArgument {
      assert(!arg.convention.isIndirectIn, "@in arguments cannot be initialized")
      if arg.convention.isInout {
        initializer = .argument(arg)
      }
      // @out arguments are initialized normally via stores.
    }
  }

  private mutating func setInitializer(instruction: Instruction) -> WalkResult {
    // An initializer must be unique and store the full value.
    if initializer != nil || isProjected {
      initializer = nil
      return .abortWalk
    }
    initializer = .store(initializingStore: instruction, initialAddress: baseAddress)
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

// Implement AddressUseVisitor
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

  mutating func loadedAddressUse(of operand: Operand, intoValue value: Value)
    -> WalkResult {
    return .continueWalk
  }

  mutating func loadedAddressUse(of operand: Operand, intoAddress address: Operand)
    -> WalkResult {
    return .continueWalk
  }

  mutating func yieldedAddressUse(of operand: Operand) -> WalkResult {
    // An inout yield is a partial write. Initialization via coroutine is not supported, so we assume a prior
    // initialization must dominate the yield.
    return .continueWalk
  }

  mutating func dependentAddressUse(of operand: Operand, dependentValue value: Value) -> WalkResult {
    return .continueWalk
  }

  mutating func dependentAddressUse(of operand: Operand, dependentAddress address: Value) -> WalkResult {
    return .continueWalk
  }

  mutating func escapingAddressUse(of operand: Operand) -> WalkResult {
    return .abortWalk
  }

  mutating func unknownAddressUse(of operand: Operand) -> WalkResult {
    return .abortWalk
  }
}

/// A live range representing the ownership of addressable memory.
///
/// This live range represents the minimal guaranteed lifetime of the object being addressed. Uses of derived addresses
/// may be extended up to the ends of this scope without violating ownership.
///
/// .liveOut objects (@in_guaranteed, @out) and .global variables have no instruction range.
///
/// .local objects (alloc_stack, yield, @in, @inout) report the single live range of the full assignment that reaches
/// this address.
///
/// .owned values (boxes and references) simply report OSSA liveness.
///
/// .borrow values report each borrow scope's range. The effective live range is their intersection. A valid use must
/// lie within all ranges.
///
/// FIXME: .borrow values should be represented with a single multiply-defined instruction range. Otherwise we will run
/// out of blockset bits as soon as we have multiple ranges (each range uses three sets). It is ok to take the
/// union of the borrow ranges since all address uses that may be extended will be already be dominated by the current
/// address. Alternatively, we can have a utility that folds two InstructionRanges together as an intersection, and
/// repeatedly fold the range of each borrow introducer.
///
/// Example:
///
///     %x = alloc_box
///     %b = begin_borrow %x -+ begin ownership range
///     %p = project_box %b   | <--- accessBase
///     %a = begin_access %p  |
///     end_access %a         | <--- address use
///     end_borrow %b        -+ end ownership range
///     destroy_value %x
///
/// This may return multiple ranges if a borrowed reference has multiple introducers:
///
///     %b1 = begin_borrow           -+        range1
///     %b2 = begin_borrow            |  -+ -+ range2
///     %s  = struct (%b1, %2)        |   |  |
///     %e  = struct_extract %s, #s.0 |   |  | intersection
///     %d = ref_element_addr %e      |   |  | where ownership
///     %a = begin_access %d          |   |  | is valid
///     end_access %a                 |   |  |
///     end_borrow %b1               -+   | -+
///     ...                               |
///     end_borrow %b2                   -+
///
/// Note: The resulting live range must be deinitialized in stack order.
enum AddressOwnershipLiveRange : CustomStringConvertible {
  case liveOut(FunctionArgument)
  case global(GlobalVariable)
  case local(Value, InstructionRange) // Value represents the local allocation
  case owned(Value, InstructionRange)
  case borrow(SingleInlineArray<(BeginBorrowValue, InstructionRange)>)

  mutating func deinitialize() {
    switch self {
    case .liveOut, .global:
      break
    case var .local(_, range):
      range.deinitialize()
    case var .owned(_, range):
      range.deinitialize()
    case var .borrow(ranges):
      for idx in ranges.indices {
        ranges[idx].1.deinitialize()
      }
    }
  }

  /// Return the live range of the addressable value that reaches 'begin', not including 'begin', which may itself be an
  /// access of the address.
  ///
  /// The range ends at the destroy or reassignment of the addressable value.
  ///
  /// Return nil if the live range is unknown.
  static func compute(for address: Value, at begin: Instruction,
                      _ localReachabilityCache: LocalVariableReachabilityCache,
                      _ context: FunctionPassContext) -> AddressOwnershipLiveRange? {
    let accessBase = address.accessBase
    switch accessBase {
    case .box, .class, .tail:
      return computeValueLiveRange(of: accessBase.reference!, context)
    case let .stack(allocStack):
      return computeLocalLiveRange(allocation: allocStack, begin: begin, localReachabilityCache, context)
    case let .global(global):
      return .global(global)
    case let .argument(arg):
      switch arg.convention {
      case .indirectInGuaranteed, .indirectOut:
        return .liveOut(arg)
      case .indirectIn, .indirectInout, .indirectInoutAliasable:
        return computeLocalLiveRange(allocation: arg, begin: begin, localReachabilityCache, context)
      default:
        return nil
      }
    case let .yield(result):
      let apply = result.parentInstruction as! BeginApplyInst
      switch apply.convention(of: result) {
      case .indirectInGuaranteed:
        var range = InstructionRange(for: address, context)
        _ = BorrowingInstruction(apply)!.visitScopeEndingOperands(context) {
          range.insert($0.instruction)
          return .continueWalk
        }
        return .local(result, range)
      case .indirectIn, .indirectInout, .indirectInoutAliasable:
        return computeLocalLiveRange(allocation: result, begin: begin, localReachabilityCache, context)
      default:
        return nil
      }
    case .storeBorrow(let sb):
      return computeValueLiveRange(of: sb.source, context)
    case .pointer, .index, .unidentified:
      return nil
    }
  }

  /// Does this inclusive range include `inst`, assuming that `inst` occurs after a definition of the live range.
  func coversUse(_ inst: Instruction) -> Bool {
    switch self {
    case .liveOut, .global:
      return true
    case let .local(_, range), let .owned(_, range):
      return range.inclusiveRangeContains(inst)
    case let .borrow(borrowRanges):
      for (_, range) in borrowRanges {
        if !range.inclusiveRangeContains(inst) {
          return false
        }
      }
      return true
    }
  }

  var description: String {
    switch self {
    case let .liveOut(arg):
      return "liveOut: \(arg)"
    case let .global(global):
      return "global: \(global)"
    case let .local(allocation, range):
      return "local: \(allocation)\n\(range)"
    case let .owned(value, range):
      return "owned: \(value)\n\(range)"
    case let .borrow(borrowRanges):
      var str = ""
      for (borrow, range) in borrowRanges {
         str += "borrow: \(borrow)\n\(range)"
      }
      return str
    }
  }
}

extension AddressOwnershipLiveRange {
  /// Compute the ownership live range of any non-address value.
  ///
  /// For an owned value, simply compute its liveness. For a guaranteed value, return a separate range for each borrow
  /// introducer.
  ///
  /// For address values, use AccessBase.computeOwnershipRange.
  ///
  /// FIXME: This should use computeLinearLiveness rather than computeKnownLiveness as soon as complete OSSA lifetimes
  /// are verified.
  private static func computeValueLiveRange(of value: Value, _ context: FunctionPassContext)
    -> AddressOwnershipLiveRange? {
    switch value.ownership {
    case .none, .unowned:
      // This is unexpected for a value with derived addresses.
      return nil
    case .owned:
      return .owned(value, computeKnownLiveness(for: value, context))
    case .guaranteed:
      return .borrow(computeBorrowLiveRange(for: value, context))
    }
  }

  private static func computeLocalLiveRange(allocation: Value, begin: Instruction,
                                            _ localReachabilityCache: LocalVariableReachabilityCache,
                                            _ context: some Context) -> AddressOwnershipLiveRange? {
    guard let localReachability = localReachabilityCache.reachability(for: allocation, context) else {
      return nil
    }
    var reachingAssignments = Stack<LocalVariableAccess>(context)
    defer { reachingAssignments.deinitialize() }

    if !localReachability.gatherReachingAssignments(for: begin, in: &reachingAssignments) {
      return nil
    }
    // Any one of the reaching assignment is sufficient to compute the minimal live range. The caller presumably only
    // cares about the live range that is dominated by 'begin'. Since all assignments gathered above reach
    // 'begin', their live ranges must all be identical on all paths through 'addressInst'.
    let assignment = reachingAssignments.first!

    var reachableUses = Stack<LocalVariableAccess>(context)
    defer { reachableUses.deinitialize() }

    localReachability.gatherKnownLifetimeUses(from: assignment, in: &reachableUses)

    let assignmentInst = assignment.instruction ?? allocation.parentFunction.entryBlock.instructions.first!
    var range = InstructionRange(begin: assignmentInst, context)
    for localAccess in reachableUses {
      if localAccess.kind == .escape {
        log("Local variable: \(allocation)\n    escapes at \(localAccess.instruction!)")
      }
      for end in localAccess.instruction!.endInstructions {
        range.insert(end)
      }
    }
    return .local(allocation, range)
  }
}

let addressOwnershipLiveRangeTest = FunctionTest("address_ownership_live_range") {
  function, arguments, context in
  let address = arguments.takeValue()
  let begin = arguments.takeInstruction()
  print("Address: \(address)")
  print("Base: \(address.accessBase)")
  print("Begin: \(begin)")
  let localReachabilityCache = LocalVariableReachabilityCache()
  guard var ownershipRange = AddressOwnershipLiveRange.compute(for: address, at: begin,
                                                               localReachabilityCache, context) else {
    print("Error: indeterminate live range")
    return
  }
  defer { ownershipRange.deinitialize() }
  print(ownershipRange)
}
