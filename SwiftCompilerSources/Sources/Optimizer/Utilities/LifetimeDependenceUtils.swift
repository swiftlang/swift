//===--- LifetimeDependenceUtils.swift - Utils for lifetime dependence ----===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Utilities that specify lifetime dependence:
//
// gatherVariableIntroducers(for:) is a use-def walk that returns the values that most closely associated with the
// variable declarations that the given value holds an instance of.
//
// LifetimeDependence.init models the lifetime dependence for a FunctionArgument or a MarkDependenceInst, categorizing
// the kind of dependence scope that the lifetime represents.
//
// LifetimeDependence.Scope.computeRange() computes the instruction range covered by a dependence scope.
//
// LifetimeDependenceDefUseWalker walks the def-use chain to find all values that depend on the given OSSA lifetime.
//
//===----------------------------------------------------------------------===//

import AST
import SIL

private let verbose = false

private func log(prefix: Bool = true, _ message: @autoclosure () -> String) {
  if verbose {
    debugLog(prefix: prefix, message())
  }
}

/// A lifetime dependence represents a scope in which some parent value is alive and accessible along with a dependent
/// value. All values derived from the dependent value must be used within this scope. This supports diagnostics on
/// non-copyable and non-escapable types.
///
/// A lifetime dependence is produced by either 'mark_dependence [nonescaping]':
///
///   %dependent = mark_dependence [nonescaping] %value on %parent
///
/// or a non-escapable function argument:
///
///   bb0(%dependent : NonEscapableThing):
///
/// A lifetime dependence identifies its parent value, the kind of scope that the parent value represents, and a
/// dependent value.
struct LifetimeDependence : CustomStringConvertible {
  enum Scope : CustomStringConvertible {
    /// A guaranteed or inout argument whose scope is provided by the caller
    /// and covers the entire function and any dependent results or yields.
    case caller(FunctionArgument)
    /// An access scope.
    case access(BeginAccessInst)
    /// A coroutine.
    case yield(Value)
    /// An owned value whose OSSA lifetime encloses nonescapable values, or a trivial variable introduced by move_value.
    case owned(Value)
    /// A borrowed value whose OSSA lifetime encloses nonescapable values, or a trivial variable introduced by
    /// begin_borrow.
    case borrowed(BeginBorrowValue)
    /// A local variable scope without ownership. The scope must be introduced by either move_value or
    /// begin_borrow. LinearLiveness computes the scope boundary.
    case local(VariableScopeInstruction)
    /// A directly accessed global variable without exclusivity. Presumably immutable and therefore immortal.
    case global(GlobalAccessBase)
    /// Singly-initialized addressable storage (likely for an immutable address-only value). The lifetime extends until
    /// the memory is destroyed. e.g. A value produced by an @in FunctionArgument, an @out apply, or an @inout
    /// FunctionArgument that is never modified inside the callee. Modified @inout FunctionArguments have caller scoped
    /// instead, and a separate analysis diagnoses mutation after the dependence is formed.
    case initialized(AccessBase.Initializer)
    // Unknown includes: stack allocations that are not singly initialized, and globals that don't have an access scope.
    case unknown(Value)

    var parentValue: Value {
      switch self {
      case let .caller(argument): return argument
      case let .access(beginAccess): return beginAccess
      case let .yield(value): return value
      case let .owned(value): return value
      case let .borrowed(beginBorrow): return beginBorrow.value
      case let .local(varScope): return varScope.scopeBegin
      case let .global(ga): return ga.address
      case let .initialized(initializer): return initializer.initialAddress
      case let .unknown(value): return value
      }
    }

    func checkPrecondition() {
      switch self {
      case let .caller(argument):
        precondition(argument.ownership != .owned, "only guaranteed or inout arguments have a caller scope")
      case .access, .local, .global, .unknown:
        break        
      case let .yield(value):
        precondition(value.definingInstruction is BeginApplyInst)
      case let .owned(value):
        precondition(value.ownership == .owned)
      case let .borrowed(beginBorrow):
        precondition(beginBorrow.value.ownership == .guaranteed)
      case let .initialized(initializer):
        let initialAddress = initializer.initialAddress
        precondition(initialAddress.type.isAddress, "expected an address")
        precondition(initialAddress is AllocStackInst || initialAddress is FunctionArgument,
                     "expected storage for a a local 'let'")
        if case let .store(store, _) = initializer {
          precondition(store is StoringInstruction || store is SourceDestAddrInstruction || store is FullApplySite,
                       "expected a store")
        }
      }
    }
    var description: String {
      {
        switch self {
        case .caller: return "Caller: "
        case .access: return "Access: "
        case .yield: return "Yield: "
        case .owned: return "Owned: "
        case .borrowed: return "Borrowed: "
        case .local: return "Local: "
        case .global: return "Global: "
        case .initialized: return "Initialized: "
        case .unknown: return "Unknown: "
        }
      }() + "\(parentValue)"
    }
  }
  let scope: Scope
  let dependentValue: Value
  
  var parentValue: Value { scope.parentValue }

  var function: Function {
    dependentValue.parentFunction
  }

  var description: String {
    return scope.description + "\nDependent: \(dependentValue)"
  }
}

extension LifetimeDependence {
  /// Construct LifetimeDependence from a function argument.
  ///
  /// Returns 'nil' for indirect results.
  init?(_ arg: FunctionArgument, _ context: some Context) {
    if arg.isIndirectResult {
      return nil
    }
    self.scope = Scope(base: arg, context)
    self.dependentValue = arg
  }

  // Construct a LifetimeDependence from a return value. This only
  // constructs a dependence for ~Escapable results that do not have a
  // lifetime dependence (@_unsafeNonescapableResult).
  //
  // This is necessary because inserting a mark_dependence placeholder for such an unsafe dependence would illegally
  // have the same base and value operand.
  //
  // TODO: handle indirect results
  init?(unsafeApplyResult value: Value, _ context: some Context) {
    if value.isEscapable {
      return nil
    }
    if (value.definingInstructionOrTerminator as! FullApplySite).hasResultDependence {
      return nil
    }
    assert(value.ownership == .owned, "unsafe apply result must be owned")
    self.scope = Scope(base: value, context)
    self.dependentValue = value
  }

  /// Construct LifetimeDependence from mark_dependence [unresolved] or mark_dependence [nonescaping].
  ///
  /// For any LifetimeDependence constructed from a mark_dependence, its `dependentValue` will be the result of the
  /// mark_dependence.
  ///
  /// Returns 'nil' for unknown dependence.
  init?(_ markDep: MarkDependenceInst, _ context: some Context) {
    switch markDep.dependenceKind {
    case .Unresolved, .NonEscaping:
      self.scope = Scope(base: markDep.base, context)
      self.dependentValue = markDep
    case .Escaping:
      return nil
    }
  }  

  /// Compute the range of the dependence scope.
  ///
  /// Returns nil if the dependence scope covers the entire function
  /// or if 'dependentValue' is part of the parent's forwarded
  /// lifetime.
  ///
  /// Note: The caller must deinitialize the returned range.
  func computeRange(_ context: Context) -> InstructionRange? {
    if dependentValue.isForwarded(from: parentValue) {
      return nil
    }
    return scope.computeRange(context)
  }

  func resolve(_ context: some Context) {
    if let mdi = dependentValue as? MarkDependenceInst {
      mdi.resolveToNonEscaping()
    }
  }
}

// Scope initialization.
extension LifetimeDependence.Scope {
  /// Construct a lifetime dependence scope from the base value that other values depend on. This derives the kind of
  /// dependence scope and its parentValue from `base`.
  ///
  /// The returned Scope must be the only scope for the given 'base' value.  This is generally non-recursive, except
  /// that finds the single borrow introducer. Use-def walking is handled by a utility such as
  /// VariableIntroducerUseDefWalker, which can handle multiple introducers.
  ///
  /// `base` represents the OSSA lifetime that the dependent value must be used within. If `base` is owned, then it
  /// directly defines the parent lifetime. If `base` is guaranteed, then it must have a single borrow introducer, which
  /// defines the parent lifetime. `base` must not be derived from a guaranteed phi or forwarded (via struct/tuple) from
  /// multiple guaranteed values.
  init(base: Value, _ context: some Context) {
    if base.type.isAddress {
      self.init(enclosingAccess: base.enclosingAccessScope, address: base, context)
      return
    }
    switch base.ownership {
    case .owned:
      self = .owned(base)
    case .guaranteed:
      self.init(guaranteed: base, context)
    case .none:
      self.init(trivial: base, context)
    case .unowned:
      self = .unknown(base)
    }
  }

  init(enclosingAccess: EnclosingAccessScope, address: Value, _ context: some Context) {
    switch enclosingAccess {
    case let .access(access):
      self = .access(access)
    case let .base(accessBase):
      self.init(accessBase: accessBase, address: address, context)
    case let .dependence(markDep):
      self = .unknown(markDep)
    }
  }

  init(accessBase: AccessBase, address: Value, _ context: some Context) {
    switch accessBase {
    case let .box(projectBox):
      // Note: the box may be in a borrow scope.
      self.init(base: projectBox.operand.value, context)
    case .stack, .class, .tail, .pointer, .index, .unidentified:
      self = .unknown(accessBase.address!)
    case .global:
      // TODO: When AccessBase directly stores GlobalAccessBase, we don't need a check here and don't need to pass
      // 'address' in to this function.
      if let ga = GlobalAccessBase(address: address) {
        self = .global(ga)
      } else {
        self = .unknown(accessBase.address ?? address)
      }
    case let .argument(arg):
      if arg.convention.isIndirectIn {
        if arg.convention.isGuaranteed {
          // The entire scope is in the caller.
          self = .caller(arg)
        } else {
          // The argument's lifetime ends locally, so model it as a local scope initialized by the argument.
          self = .initialized(.argument(arg))
        }
      } else if arg.convention.isIndirectOut || arg.convention.isInout {
        // @out arguments belong to the caller because they can never be reassigned. If they were locally mutable, then
        // the dependence would be on an access scope instead.
        //
        // @inout arguments belong to the caller because, if local mutation or reassignment were relevant, then the
        // dependence would be on an access scope instead. For example, the dependence may have a trivial type, or it
        // the argument may never be mutated in the function even though it is @inout.
        self = .caller(arg)
      } else {
        self = .unknown(address)
      }
    case let .yield(result):
      self.init(yield: result)
    case .storeBorrow(let sb):
      self = Self(base: sb.source, context)
    }
  }

  // TODO: Add a SIL verifier check that a mark_dependence [nonescaping]
  // base is never a guaranteed phi and that reborrows are never introduced for mark_dependence [nonescaping].
  //
  // TODO: If we need to handle guaranteed tuple/struct with multiple scopes, replace
  // LifetimeDependenceself.init(markDep) with a gatherDependenceScopes() utility used by both
  // LifetimeDependenceInsertion and LifetimeDependenceScopeFixup.
  private init(guaranteed base: Value, _ context: some Context) {
    var iter = base.getBorrowIntroducers(context).makeIterator()
    // If no borrow introducer was found, then this is a borrow of a trivial value. Since we can assume a single
    // introducer here, then this is the only condition under which we have a trivial introducer.
    guard let beginBorrow = iter.next() else {
      self.init(trivial: base, context)
      return
    }
    assert(iter.next() == nil,
           "guaranteed phis not allowed when diagnosing lifetime dependence")
    switch beginBorrow {
    case .beginBorrow, .loadBorrow:
      self = .borrowed(beginBorrow)
    case let .beginApply(value):
      self = .yield(value)
    case let .functionArgument(arg):
      self = .caller(arg)
    case .uncheckOwnershipConversion:
      self = .unknown(base)
    case .reborrow:
      // Reborrows do not happen during diagnostics, but this utility may be called anywhere. The optimizer should avoid
      // creating a reborrow for a mark_dependence [nonescaping]; it could violate ownership unless we can handle all
      // reborrowed inputs. It is easier to prevent this.
      fatalError("reborrows are not supported in diagnostics")
    }
  }

  private init(trivial value: Value, _ context: some Context) {
    if let arg = value as? FunctionArgument {
      self = .caller(arg)
    } else if let varScope = VariableScopeInstruction(value.definingInstruction) {
      self = .local(varScope)
    } else if let ga = GlobalAccessBase(address: value) {
      self = .global(ga)
    } else {
      self = .unknown(value)
    }
  }

  private init(yield result: MultipleValueInstructionResult) {
    // Consider an @in yield an .initialized scope because we must find the destroys.
    let apply = result.parentInstruction as! FullApplySite
    if apply.convention(of: result).isIndirectIn {
      self = .initialized(.yield(result))
      return
    }
    self = .yield(result)
  }
}

extension LifetimeDependence.Scope {
  /// Ignore "irrelevent" borrow scopes: load_borrow or begin_borrow without [var_decl]
  func ignoreBorrowScope(_ context: some Context) -> LifetimeDependence.Scope? {
    guard case let .borrowed(beginBorrowVal) = self else {
      return nil
    }
    switch beginBorrowVal {
    case let .beginBorrow(bb):
      if bb.isFromVarDecl {
        return nil
      }
      return LifetimeDependence.Scope(base: bb.borrowedValue, context).ignoreBorrowScope(context)
    case let .loadBorrow(lb):
      return LifetimeDependence.Scope(base: lb.address, context)
    default:
      fatalError("Scope.borrowed must begin begin_borrow or load_borrow")
    }
  }
}

extension LifetimeDependence.Scope {
  /// Compute the range of the dependence scope. 
  ///
  /// Returns nil if the dependence scope covers the entire function. Returns an empty range for an unknown scope.
  ///
  /// Note: The caller must deinitialize the returned range.
  func computeRange(_ context: Context) -> InstructionRange? {
    switch self {
    case .caller, .global:
      return nil
    case let .access(beginAccess):
      var range = InstructionRange(begin: beginAccess, context)
      range.insert(contentsOf: beginAccess.endInstructions)
      return range
    case let .yield(value):
      // This assumes @inout or @in_guaranteed convention.
      let def = value.definingInstruction!
      var range = InstructionRange(begin: def, context)
      _ = BorrowingInstruction(def)!.visitScopeEndingOperands(context) {
        range.insert($0.instruction)
        return .continueWalk
      }
      return range
    case let .owned(value):
      // Note: This could compute forwarded liveness instead of linear
      // liveness. That would be more forgiving for copies. But, then
      // how would we ensure that the borrowed mark_dependence value
      // is within this value's OSSA lifetime?
      return computeLinearLiveness(for: value, context)
    case let .borrowed(beginBorrow):
      return computeLinearLiveness(for: beginBorrow.value, context)
    case let .local(varScope):
      var range = InstructionRange(for: varScope.scopeBegin, context)
      // Compute liveness.
      for use in varScope.endOperands {
        range.insert(use.instruction)
      }
      return range
    case let .initialized(initializer):
      if case let .argument(arg) = initializer, arg.convention.isInout {
        return nil
      }
      return LifetimeDependence.Scope.computeInitializedRange(initializer: initializer, context)
    case let .unknown(value):
      // Ignore the lifetime of temporary trivial values. Temporaries have an unknown Scope, which means that
      // LifetimeDependence.Scope did not recognize a VariableScopeInstruction. This is important to promote
      // mark_dependence instructions emitted by SILGen to [nonescaping] (e.g. unsafeAddressor). It also allows trivial
      // value to be "extended" without actually tracking their scope, which is expected behavior. For example:
      //
      //     let span = Span(buffer.baseAddress)
      //
      // If 'baseAddress' is an opaque getter, then the temporary pointer is effectively valid
      // until the end of the function.
      if value.type.isTrivial(in: value.parentFunction) {
        return nil
      }
      // Return an empty range.
      return InstructionRange(for: value, context)
    }
  }

  // Note: an initialized range should always have a destroy_addr. For trivial 'var' variables, we have a alloc_box,
  // which has a destroy_value. For concrete trivial 'let' variables, we load the trivial value:
  //   %l = load [trivial] %0
  //   %m = move_value [var_decl] %2
  //
  // For generic trivial (BitwiseCopyable) 'let' variables, we emit a destroy_addr for the alloc_stack.
  private static func computeInitializedRange(initializer: AccessBase.Initializer, _ context: Context)
    -> InstructionRange {

    let initialAddress = initializer.initialAddress
    var range: InstructionRange
    switch initializer {
    case .argument, .yield:
      range = InstructionRange(for: initialAddress, context)
    case let .store(initializingStore, _):
      range = InstructionRange(begin: initializingStore, context)
    }
    for use in initialAddress.uses {
      let inst = use.instruction
      switch inst {
      case is DestroyAddrInst:
        range.insert(inst)
      case let sdai as SourceDestAddrInstruction
             where sdai.sourceOperand == use && sdai.isTakeOfSrc:
        range.insert(inst)
      case let li as LoadInst where li.loadOwnership == .take:
        range.insert(inst)
      default:
        break
      }
    }
    return range
  }
}

// =============================================================================
// LifetimeDependenceDefUseWalker - downward walk
// =============================================================================

/// Walk down dependent values.
///
/// First classifies all values using OwnershipUseVisitor. Delegates forwarding uses to the ForwardingUseDefWalker.
/// Transitively follows OwnershipTransitionInstructions (copy, move, borrow, and mark_dependence).  Transitively
/// follows interior pointers using AddressUseVisitor. Handles stores to and loads from local variables using
/// LocalVariableReachabilityCache.
///
/// Ignores trivial values (~Escapable types are never trivial. Escapable types may only be lifetime-dependent values if
/// they are non-trivial).
///
/// Skips uses within nested borrow scopes.
///
/// Minimal requirements:
///   needWalk(for value: Value) -> Bool
///   leafUse(of: Operand) -> WalkResult
///   deadValue(_ value: Value, using operand: Operand?) -> WalkResult
///   escapingDependence(on operand: Operand) -> WalkResult
///   inoutDependence(argument: FunctionArgument, on: Operand) -> WalkResult
///   returnedDependence(result: Operand) -> WalkResult
///   returnedDependence(address: FunctionArgument, on: Operand) -> WalkResult
///   yieldedDependence(result: Operand) -> WalkResult
/// Start walking:
///   walkDown(root: Value)
///
/// Note: this may visit values that are not dominated by `root` because of dependent phi operands.
protocol LifetimeDependenceDefUseWalker : ForwardingDefUseWalker,
                                          OwnershipUseVisitor,
                                          AddressUseVisitor {
  var function: Function { get }

  /// Dependence tracking through local variables.
  var localReachabilityCache: LocalVariableReachabilityCache { get }

  mutating func leafUse(of operand: Operand) -> WalkResult

  mutating func escapingDependence(on operand: Operand) -> WalkResult

  // Assignment to an inout argument. This does not include the indirect out result, which is considered a return
  // value.
  mutating func inoutDependence(argument: FunctionArgument, on: Operand) -> WalkResult

  mutating func returnedDependence(result: Operand) -> WalkResult

  mutating func returnedDependence(address: FunctionArgument, on: Operand) -> WalkResult

  mutating func yieldedDependence(result: Operand) -> WalkResult

  mutating func storeToYieldDependence(address: Value, of operand: Operand) -> WalkResult
}

extension LifetimeDependenceDefUseWalker {
  // Use a distict context name to avoid rdar://123424566 (Unable to open existential)
  var walkerContext: Context { context }
}

// Start a forward walk.
extension LifetimeDependenceDefUseWalker {
  mutating func walkDown(root: Value) -> WalkResult {
    if root.type.isAddress {
      if let md = root as? MarkDependenceInst, !root.isEscapable {
        // LifetimeDependence.dependentValue is typically a mark_dependence. If its 'value' address is a non-Escapable
        // local variable, then consider all other reachable uses of that local variable to be dependent uses. Remember
        // the operand to the mark_dependence as if it was a store. Diagnostics will consider this the point of variable
        // initialization.
        if visitStoredUses(of: md.valueOperand, into: md.value) == .abortWalk {
          return .abortWalk
        }
      }
      // The root address may also be an escapable mark_dependence that guards its address uses (unsafeAddress), or an
      // allocation or incoming argument. In all these cases, it is sufficient to walk down the address uses.
      return walkDownAddressUses(of: root)
    }
    return walkDownUses(of: root, using: nil)
  }
}

// Implement ForwardingDefUseWalker
extension LifetimeDependenceDefUseWalker {
  // Override ForwardingDefUseWalker.
  mutating func walkDownUses(of value: Value, using operand: Operand?)
    -> WalkResult {
    // Only track ~Escapable and @noescape types.
    if value.mayEscape {
      return .continueWalk
    }
    return walkDownUsesDefault(forwarding: value, using: operand)
  }

  // Override ForwardingDefUseWalker.
  mutating func walkDown(operand: Operand) -> WalkResult {
    // Initially delegate all usess to OwnershipUseVisitor.
    // walkDownDefault will be called for uses that forward ownership.
    return classify(operand: operand)
  }

  // Callback from (a) ForwardingDefUseWalker or (b) ownershipLeafUse.
  //
  // (a) OwnershipUseVisitor classified this as a forwardingUse(), but
  // ForwardingDefUseWalker does not recognize it as a forwarding
  // instruction. This includes apply arguments.
  //
  // (b) OwnershipUseVisitor classified this as a ownershipLeafUse(), but
  // it was not a recognized copy, destroy, or instantaneous use.
  mutating func nonForwardingUse(of operand: Operand) -> WalkResult {
    if let apply = operand.instruction as? FullApplySite {
      return visitAppliedUse(of: operand, by: apply)
    }
    if operand.instruction is ReturnInst, !operand.value.isEscapable {
      return returnedDependence(result: operand)
    }
    if operand.instruction is YieldInst, !operand.value.isEscapable {
      return yieldedDependence(result: operand)
    }
    return escapingDependence(on: operand)
  }
}

// Implement OwnershipUseVisitor
extension LifetimeDependenceDefUseWalker {
  // Handle uses that do not propagate the OSSA lifetime. They may still
  // copy the value, which propagates the dependence.
  mutating func ownershipLeafUse(of operand: Operand, isInnerLifetime: Bool)
    -> WalkResult {
    if operand.ownership == .endBorrow {
      // Record the leaf use here because, in some cases, like
      // begin_apply, we have skipped the inner uses.
      return leafUse(of: operand)
    }
    switch operand.instruction {
    case let transition as OwnershipTransitionInstruction:
      return walkDownUses(of: transition.ownershipResult, using: operand)

    case let mdi as MarkDependenceInst where mdi.isUnresolved:
      // Override mark_dependence [unresolved] to handle them just
      // like [nonescaping] even though they are not considered OSSA
      // borrows until after resolution.
      assert(operand == mdi.baseOperand)
      return dependentUse(of: operand, into: mdi)

    case is ExistentialMetatypeInst, is FixLifetimeInst, is WitnessMethodInst,
         is DynamicMethodBranchInst, is ValueMetatypeInst,
         is DestroyNotEscapedClosureInst, is ClassMethodInst, is SuperMethodInst,
         is ClassifyBridgeObjectInst, is DebugValueInst,
         is ObjCMethodInst, is ObjCSuperMethodInst, is UnmanagedRetainValueInst,
         is UnmanagedReleaseValueInst, is SelectEnumInst:
      // Catch .instantaneousUse operations that are dependence leaf uses.
      return leafUse(of: operand)

    case is DestroyValueInst, is EndLifetimeInst, is DeallocRefInst,
         is DeallocBoxInst, is DeallocExistentialBoxInst,
         is BeginCOWMutationInst, is EndCOWMutationInst,
         is EndInitLetRefInst, is DeallocPartialRefInst, is BeginDeallocRefInst:
      // Catch .destroyingConsume operations that are dependence leaf
      // uses.
      return leafUse(of: operand)

    case let si as StoringInstruction where si.sourceOperand == operand:
      return visitStoredUses(of: operand, into: si.destinationOperand.value)

    case let tai as TupleAddrConstructorInst:
      return visitStoredUses(of: operand, into: tai.destinationOperand.value)

    default:
      return nonForwardingUse(of: operand)
    }
  }

  mutating func forwardingUse(of operand: Operand, isInnerLifetime: Bool)
    -> WalkResult {
    // Delegate ownership forwarding operations to the ForwardingDefUseWalker.
    return walkDownDefault(forwarding: operand)
  }

  mutating func interiorPointerUse(of: Operand, into address: Value)
    -> WalkResult {
    return walkDownAddressUses(of: address)
  }

  mutating func pointerEscapingUse(of operand: Operand) -> WalkResult {
    return escapingDependence(on: operand)
  }

  mutating func dependentUse(of operand: Operand, into value: Value)
    -> WalkResult {
    return walkDownUses(of: value, using: operand)
  }

  mutating func borrowingUse(of operand: Operand,
                             by borrowInst: BorrowingInstruction)
    -> WalkResult {
    return visitAllBorrowUses(of: operand, by: borrowInst)
  }

  // TODO: Consider supporting lifetime dependence analysis of
  // guaranteed phis. See InteriorUseWalker.walkDown(guaranteedPhi: Phi)
  mutating func reborrowingUse(of operand: Operand, isInnerLifetime: Bool)
    -> WalkResult {
    return escapingDependence(on: operand)
  }
}

// Implement AddressUseVisitor
extension LifetimeDependenceDefUseWalker {
  /// An address projection produces a single address result and does not
  /// escape its address operand in any other way.
  mutating func projectedAddressUse(of operand: Operand, into value: Value)
    -> WalkResult {
    return walkDownAddressUses(of: value)
  }

  mutating func scopedAddressUse(of operand: Operand) -> WalkResult {
    switch operand.instruction {
    case let ba as BeginAccessInst:
      return walkDownAddressUses(of: ba)
    case let ba as BeginApplyInst:
      // visitAppliedUse only calls scopedAddressUse for ~Escapable
      // arguments that are not propagated to a result. Here, we only
      // care about the scope of the call.
      return ba.token.uses.walk { leafUse(of: $0) }
    case is StoreBorrowInst:
      return leafUse(of: operand)
    case let load as LoadBorrowInst:
      return walkDownUses(of: load, using: operand)
    default:
      fatalError("Unrecognized scoped address use: \(operand.instruction)")
    }
  }

  mutating func scopeEndingAddressUse(of operand: Operand) -> WalkResult {
    return leafUse(of: operand)
  }

  // Includes StoringInstruction.
  mutating func leafAddressUse(of operand: Operand) -> WalkResult {
    return leafUse(of: operand)
  }

  mutating func appliedAddressUse(of operand: Operand,
                                  by apply: FullApplySite) -> WalkResult {
    return visitAppliedUse(of: operand, by: apply)
  }

  mutating func loadedAddressUse(of operand: Operand, into value: Value)
    -> WalkResult {
    // Record the load itself, in case the loaded value is Escapable.
    if leafUse(of: operand) == .abortWalk {
      return .abortWalk
    }
    return walkDownUses(of: value, using: operand)
  }    

  mutating func loadedAddressUse(of operand: Operand, into address: Operand)
    -> WalkResult {
    if leafUse(of: operand) == .abortWalk {
      return .abortWalk
    }
    return visitStoredUses(of: operand, into: address.value)
  }

  mutating func yieldedAddressUse(of operand: Operand) -> WalkResult {
    if operand.value.isEscapable {
      return leafUse(of: operand)
    } else {
      return yieldedDependence(result: operand)
    }
  }

  mutating func dependentAddressUse(of operand: Operand, into value: Value)
    -> WalkResult {
    walkDownUses(of: value, using: operand)
  }    

  mutating func escapingAddressUse(of operand: Operand) -> WalkResult {
    if let mdi = operand.instruction as? MarkDependenceInst {
      assert(!mdi.isUnresolved && !mdi.isNonEscaping,
             "should be handled as a dependence by AddressUseVisitor")
    }
    // Escaping an address
    return escapingDependence(on: operand)
  }

  mutating func unknownAddressUse(of operand: Operand) -> WalkResult {
    return .abortWalk
  }

  private mutating func walkDownAddressUses(of address: Value) -> WalkResult {
    address.uses.ignoreTypeDependence.walk {
      return classifyAddress(operand: $0)
    }
  }
}

// Helpers
extension LifetimeDependenceDefUseWalker {
  // Visit uses of borrowing instruction (operandOwnerhip == .borrow).
  private mutating func visitAllBorrowUses(
    of operand: Operand, by borrowInst: BorrowingInstruction) -> WalkResult {
    switch borrowInst {
    case let .beginBorrow(bbi):
      return walkDownUses(of: bbi, using: operand)
    case let .borrowedFrom(bfi):
      return walkDownUses(of: bfi, using: operand)
    case let .storeBorrow(sbi):
      return walkDownAddressUses(of: sbi)
    case .beginApply:
      // Skip the borrow scope; the type system enforces non-escapable
      // arguments.
      return visitInnerBorrowUses(of: borrowInst, operand: operand)
    case .partialApply, .markDependence:
      fatalError("OwnershipUseVisitor should bypass partial_apply [on_stack] "
                 + "and mark_dependence [nonescaping]")
    case .startAsyncLet:
      // TODO: If async-let takes a non-escaping closure, we can
      // consider it nonescaping and visit the endAsyncLetLifetime
      // uses here.
      return escapingDependence(on: operand)
    }
  }

  // Visit stores to a local variable (alloc_box), temporary storage (alloc_stack). This handles stores of the entire
  // value and stores to a tuple element. Stores to a field within another nominal value are considered lifetime
  // dependence leaf uses; the type system enforces non-escapability on the aggregate value.
  //
  // If 'operand' is an address, then the "store" corresponds to initialization via an @out argument. The initial
  // call to visitStoredUses will have 'operand == address' where the "stored value" is the temporary stack
  // allocation for the @out parameter.
  private mutating func visitStoredUses(of operand: Operand,
                                        into address: Value) -> WalkResult {
    assert(address.type.isAddress)

    var allocation: Value?
    switch address.accessBase {
    case let .box(projectBox):
      allocation = projectBox.box.referenceRoot
    case let .stack(allocStack):
      allocation = allocStack
    case let .argument(arg):
      if arg.convention.isIndirectIn || arg.convention.isInout {
        allocation = arg
      } else if arg.convention.isIndirectOut, !arg.isEscapable {
        return returnedDependence(address: arg, on: operand)
      }
      break
    case .yield:
      return storeToYieldDependence(address: address, of: operand)
    case .global, .class, .tail, .storeBorrow, .pointer, .index, .unidentified:
      // An address produced by .storeBorrow should never be stored into.
      //
      // TODO: allow storing an immortal value into a global.
      break
    }
    if let allocation = allocation {
      if !allocation.isEscapable {
        return visitLocalStore(allocation: allocation, storedOperand: operand, storeAddress: address)
      }
    }
    if address.isEscapable {
      return .continueWalk
    }
    return escapingDependence(on: operand)
  }

  private mutating func visitLocalStore(allocation: Value, storedOperand: Operand, storeAddress: Value) -> WalkResult {
    guard let localReachability = localReachabilityCache.reachability(for: allocation, walkerContext) else {
      return escapingDependence(on: storedOperand)
    }
    var accessStack = Stack<LocalVariableAccess>(walkerContext)
    defer { accessStack.deinitialize() }

    // Get the local variable access that encloses this store.
    var storeAccess = storedOperand.instruction
    if case let .access(beginAccess) = storeAddress.enclosingAccessScope {
      storeAccess = beginAccess
    }
    if !localReachability.gatherAllReachableUses(of: storeAccess, in: &accessStack) {
      return escapingDependence(on: storedOperand)
    }
    for localAccess in accessStack {
      if visitLocalAccess(allocation: allocation, localAccess: localAccess, initialValue: storedOperand) == .abortWalk {
        return .abortWalk
      }
    }
    return .continueWalk
  }

  private mutating func visitLocalAccess(allocation: Value, localAccess: LocalVariableAccess, initialValue: Operand)
    -> WalkResult {
    switch localAccess.kind {
    case .beginAccess:
      return scopedAddressUse(of: localAccess.operand!)
    case .load:
      switch localAccess.instruction! {
      case let load as LoadInst:
        return loadedAddressUse(of: localAccess.operand!, into: load)
      case let load as LoadBorrowInst:
        return loadedAddressUse(of: localAccess.operand!, into: load)
      case let copyAddr as SourceDestAddrInstruction:
        return loadedAddressUse(of: localAccess.operand!, into: copyAddr.destinationOperand)
      default:
        return .abortWalk
      }
    case .dependence:
       // An address-forwarding mark_dependence is simply a marker that indicates the start of an in-memory
       // dependent value. Typically, it has no uses. If it does have uses, then they are visited earlier by
       // LocalVariableAccessWalker to record any other local accesses.
       return .continueWalk
    case .store:
      let si = localAccess.operand!.instruction as! StoringInstruction
      assert(si.sourceOperand == initialValue, "the only reachable store should be the current assignment")
    case .apply:
      return visitAppliedUse(of: localAccess.operand!, by: localAccess.instruction as! FullApplySite)
    case .escape:
      log("Local variable: \(allocation)\n    escapes at: \(localAccess.instruction!)")
      return escapingDependence(on: localAccess.operand!)
    case .outgoingArgument:
      let arg = allocation as! FunctionArgument
      assert(arg.type.isAddress, "returned local must be allocated with an indirect argument")
      return inoutDependence(argument: arg, on: initialValue)
    case .inoutYield:
      return yieldedDependence(result: localAccess.operand!)
    case .incomingArgument:
      fatalError("Incoming arguments are never reachable")
    }
    return .continueWalk
  }

  private mutating func visitAppliedUse(of operand: Operand, by apply: FullApplySite) -> WalkResult {
    if let conv = apply.convention(of: operand), conv.isIndirectOut {
      return leafUse(of: operand)
    }
    if apply.isCallee(operand: operand) {
      return leafUse(of: operand)
    }
    if let dep = apply.resultDependence(on: operand),
       dep == .inherit {
      // Operand is nonescapable and passed as a call argument. If the
      // result inherits its lifetime, then consider any nonescapable
      // result value to be a dependent use.
      //
      // If the lifetime dependence is scoped, then we can ignore it
      // because a mark_dependence [nonescaping] represents the
      // dependence.
      if let result = apply.singleDirectResult, !result.isEscapable {
        if dependentUse(of: operand, into: result) == .abortWalk {
          return .abortWalk
        }
      }
      for resultAddr in apply.indirectResultOperands
          where !resultAddr.value.isEscapable {
        if visitStoredUses(of: operand, into: resultAddr.value) == .abortWalk {
          return .abortWalk
        }
      }
    }
    // Regardless of lifetime dependencies, consider the operand to be
    // use for the duration of the call.
    if apply is BeginApplyInst {
      return scopedAddressUse(of: operand)
    }
    return leafUse(of: operand)
  }
}

let lifetimeDependenceScopeTest = FunctionTest("lifetime_dependence_scope") {
    function, arguments, context in
  let markDep = arguments.takeValue() as! MarkDependenceInst
  guard let dependence = LifetimeDependence(markDep, context) else {
    print("Invalid Dependence")
    return
  }
  print(dependence)
  guard var range = dependence.scope.computeRange(context) else {
    print("Caller range")
    return
  }
  defer { range.deinitialize() }
  print(range)
}

private struct LifetimeDependenceUsePrinter : LifetimeDependenceDefUseWalker {
  let context: Context
  let function: Function
  let localReachabilityCache = LocalVariableReachabilityCache()
  var visitedValues: ValueSet
  
  init(function: Function, _ context: Context) {
    self.context = context
    self.function = function
    self.visitedValues = ValueSet(context)
  }
  
  mutating func deinitialize() {
    visitedValues.deinitialize()
  }

  mutating func needWalk(for value: Value) -> Bool {
    visitedValues.insert(value)
  }

  mutating func deadValue(_ value: Value, using operand: Operand?)
    -> WalkResult {
    print("Dead value: \(value)")
    return .continueWalk
  }

  mutating func leafUse(of operand: Operand) -> WalkResult {
    print("Leaf use: \(operand)")
    return .continueWalk
  }

  mutating func escapingDependence(on operand: Operand) -> WalkResult {
    print("Escaping use: \(operand)")
    return .continueWalk
  }

  mutating func inoutDependence(argument: FunctionArgument, on operand: Operand) -> WalkResult {
    print("Out use: \(operand) in: \(argument)")
    return .continueWalk
  }

  mutating func returnedDependence(result: Operand) -> WalkResult {
    print("Returned use: \(result)")
    return .continueWalk
  }

  mutating func returnedDependence(address: FunctionArgument,
                                   on operand: Operand) -> WalkResult {
    print("Returned use: \(operand) in: \(address)")
    return .continueWalk
  }

  mutating func yieldedDependence(result: Operand) -> WalkResult {
    print("Yielded use: \(result)")
    return .continueWalk
  }

  mutating func storeToYieldDependence(address: Value, of operand: Operand) -> WalkResult {
    print("Store to yield use: \(operand) to: \(address)")
    return .continueWalk
  }
}

let lifetimeDependenceUseTest = FunctionTest("lifetime_dependence_use") {
    function, arguments, context in
  let value = arguments.takeValue()
  print("LifetimeDependence uses of: \(value)")
  var printer = LifetimeDependenceUsePrinter(function: function, context)
  defer { printer.deinitialize() }
  _ = printer.walkDown(root: value)
}


// SIL Unit tests

let argumentConventionsTest = FunctionTest("argument_conventions") {
  function, arguments, context in
  if arguments.hasUntaken {
    let value = arguments.takeValue()
    let applySite = value.definingInstruction as! ApplySite
    print("Conventions for call: \(applySite)")
    print(applySite.calleeArgumentConventions)
  } else {
    print("Conventions for function: \(function.name)")
    print(function.argumentConventions)
  }
  // TODO: print ~Escapable conformance and lifetime dependencies
}
