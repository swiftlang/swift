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
// LifetimeDependence.init models the lifetime dependence for a FunctionArgument or a MarkDependenceInstruction,
// categorizing the kind of dependence scope that the lifetime represents.
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
        precondition(initialAddress is AllocStackInst || initialAddress is FunctionArgument
                       || initialAddress is StoreBorrowInst,
                     "expected storage for a a local 'let'")
        if case let .store(store, _) = initializer {
          precondition(store is StoringInstruction || store is SourceDestAddrInstruction || store is FullApplySite
                      || store is StoreBorrowInst,
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
  let markDepInst: MarkDependenceInstruction?
  
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
    self.markDepInst = nil
  }

  // Construct a LifetimeDependence from a return value. This only
  // constructs a dependence for ~Escapable results that do not have a
  // lifetime dependence (@lifetime(immortal), @_unsafeNonescapableResult).
  //
  // This is necessary because inserting a mark_dependence placeholder for such an unsafe dependence would illegally
  // have the same base and value operand.
  init?(unsafeApplyResult value: Value, apply: FullApplySite, _ context: some Context) {
    if value.isEscapable {
      return nil
    }
    assert(!apply.hasResultDependence, "mark_dependence should be used instead")
    assert(value.ownership == .owned || value.type.isAddress, "unsafe apply result must be owned")
    self.scope = Scope(base: value, context)
    self.dependentValue = value
    self.markDepInst = nil
  }

  /// Construct LifetimeDependence from mark_dependence [unresolved] or mark_dependence [nonescaping].
  ///
  /// For a LifetimeDependence constructed from a mark_dependence, its `dependentValue` is the result of the
  /// mark_dependence. For a mark_dependence_addr, `dependentValue` is the address operand.
  ///
  /// Returns 'nil' for unknown dependence.
  init?(_ markDep: MarkDependenceInstruction, _ context: some Context) {
    self.init(scope: Scope(base: markDep.base, context), markDep: markDep)
  }

  /// Construct LifetimeDependence from mark_dependence [unresolved] or mark_dependence [nonescaping] with a custom
  /// Scope.
  ///
  /// Returns 'nil' for unknown dependence.
  init?(scope: LifetimeDependence.Scope, markDep: MarkDependenceInstruction) {
    switch markDep.dependenceKind {
    case .Unresolved, .NonEscaping:
      self.scope = scope
      switch markDep {
      case let md as MarkDependenceInst:
        self.dependentValue = md
      case let md as MarkDependenceAddrInst:
        self.dependentValue = md.address
      default:
        fatalError("unexpected MarkDependenceInstruction")
      }
      self.markDepInst = markDep
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
    if let mdi = markDepInst {
      mdi.resolveToNonEscaping()
    }
  }
}

// Scope initialization.
extension LifetimeDependence.Scope {
  /// Construct a lifetime dependence scope from the base value that other values depend on. This derives the kind of
  /// dependence scope and its parentValue from `base`.
  ///
  /// The returned Scope must be the only scope for the given 'base' value. This is generally non-recursive, except
  /// that it tries to find the single borrow introducer. General use-def walking is handled by a utility such as
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
    case .class, .tail, .pointer, .index:
      self = .unknown(accessBase.address!)
    case .unidentified:
      self = .unknown(address)
    case let .stack(allocStack):
      if let initializer = accessBase.findSingleInitializer(context) {
        if case let .store(store, _) = initializer {
          self = .initialized(.store(initializingStore: store, initialAddress: allocStack))
          return
        }
      }
      self = .unknown(allocStack)
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
      // Don't follow the stored value in case the dependence requires addressability.
      self = .initialized(.store(initializingStore: sb, initialAddress: sb))
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
    // @inout arguments belong to the coroutine because they are valid for the duration of the yield, and, if local
    // mutation or reassignment were relevant, then the dependence would be on an access scope instead.
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
  /// Ignore the lifetime of temporary trivial values (with .initialized and .unknown scopes). Temporaries have an
  /// unknown Scope, which means that LifetimeDependence.Scope did not recognize a VariableScopeInstruction. This is
  /// important to promote mark_dependence instructions emitted by SILGen to [nonescaping] (e.g. unsafeAddressor). It
  /// also allows trivial value to be "extended" without actually tracking their scope, which is expected behavior. For
  /// example:
  ///
  ///     let span = Span(buffer.baseAddress)
  ///
  /// If 'baseAddress' is an opaque getter, then the temporary pointer is effectively valid
  /// until the end of the function.
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
      let address = initializer.initialAddress
      if address.type.objectType.isTrivial(in: address.parentFunction) {
        return nil
      }
      return LifetimeDependence.Scope.computeInitializedRange(initializer: initializer, context)
    case let .unknown(value):
      if value.type.objectType.isTrivial(in: value.parentFunction) {
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
      case is EndBorrowInst:
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
///   walkDown(dependence: LifetimeDependence)
///
/// Note: this may visit values that are not dominated by `dependence` because of dependent phi operands.
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
  mutating func walkDown(dependence: LifetimeDependence) -> WalkResult {
    if let mdAddr = dependence.markDepInst as? MarkDependenceAddrInst {
      // All reachable uses of the dependent address are dependent uses. Treat the mark_dependence_addr base operand as
      // if it was a store's address operand. Diagnostics will consider this the point of variable initialization.
      return visitStoredUses(of: mdAddr.baseOperand, into: mdAddr.address)
    }
    let root = dependence.dependentValue
    if root.type.isAddress { 
      // The root address may be an escapable mark_dependence that guards its address uses (unsafeAddress), an
      // allocation, an incoming argument, or an outgoing argument. In all these cases, walk down the address uses.
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
    // Initially delegate all uses to OwnershipUseVisitor.
    // forwardingUse() will be called for uses that forward ownership, which then delegates to walkDownDefault().
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
      return dependentUse(of: operand, dependentValue: mdi)

    case is ExistentialMetatypeInst, is FixLifetimeInst, is WitnessMethodInst,
         is DynamicMethodBranchInst, is ValueMetatypeInst,
         is DestroyNotEscapedClosureInst, is ClassMethodInst, is SuperMethodInst,
         is ClassifyBridgeObjectInst, is DebugValueInst,
         is ObjCMethodInst, is ObjCSuperMethodInst, is UnmanagedRetainValueInst,
         is UnmanagedReleaseValueInst, is SelectEnumInst, is IgnoredUseInst:
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
    // Lifetime dependence is only interested in dominated uses. Treat a returned phi like a dominated use by stopping
    // at the phi operand rather than following the forwarded value to the ReturnInst.
    if let phi = Phi(using: operand), phi.isReturnValue {
      return returnedDependence(result: operand)
    }
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

  // Handle address or non-address operands.
  mutating func dependentUse(of operand: Operand, dependentValue value: Value)
    -> WalkResult {
    return walkDownUses(of: value, using: operand)
  }

  // Handle address or non-address operands.
  mutating func dependentUse(of operand: Operand, dependentAddress address: Value)
    -> WalkResult {
    // Consider this a leaf use in addition to the dependent address uses, which might all occur earlier.
    if leafUse(of: operand) == .abortWalk {
      return .abortWalk
    }
    // The lifetime dependence is effectively "copied into" the dependent address. Find all uses of the dependent
    // address as if this were a stored use.
    return visitStoredUses(of: operand, into: address)
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

  mutating func loadedAddressUse(of operand: Operand, intoValue value: Value) -> WalkResult {
    // Record the load itself, in case the loaded value is Escapable.
    if leafUse(of: operand) == .abortWalk {
      return .abortWalk
    }
    return walkDownUses(of: value, using: operand)
  }    

  // copy_addr
  mutating func loadedAddressUse(of operand: Operand, intoAddress address: Operand) -> WalkResult {
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

  mutating func dependentAddressUse(of operand: Operand, dependentValue value: Value) -> WalkResult {
    dependentUse(of: operand, dependentValue: value)
  }    

  // mark_dependence_addr
  mutating func dependentAddressUse(of operand: Operand, dependentAddress address: Value) -> WalkResult {
    dependentUse(of: operand, dependentAddress: address)
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
    if !needWalk(for: address) {
      return .continueWalk
    }
    return address.uses.ignoreTypeDependence.walk {
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
    case let .beginApply(bai):
      // First, visit the uses of any non-Escapable yields. The yielded value may be copied and used outside the
      // coroutine scope.  Now, visit the uses of the begin_apply token. This adds the coroutine scope itself to the
      for yield in bai.yieldedValues {
        if walkDownUses(of: yield, using: operand) == .abortWalk {
          return .abortWalk
        }
      }
      // lifetime to account for the scope of any arguments.
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

  // Visit a dependent local variable (alloc_box), or temporary storage (alloc_stack). The depenedency is typically from
  // storing a dependent value at `address`, but may be from an outright `mark_dependence_addr`.
  //
  // This handles stores of the entire value and stores into a member. Storing into a member makes the entire aggregate
  // a dependent value.
  //
  // If 'operand' is an address, then the "store" corresponds to a dependency on another in-memory value. This may
  // result from `copy_addr`, `mark_dependence_addr`, or initialization of an applied `@out` argument that depends on
  // another indirect parameter.
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
        return loadedAddressUse(of: localAccess.operand!, intoValue: load)
      case let load as LoadBorrowInst:
        return loadedAddressUse(of: localAccess.operand!, intoValue: load)
      case let copyAddr as SourceDestAddrInstruction:
        return loadedAddressUse(of: localAccess.operand!, intoAddress: copyAddr.destinationOperand)
      case is SwitchEnumAddrInst:
        // switch_enum_addr does not produce any values. Subsequent uses of the address (unchecked_enum_data_addr)
        // directly use the original address.
        return .continueWalk
      default:
        return .abortWalk
      }
    case .dependenceSource:
      switch localAccess.instruction! {
      case let md as MarkDependenceInst:
        if md.type.isAddress {
          return loadedAddressUse(of: localAccess.operand!, intoAddress: md.valueOperand)
        }
        return loadedAddressUse(of: localAccess.operand!, intoValue: md)
      case let md as MarkDependenceAddrInst:
        return loadedAddressUse(of: localAccess.operand!, intoAddress: md.addressOperand)
      default:
        return .abortWalk
      }
    case .dependenceDest:
      // Simply a marker that indicates the start of an in-memory dependent value. If this was a mark_dependence, uses
      // of its forwarded address has were visited by LocalVariableAccessWalker and recorded as separate local accesses.
      return .continueWalk
    case .store, .storeBorrow:
      // A store does not use the previous in-memory value.
      return .continueWalk
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
  }

  private mutating func visitAppliedUse(of operand: Operand, by apply: FullApplySite) -> WalkResult {
    if let conv = apply.convention(of: operand), conv.isIndirectOut {
      // This apply initializes an allocation.
      return dependentUse(of: operand, dependentAddress: operand.value)
    }
    if apply.isCallee(operand: operand) {
      return leafUse(of: operand)
    }
    if let dep = apply.resultDependence(on: operand),
       !dep.isScoped {
      // Operand is nonescapable and passed as a call argument. If the
      // result inherits its lifetime, then consider any nonescapable
      // result value to be a dependent use.
      //
      // If the lifetime dependence is scoped, then we can ignore it
      // because a mark_dependence [nonescaping] represents the
      // dependence.
      if let result = apply.singleDirectResult, !result.isEscapable {
        if dependentUse(of: operand, dependentValue: result) == .abortWalk {
          return .abortWalk
        }
      }
      for resultAddr in apply.indirectResultOperands
          where !resultAddr.value.isEscapable {
        if dependentUse(of: operand, dependentAddress: resultAddr.value) == .abortWalk {
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

// =============================================================================
// LifetimeDependenceUseDefWalker - upward walk
// =============================================================================

/// Walk up lifetime dependencies to the first value associated with a variable declaration.
///
/// To start walking:
///     walkUp(newLifetime: Value) -> WalkResult
///
/// This utility finds a value or address that is the best-effort "root" of the chain of temporary lifetime-dependent
/// values.
///
/// This "looks through" projections: a property that is either visible as a stored property or access via
/// unsafe[Mutable]Address.
///
///     dependsOn(lvalue.field) // finds 'lvalue' when 'field' is a stored property
///
///     dependsOn(lvalue.computed) // finds the temporary value directly returned by a getter.
///
/// SILGen emits temporary copies that violate lifetime dependence semantcs. This utility looks through such temporary
/// copies, stopping at a value that introduces an immutable variable: move_value [var_decl] or begin_borrow [var_decl],
/// or at an access of a mutable variable: begin_access [read] or begin_access [modify].
///
/// In this example, the dependence "root" is copied, borrowed, and forwarded before being used as the base operand of
/// `mark_dependence`. The dependence "root" is the parent of the outer-most dependence scope.
///
///     %root = apply                  // lifetime dependence root
///     %copy = copy_value %root
///     %parent = begin_borrow %copy   // lifetime dependence parent value
///     %base = struct_extract %parent // lifetime dependence base value
///     %dependent = mark_dependence [nonescaping] %value on %base
///
/// LifetimeDependenceUseDefWalker extends the ForwardingUseDefWalker to follow copies, moves, and
/// borrows. ForwardingUseDefWalker treats these as forward-extended lifetime introducers. But they inherit a lifetime
/// dependency from their operand because non-escapable values can be copied, moved, and borrowed. Nonetheless, all of
/// their uses must remain within original dependence scope.
///
///   # owned lifetime dependence
///   %parent = apply               // begin dependence scope -+
///   ...                                                      |
///   %1 = mark_dependence [nonescaping] %value on %parent     |
///   ...                                                      |
///   %2 = copy_value %1        -+                             |
///   # forwarding instruction   |                             |
///   %3 = struct $S (%2)        | forward-extended lifetime   |
///                              |                             | OSSA Lifetime
///   %4 = move_value %3        -+                             |
///   ...                        | forward-extended lifetime   |
///   %5 = begin_borrow %4       | -+                          |
///   # dependent use of %1      |  | forward-extended lifetime|
///   end_borrow %5              | -+                          |
///   destroy_value %4          -+                             |
///   ...                                                      |
///   destroy_value %parent        // end dependence scope    -+
///
/// All of the dependent uses including `end_borrow %5` and `destroy_value %4` must be before the end of the dependence
/// scope: `destroy_value %parent`. In this case, the dependence parent is an owned value, so the scope is simply the
/// value's OSSA lifetime.
///
/// The ForwardingUseDefWalker's PathContext is the most recent lifetime owner (Value?). If we ever want to track the
/// access path as well, then we can aggregate both the owner and access path in a single PathContext.
protocol LifetimeDependenceUseDefValueWalker : ForwardingUseDefWalker where PathContext == Value? {
  var context: Context { get }

  // 'path' is the lifetime "owner": the "def" into which the current 'value' is eventually forwarded.
  mutating func introducer(_ value: Value, _ path: PathContext) -> WalkResult

  // Minimally, check a ValueSet. This walker may traverse chains of
  // aggregation and destructuring along with phis.
  //
  // 'path' is the "owner" of the forwarded value.
  mutating func needWalk(for value: Value, _ path: PathContext) -> Bool

  // The 'newLifetime' value is not forwarded to its uses.
  mutating func walkUp(newLifetime: Value) -> WalkResult

  // 'owner' is the "def" into which the current 'value' is eventually forwarded.
  mutating func walkUp(value: Value, _ owner: Value?) -> WalkResult
}

// Defaults
extension LifetimeDependenceUseDefValueWalker {
  mutating func walkUp(value: Value, _ owner: Value?) -> WalkResult {
    walkUpDefault(value: value, owner)
  }
}

// Helpers
extension LifetimeDependenceUseDefValueWalker {
  mutating func walkUpDefault(value: Value, _ owner: Value?) -> WalkResult {
    switch value.definingInstruction {
    case let transition as OwnershipTransitionInstruction:
      return walkUp(newLifetime: transition.operand.value)
    case let load as LoadInstruction:
      return walkUp(newLifetime: load.address)
    default:
      break
    }
    // If the dependence chain has a phi, consider it a root. Dependence roots dominate all dependent values.
    if Phi(value) != nil {
      return introducer(value, owner)
    }
    // ForwardingUseDefWalker will callback to introducer() when it finds no forwarding instruction.
    return walkUpDefault(forwarded: value, owner)
  }
}

protocol LifetimeDependenceUseDefAddressWalker {
  var context: Context { get }

  // If the scoped value is trivial, then only the variable's lexical scope is relevant, and access scopes can be
  // ignored.
  var isTrivialScope: Bool { get }

  mutating func needWalk(for address: Value) -> Bool

  mutating func addressIntroducer(_ address: Value, access: AccessBaseAndScopes) -> WalkResult

  // The 'newLifetime' value is not forwarded to its uses. It may be a non-address or an address.
  mutating func walkUp(newLifetime: Value) -> WalkResult

  mutating func walkUp(address: Value, access: AccessBaseAndScopes) -> WalkResult
}

// Defaults
extension LifetimeDependenceUseDefAddressWalker {
  mutating func walkUp(address: Value, access: AccessBaseAndScopes) -> WalkResult {
    walkUpDefault(address: address, access: access)
  }
}

// Helpers
extension LifetimeDependenceUseDefAddressWalker {
  mutating func walkUp(address: Value) -> WalkResult {
    walkUp(address: address, access: address.accessBaseWithScopes)
  }

  mutating func walkUpDefault(address: Value, access: AccessBaseAndScopes) -> WalkResult {
    if !needWalk(for: address) {
      return .continueWalk
    }
    if let beginAccess = access.innermostAccess {
      // Skip the access scope for unsafe[Mutable]Address. Treat it like a projection of 'self' rather than a separate
      // variable access.
      if let addressorSelf = beginAccess.unsafeAddressorSelf {
        return walkUp(newLifetime: addressorSelf)
      }
      // Ignore the acces scope for trivial values regardless of whether it is singly-initialized. Trivial values do not
      // need to be kept alive in memory and can be safely be overwritten in the same scope. Lifetime dependence only
      // cares that the loaded value is within the lexical scope of the trivial value's variable declaration. Rather
      // than skipping all access scopes, call 'walkUp' on each nested access in case one of them needs to redirect the
      // walk, as required for 'access.unsafeAddressorSelf' or in case the implementation overrides certain accesses.
      if isTrivialScope {
        return walkUp(newLifetime: beginAccess.address)
      }
      // Generally assume an access scope introduces a variable borrow scope. And generally ignore address forwarding
      // mark_dependence.
      return addressIntroducer(beginAccess, access: access)
    }
    // Continue walking for some kinds of access base.
    switch access.base {
    case .box, .global, .class, .tail, .pointer, .index, .unidentified:
      break
    case let .stack(allocStack):
      // Ignore stack locations. Their access scopes do not affect lifetime dependence.
      return walkUp(stackInitializer: allocStack, at: address, access: access)
    case let .argument(arg):
      if arg.convention.isExclusiveIndirect {
        return addressIntroducer(arg, access: access)
      }
    case let .yield(yieldedAddress):
      // Ignore access scopes for @in or @in_guaranteed yields when all scopes are reads.
      let apply = yieldedAddress.definingInstruction as! FullApplySite
      if apply.convention(of: yieldedAddress).isIndirectIn && access.isOnlyReadAccess {
        return addressIntroducer(yieldedAddress, access: access)
      }
    case .storeBorrow(let sb):
      // Walk up through a store into a temporary.
      if access.scopes.isEmpty,
         case .stack = sb.destinationOperand.value.accessBase {
        return walkUp(newLifetime: sb.source)
      }
    }
    return addressIntroducer(access.enclosingAccess.address ?? address, access: access)
  }

  // Handle singly-initialized temporary stack locations.
  mutating func walkUp(stackInitializer allocStack: AllocStackInst, at address: Value,
                       access: AccessBaseAndScopes) -> WalkResult {
    guard let initializer = allocStack.accessBase.findSingleInitializer(context) else {
      return addressIntroducer(address, access: access)
    }
    if case let .store(store, _) = initializer {
      switch store {
      case let store as StoringInstruction:
        return walkUp(newLifetime: store.source)
      case let srcDestInst as SourceDestAddrInstruction:
        return walkUp(newLifetime: srcDestInst.source)
      case let apply as FullApplySite:
        if let f = apply.referencedFunction, f.isConvertPointerToPointerArgument {
          return walkUp(newLifetime: apply.parameterOperands[0].value)
        }
      default:
        break
      }
    }
    return addressIntroducer(address, access: access)
  }
}

/// Walk up through all new lifetimes to find the roots of a lifetime dependence chain.
struct LifetimeDependenceRootWalker : LifetimeDependenceUseDefValueWalker, LifetimeDependenceUseDefAddressWalker {
  // The ForwardingUseDefWalker's context is the most recent lifetime owner.
  typealias PathContext = Value?

  let context: Context

  // If the scoped value is trivial, then only the variable's lexical scope is relevant, and access scopes can be
  // ignored.
  let isTrivialScope: Bool

  // This visited set is only really needed for instructions with
  // multiple results, including phis.
  private var visitedValues: ValueSet

  var roots: SingleInlineArray<Value> = SingleInlineArray<Value>()

  init(_ context: Context, scopedValue: Value) {
    self.context = context
    self.isTrivialScope = scopedValue.type.isAddress
      ? scopedValue.type.objectType.isTrivial(in: scopedValue.parentFunction)
      : scopedValue.isTrivial(context)
    self.visitedValues = ValueSet(context)
  }

  mutating func deinitialize() {
    visitedValues.deinitialize()
  }
 
  mutating func introducer(_ value: Value, _ owner: Value?) -> WalkResult {
    roots.push(value)
    return .continueWalk
  }

  mutating func addressIntroducer(_ address: Value, access: AccessBaseAndScopes) -> WalkResult {
    roots.push(address)
    return .continueWalk
  }

  mutating func needWalk(for value: Value, _ owner: Value?) -> Bool {
    visitedValues.insert(value)
  }

  mutating func needWalk(for address: Value) -> Bool {
    visitedValues.insert(address)
  }

  mutating func walkUp(newLifetime: Value) -> WalkResult {
    if newLifetime.type.isAddress {
      return walkUp(address: newLifetime)
    }
    let newOwner = newLifetime.ownership == .owned ? newLifetime : nil
    return walkUp(value: newLifetime, newOwner)
  }
}

// =============================================================================
// Unit tests
// =============================================================================


let lifetimeDependenceUseTest = FunctionTest("lifetime_dependence_use") {
    function, arguments, context in
  let value = arguments.takeValue()
  var dependence: LifetimeDependence?
  switch value {
  case let arg as FunctionArgument:
    dependence = LifetimeDependence(arg, context)
  case let md as MarkDependenceInstruction:
    dependence = LifetimeDependence(md, context)
  default:
    break
  }
  guard let dependence = dependence else {
    print("Invalid dependence scope: \(value)")
    return
  }
  print("LifetimeDependence uses of: \(value)")
  var printer = LifetimeDependenceUsePrinter(function: function, context)
  defer { printer.deinitialize() }
  _ = printer.walkDown(dependence: dependence)
}

let lifetimeDependenceRootTest = FunctionTest("lifetime_dependence_root") {
    function, arguments, context in
  let value = arguments.takeValue()
  print("Lifetime dependence roots of: \(value)")
  var walker = LifetimeDependenceRootWalker(context, scopedValue: value)
  let result = walker.walkUp(newLifetime: value)
  assert(result == .continueWalk)
  for root in walker.roots {
    print("root: \(root)")
  }
  walker.deinitialize()
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
