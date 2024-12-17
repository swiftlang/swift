//===--- LifetimeDependenceUtils.swift - Utils for lifetime dependence ----===//
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
// Utilities that specify lifetime dependence:
//
// gatherVariableIntroducers(for:) is a use-def walk that returns the
// values that most closely associated with the variable declarations
// that the given value holds an instance of.
//
// LifetimeDependence.init models the lifetime dependence for a
// FunctionArgument or a MarkDependenceInst, categorizing the kind of
// dependence scope that the lifetime represents.
//
// LifetimeDependence.Scope.computeRange() computes the instruction
// range covered by a dependence scope.
//
// LifetimeDependence.visitDependenceRoots(enclosing:) is a use-def
// walk that walks up the chain of dependent values and visits the
// earliest LifetimeDependencies that whose lifetime can be inherited
// by the given value.
//
// LifetimeDependenceDefUseWalker walks the def-use chain to find all
// values that depend on the given OSSA lifetime.
//
//===----------------------------------------------------------------------===//
//
// TODO_reachingdef: see this tag (in visitStoredUses) to see where
// reaching-def analysis can improve diagnostics. The analysis will
// work as follows:
//
// Given an operand that satisfies `isAddressInitialization`,
// transitively find all uses of the stored value, conservatively.
//
// Given a load/copy_addr transitively find all values stored.
//
// This can be a reaching-def style analysis on an alloc_stack, very
// similar to the mem2reg analysis. Or it can be a cached
// walker. Either way, we only care about mutable local. Temporary
// copies should already be handled by findSingleInitializer. There is
// no need for any complicated value depenence walk. The client can do
// that. Just walk the address projections to find all writes. Partial
// writes are destructive, but only full writes initialize the local.
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

/// Walk up the value dependence chain to find the best-effort
/// variable declaration. Typically called while diagnosing an error.
///
/// Returns an array with at least one introducer value.
///
/// The walk stops at:
/// - an address
/// - a variable declaration (begin_borrow [var_decl], move_value [var_decl])
/// - the root of the dependence chain
///
/// If the introducer is an address, then the client can call
/// Value.enclosingAccess iteratively to find to AccessBase. This
/// walker is useful for finding the innermost access, which may also
/// be relevant for diagnostics.
func gatherVariableIntroducers(for value: Value, _ context: Context)
  -> SingleInlineArray<Value>
{
  var introducers = SingleInlineArray<Value>()
  var useDefVisitor = VariableIntroducerUseDefWalker(context) {
    introducers.push($0)
    return .continueWalk
  }
  defer { useDefVisitor.deinitialize() }
  _ = useDefVisitor.walkUp(valueOrAddress: value)
  assert(!introducers.isEmpty, "missing variable introducer")
  return introducers
}

/// A lifetime dependence represents a scope in which some parent
/// value is alive and accessible along with a dependent value. All
/// values derived from the dependent value must be used within this
/// scope. This supports diagnostics on non-copyable and non-escapable types.
///
/// A lifetime dependence is produced by either 'mark_dependence [nonescaping]':
///
///   %dependent = mark_dependence [nonescaping] %value on %parent
///
/// or a non-escapable function argument:
///
///   bb0(%dependent : NonEscapableThing):
///
/// A lifetime dependence identifies its parent value, the kind of
/// scope that the parent value represents, and a dependent value.
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
    /// An borrowed value whose OSSA lifetime encloses nonescapable values, or a trivial variable introduced by
    /// begin_borrow.
    case borrowed(BeginBorrowValue)
    /// Singly-initialized addressable storage (likely for an
    /// immutable address-only value). The lifetime extends until the
    /// memory is destroyed. e.g. A value produced by an @in
    /// FunctionArgument or @out apply. @inout has caller scope
    /// instead because its lifetime does not end inside the callee. A
    /// separate analysis diagnoses mutation after the dependence is
    /// formed.
    ///
    /// If `initializingStore` is nil, then the `initialAddress` is
    /// initialized on function entry.
    case initialized(initialAddress: Value, initializingStore: Instruction?)
    // Unknown includes: escapable values with local var_decl, stack allocations that are not singly initialized.
    case unknown(Value)

    var parentValue: Value {
      switch self {
      case let .caller(argument): return argument
      case let .access(beginAccess): return beginAccess
      case let .yield(value): return value
      case let .owned(value): return value
      case let .borrowed(beginBorrow): return beginBorrow.value
      case let .initialized(initialAddress, _): return initialAddress
      case let .unknown(value): return value
      }
    }

    func checkPrecondition() {
      switch self {
      case let .caller(argument):
        precondition(argument.ownership != .owned, "only guaranteed or inout arguments have a caller scope")
      case .access, .unknown:
        break        
      case let .yield(value):
        precondition(value.definingInstruction is BeginApplyInst)
      case let .owned(value):
        precondition(value.ownership == .owned)
      case let .borrowed(beginBorrow):
        precondition(beginBorrow.value.ownership == .guaranteed)
      case let .initialized(initialAddress, initializingStore):
        precondition(initialAddress.type.isAddress, "expected an address")
        precondition(initialAddress is AllocStackInst
                       || initialAddress is FunctionArgument,
                     "expected storage for a a local 'let'")
        precondition(initializingStore == nil || initializingStore is StoringInstruction
                       || initializingStore is SourceDestAddrInstruction || initializingStore is FullApplySite,
                     "expected a store")
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

  var isUnsafeApplyResult: Bool {
    if case let .owned(value) = scope {
      if let apply = value.definingInstruction as? FullApplySite {
        assert(!apply.hasResultDependence)
        return true
      }
    }
    return false
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

private extension Value {
  func isForwarded(from: Value) -> Bool {
    if self == from {
      return true
    }
    if let forward = self.forwardingInstruction,
       let singleOp = forward.singleForwardedOperand {
      return singleOp.value.isForwarded(from: from)
    }
    return false
  }
}

extension LifetimeDependence.Scope {
  /// Construct a lifetime dependence scope from the base value that other values depend on. This derives the kind of
  /// dependence scope and its parentValue from `base`.
  ///
  /// `base` represents the OSSA lifetime that the dependent value must be used within. If `base` is owned, then it
  /// directly defines the parent lifetime. If `base` is guaranteed, then it must have a single borrow introducer, which
  /// defines the parent lifetime. `base` must not be derived from a guaranteed phi or forwarded (via struct/tuple) from
  /// multiple guaranteed values.
  init(base: Value, _ context: some Context) {
    if base.type.isAddress {
      self = Self(address: base, context)
      return
    }
    switch base.ownership {
    case .owned:
      self = .owned(base)
      return
    case .guaranteed:
      self = Self(guaranteed: base, context)
    case .none:
      self = Self(variable: base, context)
    case .unowned:
      self = .unknown(base)
    }
  }

  private init(address: Value, _ context: some Context) {
    switch address.enclosingAccessScope {
    case let .scope(access):
      self = .access(access)
    case let .base(accessBase):
      self = Self(accessBase: accessBase, address: address, context)
    }
  }

  init(accessBase: AccessBase, address: Value, _ context: some Context) {
    switch accessBase {
    case let .box(projectBox):
      // Note: the box may be in a borrow scope.
      self = Self(base: projectBox.operand.value, context)
    case let .stack(allocStack):
      self = Self(allocation: allocStack, context)
    case .global:
      self = .unknown(address)
    case .class, .tail:
      let refElt = address as! UnaryInstruction
      self = Self(guaranteed: refElt.operand.value, context)
    case let .argument(arg):
      if arg.convention.isIndirectIn {
        self = .initialized(initialAddress: arg, initializingStore: nil)
      } else if arg.convention.isIndirectOut || arg.convention.isInout {
        // TODO: verify that @out values are never reassigned.
        self = .caller(arg)
      } else {
        // Note: we do not expect arg.convention.isInout because
        // mutable variables require an access scope. The .caller
        // scope is assumed to be immutable.
        self = .unknown(address)
      }
    case let .yield(result):
      self = Self(yield: result)
    case .storeBorrow(let sb):
      self = Self(base: sb.source, context)
    case .pointer, .index, .unidentified:
      self = .unknown(address)
    }
  }

  private init(guaranteed base: Value, _ context: some Context) {
    // TODO: Add a SIL verifier check that a mark_dependence [nonescaping]
    // base is never a guaranteed phi.
    var iter = base.getBorrowIntroducers(context).makeIterator()
    // If no borrow introducer was found, then this is a borrow of a trivial value. Since we can assume a single
    // introducer here, then this is the only condition under which we have a trivial introducer.
    guard let beginBorrow = iter.next() else {
      self = Self(variable: base, context)
      return
    }
    // TODO: will we need to handle tuple/struct with multiple scopes?
    assert(iter.next() == nil,
           "guaranteed phis not allowed when diagnosing lifetime dependence")
    switch beginBorrow {
    case .beginBorrow, .loadBorrow:
      self = .borrowed(beginBorrow)
    case let .beginApply(value):
      self = .yield(value)
    case let .functionArgument(arg):
      self = .caller(arg)
    case .reborrow:
      fatalError("reborrows are not supported in diagnostics")
    }
  }

  private init(variable base: Value, _ context: some Context) {
    guard let introducer = gatherVariableIntroducers(for: base, context).singleElement else {
      // TODO: do we need to handle multiple introducers in case of a tuple/struct?
      self = .unknown(base)
      return
    }
    switch introducer {
    case let arg as FunctionArgument:
      self = .caller(arg)
    case let bbi as BeginBorrowInst:
      self = .borrowed(BeginBorrowValue(bbi)!)
    case is MoveValueInst:
      self = .owned(introducer)
    default:
      self = .unknown(introducer)
    }
  }

  private init(yield result: MultipleValueInstructionResult) {
    // Consider an @in yield an .initialized scope. We must find the destroys.
    let apply = result.parentInstruction as! FullApplySite
    if apply.convention(of: result).isIndirectIn {
      self = .initialized(initialAddress: result, initializingStore: nil)
      return
    }
    self = .yield(result)
  }

  private init(allocation: AllocStackInst, _ context: Context) {
    if let initializer = allocation.accessBase.findSingleInitializer(context) {
      self = .initialized(initialAddress: initializer.initialAddress,
                          initializingStore: initializer.initializingStore)
      return
    }
    self = .unknown(allocation)
  }
}

extension LifetimeDependence.Scope {
  /// Ignore "irrelevent" borrow scopes: load_borrow or begin_borrow without [var_decl]
  func ignoreBorrowScope(_ context: some Context) -> LifetimeDependence.Scope? {
    guard case let .borrowed(beginBorrowVal) = self else {
      return self
    }
    switch beginBorrowVal {
    case let .beginBorrow(bb):
      if bb.isFromVarDecl {
        return self
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
    case .caller:
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
    case let .initialized(initialAddress, initializingStore):
      return LifetimeDependence.Scope.computeInitializedRange(
        initialAddress: initialAddress, initializingStore: initializingStore,
        context)
    case let .unknown(value):
      // Return an empty range.
      return InstructionRange(for: value, context)
    }
  }

  // !!! - handle allocations of trivial values: no destroy. Use the dealloc in that case?
  private static func computeInitializedRange(initialAddress: Value, initializingStore: Instruction?,
                                              _ context: Context)
    -> InstructionRange {

    assert(initialAddress.type.isAddress)

    var range: InstructionRange
    if let initializingStore {
      range = InstructionRange(begin: initializingStore, context)
    } else {
      range = InstructionRange(for: initialAddress, context)
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

extension LifetimeDependence {
  /// TODO: By making UseDefVisitor a NonEscapable type, we should be
  /// able to make \p visitor a non-escaping closure.
  static func visitDependenceRoots(enclosing value: Value,
    _ context: Context,
    _ visitor: @escaping (LifetimeDependence.Scope) -> WalkResult)
  -> WalkResult {
    var useDefVisitor = UseDefVisitor(context, visitor)
    defer { useDefVisitor.deinitialize() }
    return useDefVisitor.walkUp(valueOrAddress: value)
  }
  
  private struct UseDefVisitor : LifetimeDependenceUseDefWalker {
    let context: Context
    // This visited set is only really needed for instructions with
    // multiple results, including phis.
    private var visitedValues: ValueSet
    // Call \p visit rather than calling this directly.
    private let visitorClosure: (LifetimeDependence.Scope) -> WalkResult
    
    init(_ context: Context,
         _ visitor: @escaping (LifetimeDependence.Scope) -> WalkResult) {
      self.context = context
      self.visitedValues = ValueSet(context)
      self.visitorClosure = visitor
    }
    
    mutating func deinitialize() {
      visitedValues.deinitialize()
    }
    
    mutating func needWalk(for value: Value, _ owner: Value?) -> Bool {
      // FIXME: cache the value's owner, and support walking up
      // multiple guaranteed forwards to different owners, then
      // reconverging.
      visitedValues.insert(value)
    }

    // Visit the base value of a lifetime dependence.
    //
    // Address type mark_dependence [unresolved] are from
    // - an indirect function result when opaque values are not enabled.
    // - an indirect yield
    //
    // Address type `mark_dependence [nonescaping]` are for captured arguments from ClosureLifetimeFixup.
    mutating func introducer(_ value: Value, _ owner: Value?) -> WalkResult {
      let base = owner ?? value
      var scope: LifetimeDependence.Scope?
      if base.type.isAddress {
        // For inherited dependence, look through access scopes.
        scope = LifetimeDependence.Scope(accessBase: base.accessBase, address: base, context)
      } else {
        scope = LifetimeDependence.Scope(base: base, context)
      }
      guard let scope = scope else {
        // Inheritance from an escapable value.
        return .continueWalk
      }
      scope.checkPrecondition()
      return visitorClosure(scope)
    }
  }
}

/// Walk up the lifetime dependence
///
/// This uses LifetimeDependenceUseDefWalker to find the introducers
/// of a dependence chain, which represent the value's "inherited"
/// dependencies. This stops at an address, unless the address refers
/// to a singly-initialized temporary, in which case it continues to
/// walk up the stored value.
///
/// This overrides LifetimeDependenceUseDefWalker to stop at a value
/// that introduces an immutable variable: move_value [var_decl] or
/// begin_borrow [var_decl], and to stop at an access of a mutable
/// variable: begin_access.
///
/// Start walking:
///     walkUp(valueOrAddress: Value) -> WalkResult
struct VariableIntroducerUseDefWalker : LifetimeDependenceUseDefWalker {
  let context: Context
  // This visited set is only really needed for instructions with
  // multiple results, including phis.
  private var visitedValues: ValueSet

  // Call \p visit rather than calling this directly.
  private let visitorClosure: (Value) -> WalkResult

  init(_ context: Context, _ visitor: @escaping (Value) -> WalkResult) {
    self.context = context
    self.visitedValues = ValueSet(context)
    self.visitorClosure = visitor
  }

  mutating func deinitialize() {
    visitedValues.deinitialize()
  }
 
  mutating func needWalk(for value: Value, _ owner: Value?) -> Bool {
    visitedValues.insert(value)
  }

  mutating func introducer(_ value: Value, _ owner: Value?) -> WalkResult {
    return visitorClosure(value)
  }

  mutating func walkUp(value: Value, _ owner: Value?) -> WalkResult {
    if let inst = value.definingInstruction, VariableScopeInstruction(inst) != nil {
      return visitorClosure(value)
    }
    // Finding a variable introducer requires following the mark_dependence forwarded value, not the base value like the
    // default LifetimeDependenceUseDefWalker.
    if value is MarkDependenceInst {
      return walkUpDefault(forwarded: value, owner)
    }
    return walkUpDefault(dependent: value, owner: owner)
  }

  mutating func walkUp(address: Value) -> WalkResult {
    if let beginAccess = address.definingInstruction as? BeginAccessInst {
      return introducer(beginAccess, nil)
    }
    return walkUpDefault(address: address)
  }
}

/// Walk up the lifetime dependence chain.
///
/// This finds the introducers of a dependence chain. which represent the value's "inherited" dependencies. This stops
/// at phis, so all introducers dominate their dependencies. This stops at addresses in general, but if the value is
/// loaded from a singly-initialized location, then it continues walking up the value stored by the initializer. This
/// bypasses the copies to temporary memory locations emitted by SILGen.
///
/// In this example, the dependence root is copied, borrowed, and forwarded before being used as the base operand of
/// `mark_dependence`. The dependence "root" is the parent of the outer-most dependence scope.
///
///   %root = apply                  // lifetime dependence root
///   %copy = copy_value %root
///   %parent = begin_borrow %copy   // lifetime dependence parent value
///   %base = struct_extract %parent // lifetime dependence base value
///   %dependent = mark_dependence [nonescaping] %value on %base
///
/// This extends the ForwardingUseDefWalker, which finds the forward-extended lifetime introducers. Certain
/// forward-extended lifetime introducers can inherit a lifetime dependency from their operand: namely copies, moves,
/// and borrows. These introducers are considered part of their operand's dependence scope because non-escapable values
/// can be copied, moved, and borrowed. Nonetheless, all of their uses must remain within original dependence scope.
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
/// Minimal requirements:
///   var context: Context
///   introducer(_ value: Value) -> WalkResult
///   needWalk(for value: Value) -> Bool
///
/// Start walking:
///   walkUp(valueOrAddress: Value) -> WalkResult
protocol LifetimeDependenceUseDefWalker : ForwardingUseDefWalker where PathContext == Value? {
  var context: Context { get }

  /// 'owner' is the most recently visited suitable base. Generally, this is the most recent owned value. When a
  /// mark_dependence value operand is forwarded from its base operand, however, the owner is not updated because that
  /// would could lead to introducing an illegal mark_dependence with the same value for both operands.
  mutating func introducer(_ value: Value, _ owner: Value?) -> WalkResult

  // Minimally, check a ValueSet. This walker may traverse chains of
  // aggregation and destructuring along with phis.
  mutating func needWalk(for value: Value, _ owner: Value?) -> Bool

  mutating func walkUp(value: Value, _ owner: Value?) -> WalkResult

  mutating func walkUp(address: Value) -> WalkResult
}

// Implement ForwardingUseDefWalker
extension LifetimeDependenceUseDefWalker {
  mutating func walkUp(valueOrAddress: Value) -> WalkResult {
    if valueOrAddress.type.isAddress {
      return walkUp(address: valueOrAddress)
    }
    let owner = valueOrAddress.ownership == .owned ? valueOrAddress : nil
    return walkUp(value: valueOrAddress, owner)
  }

  mutating func walkUp(value: Value, _ owner: Value?) -> WalkResult {
    walkUpDefault(dependent: value, owner: owner)
  }

  mutating func walkUp(address: Value) -> WalkResult {
    walkUpDefault(address: address)
  }

  // Extend ForwardingUseDefWalker to handle copies, moves, and
  // borrows. Also transitively walk up other lifetime dependencies to
  // find the roots.
  //
  // If `value` is an address, this immediately invokes
  // `introducer()`. We only expect to see an address-type
  // `mark_dependence [nonescaping]` when opaque values are disabled.
  //
  // Handles loads as a convenience so the client receives the load's
  // address as an introducer.
  mutating func walkUpDefault(dependent value: Value, owner: Value?)
    -> WalkResult {
    switch value.definingInstruction {
    case let transition as OwnershipTransitionInstruction:
      return walkUp(newLifetime: transition.operand.value)
    case let load as LoadInstruction:
      return walkUp(address: load.address)
    case let markDep as MarkDependenceInst:
      if let dependence = LifetimeDependence(markDep, context) {
        let parent = dependence.parentValue
        if markDep.isForwarded(from: parent) {
          return walkUp(value: dependence.parentValue, owner)
        } else {
          return walkUp(newLifetime: dependence.parentValue)
        }
      }
    default:
      break
    }
    // If the dependence chain has a phi, consider it a root. Dependence roots
    // are currently expected to dominate all dependent values.
    if Phi(value) != nil {
      return introducer(value, owner)
    }
    // ForwardingUseDefWalker will callback to introducer() when it finds no forwarding instruction.
    return walkUpDefault(forwarded: value, owner)
  }

  // Walk up from a load of a singly-initialized address to find the
  // dependence root of the stored value. This ignores mutable
  // variables, which require an access scope. This ignores applies
  // because an lifetime dependence will already be expressed as a
  // mark_dependence.
  mutating func walkUpDefault(address: Value) -> WalkResult {
    if let (_, initializingStore) =
         address.accessBase.findSingleInitializer(context) {
      switch initializingStore {
      case let store as StoringInstruction:
        return walkUp(newLifetime: store.source)
      case let srcDestInst as SourceDestAddrInstruction:
        return walkUp(address: srcDestInst.sourceOperand.value)
      case let apply as FullApplySite:
        if let f = apply.referencedFunction,
           f.isConvertPointerToPointerArgument {
          return walkUp(address: apply.parameterOperands[0].value)
        }
      default:
        break
      }
    }
    return introducer(address, nil)
  }

  private mutating func walkUp(newLifetime: Value) -> WalkResult {
    let newOwner = newLifetime.ownership == .owned ? newLifetime : nil
    return walkUp(value: newLifetime, newOwner)
  }
}

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
/// TODO: override BeginAccessInst to handle EndAccessInst.
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
         is IsEscapingClosureInst, is ClassMethodInst, is SuperMethodInst,
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
    case let .storeBorrow(sbi):
      return walkDownAddressUses(of: sbi)
    case .beginApply:
      // Skip the borrow scope; the type system enforces non-escapable
      // arguments.
      return visitInnerBorrowUses(of: borrowInst)
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

  // Visit stores to a local variable (alloc_box), temporary storage
  // (alloc_stack). This handles stores of the entire value and stores
  // to a tuple element. Stores to a field within another nominal
  // value are considered lifetime dependence leaf uses; the type
  // system enforces non-escapability on the aggregate value.
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
    if case let .scope(beginAccess) = storeAddress.enclosingAccessScope {
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

let variableIntroducerTest = FunctionTest("variable_introducer") {
    function, arguments, context in
  let value = arguments.takeValue()
  print("Variable introducers of: \(value)")
  print(gatherVariableIntroducers(for: value, context))
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

let lifetimeDependenceRootTest = FunctionTest("lifetime_dependence_root") {
    function, arguments, context in
  let value = arguments.takeValue()
  _ = LifetimeDependence.visitDependenceRoots(enclosing: value, context) {
    scope in
    print("Scope: \(scope)")
    return .continueWalk
  }
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
