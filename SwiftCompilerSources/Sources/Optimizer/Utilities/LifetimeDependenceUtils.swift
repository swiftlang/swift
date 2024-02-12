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

import SIL

/// Walk up the value dependence chain to find the best-effort
/// variable declaration. Typically called while diagnosing an error.
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
func gatherVariableIntroducers(for value: Value, _ context: Context) -> [Value]
{
  var introducers: [Value] = []
  var useDefVisitor = VariableIntroducerUseDefWalker(context) {
    introducers.append($0)
    return .continueWalk
  }
  defer { useDefVisitor.deinitialize() }
  _ = useDefVisitor.walkUp(value: value)
  return introducers
}

/// A lifetime dependence represents a scope in which some parent
/// value is alive and accessible along with a dependent value. All
/// values derived from the dependent value must be used within this
/// scope. This supports diagnostics on non-escapable types.
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
/// scope that the parent value represents, and a dependent value. A
/// self-dependence has the same parent and dependent value:
///
///   %dependent = mark_dependence [nonescaping] %value on %value
///
/// Self-dependence is useful to ensure that derived values, including
/// copies, do not escape the lifetime of the original
/// value. Non-escapable function arguments are implicitly
/// self-dependent, meaning that the argument's value does not escape
/// the function body. Note that we do not insert a 'mark_dependence
/// [nonescaping]' for function arguments because the caller must
/// already represent the argument's dependence on some parent
/// value. That parent value may not be the value directly passed to
/// the argument. After inlining, an additional self-dependence on
/// argument value would be overly strict.
struct LifetimeDependence : CustomStringConvertible {
  enum Scope : CustomStringConvertible {
    /// A non-escapable, guaranteed, or inout argument whose scope is provided
    /// by the caller and covers the entire function.
    case caller(Argument)
    /// An access scope.
    case access(BeginAccessInst)
    /// An coroutine.
    case yield(Value)
    /// An owned value whose OSSA lifetime encloses nonescapable values
    case owned(Value)
    /// Singly-initialized addressible storage (likely for an
    /// immutable address-only value). The entire initialized region
    /// is a lifetime (as opposed to an individual access for mutable
    /// variables). e.g. A value produced by an @in FunctionArgument
    /// or @out apply.
    case initialized(Value)
    // TODO: make .unknown a SIL Verification error
    case unknown(Value)

    var parentValue: Value {
      switch self {
      case let .caller(argument): return argument
      case let .access(beginAccess): return beginAccess
      case let .yield(value): return value
      case let .owned(value): return value
      case let .initialized(value): return value
      case let .unknown(value): return value
      }
    }

    func checkPrecondition() {
      switch self {
      case let .caller(argument):
        precondition(!argument.type.isEscapable
          || argument.ownership == .guaranteed,
          "only non-escapable or guaranteed arguments have a caller scope")
      case .access, .unknown:
        break        
      case let .yield(value):
        precondition(value.definingInstruction is BeginApplyInst)
      case let .owned(value):
        precondition(value.ownership == .owned)
      case let .initialized(value):
        precondition(value.type.isAddress, "expected an address")
      }
    }
    var description: String {
      {
        switch self {
        case .caller: return "Caller: "
        case .access: return "Access: "
        case .yield: return "Yield: "
        case .owned: return "Owned: "
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
    dependentValue.parentFunction // dependentValue can't be undef
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
    self.scope = Scope(base: arg, context)!
    self.dependentValue = arg
  }

  /// Construct LifetimeDependence from mark_dependence [nonescaping]
  ///
  /// TODO: Add SIL verification that all mark_depedence [nonescaping]
  /// have a valid LifetimeDependence.
  init?(_ markDep: MarkDependenceInst, _ context: some Context) {
    guard markDep.isNonEscaping else { return nil }
    guard let scope = Scope(base: markDep.base, context) else {
      return nil
    }
    self.scope = scope
    self.dependentValue = markDep
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
  /// Construct a lifetime dependence scope from the base value that
  /// other values depend on. This derives the kind of dependence
  /// scope and its parentValue from `base`.
  ///
  /// `base` represents the OSSA lifetime that the dependent value
  /// must be used within. If `base` is owned, then it directly
  /// defines the parent lifetime. If `base` is guaranteed, then it
  /// must have a single borrow introducer, which defines the parent
  /// lifetime. `base` must not be derived from a guaranteed phi or
  /// forwarded (via struct/tuple) from multiple guaranteed values.
  init?(base: Value, _ context: some Context) {
    if base.type.isAddress {
      if let scope = Self(address: base, context) {
        self = scope
        return
      }
      return nil
    }
    switch base.ownership {
    case .owned:
      self = .owned(base)
      return
    case .guaranteed:
      if let scope = Self(guaranteed: base, context) {
        self = scope
        return
      }
      return nil
    case .none:
      // lifetime dependence requires a nontrivial value"
      return nil
    case .unowned:
      self = .unknown(base)
    }
  }

  private init?(address: Value, _ context: some Context) {
    switch address.enclosingAccessScope {
    case let .scope(access):
      self = .access(access)
    case let .base(accessBase):
      switch accessBase {
      case let .box(projectBox):
        self = .owned(projectBox.operand.value)
      case let .stack(allocStack):
        self = .initialized(allocStack)
      case .global:
        self = .unknown(address)
      case .class, .tail:
        let refElt = address as! UnaryInstruction
        if let scope = Self(guaranteed: refElt.operand.value, context) {
          self = scope
        }
        return nil
      case let .argument(arg):
        if arg.convention.isIndirectIn {
          self = .initialized(arg)
        } else if arg.convention.isInout {
          self = .caller(arg)
        } else {
          self = .unknown(address)
        }
      case let .yield(result):
        self = Self(yield: result)
      case .pointer, .unidentified:
        self = .unknown(address)
      }
    }
  }

  private init?(guaranteed base: Value, _ context: some Context) {
    var introducers = Stack<Value>(context)
    gatherBorrowIntroducers(for: base, in: &introducers, context)
    // If introducers is empty, then the dependence is on a trivial value, so
    // there is no dependence scope.
    //
    // TODO: Add a SIL verifier check that a mark_dependence [nonescaping]
    // base is never a guaranteed phi.
    guard let introducer = introducers.pop() else { return nil }
    assert(introducers.isEmpty,
           "guaranteed phis not allowed when diagnosing lifetime dependence")
    guard let beginBorrow = BeginBorrowValue(introducer) else {
      fatalError("unknown borrow introducer")
    }
    switch beginBorrow {
    case .beginBorrow, .loadBorrow:
      let borrowOperand = beginBorrow.baseOperand!
      guard let scope = LifetimeDependence.Scope(base: borrowOperand.value,
                                                 context) else {
        return nil
      }
      self = scope
    case let .beginApply(value):
      self = .yield(value)
    case .functionArgument:
      self = .caller(beginBorrow.value as! Argument)
    case .reborrow:
      fatalError("reborrows are not supported in diagnostics")
    }
  }

  private init(yield result: MultipleValueInstructionResult) {
    // Consider an @in yield an .initialized scope. We must find the destroys.
    let apply = result.parentInstruction as! FullApplySite
    if apply.convention(of: result).isIndirectIn {
      self = .initialized(result)
      return
    }
    self = .yield(result)
  }
}

extension LifetimeDependence.Scope {
  /// Compute the range of the dependence scope. 
  ///
  /// Returns nil if the dependence scope covers the entire function.
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
      // FIXME: This should compute forwarded liveness instead.
      return computeLinearLiveness(for: value, context)
    case let .initialized(value):
      return LifetimeDependence.Scope.computeInitializedRange(value: value,
                                                              context)
    case let .unknown(value):
      // Return an empty range.
      return InstructionRange(for: value, context)
    }
  }
  
  private static func computeInitializedRange(value: Value, _ context: Context)
    -> InstructionRange {
    assert(value.type.isAddress)

    var range: InstructionRange
    if value is FunctionArgument {
      range = InstructionRange(for: value, context)
    } else if let singleStore = findSingleStore(into: value, context) {
      range = InstructionRange(begin: singleStore, context)
    } else {
      // return an empty range
      return InstructionRange(for: value, context)
    }
    for use in value.uses {
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

  private static func findSingleStore(into address: Value, _ context: Context)
    -> Instruction? {
    var singleStore: Instruction?
    for use in address.uses {
      if use.isAddressInitialization {
        if singleStore != nil {
          return nil
        }
        singleStore = use.instruction
      }
    }
    return singleStore
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
    return useDefVisitor.walkUp(value: value)
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
    
    mutating func needWalk(for value: Value) -> Bool {
      visitedValues.insert(value)
    }

    // Visit the base value of a lifetime dependence. If the base is
    // an address, the dependence scope is the enclosing access. The
    // walker does not walk past an `mark_dependence [nonescaping]`
    // that produces an address, because that will never occur inside
    // of an access scope. An address type mark_dependence
    // [nonescaping]` can only result from an indirect function result
    // when opaque values are not enabled.
    mutating func introducer(_ value: Value) -> WalkResult {
      guard let scope = LifetimeDependence.Scope(base: value, context)
      else {
        return .continueWalk
      }
      scope.checkPrecondition()
      return visitorClosure(scope)
    }
  }
}

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
 
  mutating func needWalk(for value: Value) -> Bool {
    visitedValues.insert(value)
  }

  mutating func introducer(_ value: Value) -> WalkResult {
    return visitorClosure(value)
  }

  mutating func walkUp(value: Value) -> WalkResult {
    switch value.definingInstruction {
    case let moveInst as MoveValueInst:
      if moveInst.isFromVarDecl {
        return introducer(moveInst)
      }
    case let borrow as BeginBorrowInst:
      if borrow.isFromVarDecl {
        return introducer(borrow)
      }
    default:
      break
    }
    return walkUpDefault(dependent: value)
  }
}

/// Walk up dominating dependent values.
///
/// Find the roots of a dependence chain stopping at phis. These root
/// LifeDependence instances are the value's "inherited"
/// dependencies. In this example, the dependence root is copied,
/// borrowed, and forwarded before being used as the base operand of
/// `mark_dependence`. The dependence "root" is the parent of the
/// outer-most dependence scope.
///
///   %root = apply                  // lifetime dependence root
///   %copy = copy_value %root
///   %parent = begin_borrow %copy   // lifetime dependence parent value
///   %base = struct_extract %parent // lifetime dependence base value
///   %dependent = mark_dependence [nonescaping] %value on %base
///
/// This extends the ForwardingDefUseWalker, which finds the
/// forward-extended lifetime introducers. Certain forward-extended
/// lifetime introducers can inherit a lifetime dependency from their
/// operand: namely copies, moves, and borrows. These introducers are
/// considered part of their operand's dependence scope because
/// non-escapable values can be copied, moved, and
/// borrowed. Nonetheless, all of their uses must remain within
/// original dependence scope.
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
/// All of the dependent uses including `end_borrow %5` and
/// `destroy_value %4` must be before the end of the dependence scope:
/// `destroy_value %parent`. In this case, the dependence parent is an
/// owned value, so the scope is simply the value's OSSA lifetime.
///
/// Minimal requirements:
///   var context: Context
///   introducer(_ value: Value) -> WalkResult
///   needWalk(for value: Value) -> Bool
///
/// Start walking:
///   walkUp(value: Value) -> WalkResult
protocol LifetimeDependenceUseDefWalker : ForwardingUseDefWalker {
  var context: Context { get }

  mutating func introducer(_ value: Value) -> WalkResult

  // Minimally, check a ValueSet. This walker may traverse chains of aggregation and destructuring along with phis.
  mutating func needWalk(for value: Value) -> Bool

  mutating func walkUp(value: Value) -> WalkResult
}

extension LifetimeDependenceUseDefWalker {
  mutating func walkUp(value: Value) -> WalkResult {
    walkUpDefault(dependent: value)
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
  mutating func walkUpDefault(dependent value: Value) -> WalkResult {
    switch value.definingInstruction {
    case let copyInst as CopyValueInst:
      return walkUp(value: copyInst.fromValue)
    case let moveInst as MoveValueInst:
      return walkUp(value: moveInst.fromValue)
    case let borrow as BeginBorrowInst:
      return walkUp(value: borrow.borrowedValue)
    case let load as LoadInstruction:
      return walkUp(value: load.address)
    case let markDep as MarkDependenceInst:
      if let dependence = LifetimeDependence(markDep, context) {
        return walkUp(value: dependence.parentValue)
      }
    default:
      break
    }
    // If the dependence chain has a phi, consider it a root. Dependence roots
    // are currently expected to dominate all dependent values.
    if Phi(value) != nil {
      return introducer(value)
    }
    return walkUpDefault(forwarded: value)
  }
}

/// Walk down dependent values.
///
/// Delegates value def-use walking to the ForwardingUseDefWalker and
/// overrides copy, move, borrow, and mark_dependence.
///
/// Ignores trivial values (~Escapable types are never
/// trivial. Escapable types may only be lifetime-depenent values if
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
///   returnedDependence(result: Operand) -> WalkResult
///   returnedDependence(address: FunctionArgument, using: Operand) -> WalkResult
///
/// Start walking:
///   walkDown(root: Value)
protocol LifetimeDependenceDefUseWalker : ForwardingDefUseWalker,
                                          OwnershipUseVisitor,
                                          AddressUseVisitor {
  var function: Function { get }

  mutating func leafUse(of operand: Operand) -> WalkResult

  mutating func escapingDependence(on operand: Operand) -> WalkResult

  mutating func returnedDependence(result: Operand) -> WalkResult

  mutating func returnedDependence(address: FunctionArgument, using: Operand)
    -> WalkResult
}

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
    // Only track ~Escapable types.
    if value.type.isEscapable {
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
    if operand.instruction is ReturnInst, !operand.value.type.isEscapable {
      return returnedDependence(result: operand)
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
    case let copy as CopyingInstruction:
      return walkDownUses(of: copy, using: operand)

    case let move as MoveValueInst:
      return walkDownUses(of: move, using: operand)

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

    case let bi as BuiltinInst where bi.id == .Copy:  
      // This must be a non-address-lowered form of Builtin.Copy that
      // produces an owned value.
      assert(bi.ownership == .owned)
      return walkDownUses(of: bi, using: operand)

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
    // TODO: Call reaching-def analysis on the local variable that
    // defines `address`. Find the store that reaches this load. Then
    // find all reachable uses from that store.
    return visitStoredUses(of: operand, into: address.value)
  }

  mutating func dependentAddressUse(of operand: Operand, into value: Value)
    -> WalkResult {
    walkDownUses(of: value, using: operand)
  }    

  mutating func escapingAddressUse(of operand: Operand) -> WalkResult {
    if operand.instruction is ReturnInst, !operand.value.type.isEscapable {
      return returnedDependence(result: operand)
    }
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

    // TODO: Call reaching-def analysis on the local variable that
    // defines `address` (the analysis can be limited to this
    // store). Then find all reachable uses from this store.
    switch address.accessBase {
    case let .box(projectBox):
      if let allocBox = projectBox.box as? AllocBoxInst {
        if !needWalk(for: allocBox) {
          return .continueWalk
        }
        return walkDownUses(of: allocBox, using: operand)
      }
      break
    case let .stack(allocStack):
      if !needWalk(for: allocStack) {
        return .continueWalk
      }
      return walkDownAddressUses(of: allocStack)
    case let .argument(arg):
      if arg.convention.isIndirectIn {
        if !needWalk(for: arg) {
          return .continueWalk
        }
        return walkDownAddressUses(of: arg)
      }
      if arg.convention.isIndirectOut, !arg.type.isEscapable {
        return returnedDependence(address: arg, using: operand)
      }
      break
    case .global, .class, .tail, .yield, .pointer, .unidentified:
      break
    }
    return escapingDependence(on: operand)
  }

  private mutating func visitAppliedUse(of operand: Operand,
                                        by apply: FullApplySite) -> WalkResult {
    if let conv = apply.convention(of: operand), conv.isIndirectOut {
      return leafUse(of: operand)
    }

    // Use the convention of the original, unsubstituted function to
    // determine whether it provides a nonescapable guarantee on the
    // argument.
    guard let param = apply.operandConventions.originalParameter(of: operand)
    else {
      return leafUse(of: operand)
    }
    if param.interfaceType.isEscapable() {
      return escapingDependence(on: operand)
    }
    // If the call site returns any ~Escapable results after substitution,
    // then consider those results to be dependent uses.
    //
    // TODO: Replace these isEscapable checks with lifetime
    // dependence checks. For each result:
    //
    // If the result has no lifetime dependence, ignore it
    //
    // If the result has a lifetime dependence on this ~Escapable
    // operand, then ignore it. A mark-dependence was already
    // emitted for these, which will be checked independently.
    //
    // If the result has at least one lifetime dependence, but none
    // on this operand, it's use is considered dependent.
    if let result = apply.singleDirectResult, !result.type.isEscapable {
      if dependentUse(of: operand, into: result) == .abortWalk {
        return .abortWalk
      }
    }
    for resultAddr in apply.indirectResultOperands
        where !resultAddr.value.type.isEscapable {
      if visitStoredUses(of: operand, into: resultAddr.value) == .abortWalk {
        return .abortWalk
      }
    }
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
    print("Trivial Dependence")
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
  var visitedValues: ValueSet

  var function: Function
  
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

  mutating func returnedDependence(result: Operand) -> WalkResult {
    print("Returned use: \(result)")
    return .continueWalk
  }

  mutating func returnedDependence(address: FunctionArgument,
                                   using operand: Operand) -> WalkResult {
    print("Returned use: \(operand) in: \(address)")
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
