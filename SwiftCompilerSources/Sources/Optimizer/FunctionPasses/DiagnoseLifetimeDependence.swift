//===--- DiagnoseLifetimeDependence.swift - Lifetime dependence -----------===//
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
/// Some SIL concepts are defined here for now until the can be formalized as fundamental SIL types.
///
/// Definition: non-escapable value. Any value that either has a nonescapable type or is produced by a lifetime-dependent instruction.
///
/// Consider the non-escapable value, %nonEscapableValue, in the example below:
///
///     %access = begin_access %address : $*Container
///     %outer = apply @returnsNonEscapable(%access)
///               : $(@resultDependsOn @in Container) -> NonEscapableStruct
///     %inner = apply @forwardsNonEscapable(%outer)
///               : $(@resultDependsOn @owned NonEscapableStruct)
///               -> NonEscapableStruct
///  -> %nonEscapableValue = copy_value %inner : $NonEscapableStruct
///     %_ = apply @usesNonEscapable(%nonEscapableValue)
///          : $(NonEscapableStruct) -> ()
///     end_access %access : $*Container
///
/// Definition: lifetime introducer. Any owned or guaranteed value that is not produced by a forwarding operation or a copy. (`begin_borrow` of an owned value is non-forwarding so it does introduce a lifetime).
///
/// Definition: non-escaping scope. The lifetime of value that introduces the lifetime of a non-escapable value.
///
/// Each non-escapable value in a function has a forwarded and copied lifetime introducer. The lifetime of %nonEscapableValue is introdued by %inner.
///
///     %inner = apply @forwardsNonEscapable(%outer)
///               : $(@resultDependsOn @owned NonEscapableStruct)
///
/// The lifetime introducer may be recognized as an instruction that transfers lifetime dependence from one value to another. An apply with a @resultDependsOn parameter transfers lifetime dependence from the parameter (the parent lifetime) to the result (the dependent lifetime).
///
/// Lifetime *propagation* occurs when the parent lifetime is itself nonescapable. Finding the non-escaping scope for lifetime-propagation involves recursively asking for the parent's introducer. In this example, recursion gives us %outer as the next introducer, and %access as the next parent lifetime:
///
///     %outer = apply @returnsNonEscapable(%access)
///               : $(@resultDependsOn @in Container) -> NonEscapableStruct
///
/// Lifetime *encapsulation* (as opposed to propagation) occurs when the parent lifetime is escapable. In this case, we find the parent's enclosing scope as follows:
///
/// For owned values, the value's lifetime is itself the enclosing scope, delimited by the consumes after following any forwarding operations.
///
/// For borrowed values, the enclosing scope is created by the borrow introducer (visitBorrowIntroducers).
///
/// For addresses, the enclosing scope is created by an access scope (enclosingAcessScope). Note that an exclusive access scope *must* exist for all non-escaping values that depend on an addressible value.
///
/// In this example, the parent value is an address, so the non-escaping scope is an access scope:
///
///     %access = begin_access %address : $*Container
///
//===----------------------------------------------------------------------===//

import SIL

// DiagnosticEngine already declares DiagID. Why isn't it visible?
import ASTBridging
public typealias DiagID = BridgedDiagID

private let verbose = false

private func log(_ message: @autoclosure () -> String) {
  if verbose {
    print("### \(message())")
  }
}

private extension InstructionRange {
  // TODO: InstructionRange does not yet support phis or function args. This will be problematic when used in a pass that mutates the block.
  static func getRepresentativeInstruction(for value: Value) -> Instruction {
    if let def = value.definingInstruction {
      return def
    }
    if let result = TerminatorResult(value) {
      return result.terminator
    }
    assert(Phi(value) != nil || value is FunctionArgument)
    return value.parentBlock.instructions.first!
  }
}

/// Diagnostic pass.
///
/// Find the roots of all non-escapable values in this function. All non-escapable values either depend on a NonEscapingScope, or they are produced by a LifetimeDependentInstruction that has no dependence on a parent value (@_unsafeNonEscapableResult).
let diagnoseLifetimeDependencePass = FunctionPass(
  name: "diagnose-lifetime-dependence")
{ (function: Function, context: FunctionPassContext) in
  log("Diagnosing lifetime dependence in \(function.name)")
  
  var diagnostic = DiagnoseLifetimeDependence(context)
  defer { diagnostic.deinitialize() }
  // Finding the scopes of all non-escapable values would be overkill. They are all rooted in either a non-escapable function argument or a LifetimeDependentInstruction, so use those as starting points.
  for argument in function.arguments {
    if !argument.type.isEscapable {
      diagnostic.diagnose(scope: NonEscapingScope.caller(argument))
    }
  }
  for instruction in function.instructions {
    guard let dependentInst = LifetimeDependentInstruction(instruction) else { continue }
    precondition(instruction.results.count > 0, "a lifetime-dependent instruction must have at least one result")
    _ = dependentInst.visitDependentValues { value in
      // use-def visitor.
      NonEscapingScope.visitScopes(enclosing: value, context) {
        diagnostic.diagnose(scope: $0)
      }
      return .continueWalk
    }
  }
}

/// An instruction that produces a non-escapable value, linking it to a parent value.
private struct LifetimeDependentInstruction {
  let instruction: Instruction

  init?(_ instruction: Instruction?) {
    guard let instruction else { return nil }
    self.instruction = instruction
    if instruction is FullApplySite {
      // TODO: If the apply has a @_resultDependsOn attribute, then it is lifetime-dependent regardless of the result type.
      let abortIfNonEscapable = visitDependentValues { value in
        if !value.type.isEscapable {
          return .abortWalk
        }
        return .continueWalk
      }
      if abortIfNonEscapable == .abortWalk {
        // at least one non-escapable result means this is a lifetime-dependent instruction.
        return
      }
    }
    return nil
  }
  /// A nil parent implies that the dependent values are not enclosed by a NonEscapingScope. Their non-escaping scope is simply the lifetime of the dependent value. This happens when 'self' is a call to a @_unsafeNonEscapableResult  function.
  var dependsOnParent: Value? {
    /// TODO: when function parameters support a @_resultDependsOn attribute, set the `parent` to the corresponding function argument.
    return nil
  }
  
  // visit the immediate uses that depend on this instruction's lifetime.
  func visitDependentValues(_ visitor: (Value) -> WalkResult) -> WalkResult {
    if let beginApply = instruction as? BeginApplyInst {
      for yieldedValue in beginApply.yieldedValues {
        if visitor(yieldedValue) == .abortWalk {
          return .abortWalk
        }
      }
      return .continueWalk
    }
    let apply = instruction as! FullApplySite
    return visitor(apply.singleDirectResult!)
  }
}

/// A scope that encloses lifetime-dependent values.
private enum NonEscapingScope {
  /// A non-escapable or guaranteed argument whose scope is provided by the caller and covers the entire function.
  case caller(Argument)
  /// A scoped instruction that may enclose nonescapable values. Always a ScopedInstruction.
  case scoped(SingleValueInstruction)
  /// An owned value whose OSSA lifetime encloses nonescapable values
  case owned(Value)
  /// A value produced by a lifetime-dependent instuction with no parent lifetime (@_unsafeNonEscapableResult).
  case orphan(Value)
  
  func checkPrecondition() {
    switch self {
    case let .caller(argument):
      precondition(!argument.type.isEscapable || argument.ownership == .guaranteed,
                   "only non-escapable or guaranteed arguments have a caller scope")
    case let .scoped(inst):
      // We can't use ScopedInstruction because it is a broken abstraction. We need to code for each conformance explicitly.
      precondition(inst is BeginAccessInst)
    case let .owned(value):
      precondition(value.ownership == .owned)
    case let .orphan(value):
      precondition(
        LifetimeDependentInstruction(value.definingInstruction) != nil ,
        "an lifetime-dependent orphan must be produced by a LifetimeDependentInstruction")
    }
  }
  
  var definingValue: Value {
    switch self {
    case let .caller(argument): return argument
    case let .scoped(singleValueInst): return singleValueInst
    case let .owned(value): return value
    case let .orphan(value): return value
    }
  }

  var function: Function {
    definingValue.parentFunction // definingValue can't be undef
  }
}

/// Walk up to the enclosing scope.
private extension NonEscapingScope {
  /// Visit the non-escaping scopes that enclose a non-escapable value. Performs a use-def walk from \p value, which always terminates at a set of NonEscapableScopes.
  ///
  ///  TODO: By making UseDefVisitor a NonEscapable type, we should be able to make \p visitor a non-escaping closure.
  static func visitScopes(enclosing value: Value, _ context: Context,
                          _ visitor: @escaping (NonEscapingScope) -> ()) {
    var useDefVisitor = UseDefVisitor(context, visitor)
    defer { useDefVisitor.deinitialize() }
    _ = useDefVisitor.walkUp(value: value)
  }
  
  private struct UseDefVisitor : ForwardingUseDefWalker {
    private let context: Context
    // This visited set is only really needed for instructions with multiple results, including phis.
    private var visitedValues: ValueSet
    // Call \p visit rather than calling this directly.
    private let visitorClosure: (NonEscapingScope) -> Void
    
    init(_ context: Context,
         _ visitor: @escaping (NonEscapingScope) -> Void) {
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

    private func visit(scope: NonEscapingScope) {
      scope.checkPrecondition()
      visitorClosure(scope)
    }

    mutating func introducer(_ value: Value) -> WalkResult {
      // An escapable, owned introducer directly defines the non-escaping scope. Check for these before checking for a FunctionArgument, because an owned introducer's lifetime ends inside the callee.
      if value.type.isEscapable && value.ownership == .owned {
        visit(scope: NonEscapingScope.owned(value))
        return .continueWalk
      }
      guard let argument = value as? FunctionArgument else {
        fatalError("a non-escapable value must be enclosed by an escapable owned value or a function argument")
      }
      visit(scope: NonEscapingScope.caller(argument))
      return .continueWalk
    }

    // TODO: This needs to match LifetimeDependenceWalker. Move them into symmetric walkers.
    mutating func walkUp(value: Value) -> WalkResult {
      // For addresses, visit the access scope.
      if value.type.isAddress {
        let accessScope = value.enclosingAccessScope
        guard case let .scope(access) = accessScope else {
          fatalError("a non-escaping scope's address must be in an access scope")
        }
        visit(scope: NonEscapingScope.scoped(access))
        return .continueWalk
      }
      // TODO: handle LoadOperations like copies. If the loaded value is escapable, then call walkUp on the load's addrses. If the loaded value is non-escapable, then use reaching definition analysis to continue at the stored values.
      if let copyInst = value.definingInstruction as? CopyValueInst {
        return walkUp(value: copyInst.fromValue)
      }
      if let moveInst = value.definingInstruction as? MoveValueInst {
        return walkUp(value: moveInst.fromValue)
      }
      if let borrow = value.definingInstruction as? BeginBorrowInst {
        return walkUp(value: borrow.borrowedValue)
      }
      // Check for lifetime transfer through a lifetime-dependent instruction.
      if let dependentInst = LifetimeDependentInstruction(value.definingInstruction) {
        guard let parent = dependentInst.dependsOnParent else {
          // Handle an orphaned lifetime (@_unsafeNonEscapableResult).
          visit(scope: NonEscapingScope.orphan(value))
          return .continueWalk
        }
        // Recurse along the lifetime dependent instruction's parent.
        // This handles both lifetime propagation from one non-escapable value to another and lifetime encapsulation from an escapable value.
        return walkUp(value: parent)
      }
      return walkUpDefault(value: value)
    }
  }
}

private extension NonEscapingScope {
  /// Returns nil if the scope covers the entire function.
  ///
  /// Note: The caller must deinitialize the returned range.
  func computeRange(_ context: Context) -> InstructionRange? {
    switch self {
    case .caller: return nil
    case let .scoped(singleValueInst):
      var range = InstructionRange(begin: singleValueInst, context)
      for endInst in (singleValueInst as! BeginAccessInst).endInstructions {
        range.insert(endInst)
      }
      return range
    case let .owned(value):
      return NonEscapingScope.computeForwardingRange(value: value, context)
    case let .orphan(value):
      if value.type.isAddress {
        return NonEscapingScope.computeIndirectResultRange(value: value,
                                                           context)
      }
      return NonEscapingScope.computeForwardingRange(value: value, context)
    }
  }
  
  // TODO: need a utility for addressible lifetimes. This only happens for indirect function results. Query reaching definitions for each destroy_addr.
  private static func computeIndirectResultRange(
    value: Value, _ context: Context
  ) -> InstructionRange {
    let def = value.definingInstruction!
    // Empty range placeholder.
    return InstructionRange(begin: def, context)
  }
  
  // TODO: This should be a basic ForwardedLiveness utility. See OwnershipLiveness.h. It is a minor extension of LinearLiveness that follows ForwardedOperations.
  private static func computeForwardingRange(
    value: Value, _ context: Context
  ) -> InstructionRange {
    assert(!value.type.isAddress)
    // InstructionRange cannot directly represent the beginning of the block
    // so we fake it with getRepresentativeInstruction().
    let def = InstructionRange.getRepresentativeInstruction(for: value)
    var range = InstructionRange(begin: def, context)
    // TODO: This depends on OSSA lifetime completion, which is not currently running.
    let consumes = value.uses.lazy.filter { $0.endsLifetime }
    for consume in consumes {
      // TODO: follow ownership-forwarding operations
      range.insert(consume.instruction)
    }
    return range
  }
}


/// Lifetime dependence diagnostic
///
/// A. For each non-escaping scope, compute its range.
///
/// B. Compute all values that have a lifetime dependence on the non-escaping scope and check that they fall within the scope. This performs a def-use walk which is the inverse of finding a non-escaping scope from a non-escapable value.
private struct DiagnoseLifetimeDependence {
  let context: FunctionPassContext
  var checkedValues: ValueSet
  
  init(_ context: FunctionPassContext) {
    self.context = context
    self.checkedValues = ValueSet(context)
  }
  
  mutating func deinitialize() {
    checkedValues.deinitialize()
  }
  
  mutating func diagnose(scope: NonEscapingScope) {
    // If this scope was already checked, then skip it.
    if (!checkedValues.insert(scope.definingValue)) {
      return
    }
    log("Diagnosing lifetime dependence on scope: \(scope)")
    
    // Compute this scope's range.
    guard var range = scope.computeRange(context) else { return }
    defer { range.deinitialize() }
    
    // Check each lifetime-dependent use via a def-use visitor
    var scopeWalker = DiagnoseScopeUseWalker(scope, range: range, context)
    defer { scopeWalker.deinitialize() }
    let result = scopeWalker.walkDown(root: scope.definingValue)
    // TODO: print SIL-level diagnostics wherever we do .abortWalk
    assert(result == .continueWalk, "unimplemented lifetime dependence diagnostic")
  }
}

private struct DiagnoseScopeUseWalker : LifetimeDependenceWalker {
  let scope: NonEscapingScope
  let range: InstructionRange
  let function: Function
  let context: FunctionPassContext
  var visitedValues: ValueSet
  
  init(_ scope: NonEscapingScope, range: InstructionRange,
    _ context: FunctionPassContext) {
    self.scope = scope
    self.range = range
    self.function = scope.function
    self.context = context
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
    // Ignore a dead root value. It never escapes.
    if let operand {
      diagnose(leaf: operand)
    }
    return .continueWalk
  }

  mutating func leafUse(_ operand: Operand) -> WalkResult {
    diagnose(leaf: operand)
    return .continueWalk
  }

  func diagnose(leaf operand: Operand) {
    if !range.inclusiveRangeContains(operand.instruction) {
      reportError(operand: operand, diagID: .lifetime_outside_scope_use)
      return
    }
    log("Range contains: \(operand.instruction)")
    if !DiagnoseScopeUseWalker.hasKnownLifetime(operand) {
      reportError(operand: operand, diagID: .lifetime_outside_scope_escape)
    }
  }

  static func hasKnownLifetime(_ operand: Operand) -> Bool {
    switch operand.ownership {
    case .nonUse, .borrow, .reborrow, .guaranteedForwarding:
      assert(false, "visitNonEscapableUsers should bypass: \(operand)")
    case .trivialUse, .instantaneousUse, .unownedInstantaneousUse, .endBorrow:
      return true
    case .forwardingUnowned, .pointerEscape, .bitwiseEscape, .forwardingConsume,
      .interiorPointer:
      // TODO: handle all interiorPointers with a standard ownership def-use walker so we never reach here.
      //
      // TODO: fix forwardingConsume to only refer to instructions that produce forwarded value in the current scope so we never reach here (ForwardingDefUseWalker will bypass them).
      if let returnInst = operand.instruction as? ReturnInst {
        return returnInst.parentFunction.hasUnsafeNonEscapableResult
      }
      return false
    case .destroyingConsume:
      switch operand.instruction {
      case is DeallocBoxInst, is DeallocExistentialBoxInst, is DeallocRefInst,
        is DestroyValueInst:
        // TODO: Add EndLifetimeInst
        return true
      default:
        return false
      }
    }
  }

  func reportError(operand: Operand, diagID: DiagID) {
    // Identify the escaping value
    let forwardedDecl = ForwardedDeclaration(operand: operand, context)
    let varName = forwardedDecl.name
    if let varName {
      context.diagnosticEngine.diagnose(forwardedDecl.sourceLoc,
        .lifetime_variable_outside_scope,
        varName)
    } else {
      context.diagnosticEngine.diagnose(forwardedDecl.sourceLoc,
        .lifetime_value_outside_scope)
    }
    // Identify the parent scope
    switch scope {
    case .orphan:
      break
    default:
      // TODO: add bridging for function argument locations
      // [SILArgument.getDecl().getLoc()]
      let parentSourceLoc = scope.definingValue.definingInstruction?.location.sourceLoc
      context.diagnosticEngine.diagnose(parentSourceLoc, .lifetime_outside_scope_parent)
    }
    
    // Identify the use point.
    let userSourceLoc = operand.instruction.location.sourceLoc
    context.diagnosticEngine.diagnose(userSourceLoc, diagID)
  }
}

// TODO: make a utility.
private extension Instruction {
  func findVarDecl() -> VarDecl? {
    if let varDeclInst = self as? VarDeclInstruction {
      return varDeclInst.varDecl
    }
    for result in results {
      for use in result.uses {
        if let debugVal = use.instruction as? DebugValueInst {
          return debugVal.varDecl
        }
      }
    }
    return nil
  }
}

// Identifies a forwarded value for diagnostics.
private struct ForwardedDeclaration {
  var sourceLoc: SourceLoc?
  var varDecl: VarDecl?
  
  var name: String? {
    return varDecl?.userFacingName
  }
  
  init(operand: Operand, _ context: Context) {
    var sourceLoc: SourceLoc?
    var varDecl: VarDecl?
    NonEscapingScope.visitScopes(enclosing: operand.value, context) { scope in
      if varDecl != nil { return }
      // TODO: get the source location for block arguments.
      guard let def = scope.definingValue.definingInstruction else { return }
      varDecl = def.findVarDecl()
      if let varDecl {
        sourceLoc = varDecl.sourceLoc
      } else {
        sourceLoc = def.location.sourceLoc
      }
    }
    self.sourceLoc = sourceLoc
    self.varDecl = varDecl
  }
}

/// Walk down to the lifetime-dependent uses.
///
/// Requires needWalk(for:), leafUse(), deadValue().
///
/// Delegates value def-use walking to the ForwardingUseDefWalker and overrides copies and moves.
///
/// Delegates address def-use walking to the AddressDefUseWalker and overrides loads and copies.
///
/// Ignores trivial values (~Escapable types are never trivial. Escapable non-trivial types may be lifetime-depenent values).
///
/// TODO: override BeginAccessInst to handle EndAccessInst.
private protocol LifetimeDependenceWalker
: ForwardingDefUseWalker, AddressDefUseWalker where Path == UnusedWalkingPath {
  var function: Function { get }
}

extension LifetimeDependenceWalker {
  // Returns .abortWalk if either the closure aborts or the address walker fails to recognize an address use. In the future, the address walker will be guaranteed to succeed, and this confusion will be avoided.
  mutating func walkDownUses(of value: Value, using operand: Operand?)
  -> WalkResult {
    if value.type.isTrivial(in: function) {
      return .continueWalk
    }
    if value.type.isAddress {
      return walkDownUses(ofAddress: value, path: UnusedWalkingPath())
    }
    return walkDownUsesDefault(of: value, using: operand)
  }

  mutating func walkDown(operand: Operand) -> WalkResult {
    switch operand.instruction {
    // TODO: handle stores like copies using a standard reaching-def analysis.
    case let copy as CopyValueInst:
      return walkDownUses(of: copy, using: operand)

    case let move as MoveValueInst:
      return walkDownUses(of: move, using: operand)

    case let borrow as BeginBorrowInst:
      for endBorrow in borrow.endBorrows {
        if leafUse(endBorrow.operand) == .abortWalk {
          return .abortWalk
        }
      }
      return walkDownUses(of: borrow, using: operand)

    default:
      // Follow lifetime dependencies
      if let dependentInst = LifetimeDependentInstruction(operand.instruction) {
        return dependentInst.visitDependentValues {
          walkDownUses(of: $0, using: operand)
        }
      }
      return walkDownDefault(operand: operand)
    }
  }

  // Walk down interior pointers.
  //
  // TODO: This must be complete for correctness. Never abort the walk. Replace this with a standard address def-use walker that exhaustively recognizes all uses that could propagate a value out of the addressible memory vs. instantaneous uses. This is needed for correctness. In C++, this is the TransitiveAddressWalker. The only custom logic here should be the lifetime-dependence check and the reaching-def check for copy_addr.
  mutating func walkDown(address: Operand, path: Path) -> WalkResult {
    // Lifetime dependence ignores trivial types.
    if address.value.type.objectType.isTrivial(in: function) {
      return .continueWalk
    }
    // Follow lifetime dependencies.
    if let dependentInst =
    LifetimeDependentInstruction(address.instruction) {
      return dependentInst.visitDependentValues {
        return walkDownUses(of: $0, using: address)
      }
    }
    // Follow loaded values.
    switch address.instruction {
      // TODO: the real address def-use walker will handle all loads as a single callback.
    case let load as LoadInst:
      return walkDownUses(of: load, using: address)

    case let load as LoadBorrowInst:
      return walkDownUses(of: load, using: address)

    default:
      return walkDownDefault(address: address, path: path)
    }
  }

  mutating func leafUse(address: Operand, path: UnusedWalkingPath)
  -> WalkResult {
    // stores delegate to the implementation's general leafUse().
    switch address.instruction {
    case let copy as CopyAddrInst:
      // Ignore copies to this address, just like stores.
      //
      // TODO: handle copies from this address by performing a standard reaching-def analysis on the destination address and continuing the walk at the loads for which this is the reaching-def.
      return address == copy.sourceOperand ? .abortWalk : leafUse(address)

      // TODO: the real address def-use walker will handle all kinds of stores as a single callback.
    case is StoreInst:
      return leafUse(address)

    case let apply as FullApplySite:
      if let callerArgIdx = apply.argumentIndex(of: address) {
        let calleeArgIdx = apply.calleeArgIndex(callerArgIndex: callerArgIdx)
        let convention = apply.getArgumentConvention(calleeArgIndex: calleeArgIdx)
        if convention.isIndirectOut {
          return leafUse(address)
        }
      }
      return .abortWalk

    default:
      return .abortWalk
    }
  }
}
