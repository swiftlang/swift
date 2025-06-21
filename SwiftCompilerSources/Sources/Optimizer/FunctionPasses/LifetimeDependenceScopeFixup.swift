//===--- LifetimeDependenceScopeFixup.swift ----------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===---------------------------------------------------------------------===//
///
/// LifetimeDependenceScopeFixup pass dependencies:
///
/// - must run after OSSA lifetime completion (and before invalidation)
///
/// - must run after LifetimeDependenceInsertion
///
/// - must run before LifetimeDependenceDiagnostics
///
/// Step 1. LifetimeDependenceInsertion inserts 'mark_dependence [unresolved]' instructions for applies that return a
/// lifetime dependent value.
///
/// Step 2. LifetimeDependenceScopeFixup visits each 'mark_dependence [unresolved]'. If the dependence base is an access
/// scope, then it extends the access and any parent accesses to cover all uses of the dependent value.
///
/// Step 3. DiagnoseStaticExclusivity diagnoses an error for any overlapping access scopes. We prefer to diagnose a
/// static exclusivity violation over a escaping violation. LifetimeDependenceScopeFixup is, therefore, allowed to
/// create overlapping access scopes.
///
/// Step 4. LifetimeDependenceDiagnostics visits each 'mark_dependence [unresolved]' again and will report a violation
/// for any dependent use that was not covered by the access scope.
///
/// This is conceptually a SILGen cleanup pass, because lifetime dependencies are invalid before it runs.
///
//===---------------------------------------------------------------------===//

import SIL

private let verbose = false

private func log(prefix: Bool = true, _ message: @autoclosure () -> String) {
  if verbose {
    debugLog(prefix: prefix, message())
  }
}

/// LifetimeDependenceScopeFixup visits each mark_dependence [unresolved]. It finds the access scope of the dependence
/// base and extends it to cover the dependent uses.
///
/// If the base's access scope ends before a dependent use:
///
///     %dependentVal = mark_dependence [unresolved] %v on %innerAccess
///     end_access %innerAccess
///     apply %f(%dependentVal)
///
/// Then sink the end_access:
///
///     %dependentVal = mark_dependence [unresolved] %v on %innerAccess
///     end_access %innerAccess
///     apply %f(%dependentVal)
///
/// Recursively extend all enclosing access scopes up to an owned value or function argument. If the inner dependence is
/// on a borrow scope, extend it first:
///
///     %outerAccess = begin_access %base
///     %innerAccess = begin_access %outerAccess
///     %innerBorrow = begin_borrow [var_decl] %innerAccess
///     %dependentVal = mark_dependence [unresolved] %v on %innerBorrow
///     end_borrow %innerBorrow
///     end_access %innerAccess
///     end_access %outerAccess
///     apply %f(%dependentVal)
///
/// Is rewritten as:
///
///     apply %f(%dependentVal)
///     end_borrow %innerBorrow
///     end_access %innerAccess
///     end_access %outerAccess
///
/// If the borrow scope is not marked [var_decl], then it has no meaningful scope for diagnostics. Rather than extending
/// such scope, could redirect the dependence base to its operand:
///
///     %dependentVal = mark_dependence [unresolved] %v on %innerAccess
///
/// If a dependent use is on a function return:
///
///     sil @f $(@inout) -> () {
///     bb0(%0: $*T)
///       %outerAccess = begin_access [modify] %0
///       %innerAccess = begin_access %outerAccess
///       %dependentVal = mark_dependence [unresolved] %v on %innerAccess
///       end_access %innerAccess
///       end_access %outerAccess
///       return %dependentVal
///
/// Then rewrite the mark_dependence base operand to a function argument:
///
///       %dependentVal = mark_dependence [unresolved] %v on %0
///
let lifetimeDependenceScopeFixupPass = FunctionPass(
  name: "lifetime-dependence-scope-fixup")
{ (function: Function, context: FunctionPassContext) in
  log(prefix: false, "\n--- Scope fixup for lifetime dependence in \(function.name)")

  let localReachabilityCache = LocalVariableReachabilityCache()

  for instruction in function.instructions {
    guard let markDep = instruction as? MarkDependenceInstruction else {
      continue
    }
    guard let innerLifetimeDep = LifetimeDependence(markDep, context) else {
      continue
    }
    // Redirect the dependence base to ignore irrelevant borrow scopes.
    let newLifetimeDep = markDep.rewriteSkippingBorrow(scope: innerLifetimeDep.scope, context)

    // Recursively sink enclosing end_access, end_borrow, end_apply, and destroy_value. If the scope can be extended
    // into the caller, return the function arguments that are the dependency sources.
    var scopeExtension = ScopeExtension(localReachabilityCache, context)
    guard scopeExtension.extendScopes(dependence: newLifetimeDep) else {
      continue
    }
    let args = scopeExtension.findArgumentDependencies()

    // If the scope cannot be extended to the caller, this must be the outermost dependency level.
    // Insert end_cow_mutation_addr if needed.
    if args.isEmpty {
      createEndCOWMutationIfNeeded(lifetimeDep: newLifetimeDep, context)
    }

    // Redirect the dependence base to the function arguments. This may create additional mark_dependence instructions.
    markDep.redirectFunctionReturn(to: args, context)
  }
}

private extension Type {
  func mayHaveMutableSpan(in function: Function, _ context: FunctionPassContext) -> Bool {
    if hasArchetype {
      return true
    }
    if isBuiltinType {
      return false
    }
    // Only result types that are nominal can have a MutableSpan derived from an inout array access.
    if nominal == nil {
      return false
    }
    if nominal == context.swiftMutableSpan {
      return true
    }
    if isStruct {
      guard let fields = getNominalFields(in: function) else {
        return false
      }
      return fields.contains { $0.mayHaveMutableSpan(in: function, context) }
    }
    if isTuple {
      return tupleElements.contains { $0.mayHaveMutableSpan(in: function, context) }
    }
    if isEnum {
      guard let cases = getEnumCases(in: function) else {
        return true
      }
      return cases.contains { $0.payload?.mayHaveMutableSpan(in: function, context) ?? false }
    }
    // Classes cannot be ~Escapable, therefore cannot hold a MutableSpan.
    if isClass {
      return false
    }
    return false
  }
}

/// Insert end_cow_mutation_addr for lifetime dependent values that maybe of type MutableSpan and depend on a mutable address.
private func createEndCOWMutationIfNeeded(lifetimeDep: LifetimeDependence, _ context: FunctionPassContext) {
  var scoped : ScopedInstruction

  // Handle cases which generate mutable addresses: begin_access [modify] and yield &
  switch lifetimeDep.scope {
    case let .access(beginAccess):
      if beginAccess.accessKind != .modify {
        return
      }
      scoped = beginAccess
    case let .yield(value):
      let beginApply = value.definingInstruction as! BeginApplyInst
      if value == beginApply.token {
        return
      }
      if beginApply.convention(of: value as! MultipleValueInstructionResult) != .indirectInout {
        return
      }
      scoped = beginApply
    // None of the below cases can generate a mutable address.
    case .owned, .borrowed, .local, .initialized, .caller, .global, .unknown:
      return
  }

  guard lifetimeDep.dependentValue.type.mayHaveMutableSpan(in: lifetimeDep.dependentValue.parentFunction, context) else {
    return
  }

  for endInstruction in scoped.endInstructions {
    let builder = Builder(before: endInstruction, context)
    builder.createEndCOWMutationAddr(address: lifetimeDep.parentValue)
  }
}

private extension MarkDependenceInstruction {
  /// Rewrite the mark_dependence base operand to ignore inner borrow scopes (begin_borrow, load_borrow).
  ///
  /// Note: this could be done as a general simplification, e.g. after inlining. But currently this is only relevant for
  /// diagnostics.
  func rewriteSkippingBorrow(scope: LifetimeDependence.Scope, _ context: FunctionPassContext) -> LifetimeDependence {
    guard let newScope = scope.ignoreBorrowScope(context) else {
      return LifetimeDependence(scope: scope, markDep: self)!
    }
    let newBase = newScope.parentValue
    if newBase != self.baseOperand.value {
      self.baseOperand.set(to: newBase, context)
    }
    return LifetimeDependence(scope: newScope, markDep: self)!
  }

  func redirectFunctionReturn(to args: SingleInlineArray<FunctionArgument>, _ context: FunctionPassContext) {
    var updatedMarkDep: MarkDependenceInstruction?
    for arg in args {
      guard let currentMarkDep = updatedMarkDep else {
        self.baseOperand.set(to: arg, context)
        updatedMarkDep = self
        continue
      }
      switch currentMarkDep {
      case let mdi as MarkDependenceInst:
        updatedMarkDep = mdi.redirectFunctionReturnForward(to: arg, input: mdi, context)
      case let mdi as MarkDependenceAddrInst:
        updatedMarkDep = mdi.redirectFunctionReturnAddress(to: arg, context)
      default:
        fatalError("unexpected MarkDependenceInstruction")
      }
    }
  }
}

private extension MarkDependenceInst {
  /// Rewrite the mark_dependence base operand, setting it to a function argument.
  ///
  /// This is called when the dependent value is returned by the function and the dependence base is in the caller.
  func redirectFunctionReturnForward(to arg: FunctionArgument, input: MarkDependenceInst,
    _ context: FunctionPassContext) -> MarkDependenceInst {
    // To handle more than one function argument, new mark_dependence instructions will be chained.
    let newMarkDep = Builder(after: input, location: input.location, context)
      .createMarkDependence(value: input, base: arg, kind: .Unresolved)
    let uses = input.uses.lazy.filter {
      let inst = $0.instruction
      return inst != newMarkDep
    }
    uses.replaceAll(with: newMarkDep, context)
    return newMarkDep
  }
}

private extension MarkDependenceAddrInst {
  /// Rewrite the mark_dependence_addr base operand, setting it to a function argument.
  ///
  /// This is called when the dependent value is returned by the function and the dependence base is in the caller.
  func redirectFunctionReturnAddress(to arg: FunctionArgument, _ context: FunctionPassContext)
    -> MarkDependenceAddrInst {
    return Builder(after: self, location: self.location, context)
        .createMarkDependenceAddr(value: self.address, base: arg, kind: .Unresolved)
  }
}

/// A scope extension is a set of nested scopes and their owners. The owner is a value that represents ownership of
/// the outermost scopes, which cannot be extended; it limits how far the nested scopes can be extended.
private struct ScopeExtension {
  let context: FunctionPassContext
  let localReachabilityCache: LocalVariableReachabilityCache

  /// The ownership lifetime of the dependence base, which cannot be extended.
  var owners = SingleInlineArray<Value>()

  // Initialized after walking dependent uses. True if the scope can be extended into the caller.
  var dependsOnCaller: Bool?

  // Scopes listed in RPO over an upward walk. The outermost scope is first.
  var scopes = SingleInlineArray<ExtendableScope>()

  var innermostScope: ExtendableScope { get { scopes.last! } }

  var visitedValues: ValueSet?

  init(_ localReachabilityCache: LocalVariableReachabilityCache, _ context: FunctionPassContext) {
    self.localReachabilityCache = localReachabilityCache
    self.context = context
  }
}

/// Transitively extend nested scopes that enclose the dependence base.
///
/// If the parent function returns the dependent value, then this returns the function arguments that represent the
/// caller's scope.
///
/// Note that we cannot simply rewrite the `mark_dependence` to depend on an outer access scope. Although that would be
/// valid for a 'read' access, it would not accomplish anything useful. An inner 'read' can always be extended up to
/// the end of its outer 'read'. A nested 'read' access can never interfere with another access in the same outer
/// 'read', because it is impossible to nest a 'modify' access within a 'read'. For 'modify' accesses, however, the
/// inner scope must be extended for correctness. A 'modify' access can interfere with other 'modify' access in the same
/// scope. We rely on exclusivity diagnostics to report these interferences. For example:
///
///     sil @foo : $(@inout C) -> () {
///       bb0(%0 : $*C):
///         %a1 = begin_access [modify] %0
///         %d = apply @getDependent(%a1)
///         mark_dependence [unresolved] %d on %a1
///         end_access %a1
///         %a2 = begin_access [modify] %0
///         ...
///         end_access %a2
///         apply @useDependent(%d) // exclusivity violation
///         return
///     }
///
// The above call to `@useDependent` is an exclusivity violation because it uses a value that depends on a 'modify'
// access. This scope fixup pass must extend '%a1' to cover the `@useDependent` but must not extend the base of the
// `mark_dependence` to the outer access `%0`. This ensures that exclusivity diagnostics correctly reports the
// violation, and that subsequent optimizations do not shrink the inner access `%a1`.
extension ScopeExtension {
  mutating func extendScopes(dependence: LifetimeDependence) -> Bool {
    log("Scope fixup for lifetime dependent instructions:\n\(dependence)")

    gatherExtensions(dependence: dependence)

    // computeDependentUseRange initializes scopeExtension.dependsOnCaller.
    guard var useRange = computeDependentUseRange(of: dependence) else {
      return false
    }
    // tryExtendScopes deinitializes 'useRange'
    var scopesToExtend = SingleInlineArray<ExtendableScope>()
    guard canExtendScopes(over: &useRange, scopesToExtend: &scopesToExtend) else {
      useRange.deinitialize()
      return false
    }
    // extend(over:) must receive the original unmodified `useRange`, without intermediate scope ending instructions.
    // This deinitializes `useRange` before erasing instructions.
    extend(scopesToExtend: scopesToExtend, over: &useRange, context)
    return true
  }
}

// TODO: add parent and child indices to model a DAG of scopes. This will allow sibling scopes that do not follow a
// stack discipline among them but still share the same parent and child scopes. This can occur with dependencies on
// multiple call operands. Until then, scope extension may bail out unnecessarily while trying to extend over a sibling
// scope.
private struct ExtendableScope {
  enum Introducer {
    case scoped(ScopedInstruction)
    case owned(Value)
  }

  let scope: LifetimeDependence.Scope
  let introducer: Introducer

  var firstInstruction: Instruction {
    switch introducer {
    case let .scoped(scopedInst):
      return scopedInst
    case let .owned(value):
      if let definingInst = value.definingInstructionOrTerminator {
        return definingInst
      }
      return value.parentBlock.instructions.first!
    }
  }
  var endInstructions: LazyMapSequence<LazyFilterSequence<UseList>, Instruction> {
    switch introducer {
    case let .scoped(scopedInst):
      return scopedInst.scopeEndingOperands.users
    case let .owned(value):
      return value.uses.endingLifetime.users
    }
  }

  // Allow scope extension as long as `beginInst` is scoped instruction and does not define a variable scope.
  init?(_ scope: LifetimeDependence.Scope, beginInst: Instruction?) {
    self.scope = scope
    guard let beginInst = beginInst, VariableScopeInstruction(beginInst) == nil else {
      return nil
    }
    guard let scopedInst = beginInst as? ScopedInstruction else {
      return nil
    }
    self.introducer = .scoped(scopedInst)
  }

  // Allow extension of owned temporaries that
  // (a) are Escapable
  // (b) do not define a variable scope
  // (c) are only consumed by destroy_value
  init?(_ scope: LifetimeDependence.Scope, owner: Value) {
    self.scope = scope
    // TODO: allow extension of lifetime dependent values by implementing a ScopeExtensionWalker that extends
    // LifetimeDependenceUseDefWalker.
    guard owner.type.isEscapable(in: owner.parentFunction),
          VariableScopeInstruction(owner.definingInstruction) == nil,
          owner.uses.endingLifetime.allSatisfy({ $0.instruction is DestroyValueInst }) else {
      return nil
    }
    self.introducer = .owned(owner)
  }
}

// Gather extendable scopes.
extension ScopeExtension {
  mutating func gatherExtensions(dependence: LifetimeDependence) {
    visitedValues = ValueSet(context)
    defer {
      visitedValues!.deinitialize()
      visitedValues = nil
    }
    gatherExtensions(scope: dependence.scope)
  }

  mutating func gatherExtensions(valueOrAddress: Value) {
    if visitedValues!.insert(valueOrAddress) {
      gatherExtensions(scope: LifetimeDependence.Scope(base: valueOrAddress, context))
    }
  }

  mutating func nonExtendable(_ scope: LifetimeDependence.Scope) {
    owners.push(scope.parentValue)
  }

  // If `scope` is extendable, find its owner or outer scopes first, then push for extension.
  mutating func gatherExtensions(scope: LifetimeDependence.Scope) {
    switch scope {
    case let .access(beginAccess):
      gatherAccessExtensions(beginAccess: beginAccess)
      return

    case let .borrowed(beginBorrow):
      if let beginInst = beginBorrow.value.definingInstruction {
        if let extScope = ExtendableScope(scope, beginInst: beginInst) {
          gatherExtensions(valueOrAddress: beginBorrow.baseOperand!.value)
          scopes.push(extScope)
          return
        }
      }

    case let .yield(yieldedValue):
      let beginApply = yieldedValue.definingInstruction as! BeginApplyInst
      gatherYieldExtension(beginApply)
      scopes.push(ExtendableScope(scope, beginInst: beginApply)!)
      return

    case let .initialized(initializer):
      switch initializer {
      case let .store(initializingStore: store, initialAddress: _):
        if let sb = store as? StoreBorrowInst {
          // Follow the source for nested scopes.
          gatherExtensions(valueOrAddress: sb.source)
          scopes.push(ExtendableScope(scope, beginInst: sb)!)
          return
        }
      case .argument, .yield:
        // TODO: extend indirectly yielded scopes.
        break
      }
    case let .owned(value):
      if let extScope = ExtendableScope(scope, owner: value) {
        scopes.push(extScope)
        return
      }

    case let .local(varInst):
      switch varInst {
      case let .beginBorrow(beginBorrow):
        if let extScope = ExtendableScope(scope, beginInst: beginBorrow) {
          gatherExtensions(valueOrAddress: beginBorrow.operand.value)
          scopes.push(extScope)
          return
        }

      case let .moveValue(moveValue):
        if let extScope = ExtendableScope(scope, owner: moveValue) {
          scopes.push(extScope)
          return
        }
      }
    default:
      break
    }
    nonExtendable(scope)
  }

  /// Unlike LifetimeDependenceInsertion, this does not stop at an argument's "variable introducer" and does not stop at
  /// an addressable parameter. The purpose here is to extend any enclosing OSSA scopes as far as possible to achieve
  /// the longest possible owner lifetime, rather than to find the source-level lvalue for a call argument.
  mutating func gatherYieldExtension(_ beginApply: BeginApplyInst) {
    // Create a separate ScopeExtension for each operand that the yielded value depends on.
    for operand in beginApply.parameterOperands {
      gatherExtensions(valueOrAddress: operand.value)
    }
  }

  mutating func gatherAccessExtensions(beginAccess: BeginAccessInst) {
    let accessBaseAndScopes = beginAccess.accessBaseWithScopes
    if let baseAddress = accessBaseAndScopes.base.address {
      gatherExtensions(valueOrAddress: baseAddress)
    }
    for nestedScope in accessBaseAndScopes.scopes.reversed() {
      switch nestedScope {
      case let .access(nestedBeginAccess):
        scopes.push(ExtendableScope(.access(nestedBeginAccess), beginInst: nestedBeginAccess)!)
      case .dependence, .base:
        // ignore recursive mark_dependence base for the purpose of extending scopes. This pass will extend the base
        // of that mark_dependence (if it is unresolved) later as a separate LifetimeDependence.Scope.
        break
      }
    }
  }
}

extension ScopeExtension {
  /// Check if the dependent value depends only on function arguments and can therefore be returned to caller. If so,
  /// return the list of arguments that it depends on. If this returns an empty list, then the dependent value cannot be
  /// returned.
  ///
  /// The conditions for returning a dependent value are:
  /// - The dependent value is returned from this function.
  /// - All nested scopes are access scopes that are redundant with the caller's exclusive access scope.
  /// - All scope owners are function arguments.
  func findArgumentDependencies() -> SingleInlineArray<FunctionArgument> {
    let noCallerScope = SingleInlineArray<FunctionArgument>()
    // Check that the dependent value is returned by this function.
    if !dependsOnCaller! {
      return noCallerScope
    }
    // Check that all nested scopes that it depends on can be covered by exclusive access in the caller.
    for extScope in scopes {
      switch extScope.scope {
      case .access:
        break
      default:
        return noCallerScope
      }
    }
    // All owners must be arguments with exclusive access to depend on the caller's scope (inout_aliasable arguments do
    // not have exclusivity).
    var compatibleArgs = SingleInlineArray<FunctionArgument>()
    for owner in owners {
      guard let arg = owner as? FunctionArgument else {
        return noCallerScope
      }
      guard arg.convention.isIndirectIn || arg.convention.isInout else {
        return noCallerScope
      }
      compatibleArgs.push(arg)
    }
    return compatibleArgs
  }
}

/// Compute the range of the a scope owner. Nested scopes must stay within this range.
///
/// Abstracts over lifetimes for both addresses and values.
extension ScopeExtension {
  enum Range {
    case fullRange
    case addressRange(AddressOwnershipLiveRange)
    case valueRange(InstructionRange)

    func coversUse(_ inst: Instruction) -> Bool {
      switch self {
      case .fullRange:
        return true
      case let .addressRange(range):
        return range.coversUse(inst)
      case let .valueRange(range):
        return range.inclusiveRangeContains(inst)
      }
    }

    mutating func deinitialize() {
      switch self {
      case .fullRange:
        break
      case var .addressRange(range):
        return range.deinitialize()
      case var .valueRange(range):
        return range.deinitialize()
      }
    }

    var description: String {
      switch self {
      case .fullRange:
        return "full range"
      case let .addressRange(range):
        return range.description
      case let .valueRange(range):
        return range.description
      }
    }
  }

  /// Return nil if the scope's owner is valid across the function, such as a guaranteed function argument.
  func computeSingleOwnerRange(owner: Value) -> Range? {
    if owner.type.isAddress {
      // Get the range of the accessBase lifetime at the point where the outermost extendable scope begins.
      if let range = AddressOwnershipLiveRange.compute(for: owner, at: scopes.first!.firstInstruction,
                                                       localReachabilityCache, context) {
        return .addressRange(range)
      }
      return nil
    }
    switch owner.ownership {
    case .owned:
      return .valueRange(computeLinearLiveness(for: owner, context))
    case .guaranteed:
      if let bbv = BeginBorrowValue(owner) {
        if case .functionArgument = bbv {
          return .fullRange
        }
        return .valueRange(computeLinearLiveness(for: bbv.value, context))
      }
      return nil
    case .none:
      return .fullRange
    case .unowned:
      return nil
    }
  }

  /// Return an InstructionRange covering all the dependent uses of 'dependence'.
  ///
  /// Initialize dependsOnCaller.
  mutating func computeDependentUseRange(of dependence: LifetimeDependence) -> InstructionRange? {
    if scopes.isEmpty {
      return nil
    }
    let function = dependence.function
    var inRangeUses = [Instruction]()
    do {
      // The innermost scope that must be extended must dominate all uses.
      var walker = LifetimeDependentUseWalker(function, localReachabilityCache, context) {
        inRangeUses.append($0.instruction)
        return .continueWalk
      }
      defer {walker.deinitialize()}
      _ = walker.walkDown(dependence: dependence)
      dependsOnCaller = walker.dependsOnCaller
    }
    for owner in owners {
      guard var ownershipRange = computeSingleOwnerRange(owner: owner) else {
        return nil
      }
      defer { ownershipRange.deinitialize() }

      inRangeUses = inRangeUses.filter { ownershipRange.coversUse($0) }
    }
    var useRange = InstructionRange(begin: innermostScope.firstInstruction, context)
    useRange.insert(contentsOf: inRangeUses)

    log("Scope fixup for dependent uses:\n\(useRange)")

    // Lifetime dependent uses may not be dominated by `innermostScope`. The dependent value may be used by a phi or
    // stored into a memory location. The access may be conditional relative to such uses. If any use was not dominated,
    // then `useRange` will include the function entry. There is no way to directly check if `useRange` is
    // valid. `useRange.blockRange.isValid` is not a strong enough check because it will always succeed when
    // `useRange.begin == entryBlock` even if a use is above `useRange.begin`. Instead check if `useRange` contains the
    // first instruction, and the first instruction does not itself start `innermostScope`.
    let firstInst = function.entryBlock.instructions.first!
    if firstInst != useRange.begin, useRange.contains(firstInst) {
      useRange.deinitialize()
      return nil
    }
    return useRange
  }
}

extension ScopeExtension {
  /// Return true if all nested scopes were extended across `useRange`. `useRange` has already been pruned to be a
  /// subset of the ranges of the owners.
  ///
  /// Note: the scopes may not be strictly nested. Two adjacent scopes in the nested scopes array may have begin at the
  /// same nesting level. Their begin instructions may occur in any order relative to the nested scopes array, but we
  /// order the end instructions according to the arbitrary order that the scopes were inserted in the array. This is
  /// conservative and could extend some scopes longer than strictly necessary. To improve this, `scopes` must be
  /// represnted as a DAG by recording parent and child indices.
  func canExtendScopes(over useRange: inout InstructionRange,
                       scopesToExtend: inout SingleInlineArray<ExtendableScope>) -> Bool {
    var extendedUseRange = InstructionRange(begin: useRange.begin!, ends: useRange.ends, context)

    // Insert the first instruction of the exit blocks to mimic `useRange`. There is no way to directly copy
    // `useRange`. Inserting the exit block instructions is inaccurate, but for the purpose of canExtend() below, it has
    // the same effect as a copy of `useRange`.
    extendedUseRange.insert(contentsOf: useRange.exits)
    defer { extendedUseRange.deinitialize() }

    // Append each scope that needs extension to scopesToExtend from the inner to the outer scope.
    for extScope in scopes.reversed() {
      // An outer scope might not originally cover one of its inner scopes. Therefore, extend 'extendedUseRange' to to
      // cover this scope's end instructions. The extended scope must at least cover the original scopes because the
      // original scopes may protect other operations.
      var mustExtend = false
      for scopeEndInst in extScope.endInstructions {
        switch extendedUseRange.overlaps(pathBegin: extScope.firstInstruction, pathEnd: scopeEndInst, context) {
        case .containsPath, .containsEnd, .disjoint:
          // containsPath can occur when the extendable scope has the same begin as the use range.
          // disjoint is unexpected, but if it occurs then `extScope` must be before the useRange.
          mustExtend = true
          break
        case .containsBegin, .overlappedByPath:
          // containsBegin can occur when the extendable scope has the same begin as the use range.
          extendedUseRange.insert(scopeEndInst)
          break
        }
      }
      if !mustExtend {
        continue
      }
      scopesToExtend.push(extScope)
      if !extScope.canExtend(over: &extendedUseRange, context) {
        // Scope ending instructions cannot be inserted at the 'range' boundary. Ignore all nested scopes.
        //
        // Note: We could still extend previously prepared inner scopes up to this scope. To do that, we would
        // need to repeat the steps above: treat 'extScope' as the new owner, and recompute `useRange`. But this
        // scenario could only happen with nested coroutine, where the range boundary is reachable from the outer
        // coroutine's EndApply and AbortApply--it is vanishingly unlikely if not impossible.
        return false
      }
    }
    return true
  }
  
  // Extend the scopes that actually required extension.
  //
  // Consumes 'useRange'
  private func extend(scopesToExtend: SingleInlineArray<ExtendableScope>,
                      over useRange: inout InstructionRange,
                      _ context: some MutatingContext) {
    var deadInsts = [Instruction]()
    for extScope in scopesToExtend {
      // Extend 'useRange' to to cover this scope's end instructions. 'useRange' cannot be extended until the
      // inner scopes have been extended.
      useRange.insert(contentsOf: extScope.endInstructions)

      // Note, we could Skip extension here if we have a fully overlapping scope. But that requires computing the scope
      // of [beginInst : beginInst.endInstructions) because an outer scope may be disjoint from the inner scope but
      // still requires extension:
      //     %access = begin_access [read] %owner       // <=== outer scoope
      //     %temp = load [copy] %access
      //     end_access %access
      //     (%dependent, %token) = begin_apply (%temp) // <=== inner scope
      //     end_apply %token
      //
      deadInsts += extScope.extend(over: &useRange, context)

      // Continue checking enclosing scopes for extension even if 'mustExtend' is false. Multiple ScopeExtensions may
      // share the same inner scope, so this inner scope may already have been extended while handling a previous
      // ScopeExtension. Nonetheless, some enclosing scopes may still require extension. This only happens when a
      // yielded value depends on multiple begin_apply operands.
    }
    // 'useRange' is invalid as soon as instructions are deleted.
    useRange.deinitialize()

    // Delete original end instructions.
    for deadInst in deadInsts {
      context.erase(instruction: deadInst)
    }
  }
}

// Extend a dependence scope to cover the dependent uses.
extension ExtendableScope {
  /// Return true if new scope-ending instruction can be inserted at the range boundary.
  func canExtend(over range: inout InstructionRange, _ context: some Context) -> Bool {
    switch self.scope {
    case let .yield(yieldedValue):
      return canExtend(beginApply: yieldedValue.definingInstruction as! BeginApplyInst, over: &range, context)
    case let .initialized(initializer):
      switch initializer {
      case .argument, .yield:
        // A yield is already considered nested within the coroutine.
        break
      case let .store(initializingStore, _):
        if let sb = initializingStore as? StoreBorrowInst {
          return canExtend(storeBorrow: sb, over: &range)
        }
      }
      return true
    default:
      // non-yield scopes can always be ended at any point.
      return true
    }
  }

  func canExtend(beginApply: BeginApplyInst, over range: inout InstructionRange, _ context: some Context) -> Bool {
    let canEndAtBoundary = { (boundaryInst: Instruction) in
      switch beginApply.endReaches(block: boundaryInst.parentBlock, context) {
      case .abortReaches, .endReaches, .deadEndReaches:
        return true
      case .none:
        return false
      }
    }
    for end in range.ends {
      if (!canEndAtBoundary(end)) {
        return false
      }
    }
    for exit in range.exits {
      if (!canEndAtBoundary(exit)) {
        return false
      }
    }
    return true
  }

  /// A store borrow is considered to be nested within the scope of its stored values. It is, however, also
  /// restricted to the range of its allocation.
  ///
  /// TODO: consider rewriting the dealloc_stack instructions if we ever find that SILGen emits them sooner that
  /// we need for lifetime dependencies.
  func canExtend(storeBorrow: StoreBorrowInst, over range: inout InstructionRange) -> Bool {
    // store_borrow can be extended if all deallocations occur after the use range.
    return storeBorrow.allocStack.deallocations.allSatisfy({ !range.contains($0) })
  }

  /// Extend this scope over the 'range' boundary. Return the old scope ending instructions to be deleted.
  func extend(over range: inout InstructionRange, _ context: some MutatingContext) -> [Instruction] {
    // Collect the original end instructions and extend the range to to cover them. The resulting access scope
    // must cover the original scope because it may protect other memory operations.
    let endsToErase = self.endInstructions
    var unusedEnds = InstructionSet(context)
    for end in endsToErase {
      assert(range.inclusiveRangeContains(end))
      unusedEnds.insert(end)
    }
    defer { unusedEnds.deinitialize() }
    for end in range.ends {
      let location = end.location.asAutoGenerated
      switch end {
      case is BranchInst:
        assert(end.parentBlock.singleSuccessor!.terminator is ReturnInst,
               "a phi only ends a use range if it is a returned value")
        fallthrough
      case is ReturnInst:
        // End this inner scope just before the return. The mark_dependence base operand will be redirected to a
        // function argument.
        let builder = Builder(before: end, location: location, context)
        // Insert newEnd so that this scope will be nested in any outer scopes.
        range.insert(createEndInstruction(builder, context))
        continue
      default:
        break
      }
      if unusedEnds.contains(end) {
        unusedEnds.erase(end)
        assert(!unusedEnds.contains(end))
        continue
      }
      Builder.insert(after: end, location: location, context) {
        range.insert(createEndInstruction($0, context))
      }
    }
    for exitInst in range.exits {
      let location = exitInst.location.asAutoGenerated
      let builder = Builder(before: exitInst, location: location, context)
      range.insert(createEndInstruction(builder, context))
    }
    return endsToErase.filter { unusedEnds.contains($0) }
  }

  /// Create a scope-ending instruction at 'builder's insertion point.
  func createEndInstruction(_ builder: Builder, _ context: some Context) -> Instruction {
    switch self.scope {
    case let .access(beginAccess):
      return builder.createEndAccess(beginAccess: beginAccess)
    case let .borrowed(beginBorrow):
      return builder.createEndBorrow(of: beginBorrow.value)
    case let .yield(yieldedValue):
      let beginApply = yieldedValue.definingInstruction as! BeginApplyInst
      // createEnd() returns non-nil because beginApply.endReaches() was checked by canExtend()
      return beginApply.createEnd(builder, context)!
    case let .initialized(initializer):
      switch initializer {
      case let .store(initializingStore: store, initialAddress: _):
        if let sb = store as? StoreBorrowInst {
          // FIXME: we may need to rewrite the dealloc_stack.
          return builder.createEndBorrow(of: sb)
        }
        break
      case .argument, .yield:
        // TODO: extend indirectly yielded scopes.
        break
      }
    case let .owned(value):
      return builder.createDestroyValue(operand: value)
    case let .local(varInst):
      switch varInst {
      case let .beginBorrow(beginBorrow):
        // FIXME: we may need to rewrite the dealloc_stack.
        return builder.createEndBorrow(of: beginBorrow)
      case let .moveValue(moveValue):
        return builder.createDestroyValue(operand: moveValue)
      }
    default:
      break
    }
    fatalError("Unsupported scoped extension: \(self)")
  }
}

private extension BeginApplyInst {
  /// Create either an end_apply or abort_apply at the builder's insertion point.
  /// Return nil if it isn't possible.
  func createEnd(_ builder: Builder, _ context: some Context) -> Instruction? {
    guard let insertionBlock = builder.insertionBlock else {
      return nil
    }
    switch endReaches(block: insertionBlock, context) {
    case .none:
      return nil
    case .endReaches:
      return builder.createEndApply(beginApply: self)
    case .abortReaches:
      return builder.createAbortApply(beginApply: self)
    case .deadEndReaches:
      return builder.createEndBorrow(of: self.token)
    }
  }

  enum EndReaches {
    case endReaches
    case abortReaches
    case deadEndReaches
  }

  /// Return the single kind of coroutine termination that reaches 'reachableBlock' or nil.
  func endReaches(block reachableBlock: BasicBlock, _ context: some Context) -> EndReaches? {
    // TODO: use InlineArray<3> once bootstrapping is fixed.
    var endingBlockMap: [(EndReaches, BasicBlockSet)] = [
      (.endReaches, BasicBlockSet(context)),
      (.abortReaches, BasicBlockSet(context)),
      (.deadEndReaches, BasicBlockSet(context))
    ]
    defer {
      for index in endingBlockMap.indices {
        endingBlockMap[index].1.deinitialize()
      }
    }
    for endInst in endInstructions {
      let endKind: EndReaches
      switch endInst {
      case let endApply as EndApplyInst:
        // Cannot extend the scope of a coroutine when the resume produces a value.
        if !endApply.type.isEmpty(in: parentFunction) {
          return nil
        }
        endKind = .endReaches
      case is AbortApplyInst:
        endKind = .abortReaches
      case is EndBorrowInst:
        endKind = .deadEndReaches
      default:
        fatalError("invalid begin_apply ending instruction")
      }
      let endingBlocksIndex = endingBlockMap.firstIndex(where: { $0.0 == endKind })!
      endingBlockMap[endingBlocksIndex].1.insert(endInst.parentBlock)
    }
    var endReaches: EndReaches?
    var backwardWalk = BasicBlockWorklist(context)
    defer { backwardWalk.deinitialize() }

    let backwardVisit = { (block: BasicBlock) -> WalkResult in
      for (endKind, endingBlocks) in endingBlockMap {
        if endingBlocks.contains(block) {
          if let endReaches = endReaches, endReaches != endKind {
            return .abortWalk
          }
          endReaches = endKind
          return .continueWalk
        }
      }
      if block == self.parentBlock {
        // the insertion point is not dominated by the coroutine
        return .abortWalk
      }
      backwardWalk.pushIfNotVisited(contentsOf: block.predecessors)
      return .continueWalk
    }

    if backwardVisit(reachableBlock) == .abortWalk {
      return nil
    }
    while let block = backwardWalk.pop() {
      if backwardVisit(block) == .abortWalk {
        return nil
      }
    }
    return endReaches
  }
}

/// Visit all dependent uses.
///
/// Set 'dependsOnCaller' if a use escapes the function.
private struct LifetimeDependentUseWalker : LifetimeDependenceDefUseWalker {
  let function: Function
  let context: Context
  let visitor: (Operand) -> WalkResult
  let localReachabilityCache: LocalVariableReachabilityCache
  var visitedValues: ValueSet

  /// Set to true if the dependence is returned from the current function.
  var dependsOnCaller = false

  init(_ function: Function, _ localReachabilityCache: LocalVariableReachabilityCache, _ context: Context,
       visitor: @escaping (Operand) -> WalkResult) {
    self.function = function
    self.context = context
    self.visitor = visitor
    self.localReachabilityCache = localReachabilityCache
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
    if let operand {
      return visitor(operand)
    }
    return .continueWalk
  }

  mutating func leafUse(of operand: Operand) -> WalkResult {
    return visitor(operand)
  }

  mutating func escapingDependence(on operand: Operand) -> WalkResult {
    log(">>> Escaping dependence: \(operand)")
    _ = visitor(operand)
    // Make a best-effort attempt to extend the access scope regardless of escapes. It is possible that some mandatory
    // pass between scope fixup and diagnostics will make it possible for the LifetimeDependenceDefUseWalker to analyze
    // this use.
    return .continueWalk
  }

  mutating func inoutDependence(argument: FunctionArgument, on operand: Operand) -> WalkResult {
    dependsOnCaller = true
    return visitor(operand)
  }

  mutating func returnedDependence(result operand: Operand) -> WalkResult {
    dependsOnCaller = true
    return visitor(operand)
  }

  mutating func returnedDependence(address: FunctionArgument,
                                   on operand: Operand) -> WalkResult {
    dependsOnCaller = true
    return visitor(operand)
  }

  mutating func yieldedDependence(result: Operand) -> WalkResult {
    return visitor(result)
  }

  mutating func storeToYieldDependence(address: Value, of operand: Operand) -> WalkResult {
    return .continueWalk
  }
}

