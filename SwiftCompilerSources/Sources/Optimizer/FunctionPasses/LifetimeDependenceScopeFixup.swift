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
#if os(Windows)
  if !context.options.hasFeature(.NonescapableTypes) {
    return
  }
#endif
  log(prefix: false, "\n--- Scope fixup for lifetime dependence in \(function.name)")

  let localReachabilityCache = LocalVariableReachabilityCache()

  for instruction in function.instructions {
    guard let markDep = instruction as? MarkDependenceInst else {
      continue
    }
    guard let innerLifetimeDep = LifetimeDependence(markDep, context) else {
      continue
    }
    // Redirect the dependence base to ignore irrelevant borrow scopes.
    let newLifetimeDep = markDep.rewriteSkippingBorrow(scope: innerLifetimeDep.scope, context)

    // Recursively sink enclosing end_access, end_borrow, or end_apply.
    let args = extendScopes(dependence: newLifetimeDep, localReachabilityCache, context)

    // Redirect the dependence base to the function arguments. This may create additional mark_dependence instructions.
    markDep.redirectFunctionReturn(to: args, context)
  }
}

private extension MarkDependenceInst {
  /// Rewrite the mark_dependence base operand to ignore inner borrow scopes (begin_borrow, load_borrow).
  ///
  /// Note: this could be done as a general simplification, e.g. after inlining. But currently this is only relevant for
  /// diagnostics.
  func rewriteSkippingBorrow(scope: LifetimeDependence.Scope, _ context: FunctionPassContext) -> LifetimeDependence {
    guard let newScope = scope.ignoreBorrowScope(context) else {
      return LifetimeDependence(scope: scope, dependentValue: self)
    }
    let newBase = newScope.parentValue
    if newBase != self.baseOperand.value {
      self.baseOperand.set(to: newBase, context)
    }
    return LifetimeDependence(scope: newScope, dependentValue: self)
  }

  /// Rewrite the mark_dependence base operand, setting it to a function argument.
  ///
  /// To handle more than one function argument, new mark_dependence instructions will be chained.
  /// This is called when the dependent value is returned by the function and the dependence base is in the caller.
  func redirectFunctionReturn(to args: SingleInlineArray<FunctionArgument>, _ context: FunctionPassContext) {
    var updatedMarkDep: MarkDependenceInst?
    for arg in args {
      guard let currentMarkDep = updatedMarkDep else {
        self.baseOperand.set(to: arg, context)
        updatedMarkDep = self
        continue
      }
      let newMarkDep = Builder(after: currentMarkDep, location: currentMarkDep.location, context)
        .createMarkDependence(value: currentMarkDep, base: arg, kind: .Unresolved)
      let uses = currentMarkDep.uses.lazy.filter {
        let inst = $0.instruction
        return inst != newMarkDep
      }
      uses.replaceAll(with: newMarkDep, context)
      updatedMarkDep = newMarkDep
    }
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
private func extendScopes(dependence: LifetimeDependence,
                          _ localReachabilityCache: LocalVariableReachabilityCache,
                          _ context: FunctionPassContext) -> SingleInlineArray<FunctionArgument> {
  log("Scope fixup for lifetime dependent instructions: \(dependence)")

  // Each scope extension is a set of nested scopes and an owner. The owner is a value that represents ownerhip of the
  // outermost scope, which cannot be extended; it limits how far the nested scopes can be extended.
  guard let scopeExtensions = dependence.scope.gatherExtensions(context) else {
    return SingleInlineArray()
  }
  var dependsOnArgs = SingleInlineArray<FunctionArgument>()
  for scopeExtension in scopeExtensions {
    var scopeExtension = scopeExtension
    guard var useRange = computeDependentUseRange(of: dependence.dependentValue, within: &scopeExtension,
                                                  localReachabilityCache, context) else {
      continue
    }
    defer { useRange.deinitialize() }

    scopeExtension.extend(over: &useRange, context)

    if scopeExtension.dependsOnCaller, let arg = scopeExtension.dependsOnArg {
      dependsOnArgs.push(arg)
    }
  }
  return dependsOnArgs
}

/// All scopes nested within a single dependence base that require extension.
private struct ScopeExtension {
  /// The ownership lifetime of the dependence base, which cannot be extended.
  let owner: Value

  /// The scopes nested under 'value' that may be extended, in inside-out order. There is always at
  /// least one element, otherwise there is nothing to consider extending.
  let nestedScopes: SingleInlineArray<LifetimeDependence.Scope>

  var innerScope: LifetimeDependence.Scope { get { nestedScopes.first! } }

  /// `dependsOnArg` is set to the function argument that represents the caller's dependency source.
  ///
  /// Note: for non-address owners, this is equivalent to: owner as? FunctionArg?
  var dependsOnArg: FunctionArgument?

  /// `dependsOnCaller` is true if the dependent value is returned by the function.
  /// Initialized during computeDependentUseRange().
  var dependsOnCaller = false
}

private extension LifetimeDependence.Scope {
  /// The instruction that introduces an extendable scope. This returns a non-nil scope introducer for
  /// Extendable.nestedScopes.
  var extendableBegin: Instruction? {
    switch self {
    case let .access(beginAccess):
      return beginAccess
    case let .borrowed(beginBorrow):
      return beginBorrow.value.definingInstruction!
    case let .yield(yieldedValue):
      return yieldedValue.definingInstruction!
    default:
      return nil
    }
  }

  /// Find any nested scopes that may be extended.
  ///
  /// Return 'nil' if 'self' is not extendable.
  ///
  /// TODO: handle trivial variable scopes
  func gatherExtensions(innerScopes: SingleInlineArray<LifetimeDependence.Scope>? = nil, _ context: FunctionPassContext)
    -> SingleInlineArray<ScopeExtension>? {

    var innerScopes = innerScopes ?? SingleInlineArray()
    switch self {
    case let .access(beginAccess):
      let accessExtension = gatherAccessExtension(beginAccess: beginAccess, innerScopes: &innerScopes)
      return SingleInlineArray(element: accessExtension)
    case let .borrowed(beginBorrow):
      let borrowedValue = beginBorrow.baseOperand!.value
      let enclosingScope = LifetimeDependence.Scope(base: borrowedValue, context)
      innerScopes.push(self)
      var innerBorrowScopes = innerScopes
      innerBorrowScopes.push(enclosingScope)
      if let extensions = enclosingScope.gatherExtensions(innerScopes: innerBorrowScopes, context) {
        return extensions
      }
      // This is the outermost scope to be extended because gatherExtensions did not find an enclosing scope.
      return SingleInlineArray(element: getOuterExtension(owner: enclosingScope.parentValue, nestedScopes: innerScopes,
                                                          context))
    case let .yield(yieldedValue):
      innerScopes.push(self)
      var extensions = SingleInlineArray<ScopeExtension>()
      let applySite = yieldedValue.definingInstruction as! BeginApplyInst
      for operand in applySite.parameterOperands {
        guard let dep = applySite.resultDependence(on: operand), dep == .scope else {
          continue
        }
        // Pass a copy of innerScopes without modifying this one.
        extensions.append(contentsOf: gatherOperandExtension(on: operand, innerScopes: innerScopes, context))
      }
      return extensions
    default:
      return nil
    }
  }

  /// Unlike LifetimeDependenceInsertion this does not use gatherVariableIntroducers. The purpose here is to extend
  /// any enclosing OSSA scopes as far as possible to achieve the longest possible owner lifetime, rather than to
  /// find the "dependence root" for a call argument.
  func gatherOperandExtension(on operand: Operand, innerScopes: SingleInlineArray<LifetimeDependence.Scope>,
                              _ context: FunctionPassContext) -> SingleInlineArray<ScopeExtension> {
    let enclosingScope = LifetimeDependence.Scope(base: operand.value, context)
    if let extensions = enclosingScope.gatherExtensions(innerScopes: innerScopes, context) {
      return extensions
    }
    // This is the outermost scope to be extended because gatherExtensions did not find an enclosing scope.
    return SingleInlineArray(element: getOuterExtension(owner: operand.value, nestedScopes: innerScopes, context))
  }

  func getOuterExtension(owner: Value, nestedScopes: SingleInlineArray<LifetimeDependence.Scope>,
                         _ context: FunctionPassContext) -> ScopeExtension {
    let dependsOnArg = owner as? FunctionArgument
    return ScopeExtension(owner: owner, nestedScopes: nestedScopes, dependsOnArg: dependsOnArg)
  }

  // Find the nested access scopes that may be extended as if they are the same access. This includes any combination of
  // read/modify accesses, regardless of whether they may cause an exclusivity violation. The outer accesses will only
  // be extended as far as required such that the innermost access coveres all dependent uses.
  // Set ScopeExtension.dependsOnArg if the nested accesses are all compatible with the argument's convention. Then, if
  // all nested accesses were extended to the return statement, it is valid to logically combine them into a single
  // access for the purpose of diagnostinc lifetime dependence.
  func gatherAccessExtension(beginAccess: BeginAccessInst,
                             innerScopes: inout SingleInlineArray<LifetimeDependence.Scope>) -> ScopeExtension {
    // Finding the access base also finds all intermediate nested scopes; there is no need to recursively call
    // gatherExtensions().
    let accessBaseAndScopes = beginAccess.accessBaseWithScopes
    var isCompatibleAccess = true
    for nestedScope in accessBaseAndScopes.scopes {
      switch nestedScope {
      case let .access(nestedBeginAccess):
        innerScopes.push(.access(nestedBeginAccess))
        if nestedBeginAccess.accessKind != beginAccess.accessKind {
          isCompatibleAccess = false
        }
      case .dependence, .base:
        // ignore recursive mark_dependence base for the purpose of extending scopes. This pass will extend the base
        // of that mark_dependence (if it is unresolved) later as a separate LifetimeDependence.Scope.
        break
      }
    }
    guard case let .access(outerBeginAccess) = innerScopes.last else {
      // beginAccess is included in accessBaseWithScopes; so at least one access was added to innerScopes.
      fatalError("missing outer access")
    }
    if case let .argument(arg) = accessBaseAndScopes.base {
      if isCompatibleAccess && beginAccess.accessKind.isCompatible(with: arg.convention) {
        return ScopeExtension(owner: outerBeginAccess.address, nestedScopes: innerScopes, dependsOnArg: arg)
      }
    }
    return ScopeExtension(owner: outerBeginAccess, nestedScopes: innerScopes, dependsOnArg: nil)
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
  }

  /// Return nil if the scope's owner is valid across the function, such as a guaranteed function argument.
  func computeRange(_ localReachabilityCache: LocalVariableReachabilityCache, _ context: FunctionPassContext) -> Range?
  {
    if owner.type.isAddress {
      // Get the range of the accessBase lifetime at the point where the outermost extendable scope begins.
      if let range = AddressOwnershipLiveRange.compute(for: owner, at: nestedScopes.last!.extendableBegin!,
                                                       localReachabilityCache, context) {
        return .addressRange(range)
      }
      return nil
    }
    if owner.ownership == .owned {
      return .valueRange(computeLinearLiveness(for: owner, context))
    }
    // Trivial or guaranted owner.
    //
    // TODO: limit extension to the begin_borrow [var_decl] scope
    return .fullRange
  }
}

/// Return an InstructionRange covering all the dependent uses of 'value'.
private func computeDependentUseRange(of value: Value, within scopeExtension: inout ScopeExtension,
                                      _ localReachabilityCache: LocalVariableReachabilityCache,
                                      _ context: FunctionPassContext)
  -> InstructionRange? {

  guard var ownershipRange = scopeExtension.computeRange(localReachabilityCache, context) else {
    return nil
  }
  defer {ownershipRange.deinitialize()}

  // The innermost scope that must be extended must dominate all uses.
  var useRange = InstructionRange(begin: scopeExtension.innerScope.extendableBegin!, context)
  let function = value.parentFunction
  var walker = LifetimeDependentUseWalker(function, localReachabilityCache, context) {
    // Do not extend the useRange past the ownershipRange.
    let dependentInst = $0.instruction
    if ownershipRange.coversUse(dependentInst) {
      useRange.insert(dependentInst)
    }
    return .continueWalk
  }
  defer {walker.deinitialize()}

  _ = walker.walkDown(root: value)

  log("Scope fixup for dependent uses:\n\(useRange)")

  scopeExtension.dependsOnCaller = walker.dependsOnCaller

  // Lifetime dependenent uses may not be dominated by the access. The dependent value may be used by a phi or stored
  // into a memory location. The access may be conditional relative to such uses. If any use was not dominated, then
  // `useRange` will include the function entry.
  let firstInst = function.entryBlock.instructions.first!
  if firstInst != useRange.begin, useRange.contains(firstInst) {
    return nil
  }
  return useRange
}

// Extend nested scopes across a use-range within their owner's range.
extension ScopeExtension {
  func extend(over useRange: inout InstructionRange, _ context: some MutatingContext) {

    // Prepare to extend each scope.
    var scopesToExtend = SingleInlineArray<LifetimeDependence.Scope>()
    for innerScope in nestedScopes {
      guard let beginInst = innerScope.extendableBegin as? ScopedInstruction else {
        fatalError("all nested scopes must have a scoped begin instruction")
      }
      // Extend 'useRange' to to cover this scope's end instructions. The extended scope must at least cover the
      // original scope because the original scope may protect other operations.
      var requiresExtension = false
      for endInst in beginInst.endInstructions {
        if useRange.contains(endInst) {
          // If any end instruction is inside the new range, then all end instructions must be rewritten.
          requiresExtension = true
        } else {
          // Update 'range' with the current scope-ending instructions.
          useRange.insert(endInst)
        }
      }
      if !requiresExtension {
        break
      }
      if !innerScope.canExtend(over: &useRange, context) {
        // Scope ending instructions cannot be inserted at the 'range' boundary. Ignore all nested scopes.
        //
        // Note: We could still extend previously prepared inner scopes up to this 'innerScope'. To do that, we would
        // need to repeat the steps above: treat 'innerScope' as the new owner, and recompute 'useRange'. But this
        // scenario could only happen with nested coroutine, where the range boundary is reachable from the outer
        // coroutine's EndApply and AbortApply--it is vanishingly unlikely if not impossible.
        return
      }
      scopesToExtend.push(innerScope)
    }

    // Extend the scopes that actually required extension.
    for innerScope in scopesToExtend {
      innerScope.extend(over: &useRange, context)
    }
  }
}

// Extend a dependence scope to cover the dependent uses.
private extension LifetimeDependence.Scope {
  /// Return true if new scope-ending instruction can be inserted at the range boundary.
  func canExtend(over range: inout InstructionRange, _ context: some Context) -> Bool {
    switch self {
    case let .yield(yieldedValue):
      let beginApply = yieldedValue.definingInstruction as! BeginApplyInst
      let canEndAtBoundary = { (boundaryInst: Instruction) in
        switch beginApply.endReaches(block: boundaryInst.parentBlock, context) {
        case .abortReaches, .endReaches:
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
    default:
      // non-yield scopes can always be ended at any point.
      return true
    }
  }

  /// Extend this scope over the 'range' boundary.
  func extend(over range: inout InstructionRange, _ context: some MutatingContext) {
    guard let beginInst = extendableBegin as? ScopedInstruction else {
      fatalError("all nested scoped must have a scoped begin instruction")
    }
    // Collect the original end instructions and extend the range to to cover them. The resulting access scope
    // must cover the original scope because it may protect other memory operations.
    var endInsts = [Instruction]()
    for end in beginInst.endInstructions {
      assert(range.inclusiveRangeContains(end))
      endInsts.append(end)
    }
    insertBoundaryEnds(range: &range, context)

    // Delete original end instructions
    for endInst in endInsts {
      context.erase(instruction: endInst)
    }
  }

  /// Create new scope-ending instructions at the boundary of 'range'.
  func insertBoundaryEnds(range: inout InstructionRange, _ context: some MutatingContext) {
    for end in range.ends {
      let location = end.location.autoGenerated
      if end is ReturnInst {
        // End this inner scope just before the return. The mark_dependence base operand will be redirected to a
        // function argument.
        let builder = Builder(before: end, location: location, context)
        // Insert newEnd so that this scope will be nested in any outer scopes.
        range.insert(createEndInstruction(builder, context)!)
        continue
      }
      Builder.insert(after: end, location: location, context) {
        range.insert(createEndInstruction($0, context)!)
      }
    }
    for exitInst in range.exits {
      let location = exitInst.location.autoGenerated
      let builder = Builder(before: exitInst, location: location, context)
      range.insert(createEndInstruction(builder, context)!)
    }
  }

  /// Create a scope-ending instruction at 'builder's insertion point.
  func createEndInstruction(_ builder: Builder, _ context: some Context) -> Instruction? {
    switch self {
    case let .access(beginAccess):
      return builder.createEndAccess(beginAccess: beginAccess)
    case let .borrowed(beginBorrow):
      return builder.createEndBorrow(of: beginBorrow.value)
    case let .yield(yieldedValue):
      let beginApply = yieldedValue.definingInstruction as! BeginApplyInst
      return beginApply.createEnd(builder, context)
    default:
      return nil
    }
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
    }
  }

  enum EndReaches {
    case endReaches
    case abortReaches
  }

  /// Return the single kind of coroutine termination that reaches 'reachableBlock' or nil.
  func endReaches(block reachableBlock: BasicBlock, _ context: some Context) -> EndReaches? {
    var endBlocks = BasicBlockSet(context)
    var abortBlocks = BasicBlockSet(context)
    defer {
      endBlocks.deinitialize()
      abortBlocks.deinitialize()
    }
    for endInst in endInstructions {
      switch endInst {
      case let endApply as EndApplyInst:
        // Cannot extend the scope of a coroutine when the resume produces a value.
        if !endApply.type.isEmpty(in: parentFunction) {
          return nil
        }
        endBlocks.insert(endInst.parentBlock)
      case is AbortApplyInst:
        abortBlocks.insert(endInst.parentBlock)
      default:
        fatalError("invalid begin_apply ending instruction")
      }
    }
    var endReaches: EndReaches?
    var backwardWalk = BasicBlockWorklist(context)
    defer { backwardWalk.deinitialize() }

    let backwardVisit = { (block: BasicBlock) -> WalkResult in
      if endBlocks.contains(block) {
        switch endReaches {
        case .none:
          endReaches = .endReaches
          break
        case .endReaches:
          break
        case .abortReaches:
          return .abortWalk
        }
        return .continueWalk
      }
      if abortBlocks.contains(block) {
        switch endReaches {
        case .none:
          endReaches = .abortReaches
          break
        case .abortReaches:
          break
        case .endReaches:
          return .abortWalk
        }
        return .continueWalk
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
    return .continueWalk
  }

  mutating func storeToYieldDependence(address: Value, of operand: Operand) -> WalkResult {
    return .continueWalk
  }
}

