//===--- LifetimeDependenceInsertion.swift - insert lifetime dependence ---===//
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
///
/// Insert mark_dependence [nonescaping] markers on the owned returned
/// or yielded value of a call whose return type is non-escaping.
///
/// Pass dependencies: This must run as a SILGen cleanup pass before
/// any lifetime canonicalization or optimization can be performed.
///
//===----------------------------------------------------------------------===//

import SIL

private let verbose = false

private func log(prefix: Bool = true, _ message: @autoclosure () -> String) {
  if verbose {
    debugLog(prefix: prefix, message())
  }
}

let lifetimeDependenceInsertionPass = FunctionPass(
  name: "lifetime-dependence-insertion")
{ (function: Function, context: FunctionPassContext) in
#if os(Windows)
  if !context.options.hasFeature(.NonescapableTypes) {
    return
  }
#endif
  log(prefix: false, "\n--- Inserting lifetime dependence markers in \(function.name)")

  for instruction in function.instructions {
    if let dependentApply = LifetimeDependentApply(instruction) {
      for operand in dependentApply.applySite.parameterOperands {
        insertParameterDependencies(apply: dependentApply, target: operand, context)
      }
      insertResultDependencies(for: dependentApply, context)
    }
  }
}

/// An apply that produces a non-escapable value, linking it to a parent value.
private struct LifetimeDependentApply {
  let applySite: FullApplySite

  init?(_ instruction: Instruction) {
    guard let apply = instruction as? FullApplySite else {
      return nil
    }
    if !apply.hasLifetimeDependence {
      return nil
    }
    self.applySite = apply
  }

  init?(withResult value: Value) {
    switch value {
    case let apply as ApplyInst:
      if let dependentApply = LifetimeDependentApply(apply) {
        self = dependentApply
      }
    case let arg as Argument:
      guard let termResult = TerminatorResult(arg) else { return nil }
      switch termResult.terminator {
      case let ta as TryApplyInst:
        if termResult.successor == ta.errorBlock {
          if let dependentApply = LifetimeDependentApply(ta) {
            self = dependentApply
          }
        }
      default:
        break
      }
    default:
      break
    }
    return nil
  }
}

extension LifetimeDependentApply {
  enum TargetKind {
    case result
    case inParameter
    case inoutParameter
    case yield
    case yieldAddress
  }
  
  /// A lifetime argument that either inherits or creates a new scope for the lifetime of the argument value.
  struct LifetimeSource {
    let targetKind: TargetKind
    let convention: LifetimeDependenceConvention
    let value: Value
  }

  /// List of lifetime dependencies for a single target.
  struct LifetimeSourceInfo {
    var sources = SingleInlineArray<LifetimeSource>()
    var bases = [Value]()
  }

  func getResultDependenceSources() -> LifetimeSourceInfo? {
    guard applySite.hasResultDependence else {
      return nil
    }
    var info = LifetimeSourceInfo()
    if let beginApply = applySite as? BeginApplyInst {
      return getYieldDependenceSources(beginApply: beginApply)
    }
    for operand in applySite.parameterOperands {
      guard let dep = applySite.resultDependence(on: operand) else {
        continue
      }
      info.sources.push(LifetimeSource(targetKind: .result, convention: dep, value: operand.value))
    }
    return info
  }

  func getYieldDependenceSources(beginApply: BeginApplyInst) -> LifetimeSourceInfo? {
    var info = LifetimeSourceInfo()
    let hasScopedYield = applySite.parameterOperands.contains {
      if let dep = applySite.resultDependence(on: $0) {
        return dep == .scope
      }
      return false
    }
    if hasScopedYield {
      // for consistency, we you yieldAddress if any yielded value is an address.
      let targetKind = beginApply.yieldedValues.contains(where: { $0.type.isAddress })
        ? TargetKind.yieldAddress : TargetKind.yield
      info.sources.push(LifetimeSource(targetKind: targetKind, convention: .scope, value: beginApply.token))
    }
    for operand in applySite.parameterOperands {
      guard let dep = applySite.resultDependence(on: operand) else {
        continue
      }
      switch dep {
      case .inherit:
        continue
      case .scope:
        for yieldedValue in beginApply.yieldedValues {
          let targetKind = yieldedValue.type.isAddress ? TargetKind.yieldAddress : TargetKind.yield
          info.sources.push(LifetimeSource(targetKind: targetKind, convention: .inherit, value: operand.value))
        }
      }
    }
    return info
  }

  func getParameterDependenceSources(target: Operand) -> LifetimeSourceInfo? {
    guard let deps = applySite.parameterDependencies(target: target) else {
      return nil
    }
    var info = LifetimeSourceInfo()
    let targetKind = {
      let convention = applySite.convention(of: target)!
      switch convention {
      case .indirectInout, .indirectInoutAliasable, .packInout:
        return TargetKind.inoutParameter
      case .indirectIn, .indirectInGuaranteed, .indirectInCXX, .directOwned, .directUnowned, .directGuaranteed,
           .packOwned, .packGuaranteed:
        return TargetKind.inParameter
      case .indirectOut, .packOut:
        debugLog("\(applySite)")
        fatalError("Lifetime dependencies cannot target \(convention) parameter")
      }
    }()
    for (dep, operand) in zip(deps, applySite.parameterOperands) {
      guard let dep = dep else {
        continue
      }
      info.sources.push(LifetimeSource(targetKind: targetKind, convention: dep, value: operand.value))
    }
    return info
  }
}

private extension LifetimeDependentApply.LifetimeSourceInfo {
  mutating func initializeBases(_ context: FunctionPassContext) {
    for source in sources {
      // Inherited dependencies do not require a mark_dependence if the target is a result or yielded value. The
      // inherited lifetime is nonescapable, so either
      //
      // (a) the result or yield is never returned from this function
      //
      // (b) the inherited lifetime has a dependence root within this function (it comes from a dependent function
      // argument or scoped dependence). In this case, when that depedence root is diagnosed, the analysis will find
      // transtive uses of this apply's result.
      //
      // (c) the dependent value is passed to another call with a dependent inout argument, or it is stored to a yielded
      // address of a coroutine that has a dependent inout argument. In this case, a mark_dependence will already be
      // created for that inout argument.
      switch source.convention {
      case .inherit:
        break
      case .scope:
        initializeScopedBases(source: source, context)
      }
    }
  }

  // Scoped dependencies require a mark_dependence for every variable that introduces this scope.
  mutating func initializeScopedBases(source: LifetimeDependentApply.LifetimeSource, _ context: FunctionPassContext) {
    switch source.targetKind {
    case .yield, .yieldAddress:
      // A coroutine creates its own borrow scope, nested within its borrowed operand.
      bases.append(source.value)
    case .result, .inParameter, .inoutParameter:
      // Create a new dependence on the apply's access to the argument.
      for varIntoducer in gatherVariableIntroducers(for: source.value, context) {
        let scope = LifetimeDependence.Scope(base: varIntoducer, context)
        log("Scoped lifetime from \(source.value)")
        log("  scope: \(scope)")
        bases.append(scope.parentValue)
      }
    }
  }
}

/// If the result of this apply depends on the scope of one or more
/// arguments, then insert a mark_dependence [unresolved] from the
/// result on each argument so that the result is recognized as a
/// dependent value within each scope.
private func insertResultDependencies(for apply: LifetimeDependentApply, _ context: FunctionPassContext ) {
  guard var sources = apply.getResultDependenceSources() else {
    return
  }
  log("Creating result dependencies for \(apply.applySite)")

  // Find the dependence base for each source.
  sources.initializeBases(context)

  for dependentValue in apply.applySite.resultOrYields {
    let builder = Builder(before: dependentValue.nextInstruction, context)
    insertMarkDependencies(value: dependentValue, initializer: nil, bases: sources.bases, builder: builder, context)
  }
  for resultOper in apply.applySite.indirectResultOperands {
    let accessBase = resultOper.value.accessBase
    guard case let .store(initializingStore, initialAddress) = accessBase.findSingleInitializer(context) else {
      continue
    }
    assert(initializingStore == resultOper.instruction, "an indirect result is a store")
    Builder.insert(after: apply.applySite, context) { builder in
      insertMarkDependencies(value: initialAddress, initializer: initializingStore, bases: sources.bases,
                             builder: builder, context)
    }
  }
}

private func insertParameterDependencies(apply: LifetimeDependentApply, target: Operand,
                                         _ context: FunctionPassContext ) {
  guard var sources = apply.getParameterDependenceSources(target: target) else {
    return
  }
  log("Creating parameter dependencies for \(apply.applySite)")

  sources.initializeBases(context)

  Builder.insert(after: apply.applySite, context) {
    insertMarkDependencies(value: target.value, initializer: nil, bases: sources.bases, builder: $0, context)
  }
}

private func insertMarkDependencies(value: Value, initializer: Instruction?,
                                    bases: [Value], builder: Builder,
                                    _ context: FunctionPassContext) {
  var currentValue = value
  for base in bases {
    let markDep = builder.createMarkDependence(
      value: currentValue, base: base, kind: .Unresolved)

    // Address dependencies cannot be represented as SSA values, so it does not make sense to replace any uses of the
    // dependent address.
    //
    // TODO: either (1) insert a separate mark_dependence_addr instruction with no return value, or (2) perform data
    // flow to replace all reachable address uses, and if any aren't dominated by base, then insert an extra
    // escaping mark_dependence at this apply site that directly uses the mark_dependence [nonescaping] to force
    // diagnostics to fail.
    if !value.type.isAddress {
      let uses = currentValue.uses.lazy.filter {
        if $0.isScopeEndingUse {
          return false
        }
        let inst = $0.instruction
        return inst != markDep && inst != initializer && !(inst is Deallocation)
      }
      uses.replaceAll(with: markDep, context)
    }
    currentValue = markDep
  }
}

/// Walk up the value dependence chain to find the best-effort variable declaration. Typically called while diagnosing
/// an error.
///
/// Returns an array with at least one introducer value.
///
/// The walk stops at:
/// - a variable declaration (begin_borrow [var_decl], move_value [var_decl])
/// - a begin_access for a mutable variable access
/// - the value or address "root" of the dependence chain
func gatherVariableIntroducers(for value: Value, _ context: Context)
  -> SingleInlineArray<Value>
{
  var introducers = SingleInlineArray<Value>()
  var useDefVisitor = VariableIntroducerUseDefWalker(context, scopedValue: value) {
    introducers.push($0)
    return .continueWalk
  }
  defer { useDefVisitor.deinitialize() }
  _ = useDefVisitor.walkUp(valueOrAddress: value)
  assert(!introducers.isEmpty, "missing variable introducer")
  return introducers
}

// =============================================================================
// VariableIntroducerUseDefWalker - upward walk
// =============================================================================

/// Walk up lifetime dependencies to the first value associated with a variable declaration.
///
/// To start walking:
///     walkUp(valueOrAddress: Value) -> WalkResult
///
/// This utility finds the value or address associated with the lvalue (variable declaration) that is passed as the
/// source of a lifetime dependent argument. If no lvalue is found, then it finds the "root" of the chain of temporary
/// rvalues.
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
/// VariableIntroducerUseDefWalker extends the ForwardingUseDefWalker to follow copies, moves, and
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
struct VariableIntroducerUseDefWalker : ForwardingUseDefWalker {
  // The ForwardingUseDefWalker's context is the most recent lifetime owner.
  typealias PathContext = Value?

  let context: Context

  // If the scoped value is trivial, then only the variable's lexical scope is relevant, and access scopes can be
  // ignored.
  let isTrivialScope: Bool

  // This visited set is only really needed for instructions with
  // multiple results, including phis.
  private var visitedValues: ValueSet

  // Call \p visit rather than calling this directly.
  private let visitorClosure: (Value) -> WalkResult

  init(_ context: Context, scopedValue: Value, _ visitor: @escaping (Value) -> WalkResult) {
    self.context = context
    self.isTrivialScope = scopedValue.type.isAddress
      ? scopedValue.type.objectType.isTrivial(in: scopedValue.parentFunction)
      : scopedValue.isTrivial(context)
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

  mutating func walkUp(valueOrAddress: Value) -> WalkResult {
    if valueOrAddress.type.isAddress {
      return walkUp(address: valueOrAddress)
    }
    return walkUp(newLifetime: valueOrAddress)
  }
}

// Helpers
extension VariableIntroducerUseDefWalker {
  mutating func walkUp(newLifetime: Value) -> WalkResult {
    let newOwner = newLifetime.ownership == .owned ? newLifetime : nil
    return walkUp(value: newLifetime, newOwner)
  }

  mutating func walkUp(value: Value, _ owner: Value?) -> WalkResult {
    // Check for variable introducers: move_value, begin_value, before following OwnershipTransitionInstruction.
    if let inst = value.definingInstruction, VariableScopeInstruction(inst) != nil {
      return visitorClosure(value)
    }
    switch value.definingInstruction {
    case let transition as OwnershipTransitionInstruction:
      return walkUp(newLifetime: transition.operand.value)
    case let load as LoadInstruction:
      return walkUp(address: load.address)
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

  // Handle temporary allocations and access scopes.
  mutating func walkUp(address: Value) -> WalkResult {
    let accessBaseAndScopes = address.accessBaseWithScopes
    // Continue walking for some kinds of access base.
    switch accessBaseAndScopes.base {
    case .box, .global, .class, .tail, .pointer, .index, .unidentified:
      break
    case let .stack(allocStack):
      if allocStack.varDecl == nil {
        // Ignore temporary stack locations. Their access scopes do not affect lifetime dependence.
        return walkUp(stackInitializer: allocStack, at: address)
      }
    case let .argument(arg):
      // Ignore access scopes for @in or @in_guaranteed arguments when all scopes are reads. Do not ignore a [read]
      // access of an inout argument or outer [modify]. Mutation later with the outer scope could invalidate the
      // borrowed state in this narrow scope. Do not ignore any mark_depedence on the address.
      if arg.convention.isIndirectIn && accessBaseAndScopes.isOnlyReadAccess {
        return introducer(arg, nil)
      }
      // @inout arguments may be singly initialized (when no modification exists in this function), but this is not
      // relevant here because they require nested access scopes which can never be ignored.
    case let .yield(yieldedAddress):
      // Ignore access scopes for @in or @in_guaranteed yields when all scopes are reads.
      let apply = yieldedAddress.definingInstruction as! FullApplySite
      if apply.convention(of: yieldedAddress).isIndirectIn && accessBaseAndScopes.isOnlyReadAccess {
        return introducer(yieldedAddress, nil)
      }
    case .storeBorrow(let sb):
      // Walk up through a store into a temporary.
      if accessBaseAndScopes.scopes.isEmpty,
         case .stack = sb.destinationOperand.value.accessBase {
        return walkUp(newLifetime: sb.source)
      }
    }
    // Skip the access scope for unsafe[Mutable]Address. Treat it like a projection of 'self' rather than a separate
    // variable access.
    if case let .access(innerAccess) = accessBaseAndScopes.scopes.first,
       let addressorSelf = innerAccess.unsafeAddressorSelf {
      return walkUp(valueOrAddress: addressorSelf)
    }
    // Ignore the acces scope for trivial values regardless of whether it is singly-initialized. Trivial values do not
    // need to be kept alive in memory and can be safely be overwritten in the same scope. Lifetime dependence only
    // cares that the loaded value is within the lexical scope of the trivial value's variable declaration. Rather than
    // skipping all access scopes, call 'walkUp' on each nested access in case one of them needs to redirect the walk,
    // as required for 'access.unsafeAddressorSelf'.
    if isTrivialScope {
      switch accessBaseAndScopes.scopes.first {
      case .none, .base:
        break
      case let .access(beginAccess):
        return walkUp(address: beginAccess.address)
      case let .dependence(markDep):
        return walkUp(address: markDep.value)
      }
    }
    return introducer(accessBaseAndScopes.enclosingAccess.address ?? address, nil)
  }

  // Handle singly-initialized temporary stack locations.
  mutating func walkUp(stackInitializer allocStack: AllocStackInst, at address: Value) -> WalkResult {
    guard let initializer = allocStack.accessBase.findSingleInitializer(context) else {
      return introducer(address, nil)
    }
    if case let .store(store, _) = initializer {
      switch store {
      case let store as StoringInstruction:
        return walkUp(newLifetime: store.source)
      case let srcDestInst as SourceDestAddrInstruction:
        return walkUp(address: srcDestInst.destination)
      case let apply as FullApplySite:
        if let f = apply.referencedFunction, f.isConvertPointerToPointerArgument {
          return walkUp(address: apply.parameterOperands[0].value)
        }
      default:
        break
      }
    }
    return introducer(address, nil)
  }
}

let variableIntroducerTest = FunctionTest("variable_introducer") {
    function, arguments, context in
  let value = arguments.takeValue()
  print("Variable introducers of: \(value)")
  print(gatherVariableIntroducers(for: value, context))
}
