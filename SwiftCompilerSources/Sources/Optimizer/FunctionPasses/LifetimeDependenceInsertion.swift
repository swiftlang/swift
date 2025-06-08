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
        return dep.isScoped
      }
      return false
    }
    if hasScopedYield {
      // for consistency, we use yieldAddress if any yielded value is an address.
      let targetKind = beginApply.yieldedValues.contains(where: { $0.type.isAddress })
        ? TargetKind.yieldAddress : TargetKind.yield
      info.sources.push(LifetimeSource(targetKind: targetKind,
                                       convention: .scope(addressable: false, addressableForDeps: false),
                                       value: beginApply.token))
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
      // argument or scoped dependence). In this case, when that dependence root is diagnosed, the analysis will find
      // transitive uses of this apply's result.
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
      // addressable dependencies directly depend on the incoming address.
      if context.options.enableAddressDependencies() && source.convention.isAddressable(for: source.value) {
        bases.append(source.value)
        return
      }
      // Create a new dependence on the apply's access to the argument.
      for varIntroducer in gatherVariableIntroducers(for: source.value, context) {
        let scope = LifetimeDependence.Scope(base: varIntroducer, context)
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
    guard let initialAddress = resultOper.value.accessBase.address else {
      diagnoseUnknownDependenceSource(sourceLoc: apply.applySite.location.sourceLoc, context)
      return
    }
    Builder.insert(after: apply.applySite, context) { builder in
      insertMarkDependencies(value: initialAddress, initializer: resultOper.instruction, bases: sources.bases,
                             builder: builder, context)
    }
  }
}

private func diagnoseUnknownDependenceSource(sourceLoc: SourceLoc?, _ context: FunctionPassContext) {
  context.diagnosticEngine.diagnose(.lifetime_value_outside_scope, [], at: sourceLoc)
}

private func insertParameterDependencies(apply: LifetimeDependentApply, target: Operand,
                                         _ context: FunctionPassContext ) {
  guard var sources = apply.getParameterDependenceSources(target: target) else {
    return
  }
  log("Creating parameter dependencies for \(apply.applySite)")

  sources.initializeBases(context)

  assert(target.value.type.isAddress,
         "lifetime-dependent parameter must be 'inout'")

  Builder.insert(after: apply.applySite, context) {
    insertMarkDependencies(value: target.value, initializer: nil, bases: sources.bases, builder: $0, context)
  }
}

private func insertMarkDependencies(value: Value, initializer: Instruction?,
                                    bases: [Value], builder: Builder,
                                    _ context: FunctionPassContext) {
  var currentValue = value
  for base in bases {
    if value.type.isAddress {
      // Address dependencies cannot be represented as SSA values, so it does not make sense to replace any uses of the
      // dependent address.
      _ = builder.createMarkDependenceAddr(value: currentValue, base: base, kind: .Unresolved)
      continue
    }
    let markDep = builder.createMarkDependence(value: currentValue, base: base, kind: .Unresolved)
    let uses = currentValue.uses.lazy.filter {
      if $0.isScopeEndingUse {
        return false
      }
      let inst = $0.instruction
      return inst != markDep && inst != initializer && !(inst is Deallocation)
    }
    uses.replaceAll(with: markDep, context)
    currentValue = markDep
  }
}

/// Walk up the value dependence chain to find the best-effort variable declaration. Used to find the source of a borrow
/// dependence or to print the source variable in a diagnostic message.
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
  _ = useDefVisitor.walkUp(newLifetime: value)
  assert(!introducers.isEmpty, "missing variable introducer")
  return introducers
}

// =============================================================================
// VariableIntroducerUseDefWalker - upward walk
// =============================================================================

/// Walk up lifetime dependencies to the first value associated with a variable declaration.
///
/// To start walking:
///     walkUp(newLifetime: Value) -> WalkResult
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
/// SILGen emits temporary copies that violate lifetime dependence semantics. This utility looks through such temporary
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
struct VariableIntroducerUseDefWalker : LifetimeDependenceUseDefValueWalker, LifetimeDependenceUseDefAddressWalker {
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
 
  mutating func introducer(_ value: Value, _ owner: Value?) -> WalkResult {
    if let addrToPtr = value as? AddressToPointerInst {
      // AddressToPointer introduces the value dependence. To handle Builtin.addressOfBorrow, follow the address that
      // the pointer is derived from.
      return walkUp(address: addrToPtr.address)
    }
    return visitorClosure(value)
  }

  mutating func addressIntroducer(_ address: Value, access: AccessBaseAndScopes) -> WalkResult {
    return visitorClosure(address)
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

  /// Override to check for variable introducers: move_value, begin_value, before following
  /// OwnershipTransitionInstruction.
  mutating func walkUp(value: Value, _ owner: Value?) -> WalkResult {
    if let inst = value.definingInstruction, VariableScopeInstruction(inst) != nil {
      return visitorClosure(value)
    }
    return walkUpDefault(value: value, owner)
  }

  /// Override to check for on-stack variables before following an initializer.
  mutating func walkUp(address: Value, access: AccessBaseAndScopes) -> WalkResult {
    // Check for stack locations that correspond to an lvalue if there isn't any nested access scope.
    if access.innermostAccess == nil {
      if case let .stack(allocStack) = access.base {
        if allocStack.varDecl != nil {
          return addressIntroducer(allocStack, access: access)
        }
      }
    }
    return walkUpDefault(address: address, access: access)
  }
}

let variableIntroducerTest = FunctionTest("variable_introducer") {
    function, arguments, context in
  let value = arguments.takeValue()
  print("Variable introducers of: \(value)")
  print(gatherVariableIntroducers(for: value, context))
}
