//===--- LifetimeDependenceInsertion.swift - insert lifetime dependence ---===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2024 Apple Inc. and the Swift project authors
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

private func log(_ message: @autoclosure () -> String) {
  if verbose {
    print("### \(message())")
  }
}

let lifetimeDependenceInsertionPass = FunctionPass(
  name: "lifetime-dependence-insertion")
{ (function: Function, context: FunctionPassContext) in
  if !context.options.hasFeature(.NonescapableTypes) {
    return
  }
  log("Inserting lifetime dependence markers in \(function.name)")

  for instruction in function.instructions {
    if let dependentApply = LifetimeDependentApply(instruction) {
      insertDependencies(for: dependentApply, context)
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
    if !apply.hasResultDependence {
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
  /// A lifetime argument copies, borrows, or mutatably borrows the
  /// lifetime of the argument value.
  struct LifetimeArgument {
    let convention: LifetimeDependenceConvention
    let value: Value
  }

  func getLifetimeArguments() -> SingleInlineArray<LifetimeArgument> {
    var args = SingleInlineArray<LifetimeArgument>()
    for operand in applySite.parameterOperands {
      guard let dep = applySite.resultDependence(on: operand) else {
        continue
      }
      args.push(LifetimeArgument(convention: dep, value: operand.value))
    }
    return args
  }
}

/// If the result of this apply depends on the scope of one or more
/// arguments, then insert a mark_dependence [unresolved] from the
/// result on each argument so that the result is recognized as a
/// dependent value within each scope.
private func insertDependencies(for apply: LifetimeDependentApply,
  _ context: FunctionPassContext ) {
  precondition(apply.applySite.results.count > 0,
    "a lifetime-dependent instruction must have at least one result")

  let bases = findDependenceBases(of: apply, context)
  let builder = Builder(after: apply.applySite, context)
  for dependentValue in apply.applySite.resultOrYields {
    insertMarkDependencies(value: dependentValue, initializer: nil,
                           bases: bases, builder: builder, context)
  }
  for resultOper in apply.applySite.indirectResultOperands {
    let accessBase = resultOper.value.accessBase
    guard let (initialAddress, initializingStore) =
            accessBase.findSingleInitializer(context) else {
      continue
    }
    // TODO: This is currently too strict for a diagnostic pass. We
    // should handle/cleanup projections and casts that occur before
    // the initializingStore. Or check in the SIL verifier that all
    // stores without an access scope follow this form. Then convert
    // this bail-out to an assert.
    guard initialAddress.usesOccurOnOrAfter(instruction: initializingStore,
                                            context) else {
      continue
    }
    assert(initializingStore == resultOper.instruction,
           "an indirect result is a store")
    insertMarkDependencies(value: initialAddress,
                           initializer: initializingStore, bases: bases,
                           builder: builder, context)
  }
}

private func findDependenceBases(of apply: LifetimeDependentApply,
                                 _ context: FunctionPassContext)
  -> [Value] {
  log("Creating dependencies for \(apply.applySite)")
  var bases: [Value] = []
  for lifetimeArg in apply.getLifetimeArguments() {
    switch lifetimeArg.convention {
    case .inherit:
      continue
    case .scope:
      // Create a new dependence on the apply's access to the argument.
      for varIntoducer in gatherVariableIntroducers(for: lifetimeArg.value,
                                                    context) {
        if let scope =
             LifetimeDependence.Scope(base: varIntoducer, context) {
          log("Scoped lifetime from \(lifetimeArg.value)")
          log("  scope: \(scope)")
          bases.append(scope.parentValue)
        }
      }
    }
  }
  return bases
}

private func insertMarkDependencies(value: Value, initializer: Instruction?,
                                    bases: [Value], builder: Builder,
                                    _ context: FunctionPassContext) {
  var currentValue = value
  for base in bases {
    let markDep = builder.createMarkDependence(
      value: currentValue, base: base, kind: .Unresolved)

    let uses = currentValue.uses.lazy.filter {
      let inst = $0.instruction
      return inst != markDep && inst != initializer && !(inst is Deallocation)
    }
    uses.replaceAll(with: markDep, context)
    currentValue = markDep
  }
}

/*
/// Return base values that this return value depends on.
///
/// For lifetime copies, walk up the dependence chain to find the
/// dependence roots, inserting dependencies for any
/// LifetimeDependentApply.
private func recursivelyFindDependenceBases(of apply: LifetimeDependentApply,
                                            _ context: FunctionPassContext)
  -> [Value] {
  log("Creating dependencies for \(apply.applySite)")
  var bases: [Value] = []
  for lifetimeArg in apply.getLifetimeArguments() {
    switch lifetimeArg.convention {
    case .inherit:
      // Inherit the argument's lifetime dependence by finding the
      // roots.  This requires that a mark_dependence [nonescaping]
      // already be created for any earlier LifetimeDependentApply.
      _ = LifetimeDependence.visitDependenceRoots(enclosing: lifetimeArg.value,
                                                  context)
      { (scope: LifetimeDependence.Scope) in
        if let updatedScope = recursivelyUpdate(scope: scope, context) {
          log("Inherited lifetime from \(lifetimeArg.value)")
          log("  depends on: \(updatedScope)")
          bases.append(updatedScope.parentValue)
        }
        return .continueWalk
      }
    case .scope:
      // Create a new dependence on the apply's access to the argument.
      if let scope =
           LifetimeDependence.Scope(base: lifetimeArg.value, context) {
        log("Scoped lifetime from \(lifetimeArg.value)")
        log("  scope: \(scope)")
        bases.append(scope.parentValue)
      }
    }
  }
  return bases
}

// Recursively insert dependencies, assuming no cycle of dependent applies.
//
// TODO: needs unit test.
private func recursivelyUpdate(scope: LifetimeDependence.Scope,
  _ context: FunctionPassContext) -> LifetimeDependence.Scope? {
  if let dependentApply =
    LifetimeDependentApply(withResult: scope.parentValue) {
    insertDependencies(for: dependentApply, context)
    // If a mark_dependence [nonescaping] was created for this apply,
    // then return it as the updated dependence. Otherwise, return the
    // original dependence.
    if let markDep = scope.parentValue.uses.singleUse?.instruction
      as? MarkDependenceInst {
      return LifetimeDependence(markDep, context)?.scope
    }
  }
  return scope
}
*/
