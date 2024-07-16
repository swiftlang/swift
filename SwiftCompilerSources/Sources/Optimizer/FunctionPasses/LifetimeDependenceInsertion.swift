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
    let convention: LifetimeDependenceConvention
    let value: Value
  }

  /// List of lifetime dependencies for a single target.
  struct LifetimeSources {
    let targetKind: TargetKind
    var sources = SingleInlineArray<LifetimeSource>()
  }

  func getResultDependenceSources() -> LifetimeSources? {
    guard applySite.hasResultDependence else { return nil }
    var sources: LifetimeSources
    switch applySite {
    case let beginApply as BeginApplyInst:
      if beginApply.yieldedValues.contains(where: { $0.type.isAddress }) {
        sources = LifetimeSources(targetKind: .yieldAddress)
      } else {
        sources = LifetimeSources(targetKind: .yield)
      }
    default:
      sources = LifetimeSources(targetKind: .result)
    }
    for operand in applySite.parameterOperands {
      guard let dep = applySite.resultDependence(on: operand) else {
        continue
      }
      sources.sources.push(LifetimeSource(convention: dep, value: operand.value))
    }
    return sources
  }

  func getParameterDependenceSources(target: Operand) -> LifetimeSources? {
    guard let deps = applySite.parameterDependencies(target: target) else {
      return nil
    }
    var sources: LifetimeSources
    let convention = applySite.convention(of: target)!
    switch convention {
    case .indirectInout, .indirectInoutAliasable, .packInout:
      sources = LifetimeSources(targetKind: .inoutParameter)
    case .indirectIn, .indirectInGuaranteed, .indirectInCXX, .directOwned, .directUnowned, .directGuaranteed,
         .packOwned, .packGuaranteed:
      sources = LifetimeSources(targetKind: .inParameter)
    case .indirectOut, .packOut:
      debugLog("\(applySite)")
      fatalError("Lifetime dependencies cannot target \(convention) parameter")
    }
    for (dep, operand) in zip(deps, applySite.parameterOperands) {
      guard let dep = dep else {
        continue
      }
      sources.sources.push(LifetimeSource(convention: dep, value: operand.value))
    }
    return sources
  }

  // Scoped dependencies require a mark_dependence for every variable that introduces this scope.
  //
  // Inherited dependencies do not require a mark_dependence if the target is a result or yielded value. The inherited
  // lifetime is nonescapable, so either
  //
  // (a) the result or yield is never returned from this function
  //
  // (b) the inherited lifetime has a dependence root within this function (it comes from a dependent function argument
  // or scoped dependence). In this case, when that depedence root is diagnosed, the analysis will find transtive uses
  // of this apply's result.
  //
  // (c) the dependent value is passed to another call with a dependent inout argument, or it is stored to a yielded
  // address of a coroutine that has a dependent inout argument. In this case, a mark_dependence will already be created
  // for that inout argument.
  //
  // Parameter dependencies and yielded addresses always require a mark_dependence.
  static func findDependenceBases(sources: LifetimeSources, _ context: FunctionPassContext) -> [Value] {
    var bases: [Value] = []
    for source in sources.sources {
      switch source.convention {
      case .inherit:
        switch sources.targetKind {
        case .result, .yield:
          continue
        case .inParameter, .inoutParameter, .yieldAddress:
          _ = LifetimeDependence.visitDependenceRoots(enclosing: source.value, context) { scope in
            log("Inherited lifetime from \(source.value)")
            log("  scope: \(scope)")
            bases.append(scope.parentValue)
            return .continueWalk
          }
        }
      case .scope:
        // Create a new dependence on the apply's access to the argument.
        for varIntoducer in gatherVariableIntroducers(for: source.value, context) {
          if let scope = LifetimeDependence.Scope(base: varIntoducer, context) {
            log("Scoped lifetime from \(source.value)")
            log("  scope: \(scope)")
            bases.append(scope.parentValue)
          }
        }
      }
    }
    return bases
  }
}

/// If the result of this apply depends on the scope of one or more
/// arguments, then insert a mark_dependence [unresolved] from the
/// result on each argument so that the result is recognized as a
/// dependent value within each scope.
private func insertResultDependencies(for apply: LifetimeDependentApply, _ context: FunctionPassContext ) {
  guard let sources = apply.getResultDependenceSources() else {
    return
  }
  log("Creating dependencies for \(apply.applySite)")

  let bases = LifetimeDependentApply.findDependenceBases(sources: sources, context)

  for dependentValue in apply.applySite.resultOrYields {
    let builder = Builder(before: dependentValue.nextInstruction, context)
    insertMarkDependencies(value: dependentValue, initializer: nil, bases: bases, builder: builder, context)
  }
  for resultOper in apply.applySite.indirectResultOperands {
    let accessBase = resultOper.value.accessBase
    guard let (initialAddress, initializingStore) = accessBase.findSingleInitializer(context) else {
      continue
    }
    // TODO: This might bail-out on SIL that should be diagnosed. We should handle/cleanup projections and casts that
    // occur before the initializingStore. Or check in the SIL verifier that all stores without an access scope follow
    // this form. Then convert this bail-out to an assert.
    guard initialAddress.usesOccurOnOrAfter(instruction: initializingStore, context) else {
      continue
    }
    assert(initializingStore == resultOper.instruction, "an indirect result is a store")
    Builder.insert(after: apply.applySite, context) { builder in
      insertMarkDependencies(value: initialAddress, initializer: initializingStore, bases: bases, builder: builder,
                             context)
    }
  }
}

private func insertParameterDependencies(apply: LifetimeDependentApply, target: Operand,
                                         _ context: FunctionPassContext ) {
  guard let sources = apply.getParameterDependenceSources(target: target) else {
    return
  }
  log("Creating dependencies for \(apply.applySite)")

  let bases = LifetimeDependentApply.findDependenceBases(sources: sources, context)

  Builder.insert(after: apply.applySite, context) {
    insertMarkDependencies(value: target.value, initializer: nil, bases: bases, builder: $0, context)    
  }
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
