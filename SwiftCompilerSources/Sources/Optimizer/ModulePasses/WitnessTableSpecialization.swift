//===--- WitnessTableSpecialization.swift ---------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import AST
import SIL

/// Creates specialized witness tables for existentials of specialized generic types.
///
/// ```
///   %1 = init_existential_addr %0, $Box<Int>   // conformance Box<Int>: P
/// ```
/// Without a specialized witness table, IRGen instantiates the witness table for `Box<Int>: P`
/// at runtime from the generic witness table pattern of `Box<T>: P`, and calls through the
/// existential end up in the unspecialized witness methods.
/// With a specialized witness table
/// ```
///   sil_witness_table shared [specialized] Box<Int>: specialize <Int> (<T> Box<T>: P module test) {
///     method #P.foo: <Self where Self : P> (Self) -> () -> Int : @$specialized_witness_method
///   }
/// ```
/// IRGen emits a static witness table, which is directly referenced, and which contains the
/// specialized witness methods - if possible. Otherwise the witness table is still instantiated
/// at runtime. The specialized witness table is still kept in SIL, so that it can be used by
/// optimizations in the future, e.g. for devirtualization.
///
/// All witness methods of a specialized witness table are specialized, even if they are never
/// called. Therefore the pass doesn't run on functions which are optimized for code size.
/// Witness methods which cannot be specialized, e.g. generic method requirements, keep the original
/// generic witness.
///
/// Embedded Swift creates specialized witness tables in the mandatory pipeline.
///
let witnessTableSpecialization = ModulePass(name: "witness-table-specialization") {
  (moduleContext: ModulePassContext) in

  if moduleContext.options.enableEmbeddedSwift {
    return
  }

  var specializer = WitnessTableSpecializer(moduleContext)

  // Collect the conformances first, because specialization creates new functions.
  var conformances: [(Conformance, Location)] = []
  for function in moduleContext.functions
    where function.isDefinition && !function.isGeneric && !function.isOptimizedForSize
  {
    specializer.collectConformances(in: function, &conformances)
  }
  specializer.specialize(conformances)

  specializer.processNewFunctions()
}

private struct WitnessTableSpecializer {
  private let context: ModulePassContext
  private var history = SpecializationHistory()
  private var visitedConformances = Set<Conformance>()

  /// The functions which existed before the pass, and the new functions which are already handled.
  private var handledFunctions: Set<Function>

  /// New functions - specialized witnesses and their specialized callees - which must be handled.
  private var newFunctions: [Function] = []

  init(_ context: ModulePassContext) {
    self.context = context
    self.handledFunctions = Set(context.functions)
  }

  mutating func collectConformances(in function: Function, _ conformances: inout [(Conformance, Location)]) {
    for inst in function.instructions {
      guard let initExistential = inst as? InitExistentialInstruction else {
        continue
      }
      for conformance in initExistential.conformances
        where conformance.isConcrete && !conformance.protocol.isMarkerProtocol
      {
        let baseConf = conformance.isInherited ? conformance.inheritedConformance : conformance
        if baseConf.isSpecialized && visitedConformances.insert(conformance.canonical).inserted {
          conformances.append((conformance, initExistential.location))
        }
      }
    }
  }

  mutating func specialize(_ conformances: [(Conformance, Location)]) {
    for (conformance, location) in conformances {
      if specializeWitnessTable(for: conformance, errorLocation: location, history: &history, context) {
        var visited = Set<Conformance>()
        addWitnessMethods(of: conformance, &visited)
      }
    }
  }

  /// Adds the witnesses of the specialized witness table of `conformance` and of its nested tables.
  /// Unlike `FunctionWorklist.addWitnessMethods`, this includes the partially specialized witnesses of generic
  /// requirements.
  private mutating func addWitnessMethods(of conformance: Conformance, _ visited: inout Set<Conformance>) {
    guard conformance.isConcrete,
          visited.insert(conformance.canonical).inserted,
          let witnessTable = context.lookupWitnessTable(for: conformance.canonical),
          witnessTable.isSpecialized
    else {
      return
    }
    for entry in witnessTable.entries {
      switch entry {
      case .invalid, .associatedType:
        break
      case .method(_, let witness):
        if let witness {
          addIfNew(witness, allowGeneric: true)
        }
      case .baseProtocol(_, let baseConf):
        addWitnessMethods(of: baseConf, &visited)
      case .associatedConformance(_, let assocConf):
        addWitnessMethods(of: assocConf, &visited)
      }
    }
  }

  /// Generic functions are only added if they are new, i.e. partially specialized witnesses.
  private mutating func addIfNew(_ function: Function, allowGeneric: Bool = false) {
    if function.isDefinition && (allowGeneric || !function.isGeneric) && !function.isOptimizedForSize &&
       handledFunctions.insert(function).inserted
    {
      newFunctions.append(function)
    }
  }

  /// The specialized witnesses are created after the generic specializer ran, so their generic callees are not
  /// specialized yet, and existentials formed in those callees aren't visible to this pass. Specialize the calls
  /// in the new functions transitively and specialize the witness tables of the existentials found there.
  mutating func processNewFunctions() {
    while let function = newFunctions.popLast() {
      var calleesBeforeSpecialization = FunctionWorklist()
      calleesBeforeSpecialization.addCallees(of: function, context)
      let oldCallees = Set(calleesBeforeSpecialization.functions)

      context.transform(function: function) { functionContext in
        if functionContext.loadFunction(function: function, loadCalleesRecursively: true) {
          // Witness and class method calls on concrete types become direct calls, which can then be specialized.
          let applies = function.instructions.compactMap { $0 as? FullApplySite }
          for apply in applies where apply.referencedFunction == nil {
            _ = functionContext.tryDevirtualize(apply: apply, isMandatory: false)
          }
          // The generic specializer needs the bodies of the callees.
          for case let apply as FullApplySite in function.instructions {
            if let callee = apply.referencedFunction, !callee.isDefinition {
              _ = functionContext.loadFunction(function: callee, loadCalleesRecursively: true)
            }
          }
          // Partially specialized witnesses call generic functions with partially concrete substitutions.
          _ = functionContext.specializeApplies(in: function, isMandatory: false,
                                                allowPartialSpecialization: function.isGeneric)
        }
      }
      var conformances: [(Conformance, Location)] = []
      collectConformances(in: function, &conformances)
      specialize(conformances)

      var callees = FunctionWorklist()
      callees.addCallees(of: function, context)
      for callee in callees.functions {
        // Callees which the specializer just created can be partial specializations, which are generic.
        addIfNew(callee, allowGeneric: !oldCallees.contains(callee))
      }
    }
  }
}
