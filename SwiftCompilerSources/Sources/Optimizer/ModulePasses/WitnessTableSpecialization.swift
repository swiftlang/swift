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
/// specialized witness methods.
///
/// Embedded Swift creates specialized witness tables in the mandatory pipeline.
///
let witnessTableSpecialization = ModulePass(name: "witness-table-specialization") {
  (moduleContext: ModulePassContext) in

  if moduleContext.options.enableEmbeddedSwift {
    return
  }

  // Collect the conformances first, because specialization creates new functions.
  var conformances: [(Conformance, Function)] = []
  var visited = Set<Conformance>()

  for function in moduleContext.functions where function.isDefinition && !function.isGeneric {
    for inst in function.instructions {
      guard let initExistential = inst as? InitExistentialInstruction else {
        continue
      }
      for conformance in initExistential.conformances
        where conformance.isConcrete && !conformance.protocol.isMarkerProtocol
      {
        let baseConf = conformance.isInherited ? conformance.inheritedConformance : conformance
        if baseConf.isSpecialized && visited.insert(conformance.canonical).inserted {
          conformances.append((conformance, function))
        }
      }
    }
  }

  for (conformance, function) in conformances {
    _ = specializeWitnessTableCompletely(for: conformance, referencedFrom: function, moduleContext)
  }
}
