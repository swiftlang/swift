//===--- MandatoryPerformanceOptimizations.swift --------------------------===//
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

import SIL

let devirtualizeWitnessTables = ModulePass(name: "devirtualize-witness-tables") {
  (moduleContext: ModulePassContext) in
  
  for fn in moduleContext.functions {
    for block in fn.blocks {
      for inst in block.instructions {
        
        guard let initExistentialInst = inst as? InitExistentialAddrInst else { continue }
        for conformance in initExistentialInst.conformances {
          guard conformance.isConcrete() else { continue }
          guard let specProtocolConformance = conformance.getConcrete().getAsSpecializedProtocolConformance() else { continue }

          if moduleContext.lookUpWitnessTable(specProtocolConformance) != nil { continue }
          guard let root = specProtocolConformance.getGenericConformance() else { continue }

          guard let foundTable = moduleContext.lookUpWitnessTable(root) else { continue }

// Better implementation that takes three minutes to type check before crashing the compiler 🧚
//          let newEntries = foundTable.entries
//            .compactMap {
//              guard let fn = $0.methodFunction else { return nil }
//              guard let spec = moduleContext.specialize(function: fn, withSubstitutions: specProtocolConformance.getSubstitutionMap()) else { return nil }
//
//              return swift.SILWitnessTable.MethodWitness(
//                Requirement: $0.bridged.entry.getMethodWitness().pointee.Requirement,
//                Witness: spec.bridged.getFunction())
//            }
//            .map(swift.SILWitnessTable.Entry.init)
//            .map(WitnessTable.Entry.init)

          var newEntries = [WitnessTable.Entry]()
          for entry in foundTable.entries {
            guard entry.kind == .Method,
                  let fn = entry.methodFunction else { continue }
            guard let spec = moduleContext.specialize(function: fn, withSubstitutions: specProtocolConformance.getSubstitutionMap()) else { continue }
            
            let newWitnessFn = swift.SILWitnessTable.MethodWitness(
                Requirement: entry.bridged.entry.getMethodWitness().pointee.Requirement,
                Witness: spec.bridged.getFunction())
            let newWitness = swift.SILWitnessTable.Entry(newWitnessFn)
            newEntries.append(WitnessTable.Entry(newWitness))
          }

          WitnessTable.create(moduleContext._bridged, specProtocolConformance, newEntries)
        }
      }
    }
  }
}
