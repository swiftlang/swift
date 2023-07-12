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
          guard let spec = conformance.getConcrete().getAsSpecializedProtocolConformance() else { continue }
          guard let foundTable = moduleContext.lookUpWitnessTable(spec) else { continue }
          for entry in foundTable.entries.compactMap { $0.methodFunction } {
            moduleContext.specialize(function: entry, withSubstitutions: spec.getSubstitutionMap())
          }
        }
      }
    }
  }
}
