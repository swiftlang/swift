//===--- SimplifyOpenPackElement.swift --------------------------------------===//
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

import AST
import SIL

extension OpenPackElementInst : Simplifiable, SILCombineSimplifiable {
  func simplify(_ context: SimplifyContext) {
    replacePackElementTypes(context)
  }
}

private extension OpenPackElementInst {
  func replacePackElementTypes(_ context: SimplifyContext){

    if let dpi = operands.first?.value as? DynamicPackIndexInst,
       let ili = dpi.operands.first?.value as? IntegerLiteralInst,
       let index = ili.value
    {
      var worklist = ValueWorklist(context)

      worklist.pushIfNotVisited(self)
      let concreteType = dpi.indexedPackType.packElementTypes[index].canonical

      let substitutionMap = SubstitutionMap(
        genericSignature: genericSignature,
        replacementTypes:
        [concreteType.rawType])

      var cloner = TypeSubstitutionCloner(cloneBefore: self,
      substitutions: substitutionMap, context)

      while let v = worklist.pop() {
        for use in v.uses {
          let inst = use.instruction
          // %0
          // %t = open_pack_element
          // apply %0(%1, %2)           // type-def: t
          let _ = cloner.clone(instruction: inst) // FIXME: what if it fails
          for result in inst.results {
            if
              result.type.isOrContainsPackType(parentFunction) {
              worklist.pushIfNotVisited(result)
            }
          }
        }
      }
    }
  }
}
