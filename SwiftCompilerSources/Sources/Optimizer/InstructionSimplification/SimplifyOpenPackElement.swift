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
        let _ = dpi.operands.first?.value as? IntegerLiteralInst // when this is commented, // replacementTypes is not correctly filled
    {
      var worklist = ValueWorklist(context)

      worklist.pushIfNotVisited(self)
      let packElementTypes = dpi.indexedPackType.packElements

      var replacementTypes = [AST.Type]()
      for type in packElementTypes {
          replacementTypes.append(type.canonical.rawType)
      }

      let substitutionMap = SubstitutionMap(
        genericSignature: genericSignature,
        replacementTypes: replacementTypes)

      var cloner = TypeSubstitutionCloner(cloneBefore: self,
      substitutions: substitutionMap, context)
      defer { cloner.deinitialize() }

      // FIXME think about prefix of non-expansion element types
      // FIXME think about terminator instructions and successor BBs (TermInst)
      while let v = worklist.pop() {
        for use in v.uses {
          let inst = use.instruction
          // %0
          // %t = open_pack_element
          // apply %0(%1, %2)           // type-def: t
          for op in inst.operands {
              if
                // FIXME should probably handle multiple value instructions here
                  let svi = op.value.definingInstructionOrTerminator as? SingleValueInstruction {
                    if !worklist.hasBeenPushed(svi)  && !cloner.isCloned(value: op.value){
                        cloner.recordFoldedValue(op.value, mappedTo: op.value)
                    }
                  }
            else {
              cloner.cloneRecursively(value: op.value)
            }
          }
          // update insertion point
          _ = cloner.cloneRecursively(inst: inst)
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
