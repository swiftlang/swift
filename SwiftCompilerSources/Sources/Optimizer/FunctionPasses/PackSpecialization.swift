//===--- PackSpecialization.swift - specialize uses of parameter packs -------------===//
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

import SIL

/// We can perform this optimisation iff all dynamic_pack_index instructions have a constant argument (probably integer_literal)
let packSpecialization = FunctionPass(name: "pack-specialization") {
  (function: Function, context: FunctionPassContext) in


  for inst in function.instructions {
    guard let apply = inst as? FullApplySite,
          // Only support closures which, after generic specialization, are not generic any more.
          !apply.substitutionMap.replacementTypes.contains(where: { $0.hasArchetype }),
          let callee = apply.referencedFunction,
          callee.isDefinition,
          callee.isSpecialization,
          callee.argumentTypes.contains(where: { $0.isPack }) else { continue }

    let f = createSpecializedDeclaration(for: callee, at: apply, in: context)
  }

}

private func createSpecializedDeclaration(for callee: Function, at apply: FullApplySite, in context: FunctionPassContext) -> Function {

    let packArgs: [Int] = callee.parameters.filter { $0.isConcretePack() }.map { $0.index }
    let specializedName = context.mangle(withExplodedPackArguments: packArgs, from: callee)
    if let specialized = context.lookupFunction(name: specializedName) {
      print("Reusing existing specialized definition of ", specializedName)
      return specialized
    }

    var newParams = [ParameterInfo]()

    for arg in callee.parameters {
      if arg.isConcretePack() {
        let argParameterInfo = apply.parameter(for: apply.argumentOperands[arg.index])!
        for elem in arg.type.packElements {
          // TODO: Determine correct values for options and hasLoweredAddress
          let param = ParameterInfo(type: elem.canonicalType,
                                    convention: explodedPackElementArgumentConvention(pack: arg, elem: elem),
                                    options: argParameterInfo.options,
                                    hasLoweredAddresses: argParameterInfo.hasLoweredAddresses)
          newParams.append(param)
        }

      } else {
        let param = apply.parameter(for: apply.argumentOperands[arg.index])!
        newParams.append(param)
      }
    }

    var newResults = [ResultInfo]()
    for i in 0..<callee.argumentConventions.firstParameterIndex {
      let result = callee.argument(at: i)
      let resultInfo = callee.argumentConventions[result: i]!
      print(resultInfo)
      if resultInfo.isConcretePack() {
        for elem in result.type.packElements {
          let elemResultInfo = ResultInfo(type: elem.canonicalType, convention: explodedPackElementResultConvention(in: callee, elem: elem),
                                          options: resultInfo.options,
                                          hasLoweredAddresses: resultInfo.hasLoweredAddresses);
          newResults.append(elemResultInfo)
        }
      } else {
        newResults.append(resultInfo)
      }
    }

    let explodedDeclaration = context.createSpecializedFunctionDeclaration(from: callee,
                                                         withName: specializedName,
                                                         withParams: newParams,
                                                         withResults: newResults)

    print(packArgs, [Int](callee.arguments.filter { $0.isIndirectResult }.map { $0.index }))
    print("specializing:", callee.name, "\n\tto", specializedName)
    print(newParams)
    print(explodedDeclaration)

    return explodedDeclaration
}

private func explodedPackElementArgumentConvention(pack: FunctionArgument, elem: Type) -> ArgumentConvention {
  precondition(pack.convention == .packGuaranteed
                 || pack.convention == .packOwned
                 || pack.convention == .packInout
                 || pack.convention == .packOut)

  // If the pack element type is loadable, then we can pass it directly in the generated function.
  // TODO: Account for pack.type.isPackElementAddress with packOwned and packGuaranteed packs
  let trivial = elem.isTrivial(in: pack.parentFunction)
  let loadable = elem.isLoadable(in: pack.parentFunction)

  if loadable {
    switch pack.convention {
    case .packGuaranteed:
      // TODO: Is loadable && trivial the right situation in which to enable direct, unowned passing?
      return trivial ? .directUnowned : .directGuaranteed
    case .packOwned:
      return trivial ? .directUnowned : .directOwned
    case .packInout:
      return .indirectInout
    case .packOut:
      // For trivial output pack elements, we can return the value directly, but that isn't this function's responsibility
      return .indirectOut
    default:
      fatalError("Precondition violated")
    }
  } else {
    switch pack.convention {
    case .packGuaranteed:
      return .indirectInGuaranteed
    case .packOwned:
      return .indirectIn; // TODO: Not quite right?
    case .packInout:
      return .indirectInout
    case .packOut:
      return .indirectOut
    default:
      fatalError("Precondition violated")
    }
  }
}

private func explodedPackElementResultConvention(in function: Function, elem: Type) -> ResultConvention {
  // If the pack element type is loadable, then we can return it directly
  if elem.isTrivial(in: function) {
    return .unowned
  } else if elem.isLoadable(in: function) {
    return .owned
  } else {
    return .indirect
  }
}

private extension FunctionArgument {
  func isConcretePack() -> Bool {
    if !self.type.isPack {
      return false
    }

    switch self.convention {
    case .packGuaranteed, .packOut, .packInout, .packOwned:
      // TODO: Probably just check containsPackExpansionType
      return !(self.type.containsPackExpansionType || self.type.hasTypeParameter)
    default:
      return false
    }
  }
}

private extension ResultInfo {
  func isConcretePack() -> Bool {
    switch self.convention {
    case .pack:
      return !self.type.hasTypeParameter
    default:
      return false
    }
  }
}

// private extension FullApplySite {
//   func classifyArgumentsFor
// }
