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
    // TODO: FullApplyInst etc?
    guard let apply = inst as? ApplySite,
      // Only support closures which, after generic specialization, are not generic any more.
      !apply.substitutionMap.replacementTypes.contains(where: { $0.hasArchetype }),
      let optimized = optimizedCallee(at: apply, in: context)
    else {
      continue
    }

    print(optimized)
    // TODO
    // specializeCallsite(at: apply, with: optimized, in: context)
  }

}

private func optimizedCallee(at apply: ApplySite, in context: FunctionPassContext)
  -> PackExplodedFunction?
{
  guard let callee = apply.referencedFunction,
    callee.isDefinition,
    callee.isSpecialization,
    callee.argumentTypes.contains(where: { $0.isPack })
  else {
    return nil
  }

  let exploded = PackExplodedFunction(at: apply, in: context)

  return exploded
}

private struct PackExplodedFunction {
  public let original: Function
  public let function: Function
  public let resultMap: [(Int, [ResultInfo])]
  public let parameterMap: [(Int, [ParameterInfo])]

  init(at apply: ApplySite, in context: FunctionPassContext) {

    let callee = apply.referencedFunction!
    self.original = callee

    var newParams = [ParameterInfo]()
    var parameterMap = [(Int, [ParameterInfo])]()

    for arg in callee.parameters {
      if arg.isConcretePack() {
        let argParameterInfo = apply.parameter(for: apply.argumentOperands[arg.index])!

        let mappedParameterInfos = arg.type.packElements.map { elem in

          // TODO: Determine correct values for options and hasLoweredAddress
          ParameterInfo(
            type: elem.canonicalType,
            convention: explodedPackElementArgumentConvention(pack: arg, elem: elem),
            options: argParameterInfo.options,
            hasLoweredAddresses: argParameterInfo.hasLoweredAddresses)

        }

        parameterMap.append((arg.index, mappedParameterInfos))
        newParams += mappedParameterInfos

      } else {
        let param = apply.parameter(for: apply.argumentOperands[arg.index])!
        newParams.append(param)
      }
    }

    self.parameterMap = parameterMap

    var newResults = [ResultInfo]()
    var resultMap = [(Int, [ResultInfo])]()

    var indirectResultIdx = 0
    for resultInfo in callee.convention.results {
      print(resultInfo)
      if resultInfo.isConcretePack() {
        let mappedResultInfos = resultInfo.type.silType!.packElements.map { elem in
          ResultInfo(
            type: elem.canonicalType,
            convention: explodedPackElementResultConvention(in: callee, elem: elem),
            options: resultInfo.options,
            hasLoweredAddresses: resultInfo.hasLoweredAddresses)
        }

        resultMap.append((indirectResultIdx, mappedResultInfos))
        newResults += mappedResultInfos
      } else {
        newResults.append(resultInfo)
      }

      if resultInfo.isSILIndirect {
        indirectResultIdx += 1
      }
    }
    // We should have walked through all the indirect results, and no further.
    assert(indirectResultIdx == callee.argumentConventions.firstParameterIndex)

    self.resultMap = resultMap

    let packArgIndices: [Int] = callee.arguments.filter { $0.isConcretePack() }.map { $0.index }
    let specializedName = context.mangle(withExplodedPackArguments: packArgIndices, from: callee)
    if let existingFunction = context.lookupFunction(name: specializedName) {
      print("Reusing existing specialized definition of ", specializedName)
      function = existingFunction
    } else {

      function = context.createSpecializedFunctionDeclaration(
        from: callee,
        withName: specializedName,
        withParams: newParams,
        withResults: newResults)

      self.buildOptimizedClosure(apply: apply, context)
    }

    print(packArgIndices, [Int](callee.arguments.filter { $0.isIndirectResult }.map { $0.index }))
    print("specializing:", callee.name, "\n\tto", specializedName)
    print(newParams)
    print(function)

  }

  private struct ExplodedArgument {
    public let packIdx: ScalarPackIndexInst
    // A reference to an indirect function argument that the final value must be written back to, if necessary
    public let output: FunctionArgument?
  }

  private struct ArgumentMapping {
    public let allocPack: AllocPackInst
    public let arguments: [ExplodedArgument]
  }

  private struct ArgumentMap {
    public let map: [(Int, ArgumentMapping)]
  }

  /// Build the body of the pack-specialized function
  private func buildOptimizedClosure(apply: ApplySite, _ context: FunctionPassContext) {

    context.buildSpecializedFunction(specializedFunction: function) { (opt, specContext) in
      // Callee is a specialized function, so we don't need to specialize it again.
      cloneFunction(from: original, toEmpty: opt, specContext)

      let argumentMap = explodePackArguments(from: original, to: opt, specContext)
      print(argumentMap)
    }

    context.notifyNewFunction(function: function, derivedFrom: original)
  }

  /// Explode the types of the entry block's pack arguments, and track the correspondence between exploded arguments and local packs
  private func explodePackArguments(
    from original: Function, to opt: Function, _ specContext: FunctionPassContext
  ) -> ArgumentMap {

    let entryBlock = opt.entryBlock

    let builder = Builder(atBeginOf: entryBlock, specContext)

    var argumentMap = [(Int, ArgumentMapping)]()

    let explodePackArgument = { (idx: Int, packElementBecomesArgument: (Int) -> Bool) in
      let originalPackArg = entryBlock.arguments[idx]
      let packType = originalPackArg.type.objectType
      let astPackType = packType.approximateFormalPackType

      let localPack = builder.createAllocPack(packType)
      originalPackArg.uses.replaceAll(with: localPack, specContext)
      entryBlock.eraseArgument(at: idx, specContext)

      let ownership = originalPackArg.ownership

      var mappings = [ExplodedArgument]()
      var insertArgumentPosition = idx
      for (i, type) in packType.packElements.enumerated() {
        let packIdx = builder.createScalarPackIndex(
          componentIndex: i, indexedPackType: astPackType)

        let output: FunctionArgument?
        if packElementBecomesArgument(i) {
          let arg = entryBlock.insertFunctionArgument(
            atPosition: insertArgumentPosition, type: type, ownership: ownership, specContext)
          insertArgumentPosition += 1
          _ = builder.createPackElementSet(elementValue: arg, packIndex: packIdx, pack: localPack)
          output = arg
        } else {
          output = nil
        }

        mappings.append(ExplodedArgument(packIdx: packIdx, output: output))
      }
      argumentMap.append((idx, ArgumentMapping(allocPack: localPack, arguments: mappings)))
    }

    for (key, _) in parameterMap.reversed() {
      explodePackArgument(key, { _ in true })
    }

    for (key, resultInfos) in resultMap.reversed() {
      explodePackArgument(key, { i in resultInfos[i].isSILIndirect })
    }

    return ArgumentMap(map: argumentMap)
  }

  private func unrollPackLoops(in opt: Function, specContext: FunctionPassContext) {

    for loop in specContext.loopTree.loops {
      // Match the specific shape of a dynamic pack loop we can unroll.
      // Since these loops only get generated in one place, they have a highly
      // predictable structure.
      // The loop body itself should be a single basic block, so unrolling is straightforward.
      let header = loop.header

      guard loop.hasSingleExitBlock,
        loop.innerLoops.isEmpty,
        let exit = loop.exitBlocks.first,
        let branch = header.instructions.last as? CondBranchInst,
        let conditionInst = branch.condition.definingInstruction as? BuiltinInst,
        conditionInst.id == .ICMP_EQ,
        let packLengthInst = conditionInst.arguments.compactMap({ $0 as? PackLengthInst }).first,
        !packLengthInst.packType.containsPackExpansionType
      else {
        continue
      }

      var cloner = Cloner(cloneBefore: loop.preheader!.terminator, specContext)
      defer { cloner.deinitialize() }
      let iterations = packLengthInst.packType.numPackElements
    }
  }
}

private func explodedPackElementArgumentConvention(pack: FunctionArgument, elem: Type)
  -> ArgumentConvention
{
  precondition(
    pack.convention == .packGuaranteed
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
      preconditionFailure()
    }
  } else {
    switch pack.convention {
    case .packGuaranteed:
      return .indirectInGuaranteed
    case .packOwned:
      return .indirectIn  // TODO: Not quite right?
    case .packInout:
      return .indirectInout
    case .packOut:
      return .indirectOut
    default:
      preconditionFailure()
    }
  }
}

private func explodedPackElementResultConvention(in function: Function, elem: Type)
  -> ResultConvention
{
  // If the pack element type is loadable, then we can return it directly
  if elem.isTrivial(in: function) {
    return .unowned  // TODO: Verify this does the right thing before enabling
  } else if elem.isLoadable(in: function) {
    return .owned
  } else {
    return .indirect
  }
}

extension FunctionArgument {
  fileprivate func isConcretePack() -> Bool {
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

extension ResultInfo {
  fileprivate func isConcretePack() -> Bool {
    switch self.convention {
    case .pack:
      return !self.type.hasTypeParameter
    default:
      return false
    }
  }
}
