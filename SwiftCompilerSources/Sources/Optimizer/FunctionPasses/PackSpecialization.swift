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
    // A reference to the stack location holding the argument's value, if the argument/return value is direct
    public let allocStack: AllocStackInst?
  }

  private struct ArgumentMapping {
    public let allocPack: AllocPackInst
    public let arguments: [ExplodedArgument]
  }

  private struct ArgumentMap {
    public let map: [Int: ArgumentMapping]
  }

  /// Build the body of the pack-specialized function
  private func buildOptimizedClosure(apply: ApplySite, _ context: FunctionPassContext) {

    context.buildSpecializedFunction(specializedFunction: function) { (opt, specContext) in
      // Callee is a specialized function, so we don't need to specialize it again.
      cloneFunction(from: original, toEmpty: opt, specContext)

      let argumentMap = explodePackArguments(from: original, to: opt, specContext)

      // Build a block that extracts the necessary direct return values and returns them, if necessary.
      // This is only necessary if any pack result elements were mapped to direct results, and the
      // function contains at least one return statement.
      if resultMap.contains(where: { (_, resultInfos) in
        resultInfos.contains(where: { !$0.isSILIndirect })
      }) {
        if let originalReturnStatement = original.blocks.lazy.first(where: {
          $0.terminator is ReturnInst
        })?.terminator as? ReturnInst {
          let originalReturnType = originalReturnStatement.returnedValue.type

          let originalReturnTupleElements: [Type]
          if originalReturnType.isTuple {
            originalReturnTupleElements = [Type](originalReturnType.tupleElements)
          } else {
            originalReturnTupleElements = [originalReturnType]
          }
          let returnShimBB = addReturnShimBlock(
            in: opt, argumentMap: argumentMap, originalReturnTypes: originalReturnTupleElements,
            specContext)

          // Now modify all existing return blocks to instead branch to this return shim block
          for bb in opt.blocks where (bb.terminator is ReturnInst) && bb != returnShimBB {
            let returnInst = bb.terminator as! ReturnInst
            let returnedValue = returnInst.returnedValue
            bb.erase(instruction: returnInst, specContext)

            let builder = Builder(atEndOf: bb, location: opt.location, specContext)

            let forwardedValues: [any Value]
            if !returnedValue.type.isTuple {
              forwardedValues = [returnedValue]
            } else if let tupleInst = returnedValue.definingInstruction as? TupleInst {
              forwardedValues = [any Value](tupleInst.operands.values)
            } else {
              forwardedValues = returnedValue.type.tupleElements.indices.map { i in
                builder.createTupleExtract(tuple: returnedValue, elementIndex: i)
              }
            }

            builder.createBranch(to: returnShimBB, arguments: forwardedValues)
          }
        }
      }
      for bb in opt.blocks where bb.isReachableExitBlock {
        insertArgumentPackDeallocations(in: bb, argumentMap: argumentMap, specContext)
      }
    }

    context.notifyNewFunction(function: function, derivedFrom: original)
  }

  /// Explode the types of the entry block's pack arguments, and track the correspondence between exploded arguments and local packs
  private func explodePackArguments(
    from original: Function, to opt: Function, _ specContext: FunctionPassContext
  ) -> ArgumentMap {

    let entryBlock = opt.entryBlock
    let builder = Builder(atBeginOf: entryBlock, specContext)

    let prepareExplosion = { idx in

      let originalPackArg = entryBlock.arguments[idx]
      let packType = originalPackArg.type.objectType

      let localPack = builder.createAllocPack(packType)
      originalPackArg.uses.replaceAll(with: localPack, specContext)

      entryBlock.eraseArgument(at: idx, specContext)

      return (localPack, packType, originalPackArg.ownership)
    }

    var argumentMap = [Int: ArgumentMapping]()

    // Explode parameters
    for (idx, parameterInfos) in parameterMap.reversed() {

      let (localPack, packType, ownership) = prepareExplosion(idx)

      var mappings = [ExplodedArgument]()
      for (i, type) in packType.packElements.enumerated() {
        let packIdx = builder.createScalarPackIndex(
          componentIndex: i, indexedPackType: packType.approximateFormalPackType)
        let argument = entryBlock.insertFunctionArgument(
          atPosition: idx + i, type: type, ownership: ownership, specContext)

        let allocStack: AllocStackInst?
        if parameterInfos[i].isSILIndirect {
          builder.createPackElementSet(elementValue: argument, packIndex: packIdx, pack: localPack)
          allocStack = nil
        } else {
          let alloc = builder.createAllocStack(type)
          builder.createStore(source: argument, destination: alloc, ownership: .unqualified)
          builder.createPackElementSet(elementValue: alloc, packIndex: packIdx, pack: localPack)
          allocStack = alloc
        }

        mappings.append(ExplodedArgument(packIdx: packIdx, allocStack: allocStack))

      }
      argumentMap[idx] = ArgumentMapping(allocPack: localPack, arguments: mappings)
    }

    // Explode results
    for (idx, resultInfos) in resultMap.reversed() {

      let (localPack, packType, ownership) = prepareExplosion(idx)

      var mappings = [ExplodedArgument]()
      var insertArgumentPosition = idx
      for (i, type) in packType.packElements.enumerated() {
        let packIdx = builder.createScalarPackIndex(
          componentIndex: i, indexedPackType: packType.approximateFormalPackType)

        let allocStack: AllocStackInst?
        if resultInfos[i].isSILIndirect {
          let argument = entryBlock.insertFunctionArgument(
            atPosition: insertArgumentPosition, type: type, ownership: ownership, specContext)
          builder.createPackElementSet(elementValue: argument, packIndex: packIdx, pack: localPack)
          allocStack = nil
          insertArgumentPosition += 1
        } else {
          // TODO: This is an indirect result, so we rely on the existing code to initialize it?
          let alloc = builder.createAllocStack(type)
          builder.createPackElementSet(elementValue: alloc, packIndex: packIdx, pack: localPack)
          allocStack = alloc
        }

        mappings.append(ExplodedArgument(packIdx: packIdx, allocStack: allocStack))

      }
      argumentMap[idx] = ArgumentMapping(allocPack: localPack, arguments: mappings)

    }

    return ArgumentMap(map: argumentMap)
  }

  /// Insert dealloc_pack instructions for the alloc_pack's corresponding to the original function's pack arguments.
  private func insertArgumentPackDeallocations(
    in bb: BasicBlock, argumentMap: ArgumentMap, _ context: FunctionPassContext
  ) {

    let terminator = bb.terminator

    let builder = Builder(before: terminator, context)

    let createDeallocations = { (idx: Int) in
      let mapped = argumentMap.map[idx]!
      for argument in mapped.arguments.reversed() {
        if let allocStack = argument.allocStack {
          // TODO: We take from the stack allocations when preparing the return block, so we don't need to destroy their addresses?
          builder.createDestroyAddr(address: allocStack)
          builder.createDeallocStack(allocStack)
        }
      }
      builder.createDeallocPack(argumentMap.map[idx]!.allocPack)
    }

    // Emit dealloc_packs in the opposite order to the alloc_packs
    for (key, _) in resultMap {
      createDeallocations(key)
    }
    for (key, _) in parameterMap {
      createDeallocations(key)
    }
  }

  private func addReturnShimBlock(
    in opt: Function, argumentMap: ArgumentMap, originalReturnTypes: [Type],
    _ context: FunctionPassContext
  ) -> BasicBlock {
    let bb = opt.appendNewBlock(context)
    let builder = Builder(atEndOf: bb, location: opt.location, context)

    let returnBlockArguments = originalReturnTypes.map {
      bb.addArgument(type: $0, ownership: .unowned, context)
    }

    var returnValues = [any Value]()

    // Thread together the original and exploded direct return values
    var resultMapIdx = 0
    var originalReturnTypesIdx = 0
    for (i, originalResult) in self.original.convention.results.enumerated()
    where originalResult.isConcretePack() || !originalResult.isSILIndirect {

      if !resultMap.indices.contains(resultMapIdx) || resultMap[resultMapIdx].0 != i {
        returnValues.append(returnBlockArguments[originalReturnTypesIdx])
        originalReturnTypesIdx += 1

      } else {

        let (originalPackArgumentIdx, _) = resultMap[resultMapIdx]

        let argumentMapping = argumentMap.map[originalPackArgumentIdx]!
        for argument in argumentMapping.arguments
        where argument.allocStack != nil {
          returnValues.append(
            builder.createLoad(fromAddress: argument.allocStack!, ownership: .unqualified))
        }

        resultMapIdx += 1
      }
    }

    // Return the single value directly, rather than constructing a single-element tuple for it.
    if returnValues.count == 1 {
      builder.createReturn(of: returnValues[0])
    } else {
      let tupleType = context.getTupleType(elements: returnValues.map { $0.type }).loweredType(
        in: opt)
      let tuple = builder.createTuple(type: tupleType, elements: returnValues)
      builder.createReturn(of: tuple)
    }

    return bb
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
