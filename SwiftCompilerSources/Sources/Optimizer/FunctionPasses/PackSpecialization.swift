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

import AST
import SIL

/// We can perform this optimisation iff all dynamic_pack_index instructions have a constant argument (probably integer_literal)
let packSpecialization = FunctionPass(name: "pack-specialization") {
  (function: Function, context: FunctionPassContext) in

  for inst in function.instructions {
    // TODO: FullApplyInst, ApplySite etc?
    guard let apply = inst as? ApplyInst,
      // Only support closures which, after generic specialization, are not generic any more.
      !apply.substitutionMap.replacementTypes.contains(where: { $0.hasArchetype }),
      let optimized = optimizedCallee(at: apply, in: context)
    else {
      continue
    }

    specializeCallsite(at: apply, with: optimized, context)
  }

}

private func specializeCallsite(
  at apply: ApplyInst, with optimized: PackExplodedFunction, _ context: FunctionPassContext
) {
  let builder = Builder(before: apply, context)

  // Populate the arguments list with the appropriate set of indirect results and parameters
  var newArguments = [any Value]()
  var parameterBorrows = [LoadBorrowInst]()

  var packArgumentIndices = [Int: [ScalarPackIndexInst]]()
  for (idx, argument) in apply.arguments.enumerated() {
    if argument.type.isPack {
      var indices = [ScalarPackIndexInst]()
      let packType = optimized.packASTTypes[idx]!
      for elementIdx in argument.type.packElements.indices {
        indices.append(
          builder.createScalarPackIndex(componentIndex: elementIdx, indexedPackType: packType))
      }
      packArgumentIndices[idx] = indices
    }
  }

  var resultIdx = 0
  var parameterIdx = 0
  for (originalArgumentIdx, originalArgument) in apply.arguments.enumerated() {
    if let originalPack = originalArgument as? AllocPackInst {
      // This code assumes the pack elements are all addresses
      assert(originalPack.type.isPackElementAddress)

      let packIndices = packArgumentIndices[originalArgumentIdx]!
      if resultIdx < optimized.resultMap.count {
        let (mappedResultIdx, resultInfos) = optimized.resultMap[resultIdx]
        assert(mappedResultIdx == originalArgumentIdx)

        for (packElementIdx, (resultInfo, packIdx)) in zip(resultInfos, packIndices).enumerated()
        where resultInfo.isSILIndirect {
          let indirectResultAddress = builder.createPackElementGet(
            packIndex: packIdx, pack: originalPack,
            elementType: originalPack.type.packElements[packElementIdx])
          newArguments.append(indirectResultAddress)
        }
        resultIdx += 1
      } else {
        assert(
          parameterIdx < optimized.parameterMap.count,
          "Application takes more parameter pack arguments than were exploded.")

        let (mappedParameterIdx, parameterInfos) = optimized.parameterMap[parameterIdx]
        assert(mappedParameterIdx == originalArgumentIdx)

        for (packElementIdx, (parameterInfo, packIdx)) in zip(parameterInfos, packIndices)
          .enumerated()
        {
          let indirectParameterAddress = builder.createPackElementGet(
            packIndex: packIdx, pack: originalPack,
            elementType: originalPack.type.packElements[packElementIdx])

          assert(
            parameterInfo.convention != .packOut && parameterInfo.convention != .packInout
              && parameterInfo.convention != .packOwned
              && parameterInfo.convention != .packGuaranteed
              && parameterInfo.convention != .indirectOut)
          switch parameterInfo.convention {
          case .directUnowned:
            newArguments.append(
              builder.createLoad(
                fromAddress: indirectParameterAddress,
                ownership: loadOwnership(for: indirectParameterAddress, normal: .trivial)))
          case .directGuaranteed:
            let borrow = builder.createLoadBorrow(fromAddress: indirectParameterAddress)
            newArguments.append(borrow)
            parameterBorrows.append(borrow)
          case .directOwned:
            newArguments.append(
              builder.createLoad(
                fromAddress: indirectParameterAddress,
                ownership: loadOwnership(for: indirectParameterAddress, normal: .take)))
          case .indirectIn, .indirectInCXX, .indirectInGuaranteed, .indirectInout,
            .indirectInoutAliasable:
            newArguments.append(indirectParameterAddress)
          default:
            assert(false, "Unsupported exploded parameter pack element convention.")
          }
        }

        parameterIdx += 1
      }

    } else {
      newArguments.append(originalArgument)
    }
  }

  // Emit a call to the optimized function
  let optimizedFunctionRef = builder.createFunctionRef(optimized.function)
  let optimizedApply = builder.createApply(
    function: optimizedFunctionRef, SubstitutionMap(), arguments: newArguments)

  // Release any borrows of @guaranteed parameters
  for borrow in parameterBorrows {
    builder.createEndBorrow(of: borrow)
  }

  let writeBackPackElement = { (value: any Value, argumentIdx: Int, elementIdx: Int) in
    let originalPackArgument = apply.arguments[argumentIdx]
    let packIdx = packArgumentIndices[argumentIdx]![elementIdx]
    let outputResultAddress = builder.createPackElementGet(
      packIndex: packIdx, pack: originalPackArgument,
      elementType: originalPackArgument.type.packElements[elementIdx])
    builder.createStore(
      source: value, destination: outputResultAddress,
      // The callee is responsible for initializing return pack elements, so preserve this expectation
      ownership: storeOwnership(for: value, normal: .initialize))
  }

  // Map the exploded direct results of the optimized function back to the corresponding indirect pack results of the original function.
  if !optimizedApply.type.isTuple {
    // Handle the single return value case
    if !apply.type.isTuple {
      // Both return a single value directly: just replace it
      apply.uses.replaceAll(with: optimizedApply, context)
    } else {
      // The original should have no more arguments than the optimized function (which has 1), and
      // in this case it also doesn't have exactly 1. It must have none.
      assert(
        apply.type.isVoid,
        "Pack specialization has fewer direct return values than original function.")
      // Write back the single pack element
      outerLoop: for (packArgumentIdx, mappedResultInfos) in optimized.resultMap {
        for (packElementIdx, resultInfo) in mappedResultInfos.enumerated() {
          if !resultInfo.isSILIndirect {
            writeBackPackElement(optimizedApply, packArgumentIdx, packElementIdx)
            break outerLoop
          }
        }
      }
    }

  } else if optimizedApply.type.tupleElements.isEmpty {
    // Handle both return types being void. To ensure it is safe to erase the original apply,
    // substitute all uses, even though there's no reason why there should be any.
    assert(apply.type.isVoid)
    apply.uses.replaceAll(with: optimizedApply, context)

  } else {
    // Handle a tuple with multiple elements. Since there could be many uses of the original apply's
    // result tuple, we construct a new one with the corresponding values from optimizedApply.

    var substitutedResultTupleElements = [any Value]()
    var resultMapIterator = optimized.resultMap.makeIterator()
    var optimizedDirectResultIdx = 0

    let destructuredTuple = builder.createDestructureTuple(tuple: optimizedApply)
    var destructuredTupleElements = destructuredTuple.results.makeIterator()
    for (originalResultIdx, resultInfo) in apply.functionConvention.results.enumerated() {
      // We only need to handle direct and pack results, since indirect results are handled above
      if !resultInfo.isSILIndirect {
        // Direct results of the original function are mapped to direct results of the optimized function.
        substitutedResultTupleElements.append(destructuredTupleElements.next()!)
        optimizedDirectResultIdx += 1

      } else if resultInfo.isConcretePack() {
        // Some elements of pack results may get mapped to direct results of the optimized function.
        let (mappedOriginalResultIdx, mappedResults) = resultMapIterator.next()!
        assert(originalResultIdx == mappedOriginalResultIdx)

        for (packElementIdx, mappedDirectResult) in mappedResults.enumerated()
        where !mappedDirectResult.isSILIndirect {
          writeBackPackElement(
            destructuredTupleElements.next()!,
            originalResultIdx, packElementIdx
          )
          optimizedDirectResultIdx += 1
        }
      }
    }

    // If exactly one direct result was mapped, don't generate a tuple
    if substitutedResultTupleElements.count == 1 {
      apply.uses.replaceAll(with: substitutedResultTupleElements[0], context)
    } else {
      let substitutedResultTuple = builder.createTuple(
        type: apply.type, elements: substitutedResultTupleElements)
      apply.uses.replaceAll(with: substitutedResultTuple, context)
    }
  }

  apply.parentBlock.erase(instruction: apply, context)
}

private func optimizedCallee(at apply: ApplySite, in context: FunctionPassContext)
  -> PackExplodedFunction?
{
  // Only perform pack specialization when the callee has pack arguments
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
  public let packASTTypes: [Int: CanonicalType]

  init(at apply: ApplySite, in context: FunctionPassContext) {

    let callee = apply.referencedFunction!
    self.original = callee

    var packASTTypes = [Int: CanonicalType]()
    for (i, argument) in original.arguments.enumerated() where argument.type.isPack {
      packASTTypes[i] = argument.type.approximateFormalPackType
    }
    self.packASTTypes = packASTTypes

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
      function = existingFunction
    } else {

      function = context.createSpecializedFunctionDeclaration(
        from: callee,
        withName: specializedName,
        withParams: newParams,
        withResults: newResults)

      self.buildOptimizedClosure(apply: apply, context)
    }
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
            } else {
              forwardedValues = [any Value](
                builder.createDestructureTuple(tuple: returnedValue).results)
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

      return (localPack, packType)
    }

    var argumentMap = [Int: ArgumentMapping]()

    // Explode parameters
    for (idx, parameterInfos) in parameterMap.reversed() {

      let (localPack, packType) = prepareExplosion(idx)

      var mappings = [ExplodedArgument]()
      for (i, (type, parameterInfo)) in zip(packType.packElements, parameterInfos).enumerated() {
        let packIdx = builder.createScalarPackIndex(
          componentIndex: i, indexedPackType: self.packASTTypes[idx]!)
        let argument = entryBlock.insertFunctionArgument(
          atPosition: idx + i, type: type,
          ownership: opt.getValueOwnership(type: type, convention: parameterInfo.convention),
          specContext)

        let allocStack: AllocStackInst?
        if parameterInfo.isSILIndirect {
          builder.createPackElementSet(elementValue: argument, packIndex: packIdx, pack: localPack)
          allocStack = nil
        } else {
          let alloc = builder.createAllocStack(type)
          // We need to copy @guaranteed arguments if we put them on the stack, since we don't own them.
          // TODO: Can this copy be elided?
          let ownedValue: any Value
          if parameterInfo.convention == .directGuaranteed {
            ownedValue = builder.createCopyValue(operand: argument)
          } else {
            ownedValue = argument
          }

          let ownership = storeOwnership(for: argument, normal: .initialize)
          builder.createStore(
            source: ownedValue, destination: alloc,
            ownership: ownership)
          builder.createPackElementSet(elementValue: alloc, packIndex: packIdx, pack: localPack)
          allocStack = alloc
        }

        mappings.append(ExplodedArgument(packIdx: packIdx, allocStack: allocStack))

      }
      argumentMap[idx] = ArgumentMapping(allocPack: localPack, arguments: mappings)
    }

    // Explode results
    for (idx, resultInfos) in resultMap.reversed() {

      let (localPack, packType) = prepareExplosion(idx)

      var mappings = [ExplodedArgument]()
      var insertArgumentPosition = idx
      for (i, (type, resultInfo)) in zip(packType.packElements, resultInfos).enumerated() {
        let packIdx = builder.createScalarPackIndex(
          componentIndex: i, indexedPackType: self.packASTTypes[idx]!)

        let allocStack: AllocStackInst?
        if resultInfo.isSILIndirect {
          let argument = entryBlock.insertFunctionArgument(
            atPosition: insertArgumentPosition, type: type,
            ownership: opt.getValueOwnership(
              type: type, convention: ArgumentConvention(result: resultInfo.convention)),
            specContext)
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
          let allocStack = argument.allocStack!
          returnValues.append(
            builder.createLoad(
              fromAddress: allocStack,
              ownership: loadOwnership(for: allocStack, normal: .take))
          )
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

private func storeOwnership(for value: any Value, normal: StoreInst.StoreOwnership)
  -> StoreInst.StoreOwnership
{
  let function = value.parentFunction
  if !function.hasOwnership {
    return .unqualified
  }

  if value.type.isTrivial(in: function) {
    return .trivial
  } else {
    return normal
  }
}

private func loadOwnership(for value: any Value, normal: LoadInst.LoadOwnership)
  -> LoadInst.LoadOwnership
{
  let function = value.parentFunction
  if !function.hasOwnership {
    return .unqualified
  }

  if value.type.isTrivial(in: function) {
    return .trivial
  } else {
    return .take
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
