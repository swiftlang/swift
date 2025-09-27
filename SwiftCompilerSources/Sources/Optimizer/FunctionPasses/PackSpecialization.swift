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
      let specialized = specializeCallee(apply: apply, context: context)
    else {
      continue
    }

    let callSiteSpecializer = CallSiteSpecializer(
      apply: apply, specialized: specialized, context: context)
    callSiteSpecializer.specialize()
  }

}

private class CallSiteSpecializer {
  let apply: ApplyInst
  let builder: Builder
  let specialized: PackExplodedFunction
  let context: FunctionPassContext
  let packArgumentIndices: [Int: [ScalarPackIndexInst]]

  init(apply: ApplyInst, specialized: PackExplodedFunction, context: FunctionPassContext) {
    self.apply = apply
    self.builder = Builder(before: apply, context)
    self.specialized = specialized
    self.context = context

    // Create scalar pack indices for each pack argument's elements
    var packArgumentIndices = [Int: [ScalarPackIndexInst]]()
    for (idx, argument) in apply.arguments.enumerated() {
      if argument.type.isPack {
        var indices = [ScalarPackIndexInst]()
        let packType = specialized.packASTTypes[idx]!
        for elementIdx in argument.type.packElements.indices {
          indices.append(
            builder.createScalarPackIndex(componentIndex: elementIdx, indexedPackType: packType))
        }
        packArgumentIndices[idx] = indices
      }
    }

    self.packArgumentIndices = packArgumentIndices
  }

  private func writeBackPackElement(value: any Value, argumentIndex: Int, elementIndex: Int) {
    let originalPackArgument = apply.arguments[argumentIndex]
    let packIndex = packArgumentIndices[argumentIndex]![elementIndex]
    let outputResultAddress = builder.createPackElementGet(
      packIndex: packIndex, pack: originalPackArgument,
      elementType: originalPackArgument.type.packElements[elementIndex])
    builder.createStore(
      source: value, destination: outputResultAddress,
      // The callee is responsible for initializing return pack elements
      ownership: storeOwnership(for: value, normal: .initialize))
  }

  // Collect values to pass as indirect result arguments to the new function from
  // the original call site.
  private func collectNewResultArguments() -> [any Value] {
    var resultIterator = specialized.resultMap.makeIterator()
    var newResults = [any Value]()

    for (index, argument) in apply.arguments[
      0..<apply.referencedFunction!.argumentConventions.firstParameterIndex
    ].enumerated() {
      // Do not modify non-packs, or packs that contain
      if !argument.type.shouldExplode {
        newResults.append(argument)
        continue
      }

      assert(
        argument.type.isPackElementAddress, "This code assumes the pack elements are all addresses")

      let packIndices = packArgumentIndices[index]!
      let (mappedResultIdx, resultInfos) = resultIterator.next()!
      assert(mappedResultIdx == index)

      for (packElementIdx, (resultInfo, packIdx)) in zip(resultInfos, packIndices).enumerated()
      where resultInfo.isSILIndirect {
        let indirectResultAddress = builder.createPackElementGet(
          packIndex: packIdx, pack: argument,
          elementType: argument.type.packElements[packElementIdx])
        newResults.append(indirectResultAddress)
      }

    }
    return newResults
  }

  // Collect values to pass as parameters to the new function from the original
  // call site. Also collect any borrows of parameters that need to be released
  // after the call.
  private func collectNewParameterArguments() -> ([any Value], [LoadBorrowInst]) {

    var parameterIterator = specialized.parameterMap.makeIterator()
    var newParameters = [any Value]()
    var parameterBorrows = [LoadBorrowInst]()
    for (index, argument) in apply.arguments[
      apply.referencedFunction!.argumentConventions.firstParameterIndex...
    ].enumerated() {

      if !argument.type.shouldExplode {
        newParameters.append(argument)
        continue
      }

      assert(
        argument.type.isPackElementAddress, "This code assumes the pack elements are all addresses")

      let packIndices = packArgumentIndices[index]!
      let (mappedParameterIdx, parameterInfos) = parameterIterator.next()!
      assert(mappedParameterIdx == index)

      for (packElementIdx, (parameterInfo, packIdx)) in zip(parameterInfos, packIndices)
        .enumerated()
      {
        let indirectParameterAddress = builder.createPackElementGet(
          packIndex: packIdx, pack: argument,
          elementType: argument.type.packElements[packElementIdx])

        assert(
          parameterInfo.convention != .packOut && parameterInfo.convention != .packInout
            && parameterInfo.convention != .packOwned
            && parameterInfo.convention != .packGuaranteed
            && parameterInfo.convention != .indirectOut)

        switch parameterInfo.convention {
        case .directUnowned:
          newParameters.append(
            builder.createLoad(
              fromAddress: indirectParameterAddress,
              ownership: loadOwnership(for: indirectParameterAddress, normal: .trivial)))
        case .directGuaranteed:
          let borrow = builder.createLoadBorrow(fromAddress: indirectParameterAddress)
          newParameters.append(borrow)
          parameterBorrows.append(borrow)
        case .directOwned:
          newParameters.append(
            builder.createLoad(
              fromAddress: indirectParameterAddress,
              ownership: loadOwnership(for: indirectParameterAddress, normal: .take)))
        case .indirectIn, .indirectInCXX, .indirectInGuaranteed, .indirectInout,
          .indirectInoutAliasable:
          newParameters.append(indirectParameterAddress)
        case .packOut, .packInout, .packOwned, .packGuaranteed, .indirectOut:
          assert(false, "Unsupported exploded parameter pack element convention.")
        }
      }

    }

    return (newParameters, parameterBorrows)
  }

  private func createSpecializedApply(newArguments: [any Value]) -> ApplyInst {
    // Emit a call to the specialized function
    let specializedFunctionRef = builder.createFunctionRef(specialized.specialized)
    let specializedApply = builder.createApply(
      function: specializedFunctionRef, SubstitutionMap(), arguments: newArguments)
    return specializedApply
  }

  /// Map the direct results of the specialized function back to the corresponding
  /// indirect pack or direct results of the original function.
  private func substituteNewDirectResults(specializedApply: ApplyInst) {
    if !specializedApply.type.isTuple {
      // Handle the single return value case
      if !apply.type.isTuple {
        // Both return a single value directly: just replace it
        apply.uses.replaceAll(with: specializedApply, context)
      } else {
        // The original should have no more arguments than the specialized function (which has 1), and
        // in this case it also doesn't have exactly 1. It must have none.
        assert(
          apply.type.isVoid,
          "Pack specialization has fewer direct return values than original function.")
        // Write back the single pack element
        outerLoop: for (packArgumentIdx, mappedResultInfos) in specialized.resultMap {
          for (packElementIdx, resultInfo) in mappedResultInfos.enumerated() {
            if !resultInfo.isSILIndirect {
              writeBackPackElement(
                value: specializedApply, argumentIndex: packArgumentIdx,
                elementIndex: packElementIdx)
              break outerLoop
            }
          }
        }
      }

    } else if specializedApply.type.tupleElements.isEmpty {
      // Handle both return types being void. To ensure it is safe to erase the original apply,
      // substitute all uses, even though there's no reason why there should be any.
      assert(apply.type.isVoid)
      apply.uses.replaceAll(with: specializedApply, context)

    } else {
      // Handle a tuple with multiple elements. Since there could be many uses of the original apply's
      // result tuple, we construct a new one with the corresponding values from specializedApply.

      var substitutedResultTupleElements = [any Value]()
      var resultMapIterator = specialized.resultMap.makeIterator()

      let destructuredTuple = builder.createDestructureTuple(tuple: specializedApply)
      var destructuredTupleElements = destructuredTuple.results.makeIterator()
      for (originalResultIdx, resultInfo) in apply.functionConvention.results.enumerated() {
        // We only need to handle direct and pack results, since indirect results are handled above
        if !resultInfo.isSILIndirect {
          // Direct results of the original function are mapped to direct results of the specialized function.
          substitutedResultTupleElements.append(destructuredTupleElements.next()!)

        } else if resultInfo.type.shouldExplode {
          // Some elements of pack results may get mapped to direct results of the specialized function.
          let (mappedOriginalResultIdx, mappedResults) = resultMapIterator.next()!
          assert(originalResultIdx == mappedOriginalResultIdx)

          for (packElementIdx, mappedDirectResult) in mappedResults.enumerated()
          where !mappedDirectResult.isSILIndirect {
            writeBackPackElement(
              value: destructuredTupleElements.next()!, argumentIndex: originalResultIdx,
              elementIndex: packElementIdx)
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
  }

  func specialize() {
    // Populate the arguments list with the appropriate set of indirect results and parameters
    let newResults = collectNewResultArguments()
    let (newParameters, parameterBorrows) = collectNewParameterArguments()

    let specializedApply = createSpecializedApply(newArguments: newResults + newParameters)

    // Release any borrows that were created to pass parameters
    for borrow in parameterBorrows {
      builder.createEndBorrow(of: borrow)
    }

    substituteNewDirectResults(specializedApply: specializedApply)

    context.erase(instruction: apply)
  }
}

private func specializeCallee(apply: ApplySite, context: FunctionPassContext)
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

  let exploded = PackExplodedFunction(at: apply, context)

  return exploded
}

/// A description of the mapping from an original function with pack arguments,
/// to a new one where those arguments have been exploded into separate
/// parameters and results.
private struct PackExplodedFunction {
  public let original: Function
  public let specialized: Function
  /// Each element stores the set of results of the new function corresponding
  /// to the pack argument of the original at the given index, in ascending
  /// order.
  public let resultMap: [(Int, [ResultInfo])]
  /// Each element stores the set of parameters of the new function
  /// corresponding to the pack argument of the original at the given
  /// index, in ascending order.
  public let parameterMap: [(Int, [ParameterInfo])]
  // Maps indices of pack arguments of the original function to its
  // `approximateFormalPackType` (a Canonical AST PackType).
  public let packASTTypes: [Int: CanonicalType]

  init(at apply: ApplySite, _ context: FunctionPassContext) {

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
      if arg.type.shouldExplode {
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
      if resultInfo.type.shouldExplode {
        let mappedResultInfos = resultInfo.type.silType!.packElements.map { elem in
          // TODO: Determine correct values for options and hasLoweredAddress
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

    let packArgIndices: [Int] = callee.arguments.filter { $0.type.shouldExplode }.map { $0.index }
    let specializedName = context.mangle(withExplodedPackArguments: packArgIndices, from: callee)
    if let existingFunction = context.lookupFunction(name: specializedName) {
      specialized = existingFunction
    } else {

      specialized = context.createSpecializedFunctionDeclaration(
        from: callee,
        withName: specializedName,
        withParams: newParams,
        withResults: newResults)

      self.buildSpecializedFunction(apply: apply, context)
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
  private func buildSpecializedFunction(apply: ApplySite, _ context: FunctionPassContext) {

    context.buildSpecializedFunction(specializedFunction: specialized) {
      (specialized, specContext) in
      // Callee is a specialized function, so we don't need to specialize it again.
      cloneFunction(from: original, toEmpty: specialized, specContext)

      let argumentMap = explodePackArguments(from: original, to: specialized, specContext)

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
            in: specialized, argumentMap: argumentMap,
            originalReturnTypes: originalReturnTupleElements,
            specContext)

          // Now modify all existing return blocks to instead branch to this return shim block
          for bb in specialized.blocks where (bb.terminator is ReturnInst) && bb != returnShimBB {
            redirect(returnBB: bb, to: returnShimBB, specContext)
          }
        }
      }
      for bb in specialized.blocks where bb.isReachableExitBlock {
        insertArgumentPackDeallocations(in: bb, argumentMap: argumentMap, specContext)
      }
    }

    context.notifyNewFunction(function: specialized, derivedFrom: original)
  }

  private func redirect(
    returnBB bb: BasicBlock, to returnShimBB: BasicBlock, _ specContext: FunctionPassContext
  ) {
    let returnInst = bb.terminator as! ReturnInst
    let returnedValue = returnInst.returnedValue
    let builder = Builder(before: returnInst, specContext)

    specContext.erase(instruction: returnInst)

    let forwardedValues: [any Value]
    if !returnedValue.type.isTuple {
      forwardedValues = [returnedValue]
    } else {
      forwardedValues = [any Value](
        builder.createDestructureTuple(tuple: returnedValue).results)
    }

    builder.createBranch(to: returnShimBB, arguments: forwardedValues)
  }

  /// Explode the types of the entry block's pack arguments, and track the correspondence between exploded arguments and local packs
  private func explodePackArguments(
    from original: Function, to specialized: Function, _ context: FunctionPassContext
  ) -> ArgumentMap {

    let entryBlock = specialized.entryBlock
    let builder = Builder(atBeginOf: entryBlock, context)

    let prepareExplosion = { idx in

      let originalPackArg = entryBlock.arguments[idx]
      let packType = originalPackArg.type.objectType

      let localPack = builder.createAllocPack(packType)
      originalPackArg.uses.replaceAll(with: localPack, context)

      entryBlock.eraseArgument(at: idx, context)

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
          ownership: specialized.getValueOwnership(
            type: type, convention: parameterInfo.convention),
          context)

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
            ownership: specialized.getValueOwnership(
              type: type, convention: ArgumentConvention(result: resultInfo.convention)),
            context)
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
    in specialized: Function, argumentMap: ArgumentMap, originalReturnTypes: [Type],
    _ context: FunctionPassContext
  ) -> BasicBlock {
    let bb = specialized.appendNewBlock(context)
    let builder = Builder(atEndOf: bb, location: specialized.location, context)

    let returnBlockArguments = originalReturnTypes.map {
      bb.addArgument(type: $0, ownership: .unowned, context)
    }

    var returnValues = [any Value]()

    // Thread together the original and exploded direct return values
    var resultMapIdx = 0
    var originalReturnTypesIdx = 0
    for (i, originalResult) in self.original.convention.results.enumerated()
    where originalResult.type.shouldExplode || !originalResult.isSILIndirect {

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
        in: specialized)
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

extension TypeProperties {
  /// A pack argument can explode if it contains no pack expansion types
  fileprivate var shouldExplode: Bool {
    return isPack && !containsPackExpansionType
  }
}
