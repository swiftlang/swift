//===--- PackSpecialization.swift - Eliminate packs in function arguments -===//
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

/// Splits pack arguments (parameters & indirect results) into separate
/// arguments for each member of the pack. The argument conventions of the new
/// parameters and results are determined based on whether their types are
/// trivial or loadable.
///
/// A new function is created, with a name generated using the exploded argument
/// mangling. This will be a modified version of the original, with local pack
/// allocations introduced as substitutes for the exploded pack arguments of the
/// original function. Call sites are also modified to extract the elements of
/// each pack argument that are passed to (or returned from) the new function
/// separately.
///
/// The changes this pass makes would ideally occur during generic
/// specialization. The two should be merged if the GenericSpecializer pass is
/// ever re-written in Swift.
///
/// Original:
///
/// sil [ossa] @double_up : $@convention(thin) (@pack_guaranteed Pack{Int, String}) -> (@pack_out Pack{Int, String}, @pack_out Pack{Int, String}) {
/// bb0(%0 : $*Pack{Int, String}, %1 : $*Pack{Int, String}, %2 : $*Pack{Int, String}):
///   debug_value %2, let, name "xs", argno 1, expr op_deref
///   ...
///
/// Modified:
///
/// sil shared [ossa] @double_upTf8xxx_n : $@convention(thin) (Int, @guaranteed String) -> (Int, @owned String, Int, @owned String) {
/// bb0(%0 : $Int, %1 : @guaranteed $String):
///   %2 = alloc_pack $Pack{Int, String}
///   %3 = scalar_pack_index 0 of $Pack{Int, String}
///   %4 = alloc_stack $Int
///   store %0 to [trivial] %4
///   pack_element_set %4 into %3 of %2
///   %7 = scalar_pack_index 1 of $Pack{Int, String}
///   %8 = alloc_stack $String
///   %9 = copy_value %1
///   store %9 to [init] %8
///   pack_element_set %8 into %7 of %2
///
///   ... etc. for indirect result arguments of original function (%0 and %1)
///
///   debug_value %2, let, name "xs", argno 1, expr op_deref
///   ...
///
let packSpecialization = FunctionPass(name: "pack-specialization") {
  (function: Function, context: FunctionPassContext) in

  // This pass requires both the caller and callee to be in OSSA.
  if !function.hasOwnership {
    return
  }

  for inst in function.instructions {
    // Only support full application for now.
    guard let apply = inst as? ApplyInst,
      // Only specialize calls to OSSA functions
      apply.referencedFunction?.hasOwnership ?? false,
      let specialized = specializeCallee(apply: apply, context: context)
    else {
      continue
    }

    let callSiteSpecializer = CallSiteSpecializer(
      apply: apply, specialized: specialized, context: context)
    callSiteSpecializer.specialize()
  }

}

/// A context in which to modify call sites during pack explosion
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
      if argument.type.isSILPack {
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

    for (index, resultOperand) in apply.indirectResultOperands.enumerated() {
      let argument = resultOperand.value
      if !argument.type.shouldExplode {
        newResults.append(argument)
        continue
      }

      // TODO: Support direct packs
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
    for (index, argument) in apply.arguments.enumerated().dropFirst(
      specialized.original.argumentConventions.firstParameterIndex)
    {

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
      function: specializedFunctionRef, apply.substitutionMap, arguments: newArguments)
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

        } else if resultInfo.type.silType!.shouldExplode {
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
    callee.argumentTypes.contains(where: { $0.shouldExplode })
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
    for (i, argument) in original.arguments.enumerated() where argument.type.shouldExplode {
      packASTTypes[i] = argument.type.approximateFormalPackType
    }
    self.packASTTypes = packASTTypes

    let (newParameters, parameterMap) = PackExplodedFunction.computeParameters(for: original)

    self.parameterMap = parameterMap

    let (newResults, resultMap) = PackExplodedFunction.computeResults(for: original)
    self.resultMap = resultMap

    let packArgIndices: [Int] = original.arguments.filter { $0.type.shouldExplode }.map { $0.index }
    let specializedName = context.mangle(withExplodedPackArguments: packArgIndices, from: original)
    if let existingFunction = context.lookupFunction(name: specializedName) {
      specialized = existingFunction
    } else {

      specialized = context.createSpecializedFunctionDeclaration(
        from: original,
        withName: specializedName,
        withParams: newParameters,
        withResults: newResults,
        makeThin: true)

      self.buildSpecializedFunction(apply: apply, context)
    }
  }

  private struct ExplodedArgument {
    public let packIdx: ScalarPackIndexInst
    /// A reference to the stack location holding the argument's value, if the argument/return value is direct
    public let allocStack: AllocStackInst?
    /// The borrowing reference to an @guaranteed argument
    public let storeBorrow: StoreBorrowInst?
  }

  private struct ArgumentMapping {
    public let allocPack: AllocPackInst
    public let arguments: [ExplodedArgument]
  }

  private struct ArgumentMap {
    public let map: [Int: ArgumentMapping]
  }

  /// Compute the parameter types for the pack-exploded version of a function,
  /// and the mapping between the original function's pack parameters, and the
  /// corresponding exploded parameters of the new function.
  fileprivate static func computeParameters(for function: Function) -> (
    [ParameterInfo], [(Int, [ParameterInfo])]
  ) {
    var newParams = [ParameterInfo]()
    var parameterMap = [(Int, [ParameterInfo])]()

    for (argument, parameterInfo) in zip(function.parameters, function.convention.parameters) {
      if argument.type.shouldExplode {

        let mappedParameterInfos = argument.type.packElements.map { elem in

          // TODO: Determine correct values for options and hasLoweredAddress
          ParameterInfo(
            type: elem.canonicalType,
            convention: explodedPackElementArgumentConvention(
              pack: parameterInfo, type: elem, in: function),
            options: parameterInfo.options,
            hasLoweredAddresses: parameterInfo.hasLoweredAddresses)

        }

        parameterMap.append((argument.index, mappedParameterInfos))
        newParams += mappedParameterInfos

      } else {
        // Leave the original argument unchanged
        newParams.append(parameterInfo)
      }
    }

    return (newParams, parameterMap)
  }

  /// Compute the result types for the pack-exploded version of a function, and
  /// the mapping between the original function's pack results, and the
  /// corresponding exploded results of the new function.
  private static func computeResults(for function: Function) -> (
    [ResultInfo], [(Int, [ResultInfo])]
  ) {
    var resultMap = [(Int, [ResultInfo])]()
    var newResults = [ResultInfo]()

    var indirectResultIdx = 0
    for resultInfo in function.convention.results {
      let silType = resultInfo.type.silType!
      if silType.shouldExplode {
        let mappedResultInfos = silType.packElements.map { elem in
          // TODO: Determine correct values for options and hasLoweredAddress
          ResultInfo(
            type: elem.canonicalType,
            convention: explodedPackElementResultConvention(in: function, type: elem),
            options: resultInfo.options,
            hasLoweredAddresses: resultInfo.hasLoweredAddresses)
        }

        resultMap.append((indirectResultIdx, mappedResultInfos))
        newResults += mappedResultInfos
      } else {
        // Leave the original result unchanged
        newResults.append(resultInfo)
      }

      if resultInfo.isSILIndirect {
        indirectResultIdx += 1
      }
    }

    // We should have walked through all the indirect results, and no further.
    assert(indirectResultIdx == function.argumentConventions.firstParameterIndex)

    return (newResults, resultMap)
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
      // Only emit deallocations when actually leaving the function context.
      for bb in specialized.blocks where bb.terminator.isFunctionExiting {
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
    let builder = Builder(atEndOf: bb, location: returnInst.location, specContext)

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
          ownership: Ownership(in: specialized, of: type, with: parameterInfo.convention),
          context)

        let allocStack: AllocStackInst?
        let storeBorrow: StoreBorrowInst?
        if parameterInfo.isSILIndirect {
          builder.createPackElementSet(elementValue: argument, packIndex: packIdx, pack: localPack)
          allocStack = nil
          storeBorrow = nil
        } else {
          let alloc = builder.createAllocStack(type)

          let address: any Value
          if parameterInfo.convention == .directGuaranteed {
            // We do not own @guaranteed arguments, so we must use store_borrow instead of store.
            let result = builder.createStoreBorrow(source: argument, destination: alloc)
            address = result
            storeBorrow = result
          } else {

            let ownership = storeOwnership(for: argument, normal: .initialize)
            builder.createStore(
              source: argument, destination: alloc,
              ownership: ownership)
            address = alloc
            storeBorrow = nil
          }
          builder.createPackElementSet(elementValue: address, packIndex: packIdx, pack: localPack)
          allocStack = alloc
        }

        mappings.append(
          ExplodedArgument(packIdx: packIdx, allocStack: allocStack, storeBorrow: storeBorrow))

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
            ownership: Ownership(
              in: specialized,
              of: type, with: ArgumentConvention(result: resultInfo.convention)),
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

        // Results have no initial value that could need to be borrowed.
        mappings.append(
          ExplodedArgument(packIdx: packIdx, allocStack: allocStack, storeBorrow: nil))

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
        if let storeBorrow = argument.storeBorrow {
          // Also release borrows of any @guaranteed parameters
          builder.createEndBorrow(of: storeBorrow)
        }
        if let allocStack = argument.allocStack {
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
    var returnBlockArgumentsIdx = 0
    for (i, originalResult) in self.original.convention.results.enumerated()
    where originalResult.type.silType!.shouldExplode || !originalResult.isSILIndirect {

      if !resultMap.indices.contains(resultMapIdx) || resultMap[resultMapIdx].0 != i {
        returnValues.append(returnBlockArguments[returnBlockArgumentsIdx])
        returnBlockArgumentsIdx += 1

      } else {

        let (originalPackArgumentIdx, _) = resultMap[resultMapIdx]

        let argumentMapping = argumentMap.map[originalPackArgumentIdx]!
        for argument in argumentMapping.arguments
        where argument.allocStack != nil {
          assert(
            argument.storeBorrow == nil,
            "Indirect results should not have an associated StoreBorrowInst.")

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

/// Compute the appropriate argument convention for an exploded member of the given pack parameter with the given type, in the given function
private func explodedPackElementArgumentConvention(
  pack: ParameterInfo, type: Type, in function: Function
)
  -> ArgumentConvention
{
  precondition(
    pack.convention == .packGuaranteed
      || pack.convention == .packOwned
      || pack.convention == .packInout)

  // If the pack element type is loadable, then we can pass it directly in the generated function.
  // TODO: Account for pack.type.isPackElementAddress with packOwned and packGuaranteed packs
  let trivial = type.isTrivial(in: function)
  let loadable = type.isLoadable(in: function)

  switch pack.convention {
  case .packGuaranteed:
    if trivial {
      return .directUnowned
    } else if loadable {
      return .directGuaranteed
    } else {
      return .indirectInGuaranteed
    }
  case .packOwned:
    if trivial {
      return .directUnowned
    } else if loadable {
      return .directOwned
    } else {
      return .indirectIn
    }
  case .packInout:
    return .indirectInout
  default:
    preconditionFailure()
  }
}

/// Compute the appropriate argument convention for an exploded member of the
/// given indirect result pack argument with the given result pack. For a
/// return, the convention has to be packOut, so we know its elements are passed
/// indirectly.
private func explodedPackElementResultConvention(in function: Function, type: Type)
  -> ResultConvention
{
  // If the pack element type is loadable, then we can return it directly
  if type.isTrivial(in: function) {
    return .unowned
  } else if type.isLoadable(in: function) {
    return .owned
  } else {
    return .indirect
  }
}

private func storeOwnership(for value: any Value, normal: StoreInst.StoreOwnership)
  -> StoreInst.StoreOwnership
{
  let function = value.parentFunction
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
  if value.type.isTrivial(in: function) {
    return .trivial
  } else {
    return .take
  }
}

extension Type {
  /// A pack argument can explode if it contains no pack expansion types
  fileprivate var shouldExplode: Bool {
    // For now, we only attempt to explode indirect packs, since these are the most common and inefficient.
    return isSILPack && !containsPackExpansionType && isPackElementAddress
  }
}
