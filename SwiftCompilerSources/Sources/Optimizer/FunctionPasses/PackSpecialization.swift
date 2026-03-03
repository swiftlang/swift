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
/// See below for SIL examples.
let packSpecialization = FunctionPass(name: "pack-specialization") {
  (function: Function, context: FunctionPassContext) in

  // This pass requires both the caller and callee to be in OSSA.
  if !function.hasOwnership {
    return
  }

  // TODO: Support pack specialization for any FullApplySite, not just ApplyInst.
  for case let apply as ApplyInst in function.instructions {
    // Only support normal apply instructions for now.
    guard let callee = apply.referencedFunction,
      // Only specialize calls to OSSA functions
      callee.hasOwnership,
      let specialized = specializeCallee(apply: apply, context: context)
    else {
      continue
    }

    let callSiteSpecializer = CallSiteSpecializer(
      apply: apply, specialized: specialized, context: context)
    callSiteSpecializer.specialize()
  }

}

//===----------------------------------------------------------------------===//
// Call Site Specialization
//===----------------------------------------------------------------------===//

/// A context in which to modify call sites during pack specialization.
private struct CallSiteSpecializer {
  let apply: ApplyInst
  let callee: PackExplodedFunction
  let context: FunctionPassContext

  /// Mapping from the indices of the indirect pack arguments of the original
  /// function to an array of scalar pack indices, used to access their elements
  typealias PackArgumentIndices = [Int: [ScalarPackIndexInst]]

  init(apply: ApplyInst, specialized: PackExplodedFunction, context: FunctionPassContext) {
    self.apply = apply
    self.callee = specialized
    self.context = context
  }

  /// Perform call-site specialization.
  func specialize() {
    let packArgumentIndices = self.createScalarPackIndices()

    // Populate the arguments list with the appropriate set of indirect results and parameters
    let results = self.collectNewIndirectResults(using: packArgumentIndices)
    let parameters = self.collectNewParameters(
      using: packArgumentIndices)

    // Emit a call to the specialized function
    let specializedApply = self.createSpecializedApply(arguments: results + parameters.arguments)

    self.substituteNewDirectResults(from: specializedApply, using: packArgumentIndices)

    // Both substituteNewDirectResults and createEndBorrows insert instructions
    // immediately after specializedApply. We create the end_borrows last to
    // minimise their borrow scopes.
    self.createEndBorrows(for: parameters.borrows, after: specializedApply)

    self.context.erase(instruction: self.apply)
  }

  /// Create indices to access elements of the pack arguments that will explode.
  private func createScalarPackIndices() -> PackArgumentIndices {
    let builder = Builder(before: self.apply, self.context)
    // Create scalar pack indices for each pack argument's elements
    var packArgumentIndices = PackArgumentIndices()
    for (idx, argument) in self.apply.arguments.enumerated()
        where argument.type.shouldExplode
    {
      var indices = [ScalarPackIndexInst]()
      let packType = self.callee.packASTTypes[idx]!
      for elementIdx in argument.type.packElements.indices {
        indices.append(
          builder.createScalarPackIndex(componentIndex: elementIdx, indexedPackType: packType))
      }
      packArgumentIndices[idx] = indices

    }

    return packArgumentIndices
  }

  /// Collect addresses to pass as indirect result arguments to the new function from
  /// the original call site.
  ///
  /// Indirect results must be processed before generating the specialized call,
  /// since they are passed as operands to the apply, while direct results must
  /// be processed after generating the specialized call, since they are
  /// extracted from its result value. See substituteNewDirectResults.
  ///
  /// Before: %fn : () -> (@pack_out Pack{C1, T2}, NP)
  ///
  /// %non_pack = apply %fn(%pack)
  ///
  /// After: %specialized_fn : () -> (@out C1, T2, NP)
  ///
  /// %c1_addr = pack_element_get 0 %pack
  /// (%non_pack, %t2) = apply %specialized_fn(%c1_addr)
  ///
  private func collectNewIndirectResults(using packArgumentIndices: PackArgumentIndices)
    -> [any Value]
  {
    let builder = Builder(before: self.apply, self.context)
    var resultIterator = self.callee.resultMap.makeIterator()
    var newResults = [any Value]()

    for (index, resultOperand) in self.apply.indirectResultOperands.enumerated() {
      let argument = resultOperand.value
      if !argument.type.shouldExplode {
        newResults.append(argument)
        continue
      }

      let packIndices = packArgumentIndices[index]!
      let mapped = resultIterator.next()!
      assert(
        mapped.argumentIndex == index,
        "Iteration over mapped and apply-site indirect result packs is misaligned.")

      for (packElementIdx, (resultInfo, packIdx)) in zip(mapped.expandedElements, packIndices).enumerated()
          where resultInfo.isSILIndirect
      {
        let indirectResultAddress = builder.createPackElementGet(
          packIndex: packIdx, pack: argument,
          elementType: argument.type.packElements[packElementIdx])
        newResults.append(indirectResultAddress)
      }

    }
    return newResults
  }

  /// Collect values to pass as parameters to the new function from the original
  /// call site. Also collect any borrows for @guaranteed parameters that need to
  /// be released after the call.
  ///
  /// Non-pack parameters, and pack elements that map to indirect parameters,
  /// are passed as addresses. Pack elements that map to direct parameters must
  /// be loaded, if the original pack's elements were stored indirectly
  /// (isPackElementAddress). We currently only attempt to eliminate indirect
  /// packs (see TypeProperties.shouldExplode below), so these loads are always
  /// generated.
  ///
  /// TODO: Update this pass to handle direct packs if they are ever generated
  /// often enough to have a performance impact.
  ///
  /// Before: %fn : (@pack_owned Pack{C1, T2}, NP, @pack_guaranteed Pack{C3, T4}) -> ()
  ///
  /// %result = apply %fn(%pack1, %non_pack, %pack2)
  ///
  /// After: %specialized_fn : (@in C1, @owned T2, NP, @in_guaranteed C3, @guaranteed T4) -> ()
  ///
  /// %arg1addr = pack_element_get 0 %pack1
  /// %arg2addr = pack_element_get 1 %pack1
  /// %arg2 = load %arg2addr                 - arg2 is passed @owned or unowned
  /// %arg3addr = pack_element_get 0 %pack2
  /// %arg4addr = pack_element_get 1 %pack2
  /// %arg4 = load_borrow %arg4addr          - arg4 is passed @guaranteed
  ///
  /// %new_result = apply %specialized_fn(%arg1addr, %arg2, %non_pack, %arg3addr, %arg4)
  ///
  private func collectNewParameters(using packArgumentIndices: PackArgumentIndices) -> (
    arguments: [any Value], borrows: [LoadBorrowInst]
  ) {
    let builder = Builder(before: self.apply, self.context)

    var parameterIterator = self.callee.parameterMap.makeIterator()
    var newParameters = [any Value]()
    var parameterBorrows = [LoadBorrowInst]()
    for (index, argument) in self.apply.arguments.enumerated().dropFirst(
      self.callee.original.argumentConventions.firstParameterIndex)
    {

      if !argument.type.shouldExplode {
        newParameters.append(argument)
        continue
      }

      let packIndices = packArgumentIndices[index]!
      let mapped = parameterIterator.next()!
      assert(
        mapped.argumentIndex == index,
        "Iteration over mapped and apply-site indirect parameter packs is misaligned.")

      for (packElementIdx, (parameterInfo, packIdx)) in zip(mapped.expandedElements, packIndices)
        .enumerated()
      {
        let indirectParameterAddress = builder.createPackElementGet(
          packIndex: packIdx, pack: argument,
          elementType: argument.type.packElements[packElementIdx])

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
          preconditionFailure("Unsupported exploded parameter pack element convention.")
        }
      }

    }

    return (newParameters, parameterBorrows)
  }

  /// Create an application of the specialized function to the supplied arguments.
  private func createSpecializedApply(arguments: [any Value]) -> ApplyInst {
    let builder = Builder(before: self.apply, self.context)

    // Emit a call to the specialized function
    let specializedFunctionRef = builder.createFunctionRef(self.callee.specialized)
    let specializedApply = builder.createApply(
      function: specializedFunctionRef, self.apply.substitutionMap, arguments: arguments)
    return specializedApply
  }

  /// Map the direct results of the specialized function back to the
  /// corresponding indirect pack or direct results of the original function.
  ///
  /// If an element of an indirect result pack was mapped to a direct return
  /// value, writeBack is used to write the value back to that original pack
  /// element.
  ///
  /// Also collect the direct return values that correspond to those of the
  /// original function, and use them to replace all uses of the original
  /// result.
  ///
  /// The stack allocations and accesses for results can then be easily
  /// eliminated by other passes.
  private func substituteNewDirectResults(
    from specializedApply: ApplyInst, using packArgumentIndices: PackArgumentIndices
  ) {
    if specializedApply.type == self.apply.type {
      // No direct return values were added: we just need to replace the original result:
      //
      // Before: %fn : (...) -> Int
      // %1 = apply %fn(...)
      //
      // After: %specialized_fn : (...) -> Int
      // %1 = apply %specialized_fn(...)

      self.apply.uses.replaceAll(with: specializedApply, self.context)

    } else if !specializedApply.type.isTuple {
      assert(
        self.apply.type.isVoid,
        "Pack specialized function has fewer direct return values than original function.")

      // Write back the single direct result:
      //
      // Before: %fn : (...) -> @pack_out Pack{Int}
      // %1 = apply %fn(%pack_addr, ...)
      //
      // After: %specialized_fn : (...) -> Int
      // %result = apply %specialized_fn(...)
      // %result_addr = pack_element_get 1 %pack_addr
      // store %result to %result_addr

      self.replaceOriginalResults(withResultsOf: specializedApply, using: packArgumentIndices)

    } else {
      // Write back multiple direct results:
      //
      // Before: %fn : () -> (Int, Int, @pack_out Pack{Int, Double})
      //
      // pack_element_set %addr to 0 of %result_pack
      // ...
      // %original_result = apply %fn(%result_pack)
      //
      // %value = load %addr
      // operate %original_result
      //
      // After: %specialized_fn : () -> (Int, Int, Double)
      //
      // pack_element_set %addr to 0 of %result_pack
      // ...
      // %result = apply %specialized_fn()
      // (%original1, %original2, %pack1, %pack2) = destructure_tuple %result  ╶┬──╴code inserted by substituteNewDirectResults
      // %reconstructed_result = tuple (%original1, %original2)                ╶┘
      // %addr = pack_element_get 0 of %result_pack                            ╶┬──╴code inserted by replaceOriginalResults
      // store %result_value to %addr                                           │
      // ... etc. for other pack elements                                      ╶┘
      //
      // %value = load %addr
      // operate %reconstructed_result

      let builder = Builder(after: specializedApply, self.context)
      let destructuredTuple = builder.createDestructureTuple(tuple: specializedApply)

      self.replaceOriginalResults(withResultsOf: destructuredTuple, using: packArgumentIndices)
    }
  }

  /// Replace the results of the original callee that were mapped to direct
  /// result values with the results of resultInstruction.
  ///
  /// See substituteNewDirectResults for code examples.
  private func replaceOriginalResults(
    withResultsOf resultInstruction: Instruction,
    using packArgumentIndices: PackArgumentIndices
  ) {
    let builder = Builder(after: resultInstruction, self.context)

    var directResults = resultInstruction.results.makeIterator()
    var substitutedResultTupleElements = [any Value]()
    var mappedResultPacks = self.callee.resultMap.makeIterator()
    var indirectResultIterator = self.apply.indirectResultOperands.makeIterator()

    for resultInfo in self.apply.functionConvention.results {
      // We only need to handle direct and pack results, since indirect results are handled above
      if !resultInfo.isSILIndirect {
        // Direct results of the original function are mapped to direct results of the specialized function.
        substitutedResultTupleElements.append(directResults.next()!)

      } else {
        guard let indirectResult = indirectResultIterator.next()?.value,
              indirectResult.type.shouldExplode
        else {
          continue
        }

        do {
          // Some elements of pack results may get mapped to direct results of the specialized function.
          // We handle those here.
          let mapped = mappedResultPacks.next()!


          let packIndices = packArgumentIndices[mapped.argumentIndex]!

          for (mappedDirectResult, (packIndex, elementType)) in zip(
                mapped.expandedElements, zip(packIndices, indirectResult.type.packElements)
              )
              where !mappedDirectResult.isSILIndirect
          {

            let result = directResults.next()!
            let outputResultAddress = builder.createPackElementGet(
              packIndex: packIndex, pack: indirectResult,
              elementType: elementType)

            builder.createStore(
              source: result, destination: outputResultAddress,
              // The callee is responsible for initializing return pack elements
              ownership: storeOwnership(for: result, normal: .initialize))
          }
        }
      }
    }

    // Replace all uses of the original apply's result, if necessary.
    if !self.apply.uses.isEmpty {
      // Since there could be many uses of the original apply's result tuple, we
      // construct a new one with the corresponding values from specializedApply.
      // If exactly one direct result was mapped, don't generate a tuple.
      if substitutedResultTupleElements.count == 1 {
        self.apply.uses.replaceAll(with: substitutedResultTupleElements[0], self.context)
      } else {
        let substitutedResultTuple = builder.createTuple(
          type: self.apply.type, elements: substitutedResultTupleElements)
        self.apply.uses.replaceAll(with: substitutedResultTuple, self.context)
      }
    }
  }

  /// Release borrows of parameters that were passed as @guaranteed.
  private func createEndBorrows(for borrows: [LoadBorrowInst], after: Instruction) {
    let builder = Builder(after: after, self.context)

    // Release any borrows that were created to pass parameters
    for borrow in borrows {
      builder.createEndBorrow(of: borrow)
    }
  }
}

//===----------------------------------------------------------------------===//
// Callee Specialization
//===----------------------------------------------------------------------===//

private func specializeCallee(apply: ApplySite, context: FunctionPassContext)
  -> PackExplodedFunction?
{
  // Only perform pack specialization when the callee has pack arguments
  guard let callee = apply.referencedFunction,
    callee.isDefinition,
    callee.argumentTypes.contains(where: { $0.shouldExplode })
  else {
    return nil
  }

  let exploded = PackExplodedFunction(from: callee, context)

  return exploded
}

/// Given a function with parameter pack arguments that can be eliminated (or
/// "exploded"), this struct constructs a specialized version of that function,
/// with each such argument split into individual parameters or results for each
/// member of each pack.
///
/// It also produces a mapping between the results and parameters of the
/// original and specialized functions, allowing calls to one to be replaced
/// with calls to the other, while retaining the same behaviour.
///
/// Whether a pack argument can be eliminated is determined using the
/// TypeProperties.shouldExplode computed property (see below).
///
/// For each pack argument of the original function, a corresponding pack is
/// allocated on the stack at the start of the specialized function. Each
/// parameter of the specialized function is stored at its corresponding element
/// of the appropriate local pack.
///
/// Most packs store the addresses of their elements. For pack elements that are
/// mapped to direct arguments, we allocate a stack location, store the value in
/// it, and store its address in the appropriate local pack.
///
/// For result pack elements that are mapped to direct result values, we
/// similarly store the address of a dedicated stack location in the
/// corresponding local pack. We can then load the value from that stack
/// location at the end of the function to return it.
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
private struct PackExplodedFunction {
  public let original: Function
  public let specialized: Function

  /// Represents the set of results of the new function corresponding to the
  /// pack argument of the original function at the given index, in ascending
  /// order.
  struct MappedResult {
    /// Index of the indirect pack argument of the original function for this result.
    let argumentIndex: Int
    /// Index of this pack in the function's result type tuple.
    /// For a non-tuple result, this is 0.
    let resultIndex: Int
    /// ResultInfo for the results produced by exploding the original result.
    ///
    /// NOTE: The expandedElements members of MappedResult & MappedParameter
    /// correspond to slices of the [ResultInfo] and [ParameterInfo] arrays
    /// produced at the same time as the ResultMap & ParameterMap respectively.
    /// Replacing these members with integer ranges or spans referring to those
    /// full arrays could be an easy performance optimization if this pass
    /// becomes a bottleneck.
    let expandedElements: [ResultInfo]
  }

  typealias ResultMap = [MappedResult]
  public let resultMap: ResultMap

  /// Represents the set of parameters of the new function corresponding to the
  /// pack argument of the original function at the given index, in ascending
  /// order.
  struct MappedParameter {
    let argumentIndex: Int
    /// ParameterInfo for the parameters produced by exploding the original parameter.
    let expandedElements: [ParameterInfo]
  }

  typealias ParameterMap = [MappedParameter]
  public let parameterMap: ParameterMap

  /// Maps indices of pack arguments of the original function to its
  /// `approximateFormalPackType` (a Canonical AST PackType).
  /// Saves recomputing these types every time they are needed,
  /// which would be unnecessarily expensive.
  public let packASTTypes: [Int: CanonicalType]

  init(from original: Function, _ context: FunctionPassContext) {

    self.original = original

    var packASTTypes = [Int: CanonicalType]()
    for (i, argument) in original.arguments.enumerated()
        where argument.type.shouldExplode
    {
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
        // If a method has a dynamic self parameter, it cannot be converted into a thin function (non-method).
        withRepresentation: original.mayBindDynamicSelf ? nil : .thin)

      self.buildSpecializedFunction(context)
    }
  }

  // Internal data structures used while building the specialized function.

  /// A collection of local values used to associate an argument of the
  /// specialized function with its corresponding pack element.
  private struct ExplodedArgument {
    /// The index of this element within its original pack.
    public let packIdx: ScalarPackIndexInst

    enum ConventionResources {
      case indirect
      case direct(AllocStackInst)
      case directGuaranteed(AllocStackInst, StoreBorrowInst)
    }

    /// The local resources associated with the argument that must be cleaned
    /// up, determined by its argument convention.
    public let resources: ConventionResources
  }

  /// Information about the local pack created to replace a pack parameter in
  /// the specialized function.
  private struct ArgumentMapping {
    public let allocPack: AllocPackInst
    public let arguments: [ExplodedArgument]
  }

  /// A mapping from the indices of pack arguments of the original function to
  /// their corresponding local packs in the specialized function.
  private typealias ArgumentMap = [Int: ArgumentMapping]

  /// Compute the parameter types for the pack-exploded version of a function,
  /// and the mapping between the original function's pack parameters, and the
  /// corresponding exploded parameters of the new function.
  fileprivate static func computeParameters(for function: Function) -> (
    parameters: [ParameterInfo], mapping: ParameterMap
  ) {
    var newParameters = [ParameterInfo]()
    var parameterMap = ParameterMap()

    for (argument, parameterInfo) in zip(function.parameters, function.convention.parameters) {
      if argument.type.shouldExplode {
        let mappedParameterInfos = argument.type.packElements.map { element in
          ParameterInfo(
            type: element.hasArchetype ? element.rawType.mapOutOfEnvironment().canonical : element.canonicalType,
            convention: explodedPackElementArgumentConvention(
              pack: parameterInfo, type: element, in: function),
            options: parameterInfo.options,
            hasLoweredAddresses: parameterInfo.hasLoweredAddresses)
        }

        parameterMap.append(MappedParameter(argumentIndex: argument.index, expandedElements: mappedParameterInfos))
        newParameters += mappedParameterInfos

      } else {
        // Leave the original argument unchanged
        newParameters.append(parameterInfo)
      }
    }

    return (newParameters, parameterMap)
  }

  /// Compute the result types for the pack-exploded version of a function, and
  /// the mapping between the original function's pack results, and the
  /// corresponding exploded results of the new function.
  private static func computeResults(for function: Function) -> (
    results: [ResultInfo], mapping: ResultMap
  ) {
    var resultMap = ResultMap()
    var newResults = [ResultInfo]()

    var indirectResultIterator = function.arguments[0..<function.convention.indirectSILResultCount]
      .lazy.enumerated().makeIterator()

    for (resultIndex, resultInfo) in function.convention.results.enumerated() {
      assert(
        !resultInfo.isSILIndirect || !indirectResultIterator.isEmpty,
        "There must be exactly as many indirect results in the function convention and argument list."
      )

      guard resultInfo.isSILIndirect,
        // There should always be a value here (expressed by the assert above).
        let (indirectResultIdx, indirectResult) = indirectResultIterator.next(),
        indirectResult.type.shouldExplode
      else {
        newResults.append(resultInfo)
        continue
      }

      let mappedResultInfos = indirectResult.type.packElements.map { element in
        ResultInfo(
          type: element.hasArchetype ? element.rawType.mapOutOfEnvironment().canonical : element.canonicalType,
          convention: explodedPackElementResultConvention(in: function, type: element),
          options: resultInfo.options,
          hasLoweredAddresses: resultInfo.hasLoweredAddresses)
      }

      resultMap.append(
        MappedResult(
          argumentIndex: indirectResultIdx, resultIndex: resultIndex,
          expandedElements: mappedResultInfos))
      newResults += mappedResultInfos

    }

    assert(
      indirectResultIterator.isEmpty,
      "We should have walked through all the indirect results, and no further.")

    return (newResults, resultMap)
  }

  /// Build the body of the pack-specialized function.
  private func buildSpecializedFunction(_ context: FunctionPassContext) {

    context.buildSpecializedFunction(specializedFunction: specialized) {
      (specialized, specContext) in
      cloneFunction(from: original, toEmpty: specialized, specContext)

      let argumentMap = explodePackArguments(from: original, to: specialized, specContext)

      // Modify the return block to extract and return the necessary direct
      // return values from their local stack allocations, if necessary. This is
      // only necessary if any pack result elements were mapped to direct
      // results, and the function contains at least one return statement.
      if resultMap.contains(where: {
        $0.expandedElements.contains(where: { !$0.isSILIndirect })
      }) {
        if let originalReturnStatement = specialized.returnInstruction as? ReturnInst {
          self.createExplodedReturn(
            replacing: originalReturnStatement, argumentMap: argumentMap, specContext)
        }
      }

      // Emit cleanup code at all exit points of the function.
      for bb in specialized.blocks
          where bb.terminator.isFunctionExiting
      {
        self.createCleanup(before: bb.terminator, argumentMap: argumentMap, specContext)
      }
    }

    context.notifyNewFunction(function: specialized, derivedFrom: original)
  }

  /// Replace the original return statement with one that returns all the
  /// original result values, as well as the direct results corresponding to
  /// indirect pack results in the original function.
  ///
  /// Care is taken to thread the original and new results together in an order
  /// that matches the original function:
  ///
  /// Before: () -> (Int, @pack_out Pack{Double, Int}, Double)
  ///
  /// pack_element_set 0 of %out_pack to %pack_double
  /// pack_element_set 1 of %out_pack to %pack_int
  ///
  /// return (%int, %double)
  ///
  /// After: () -> (Int, Double, Int, Double)
  ///
  /// return (%int, %pack_double, %pack_int, %double)
  ///
  private func createExplodedReturn(
    replacing originalReturn: ReturnInst, argumentMap: ArgumentMap, _ context: FunctionPassContext
  ) {
    let builder = Builder(
      before: originalReturn, location: originalReturn.location.asCleanup, context)

    let originalValue = originalReturn.returnedValue

    let originalReturnTupleElements: [Value]
    if originalValue.type.isVoid {
      originalReturnTupleElements = []
    } else if originalValue.type.isTuple {
      originalReturnTupleElements = [Value](
        builder.createDestructureTuple(tuple: originalValue).results)
    } else {
      originalReturnTupleElements = [originalValue]
    }

    // Thread together the original and exploded direct return values.
    let theReturnValues: [any Value]
    do {
      var returnValues = [any Value]()
      // The next original result to process.
      var resultIndex = 0
      var originalDirectResultIterator = originalReturnTupleElements.makeIterator()

      for mappedResult in resultMap {

        // Collect any direct results before the next mappedResult.
        while resultIndex < mappedResult.resultIndex {
          if !self.original.convention.results[resultIndex].isSILIndirect {
            returnValues.append(originalDirectResultIterator.next()!)
          }
          resultIndex += 1
        }

        assert(resultIndex == mappedResult.resultIndex, "The next pack result is not skipped.")

        let argumentMapping = argumentMap[mappedResult.argumentIndex]!
        for argument in argumentMapping.arguments {

          switch argument.resources {
          case .indirect:
            break
          case .direct(let allocStack):
            returnValues.append(
              builder.createLoad(
                fromAddress: allocStack,
                ownership: loadOwnership(for: allocStack, normal: .take))
            )
          case .directGuaranteed(_, _):
            preconditionFailure(
              "A pack-exploded result value has an associated store_borrow, but there should be no initial value to borrow."
            )
          }
        }

        // We have finished processing mappedResult, so step forward.
        resultIndex += 1
      }

      // Collect any remaining original direct results.
      while let directResult = originalDirectResultIterator.next() {
        returnValues.append(directResult)
      }

      theReturnValues = returnValues
    }

    // Return a single return value directly, rather than constructing a single-element tuple for it.
    if theReturnValues.count == 1 {
      builder.createReturn(of: theReturnValues[0])
    } else {
      let tupleElementTypes = theReturnValues.map { $0.type }
      let tupleType = context.getTupleType(elements: tupleElementTypes).loweredType(
        in: specialized)
      let tuple = builder.createTuple(type: tupleType, elements: theReturnValues)
      builder.createReturn(of: tuple)
    }

    context.erase(instruction: originalReturn)
  }

  /// Create a local stack-allocated pack to replace the argument at the given
  /// index. We pass the builder so the order of inserted instructions (notably
  /// stack allocations) is more predictable.
  private func prepareExplosion(
    in function: Function, at index: Int, builder: Builder, _ context: FunctionPassContext
  ) -> (AllocPackInst, Type) {
    let entryBlock = function.entryBlock

    let originalPackArg = entryBlock.arguments[index]
    let packType = originalPackArg.type.objectType

    let localPack = builder.createAllocPack(packType)
    originalPackArg.uses.replaceAll(with: localPack, context)

    entryBlock.eraseArgument(at: index, context)

    return (localPack, packType)
  }

  /// Explode the types of the entry block's pack arguments, and track the
  /// correspondence between exploded arguments and local packs.
  private func explodePackArguments(
    from original: Function, to specialized: Function, _ context: FunctionPassContext
  ) -> ArgumentMap {

    let entryBlock = specialized.entryBlock
    let builder = Builder(atBeginOf: entryBlock, context)

    var argumentMap = ArgumentMap()

    // Explode the arguments in reverse order. This ensures that the index of
    // each argument is not affected by other arguments exploding before it has
    // been processed. Parameters come after results, so we explode them first.

    // Explode parameters.
    for mapped in parameterMap.reversed() {

      let (localPack, packType) = prepareExplosion(
        in: specialized, at: mapped.argumentIndex, builder: builder, context)

      var mappings = [ExplodedArgument]()
      for (i, (type, parameterInfo)) in zip(packType.packElements, mapped.expandedElements)
        .enumerated()
      {
        let packIdx = builder.createScalarPackIndex(
          componentIndex: i, indexedPackType: self.packASTTypes[mapped.argumentIndex]!)
        let argument = entryBlock.insertFunctionArgument(
          atPosition: mapped.argumentIndex + i,
          type: parameterInfo.isSILIndirect ? type.addressType : type,
          ownership: Ownership(in: specialized, of: type, with: parameterInfo.convention),
          context)

        let address: Value
        let resources: ExplodedArgument.ConventionResources
        if parameterInfo.isSILIndirect {
          builder.createPackElementSet(elementValue: argument, packIndex: packIdx, pack: localPack)
          resources = .indirect
        } else {
          let alloc = builder.createAllocStack(type)

          if parameterInfo.convention == .directGuaranteed {
            // We do not own @guaranteed arguments, so we must use store_borrow instead of store.
            let storeBorrow = builder.createStoreBorrow(source: argument, destination: alloc)
            address = storeBorrow
            resources = .directGuaranteed(alloc, storeBorrow)
          } else {
            let ownership = storeOwnership(for: argument, normal: .initialize)
            builder.createStore(
              source: argument, destination: alloc,
              ownership: ownership)
            address = alloc
            resources = .direct(alloc)
          }
          builder.createPackElementSet(elementValue: address, packIndex: packIdx, pack: localPack)
        }

        mappings.append(
          ExplodedArgument(packIdx: packIdx, resources: resources))

      }
      argumentMap[mapped.argumentIndex] = ArgumentMapping(allocPack: localPack, arguments: mappings)
    }

    // Explode results.
    for mapped in resultMap.reversed() {

      let (localPack, packType) = prepareExplosion(
        in: specialized, at: mapped.argumentIndex, builder: builder, context)

      var mappings = [ExplodedArgument]()
      var insertArgumentPosition = mapped.argumentIndex
      for (i, (type, resultInfo)) in zip(packType.packElements, mapped.expandedElements)
        .enumerated()
      {
        let packIdx = builder.createScalarPackIndex(
          componentIndex: i, indexedPackType: self.packASTTypes[mapped.argumentIndex]!)

        let resources: ExplodedArgument.ConventionResources
        if resultInfo.isSILIndirect {
          // We only insert arguments for results that must be returned indirectly, so always use the addressType.
          let argument = entryBlock.insertFunctionArgument(
            atPosition: insertArgumentPosition, type: type.addressType,
            ownership: Ownership(
              in: specialized,
              of: type, with: ArgumentConvention(result: resultInfo.convention)),
            context)
          builder.createPackElementSet(elementValue: argument, packIndex: packIdx, pack: localPack)
          resources = .indirect
          insertArgumentPosition += 1
        } else {
          // It is the callee's responsibility to initialize indirect results,
          // so the original function body already does this.
          // We do not need to initialize here.
          let alloc = builder.createAllocStack(type)
          builder.createPackElementSet(elementValue: alloc, packIndex: packIdx, pack: localPack)
          resources = .direct(alloc)
        }

        // Results have no initial value that could need to be borrowed.
        mappings.append(
          ExplodedArgument(packIdx: packIdx, resources: resources))

      }
      argumentMap[mapped.argumentIndex] = ArgumentMapping(allocPack: localPack, arguments: mappings)

    }

    return argumentMap
  }

  /// Clean up locals, most notably alloc_packs, created as part of pack specialization.
  private func createCleanup(
    before terminator: TermInst, argumentMap: ArgumentMap, _ context: FunctionPassContext
  ) {

    let builder = Builder(before: terminator, location: terminator.location.asCleanup, context)

    let createDeallocations = { (idx: Int) in
      let mapped = argumentMap[idx]!
      for argument in mapped.arguments.reversed() {
        switch argument.resources {
        case .indirect:
          break
        case .direct(let allocStack):
          builder.createDeallocStack(allocStack)
        case .directGuaranteed(let allocStack, let storeBorrow):
          builder.createEndBorrow(of: storeBorrow)
          builder.createDeallocStack(allocStack)
        }
      }
      builder.createDeallocPack(argumentMap[idx]!.allocPack)
    }

    // Emit dealloc_packs in the opposite order to the alloc_packs. Since
    // alloc_packs are created while iterating backwards over the arguments,
    // iterate forwards.
    for mapped in resultMap {
      createDeallocations(mapped.argumentIndex)
    }
    for mapped in parameterMap {
      createDeallocations(mapped.argumentIndex)
    }
  }
}

/// Compute the appropriate argument convention for an exploded member of the given pack parameter with the given type, in the given function
private func explodedPackElementArgumentConvention(
  pack: ParameterInfo, type: Type, in function: Function
)
  -> ArgumentConvention
{
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
    return normal
  }
}

extension Type {
  /// A pack argument can explode if it contains no pack expansion types
  fileprivate var shouldExplode: Bool {
    // For now, we only attempt to explode indirect packs, since these are the most common and inefficient.
    return isSILPack && !containsSILPackExpansionType && isSILPackElementAddress
  }
}
