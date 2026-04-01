//===--- ClosureSpecialization.swift ---------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2024 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===-----------------------------------------------------------------------===//

import AST
import SIL

private let verbose = false

private func log(prefix: Bool = true, _ message: @autoclosure () -> String) {
  if verbose {
    debugLog(prefix: prefix, "[ADCS] " + message())
  }
}

/// Closure Specialization
/// ----------------------
/// Specializes functions which take a closure (a `partial_apply` or `thin_to_thick_function` as argument.
/// The closure is created directly in the specialized function whereas the captured arguments are passed
/// as additional arguments to the specialized function. As a heuristic, this is only done if the closure
/// is actually called in the function because after specialization, the closure and its call can be
/// further optimized, e.g. the closure can be inlined.
///
/// ```
///   %3 = function_ref @closure
///   %4 = partial_apply %3(%1, %2) : (Float, Int, Bool) -> ()
///   %5 = function_ref @closure_user
///   apply %5(%4)
///   ...
///
/// sil @closure_user : ((Float) -> (), Float) -> () {
/// bb0(%0 : $(Float) -> (), %1 : $Float):
///   apply %0(%1)
///   ...
/// ```
/// ->
/// ```
///   %5 = function_ref @specialized_closure_user
///   apply %5(%1, %2)
///   ...
///
/// sil @specialized_closure_user : (Float, Int, Bool) -> () {
/// bb0(%0 : $Float, %1 : $Int, %2 : $Bool):
///   %3 = function_ref @closure
///   %4 = partial_apply %3(%1, %2)
///   apply %3(%0)
///   ...
/// ```
///
let closureSpecialization = FunctionPass(name: "closure-specialization") {
  (function: Function, context: FunctionPassContext) in

  guard function.hasOwnership else {
    return
  }

  for inst in function.instructions {
    if let apply = inst as? FullApplySite {
      _ = trySpecialize(apply: apply, context)
    }
  }
  if context.needFixStackNesting {
    context.fixStackNesting(in: function)
  }
}

/// AutoDiff Closure Specialization
/// -------------------------------
/// This optimization performs closure specialization tailored for the patterns seen in Swift Autodiff. In principle,
/// the optimization does the same thing as the general closure specialization pass. However, it is tailored to the
/// patterns of Swift Autodiff.
///
/// The compiler performs reverse-mode differentiation on functions marked with `@differentiable(reverse)`. In doing so,
/// it generates corresponding VJP and Pullback functions, which perform the forward and reverse pass respectively. You
/// can think of VJPs as functions that "differentiate" an original function and Pullbacks as the calculated
/// "derivative" of the original function.
///
/// VJPs always return a tuple of 2 values -- the original result and the Pullback. Pullbacks are essentially a chain
/// of closures, where the closure-contexts are implicitly used as the so-called "tape" during the reverse
/// differentiation process. It is this chain of closures contained within the Pullbacks that this optimization aims
/// to optimize via closure specialization.
///
/// The code patterns that this optimization targets, look similar to the one below:
/// ``` swift
///
/// // Since `foo` is marked with the `differentiable(reverse)` attribute the compiler
/// // will generate corresponding VJP and Pullback functions in SIL. Let's assume that
/// // these functions are called `vjp_foo` and `pb_foo` respectively.
/// @differentiable(reverse)
/// func foo(_ x: Float) -> Float {
///   return sin(x)
/// }
///
/// //============== Before closure specialization ==============//
/// // VJP of `foo`. Returns the original result and the Pullback of `foo`.
/// sil @vjp_foo: $(Float) -> (originalResult: Float, pullback: (Float) -> Float) {
/// bb0(%0: $Float):
///   // __Inlined__ `vjp_sin`: It is important for all intermediate VJPs to have
///   // been inlined in `vjp_foo`, otherwise `vjp_foo` will not be able to determine
///   // that `pb_foo` is closing over other closures and no specialization will happen.
///                                                                               \
///   %originalResult = apply @sin(%0): $(Float) -> Float                          \__ Inlined `vjp_sin`
///   %partially_applied_pb_sin = partial_apply pb_sin(%0): $(Float) -> Float      /
///                                                                               /
///
///   %pb_foo = function_ref @pb_foo: $@convention(thin) (Float, (Float) -> Float) -> Float
///   %partially_applied_pb_foo = partial_apply %pb_foo(%partially_applied_pb_sin): $(Float, (Float) -> Float) -> Float
///
///   return (%originalResult, %partially_applied_pb_foo)
/// }
///
/// // Pullback of `foo`.
/// //
/// // It receives what are called as intermediate closures that represent
/// // the calculations that the Pullback needs to perform to calculate a function's
/// // derivative.
/// //
/// // The intermediate closures may themselves contain intermediate closures and
/// // that is why the Pullback for a function differentiated at the "top" level
/// // may end up being a "chain" of closures.
/// sil @pb_foo: $(Float, (Float) -> Float) -> Float {
/// bb0(%0: $Float, %pb_sin: $(Float) -> Float):
///   %derivative_of_sin = apply %pb_sin(%0): $(Float) -> Float
///   return %derivative_of_sin: Float
/// }
///
/// //============== After closure specialization ==============//
/// sil @vjp_foo: $(Float) -> (originalResult: Float, pullback: (Float) -> Float) {
/// bb0(%0: $Float):
///   %originalResult = apply @sin(%0): $(Float) -> Float
///
///   // Before the optimization, pullback of `foo` used to take a closure for computing
///   // pullback of `sin`. Now, the specialized pullback of `foo` takes the arguments that
///   // pullback of `sin` used to close over and pullback of `sin` is instead copied over
///   // inside pullback of `foo`.
///   %specialized_pb_foo = function_ref @specialized_pb_foo: $@convention(thin) (Float, Float) -> Float
///   %partially_applied_pb_foo = partial_apply %specialized_pb_foo(%0): $(Float, Float) -> Float
///
///   return (%originalResult, %partially_applied_pb_foo)
/// }
///
/// sil @specialized_pb_foo: $(Float, Float) -> Float {
/// bb0(%0: $Float, %1: $Float):
///   %2 = partial_apply @pb_sin(%1): $(Float) -> Float
///   %3 = apply %2(): $() -> Float
///   return %3: $Float
/// }
/// ```
///
let autodiffClosureSpecialization = FunctionPass(name: "autodiff-closure-specialization") {
  (function: Function, context: FunctionPassContext) in

  guard function.hasOwnership else {
    return
  }

  guard !function.isDefinedExternally,
    function.isAutodiffVJP
  else {
    return
  }

  var remainingSpecializationRounds = 5

  repeat {
    var changed = false
    for inst in function.instructions {
      if let partialApply = inst as? PartialApplyInst,
         partialApply.isPullbackInResultOfAutodiffVJP
      {
        if trySpecialize(apply: partialApply, context) {
          changed = true
        }
      }
    }
    if context.needFixStackNesting {
      context.fixStackNesting(in: function)
    }
    if !changed {
      break
    }

    remainingSpecializationRounds -= 1
  } while remainingSpecializationRounds > 0
}

// ===================== Utility functions and extensions ===================== //

private func trySpecialize(apply: ApplySite, _ context: FunctionPassContext) -> Bool {
  guard isCalleeSpecializable(of: apply),
        let specialization = analyzeArguments(of: apply, context)
  else {
    return false
  }

  let specializedParameters = specialization.getSpecializedParameters()

  // A function cannot have more than one "isolated" parameter.
  guard numberOfIsolatedParameters(specializedParameters) <= 1 else {
    return false
  }

  // We need to make each captured argument of all closures a unique Value. Otherwise the Cloner would
  // wrongly map added capture arguments to the re-generated closure in the specialized function:
  //
  //   %4 = partial_apply %3(%1, %1) : (Int, Int) -> ()  // %1 is captured twice
  //
  // sil @specialized_closure_user : (Int, Int) -> () {
  // bb0(%0 : $Int, %1 : $Int):
  //   %3 = function_ref @closure
  //   %4 = partial_apply %3(%0, %0)   <- instead of an argument list of `(%0, %1)`!
  //
  // This wouldn't be a problem per se - the code is correct. However, this is not reflected in the mangling.
  // And the same specialized function could be re-used at a call-site where not two identical values are
  // passed to the closure.
  //
  specialization.uniqueCaptureArguments(context)

  let specializedFunction = specialization.getOrCreateSpecializedFunction(specializedParameters, context)

  specialization.unUniqueCaptureArguments(context)

  specialization.rewriteApply(for: specializedFunction, context)

  specialization.deleteDeadClosures(context)

  return true
}

private func isCalleeSpecializable(of apply: ApplySite) -> Bool {
  if let callee = apply.referencedFunction,
     callee.isDefinition,

     // Calling `cloneRecursively` from `SpecializationInfo.cloneClosures`
     // requires the callee having ownership info. Otherwise, the cloner
     // uses `recordFoldedValue` instead of `recordClonedInstruction`, and
     // `postProcess` hook is not called, which leads to an assertion
     // failure in `BridgedClonerImpl::cloneInst`.
     callee.hasOwnership,

     // We don't support generic functions (yet)
     !apply.hasSubstitutions,

     // Don't specialize non-fragile (read as non-serialized) callees if the caller is fragile; the
     // specialized callee will have shared linkage, and thus cannot be referenced from the fragile caller.
     !(apply.parentFunction.isSerialized && !callee.isSerialized),

     // If the callee uses a dynamic Self, we cannot specialize it, since the resulting specialization
     // might no longer have 'self' as the last parameter.
     //
     // TODO: Keep the self argument the last when appending arguments.
     !callee.mayBindDynamicSelf,

     // Don't support self-recursive functions because that would result in duplicate mapping of values when cloning.
     callee != apply.parentFunction
  {
    return true
  }
  return false
}

private func analyzeArguments(of apply: ApplySite, _ context: FunctionPassContext) -> SpecializationInfo? {
  var argumentsToSpecialize = [(Operand, Closure)]()
  var rootClosures = [PartialApplyInst]()
  var rootClosuresAdded = InstructionSet(context)
  defer { rootClosuresAdded.deinitialize() }

  for argOp in apply.argumentOperands {
    var visited = ValueSet(context)
    defer { visited.deinitialize() }
    if let closure = findSpecializableClosure(of: argOp.value, &visited),
       // Ok, we know that we can perform the optimization but not whether or not the optimization
       // is profitable. Check if the closure is actually called in the callee (or in a function
       // called by the callee). This opens optimization opportunities, like inlining.
       isClosureApplied(apply.calleeArgument(of: argOp, in: apply.referencedFunction!)!)
    {
      argumentsToSpecialize.append((argOp, closure))
      if let partialApply = closure as? PartialApplyInst,
         rootClosuresAdded.insert(partialApply)
      {
        rootClosures.append(partialApply)
      }
    }
  }
  if argumentsToSpecialize.isEmpty {
    return nil
  }
  return SpecializationInfo(apply: apply, closureArguments: argumentsToSpecialize, rootClosures: rootClosures)
}

// Walks down the use-def chain of a function argument, recursively, to find a rootClosure.
private func findSpecializableClosure(of value: Value, _ visited: inout ValueSet) -> Closure? {
  visited.insert(value)

  let specializationLevelLimit = 2

  switch value {
  case is ConvertFunctionInst,
       is ConvertEscapeToNoEscapeInst,
       is MoveValueInst,
       is CopyValueInst:
    return findSpecializableClosure(of: (value as! UnaryInstruction).operand.value, &visited)

  case let mdi as MarkDependenceInst:
    guard mdi.value.type.isNoEscapeFunction, mdi.value.type.isThickFunction else {
      return nil
    }
    guard let operandClosure = findSpecializableClosure(of: mdi.value, &visited) else {
      return nil
    }
    // Make sure that the mark_dependence's base is part of the use-def chain and will therefore be cloned as well.
    if !visited.contains(mdi.base) {
      return nil
    }
    return operandClosure

  case let partialApply as PartialApplyInst:
    // Don't specialize for re-abstractions via a partial_apply, but treat such re-abstractions like
    // closure "conversions". E.g.
    // ```
    //   %1 = partial_apply             // root closure
    //   %2 = function_ref @thunk
    //   %3 = partial_apply %2(%1)      // re-abstraction
    //   apply %f(%3)
    // ```
    if partialApply.isPartialApplyOfThunk,
       let argumentClosure = findSpecializableClosure(of: partialApply.arguments[0], &visited)
    {
      return argumentClosure
    }
    guard let callee = partialApply.referencedFunction,
          !partialApply.hasSubstitutions,

          // Avoid an infinite specialization loop caused by repeated runs of ClosureSpecialization and
          // ConstantCapturePropagation.
          // ConstantCapturePropagation propagates constant function-literals. Such function specializations
          // can then be optimized again by ClosureSpecialization and so on. This happens if a closure argument
          // is called _and_ referenced in another closure, which is passed to a recursive call. E.g.
          //
          // func foo(_ c: @escaping () -> ()) {
          //  c() foo({ c() })
          // }
          //
          // A limit of 2 is good enough and will not be exceed in "regular" optimization scenarios.
          callee.specializationLevel <= specializationLevelLimit,

          // Functions with a readnone, readonly or releasenone effect and a consumed captures cannot be
          // specialized because the captured arguments are passed as owned arguments to the specialized
          // function. Destroy for such arguments in the specialized function violates the effect.
          (partialApply.isOnStack || callee.effectAllowsSpecialization),

          // TODO: handle other kind of indirect arguments
          partialApply.hasOnlyInoutIndirectArguments,

          (partialApply.isOnStack || partialApply.allArgumentsCanBeCopied)
    else {
      return nil
    }
    return partialApply

  case let tttfi as ThinToThickFunctionInst:
    guard let callee = tttfi.referencedFunction,
          callee.specializationLevel <= specializationLevelLimit
    else {
      return nil
    }
    return tttfi

  default:
    return nil
  }
}

/// Either a `partial_apply` or a `thin_to_thick_function`
private typealias Closure = SingleValueInstruction

/// Information about the function to be specialized and for which closure arguments.
private struct SpecializationInfo {

  // The apply which we want to specialize
  let apply: ApplySite

  // All closure arguments of the apply which we want to replace (usually there is one, but there can be
  // multiple).
  // Note that the `Closure` is not necessarily the value of the `Operand`. There can be function conventions
  // and re-abstractions (via a thunk) in-between:
  //
  //   %4 = partial_apply %3(%1) : (Int) -> ()   // %4 = rootClosure
  //   %5 = convert_function %4
  //   %6 = copy_value %5
  //   apply %5(%5)                              // %5 = closureArgument
  //
  let closureArguments: [(closureArgument: Operand, rootClosure: Closure)]

  // All rootClosures of `closureArguments` which are `partial_apply`s, and uniqued: if a rootClosure
  // appears multiple times in `closureArguments`, it's only added a single time here.
  let rootClosures: [PartialApplyInst]

  // The function to specialize
  var callee: Function { apply.referencedFunction! }

  private typealias Cloner = SIL.Cloner<FunctionPassContext>

  func getOrCreateSpecializedFunction(_ specializedParameters: [ParameterInfo],
                                      _ context: FunctionPassContext
  ) -> Function {
    let specializedFunctionName = getSpecializedFunctionName(context)

    if let existingSpecializedFunction = context.lookupFunction(name: specializedFunctionName) {
      return existingSpecializedFunction
    }

    let specializedFunction =
      context.createSpecializedFunctionDeclaration(
        from: callee, withName: specializedFunctionName,
        withParams: specializedParameters,
        // The specialized function is always a thin function. This is important because we add additional
        // parameters after the Self parameter of witness methods. In this case the new function is not a
        // method anymore.
        withRepresentation: .thin, makeBare: true)

    context.buildSpecializedFunction(
      specializedFunction: specializedFunction,
      buildFn: { (specializedFunction, specializedContext) in
        var cloner = Cloner(cloneToEmptyFunction: specializedFunction, specializedContext)
        defer { cloner.deinitialize() }

        cloneAndSpecializeFunctionBody(using: &cloner)
        // Cloning a whole function, even if it contains an `unreachable`, doesn't require lifetime completion.
        specializedContext.setNeedCompleteLifetimes(to: false)
      })

    context.notifyNewFunction(function: specializedFunction, derivedFrom: callee)

    return specializedFunction
  }

  private func getSpecializedFunctionName(_ context: FunctionPassContext) -> String {
    var visited = Dictionary<Closure, Int>()

    let argumentManglings = closureArguments.map { (argOp, closure) in
      let argIdx = apply.calleeArgumentIndex(of: argOp)!
      if let prevArgIdx = visited[closure] {

        // If the same closure is passed multiple times to a function, we need to reflect this in the mangling:
        //
        //   %3 = function_ref @closure
        //   %4 = partial_apply %3(%1) : (Int) -> ()
        //   apply %6(%4, %4)
        //
        // sil @specialized_closure_user : (Int) -> () {
        //
        // is different than
        //
        //   %3 = function_ref @closure
        //   %4 = partial_apply %3(%1) : (Int) -> ()
        //   %5 = partial_apply %3(%1) : (Int) -> ()
        //   apply %6(%4, %5)
        //
        // sil @specialized_closure_user : (Int, Int) -> () {
        //
        return (argIdx, FunctionPassContext.ClosureArgumentMangling.previousArgumentIndex(prevArgIdx))
      } else {
        visited[closure] = argIdx
        return (argIdx, FunctionPassContext.ClosureArgumentMangling.closure(closure))
      }
    }
    return context.mangle(withClosureArguments: argumentManglings, from: callee)
  }

  func getSpecializedParameters() -> [ParameterInfo] {
    var specializedParamInfoList: [ParameterInfo] = []

    // Start by adding all original parameters except for the closure parameters.
    let firstParamIndex = callee.argumentConventions.firstParameterIndex
    for (index, paramInfo) in callee.convention.parameters.enumerated() {
      let argIndex = index + firstParamIndex
      if !closureArguments.contains(where: { apply.calleeArgumentIndex(of: $0.0) == argIndex}) {
        specializedParamInfoList.append(paramInfo)
      }
    }

    // Now, append parameters captured by each of the root closures.
    for partialApply in rootClosures {
      let closureConvention = partialApply.functionConvention
      let unappliedArgumentCount = partialApply.unappliedArgumentCount - closureConvention.indirectSILResultCount

      for paramInfo in closureConvention.parameters[unappliedArgumentCount...] {
        let newParamInfo = paramInfo.withSpecializedConvention(for: partialApply, in: callee)
        specializedParamInfoList.append(newParamInfo)
      }
    }

    return specializedParamInfoList
  }

  private func cloneAndSpecializeFunctionBody(using cloner: inout Cloner) {
    addFunctionArgumentsWithoutClosures(using: &cloner)

    for rootClosure in rootClosures {
      addFunctionArgumentsForCaptures(of: rootClosure, using: &cloner)
    }

    let clonedClosureArguments = cloneClosures(using: &cloner)

    cloner.cloneFunctionBody(from: callee)

    addMissingDestroysAtFunctionExits(for: clonedClosureArguments, cloner.context)
  }

  private func addFunctionArgumentsWithoutClosures(using cloner: inout Cloner) {
    let clonedEntryBlock = cloner.getOrCreateEntryBlock()

    for originalArg in callee.arguments where !isClosureArgument(calleeArgument: originalArg) {
      let argType = originalArg.type.getLoweredType(in: cloner.targetFunction)
      let clonedArg = clonedEntryBlock.addFunctionArgument(type: argType, cloner.context)
      clonedArg.copyFlags(from: originalArg, cloner.context)
      cloner.recordFoldedValue(originalArg, mappedTo: clonedArg)
    }
  }

  private func addFunctionArgumentsForCaptures(of closure: PartialApplyInst, using cloner: inout Cloner) {
    for originalClosureArg in closure.arguments {
      let capturedArg = cloner.targetFunction.entryBlock.addFunctionArgument(
        type: originalClosureArg.type.getLoweredType(in: cloner.targetFunction),
        cloner.context)
      if !cloner.isCloned(value: originalClosureArg) {
        cloner.recordFoldedValue(originalClosureArg, mappedTo: capturedArg)
      }
    }
  }

  private func cloneClosures(using cloner: inout Cloner) -> [Value] {
    return closureArguments.map { (closureArgOp, _) in
      let clonedArg = cloner.cloneRecursively(value: closureArgOp.value)

      let originalArg = apply.calleeArgument(of: closureArgOp, in: callee)!

      let clonedValue: Value
      if clonedArg.ownership == .owned && apply.convention(of: closureArgOp) == .directGuaranteed {
        let block = cloner.targetFunction.entryBlock
        let builder = Builder(atEndOf: block, location: block.instructions.last!.location, cloner.context)
        clonedValue = builder.createBeginBorrow(of: clonedArg)
      } else {
        clonedValue = clonedArg
      }
      cloner.recordFoldedValue(originalArg, mappedTo: clonedValue)

      return clonedValue
    }
  }

  func rewriteApply(for specializedFunction: Function, _ context: FunctionPassContext) {
    insertCompensatingDestroysForOwnedClosureArguments(context)

    let newApplyArgs = getNewApplyArguments(context)

    apply.replace(withCallTo: specializedFunction, arguments: newApplyArgs, context)
  }

  private func insertCompensatingDestroysForOwnedClosureArguments(_ context: FunctionPassContext) {
    let builder = Builder(before: apply, context)
    for (argOp, _) in closureArguments where argOp.endsLifetime {
      builder.createDestroyValue(operand: argOp.value)
    }
  }

  private func getNewApplyArguments(_ context: FunctionPassContext) -> [Value] {
    let newCapturedArguments = rootClosures.flatMap { partialApply in
      partialApply.arguments.map { capturedArg in
        if partialApply.isOnStack || capturedArg.ownership == .none {
          // Non-escaping closures don't consume their captures. Therefore we pass them also as "guaranteed"
          // arguments to the specialized function.
          // Note that because the non-escaping closure was passed to the original function, this guarantees
          // that the lifetime of the captured arguments also extend to at least the apply of the function.
          capturedArg
        } else {
          // Escaping closures consume their captures. Therefore we pass them as "owned" arguments to the
          // specialized function.
          capturedArg.copy(at: partialApply, andMakeAvailableIn: apply.parentBlock, context)
        }
      }
    }
    return nonClosureArguments.values + newCapturedArguments
  }

  func uniqueCaptureArguments(_ context: FunctionPassContext) {
    let builder = Builder(before: apply, context)

    // Insert identity cast instructions for all closure arguments to make them unique. We could use any kind
    // of forwarding instruction - we'll delete them afterwards, anyway.
    //
    //   %4 = partial_apply %3(%1, %1) : (Int, Int) -> ()  // %1 is captured twice
    // ->
    //   %2 = unchecked_value_cast %1
    //   %3 = unchecked_value_cast %1
    //   %4 = partial_apply %3(%2, %3) : (Int, Int) -> ()  // all arguments are unique values now!
    //
    for closure in rootClosures {
      for argOp in closure.argumentOperands {
        let cast = builder.createUncheckedValueCast(from: argOp.value, to: argOp.value.type)
        argOp.set(to: cast, context)
      }
    }
  }

  func unUniqueCaptureArguments(_ context: FunctionPassContext) {
    // Remove the inserted identity casts again.
    for closure in rootClosures {
      for argOp in closure.argumentOperands {
        let cast = argOp.value as! UncheckedValueCastInst
        cast.replace(with: cast.fromValue, context)
      }
    }
  }

  func deleteDeadClosures(_ context: FunctionPassContext) {
    for  (_, closure) in closureArguments where !closure.isDeleted {
      if context.tryDeleteDeadClosure(closure: closure) {
        context.notifyInvalidatedStackNesting()
      }
    }
  }

  private var nonClosureArguments: LazyFilterSequence<OperandArray> {
    apply.argumentOperands.lazy.filter{ argOp in !closureArguments.contains{ $0.0 == argOp } }
  }

  private func isClosureArgument(calleeArgument: FunctionArgument) -> Bool {
    closureArguments.contains { apply.calleeArgument(of: $0.0, in: callee) == calleeArgument }
  }
}

private func isClosureApplied(_ closure: Value) -> Bool {
  var handledFuncs: Set<Function> = []
  return checkRecursivelyIfClosureIsApplied(closure, &handledFuncs)
}

private func checkRecursivelyIfClosureIsApplied(_ closure: Value, _ handledFuncs: inout Set<Function>) -> Bool {
  for use in closure.uses {
    switch use.instruction {

    case let apply as FullApplySite:
      if apply.callee == closure {
        return true
      }
      let recursionBudget = 8

      // Recurse into called function
      if let callee = apply.referencedFunction,
         callee.isDefinition,
         handledFuncs.insert(callee).inserted,
         handledFuncs.count <= recursionBudget,
         let calleeArg = apply.calleeArgument(of: use, in: callee)
      {
        if checkRecursivelyIfClosureIsApplied(calleeArg, &handledFuncs) {
          return true
        }
      }

    case is CopyValueInst, is MoveValueInst:
      if checkRecursivelyIfClosureIsApplied(use.instruction as! SingleValueInstruction, &handledFuncs) {
        return true
      }

    default:
      break
    }
  }

  return false
}

/// Add destroys for values which are not consumed, yet.
/// There are two cases of values for which we need this:
/// 1. guaranteed closure arguments
/// 2. operands of copies at the call site
///
/// ```
///   %1 = partial_apply %closure   // is cloned to the specialized function
///   %2 = copy_value %1            // is cloned to the specialized function
///   apply %function(%2) : (@guaranteed () -> ()) -> ()
///   destroy_value %1
///
/// sil @specializedFunction() -> () {
/// bb0:
///   %1 = partial_apply %closure
///   %2 = copy_value %1
///   ... // body
///   destroy_value %2              // case 1: destroy for the guaranteed closure argument
///   destroy_value %1              // case 2: destroy for the operand of the copy
///   return
/// ```
private func addMissingDestroysAtFunctionExits(for clonedArguments: [Value], _ context: FunctionPassContext) {
  var needDestroy = ValueWorklist(context)
  defer { needDestroy.deinitialize() }

  for clonedArg in clonedArguments {
    if let beginBorrow = clonedArg as? BeginBorrowInst {
      Builder.insertCleanupAtFunctionExits(of: beginBorrow.parentFunction, context) { builder in
        builder.createEndBorrow(of: beginBorrow)
      }
      completeLifetime(of: beginBorrow, context)
    }
    findValuesWhichNeedDestroyRecursively(value: clonedArg, needDestroy: &needDestroy)
  }

  while let valueToDestroy = needDestroy.pop() {
    Builder.insertCleanupAtFunctionExits(of: valueToDestroy.parentFunction, context) { builder in
      builder.createDestroyValue(operand: valueToDestroy)
    }
    completeLifetime(of: valueToDestroy, context)
  }
}

private func findValuesWhichNeedDestroyRecursively(value: Value, needDestroy: inout ValueWorklist) {
  if let svi = value as? SingleValueInstruction {
    for op in svi.operands {
      findValuesWhichNeedDestroyRecursively(value: op.value, needDestroy: &needDestroy)
    }
  }
  if value.ownership == .owned && value.uses.endingLifetime.isEmpty {
    needDestroy.pushIfNotVisited(value)
  }
}

private extension ParameterInfo {
  func withSpecializedConvention(for partialApply: PartialApplyInst, in callee: Function) -> Self {
    let argType = type.loweredType(in: partialApply.parentFunction)
    let specializedParamConvention = if self.convention.isIndirect {
      self.convention
    } else {
      if argType.isTrivial(in: callee) {
        ArgumentConvention.directUnowned
      } else {
        if partialApply.isOnStack {
          ArgumentConvention.directGuaranteed
        } else {
          ArgumentConvention.directOwned
        }
      }
    }

    return ParameterInfo(
      type: self.type, convention: specializedParamConvention, options: self.options,
      hasLoweredAddresses: self.hasLoweredAddresses)
  }
}

private func numberOfIsolatedParameters(_ params: [ParameterInfo]) -> Int {
  var count = 0
  for param in params where param.hasOption(.isolated) {
    count += 1
  }
  return count
}

private extension PartialApplyInst {
  /// True, if the closure obtained from this partial_apply is the
  /// pullback returned from an autodiff VJP
  var isPullbackInResultOfAutodiffVJP: Bool {
    if self.parentFunction.isAutodiffVJP,
      let use = self.uses.singleUse,
      let tupleInst = use.instruction as? TupleInst,
      let returnInst = self.parentFunction.returnInstruction,
      tupleInst == returnInst.returnedValue
    {
      return true
    }

    return false
  }

  var isPartialApplyOfThunk: Bool {
    if self.numArguments == 1,
      let fun = self.referencedFunction,
      fun.thunkKind == .reabstractionThunk || fun.thunkKind == .thunk,
      self.arguments[0].type.isLoweredFunction,
      self.arguments[0].type.isReferenceCounted(in: self.parentFunction) || self.callee.type.isThickFunction
    {
      return true
    }
    return false
  }

  var hasOnlyInoutIndirectArguments: Bool {
    self.argumentOperands
      .filter { !$0.value.type.isObject }
      .allSatisfy { self.convention(of: $0)!.isInout }
  }

  var allArgumentsCanBeCopied: Bool {
    arguments.allSatisfy { !$0.type.isMoveOnly }
  }
}

private extension EnumInst {
  var caseName: StringRef {
    return self.type.getEnumCases(in: self.parentFunction)![self.caseIndex]!.name
  }

  var isOptionalSome: Bool {
    assert(self.type.isOptional)
    assert(self.caseIndex == Builder.optionalNoneCaseIndex || self.caseIndex == Builder.optionalSomeCaseIndex)
    return self.caseIndex == Builder.optionalSomeCaseIndex
  }

  var isOptionalNone: Bool {
    assert(self.type.isOptional)
    assert(self.caseIndex == Builder.optionalNoneCaseIndex || self.caseIndex == Builder.optionalSomeCaseIndex)
    return self.caseIndex == Builder.optionalNoneCaseIndex
  }
}

private extension Function {
  var effectAllowsSpecialization: Bool {
    switch self.effectAttribute {
    case .readNone, .readOnly, .releaseNone: return false
    default: return true
    }
  }
}

private extension Collection {
  var singleElementAssumingAtMostOne : Element? {
    assert(self.count <= 1)
    return self.singleElement
  }
}

private extension BasicBlock {
  func getBranchTracingEnumArg(vjp: Function) -> Argument? {
    return self.arguments.filter { $0.type.isBranchTracingEnum(in: vjp) }.singleElementAssumingAtMostOne
  }
}

// Consider the code snippet below. This is what returning basic block of VJP usually ends with.
//   // function_ref pullback of foo(_:)
//   %pullbackOfFoo = function_ref @$pullbackOfFoo : $@convention(thin) (Float, _AD__$foo_bbA__Pred__src_0_wrt_0) -> Float
//   %paiOfPullback = partial_apply [callee_guaranteed] %pullbackOfFoo(%bte) : $@convention(thin) (Float, _AD__$foo_bbA__Pred__src_0_wrt_0) -> Float
//   %resWithPullback = tuple (%result, %paiOfPullback)
//   return %resWithPullback
//
// The function below finds %paiOfPullback in terms of the example above.
private func getPartialApplyOfPullbackInExitVJPBB(vjp: Function) -> PartialApplyInst? {
  log("getPartialApplyOfPullbackInExitVJPBB: running for VJP \(vjp.name)")
  guard let exitBB = vjp.blocks.filter({ $0.terminator as? ReturnInst != nil }).singleElementAssumingAtMostOne
  else {
    log("getPartialApplyOfPullbackInExitVJPBB: exit BB not found, aborting")
    return nil
  }

  let ri = exitBB.terminator as! ReturnInst
  guard let retValDefiningInstr = ri.returnedValue.definingInstruction else {
    log("getPartialApplyOfPullbackInExitVJPBB: return value is not defined by an instruction, aborting")
    return nil
  }

  func handleConvertFunctionOrPartialApply(inst: Instruction) -> PartialApplyInst? {
    if let pai = inst as? PartialApplyInst {
      log("getPartialApplyOfPullbackInExitVJPBB: success")
      return pai
    }
    if let cfi = inst as? ConvertFunctionInst {
      if let pai = cfi.fromFunction as? PartialApplyInst {
        log("getPartialApplyOfPullbackInExitVJPBB: success")
        return pai
      }
      log("getPartialApplyOfPullbackInExitVJPBB: fromFunction operand of convert_function instruction is not defined by partial_apply instruction, aborting")
      return nil
    }
    log("getPartialApplyOfPullbackInExitVJPBB: unexpected instruction type, aborting")
    return nil
  }

  if let ti = retValDefiningInstr as? TupleInst {
    log("getPartialApplyOfPullbackInExitVJPBB: return value is defined by tuple instruction")
    if ti.operands.count < 2 {
      log("getPartialApplyOfPullbackInExitVJPBB: tuple instruction has \(ti.operands.count) operands, but at least 2 expected, aborting")
      return nil
    }
    guard let lastTupleElemDefiningInst = ti.operands.last!.value.definingInstruction else {
      log("getPartialApplyOfPullbackInExitVJPBB: last tuple element is not defined by an instruction, aborting")
      return nil
    }
    return handleConvertFunctionOrPartialApply(inst: lastTupleElemDefiningInst)
  }

  return handleConvertFunctionOrPartialApply(inst: retValDefiningInstr)
}

// Consider the following code in a VJP when handling a throwing function:
//   bbA:
//     %closure = ...
//     %optionalWrapper1 = enum $Optional<ClosureType>, #Optional.some!enumelt, %closure
//     %payloadTuple1 = tuple (..., %optionalWrapper1)
//     %bteWithSome = enum $_AD__$xxx_bbN__Pred__xxx, #_AD__$xxx_bbN__Pred__xxx.bbK!enumelt, %payloadTuple1
//   bbB:
//     %optionalWrapper2 = enum $Optional<ClosureType>, #Optional.none!enumelt
//     %payloadTuple2 = tuple (..., %optionalWrapper2)
//     %bteWithNone = enum $_AD__$xxx_bbM__Pred__xxx, #_AD__$xxx_bbM__Pred__xxx.bbK!enumelt, %payloadTuple2
//
// During specialization, we need to change the underlying type for 'none' optional value %optionalWrapper2.
// The code below for each 'some' optional value (%optionalWrapper1 in the example above) finds the corresponding
// 'none' optional value and saves info about found value to allow further specialization. Pay attention to
// exact same names for branch tracing enum cases used for payload tuples in both cases (bbK in the example above).
private func findOptionalNoneMatchingOptionalSome(in vjp: Function, closuresInBTE: [ClosureInBTE]) -> [ClosureInBTE] {
  let branchTracingEnumInstructions: [EnumInst] = vjp.instructions.filter { $0 is EnumInst }.map {
    $0 as! EnumInst
  }.filter { $0.type.isBranchTracingEnum(in: vjp) }

  var closuresInBTEForOptionalNone = [ClosureInBTE]()

  for closureInBTE in closuresInBTE {
    if closureInBTE.optionalWrapper == nil {
      continue
    }
    assert(closureInBTE.optionalWrapper!.isOptionalSome)
    // In terms of the example above, we've found %closure and %optionalWrapper1.

    let enumCase = closureInBTE.enumCase
    // In terms of the example above, enumCase.name is now equal to "bbK".

    let bteWithNoneArr = branchTracingEnumInstructions.filter {
      $0.caseName == enumCase.name && $0.type != enumCase.enumType(in: vjp)
    }
    guard let bteWithNone = bteWithNoneArr.singleElement else {
      assert(bteWithNoneArr.isEmpty)
      continue
    }

    let payloadTuple = bteWithNone.operands.singleElement!.value as! TupleInst
    let optionalNone = payloadTuple.operands.last!.value as! EnumInst
    assert(optionalNone.isOptionalNone)

    closuresInBTEForOptionalNone.append(ClosureInBTE(
      closure: closureInBTE.closure,
      subsetThunk: closureInBTE.subsetThunk,
      optionalWrapper: optionalNone,
      useInPayload: payloadTuple.operands.last!,
      enumCase: bteWithNone.type.getEnumCases(in: vjp)![bteWithNone.caseIndex]!
    ))
  }

  return closuresInBTEForOptionalNone
}

typealias BTEPayloadArgOfPbBBInfo = (arg: Argument, enumCase: EnumCase, throwingSuccessor: BasicBlock?)

// Consider a basic block argument which is a payload tuple of a branch tracing enum payload case.
// The last element type of this tuple might be not a closure type, but an optional of a closure type.
// Such optionals are unwrapped with switch_enum instruction. This function finds the successor basic
// block for 'some' case of the switch_enum accepting this optional closure (given that the optional
// closure comes from the last element of branch tracing enum payload tuple BB argument).
//
// For the example below, find bbM successor when given %arg argument:
//
//   bbN(..., %arg : $(predecessor: _AD__$xxx_bbA__Pred__xxx, ClosureType1, ..., Optional<ClosureTypeNum>)):
//     // ...
//     %last_closure = tuple_extract %arg, num
//     // Alternatively, we might have (..., %last_closure) = destructure_tuple %arg
//     switch_enum %last_closure, case #Optional.some!enumelt: bbM, case #Optional.none!enumelt: bbK
//
//   bbM(%closure: ClosureTypeNum):
//     // ...
private func getSuccessorForOptionalSome(arg: Argument) -> BasicBlock? {
  let bb = arg.parentBlock
  guard let sei = bb.terminator as? SwitchEnumInst,
        sei.enumOp.type.isOptional
  else {
    return nil
  }
  assert(bb.successors.count == 2)

  if let tei = sei.enumOp.definingInstruction as? TupleExtractInst {
    if tei.tuple != arg {
      return nil
    }
    assert(tei.fieldIndex + 1 == arg.type.tupleElements.count)
  } else if let dti = sei.enumOp.definingInstruction as? DestructureTupleInst {
    if dti.tuple != arg {
      return nil
    }
    assert(dti.results.last! == sei.enumOp)
  } else {
    return nil
  }

  return sei.getUniqueSuccessor(forCaseIndex: Builder.optionalSomeCaseIndex)!
}

// If the pullback's basic block has an argument which is a payload tuple of the
// branch tracing enum corresponding to the given VJP, return this argument and any valid combination
// of a branch tracing enum type and its case index having the same payload tuple type as the argument.
// The function assumes that no more than one such argument is present.
//
// To find the payload tuple argument, we look at any of the predecessor blocks and see how
// a particular argument is calculated. We consider the argument to be a payload tuple of a branch
// tracing enum if it is calculated as a result of unchecked_enum_data or switch_enum instruction
// accepting a branch tracing enum.
//
// For the example below, find %argB and %argD arguments in bbB and bbD basic blocks correspondingly.
//
//   bbA(...):
//     // Consider %bteA having branch tracing enum type in given vjp, e.g. _AD__$xxx_bbA__Pred__xxx
//     %payloadA = unchecked_enum_data %bteA, #_AD__$xxx_bbA__Pred__xxx.bbX!enumelt
//     br bbB(..., %payloadA, ...)
//
//   bbB(..., %argB : $(..., ..., ...), ...):
//     // ...
//
//   bbC(...):
//     // Consider %bteC having branch tracing enum type in given vjp, e.g. _AD__$xxx_bbC__Pred__xxx
//     switch_enum %bteC, #_AD__$xxx_bbC__Pred__xxx.bbY!enumelt: bbD, ...
//
//   bbD(%argD : $(..., ..., ...)):
//     // ...
private func getBTEPayloadArgOfPbBBInfo(_ bb: BasicBlock, vjp: Function)
  -> BTEPayloadArgOfPbBBInfo?
{
  log("getBTEPayloadArgOfPbBBInfo: basic block \(bb.shortDescription) in pullback \(bb.parentFunction.name)")
  guard let predBB = bb.predecessors.first else {
    log("getBTEPayloadArgOfPbBBInfo: the bb has no predecessors, aborting")
    return nil
  }

  log("getBTEPayloadArgOfPbBBInfo: start iterating over bb args")
  for arg in bb.arguments {
    log("getBTEPayloadArgOfPbBBInfo: \(arg)")
    if !arg.type.isTuple {
      log("getBTEPayloadArgOfPbBBInfo: arg is not a tuple, skipping")
      continue
    }

    if let bi = predBB.terminator as? BranchInst {
      log("getBTEPayloadArgOfPbBBInfo: terminator of pred bb is branch instruction")
      guard let uedi = bi.operands[arg.index].value.definingInstruction as? UncheckedEnumDataInst
      else {
        log("getBTEPayloadArgOfPbBBInfo: operand corresponding to the argument is not defined by unchecked_enum_data instruction")
        continue
      }
      let enumType = uedi.`enum`.type
      if !enumType.isBranchTracingEnum(in: vjp) {
        log("getBTEPayloadArgOfPbBBInfo: enum type \(enumType) is not a branch tracing enum in VJP \(vjp.name)")
        continue
      }

      log("getBTEPayloadArgOfPbBBInfo: success")
      return BTEPayloadArgOfPbBBInfo(
        arg: arg,
        enumCase: enumType.getEnumCases(in: vjp)![uedi.caseIndex]!,
        throwingSuccessor: getSuccessorForOptionalSome(arg: arg)
      )
    }

    if let sei = predBB.terminator as? SwitchEnumInst {
      log("getBTEPayloadArgOfPbBBInfo: terminator of pred bb is switch_enum instruction")
      let enumType = sei.enumOp.type
      if !enumType.isBranchTracingEnum(in: vjp) {
        log("getBTEPayloadArgOfPbBBInfo: enum type \(enumType) is not a branch tracing enum in VJP \(vjp.name)")
        continue
      }

      log("getBTEPayloadArgOfPbBBInfo: success")
      return BTEPayloadArgOfPbBBInfo(
        arg: arg,
        enumCase: enumType.getEnumCases(in: vjp)![sei.getUniqueCase(forSuccessor: bb)!]!,
        throwingSuccessor: getSuccessorForOptionalSome(arg: arg)
      )
    }
  }

  log("getBTEPayloadArgOfPbBBInfo: finish iterating over bb args; branch tracing enum arg not found")
  return nil
}

private extension Instruction {
  var asSupportedClosure: SingleValueInstruction? {
    switch self {
    case let tttf as ThinToThickFunctionInst where tttf.callee is FunctionRefInst:
      return tttf
    // TODO: figure out what to do with non-inout indirect arguments
    // https://forums.swift.org/t/non-inout-indirect-types-not-supported-in-closure-specialization-optimization/70826
    case let pai as PartialApplyInst
    where pai.callee is FunctionRefInst && pai.hasOnlyInoutIndirectArguments:
      return pai
    default:
      return nil
    }
  }

  var asSubsetThunk: PartialApplyInst? {
    guard let pai = self as? PartialApplyInst,
          pai.argumentOperands.singleElement != nil,
          let function = pai.referencedFunction,
          function.bridged.isAutodiffSubsetParametersThunk()
    else {
      return nil
    }
    return pai
  }

  var asOptionalWrapper: EnumInst? {
    guard let ei = self as? EnumInst,
          ei.operands.singleElement != nil,
          ei.type.isOptional
    else {
      return nil
    }
    return ei
  }
}

// For a closure which might potentially be a part of a branch tracing enum payload tuple
// (or a part of multiple such tuples) find actual uses in these tuples. Note that at this point
// we do not support cases when closure has any unexpected uses which are not branch tracing
// enum payloads. So, if any non-BTE payload use is found, we consider the given closure
// non-specializable and return no uses as for now.
//
// Also note that the closure might be used in a BTE payload not directly but after
// "wrapping" it in either an optional or an autodiff subset parameters thunk. If such a "wrapper"
// is detected, we only allow it as a single direct use of closure and then search for BTE payload
// uses of that wrapper.
//
// Considering the code below, the function will find:
// - for %closure1: uses in %payload11 and %payload12;
// - for %closure2: uses in %payload21 and %payload22 with %subsetThunk
// - for %closure3: uses in %payload31 and %payload32 with %optionalWrapper
//
//   %closure1 = partial_apply %foo1(...)
//   %payload11 = tuple (..., %closure1, ...)
//   %bte11 = enum $_AD__$xxx, #_AD__$xxx.bbXXX!enumelt, %payload11
//   %payload12 = tuple (..., %closure1, ...)
//   %bte12 = enum $_AD__$xxx, #_AD__$xxx.bbXXX!enumelt, %payload12
//
//   %closure2 = partial_apply %foo2(...) // user: %subsetThunk
//   // function_ref autodiff subset parameters thunk for ...
//   %subsetThunkFn = function_ref @$xxx
//   %subsetThunk = partial_apply %subsetThunkFn(%closure2)
//   %payload21 = tuple (..., %subsetThunk, ...)
//   %bte21 = enum $_AD__$xxx, #_AD__$xxx.bbXXX!enumelt, %payload21
//   %payload22 = tuple (..., %subsetThunk, ...)
//   %bte22 = enum $_AD__$xxx, #_AD__$xxx.bbXXX!enumelt, %payload22
//
//   %closure3 = thin_to_thick_function %foo3
//   %optionalWrapper12 = enum $Optional<...>, #Optional.some!enumelt, %closure3
//   %payload31 = tuple (..., %optionalWrapper, ...)
//   %bte31 = enum $_AD__$xxx, #_AD__$xxx.bbXXX!enumelt, %payload31
//   %payload32 = tuple (..., %optionalWrapper, ...)
//   %bte32 = enum $_AD__$xxx, #_AD__$xxx.bbXXX!enumelt, %payload32
private func findBTEUses(for rootClosure: SingleValueInstruction) -> [ClosureInBTE] {
  log("findBTEUses: running for \(rootClosure)")
  let vjp = rootClosure.parentFunction
  var closuresInBTE = [ClosureInBTE]()

  let subsetThunk = rootClosure.uses.singleElement?.instruction.asSubsetThunk
  let optionalWrapper = rootClosure.uses.singleElement?.instruction.asOptionalWrapper
  assert(subsetThunk == nil || optionalWrapper == nil)
  let closure = subsetThunk ?? (optionalWrapper ?? rootClosure)

  for use in closure.uses {
    guard let ti = use.instruction as? TupleInst else {
      log("findBTEUses: unexpected use of closure, aborting: \(use)")
      return []
    }
    var closureInBTEForPayloadArr = [ClosureInBTE]()
    for tiUse in ti.uses {
      guard let ei = tiUse.instruction as? EnumInst else {
        log("findBTEUses: unexpected use of payload tuple, aborting: \(tiUse)")
        return []
      }
      guard ei.type.isBranchTracingEnum(in: vjp) else {
        log("findBTEUses: enum type \(ei.type) is not a " +
            "branch tracing enum in VJP \(vjp.name), aborting")
        return []
      }
      let enumCase = ei.type.getEnumCases(in: vjp)![ei.caseIndex]!
      let closureInBTE = ClosureInBTE(
        closure: rootClosure,
        subsetThunk: subsetThunk,
        optionalWrapper: optionalWrapper,
        useInPayload: use,
        enumCase: enumCase
      )
      // Avoid duplicates in case when multiple identical enum instructions accept `ti` as payload
      if !closureInBTEForPayloadArr.contains(closureInBTE) {
        closureInBTEForPayloadArr.append(closureInBTE)
      }
    }
    closuresInBTE.append(contentsOf: closureInBTEForPayloadArr)
  }
  log("findBTEUses: found \(closuresInBTE.count) uses of the following closure in branch tracing enums: \(rootClosure)")
  return closuresInBTE
}

private func findClosuresInBTE(paiOfPullback: PartialApplyInst) -> [ClosureInBTE] {
  let vjp = paiOfPullback.parentFunction
  var subsetThunks = Set<SingleValueInstruction>()
  var closuresInBTE = [ClosureInBTE]()
  for inst in vjp.instructions {
    guard inst != paiOfPullback,
          let rootClosure = inst.asSupportedClosure,
          !subsetThunks.contains(rootClosure)
    else {
      continue
    }

    let currentClosuresInBTE = findBTEUses(for: rootClosure)
    closuresInBTE.append(contentsOf: currentClosuresInBTE)
    subsetThunks.formUnion(closuresInBTE.filter{ $0.subsetThunk != nil }.map{ $0.subsetThunk! })
  }

  closuresInBTE.append(contentsOf:
    findOptionalNoneMatchingOptionalSome(in: vjp, closuresInBTE: closuresInBTE))

  return closuresInBTE
}

private func getSpecializedBTEDict(closuresInBTE: [ClosureInBTE], paiOfPullback: PartialApplyInst, _ context: FunctionPassContext) -> [Type: Type] {
  guard !closuresInBTE.isEmpty else {
    return [:]
  }

  let vjp = paiOfPullback.parentFunction
  let pullback = paiOfPullback.referencedFunction!
  let enumTypeOfEntryBBArg = pullback.entryBlock.getBranchTracingEnumArg(vjp: vjp)!.type

  return autodiffSpecializeBranchTracingEnums(
    topVJP: vjp, topBTE: enumTypeOfEntryBBArg,
    closuresInBTE: closuresInBTE, context: context)
}

private struct AutoDiffSpecializationInfo {
  let paiOfPullback: PartialApplyInst
  let closuresInBTE: [ClosureInBTE]
  let specializedBTEDict: [Type: Type]

  var vjp: Function { paiOfPullback.parentFunction }
  var pullback: Function { paiOfPullback.referencedFunction! }

  init(vjp: Function, _ context: FunctionPassContext) {
    self.paiOfPullback = getPartialApplyOfPullbackInExitVJPBB(vjp: vjp)!
    self.closuresInBTE = findClosuresInBTE(paiOfPullback: self.paiOfPullback)
    self.specializedBTEDict = getSpecializedBTEDict(
      closuresInBTE: self.closuresInBTE, paiOfPullback: self.paiOfPullback, context)
  }
}

let getAutoDiffSpecializationInfoTest = FunctionTest("autodiff_get_specialization_info") {
  function, arguments, context in
  let autodiffSpecializationInfo = AutoDiffSpecializationInfo(vjp: function, context)
  print("Run getAutoDiffSpecializationInfo for VJP \(function.name): autodiffSpecializationInfo = (")
  print("  pullback = \(autodiffSpecializationInfo.pullback.name)")
  print("  closuresInBTE = [")
  for closureInBTE in autodiffSpecializationInfo.closuresInBTE {
    print("    ClosureInBTE(")
    print("      closure: \(closureInBTE.closure)")
    print("      subsetThunk: " + (closureInBTE.subsetThunk == nil ? "nil" : "\(closureInBTE.subsetThunk!)"))
    print("      optionalWrapper: " + (closureInBTE.optionalWrapper == nil ? "nil" : "\(closureInBTE.optionalWrapper!)"))
    print("      useInPayload: \(closureInBTE.useInPayload)")
    let enumCase = closureInBTE.enumCase
    print("      enumCase: \(enumCase.enumType(in: function)).\(enumCase.name)")
    print("    )")
  }
  print("  ]\n)\n")
}

private func specializeBranchTracingEnumBBArgInVJP(
  arg: Argument, specializedBTEDict: [Type: Type], context: FunctionPassContext
) -> Argument {
  let bb = arg.parentBlock
  assert(specializedBTEDict[arg.type] != nil)
  let newType = specializedBTEDict[arg.type]!
  return bb.insertPhiArgument(
    atPosition: arg.index, type: newType, ownership: arg.ownership, context)
}

let specializeBranchTracingEnums = FunctionTest("autodiff_specialize_branch_tracing_enums") {
  function, arguments, context in
  let autodiffSpecializationInfo = AutoDiffSpecializationInfo(vjp: function, context)
  let specializedBTEDict = autodiffSpecializationInfo.specializedBTEDict
  print("Specialized branch tracing enum dict for VJP \(function.name) contains \(specializedBTEDict.count) elements:")

  var keys = [Type](specializedBTEDict.keys)

  func compareTypes(lhs: Type, rhs: Type) -> Bool { "\(lhs)" < "\(rhs)" }
  keys.sort(by: compareTypes)

  for (idx, key) in keys.enumerated() {
    print("non-specialized BTE \(idx): \(key.nominal!.description)")
    print("specialized BTE \(idx): \(specializedBTEDict[key]!.nominal!.description)")
  }
  print("")
}

let specializeBTEArgInVjpBB = FunctionTest("autodiff_specialize_bte_arg_in_vjp_bb") {
  function, arguments, context in
  let autodiffSpecializationInfo = AutoDiffSpecializationInfo(vjp: function, context)
  let specializedBTEDict = autodiffSpecializationInfo.specializedBTEDict

  print("Specialized BTE arguments of basic blocks in VJP \(function.name):")
  for bb in function.blocks {
    guard let arg = bb.getBranchTracingEnumArg(vjp: function) else {
      continue
    }
    let newArg = specializeBranchTracingEnumBBArgInVJP(
      arg: arg, specializedBTEDict: specializedBTEDict, context: context)
    print("\(newArg)")
    bb.eraseArgument(at: newArg.index, context)
  }
  print("")
}

private func specializePayloadTupleBBArgInPullback(
  arg: Argument,
  enumCase: EnumCase,
  context: FunctionPassContext
) -> Argument {
  let bb = arg.parentBlock
  let newPayloadTupleTy = enumCase.payload!

  return bb.insertPhiArgument(
    atPosition: arg.index, type: newPayloadTupleTy, ownership: arg.ownership, context)
}

let specializePayloadArgInPullbackBB = FunctionTest("autodiff_specialize_payload_arg_in_pb_bb") {
  function, arguments, context in
  let autodiffSpecializationInfo = AutoDiffSpecializationInfo(vjp: function, context)
  let specializedBTEDict = autodiffSpecializationInfo.specializedBTEDict

  let pullback = autodiffSpecializationInfo.pullback
  print("Specialized BTE payload arguments of basic blocks in pullback \(pullback.name):")
  for bb in pullback.blocks {
    guard
      let (arg, enumCase, throwingSuccessor) = getBTEPayloadArgOfPbBBInfo(bb, vjp: function)
    else {
      continue
    }

    let enumType = specializedBTEDict[enumCase.enumType(in: function)]!
    let newArg = specializePayloadTupleBBArgInPullback(
      arg: arg,
      enumCase: enumType.getEnumCases(in: function)![enumCase.index]!,
      context: context)
    print("\(newArg)")
    bb.eraseArgument(at: newArg.index, context)

    if let successor = throwingSuccessor {
      let newArg = specializeOptionalBBArgInPullback(
        bb: successor,
        newOptionalType: enumType.getEnumCases(in: pullback)![enumCase.index]!.payload!.tupleElements.last!,
        context: context)
      print("\(newArg)")
      successor.eraseArgument(at: newArg.index, context)
    }
  }
  print("")
}

private func specializeOptionalBBArgInPullback(
  bb: BasicBlock,
  newOptionalType: Type,
  context: FunctionPassContext
) -> Argument {
  let arg = bb.arguments.singleElement!

  let underlyingType = newOptionalType.rawType.optionalObjectType.loweredTypeWithAbstractionPattern(
    in: bb.parentFunction)

  return bb.insertPhiArgument(
    atPosition: arg.index, type: underlyingType, ownership: arg.ownership, context)
}
