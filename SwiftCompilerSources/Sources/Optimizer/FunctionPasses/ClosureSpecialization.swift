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

  let specializedFunction = specialization.getOrCreateSpecializedFunction(context)

  specialization.unUniqueCaptureArguments(context)

  specialization.rewriteApply(for: specializedFunction, context)

  specialization.deleteDeadClosures(context)

  return true
}

private func isCalleeSpecializable(of apply: ApplySite) -> Bool {
  if let callee = apply.referencedFunction,
     callee.isDefinition,

     // We don't support generic functions (yet)
     !apply.hasSubstitutions,

     // Don't specialize non-fragile (read as non-serialized) callees if the caller is fragile; the
     // specialized callee will have shared linkage, and thus cannot be referenced from the fragile caller.
     !(apply.parentFunction.isSerialized && !callee.isSerialized),

     // If the callee uses a dynamic Self, we cannot specialize it, since the resulting specialization
     // might no longer have 'self' as the last parameter.
     //
     // TODO: Keep the self argument the last when appending arguments.
     !callee.mayBindDynamicSelf
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

  func getOrCreateSpecializedFunction(_ context: FunctionPassContext) -> Function {
    let specializedFunctionName = getSpecializedFunctionName(context)

    if let existingSpecializedFunction = context.lookupFunction(name: specializedFunctionName) {
      return existingSpecializedFunction
    }

    let specializedParameters = getSpecializedParameters()

    let specializedFunction =
      context.createSpecializedFunctionDeclaration(
        from: callee, withName: specializedFunctionName,
        withParams: specializedParameters,
        // The specialized function is always a thin function. This is important because we add additional
        // parameters after the Self parameter of witness methods. In this case the new function is not a
        // method anymore.
        makeThin: true, makeBare: true)

    context.buildSpecializedFunction(
      specializedFunction: specializedFunction,
      buildFn: { (specializedFunction, specializedContext) in
        var cloner = Cloner(cloneToEmptyFunction: specializedFunction, specializedContext)
        defer { cloner.deinitialize() }

        cloneAndSpecializeFunctionBody(using: &cloner)
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

  private func getSpecializedParameters() -> [ParameterInfo] {
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
      cloner.recordFoldedValue(originalArg, mappedTo: clonedArg)

      return clonedArg
    }
  }

  func rewriteApply(for specializedFunction: Function, _ context: FunctionPassContext) {
    insertCompensatingDestroysForOwnedClosureArguments(context)

    let newApplyArgs = getNewApplyArguments(context)

    let builder = Builder(before: apply, context)
    let funcRef = builder.createFunctionRef(specializedFunction)

    switch apply {
    case let oldPartialApply as PartialApplyInst:
      let newPartialApply = builder.createPartialApply(
        function: funcRef, substitutionMap: SubstitutionMap(),
        capturedArguments: newApplyArgs, calleeConvention: oldPartialApply.calleeConvention,
        hasUnknownResultIsolation: oldPartialApply.hasUnknownResultIsolation,
        isOnStack: oldPartialApply.isOnStack)
      oldPartialApply.replace(with: newPartialApply, context)

    case let oldApply as ApplyInst:
      let newApply = builder.createApply(function: funcRef, SubstitutionMap(), arguments: newApplyArgs,
                                         isNonThrowing: oldApply.isNonThrowing,
                                         isNonAsync: oldApply.isNonAsync)
      oldApply.replace(with: newApply, context)

    case let oldTryApply as TryApplyInst:
      builder.createTryApply(function: funcRef, SubstitutionMap(), arguments: newApplyArgs,
                             normalBlock: oldTryApply.normalBlock, errorBlock: oldTryApply.errorBlock,
                             isNonAsync: oldTryApply.isNonAsync)
      context.erase(instruction: oldTryApply)

    case let oldBeginApply as BeginApplyInst:
      let newApply = builder.createBeginApply(function: funcRef, SubstitutionMap(), arguments: newApplyArgs,
                                              isNonThrowing: oldBeginApply.isNonThrowing,
                                              isNonAsync: oldBeginApply.isNonAsync)
      oldBeginApply.replace(with: newApply, context)
    default:
      fatalError("unknown apply")
    }
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
    findValuesWhichNeedDestroyRecursively(value: clonedArg, needDestroy: &needDestroy)
  }

  while let valueToDestroy = needDestroy.pop() {
    Builder.insertCleanupAtFunctionExits(of: valueToDestroy.parentFunction, context) { builder in
      builder.createDestroyValue(operand: valueToDestroy)
    }
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

private extension Function {
  var effectAllowsSpecialization: Bool {
    switch self.effectAttribute {
    case .readNone, .readOnly, .releaseNone: return false
    default: return true
    }
  }
}

extension Collection {
  func getExactlyOneOrNil() -> Element? {
    assert(self.count <= 1)
    return self.first
  }
}

extension Type: Hashable {
  func isBranchTracingEnumIn(vjp: Function) -> Bool {
    return self.bridged.isAutodiffBranchTracingEnumInVJP(vjp.bridged)
  }

  public func hash(into hasher: inout Hasher) {
    hasher.combine(bridged.opaqueValue)
  }
}

extension BasicBlock {
  fileprivate func getBranchTracingEnumArg(vjp: Function) -> Argument? {
    return self.arguments.filter { $0.type.isBranchTracingEnumIn(vjp: vjp) }.getExactlyOneOrNil()
  }
}

typealias ClosureInfoMultiBB = (
  closure: SingleValueInstruction,
  capturedArgs: [Value],
  subsetThunk: PartialApplyInst?,
  payloadTuple: TupleInst,
  idxInPayload: Int,
  enumTypeAndCase: EnumTypeAndCase
)

typealias EnumTypeAndCase = (
  enumType: Type,
  caseIdx: Int
)

typealias BTEToPredsDict = [SIL.`Type`: [SIL.`Type`]]

typealias BTECaseToClosureListDict = [Int: [ClosureInfoMultiBB]]

typealias SpecBTEDict = [SIL.`Type`: SIL.`Type`]

func getCapturedArgTypesTupleForClosure(
  closure: SingleValueInstruction, context: FunctionPassContext
) -> AST.`Type` {
  var capturedArgTypes = [AST.`Type`]()
  if let pai = closure as? PartialApplyInst {
    for arg in pai.arguments {
      capturedArgTypes.append(arg.type.rawType)
    }
  } else if closure as? ThinToThickFunctionInst != nil {
    // Nothing captured
  } else {
    assert(false)
  }
  return context.getTupleType(elements: capturedArgTypes)
}

func getBranchTracingEnumPreds(bteType: SIL.`Type`, vjp: Function) -> [SIL.`Type`] {
  var btePreds = [SIL.`Type`]()
  guard let enumCases = bteType.getEnumCases(in: vjp) else {
    return btePreds
  }
  for enumCase in enumCases {
    let payloadType: SIL.`Type` = enumCase.payload!
    if payloadType.tupleElements.count == 0 {
      continue
    }
    let firstTupleElementType: SIL.`Type` = payloadType.tupleElements[0]
    if firstTupleElementType.isBranchTracingEnumIn(vjp: vjp) {
      btePreds.append(firstTupleElementType)
    }
  }
  return btePreds
}

func iterateOverBranchTracingEnumPreds(
  bteToPredsDict: inout BTEToPredsDict,
  currentBTEType: SIL.`Type`,
  vjp: Function
) {
  let currentBTEPreds: [SIL.`Type`] = getBranchTracingEnumPreds(bteType: currentBTEType, vjp: vjp)
  bteToPredsDict[currentBTEType] = currentBTEPreds
  for currentBTEPred in currentBTEPreds {
    if !bteToPredsDict.keys.contains(currentBTEPred) {
      iterateOverBranchTracingEnumPreds(
        bteToPredsDict: &bteToPredsDict, currentBTEType: currentBTEPred, vjp: vjp)
    }
  }
}

func getBranchTracingEnumQueue(topBTEType: SIL.`Type`, vjp: Function) -> [SIL.`Type`] {
  var bteToPredsDict = BTEToPredsDict()
  iterateOverBranchTracingEnumPreds(
    bteToPredsDict: &bteToPredsDict,
    currentBTEType: topBTEType,
    vjp: vjp)
  var bteQueue = [SIL.`Type`]()
  let totalEnums = bteToPredsDict.count

  for i in 0..<totalEnums {
    for bteType in bteToPredsDict.keys {
      let btePreds: [SIL.`Type`] = bteToPredsDict[bteType]!
      if btePreds.count != 0 {
        continue
      }
      assert(!bteQueue.contains(bteType))
      bteQueue.append(bteType)
      break
    }
    assert(bteQueue.count == i + 1)
    bteToPredsDict.removeValue(forKey: bteQueue.last!)
    for bteType in bteToPredsDict.keys {
      let btePreds: [SIL.`Type`] = bteToPredsDict[bteType]!
      bteToPredsDict[bteType] = btePreds.filter { $0 != bteQueue.last! }
    }
  }
  assert(bteQueue.count == totalEnums)

  return bteQueue
}

// NOTE: this is adopted from
// lib/SILOptimizer/Differentiation/PullbackCloner.cpp.
/// Remap any archetypes into the current function's context.
func remapType(ty: SIL.`Type`, function: Function) -> SIL.`Type` {
  var silType = ty
  if silType.rawType.hasArchetype {
    silType = silType.mapTypeOutOfContext(in: function)
  }
  let remappedCanType = silType.rawType.getReducedType(
    of: function.loweredFunctionType.substitutedGenericSignatureOfFunctionType.genericSignature)
  let remappedSILType = remappedCanType.loweredType(in: function)
  if !function.genericSignature.isEmpty {
    return function.mapTypeIntoContext(remappedSILType)
  }
  return remappedSILType
}

func getBranchingTraceEnumLoweredType(ed: Decl, vjp: Function) -> SIL.`Type` {
  ed.bridged.getAs(NominalTypeDecl.self).declaredInterfaceType.canonical.loweredType(in: vjp)
}

func getSourceFileFor(derivative: Function) -> SourceFile {
  if let sourceFile = derivative.sourceFile {
    return sourceFile
  }
  return derivative.bridged.getFilesForModule().withElements(ofType: FileUnit.self) {
    for fileUnit in $0 {
      if let sourceFile = fileUnit.asSourceFile {
        return sourceFile
      }
    }
    assert(false)
    return nil
  }!
}

func cloneGenericParameters(
  astContext: ASTContext, declContext: DeclContext, canonicalGenericSig: CanonicalGenericSignature
) -> GenericParamList {
  var params: [BridgedGenericTypeParamDecl] = []
  for type in canonicalGenericSig.genericSignature.genericParameters {
    assert(type.isGenericTypeParameter)
    params.append(
      BridgedGenericTypeParamDecl.createImplicit(
        declContext: declContext,
        name: type.nameOfGenericTypeParameter,
        depth: type.depthOfGenericTypeParameter,
        index: type.indexOfGenericTypeParameter,
        paramKind: type.kindOfGenericTypeParameter))
  }
  return GenericParamList.createParsed(
    astContext, leftAngleLoc: nil, parameters: params,
    genericWhereClause: nil,
    rightAngleLoc: nil)
}

func autodiffSpecializeBranchTracingEnum(
  bteType: SIL.`Type`, topVJP: Function,
  bteCaseToClosureListDict: BTECaseToClosureListDict,
  specBTEDict: [SIL.`Type`: SIL.`Type`],
  context: FunctionPassContext
) -> SIL.`Type` {
  assert(specBTEDict[bteType] == nil)

  let oldED = bteType.nominal as! EnumDecl
  let declContext = oldED.declContext
  let astContext = declContext.astContext

  var newEDNameStr: String = oldED.name.string + "_spec"
  var newPLs = [ParameterList]()

  for enumCase in bteType.getEnumCases(in: topVJP)! {
    let oldPayloadTupleType: Type = enumCase.payload!
    let oldEED: EnumElementDecl = enumCase.enumElementDecl

    let oldPL: ParameterList = oldEED.parameterList
    assert(oldPL.size == 1)
    let oldPD: BridgedParamDecl = oldPL[0]

    let closureInfosMultiBB: [ClosureInfoMultiBB] = bteCaseToClosureListDict[enumCase.index] ?? []

    var newECDNameSuffix: String = ""
    var newPayloadTupleElementTypes = [(label: Identifier, type: AST.`Type`)]()

    for idxInPayloadTuple in 0..<oldPayloadTupleType.tupleElements.count {
      let label: Identifier = oldPayloadTupleType.tupleElements.label(at: idxInPayloadTuple)
      var newPayloadTupleEltType = AST.`Type`?(nil)
      if let closureInfoMultiBB = closureInfosMultiBB.first(where: {
        $0.idxInPayload == idxInPayloadTuple
      }) {
        newECDNameSuffix += "_\(idxInPayloadTuple)"
        newPayloadTupleEltType = getCapturedArgTypesTupleForClosure(
          closure: closureInfoMultiBB.closure, context: context)
      } else {
        newPayloadTupleEltType = oldPayloadTupleType.tupleElements[idxInPayloadTuple].rawType
        if idxInPayloadTuple == 0
          && oldPayloadTupleType.tupleElements[idxInPayloadTuple].isBranchTracingEnumIn(vjp: topVJP)
        {
          let predED = newPayloadTupleEltType!.nominal as! EnumDecl
          let predBTEType: SIL.`Type` = remapType(
            ty: getBranchingTraceEnumLoweredType(ed: predED, vjp: topVJP),
            function: topVJP)
          newPayloadTupleEltType = specBTEDict[predBTEType]!.rawType
        }
      }
      newPayloadTupleElementTypes.append((label: label, type: newPayloadTupleEltType!))
    }
    let newTupleType =
      context.getTupleType(elements: newPayloadTupleElementTypes)
      .mapTypeOutOfContext()

    let newPD = oldPD.cloneWithoutType()

    newPD.setInterfaceType(type: newTupleType)
    let newPL =
      ParameterList.createParsed(
        astContext, leftParenLoc: nil, parameters: [newPD],
        rightParenLoc: nil)
    newPLs.append(newPL)

    if newECDNameSuffix.count != 0 {
      newEDNameStr += "_\(oldEED.name)\(newECDNameSuffix)"
    }
  }

  let canonicalGenericSig = topVJP.genericSignature.canonicalSignature
  var genericParams = GenericParamList?(nil)
  if !canonicalGenericSig.isEmpty {
    genericParams = cloneGenericParameters(
      astContext: astContext, declContext: declContext, canonicalGenericSig: canonicalGenericSig
    )
  }

  let newED = BridgedEnumDecl.createParsed(
      astContext, declContext: declContext,
      enumKeywordLoc: nil,
      name: newEDNameStr,
      nameLoc: nil,
      genericParamList: genericParams,
      inheritedTypes: [],
      genericWhereClause: nil,
      braceRange: SourceRange(start: nil))

  newED.asDecl.setImplicit()
  if !canonicalGenericSig.isEmpty {
    newED.asGenericContext.setGenericSignature(canonicalGenericSig.genericSignature.bridged)
  }

  for (idx, enumCase) in bteType.getEnumCases(in: topVJP)!.enumerated() {
    let oldEED: EnumElementDecl = enumCase.enumElementDecl
    let newPL: ParameterList = newPLs[idx]
    let newEED = BridgedEnumElementDecl.createParsed(
      astContext, declContext: newED.asDeclContext,
      name: oldEED.baseIdentifier, nameLoc: nil,
      parameterList: newPL,
      equalsLoc: nil, rawValue: nil)
    newEED.asDecl.setImplicit()
    newED.asNominalTypeDecl.addMember(newEED.asDecl)
  }

  newED.asValueDecl.setAccess(swift.AccessLevel.public)
  getSourceFileFor(derivative: topVJP).addTopLevelDecl(newED.asDecl)

  let newEnumType: SIL.`Type` = remapType(
    ty: getBranchingTraceEnumLoweredType(ed: newED.asDecl.decl, vjp: topVJP),
    function: topVJP)

  return newEnumType
}

func autodiffSpecializeBranchTracingEnums(
  topVJP: Function, topBTE: SIL.`Type`, closureInfosMultiBB: [ClosureInfoMultiBB],
  context: FunctionPassContext
) -> SpecBTEDict {
  let bteQueue: [SIL.`Type`] = getBranchTracingEnumQueue(topBTEType: topBTE, vjp: topVJP)

  var specBTEDict = [SIL.`Type`: SIL.`Type`]()
  for bteType in bteQueue {
    let ed = bteType.nominal as! EnumDecl
    let silType = remapType(
      ty: getBranchingTraceEnumLoweredType(ed: ed, vjp: topVJP), function: topVJP)

    var bteCaseToClosureListDict = BTECaseToClosureListDict()
    for closureInfoMultiBB in closureInfosMultiBB {
      if closureInfoMultiBB.enumTypeAndCase.enumType != bteType {
        continue
      }
      if bteCaseToClosureListDict[closureInfoMultiBB.enumTypeAndCase.caseIdx] == nil {
        bteCaseToClosureListDict[closureInfoMultiBB.enumTypeAndCase.caseIdx] = []
      }
      bteCaseToClosureListDict[closureInfoMultiBB.enumTypeAndCase.caseIdx]!.append(
        closureInfoMultiBB)
    }

    specBTEDict[silType] = autodiffSpecializeBranchTracingEnum(
      bteType: silType, topVJP: topVJP, bteCaseToClosureListDict: bteCaseToClosureListDict,
      specBTEDict: specBTEDict, context: context)
  }

  return specBTEDict
}

private func getPartialApplyOfPullbackInExitVJPBB(vjp: Function) -> PartialApplyInst? {
  log("getPartialApplyOfPullbackInExitVJPBB: running for VJP \(vjp.name)")
  guard let exitBB = vjp.blocks.filter({ $0.terminator as? ReturnInst != nil }).getExactlyOneOrNil()
  else {
    log("getPartialApplyOfPullbackInExitVJPBB: exit BB not found, aborting")
    return nil
  }

  let ri = exitBB.terminator as! ReturnInst
  guard let retValDefiningInstr = ri.returnedValue.definingInstruction else {
    log(
      "getPartialApplyOfPullbackInExitVJPBB: return value is not defined by an instruction, aborting"
    )
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
      log(
        "getPartialApplyOfPullbackInExitVJPBB: fromFunction operand of convert_function instruction is not defined by partial_apply instruction, aborting"
      )
      return nil
    }
    log("getPartialApplyOfPullbackInExitVJPBB: unexpected instruction type, aborting")
    return nil
  }

  if let ti = retValDefiningInstr as? TupleInst {
    log("getPartialApplyOfPullbackInExitVJPBB: return value is defined by tuple instruction")
    if ti.operands.count < 2 {
      log(
        "getPartialApplyOfPullbackInExitVJPBB: tuple instruction has \(ti.operands.count) operands, but at least 2 expected, aborting"
      )
      return nil
    }
    guard let lastTupleElemDefiningInst = ti.operands.last!.value.definingInstruction else {
      log(
        "getPartialApplyOfPullbackInExitVJPBB: last tuple element is not defined by an instruction, aborting"
      )
      return nil
    }
    return handleConvertFunctionOrPartialApply(inst: lastTupleElemDefiningInst)
  }

  return handleConvertFunctionOrPartialApply(inst: retValDefiningInstr)
}

private func getPullbackClosureInfoMultiBB(in vjp: Function, _ context: FunctionPassContext)
  -> PullbackClosureInfo
{
  let paiOfPbInExitVjpBB = getPartialApplyOfPullbackInExitVJPBB(vjp: vjp)!
  var pullbackClosureInfo = PullbackClosureInfo(paiOfPullback: paiOfPbInExitVjpBB)
  var subsetThunkArr = [SingleValueInstruction]()

  for inst in vjp.instructions {
    if inst == paiOfPbInExitVjpBB {
      continue
    }
    if inst.asSupportedClosure == nil {
      continue
    }

    let rootClosure = inst.asSupportedClosure!
    if subsetThunkArr.contains(rootClosure) {
      continue
    }

    let closureInfoArr = handleNonAppliesMultiBB(for: rootClosure, context)
    pullbackClosureInfo.closureInfosMultiBB.append(contentsOf: closureInfoArr)
    subsetThunkArr.append(
      contentsOf: closureInfoArr.filter { $0.subsetThunk != nil }.map { $0.subsetThunk! })
  }

  return pullbackClosureInfo
}

typealias BTEPayloadArgOfPbBBWithBTETypeAndCase = (arg: Argument, enumTypeAndCase: EnumTypeAndCase)

// If the pullback's basic block has an argument which is a payload tuple of the
// branch tracing enum corresponding to the given VJP, return this argument and any valid combination
// of a branch tracing enum type and its case index having the same payload tuple type as the argument.
// The function assumes that no more than one such argument is present.
private func getBTEPayloadArgOfPbBBWithBTETypeAndCase(_ bb: BasicBlock, vjp: Function)
  -> BTEPayloadArgOfPbBBWithBTETypeAndCase?
{
  log(
    "getBTEPayloadArgOfPbBBWithBTETypeAndCase: basic block \(bb.shortDescription) in pullback \(bb.parentFunction.name)"
  )
  guard let predBB = bb.predecessors.first else {
    log("getBTEPayloadArgOfPbBBWithBTETypeAndCase: the bb has no predecessors, aborting")
    return nil
  }

  log("getBTEPayloadArgOfPbBBWithBTETypeAndCase: start iterating over bb args")
  for arg in bb.arguments {
    log("getBTEPayloadArgOfPbBBWithBTETypeAndCase: \(arg)")
    if !arg.type.isTuple {
      log("getBTEPayloadArgOfPbBBWithBTETypeAndCase: arg is not a tuple, skipping")
      continue
    }

    if let bi = predBB.terminator as? BranchInst {
      log("getBTEPayloadArgOfPbBBWithBTETypeAndCase: terminator of pred bb is branch instruction")
      guard let uedi = bi.operands[arg.index].value.definingInstruction as? UncheckedEnumDataInst
      else {
        log(
          "getBTEPayloadArgOfPbBBWithBTETypeAndCase: operand corresponding to the argument is not defined by unchecked_enum_data instruction"
        )
        continue
      }
      let enumType = uedi.`enum`.type
      if !enumType.isBranchTracingEnumIn(vjp: vjp) {
        log(
          "getBTEPayloadArgOfPbBBWithBTETypeAndCase: enum type \(enumType) is not a branch tracing enum in VJP \(vjp.name)"
        )
        continue
      }

      log("getBTEPayloadArgOfPbBBWithBTETypeAndCase: success")
      return BTEPayloadArgOfPbBBWithBTETypeAndCase(
        arg: arg,
        enumTypeAndCase: (
          enumType: enumType,
          caseIdx: uedi.caseIndex
        )
      )
    }

    if let sei = predBB.terminator as? SwitchEnumInst {
      log(
        "getBTEPayloadArgOfPbBBWithBTETypeAndCase: terminator of pred bb is switch_enum instruction"
      )
      let enumType = sei.enumOp.type
      if !enumType.isBranchTracingEnumIn(vjp: vjp) {
        log(
          "getBTEPayloadArgOfPbBBWithBTETypeAndCase: enum type \(enumType) is not a branch tracing enum in VJP \(vjp.name)"
        )
        continue
      }

      log("getBTEPayloadArgOfPbBBWithBTETypeAndCase: success")
      return BTEPayloadArgOfPbBBWithBTETypeAndCase(
        arg: arg,
        enumTypeAndCase: (
          enumType: enumType,
          caseIdx: sei.getUniqueCase(forSuccessor: bb)!
        )
      )
    }
  }

  log(
    "getBTEPayloadArgOfPbBBWithBTETypeAndCase: finish iterating over bb args; branch tracing enum arg not found"
  )
  return nil
}

extension PartialApplyInst {
  func isSubsetThunk() -> Bool {
    if self.argumentOperands.singleElement == nil {
      return false
    }
    guard let function = self.referencedFunction else {
      return false
    }
    return function.bridged.isAutodiffSubsetParametersThunk()
  }
}

private func handleNonAppliesMultiBB(
  for rootClosure: SingleValueInstruction,
  _ context: FunctionPassContext
)
  -> [ClosureInfoMultiBB]
{
  log("handleNonAppliesMultiBB: running for \(rootClosure)")
  let vjp = rootClosure.parentFunction
  var closureInfoArr = [ClosureInfoMultiBB]()

  var closure = rootClosure
  var subsetThunk = PartialApplyInst?(nil)
  if rootClosure.uses.singleElement != nil {
    if let pai = closure.uses.singleElement!.instruction as? PartialApplyInst {
      if pai.isSubsetThunk() {
        log("handleNonAppliesMultiBB: found subset thunk \(pai)")
        subsetThunk = pai
        closure = pai
      }
    }
  }

  for use in closure.uses {
    guard let ti = use.instruction as? TupleInst else {
      log("handleNonAppliesMultiBB: unexpected use of closure, aborting: \(use)")
      return []
    }
    for tiUse in ti.uses {
      guard let ei = tiUse.instruction as? EnumInst else {
        log("handleNonAppliesMultiBB: unexpected use of payload tuple, aborting: \(tiUse)")
        return []
      }
      if !ei.type.isBranchTracingEnumIn(vjp: vjp) {
        log(
          "handleNonAppliesMultiBB: enum type \(ei.type) is not a branch tracing enum in VJP \(vjp.name), aborting"
        )
        return []
      }
      var capturedArgs = [Value]()
      if let pai = rootClosure as? PartialApplyInst {
        capturedArgs = pai.argumentOperands.map { $0.value }
      }
      log(
        "handleNonAppliesMultiBB: creating closure info with enum type \(ei.type), case index \(ei.caseIndex), index in payload tuple \(use.index) and payload tuple \(ti)"
      )
      let enumTypeAndCase = (enumType: ei.type, caseIdx: ei.caseIndex)
      closureInfoArr.append(
        ClosureInfoMultiBB(
          closure: rootClosure,
          capturedArgs: capturedArgs,
          subsetThunk: subsetThunk,
          payloadTuple: ti,
          idxInPayload: use.index,
          enumTypeAndCase: enumTypeAndCase
        ))
    }
  }
  log(
    "handleNonAppliesMultiBB: created \(closureInfoArr.count) closure info entries for \(rootClosure)"
  )
  return closureInfoArr
}

extension Instruction {
  fileprivate var asSupportedClosure: SingleValueInstruction? {
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
  fileprivate var isSupportedClosure: Bool {
    asSupportedClosure != nil
  }
}

/// Represents a partial_apply of pullback capturing one or more closure arguments.
private struct PullbackClosureInfo {
  let paiOfPullback: PartialApplyInst
  var closureInfosMultiBB: [ClosureInfoMultiBB] = []

  init(paiOfPullback: PartialApplyInst) {
    self.paiOfPullback = paiOfPullback
  }
  var pullbackFn: Function {
    paiOfPullback.referencedFunction!
  }
}

let getPullbackClosureInfoMultiBBTest = FunctionTest(
  "autodiff_closure_specialize_get_pullback_closure_info_multi_bb"
) {
  function, arguments, context in
  let pullbackClosureInfo = getPullbackClosureInfoMultiBB(in: function, context)
  print("Run getPullbackClosureInfoMultiBB for VJP \(function.name): pullbackClosureInfo = (")
  print("  pullbackFn = \(pullbackClosureInfo.pullbackFn.name)")
  print("  closureInfosMultiBB = [")
  for closureInfoMultiBB in pullbackClosureInfo.closureInfosMultiBB {
    print("    ClosureInfoMultiBB(")
    print("      closure: \(closureInfoMultiBB.closure)")
    print("      capturedArgs: [")
    for capturedArg in closureInfoMultiBB.capturedArgs {
      print("      \(capturedArg)")
    }
    print("      ]")
    let subsetThunkStr =
      (closureInfoMultiBB.subsetThunk == nil ? "nil" : "\(closureInfoMultiBB.subsetThunk!)")
    print("      subsetThunk: \(subsetThunkStr)")
    print("      payloadTuple: \(closureInfoMultiBB.payloadTuple)")
    print("      idxInPayload: \(closureInfoMultiBB.idxInPayload)")
    print("      enumTypeAndCase: \(closureInfoMultiBB.enumTypeAndCase)")
    print("    )")
  }
  print("  ]\n)\n")
}

func getSpecBTEDict(vjp: Function, context: FunctionPassContext) -> SpecBTEDict {
  let pullbackClosureInfo = getPullbackClosureInfoMultiBB(in: vjp, context)
  let pb = pullbackClosureInfo.pullbackFn
  let enumTypeOfEntryBBArg = pb.entryBlock.getBranchTracingEnumArg(vjp: vjp)!.type
  let enumDict = autodiffSpecializeBranchTracingEnums(
    topVJP: vjp, topBTE: enumTypeOfEntryBBArg,
    closureInfosMultiBB: pullbackClosureInfo.closureInfosMultiBB, context: context)
  return enumDict
}

func specializeBranchTracingEnumBBArgInVJP(
  arg: Argument, specBTEDict: SpecBTEDict, context: FunctionPassContext
) -> Argument {
  let bb = arg.parentBlock
  assert(specBTEDict[arg.type] != nil)
  let newType = specBTEDict[arg.type]!
  return bb.insertPhiArgument(
    atPosition: arg.index, type: newType, ownership: arg.ownership, context)
}

extension SIL.`Type`: Comparable {
  public static func < (lhs: SIL.`Type`, rhs: SIL.`Type`) -> Bool {
    return "\(lhs)" < "\(rhs)"
  }
}

let specializeBranchTracingEnums = FunctionTest("autodiff_specialize_branch_tracing_enums") {
  function, arguments, context in
  let enumDict = getSpecBTEDict(vjp: function, context: context)
  print(
    "Specialized branch tracing enum dict for VJP \(function.name) contains \(enumDict.count) elements:"
  )

  var keys = [SIL.`Type`](enumDict.keys)
  keys.sort()
  for (idx, key) in keys.enumerated() {
    print("non-specialized BTE \(idx): \(key.nominal!.description)")
    print("specialized BTE \(idx): \(enumDict[key]!.nominal!.description)")
  }
  print("")
}

let specializeBTEArgInVjpBB = FunctionTest("autodiff_specialize_bte_arg_in_vjp_bb") {
  function, arguments, context in
  let enumDict = getSpecBTEDict(vjp: function, context: context)
  print("Specialized BTE arguments of basic blocks in VJP \(function.name):")
  for bb in function.blocks {
    guard let arg = bb.getBranchTracingEnumArg(vjp: function) else {
      continue
    }
    let newArg = specializeBranchTracingEnumBBArgInVJP(
      arg: arg, specBTEDict: enumDict, context: context)
    print("\(newArg)")
    bb.eraseArgument(at: newArg.index, context)
  }
  print("")
}

func specializePayloadTupleBBArgInPullback(
  arg: Argument,
  enumTypeAndCase: EnumTypeAndCase,
  context: FunctionPassContext
) -> Argument {
  let bb = arg.parentBlock
  let newEnumType = enumTypeAndCase.enumType

  var newPayloadTupleTy = SIL.`Type`?(nil)
  for enumCase in newEnumType.getEnumCases(in: arg.parentFunction)! {
    if enumCase.index == enumTypeAndCase.caseIdx {
      newPayloadTupleTy = enumCase.payload!
      break
    }
  }
  assert(newPayloadTupleTy != nil)

  return bb.insertPhiArgument(
    atPosition: arg.index, type: newPayloadTupleTy!, ownership: arg.ownership, context)
}

let specializePayloadArgInPullbackBB = FunctionTest("autodiff_specialize_payload_arg_in_pb_bb") {
  function, arguments, context in
  let pullbackClosureInfo = getPullbackClosureInfoMultiBB(in: function, context)
  let pb = pullbackClosureInfo.pullbackFn
  let enumDict = getSpecBTEDict(vjp: function, context: context)

  print("Specialized BTE payload arguments of basic blocks in pullback \(pb.name):")
  for bb in pb.blocks {
    guard
      let (arg, enumTypeAndCase) = getBTEPayloadArgOfPbBBWithBTETypeAndCase(bb, vjp: function)
    else {
      continue
    }

    let enumType = enumDict[enumTypeAndCase.enumType]!
    let newArg = specializePayloadTupleBBArgInPullback(
      arg: arg,
      enumTypeAndCase: (enumType: enumType, caseIdx: enumTypeAndCase.caseIdx),
      context: context)
    print("\(newArg)")
    bb.eraseArgument(at: newArg.index, context)
  }
  print("")
}
