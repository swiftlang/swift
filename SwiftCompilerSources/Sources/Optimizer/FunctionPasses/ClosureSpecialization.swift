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

  runClosureSpecialization(function: function, isRootInvocation: true, context: context)
}

func runClosureSpecialization(function: Function, isRootInvocation: Bool, context: FunctionPassContext) {
  guard function.hasOwnership else {
    return
  }

  var remainingSpecializationRounds = 5

  repeat {
    var changed = false

    for inst in function.instructions {
      if let apply = inst as? FullApplySite {
        if trySpecialize(apply: apply, isRootInvocation: isRootInvocation, context) {
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
/// VJPs return the original result(s) and the Pullback. Pullbacks are essentially a chain
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
///   %3 = apply %2(%0): $(Float) -> Float
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

  let isSingleBB = function.blocks.singleElement != nil
  var bteSpecEligibility = BTESpecEligibility.ineligible

  if !isSingleBB {
    log(
      "\n\nTrying to run AutoDiff Closure Specialization pass on \(function.name.string)")
    bteSpecEligibility = checkIfCanRun(vjp: function, context: context)
    if bteSpecEligibility != .ineligible {
      log(
        "The VJP \(function.name.string) has passed the preliminary check. Proceeding to running the pass"
      )
    }
  }

  var remainingSpecializationRounds = 5

  repeat {
    var changed = false

    guard let returnInst = function.returnInstruction,
      let tupleInst = returnInst.returnedValue.definingInstruction as? TupleInst,
      let lastTupleOp = tupleInst.operands.last,
      let partialApplyInst = lastTupleOp.value.definingInstruction as? PartialApplyInst,
      partialApplyInst.uses.singleUse?.instruction == tupleInst
    else {
      break
    }

    if trySpecialize(apply: partialApplyInst, isRootInvocation: true, context) {
      changed = true
    }

    if context.needFixStackNesting {
      context.fixStackNesting(in: function)
    }

    if !changed {
      break
    }

    remainingSpecializationRounds -= 1
  } while remainingSpecializationRounds > 0

  if !isSingleBB && bteSpecEligibility == .eligible {
    guard let closureAnalysis = PullbackClosureAnalysis(vjp: function),
          closureAnalysis.validatedPayloadFlows != nil else { return }

    guard !closureAnalysis.closuresInBTE.isEmpty else {
      log(
        "Unable to detect closures to be specialized in \(function.name.string), skipping the pass")
      return
    }

    guard let specializedBTEDict = synthesizeSpecializedEnums(
      from: closureAnalysis, context: context)
    else { return }

    specializeAgainstBTE(
      closureAnalysis: closureAnalysis,
      specializedBTEDict: specializedBTEDict,
      function: function,
      context: context)
  }
}

private func trySpecialize(apply: ApplySite, isRootInvocation: Bool, _ context: FunctionPassContext) -> Bool {
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

  let (specializedFunction, isNewFunction) = specialization.getOrCreateSpecializedFunction(specializedParameters, context)

  specialization.unUniqueCaptureArguments(context)

  specialization.rewriteApply(for: specializedFunction, context)

  specialization.deleteDeadClosures(context)

  if isNewFunction && isRootInvocation {
    /// Further specialization rounds can leave the cloned callee holding a specializable closure and an `apply` which
    /// takes this closure as an argument. Consider the case when this specializable closure captures an argument from
    /// the cloned callee, and this argument by itself is a specializable closure constructed in caller and passed to
    /// the cloned callee as a parameter. Since the applied-check stops at `partial_apply` operands, a closure still
    /// sitting in the caller (`partial_apply` of `closure0` in example) only becomes specializable once the callee it
    /// is passed to has itself been specialized deeply enough for that closure to appear transitively full-applied.
    ///
    /// We specialze now rather than defer to separate closure specialization run: the caller's specialization loop does
    /// revisit its apply each round, but unless we mutate the cloned callee now the applied-check still fails and the
    /// loop makes no progress. So, by the time the closure specialization pass reaches the specialized callee (it's
    /// added to pass manager worklist via `notifyNewFunction`) and does specialization for it, the caller's specialization
    /// has already finished and its apply of callee is never revisited at all - stranding the `partial_apply` of `closure0`.
    ///
    /// Running `runClosureSpecialization` synchronously on the cloned callee we've just produced fully specializes it
    /// before the caller's next specialization round, so the caller re-inspects the same apply, now sees `closure0` as
    /// transitively applied, and specializes it - leaving no `partial_apply` of a specializable closure in the caller.
    /// This recursion terminates: recursive specialization run on the just-specialized callee is only triggered from the
    /// actual pass invocation (`isRootInvocation`), so we won't trigger specialization of callees nested deeper.
    ///
    /// Round 2 for caller: specialize apply of specialized_callee_r1.
    ///
    ///   caller(%0 : Float, %1 : Float) -> Float:
    ///     %9  = partial_apply @closure0(%0)  : (Float) -> Float
    ///     return apply @specialized_callee_r2(%1, %9)
    ///
    ///   specialized_callee_r2(%0 : Float, %1 : (Float) -> Float) -> Float:
    ///     %15 = partial_apply @closure1(%1) : (Float) -> Float
    ///     return apply @closure2(%0, %15)
    ///
    /// Recursive round 1 for specialized_callee_r2: specialize apply of closure2.
    ///
    ///   specialized_callee_r2(%0 : Float, %1 : (Float) -> Float) -> Float:
    ///     return apply @specialized_closure2_r1(%0, %1)
    ///
    ///   specialized_closure2_r1(%0 : Float, %1 : (Float) -> Float) -> Float:
    ///     %9 = apply @closure1(%0, %1) // <-- folded from _/ %3 = partial_apply @closure1(%1)
    ///                                  //                  \ %9 = apply %3(%0)
    ///     return %9
    ///
    /// Now `%9 = partial_apply @closure0(%0)` in caller is recognized as transitively applied in
    /// `specialized_callee_r2`->`closure1`, making it subject for specialization during the 3rd round.
    ///
    /// Round 3 for caller: specialize apply of specialized_callee_r2.
    ///
    ///   caller(%0 : Float, %1 : Float) -> Float:
    ///     return apply @specialized_callee_r3(%1, %0)
    ///
    ///   specialized_callee_r3(%0 : Float, %1 : Float) -> Float:
    ///     %9  = partial_apply @closure0(%1)  : (Float) -> Float
    ///     return apply @specialized_closure2_r1(%0, %9)
    ///
    /// Further specialization rounds of `specialized_callee_r3` and other functions are omitted.
    /// Final specialization result contains no `partial_apply` left:
    ///
    ///   caller(%0 : Float, %1 : Float) -> Float:
    ///     return apply @specialized_callee_final(%1, %0)
    ///   specialized_callee_final(%0 : Float, %1 : Float) -> Float:
    ///     return apply @specialized_closure2_final(%0, %1)
    ///   specialized_closure2_final(%0 : Float, %1 : Float) -> Float:
    ///     return apply @specialized_closure1_final(%0, %1)
    ///   specialized_closure1_final(%0 : Float, %1 : Float) -> Float:
    ///     return apply @closure0(%0, %1)

    context.buildSpecializedFunction(specializedFunction: specializedFunction) {
      (specializedFunction, specializedContext) in
      runClosureSpecialization(function: specializedFunction, isRootInvocation: false, context: specializedContext)
    }
  }

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
  var capturedDependencies = [CapturedDependency]()
  var rootClosuresAdded = InstructionSet(context)
  defer { rootClosuresAdded.deinitialize() }

  for argOp in apply.argumentOperands {
    var visited = ValueSet(context)
    defer { visited.deinitialize() }
    var argumentDependencies = [CapturedDependency]()
    if let closure = findSpecializableClosure(of: argOp.value, &visited, &argumentDependencies),
       // Ok, we know that we can perform the optimization but not whether or not the optimization
       // is profitable. Check if the closure is actually called in the callee (or in a function
       // called by the callee). This opens optimization opportunities, like inlining.
       isClosureApplied(apply.calleeArgument(of: argOp, in: apply.referencedFunction!)!)
    {
      argumentsToSpecialize.append((argOp, closure))
      capturedDependencies.append(contentsOf: argumentDependencies)
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
  return SpecializationInfo(apply: apply, closureArguments: argumentsToSpecialize, rootClosures: rootClosures,
                            capturedDependencies: capturedDependencies)
}

// Walks down the use-def chain of a function argument, recursively, to find a rootClosure.
private func findSpecializableClosure(of value: Value, _ visited: inout ValueSet,
                                      _ capturedDependencies: inout [CapturedDependency]) -> Closure? {
  visited.insert(value)

  let specializationLevelLimit = 2

  switch value {
  case is ConvertFunctionInst,
       is ConvertEscapeToNoEscapeInst,
       is MoveValueInst,
       is CopyValueInst:
    return findSpecializableClosure(of: (value as! UnaryInstruction).operand.value, &visited, &capturedDependencies)

  case let mdi as MarkDependenceInst:
    guard mdi.value.type.isNoEscapeFunction, mdi.value.type.isThickFunction else {
      return nil
    }
    guard let operandClosure = findSpecializableClosure(of: mdi.value, &visited, &capturedDependencies) else {
      return nil
    }
    // A base not in the closure's use-def chain must be a root-closure capture; record it for `uniqueCaptureArguments`.
    if !visited.contains(mdi.base) {
      guard let rootClosure = operandClosure as? PartialApplyInst,
            rootClosure.arguments.contains(where: { $0 == mdi.base })
      else {
        return nil
      }
      capturedDependencies.append((closure: rootClosure, markDependence: mdi))
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
    // The thunk's partial_apply is cloned into the specialized function. 
    // It may have substitutions that reference archetypes of the caller, bailout.
    if partialApply.isPartialApplyOfThunk,
       !partialApply.substitutionMap.replacementTypes.contains(where: { $0.hasArchetype })
    {
      // Keep the recorded dependencies only if the thunk's argument provides the root closure;
      // otherwise the thunk's partial_apply itself is tried as the root, below.
      var argumentDependencies = [CapturedDependency]()
      if let argumentClosure = findSpecializableClosure(of: partialApply.arguments[0], &visited,
                                                        &argumentDependencies) {
        capturedDependencies.append(contentsOf: argumentDependencies)
        return argumentClosure
      }
    }
    guard let callee = partialApply.referencedFunction,
          !partialApply.hasSubstitutions,

          // Captures are appended to the specialized function's parameters. A
          // dynamic-Self metadata argument must remain the last parameter.
          (!callee.isDefinition || !callee.mayBindDynamicSelf),

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
          partialApply.hasOnlySupportedIndirectArguments,

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

/// A `mark_dependence` whose base is a root-closure capture, paired with that capturing closure.
private typealias CapturedDependency = (closure: PartialApplyInst, markDependence: MarkDependenceInst)

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

  // `mark_dependence`s whose base is a root-closure capture. The base is redirected onto the
  // capture's cast in `uniqueCaptureArguments`.
  let capturedDependencies: [CapturedDependency]

  // The function to specialize
  var callee: Function { apply.referencedFunction! }

  private typealias Cloner = SIL.Cloner<FunctionPassContext>

  func getOrCreateSpecializedFunction(_ specializedParameters: [ParameterInfo],
                                      _ context: FunctionPassContext
  ) -> (Function, isNewFunction: Bool) {
    let specializedFunctionName = getSpecializedFunctionName(context)

    if let existingSpecializedFunction = context.lookupFunction(name: specializedFunctionName) {
      return (existingSpecializedFunction, false)
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

    return (specializedFunction, true)
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

      for (argOp, paramInfo) in zip(partialApply.argumentOperands,
                                     closureConvention.parameters[unappliedArgumentCount...]) {
        let newParamInfo = paramInfo.withSpecializedConvention(for: argOp, in: partialApply, callee: callee)
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

    /// When we have several partial_apply instructions capturing each other in a chain,
    /// we might be able to perform several specialization rounds. See also test/SILOptimizer/closure_specialization_nested.sil.
    ///
    /// Whether a closure gets specialized depends on whether it is fully applied, possibly
    /// transitively, in the callee (the gate is in `analyzeArguments` via `isClosureApplied`).
    /// The goal across all rounds is to leave no specializable closures left in the caller.
    ///
    /// When `%15 = partial_apply @closure2(%12)` from caller is cloned into the specialized callee it comes back as a
    /// `partial_apply @closure2(%1)` serving as a callee of the further `apply`, so specialized callee argument `%1`
    /// which is by itself a closure `%12 = partial_apply @closure1(%9)` coming from caller is only a `partial_apply`
    /// operand and is therefore invisible to the applied-check.
    ///
    /// Folding `partial_apply`+`apply` into a single `apply` (via `tryOptimizeApplyOfPartialApply`) turns `partial_apply`
    /// of `closure1` in caller into a direct full-apply operand, so the next specialization round on `caller` can see
    /// that this closure is transitively applied in `closure2`. Without the fold the `partial_apply` of `closure1`
    /// closure is never recognized as applied and its `partial_apply` stays behind.
    ///
    /// We fold now rather than defer to separate sil-combiner run: the caller's specialization loop does revisit its
    /// apply each round, but unless we mutate the cloned callee now the applied-check still fails and the loop makes no
    /// progress. So, by the time the sil-combiner reaches the specialized callee (it's added to pass manager worklist
    /// via `notifyNewFunction`) and does the fold inside it, the caller's specialization has already finished and its
    /// apply of callee is never revisited at all - stranding the `partial_apply` of `closure1`.
    ///
    ///   sil @closure0 : (Float, Float) -> Float             // leaf
    ///   sil @closure1 : (Float, (Float) -> Float) -> Float  // bb0(%x, %f): return apply %f(%x)
    ///   sil @closure2 : (Float, (Float) -> Float) -> Float  // bb0(%x, %f): return apply %f(%x)
    ///   sil @callee   : (Float, (Float) -> Float) -> Float  // bb0(%x, %f): return apply %f(%x)
    ///
    ///   caller(%0 : Float, %1 : Float) -> Float:
    ///     %9  = partial_apply @closure0(%0)  : (Float) -> Float
    ///     %12 = partial_apply @closure1(%9)  : (Float) -> Float
    ///     %15 = partial_apply @closure2(%12) : (Float) -> Float
    ///     return apply @callee(%1, %15)
    ///
    /// Round 1 for caller: specialize apply of callee
    ///
    ///   caller(%0 : Float, %1 : Float) -> Float:
    ///     %9  = partial_apply @closure0(%0)  : (Float) -> Float
    ///     %12 = partial_apply @closure1(%9)  : (Float) -> Float
    ///     return apply @specialized_callee_r1(%1, %12)
    ///
    ///   specialized_callee_r1(%0 : Float, %1 : (Float) -> Float) -> Float:
    ///     return apply @closure2(%0, %1) // <-- folded from _/ %3 = partial_apply @closure2(%1)
    ///                                    //                  \ %9 = apply %3(%0)
    ///
    /// Now `%12 = partial_apply @closure1(%9)` in caller is recognized as transitively applied in
    /// `specialized_callee_r1`->`closure2`, making it subject for specialization during the 2nd round.

    for rootClosure in rootClosures {
      let clonedRootClosure = cloner.getClonedValue(of: rootClosure) as! PartialApplyInst
      let argsAreKeptAlive = cloner.context.tryOptimizeApplyOfPartialApply(closure: clonedRootClosure)
      // Unlike a regular closure (which only ever borrows, or independently copies, its captures),
      // an at-most-once closure can have consuming captures. When the fold above transfers a
      // non-Copyable capture to the new direct call, it can't copy it, so it leaves `Undef` in
      // `clonedRootClosure`'s own operand instead. Leaving such a closure's `partial_apply` behind
      // would still run its destructor at runtime, which would release whatever garbage is left
      // in that now-`Undef`'d capture slot.
      if clonedRootClosure.hasCalledAtMostOnceSemantics {
        _ = cloner.context.tryDeleteDeadClosure(closure: clonedRootClosure, needKeepArgsAlive: !argsAreKeptAlive)
      }
    }
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
      partialApply.argumentOperands.map { argOp -> Value in
        let capturedArg = argOp.value
        if partialApply.hasCalledAtMostOnceSemantics {
          // An at-most-once closure can consume its captures and always gets a destructor even
          // if it's stack-promoted. So all of the arguments that are consumed have to be passed
          // the same way to the specialized version.
          if capturedArg.ownership != .none &&
              (argOp.ownership == .destroyingConsume || argOp.ownership == .forwardingConsume) {
            if capturedArg.type.isMoveOnly {
              // A move-only value cannot be copied. Transfer it directly to the specialized
              // call instead, and clear the original `partial_apply`'s operand (it isn't being
              // deleted here, only folded into a direct call) so that a later cleanup pass
              // doesn't also try to destroy the same value when it deletes the now-dead
              // `partial_apply`.
              argOp.set(to: Undef.get(type: capturedArg.type, context), context)
              return capturedArg
            }
            return capturedArg.copy(at: partialApply, andMakeAvailableIn: apply.parentBlock, context)
          }
          return capturedArg
        }
        if partialApply.isOnStack || capturedArg.ownership == .none {
          // Non-escaping closures don't consume their captures. Therefore we pass them also as "guaranteed"
          // arguments to the specialized function.
          // Note that because the non-escaping closure was passed to the original function, this guarantees
          // that the lifetime of the captured arguments also extend to at least the apply of the function.
          return capturedArg
        } else {
          // Escaping closures consume their captures. Therefore we pass them as "owned" arguments to the
          // specialized function.
          return capturedArg.copy(at: partialApply, andMakeAvailableIn: apply.parentBlock, context)
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
        let capturedValue = argOp.value
        let cast = builder.createUncheckedValueCast(from: capturedValue, to: capturedValue.type)
        argOp.set(to: cast, context)

        // Redirect the mark_dependence base onto the same cast: the cloner only maps the cast to
        // the capture argument, and a shared base must follow its own closure's capture.
        for dependency in capturedDependencies
        where dependency.closure == closure && dependency.markDependence.base == capturedValue {
          dependency.markDependence.baseOperand.set(to: cast, context)
        }
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
  let recursionBudget = 8
  for use in closure.uses {
    switch use.instruction {

    case is ConvertFunctionInst,
         is ConvertEscapeToNoEscapeInst,
         is MoveValueInst,
         is CopyValueInst:
      if checkRecursivelyIfClosureIsApplied(use.instruction as! SingleValueInstruction, &handledFuncs) {
        return true
      }

    case let pai as PartialApplyInst:
      if pai.isPartialApplyOfThunk {
        // `pai.isPartialApplyOfThunk` implies that the captured closure (`closure` here) is applied in the thunk.
        // If the thunk closure (`pai` here) is applied by itself as well, the closure captured by
        // the thunk closure also becomes effectively applied transitively.
        return checkRecursivelyIfClosureIsApplied(pai, &handledFuncs)
      }

      guard let callee = pai.referencedFunction,
            callee.isDefinition,
            handledFuncs.insert(callee).inserted,
            handledFuncs.count <= recursionBudget,
            let calleeArg = pai.calleeArgument(of: use, in: callee)
      else {
        continue
      }

      // Check if the captured closure (`closure` here) is applied in context of wrapper closure (`pai` here).
      if checkRecursivelyIfClosureIsApplied(calleeArg, &handledFuncs) {
        // If the wrapper closure (`pai` here) is applied by itself complementary to the captured closure applied
        // in context of wrapper closure, the captured closure also becomes effectively applied transitively.
        // For example, treat `%closure1` as applied in `@foo`:
        //   sil @wrapper(%0, %1, %closure) { apply %closure(%1, %0) }
        //   sil @foo(%closure1, %arg1, %arg2) {
        //     %closure2 = partial_apply @wrapper(%closure1)
        //     apply %closure2(%arg1, %arg2)
        //   }
        return checkRecursivelyIfClosureIsApplied(pai, &handledFuncs)
      }

    case let apply as FullApplySite:
      if apply.callee == closure {
        return true
      }

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
  func withSpecializedConvention(for argOp: Operand, in partialApply: PartialApplyInst, callee: Function) -> Self {
    let argType = type.loweredType(in: partialApply.parentFunction)
    let specializedParamConvention = if self.convention.isIndirect {
      self.convention
    } else if partialApply.hasCalledAtMostOnceSemantics {
      if argType.isTrivial(in: callee) {
        ArgumentConvention.directUnowned
      } else if argOp.ownership == .destroyingConsume || argOp.ownership == .forwardingConsume {
        ArgumentConvention.directOwned
      } else {
        ArgumentConvention.directGuaranteed
      }
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
  var isPartialApplyOfThunk: Bool {
    if self.numArguments == 1,
      let fun = self.referencedFunction,
      fun.thunkKind == .reabstractionThunk || fun.isAutodiffSubsetParametersThunk,
      self.arguments[0].type.isLoweredFunction,
      self.arguments[0].type.isReferenceCounted(in: self.parentFunction) || self.callee.type.isThickFunction
    {
      return true
    }
    return false
  }

  var hasOnlySupportedIndirectArguments: Bool {
    self.argumentOperands
      .filter { !$0.value.type.isObject }
      .allSatisfy {
        let conv = self.convention(of: $0)!
        return conv.isInout || (self.isOnStack && conv == .indirectInGuaranteed)
      }
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

private enum BTESpecEligibility {
  case noBTE
  case eligible
  case ineligible
}

private func validatePullbackBTEUsage(pb: Function, bteArg: Argument, vjp: Function, prefixFail: String) -> Bool {
  if pb.blocks.singleElement != nil {
    guard let _ = pb.entryBlock.terminator as? ReturnInst else {
      log(
        prefixFail + "unexpected terminator instruction in the entry block of the pullback "
          + pb.name.string
          + " (expected return inst for single-bb pullback)")
      log("  terminator: " + pb.entryBlock.terminator.description)
      log("  parent block begin")
      log("  " + pb.entryBlock.description)
      log("  parent block end")
      return false
    }
    log(
      "Pullback: single-bb; \(bteArg.uses.count) uses of branch tracing enum pullback argument found."
    )
    if !bteArg.uses.isEmpty {
      log(
        prefixFail + "single-bb pullback has uses of BTE arg")
      var needBreak = true
      if bteArg.uses.singleElement != nil {
        let useInst = bteArg.uses.singleUse!.instruction
        if useInst as? ApplyInst != nil {
          log("Single use of BTE arg is apply inst")
        }
        if useInst as? DestroyValueInst != nil {
          log("Single use of BTE arg is destroy_value inst")
          needBreak = false
        }
      }
      if needBreak {
        return false
      }
    }
  } else {
    if bteArg.uses.isEmpty {
      log(prefixFail + "no uses of pullback bte arg found")
      return false
    }
    if bteArg.uses.singleElement == nil {
      log(prefixFail + "multiple uses of pullback bte arg found")
      for (idx, use) in bteArg.uses.enumerated() {
        log("use \(idx): \(use)")
      }
      return false
    }

    guard bteArg.uses.singleUse!.instruction as? SwitchEnumInst != nil else {
      log(
        prefixFail + "unexpected use of BTE argument of pullback " + pb.name.string
          + " (only switch_enum_inst is supported)")
      log("  use: \(bteArg.uses.singleUse!.instruction)")
      log("  parent block begin")
      log("  \(bteArg.uses.singleUse!.instruction.parentBlock)")
      log("  parent block end")
      return false
    }
  }
  return true
}

private func validatePullbackSwitchEnumTerminators(pb: Function, prefixFail: String) -> Bool {
  for pbBB in pb.blocks {
    guard let sei = pbBB.terminator as? SwitchEnumInst else {
      continue
    }
    if sei.getSuccessorForDefault() != nil {
      log(
        prefixFail + "switch_enum_inst from the \(pbBB.shortDescription) of the pullback "
          + pb.name.string
          + " has default destination set, which is not supported")
      return false
    }
  }
  return true
}

private func validateVJPBlockBTEArgs(vjp: Function, prefixFail: String) -> Bool {
  for vjpBB in vjp.blocks {
    if vjpBB.arguments.filter ({ $0.type.isBranchTracingEnum(in: vjp) }).count > 1 {
      log(
        prefixFail + "several arguments of VJP " + vjp.name.string + " basic block "
          + vjpBB.shortDescription + " are branch tracing enums, but not more than 1 is supported"
      )
      return false
    }
  }
  return true
}

private func checkIfCanRun(vjp: Function, context: FunctionPassContext) -> BTESpecEligibility {
  assert(vjp.blocks.singleElement == nil)

  let prefixFail = "Cannot run AutoDiff Closure Specialization on \(vjp.name.string): "
  guard let paiOfPb = getPartialApplyOfPullbackInExitVJPBB(vjp: vjp) else {
    log(
      "\(prefixFail)partial_apply of pullback not found in exit basic block of VJP")
    return .ineligible
  }
  var branchTracingEnumArgCounter = 0
  for arg in paiOfPb.arguments {
    if arg.type.isBranchTracingEnum(in: vjp) {
      branchTracingEnumArgCounter += 1
    }
  }

  // Check for loops before evaluating the BTE arg count.
  for inst in vjp.instructions {
    guard let builtinInst = inst as? BuiltinInst else {
      continue
    }
    if builtinInst.name.string == "autoDiffProjectTopLevelSubcontext" {
      log(
        "\(prefixFail)VJP seems to contain a loop (builtin autoDiffProjectTopLevelSubcontext detected), this is not supported"
      )
      return .ineligible
    }
  }

  if branchTracingEnumArgCounter == 0 {
    log("This is multi-BB case which would be handled as single-BB case")
    return .noBTE
  }

  guard branchTracingEnumArgCounter == 1 else {
    log(
      "\(prefixFail)partial_apply of pullback in exit basic block of VJP has \(branchTracingEnumArgCounter) branch tracing enum arguments, but exactly 1 is expected"
    )
    return .ineligible
  }

  guard let pb = paiOfPb.referencedFunction else {
    log(
      "\(prefixFail)cannot obtain pullback function reference from the partial_apply of pullback in exit basic block of VJP"
    )
    return .ineligible
  }
  guard pb.isDefinition, pb.hasOwnership else {
    log("\(prefixFail)pullback \(pb.name.string) has no body with ownership information")
    return .ineligible
  }
  guard computeTopologicalBlockOrder(of: pb) != nil else { return .ineligible }
  guard let bteArgOfPb = pb.entryBlock.getBranchTracingEnumArg(vjp: vjp) else {
    log(
      "\(prefixFail)cannot get branch tracing enum argument of the pullback function \(pb.name.string)"
    )
    return .ineligible
  }

  guard validatePullbackBTEUsage(pb: pb, bteArg: bteArgOfPb, vjp: vjp, prefixFail: prefixFail)
  else {
    return .ineligible
  }

  guard ensureEnumPayloadsAreTupleInst(vjp: vjp) else {
    log("\(prefixFail)branch tracing enum payload is not defined by a TupleInst")
    return .ineligible
  }

  guard validatePullbackSwitchEnumTerminators(pb: pb, prefixFail: prefixFail) else {
    return .ineligible
  }

  guard validateVJPBlockBTEArgs(vjp: vjp, prefixFail: prefixFail) else {
    return .ineligible
  }

  return .eligible
}

extension UseList {
  var count : Int {
    var n = 0
    for _ in self {
      n += 1
    }
    return n
  }

  var isAtLeastTwo : Bool {
    !self.isEmpty && self.singleElement == nil
  }

  var isExactlyTwo : Bool {
    var n = 0
    for _ in self {
      n += 1
      if n > 2 {
        return false
      }
    }
    return n == 2
  }
}

extension BasicBlockList {
  var count : Int {
    var n = 0
    for _ in self {
      n += 1
    }
    return n
  }
}

private func ensureEnumPayloadsAreTupleInst(vjp: Function) -> Bool {
  for inst in vjp.instructions {
    guard let ei = inst as? EnumInst else {
      continue
    }
    if !ei.type.isBranchTracingEnum(in: vjp) {
      continue
    }
    guard let _ = ei.payload?.definingInstruction as? TupleInst else {
      log("Branch tracing enum payload is not defined by a TupleInst: \(ei)")
      return false
    }
  }
  return true
}

private enum PayloadValues {
  case destructureTuple([Value])
  case tupleExtract([Value])
  case zeroUses
  case unsupported
}

private enum PayloadUseShape {
  case apply(ApplyInst)
  case destroyValue(DestroyValueInst)
  case uncheckedEnumData(UncheckedEnumDataInst)
  case switchEnum(SwitchEnumInst)
}

// A payload can reach a block as a whole tuple or as a projected field after
// phi simplification. Record its origins once; validation and cloning use the
// same logical field index, independent of where the projection was placed.
private struct PayloadFlow {
  let argument: Argument
  let fieldIndex: Int?
  let origins: [EnumCase]
  let forwardedPayloads: [UncheckedEnumDataInst]
  var closureInfos = [ClosureInBTE]()

  init?(_ argument: Argument, vjp: Function) {
    guard !argument.isReborrow else { return nil }
    var origins = [EnumCase]()
    var payloads = [UncheckedEnumDataInst]()
    var fieldIndex: Int? = nil
    for predecessor in argument.parentBlock.predecessors {
      let enumValue: Value
      let caseIndex: Int
      var incomingIndex: Int? = nil
      switch predecessor.terminator {
      case let branch as BranchInst:
        let operand = branch.operands[argument.index]
        var payload = operand.value
        if let extract = payload.definingInstruction as? TupleExtractInst {
          guard extract.tuple.type.isTrivial(in: argument.parentFunction),
                payload.uses.singleUse == operand else { return nil }
          incomingIndex = extract.fieldIndex
          payload = extract.tuple
        } else if let destructure = payload.definingInstruction as? DestructureTupleInst,
                  let element = payload as? MultipleValueInstructionResult {
          guard payload.uses.singleUse == operand,
                let usedElement = destructure.results.filter({ !$0.uses.isEmpty }).singleElement,
                usedElement == element
          else { return nil }
          incomingIndex = element.index
          payload = destructure.tuple
        }
        guard let enumData = payload.definingInstruction as? UncheckedEnumDataInst,
              payload.uses.singleUse?.instruction ==
                (incomingIndex == nil ? branch : operand.value.definingInstruction!)
        else { return nil }
        payloads.append(enumData)
        enumValue = enumData.enum
        caseIndex = enumData.caseIndex
      case let dispatch as SwitchEnumInst:
        guard let index = dispatch.getUniqueCase(forSuccessor: argument.parentBlock)
        else { return nil }
        enumValue = dispatch.enumOp
        caseIndex = index
      default:
        return nil
      }
      guard enumValue.type.isBranchTracingEnum(in: vjp),
            origins.isEmpty || fieldIndex == incomingIndex else { return nil }
      let enumType = remapBranchTracingEnumType(enumValue.type, into: vjp)
      guard let enumCase = enumType.getEnumCases(in: vjp)?[caseIndex],
            enumCase.payload?.isTuple == true else { return nil }
      origins.append(enumCase)
      fieldIndex = incomingIndex
    }
    guard !origins.isEmpty else { return nil }
    self.argument = argument
    self.fieldIndex = fieldIndex
    self.origins = origins
    self.forwardedPayloads = payloads
  }
}

private func classifyPayloadUse(_ use: Operand) -> PayloadUseShape? {
  switch use.instruction {
  case let ai as ApplyInst:
    guard use == ai.operands[ApplyOperandConventions.calleeIndex] else { return nil }
    return .apply(ai)
  case let dvi as DestroyValueInst:
    return .destroyValue(dvi)
  case let uedi as UncheckedEnumDataInst:
    return .uncheckedEnumData(uedi)
  case is ConvertFunctionInst:
    // Generated taped callbacks are invoked with their stored function type.
    // Do not synthesize conversions for an unexpected payload callable.
    return nil
  case let sei as SwitchEnumInst:
    return .switchEnum(sei)
  default:
    return nil
  }
}

private func getPayloadValues(payload: Argument, vjp: Function) -> PayloadValues {
  if payload.uses.isEmpty {
    return PayloadValues.zeroUses
  }

  var results = [Value]()

  if let singleUse = payload.uses.singleUse,
     let dti = singleUse.instruction as? DestructureTupleInst
  {
    // TODO: do we need to check that results is not empty?
    if dti.operands[0].value.type.tupleElements.count != 0
      && dti.results[0].type.isBranchTracingEnum(in: vjp) && dti.results[0].uses.isAtLeastTwo
    {
      return PayloadValues.unsupported
    }
    for result in dti.results {
      results.append(result)
    }
    return PayloadValues.destructureTuple(results)
  }

  var idxs = [Int]()
  for use in payload.uses {
    guard let tei = use.instruction as? TupleExtractInst else {
      return PayloadValues.unsupported
    }
    if idxs.contains(tei.fieldIndex) {
      return PayloadValues.unsupported
    }
    if tei.fieldIndex == 0 && tei.type.isBranchTracingEnum(in: vjp)
      && tei.results[0].uses.isAtLeastTwo
    {
      return PayloadValues.unsupported
    }
    idxs.append(tei.fieldIndex)
    results.append(tei)
  }
  return PayloadValues.tupleExtract(results)
}

// Capture extraction and cleanup are emitted at the callback application.
// Require one direct application; unused or repeatedly applied fields would
// need independent cleanup or per-call copies, which this rewrite does not emit.
// Any destroys of an owned field must follow the application in the same block.
// Decline discard-only paths instead of dropping their capture cleanup.
private func hasSingleTapedInvocation(_ value: Value) -> Bool {
  var invocation: ApplyInst? = nil
  for use in value.uses {
    if use.instruction is DestroyValueInst { continue }
    guard invocation == nil,
          let apply = use.instruction as? ApplyInst,
          use == apply.operands[ApplyOperandConventions.calleeIndex] else { return false }
    invocation = apply
  }
  guard let invocation else { return false }
  if value.ownership == .owned {
    for use in value.uses {
      guard let destroy = use.instruction as? DestroyValueInst else { continue }
      guard destroy.parentBlock == invocation.parentBlock,
            invocation.strictlyDominatesInBlock(destroy) else { return false }
    }
  }
  return true
}

private func checkIfCanRunForPayloadValues(
  results: [Value], forwardedPayloads: Set<UncheckedEnumDataInst>,
  prefixFail: String, pb: Function, pbBB: BasicBlock
) -> Bool {
  for result in results {
    if result.type.isLoweredFunction && !hasSingleTapedInvocation(result) {
      return false
    }
    for use in result.uses {
      if let enumData = use.instruction as? UncheckedEnumDataInst,
         !enumData.uses.isEmpty && !forwardedPayloads.contains(enumData) {
        return false
      }
      guard classifyPayloadUse(use) != nil else {
        log(
          prefixFail + "unexpected use of an element of the tuple being argument of pullback "
            + pb.name.string + " basic block " + pbBB.shortDescription)
        log("  result: \(result)")
        log("  use.instruction: \(use.instruction)")
        return false
      }


    }
  }
  return true
}

private func logSpecializationStats(
  closuresSet: Set<SingleValueInstruction>,
  totalSupportedClosures: Int,
  function: Function
) {
  var totalClosures: Int = 0
  for inst in function.instructions {
    let paiOpt = inst as? PartialApplyInst
    let tttfOpt = inst as? ThinToThickFunctionInst
    if paiOpt != nil || tttfOpt != nil {
      totalClosures += 1
    }
  }

  let specializedClosures = totalSupportedClosures - closuresSet.count
  var msg =
    "Specialized \(specializedClosures) out of \(totalSupportedClosures) supported closures "
  msg += "(rate \(Float(specializedClosures) / Float(totalSupportedClosures))). "
  msg += "Total number of closures is \(totalClosures)"
  log(msg)
}

private func synthesizeSpecializedEnums(
  from closureAnalysis: PullbackClosureAnalysis,
  context: FunctionPassContext
) -> SpecializedBTEMap? {
  guard !closureAnalysis.closuresInBTE.isEmpty else { return [:] }
  return autodiffSpecializeBranchTracingEnums(
    topVJP: closureAnalysis.vjp,
    bteSpecializationQueue: closureAnalysis.bteSpecializationQueue,
    closuresInBTE: closureAnalysis.closuresInBTE,
    context: context)
}

private func eraseDeadSpecializedClosures(
  from closuresInBTE: [ClosureInBTE],
  context: FunctionPassContext
) -> Set<SingleValueInstruction> {
  var closuresSet = Set<SingleValueInstruction>()
  var allocStacks = Set<AllocStackInst>()

  for closureInfo in closuresInBTE {
    if let pai = closureInfo.closure as? PartialApplyInst {
      for op in pai.argumentOperands {
        guard op.value.type.isAddress else {
          continue
        }
        // These invariants hold due to prior checks in Instruction.asSupportedClosure
        let allocStack = op.value as! AllocStackInst
        assert(allocStack.uses.count == 3)
        allocStacks.insert(allocStack)
      }
    }

    closuresSet.insert(closureInfo.closure)
    for reabstraction in closureInfo.adapters {
      closuresSet.insert(reabstraction)
    }
  }

  var oldSetSize = 0
  repeat {
    oldSetSize = closuresSet.count
    let closures = Array(closuresSet)
    for closure in closures {
      if closure.uses.isEmpty {
        context.erase(instruction: closure)
        closuresSet.remove(closure)
      }
    }
  } while oldSetSize != closuresSet.count

  for allocStack in allocStacks {
    assert(allocStack.uses.count == 2)
    for use in allocStack.uses {
      assert(use.instruction is DeallocStackInst || use.instruction is StoreInst)
      context.erase(instruction: use.instruction)
    }
    context.erase(instruction: allocStack)
  }

  return closuresSet
}

private func specializeAgainstBTE(
  closureAnalysis: PullbackClosureAnalysis,
  specializedBTEDict: SpecializedBTEMap,
  function: Function,
  context: FunctionPassContext
) {
  let totalSupportedClosures = Set(closureAnalysis.closuresInBTE.map { $0.closure }).count

  let autodiffSpecializationInfo = AutoDiffSpecializationInfo(
    closureAnalysis: closureAnalysis,
    specializedBTEDict: specializedBTEDict)

  let specInfo = PullbackSpecializationAgainstBTE()

  let (specializedFunction, alreadyExists) =
    specInfo.getOrCreatePullbackSpecializedAgainstBTE(
      basedOn: autodiffSpecializationInfo,
      enumDict: autodiffSpecializationInfo.specializedBTEDict,
      context)

  if !alreadyExists {
    context.notifyNewFunction(
      function: specializedFunction, derivedFrom: autodiffSpecializationInfo.pullback)
  }

  rewriteVJPForBTEPullbackSpecialization(
    using: specializedFunction,
    autodiffSpecializationInfo: autodiffSpecializationInfo,
    enumDict: autodiffSpecializationInfo.specializedBTEDict,
    context: context)

  let remainingClosures = eraseDeadSpecializedClosures(
    from: closureAnalysis.closuresInBTE, context: context)

  logSpecializationStats(
    closuresSet: remainingClosures,
    totalSupportedClosures: totalSupportedClosures,
    function: function)
}

private func replaceEnumInstructionsWithSpecializedTypes(
  in vjp: Function, enumDict: SpecializedBTEMap, context: FunctionPassContext
) {
  for inst in vjp.instructions {
    guard let ei = inst as? EnumInst else {
      continue
    }
    guard let newEnumType = enumDict[ei.results[0].type] else {
      continue
    }

    let builder = Builder(before: ei, context)
    let newEI = builder.createEnum(
      caseIndex: ei.caseIndex, payload: ei.payload, enumType: newEnumType)
    ei.replace(with: newEI, context)
  }
}

private func specializeBTEBlockArgsInVJP(
  vjp: Function, enumDict: SpecializedBTEMap, context: FunctionPassContext
) {
  for bb in vjp.blocks {
    guard let arg = bb.getBranchTracingEnumArg(vjp: vjp) else {
      continue
    }
    if enumDict[arg.type] == nil {
      continue
    }
    let newArg = specializeBranchTracingEnumBBArgInVJP(
      arg: arg, specializedBTEDict: enumDict, context: context)
    arg.uses.replaceAll(with: newArg, context)
    bb.eraseArgument(at: arg.index, context)
  }
}

private func buildReplacementElement(
  capturedArgs: [Value], isOptionalSome: Bool?,
  tupleType: Type, vjp: Function, builder: Builder
) -> Value {
  if let isOptionalSome = isOptionalSome {
    let optionalTupleType = tupleType.rawType.optionalType.loweredType(in: vjp)
    if isOptionalSome {
      let tuple = builder.createTuple(type: tupleType, elements: capturedArgs)
      return builder.createOptionalSome(operand: tuple, type: optionalTupleType)
    } else {
      return builder.createOptionalNone(type: optionalTupleType)
    }
  } else {
    return builder.createTuple(type: tupleType, elements: capturedArgs)
  }
}

private func rewritePayloadTuplesInVJP(
  vjp: Function, payloads: Set<TupleInst>,
  closureInfos: inout [ClosureInBTE], context: FunctionPassContext
) {
  for payload in payloads {
    let ti = payload
    if ti.operands.count == 0 {
      continue
    }

    var indexesToExclude = [Int]()

    var tupleIdxToCapturedArgs = [Int: (values: [Value], isOptionalSome: Bool?)]()
    for (idx, closureInfo) in closureInfos.enumerated() {
      if closureInfo.payloadTuple != ti {
        continue
      }
      indexesToExclude.append(idx)
      let idxInTuple = closureInfo.indexInPayload
      assert(ti.operands[idxInTuple].value == closureInfo.valueInPayload)
      var isOptionalSome: Bool? = nil
      if closureInfo.optionalWrapper != nil {
        isOptionalSome = !closureInfo.optionalWrapper!.isOptionalNone
      }
      tupleIdxToCapturedArgs[idxInTuple] = (
        values: closureInfo.capturedArgs, isOptionalSome: isOptionalSome
      )
    }

    for idx in indexesToExclude.reversed() {
      closureInfos.remove(at: idx)
    }

    var newPayloadValues = [Value]()
    for (opIdx, op) in ti.operands.enumerated() {
      if tupleIdxToCapturedArgs[opIdx] == nil {
        newPayloadValues.append(op.value)
        continue
      }

      if tupleIdxToCapturedArgs[opIdx]!.isOptionalSome != nil {
        assert(opIdx + 1 == ti.operands.count)
      }
      let builderPred = Builder(before: ti, context)
      let entry = tupleIdxToCapturedArgs[opIdx]!
      let tupleType = context.getTupleType(
        elements: entry.values.map { $0.type }
      ).loweredType(in: vjp)
      let replacement = buildReplacementElement(
        capturedArgs: entry.values, isOptionalSome: entry.isOptionalSome,
        tupleType: tupleType, vjp: vjp, builder: builderPred)
      newPayloadValues.append(replacement)
    }

    let oldElements = ti.type.tupleElements
    var newLabeledElements = [(label: Identifier, type: AST.`Type`)]()
    for (idx, value) in newPayloadValues.enumerated() {
      newLabeledElements.append(
        (
          label: oldElements.label(at: idx),
          type: value.type.rawType
        ))
    }
    let newTupleType = context.getTupleType(elements: newLabeledElements)
      .loweredType(in: vjp)

    let builderPred = Builder(before: ti, context)
    let newPayload = builderPred.createTuple(type: newTupleType, elements: newPayloadValues)
    ti.replace(with: newPayload, context)
  }
}

private func cleanupDeadOptionalEnums(in vjp: Function, context: FunctionPassContext) {
  var wasUpdated = false
  repeat {
    wasUpdated = false
    for inst in vjp.instructions {
      guard let enumOpt = inst as? EnumInst else {
        continue
      }
      if enumOpt.type.isOptional && enumOpt.uses.isEmpty {
        context.erase(instruction: enumOpt)
        wasUpdated = true
      }
    }
  } while wasUpdated
}

private func rewriteVJPForBTEPullbackSpecialization(
  using specializedCallee: Function, autodiffSpecializationInfo: AutoDiffSpecializationInfo,
  enumDict: SpecializedBTEMap,
  context: FunctionPassContext
) {
  let vjp = autodiffSpecializationInfo.paiOfPullback.parentFunction
  var closureInfos = autodiffSpecializationInfo.closuresInBTE

  replaceEnumInstructionsWithSpecializedTypes(in: vjp, enumDict: enumDict, context: context)

  specializeBTEBlockArgsInVJP(vjp: vjp, enumDict: enumDict, context: context)

  let pai = autodiffSpecializationInfo.paiOfPullback
  pai.replace(withCallTo: specializedCallee, arguments: Array(pai.arguments), context)

  rewritePayloadTuplesInVJP(
    vjp: vjp, payloads: autodiffSpecializationInfo.closureAnalysis.payloads,
    closureInfos: &closureInfos, context: context)

  cleanupDeadOptionalEnums(in: vjp, context: context)
}

private func findEnumsAndPayloadsInVjp(vjp: Function) -> [EnumInst: TupleInst] {
  var dict = [EnumInst: TupleInst]()
  for inst in vjp.instructions {
    guard let ei = inst as? EnumInst else {
      continue
    }
    if !ei.type.isBranchTracingEnum(in: vjp) {
      continue
    }
    let ti = ei.operands[0].value.definingInstruction as! TupleInst
    dict[ei] = ti
  }
  return dict
}

private func getParametersForPullbackSpecializedAgainstBTE(
  basedOn autodiffSpecializationInfo: AutoDiffSpecializationInfo, pb: Function, enumType: Type,
  enumDict: SpecializedBTEMap,
  _ context: FunctionPassContext
) -> [ParameterInfo] {
  let applySiteCallee = autodiffSpecializationInfo.pullback
  var specializedParamInfoList: [ParameterInfo] = []
  var foundBranchTracingEnumParam = false
  // Start by adding all original parameters except for the closure parameters.
  for paramInfo in applySiteCallee.convention.parameters {
    if paramInfo.type != enumType.rawType.mapOutOfEnvironment().canonical {
      specializedParamInfoList.append(paramInfo)
      continue
    }
    assert(!foundBranchTracingEnumParam)
    foundBranchTracingEnumParam = true
    let newParamInfo = ParameterInfo(
      type: enumDict[enumType]!.rawType.mapOutOfEnvironment().canonical,
      convention: paramInfo.convention,
      options: paramInfo.options, hasLoweredAddresses: paramInfo.hasLoweredAddresses)
    specializedParamInfoList.append(newParamInfo)
  }
  assert(foundBranchTracingEnumParam)
  return specializedParamInfoList
}

private func getEnumCasesForSwitchEnumInst(_ sei: SwitchEnumInst) -> [(Int, BasicBlock)] {
  (0..<sei.numCases).compactMap { i in
    sei.getUniqueSuccessor(forCaseIndex: i).map { (i, $0) }
  }
}

private struct PayloadRewriteContext {
  let closureInfoArray: [ClosureInBTE]
}

private func findMatchingClosureInfo(
  in closureInfoArray: [ClosureInBTE], forPayloadIndex index: Int
) -> ClosureInBTE? {
  var result: ClosureInBTE? = nil
  for closureInfo in closureInfoArray {
    if closureInfo.indexInPayload == index {
      if result != nil {
        assert(result!.closure == closureInfo.closure)
        assert(result!.payloadTuple == closureInfo.payloadTuple)
      } else {
        result = closureInfo
      }
    }
  }
  return result
}

private func insertLifetimeEndIfNeeded(
  for value: Value, before insertionPoint: Instruction, _ context: FunctionPassContext
) {
  if value.type.isTrivial(in: value.parentFunction) {
    return
  }
  for use in value.uses {
    if use.endsLifetime {
      return
    }
  }
  let builder = Builder(before: insertionPoint, context)
  assert(value.parentFunction.hasOwnership)
  builder.createDestroyValue(operand: value)
}

private func extractTupleElements(
  from tuple: Value, builder: Builder
) -> [Value] {
  var elements = [Value]()
  // Consume owned captures; borrowed and trivial tuples use nonconsuming projections.
  if tuple.ownership != .owned {
    for (tupleIdx, _) in tuple.type.tupleElements.enumerated() {
      elements.append(builder.createTupleExtract(tuple: tuple, elementIndex: tupleIdx))
    }
  } else {
    for result in builder.createDestructureTuple(tuple: tuple).results {
      elements.append(result)
    }
  }
  return elements
}

// The recipe belongs to the VJP; its contextual result type must be lowered
// in the specialized pullback, just like the reconstructed thunk substitutions.
private func recreateFunctionConversion(
  _ conversion: ConvertFunctionInst, of closure: Value, builder: Builder
) -> ConvertFunctionInst {
  let function = closure.parentFunction
  let type = function.mapTypeIntoEnvironment(conversion.type.rawType.mapOutOfEnvironment())
    .canonical.loweredType(in: function)
  return builder.createConvertFunction(
    originalFunction: closure, resultType: type, kind: conversion.conversionKind)
}

private func rewriteApplyDirectClosure(
  ai: ApplyInst, closureInfo: ClosureInBTE, extractedElements: [Value],
  builder: Builder, _ context: FunctionPassContext
) {
  if closureInfo.adapters.isEmpty {
    var capturedArguments = extractedElements
    var indirectCaptures = [(allocation: AllocStackInst, isTrivial: Bool)]()
    if let partialApply = closureInfo.closure as? PartialApplyInst {
      for (index, operand) in partialApply.argumentOperands.enumerated() {
        if operand.value.type.isAddress {
          let capture = capturedArguments[index]
          let allocation = builder.createAllocStack(capture.type)
          builder.createStore(source: capture, destination: allocation, ownership: .initialize)
          capturedArguments[index] = allocation
          indirectCaptures.append((allocation, capture.type.isTrivial(in: ai.parentFunction)))
        }
      }
    }
    var newArgs = Array(ai.arguments)
    newArgs.append(contentsOf: capturedArguments)
    let vjpFn = closureInfo.closure.asSupportedClosureFn!
    let newFri = builder.createFunctionRef(vjpFn)
    // A thick closure application has already applied the root function's
    // generic substitutions. Recover them from the closure being eliminated.
    let substitutions = (closureInfo.closure as? PartialApplyInst)?.substitutionMap ?? ai.substitutionMap
    let newAi = builder.createApply(
      function: newFri, substitutions.substitute(with: ai.parentFunction.forwardingSubstitutionMap),
      arguments: newArgs)
    // An indirect capture is part of the closure context, but a direct call
    // needs address storage lasting through the guaranteed-indirect call.
    for capture in indirectCaptures.reversed() {
      if !capture.isTrivial {
        builder.createDestroyAddr(address: capture.allocation)
      }
      builder.createDeallocStack(capture.allocation)
    }
    ai.replace(with: newAi, context)

    // TODO: maybe we can set insertion point earlier
    for res in extractedElements {
      insertLifetimeEndIfNeeded(for: res, before: newAi.parentBlock.terminator, context)
    }

    return
  }

  var extractedElements = extractedElements

  let vjpFn = closureInfo.closure.asSupportedClosureFn!
  let newFri = builder.createFunctionRef(vjpFn)
  var indirectArgs = [(idx: Int, allocStack: AllocStackInst, store: StoreInst)]()
  let rootClosure : SingleValueInstruction
  if let pai = closureInfo.closure as? PartialApplyInst {
    for idx in 0..<extractedElements.count {
      if pai.arguments[idx].type.isAddress {
        let allocStack = builder.createAllocStack(extractedElements[idx].type)
        let store = builder.createStore(source: extractedElements[idx], destination: allocStack, ownership: StoreInst.StoreOwnership.initialize)
        indirectArgs.append((idx: idx, allocStack: allocStack, store: store))
        extractedElements[idx] = allocStack
      }
    }

    rootClosure = builder.createPartialApply(
      function: newFri,
      substitutionMap: pai.substitutionMap.substitute(with: ai.parentFunction.forwardingSubstitutionMap),
      capturedArguments: extractedElements,
      calleeConvention: pai.calleeConvention,
      hasUnknownResultIsolation: pai.hasUnknownResultIsolation,
      isOnStack: pai.isOnStack,
      isNested: pai.isNested,
      executionSemantics: pai.executionSemantics)

    for (idx, allocStack, store) in indirectArgs.reversed() {
      extractedElements[idx] = store.source
      builder.createDeallocStack(allocStack)
    }
  } else {
    assert(closureInfo.closure is ThinToThickFunctionInst)
    let type = ai.parentFunction.mapTypeIntoEnvironment(
      closureInfo.closure.type.rawType.mapOutOfEnvironment()).canonical.loweredType(in: ai.parentFunction)
    rootClosure = builder.createThinToThickFunction(thinFunction: newFri, resultType: type)
  }

  var allNewClosures : [SingleValueInstruction] = [rootClosure]

  var currentClosure = rootClosure
  for adapter in closureInfo.adapters.dropLast() {
    if let conversion = adapter as? ConvertFunctionInst {
      currentClosure = recreateFunctionConversion(conversion, of: currentClosure, builder: builder)
      allNewClosures.append(currentClosure)
      continue
    }
    let reabstraction = adapter as! PartialApplyInst
    let reabstractionFn = reabstraction.asSupportedClosureFn!
    let reabstractionFri = builder.createFunctionRef(reabstractionFn)
    let newReabstraction = builder.createPartialApply(
      function: reabstractionFri,
      substitutionMap: reabstraction.substitutionMap.substitute(with: ai.parentFunction.forwardingSubstitutionMap),
      capturedArguments: [currentClosure],
      calleeConvention: reabstraction.calleeConvention,
      hasUnknownResultIsolation: reabstraction.hasUnknownResultIsolation,
      isOnStack: reabstraction.isOnStack,
      isNested: reabstraction.isNested,
      executionSemantics: reabstraction.executionSemantics)
    currentClosure = newReabstraction
    allNewClosures.append(newReabstraction)
  }

  let newAi: ApplyInst
  if let conversion = closureInfo.adapters.last as? ConvertFunctionInst {
    let convertedClosure = recreateFunctionConversion(conversion, of: currentClosure, builder: builder)
    allNewClosures.append(convertedClosure)
    // Keep the converted callable's ABI. SILCombine subsequently folds the
    // conversion and partial application, including any required argument casts.
    newAi = builder.createApply(
      function: convertedClosure, ai.substitutionMap, arguments: Array(ai.arguments),
      isNonThrowing: ai.isNonThrowing, isNonAsync: ai.isNonAsync,
      specializationInfo: ai.specializationInfo, argumentLocationsFrom: ai)
  } else {
    let reabstraction = closureInfo.adapters.last as! PartialApplyInst
    var newArgs = Array(ai.arguments)
    newArgs.append(currentClosure)
    let fn = reabstraction.asSupportedClosureFn!
    let newFriForAi = builder.createFunctionRef(fn)
    newAi = builder.createApply(
      function: newFriForAi,
      reabstraction.substitutionMap.substitute(with: ai.parentFunction.forwardingSubstitutionMap),
      arguments: newArgs)
  }
  ai.replace(with: newAi, context)

  for newClosure in allNewClosures.reversed() {
    insertLifetimeEndIfNeeded(for: newClosure, before: newAi.parentBlock.terminator, context)
  }

  // TODO: maybe we can set insertion point earlier
  for res in extractedElements {
    insertLifetimeEndIfNeeded(for: res, before: newAi.parentBlock.terminator, context)
  }
}

private func rewriteApplyUse(
  ai: ApplyInst, resultIdx: Int,
  result: Value, rewriteCtx: PayloadRewriteContext, _ context: FunctionPassContext
) {
  let builder = Builder(before: ai, context)
  if let closureInfo = findMatchingClosureInfo(
    in: rewriteCtx.closureInfoArray, forPayloadIndex: resultIdx)
  {
    let extractedElements = extractTupleElements(
      from: result, builder: builder)
    rewriteApplyDirectClosure(
      ai: ai, closureInfo: closureInfo, extractedElements: extractedElements,
      builder: builder, context)
  } else {
    ai.setOperand(at: ApplyOperandConventions.calleeIndex, to: result, context)
  }
}

private func rewriteDestroyValueUse(
  dvi: DestroyValueInst, resultIdx: Int,
  result: Value, rewriteCtx: PayloadRewriteContext, _ context: FunctionPassContext
) {
  let isClosurePayload = rewriteCtx.closureInfoArray.contains { $0.indexInPayload == resultIdx }
  if !isClosurePayload && !result.type.isTrivial(in: dvi.parentFunction) {
    dvi.setOperand(at: 0, to: result, context)
  } else {
    context.erase(instruction: dvi)
  }
}

private func rewriteUsesOfPayloadItem(
  use: Operand, resultIdx: Int,
  result: Value, rewriteCtx: PayloadRewriteContext, _ context: FunctionPassContext
) {
  guard let payloadUseShape = classifyPayloadUse(use) else {
    assert(false)
    return
  }

  switch payloadUseShape {
  case .apply(let ai):
    rewriteApplyUse(
      ai: ai, resultIdx: resultIdx,
      result: result, rewriteCtx: rewriteCtx, context)

  case .destroyValue(let dvi):
    rewriteDestroyValueUse(
      dvi: dvi, resultIdx: resultIdx,
      result: result, rewriteCtx: rewriteCtx, context)

  case .uncheckedEnumData(let uedi):
    let builder = Builder(before: uedi, context)
    let newUedi = builder.createUncheckedEnumData(
      enum: result, caseIndex: uedi.caseIndex,
      resultType: result.type.getEnumCases(in: uedi.parentFunction)![uedi.caseIndex]!.payload!)
    uedi.replace(with: newUedi, context)

  case .switchEnum(let sei):
    let builder = Builder(before: sei, context)
    builder.createSwitchEnum(
      enum: result, cases: getEnumCasesForSwitchEnumInst(sei))
    context.erase(instruction: sei)
  }
}

// ===================== Utility functions and extensions ===================== //

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
private func findOptionalNoneMatchingOptionalSome(
  in vjp: Function, closuresInBTE: [ClosureInBTE], bteTypes: Set<Type>
) -> [ClosureInBTE]? {
  let branchTracingEnumInstructions: [EnumInst] = vjp.instructions.filter { $0 is EnumInst }.map {
    $0 as! EnumInst
  }.filter {
    $0.type.isBranchTracingEnum(in: vjp)
      && bteTypes.contains(remapBranchTracingEnumType($0.type, into: vjp))
  }

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
    guard let first = bteWithNoneArr.first else { continue }
    let noneType = remapBranchTracingEnumType(first.type, into: vjp)
    // CFG duplication can produce several constructors of the same bound
    // case. Their payload values have already been checked for equality.
    // Different counterpart cases are ambiguous and remain unsupported.
    guard bteWithNoneArr.allSatisfy({
      remapBranchTracingEnumType($0.type, into: vjp) == noneType
        && $0.caseIndex == first.caseIndex
    }) else { return nil }

    for bteWithNone in bteWithNoneArr {
      let payloadTuple = bteWithNone.operands.singleElement!.value as! TupleInst
      let optionalNone = payloadTuple.operands.last!.value as! EnumInst
      assert(optionalNone.isOptionalNone)

      let info = ClosureInBTE(
        closure: closureInBTE.closure,
        adapters: closureInBTE.adapters,
        optionalWrapper: optionalNone,
        useInPayload: payloadTuple.operands.last!,
        enumCase: bteWithNone.type.getEnumCases(in: vjp)![bteWithNone.caseIndex]!
      )
      // Several constructors can share one payload tuple.
      if !closuresInBTEForOptionalNone.contains(info) {
        closuresInBTEForOptionalNone.append(info)
      }
    }
  }

  return closuresInBTEForOptionalNone
}

// Function-test hooks for whole-tuple payloads use the same origin analysis as
// projected fields. Production rewriting consumes the complete flow plan.
private func getBTEPayloadArgOfPbBBInfo(_ bb: BasicBlock, vjp: Function)
  -> (arg: Argument, enumCase: EnumCase)?
{
  for argument in bb.arguments {
    if let flow = PayloadFlow(argument, vjp: vjp), flow.fieldIndex == nil {
      return (argument, flow.origins[0])
    }
  }
  return nil
}

extension ClosureInBTE {
  var capturedArgs : [Value] {
    if let pai = self.closure as? PartialApplyInst {
      var paiArgs = Array(pai.arguments)
      for idx in 0..<paiArgs.count {
        if paiArgs[idx].type.isAddress {
          assert(paiArgs[idx].uses.count == 3)
          let store = paiArgs[idx].uses.filter { $0.instruction is StoreInst }.map{ $0.instruction as! StoreInst }.singleElement!
          paiArgs[idx] = store.source
        }
      }
      return paiArgs
    }
    guard self.closure is ThinToThickFunctionInst else {
      fatalError("unexpected closure type")
    }
    return []
  }
}

private extension Instruction {
  var asSupportedClosure: SingleValueInstruction? {
    switch self {
    case let tttf as ThinToThickFunctionInst where tttf.callee is FunctionRefInst:
      return tttf
    case let pai as PartialApplyInst
    where pai.callee is FunctionRefInst:
      // An opened existential substitution implicitly captures metadata and
      // witnesses whose defining instruction belongs to the VJP. Explicit
      // capture arguments alone cannot reconstruct that environment in the pullback.
      guard !pai.substitutionMap.replacementTypes.contains(where: { $0.hasExistentialArchetype }) else {
        return nil
      }
      // BTE specialization loads and removes indirect captures, so it requires
      // a dedicated stack allocation even for conventions supported by the general pass.
      for op in pai.argumentOperands {
        // A mutable capture's SIL box cannot be stored in an AST enum payload.
        // Keep the callback opaque so its box identity and sharing are preserved.
        guard !op.value.type.isBox else { return nil }
        guard op.value.type.isAddress else {
          continue
        }
        // Reconstruct address captures for guaranteed-indirect calls.
        guard pai.convention(of: op) == .indirectInGuaranteed else { return nil }
        guard let allocStack = op.value as? AllocStackInst else {
          return nil
        }
        guard allocStack.uses.count == 3 else {
          return nil
        }
        guard let storeUse = allocStack.uses.filter({ $0.instruction is StoreInst }).singleElement else {
          return nil
        }
        let store = storeUse.instruction as! StoreInst
        guard case .initialize = store.storeOwnership else {
          return nil
        }
        guard let paiUse = allocStack.uses.filter({ $0.instruction is PartialApplyInst }).singleElement else {
          return nil
        }
        assert(paiUse.instruction == pai)
        guard allocStack.uses.filter({ $0.instruction is DeallocStackInst }).singleElement != nil else {
          return nil
        }
        continue
      }
      return pai
    default:
      return nil
    }
  }

  var asSupportedClosureFn: Function? {
    let callee: Value?
    switch asSupportedClosure {
    case let tttf as ThinToThickFunctionInst:
      callee = tttf.callee
    case let pai as PartialApplyInst:
      callee = pai.callee
    default:
      callee = nil
    }
    return (callee as? FunctionRefInst)?.referencedFunction
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
private func findBTEUses(for rootClosure: SingleValueInstruction) -> [ClosureInBTE]? {
  log("findBTEUses: running for \(rootClosure)")
  let vjp = rootClosure.parentFunction
  var closuresInBTE = [ClosureInBTE]()

  var adapters = [SingleValueInstruction]()
  var currentClosure = rootClosure
  while let singleUse = currentClosure.uses.singleElement {
    if let conversion = singleUse.instruction as? ConvertFunctionInst {
      // Preserve ordinary ABI conversions in the reconstruction recipe. The
      // scoped escaping/discarding conversions require separate lifetime rules.
      guard conversion.conversionKind == .ordinary else { return [] }
      currentClosure = conversion
      adapters.append(conversion)
      continue
    }
    guard let pai = singleUse.instruction as? PartialApplyInst else {
      break
    }

    // Decline an unknown callee before any enum is synthesized. Only actual
    // reabstraction/subset thunks are transparent; an ordinary closure remains
    // a root even when its only capture is another function.
    guard pai.callee is FunctionRefInst else { return nil }
    guard pai.isPartialApplyOfThunk else { break }
    guard pai.asSupportedClosure != nil else { return [] }

    currentClosure = pai
    adapters.append(pai)
  }

  // Generated taped pullbacks are nonthrowing. Leave unexpected throwing
  // callables opaque rather than reconstructing their error-result ABI.
  // Check the outer callable: an inner throws(Never) root may have a
  // nonthrowing reabstraction thunk which we can still specialize.
  guard FunctionConvention(for: currentClosure.type.rawType.canonical, in: vjp).errorResult == nil
  else { return [] }

  let optionalWrapper = currentClosure.uses.singleElement?.instruction.asOptionalWrapper
  let closure = optionalWrapper ?? currentClosure

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
        adapters: adapters,
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

private func findClosuresInBTE(paiOfPullback: PartialApplyInst, bteTypes: Set<Type>) -> [ClosureInBTE]? {
  let vjp = paiOfPullback.parentFunction
  var adapters = Set<SingleValueInstruction>()
  var closuresInBTE = [ClosureInBTE]()
  for inst in vjp.instructions {
    guard inst != paiOfPullback,
          let rootClosure = inst.asSupportedClosure,
          !adapters.contains(rootClosure)
    else {
      continue
    }

    guard let currentClosuresInBTE = findBTEUses(for: rootClosure) else { return nil }
    adapters.formUnion(currentClosuresInBTE.flatMap(\.adapters))
    // Keep a shared root opaque if any of its payload uses remains outside
    // this plan. Its captures cannot be consumed while that closure survives.
    if currentClosuresInBTE.allSatisfy({ bteTypes.contains($0.enumCase.enumType(in: vjp)) }) {
      closuresInBTE.append(contentsOf: currentClosuresInBTE)
    }
  }

  guard let optionalNoneClosures = findOptionalNoneMatchingOptionalSome(
    in: vjp, closuresInBTE: closuresInBTE, bteTypes: bteTypes) else { return nil }
  closuresInBTE.append(contentsOf: optionalNoneClosures)

  return closuresInBTE
}

private typealias SpecializedBTEMap = [Type: Type]

// Retyping an enum also changes its SSA uses. The current rewrite handles
// forwarding through phis, predecessor payloads and the returned pullback,
// but cannot change another callback's signature or an unrelated tape schema.
private func validateVJPTapeScope(
  vjp: Function, bteTypes: Set<Type>, payloads: Set<TupleInst>,
  paiOfPullback: PartialApplyInst
) -> Bool {
  func isSelected(_ value: Value) -> Bool {
    value.type.isBranchTracingEnum(in: vjp)
      && bteTypes.contains(remapBranchTracingEnumType(value.type, into: vjp))
  }
  func hasSupportedUses(_ value: Value) -> Bool {
    value.uses.allSatisfy { use in
      switch use.instruction {
      case is BranchInst, is CondBranchInst, is DebugValueInst:
        return true
      case let tuple as TupleInst:
        return payloads.contains(tuple)
      default:
        return use.instruction == paiOfPullback
      }
    }
  }
  for block in vjp.blocks {
    for argument in block.arguments where isSelected(argument) {
      guard let phi = Phi(argument),
            phi.incomingOperands.allSatisfy({ $0.value is Argument || $0.value is EnumInst }),
            hasSupportedUses(argument) else { return false }
    }
    for instruction in block.instructions {
      for result in instruction.results where isSelected(result) {
        guard instruction is EnumInst, hasSupportedUses(result) else { return false }
      }
    }
  }
  return true
}

private struct PullbackClosureAnalysis {
  let paiOfPullback: PartialApplyInst
  let bteSpecializationQueue: [Type]
  let payloads: Set<TupleInst>
  let closuresInBTE: [ClosureInBTE]
  // Nil means unsupported pullback flow; schema-only queries remain usable.
  let validatedPayloadFlows: [PayloadFlow]?

  var vjp: Function { paiOfPullback.parentFunction }
  var pullback: Function { paiOfPullback.referencedFunction! }

  init?(vjp: Function) {
    guard let paiOfPullback = getPartialApplyOfPullbackInExitVJPBB(vjp: vjp),
          let pullback = paiOfPullback.referencedFunction,
          pullback.isDefinition, pullback.hasOwnership else { return nil }

    // BTE specialization runs after serialization. Before then, newly exposed
    // callback references or the shared specialized pullback may be unavailable
    // to a serialized body. Decline before synthesizing any tape declarations.
    guard !vjp.isAnySerialized, !pullback.isAnySerialized else { return nil }

    // Check the actual pullback after ordinary specialization. Capture
    // reconstruction consumes nontrivial tapes, as generated pullbacks do;
    // borrowed tapes require a different ownership rewrite.
    for argument in pullback.entryBlock.arguments where argument.type.isBranchTracingEnum(in: vjp) {
      guard argument.type.isTrivial(in: pullback) || argument.ownership == .owned else { return nil }
    }

    // A nested pullback may capture another instantiation of the same generic
    // function's tape. Only specialize the returned tape's predecessor graph;
    // preserve the nested tape together with the function that consumes it.
    if let topBTE = pullback.entryBlock.getBranchTracingEnumArg(vjp: vjp) {
      guard let queue = getBranchTracingEnumSpecializationQueue(
        topBTEType: remapBranchTracingEnumType(topBTE.type, into: vjp), in: vjp)
      else { return nil }
      self.bteSpecializationQueue = queue
    } else {
      self.bteSpecializationQueue = []
    }
    let bteTypes = Set(bteSpecializationQueue)

    // A pullback case identifies one VJP payload tuple. Distinct constructors
    // sharing that case are only supported when CFG simplification duplicated
    // an identical ordered list of saved SSA values. Decline different plans
    // before synthesizing any specialized enum declarations.
    var payloadsByCase = [Type: [Int: TupleInst]]()
    var payloads = Set<TupleInst>()
    for (enumInst, payload) in findEnumsAndPayloadsInVjp(vjp: vjp) {
      let enumType = remapBranchTracingEnumType(enumInst.type, into: vjp)
      guard bteTypes.contains(enumType) else { continue }
      if let previous = payloadsByCase[enumType]?[enumInst.caseIndex],
         !previous.hasSamePayloadValues(as: payload) {
        return nil
      }
      payloadsByCase[enumType, default: [:]][enumInst.caseIndex] = payload
      payloads.insert(payload)
    }
    self.payloads = payloads
    // Replacing a tuple changes all of its enum consumers. Do not cross the
    // boundary into an enum whose declaration is being preserved. Empty
    // tuples are never rewritten and can be shared with a nested tape.
    guard payloads.allSatisfy({ payload in
      payload.operands.isEmpty || payload.uses.allSatisfy { use in
        guard let enumInst = use.instruction as? EnumInst else { return false }
        return enumInst.type.isBranchTracingEnum(in: vjp)
          && bteTypes.contains(remapBranchTracingEnumType(enumInst.type, into: vjp))
      }
    }) else { return nil }
    guard validateVJPTapeScope(
      vjp: vjp, bteTypes: bteTypes, payloads: payloads, paiOfPullback: paiOfPullback)
    else { return nil }

    self.paiOfPullback = paiOfPullback
    guard let closuresInBTE = findClosuresInBTE(paiOfPullback: paiOfPullback, bteTypes: bteTypes)
    else { return nil }
    // Optional callbacks are supported when the pullback only destroys the
    // tape. Live optional payloads require a separate successor rewrite.
    guard paiOfPullback.referencedFunction!.blocks.singleElement != nil
            || closuresInBTE.allSatisfy({ $0.optionalWrapper == nil }) else { return nil }
    self.closuresInBTE = closuresInBTE
    self.validatedPayloadFlows = closuresInBTE.isEmpty ? [] : analyzePayloadFlows(
      in: paiOfPullback.referencedFunction!, vjp: vjp,
      bteTypes: bteTypes, payloadsByCase: payloadsByCase, closureInfos: closuresInBTE)
  }
}

private struct AutoDiffSpecializationInfo {
  let closureAnalysis: PullbackClosureAnalysis
  let specializedBTEDict: [Type: Type]

  var paiOfPullback: PartialApplyInst { closureAnalysis.paiOfPullback }
  var closuresInBTE: [ClosureInBTE] { closureAnalysis.closuresInBTE }
  var vjp: Function { closureAnalysis.vjp }
  var pullback: Function { closureAnalysis.pullback }

  init(closureAnalysis: PullbackClosureAnalysis, specializedBTEDict: [Type: Type]) {
    self.closureAnalysis = closureAnalysis
    self.specializedBTEDict = specializedBTEDict
  }

  init?(vjp: Function, _ context: FunctionPassContext) {
    guard let closureAnalysis = PullbackClosureAnalysis(vjp: vjp),
          let specializedBTEDict = synthesizeSpecializedEnums(
            from: closureAnalysis, context: context)
    else { return nil }
    self.init(closureAnalysis: closureAnalysis, specializedBTEDict: specializedBTEDict)
  }

  func pullbackNameSpecializedAgainstBTE(_ context: FunctionPassContext) -> String {
    let operand = paiOfPullback.argumentOperands.filter {
      $0.value.type.isBranchTracingEnum(in: vjp)
    }.singleElement!
    let originalType = remapBranchTracingEnumType(operand.value.type, into: vjp)
    return context.mangle(
      withSpecializedBranchTracingEnum: specializedBTEDict[originalType]!,
      argIdx: paiOfPullback.calleeArgumentIndex(of: operand)!,
      in: vjp, from: pullback)
  }
}

private func computeTopologicalBlockOrder(of function: Function) -> [BasicBlock]? {
  var visited: Set<BasicBlock> = [function.entryBlock]
  var order = [function.entryBlock]
  while order.count < function.blocks.count {
    let previousCount = order.count
    for bb in function.blocks where !visited.contains(bb) {
      if bb.predecessors.allSatisfy({ visited.contains($0) }) {
        visited.insert(bb)
        order.append(bb)
      }
    }
    // Source loop tapes are rejected separately. Still decline an unexpected
    // cyclic pullback instead of spinning forever on its unvisited blocks.
    guard order.count > previousCount else { return nil }
  }
  return order
}

// All paths into a logical payload value must agree on the callable plan used
// to rewrite it. Whole tuples and projected fields share this check.
private func analyzePayloadFlows(
  in pullback: Function, vjp: Function, bteTypes: Set<Type>,
  payloadsByCase: [Type: [Int: TupleInst]],
  closureInfos: [ClosureInBTE]
) -> [PayloadFlow]? {
  let allFlows = pullback.blocks.flatMap { block in
    block.arguments.compactMap { PayloadFlow($0, vjp: vjp) }
  }
  var flows = [PayloadFlow]()
  for flow in allFlows {
    let selectedOrigins = flow.origins.filter { bteTypes.contains($0.enumType(in: vjp)) }
    if selectedOrigins.isEmpty { continue }
    // A phi cannot mix a changed payload with an unchanged nested tape.
    guard selectedOrigins.count == flow.origins.count else { return nil }
    flows.append(flow)
  }
  let forwardedPayloads = Set(flows.flatMap { $0.forwardedPayloads })
  var validated = [PayloadFlow]()
  for var flow in flows {
    var commonPlan: [ClosureInBTE]? = nil
    for origin in flow.origins {
      let payload = payloadsByCase[origin.enumType(in: vjp)]?[origin.index]
      let plan = payload.map {
        collectMatchingClosureInfos(forPayload: $0, allClosureInfos: closureInfos).filter {
          flow.fieldIndex == nil || $0.indexInPayload == flow.fieldIndex
        }
      } ?? []
      if payload == nil && closureInfos.contains(where: {
        $0.enumCase.enumType(in: vjp) == origin.enumType(in: vjp)
          && $0.enumCase.index == origin.index
      }) { return nil }
      if let commonPlan {
        guard commonPlan.count == plan.count,
              plan.allSatisfy({ info in commonPlan.contains(where: {
                $0.indexInPayload == info.indexInPayload && $0.closure == info.closure
                  && $0.adapters == info.adapters
                  && $0.optionalWrapper == info.optionalWrapper
              }) }) else { return nil }
      } else {
        commonPlan = plan
      }
    }
    flow.closureInfos = commonPlan ?? []
    let values: [Value]
    if flow.fieldIndex != nil {
      guard flow.argument.type.isLoweredFunction
              || (flow.argument.type.isBranchTracingEnum(in: vjp)
                    && !flow.argument.uses.isAtLeastTwo) else { return nil }
      values = [flow.argument]
    } else {
      switch getPayloadValues(payload: flow.argument, vjp: vjp) {
      case .zeroUses: values = []
      case .unsupported: return nil
      case .destructureTuple(let results), .tupleExtract(let results): values = results
      }
    }
    guard checkIfCanRunForPayloadValues(
      results: values, forwardedPayloads: forwardedPayloads,
      prefixFail: "", pb: pullback, pbBB: flow.argument.parentBlock) else { return nil }
    validated.append(flow)
  }
  // A switch supplies a whole payload without an explicit incoming operand.
  // Every such successor must have a validated flow before its type is changed.
  for block in pullback.blocks {
    if let dispatch = block.terminator as? SwitchEnumInst,
       dispatch.enumOp.type.isBranchTracingEnum(in: vjp),
       bteTypes.contains(remapBranchTracingEnumType(dispatch.enumOp.type, into: vjp)) {
      guard block.successors.allSatisfy({ successor in
        validated.contains { $0.argument.parentBlock == successor && $0.fieldIndex == nil }
      }) else { return nil }
    }
  }
  return validated
}

extension ClosureInBTE {
  /// The value that actually appears as the payload-tuple operand for this closure:
  /// the optional wrapper, outermost adapter, or raw closure.
  var valueInPayload: Value {
    if let optionalWrapper { return optionalWrapper }
    if let reabstraction = adapters.last { return reabstraction }
    return closure
  }
}

private func collectMatchingClosureInfos(
  forPayload ti: TupleInst, allClosureInfos: [ClosureInBTE]
) -> [ClosureInBTE] {
  allClosureInfos.filter { info in
    info.payloadTuple == ti && ti.operands.contains { $0.value == info.valueInPayload }
  }
}

private struct PullbackSpecializationAgainstBTE {
  typealias Cloner = SIL.Cloner<FunctionPassContext>

  func getOrCreatePullbackSpecializedAgainstBTE(
    basedOn autodiffSpecializationInfo: AutoDiffSpecializationInfo,
    enumDict: SpecializedBTEMap,
    _ context: FunctionPassContext
  )
    -> (function: Function, alreadyExists: Bool)
  {
    let pb = autodiffSpecializationInfo.pullback
    let vjp = autodiffSpecializationInfo.paiOfPullback.parentFunction

    let specializedPbName = autodiffSpecializationInfo.pullbackNameSpecializedAgainstBTE(context)
    if let specializedPb = context.lookupFunction(name: specializedPbName) {
      return (specializedPb, true)
    }

    let enumTypeOfEntryBBArg = remapBranchTracingEnumType(
      pb.entryBlock.getBranchTracingEnumArg(vjp: vjp)!.type, into: vjp)

    let specializedParameters = getParametersForPullbackSpecializedAgainstBTE(
      basedOn: autodiffSpecializationInfo, pb: pb, enumType: enumTypeOfEntryBBArg,
      enumDict: enumDict,
      context)

    let specializedPb =
      context.createSpecializedFunctionDeclaration(
        from: pb, withName: specializedPbName,
        withParams: specializedParameters,
        makeBare: true)

    context.buildSpecializedFunction(
      specializedFunction: specializedPb,
      buildFn: { (specializedPb, specializedContext) in
        var cloner = Cloner(cloneToEmptyFunction: specializedPb, specializedContext)
        defer { cloner.deinitialize() }

        cloneAndSpecializePullbackAgainstBTE(
          using: &cloner, autodiffSpecializationInfo: autodiffSpecializationInfo, enumDict: enumDict
        )
        // Cloning a whole function, even if it contains an `unreachable`, doesn't require lifetime completion.
        specializedContext.setNeedCompleteLifetimes(to: false)
      })

    return (specializedPb, false)
  }

  private typealias PayloadProjection = (
    resultIdx: Int, oldResultUses: [Operand], replacementValue: Value
  )

  private func rebuildPayloadProjections(
    of payload: Argument, fieldIndex: Int?, originalUses: [Operand],
    context: FunctionPassContext
  ) -> (values: [PayloadProjection], obsoleteDestructure: DestructureTupleInst?) {
    if let fieldIndex {
      // Predecessors have remapped the enum data. Recreate its field projections
      // with the specialized types, leaving the phi in canonical field form.
      for operand in Phi(payload)!.incomingOperands {
        let oldProjection = operand.value.definingInstruction!
        let builder = Builder(before: oldProjection, context)
        if let old = oldProjection as? DestructureTupleInst {
          let replacement = builder.createDestructureTuple(tuple: old.tuple)
          for (oldResult, newResult) in zip(old.results, replacement.results) {
            oldResult.uses.replaceAll(with: newResult, context)
          }
          context.erase(instruction: old)
        } else {
          let old = oldProjection as! TupleExtractInst
          old.replace(with: builder.createTupleExtract(
            tuple: old.tuple, elementIndex: fieldIndex), context)
        }
      }
      return ([(fieldIndex, originalUses, payload)], nil)
    }
    if let singleUse = payload.uses.singleUse,
       let oldDti = singleUse.instruction as? DestructureTupleInst {
      let builder = Builder(before: oldDti, context)
      let newDti = builder.createDestructureTuple(tuple: oldDti.tuple)
      let projections = oldDti.results.enumerated().map { resultIdx, result in
        (
          resultIdx: resultIdx,
          oldResultUses: Array(result.uses),
          replacementValue: newDti.results[resultIdx] as Value
        )
      }
      // Keep the old consuming projection until all its result uses are rewritten.
      return (projections, oldDti)
    }

    var projections = [PayloadProjection]()
    for use in Array(payload.uses) {
      let oldTei = use.instruction as! TupleExtractInst
      let builder = Builder(before: oldTei, context)
      let newTei = builder.createTupleExtract(tuple: oldTei.tuple, elementIndex: oldTei.fieldIndex)
      projections.append((
        resultIdx: oldTei.fieldIndex,
        oldResultUses: Array(oldTei.results[0].uses),
        replacementValue: newTei.results[0]))
      oldTei.replace(with: newTei, context)
    }
    return (projections, nil)
  }

  private func rewriteProjectedPayloadValues(
    projections: [PayloadProjection],
    rewriteCtx: PayloadRewriteContext,
    context: FunctionPassContext
  ) {
    for projection in projections {
      for use in projection.oldResultUses {
        rewriteUsesOfPayloadItem(
          use: use,
          resultIdx: projection.resultIdx,
          result: projection.replacementValue,
          rewriteCtx: rewriteCtx,
          context)
      }
    }
  }

  func cloneAndSpecializePullbackAgainstBTE(
    using cloner: inout Cloner, autodiffSpecializationInfo: AutoDiffSpecializationInfo,
    enumDict: SpecializedBTEMap
  ) {
    // Clone with the original operand types. Specializing the tape afterwards
    // keeps type-changing ownership cleanup local to this transformation.
    cloner.cloneFunctionBody(from: autodiffSpecializationInfo.pullback)
    specializeEntryTapeArgument(
      using: &cloner, info: autodiffSpecializationInfo, enumDict: enumDict)

    let vjp = autodiffSpecializationInfo.paiOfPullback.parentFunction
    let bbQueue = computeTopologicalBlockOrder(of: autodiffSpecializationInfo.pullback)!
    let flowsByBlock = Dictionary(grouping: autodiffSpecializationInfo.closureAnalysis.validatedPayloadFlows!) {
      $0.argument.parentBlock
    }

    for originalBlock in bbQueue {
      let bb = cloner.getClonedBlock(for: originalBlock)
      // With single-bb, we've ensured that there are no uses of BTE arg, so no manipulation required
      if bb == cloner.targetFunction.entryBlock && cloner.targetFunction.blocks.singleElement == nil
      {
        let bteArg = bb.getBranchTracingEnumArg(
          vjp: autodiffSpecializationInfo.paiOfPullback.parentFunction)!
        let sei = bteArg.uses.singleUse!.instruction as! SwitchEnumInst
        let builderEntry = Builder(before: sei, cloner.context)

        builderEntry.createSwitchEnum(
          enum: sei.enumOp, cases: getEnumCasesForSwitchEnumInst(sei))
        cloner.context.erase(instruction: sei)

        continue
      }

      for flow in flowsByBlock[originalBlock] ?? [] {
        let arg = cloner.getClonedValue(of: flow.argument) as! Argument
        let enumType = remapBranchTracingEnumType(
          enumDict[flow.origins[0].enumType(in: vjp)]!, into: cloner.targetFunction)
        let enumCase = enumType.getEnumCases(in: cloner.targetFunction)![flow.origins[0].index]!
        let closureInfoArray = flow.closureInfos
        let oldUses = Array(arg.uses)
        let payloadType = enumCase.payload!
        let newType = flow.fieldIndex.map { payloadType.tupleElements[$0] } ?? payloadType
        let newArg = bb.replacePhiArgumentAndReplaceAllUses(
          at: arg.index, type: newType,
          ownership: newType.isTrivial(in: cloner.targetFunction) ? .none : arg.ownership,
          cloner.context)

        if newArg.uses.isEmpty && flow.fieldIndex == nil {
          continue
        }

        let projections = rebuildPayloadProjections(
          of: newArg, fieldIndex: flow.fieldIndex, originalUses: oldUses, context: cloner.context)
        let rewriteCtx = PayloadRewriteContext(
          closureInfoArray: closureInfoArray)
        rewriteProjectedPayloadValues(
          projections: projections.values,
          rewriteCtx: rewriteCtx,
          context: cloner.context)
        if let oldDti = projections.obsoleteDestructure {
          cloner.context.erase(instruction: oldDti)
        }
      }
    }
  }

  private func specializeEntryTapeArgument(
    using cloner: inout Cloner, info: AutoDiffSpecializationInfo,
    enumDict: SpecializedBTEMap
  ) {
    let sourceArg = info.pullback.entryBlock.getBranchTracingEnumArg(vjp: info.vjp)!
    let enumType = remapBranchTracingEnumType(sourceArg.type, into: info.vjp)
    let function = cloner.targetFunction
    let entry = function.entryBlock
    let oldArg = entry.arguments[sourceArg.index] as! FunctionArgument
    let newType = remapBranchTracingEnumType(enumDict[enumType]!, into: function)
    let isTrivial = newType.isTrivial(in: function)
    let newArg = entry.insertFunctionArgument(
      atPosition: oldArg.index, type: newType,
      ownership: isTrivial ? .none : oldArg.ownership, cloner.context)
    newArg.copyFlags(from: oldArg, cloner.context)
    for use in Array(oldArg.uses) {
      if isTrivial, let destroy = use.instruction as? DestroyValueInst {
        cloner.context.erase(instruction: destroy)
      } else {
        use.set(to: newArg, cloner.context)
      }
    }
    entry.eraseArgument(at: oldArg.index, cloner.context)
  }
}

let getAutoDiffSpecializationInfoTest = FunctionTest("autodiff_get_specialization_info") {
  function, arguments, context in
  guard let autodiffSpecializationInfo = AutoDiffSpecializationInfo(vjp: function, context) else {
    print("Unsupported branch tracing enum specialization")
    return
  }
  print("Run getAutoDiffSpecializationInfo for VJP \(function.name): autodiffSpecializationInfo = (")
  print("  pullback = \(autodiffSpecializationInfo.pullback.name)")
  print("  closuresInBTE = [")
  for closureInBTE in autodiffSpecializationInfo.closuresInBTE {
    print("    ClosureInBTE(")
    print("      closure: \(closureInBTE.closure)")
    print("      adapters: " + (closureInBTE.adapters.isEmpty ? "[]" : "["))
    for reabstraction in closureInBTE.adapters {
      print("        \(reabstraction)")
    }
    if !closureInBTE.adapters.isEmpty {
      print("    ]")
    }
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
  guard let autodiffSpecializationInfo = AutoDiffSpecializationInfo(vjp: function, context) else {
    print("Unsupported branch tracing enum specialization")
    return
  }
  let specializedBTEDict = autodiffSpecializationInfo.specializedBTEDict
  print("Specialized branch tracing enum dict for VJP \(function.name) contains \(specializedBTEDict.count) elements:")

  var keys = [Type](specializedBTEDict.keys)

  func compareTypes(lhs: Type, rhs: Type) -> Bool { "\(lhs)" < "\(rhs)" }
  keys.sort(by: compareTypes)

  for (idx, key) in keys.enumerated() {
    let original = String(taking: key.nominal!.bridged.getDebugDescription(true))
    let specialized = String(taking: specializedBTEDict[key]!.nominal!.bridged.getDebugDescription(true))
    print("non-specialized BTE \(idx): \(original)")
    print("specialized BTE \(idx): \(specialized)")
  }
  print("")
}

let specializeBTEArgInVjpBB = FunctionTest("autodiff_specialize_bte_arg_in_vjp_bb") {
  function, arguments, context in
  guard let autodiffSpecializationInfo = AutoDiffSpecializationInfo(vjp: function, context) else {
    print("Unsupported branch tracing enum specialization")
    return
  }
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
  guard let autodiffSpecializationInfo = AutoDiffSpecializationInfo(vjp: function, context) else {
    print("Unsupported branch tracing enum specialization")
    return
  }
  let specializedBTEDict = autodiffSpecializationInfo.specializedBTEDict

  let pullback = autodiffSpecializationInfo.pullback
  print("Specialized BTE payload arguments of basic blocks in pullback \(pullback.name):")
  for bb in pullback.blocks {
    guard
      let (arg, enumCase) = getBTEPayloadArgOfPbBBInfo(bb, vjp: function)
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
  }
  print("")
}
