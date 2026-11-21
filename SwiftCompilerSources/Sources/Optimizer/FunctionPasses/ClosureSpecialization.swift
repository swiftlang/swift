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

    // Keep differentiable_function specialization logic in closure-specialization pass rather than in autodiff-closure-specialization
    // pass since ordering of the latter pass does not allow for full specialization in some cases. See test8 in
    // test/AutoDiff/validation-test/differentiable_function_specialization.swift
    for inst in function.instructions {
      if let apply = inst as? FullApplySite {
        if trySpecializeDifferentiableFunction(apply: apply, isRootInvocation: isRootInvocation, context) {
          changed = true
        }
      }
    }

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

  let branchTracingEnums = screenVJPForBTESpecialization(function)
  for _ in 0..<5 {
    guard let partialApply = getReturnedPullback(of: function, requireSingleNonDebugUse: true) else { break }
    let changed = trySpecialize(apply: partialApply, isRootInvocation: true, context)
    if context.needFixStackNesting {
      context.fixStackNesting(in: function)
    }
    if !changed { break }
  }

  guard let branchTracingEnums, let plan = BTESpecializationPlan(branchTracingEnums, context) else { return }
  specializeAgainstBTE(plan, context: context)
}

// ===================== Utility functions and extensions ===================== //

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

  let closureArgumentValues = specialization.closureArguments.map { $0.0.value }

  specialization.rewriteApply(for: specializedFunction, context)

  specialization.deleteDeadClosures(passedAs: closureArgumentValues, context)

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

private func trySpecializeDifferentiableFunction(apply: ApplySite, isRootInvocation: Bool, _ context: FunctionPassContext) -> Bool {
  guard isCalleeSpecializable(of: apply),
        let (specialization, argumentManglings) = analyzeDifferentiableFunctionArguments(of: apply, context)
  else {
    return false
  }

  let specializedParameters = specialization.getSpecializedParameters()
  let name = context.mangle(withDifferentiableFunctionArguments: argumentManglings,
                            from: apply.referencedFunction!)
  let (specializedFunction, isNewFunction) = specialization.getOrCreateSpecializedFunction(
    specializedParameters, context, named: name)

  specialization.rewriteApply(for: specializedFunction, context)

  if isNewFunction && isRootInvocation {
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
     callee != apply.parentFunction,

     // Avoid an infinite specialization loop: if a closure is called and also captured in another
     // closure which is passed to a recursive call, each specialization of the callee contains a call
     // which can be specialized again, with even more propagated closures. E.g.
     //
     //   func foo(_ c: @escaping () -> ()) {
     //     c()
     //     foo({ c() })
     //   }
     //
     // Note that specializing nested closures in multiple rounds legitimately creates closure
     // specializations of closure specializations, e.g. three levels in closure_specialization_nested.sil.
     // A limit of 2 for the callee is good enough and will not be exceeded in "regular" optimization scenarios.
     callee.closureSpecializationLevel <= 2
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
       isClosureApplied(apply.calleeArgument(of: argOp, in: apply.referencedFunction!)!),
       // Specializing must not leave a trapping destroy behind, or move a capture that
       // another use still needs.
       isOnlyForwarded(to: argOp, from: closure)
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

private func analyzeDifferentiableFunctionArguments(of apply: ApplySite, _ context: FunctionPassContext)
  -> (SpecializationInfo, [FunctionPassContext.DifferentiableFunctionArgumentMangling])? {
  var argumentsToSpecialize = [(Operand, Closure)]()
  var argumentManglings = [FunctionPassContext.DifferentiableFunctionArgumentMangling]()

  for argOp in apply.argumentOperands {
    if let functionInfo = findSpecializableDifferentiableFunction(of: argOp.value, context),
       isClosureApplied(apply.calleeArgument(of: argOp, in: apply.referencedFunction!)!)
    {
      argumentsToSpecialize.append((argOp, functionInfo.function))
      argumentManglings.append((apply.calleeArgumentIndex(of: argOp)!, functionInfo.function,
                                functionInfo.originalRoot, functionInfo.jvpRoot, functionInfo.vjpRoot))
    }
  }
  if argumentsToSpecialize.isEmpty {
    return nil
  }

  return (SpecializationInfo(apply: apply, closureArguments: argumentsToSpecialize,
                             rootClosures: [], capturedDependencies: []), argumentManglings)
}

// Walks down the use-def chain of a function argument, recursively, to find a rootClosure.
private func findSpecializableClosure(of value: Value, _ visited: inout ValueSet,
                                      _ capturedDependencies: inout [CapturedDependency]) -> Closure? {
  visited.insert(value)

  let specializationLevelLimit = 2

  switch value {
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
    // A conversion of a closure.
    guard let operand = forwardedClosureOperand(of: value) else {
      return nil
    }
    return findSpecializableClosure(of: operand.value, &visited, &capturedDependencies)
  }
}

private func findSpecializableDifferentiableFunction(of value: Value, _ context: FunctionPassContext)
  -> (function: DifferentiableFunctionInst, originalRoot: ThinToThickFunctionInst,
      jvpRoot: ThinToThickFunctionInst, vjpRoot: ThinToThickFunctionInst)? {
  switch value {
  case is ConvertFunctionInst,
       is ConvertEscapeToNoEscapeInst,
       is MoveValueInst,
       is CopyValueInst:
    return findSpecializableDifferentiableFunction(of: (value as! UnaryInstruction).operand.value, context)

  case let differentiableFunction as DifferentiableFunctionInst:
    func findComponentRoot(_ component: DifferentiableFunctionTypeComponent) -> ThinToThickFunctionInst? {
      guard let value = differentiableFunction.getExtractee(extractee: component) else {
        return nil
      }
      var visited = ValueSet(context)
      defer { visited.deinitialize() }
      var capturedDependencies = [CapturedDependency]()
      return findSpecializableClosure(of: value, &visited, &capturedDependencies) as? ThinToThickFunctionInst
    }
    guard let originalRoot = findComponentRoot(.original),
          let jvpRoot = findComponentRoot(.jvp),
          let vjpRoot = findComponentRoot(.vjp)
    else {
      return nil
    }
    return (differentiableFunction, originalRoot, jvpRoot, vjpRoot)

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
    return getOrCreateSpecializedFunction(
      specializedParameters, context, named: getSpecializedFunctionName(context))
  }

  func getOrCreateSpecializedFunction(_ specializedParameters: [ParameterInfo],
                                      _ context: FunctionPassContext,
                                      named specializedFunctionName: String
  ) -> (Function, isNewFunction: Bool) {
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
      // a noncopyable closure can have consuming captures. When the fold above transfers a
      // non-Copyable capture to the new direct call, it can't copy it, so it leaves `Undef` in
      // `clonedRootClosure`'s own operand instead. Leaving such a closure's `partial_apply` behind
      // would still run its destructor at runtime, which would release whatever garbage is left
      // in that now-`Undef`'d capture slot.
      if clonedRootClosure.isNoncopyable {
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
        if partialApply.isNoncopyable {
          // A noncopyable closure can consume its captures and always gets a destructor even
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

  /// `argumentValues` are the values that were passed for `closureArguments`.
  func deleteDeadClosures(passedAs argumentValues: [Value], _ context: FunctionPassContext) {
    for ((_, closure), argumentValue) in zip(closureArguments, argumentValues) where !closure.isDeleted {
      // A nondeinitable closure traps when it's destroyed, so the compensating destroy of the argument
      // must not survive: move it up to the root closure, which can then be deleted.
      foldDeadForwardingChain(from: argumentValue, to: closure, context)
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
         is CopyValueInst,
         is BeginBorrowInst,
         is DifferentiableFunctionExtractInst:
      if checkRecursivelyIfClosureIsApplied(use.instruction as! SingleValueInstruction, &handledFuncs) {
        return true
      }

    case let pai as PartialApplyInst:
      if pai.isPartialApplyOfThunk {
        // `pai.isPartialApplyOfThunk` implies that the captured closure (`closure` here) is applied in the thunk.
        // If the thunk closure (`pai` here) is applied by itself as well, the closure captured by
        // the thunk closure also becomes effectively applied transitively.
        if checkRecursivelyIfClosureIsApplied(pai, &handledFuncs) {
          return true
        }
        continue
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
        if checkRecursivelyIfClosureIsApplied(pai, &handledFuncs) {
          return true
        }
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

    // Assume the closure is applied if its set as a differentiable_function component.
    case is DifferentiableFunctionInst:
      return true

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
    addMissingEndBorrowsRecursively(value: clonedArg, context)
    findValuesWhichNeedDestroyRecursively(value: clonedArg, needDestroy: &needDestroy)
  }

  while let valueToDestroy = needDestroy.pop() {
    Builder.insertCleanupAtFunctionExits(of: valueToDestroy.parentFunction, context) { builder in
      builder.createDestroyValue(operand: valueToDestroy)
    }
    completeLifetime(of: valueToDestroy, context)
  }
}

/// Adds end_borrows for the begin_borrow which is created for an owned closure argument, but also
/// for begin_borrows inside the cloned closure value chain. The latter can come from a previous
/// round of closure specialization, e.g.
/// ```
///   %1 = partial_apply %closure
///   %2 = begin_borrow %1           // created by the previous specialization round
///   %3 = convert_function %2
///   apply %function(%3) : (@guaranteed () -> ()) -> ()
/// ```
/// The cloner only clones the value chain and not the end_borrows of the begin_borrow.
///
/// Inner borrow scopes are visited first so that their end_borrows are inserted before the end_borrows
/// of outer scopes.
private func addMissingEndBorrowsRecursively(value: Value, _ context: FunctionPassContext) {
  if let beginBorrow = value as? BeginBorrowInst, beginBorrow.scopeEndingOperands.isEmpty {
    Builder.insertCleanupAtFunctionExits(of: beginBorrow.parentFunction, context) { builder in
      builder.createEndBorrow(of: beginBorrow)
    }
    completeLifetime(of: beginBorrow, context)
  }
  if let svi = value as? SingleValueInstruction {
    for op in svi.operands {
      addMissingEndBorrowsRecursively(value: op.value, context)
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
  func withSpecializedConvention(for argOp: Operand, in partialApply: PartialApplyInst, callee: Function) -> Self {
    let argType = type.loweredType(in: partialApply.parentFunction)
    let specializedParamConvention = if self.convention.isIndirect {
      self.convention
    } else if partialApply.isNoncopyable {
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

/// Returns the operand from which `value` forwards a closure, if `value` is a conversion or a
/// re-abstraction thunk of a closure. `findSpecializableClosure` uses this to look through
/// conversions, and adds checks of its own for `mark_dependence` and thunks. `isOnlyForwarded` and
/// `foldDeadForwardingChain` use it to walk the chain that `findSpecializableClosure` found.
private func forwardedClosureOperand(of value: Value) -> Operand? {
  switch value {
  case let conversion as ConvertFunctionInst:
    return conversion.operand
  case let conversion as ConvertEscapeToNoEscapeInst:
    return conversion.operand
  case let move as MoveValueInst:
    return move.operand
  case let copy as CopyValueInst:
    return copy.operand
  // In OSSA a closure can be wrapped in a `begin_borrow` before it is passed as a `@guaranteed`
  // argument to an apply. This is e.g. the case for a closure which is re-created in a
  // previously specialized function.
  case let borrow as BeginBorrowInst:
    return borrow.operand
  case let mdi as MarkDependenceInst:
    return mdi.valueOperand
  case let thunk as PartialApplyInst where thunk.isPartialApplyOfThunk:
    return thunk.argumentOperands[0]
  default:
    return nil
  }
}

/// Specializing for a closure argument replaces the argument with a destroy of it, which
/// `foldDeadForwardingChain` then moves up the chain of conversions and re-abstraction thunks from
/// `closure` to `argOp`. If the chain contains a nondeinitable closure, or `closure` has a move-only
/// capture, each noncopyable value in the chain must not have any other use, and the fold must be
/// able to reach `closure`. Otherwise a nondeinitable closure would be destroyed on the path to the
/// specialized apply, which traps, or deleted together with a destroy on another path, which skips
/// a trap. And a move-only capture would be transferred to the specialized apply while another use
/// still needs it.
private func isOnlyForwarded(to argOp: Operand, from closure: Closure) -> Bool {
  var hasNondeinitableClosure = false
  // True if each noncopyable value in the chain has no other use and the fold can reach `closure`.
  var canFold = true
  var use = argOp
  while true {
    let value = use.value
    // The context of a nondeinitable closure traps when it's destroyed.
    if value is PartialApplyInst, !value.type.isImplicitlyDestroyable(in: value.parentFunction) {
      hasNondeinitableClosure = true
    }
    // A value without ownership is never destroyed.
    if value.type.isMoveOnly, value.ownership != .none,
       !use.endsLifetime || !value.uses.ignoreDebugUses.allSatisfy({ $0 == use || $0.isDeadEndDestroy })
    {
      canFold = false
    }
    if value == closure {
      break
    }
    guard let operand = forwardedClosureOperand(of: value) else {
      return false
    }
    if value.type.isMoveOnly, !canMoveDestroys(to: operand) {
      canFold = false
    }
    use = operand
  }
  let hasMoveOnlyCapture = (closure as? PartialApplyInst).map { !$0.allArgumentsCanBeCopied } ?? false
  return canFold || !(hasNondeinitableClosure || hasMoveOnlyCapture)
}

/// True if the destroys of the value that `operand`'s instruction forwards can be replaced with
/// destroys of `operand`'s value, because the instruction consumes it or it has no ownership.
private func canMoveDestroys(to operand: Operand) -> Bool {
  operand.endsLifetime || operand.value.ownership == .none
}

/// Replaces the destroys of each noncopyable value in the chain of conversions and re-abstraction
/// thunks from `closure` to `value` with destroys of the value it forwards, and deletes it, so that
/// `closure` can be deleted.
///
/// FIXME: This works around `tryDeleteDeadClosure`, which can't look through a `convert_function`
/// or delete a thunk that captures a noncopyable closure. Teach it to, and remove this
/// (rdar://192503315).
///
/// FIXME: The steps for conversions repeat `tryRemoveForwardingInstruction` in
/// SimplifyDestroyValue.swift. Share it once it works in any pass and with these instructions.
private func foldDeadForwardingChain(from value: Value, to closure: Closure, _ context: FunctionPassContext) {
  var value = value
  while value != closure, value.type.isMoveOnly,
        let operand = forwardedClosureOperand(of: value),
        canMoveDestroys(to: operand),
        value.uses.ignoreDebugUses.allSatisfy({ $0.instruction is DestroyValueInst })
  {
    let forwardedValue = operand.value
    for destroy in value.uses.users(ofType: DestroyValueInst.self) {
      // A value without ownership is never destroyed.
      if forwardedValue.ownership != .none {
        Builder(before: destroy, context).createDestroyValue(operand: forwardedValue, isDeadEnd: destroy.isDeadEnd)
      }
      context.erase(instruction: destroy)
    }
    if let thunk = value as? PartialApplyInst, thunk.isOnStack {
      context.notifyInvalidatedStackNesting()
    }
    context.erase(instruction: value.definingInstruction!)
    value = forwardedValue
  }
}

private extension Operand {
  var isDeadEndDestroy: Bool {
    (instruction as? DestroyValueInst)?.isDeadEnd ?? false
  }
}

private final class BranchTracingEnumTypeCache {
  let vjp: Function
  private var matches = [Type: Bool]()

  init(vjp: Function) {
    self.vjp = vjp
  }

  func isBranchTracingEnum(_ type: Type) -> Bool {
    if let match = matches[type] { return match }
    let match = type.isBranchTracingEnum(in: vjp)
    matches[type] = match
    return match
  }

  func bteCapture(in partialApply: PartialApplyInst) -> Operand? {
    partialApply.argumentOperands.filter { isBranchTracingEnum($0.value.type) }.singleElement
  }
}

// Screen the VJP before ordinary specialization changes its captures.
private func screenVJPForBTESpecialization(_ vjp: Function) -> BranchTracingEnumTypeCache? {
  guard vjp.blocks.singleElement == nil, let partialApply = getReturnedPullback(of: vjp) else { return nil }
  let branchTracingEnums = BranchTracingEnumTypeCache(vjp: vjp)
  guard branchTracingEnums.bteCapture(in: partialApply) != nil,
        !vjp.instructions.contains(where: {
          ($0 as? BuiltinInst)?.id == .AutoDiffProjectTopLevelSubcontext
        }),
        vjp.blocks.allSatisfy({ block in
          block.arguments.filter { branchTracingEnums.isBranchTracingEnum($0.type) }.count <= 1
        }) else { return nil }
  return branchTracingEnums
}

private typealias PayloadField = (index: Int, value: Value)

private func rebuildExtract(_ extract: TupleExtractInst, _ context: FunctionPassContext) -> TupleExtractInst {
  let builder = Builder(before: extract, context)
  let replacement = builder.createTupleExtract(tuple: extract.tuple, elementIndex: extract.fieldIndex)
  extract.replace(with: replacement, context)
  return replacement
}

private func rebuildDestructure(
  _ destructure: DestructureTupleInst, _ context: FunctionPassContext
) -> DestructureTupleInst {
  let builder = Builder(before: destructure, context)
  let replacement = builder.createDestructureTuple(tuple: destructure.tuple)
  destructure.replace(with: replacement, context)
  return replacement
}

private func matchIncomingPayload(from operand: Operand) -> (tupleUse: Operand, fieldIndex: Int?)? {
  let value = operand.value
  guard value.uses.singleUse == operand else { return nil }

  if let extract = value as? TupleExtractInst {
    guard extract.tuple.type.isTrivial(in: extract.parentFunction) else { return nil }
    return (extract.operand, extract.fieldIndex)
  }
  if let element = value as? MultipleValueInstructionResult,
     let destructure = element.definingInstruction as? DestructureTupleInst {
    guard destructure.results.allSatisfy({ $0 == element || $0.uses.isEmpty }) else { return nil }
    return (destructure.operand, element.index)
  }
  return (operand, nil)
}

// Phi simplification can move a payload projection to the incoming edges.
// The field index is the same whether the block receives the tuple or one field.
//
// A branch forwards the payload explicitly, while a switch_enum passes it
// implicitly. In the examples below, %argB and %argD receive such payloads.
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
private struct PullbackPayloadArgument {
  let argument: Argument
  let projectedFieldIndex: Int?
  let incomingCases: [EnumCase]
  let forwardedEnumData: [UncheckedEnumDataInst]
  var closureInfos = [ClosureInBTE]()

  init?(_ argument: Argument, vjp: Function) {
    guard !argument.isReborrow else { return nil }
    var incomingCases = [EnumCase]()
    var forwardedEnumData = [UncheckedEnumDataInst]()
    var projectedFieldIndex: Int? = nil
    for predecessor in argument.parentBlock.predecessors {
      let enumValue: Value
      let caseIndex: Int
      var incomingIndex: Int? = nil
      switch predecessor.terminator {
      case let branch as BranchInst:
        let operand = branch.operands[argument.index]
        guard let incoming = matchIncomingPayload(from: operand),
              let enumData = incoming.tupleUse.value as? UncheckedEnumDataInst,
              enumData.uses.singleUse == incoming.tupleUse
        else { return nil }
        incomingIndex = incoming.fieldIndex
        forwardedEnumData.append(enumData)
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
            incomingCases.isEmpty || projectedFieldIndex == incomingIndex else { return nil }
      let enumType = remapBranchTracingEnumType(enumValue.type, into: vjp)
      guard let enumCase = enumType.getEnumCases(in: vjp)?[caseIndex],
            enumCase.payload?.isTuple == true else { return nil }
      incomingCases.append(enumCase)
      projectedFieldIndex = incomingIndex
    }
    guard !incomingCases.isEmpty else { return nil }
    self.argument = argument
    self.projectedFieldIndex = projectedFieldIndex
    self.incomingCases = incomingCases
    self.forwardedEnumData = forwardedEnumData
  }
}

private extension PullbackPayloadArgument {
  enum FieldKind {
    case unchanged
    case predecessor
    case storedClosure(ClosureInBTE)
  }

  func kind(ofField index: Int, in vjp: Function) -> FieldKind {
    if let info = closureInfos.first(where: { $0.indexInPayload == index }) {
      return .storedClosure(info)
    }
    if index == 0 && incomingCases[0].payload!.tupleElements[0].isBranchTracingEnum(in: vjp) {
      return .predecessor
    }
    return .unchanged
  }

  func projectedFields() -> [PayloadField]? {
    if let projectedFieldIndex {
      return [(projectedFieldIndex, argument)]
    }
    if let destructure = argument.uses.singleUse?.instruction as? DestructureTupleInst {
      return destructure.results.enumerated().map { ($0.offset, $0.element) }
    }
    var fields = [PayloadField]()
    var indices = Set<Int>()
    for use in argument.uses {
      guard let extract = use.instruction as? TupleExtractInst,
            indices.insert(extract.fieldIndex).inserted else { return nil }
      fields.append((extract.fieldIndex, extract))
    }
    return fields
  }

  func rebuildProjections(
    of clonedArgument: Argument, using cloner: inout SIL.Cloner<FunctionPassContext>
  ) -> [PayloadField] {
    let context = cloner.context
    if let projectedFieldIndex {
      // The predecessor rewrite has already retyped the cloned tuple operands.
      for incoming in Phi(argument)!.incomingValues {
        let cloned = cloner.getClonedValue(of: incoming)
        if incoming is TupleExtractInst {
          _ = rebuildExtract(cloned as! TupleExtractInst, context)
        } else {
          _ = rebuildDestructure(cloned.definingInstruction as! DestructureTupleInst, context)
        }
      }
      return [(projectedFieldIndex, clonedArgument)]
    }

    // An empty destructure has no result to look up in the cloner.
    if argument.uses.singleUse?.instruction is DestructureTupleInst {
      let destructure = clonedArgument.uses.singleUse!.instruction as! DestructureTupleInst
      return rebuildDestructure(destructure, context).results.enumerated().map { ($0.offset, $0.element) }
    }
    return Array(clonedArgument.uses).map { use in
      let extract = use.instruction as! TupleExtractInst
      return (extract.fieldIndex, rebuildExtract(extract, context))
    }
  }
}

// Capture cleanup is emitted at a single call site. Discard-only paths or
// repeated calls would need separate cleanup or per-call copies.
private func hasSingleClosureApply(_ value: Value) -> Bool {
  guard let use = value.uses.ignore(usersOfType: DestroyValueInst.self).singleUse,
        let apply = use.instruction as? ApplyInst,
        use == apply.operands[ApplyOperandConventions.calleeIndex] else { return false }
  return value.ownership != .owned || value.uses.users(ofType: DestroyValueInst.self).allSatisfy { destroy in
    destroy.parentBlock == apply.parentBlock && apply.strictlyDominatesInBlock(destroy)
  }
}

private func canRetypePredecessor(_ value: Value, forwardedEnumData: Set<UncheckedEnumDataInst>) -> Bool {
  guard value.uses.isEmpty || value.uses.isSingleUse else { return false }
  switch value.uses.singleUse?.instruction {
  case nil, is DestroyValueInst, is SwitchEnumInst:
    return true
  case let data as UncheckedEnumDataInst:
    return data.uses.isEmpty || forwardedEnumData.contains(data)
  default:
    return false
  }
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
) -> SpecializedBTEMap {
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

  let optionalWrappers = Set(closuresInBTE.compactMap(\.optionalWrapper))
  for wrapper in optionalWrappers where wrapper.uses.ignoreDebugUses.isEmpty {
    context.erase(instruction: wrapper)
  }

  for closureInfo in closuresInBTE {
    for store in closureInfo.indirectCaptureStores.values {
      allocStacks.insert(store.destination as! AllocStackInst)
    }

    closuresSet.insert(closureInfo.closure)
    closuresSet.formUnion(closureInfo.adapters)
  }

  var oldSetSize = 0
  repeat {
    oldSetSize = closuresSet.count
    let closures = Array(closuresSet)
    for closure in closures {
      if closure.uses.ignoreDebugUses.isEmpty {
        context.erase(instruction: closure)
        closuresSet.remove(closure)
      }
    }
  } while oldSetSize != closuresSet.count

  for allocStack in allocStacks {
    assert(allocStack.uses.dropFirst().isSingleUse)
    for use in allocStack.uses {
      assert(use.instruction is DeallocStackInst || use.instruction is StoreInst)
      context.erase(instruction: use.instruction)
    }
    context.erase(instruction: allocStack)
  }

  return closuresSet
}

private func specializeAgainstBTE(_ plan: BTESpecializationPlan, context: FunctionPassContext) {
  let closuresInBTE = plan.closuresInBTE
  let totalSupportedClosures = Set(closuresInBTE.map { $0.closure }).count

  let specInfo = PullbackSpecializationAgainstBTE()

  let (specializedFunction, alreadyExists) =
    specInfo.getOrCreatePullbackSpecializedAgainstBTE(
      basedOn: plan,
      context)

  if !alreadyExists {
    context.notifyNewFunction(
      function: specializedFunction, derivedFrom: plan.pullback)
  }

  rewriteVJPForBTEPullbackSpecialization(
    using: specializedFunction,
    plan: plan,
    context: context)

  let remainingClosures = eraseDeadSpecializedClosures(
    from: closuresInBTE, context: context)

  logSpecializationStats(
    closuresSet: remainingClosures,
    totalSupportedClosures: totalSupportedClosures,
    function: plan.vjp)
}

private func specializeVJPEnumConstructors(
  in vjp: Function, enumDict: SpecializedBTEMap, context: FunctionPassContext
) -> [TupleInst: Type] {
  let casePayloadTypes = enumDict.mapValues { type in
    type.getEnumCases(in: vjp)!.map { $0.payload! }
  }
  var payloadTypes = [TupleInst: Type]()
  for case let constructor as EnumInst in vjp.instructions {
    guard let newType = enumDict[constructor.type] else { continue }
    let payload = constructor.payload!
    let payloadType = casePayloadTypes[constructor.type]![constructor.caseIndex]
    if payload.type.isVoid {
      assert(payload.type == payloadType)
    } else {
      let tuple = payload as! TupleInst
      let previousType = payloadTypes.updateValue(payloadType, forKey: tuple)
      assert(previousType == nil || previousType == payloadType, "shared payloads must agree on their layout")
    }

    // The original payload determines the constructor's forwarding ownership,
    // even if specialization makes this case's payload trivial.
    let builder = Builder(before: constructor, context)
    let replacement = builder.createEnum(caseIndex: constructor.caseIndex, payload: payload, enumType: newType)
    constructor.replace(with: replacement, context)
  }
  return payloadTypes
}

private func specializeBTEBlockArgsInVJP(
  vjp: Function, enumDict: SpecializedBTEMap, context: FunctionPassContext
) {
  for bb in vjp.blocks {
    guard let arg = bb.getBranchTracingEnumArg(vjp: vjp) else {
      continue
    }
    guard let newArg = specializeBranchTracingEnumBBArgInVJP(
      arg: arg, specializedBTEDict: enumDict, context: context) else { continue }
    arg.uses.replaceAll(with: newArg, context)
    bb.eraseArgument(at: arg.index, context)
  }
}

private func buildCaptureField(
  _ closureInfo: ClosureInBTE, type: Type, in vjp: Function, builder: Builder
) -> Value {
  guard let optional = closureInfo.optionalWrapper else {
    return builder.createTuple(type: type, elements: closureInfo.capturedArgs)
  }
  if optional.isOptionalNone {
    return builder.createOptionalNone(type: type)
  }
  let capturesType = type.getEnumCases(in: vjp)![Builder.optionalSomeCaseIndex]!.payload!
  let captures = builder.createTuple(type: capturesType, elements: closureInfo.capturedArgs)
  return builder.createOptionalSome(operand: captures, type: type)
}

private func rewritePayloadTuplesInVJP(
  vjp: Function, payloadTypes: [TupleInst: Type],
  closureInfos: [ClosureInBTE], context: FunctionPassContext
) {
  let closuresByPayload = Dictionary(grouping: closureInfos, by: \.payloadTuple)
  for (payload, type) in payloadTypes where payload.type != type {
    let closures = Dictionary(
      (closuresByPayload[payload] ?? []).map { ($0.indexInPayload, $0) },
      uniquingKeysWith: { _, last in last })
    let fields = type.tupleElements
    let builder = Builder(before: payload, context)
    let elements = payload.operands.enumerated().map { index, operand in
      guard let closureInfo = closures[index] else { return operand.value }
      assert(operand.value == closureInfo.valueInPayload)
      return buildCaptureField(closureInfo, type: fields[index], in: vjp, builder: builder)
    }
    assert(elements.elementsEqual(fields, by: { $0.type == $1 }))
    payload.replace(with: builder.createTuple(type: type, elements: elements), context)
  }
}

private func rewriteVJPForBTEPullbackSpecialization(
  using specializedCallee: Function, plan: BTESpecializationPlan,
  context: FunctionPassContext
) {
  let vjp = plan.vjp
  let enumDict = plan.specializedBTEDict

  // Retype predecessor values before rebuilding the tuples that store them.
  let payloadTypes = specializeVJPEnumConstructors(in: vjp, enumDict: enumDict, context: context)

  specializeBTEBlockArgsInVJP(vjp: vjp, enumDict: enumDict, context: context)

  let pai = plan.paiOfPullback
  pai.replace(withCallTo: specializedCallee, arguments: Array(pai.arguments), context)

  rewritePayloadTuplesInVJP(
    vjp: vjp, payloadTypes: payloadTypes,
    closureInfos: plan.closuresInBTE, context: context)
}

private func getParametersForPullbackSpecializedAgainstBTE(
  _ plan: BTESpecializationPlan
) -> [ParameterInfo] {
  var parameters = Array(plan.pullback.convention.parameters)
  let index = plan.pullback.argumentConventions.parameterIndex(ofArgumentIndex: plan.bteArgument.index)!
  let parameter = parameters[index]
  let newType = plan.specializedBTEType.rawType.mapOutOfEnvironment().canonical
  parameters[index] = ParameterInfo(
    type: newType, convention: parameter.convention,
    options: parameter.options, hasLoweredAddresses: parameter.hasLoweredAddresses)
  return parameters
}

private func insertLifetimeEndIfNeeded(
  for value: Value, before insertionPoint: Instruction, _ context: FunctionPassContext
) {
  guard !value.type.isTrivial(in: value.parentFunction),
        value.uses.endingLifetime.isEmpty else { return }
  let builder = Builder(before: insertionPoint, context)
  assert(value.parentFunction.hasOwnership)
  builder.createDestroyValue(operand: value)
}

private func extractTupleElements(
  from tuple: Value, builder: Builder
) -> [Value] {
  if tuple.ownership == .owned {
    return Array(builder.createDestructureTuple(tuple: tuple).results)
  }
  return tuple.type.tupleElements.indices.map { index in
    builder.createTupleExtract(tuple: tuple, elementIndex: index)
  }
}

// The conversion type comes from the VJP's generic environment.
private func recreateFunctionConversion(
  _ conversion: ConvertFunctionInst, of closure: Value, builder: Builder
) -> ConvertFunctionInst {
  let function = closure.parentFunction
  let type = function.mapTypeIntoEnvironment(conversion.type.rawType.mapOutOfEnvironment())
    .canonical.loweredType(in: function)
  return builder.createConvertFunction(
    originalFunction: closure, resultType: type, kind: conversion.conversionKind)
}

private func materializeIndirectCaptures(
  _ captures: [Value], for partialApply: PartialApplyInst?, builder: Builder
) -> (arguments: [Value], storage: [AllocStackInst]) {
  guard let partialApply else { return (captures, []) }

  var arguments = captures
  var storage = [AllocStackInst]()
  for (index, operand) in partialApply.argumentOperands.enumerated() where operand.value.type.isAddress {
    let capture = captures[index]
    let allocation = builder.createAllocStack(capture.type)
    builder.createStore(source: capture, destination: allocation, ownership: .initialize)
    arguments[index] = allocation
    storage.append(allocation)
  }
  return (arguments, storage)
}

private func rebuildPartialApply(
  _ original: PartialApplyInst, callee: FunctionRefInst, captures: [Value], builder: Builder
) -> PartialApplyInst {
  builder.createPartialApply(
    function: callee,
    substitutionMap: original.substitutionMap.substitute(with: callee.parentFunction.forwardingSubstitutionMap),
    capturedArguments: captures,
    calleeConvention: original.calleeConvention,
    hasUnknownResultIsolation: original.hasUnknownResultIsolation,
    isOnStack: original.isOnStack,
    isNested: original.isNested,
    executionSemantics: original.executionSemantics)
}

private func createReplacementApply(
  basedOn apply: ApplyInst, callee: Value, substitutions: SubstitutionMap,
  appending captures: [Value] = [], builder: Builder
) -> ApplyInst {
  builder.createApply(
    function: callee, substitutions, arguments: Array(apply.arguments) + captures,
    isNonThrowing: apply.isNonThrowing, isNonAsync: apply.isNonAsync,
    specializationInfo: apply.specializationInfo, argumentLocationsFrom: apply)
}

private func emitDirectClosureApply(
  _ apply: ApplyInst, root: SingleValueInstruction, captures: [Value], builder: Builder
) -> ApplyInst {
  let partialApply = root as? PartialApplyInst
  let (arguments, storage) = materializeIndirectCaptures(captures, for: partialApply, builder: builder)
  let callee = builder.createFunctionRef(root.referencedClosureFunction!)
  let substitutions = partialApply?.substitutionMap ?? apply.substitutionMap
  let replacement = createReplacementApply(
    basedOn: apply, callee: callee,
    substitutions: substitutions.substitute(with: apply.parentFunction.forwardingSubstitutionMap),
    appending: arguments, builder: builder)

  // The guaranteed-indirect call borrows the slots' contents.
  for allocation in storage.reversed() {
    if !allocation.type.objectType.isTrivial(in: apply.parentFunction) {
      builder.createDestroyAddr(address: allocation)
    }
    builder.createDeallocStack(allocation)
  }
  return replacement
}

private func rebuildRootClosure(
  _ root: SingleValueInstruction, captures: [Value], builder: Builder
) -> SingleValueInstruction {
  let callee = builder.createFunctionRef(root.referencedClosureFunction!)
  guard let partialApply = root as? PartialApplyInst else {
    assert(root is ThinToThickFunctionInst)
    let function = callee.parentFunction
    let type = function.mapTypeIntoEnvironment(root.type.rawType.mapOutOfEnvironment())
      .canonical.loweredType(in: function)
    return builder.createThinToThickFunction(thinFunction: callee, resultType: type)
  }

  let (arguments, storage) = materializeIndirectCaptures(captures, for: partialApply, builder: builder)
  let rebuilt = rebuildPartialApply(partialApply, callee: callee, captures: arguments, builder: builder)
  // The partial_apply takes the captured values, leaving the slots empty.
  for allocation in storage.reversed() {
    builder.createDeallocStack(allocation)
  }
  return rebuilt
}

private func emitAdaptedClosureApply(
  _ apply: ApplyInst, closureInfo: ClosureInBTE, captures: [Value], builder: Builder
) -> (call: ApplyInst, rebuiltClosures: [SingleValueInstruction]) {
  let adapters = closureInfo.adapters
  let outerThunk = adapters.last as? PartialApplyInst
  let adaptersToReplay = outerThunk == nil ? adapters[...] : adapters.dropLast()
  var rebuiltClosures = [rebuildRootClosure(closureInfo.closure, captures: captures, builder: builder)]
  for adapter in adaptersToReplay {
    let capturedClosure = rebuiltClosures.last!
    if let conversion = adapter as? ConvertFunctionInst {
      rebuiltClosures.append(recreateFunctionConversion(conversion, of: capturedClosure, builder: builder))
    } else {
      let thunk = adapter as! PartialApplyInst
      let callee = builder.createFunctionRef(thunk.referencedFunction!)
      rebuiltClosures.append(rebuildPartialApply(
        thunk, callee: callee, captures: [capturedClosure], builder: builder))
    }
  }

  let rebuilt = rebuiltClosures.last!
  if let outerThunk {
    let callee = builder.createFunctionRef(outerThunk.referencedFunction!)
    let replacement = createReplacementApply(
      basedOn: apply, callee: callee,
      substitutions: outerThunk.substitutionMap.substitute(with: apply.parentFunction.forwardingSubstitutionMap),
      appending: [rebuilt], builder: builder)
    return (replacement, rebuiltClosures)
  }

  // SILCombine handles the ABI casts when it folds the conversion.
  let replacement = createReplacementApply(
    basedOn: apply, callee: rebuilt, substitutions: apply.substitutionMap, builder: builder)
  return (replacement, rebuiltClosures)
}

private func rewriteStoredClosureApply(
  _ apply: ApplyInst, closureInfo: ClosureInBTE, captures: Value, _ context: FunctionPassContext
) {
  let builder = Builder(before: apply, context)
  let captureValues = extractTupleElements(from: captures, builder: builder)
  let replacement: ApplyInst
  let rebuiltClosures: [SingleValueInstruction]
  if closureInfo.adapters.isEmpty {
    replacement = emitDirectClosureApply(apply, root: closureInfo.closure, captures: captureValues, builder: builder)
    rebuiltClosures = []
  } else {
    (replacement, rebuiltClosures) = emitAdaptedClosureApply(
      apply, closureInfo: closureInfo, captures: captureValues, builder: builder)
  }
  apply.replace(with: replacement, context)

  let terminator = replacement.parentBlock.terminator
  for closure in rebuiltClosures.reversed() {
    insertLifetimeEndIfNeeded(for: closure, before: terminator, context)
  }
  for capture in captureValues {
    insertLifetimeEndIfNeeded(for: capture, before: terminator, context)
  }
}

private func rewriteStoredClosureUses(
  _ captures: Value, closureInfo: ClosureInBTE, _ context: FunctionPassContext
) {
  for use in Array(captures.uses) {
    if let apply = use.instruction as? ApplyInst {
      rewriteStoredClosureApply(apply, closureInfo: closureInfo, captures: captures, context)
    } else {
      assert(use.instruction is DestroyValueInst)
      context.erase(instruction: use.instruction)
    }
  }
}

private func rewritePredecessorUse(_ value: Value, _ context: FunctionPassContext) {
  switch value.uses.singleUse?.instruction {
  case let data as UncheckedEnumDataInst:
    let builder = Builder(before: data, context)
    let payloadType = value.type.getEnumCases(in: data.parentFunction)![data.caseIndex]!.payload!
    data.replace(with: builder.createUncheckedEnumData(
      enum: value, caseIndex: data.caseIndex, resultType: payloadType), context)
  case let dispatch as SwitchEnumInst:
    let builder = Builder(before: dispatch, context)
    builder.createSwitchEnum(enum: value, cases: Array(dispatch.cases))
    context.erase(instruction: dispatch)
  case let destroy as DestroyValueInst:
    if value.type.isTrivial(in: destroy.parentFunction) {
      context.erase(instruction: destroy)
    }
  default:
    assert(value.uses.isEmpty)
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
// With no direct primal results, the VJP returns the pullback itself.
private func getReturnedPullback(of vjp: Function, requireSingleNonDebugUse: Bool = false) -> PartialApplyInst? {
  guard let returnInst = vjp.returnInstruction as? ReturnInst else { return nil }
  var value = returnInst.returnedValue
  if let tuple = value as? TupleInst {
    guard tuple.operands.count >= 2 else { return nil }
    value = tuple.operands.last!.value
  }

  while let conversion = value as? ConvertFunctionInst {
    guard conversion.conversionKind == .ordinary,
          !requireSingleNonDebugUse || conversion.uses.ignoreDebugUses.isSingleUse else { return nil }
    value = conversion.fromFunction
  }
  guard let pullback = value as? PartialApplyInst,
        !requireSingleNonDebugUse || pullback.uses.ignoreDebugUses.isSingleUse else { return nil }
  return pullback
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
  _ branchTracingEnums: BranchTracingEnumTypeCache,
  closuresInBTE: [ClosureInBTE], bteTypes: Set<Type>
) -> [ClosureInBTE]? {
  let vjp = branchTracingEnums.vjp
  let branchTracingEnumInstructions: [EnumInst] = vjp.instructions.filter { $0 is EnumInst }.map {
    $0 as! EnumInst
  }.filter {
    branchTracingEnums.isBranchTracingEnum($0.type)
      && bteTypes.contains(remapBranchTracingEnumType($0.type, into: vjp))
  }

  var closuresInBTEForOptionalNone = [ClosureInBTE]()

  for closureInBTE in closuresInBTE {
    guard let some = closureInBTE.optionalWrapper else { continue }
    assert(some.isOptionalSome)
    // In terms of the example above, we've found %closure and %optionalWrapper1.

    let enumCase = closureInBTE.enumCase
    // In terms of the example above, enumCase.name is now equal to "bbK".

    let bteWithNoneArr = branchTracingEnumInstructions.filter {
      $0.caseName == enumCase.name && $0.type != enumCase.enumType(in: vjp)
    }
    guard let first = bteWithNoneArr.first else { continue }
    let noneType = remapBranchTracingEnumType(first.type, into: vjp)
    // CFG duplication can produce several constructors of one counterpart case.
    guard bteWithNoneArr.allSatisfy({
      remapBranchTracingEnumType($0.type, into: vjp) == noneType
        && $0.caseIndex == first.caseIndex
    }) else { return nil }

    for bteWithNone in bteWithNoneArr {
      guard let payloadTuple = bteWithNone.payload as? TupleInst,
            let operand = payloadTuple.operands.last,
            let optionalNone = operand.value as? EnumInst,
            optionalNone.type == some.type, optionalNone.isOptionalNone
      else { return nil }

      let info = ClosureInBTE(
        closure: closureInBTE.closure,
        adapters: closureInBTE.adapters,
        indirectCaptureStores: closureInBTE.indirectCaptureStores,
        optionalWrapper: optionalNone,
        useInPayload: operand,
        enumCase: bteWithNone.type.getEnumCases(in: vjp)![bteWithNone.caseIndex]!
      )
      if !closuresInBTEForOptionalNone.contains(info) {
        closuresInBTEForOptionalNone.append(info)
      }
    }
  }

  return closuresInBTEForOptionalNone
}

// The function test queries whole-tuple payload arguments.
private func getBTEPayloadArgOfPbBBInfo(_ bb: BasicBlock, vjp: Function)
  -> (arg: Argument, enumCase: EnumCase)?
{
  for argument in bb.arguments {
    if let payloadArgument = PullbackPayloadArgument(argument, vjp: vjp), payloadArgument.projectedFieldIndex == nil {
      return (argument, payloadArgument.incomingCases[0])
    }
  }
  return nil
}

extension ClosureInBTE {
  var capturedArgs : [Value] {
    guard let partialApply = closure as? PartialApplyInst else { return [] }
    return partialApply.arguments.enumerated().map { index, argument in
      indirectCaptureStores[index]?.source ?? argument
    }
  }
}

private extension SingleValueInstruction {
  var indirectCaptureStoresForBTESpecialization: [Int: StoreInst]? {
    switch self {
    case let closure as ThinToThickFunctionInst where closure.referencedFunction != nil:
      return [:]
    case let partialApply as PartialApplyInst where partialApply.referencedFunction != nil:
      // The pullback cannot reconstruct the VJP's opened type metadata and witnesses.
      guard !partialApply.substitutionMap.replacementTypes.contains(where: { $0.hasExistentialArchetype }) else {
        return nil
      }
      // Moving an indirect capture into the enum requires a dedicated stack slot.
      var stores = [Int: StoreInst]()
      for (index, operand) in partialApply.argumentOperands.enumerated() {
        // A mutable capture's SIL box cannot be stored in an AST enum payload.
        guard !operand.value.type.isBox else { return nil }
        guard operand.value.type.isAddress else { continue }
        guard partialApply.convention(of: operand) == .indirectInGuaranteed,
              let allocation = operand.value as? AllocStackInst,
              allocation.uses.dropFirst(3).isEmpty,
              let store = allocation.uses.singleUser(ofType: StoreInst.self),
              case .initialize = store.storeOwnership,
              allocation.uses.singleUser(ofType: PartialApplyInst.self) == partialApply,
              allocation.uses.singleUser(ofType: DeallocStackInst.self) != nil else { return nil }
        stores[index] = store
      }
      return stores
    default:
      return nil
    }
  }

  var referencedClosureFunction: Function? {
    (self as? PartialApplyInst)?.referencedFunction ?? (self as? ThinToThickFunctionInst)?.referencedFunction
  }
}

private extension Instruction {
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
// "wrapping" it in function conversions, reabstraction or subset thunks, or an optional.
// Each wrapper must be the previous value's sole non-debug use.
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
private func findBTEUses(
  for rootClosure: SingleValueInstruction, indirectCaptureStores: [Int: StoreInst],
  branchTracingEnums: BranchTracingEnumTypeCache
) -> [ClosureInBTE]? {
  log("findBTEUses: running for \(rootClosure)")
  let vjp = rootClosure.parentFunction
  var closuresInBTE = [ClosureInBTE]()

  var adapters = [SingleValueInstruction]()
  var currentClosure = rootClosure
  while let singleUse = currentClosure.uses.ignoreDebugUses.singleUse {
    if let conversion = singleUse.instruction as? ConvertFunctionInst {
      // Scoped escaping and discarding conversions have separate lifetime rules.
      guard conversion.conversionKind == .ordinary else { return [] }
      currentClosure = conversion
      adapters.append(conversion)
      continue
    }
    guard let pai = singleUse.instruction as? PartialApplyInst else {
      break
    }

    // An ordinary closure remains a root even when its only capture is a closure.
    guard pai.callee is FunctionRefInst else { return nil }
    guard pai.isPartialApplyOfThunk else { break }
    guard pai.indirectCaptureStoresForBTESpecialization != nil else { return [] }

    currentClosure = pai
    adapters.append(pai)
  }

  // A nonthrowing outer thunk can adapt a root closure with throws(Never).
  guard FunctionConvention(for: currentClosure.type.rawType.canonical, in: vjp).errorResult == nil
  else { return [] }

  let optionalWrapper = currentClosure.uses.ignoreDebugUses.singleUse?.instruction.asOptionalWrapper
  let closure = optionalWrapper ?? currentClosure

  for use in closure.uses.ignoreDebugUses {
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
      guard branchTracingEnums.isBranchTracingEnum(ei.type) else {
        log("findBTEUses: enum type \(ei.type) is not a " +
            "branch tracing enum in VJP \(vjp.name), aborting")
        return []
      }
      let enumCase = ei.type.getEnumCases(in: vjp)![ei.caseIndex]!
      let closureInBTE = ClosureInBTE(
        closure: rootClosure,
        adapters: adapters,
        indirectCaptureStores: indirectCaptureStores,
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

private func findClosuresInBTE(
  paiOfPullback: PartialApplyInst, bteTypes: Set<Type>, branchTracingEnums: BranchTracingEnumTypeCache
) -> [ClosureInBTE]? {
  let vjp = paiOfPullback.parentFunction
  let canSpecializeOptionals = paiOfPullback.referencedFunction!.blocks.singleElement != nil
  var adapters = Set<SingleValueInstruction>()
  var closuresInBTE = [ClosureInBTE]()
  for case let rootClosure as SingleValueInstruction in vjp.instructions {
    guard rootClosure != paiOfPullback,
          !adapters.contains(rootClosure),
          let indirectCaptureStores = rootClosure.indirectCaptureStoresForBTESpecialization
    else {
      continue
    }

    guard let currentClosuresInBTE = findBTEUses(
      for: rootClosure, indirectCaptureStores: indirectCaptureStores, branchTracingEnums: branchTracingEnums)
    else { return nil }
    adapters.formUnion(currentClosuresInBTE.flatMap(\.adapters))
    // A live optional requires rewriting its switch successors.
    if !canSpecializeOptionals && currentClosuresInBTE.contains(where: { $0.optionalWrapper != nil }) {
      continue
    }
    // The closure keeps its captures while any payload use survives.
    if currentClosuresInBTE.allSatisfy({ bteTypes.contains($0.enumCase.enumType(in: vjp)) }) {
      closuresInBTE.append(contentsOf: currentClosuresInBTE)
    }
  }

  guard let optionalNoneClosures = findOptionalNoneMatchingOptionalSome(
    branchTracingEnums, closuresInBTE: closuresInBTE, bteTypes: bteTypes) else { return nil }
  closuresInBTE.append(contentsOf: optionalNoneClosures)

  return closuresInBTE
}

private typealias SpecializedBTEMap = [Type: Type]

private extension EnumCase {
  func specialized(
    using replacements: SpecializedBTEMap, from vjp: Function, in function: Function
  ) -> EnumCase? {
    guard let type = replacements[enumType(in: vjp)] else { return nil }
    let specializedType = remapBranchTracingEnumType(type, into: function)
    return specializedType.getEnumCases(in: function)![index]
  }
}

private func validateVJPTapeScope(
  _ branchTracingEnums: BranchTracingEnumTypeCache, bteTypes: Set<Type>, payloads: Set<TupleInst>,
  paiOfPullback: PartialApplyInst
) -> Bool {
  let vjp = branchTracingEnums.vjp
  func isSelected(_ value: Value) -> Bool {
    branchTracingEnums.isBranchTracingEnum(value.type)
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

// Keep different instantiations of the same generic enum case distinct.
private struct BTECaseKey: Hashable {
  let type: Type
  let index: Int
}

private struct PullbackClosureAnalysis {
  let vjp: Function
  let paiOfPullback: PartialApplyInst
  let pullback: Function
  let bteArgument: FunctionArgument?
  let bteType: Type?
  let bteSpecializationQueue: [Type]
  let closuresInBTE: [ClosureInBTE]
  // Schema-only queries can proceed without validated payload arguments.
  let payloadArguments: [PullbackPayloadArgument]?

  init?(_ branchTracingEnums: BranchTracingEnumTypeCache) {
    let vjp = branchTracingEnums.vjp
    guard let paiOfPullback = getReturnedPullback(of: vjp),
          let pullback = paiOfPullback.referencedFunction,
          pullback.isDefinition, pullback.hasOwnership else { return nil }

    // Specialization can introduce references unavailable to serialized bodies.
    guard !vjp.isAnySerialized, !pullback.isAnySerialized else { return nil }

    // Capture reconstruction consumes nontrivial tapes.
    let bteArguments = pullback.entryBlock.arguments.filter { branchTracingEnums.isBranchTracingEnum($0.type) }
    guard bteArguments.count <= 1, bteArguments.allSatisfy({
      $0.type.isTrivial(in: pullback) || $0.ownership == .owned
    }) else { return nil }
    self.vjp = vjp
    self.pullback = pullback
    self.bteArgument = bteArguments.first as? FunctionArgument
    self.bteType = bteArgument.map { remapBranchTracingEnumType($0.type, into: vjp) }

    // A nested pullback can capture a different instantiation of the same tape.
    if let bteType {
      guard let queue = getBranchTracingEnumSpecializationQueue(topBTEType: bteType, in: vjp) else { return nil }
      self.bteSpecializationQueue = queue
    } else {
      self.bteSpecializationQueue = []
    }
    let bteTypes = Set(bteSpecializationQueue)

    var payloadsByCase = [BTECaseKey: Set<TupleInst>]()
    var payloads = Set<TupleInst>()
    for case let constructor as EnumInst in vjp.instructions {
      guard branchTracingEnums.isBranchTracingEnum(constructor.type) else { continue }
      guard let payload = constructor.payload else { return nil }
      // An empty tuple can be forwarded through a block argument after
      // inlining. It has no fields to inspect or retype.
      if payload.type.isVoid { continue }
      guard let tuple = payload as? TupleInst else { return nil }
      let enumType = remapBranchTracingEnumType(constructor.type, into: vjp)
      guard bteTypes.contains(enumType) else { continue }
      payloadsByCase[BTECaseKey(type: enumType, index: constructor.caseIndex), default: []].insert(tuple)
      payloads.insert(tuple)
    }
    // Changing a shared tuple retypes all of its enum consumers.
    guard payloads.allSatisfy({ payload in
      payload.uses.allSatisfy { use in
        guard let enumInst = use.instruction as? EnumInst else { return false }
        return branchTracingEnums.isBranchTracingEnum(enumInst.type)
          && bteTypes.contains(remapBranchTracingEnumType(enumInst.type, into: vjp))
      }
    }) else { return nil }
    guard validateVJPTapeScope(
      branchTracingEnums, bteTypes: bteTypes, payloads: payloads, paiOfPullback: paiOfPullback)
    else { return nil }

    self.paiOfPullback = paiOfPullback
    guard let closuresInBTE = findClosuresInBTE(
      paiOfPullback: paiOfPullback, bteTypes: bteTypes, branchTracingEnums: branchTracingEnums)
    else { return nil }
    guard !closuresInBTE.isEmpty else {
      self.closuresInBTE = []
      self.payloadArguments = []
      return
    }
    // Defer rejecting roots so later merges still see all locally agreed fields.
    var rejectedRoots = Set<SingleValueInstruction>()

    let closuresByPayload = Dictionary(grouping: closuresInBTE, by: \.payloadTuple).mapValues { closures in
      let payload = closures[0].payloadTuple
      guard !payload.uses.isSingleUse else { return closures }
      let byCase = Dictionary(grouping: closures) { info in
        BTECaseKey(type: info.enumCase.enumType(in: vjp), index: info.enumCase.index)
      }
      let incoming = payload.uses.map { use in
        let constructor = use.instruction as! EnumInst
        let type = remapBranchTracingEnumType(constructor.type, into: vjp)
        return byCase[BTECaseKey(type: type, index: constructor.caseIndex)] ?? []
      }
      return mergeClosureFields(incoming, rejectingRootsInto: &rejectedRoots)
    }

    let closuresByCase = payloadsByCase.mapValues { payloads in
      mergeClosureFields(payloads.map { closuresByPayload[$0] ?? [] }, rejectingRootsInto: &rejectedRoots)
    }

    let payloadArguments = analyzePullbackPayloadArguments(
      in: pullback, vjp: vjp,
      bteTypes: bteTypes, closuresByCase: closuresByCase, rejectedRoots: &rejectedRoots)

    self.closuresInBTE = closuresInBTE.filter { !rejectedRoots.contains($0.closure) }
    self.payloadArguments = payloadArguments?.map { payloadArgument in
      var payloadArgument = payloadArgument
      payloadArgument.closureInfos.removeAll { rejectedRoots.contains($0.closure) }
      return payloadArgument
    }
  }
}

private struct BTESpecializationPlan {
  let closureAnalysis: PullbackClosureAnalysis
  let bteArgument: FunctionArgument
  let specializedBTEType: Type
  let bteSwitch: SwitchEnumInst?
  let blockOrder: [BasicBlock]
  let payloadArguments: [PullbackPayloadArgument]
  let specializedBTEDict: SpecializedBTEMap
  let specializedPullbackName: String

  var paiOfPullback: PartialApplyInst { closureAnalysis.paiOfPullback }
  var closuresInBTE: [ClosureInBTE] { closureAnalysis.closuresInBTE }
  var vjp: Function { closureAnalysis.vjp }
  var pullback: Function { closureAnalysis.pullback }

  init?(_ branchTracingEnums: BranchTracingEnumTypeCache, _ context: FunctionPassContext) {
    guard let analysis = PullbackClosureAnalysis(branchTracingEnums),
          !analysis.closuresInBTE.isEmpty,
          let bteArgument = analysis.bteArgument, let bteType = analysis.bteType,
          let capture = branchTracingEnums.bteCapture(in: analysis.paiOfPullback),
          let captureIndex = analysis.paiOfPullback.calleeArgumentIndex(of: capture),
          let payloadArguments = analysis.payloadArguments,
          let blockOrder = predecessorFirstBlockOrder(of: analysis.pullback, context) else { return nil }

    let bteSwitch: SwitchEnumInst?
    if blockOrder.count == 1 {
      guard analysis.pullback.entryBlock.terminator is ReturnInst,
            bteArgument.uses.isEmpty || bteArgument.uses.singleUse?.instruction is DestroyValueInst
      else { return nil }
      bteSwitch = nil
    } else {
      guard let dispatch = bteArgument.uses.singleUse?.instruction as? SwitchEnumInst else { return nil }
      bteSwitch = dispatch
    }

    let specializedTypes = synthesizeSpecializedEnums(from: analysis, context: context)
    let captureType = remapBranchTracingEnumType(capture.value.type, into: analysis.vjp)
    self.specializedPullbackName = context.mangle(
      withSpecializedBranchTracingEnum: specializedTypes[captureType]!, argIdx: captureIndex,
      in: analysis.vjp, from: analysis.pullback)
    self.closureAnalysis = analysis
    self.bteArgument = bteArgument
    self.specializedBTEType = specializedTypes[bteType]!
    self.bteSwitch = bteSwitch
    self.blockOrder = blockOrder
    self.payloadArguments = payloadArguments
    self.specializedBTEDict = specializedTypes
  }
}

private func predecessorFirstBlockOrder(of function: Function, _ context: FunctionPassContext) -> [BasicBlock]? {
  let order = context.withNestedFunctionContext(for: function) { pullbackContext in
    function.postOrderBlocks(pullbackContext).reversed()
  }
  var visited = Set<BasicBlock>()
  for block in order {
    guard block.predecessors.allSatisfy(visited.contains) else { return nil }
    visited.insert(block)
  }
  // The cloner only maps blocks reachable from the entry.
  return function.blocks.allSatisfy(visited.contains) ? order : nil
}

// A field can change only if it has the same stored closure on every incoming path.
// A path may describe the same field more than once.
private func mergeClosureFields(
  _ incoming: [[ClosureInBTE]], rejectingRootsInto rejectedRoots: inout Set<SingleValueInstruction>
) -> [ClosureInBTE] {
  let occurrences = incoming.enumerated().flatMap { path, closures in
    closures.map { (path: path, closureInfo: $0) }
  }
  let byField = Dictionary(grouping: occurrences) { $0.closureInfo.indexInPayload }
  return byField.values.compactMap { field in
    let first = field[0].closureInfo
    guard Set(field.lazy.map(\.path)).count == incoming.count,
          field.allSatisfy({ $0.closureInfo.hasSameReconstruction(as: first) }) else {
      rejectedRoots.formUnion(field.lazy.map { $0.closureInfo.closure })
      return nil
    }
    return first
  }
}

private func analyzePullbackPayloadArguments(
  in pullback: Function, vjp: Function, bteTypes: Set<Type>,
  closuresByCase: [BTECaseKey: [ClosureInBTE]],
  rejectedRoots: inout Set<SingleValueInstruction>
) -> [PullbackPayloadArgument]? {
  let allPayloadArguments = pullback.blocks.flatMap { block in
    block.arguments.compactMap { PullbackPayloadArgument($0, vjp: vjp) }
  }
  var payloadArguments = [PullbackPayloadArgument]()
  for payloadArgument in allPayloadArguments {
    let selectedCases = payloadArgument.incomingCases.filter { bteTypes.contains($0.enumType(in: vjp)) }
    if selectedCases.isEmpty { continue }
    // A phi cannot mix a changed payload with an unchanged nested tape.
    guard selectedCases.count == payloadArgument.incomingCases.count else { return nil }
    payloadArguments.append(payloadArgument)
  }
  let forwardedEnumData = Set(payloadArguments.flatMap { $0.forwardedEnumData })
  var validated = [PullbackPayloadArgument]()
  for var payloadArgument in payloadArguments {
    let incoming = payloadArgument.incomingCases.map { enumCase in
      let closures = closuresByCase[BTECaseKey(type: enumCase.enumType(in: vjp), index: enumCase.index)] ?? []
      guard let index = payloadArgument.projectedFieldIndex else { return closures }
      return closures.filter { $0.indexInPayload == index }
    }
    payloadArgument.closureInfos = mergeClosureFields(incoming, rejectingRootsInto: &rejectedRoots)
    guard let fields = payloadArgument.projectedFields() else { return nil }
    for (index, value) in fields {
      switch payloadArgument.kind(ofField: index, in: vjp) {
      case .unchanged:
        break
      case .predecessor:
        guard canRetypePredecessor(value, forwardedEnumData: forwardedEnumData) else { return nil }
      case .storedClosure(let info):
        if !hasSingleClosureApply(value) {
          rejectedRoots.insert(info.closure)
        }
      }
    }
    validated.append(payloadArgument)
  }
  // A switch supplies a whole payload without an explicit incoming operand.
  // Validate each such successor's payload argument before changing its type.
  for block in pullback.blocks {
    if let dispatch = block.terminator as? SwitchEnumInst,
       dispatch.enumOp.type.isBranchTracingEnum(in: vjp),
       bteTypes.contains(remapBranchTracingEnumType(dispatch.enumOp.type, into: vjp)) {
      guard dispatch.getSuccessorForDefault() == nil, block.successors.allSatisfy({ successor in
        validated.contains { $0.argument.parentBlock == successor && $0.projectedFieldIndex == nil }
      }) else { return nil }
    }
  }
  return validated
}

extension ClosureInBTE {
  var valueInPayload: Value {
    if let optionalWrapper { return optionalWrapper }
    if let adapter = adapters.last { return adapter }
    return closure
  }
}

private struct PullbackSpecializationAgainstBTE {
  typealias Cloner = SIL.Cloner<FunctionPassContext>

  func getOrCreatePullbackSpecializedAgainstBTE(
    basedOn plan: BTESpecializationPlan, _ context: FunctionPassContext
  ) -> (function: Function, alreadyExists: Bool) {
    let specializedPbName = plan.specializedPullbackName
    if let specializedPb = context.lookupFunction(name: specializedPbName) {
      return (specializedPb, true)
    }

    let specializedParameters = getParametersForPullbackSpecializedAgainstBTE(plan)
    let specializedPb =
      context.createSpecializedFunctionDeclaration(
        from: plan.pullback, withName: specializedPbName,
        withParams: specializedParameters,
        makeBare: true)

    context.buildSpecializedFunction(
      specializedFunction: specializedPb,
      buildFn: { (specializedPb, specializedContext) in
        var cloner = Cloner(cloneToEmptyFunction: specializedPb, specializedContext)
        defer { cloner.deinitialize() }

        cloneAndSpecializePullbackAgainstBTE(using: &cloner, plan: plan)
        // Whole-function cloning does not introduce incomplete lifetimes.
        specializedContext.setNeedCompleteLifetimes(to: false)
      })

    return (specializedPb, false)
  }

  private func cloneAndSpecializePullbackAgainstBTE(using cloner: inout Cloner, plan: BTESpecializationPlan) {
    // Clone with the original types so the cloner preserves ownership.
    cloner.cloneFunctionBody(from: plan.pullback)
    let bteArgument = specializeEntryBTEArgument(using: &cloner, plan: plan)
    if let dispatch = plan.bteSwitch {
      let oldDispatch = cloner.getClonedBlock(for: dispatch.parentBlock).terminator
      let cases = dispatch.cases.map { index, successor in
        (index, cloner.getClonedBlock(for: successor))
      }
      Builder(before: oldDispatch, cloner.context).createSwitchEnum(enum: bteArgument, cases: cases)
      cloner.context.erase(instruction: oldDispatch)
    }

    let payloadArgumentsByBlock = Dictionary(grouping: plan.payloadArguments) { $0.argument.parentBlock }
    let vjp = plan.vjp
    let target = cloner.targetFunction
    for originalBlock in plan.blockOrder.dropFirst() {
      let bb = cloner.getClonedBlock(for: originalBlock)
      for payloadArgument in payloadArgumentsByBlock[originalBlock] ?? [] {
        // The first incoming case can retain its original type.
        guard let enumCase = payloadArgument.incomingCases.lazy.compactMap({
          $0.specialized(using: plan.specializedBTEDict, from: vjp, in: target)
        }).first else { continue }
        let arg = cloner.getClonedValue(of: payloadArgument.argument) as! Argument
        let payloadType = enumCase.payload!
        let newType = payloadArgument.projectedFieldIndex.map { payloadType.tupleElements[$0] } ?? payloadType
        let newArg = bb.replacePhiArgumentAndReplaceAllUses(
          at: arg.index, type: newType,
          ownership: newType.isTrivial(in: cloner.targetFunction) ? .none : arg.ownership,
          cloner.context)

        let fields = payloadArgument.rebuildProjections(of: newArg, using: &cloner)
        for (index, value) in fields {
          switch payloadArgument.kind(ofField: index, in: vjp) {
          case .unchanged:
            break
          case .predecessor:
            rewritePredecessorUse(value, cloner.context)
          case .storedClosure(let info):
            rewriteStoredClosureUses(value, closureInfo: info, cloner.context)
          }
        }
      }
    }
  }

  private func specializeEntryBTEArgument(using cloner: inout Cloner, plan: BTESpecializationPlan) -> FunctionArgument {
    let function = cloner.targetFunction
    let entry = function.entryBlock
    let oldArg = cloner.getClonedValue(of: plan.bteArgument) as! FunctionArgument
    let newType = remapBranchTracingEnumType(plan.specializedBTEType, into: function)
    let isTrivial = newType.isTrivial(in: function)
    let newArg = entry.insertFunctionArgument(
      atPosition: oldArg.index, type: newType,
      ownership: isTrivial ? .none : oldArg.ownership, cloner.context)
    newArg.copyFlags(from: oldArg, cloner.context)
    if isTrivial {
      cloner.context.erase(instructions: oldArg.uses.users(ofType: DestroyValueInst.self))
    }
    oldArg.uses.replaceAll(with: newArg, cloner.context)
    entry.eraseArgument(at: oldArg.index, cloner.context)
    return newArg
  }
}

let getAutoDiffSpecializationInfoTest = FunctionTest("autodiff_get_specialization_info") {
  function, arguments, context in
  guard let analysis = PullbackClosureAnalysis(BranchTracingEnumTypeCache(vjp: function)) else {
    print("Unsupported branch tracing enum specialization")
    return
  }
  print("Run getAutoDiffSpecializationInfo for VJP \(function.name): autodiffSpecializationInfo = (")
  print("  pullback = \(analysis.pullback.name)")
  print("  closuresInBTE = [")
  for closureInBTE in analysis.closuresInBTE {
    print("    ClosureInBTE(")
    print("      closure: \(closureInBTE.closure)")
    print("      adapters: " + (closureInBTE.adapters.isEmpty ? "[]" : "["))
    for adapter in closureInBTE.adapters {
      print("        \(adapter)")
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
) -> Argument? {
  let bb = arg.parentBlock
  guard let newType = specializedBTEDict[arg.type] else { return nil }
  return bb.insertPhiArgument(
    atPosition: arg.index, type: newType, ownership: arg.ownership, context)
}

let specializeBranchTracingEnums = FunctionTest("autodiff_specialize_branch_tracing_enums") {
  function, arguments, context in
  guard let analysis = PullbackClosureAnalysis(BranchTracingEnumTypeCache(vjp: function)) else {
    print("Unsupported branch tracing enum specialization")
    return
  }
  let specializedBTEDict = synthesizeSpecializedEnums(from: analysis, context: context)
  print("Specialized branch tracing enum dict for VJP \(function.name) contains \(specializedBTEDict.count) elements:")

  var keys = [Type](specializedBTEDict.keys)

  func compareTypes(lhs: Type, rhs: Type) -> Bool { "\(lhs)" < "\(rhs)" }
  keys.sort(by: compareTypes)

  func printEnum(_ type: Type, prefix: String) {
    print("\(prefix): enum \(type.nominal!.name) {")
    for enumCase in type.getEnumCases(in: function)! {
      let payload = enumCase.payload.map { ": \($0)" } ?? ""
      print("  case \(enumCase.name)\(payload)")
    }
    print("}")
  }

  for (idx, key) in keys.enumerated() {
    printEnum(key, prefix: "non-specialized BTE \(idx)")
    printEnum(specializedBTEDict[key]!, prefix: "specialized BTE \(idx)")
  }
  print("")
}

let specializeBTEArgInVjpBB = FunctionTest("autodiff_specialize_bte_arg_in_vjp_bb") {
  function, arguments, context in
  guard let analysis = PullbackClosureAnalysis(BranchTracingEnumTypeCache(vjp: function)) else {
    print("Unsupported branch tracing enum specialization")
    return
  }
  let specializedBTEDict = synthesizeSpecializedEnums(from: analysis, context: context)

  print("Specialized BTE arguments of basic blocks in VJP \(function.name):")
  for bb in function.blocks {
    guard let arg = bb.getBranchTracingEnumArg(vjp: function) else {
      continue
    }
    guard let newArg = specializeBranchTracingEnumBBArgInVJP(
      arg: arg, specializedBTEDict: specializedBTEDict, context: context) else { continue }
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
  guard let analysis = PullbackClosureAnalysis(BranchTracingEnumTypeCache(vjp: function)) else {
    print("Unsupported branch tracing enum specialization")
    return
  }
  let specializedBTEDict = synthesizeSpecializedEnums(from: analysis, context: context)

  let pullback = analysis.pullback
  print("Specialized BTE payload arguments of basic blocks in pullback \(pullback.name):")
  for bb in pullback.blocks {
    guard
      let (arg, enumCase) = getBTEPayloadArgOfPbBBInfo(bb, vjp: function),
      let specializedCase = enumCase.specialized(using: specializedBTEDict, from: function, in: function)
    else {
      continue
    }

    let newArg = specializePayloadTupleBBArgInPullback(
      arg: arg,
      enumCase: specializedCase,
      context: context)
    print("\(newArg)")
    bb.eraseArgument(at: newArg.index, context)
  }
  print("")
}
