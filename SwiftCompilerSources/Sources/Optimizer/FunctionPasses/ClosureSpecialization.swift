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

/// This file contains the closure-specialization optimizations for general and differentiable Swift.

/// General Closure Specialization
/// ------------------------------------
/// TODO: Add description when the functionality is added.

/// AutoDiff Closure Specialization
/// -------------------------------
/// This optimization performs closure specialization tailored for the patterns seen in Swift Autodiff. In principle,
/// the optimization does the same thing as the existing closure specialization pass. However, it is tailored to the
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

import AST
import SIL
import SILBridging

private let verbose = false

private func log(prefix: Bool = true, _ message: @autoclosure () -> String) {
  if verbose {
    debugLog(prefix: prefix, message())
  }
}

// =========== Entry point =========== //
let generalClosureSpecialization = FunctionPass(name: "experimental-swift-based-closure-specialization") {
  (function: Function, context: FunctionPassContext) in
  // TODO: Implement general closure specialization optimization
  print("NOT IMPLEMENTED")
}

let autodiffClosureSpecialization = FunctionPass(name: "autodiff-closure-specialization") {
  (function: Function, context: FunctionPassContext) in

  guard !function.isDefinedExternally,
        function.isAutodiffVJP else {
    return
  }
  
  var remainingSpecializationRounds = 5

  repeat {
    guard let pullbackClosureInfo = getPullbackClosureInfo(in: function, context) else {
      break
    }

    var (specializedFunction, alreadyExists) = getOrCreateSpecializedFunction(basedOn: pullbackClosureInfo, context)

    if !alreadyExists {
      context.notifyNewFunction(function: specializedFunction, derivedFrom: pullbackClosureInfo.pullbackFn)
    }

    rewriteApplyInstruction(using: specializedFunction, pullbackClosureInfo: pullbackClosureInfo, context)

    var deadClosures = InstructionWorklist(context)
    pullbackClosureInfo.closureArgDescriptors
      .map { $0.closure }
      .forEach { deadClosures.pushIfNotVisited($0) }

    defer {
      deadClosures.deinitialize()
    }

    while let deadClosure = deadClosures.pop() {
      let isDeleted = context.tryDeleteDeadClosure(closure: deadClosure as! SingleValueInstruction)
      if isDeleted {
        context.notifyInvalidatedStackNesting()
      }
    }

    if context.needFixStackNesting {
      context.fixStackNesting(in: function)
    }

    remainingSpecializationRounds -= 1
  } while remainingSpecializationRounds > 0
}

// =========== Top-level functions ========== //

private let specializationLevelLimit = 2

private func getPullbackClosureInfo(in caller: Function, _ context: FunctionPassContext) -> PullbackClosureInfo? {
  /// __Root__ closures created via `partial_apply` or `thin_to_thick_function` may be converted and reabstracted
  /// before finally being used at an apply site. We do not want to handle these intermediate closures separately
  /// as they are handled and cloned into the specialized function as part of the root closures. Therefore, we keep
  /// track of these intermediate closures in a set.
  ///
  /// This set is populated via the `markConvertedAndReabstractedClosuresAsUsed` function which is called when we're
  /// handling the different uses of our root closures.
  ///
  /// Below SIL example illustrates the above point.
  /// ```
  /// // The below set of a "root" closure and its reabstractions/conversions
  /// // will be handled as a unit and the entire set will be copied over
  /// // in the specialized version of `takesClosure` if we determine that we
  /// // can specialize `takesClosure` against its closure argument.
  ///                                                                                                          __
  /// %someFunction = function_ref @someFunction: $@convention(thin) (Int, Int) -> Int                            \
  /// %rootClosure = partial_apply [callee_guaranteed] %someFunction (%someInt): $(Int, Int) -> Int                \
  /// %thunk = function_ref @reabstractionThunk : $@convention(thin) (@callee_guaranteed (Int) -> Int) -> @out Int /
  /// %reabstractedClosure = partial_apply [callee_guaranteed] %thunk(%rootClosure) :                             /
  ///                        $@convention(thin) (@callee_guaranteed (Int) -> Int) -> @out Int                  __/
  ///
  /// %takesClosure = function_ref @takesClosure : $@convention(thin) (@owned @callee_guaranteed (Int) -> @out Int) -> Int
  /// %result = partial_apply %takesClosure(%reabstractedClosure) : $@convention(thin) (@owned @callee_guaranteed () -> @out Int) -> Int
  /// ret %result
  /// ```
  var convertedAndReabstractedClosures = InstructionSet(context)

  defer {
    convertedAndReabstractedClosures.deinitialize()
  }

  var pullbackClosureInfoOpt = PullbackClosureInfo?(nil)

  for inst in caller.instructions {
    if !convertedAndReabstractedClosures.contains(inst),
       let rootClosure = inst.asSupportedClosure
    {
      updatePullbackClosureInfo(for: rootClosure, in: &pullbackClosureInfoOpt,
                                convertedAndReabstractedClosures: &convertedAndReabstractedClosures, context)
    }
  }

  return pullbackClosureInfoOpt
}

private func getOrCreateSpecializedFunction(basedOn pullbackClosureInfo: PullbackClosureInfo, _ context: FunctionPassContext)
  -> (function: Function, alreadyExists: Bool)
{
  let specializedFunctionName = pullbackClosureInfo.specializedCalleeName(context)
  if let specializedFunction = context.lookupFunction(name: specializedFunctionName) {
    return (specializedFunction, true)
  }

  let pullbackFn = pullbackClosureInfo.pullbackFn
  let specializedParameters = pullbackFn.convention.getSpecializedParameters(basedOn: pullbackClosureInfo)

  let specializedFunction =
    context.createSpecializedFunctionDeclaration(from: pullbackFn, withName: specializedFunctionName,
                                                 withParams: specializedParameters,
                                                 makeThin: true, makeBare: true)

  context.buildSpecializedFunction(specializedFunction: specializedFunction,
                                   buildFn: { (emptySpecializedFunction, functionPassContext) in
                                      var closureSpecCloner = Cloner(cloneToEmptyFunction: emptySpecializedFunction, functionPassContext)
                                      defer { closureSpecCloner.deinitialize() }
                                      closureSpecCloner.cloneAndSpecializeFunctionBody(using: pullbackClosureInfo)
                                   })

  return (specializedFunction, false)
}

private func rewriteApplyInstruction(using specializedCallee: Function, pullbackClosureInfo: PullbackClosureInfo,
                                     _ context: FunctionPassContext) {
  let newApplyArgs = pullbackClosureInfo.getArgumentsForSpecializedApply(of: specializedCallee)

  for newApplyArg in newApplyArgs {
    if case let .PreviouslyCaptured(capturedArg, needsRetain, parentClosureArgIndex) = newApplyArg,
       needsRetain
    {
      let closureArgDesc = pullbackClosureInfo.closureArgDesc(at: parentClosureArgIndex)!
      var builder = Builder(before: closureArgDesc.closure, context)

      // TODO: Support only OSSA instructions once the OSSA elimination pass is moved after all function optimization
      // passes.
      if pullbackClosureInfo.paiOfPullback.parentBlock != closureArgDesc.closure.parentBlock {
        // Emit the retain and release that keeps the argument live across the callee using the closure.
        builder.createRetainValue(operand: capturedArg)

        for instr in closureArgDesc.lifetimeFrontier {
          builder = Builder(before: instr, context)
          builder.createReleaseValue(operand: capturedArg)
        }

        // Emit the retain that matches the captured argument by the partial_apply in the callee that is consumed by
        // the partial_apply.
        builder = Builder(before: pullbackClosureInfo.paiOfPullback, context)
        builder.createRetainValue(operand: capturedArg)
      } else {
        builder.createRetainValue(operand: capturedArg)
      }
    }
  }

  // Rewrite apply instruction
  var builder = Builder(before: pullbackClosureInfo.paiOfPullback, context)
  let oldPartialApply = pullbackClosureInfo.paiOfPullback
  let funcRef = builder.createFunctionRef(specializedCallee)
  let capturedArgs = Array(newApplyArgs.map { $0.value })

  let newPartialApply = builder.createPartialApply(function: funcRef, substitutionMap: SubstitutionMap(),
                                                   capturedArguments: capturedArgs, calleeConvention: oldPartialApply.calleeConvention,
                                                   hasUnknownResultIsolation: oldPartialApply.hasUnknownResultIsolation,
                                                   isOnStack: oldPartialApply.isOnStack)

  builder = Builder(before: pullbackClosureInfo.paiOfPullback.next!, context)
  // TODO: Support only OSSA instructions once the OSSA elimination pass is moved after all function optimization
  // passes.
  for closureArgDesc in pullbackClosureInfo.closureArgDescriptors {
    if closureArgDesc.isClosureConsumed,
       !closureArgDesc.isPartialApplyOnStack,
       !closureArgDesc.parameterInfo.isTrivialNoescapeClosure
    {
      builder.createReleaseValue(operand: closureArgDesc.closure)
    }
  }

  oldPartialApply.replace(with: newPartialApply, context)
}

// ===================== Utility functions and extensions ===================== //

private func updatePullbackClosureInfo(for rootClosure: SingleValueInstruction, in pullbackClosureInfoOpt: inout PullbackClosureInfo?,
                                       convertedAndReabstractedClosures: inout InstructionSet, _ context: FunctionPassContext) {
  var rootClosurePossibleLiveRange = InstructionRange(begin: rootClosure, context)
  defer {
    rootClosurePossibleLiveRange.deinitialize()
  }

  var rootClosureApplies = OperandWorklist(context)
  defer {
    rootClosureApplies.deinitialize()
  }

  // A "root" closure undergoing conversions and/or reabstractions has additional restrictions placed upon it, in order
  // for a pullback to be specialized against it. We handle conversion/reabstraction uses before we handle apply uses
  // to gather the parameters required to evaluate these restrictions or to skip pullback's uses of "unsupported"
  // closures altogether.
  //
  // There are currently 2 restrictions that are evaluated prior to specializing a pullback against a converted and/or
  // reabstracted closure -
  // 1. A reabstracted root closure can only be specialized against, if the reabstracted closure is ultimately passed
  //    trivially (as a noescape+thick function) as captured argument of pullback's partial_apply.
  //
  // 2. A root closure may be a partial_apply [stack], in which case we need to make sure that all mark_dependence
  //    bases for it will be available in the specialized callee in case the pullback is specialized against this root
  //    closure.

  let (foundUnexpectedUse, haveUsedReabstraction) =
    handleNonApplies(for: rootClosure, rootClosureApplies: &rootClosureApplies,
                     rootClosurePossibleLiveRange: &rootClosurePossibleLiveRange, context);


  if foundUnexpectedUse {
    return
  }

  let intermediateClosureArgDescriptorData =
    handleApplies(for: rootClosure, pullbackClosureInfoOpt: &pullbackClosureInfoOpt, rootClosureApplies: &rootClosureApplies,
                  rootClosurePossibleLiveRange: &rootClosurePossibleLiveRange,
                  convertedAndReabstractedClosures: &convertedAndReabstractedClosures,
                  haveUsedReabstraction: haveUsedReabstraction, context)

  if pullbackClosureInfoOpt == nil {
    return
  }

  finalizePullbackClosureInfo(for: rootClosure, in: &pullbackClosureInfoOpt,
                              rootClosurePossibleLiveRange: rootClosurePossibleLiveRange,
                              intermediateClosureArgDescriptorData: intermediateClosureArgDescriptorData, context)
}

/// Handles all non-apply direct and transitive uses of `rootClosure`.
///
/// Returns:
/// haveUsedReabstraction - whether the root closure is reabstracted via a thunk
/// foundUnexpectedUse - whether the root closure is directly or transitively used in an instruction that we don't know
///                      how to handle. If true, then `rootClosure` should not be specialized against.
private func handleNonApplies(for rootClosure: SingleValueInstruction,
                              rootClosureApplies: inout OperandWorklist,
                              rootClosurePossibleLiveRange: inout InstructionRange,
                              _ context: FunctionPassContext)
  -> (foundUnexpectedUse: Bool, haveUsedReabstraction: Bool)
{
  var foundUnexpectedUse = false
  var haveUsedReabstraction = false

  /// The root closure or an intermediate closure created by reabstracting the root closure may be a `partial_apply
  /// [stack]` and we need to make sure that all `mark_dependence` bases for this `onStack` closure will be available in
  /// the specialized callee, in case the pullback is specialized against this root closure.
  ///
  /// `possibleMarkDependenceBases` keeps track of all potential values that may be used as bases for creating
  /// `mark_dependence`s for our `onStack` root/reabstracted closures. For root closures these values are non-trivial
  /// closure captures (which are always available as function arguments in the specialized callee). For reabstracted
  /// closures these values may be the root closure or its conversions (below is a short SIL example representing this
  /// case).
  /// ```
  /// %someFunction = function_ref @someFunction : $@convention(thin) (Int) -> Int
  /// %rootClosure = partial_apply [callee_guaranteed] %someFunction(%someInt) : $@convention(thin) (Int) -> Int
  /// %noescapeRootClosure = convert_escape_to_noescape %rootClosure : $@callee_guaranteed () -> Int to $@noescape @callee_guaranteed () -> Int
  /// %thunk = function_ref @reabstractionThunk : $@convention(thin) (@noescape @callee_guaranteed () -> Int) -> @out Int
  /// %thunkedRootClosure = partial_apply [callee_guaranteed] [on_stack] %thunk(%noescapeRootClosure) : $@convention(thin) (@noescape @callee_guaranteed () -> Int) -> @out Int
  /// %dependency = mark_dependence %thunkedRootClosure : $@noescape @callee_guaranteed () -> @out Int on %noescapeClosure : $@noescape @callee_guaranteed () -> Int
  /// %takesClosure = function_ref @takesClosure : $@convention(thin) (@owned @noescape @callee_guaranteed () -> @out Int)
  /// %ret = apply %takesClosure(%dependency) : $@convention(thin) (@owned @noescape @callee_guaranteed () -> @out Int)
  /// ```
  ///
  /// Any value outside of the aforementioned values is not going to be available in the specialized callee and a
  /// `mark_dependence` of the root closure on such a value means that we cannot specialize the pullback against it.
  var possibleMarkDependenceBases = ValueSet(context)
  defer {
    possibleMarkDependenceBases.deinitialize()
  }

  var rootClosureConversionsAndReabstractions = OperandWorklist(context)
  rootClosureConversionsAndReabstractions.pushIfNotVisited(contentsOf: rootClosure.uses)
  defer {
    rootClosureConversionsAndReabstractions.deinitialize()
  }

  if let pai = rootClosure as? PartialApplyInst {
    for arg in pai.arguments {
      possibleMarkDependenceBases.insert(arg)
    }
  }
  
  while let use = rootClosureConversionsAndReabstractions.pop() {
    switch use.instruction {
    case let cfi as ConvertFunctionInst:
      rootClosureConversionsAndReabstractions.pushIfNotVisited(contentsOf: cfi.uses)
      possibleMarkDependenceBases.insert(cfi)
      rootClosurePossibleLiveRange.insert(use.instruction)

    case let cvt as ConvertEscapeToNoEscapeInst:
      rootClosureConversionsAndReabstractions.pushIfNotVisited(contentsOf: cvt.uses)
      possibleMarkDependenceBases.insert(cvt)
      rootClosurePossibleLiveRange.insert(use.instruction)

    case let pai as PartialApplyInst:
      if !pai.isPullbackInResultOfAutodiffVJP,
          pai.isSupportedClosure,
          pai.isPartialApplyOfThunk,
          // Argument must be a closure
          pai.arguments[0].type.isThickFunction
      {
        rootClosureConversionsAndReabstractions.pushIfNotVisited(contentsOf: pai.uses)
        possibleMarkDependenceBases.insert(pai)
        rootClosurePossibleLiveRange.insert(use.instruction)
        haveUsedReabstraction = true
      } else if pai.isPullbackInResultOfAutodiffVJP {
        rootClosureApplies.pushIfNotVisited(use)
      }

    case let mv as MoveValueInst:
      rootClosureConversionsAndReabstractions.pushIfNotVisited(contentsOf: mv.uses)
      possibleMarkDependenceBases.insert(mv)
      rootClosurePossibleLiveRange.insert(use.instruction)

    case let mdi as MarkDependenceInst:
      if possibleMarkDependenceBases.contains(mdi.base),
          mdi.value == use.value,
          mdi.value.type.isNoEscapeFunction,
          mdi.value.type.isThickFunction
      {
        rootClosureConversionsAndReabstractions.pushIfNotVisited(contentsOf: mdi.uses)
        rootClosurePossibleLiveRange.insert(use.instruction)
      }
    
    case is CopyValueInst,
         is DestroyValueInst,
         is RetainValueInst,
         is ReleaseValueInst,
         is StrongRetainInst,
         is StrongReleaseInst:
      rootClosurePossibleLiveRange.insert(use.instruction)

    case let ti as TupleInst:
      if ti.parentFunction.isAutodiffVJP,
         let returnInst = ti.parentFunction.returnInstruction,
         ti == returnInst.returnedValue
      {
        // This is the pullback closure returned from an Autodiff VJP and we don't need to handle it.
      } else {
        fallthrough
      }

    default:
      foundUnexpectedUse = true
      log("Found unexpected direct or transitive user of root closure: \(use.instruction)")
      return (foundUnexpectedUse, haveUsedReabstraction)
    }
  }

  return (foundUnexpectedUse, haveUsedReabstraction)
}

private typealias IntermediateClosureArgDescriptorDatum = (applySite: SingleValueInstruction, closureArgIndex: Int, paramInfo: ParameterInfo)

private func handleApplies(for rootClosure: SingleValueInstruction, pullbackClosureInfoOpt: inout PullbackClosureInfo?,
                           rootClosureApplies: inout OperandWorklist,
                           rootClosurePossibleLiveRange: inout InstructionRange,
                           convertedAndReabstractedClosures: inout InstructionSet, haveUsedReabstraction: Bool,
                           _ context: FunctionPassContext) -> [IntermediateClosureArgDescriptorDatum]
{
  var intermediateClosureArgDescriptorData: [IntermediateClosureArgDescriptorDatum] = []
  
  while let use = rootClosureApplies.pop() {
    rootClosurePossibleLiveRange.insert(use.instruction)

    // TODO [extend to general swift]: Handle full apply sites
    guard let pai = use.instruction as? PartialApplyInst else {
      continue
    }

    // TODO: Handling generic closures may be possible but is not yet implemented
    if pai.hasSubstitutions || !pai.calleeIsDynamicFunctionRef || !pai.isPullbackInResultOfAutodiffVJP {
      continue
    }

    guard let callee = pai.referencedFunction else {
      continue
    }

    if callee.isDefinedExternally {
      continue
    }

    // Don't specialize non-fragile (read as non-serialized) callees if the caller is fragile; the specialized callee
    // will have shared linkage, and thus cannot be referenced from the fragile caller.
    let caller = rootClosure.parentFunction
    if caller.isSerialized && !callee.isSerialized {
      continue
    }

    // If the callee uses a dynamic Self, we cannot specialize it, since the resulting specialization might no longer
    // have 'self' as the last parameter.
    //
    // TODO: We could fix this by inserting new arguments more carefully, or changing how we model dynamic Self
    // altogether.
    if callee.mayBindDynamicSelf {
      continue
    }

    // Proceed if the closure is passed as an argument (and not called). If it is called we have nothing to do.
    //
    // `closureArgumentIndex` is the index of the closure in the callee's argument list.
    guard let closureArgumentIndex = pai.calleeArgumentIndex(of: use) else {
      continue
    }

    // Ok, we know that we can perform the optimization but not whether or not the optimization is profitable. Check if
    // the closure is actually called in the callee (or in a function called by the callee).
    if !isClosureApplied(in: callee, closureArgIndex: closureArgumentIndex) {
      continue
    }

    let onlyHaveThinToThickClosure = rootClosure is ThinToThickFunctionInst && !haveUsedReabstraction

    guard let closureParamInfo = pai.operandConventions[parameter: use.index] else {
      fatalError("While handling apply uses, parameter info not found for operand: \(use)!")
    }

    // If we are going to need to release the copied over closure, we must make sure that we understand all the exit
    // blocks, i.e., they terminate with an instruction that clearly indicates whether to release the copied over
    // closure or leak it.
    if closureParamInfo.convention.isGuaranteed,
       !onlyHaveThinToThickClosure,
       !callee.blocks.allSatisfy({ $0.isReachableExitBlock || $0.terminator is UnreachableInst })
    {
      continue
    }

    // Functions with a readnone, readonly or releasenone effect and a nontrivial context cannot be specialized.
    // Inserting a release in such a function results in miscompilation after other optimizations. For now, the
    // specialization is disabled.
    //
    // TODO: A @noescape closure should never be converted to an @owned argument regardless of the function's effect
    // attribute.
    if !callee.effectAllowsSpecialization && !onlyHaveThinToThickClosure {
      continue
    }

    // Avoid an infinite specialization loop caused by repeated runs of ClosureSpecializer and CapturePropagation.
    // CapturePropagation propagates constant function-literals. Such function specializations can then be optimized
    // again by the ClosureSpecializer and so on. This happens if a closure argument is called _and_ referenced in
    // another closure, which is passed to a recursive call. E.g.
    //
    // func foo(_ c: @escaping () -> ()) {
    //  c() foo({ c() })
    // }
    //
    // A limit of 2 is good enough and will not be exceed in "regular" optimization scenarios.
    let closureCallee = rootClosure is PartialApplyInst
                        ? (rootClosure as! PartialApplyInst).referencedFunction!
                        : (rootClosure as! ThinToThickFunctionInst).referencedFunction!

    if closureCallee.specializationLevel > specializationLevelLimit {
      continue
    }

    if haveUsedReabstraction {
      markConvertedAndReabstractedClosuresAsUsed(rootClosure: rootClosure, convertedAndReabstractedClosure: use.value,
                                                 convertedAndReabstractedClosures: &convertedAndReabstractedClosures)
    }
    
    if pullbackClosureInfoOpt == nil {
      pullbackClosureInfoOpt = PullbackClosureInfo(paiOfPullback: pai)
    } else {
      assert(pullbackClosureInfoOpt!.paiOfPullback == pai)
    }

    intermediateClosureArgDescriptorData
      .append((applySite: pai, closureArgIndex: closureArgumentIndex, paramInfo: closureParamInfo))
  }

  return intermediateClosureArgDescriptorData
}

/// Finalizes the pullback closure info for a given root closure by adding a corresponding `ClosureArgDescriptor`
private func finalizePullbackClosureInfo(for rootClosure: SingleValueInstruction, in pullbackClosureInfoOpt: inout PullbackClosureInfo?,
                                         rootClosurePossibleLiveRange: InstructionRange,
                                         intermediateClosureArgDescriptorData: [IntermediateClosureArgDescriptorDatum],
                                         _ context: FunctionPassContext) {
  assert(pullbackClosureInfoOpt != nil)

  let closureInfo = ClosureInfo(closure: rootClosure, lifetimeFrontier: Array(rootClosurePossibleLiveRange.ends))

  for (applySite, closureArgumentIndex, parameterInfo) in intermediateClosureArgDescriptorData {
    if pullbackClosureInfoOpt!.paiOfPullback != applySite {
      fatalError("ClosureArgDescriptor's applySite field is not equal to pullback's partial_apply; got \(applySite)!")
    }
    let closureArgDesc = ClosureArgDescriptor(closureInfo: closureInfo, closureArgumentIndex: closureArgumentIndex,
                                              parameterInfo: parameterInfo)
    pullbackClosureInfoOpt!.appendClosureArgDescriptor(closureArgDesc)
  }
}

private func isClosureApplied(in callee: Function, closureArgIndex index: Int) -> Bool {
  func inner(_ callee: Function, _ index: Int, _ handledFuncs: inout Set<Function>) -> Bool {
    let closureArg = callee.argument(at: index)

    for use in closureArg.uses {
      if let fai = use.instruction as? ApplySite {
        if fai.callee == closureArg {
          return true
        }

        if let faiCallee = fai.referencedFunction,
           !faiCallee.blocks.isEmpty,
           handledFuncs.insert(faiCallee).inserted,
           handledFuncs.count <= recursionBudget
        {
          if inner(faiCallee, fai.calleeArgumentIndex(of: use)!, &handledFuncs) {
            return true
          }
        }
      }
    }

    return false
  }

  // Limit the number of recursive calls to not go into exponential behavior in corner cases.
  let recursionBudget = 8
  var handledFuncs: Set<Function> = []
  return inner(callee, index, &handledFuncs)
}

/// Marks any converted/reabstracted closures, corresponding to a given root closure as used. We do not want to
/// look at such closures separately as during function specialization they will be handled as part of the root closure.
private func markConvertedAndReabstractedClosuresAsUsed(rootClosure: Value, convertedAndReabstractedClosure: Value,
                                                        convertedAndReabstractedClosures: inout InstructionSet)
{
  if convertedAndReabstractedClosure != rootClosure {
    switch convertedAndReabstractedClosure {
    case let pai as PartialApplyInst:
      convertedAndReabstractedClosures.insert(pai)
      return
        markConvertedAndReabstractedClosuresAsUsed(rootClosure: rootClosure,
                                                   convertedAndReabstractedClosure: pai.arguments[0],
                                                   convertedAndReabstractedClosures: &convertedAndReabstractedClosures)
    case let cvt as ConvertFunctionInst:
      convertedAndReabstractedClosures.insert(cvt)
      return
        markConvertedAndReabstractedClosuresAsUsed(rootClosure: rootClosure,
                                                   convertedAndReabstractedClosure: cvt.fromFunction,
                                                   convertedAndReabstractedClosures: &convertedAndReabstractedClosures)
    case let cvt as ConvertEscapeToNoEscapeInst:
      convertedAndReabstractedClosures.insert(cvt)
      return
        markConvertedAndReabstractedClosuresAsUsed(rootClosure: rootClosure,
                                                   convertedAndReabstractedClosure: cvt.fromFunction,
                                                   convertedAndReabstractedClosures: &convertedAndReabstractedClosures)
    case let mdi as MarkDependenceInst:
      convertedAndReabstractedClosures.insert(mdi)
      return
        markConvertedAndReabstractedClosuresAsUsed(rootClosure: rootClosure, convertedAndReabstractedClosure: mdi.value,
                                                   convertedAndReabstractedClosures: &convertedAndReabstractedClosures)
    default:
      log("Parent function of pullbackClosureInfo: \(rootClosure.parentFunction)")
      log("Root closure: \(rootClosure)")
      log("Converted/reabstracted closure: \(convertedAndReabstractedClosure)")
      fatalError("While marking converted/reabstracted closures as used, found unexpected instruction: \(convertedAndReabstractedClosure)")
    }
  }
}

private extension Cloner where Context == FunctionPassContext {
  func cloneAndSpecializeFunctionBody(using pullbackClosureInfo: PullbackClosureInfo) {
    self.cloneEntryBlockArgsWithoutOrigClosures(usingOrigCalleeAt: pullbackClosureInfo)

    let (allSpecializedEntryBlockArgs, closureArgIndexToAllClonedReleasableClosures) = cloneAllClosures(at: pullbackClosureInfo)

    self.cloneFunctionBody(from: pullbackClosureInfo.pullbackFn, entryBlockArguments: allSpecializedEntryBlockArgs)

    self.insertCleanupCodeForClonedReleasableClosures(
      from: pullbackClosureInfo, closureArgIndexToAllClonedReleasableClosures: closureArgIndexToAllClonedReleasableClosures)
  }

  private func cloneEntryBlockArgsWithoutOrigClosures(usingOrigCalleeAt pullbackClosureInfo: PullbackClosureInfo) {
    let originalEntryBlock = pullbackClosureInfo.pullbackFn.entryBlock
    let clonedFunction = self.targetFunction
    let clonedEntryBlock = self.getOrCreateEntryBlock()

    originalEntryBlock.arguments
      .enumerated()
      .filter { index, _ in !pullbackClosureInfo.hasClosureArg(at: index) }
      .forEach { _, arg in
        let clonedEntryBlockArgType = arg.type.getLoweredType(in: clonedFunction)
        let clonedEntryBlockArg = clonedEntryBlock.addFunctionArgument(type: clonedEntryBlockArgType, self.context)
        clonedEntryBlockArg.copyFlags(from: arg as! FunctionArgument, self.context)
      }
  }

  /// Clones all closures, originally passed to the callee at the given pullbackClosureInfo, into the specialized function.
  ///
  /// Returns the following -
  /// - allSpecializedEntryBlockArgs: Complete list of entry block arguments for the specialized function. This includes
  ///   the original arguments to the function (minus the closure arguments) and the arguments representing the values
  ///   originally captured by the skipped closure arguments.
  ///
  /// - closureArgIndexToAllClonedReleasableClosures: Mapping from a closure's argument index at `pullbackClosureInfo` to the list
  ///   of corresponding releasable closures cloned into the specialized function. We have a "list" because we clone
  ///   "closure chains", which consist of a "root" closure and its conversions/reabstractions. This map is used to
  ///   generate cleanup code for the cloned closures in the specialized function.
  private func cloneAllClosures(at pullbackClosureInfo: PullbackClosureInfo)
    -> (allSpecializedEntryBlockArgs: [Value],
        closureArgIndexToAllClonedReleasableClosures: [Int: [SingleValueInstruction]])
  {
    func entryBlockArgsWithOrigClosuresSkipped() -> [Value?] {
      let clonedEntryBlock = self.getOrCreateEntryBlock()
      var clonedNonClosureEntryBlockArgs = clonedEntryBlock.arguments.makeIterator()

      return pullbackClosureInfo.pullbackFn
        .entryBlock
        .arguments
        .enumerated()
        .reduce(into: []) { result, origArgTuple in
          let (index, _) = origArgTuple
          if !pullbackClosureInfo.hasClosureArg(at: index) {
            result.append(clonedNonClosureEntryBlockArgs.next())
          } else {
            result.append(Optional.none)
          }
        }
    }

    var entryBlockArgs: [Value?] = entryBlockArgsWithOrigClosuresSkipped()
    var closureArgIndexToAllClonedReleasableClosures: [Int: [SingleValueInstruction]] = [:]

    for closureArgDesc in pullbackClosureInfo.closureArgDescriptors {
      let (finalClonedReabstractedClosure, allClonedReleasableClosures) =
        self.cloneClosureChain(representedBy: closureArgDesc, at: pullbackClosureInfo)

      entryBlockArgs[closureArgDesc.closureArgIndex] = finalClonedReabstractedClosure
      closureArgIndexToAllClonedReleasableClosures[closureArgDesc.closureArgIndex] = allClonedReleasableClosures
    }

    return (entryBlockArgs.map { $0! }, closureArgIndexToAllClonedReleasableClosures)
  }

  private func cloneClosureChain(representedBy closureArgDesc: ClosureArgDescriptor, at pullbackClosureInfo: PullbackClosureInfo)
    -> (finalClonedReabstractedClosure: SingleValueInstruction, allClonedReleasableClosures: [SingleValueInstruction])
  {
    let (origToClonedValueMap, capturedArgRange) = self.addEntryBlockArgs(forValuesCapturedBy: closureArgDesc)
    let clonedFunction = self.targetFunction
    let clonedEntryBlock = self.getOrCreateEntryBlock()
    let clonedClosureArgs = Array(clonedEntryBlock.arguments[capturedArgRange])

    let builder = clonedEntryBlock.instructions.isEmpty
                  ? Builder(atStartOf: clonedFunction, self.context)
                  : Builder(atEndOf: clonedEntryBlock, location: clonedEntryBlock.instructions.last!.location, self.context)

    let clonedRootClosure = builder.cloneRootClosure(representedBy: closureArgDesc, capturedArguments: clonedClosureArgs)

    let finalClonedReabstractedClosure =
      builder.cloneRootClosureReabstractions(rootClosure: closureArgDesc.closure, clonedRootClosure: clonedRootClosure,
                                             reabstractedClosure: pullbackClosureInfo.appliedArgForClosure(at: closureArgDesc.closureArgIndex)!,
                                             origToClonedValueMap: origToClonedValueMap,
                                             self.context)

    let allClonedReleasableClosures = [ finalClonedReabstractedClosure ];
    return (finalClonedReabstractedClosure, allClonedReleasableClosures)
  }

  private func addEntryBlockArgs(forValuesCapturedBy closureArgDesc: ClosureArgDescriptor)
    -> (origToClonedValueMap: [HashableValue: Value], capturedArgRange: Range<Int>)
  {
    var origToClonedValueMap: [HashableValue: Value] = [:]
    let clonedFunction = self.targetFunction
    let clonedEntryBlock = self.getOrCreateEntryBlock()

    let capturedArgRangeStart = clonedEntryBlock.arguments.count
      
    for arg in closureArgDesc.arguments {
      let capturedArg = clonedEntryBlock.addFunctionArgument(type: arg.type.getLoweredType(in: clonedFunction),
                                                              self.context)
      origToClonedValueMap[arg] = capturedArg
    }

    let capturedArgRangeEnd = clonedEntryBlock.arguments.count
    let capturedArgRange = capturedArgRangeStart == capturedArgRangeEnd
                           ? 0..<0
                           : capturedArgRangeStart..<capturedArgRangeEnd

    return (origToClonedValueMap, capturedArgRange)
  }

  private func insertCleanupCodeForClonedReleasableClosures(from pullbackClosureInfo: PullbackClosureInfo,
                                                            closureArgIndexToAllClonedReleasableClosures: [Int: [SingleValueInstruction]])
  {
    for closureArgDesc in pullbackClosureInfo.closureArgDescriptors {
      let allClonedReleasableClosures = closureArgIndexToAllClonedReleasableClosures[closureArgDesc.closureArgIndex]!

      // Insert a `destroy_value`, for all releasable closures, in all reachable exit BBs if the closure was passed as a
      // guaranteed parameter or its type was noescape+thick. This is b/c the closure was passed at +0 originally and we
      // need to balance the initial increment of the newly created closure(s).
      if closureArgDesc.isClosureGuaranteed || closureArgDesc.parameterInfo.isTrivialNoescapeClosure,
         !allClonedReleasableClosures.isEmpty
      {
        for exitBlock in pullbackClosureInfo.reachableExitBBsInCallee {
          let clonedExitBlock = self.getClonedBlock(for: exitBlock)
          
          let terminator = clonedExitBlock.terminator is UnreachableInst
                           ? clonedExitBlock.terminator.previous!
                           : clonedExitBlock.terminator

          let builder = Builder(before: terminator, self.context)

          for closure in allClonedReleasableClosures {
            if let pai = closure as? PartialApplyInst {
              builder.destroyPartialApply(pai: pai, self.context)
            }
          }
        }
      }
    }

    if (self.context.needFixStackNesting) {
      self.context.fixStackNesting(in: targetFunction)
    }
  }
}

private extension [HashableValue: Value] {
  subscript(key: Value) -> Value? {
    get {
      self[key.hashable]
    }
    set {
      self[key.hashable] = newValue
    }
  }
}

private extension PullbackClosureInfo {
  enum NewApplyArg {
    case Original(Value)
    // TODO: This can be simplified in OSSA. We can just do a copy_value for everything - except for addresses???
    case PreviouslyCaptured(
      value: Value, needsRetain: Bool, parentClosureArgIndex: Int)

    var value: Value {
      switch self {
      case let .Original(originalArg):
        return originalArg
      case let .PreviouslyCaptured(capturedArg, _, _):
        return capturedArg
      }
    }
  }

  func getArgumentsForSpecializedApply(of specializedCallee: Function) -> [NewApplyArg]
  {
    var newApplyArgs: [NewApplyArg] = []

    // Original arguments
    for (applySiteIndex, arg) in self.paiOfPullback.arguments.enumerated() {
      let calleeArgIndex = self.paiOfPullback.unappliedArgumentCount + applySiteIndex
      if !self.hasClosureArg(at: calleeArgIndex) {
        newApplyArgs.append(.Original(arg))
      }
    }

    // Previously captured arguments
    for closureArgDesc in self.closureArgDescriptors {
      for (applySiteIndex, capturedArg) in closureArgDesc.arguments.enumerated() {
        let needsRetain = closureArgDesc.isCapturedArgNonTrivialObjectType(applySiteIndex: applySiteIndex,
                                                                           specializedCallee: specializedCallee)

        newApplyArgs.append(.PreviouslyCaptured(value: capturedArg, needsRetain: needsRetain,
                                                parentClosureArgIndex: closureArgDesc.closureArgIndex))
      }
    }

    return newApplyArgs
  }
}

private extension ClosureArgDescriptor {
  func isCapturedArgNonTrivialObjectType(applySiteIndex: Int, specializedCallee: Function) -> Bool {
    precondition(self.closure is PartialApplyInst, "ClosureArgDescriptor is not for a partial_apply closure!")

    let capturedArg = self.arguments[applySiteIndex]
    let pai = self.closure as! PartialApplyInst
    let capturedArgIndexInCallee = applySiteIndex + pai.unappliedArgumentCount
    let capturedArgConvention = self.callee.argumentConventions[capturedArgIndexInCallee]

    return !capturedArg.type.isTrivial(in: specializedCallee) &&
           !capturedArgConvention.isAllowedIndirectConvForClosureSpec
  }
}

private extension Builder {
  func cloneRootClosure(representedBy closureArgDesc: ClosureArgDescriptor, capturedArguments: [Value])
    -> SingleValueInstruction
  {
    let function = self.createFunctionRef(closureArgDesc.callee)

    if let pai = closureArgDesc.closure as? PartialApplyInst {
      return self.createPartialApply(function: function, substitutionMap: SubstitutionMap(),
                                     capturedArguments: capturedArguments, calleeConvention: pai.calleeConvention,
                                     hasUnknownResultIsolation: pai.hasUnknownResultIsolation,
                                     isOnStack: pai.isOnStack)
    } else {
      return self.createThinToThickFunction(thinFunction: function, resultType: closureArgDesc.closure.type)
    }
  }

  func cloneRootClosureReabstractions(rootClosure: Value, clonedRootClosure: Value, reabstractedClosure: Value,
                                      origToClonedValueMap: [HashableValue: Value], _ context: FunctionPassContext)
    -> SingleValueInstruction
  {
    func inner(_ rootClosure: Value, _ clonedRootClosure: Value, _ reabstractedClosure: Value,
               _ origToClonedValueMap: inout [HashableValue: Value]) -> Value {
      switch reabstractedClosure {
        case let reabstractedClosure where reabstractedClosure == rootClosure:
          origToClonedValueMap[reabstractedClosure] = clonedRootClosure
          return clonedRootClosure
        
        case let cvt as ConvertFunctionInst:
          let toBeReabstracted = inner(rootClosure, clonedRootClosure, cvt.fromFunction,
                                       &origToClonedValueMap)
          let reabstracted = self.createConvertFunction(originalFunction: toBeReabstracted, resultType: cvt.type,
                                                        withoutActuallyEscaping: cvt.withoutActuallyEscaping)
          origToClonedValueMap[cvt] = reabstracted
          return reabstracted
        
        case let cvt as ConvertEscapeToNoEscapeInst:
          let toBeReabstracted = inner(rootClosure, clonedRootClosure, cvt.fromFunction,
                                       &origToClonedValueMap)
          let reabstracted = self.createConvertEscapeToNoEscape(originalFunction: toBeReabstracted, resultType: cvt.type,
                                                                isLifetimeGuaranteed: true)
          origToClonedValueMap[cvt] = reabstracted
          return reabstracted

        case let pai as PartialApplyInst:
          let toBeReabstracted = inner(rootClosure, clonedRootClosure, pai.arguments[0],
                                       &origToClonedValueMap)
          
          guard let function = pai.referencedFunction else {
            log("Parent function of pullbackClosureInfo: \(rootClosure.parentFunction)")
            log("Root closure: \(rootClosure)")
            log("Unsupported reabstraction closure: \(pai)")
            fatalError("Encountered unsupported reabstraction (via partial_apply) of root closure!")
          }

          let fri = self.createFunctionRef(function)
          let reabstracted = self.createPartialApply(function: fri, substitutionMap: SubstitutionMap(),
                                                     capturedArguments: [toBeReabstracted],
                                                     calleeConvention: pai.calleeConvention,
                                                     hasUnknownResultIsolation: pai.hasUnknownResultIsolation,
                                                     isOnStack: pai.isOnStack)
          origToClonedValueMap[pai] = reabstracted
          return reabstracted
        
        case let mdi as MarkDependenceInst:
          let toBeReabstracted = inner(rootClosure, clonedRootClosure, mdi.value, &origToClonedValueMap)
          let base = origToClonedValueMap[mdi.base]!
          let reabstracted = self.createMarkDependence(value: toBeReabstracted, base: base, kind: .Escaping)
          origToClonedValueMap[mdi] = reabstracted
          return reabstracted
        
        default:
          log("Parent function of pullbackClosureInfo: \(rootClosure.parentFunction)")
          log("Root closure: \(rootClosure)")
          log("Converted/reabstracted closure: \(reabstractedClosure)")
          fatalError("Encountered unsupported reabstraction of root closure: \(reabstractedClosure)")
      }
    }

    var origToClonedValueMap = origToClonedValueMap
    let finalClonedReabstractedClosure = inner(rootClosure, clonedRootClosure, reabstractedClosure,
                                               &origToClonedValueMap)
    return (finalClonedReabstractedClosure as! SingleValueInstruction)
  }

  func destroyPartialApply(pai: PartialApplyInst, _ context: FunctionPassContext){
    // TODO: Support only OSSA instructions once the OSSA elimination pass is moved after all function optimization
    // passes.

    if pai.isOnStack {
      // for arg in pai.arguments {
      //   self.createDestroyValue(operand: arg)
      // }
      // self.createDestroyValue(operand: pai)

      if pai.parentFunction.hasOwnership {
      // Under OSSA, the closure acts as an owned value whose lifetime is a borrow scope for the captures, so we need to
      // end the borrow scope before ending the lifetimes of the captures themselves.
        self.createDestroyValue(operand: pai)
        self.destroyCapturedArgs(for: pai)
      } else {
        self.destroyCapturedArgs(for: pai)
        self.createDeallocStack(pai)
        context.notifyInvalidatedStackNesting()
      }
    } else {
      if pai.parentFunction.hasOwnership {
        self.createDestroyValue(operand: pai)
      } else {
        self.createReleaseValue(operand: pai)
      }
    }
  }
}

private extension FunctionConvention {
  func getSpecializedParameters(basedOn pullbackClosureInfo: PullbackClosureInfo) -> [ParameterInfo] {
    let pullbackFn = pullbackClosureInfo.pullbackFn
    var specializedParamInfoList: [ParameterInfo] = []

    // Start by adding all original parameters except for the closure parameters.
    let firstParamIndex = pullbackFn.argumentConventions.firstParameterIndex
    for (index, paramInfo) in pullbackFn.convention.parameters.enumerated() {
      let argIndex = index + firstParamIndex
      if !pullbackClosureInfo.hasClosureArg(at: argIndex) {
        specializedParamInfoList.append(paramInfo)
      }
    }

    // Now, append parameters captured by each of the original closure parameter.
    //
    // Captured parameters are always appended to the function signature. If the argument type of the captured
    // parameter in the callee is:
    // - direct and trivial, pass the new parameter as Direct_Unowned.
    // - direct and non-trivial, pass the new parameter as Direct_Owned.
    // - indirect, pass the new parameter using the same parameter convention as in
    //   the original closure.
    for closureArgDesc in pullbackClosureInfo.closureArgDescriptors {
      if let closure = closureArgDesc.closure as? PartialApplyInst {
        let closureCallee = closureArgDesc.callee
        let closureCalleeConvention = closureCallee.convention
        let unappliedArgumentCount = closure.unappliedArgumentCount - closureCalleeConvention.indirectSILResultCount

        let prevCapturedParameters =
          closureCalleeConvention
          .parameters[unappliedArgumentCount...]
          .enumerated()
          .map { index, paramInfo in
            let argIndexOfParam = closureCallee.argumentConventions.firstParameterIndex + unappliedArgumentCount + index
            let argType = closureCallee.argumentTypes[argIndexOfParam]
            return paramInfo.withSpecializedConvention(isArgTypeTrivial: argType.isTrivial(in: closureCallee))
          }

        specializedParamInfoList.append(contentsOf: prevCapturedParameters)
      }
    }

    return specializedParamInfoList
  }
}

private extension ParameterInfo {
  func withSpecializedConvention(isArgTypeTrivial: Bool) -> Self {
    let specializedParamConvention = self.convention.isAllowedIndirectConvForClosureSpec
      ? self.convention
      : isArgTypeTrivial ? ArgumentConvention.directUnowned : ArgumentConvention.directOwned

    return ParameterInfo(type: self.type, convention: specializedParamConvention, options: self.options,
                         hasLoweredAddresses: self.hasLoweredAddresses)
  }

  var isTrivialNoescapeClosure: Bool {
    SILFunctionType_isTrivialNoescape(type.bridged)
  }
}

private extension ArgumentConvention {
  var isAllowedIndirectConvForClosureSpec: Bool {
    switch self {
    case .indirectInout, .indirectInoutAliasable:
      return true
    default:
      return false
    }
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
}

private extension Instruction {
  var asSupportedClosure: SingleValueInstruction? {
    switch self {
    case let tttf as ThinToThickFunctionInst where tttf.callee is FunctionRefInst:
      return tttf
    // TODO: figure out what to do with non-inout indirect arguments
    // https://forums.swift.org/t/non-inout-indirect-types-not-supported-in-closure-specialization-optimization/70826
    case let pai as PartialApplyInst where pai.callee is FunctionRefInst && pai.hasOnlyInoutIndirectArguments:
      return pai
    default:
      return nil
    }
  }

  var isSupportedClosure: Bool {
    asSupportedClosure != nil
  }
}

private extension ApplySite {
  var calleeIsDynamicFunctionRef: Bool {
    return !(callee is DynamicFunctionRefInst || callee is PreviousDynamicFunctionRefInst)
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

// ===================== Utility Types ===================== //
private struct OrderedDict<Key: Hashable, Value> {
  private var valueIndexDict: [Key: Int] = [:]
  private var entryList: [(Key, Value)] = []

  subscript(key: Key) -> Value? {
    if let index = valueIndexDict[key] {
      return entryList[index].1
    }
    return nil
  }

  mutating func insert(key: Key, value: Value) {
    if valueIndexDict[key] == nil {
      valueIndexDict[key] = entryList.count
      entryList.append((key, value))
    }
  }

  mutating func update(key: Key, value: Value) {
    if let index = valueIndexDict[key] {
      entryList[index].1 = value
    }
  }

  var keys: LazyMapSequence<Array<(Key, Value)>, Key> {
    entryList.lazy.map { $0.0 }
  }

  var values: LazyMapSequence<Array<(Key, Value)>, Value> {
    entryList.lazy.map { $0.1 }
  }
}

/// Represents all the information required to represent a closure in isolation, i.e., outside of a pullback's partial_apply context
/// where the closure may be getting captured as an argument.
///
/// Composed with other information inside a `ClosureArgDescriptor` to represent a closure as a captured argument of a pullback's partial_apply.
private struct ClosureInfo {
  let closure: SingleValueInstruction
  let lifetimeFrontier: [Instruction]

  init(closure: SingleValueInstruction, lifetimeFrontier: [Instruction]) {
    self.closure = closure
    self.lifetimeFrontier = lifetimeFrontier
  }

}

/// Represents a closure as a captured argument of a pullback's partial_apply.
private struct ClosureArgDescriptor {
  let closureInfo: ClosureInfo
  /// The index of the closure in the pullback's partial_apply argument list.
  let closureArgumentIndex: Int
  let parameterInfo: ParameterInfo

  var closure: SingleValueInstruction {
    closureInfo.closure
  }
  var lifetimeFrontier: [Instruction] {
    closureInfo.lifetimeFrontier
  }

  var isPartialApplyOnStack: Bool {
    if let pai = closure as? PartialApplyInst {
      return pai.isOnStack
    }
    return false
  }

  var callee: Function {
    if let pai = closure as? PartialApplyInst {
      return pai.referencedFunction!
    } else {
      return (closure as! ThinToThickFunctionInst).referencedFunction!
    }
  }

  var location: Location {
    closure.location
  }

  var closureArgIndex: Int {
    closureArgumentIndex
  }

  var closureParamInfo: ParameterInfo {
    parameterInfo
  }

  var numArguments: Int {
    if let pai = closure as? PartialApplyInst {
      return pai.numArguments
    } else {
      return 0
    }
  }

  var arguments: LazyMapSequence<OperandArray, Value> {
    if let pai = closure as? PartialApplyInst {
      return pai.arguments
    }

    return OperandArray.empty.lazy.map { $0.value } as LazyMapSequence<OperandArray, Value>
  }

  var isClosureGuaranteed: Bool {
    closureParamInfo.convention.isGuaranteed
  }

  var isClosureConsumed: Bool {
    closureParamInfo.convention.isConsumed
  }
}

/// Represents a partial_apply of pullback capturing one or more closure arguments.
private struct PullbackClosureInfo {
  let paiOfPullback: PartialApplyInst
  var closureArgDescriptors: [ClosureArgDescriptor] = []

  init(paiOfPullback: PartialApplyInst) {
    self.paiOfPullback = paiOfPullback
  }

  mutating func appendClosureArgDescriptor(_ descriptor: ClosureArgDescriptor) {
    self.closureArgDescriptors.append(descriptor)
  }

  var pullbackFn: Function {
    paiOfPullback.referencedFunction!
  }

  var reachableExitBBsInCallee: [BasicBlock] {
    pullbackFn.blocks.filter { $0.isReachableExitBlock }
  }

  func hasClosureArg(at index: Int) -> Bool {
    closureArgDescriptors.contains { $0.closureArgumentIndex == index }
  }

  func closureArgDesc(at index: Int) -> ClosureArgDescriptor? {
    closureArgDescriptors.first { $0.closureArgumentIndex == index }
  }

  func appliedArgForClosure(at index: Int) -> Value? {
    if let closureArgDesc = closureArgDesc(at: index) {
      return paiOfPullback.arguments[closureArgDesc.closureArgIndex - paiOfPullback.unappliedArgumentCount]
    }

    return nil
  }

  func specializedCalleeName(_ context: FunctionPassContext) -> String {
    let closureArgs = Array(self.closureArgDescriptors.map {
      (argumentIndex: $0.closureArgIndex, argumentValue: $0.closure)
    })
    return context.mangle(withClosureArguments: closureArgs, from: pullbackFn)
  }
}

// ===================== Unit tests ===================== //

let getPullbackClosureInfoTest = FunctionTest("autodiff_closure_specialize_get_pullback_closure_info") { function, arguments, context in
  print("Specializing closures in function: \(function.name)")
  print("===============================================")
  let pullbackClosureInfo = getPullbackClosureInfo(in: function, context)!
  print("PartialApply of pullback: \(pullbackClosureInfo.paiOfPullback)")
  print("Passed in closures: ")
  for index in pullbackClosureInfo.closureArgDescriptors.indices {
    var closureArgDescriptor = pullbackClosureInfo.closureArgDescriptors[index]
    print("\(index+1). \(closureArgDescriptor.closureInfo.closure)")
  }
  print("\n")
}

let specializedFunctionSignatureAndBodyTest = FunctionTest(
  "autodiff_closure_specialize_specialized_function_signature_and_body") { function, arguments, context in

  let pullbackClosureInfo = getPullbackClosureInfo(in: function, context)!

  let (specializedFunction, _) = getOrCreateSpecializedFunction(basedOn: pullbackClosureInfo, context)
  print("Generated specialized function: \(specializedFunction.name)")
  print("\(specializedFunction)\n")
}

let rewrittenCallerBodyTest = FunctionTest("autodiff_closure_specialize_rewritten_caller_body") { function, arguments, context in
  let pullbackClosureInfo = getPullbackClosureInfo(in: function, context)!

  let (specializedFunction, _) = getOrCreateSpecializedFunction(basedOn: pullbackClosureInfo, context)
  rewriteApplyInstruction(using: specializedFunction, pullbackClosureInfo: pullbackClosureInfo, context)

  print("Rewritten caller body for: \(function.name):")
  print("\(function)\n")
}
