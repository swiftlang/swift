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

  runClosureSpecialization(function: function, context: context)
}

func runClosureSpecialization(function: Function, context: FunctionPassContext) {
  guard function.hasOwnership else {
    return
  }

  var remainingSpecializationRounds = 5

  repeat {
    var changed = false

    for inst in function.instructions {
      if let apply = inst as? FullApplySite {
        if trySpecialize(apply: apply, context) {
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

  let isSingleBB = function.blocks.singleElement != nil
  var bteSpecEligibility = BTESpecEligibility.ineligible

  if !isSingleBB {
    log(
      "\n\nTrying to run AutoDiff Closure Specialization pass on " + function.name.string)
    bteSpecEligibility = checkIfCanRun(vjp: function, context: context)
    if bteSpecEligibility != .ineligible {
      log(
        "The VJP " + function.name.string
          + " has passed the preliminary check. Proceeding to running the pass")
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

    if trySpecialize(apply: partialApplyInst, context) {
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
    remainingSpecializationRounds = 5
    repeat {
      log("Remaining specialization rounds: " + String(remainingSpecializationRounds))

      let autodiffSpecializationInfo = AutoDiffSpecializationInfo(vjp: function, context)

      if autodiffSpecializationInfo.closuresInBTE.count == 0 {
        log(
          "Unable to detect closures to be specialized in " + function.name.string
            + ", skipping the pass")
        break
      }

      multiBBHelper(
        autodiffSpecializationInfo: autodiffSpecializationInfo, function: function,
        context: context)

      remainingSpecializationRounds -= 1
    } while remainingSpecializationRounds > 0
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
    if vjpBB.getBranchTracingEnumArg(vjp: vjp) != nil {
      break
    }
    for arg in vjpBB.arguments {
      if arg.type.isBranchTracingEnum(in: vjp) {
        log(
          prefixFail + "several arguments of VJP " + vjp.name.string + " basic block "
            + vjpBB.shortDescription + " are branch tracing enums, but not more than 1 is supported"
        )
        return false
      }
    }
  }
  return true
}

private func validatePullbackPayloadBlocks(pb: Function, vjp: Function, prefixFail: String) -> Bool {
  for pbBB in pb.blocks {
    guard let (argOfPbBB, _, _) = getBTEPayloadArgOfPbBBInfo(pbBB, vjp: vjp) else {
      continue
    }
    let payloadValues = getPayloadValues(payload: argOfPbBB, vjp: vjp)
    switch payloadValues {
    case .zeroUses:
      ()
    case .unsupported:
      return false
    case .destructureTuple(let results):
      if !checkIfCanRunForPayloadValues(
        results: results, prefixFail: prefixFail, pb: pb, pbBB: pbBB)
      {
        return false
      }
    case .tupleExtract(let results):
      if !checkIfCanRunForPayloadValues(
        results: results, prefixFail: prefixFail, pb: pb, pbBB: pbBB)
      {
        return false
      }
    }
  }
  return true
}

private func checkIfCanRun(vjp: Function, context: FunctionPassContext) -> BTESpecEligibility {
  assert(vjp.blocks.singleElement == nil)

  let prefixFail = "Cannot run AutoDiff Closure Specialization on " + vjp.name.string + ": "
  guard let paiOfPb = getPartialApplyOfPullbackInExitVJPBB(vjp: vjp) else {
    log(
      prefixFail + "partial_apply of pullback not found in exit basic block of VJP")
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
        prefixFail
          + "VJP seems to contain a loop (builtin autoDiffProjectTopLevelSubcontext detected), this is not supported"
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
      prefixFail + "partial_apply of pullback in exit basic block of VJP has "
        + String(branchTracingEnumArgCounter)
        + " branch tracing enum arguments, but exactly 1 is expected")
    return .ineligible
  }

  guard let pb = paiOfPb.referencedFunction else {
    log(
      prefixFail
        + "cannot obtain pullback function reference from the partial_apply of pullback in exit basic block of VJP"
    )
    return .ineligible
  }
  guard let bteArgOfPb = pb.entryBlock.getBranchTracingEnumArg(vjp: vjp) else {
    log(
      prefixFail + "cannot get branch tracing enum argument of the pullback function "
        + pb.name.string)
    return .ineligible
  }

  guard validatePullbackBTEUsage(pb: pb, bteArg: bteArgOfPb, vjp: vjp, prefixFail: prefixFail) else {
    return .ineligible
  }

  guard ensureEnumPayloadsAreTupleInst(vjp: vjp) else {
    log(prefixFail + "branch tracing enum payload is not defined by a TupleInst")
    return .ineligible
  }

  guard validatePullbackSwitchEnumTerminators(pb: pb, prefixFail: prefixFail) else {
    return .ineligible
  }

  guard validateVJPBlockBTEArgs(vjp: vjp, prefixFail: prefixFail) else {
    return .ineligible
  }

  guard validatePullbackPayloadBlocks(pb: pb, vjp: vjp, prefixFail: prefixFail) else {
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
    let instOpt = ei.operands[0].value.definingInstruction
    if instOpt == nil {
      log("Branch tracing enum payload is not defined by an instruction: \(ei)")
      log("Parent BB begin")
      log("\(ei.parentBlock)")
      log("Parent BB end")
      return false
    }
    let tiOpt = instOpt as? TupleInst
    if tiOpt == nil {
      log("Branch tracing enum payload is defined by a non-tuple instruction: \(ei)")
      log("Defining instruction: \(instOpt!)")
      return false
    }
  }
  return true
}

enum PayloadValues {
  case destructureTuple([Value])
  case tupleExtract([Value])
  case zeroUses
  case unsupported
}

func getPayloadValues(payload: Argument, vjp: Function) -> PayloadValues {
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

private func validateUncheckedEnumDataPayloadUse(uedi: UncheckedEnumDataInst, prefixFail: String) -> Bool {
  if uedi.uses.isAtLeastTwo {
    log(
      prefixFail
        + "unchecked_enum_data instr has \(uedi.uses.count) uses, but no more than 1 is allowed"
    )
    log("  uedi: \(uedi)")
    log("  uedi uses begin")
    for uediUse in uedi.uses {
      log("  uediUse.instruction: \(uediUse.instruction)")
    }
    log("  uedi uses end")
    return false
  }
  if let singleUse = uedi.uses.singleUse {
    if singleUse.instruction as? BranchInst == nil {
      log(
        prefixFail + "unchecked_enum_data instr has unexpected single use")
      log("  uedi: \(uedi)")
      log("  uedi use: \(singleUse.instruction)")
      for (idx, uediUseResult) in singleUse.instruction.results.enumerated() {
        log("  uedi use result \(idx) uses begin")
        for useOfResult in uediUseResult.uses {
          log("    uedi use use: \(useOfResult.instruction)")
        }
        log("  uedi use result \(idx) uses end")
      }
      return false
    }
  }
  return true
}

private func validateConvertFunctionPayloadUse(cfi: ConvertFunctionInst, prefixFail: String) -> Bool {
  if !cfi.uses.isExactlyTwo {
    log(
      prefixFail
        + "expected exactly 2 uses of convert_function use of payload tuple element, found \(cfi.uses.count)"
    )
    for (idx, cfiUse) in cfi.uses.enumerated() {
      log("use \(idx): \(cfiUse)")
    }
    return false
  }
  var bbiUse = Operand?(nil)
  var dviUse = Operand?(nil)
  var sriUse = Operand?(nil)
  for cfiUse in cfi.uses {
    switch cfiUse.instruction {
    case _ as BeginBorrowInst:
      if bbiUse != nil {
        log(
          prefixFail
            + "multiple begin_borrow uses of convert_function result found, but exactly 1 expected"
        )
        return false
      }
      bbiUse = cfiUse
    case _ as DestroyValueInst:
      if dviUse != nil {
        log(
          prefixFail
            + "multiple destroy_value uses of convert_function result found, but exactly 1 expected"
        )
        return false
      }
      dviUse = cfiUse
    case _ as StrongReleaseInst:
      if sriUse != nil {
        log(
          prefixFail
            + "multiple strong_release uses of convert_function result found, but exactly 1 expected"
        )
        return false
      }
      sriUse = cfiUse
    default:
      log(
        prefixFail + "unexpected use of convert_function result found: \(cfiUse)")
      return false
    }
  }
  assert((dviUse != nil) != (sriUse != nil))
  assert(bbiUse != nil)
  return true
}

private func validateBeginBorrowPayloadUse(bbi: BeginBorrowInst, prefixFail: String) -> Bool {
  if !bbi.uses.isExactlyTwo {
    log(
      prefixFail
        + "expected exactly 2 uses of begin_borrow use of payload tuple element, found \(bbi.uses.count)"
    )
    for (idx, bbiUse) in bbi.uses.enumerated() {
      log("use \(idx): \(bbiUse)")
    }
    return false
  }
  var aiUse = Operand?(nil)
  var ebUse = Operand?(nil)
  for bbiUse in bbi.uses {
    switch bbiUse.instruction {
    case _ as EndBorrowInst:
      if ebUse != nil {
        log(
          prefixFail
            + "multiple end_borrow uses of begin_borrow result found, but exactly 1 expected"
        )
        return false
      }
      ebUse = bbiUse
    case _ as ApplyInst:
      if aiUse != nil {
        log(
          prefixFail
            + "multiple apply uses of begin_borrow result found, but exactly 1 expected")
        return false
      }
      aiUse = bbiUse
    default:
      log(
        prefixFail + "unexpected use of begin_borrow result found: \(bbiUse)")
      return false
    }
  }
  assert(ebUse != nil)
  assert(aiUse != nil)
  return true
}

func checkIfCanRunForPayloadValues(
  results: [Value], prefixFail: String, pb: Function, pbBB: BasicBlock
) -> Bool {
  for result in results {
    for use in result.uses {
      switch use.instruction {
      case _ as ApplyInst:
        ()
      case _ as DestroyValueInst:
        ()
      case _ as StrongReleaseInst:
        ()
      case let uedi as UncheckedEnumDataInst:
        if !validateUncheckedEnumDataPayloadUse(uedi: uedi, prefixFail: prefixFail) {
          return false
        }
      case let cfi as ConvertFunctionInst:
        if !validateConvertFunctionPayloadUse(cfi: cfi, prefixFail: prefixFail) {
          return false
        }
      case let bbi as BeginBorrowInst:
        if !validateBeginBorrowPayloadUse(bbi: bbi, prefixFail: prefixFail) {
          return false
        }
      case _ as SwitchEnumInst:
        ()
      case _ as TupleExtractInst:
        ()

      default:
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

private func multiBBHelper(
  autodiffSpecializationInfo: AutoDiffSpecializationInfo, function: Function,
  context: FunctionPassContext
) {
  var closuresSet = Set<SingleValueInstruction>()
  for closureInfo in autodiffSpecializationInfo.closuresInBTE {
    if let subsetThunk = closureInfo.subsetThunk {
      closuresSet.insert(subsetThunk)
    }
    closuresSet.insert(closureInfo.closure)
  }
  let totalSupportedClosures = closuresSet.count

  var totalClosures: Int = 0
  for inst in function.instructions {
    let paiOpt = inst as? PartialApplyInst
    let tttfOpt = inst as? ThinToThickFunctionInst
    if paiOpt != nil || tttfOpt != nil {
      totalClosures += 1
    }
  }

  var enumDict = SpecBTEDict()

  let specInfo = SpecializationInfoCFG()

  let (specializedFunction, alreadyExists) =
    specInfo.getOrCreateSpecializedFunctionCFG(
      basedOn: autodiffSpecializationInfo, enumDict: &enumDict, context)

  if !alreadyExists {
    context.notifyNewFunction(
      function: specializedFunction, derivedFrom: autodiffSpecializationInfo.pullback)
  }

  rewriteApplyInstructionCFG(
    using: specializedFunction, autodiffSpecializationInfo: autodiffSpecializationInfo,
    enumDict: enumDict, context: context)

  var specializedClosures: Int = 0
  var oldSetSize = 0
  repeat {
    oldSetSize = closuresSet.count
    let closures = Array(closuresSet)
    for closure in closures {
      if closure.uses.isEmpty {
        specializedClosures += 1
        context.erase(instruction: closure)
        closuresSet.remove(closure)
      }
    }
  } while oldSetSize != closuresSet.count

  var msg =
    "Specialized " + String(specializedClosures) + " out of " + String(totalSupportedClosures)
    + " supported closures "
  msg += "(rate " + String(Float(specializedClosures) / Float(totalSupportedClosures)) + "). "
  msg += "Total number of closures is " + String(totalClosures)
  log(msg)
}

private func replaceEnumInstructionsWithSpecializedTypes(
  in vjp: Function, enumDict: SpecBTEDict, context: FunctionPassContext
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
  vjp: Function, enumDict: SpecBTEDict, context: FunctionPassContext
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

private func replacePullbackPartialApply(
  pai: PartialApplyInst, specializedCallee: Function, context: FunctionPassContext
) -> PartialApplyInst {
  let builderSucc = Builder(
    before: pai,
    location: pai.parentBlock.instructions.last!.location, context)

  let newFunctionRefInst = builderSucc.createFunctionRef(specializedCallee)
  var newCapturedArgs = [Value]()
  for paiArg in pai.arguments {
    newCapturedArgs.append(paiArg)
  }
  let newPai: PartialApplyInst = builderSucc.createPartialApply(
    function: newFunctionRefInst, substitutionMap: pai.substitutionMap,
    capturedArguments: newCapturedArgs, calleeConvention: pai.calleeConvention,
    hasUnknownResultIsolation: pai.hasUnknownResultIsolation, isOnStack: pai.isOnStack, isNested: pai.isNested)

  pai.replace(with: newPai, context)
  return newPai
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
  vjp: Function, closureInfos: inout [ClosureInBTE], context: FunctionPassContext
) {
  let enumToPayload = findEnumsAndPayloadsInVjp(vjp: vjp)
  let payloads = Set<TupleInst>(enumToPayload.values)

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
      assert(
        (closureInfo.subsetThunk == nil && ti.operands[idxInTuple].value == closureInfo.closure)
          || (closureInfo.subsetThunk != nil
            && ti.operands[idxInTuple].value == closureInfo.subsetThunk!)
          || (closureInfo.optionalWrapper != nil
            && (closureInfo.optionalWrapper!.isOptionalNone
              || ti.operands[idxInTuple].value
                == (closureInfo.closure.uses.singleUse!.instruction as! EnumInst)))
      )
      var isOptionalSome = Bool?(nil)
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
      newLabeledElements.append((label: oldElements.label(at: idx),
                                type: value.type.rawType))
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

private func rewriteApplyInstructionCFG(
  using specializedCallee: Function, autodiffSpecializationInfo: AutoDiffSpecializationInfo,
  enumDict: SpecBTEDict,
  context: FunctionPassContext
) {
  let vjp = autodiffSpecializationInfo.paiOfPullback.parentFunction
  var closureInfos = autodiffSpecializationInfo.closuresInBTE

  replaceEnumInstructionsWithSpecializedTypes(in: vjp, enumDict: enumDict, context: context)

  specializeBTEBlockArgsInVJP(vjp: vjp, enumDict: enumDict, context: context)

  let _ = replacePullbackPartialApply(
    pai: autodiffSpecializationInfo.paiOfPullback, specializedCallee: specializedCallee, context: context)

  rewritePayloadTuplesInVJP(vjp: vjp, closureInfos: &closureInfos, context: context)

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

private func getSpecializedParametersCFG(
  basedOn autodiffSpecializationInfo: AutoDiffSpecializationInfo, pb: Function, enumType: Type,
  enumDict: SpecBTEDict,
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
  var enumCases = [(Int, BasicBlock)]()
  for i in 0..<sei.numCases {
    let bbForCase = sei.getUniqueSuccessor(forCaseIndex: i)
    if bbForCase != nil {
      enumCases.append((i, bbForCase!))
    }
  }
  return enumCases
}

private struct PayloadRewriteContext {
  let closureInfoArray: [ClosureInBTE]
  let useTei: Bool
  let throwingSuccessor: BasicBlock?
}

private func findMatchingClosureInfo(in closureInfoArray: [ClosureInBTE], forPayloadIndex index: Int) -> ClosureInBTE? {
  var result = ClosureInBTE?(nil)
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
  if value.parentFunction.hasOwnership {
    builder.createDestroyValue(operand: value)
  } else {
    builder.createReleaseValue(operand: value)
  }
}

private func extractTupleElements(
  from tuple: Value, usetupleExtract: Bool, builder: Builder
) -> [Value] {
  var elements = [Value]()
  if usetupleExtract {
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

private func rewriteConvertFunctionUse(
  cfi: ConvertFunctionInst, resultIdx: Int,
  result: Value, rewriteCtx: PayloadRewriteContext, _ context: FunctionPassContext
) {
  if findMatchingClosureInfo(in: rewriteCtx.closureInfoArray, forPayloadIndex: resultIdx) != nil {
    assert(cfi.uses.isExactlyTwo)
    let bbiUse = cfi.uses.filter { $0.instruction as? BeginBorrowInst   != nil }.singleElement!
    let dviUse = cfi.uses.filter { $0.instruction as? DestroyValueInst  != nil }.singleElementAssumingAtMostOne
    let sriUse = cfi.uses.filter { $0.instruction as? StrongReleaseInst != nil }.singleElementAssumingAtMostOne
    assert((dviUse != nil) != (sriUse != nil))
    if dviUse != nil {
      context.erase(instruction: dviUse!.instruction)
    } else {
      context.erase(instruction: sriUse!.instruction)
    }
    rewriteUsesOfPayloadItem(
      use: bbiUse, resultIdx: resultIdx,
      result: result, rewriteCtx: rewriteCtx, context)
    context.erase(instruction: cfi)
  } else {
    let builder = Builder(before: cfi, context)
    let newCFI = builder.createConvertFunction(
      originalFunction: result,
      resultType: cfi.type,
      withoutActuallyEscaping: cfi.withoutActuallyEscaping)
    cfi.replace(with: newCFI, context)
  }
}

private func rewriteBeginBorrowUse(
  bbi: BeginBorrowInst, resultIdx: Int,
  result: Value, rewriteCtx: PayloadRewriteContext, _ context: FunctionPassContext
) {
  if findMatchingClosureInfo(in: rewriteCtx.closureInfoArray, forPayloadIndex: resultIdx) != nil {
    assert(bbi.uses.isExactlyTwo)
    let aiUse = bbi.uses.filter { $0.instruction as? ApplyInst     != nil }.singleElement!
    let ebUse = bbi.uses.filter { $0.instruction as? EndBorrowInst != nil }.singleElement!
    context.erase(instruction: ebUse.instruction)
    rewriteUsesOfPayloadItem(
      use: aiUse, resultIdx: resultIdx,
      result: result, rewriteCtx: rewriteCtx, context)
    context.erase(instruction: bbi)
  } else {
    let builder = Builder(before: bbi, context)
    let newBBI = builder.createBeginBorrow(
      of: result,
      isLexical: bbi.isLexical,
      hasPointerEscape: bbi.hasPointerEscape,
      isFromVarDecl: bbi.isFromVarDecl)
    bbi.replace(with: newBBI, context)
  }
}

private func rewriteApplyDirectClosure(
  ai: ApplyInst, closureInfo: ClosureInBTE, extractedElements: [Value],
  builder: Builder, _ context: FunctionPassContext
) {
  var newArgs = [Value]()
  for op in ai.argumentOperands {
    newArgs.append(op.value)
  }
  newArgs.append(contentsOf: extractedElements)
  let vjpFn = closureInfo.closure.asSupportedClosureFn!
  let newFri = builder.createFunctionRef(vjpFn)
  let newAi = builder.createApply(
    function: newFri, ai.substitutionMap, arguments: newArgs)
  ai.replace(with: newAi, context)

  // TODO: maybe we can set insertion point earlier
  for res in extractedElements {
    insertLifetimeEndIfNeeded(for: res, before: newAi.parentBlock.terminator, context)
  }
}

private func rewriteApplyViaSubsetThunk(
  ai: ApplyInst, closureInfo: ClosureInBTE, extractedElements: [Value],
  builder: Builder, _ context: FunctionPassContext
) {
  var newClosure = SingleValueInstruction?(nil)
  if let pai = closureInfo.closure as? PartialApplyInst {
    let vjpFn = closureInfo.closure.asSupportedClosureFn!
    let newFri = builder.createFunctionRef(vjpFn)
    let newPai = builder.createPartialApply(
      function: newFri, substitutionMap: pai.substitutionMap,
      capturedArguments: extractedElements, calleeConvention: pai.calleeConvention,
      hasUnknownResultIsolation: pai.hasUnknownResultIsolation,
      isOnStack: pai.isOnStack, isNested: pai.isNested)
    newClosure = newPai

    // TODO: maybe we can set insertion point earlier
    for res in extractedElements {
      insertLifetimeEndIfNeeded(for: res, before: newPai.parentBlock.terminator, context)
    }
  } else {
    let tttfi = closureInfo.closure as! ThinToThickFunctionInst
    let vjpFn = closureInfo.closure.asSupportedClosureFn!
    let newFri = builder.createFunctionRef(vjpFn)
    let newTttfi = builder.createThinToThickFunction(
      thinFunction: newFri, resultType: tttfi.type)
    newClosure = newTttfi
  }
  assert(newClosure != nil)
  let subsetThunkFn = closureInfo.subsetThunk!.referencedFunction!
  let newFri = builder.createFunctionRef(subsetThunkFn)

  var newArgs = [Value]()
  for op in ai.argumentOperands {
    newArgs.append(op.value)
  }
  newArgs.append(newClosure!)
  let newAi = builder.createApply(
    function: newFri, ai.substitutionMap, arguments: newArgs)
  ai.replace(with: newAi, context)
  assert(newClosure!.uses.singleUse != nil)
  insertLifetimeEndIfNeeded(for: newClosure!, before: newAi.parentBlock.terminator, context)
}

private func rewriteApplyUse(
  ai: ApplyInst, resultIdx: Int,
  result: Value, rewriteCtx: PayloadRewriteContext, _ context: FunctionPassContext
) {
  let builder = Builder(before: ai, context)
  if let closureInfo = findMatchingClosureInfo(in: rewriteCtx.closureInfoArray, forPayloadIndex: resultIdx) {
    let extractedElements = extractTupleElements(from: result, usetupleExtract: rewriteCtx.useTei, builder: builder)
    if closureInfo.subsetThunk == nil {
      rewriteApplyDirectClosure(
        ai: ai, closureInfo: closureInfo, extractedElements: extractedElements,
        builder: builder, context)
    } else {
      rewriteApplyViaSubsetThunk(
        ai: ai, closureInfo: closureInfo, extractedElements: extractedElements,
        builder: builder, context)
    }
  } else {
    var newArgs = [Value]()
    for op in ai.argumentOperands {
      newArgs.append(op.value)
    }
    let newAi = builder.createApply(
      function: result, ai.substitutionMap, arguments: newArgs)
    ai.replace(with: newAi, context)
  }
}

private func rewriteDestroyValueUse(
  dvi: DestroyValueInst, resultIdx: Int,
  result: Value, rewriteCtx: PayloadRewriteContext, _ context: FunctionPassContext
) {
  let isClosurePayload = rewriteCtx.closureInfoArray.contains { $0.indexInPayload == resultIdx }
  if !isClosurePayload {
    let builder = Builder(before: dvi, context)
    if dvi.parentFunction.hasOwnership {
      builder.createDestroyValue(operand: result)
    } else {
      builder.createReleaseValue(operand: result)
    }
  }
  context.erase(instruction: dvi)
}

private func rewriteStrongReleaseUse(
  sri: StrongReleaseInst, resultIdx: Int,
  result: Value, rewriteCtx: PayloadRewriteContext, _ context: FunctionPassContext
) {
  let isClosurePayload = rewriteCtx.closureInfoArray.contains { $0.indexInPayload == resultIdx }
  if !isClosurePayload {
    let builder = Builder(before: sri, context)
    builder.createStrongRelease(operand: result)
  }
  context.erase(instruction: sri)
}

private func rewriteUsesOfPayloadItem(
  use: Operand, resultIdx: Int,
  result: Value, rewriteCtx: PayloadRewriteContext, _ context: FunctionPassContext
) {
  switch use.instruction {
  case let cfi as ConvertFunctionInst:
    rewriteConvertFunctionUse(
      cfi: cfi, resultIdx: resultIdx,
      result: result, rewriteCtx: rewriteCtx, context)

  case let bbi as BeginBorrowInst:
    rewriteBeginBorrowUse(
      bbi: bbi, resultIdx: resultIdx,
      result: result, rewriteCtx: rewriteCtx, context)

  case let ai as ApplyInst:
    rewriteApplyUse(
      ai: ai, resultIdx: resultIdx,
      result: result, rewriteCtx: rewriteCtx, context)

  case let dvi as DestroyValueInst:
    rewriteDestroyValueUse(
      dvi: dvi, resultIdx: resultIdx,
      result: result, rewriteCtx: rewriteCtx, context)

  case let sri as StrongReleaseInst:
    rewriteStrongReleaseUse(
      sri: sri, resultIdx: resultIdx,
      result: result, rewriteCtx: rewriteCtx, context)

  case let tei as TupleExtractInst:
    let builder = Builder(before: tei, context)
    let newTei = builder.createTupleExtract(tuple: tei.tuple, elementIndex: tei.fieldIndex)
    tei.replace(with: newTei, context)

  case let uedi as UncheckedEnumDataInst:
    let builder = Builder(before: uedi, context)
    let newUedi = builder.createUncheckedEnumData(
      enum: result, caseIndex: uedi.caseIndex,
      resultType: result.type.getEnumCases(in: uedi.parentFunction)![uedi.caseIndex]!.payload!)
    uedi.replace(with: newUedi, context)

  case let sei as SwitchEnumInst:
    let builder = Builder(before: sei, context)
    builder.createSwitchEnum(
      enum: result, cases: getEnumCasesForSwitchEnumInst(sei))
    context.erase(instruction: sei)

    if let successor = rewriteCtx.throwingSuccessor {
      let arg = successor.arguments.singleElement!
      let argUses = Array(arg.uses)
      for argUse in argUses {
        rewriteUsesOfPayloadItem(use: argUse, resultIdx: resultIdx, result: arg, rewriteCtx: rewriteCtx, context)
      }
    }

  default:
    assert(false)
  }
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
      let _ = cloner.context.tryOptimizeApplyOfPartialApply(closure: clonedRootClosure)
    }

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
    /// This recursion terminates: `runClosureSpecialization` performs at most 5 rounds per function, and
    /// `findSpecializableClosure` only specializes closures whose callee has `specializationLevel <= 2`.
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

    runClosureSpecialization(function: cloner.targetFunction, context: cloner.context)
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

extension ClosureInBTE {
  var capturedArgs : [Value] {
    if let pai = self.closure as? PartialApplyInst {
      var newCapturedArgs = [Value]()
      for paiArg in pai.arguments {
        newCapturedArgs.append(paiArg)
      }
      return newCapturedArgs
    }
    guard let _ = self.closure as? ThinToThickFunctionInst else {
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
    // TODO: figure out what to do with non-inout indirect arguments
    // https://forums.swift.org/t/non-inout-indirect-types-not-supported-in-closure-specialization-optimization/70826
    case let pai as PartialApplyInst
    where pai.callee is FunctionRefInst && pai.hasOnlyInoutIndirectArguments:
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

private typealias SpecBTEDict = [Type: Type]

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

  func specializedCalleeNameCFG(_ context: FunctionPassContext) -> String {
    let argAndIdxInPbPAI = paiOfPullback.arguments.enumerated().filter {
      $0.1.type.isBranchTracingEnum(in: vjp)
    }.singleElement!
    return context.mangle(
      withBranchTracingEnum: argAndIdxInPbPAI.1, argIdx: argAndIdxInPbPAI.0,
      from: pullback)
  }
}

private func computeTopologicalBlockOrder(of function: Function) -> [BasicBlock] {
  var bbVisited = [BasicBlock: Bool]()
  bbVisited[function.entryBlock] = true
  var bbQueue = [BasicBlock]()
  bbQueue.append(function.entryBlock)
  while bbVisited.count != function.blocks.count {
    for bb in function.blocks {
      if bbVisited[bb] == true {
        continue
      }
      var allPredsVisited = true
      for predBB in bb.predecessors {
        if bbVisited[predBB] != true {
          allPredsVisited = false
          break
        }
      }
      if allPredsVisited {
        bbQueue.append(bb)
        bbVisited[bb] = true
      }
    }
  }
  return bbQueue
}

private func findCorrespondingVJPPayloadTuple(
  forBB bb: BasicBlock, argIndex: Int, enumToPayload: [EnumInst: TupleInst], enumDict: SpecBTEDict
) -> TupleInst? {
  let predBB = bb.predecessors.first!
  let enumType: Type
  let caseIdx: Int
  if let brInst = predBB.terminator as? BranchInst {
    let uedi = brInst.operands[argIndex].value.definingInstruction as! UncheckedEnumDataInst
    enumType = uedi.`enum`.type
    caseIdx = uedi.caseIndex
  } else if let sei = predBB.terminator as? SwitchEnumInst {
    enumType = sei.enumOp.type
    caseIdx = sei.getUniqueCase(forSuccessor: bb)!
  } else {
    return nil
  }
  for (enumInst, payload) in enumToPayload {
    if enumDict[enumInst.type] == enumType && enumInst.caseIndex == caseIdx {
      return payload
    }
  }
  return nil
}

private func collectMatchingClosureInfos(
  forPayload tiInVjp: TupleInst, allClosureInfos: [ClosureInBTE]
) -> [ClosureInBTE] {
  var closureInfoArray = [ClosureInBTE]()
  for (opIdx, op) in tiInVjp.operands.enumerated() {
    let val = op.value
    for closureInfo in allClosureInfos {
      if ((closureInfo.subsetThunk == nil && closureInfo.closure == val)
        || (closureInfo.subsetThunk != nil && closureInfo.subsetThunk! == val)
        || (closureInfo.optionalWrapper != nil && (closureInfo.closure.uses.singleUse!.instruction as! EnumInst) == val))
        && closureInfo.payloadTuple == tiInVjp
      {
        assert(closureInfo.indexInPayload == opIdx)
        closureInfoArray.append(closureInfo)
      }
    }
  }
  return closureInfoArray
}

private struct SpecializationInfoCFG {
  typealias Cloner = SIL.Cloner<FunctionPassContext>

func getOrCreateSpecializedFunctionCFG(
  basedOn autodiffSpecializationInfo: AutoDiffSpecializationInfo, enumDict: inout SpecBTEDict,
  _ context: FunctionPassContext
)
  -> (function: Function, alreadyExists: Bool)
{
  let pb = autodiffSpecializationInfo.pullback
  let vjp = autodiffSpecializationInfo.paiOfPullback.parentFunction

  let specializedPbName = autodiffSpecializationInfo.specializedCalleeNameCFG(context)
  if let specializedPb = context.lookupFunction(name: specializedPbName) {
    return (specializedPb, true)
  }

  let enumTypeOfEntryBBArg = pb.entryBlock.getBranchTracingEnumArg(vjp: vjp)!.type
  enumDict = autodiffSpecializationInfo.specializedBTEDict

  let specializedParameters = getSpecializedParametersCFG(
    basedOn: autodiffSpecializationInfo, pb: pb, enumType: enumTypeOfEntryBBArg, enumDict: enumDict,
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

      cloneAndSpecializeFunctionBodyCFG(using: &cloner, autodiffSpecializationInfo: autodiffSpecializationInfo, enumDict: enumDict)
      // Cloning a whole function, even if it contains an `unreachable`, doesn't require lifetime completion.
      specializedContext.setNeedCompleteLifetimes(to: false)
  })

  context.notifyNewFunction(function: specializedPb, derivedFrom: pb)

  return (specializedPb, false)
}

  func cloneAndSpecializeFunctionBodyCFG(
    using cloner: inout Cloner, autodiffSpecializationInfo: AutoDiffSpecializationInfo, enumDict: SpecBTEDict
  ) {
    let closureInfos = autodiffSpecializationInfo.closuresInBTE
    self.cloneEntryBlockArgsWithoutOrigClosuresCFG(
      using: &cloner, usingOrigCalleeAt: autodiffSpecializationInfo, enumDict: enumDict)

    var args = [Value]()
    for arg in cloner.targetFunction.entryBlock.arguments {
      args.append(arg)
    }

    cloner.cloneFunctionBody(from: autodiffSpecializationInfo.pullback, entryBlockArguments: args)

    let bbQueue = computeTopologicalBlockOrder(of: cloner.targetFunction)

    for bb in bbQueue {
      // With single-bb, we've ensured that there are no uses of BTE arg, so no manipulation required
      if bb == cloner.targetFunction.entryBlock && cloner.targetFunction.blocks.singleElement == nil {
        let bteArg = bb.getBranchTracingEnumArg(
          vjp: autodiffSpecializationInfo.paiOfPullback.parentFunction)!
        let sei = bteArg.uses.singleUse!.instruction as! SwitchEnumInst
        let builderEntry = Builder(before: sei, cloner.context)

        builderEntry.createSwitchEnum(
          enum: sei.enumOp, cases: getEnumCasesForSwitchEnumInst(sei))
        cloner.context.erase(instruction: sei)

        continue
      }

      guard
        let (arg, enumCase, throwingSuccessor) = getBTEPayloadArgOfPbBBInfo(
          bb, vjp: autodiffSpecializationInfo.paiOfPullback.parentFunction)
      else {
        continue
      }

      let enumToPayload = findEnumsAndPayloadsInVjp(
        vjp: autodiffSpecializationInfo.paiOfPullback.parentFunction)
      let tiInVjp = findCorrespondingVJPPayloadTuple(
        forBB: bb, argIndex: arg.index, enumToPayload: enumToPayload, enumDict: enumDict)

      let closureInfoArray: [ClosureInBTE] = {
        if let tiInVjp = tiInVjp {
          return collectMatchingClosureInfos(forPayload: tiInVjp, allClosureInfos: closureInfos)
        }
        return []
      }()
      let newArg = specializePayloadTupleBBArgInPullback(
        arg: arg, enumCase: enumCase, context: cloner.context)
      arg.uses.replaceAll(with: newArg, cloner.context)
      bb.eraseArgument(at: arg.index, cloner.context)

      if newArg.uses.isEmpty {
        continue
      }

      if let successor = throwingSuccessor {
        let oldArg = successor.arguments.singleElement!
        let newArg = specializeOptionalBBArgInPullback(
          bb: successor, newOptionalType: enumCase.payload!.tupleElements.last!, context: cloner.context)
        oldArg.uses.replaceAll(with: newArg, cloner.context)
        successor.eraseArgument(at: oldArg.index, cloner.context)
      }

      if newArg.uses.singleUse != nil
        && newArg.uses.singleUse!.instruction as? DestructureTupleInst != nil
      {
        let oldDti = newArg.uses.singleUse!.instruction as! DestructureTupleInst
        let builderBeforeOldDti = Builder(before: oldDti, cloner.context)
        let newDti = builderBeforeOldDti.createDestructureTuple(tuple: oldDti.tuple)

        let rewriteCtx = PayloadRewriteContext(
          closureInfoArray: closureInfoArray, useTei: false,
          throwingSuccessor: throwingSuccessor)
        for (resultIdx, result) in oldDti.results.enumerated() {
          let resultUses = Array(result.uses)
          for use in resultUses {
            rewriteUsesOfPayloadItem(
              use: use, resultIdx: resultIdx,
              result: newDti.results[resultIdx],
              rewriteCtx: rewriteCtx, cloner.context)
          }
        }

        cloner.context.erase(instruction: oldDti)
        continue
      }

      let rewriteCtx = PayloadRewriteContext(
        closureInfoArray: closureInfoArray, useTei: true,
        throwingSuccessor: throwingSuccessor)
      let newArgUses = Array(newArg.uses)
      for newArgUse in newArgUses {
        let oldTei = newArgUse.instruction as! TupleExtractInst
        let builderBeforeOldTei = Builder(before: oldTei, cloner.context)
        let newTei = builderBeforeOldTei.createTupleExtract(
          tuple: oldTei.tuple, elementIndex: oldTei.fieldIndex)

        let oldTeiResultUses = Array(oldTei.results[0].uses)
        for use in oldTeiResultUses {
          rewriteUsesOfPayloadItem(
            use: use, resultIdx: oldTei.fieldIndex,
            result: newTei.results[0],
            rewriteCtx: rewriteCtx, cloner.context)
        }

        oldTei.replace(with: newTei, cloner.context)
      }
    }
  }

  private func cloneEntryBlockArgsWithoutOrigClosuresCFG(
    using cloner: inout Cloner, usingOrigCalleeAt autodiffSpecializationInfo: AutoDiffSpecializationInfo, enumDict: SpecBTEDict
  ) {
    let pb = autodiffSpecializationInfo.pullback
    let enumType = pb.entryBlock.getBranchTracingEnumArg(
      vjp: autodiffSpecializationInfo.paiOfPullback.parentFunction)!.type

    let originalEntryBlock = autodiffSpecializationInfo.pullback.entryBlock
    let clonedFunction = cloner.targetFunction
    let clonedEntryBlock = cloner.getOrCreateEntryBlock()

    for arg in originalEntryBlock.arguments {
      var clonedEntryBlockArgType = arg.type.getLoweredType(in: clonedFunction)
      if clonedEntryBlockArgType == enumType {
        // The requested dict element is always present since we have at least 1 closure (otherwise, we wouldn't go here).
        // It causes re-write of the corresponding branch tracing enum, and the top enum type will be re-written transitively.
        clonedEntryBlockArgType = enumDict[enumType]!
      }
      let clonedEntryBlockArg = clonedEntryBlock.addFunctionArgument(
        type: clonedEntryBlockArgType, cloner.context)
      clonedEntryBlockArg.copyFlags(from: arg as! FunctionArgument, cloner.context)
    }
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
