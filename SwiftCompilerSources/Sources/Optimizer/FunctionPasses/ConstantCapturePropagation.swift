//===--- ConstantCapturePropagation.swift ---------------------------------===//
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

import SIL
import AST

/// Propagates constant closure captures by specializing the partially applied function:
///
/// ```
///   %1 = function_ref @closure
///   %2 = integer_literal $Builtin.Int64, 27
///   %3 = partial_apply %1(%2)
///
/// sil @closure : $(Builtin.Int64) -> () {
/// bb0(%0 : $Builtin.Int64):
///   ...
/// ```
/// ->
/// ```
///   %1 = function_ref @specialized_closure
///   %3 = partial_apply %1()
///
/// sil @specialized_closure : $() -> () {
/// bb0:
///   %0 = integer_literal $Builtin.Int64, 27
///   ...
/// ```
///
/// Also, optimizes away a `partial_apply` of a thunk to a closure where all partially applied
/// arguments are dead:
///
/// ```
///   %2 = function_ref @thunk
///   %3 = partial_apply %2(%1)
///
/// sil @thunk : $(T, V) -> () {
/// bb0(%0 : $T, %1 : $V):        // %1 is dead
///   %2 = function_ref @closure
///   %3 = apply %2(%0)           // alive arguments are forwarded 1:1
///   return %3
/// }
/// ```
/// ->
/// ```
///   %2 = function_ref @closure
///   %3 = thin_to_thick_function %2
/// ```
///
let constantCapturePropagation = FunctionPass(name: "constant-capture-propagation") {
  (function: Function, context: FunctionPassContext) in

  for inst in function.instructions {
    guard let partialApply = inst as? PartialApplyInst,
          // Only support closures which - after generic specialization - are not generic anymore.
          !partialApply.substitutionMap.replacementTypes.contains(where: { $0.hasArchetype })
    else {
      continue
    }

    if !context.continueWithNextSubpassRun(for: partialApply) {
      return
    }

    optimizeClosureWithDeadCaptures(of: partialApply, context)

    if partialApply.isDeleted {
      continue
    }

    constantPropagateCaptures(of: partialApply, context)
  }
}

private func optimizeClosureWithDeadCaptures(of partialApply: PartialApplyInst, _ context: FunctionPassContext) {
  if let callee = getSpecializedCalleeWithDeadParams(of: partialApply, context) {
    rewritePartialApply(partialApply, withSpecialized: callee, arguments: [], context)
  }
}

private func constantPropagateCaptures(of partialApply: PartialApplyInst, _ context: FunctionPassContext) {
  guard let callee = partialApply.referencedFunction,
        callee.isDefinition,
        let (constArgs, nonConstArgs) = partialApply.classifyArgumentsForConstness()
  else {
    return
  }

  let specializedName = context.mangle(withConstantCaptureArguments: constArgs.map {
                                           (partialApply.calleeArgumentIndex(of: $0)!, $0.value)
                                         },
                                       from: callee)

  let specializedCallee: Function
  if let existing = context.lookupFunction(name: specializedName) {
    specializedCallee = existing
  } else {
    specializedCallee = specializeClosure(specializedName: specializedName,
                                          partialApply: partialApply,
                                          constantArguments: constArgs, nonConstantArguments: nonConstArgs,
                                          context)
  }
  if !partialApply.isOnStack {
    // Escaping closures consume their arguments. Therefore we need to destroy the removed argument values.
    addCompensatingDestroys(for: constArgs, context)
  }
  let newArguments = nonConstArgs.map { $0.value }
  rewritePartialApply(partialApply, withSpecialized: specializedCallee, arguments: newArguments, context)
}

private func getSpecializedCalleeWithDeadParams(of partialApply: PartialApplyInst,
                                                _ context: FunctionPassContext
) -> Function? {
  guard let specialized = partialApply.getCalleeOfForwardingThunkWithDeadCaptures(),
        specialized.abi == .Swift
  else {
    return nil
  }

  // Specialize the callee if it is generic
  if partialApply.substitutionMap.hasAnySubstitutableParams {
    guard specialized.isDefinition,
          partialApply.referencedFunction!.shouldOptimize, specialized.shouldOptimize
    else {
      return nil
    }

    let genericSpecialized = context.specialize(function: specialized, for: partialApply.substitutionMap,
                                                convertIndirectToDirect: false, isMandatory: false)
    return genericSpecialized
  }

  return specialized
}

private func specializeClosure(specializedName: String,
                               partialApply: PartialApplyInst,
                               constantArguments: [Operand], nonConstantArguments: [Operand],
                               _ context: FunctionPassContext
) -> Function {
  let callee = partialApply.referencedFunction!
  var newParams = [ParameterInfo]()
  newParams.append(contentsOf: callee.convention.parameters.dropLast(partialApply.numArguments))
  newParams.append(contentsOf: nonConstantArguments.map { partialApply.parameter(for: $0)! })

  let isGeneric = newParams.contains { $0.type.hasTypeParameter } ||
                  callee.convention.results.contains { $0.type.hasTypeParameter() } ||
                  callee.convention.errorResult?.type.hasTypeParameter() ?? false

  let specializedClosure = context.createSpecializedFunctionDeclaration(from: callee,
                                                                        withName: specializedName,
                                                                        withParams: newParams,
                                                                        preserveGenericSignature: isGeneric)

  context.buildSpecializedFunction(specializedFunction: specializedClosure) { (specializedClosure, specContext) in
    cloneAndSpecializeFunction(from: callee, toEmpty: specializedClosure,
                               substitutions: partialApply.substitutionMap,
                               specContext)

    let entryBlock = specializedClosure.entryBlock
    for constArgOp in constantArguments {
      cloneArgument(constArgOp, of: partialApply, to: specializedClosure, specContext)
    }
    // Erase the cloned arguments from the entry block.
    for constArgOp in constantArguments.reversed() {
      let calleeArgIdx = partialApply.calleeArgumentIndex(of: constArgOp)!
      entryBlock.eraseArgument(at: calleeArgIdx, specContext)
    }
  }
  context.notifyNewFunction(function: specializedClosure, derivedFrom: callee)
  return specializedClosure
}

private func cloneArgument(_ argumentOp: Operand,
                           of partialApply: PartialApplyInst,
                           to targetFunction: Function,
                           _ context: FunctionPassContext
) {
  var argCloner = Cloner(cloneBefore: targetFunction.entryBlock.instructions.first!, context)
  defer { argCloner.deinitialize() }

  let clonedArg = argCloner.cloneRecursively(value: argumentOp.value)
  let calleeArgIdx = partialApply.calleeArgumentIndex(of: argumentOp)!
  let calleeArg = targetFunction.arguments[calleeArgIdx]
  calleeArg.uses.replaceAll(with: clonedArg, context)

  if partialApply.calleeArgumentConventions[calleeArgIdx].isGuaranteed {
    // If the original argument was passed as guaranteed, i.e. is _not_ destroyed in the closure, we have
    // to destroy the cloned argument at function exits.
    for block in targetFunction.blocks where block.terminator.isFunctionExiting {
      let builder = Builder(before: block.terminator, context)
      builder.emitDestroy(of: clonedArg)
    }
  }

}

private func addCompensatingDestroys(for constantArguments: [Operand], _ context: FunctionPassContext) {
  for argOp in constantArguments {
    let builder = Builder(before: argOp.instruction, context)
    builder.emitDestroy(of: argOp.value)
  }
}

private func rewritePartialApply(_ partialApply: PartialApplyInst, withSpecialized specialized: Function,
                                 arguments: [Value], _ context: FunctionPassContext) {
  let builder = Builder(before: partialApply, context)
  let fri = builder.createFunctionRef(specialized)
  let newClosure: Value
  if arguments.isEmpty {
    newClosure = builder.createThinToThickFunction(thinFunction: fri, resultType: partialApply.type)
    context.erase(instructions: partialApply.uses.users(ofType: DeallocStackInst.self))
  } else {
    newClosure = builder.createPartialApply(
      function: fri,
      substitutionMap: specialized.genericSignature.isEmpty ? SubstitutionMap() : partialApply.substitutionMap,
      capturedArguments: arguments, calleeConvention: partialApply.calleeConvention,
      hasUnknownResultIsolation: partialApply.hasUnknownResultIsolation, isOnStack: partialApply.isOnStack)
  }
  partialApply.uses.replaceAll(with: newClosure, context)

  // Bypass any mark_dependence on the captures we specialized away.
  //
  // TODO: If we start to specialize away key path literals with operands (subscripts etc.), then a
  // dependence of the new partial_apply on those operands may still exist. However, we should still
  // leave the key path itself out of the dependency chain, and introduce dependencies on those
  // operands instead, so that the key path object itself can be made dead.
  for md in newClosure.uses.users(ofType: MarkDependenceInst.self) {
    if md.base.uses.getSingleUser(ofType: PartialApplyInst.self) == partialApply {
      md.replace(with: newClosure, context)
    }
  }
  context.erase(instruction: partialApply)
}

private extension PartialApplyInst {

  /// Returns the callee if this is a `partial_apply` of a thunk which directly forwards all arguments
  /// to the callee and has no other side-effects.
  func getCalleeOfForwardingThunkWithDeadCaptures() -> Function? {
    guard let thunk = referencedFunction,
          let thunkEntryBlock = thunk.blocks.first
    else {
      return nil
    }
    let numDeadArguments = self.arguments.count
    let numAliveArgs = thunkEntryBlock.arguments.count - numDeadArguments
    let deadCalleeArgs = thunkEntryBlock.arguments.dropFirst(numAliveArgs)

    // TODO: handle non-trivial dead arguments, i.e. accept destroy instructions of such arguments.
    guard deadCalleeArgs.allSatisfy({ $0.type.isTrivial(in: thunk )}) else {
      return nil
    }

    var callee: Function? = nil
    var returnValue: Value? = nil
    var errorValue: Value? = nil

    for inst in thunk.instructions {
      switch inst {
      case let apply as ApplyInst:
        guard callee == nil, let c = apply.getCalleeWithForwardedArguments(numArguments: numAliveArgs) else {
          return nil
        }
        callee = c
        returnValue = apply
      case let tryApply as TryApplyInst:
        guard callee == nil,
              let c = tryApply.getCalleeWithForwardedArguments(numArguments: numAliveArgs)
        else {
          return nil
        }
        callee = c
        returnValue = tryApply.normalBlock.arguments.first
        errorValue = tryApply.errorBlock.arguments.first
      case let returnInst as ReturnInst:
        guard let returnValue, returnInst.returnedValue == returnValue else {
          return nil
        }
      case let throwInst as ThrowInst:
        guard let errorValue, throwInst.thrownValue == errorValue else {
          return nil
        }
      case is TermInst:
        return nil
      default:
        if inst.mayHaveSideEffects {
          return nil
        }
      }
    }
    return callee
  }

  func classifyArgumentsForConstness() -> (constArguments: [Operand], nonConstArguments: [Operand])? {
    var constArgs = [Operand]()
    var nonConstArgs = [Operand]()
    var hasKeypath = false
    for argOp in argumentOperands {
      // In non-OSSA we don't know where to insert the compensating release for a propagated keypath.
      // Therefore bail if a keypath has multiple uses.
      switch argOp.value.isConstant(requireSingleUse: !parentFunction.hasOwnership && !isOnStack) {
      case .constant:
        constArgs.append(argOp)
      case .constantWithKeypath:
        constArgs.append(argOp)
        hasKeypath = true
      case .notConstant:
        nonConstArgs.append(argOp)
      }
    }

    // The optimization is beneficial if we can either get rid of all captures, because this
    // avoids allocating the context.
    // Or if we can constant propagate at least one keypath. Keypaths are so expensive that constant
    // propagating a single keypath is already beneficial.
    if !constArgs.isEmpty,
       nonConstArgs.isEmpty || hasKeypath
    {
      return (constArgs, nonConstArgs)
    }
    return nil
  }
}

private extension FullApplySite {
  func getCalleeWithForwardedArguments(numArguments: Int) -> Function? {
    if let callee = referencedFunction,
       callee.numArguments == numArguments,
       zip(parentFunction.entryBlock.arguments, arguments).allSatisfy({ $0.0 == $0.1 })
    {
      return callee
    }
    return nil
  }
}

private enum ConstantKind {
  case notConstant
  case constant
  case constantWithKeypath

  func merge(with other: ConstantKind) -> ConstantKind {
    switch (self, other) {
      case (.notConstant, _):      return .notConstant
      case (_, .notConstant):      return .notConstant
      case (.constant, .constant): return .constant
      default:                     return .constantWithKeypath
    }
  }
}

private extension Value {
  func isConstant(requireSingleUse: Bool) -> ConstantKind {
    // All instructions handled here must also be handled in
    // `FunctionSignatureSpecializationMangler::mangleConstantProp`.
    let result: ConstantKind
    switch self {
    case let si as StructInst:
      result = si.operands.reduce(.constant, {
        $0.merge(with: $1.value.isConstant(requireSingleUse: requireSingleUse))
      })
    case is ThinToThickFunctionInst, is ConvertFunctionInst, is UpcastInst, is OpenExistentialRefInst:
      result = (self as! UnaryInstruction).operand.value.isConstant(requireSingleUse: requireSingleUse)
    case is StringLiteralInst, is IntegerLiteralInst, is FloatLiteralInst, is FunctionRefInst, is GlobalAddrInst:
      result = .constant
    case let keyPath as KeyPathInst:
      guard keyPath.operands.isEmpty,
            keyPath.hasPattern,
            !keyPath.substitutionMap.hasAnySubstitutableParams
      else {
        return .notConstant
      }
      result = .constantWithKeypath
    default:
      return .notConstant
    }
    if requireSingleUse, result == .constantWithKeypath, !uses.ignoreDebugUses.isSingleUse {
      return .notConstant
    }
    return result
  }
}
