//===--- SimplifyApply.swift ----------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2023 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import SIL
import AST

extension ApplyInst : OnoneSimplifiable, SILCombineSimplifiable {
  func simplify(_ context: SimplifyContext) {
    if tryTransformThickToThinCallee(of: self, context) {
      return
    }
    if context.tryOptimizeKeypath(apply: self) {
      context.erase(instruction: self)
      return
    }
    if context.tryDevirtualize(apply: self, isMandatory: false) != nil {
      return
    }
    if !context.preserveDebugInfo {
      _ = tryReplaceExistentialArchetype(of: self, context)
    }
  }
}

extension TryApplyInst : OnoneSimplifiable, SILCombineSimplifiable {
  func simplify(_ context: SimplifyContext) {
    if context.tryDevirtualize(apply: self, isMandatory: false) != nil {
      return
    }
    if !context.preserveDebugInfo {
      _ = tryReplaceExistentialArchetype(of: self, context)
    }
  }
}

extension BeginApplyInst : OnoneSimplifiable {
  func simplify(_ context: SimplifyContext) {
    _ = context.tryDevirtualize(apply: self, isMandatory: false)
  }
}

/// Optimizes a thick function call if the callee is a `thin_to_thick_function` instruction:
///
///   %2 = thin_to_thick_function %1
///   %3 = apply %2(...) : @callee_guaranteed
/// ->
///   %2 = thin_to_thick_function %1
///   %3 = apply %1(...): @convention(thin)
///
private func tryTransformThickToThinCallee(of apply: ApplyInst, _ context: SimplifyContext) -> Bool {
  if let tttf = apply.callee as? ThinToThickFunctionInst,
     !apply.callee.type.isCalleeConsumedFunction
  {
    let builder = Builder(before: apply, context)
    let newApply = builder.createApply(function: tttf.operand.value,
                                       apply.substitutionMap,
                                       arguments: Array(apply.arguments),
                                       isNonThrowing: apply.isNonThrowing,
                                       isNonAsync: apply.isNonAsync,
                                       specializationInfo: apply.specializationInfo)
    apply.replace(with: newApply, context)
    return true
  }
  return false
}

/// If the apply uses an existential archetype (`@opened("...")`) and the concrete type is known,
/// replace the existential archetype with the concrete type
///   1. in the apply's substitution map
///   2. in the arguments, e.g. by inserting address casts
/// For example:
/// ```
///   %5 = apply %1<@opend("...")>(%2) : <τ_0_0> (τ_0_0) -> ()
/// ```
/// ->
/// ```
///   %4 = unchecked_addr_cast %2 to $*ConcreteType
///   %5 = apply %1<ConcreteType>(%4) : <τ_0_0> (τ_0_0) -> ()
/// ```
private func tryReplaceExistentialArchetype(of apply: ApplyInst, _ context: SimplifyContext) -> Bool {
  if let concreteType = apply.concreteTypeOfDependentExistentialArchetype,
     apply.canReplaceExistentialArchetype()
  {
    let builder = Builder(after: apply, context)

    let newApply = builder.createApply(
      function: apply.callee,
      apply.replaceOpenedArchetypeInSubstituations(withConcreteType: concreteType, context),
      arguments: apply.replaceExistentialArchetypeInArguments(withConcreteType: concreteType, context),
      isNonThrowing: apply.isNonThrowing, isNonAsync: apply.isNonAsync,
      specializationInfo: apply.specializationInfo)
    apply.replace(with: newApply, context)

    return true
  }
  return false
}

// The same as the previous function, just for try_apply instructions.
private func tryReplaceExistentialArchetype(of tryApply: TryApplyInst, _ context: SimplifyContext) -> Bool {
  if let concreteType = tryApply.concreteTypeOfDependentExistentialArchetype,
     tryApply.canReplaceExistentialArchetype()
  {
    let builder = Builder(before: tryApply, context)

    builder.createTryApply(
      function: tryApply.callee,
      tryApply.replaceOpenedArchetypeInSubstituations(withConcreteType: concreteType, context),
      arguments: tryApply.replaceExistentialArchetypeInArguments(withConcreteType: concreteType, context),
      normalBlock: tryApply.normalBlock, errorBlock: tryApply.errorBlock,
      isNonAsync: tryApply.isNonAsync,
      specializationInfo: tryApply.specializationInfo)
    context.erase(instruction: tryApply)

    return true
  }
  return false
}

private extension FullApplySite {
  // Precondition: the apply uses only a single existential archetype.
  // This is checked in `concreteTypeOfDependentExistentialArchetype`
  func canReplaceExistentialArchetype() -> Bool {
    // Make sure that existential archetype _is_ a replacement type and not e.g. _contained_ in a
    // replacement type, like
    //    apply %1<Array<@opened("...")>()
    guard substitutionMap.replacementTypes.contains(where: { $0.isExistentialArchetype }),
          substitutionMap.replacementTypes.allSatisfy({ $0.isExistentialArchetype || !$0.hasLocalArchetype })
    else {
      return false
    }

    // Don't allow existential archetypes in direct results and error results.
    // Note that an opened existential value is address only, so it cannot be a direct result anyway
    // (but it can be once we have opaque values).
    // Also don't support things like direct `Array<@opened("...")>` return values.
    if let singleDirectResult, singleDirectResult.type.astType.hasLocalArchetype {
      return false
    }
    if let singleDirectErrorResult, singleDirectErrorResult.type.astType.hasLocalArchetype {
      return false
    }

    return arguments.allSatisfy { value in
      let astTy = value.type.astType
      // Allow three cases:
             // case 1. the argument _is_ the existential archetype
      return astTy.isExistentialArchetype ||
             // case 2. the argument _is_ a metatype of the existential archetype
             (astTy.isMetatypeType && astTy.instanceTypeOfMetatype.isExistentialArchetype) ||
             // case 3. the argument has nothing to do with the existential archetype (or any other local archetype)
             !astTy.hasLocalArchetype
    }
  }

  func replaceExistentialArchetypeInArguments(
    withConcreteType concreteType: CanonicalType,
    _ context: SimplifyContext
  ) -> [Value] {
    let newArgs = arguments.map { (arg) -> Value in
      let argTy = arg.type.astType
      if argTy.isExistentialArchetype {
        // case 1. the argument _is_ the existential archetype:
        //         just insert an address cast to satisfy type equivalence.
        let builder = Builder(before: self, context)
        let concreteSILType = concreteType.loweredType(in: self.parentFunction)
        return builder.createUncheckedAddrCast(from: arg, to: concreteSILType.addressType)
      }
      if argTy.isMetatypeType, argTy.instanceTypeOfMetatype.isExistentialArchetype {
        // case 2. the argument _is_ a metatype of the existential archetype:
        //         re-create the metatype with the concrete type.
        let builder = Builder(before: self, context)
        return builder.createMetatype(ofInstanceType: concreteType, representation: argTy.representationOfMetatype)
      }
      // case 3. the argument has nothing to do with the existential archetype (or any other local archetype)
      return arg
    }
    return Array(newArgs)
  }

  func replaceOpenedArchetypeInSubstituations(
    withConcreteType concreteType: CanonicalType,
    _ context: SimplifyContext
  ) -> SubstitutionMap {
    let openedArcheType = substitutionMap.replacementTypes.first(where: { $0.isExistentialArchetype })!

    let newReplacementTypes = substitutionMap.replacementTypes.map {
      return $0 == openedArcheType ? concreteType.type : $0
    }
    let genSig = callee.type.astType.invocationGenericSignatureOfFunctionType
    return SubstitutionMap(genericSignature: genSig, replacementTypes: newReplacementTypes)
  }
}
