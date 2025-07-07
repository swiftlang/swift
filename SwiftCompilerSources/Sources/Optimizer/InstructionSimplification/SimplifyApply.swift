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
    if tryRemoveArrayCast(apply: self, context) {
      return
    }
    if tryOptimizeEnumComparison(apply: self, context) {
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

/// Removes casts between arrays of the same type.
///
///   %1 = function_ref @_arrayConditionalCast : (@guaranteed Array<Int>) -> @owned Optional<Array<Int>>
///   %2 = apply %1(%0) : (@guaranteed Array<Int>) -> @owned Optional<Array<Int>>
/// ->
///   %1 = copy_value %0
///   %2 = enum $Optional<Array<Int>>, #Optional.some!enumelt, %1
///
private func tryRemoveArrayCast(apply: ApplyInst, _ context: SimplifyContext) -> Bool {
  guard let callee = apply.referencedFunction,
        callee.hasSemanticsAttribute("array.conditional_cast"),
        apply.parentFunction.hasOwnership,

          // Check if the cast function has the expected calling convention
        apply.arguments.count == 1,
        apply.convention(of: apply.argumentOperands[0]) == .directGuaranteed,
        apply.functionConvention.results[0].convention == .owned,
        apply.type.isOptional,

        // Check if the source and target type of the cast is identical.
        // Note that we are checking the _formal_ element types and not the lowered types, because
        // the element types are replacement type in the Array's substitution map and this is a formal type.
        apply.arguments[0].type == apply.type.optionalPayloadType(in: apply.parentFunction)
  else {
    return false
  }

  let builder = Builder(after: apply, context)
  let copiedArray = builder.createCopyValue(operand: apply.arguments[0])
  let optional = builder.createEnum(caseIndex: 1, payload: copiedArray, enumType: apply.type)
  apply.replace(with: optional, context)
  return true
}

/// Optimize (the very inefficient) RawRepresentable comparison to a simple compare of enum tags.
/// For example,
/// ```
///   enum E: String {
///     case  a, b, c
///   }
/// ```
/// is compared by getting the raw values of both operands and doing a string compare.
/// This peephole optimizations replaces the call to such a comparison function with a direct compare of
/// the enum tags, which boils down to a single integer comparison instruction.
///
private func tryOptimizeEnumComparison(apply: ApplyInst, _ context: SimplifyContext) -> Bool {
  guard let callee = apply.referencedFunction,
        apply.arguments.count == 2,
        callee.hasSemanticsAttribute("rawrepresentable.is_equal"),
        apply.type.isStruct
  else {
    return false
  }
  let lhs = apply.arguments[0]
  let rhs = apply.arguments[1]
  guard let enumDecl = lhs.type.nominal as? EnumDecl,
        enumDecl.hasRawType,
        !enumDecl.isResilient(in: apply.parentFunction),
        !enumDecl.hasClangNode,
        lhs.type.isAddress,
        lhs.type == rhs.type
  else {
    return false
  }
  let builder = Builder(before: apply, context)
  let tagType = context.getBuiltinIntegerType(bitWidth: 32)
  let lhsTag = builder.createBuiltin(name: "getEnumTag", type: tagType,
                                     substitutions: apply.substitutionMap, arguments: [lhs])
  let rhsTag = builder.createBuiltin(name: "getEnumTag", type: tagType,
                                     substitutions: apply.substitutionMap, arguments: [rhs])
  let builtinBoolType = context.getBuiltinIntegerType(bitWidth: 1)
  let cmp = builder.createBuiltin(name: "cmp_eq_Int32", type: builtinBoolType, arguments: [lhsTag, rhsTag])
  let booleanResult = builder.createStruct(type: apply.type, elements: [cmp])
  apply.replace(with: booleanResult, context)
  return true
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
      apply.replaceOpenedArchetypeInSubstitutions(withConcreteType: concreteType, context),
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
      tryApply.replaceOpenedArchetypeInSubstitutions(withConcreteType: concreteType, context),
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
    if let singleDirectResult, singleDirectResult.type.hasLocalArchetype {
      return false
    }
    if let singleDirectErrorResult, singleDirectErrorResult.type.hasLocalArchetype {
      return false
    }

    return arguments.allSatisfy { value in
      let type = value.type
      // Allow three cases:
             // case 1. the argument _is_ the existential archetype
      return type.isExistentialArchetype ||
             // case 2. the argument _is_ a metatype of the existential archetype
             (type.isMetatype && type.canonicalType.instanceTypeOfMetatype.isExistentialArchetype) ||
             // case 3. the argument has nothing to do with the existential archetype (or any other local archetype)
             !type.hasLocalArchetype
    }
  }

  func replaceExistentialArchetypeInArguments(
    withConcreteType concreteType: CanonicalType,
    _ context: SimplifyContext
  ) -> [Value] {
    let newArgs = arguments.map { (arg) -> Value in
      if arg.type.isExistentialArchetype {
        // case 1. the argument _is_ the existential archetype:
        //         just insert an address cast to satisfy type equivalence.
        let builder = Builder(before: self, context)
        let concreteSILType = concreteType.loweredType(in: self.parentFunction)
        return builder.createUncheckedAddrCast(from: arg, to: concreteSILType.addressType)
      }
      if arg.type.isMetatype, arg.type.canonicalType.instanceTypeOfMetatype.isExistentialArchetype {
        // case 2. the argument _is_ a metatype of the existential archetype:
        //         re-create the metatype with the concrete type.
        let builder = Builder(before: self, context)
        return builder.createMetatype(ofInstanceType: concreteType, representation: arg.type.representationOfMetatype)
      }
      // case 3. the argument has nothing to do with the existential archetype (or any other local archetype)
      return arg
    }
    return Array(newArgs)
  }

  func replaceOpenedArchetypeInSubstitutions(
    withConcreteType concreteType: CanonicalType,
    _ context: SimplifyContext
  ) -> SubstitutionMap {
    let openedArcheType = substitutionMap.replacementTypes.first(where: { $0.isExistentialArchetype })!

    let newReplacementTypes = substitutionMap.replacementTypes.map {
      return $0 == openedArcheType ? concreteType.rawType : $0
    }
    let genSig = callee.type.invocationGenericSignatureOfFunction
    return SubstitutionMap(genericSignature: genSig, replacementTypes: newReplacementTypes)
  }
}

private extension Type {
  func optionalPayloadType(in function: Function) -> Type {
    let subs = contextSubstitutionMap
    return subs.replacementTypes[0].loweredType(in: function)
  }
}
