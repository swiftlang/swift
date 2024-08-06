//===--- SimplifyBuiltin.swift --------------------------------------------===//
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

extension BuiltinInst : OnoneSimplifyable {
  func simplify(_ context: SimplifyContext) {
    switch id {
      case .IsConcrete:
        // Don't constant fold a Builtin.isConcrete of a type with archetypes in the middle
        // of the pipeline, because a generic specializer might run afterwards which turns the
        // type into a concrete type.
        optimizeIsConcrete(allowArchetypes: false, context)
      case .IsSameMetatype:
        optimizeIsSameMetatype(context)
      case .Once:
        optimizeBuiltinOnce(context)
      case .CanBeObjCClass:
        optimizeCanBeClass(context)
      case .AssertConf:
        optimizeAssertConfig(context)
      case .Sizeof,
           .Strideof,
           .Alignof:
        optimizeTargetTypeConst(context)
      case .DestroyArray:
        if let elementType = substitutionMap.replacementTypes[0],
           elementType.isTrivial(in: parentFunction)
        {
          context.erase(instruction: self)
          return
        }
        optimizeArgumentToThinMetatype(argument: 0, context)
      case .CopyArray,
           .TakeArrayNoAlias,
           .TakeArrayFrontToBack,
           .TakeArrayBackToFront,
           .AssignCopyArrayNoAlias,
           .AssignCopyArrayFrontToBack,
           .AssignCopyArrayBackToFront,
           .AssignTakeArray,
           .AllocVector,
           .IsPOD:
        optimizeArgumentToThinMetatype(argument: 0, context)
      case .ICMP_EQ:
        constantFoldIntegerEquality(isEqual: true, context)
      case .ICMP_NE:
        constantFoldIntegerEquality(isEqual: false, context)
      default:
        if let literal = constantFold(context) {
          uses.replaceAll(with: literal, context)
        }
    }
  }
}

extension BuiltinInst : LateOnoneSimplifyable {
  func simplifyLate(_ context: SimplifyContext) {
    if id == .IsConcrete {
      // At the end of the pipeline we can be sure that the isConcrete's type doesn't get "more" concrete.
      optimizeIsConcrete(allowArchetypes: true, context)
    } else {
      simplify(context)
    }
  }
}

private extension BuiltinInst {
  func optimizeIsConcrete(allowArchetypes: Bool, _ context: SimplifyContext) {
    let hasArchetype = operands[0].value.type.hasArchetype
    if hasArchetype && !allowArchetypes {
      return
    }
    let builder = Builder(before: self, context)
    let result = builder.createIntegerLiteral(hasArchetype ? 0 : 1, type: type)
    uses.replaceAll(with: result, context)
    context.erase(instruction: self)
  }

  func optimizeIsSameMetatype(_ context: SimplifyContext) {
    let lhs = operands[0].value
    let rhs = operands[1].value

    guard let equal = typesOfValuesAreEqual(lhs, rhs, in: parentFunction) else {
      return
    }
    let builder = Builder(before: self, context)
    let result = builder.createIntegerLiteral(equal ? 1 : 0, type: type)

    uses.replaceAll(with: result, context)
  }

  func optimizeBuiltinOnce(_ context: SimplifyContext) {
    guard let callee = calleeOfOnce, callee.isDefinition else {
      return
    }
    context.notifyDependency(onBodyOf: callee)

    // If the callee is side effect-free we can remove the whole builtin "once".
    // We don't use the callee's memory effects but instead look at all callee instructions
    // because memory effects are not computed in the Onone pipeline, yet.
    // This is no problem because the callee (usually a global init function )is mostly very small,
    // or contains the side-effect instruction `alloc_global` right at the beginning.
    if callee.instructions.contains(where: hasSideEffectForBuiltinOnce) {
      return
    }
    for use in uses {
      let ga = use.instruction as! GlobalAddrInst
      ga.clearToken(context)
    }
    context.erase(instruction: self)
  }

  var calleeOfOnce: Function? {
    let callee = operands[1].value
    if let fri = callee as? FunctionRefInst {
      return fri.referencedFunction
    }
    return nil
  }

  func optimizeCanBeClass(_ context: SimplifyContext) {
    guard let ty = substitutionMap.replacementTypes[0] else {
      return
    }
    let literal: IntegerLiteralInst
    switch ty.canBeClass {
    case .IsNot:
      let builder = Builder(before: self, context)
      literal = builder.createIntegerLiteral(0,  type: type)
    case .Is:
      let builder = Builder(before: self, context)
      literal = builder.createIntegerLiteral(1,  type: type)
    case .CanBe:
      return
    default:
      fatalError()
    }
    uses.replaceAll(with: literal, context)
    context.erase(instruction: self)
  }

  func optimizeAssertConfig(_ context: SimplifyContext) {
    let literal: IntegerLiteralInst
    switch context.options.assertConfiguration {
    case .enabled:
      let builder = Builder(before: self, context)
      literal = builder.createIntegerLiteral(1,  type: type)
    case .disabled:
      let builder = Builder(before: self, context)
      literal = builder.createIntegerLiteral(0,  type: type)
    default:
      return
    }
    uses.replaceAll(with: literal, context)
    context.erase(instruction: self)
  }
  
  func optimizeTargetTypeConst(_ context: SimplifyContext) {
    guard let ty = substitutionMap.replacementTypes[0] else {
      return
    }
    
    let value: Int?
    switch id {
    case .Sizeof:
      value = ty.getStaticSize(context: context)
    case .Strideof:
      value = ty.getStaticStride(context: context)
    case .Alignof:
      value = ty.getStaticAlignment(context: context)
    default:
      fatalError()
    }
    
    guard let value else {
      return
    }
    
    let builder = Builder(before: self, context)
    let literal = builder.createIntegerLiteral(value, type: type)
    uses.replaceAll(with: literal, context)
    context.erase(instruction: self)
  }
  
  func optimizeArgumentToThinMetatype(argument: Int, _ context: SimplifyContext) {
    let type: Type

    if let metatypeInst = operands[argument].value as? MetatypeInst {
      type = metatypeInst.type
    } else if let initExistentialInst = operands[argument].value as? InitExistentialMetatypeInst {
      type = initExistentialInst.metatype.type
    } else {
      return
    }

    guard type.representationOfMetatype(in: parentFunction) == .Thick else {
      return
    }
    
    let instanceType = type.loweredInstanceTypeOfMetatype(in: parentFunction)
    let builder = Builder(before: self, context)
    let newMetatype = builder.createMetatype(of: instanceType, representation: .Thin)
    operands[argument].set(to: newMetatype, context)
  }

  func constantFoldIntegerEquality(isEqual: Bool, _ context: SimplifyContext) {
    if constantFoldStringNullPointerCheck(isEqual: isEqual, context) {
      return
    }
    if let literal = constantFold(context) {
      uses.replaceAll(with: literal, context)
    }
  }

  func constantFoldStringNullPointerCheck(isEqual: Bool, _ context: SimplifyContext) -> Bool {
    if operands[1].value.isZeroInteger &&
       operands[0].value.lookThroughScalarCasts is StringLiteralInst
    {
      let builder = Builder(before: self, context)
      let result = builder.createIntegerLiteral(isEqual ? 0 : 1, type: type)
      uses.replaceAll(with: result, context)
      context.erase(instruction: self)
      return true
    }
    return false
  }
}

private extension Value {
  var isZeroInteger: Bool {
    if let literal = self as? IntegerLiteralInst,
       let value = literal.value
    {
      return value == 0
    }
    return false
  }

  var lookThroughScalarCasts: Value {
    guard let bi = self as? BuiltinInst else {
      return self
    }
    switch bi.id {
    case .ZExt, .ZExtOrBitCast, .PtrToInt:
      return bi.operands[0].value.lookThroughScalarCasts
    default:
      return self
    }
  }
}

private func hasSideEffectForBuiltinOnce(_ instruction: Instruction) -> Bool {
  switch instruction {
  case is DebugStepInst, is DebugValueInst:
    return false
  default:
    return instruction.mayReadOrWriteMemory ||
           instruction.hasUnspecifiedSideEffects
  }
}

private func typesOfValuesAreEqual(_ lhs: Value, _ rhs: Value, in function: Function) -> Bool? {
  if lhs == rhs {
    return true
  }

  guard let lhsExistential = lhs as? InitExistentialMetatypeInst,
        let rhsExistential = rhs as? InitExistentialMetatypeInst else {
    return nil
  }

  let lhsMetatype = lhsExistential.metatype.type
  let rhsMetatype = rhsExistential.metatype.type
  if lhsMetatype.isDynamicSelfMetatype != rhsMetatype.isDynamicSelfMetatype {
    return nil
  }
  let lhsTy = lhsMetatype.loweredInstanceTypeOfMetatype(in: function)
  let rhsTy = rhsMetatype.loweredInstanceTypeOfMetatype(in: function)

  // Do we know the exact types? This is not the case e.g. if a type is passed as metatype
  // to the function.
  let typesAreExact = lhsExistential.metatype is MetatypeInst &&
                      rhsExistential.metatype is MetatypeInst

  if typesAreExact {
    // We need to compare the not lowered types, because function types may differ in their original version
    // but are equal in the lowered version, e.g.
    //   ((Int, Int) -> ())
    //   (((Int, Int)) -> ())
    //
    if lhsMetatype == rhsMetatype {
      return true
    }
    // Comparing types of different classes which are in a sub-class relation is not handled by the
    // cast optimizer (below).
    if lhsTy.isClass && rhsTy.isClass && lhsTy.nominal != rhsTy.nominal {
      return false
    }
  }

  // If casting in either direction doesn't work, the types cannot be equal.
  if !(canDynamicallyCast(from: lhsTy, to: rhsTy, in: function, sourceTypeIsExact: typesAreExact) ?? true) ||
     !(canDynamicallyCast(from: rhsTy, to: lhsTy, in: function, sourceTypeIsExact: typesAreExact) ?? true) {
    return false
  }
  return nil
}
