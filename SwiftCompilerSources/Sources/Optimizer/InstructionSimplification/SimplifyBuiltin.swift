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

extension BuiltinInst : OnoneSimplifiable, SILCombineSimplifiable {
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
        let elementType = substitutionMap.replacementType.loweredType(in: parentFunction, maximallyAbstracted: true)
        if elementType.isTrivial(in: parentFunction) {
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

extension BuiltinInst : LateOnoneSimplifiable {
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
    let literal: IntegerLiteralInst
    switch substitutionMap.replacementType.canonical.canBeClass {
    case .isNot:
      let builder = Builder(before: self, context)
      literal = builder.createIntegerLiteral(0,  type: type)
    case .is:
      let builder = Builder(before: self, context)
      literal = builder.createIntegerLiteral(1,  type: type)
    case .canBe:
      return
    }
    self.replace(with: literal, context)
  }

  func optimizeAssertConfig(_ context: SimplifyContext) {
    // The values for the assert_configuration call are:
    // 0: Debug
    // 1: Release
    // 2: Fast / Unchecked
    let config = context.options.assertConfiguration
    switch config {
    case .debug, .release, .unchecked:
      let builder = Builder(before: self, context)
      let literal = builder.createIntegerLiteral(config.integerValue, type: type)
      uses.replaceAll(with: literal, context)
      context.erase(instruction: self)
    case .unknown:
      return
    }
  }
  
  func optimizeTargetTypeConst(_ context: SimplifyContext) {
    let ty = substitutionMap.replacementType.loweredType(in: parentFunction, maximallyAbstracted: true)
    let value: Int?
    switch id {
    case .Sizeof:
      value = ty.getStaticSize(context: context)
    case .Strideof:
      if isUsedAsStrideOfIndexRawPointer(context) {
        // Constant folding `stride` would prevent index_raw_pointer simplification.
        // See `simplifyIndexRawPointer` in SimplifyPointerToAddress.swift.
        return
      }
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

  private func isUsedAsStrideOfIndexRawPointer(_ context: SimplifyContext) -> Bool {
    var worklist = ValueWorklist(context)
    defer { worklist.deinitialize() }
    worklist.pushIfNotVisited(self)
    while let v = worklist.pop() {
      for use in v.uses {
        switch use.instruction {
        case let builtin as BuiltinInst:
          switch builtin.id {
          case .SMulOver, .TruncOrBitCast, .SExtOrBitCast, .ZExtOrBitCast:
            worklist.pushIfNotVisited(builtin)
          default:
            break
          }
        case let tupleExtract as TupleExtractInst where tupleExtract.fieldIndex == 0:
          worklist.pushIfNotVisited(tupleExtract)
        case is IndexRawPointerInst:
          return true
        default:
          break
        }
      }
    }
    return false
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

    guard type.representationOfMetatype == .thick else {
      return
    }
    
    let builder = Builder(before: self, context)
    let newMetatype = builder.createMetatype(ofInstanceType: type.canonicalType.instanceTypeOfMetatype,
                                             representation: .thin)
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

  let lhsTy = lhsExistential.metatype.type.canonicalType.instanceTypeOfMetatype
  let rhsTy = rhsExistential.metatype.type.canonicalType.instanceTypeOfMetatype
  if lhsTy.isDynamicSelf != rhsTy.isDynamicSelf {
    return nil
  }

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
    if lhsTy == rhsTy {
      return true
    }
    // Comparing types of different classes which are in a sub-class relation is not handled by the
    // cast optimizer (below).
    if lhsTy.isClass && rhsTy.isClass && lhsTy.nominal != rhsTy.nominal {
      return false
    }

    // Failing function casts are not supported by the cast optimizer (below).
    // (Reason: "Be conservative about function type relationships we may add in the future.")
    if lhsTy.isFunction && rhsTy.isFunction && lhsTy != rhsTy && !lhsTy.hasArchetype && !rhsTy.hasArchetype {
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
