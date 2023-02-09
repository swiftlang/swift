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
      default:
        // TODO: handle other builtin types
        break
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

    guard let equal = typesOfValuesAreEqual(lhs, rhs) else {
      return
    }
    let builder = Builder(before: self, context)
    let result = builder.createIntegerLiteral(equal ? 1 : 0, type: type)

    uses.replaceAll(with: result, context)
  }
}

private func typesOfValuesAreEqual(_ lhs: Value, _ rhs: Value) -> Bool? {
  if lhs == rhs {
    return true
  }

  guard let lhsExistential = lhs as? InitExistentialMetatypeInst,
        let rhsExistential = rhs as? InitExistentialMetatypeInst else {
    return nil
  }

  let lhsTy = lhsExistential.operand.type.instanceTypeOfMetatype
  let rhsTy = rhsExistential.operand.type.instanceTypeOfMetatype

  // Do we know the exact types? This is not the case e.g. if a type is passed as metatype
  // to the function.
  let typesAreExact = lhsExistential.operand is MetatypeInst &&
                      rhsExistential.operand is MetatypeInst

  switch (lhsTy.typeKind, rhsTy.typeKind) {
  case (_, .unknown), (.unknown, _):
    return nil
  case (let leftKind, let rightKind) where leftKind != rightKind:
    // E.g. a function type is always different than a struct, regardless of what archetypes
    // the two types may contain.
    return false
  case (.struct, .struct), (.enum, .enum):
    // Two different structs/enums are always not equal, regardless of what archetypes
    // the two types may contain.
    if lhsTy.nominal != rhsTy.nominal {
      return false
    }
  case (.class, .class):
    // In case of classes this only holds if we know the exact types.
    // Otherwise one class could be a sub-class of the other class.
    if typesAreExact && lhsTy.nominal != rhsTy.nominal {
      return false
    }
  default:
    break
  }

  if !typesAreExact {
    // Types which e.g. come from type parameters may differ at runtime while the declared AST types are the same.
    return nil
  }

  if lhsTy.hasArchetype || rhsTy.hasArchetype {
    // We don't know anything about archetypes. They may be identical at runtime or not.
    // We could do something more sophisticated here, e.g. look at conformances. But for simplicity,
    // we are just conservative.
    return nil
  }

  // Generic ObjectiveC class, which are specialized for different NSObject types have different AST types
  // but the same runtime metatype.
  if lhsTy.isOrContainsObjectiveCClass || rhsTy.isOrContainsObjectiveCClass {
    return nil
  }

  return lhsTy == rhsTy
}

private extension Type {
  enum TypeKind {
    case `struct`, `class`, `enum`, tuple, function, unknown
  }

  var typeKind: TypeKind {
    if isStruct  { return .struct }
    if isClass  { return .class }
    if isEnum  { return .enum }
    if isTuple    { return .tuple }
    if isFunction { return .function }
    return .unknown
  }
}
