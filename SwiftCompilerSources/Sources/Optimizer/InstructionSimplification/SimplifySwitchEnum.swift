//===--- SimplifySwitchEnum.swift -----------------------------------------===//
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

import AST
import SIL

extension SwitchEnumInst : OnoneSimplifiable {
  func simplify(_ context: SimplifyContext) {
    // First try to fold the `switch_enum` with a statically known `enum` operand into a branch.
    // Only if that doesn't apply, try to fold an optional pointer comparison.
    if simplifyToBranch(context) {
      return
    }
    _ = simplifyOptionalPointerComparison(context)
  }
}

private extension SwitchEnumInst {

  // Removes an `enum` - `switch_enum` pair:
  // ```
  //     %1 = enum $E, #someCase, %payload
  //     switch_enum %1, case #someCase: bb1, ...
  //   bb1(%payloadArgument):
  // ```
  // ->
  // ```
  //   br bb1(%payload)
  //   bb1(%payloadArgument):
  // ```
  //
  // Other case blocks of the switch_enum become dead.
  //
  func simplifyToBranch(_ context: SimplifyContext) -> Bool {
    guard let enumInst = enumOp as? EnumInst,
          let caseBlock = getUniqueSuccessor(forCaseIndex: enumInst.caseIndex) else
    {
      return false
    }

    let singleUse = context.preserveDebugInfo ? enumInst.uses.singleUse : enumInst.uses.ignoreDebugUses.singleUse
    let canEraseEnumInst = singleUse?.instruction == self

    if !canEraseEnumInst && parentFunction.hasOwnership && enumInst.ownership == .owned {
      // We cannot add more uses to the `enum` instruction without inserting a copy.
      return false
    }

    let builder = Builder(before: self, context)
    switch caseBlock.arguments.count {
    case 0:
      precondition(enumInst.payload == nil || !parentFunction.hasOwnership,
                   "missing payload argument in switch_enum case block")
      builder.createBranch(to: caseBlock)
      context.erase(instruction: self)
    case 1:
      builder.createBranch(to: caseBlock, arguments: [enumInst.payload!])
      context.erase(instruction: self)
      updateBorrowedFrom(for: [Phi(caseBlock.arguments[0])!], context)
    default:
      fatalError("case block of switch_enum cannot have more than 1 argument")
    }

    if canEraseEnumInst {
      context.erase(instruction: enumInst)
    }
    return true
  }

  // Replaces a comparison of a non-optional class reference or raw pointer with an optional one,
  // which is lowered to a `switch_enum` plus a pointer comparison in the payload block, with a
  // single pointer comparison:
  // ```
  //     switch_enum %optional, case #some: bb1, case #none: bb2
  //   bb1(%payload):
  //     %pp = ref_to_raw_pointer %payload
  //     %ap = ref_to_raw_pointer %a
  //     %c = builtin "cmp_eq_RawPointer"(%ap, %pp)
  //     br bb3(%c)
  //   bb2:
  //     %f = integer_literal $Builtin.Int1, 0
  //     br bb3(%f)
  // ```
  // ->
  // ```
  //     %ap = ref_to_raw_pointer %a
  //     %or = unchecked_ref_cast %optional to $SomeClass
  //     %op = ref_to_raw_pointer %or
  //     %c = builtin "cmp_eq_RawPointer"(%ap, %op)
  //     br bb3(%c)
  // ```
  func simplifyOptionalPointerComparison(_ context: SimplifyContext) -> Bool {
    guard enumOp.type.isOptional, enumOp.ownership != .owned else {
      return false
    }

    guard let someBlock = getUniqueSuccessor(forCaseIndex: EnumDecl.optionalSomeCaseIndex),
          let noneBlock = getUniqueSuccessor(forCaseIndex: EnumDecl.optionalNoneCaseIndex),
          !someBlock.hasSideEffects, !noneBlock.hasSideEffects,
          let someBranch = someBlock.terminator as? BranchInst,
          let noneBranch = noneBlock.terminator as? BranchInst,
          someBranch.targetBlock == noneBranch.targetBlock,
          someBranch.operands.count == 1, noneBranch.operands.count == 1,
          let falseLiteral = noneBranch.operands[0].value as? IntegerLiteralInst,
          falseLiteral.value == 0,
          let cmp = someBranch.operands[0].value as? BuiltinInst,
          cmp.name == "cmp_eq_RawPointer", cmp.arguments.count == 2,
          let (lhs, rhs) = cmp.comparedUnderlyingPointers
    else {
      return false
    }

    let payload = someBlock.arguments[0]
    let nonOptional: Value
    if lhs == payload {
      nonOptional = rhs
    } else if rhs == payload {
      nonOptional = lhs
    } else {
      return false
    }
    guard nonOptional.type == payload.type, nonOptional.parentBlock != someBlock else {
      return false
    }

    let builder = Builder(before: self, context)
    let rawPointerType = cmp.arguments[0].type
    let nonOptionalPointer: Value
    let optionalPointer: Value
    if payload.type.isHeapObjectReferenceType {
      guard nonOptional is FunctionArgument else {
        return false
      }
      nonOptionalPointer = builder.createRefToRawPointer(from: nonOptional, to: rawPointerType)
      let optionalRef = builder.createUncheckedRefCast(from: enumOp, to: payload.type)
      optionalPointer = builder.createRefToRawPointer(from: optionalRef, to: rawPointerType)
    } else {
      nonOptionalPointer = builder.createUncheckedTrivialBitCast(from: nonOptional, to: rawPointerType)
      optionalPointer = builder.createUncheckedTrivialBitCast(from: enumOp, to: rawPointerType)
    }
    let result = builder.createBuiltinBinaryFunction(name: "cmp_eq", operandType: rawPointerType,
                                                     resultType: cmp.type,
                                                     arguments: [nonOptionalPointer, optionalPointer])
    builder.createBranch(to: someBranch.targetBlock, arguments: [result])
    context.erase(instruction: self)
    return true
  }
}

private extension BasicBlock {
  var hasSideEffects: Bool {
    instructions.contains { $0.mayHaveSideEffects || $0.hasUnspecifiedSideEffects }
  }
}

private extension BuiltinInst {
  var comparedUnderlyingPointers: (Value, Value)? {
    switch (arguments[0], arguments[1]) {
    case let (lhs as RefToRawPointerInst, rhs as RefToRawPointerInst):
      guard lhs.parentBlock == parentBlock, rhs.parentBlock == parentBlock else {
        return nil
      }
      return (lhs.operand.value, rhs.operand.value)
    case let (lhs as StructExtractInst, rhs as StructExtractInst):
      guard lhs.struct.type == rhs.struct.type, lhs.fieldIndex == rhs.fieldIndex,
            lhs.struct.type.isTrivial(in: parentFunction),
            lhs.struct.type.getNominalFields(in: parentFunction)?.count == 1,
            lhs.parentBlock == parentBlock, rhs.parentBlock == parentBlock else {
        return nil
      }
      return (lhs.struct, rhs.struct)
    default:
      return nil
    }
  }
}
