//===--- ApplySite.swift - Defines the ApplySite protocols ----------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2021 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import SILBridging

/// Argument conventions indexed on an apply's operand.
///
/// `partial_apply` operands correspond to a suffix of the callee
/// arguments.
///
/// Example:
/// ```
/// func callee(a, b, c, d, e) { }
///
/// %pa = partial_apply @callee(c, d, e)
/// // operand indices:         1, 2, 3
/// // callee indices:          2, 3, 4
///
///  %a = apply         %pa    (a, b)
/// // operand indices:         1, 2
/// // callee indices:          0, 1
/// ```
public struct ApplyOperandConventions : Collection {
  public static let calleeIndex: Int = 0
  public static let firstArgumentIndex = 1

  /// Callee's argument conventions indexed on the function's arguments.
  public let calleeArgumentConventions: ArgumentConventions

  public let unappliedArgumentCount: Int

  public var appliedArgumentCount: Int {
    calleeArgumentConventions.count - unappliedArgumentCount
  }

  public func isCallee(operand: Operand) -> Bool {
    return operand.index == ApplyOperandConventions.calleeIndex
  }

  public var startIndex: Int { ApplyOperandConventions.firstArgumentIndex }

  public var endIndex: Int { ApplyOperandConventions.firstArgumentIndex + appliedArgumentCount }

  public func index(after index: Int) -> Int {
    return index + 1
  }

  public subscript(_ operandIndex: Int) -> ArgumentConvention {
    return calleeArgumentConventions[calleeArgumentIndex(ofOperandIndex: operandIndex)!]
  }

  public func convention(of operand: Operand) -> ArgumentConvention? {
    guard let argIdx = calleeArgumentIndex(of: operand) else {
      return nil
    }
    return calleeArgumentConventions[argIdx]
  }

  public func originalParameter(of operand: Operand) -> ParameterInfo? {
    guard let argIdx = calleeArgumentIndex(of: operand) else {
      return nil
    }
    guard argIdx >= calleeArgumentConventions.firstParameterIndex else {
      return nil
    }
    return calleeArgumentConventions.originalParameters[argIdx]
  }

  public var firstParameterOperandIndex: Int {
    return ApplyOperandConventions.firstArgumentIndex +
      calleeArgumentConventions.firstParameterIndex
  }

  // TODO: rewrite uses of this API to pass an Operand instead, and
  // make this private. No client should have multiple integer
  // indices, some of which are caller indices, and some of which are
  // callee indices.
  public func calleeArgumentIndex(ofOperandIndex index: Int) -> Int? {
    let callerArgIdx = index - ApplyOperandConventions.firstArgumentIndex
    guard callerArgIdx >= 0 else { return nil }

    let calleeArgIdx = callerArgIdx + unappliedArgumentCount
    guard calleeArgIdx < calleeArgumentConventions.count else { return nil }
    return calleeArgIdx
  }

  public func calleeArgumentIndex(of operand: Operand) -> Int? {
    calleeArgumentIndex(ofOperandIndex: operand.index)
  }
}

public protocol ApplySite : Instruction {
  var operands: OperandArray { get }
  var numArguments: Int { get }
  var substitutionMap: SubstitutionMap { get }

  var unappliedArgumentCount: Int { get }
}

extension ApplySite {
  public var callee: Value { operands[ApplyOperandConventions.calleeIndex].value }

  public var isAsync: Bool {
    return callee.type.isAsyncFunction
  }

  /// Returns the subset of operands which are argument operands.
  ///
  /// This does not include the callee function operand.
  public var argumentOperands: OperandArray {
    let numArgs = bridged.ApplySite_getNumArguments()
    let offset = ApplyOperandConventions.firstArgumentIndex
    return operands[offset..<(numArgs + offset)]
  }

  /// Returns the subset of operand values which are arguments.
  ///
  /// This does not include the callee function operand.
  public var arguments: LazyMapSequence<OperandArray, Value> {
    argumentOperands.values
  }

  /// Indirect results including the error result.
  public var indirectResultOperands: OperandArray {
    let offset = ApplyOperandConventions.firstArgumentIndex
    return operands[offset..<operandConventions.firstParameterOperandIndex]
  }

  public var substitutionMap: SubstitutionMap {
    SubstitutionMap(bridged.ApplySite_getSubstitutionMap())
  }

  public var calleeArgumentConventions: ArgumentConventions {
    ArgumentConventions(originalFunctionConvention: originalFunctionConvention,
                        substitutedFunctionConvention: substitutedFunctionConvention)
  }

  public var operandConventions: ApplyOperandConventions {
    ApplyOperandConventions(
      calleeArgumentConventions: calleeArgumentConventions,
      unappliedArgumentCount: bridged.PartialApply_getCalleeArgIndexOfFirstAppliedArg())
  }

  /// Returns true if `operand` is the callee function operand and not am argument operand.
  public func isCallee(operand: Operand) -> Bool {
    operandConventions.isCallee(operand: operand)
  }

  public func convention(of operand: Operand) -> ArgumentConvention? {
    operandConventions.convention(of: operand)
  }
  
  public var yieldConventions: YieldConventions {
    YieldConventions(originalFunctionConvention: originalFunctionConvention,
                     substitutedFunctionConvention: substitutedFunctionConvention)
  }

  public func convention(of yield: MultipleValueInstructionResult)
    -> ArgumentConvention {
    assert(yield.definingInstruction == self)
    return yieldConventions[yield.index]
  }

  /// Converts an argument index of a callee to the corresponding apply operand.
  ///
  /// If the apply does not actually apply that argument, it returns nil.
  ///
  /// Example:
  /// ```
  ///                 func callee(v, w, x, y, z) { }
  /// // caller operands in %pa:  -, -, c, d, e     ("-" == nil)
  /// // caller operands in %a:   a, b, -, -, -
  ///
  /// %pa = partial_apply @callee(c, d, e)
  ///  %a = apply         %pa    (a, b)
  /// ```
  public func operand(forCalleeArgumentIndex calleeArgIdx: Int) -> Operand? {
    let callerArgIdx = calleeArgIdx - operandConventions.unappliedArgumentCount
    guard callerArgIdx >= 0 && callerArgIdx < numArguments else { return nil }
    return argumentOperands[callerArgIdx]
  }

  public var referencedFunction: Function? {
    if let fri = callee as? FunctionRefInst {
      return fri.referencedFunction
    }
    return nil
  }

  public func hasSemanticsAttribute(_ attr: StaticString) -> Bool {
    if let callee = referencedFunction {
      return callee.hasSemanticsAttribute(attr)
    }
    return false
  }

  /// Returns the argument index of an operand.
  ///
  /// Returns nil if 'operand' is not an argument operand. This is the case if
  /// it's the callee function operand.
  ///
  /// Warning: the returned integer can be misused as an index into
  /// the wrong collection. Replace uses of this API with safer APIs.
  public func calleeArgumentIndex(of operand: Operand) -> Int? {
    operandConventions.calleeArgumentIndex(of: operand)
  }
}

extension ApplySite {
  private var originalFunctionConvention: FunctionConvention {
    FunctionConvention(for: callee.type.bridged.getASTType(),
                       in: parentFunction)
  }

  private var substitutedFunctionConvention: FunctionConvention {
    FunctionConvention(for: bridged.ApplySite_getSubstitutedCalleeType(),
                       in: parentFunction)
  }
}

public protocol FullApplySite : ApplySite {
  var singleDirectResult: Value? { get }
}

extension FullApplySite {
  public var unappliedArgumentCount: Int { 0 }

  /// The number of indirect out arguments.
  ///
  /// 0 if the callee has a direct or no return value and 1, if it has an indirect return value.
  public var numIndirectResultArguments: Int {
    return bridged.FullApplySite_numIndirectResultArguments()
  }
}
