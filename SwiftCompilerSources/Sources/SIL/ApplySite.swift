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

public struct ApplyOperands {
  public static let calleeOperandIndex: Int = 0
  public static let firstArgumentIndex = 1
}

public protocol ApplySite : Instruction {
  var operands: OperandArray { get }
  var numArguments: Int { get }
  var substitutionMap: SubstitutionMap { get }
  func calleeArgIndex(callerArgIndex: Int) -> Int
  func callerArgIndex(calleeArgIndex: Int) -> Int?
  func getArgumentConvention(calleeArgIndex: Int) -> ArgumentConvention
}

extension ApplySite {
  public var callee: Value { operands[ApplyOperands.calleeOperandIndex].value }

  public var arguments: LazyMapSequence<OperandArray, Value> {
    operands[1..<operands.count].lazy.map { $0.value }
  }

  public var substitutionMap: SubstitutionMap {
    SubstitutionMap(ApplySite_getSubstitutionMap(bridged))
  }

  public func argumentIndex(of operand: Operand) -> Int? {
    let opIdx = operand.index
    if opIdx >= ApplyOperands.firstArgumentIndex &&
       opIdx <= ApplyOperands.firstArgumentIndex + numArguments {
      return opIdx - ApplyOperands.firstArgumentIndex
    }
    return nil
  }

  public func getArgumentConvention(calleeArgIndex: Int) -> ArgumentConvention {
    return ApplySite_getArgumentConvention(bridged, calleeArgIndex).convention
  }
  
  public var referencedFunction: Function? {
    if let fri = callee as? FunctionRefInst {
      return fri.referencedFunction
    }
    return nil
  }
}

public protocol FullApplySite : ApplySite {
  var singleDirectResult: Value? { get }
}

extension FullApplySite {
  public func calleeArgIndex(callerArgIndex: Int) -> Int { callerArgIndex }
  public func callerArgIndex(calleeArgIndex: Int) -> Int? { calleeArgIndex }
}
