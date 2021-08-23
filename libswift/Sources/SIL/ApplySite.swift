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

public struct ApplyOperands {
  public static let calleeOperandIndex: Int = 0
  public static let firstArgumentIndex = 1
}

public protocol ApplySite : AnyObject {
  var operands: OperandArray { get }
  var numArguments: Int { get }
}

extension ApplySite {
  public var callee: Value { operands[ApplyOperands.calleeOperandIndex].value }

  public func argumentIndex(of operand: Operand) -> Int? {
    let opIdx = operand.index
    if opIdx >= ApplyOperands.firstArgumentIndex &&
       opIdx <= ApplyOperands.firstArgumentIndex + numArguments {
      return opIdx - ApplyOperands.firstArgumentIndex
    }
    return nil
  }

}

public protocol FullApplySite : ApplySite {
  var singleDirectResult: Value? { get }
}
