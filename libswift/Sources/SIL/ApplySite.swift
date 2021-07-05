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

public protocol ApplySite : AnyObject {
  var operands: OperandArray { get }
  var numArguments: Int { get }
}

private let firstArgumentIndex = 1

extension ApplySite {
  public var callee: Value { operands[0].value }

  public func argumentIndex(of operand: Operand) -> Int? {
    let opIdx = operand.index
    if opIdx >= firstArgumentIndex &&
       opIdx <= firstArgumentIndex + numArguments {
      return opIdx - firstArgumentIndex
    }
    return nil
  }
}

public protocol FullApplySite : ApplySite {
  var singleDirectResult: Value? { get }
}
