//===--- Builder.swift -  Building and modifying SIL ----------------------===//
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

public typealias Builder = swift.SILBuilder

extension Builder {
  public init(at insertionPoint: Instruction) {
    self = SILBuilder_init(insertionPoint.bridged)
  }
  
  public mutating func createIntegerLiteral(at loc: Location, type: Type, value: Int) -> IntegerLiteralInst {
    let ilPtr = createIntegerLiteral(
      unsafeBitCast(loc, to: swift.SILLocation.self),
      unsafeBitCast(type, to: swift.SILType.self),
      value)
    let ilInst = Unmanaged<Instruction>.fromOpaque(UnsafeRawPointer(ilPtr)!)
      .takeUnretainedValue()
    return ilInst as! IntegerLiteralInst
  }
}
