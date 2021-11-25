//===--- Value.swift - the Value protocol ---------------------------------===//
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

public typealias Value = swift.ValueBase

extension Value {
  public var description: String {
    var s = SILNode_debugDescription(self)
    return String(cString: s.c_str())
  }

  public var uses: UseList { UseList(SILValue_firstUse(self)) }

  public var type: Type { SILValue_getType(self) }
}

public func==(_ a: Value, _ b: Value) -> Bool { isPtrEq(a, b) }
