//===--- GlobalVariable.swift - Defines the GlobalVariable class ----------===//
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

import Basic
import SILBridging

final public class GlobalVariable : CustomStringConvertible, HasShortDescription {
  public var name: StringRef {
    return StringRef(bridged: SILGlobalVariable_getName(bridged))
  }

  public var description: String {
    var s = SILGlobalVariable_debugDescription(bridged)
    return String(cString: s.c_str())
  }

  public var shortDescription: String { name.string }

  // TODO: initializer instructions

  var bridged: BridgedGlobalVar { BridgedGlobalVar(obj: SwiftObject(self)) }
}

// Bridging utilities

extension BridgedGlobalVar {
  var globalVar: GlobalVariable { obj.getAs(GlobalVariable.self) }
}
