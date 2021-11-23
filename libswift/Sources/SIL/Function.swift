//===--- Function.swift - Defines the Function class ----------------------===//
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

public typealias Function = swift.SILFunction

extension Function : CustomStringConvertible {
  public var name: String {
    return SILFunction_getName(self).string
  }

  final public var description: String {
    var s = SILFunction_debugDescription(self)
    return String(cString: s.c_str())
  }

  public var entryBlock: BasicBlock { SILFunction_firstBlock(self)! }

  public var blocks : List<BasicBlock> {
    return List(startAt: entryBlock)
  }

  public var arguments: LazyMapSequence<ArgumentArray, FunctionArgument> {
    entryBlock.arguments.lazy.map { getAsSILFunctionArgument(getAsValue($0))! }
  }
  
  public var numIndirectResultArguments: Int {
    SILFunction_numIndirectResultArguments(self)
  }
  
  public var hasSelfArgument: Bool {
    SILFunction_getSelfArgumentIndex(self) >= 0
  }
  
  public var selfArgumentIndex: Int {
    let selfIdx = SILFunction_getSelfArgumentIndex(self)
    assert(selfIdx >= 0)
    return selfIdx
  }
}
