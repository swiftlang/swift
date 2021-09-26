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

final public class Function : CustomStringConvertible {
  public var name: String {
    return SILFunction_getName(bridged).string
  }

  final public var description: String {
    return SILFunction_debugDescription(bridged).string
  }

  public var entryBlock: BasicBlock {
    SILFunction_firstBlock(bridged).block!
  }

  public var blocks : List<BasicBlock> {
    return List(startAt: SILFunction_firstBlock(bridged).block)
  }

  public var reverseBlocks : ReverseList<BasicBlock> {
    return ReverseList(startAt: SILFunction_lastBlock(bridged).block)
  }

  public var arguments: LazyMapSequence<ArgumentArray, FunctionArgument> {
    entryBlock.arguments.lazy.map { $0 as! FunctionArgument }
  }
  
  public var numIndirectResultArguments: Int {
    SILFunction_numIndirectResultArguments(bridged)
  }
  
  public var hasSelfArgument: Bool {
    SILFunction_getSelfArgumentIndex(bridged) >= 0
  }
  
  public var selfArgumentIndex: Int {
    let selfIdx = SILFunction_getSelfArgumentIndex(bridged)
    assert(selfIdx >= 0)
    return selfIdx
  }

  public var bridged: BridgedFunction { BridgedFunction(obj: SwiftObject(self)) }
}

// Bridging utilities

extension BridgedFunction {
  public var function: Function { obj.getAs(Function.self) }
}
