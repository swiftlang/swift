//===--- Argument.swift - Defines the Argument classes --------------===//
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

public typealias Argument = swift.SILArgument

/// A basic block argument.
///
/// Maps to both, SILPhiArgument and SILFunctionArgument.
extension swift.SILArgument {
  public var block: BasicBlock {
    return SILArgument_getParent(self)
  }
  
  public var index: Int { Int(getIndex()) }
}

public func ==(lhs: Argument, rhs: Argument) -> Bool {
  isPtrEq(getAsValue(lhs)!, getAsValue(rhs)!)
}

public typealias FunctionArgument = swift.SILFunctionArgument;
public typealias BlockArgument = swift.SILPhiArgument;
extension BlockArgument {
  public var block: BasicBlock {
    return SILArgument_getParent(getAsSILArgument(self)!)
  }
  
  public var index: Int { getAsSILArgument(self)!.index }

  /// Note: critical edges are not supported, i.e. this is false if there is
  /// a cond_br in the predecessors.
  public var isPhiArgument: Bool {
    block.predecessors.allSatisfy { isaBranchInst(getAsSILInstruction($0.terminator)) }
  }

  public var incomingPhiOperands: LazyMapSequence<PredecessorList, Operand> {
    assert(isPhiArgument)
    let idx = index
    return block.predecessors.lazy.map { $0.terminator.operands[idx] }
  }

  public var incomingPhiValues: LazyMapSequence<PredecessorList, Value> {
    assert(isPhiArgument)
    let idx = index
    return block.predecessors.lazy.map { $0.terminator.operands[idx].value }
  }
}
