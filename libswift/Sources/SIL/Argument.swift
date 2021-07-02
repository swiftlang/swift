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

/// A basic block argument.
///
/// Maps to both, SILPhiArgument and SILFunctionArgument.
public class Argument : Value, Equatable {
  public var definingInstruction: Instruction? { nil }

  public var block: BasicBlock {
    return SILArgument_getParent(bridged).block
  }

  var bridged: BridgedArgument { BridgedArgument(obj: SwiftObject(self)) }
  
  public var index: Int {
    return block.arguments.firstIndex(of: self)!
  }
  
  public static func ==(lhs: Argument, rhs: Argument) -> Bool {
    lhs === rhs
  }
}

final public class FunctionArgument : Argument {
}

final public class BlockArgument : Argument {
  /// Note: critical edges are not supported, i.e. this is false if there is
  /// a cond_br in the predecessors.
  public var isPhiArgument: Bool {
    block.predecessors.allSatisfy { $0.terminator is BranchInst }
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

// Bridging utilities

extension BridgedArgument {
  var argument: Argument { obj.getAs(Argument.self) }
  var functionArgument: FunctionArgument { obj.getAs(FunctionArgument.self) }
}
