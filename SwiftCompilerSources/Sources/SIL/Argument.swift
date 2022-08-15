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

import Basic
import SILBridging

/// A basic block argument.
///
/// Maps to both, SILPhiArgument and SILFunctionArgument.
public class Argument : Value, Equatable {
  public var definingInstruction: Instruction? { nil }
  public var definingBlock: BasicBlock { block }

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
  public var isExclusiveIndirectParameter: Bool {
    SILArgument_isExclusiveIndirectParameter(bridged) != 0
  }
}

final public class BlockArgument : Argument {
  public var isPhiArgument: Bool {
    block.predecessors.allSatisfy {
      let term = $0.terminator
      return term is BranchInst || term is CondBranchInst
    }
  }

  public var incomingPhiOperands: LazyMapSequence<PredecessorList, Operand> {
    assert(isPhiArgument)
    let idx = index
    return block.predecessors.lazy.map {
      switch $0.terminator {
        case let br as BranchInst:
          return br.operands[idx]
        case let condBr as CondBranchInst:
          if condBr.trueBlock == self.block {
            assert(condBr.falseBlock != self.block)
            return condBr.trueOperands[idx]
          } else {
            assert(condBr.falseBlock == self.block)
            return condBr.falseOperands[idx]
          }
        default:
          fatalError("wrong terminator for phi-argument")
      }
    }
  }

  public var incomingPhiValues: LazyMapSequence<LazyMapSequence<PredecessorList, Operand>, Value> {
    incomingPhiOperands.lazy.map { $0.value }
  }
}

// Bridging utilities

extension BridgedArgument {
  public var argument: Argument { obj.getAs(Argument.self) }
  public var blockArgument: BlockArgument { obj.getAs(BlockArgument.self) }
  public var functionArgument: FunctionArgument { obj.getAs(FunctionArgument.self) }
}
