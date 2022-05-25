//===--- BasicBlock.swift - Defines the BasicBlock class ------------------===//
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

final public class BasicBlock : ListNode, CustomStringConvertible, HasShortDescription {
  public var next: BasicBlock? { SILBasicBlock_next(bridged).block }
  public var previous: BasicBlock? { SILBasicBlock_previous(bridged).block }

  // Needed for ReverseList<BasicBlock>.reversed(). Never use directly.
  public var _firstInList: BasicBlock { SILFunction_firstBlock(function.bridged).block! }
  // Needed for List<BasicBlock>.reversed(). Never use directly.
  public var _lastInList: BasicBlock { SILFunction_lastBlock(function.bridged).block! }

  public var function: Function { SILBasicBlock_getFunction(bridged).function }

  public var description: String {
    var s = SILBasicBlock_debugDescription(bridged)
    return String(cString: s.c_str())
  }
  public var shortDescription: String { name }

  public var arguments: ArgumentArray { ArgumentArray(block: self) }

  public var instructions: List<Instruction> {
    List(first: SILBasicBlock_firstInst(bridged).instruction)
  }

  public var terminator: TermInst {
    SILBasicBlock_lastInst(bridged).instruction as! TermInst
  }

  public var successors: SuccessorArray { terminator.successors }

  public var predecessors: PredecessorList {
    PredecessorList(startAt: SILBasicBlock_getFirstPred(bridged))
  }

  public var singlePredecessor: BasicBlock? {
    var preds = predecessors
    if let p = preds.next() {
      if preds.next() == nil {
        return p
      }
    }
    return nil
  }
  
  public var hasSinglePredecessor: Bool { singlePredecessor != nil }

  /// The index of the basic block in its function.
  /// This has O(n) complexity. Only use it for debugging
  public var index: Int {
    for (idx, block) in function.blocks.enumerated() {
      if block == self { return idx }
    }
    fatalError()
  }
 
  public var name: String { "bb\(index)" }

  public var bridged: BridgedBasicBlock { BridgedBasicBlock(obj: SwiftObject(self)) }
}

public func == (lhs: BasicBlock, rhs: BasicBlock) -> Bool { lhs === rhs }
public func != (lhs: BasicBlock, rhs: BasicBlock) -> Bool { lhs !== rhs }

public struct ArgumentArray : RandomAccessCollection {
  fileprivate let block: BasicBlock

  public var startIndex: Int { return 0 }
  public var endIndex: Int { SILBasicBlock_getNumArguments(block.bridged) }

  public subscript(_ index: Int) -> Argument {
    SILBasicBlock_getArgument(block.bridged, index).argument
  }
}

public struct SuccessorArray : RandomAccessCollection, FormattedLikeArray {
  private let succArray: BridgedArrayRef

  init(succArray: BridgedArrayRef) {
    self.succArray = succArray
  }

  public var startIndex: Int { return 0 }
  public var endIndex: Int { return Int(succArray.numElements) }

  public subscript(_ index: Int) -> BasicBlock {
    precondition(index >= 0 && index < endIndex)
    let s = BridgedSuccessor(succ: succArray.data! + index &* BridgedSuccessorSize);
    return SILSuccessor_getTargetBlock(s).block
  }
}

public struct PredecessorList : CollectionLikeSequence, IteratorProtocol {
  private var currentSucc: OptionalBridgedSuccessor

  public init(startAt: OptionalBridgedSuccessor) { currentSucc = startAt }

  public mutating func next() -> BasicBlock? {
    if let succPtr = currentSucc.succ {
      let succ = BridgedSuccessor(succ: succPtr)
      currentSucc = SILSuccessor_getNext(succ)
      return SILSuccessor_getContainingInst(succ).instruction.block
    }
    return nil
  }
}


// Bridging utilities

extension BridgedBasicBlock {
  public var block: BasicBlock { obj.getAs(BasicBlock.self) }
  public var optional: OptionalBridgedBasicBlock {
    OptionalBridgedBasicBlock(obj: self.obj)
  }
}

extension OptionalBridgedBasicBlock {
  public var block: BasicBlock? { obj.getAs(BasicBlock.self) }
  public static var none: OptionalBridgedBasicBlock {
    OptionalBridgedBasicBlock(obj: nil)
  }
}

extension Optional where Wrapped == BasicBlock {
  public var bridged: OptionalBridgedBasicBlock {
    OptionalBridgedBasicBlock(obj: self?.bridged.obj)
  }
}
