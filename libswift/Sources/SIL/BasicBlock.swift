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

import SILBridging

extension swift.SILBasicBlock : ListNode, CustomStringConvertible {
  public var next: BasicBlock? { SILBasicBlock_next(self) }
  public var previous: BasicBlock? { SILBasicBlock_previous(self) }

  public var function: Function { SILBasicBlock_getFunction(self) }

  public var description: String {
    var s = SILBasicBlock_debugDescription(self)
    return String(cString: s.c_str())
  }

  public var arguments: ArgumentArray { ArgumentArray(block: self) }

  public var instructions: List<Instruction> {
    List(startAt: SILBasicBlock_firstInst(self))
  }

  public var reverseInstructions: ReverseList<Instruction> {
    ReverseList(startAt: SILBasicBlock_lastInst(self))
  }

  public var terminator: swift.TermInst {
    getAsTermInst(SILBasicBlock_lastInst(self)!)!
  }

  public var successors: SuccessorArray { terminator.successors }
  
  public var predecessors: PredecessorList {
    PredecessorList(startAt: SILBasicBlock_getFirstPred(self))
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

  /// The index of the basic block in its function.
  /// This has O(n) complexity. Only use it for debugging
  public var index: Int {
    for (idx, block) in function.blocks.enumerated() {
      if block == self { return idx }
    }
    fatalError()
  }
  
  public var label: String { "bb\(index)" }
}

public func == (lhs: BasicBlock, rhs: BasicBlock) -> Bool { isPtrEq(lhs, rhs) }
public func != (lhs: BasicBlock, rhs: BasicBlock) -> Bool { !(lhs == rhs) }

public typealias BasicBlock = swift.SILBasicBlock

public struct ArgumentArray : RandomAccessCollection {
  fileprivate let block: BasicBlock

  public var startIndex: Int { return 0 }
  public var endIndex: Int { SILBasicBlock_getNumArguments(block) }

  public subscript(_ index: Int) -> Argument {
    SILBasicBlock_getArgument(block, index)
  }
}

public struct SuccessorArray : RandomAccessCollection, CustomReflectable {
  private let succArray: BridgedArrayRef
  
  init(succArray: BridgedArrayRef) {
    self.succArray = succArray
  }
  
  public var startIndex: Int { return 0 }
  public var endIndex: Int { return Int(succArray.numElements) }
  
  public subscript(_ index: Int) -> BasicBlock {
    precondition(index >= 0 && index < endIndex)
    let s = BridgedSuccessor(succ: succArray.data + index &* BridgedSuccessorSize);
    return SILSuccessor_getTargetBlock(s)
  }
  
  public var customMirror: Mirror {
    let c: [Mirror.Child] = map { (label: nil, value: $0.label) }
    return Mirror(self, children: c)
  }
}

public struct PredecessorList : Sequence, IteratorProtocol, CustomReflectable {
  private var currentSucc: OptionalBridgedSuccessor
  
  public init(startAt: OptionalBridgedSuccessor) { currentSucc = startAt }

  public mutating func next() -> BasicBlock? {
    if let succPtr = currentSucc.succ {
      let succ = BridgedSuccessor(succ: succPtr)
      currentSucc = SILSuccessor_getNext(succ)
      return SILSuccessor_getContainingInst(succ).block
    }
    return nil
  }

  public var customMirror: Mirror {
    let c: [Mirror.Child] = map { (label: nil, value: $0) }
    return Mirror(self, children: c)
  }
}
