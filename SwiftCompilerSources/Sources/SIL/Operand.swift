//===--- Operand.swift - Instruction operands -----------------------------===//
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

/// An operand of an instruction.
public struct Operand : CustomStringConvertible, CustomReflectable, Hashable {
  fileprivate let bridged: BridgedOperand

  init(_ bridged: BridgedOperand) {
    self.bridged = bridged
  }

  public var value: Value {
    Operand_getValue(bridged).value
  }

  public static func ==(lhs: Operand, rhs: Operand) -> Bool {
    return lhs.bridged.op == rhs.bridged.op
  }

  public var instruction: Instruction {
    return Operand_getUser(bridged).instruction
  }
  
  public var index: Int { instruction.operands.getIndex(of: self) }

  public func hash(into: inout Hasher) {
    into.combine(instruction)
    into.combine(index)
  }
  
  /// True if the operand is used to describe a type dependency, but it's not
  /// used as value.
  public var isTypeDependent: Bool { Operand_isTypeDependent(bridged) != 0 }
  
  public var description: String { "operand #\(index) of \(instruction)" }
  
  public var customMirror: Mirror { Mirror(self, children: []) }
}

public struct OperandArray : RandomAccessCollection, CustomReflectable {
  private let opArray: BridgedArrayRef
  
  init(opArray: BridgedArrayRef) {
    self.opArray = opArray
  }
  
  public var startIndex: Int { return 0 }
  public var endIndex: Int { return Int(opArray.numElements) }
  
  public subscript(_ index: Int) -> Operand {
    precondition(index >= 0 && index < endIndex)
    return Operand(BridgedOperand(op: opArray.data! + index &* BridgedOperandSize))
  }
  
  public func getIndex(of operand: Operand) -> Int {
    let idx = (operand.bridged.op - UnsafeRawPointer(opArray.data!)) /
                BridgedOperandSize
    precondition(self[idx].bridged.op == operand.bridged.op)
    return idx
  }
  
  public var customMirror: Mirror {
    let c: [Mirror.Child] = map { (label: nil, value: $0.value) }
    return Mirror(self, children: c)
  }
  
  /// Returns a sub-array defined by `bounds`.
  ///
  /// Note: this does not return a Slice. The first index of the returnd array is always 0.
  public subscript(bounds: Range<Int>) -> OperandArray {
    precondition(bounds.lowerBound >= 0)
    precondition(bounds.upperBound <= endIndex)
    return OperandArray(opArray: BridgedArrayRef(
      data: opArray.data! + bounds.lowerBound &* BridgedOperandSize,
      numElements: bounds.upperBound - bounds.lowerBound))
  }
}

public struct UseList : CollectionLikeSequence {
  public struct Iterator : IteratorProtocol {
    var currentOpPtr: UnsafeRawPointer?
    
    public mutating func next() -> Operand? {
      if let opPtr = currentOpPtr {
        let bridged = BridgedOperand(op: opPtr)
        currentOpPtr = Operand_nextUse(bridged).op
        return Operand(bridged)
      }
      return nil
    }
  }

  private let firstOpPtr: UnsafeRawPointer?

  init(_ firstOpPtr: OptionalBridgedOperand) {
    self.firstOpPtr = firstOpPtr.op
  }

  public var singleUse: Operand? {
    if let opPtr = firstOpPtr {
      if Operand_nextUse(BridgedOperand(op: opPtr)).op != nil { return nil }
      return Operand(BridgedOperand(op: opPtr))
    }
    return nil
  }

  public var isSingleUse: Bool { singleUse != nil }

  public func makeIterator() -> Iterator {
    return Iterator(currentOpPtr: firstOpPtr)
  }
}
