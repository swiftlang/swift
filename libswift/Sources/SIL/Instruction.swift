//===--- Instruction.swift - Defines the Instruction classes --------------===//
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

//===----------------------------------------------------------------------===//
//                       Instruction base classes
//===----------------------------------------------------------------------===//

public typealias Instruction = SILInstruction

extension SILInstruction : ListNode, CustomStringConvertible {
  final public var next: Instruction? { SILInstruction_next(self) }

  final public var previous: Instruction? { SILInstruction_previous(self) }

  final public var block: BasicBlock {
    SILInstruction_getParent(self)
  }

  final public var description: String {
    var s = SILInstruction_debugDescription(self)
    return String(cString: s.c_str())
  }
  
  final public var operands: OperandArray {
    return OperandArray(opArray: SILInstruction_getOperands(self))
  }
  
  fileprivate var resultCount: Int { Int(getNumResults()) }

  public struct Results : RandomAccessCollection {
    fileprivate let inst: Instruction
    fileprivate let numResults: Int

    public var startIndex: Int { 0 }
    public var endIndex: Int { numResults }
    public subscript(_ index: Int) -> Value { getAsValue(inst.getResult(UInt32(index)))! }
  }

  final public var results: Results {
    Results(inst: self, numResults: resultCount)
  }

  final public var location: Location {
    return Location(bridgedLocation: SILInstruction_getLocation(self))
  }

  final public var mayHaveSideEffects: Bool {
    return mayTrap() || mayWriteToMemory
  }

  final public var mayReadFromMemory: Bool {
    switch SILInstruction_getMemBehavior(self) {
      case MayReadBehavior, MayReadWriteBehavior, MayHaveSideEffectsBehavior:
        return true
      default:
        return false
    }
  }

  final public var mayWriteToMemory: Bool {
    switch SILInstruction_getMemBehavior(self) {
      case MayWriteBehavior, MayReadWriteBehavior, MayHaveSideEffectsBehavior:
        return true
      default:
        return false
    }
  }

  final public var mayReadOrWriteMemory: Bool {
    switch SILInstruction_getMemBehavior(self) {
      case MayReadBehavior, MayWriteBehavior, MayReadWriteBehavior,
           MayHaveSideEffectsBehavior:
        return true
      default:
        return false
    }
  }
}

/// Instructions, which have a single operand.
public protocol UnaryInstruction : AnyObject {
  var operands: OperandArray { get }
  var operand: Value { get }
}

extension UnaryInstruction {
  public var operand: Value { operands[0].value }
}

//===----------------------------------------------------------------------===//
//                             no-value instructions
//===----------------------------------------------------------------------===//

extension swift.StoreInst {
  final public var operands: OperandArray {
    return OperandArray(opArray: SILInstruction_getOperands(getAsSILInstruction(self)))
  }

  public var sourceOperand: Operand { return operands[0] }
  public var destinationOperand: Operand { return operands[1] }
  public var source: Value { return sourceOperand.value }
  public var destination: Value { return destinationOperand.value }
  
  // must match with enum class StoreOwnershipQualifier
  public enum StoreOwnership: Int {
    case unqualified = 0, initialize = 1, assign = 2, trivial = 3
  }
  public var destinationOwnership: StoreOwnership {
    StoreOwnership(rawValue: StoreInst_getStoreOwnership(self))!
  }
}

extension swift.CopyAddrInst {
  final public var operands: OperandArray {
    return OperandArray(opArray: SILInstruction_getOperands(getAsSILInstruction(self)))
  }

  public var sourceOperand: Operand { return operands[0] }
  public var destinationOperand: Operand { return operands[1] }
  public var source: Value { return sourceOperand.value }
  public var destination: Value { return destinationOperand.value }
}

extension swift.EndAccessInst {
  final public var operand: Value {
    return OperandArray(opArray: SILInstruction_getOperands(getAsSILInstruction(self)))[0].value
  }
  
  public var beginAccess: swift.BeginAccessInst {
    return getAsBeginAccessInst(operand)!
  }
}

extension swift.EndBorrowInst {}

extension swift.CondFailInst {
  public var message: String { CondFailInst_getMessage(self).string }
  
  final public var operand: Value {
    return OperandArray(opArray: SILInstruction_getOperands(getAsSILInstruction(self)))[0].value
  }
  
  final public var location: Location {
    return Location(bridgedLocation: SILInstruction_getLocation(getAsSILInstruction(self)))
  }
  
  final public var next: Instruction? { SILInstruction_next(getAsSILInstruction(self)) }

  final public var previous: Instruction? { SILInstruction_previous(getAsSILInstruction(self)) }
}

extension swift.UnconditionalCheckedCastAddrInst {}

public protocol RefCountingInst { }

extension swift.StrongRetainInst : RefCountingInst {
  final public var operand: Value {
    return OperandArray(opArray: SILInstruction_getOperands(getAsSILInstruction(self)))[0].value
  }

  final public var next: Instruction? { SILInstruction_next(getAsSILInstruction(self)) }

  final public var previous: Instruction? { SILInstruction_previous(getAsSILInstruction(self)) }
}

extension swift.RetainValueInst : RefCountingInst { }

extension swift.StrongReleaseInst : RefCountingInst {
  final public var operand: Value {
    return OperandArray(opArray: SILInstruction_getOperands(getAsSILInstruction(self)))[0].value
  }
}

extension swift.ReleaseValueInst : RefCountingInst { }

//===----------------------------------------------------------------------===//
//                           single-value instructions
//===----------------------------------------------------------------------===//

extension swift.UpcastInst {
  final public var operand: Value {
    return OperandArray(opArray: SILInstruction_getOperands(getAsSILInstruction(self)))[0].value
  }
}

extension swift.UncheckedRefCastInst {
  final public var operand: Value {
    return OperandArray(opArray: SILInstruction_getOperands(getAsSILInstruction(self)))[0].value
  }
}

extension swift.RawPointerToRefInst {
  final public var operand: Value {
    return OperandArray(opArray: SILInstruction_getOperands(getAsSILInstruction(self)))[0].value
  }
}

extension swift.AddressToPointerInst {
  final public var operand: Value {
    return OperandArray(opArray: SILInstruction_getOperands(getAsSILInstruction(self)))[0].value
  }
}

extension swift.InitExistentialRefInst {
  final public var operand: Value {
    return OperandArray(opArray: SILInstruction_getOperands(getAsSILInstruction(self)))[0].value
  }
}

extension swift.TupleExtractInst {
  public var fieldIndex: Int { TupleExtractInst_fieldIndex(self) }
  
  final public var operand: Value {
    return OperandArray(opArray: SILInstruction_getOperands(getAsSILInstruction(self)))[0].value
  }
}

extension swift.TupleElementAddrInst {
  public var fieldIndex: Int { TupleElementAddrInst_fieldIndex(self) }
}

extension swift.StructExtractInst {
  public var fieldIndex: Int { StructExtractInst_fieldIndex(self) }
}

extension swift.StructElementAddrInst {
  public var fieldIndex: Int { StructElementAddrInst_fieldIndex(self) }
}

extension swift.EnumInst {
  final public var operands: OperandArray {
    return OperandArray(opArray: SILInstruction_getOperands(getAsSILInstruction(self)))
  }

  public var caseIndex: Int { EnumInst_caseIndex(self) }
  
  public var operand: Value? { operands.first?.value }
}

extension swift.UncheckedEnumDataInst {
  public var caseIndex: Int { UncheckedEnumDataInst_caseIndex(self) }
}

extension swift.RefElementAddrInst {
  public var fieldIndex: Int { RefElementAddrInst_fieldIndex(self) }
}

extension swift.ConvertFunctionInst {
  final public var operand: Value {
    return OperandArray(opArray: SILInstruction_getOperands(getAsSILInstruction(self)))[0].value
  }
}

extension swift.PartialApplyInst {
  public var numArguments: Int { PartialApplyInst_numArguments(self) }
}

//===----------------------------------------------------------------------===//
//                            multi-value instructions
//===----------------------------------------------------------------------===//

extension swift.BeginApplyInst {
  public var numArguments: Int { BeginApplyInst_numArguments(self) }
  
  public var singleDirectResult: Value? { nil }
}

//===----------------------------------------------------------------------===//
//                            terminator instructions
//===----------------------------------------------------------------------===//

extension swift.TermInst {
  public var successors: SuccessorArray {
    return SuccessorArray(succArray: TermInst_getSuccessors(self))
  }
  
  final public var operands: OperandArray {
    return OperandArray(opArray: SILInstruction_getOperands(getAsSILInstruction(self)))
  }
}

extension swift.TryApplyInst {
  public var successors: SuccessorArray {
    return SuccessorArray(succArray: TermInst_getSuccessors(getAsTermInst(getAsSILInstruction(self))!))
  }

  public var numArguments: Int { TryApplyInst_numArguments(self) }
  
  public var normalBlock: BasicBlock { successors[0] }
  public var errorBlock: BasicBlock { successors[1] }
  
  public var singleDirectResult: Value? { getAsValue(normalBlock.arguments[0]) }
}

extension swift.BranchInst {
  public var targetBlock: BasicBlock { BranchInst_getTargetBlock(self) }
  
  public func getArgument(for operand: Operand) -> Argument {
    return targetBlock.arguments[operand.index]
  }
}

extension swift.SwitchEnumInst {
  public var successors: SuccessorArray {
    return SuccessorArray(succArray: TermInst_getSuccessors(getAsTermInst(getAsSILInstruction(self))!))
  }

  final public var operands: OperandArray {
    return OperandArray(opArray: SILInstruction_getOperands(getAsSILInstruction(self)))
  }

  public var enumOp: Value { operands[0].value }

  public struct CaseIndexArray : RandomAccessCollection {
    fileprivate let switchEnum: swift.SwitchEnumInst

    public var startIndex: Int { return 0 }
    public var endIndex: Int { SwitchEnumInst_getNumCases(switchEnum) }

    public subscript(_ index: Int) -> Int {
      SwitchEnumInst_getCaseIndex(switchEnum, index)
    }
  }

  var caseIndices: CaseIndexArray { CaseIndexArray(switchEnum: self) }

  var cases: Zip2Sequence<CaseIndexArray, SuccessorArray> {
    zip(caseIndices, successors)
  }

  // This does not handle the special case where the default covers exactly
  // the "missing" case.
  public func getUniqueSuccessor(forCaseIndex: Int) -> BasicBlock? {
    cases.first(where: { $0.0 == forCaseIndex })?.1
  }

  // This does not handle the special case where the default covers exactly
  // the "missing" case.
  public func getUniqueCase(forSuccessor: BasicBlock) -> Int? {
    cases.first(where: { $0.1 == forSuccessor })?.0
  }
}
