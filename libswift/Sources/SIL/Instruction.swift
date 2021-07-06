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

public class Instruction : ListNode, CustomStringConvertible, Hashable {
  final public var next: Instruction? {
    SILInstruction_next(bridged).instruction
  }

  final public var previous: Instruction? {
    SILInstruction_previous(bridged).instruction
  }

  final public var block: BasicBlock {
    SILInstruction_getParent(bridged).block
  }

  final public var description: String {
    SILNode_debugDescription(bridgedNode).takeString()
  }
  
  final public var operands: OperandArray {
    return OperandArray(opArray: SILInstruction_getOperands(bridged))
  }
  
  fileprivate var resultCount: Int { 0 }
  fileprivate func getResult(index: Int) -> Value { fatalError() }

  final public var results: InstructionResults { InstructionResults(self) }

  final public var location: Location {
    return Location(bridgedLocation: SILInstruction_getLocation(bridged))
  }

  public var mayTrap: Bool { false }

  final public var mayHaveSideEffects: Bool {
    return mayTrap || mayWriteToMemory
  }

  final public var mayReadFromMemory: Bool {
    switch SILInstruction_getMemBehavior(bridged) {
      case MayReadBehavior, MayReadWriteBehavior, MayHaveSideEffectsBehavior:
        return true
      default:
        return false
    }
  }

  final public var mayWriteToMemory: Bool {
    switch SILInstruction_getMemBehavior(bridged) {
      case MayWriteBehavior, MayReadWriteBehavior, MayHaveSideEffectsBehavior:
        return true
      default:
        return false
    }
  }

  final public var mayReadOrWriteMemory: Bool {
    switch SILInstruction_getMemBehavior(bridged) {
      case MayReadBehavior, MayWriteBehavior, MayReadWriteBehavior,
           MayHaveSideEffectsBehavior:
        return true
      default:
        return false
    }
  }

  public static func ==(lhs: Instruction, rhs: Instruction) -> Bool {
    lhs === rhs
  }

  public func hash(into hasher: inout Hasher) {
    hasher.combine(ObjectIdentifier(self))
  }

  public var bridged: BridgedInstruction {
    BridgedInstruction(obj: SwiftObject(self))
  }
  var bridgedNode: BridgedNode { BridgedNode(obj: SwiftObject(self)) }
}

extension BridgedInstruction {
  public var instruction: Instruction { obj.getAs(Instruction.self) }
  public func getAs<T: Instruction>(_ instType: T.Type) -> T { obj.getAs(T.self) }
}

extension OptionalBridgedInstruction {
  var instruction: Instruction? { obj.getAs(Instruction.self) }
}

public struct InstructionResults : Sequence, IteratorProtocol {
  let inst: Instruction
  let numResults: Int
  var index: Int = 0

  init(_ inst: Instruction) {
    self.inst = inst
    numResults = inst.resultCount
  }

  public mutating func next() -> Value? {
    let idx = index
    if idx < numResults {
      index += 1
      return inst.getResult(index: idx)
    }
    return nil
  }
}

public class SingleValueInstruction : Instruction, Value {
  final public var definingInstruction: Instruction? { self }

  fileprivate final override var resultCount: Int { 1 }
  fileprivate final override func getResult(index: Int) -> Value { self }
}

public final class MultipleValueInstructionResult : Value {
  final public var description: String {
    SILNode_debugDescription(bridgedNode).takeString()
  }

  public var definingInstruction: Instruction? {
    MultiValueInstResult_getParent(bridged).instruction
  }

  var bridged: BridgedMultiValueResult {
    BridgedMultiValueResult(obj: SwiftObject(self))
  }
  var bridgedNode: BridgedNode { BridgedNode(obj: SwiftObject(self)) }
}

extension BridgedMultiValueResult {
  var result: MultipleValueInstructionResult {
    obj.getAs(MultipleValueInstructionResult.self)
  }
}

public class MultipleValueInstruction : Instruction {
  fileprivate final override var resultCount: Int {
    return MultipleValueInstruction_getNumResults(bridged)
  }
  fileprivate final override func getResult(index: Int) -> Value {
    MultipleValueInstruction_getResult(bridged, index).result
  }
}

//===----------------------------------------------------------------------===//
//                             no-value instructions
//===----------------------------------------------------------------------===//

/// Used for all non-value instructions which are not implemented here, yet.
/// See registerBridgedClass() in SILBridgingUtils.cpp.
final public class UnimplementedInstruction : Instruction {
}

final public class StoreInst : Instruction {
  public var sourceOperand: Operand { return operands[0] }
  public var destinationOperand: Operand { return operands[1] }
  public var source: Value { return sourceOperand.value }
  public var destination: Value { return destinationOperand.value }
}

final public class CopyAddrInst : Instruction {
  public var sourceOperand: Operand { return operands[0] }
  public var destinationOperand: Operand { return operands[1] }
  public var source: Value { return sourceOperand.value }
  public var destination: Value { return destinationOperand.value }
}

final public class EndAccessInst : Instruction {
  public var beginAccess: BeginAccessInst {
    return operands[0].value as! BeginAccessInst
  }
}

final public class EndBorrowInst : Instruction {
  public var borrowedValue: Value { return operands[0].value }
}

final public class DeallocStackInst : Instruction {
  public var allocstack: AllocStackInst {
    return operands[0].value as! AllocStackInst
  }
}

final public class CondFailInst : Instruction {
  public var condition: Value { operands[0].value }
  
  public override var mayTrap: Bool { true }

  public var message: String { CondFailInst_getMessage(bridged).string }
}

final public class FixLifetimeInst : Instruction {
  public var value: Value { operands[0].value }
}

final public class DebugValueInst : Instruction {
  public var value: Value { operands[0].value }
}

final public class DebugValueAddrInst : Instruction {
  public var address: Value { operands[0].value }
}

final public class UnconditionalCheckedCastAddrInst : Instruction {
  public override var mayTrap: Bool { true }
}

final public class SetDeallocatingInst : Instruction {
  public var reference: Value { operands[0].value }
}

final public class DeallocRefInst : Instruction {
  public var reference: Value { operands[0].value }
}

public class RefCountingInst : Instruction {
  final public var reference: Value { operands[0].value }
}

final public class StrongRetainInst : RefCountingInst {
}

final public class RetainValueInst : RefCountingInst {
}

final public class StrongReleaseInst : RefCountingInst {
}

final public class ReleaseValueInst : RefCountingInst {
}

final public class DestroyValueInst : Instruction {
  public var value: Value { operands[0].value }
}

final public class DestroyAddrInst : Instruction {
  public var address: Value { operands[0].value }
}

final public class UnimplementedRefCountingInst : RefCountingInst {}

//===----------------------------------------------------------------------===//
//                           single-value instructions
//===----------------------------------------------------------------------===//

/// Used for all SingleValueInstructions which are not implemented here, yet.
/// See registerBridgedClass() in SILBridgingUtils.cpp.
final public class UnimplementedSingleValueInst : SingleValueInstruction {
}

final public class LoadInst : SingleValueInstruction {
  public var address: Value { operands[0].value }
}

final public class LoadBorrowInst : SingleValueInstruction {
  public var address: Value { operands[0].value }
}

final public class BuiltinInst : SingleValueInstruction {}

final public class UpcastInst : SingleValueInstruction {
  public var source: Value { operands[0].value }
}

final public class InitExistentialRefInst : SingleValueInstruction {
  public var reference: Value { operands[0].value }
}

final public class OpenExistentialRefInst : SingleValueInstruction {
  public var existential: Value { operands[0].value }
}

final public class InitExistentialMetatypeInst : SingleValueInstruction {
  public var existential: Value { operands[0].value }
}

final public class OpenExistentialMetatypeInst : SingleValueInstruction {
  public var existentialMetatype: Value { operands[0].value }
}

final public class ValueMetatypeInst : SingleValueInstruction {
  public var value: Value { operands[0].value }
}

final public class ExistentialMetatypeInst : SingleValueInstruction {
  public var existential: Value { operands[0].value }
}

public class GlobalAccessInst : SingleValueInstruction {
  final public var global: GlobalVariable {
    GlobalAccessInst_getGlobal(bridged).globalVar
  }
}

final public class GlobalAddrInst : GlobalAccessInst {}

final public class GlobalValueInst : GlobalAccessInst {}

final public class TupleInst : SingleValueInstruction {
}

final public class TupleExtractInst : SingleValueInstruction {
  public var tuple: Value { operands[0].value }
  public var fieldIndex: Int { TupleExtractInst_fieldIndex(bridged) }
}

final public class TupleElementAddrInst : SingleValueInstruction {
  public var tupleAddress: Value { operands[0].value }
  public var fieldIndex: Int { TupleElementAddrInst_fieldIndex(bridged) }
}

final public class StructInst : SingleValueInstruction {
}

final public class StructExtractInst : SingleValueInstruction {
  public var structOp: Value { operands[0].value }
  public var fieldIndex: Int { StructExtractInst_fieldIndex(bridged) }
}

final public class StructElementAddrInst : SingleValueInstruction {
  public var structAddress: Value { operands[0].value }
  public var fieldIndex: Int { StructElementAddrInst_fieldIndex(bridged) }
}

final public class EnumInst : SingleValueInstruction {
  public var payload: Value { operands[0].value }
  public var caseIndex: Int { EnumInst_caseIndex(bridged) }
}

final public class UncheckedEnumDataInst : SingleValueInstruction {
  public var enumOp: Value { operands[0].value }
  public var caseIndex: Int { UncheckedEnumDataInst_caseIndex(bridged) }
}

final public class RefElementAddrInst : SingleValueInstruction {
  public var reference: Value { operands[0].value }
  public var fieldIndex: Int { RefElementAddrInst_fieldIndex(bridged) }
}

final public class RefTailAddrInst : SingleValueInstruction {
  public var reference: Value { operands[0].value }
}

final public class UnconditionalCheckedCastInst : SingleValueInstruction {
  public override var mayTrap: Bool { true }
}

final public class UnconditionalCheckedCastValueInst : SingleValueInstruction {
  public override var mayTrap: Bool { true }
}

final public class BeginAccessInst : SingleValueInstruction {
  public var address: Value { operands[0].value }
}

final public class BeginBorrowInst : SingleValueInstruction {
  public var source: Value { operands[0].value }
}

final public class CopyValueInst : SingleValueInstruction {
  public var source: Value { operands[0].value }
}

final public class ClassifyBridgeObjectInst : SingleValueInstruction {
  public var reference: Value { operands[0].value }
}

final public class PartialApplyInst : SingleValueInstruction, ApplySite {
  public var numArguments: Int { PartialApplyInst_numArguments(bridged) }
}

final public class ApplyInst : SingleValueInstruction, FullApplySite {
  public var numArguments: Int { ApplyInst_numArguments(bridged) }
  
  public var singleDirectResult: Value? { self }
}

//===----------------------------------------------------------------------===//
//                      single-value allocation instructions
//===----------------------------------------------------------------------===//

public protocol Allocation : AnyObject { }

final public class AllocStackInst : SingleValueInstruction, Allocation {
}

final public class AllocRefInst : SingleValueInstruction, Allocation {
}

final public class AllocRefDynamicInst : SingleValueInstruction, Allocation {
}

final public class AllocValueBufferInst : SingleValueInstruction, Allocation {
}

final public class AllocBoxInst : SingleValueInstruction, Allocation {
}

final public class AllocExistentialBoxInst : SingleValueInstruction, Allocation {
}

//===----------------------------------------------------------------------===//
//                            multi-value instructions
//===----------------------------------------------------------------------===//

final public class BeginCOWMutationInst : MultipleValueInstruction {
}

final public class DestructureStructInst : MultipleValueInstruction {
}

final public class DestructureTupleInst : MultipleValueInstruction {
}

final public class BeginApplyInst : MultipleValueInstruction, FullApplySite {
  public var numArguments: Int { BeginApplyInst_numArguments(bridged) }
  
  public var singleDirectResult: Value? { nil }
}

//===----------------------------------------------------------------------===//
//                            terminator instructions
//===----------------------------------------------------------------------===//

public class TermInst : Instruction {
  final public var successors: SuccessorArray {
    SuccessorArray(succArray: TermInst_getSuccessors(bridged))
  }
}

final public class UnreachableInst : TermInst {
}

final public class ReturnInst : TermInst {
}

final public class ThrowInst : TermInst {
}

final public class YieldInst : TermInst {
}

final public class UnwindInst : TermInst {
}

final public class TryApplyInst : TermInst, FullApplySite {
  public var numArguments: Int { TryApplyInst_numArguments(bridged) }
  
  public var normalBlock: BasicBlock { successors[0] }
  public var errorBlock: BasicBlock { successors[1] }
  
  public var singleDirectResult: Value? { normalBlock.arguments[0] }
}

final public class BranchInst : TermInst {
  public var targetBlock: BasicBlock { BranchInst_getTargetBlock(bridged).block }
  
  public func getArgument(for operand: Operand) -> Argument {
    return targetBlock.arguments[operand.index]
  }
}

final public class CondBranchInst : TermInst {
}

final public class SwitchValueInst : TermInst {
}

final public class SwitchEnumInst : TermInst {

  public var enumOp: Value { operands[0].value }

  public struct CaseIndexArray : RandomAccessCollection {
    fileprivate let switchEnum: SwitchEnumInst

    public var startIndex: Int { return 0 }
    public var endIndex: Int { SwitchEnumInst_getNumCases(switchEnum.bridged) }

    public subscript(_ index: Int) -> Int {
      SwitchEnumInst_getCaseIndex(switchEnum.bridged, index)
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

final public class SwitchEnumAddrInst : TermInst {
}

final public class DynamicMethodBranchInst : TermInst {
}

final public class AwaitAsyncContinuationInst : TermInst {
}

final public class CheckedCastBranchInst : TermInst {
}

final public class CheckedCastAddrBranchInst : TermInst {
}

final public class CheckedCastValueBranchInst : TermInst {
}

