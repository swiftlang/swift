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

public class Instruction: ListNode, CustomStringConvertible, Hashable {
  public final var next: Instruction? {
    SILInstruction_next(bridged).instruction
  }

  public final var previous: Instruction? {
    SILInstruction_previous(bridged).instruction
  }

  // Needed for ReverseList<Instruction>.reversed(). Never use directly.
  public var _firstInList: Instruction { SILBasicBlock_firstInst(block.bridged).instruction! }
  // Needed for List<Instruction>.reversed(). Never use directly.
  public var _lastInList: Instruction { SILBasicBlock_lastInst(block.bridged).instruction! }

  public final var block: BasicBlock {
    SILInstruction_getParent(bridged).block
  }

  public final var function: Function { block.function }

  public final var description: String {
    SILNode_debugDescription(bridgedNode).takeString()
  }

  public final var operands: OperandArray {
    return OperandArray(opArray: SILInstruction_getOperands(bridged))
  }

  fileprivate var resultCount: Int { 0 }
  fileprivate func getResult(index _: Int) -> Value { fatalError() }

  public struct Results: RandomAccessCollection {
    fileprivate let inst: Instruction
    fileprivate let numResults: Int

    public var startIndex: Int { 0 }
    public var endIndex: Int { numResults }
    public subscript(_ index: Int) -> Value { inst.getResult(index: index) }
  }

  public final var results: Results {
    Results(inst: self, numResults: resultCount)
  }

  public final var location: Location {
    return Location(bridgedLocation: SILInstruction_getLocation(bridged))
  }

  public var mayTrap: Bool { false }

  public final var mayHaveSideEffects: Bool {
    return mayTrap || mayWriteToMemory
  }

  public final var mayReadFromMemory: Bool {
    switch SILInstruction_getMemBehavior(bridged) {
    case MayReadBehavior, MayReadWriteBehavior, MayHaveSideEffectsBehavior:
      return true
    default:
      return false
    }
  }

  public final var mayWriteToMemory: Bool {
    switch SILInstruction_getMemBehavior(bridged) {
    case MayWriteBehavior, MayReadWriteBehavior, MayHaveSideEffectsBehavior:
      return true
    default:
      return false
    }
  }

  public final var mayReadOrWriteMemory: Bool {
    switch SILInstruction_getMemBehavior(bridged) {
    case MayReadBehavior, MayWriteBehavior, MayReadWriteBehavior,
         MayHaveSideEffectsBehavior:
      return true
    default:
      return false
    }
  }

  public final var mayReadRefCount: Bool {
    switch self {
    case is IsUniqueInst, is BeginCOWMutationInst, is IsEscapingClosureInst:
      return true
    default:
      return false
    }
  }

  public final var mayRelease: Bool {
    return SILInstruction_mayRelease(bridged)
  }

  public static func == (lhs: Instruction, rhs: Instruction) -> Bool {
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

public extension BridgedInstruction {
  var instruction: Instruction { obj.getAs(Instruction.self) }
  func getAs<T: Instruction>(_: T.Type) -> T { obj.getAs(T.self) }
}

extension OptionalBridgedInstruction {
  var instruction: Instruction? { obj.getAs(Instruction.self) }
}

public class SingleValueInstruction: Instruction, Value {
  public final var definingInstruction: Instruction? { self }

  override fileprivate final var resultCount: Int { 1 }
  override fileprivate final func getResult(index _: Int) -> Value { self }
}

public final class MultipleValueInstructionResult: Value {
  public final var description: String {
    SILNode_debugDescription(bridgedNode).takeString()
  }

  public var instruction: Instruction {
    MultiValueInstResult_getParent(bridged).instruction
  }

  public var definingInstruction: Instruction? { instruction }

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

public class MultipleValueInstruction: Instruction {
  override fileprivate final var resultCount: Int {
    return MultipleValueInstruction_getNumResults(bridged)
  }

  override fileprivate final func getResult(index: Int) -> Value {
    MultipleValueInstruction_getResult(bridged, index).result
  }
}

/// Instructions, which have a single operand.
public protocol UnaryInstruction: AnyObject {
  var operands: OperandArray { get }
  var operand: Value { get }
}

public extension UnaryInstruction {
  var operand: Value { operands[0].value }
}

//===----------------------------------------------------------------------===//
//                             no-value instructions
//===----------------------------------------------------------------------===//

/// Used for all non-value instructions which are not implemented here, yet.
/// See registerBridgedClass() in SILBridgingUtils.cpp.
public final class UnimplementedInstruction: Instruction {}

public final class StoreInst: Instruction {
  public var sourceOperand: Operand { return operands[0] }
  public var destinationOperand: Operand { return operands[1] }
  public var source: Value { return sourceOperand.value }
  public var destination: Value { return destinationOperand.value }

  // must match with enum class StoreOwnershipQualifier
  public enum StoreOwnership: Int {
    case unqualified = 0, initialize = 1, assign = 2, trivial = 3
  }

  public var destinationOwnership: StoreOwnership {
    StoreOwnership(rawValue: StoreInst_getStoreOwnership(bridged))!
  }
}

public final class CopyAddrInst: Instruction {
  public var sourceOperand: Operand { return operands[0] }
  public var destinationOperand: Operand { return operands[1] }
  public var source: Value { return sourceOperand.value }
  public var destination: Value { return destinationOperand.value }
}

public final class EndAccessInst: Instruction, UnaryInstruction {
  public var beginAccess: BeginAccessInst {
    return operand as! BeginAccessInst
  }
}

public final class EndBorrowInst: Instruction, UnaryInstruction {}

public final class DeallocStackInst: Instruction, UnaryInstruction {
  public var allocStack: AllocStackInst {
    return operand as! AllocStackInst
  }
}

public final class DeallocStackRefInst: Instruction, UnaryInstruction {
  public var allocRef: AllocRefInst { operand as! AllocRefInst }
}

public final class CondFailInst: Instruction, UnaryInstruction {
  override public var mayTrap: Bool { true }

  public var message: String { CondFailInst_getMessage(bridged).string }
}

public final class FixLifetimeInst: Instruction, UnaryInstruction {}

public final class DebugValueInst: Instruction, UnaryInstruction {}

public final class UnconditionalCheckedCastAddrInst: Instruction {
  override public var mayTrap: Bool { true }
}

public final class SetDeallocatingInst: Instruction, UnaryInstruction {}

public final class DeallocRefInst: Instruction, UnaryInstruction {}

public class RefCountingInst: Instruction, UnaryInstruction {
  public var isAtomic: Bool { RefCountingInst_getIsAtomic(bridged) }
}

public final class StrongRetainInst: RefCountingInst {}

public final class RetainValueInst: RefCountingInst {}

public final class StrongReleaseInst: RefCountingInst {}

public final class ReleaseValueInst: RefCountingInst {}

public final class DestroyValueInst: Instruction, UnaryInstruction {}

public final class DestroyAddrInst: Instruction, UnaryInstruction {}

public final class UnimplementedRefCountingInst: RefCountingInst {}

//===----------------------------------------------------------------------===//
//                           single-value instructions
//===----------------------------------------------------------------------===//

/// Used for all SingleValueInstructions which are not implemented here, yet.
/// See registerBridgedClass() in SILBridgingUtils.cpp.
public final class UnimplementedSingleValueInst: SingleValueInstruction {}

public final class LoadInst: SingleValueInstruction, UnaryInstruction {}

public final class LoadBorrowInst: SingleValueInstruction, UnaryInstruction {}

public final class BuiltinInst: SingleValueInstruction {
  // TODO: find a way to directly reuse the BuiltinValueKind enum
  public enum ID {
    case None
    case DestroyArray
  }

  public var id: ID? {
    switch BuiltinInst_getID(bridged) {
    case DestroyArrayBuiltin: return .DestroyArray
    default: return .None
    }
  }
}

public final class UpcastInst: SingleValueInstruction, UnaryInstruction {}

public
final class UncheckedRefCastInst: SingleValueInstruction, UnaryInstruction {}

public
final class RawPointerToRefInst: SingleValueInstruction, UnaryInstruction {}

public
final class AddressToPointerInst: SingleValueInstruction, UnaryInstruction {}

public
final class PointerToAddressInst: SingleValueInstruction, UnaryInstruction {}

public
final class IndexAddrInst: SingleValueInstruction {}

public
final class InitExistentialRefInst: SingleValueInstruction, UnaryInstruction {}

public
final class OpenExistentialRefInst: SingleValueInstruction, UnaryInstruction {}

public
final class InitExistentialValueInst: SingleValueInstruction, UnaryInstruction {}

public
final class OpenExistentialValueInst: SingleValueInstruction, UnaryInstruction {}

public
final class InitExistentialAddrInst: SingleValueInstruction, UnaryInstruction {}

public
final class OpenExistentialAddrInst: SingleValueInstruction, UnaryInstruction {}

public
final class OpenExistentialBoxInst: SingleValueInstruction, UnaryInstruction {}

public
final class OpenExistentialBoxValueInst: SingleValueInstruction, UnaryInstruction {}

public
final class InitExistentialMetatypeInst: SingleValueInstruction, UnaryInstruction {}

public
final class OpenExistentialMetatypeInst: SingleValueInstruction, UnaryInstruction {}

public
final class ValueMetatypeInst: SingleValueInstruction, UnaryInstruction {}

public
final class ExistentialMetatypeInst: SingleValueInstruction, UnaryInstruction {}

public class GlobalAccessInst: SingleValueInstruction {
  public final var global: GlobalVariable {
    GlobalAccessInst_getGlobal(bridged).globalVar
  }
}

public final class FunctionRefInst: GlobalAccessInst {
  public var referencedFunction: Function {
    FunctionRefInst_getReferencedFunction(bridged).function
  }
}

public final class GlobalAddrInst: GlobalAccessInst {}

public final class GlobalValueInst: GlobalAccessInst {}

public final class IntegerLiteralInst: SingleValueInstruction {}

public final class TupleInst: SingleValueInstruction {}

public final class TupleExtractInst: SingleValueInstruction, UnaryInstruction {
  public var fieldIndex: Int { TupleExtractInst_fieldIndex(bridged) }
}

public
final class TupleElementAddrInst: SingleValueInstruction, UnaryInstruction {
  public var fieldIndex: Int { TupleElementAddrInst_fieldIndex(bridged) }
}

public final class StructInst: SingleValueInstruction {
  public var uniqueNonTrivialFieldValue: Value? {
    StructInst_getUniqueNonTrivialFieldValue(bridged).value
  }
}

public final class StructExtractInst: SingleValueInstruction, UnaryInstruction {
  public var fieldIndex: Int { StructExtractInst_fieldIndex(bridged) }
  public var isFieldOnlyNonTrivialField: Bool {
    StructExtractInst_isFieldOnlyNonTrivialField(bridged)
  }
}

public
final class StructElementAddrInst: SingleValueInstruction, UnaryInstruction {
  public var fieldIndex: Int { StructElementAddrInst_fieldIndex(bridged) }
}

public final class EnumInst: SingleValueInstruction {
  public var caseIndex: Int { EnumInst_caseIndex(bridged) }

  public var operand: Value? { operands.first?.value }
}

public
final class UncheckedEnumDataInst: SingleValueInstruction, UnaryInstruction {
  public var caseIndex: Int { UncheckedEnumDataInst_caseIndex(bridged) }
}

public final class RefElementAddrInst: SingleValueInstruction, UnaryInstruction {
  public var fieldIndex: Int { RefElementAddrInst_fieldIndex(bridged) }
}

public final class RefTailAddrInst: SingleValueInstruction, UnaryInstruction {}

public
final class UnconditionalCheckedCastInst: SingleValueInstruction, UnaryInstruction {
  override public var mayTrap: Bool { true }
}

public
final class UnconditionalCheckedCastValueInst: SingleValueInstruction,
  UnaryInstruction
{
  override public var mayTrap: Bool { true }
}

public
final class ConvertFunctionInst: SingleValueInstruction, UnaryInstruction {}

public
final class ThinToThickFunctionInst: SingleValueInstruction, UnaryInstruction {}

public
final class ObjCExistentialMetatypeToObjectInst: SingleValueInstruction,
  UnaryInstruction {}

public
final class ObjCMetatypeToObjectInst: SingleValueInstruction, UnaryInstruction {}

public
final class ValueToBridgeObjectInst: SingleValueInstruction, UnaryInstruction {}

public
final class MarkDependenceInst: SingleValueInstruction {
  public var value: Value { return operands[0].value }
  public var base: Value { return operands[1].value }
}

public final class BridgeObjectToRefInst: SingleValueInstruction,
  UnaryInstruction {}

public final class BeginAccessInst: SingleValueInstruction, UnaryInstruction {}

public final class BeginBorrowInst: SingleValueInstruction, UnaryInstruction {}

public final class CopyValueInst: SingleValueInstruction, UnaryInstruction {}

public final class EndCOWMutationInst: SingleValueInstruction, UnaryInstruction {}

public
final class ClassifyBridgeObjectInst: SingleValueInstruction, UnaryInstruction {}

public final class PartialApplyInst: SingleValueInstruction, ApplySite {
  public var numArguments: Int { PartialApplyInst_numArguments(bridged) }
}

public final class ApplyInst: SingleValueInstruction, FullApplySite {
  public var numArguments: Int { ApplyInst_numArguments(bridged) }

  public var singleDirectResult: Value? { self }
}

public final class ClassMethodInst: SingleValueInstruction, UnaryInstruction {}

public final class SuperMethodInst: SingleValueInstruction, UnaryInstruction {}

public final class ObjCMethodInst: SingleValueInstruction, UnaryInstruction {}

public final class ObjCSuperMethodInst: SingleValueInstruction, UnaryInstruction {}

public final class WitnessMethodInst: SingleValueInstruction {}

public final class IsUniqueInst: SingleValueInstruction, UnaryInstruction {}

public final class IsEscapingClosureInst: SingleValueInstruction, UnaryInstruction {}

//===----------------------------------------------------------------------===//
//                      single-value allocation instructions
//===----------------------------------------------------------------------===//

public protocol Allocation: AnyObject {}

public final class AllocStackInst: SingleValueInstruction, Allocation {}

public class AllocRefInstBase: SingleValueInstruction, Allocation {
  public final var isObjC: Bool { AllocRefInstBase_isObjc(bridged) != 0 }

  public final var canAllocOnStack: Bool {
    AllocRefInstBase_canAllocOnStack(bridged) != 0
  }
}

public final class AllocRefInst: AllocRefInstBase {}

public final class AllocRefDynamicInst: AllocRefInstBase {}

public final class AllocBoxInst: SingleValueInstruction, Allocation {}

public final class AllocExistentialBoxInst: SingleValueInstruction, Allocation {}

//===----------------------------------------------------------------------===//
//                            multi-value instructions
//===----------------------------------------------------------------------===//

public final class BeginCOWMutationInst: MultipleValueInstruction,
  UnaryInstruction
{
  public var uniquenessResult: Value { return getResult(index: 0) }
  public var bufferResult: Value { return getResult(index: 1) }
}

public final class DestructureStructInst: MultipleValueInstruction {}

public final class DestructureTupleInst: MultipleValueInstruction {}

public final class BeginApplyInst: MultipleValueInstruction, FullApplySite {
  public var numArguments: Int { BeginApplyInst_numArguments(bridged) }

  public var singleDirectResult: Value? { nil }
}

public final class RefToBridgeObjectInst: MultipleValueInstruction {}

//===----------------------------------------------------------------------===//
//                            terminator instructions
//===----------------------------------------------------------------------===//

public class TermInst: Instruction {
  public final var successors: SuccessorArray {
    SuccessorArray(succArray: TermInst_getSuccessors(bridged))
  }
}

public final class UnreachableInst: TermInst {}

public final class ReturnInst: TermInst, UnaryInstruction {}

public final class ThrowInst: TermInst, UnaryInstruction {}

public final class YieldInst: TermInst {}

public final class UnwindInst: TermInst {}

public final class TryApplyInst: TermInst, FullApplySite {
  public var numArguments: Int { TryApplyInst_numArguments(bridged) }

  public var normalBlock: BasicBlock { successors[0] }
  public var errorBlock: BasicBlock { successors[1] }

  public var singleDirectResult: Value? { normalBlock.arguments[0] }
}

public final class BranchInst: TermInst {
  public var targetBlock: BasicBlock { BranchInst_getTargetBlock(bridged).block }

  public func getArgument(for operand: Operand) -> Argument {
    return targetBlock.arguments[operand.index]
  }
}

public final class CondBranchInst: TermInst {}

public final class SwitchValueInst: TermInst {}

public final class SwitchEnumInst: TermInst {
  public var enumOp: Value { operands[0].value }

  public struct CaseIndexArray: RandomAccessCollection {
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

public final class SwitchEnumAddrInst: TermInst {}

public final class DynamicMethodBranchInst: TermInst {}

public final class AwaitAsyncContinuationInst: TermInst, UnaryInstruction {}

public final class CheckedCastBranchInst: TermInst, UnaryInstruction {}

public final class CheckedCastAddrBranchInst: TermInst, UnaryInstruction {}

public final class CheckedCastValueBranchInst: TermInst, UnaryInstruction {}
