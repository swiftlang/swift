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

import Basic
import AST
import SILBridging

//===----------------------------------------------------------------------===//
//                       Instruction base classes
//===----------------------------------------------------------------------===//

@_semantics("arc.immortal")
public class Instruction : CustomStringConvertible, Hashable {
  final public var next: Instruction? {
    bridged.getNext().instruction
  }

  final public var previous: Instruction? {
    bridged.getPrevious().instruction
  }

  final public var parentBlock: BasicBlock {
    bridged.getParent().block
  }

  final public var parentFunction: Function { parentBlock.parentFunction }

  final public var description: String {
    return String(taking: bridged.getDebugDescription())
  }

  final public var isDeleted: Bool {
    return bridged.isDeleted()
  }

  final public var isInStaticInitializer: Bool { bridged.isInStaticInitializer() }

  final public var operands: OperandArray {
    let operands = bridged.getOperands()
    return OperandArray(base: operands.base, count: operands.count)
  }

  // All operands defined by the operation.
  // Returns the prefix of `operands` that does not include trailing type dependent operands.
  final public var definedOperands: OperandArray {
    let allOperands = operands
    let typeOperands = bridged.getTypeDependentOperands()
    return allOperands[0 ..< (allOperands.count - typeOperands.count)]
  }

  final public var typeDependentOperands: OperandArray {
    let allOperands = operands
    let typeOperands = bridged.getTypeDependentOperands()
    return allOperands[(allOperands.count - typeOperands.count) ..< allOperands.count]
  }

  fileprivate var resultCount: Int { 0 }
  fileprivate func getResult(index: Int) -> Value { fatalError() }

  public struct Results : RandomAccessCollection {
    fileprivate let inst: Instruction
    fileprivate let numResults: Int

    public var startIndex: Int { 0 }
    public var endIndex: Int { numResults }
    public subscript(_ index: Int) -> Value { inst.getResult(index: index) }
  }

  final public var results: Results {
    Results(inst: self, numResults: resultCount)
  }

  final public var location: Location {
    return Location(bridged: bridged.getLocation())
  }

  public var mayTrap: Bool { false }

  final public var mayHaveSideEffects: Bool {
    return mayTrap || mayWriteToMemory
  }

  final public var memoryEffects: SideEffects.Memory {
    switch bridged.getMemBehavior() {
    case .None:
      return SideEffects.Memory()
    case .MayRead:
      return SideEffects.Memory(read: true)
    case .MayWrite:
      return SideEffects.Memory(write: true)
    case .MayReadWrite, .MayHaveSideEffects:
      return SideEffects.Memory(read: true, write: true)
    default:
      fatalError("invalid memory behavior")
    }
  }

  final public var mayReadFromMemory: Bool { memoryEffects.read }
  final public var mayWriteToMemory: Bool { memoryEffects.write }
  final public var mayReadOrWriteMemory: Bool { memoryEffects.read || memoryEffects.write }

  public final var maySuspend: Bool {
    return bridged.maySuspend()
  }

  public final var mayRelease: Bool {
    return bridged.mayRelease()
  }

  public final var hasUnspecifiedSideEffects: Bool {
    return bridged.mayHaveSideEffects()
  }

  public final var mayAccessPointer: Bool {
    return bridged.mayAccessPointer()
  }

  public final var mayLoadWeakOrUnowned: Bool {
    return bridged.mayLoadWeakOrUnowned()
  }

  public final var maySynchronize: Bool {
    return bridged.maySynchronize()
  }

  public final var mayBeDeinitBarrierNotConsideringSideEffects: Bool {
    return bridged.mayBeDeinitBarrierNotConsideringSideEffects()
  }

  public final var isEndOfScopeMarker: Bool {
    switch self {
    case is EndAccessInst, is EndBorrowInst:
      return true;
    default:
      return false;
    }
  }

  /// Incidental uses are marker instructions that do not propagate
  /// their operand.
  public final var isIncidentalUse: Bool {
    switch self {
    case is DebugValueInst, is FixLifetimeInst, is EndLifetimeInst,
         is IgnoredUseInst:
      return true
    default:
      return isEndOfScopeMarker
    }
  }

  public func visitReferencedFunctions(_ cl: (Function) -> ()) {
  }

  public static func ==(lhs: Instruction, rhs: Instruction) -> Bool {
    lhs === rhs
  }
  
  public func isIdenticalTo(_ otherInst: Instruction) -> Bool {
    return bridged.isIdenticalTo(otherInst.bridged)
  }

  public func hash(into hasher: inout Hasher) {
    hasher.combine(ObjectIdentifier(self))
  }

  public var bridged: BridgedInstruction {
    BridgedInstruction(SwiftObject(self))
  }
}

extension BridgedInstruction {
  public var instruction: Instruction { obj.getAs(Instruction.self) }
  public func getAs<T: Instruction>(_ instType: T.Type) -> T { obj.getAs(T.self) }
  public var optional: OptionalBridgedInstruction {
    OptionalBridgedInstruction(self.obj)
  }
}

extension OptionalBridgedInstruction {
  public var instruction: Instruction? { obj.getAs(Instruction.self) }

  public static var none: OptionalBridgedInstruction {
    OptionalBridgedInstruction()
  }
}

extension Optional where Wrapped == Instruction {
  public var bridged: OptionalBridgedInstruction {
    OptionalBridgedInstruction(self?.bridged.obj)
  }
}

public class SingleValueInstruction : Instruction, Value {
  final public var definingInstruction: Instruction? { self }

  fileprivate final override var resultCount: Int { 1 }
  fileprivate final override func getResult(index: Int) -> Value { self }

  public static func ==(lhs: SingleValueInstruction, rhs: SingleValueInstruction) -> Bool {
    lhs === rhs
  }

  public var isLexical: Bool { false }
}

public final class MultipleValueInstructionResult : Value, Hashable {
  public var parentInstruction: MultipleValueInstruction {
    bridged.getParent().getAs(MultipleValueInstruction.self)
  }

  public var definingInstruction: Instruction? { parentInstruction }

  public var parentBlock: BasicBlock { parentInstruction.parentBlock }

  public var isLexical: Bool { false }

  public var index: Int { bridged.getIndex() }

  var bridged: BridgedMultiValueResult {
    BridgedMultiValueResult(obj: SwiftObject(self))
  }

  public static func ==(lhs: MultipleValueInstructionResult, rhs: MultipleValueInstructionResult) -> Bool {
    lhs === rhs
  }

  public func hash(into hasher: inout Hasher) {
    hasher.combine(ObjectIdentifier(self))
  }
}

extension BridgedMultiValueResult {
  var result: MultipleValueInstructionResult {
    obj.getAs(MultipleValueInstructionResult.self)
  }
}

public class MultipleValueInstruction : Instruction {
  fileprivate final override var resultCount: Int {
    bridged.MultipleValueInstruction_getNumResults()
  }
  fileprivate final override func getResult(index: Int) -> Value {
    bridged.MultipleValueInstruction_getResult(index).result
  }
}

/// Instructions, which have a single operand (not including type-dependent operands).
public protocol UnaryInstruction : Instruction {
  var operand: Operand { get }
}

extension UnaryInstruction {
  public var operand: Operand { operands[0] }
}

//===----------------------------------------------------------------------===//
//                             no-value instructions
//===----------------------------------------------------------------------===//

/// Only one of the operands may have an address type.
public protocol StoringInstruction : Instruction {
  var operands: OperandArray { get }
}

extension StoringInstruction {
  public var sourceOperand: Operand { return operands[0] }
  public var destinationOperand: Operand { return operands[1] }
  public var source: Value { return sourceOperand.value }
  public var destination: Value { return destinationOperand.value }
}

final public class StoreInst : Instruction, StoringInstruction {
  // must match with enum class StoreOwnershipQualifier
  public enum StoreOwnership: Int {
    case unqualified = 0, initialize = 1, assign = 2, trivial = 3

    public init(for type: Type, in function: Function, initialize: Bool) {
      if function.hasOwnership {
        if type.isTrivial(in: function) {
          self = .trivial
        } else {
          self = initialize ? .initialize : .assign
        }
      } else {
        self = .unqualified
      }
    }
  }
  public var storeOwnership: StoreOwnership {
    StoreOwnership(rawValue: bridged.StoreInst_getStoreOwnership())!
  }
}

final public class StoreWeakInst : Instruction, StoringInstruction { }
final public class StoreUnownedInst : Instruction, StoringInstruction { }

final public class AssignInst : Instruction, StoringInstruction {
  // must match with enum class swift::AssignOwnershipQualifier
  public enum AssignOwnership: Int {
    case unknown = 0, reassign = 1, reinitialize = 2, initialize = 3
  }

  public var assignOwnership: AssignOwnership {
    AssignOwnership(rawValue: bridged.AssignInst_getAssignOwnership())!
  }
}

final public class AssignByWrapperInst : Instruction, StoringInstruction {}

final public class AssignOrInitInst : Instruction, StoringInstruction {}

/// Instruction that copy or move from a source to destination address.
public protocol SourceDestAddrInstruction : Instruction {
  var sourceOperand: Operand { get }
  var destinationOperand: Operand { get }
  var isTakeOfSource: Bool { get }
  var isInitializationOfDestination: Bool { get }
}

extension SourceDestAddrInstruction {
  public var sourceOperand: Operand { operands[0] }
  public var destinationOperand: Operand { operands[1] }
  public var source: Value { sourceOperand.value }
  public var destination: Value { destinationOperand.value }
}

final public class CopyAddrInst : Instruction, SourceDestAddrInstruction {
  public var isTakeOfSource: Bool {
    bridged.CopyAddrInst_isTakeOfSrc()
  }
  public var isInitializationOfDestination: Bool {
    bridged.CopyAddrInst_isInitializationOfDest()
  }
}

final public class ExplicitCopyAddrInst : Instruction, SourceDestAddrInstruction {
  public var source: Value { return sourceOperand.value }
  public var destination: Value { return destinationOperand.value }

  public var isTakeOfSource: Bool {
    bridged.ExplicitCopyAddrInst_isTakeOfSrc()
  }
  public var isInitializationOfDestination: Bool {
    bridged.ExplicitCopyAddrInst_isInitializationOfDest()
  }
}

final public class MarkUninitializedInst : SingleValueInstruction, UnaryInstruction {

  /// This enum captures what the mark_uninitialized instruction is designating.
  ///
  // Warning: this enum must be in sync with MarkUninitializedInst::Kind
  public enum Kind: Int {
    /// The start of a normal variable live range.
    case variable = 0

    /// "self" in a struct, enum, or root class.
    case rootSelf = 1

    /// The same as "RootSelf", but in a case where it's not really safe to treat 'self' as root
    /// because the original module might add more stored properties.
    ///
    /// This is only used for Swift 4 compatibility. In Swift 5, cross-module initializers are always delegatingSelf.
    case crossModuleRootSelf = 2

    /// "self" in a derived (non-root) class.
    case derivedSelf = 3

    /// "self" in a derived (non-root) class whose stored properties have already been initialized.
    case derivedSelfOnly = 4

    /// "self" on a struct, enum, or class in a delegating constructor (one that calls self.init).
    case delegatingSelf = 5

    /// "self" in a delegating class initializer where memory has already been allocated.
    case delegatingSelfAllocated = 6

    /// An indirectly returned result which has to be checked for initialization.
    case indirectResult = 7
  }

  public var kind: Kind { Kind(rawValue: bridged.MarkUninitializedInst_getKind())! }
}

final public class CondFailInst : Instruction, UnaryInstruction {
  public var condition: Value { operand.value }
  public override var mayTrap: Bool { true }

  public var message: StringRef { StringRef(bridged: bridged.CondFailInst_getMessage()) }
}

final public class IncrementProfilerCounterInst : Instruction {}

final public class MarkFunctionEscapeInst : Instruction {}

final public class HopToExecutorInst : Instruction, UnaryInstruction {}

final public class FixLifetimeInst : Instruction, UnaryInstruction {}

// See C++ VarDeclCarryingInst
public protocol VarDeclInstruction {
  var varDecl: VarDecl? { get }
}

/// A scoped instruction whose single result introduces a variable scope.
///
/// The scope-ending uses represent the end of the variable scope. This allows trivial 'let' variables to be treated
/// like a value with ownership. 'var' variables are primarily represented as addressable allocations via alloc_box or
/// alloc_stack, but may have redundant VariableScopeInstructions.
public enum VariableScopeInstruction {
  case beginBorrow(BeginBorrowInst)
  case moveValue(MoveValueInst)

  public init?(_ inst: Instruction?) {
    switch inst {
    case let bbi as BeginBorrowInst:
      guard bbi.isFromVarDecl else {
        return nil
      }
      self = .beginBorrow(bbi)
    case let mvi as MoveValueInst:
      guard mvi.isFromVarDecl else {
        return nil
      }
      self = .moveValue(mvi)
    default:
      return nil
    }
  }

  public var instruction: Instruction {
    switch self {
    case let .beginBorrow(bbi):
      return bbi
    case let .moveValue(mvi):
      return mvi
    }
  }

  public var scopeBegin: Value {
    instruction as! SingleValueInstruction
  }

  public var endOperands: LazyFilterSequence<UseList> {
    scopeBegin.uses.lazy.filter { $0.endsLifetime || $0.instruction is ExtendLifetimeInst }
  }

  // TODO: assert that VarDecl is valid whenever isFromVarDecl returns tyue.
  public func findVarDecl() -> VarDecl? {
    // SILGen may produce double var_decl instructions for the same variable:
    //   %box = alloc_box [var_decl] "x"
    //   begin_borrow %box [var_decl]
    //
    // Therefore, first check if begin_borrow or move_value has any debug_value users.
    if let debugVarDecl = instruction.findVarDeclFromDebugUsers() {
      return debugVarDecl
    }
    // Otherwise, assume that the var_decl is associated with its operand's var_decl.
    return instruction.operands[0].value.definingInstruction?.findVarDecl()
  }
}

extension Instruction {
  /// Find a variable declaration assoicated with this instruction.
  public func findVarDecl() -> VarDecl? {
    if let varDeclInst = self as? VarDeclInstruction {
      return varDeclInst.varDecl
    }
    if let varScopeInst = VariableScopeInstruction(self) {
      return varScopeInst.findVarDecl()
    }
    return findVarDeclFromDebugUsers()
  }

  func findVarDeclFromDebugUsers() -> VarDecl? {
    for result in results {
      if let varDecl = result.findVarDeclFromDebugUsers() {
        return varDecl
      }
    }
    return nil
  }
}

extension Value {
  public func findVarDecl() -> VarDecl? {
    if let arg = self as? Argument {
      return arg.findVarDecl()
    }
    return findVarDeclFromDebugUsers()
  }

  func findVarDeclFromDebugUsers() -> VarDecl? {
    for use in uses {
      if let debugVal = use.instruction as? DebugValueInst {
        return debugVal.varDecl
      }
    }
    return nil
  }
}

public protocol DebugVariableInstruction : VarDeclInstruction {
  typealias DebugVariable = BridgedSILDebugVariable

  var debugVariable: DebugVariable? { get }
}

/// A meta instruction is an instruction whose location is not interesting as
/// it is impossible to set a breakpoint on it.
/// That could be because the instruction does not generate code (such as
/// `debug_value`), or because the generated code would be in the prologue
/// (`alloc_stack`).
/// When we are moving code onto an unknown instruction (such as the start of a
/// basic block), we want to ignore any meta instruction that might be there.
public protocol MetaInstruction: Instruction {}

final public class DebugValueInst : Instruction, UnaryInstruction, DebugVariableInstruction, MetaInstruction {
  public var varDecl: VarDecl? {
    bridged.DebugValue_getDecl().getAs(VarDecl.self)
  }

  public var debugVariable: DebugVariable? {
    return bridged.DebugValue_hasVarInfo() ? bridged.DebugValue_getVarInfo() : nil
  }
}

final public class DebugStepInst : Instruction {}

final public class SpecifyTestInst : Instruction {}

final public class UnconditionalCheckedCastAddrInst : Instruction, SourceDestAddrInstruction {
  public var sourceFormalType: CanonicalType {
    CanonicalType(bridged: bridged.UnconditionalCheckedCastAddr_getSourceFormalType())
  }
  public var targetFormalType: CanonicalType {
    CanonicalType(bridged: bridged.UnconditionalCheckedCastAddr_getTargetFormalType())
  }

  public var isTakeOfSource: Bool { true }
  public var isInitializationOfDestination: Bool { true }
  public override var mayTrap: Bool { true }

  public var checkedCastOptions: CheckedCastInstOptions {
     .init(storage: bridged.UnconditionalCheckedCastAddr_getCheckedCastOptions().storage)
  }
}

final public class BeginDeallocRefInst : SingleValueInstruction, UnaryInstruction {
  public var reference: Value { operands[0].value }
  public var allocation: AllocRefInstBase { operands[1].value as! AllocRefInstBase }
}

final public class EndInitLetRefInst : SingleValueInstruction, UnaryInstruction {}

final public class BindMemoryInst : SingleValueInstruction {}
final public class RebindMemoryInst : SingleValueInstruction {}

public class RefCountingInst : Instruction, UnaryInstruction {
  public var isAtomic: Bool { bridged.RefCountingInst_getIsAtomic() }
}

final public class StrongRetainInst : RefCountingInst {
  public var instance: Value { operand.value }
}

final public class StrongRetainUnownedInst : RefCountingInst {}

final public class UnownedRetainInst : RefCountingInst {
  public var instance: Value { operand.value }
}

final public class RetainValueInst : RefCountingInst {
  public var value: Value { return operand.value }
}

final public class UnmanagedRetainValueInst : RefCountingInst {
  public var value: Value { return operand.value }
}

final public class RetainValueAddrInst : RefCountingInst {
}

final public class ReleaseValueAddrInst : RefCountingInst {
}

final public class StrongReleaseInst : RefCountingInst {
  public var instance: Value { operand.value }
}

final public class UnownedReleaseInst : RefCountingInst {
  public var instance: Value { operand.value }
}

final public class ReleaseValueInst : RefCountingInst {
  public var value: Value { return operand.value }
}

final public class UnmanagedReleaseValueInst : RefCountingInst {
  public var value: Value { return operand.value }
}

final public class AutoreleaseValueInst : RefCountingInst {}
final public class UnmanagedAutoreleaseValueInst : RefCountingInst {}

final public class DestroyValueInst : Instruction, UnaryInstruction {
  public var destroyedValue: Value { operand.value }

  /// True if this `destroy_value` is inside a dead-end block is only needed to formally
  /// end the lifetime of its operand.
  /// Such `destroy_value` instructions are lowered to no-ops.
  public var isDeadEnd: Bool { bridged.DestroyValueInst_isDeadEnd() }
}

final public class DestroyAddrInst : Instruction, UnaryInstruction {
  public var destroyedAddress: Value { operand.value }
}

final public class EndLifetimeInst : Instruction, UnaryInstruction {}

final public class ExtendLifetimeInst : Instruction, UnaryInstruction {}

final public class InjectEnumAddrInst : Instruction, UnaryInstruction, EnumInstruction {
  public var `enum`: Value { operand.value }
  public var caseIndex: Int { bridged.InjectEnumAddrInst_caseIndex() }
}

//===----------------------------------------------------------------------===//
//                      no-value deallocation instructions
//===----------------------------------------------------------------------===//

public protocol Deallocation : Instruction {
  var allocatedValue: Value { get }
}

extension Deallocation {
  public var allocatedValue: Value { operands[0].value }
}


final public class DeallocStackInst : Instruction, UnaryInstruction, Deallocation {}

final public class DeallocStackRefInst : Instruction, UnaryInstruction, Deallocation {
  public var allocRef: AllocRefInstBase { operand.value as! AllocRefInstBase }
}

final public class DeallocRefInst : Instruction, UnaryInstruction, Deallocation {}

final public class DeallocPartialRefInst : Instruction, Deallocation {}

final public class DeallocBoxInst : Instruction, UnaryInstruction, Deallocation {}

final public class DeallocExistentialBoxInst : Instruction, UnaryInstruction, Deallocation {}

//===----------------------------------------------------------------------===//
//                           single-value instructions
//===----------------------------------------------------------------------===//

public protocol LoadInstruction: SingleValueInstruction, UnaryInstruction {}

extension LoadInstruction {
  public var address: Value { operand.value }
}

final public class LoadInst : SingleValueInstruction, LoadInstruction {
  // must match with enum class LoadOwnershipQualifier
  public enum LoadOwnership: Int {
    case unqualified = 0, take = 1, copy = 2, trivial = 3
  }
  public var loadOwnership: LoadOwnership {
    LoadOwnership(rawValue: bridged.LoadInst_getLoadOwnership())!
  }
}

final public class LoadWeakInst : SingleValueInstruction, LoadInstruction {}
final public class LoadUnownedInst : SingleValueInstruction, LoadInstruction {}

final public class BuiltinInst : SingleValueInstruction {
  public typealias ID = BridgedInstruction.BuiltinValueKind

  public var id: ID {
    return bridged.BuiltinInst_getID()
  }

  public var name: StringRef {
    return StringRef(bridged: bridged.BuiltinInst_getName())
  }

  public var intrinsicID: BridgedInstruction.IntrinsicID {
    return bridged.BuiltinInst_getIntrinsicID()
  }

  public var substitutionMap: SubstitutionMap {
    SubstitutionMap(bridged: bridged.BuiltinInst_getSubstitutionMap())
  }

  public var arguments: LazyMapSequence<OperandArray, Value> {
    operands.values
  }
}

final public class UpcastInst : SingleValueInstruction, UnaryInstruction {
  public var fromInstance: Value { operand.value }
}

final public
class UncheckedRefCastInst : SingleValueInstruction, UnaryInstruction {
  public var fromInstance: Value { operand.value }
}

final public
class UncheckedRefCastAddrInst : Instruction, SourceDestAddrInstruction {
  public var isTakeOfSource: Bool { true }
  public var isInitializationOfDestination: Bool { true }
}

final public class UncheckedAddrCastInst : SingleValueInstruction, UnaryInstruction {
  public var fromAddress: Value { operand.value }
}

final public class UncheckedTrivialBitCastInst : SingleValueInstruction, UnaryInstruction {
  public var fromValue: Value { operand.value }
}

final public class UncheckedBitwiseCastInst : SingleValueInstruction, UnaryInstruction {}
final public class UncheckedValueCastInst : SingleValueInstruction, UnaryInstruction {}

final public class RefToRawPointerInst : SingleValueInstruction, UnaryInstruction {}
final public class RefToUnmanagedInst : SingleValueInstruction, UnaryInstruction {}
final public class RefToUnownedInst : SingleValueInstruction, UnaryInstruction {}
final public class UnmanagedToRefInst : SingleValueInstruction, UnaryInstruction {}
final public class UnownedToRefInst : SingleValueInstruction, UnaryInstruction {}

final public
class RawPointerToRefInst : SingleValueInstruction, UnaryInstruction {
  public var pointer: Value { operand.value }
}

final public
class AddressToPointerInst : SingleValueInstruction, UnaryInstruction {
  public var address: Value { operand.value }

  public var needsStackProtection: Bool {
    bridged.AddressToPointerInst_needsStackProtection()
  }
}

final public
class PointerToAddressInst : SingleValueInstruction, UnaryInstruction {
  public var pointer: Value { operand.value }
  public var isStrict: Bool { bridged.PointerToAddressInst_isStrict() }
  public var isInvariant: Bool { bridged.PointerToAddressInst_isInvariant() }

  public var alignment: Int? {
    let maybeAlign = bridged.PointerToAddressInst_getAlignment()
    if maybeAlign == 0 {
      return nil
    }
    return Int(exactly: maybeAlign)
  }
}

public protocol IndexingInstruction: SingleValueInstruction {
  var base: Value { get }
  var index: Value { get }
}

extension IndexingInstruction {
  public var base: Value { operands[0].value }
  public var index: Value { operands[1].value }
}

final public
class IndexAddrInst : SingleValueInstruction, IndexingInstruction {
  public var needsStackProtection: Bool {
    bridged.IndexAddrInst_needsStackProtection()
  }
}

final public class IndexRawPointerInst : SingleValueInstruction, IndexingInstruction {}

final public
class TailAddrInst : SingleValueInstruction, IndexingInstruction {}

final public
class InitExistentialRefInst : SingleValueInstruction, UnaryInstruction {
  public var instance: Value { operand.value }

  public var conformances: ConformanceArray {
    ConformanceArray(bridged: bridged.InitExistentialRefInst_getConformances())
  }

  public var formalConcreteType: CanonicalType {
    CanonicalType(bridged: bridged.InitExistentialRefInst_getFormalConcreteType())
  }
}

final public
class OpenExistentialRefInst : SingleValueInstruction, UnaryInstruction {
  public var existential: Value { operand.value }
}

final public
class InitExistentialValueInst : SingleValueInstruction, UnaryInstruction {}

final public
class OpenExistentialValueInst : SingleValueInstruction, UnaryInstruction {}

final public
class InitExistentialAddrInst : SingleValueInstruction, UnaryInstruction {}

final public
class DeinitExistentialAddrInst : Instruction, UnaryInstruction {}

final public
class DeinitExistentialValueInst : Instruction {}

final public
class OpenExistentialAddrInst : SingleValueInstruction, UnaryInstruction {
  public var isImmutable: Bool { bridged.OpenExistentialAddr_isImmutable() }
}

final public
class OpenExistentialBoxInst : SingleValueInstruction, UnaryInstruction {}

final public
class OpenExistentialBoxValueInst : SingleValueInstruction, UnaryInstruction {}

final public
class InitExistentialMetatypeInst : SingleValueInstruction, UnaryInstruction {
  public var metatype: Value { operand.value }
}

final public
class OpenExistentialMetatypeInst : SingleValueInstruction, UnaryInstruction {}

final public class MetatypeInst : SingleValueInstruction {}

final public
class ValueMetatypeInst : SingleValueInstruction, UnaryInstruction {}

final public
class ExistentialMetatypeInst : SingleValueInstruction, UnaryInstruction {}

final public class ObjCProtocolInst : SingleValueInstruction {}

final public class TypeValueInst: SingleValueInstruction, UnaryInstruction {
  public var paramType: CanonicalType {
    CanonicalType(bridged: bridged.TypeValueInst_getParamType())
  }

  /// Returns the value of the Integer type is known and fits into an `Int`.
  public var value: Int? {
    if paramType.isInteger {
      return paramType.valueOfInteger
    }
    return nil
  }
}

public class GlobalAccessInstruction : SingleValueInstruction {
  final public var global: GlobalVariable {
    bridged.GlobalAccessInst_getGlobal().globalVar
  }
}

public class FunctionRefBaseInst : SingleValueInstruction {
  public var referencedFunction: Function {
    bridged.FunctionRefBaseInst_getReferencedFunction().function
  }

  public override func visitReferencedFunctions(_ cl: (Function) -> ()) {
    cl(referencedFunction)
  }
}

final public class FunctionRefInst : FunctionRefBaseInst {
}

final public class DynamicFunctionRefInst : FunctionRefBaseInst {
}

final public class PreviousDynamicFunctionRefInst : FunctionRefBaseInst {
}

final public class GlobalAddrInst : GlobalAccessInstruction, VarDeclInstruction {
  public var varDecl: VarDecl? {
    bridged.GlobalAddr_getDecl().getAs(VarDecl.self)
  }

  public var dependencyToken: Value? {
    operands.count == 1 ? operands[0].value : nil
  }
}

final public class GlobalValueInst : GlobalAccessInstruction {
  public var isBare: Bool { bridged.GlobalValueInst_isBare() }
}

final public class BaseAddrForOffsetInst : SingleValueInstruction {}

final public class AllocGlobalInst : Instruction {
  public var global: GlobalVariable {
    bridged.AllocGlobalInst_getGlobal().globalVar
  }
}

final public class IntegerLiteralInst : SingleValueInstruction {
  public var value: Int? {
    let optionalInt = bridged.IntegerLiteralInst_getValue()
    if optionalInt.hasValue {
      return optionalInt.value
    }
    return nil
  }
}

final public class FloatLiteralInst : SingleValueInstruction {
}

final public class StringLiteralInst : SingleValueInstruction {
  public enum Encoding {
    case Bytes
    case UTF8
    /// UTF-8 encoding of an Objective-C selector.
    case ObjCSelector
    case UTF8_OSLOG
  }

  public var value: StringRef { StringRef(bridged: bridged.StringLiteralInst_getValue()) }

  public var encoding: Encoding {
    switch bridged.StringLiteralInst_getEncoding() {
    case 0: return .Bytes
    case 1: return .UTF8
    case 2: return .ObjCSelector
    case 3: return .UTF8_OSLOG
    default: fatalError("invalid encoding in StringLiteralInst")
    }
  }
}

final public class HasSymbolInst : SingleValueInstruction {}

final public class TupleInst : SingleValueInstruction {}

final public class TupleExtractInst : SingleValueInstruction, UnaryInstruction  {
  public var `tuple`: Value { operand.value }
  public var fieldIndex: Int { bridged.TupleExtractInst_fieldIndex() }
}

final public
class TupleElementAddrInst : SingleValueInstruction, UnaryInstruction {
  public var `tuple`: Value { operand.value }
  public var fieldIndex: Int { bridged.TupleElementAddrInst_fieldIndex() }
}

final public class TupleAddrConstructorInst : Instruction {
  public var destinationOperand: Operand { operands[0] }
}

final public class StructInst : SingleValueInstruction {
}

final public class StructExtractInst : SingleValueInstruction, UnaryInstruction {
  public var `struct`: Value { operand.value }
  public var fieldIndex: Int { bridged.StructExtractInst_fieldIndex() }
}

final public
class StructElementAddrInst : SingleValueInstruction, UnaryInstruction {
  public var `struct`: Value { operand.value }
  public var fieldIndex: Int { bridged.StructElementAddrInst_fieldIndex() }
}

public protocol EnumInstruction : AnyObject {
  var caseIndex: Int { get }
}

final public class EnumInst : SingleValueInstruction, EnumInstruction {
  public var caseIndex: Int { bridged.EnumInst_caseIndex() }

  public var operand: Operand? { operands.first }
  public var payload: Value? { operand?.value }
}

final public class UncheckedEnumDataInst : SingleValueInstruction, UnaryInstruction, EnumInstruction {
  public var `enum`: Value { operand.value }
  public var caseIndex: Int { bridged.UncheckedEnumDataInst_caseIndex() }
}

final public class InitEnumDataAddrInst : SingleValueInstruction, UnaryInstruction, EnumInstruction {
  public var `enum`: Value { operand.value }
  public var caseIndex: Int { bridged.InitEnumDataAddrInst_caseIndex() }
}

final public class UncheckedTakeEnumDataAddrInst : SingleValueInstruction, UnaryInstruction, EnumInstruction {
  public var `enum`: Value { operand.value }
  public var caseIndex: Int { bridged.UncheckedTakeEnumDataAddrInst_caseIndex() }
}

final public class SelectEnumInst : SingleValueInstruction {
  public var enumOperand: Operand { operands[0] }
}

final public class RefElementAddrInst : SingleValueInstruction, UnaryInstruction, VarDeclInstruction {
  public var instance: Value { operand.value }
  public var fieldIndex: Int { bridged.RefElementAddrInst_fieldIndex() }

  public var fieldIsLet: Bool { bridged.RefElementAddrInst_fieldIsLet() }

  public var isImmutable: Bool { bridged.RefElementAddrInst_isImmutable() }
  
  public var varDecl: VarDecl? {
    bridged.RefElementAddr_getDecl().getAs(VarDecl.self)
  }
}

final public class RefTailAddrInst : SingleValueInstruction, UnaryInstruction {
  public var instance: Value { operand.value }

  public var isImmutable: Bool { bridged.RefTailAddrInst_isImmutable() }
}

final public class KeyPathInst : SingleValueInstruction {
  public override func visitReferencedFunctions(_ cl: (Function) -> ()) {
    var results = BridgedInstruction.KeyPathFunctionResults()
    for componentIdx in 0..<bridged.KeyPathInst_getNumComponents() {
      bridged.KeyPathInst_getReferencedFunctions(componentIdx, &results)
      let numFuncs = results.numFunctions
      withUnsafePointer(to: &results) {
        $0.withMemoryRebound(to: BridgedFunction.self, capacity: numFuncs) {
          let functions = UnsafeBufferPointer(start: $0, count: numFuncs)
          for idx in 0..<numFuncs {
            cl(functions[idx].function)
          }
        }
      }
    }
  }
}

final public
class UnconditionalCheckedCastInst : SingleValueInstruction, UnaryInstruction {
  public override var mayTrap: Bool { true }
  
  public var sourceFormalType: CanonicalType {
    CanonicalType(bridged: bridged.UnconditionalCheckedCast_getSourceFormalType())
  }
  public var targetFormalType: CanonicalType {
    CanonicalType(bridged: bridged.UnconditionalCheckedCast_getTargetFormalType())
  }

  public var checkedCastOptions: CheckedCastInstOptions {
     .init(storage: bridged.UnconditionalCheckedCast_getCheckedCastOptions().storage)
  }
}

final public
class ConvertFunctionInst : SingleValueInstruction, UnaryInstruction {
  public var fromFunction: Value { operand.value }
  public var withoutActuallyEscaping: Bool { bridged.ConvertFunctionInst_withoutActuallyEscaping() }
}

final public
class ThinToThickFunctionInst : SingleValueInstruction, UnaryInstruction {
  public var callee: Value { operand.value }

  public var referencedFunction: Function? {
    if let fri = callee as? FunctionRefInst {
      return fri.referencedFunction
    }
    return nil
  }
}

final public class ThickToObjCMetatypeInst : SingleValueInstruction {}
final public class ObjCToThickMetatypeInst : SingleValueInstruction {}

final public class CopyBlockInst : SingleValueInstruction, UnaryInstruction {}
final public class CopyBlockWithoutEscapingInst : SingleValueInstruction {}

final public
class ConvertEscapeToNoEscapeInst : SingleValueInstruction, UnaryInstruction {
  public var fromFunction: Value { operand.value }
}

final public
class ObjCExistentialMetatypeToObjectInst : SingleValueInstruction {}

final public
class ObjCMetatypeToObjectInst : SingleValueInstruction, UnaryInstruction {}

final public
class ValueToBridgeObjectInst : SingleValueInstruction, UnaryInstruction {
  public var value: Value { return operand.value }
}

final public
class GetAsyncContinuationInst : SingleValueInstruction {}

final public
class GetAsyncContinuationAddrInst : SingleValueInstruction, UnaryInstruction {}

final public class ExtractExecutorInst : SingleValueInstruction {}

public enum MarkDependenceKind: Int32 {
  case Unresolved = 0
  case Escaping = 1
  case NonEscaping = 2
}

public protocol MarkDependenceInstruction: Instruction {
  var dependenceKind: MarkDependenceKind { get }
}

extension MarkDependenceInstruction {
  public var isNonEscaping: Bool { dependenceKind == .NonEscaping }
  public var isUnresolved: Bool { dependenceKind == .Unresolved }

  public var valueOrAddressOperand: Operand { operands[0] }
  public var valueOrAddress: Value { valueOrAddressOperand.value }
  public var baseOperand: Operand { operands[1] }
  public var base: Value { baseOperand.value }
}

final public class MarkDependenceInst : SingleValueInstruction, MarkDependenceInstruction {
  public var valueOperand: Operand { valueOrAddressOperand }
  public var value: Value { return valueOperand.value }

  public var dependenceKind: MarkDependenceKind {
    MarkDependenceKind(rawValue: bridged.MarkDependenceInst_dependenceKind().rawValue)!
  }

  public var hasScopedLifetime: Bool {
    return isNonEscaping && type.isObject && ownership == .owned && type.isEscapable(in: parentFunction)
  }
}

final public class MarkDependenceAddrInst : Instruction, MarkDependenceInstruction {
  public var addressOperand: Operand { valueOrAddressOperand }
  public var address: Value { return addressOperand.value }

  public var dependenceKind: MarkDependenceKind {
    MarkDependenceKind(rawValue: bridged.MarkDependenceAddrInst_dependenceKind().rawValue)!
  }
}

final public class RefToBridgeObjectInst : SingleValueInstruction {
  public var convertedOperand: Operand { operands[0] }
  public var maskOperand: Operand { operands[1] }
}

final public class BridgeObjectToRefInst : SingleValueInstruction, UnaryInstruction {}

final public class BridgeObjectToWordInst : SingleValueInstruction, UnaryInstruction {}

final public class BorrowedFromInst : SingleValueInstruction, BeginBorrowInstruction {
  public var borrowedValue: Value { operands[0].value }
  public var borrowedPhi: Phi { Phi(borrowedValue)! }
  public var enclosingOperands: OperandArray {
    let ops = operands
    return ops[1..<ops.count]
  }
  public var enclosingValues: LazyMapSequence<LazySequence<OperandArray>.Elements, Value> {
    enclosingOperands.values
  }

  public var scopeEndingOperands: LazyFilterSequence<UseList> { uses.endingLifetime }
}

final public class ProjectBoxInst : SingleValueInstruction, UnaryInstruction {
  public var box: Value { operand.value }
  public var fieldIndex: Int { bridged.ProjectBoxInst_fieldIndex() }
}

final public class ProjectExistentialBoxInst : SingleValueInstruction, UnaryInstruction {}

public protocol CopyingInstruction : SingleValueInstruction, UnaryInstruction, OwnershipTransitionInstruction {}

final public class CopyValueInst : SingleValueInstruction, CopyingInstruction {
  public var fromValue: Value { operand.value }
}

final public class ExplicitCopyValueInst : SingleValueInstruction, CopyingInstruction {
  public var fromValue: Value { operand.value }
}

final public class UnownedCopyValueInst : SingleValueInstruction, CopyingInstruction {}
final public class WeakCopyValueInst : SingleValueInstruction, CopyingInstruction {}

final public class UncheckedOwnershipConversionInst : SingleValueInstruction {}

final public class MoveValueInst : SingleValueInstruction, UnaryInstruction {
  public var fromValue: Value { operand.value }

  public override var isLexical: Bool { bridged.MoveValue_isLexical() }
  public var hasPointerEscape: Bool { bridged.MoveValue_hasPointerEscape() }
  public var isFromVarDecl: Bool { bridged.MoveValue_isFromVarDecl() }
}

final public class DropDeinitInst : SingleValueInstruction, UnaryInstruction {
  public var fromValue: Value { operand.value }
}

final public class StrongCopyUnownedValueInst : SingleValueInstruction, UnaryInstruction {}
final public class StrongCopyUnmanagedValueInst : SingleValueInstruction, UnaryInstruction  {}
final public class StrongCopyWeakValueInst : SingleValueInstruction, UnaryInstruction  {}

final public class EndCOWMutationInst : SingleValueInstruction, UnaryInstruction {
  public var instance: Value { operand.value }
  public var doKeepUnique: Bool { bridged.EndCOWMutationInst_doKeepUnique() }
}

final public class EndCOWMutationAddrInst : Instruction, UnaryInstruction {
  public var address: Value { operand.value }
}

final public
class ClassifyBridgeObjectInst : SingleValueInstruction, UnaryInstruction {}

final public class PartialApplyInst : SingleValueInstruction, ApplySite {
  public var numArguments: Int { bridged.PartialApplyInst_numArguments() }

  /// Warning: isOnStack returns false for all closures prior to ClosureLifetimeFixup, even if they capture on-stack
  /// addresses and need to be diagnosed as non-escaping closures. Use mayEscape to determine whether a closure is
  /// non-escaping prior to ClosureLifetimeFixup.
  public var isOnStack: Bool { bridged.PartialApplyInst_isOnStack() }

  // Warning: ClosureLifetimeFixup does not promote all non-escaping closures to on-stack. When that promotion fails, it
  // creates a fake destroy of the closure after the captured values that the closure depends on. This is invalid OSSA,
  // so OSSA utilities need to bail-out when isOnStack is false even if hasNoescapeCapture is true to avoid encoutering
  // an invalid nested lifetime.
  public var mayEscape: Bool { !isOnStack && !hasNoescapeCapture }

  /// True if this closure captures anything nonescaping.
  public var hasNoescapeCapture: Bool {
    if operandConventions.contains(.indirectInoutAliasable) {
      return true
    }
    return arguments.contains { $0.type.containsNoEscapeFunction }
  }

  public var hasUnknownResultIsolation: Bool { bridged.PartialApplyInst_hasUnknownResultIsolation() }
  public var unappliedArgumentCount: Int { bridged.PartialApply_getCalleeArgIndexOfFirstAppliedArg() }
  public var calleeConvention: ArgumentConvention { type.bridged.getCalleeConvention().convention }
}

final public class ApplyInst : SingleValueInstruction, FullApplySite {
  public var numArguments: Int { bridged.ApplyInst_numArguments() }

  public var singleDirectResult: Value? { self }
  public var singleDirectErrorResult: Value? { nil }

  public var isNonThrowing: Bool { bridged.ApplyInst_getNonThrowing() }
  public var isNonAsync: Bool { bridged.ApplyInst_getNonAsync() }

  public typealias SpecializationInfo = BridgedGenericSpecializationInformation

  public var specializationInfo: SpecializationInfo { bridged.ApplyInst_getSpecializationInfo() }
}

final public class FunctionExtractIsolationInst : SingleValueInstruction {}

final public class ClassMethodInst : SingleValueInstruction, UnaryInstruction {
  public var member: DeclRef { DeclRef(bridged: bridged.ClassMethodInst_getMember()) }
}

final public class SuperMethodInst : SingleValueInstruction, UnaryInstruction {}

final public class ObjCMethodInst : SingleValueInstruction, UnaryInstruction {}

final public class ObjCSuperMethodInst : SingleValueInstruction, UnaryInstruction {}

final public class WitnessMethodInst : SingleValueInstruction {
  public var member: DeclRef { DeclRef(bridged: bridged.WitnessMethodInst_getMember()) }
  public var lookupType: CanonicalType { CanonicalType(bridged: bridged.WitnessMethodInst_getLookupType()) }
  public var lookupProtocol: ProtocolDecl { bridged.WitnessMethodInst_getLookupProtocol().getAs(ProtocolDecl.self) }
  public var conformance: Conformance { Conformance(bridged: bridged.WitnessMethodInst_getConformance()) }
}

final public class IsUniqueInst : SingleValueInstruction, UnaryInstruction {}

final public class DestroyNotEscapedClosureInst : SingleValueInstruction, UnaryInstruction {}

final public class MarkUnresolvedNonCopyableValueInst: SingleValueInstruction, UnaryInstruction {
  // The raw values must match swift::MarkUnresolvedNonCopyableValueInst::CheckKind
  public enum CheckKind: Int {
    case invalid = 0

    /// A signal to the move only checker to perform checking that allows for
    /// this value to be consumed along its boundary (in the case of let/var
    /// semantics) and also written over in the case of var semantics. NOTE: Of
    /// course this still implies the value cannot be copied and can be consumed
    /// only once along all program paths.
    case consumableAndAssignable

    /// A signal to the move only checker to perform no consume or assign
    /// checking. This forces the result of this instruction owned value to
    /// never be consumed (for let/var semantics) or assigned over (for var
    /// semantics). Of course, we still allow for non-consuming uses.
    case noConsumeOrAssign

    /// A signal to the move checker that the given value cannot be consumed,
    /// but is allowed to be assigned over. This is used for situations like
    /// global_addr/ref_element_addr/closure escape where we do not want to
    /// allow for the user to take the value (leaving the memory in an
    /// uninitialized state), but we are ok with the user assigning a new value,
    /// completely assigning over the value at once.
    case assignableButNotConsumable

    /// A signal to the move checker that the given value cannot be consumed or
    /// assigned, but is allowed to be initialized. This is used for situations
    /// like class initializers.
    case initableButNotConsumable
  }

  var checkKind: CheckKind { CheckKind(rawValue: bridged.MarkUnresolvedNonCopyableValue_getCheckKind())! }
  var isStrict: Bool { bridged.MarkUnresolvedNonCopyableValue_isStrict() }
}

final public class MarkUnresolvedReferenceBindingInst : SingleValueInstruction {}

final public class MarkUnresolvedMoveAddrInst : Instruction, SourceDestAddrInstruction {
  public var isTakeOfSource: Bool { true }
  public var isInitializationOfDestination: Bool { true }
}

final public class CopyableToMoveOnlyWrapperValueInst: SingleValueInstruction, UnaryInstruction {}

final public class MoveOnlyWrapperToCopyableValueInst: SingleValueInstruction, UnaryInstruction {}

final public class MoveOnlyWrapperToCopyableBoxInst: SingleValueInstruction, UnaryInstruction {}

final public class CopyableToMoveOnlyWrapperAddrInst
  : SingleValueInstruction, UnaryInstruction {}

final public class MoveOnlyWrapperToCopyableAddrInst
  : SingleValueInstruction, UnaryInstruction {}

final public class ObjectInst : SingleValueInstruction {
  public var baseOperands: OperandArray {
    operands[0..<bridged.ObjectInst_getNumBaseElements()]
  }

  public var tailOperands: OperandArray {
    let ops = operands
    return ops[bridged.ObjectInst_getNumBaseElements()..<ops.endIndex]
  }
}

final public class VectorInst : SingleValueInstruction {
}

final public class VectorBaseAddrInst : SingleValueInstruction, UnaryInstruction {
  public var vector: Value { operand.value }
}

final public class DifferentiableFunctionInst: SingleValueInstruction {}

final public class LinearFunctionInst: SingleValueInstruction {}
final public class DifferentiableFunctionExtractInst: SingleValueInstruction {}
final public class LinearFunctionExtractInst: SingleValueInstruction {}
final public class DifferentiabilityWitnessFunctionInst: SingleValueInstruction {}

final public class ProjectBlockStorageInst: SingleValueInstruction, UnaryInstruction {}

final public class InitBlockStorageHeaderInst: SingleValueInstruction {}

//===----------------------------------------------------------------------===//
//                      single-value allocation instructions
//===----------------------------------------------------------------------===//

public protocol Allocation : SingleValueInstruction { }

final public class AllocStackInst : SingleValueInstruction, Allocation, DebugVariableInstruction, MetaInstruction {
  public var hasDynamicLifetime: Bool { bridged.AllocStackInst_hasDynamicLifetime() }
  public var isFromVarDecl: Bool { bridged.AllocStackInst_isFromVarDecl() }
  public var usesMoveableValueDebugInfo: Bool { bridged.AllocStackInst_usesMoveableValueDebugInfo() }
  public override var isLexical: Bool { bridged.AllocStackInst_isLexical() }

  public var varDecl: VarDecl? {
    bridged.AllocStack_getDecl().getAs(VarDecl.self)
  }

  public var debugVariable: DebugVariable? {
    return bridged.AllocStack_hasVarInfo() ? bridged.AllocStack_getVarInfo() : nil
  }

  public var deallocations: LazyMapSequence<LazyFilterSequence<UseList>, DeallocStackInst> {
    uses.users(ofType: DeallocStackInst.self)
  }
}

public class AllocRefInstBase : SingleValueInstruction, Allocation {
  final public var isObjC: Bool { bridged.AllocRefInstBase_isObjc() }

  final public var canAllocOnStack: Bool {
    bridged.AllocRefInstBase_canAllocOnStack()
  }

  final public var tailAllocatedCounts: OperandArray {
    let numTailTypes = bridged.AllocRefInstBase_getNumTailTypes()
    return operands[0..<numTailTypes]
  }

  final public var tailAllocatedTypes: TypeArray {
    TypeArray(bridged: bridged.AllocRefInstBase_getTailAllocatedTypes())
  }
}

final public class AllocRefInst : AllocRefInstBase {
  public var isBare: Bool { bridged.AllocRefInst_isBare() }
}

final public class AllocRefDynamicInst : AllocRefInstBase {
  public var isDynamicTypeDeinitAndSizeKnownEquivalentToBaseType: Bool {
    bridged.AllocRefDynamicInst_isDynamicTypeDeinitAndSizeKnownEquivalentToBaseType()
  }

  public var metatypeOperand: Operand {
    let numTailTypes = bridged.AllocRefInstBase_getNumTailTypes()
    return operands[numTailTypes]
  }
}

final public class AllocBoxInst : SingleValueInstruction, Allocation, DebugVariableInstruction {

  public var varDecl: VarDecl? {
    bridged.AllocBox_getDecl().getAs(VarDecl.self)
  }

  public var debugVariable: DebugVariable? {
    return bridged.AllocBox_hasVarInfo() ? bridged.AllocBox_getVarInfo() : nil
  }

  public var hasDynamicLifetime: Bool { bridged.AllocBoxInst_hasDynamicLifetime() }
}

final public class AllocExistentialBoxInst : SingleValueInstruction, Allocation {
}

//===----------------------------------------------------------------------===//
//                           scoped instructions
//===----------------------------------------------------------------------===//

/// An instruction whose side effects extend across a scope including other instructions. These are always paired with a
/// scope ending instruction such as `begin_access` (ending with `end_access`) and `begin_borrow` (ending with
/// `end_borrow`).
public protocol ScopedInstruction: Instruction {
  var scopeEndingOperands: LazyFilterSequence<UseList> { get }

  var endInstructions: EndInstructions { get }
}

extension Instruction {
  /// Return the sequence of use points of any instruction.
  public var endInstructions: EndInstructions {
    if let scopedInst = self as? ScopedInstruction {
      return .scoped(scopedInst.scopeEndingOperands.users)
    }
    return .single(self)
  }
}

/// Single-value instructions beginning a borrow-scope which end with an `end_borrow` or a branch to a re-borrow phi.
/// See also `BeginBorrowValue` which represents all kind of `Value`s which begin a borrow scope.
public protocol BeginBorrowInstruction : SingleValueInstruction, ScopedInstruction {
}

final public class EndBorrowInst : Instruction, UnaryInstruction {
  public var borrow: Value { operand.value }
}

final public class BeginBorrowInst : SingleValueInstruction, UnaryInstruction, BeginBorrowInstruction {
  public var borrowedValue: Value { operand.value }

  public override var isLexical: Bool { bridged.BeginBorrow_isLexical() }
  public var hasPointerEscape: Bool { bridged.BeginBorrow_hasPointerEscape() }
  public var isFromVarDecl: Bool { bridged.BeginBorrow_isFromVarDecl() }

  public var scopeEndingOperands: LazyFilterSequence<UseList> { uses.endingLifetime }
}

final public class LoadBorrowInst : SingleValueInstruction, LoadInstruction, BeginBorrowInstruction {

  // True if the invariants on `load_borrow` have not been checked and should not be strictly enforced.
  //
  // This can only occur during raw SIL before move-only checking occurs. Developers can write incorrect
  // code using noncopyable types that consumes or mutates a memory location while that location is borrowed,
  // but the move-only checker must diagnose those problems before canonical SIL is formed.
  public var isUnchecked: Bool { bridged.LoadBorrowInst_isUnchecked() }

  public var scopeEndingOperands: LazyFilterSequence<UseList> { uses.endingLifetime }
}

final public class StoreBorrowInst : SingleValueInstruction, StoringInstruction, BeginBorrowInstruction {
  public var allocStack: AllocStackInst {
    var dest = destination
    if let mark = dest as? MarkUnresolvedNonCopyableValueInst {
      dest = mark.operand.value
    }
    return dest as! AllocStackInst
  }

  public var scopeEndingOperands: LazyFilterSequence<UseList> {
    return self.uses.lazy.filter { $0.instruction is EndBorrowInst }
  }

  public var endBorrows: LazyMapSequence<LazyFilterSequence<UseList>, EndBorrowInst> {
    // A `store_borrow` is an address value.
    // Only `end_borrow`s (with this address operand) can end such a borrow scope.
    uses.users(ofType: EndBorrowInst.self)
  }
}

final public class BeginAccessInst : SingleValueInstruction, UnaryInstruction {
  // The raw values must match SILAccessKind.
  public enum AccessKind: Int {
    case `init` = 0
    case read = 1
    case modify = 2
    case `deinit` = 3
  }
  public var accessKind: AccessKind {
    AccessKind(rawValue: bridged.BeginAccessInst_getAccessKind())!
  }

  public var isStatic: Bool { bridged.BeginAccessInst_isStatic() }
  public var isUnsafe: Bool { bridged.BeginAccessInst_isUnsafe() }

  public var address: Value { operand.value }

  public typealias EndAccessInstructions = LazyMapSequence<LazyFilterSequence<UseList>, EndAccessInst>

  public var endAccessInstructions: EndAccessInstructions {
    scopeEndingOperands.map { $0.instruction as! EndAccessInst }
  }
}

final public class EndAccessInst : Instruction, UnaryInstruction {
  public var beginAccess: BeginAccessInst {
    return operand.value as! BeginAccessInst
  }
}

extension BeginAccessInst : ScopedInstruction {
  public var scopeEndingOperands: LazyFilterSequence<UseList> {
    return uses.lazy.filter { $0.instruction is EndAccessInst }
  }
}

// Unpaired accesses do not have a static scope, are generally unsupported by the optimizer, and should be avoided.
final public class BeginUnpairedAccessInst : Instruction {}

final public class EndUnpairedAccessInst : Instruction {}


final public class BeginApplyInst : MultipleValueInstruction, FullApplySite {
  public var numArguments: Int { bridged.BeginApplyInst_numArguments() }
  public var isCalleeAllocated: Bool { bridged.BeginApplyInst_isCalleeAllocated() }

  public var singleDirectResult: Value? { nil }
  public var singleDirectErrorResult: Value? { nil }

  public var token: Value { getResult(index: resultCount - (isCalleeAllocated ? 2 : 1)) }

  public var yieldedValues: Results {
    Results(inst: self, numResults: resultCount - (isCalleeAllocated ? 2 : 1))
  }
}

final public class EndApplyInst : SingleValueInstruction, UnaryInstruction {
  public var token: MultipleValueInstructionResult { operand.value as! MultipleValueInstructionResult }
  public var beginApply: BeginApplyInst { token.parentInstruction as! BeginApplyInst }
}

final public class AbortApplyInst : Instruction, UnaryInstruction {
  public var token: MultipleValueInstructionResult { operand.value as! MultipleValueInstructionResult }
  public var beginApply: BeginApplyInst { token.parentInstruction as! BeginApplyInst }
}

extension BeginApplyInst : ScopedInstruction {
  public var scopeEndingOperands: LazyFilterSequence<UseList> {
    return token.uses.lazy.filter { $0.isScopeEndingUse }
  }
}

/// A sequence representing the use points of an instruction for the purpose of liveness and the general
/// nesting of scopes.
///
/// Abstracts over simple single-use instructions vs. an instruction that is always paired with scope ending
/// instructions that denote the end of the scoped operation.
public enum EndInstructions: CollectionLikeSequence {
  public typealias EndScopedInstructions = LazyMapSequence<LazyFilterSequence<UseList>, Instruction>

  case single(Instruction)
  case scoped(EndScopedInstructions)

  public enum Iterator : IteratorProtocol {
    case single(Instruction?)
    case scoped(EndScopedInstructions.Iterator)

    public mutating func next() -> Instruction? {
      switch self {
      case let .single(inst):
        if let result = inst {
          self = .single(nil)
          return result
        }
        return nil
      case var .scoped(iter):
        let result = iter.next()
        self = .scoped(iter)
        return result
      }
    }
  }

  public func makeIterator() -> Iterator {
    switch self {
    case let .single(inst):
      return .single(inst)
    case let .scoped(endScoped):
      return .scoped(endScoped.makeIterator())
    }
  }
}

//===----------------------------------------------------------------------===//
//                            multi-value instructions
//===----------------------------------------------------------------------===//

final public class BeginCOWMutationInst : MultipleValueInstruction, UnaryInstruction {
  public var instance: Value { operand.value }
  public var uniquenessResult: Value { return getResult(index: 0) }
  public var instanceResult: Value { return getResult(index: 1) }
}

final public class DestructureStructInst : MultipleValueInstruction, UnaryInstruction {
  public var `struct`: Value { operand.value }
}

final public class DestructureTupleInst : MultipleValueInstruction, UnaryInstruction {
  public var `tuple`: Value { operand.value }
}

//===----------------------------------------------------------------------===//
//                           parameter pack instructions
//===----------------------------------------------------------------------===//

final public class AllocPackInst : SingleValueInstruction, Allocation {}
final public class AllocPackMetadataInst : SingleValueInstruction, Allocation {}

final public class DeallocPackInst : Instruction, UnaryInstruction, Deallocation {}
final public class DeallocPackMetadataInst : Instruction, Deallocation {}

final public class OpenPackElementInst : SingleValueInstruction {}
final public class PackLengthInst : SingleValueInstruction {}
final public class DynamicPackIndexInst : SingleValueInstruction {}
final public class PackPackIndexInst : SingleValueInstruction {}
final public class ScalarPackIndexInst : SingleValueInstruction {}

final public class TuplePackExtractInst: SingleValueInstruction {
  public var indexOperand: Operand { operands[0] }
  public var tupleOperand: Operand { operands[1] }
}

final public class TuplePackElementAddrInst: SingleValueInstruction {
  public var indexOperand: Operand { operands[0] }
  public var tupleOperand: Operand { operands[1] }
}

final public class PackElementGetInst: SingleValueInstruction {}

final public class PackElementSetInst: Instruction {}

//===----------------------------------------------------------------------===//
//                            terminator instructions
//===----------------------------------------------------------------------===//

public class TermInst : Instruction {
  final public var successors: SuccessorArray {
    let succArray = bridged.TermInst_getSuccessors()
    return SuccessorArray(base: succArray.base, count: succArray.count)
  }
  
  public var isFunctionExiting: Bool { false }
}

final public class UnreachableInst : TermInst {
}

final public class ReturnInst : TermInst, UnaryInstruction {
  public var returnedValue: Value { operand.value }
  public override var isFunctionExiting: Bool { true }
}

final public class ThrowInst : TermInst, UnaryInstruction {
  public var thrownValue: Value { operand.value }
  public override var isFunctionExiting: Bool { true }
}

final public class ThrowAddrInst : TermInst {
  public override var isFunctionExiting: Bool { true }
}

final public class YieldInst : TermInst {
  public func convention(of operand: Operand) -> ArgumentConvention {
    return bridged.YieldInst_getConvention(operand.bridged).convention
  }
}

final public class UnwindInst : TermInst {
  public override var isFunctionExiting: Bool { true }
}

final public class TryApplyInst : TermInst, FullApplySite {
  public var numArguments: Int { bridged.TryApplyInst_numArguments() }

  public var normalBlock: BasicBlock { successors[0] }
  public var errorBlock: BasicBlock { successors[1] }

  public var singleDirectResult: Value? { normalBlock.arguments[0] }
  public var singleDirectErrorResult: Value? { errorBlock.arguments[0] }

  public var isNonAsync: Bool { bridged.TryApplyInst_getNonAsync() }
  public var specializationInfo: ApplyInst.SpecializationInfo { bridged.TryApplyInst_getSpecializationInfo() }
}

final public class BranchInst : TermInst {
  public var targetBlock: BasicBlock { bridged.BranchInst_getTargetBlock().block }

  /// Returns the target block argument for the cond_br `operand`.
  public func getArgument(for operand: Operand) -> Argument {
    return targetBlock.arguments[operand.index]
  }
}

final public class CondBranchInst : TermInst {
  public var trueBlock: BasicBlock { successors[0] }
  public var falseBlock: BasicBlock { successors[1] }

  public var condition: Value { operands[0].value }

  public var trueOperands: OperandArray { operands[1..<(bridged.CondBranchInst_getNumTrueArgs() &+ 1)] }
  public var falseOperands: OperandArray {
    let ops = operands
    return ops[(bridged.CondBranchInst_getNumTrueArgs() &+ 1)..<ops.count]
  }

  /// Returns the true or false block argument for the cond_br `operand`.
  ///
  /// Return nil if `operand` is the condition itself.
  public func getArgument(for operand: Operand) -> Argument? {
    let opIdx = operand.index
    if opIdx == 0 {
      return nil
    }
    let argIdx = opIdx - 1
    let numTrueArgs = bridged.CondBranchInst_getNumTrueArgs()
    if (0..<numTrueArgs).contains(argIdx) {
      return trueBlock.arguments[argIdx]
    } else {
      return falseBlock.arguments[argIdx - numTrueArgs]
    }
  }
}

final public class SwitchValueInst : TermInst {
}

final public class SwitchEnumInst : TermInst {

  public var enumOp: Value { operands[0].value }

  public struct CaseIndexArray : RandomAccessCollection {
    fileprivate let switchEnum: SwitchEnumInst

    public var startIndex: Int { return 0 }
    public var endIndex: Int { switchEnum.bridged.SwitchEnumInst_getNumCases() }

    public subscript(_ index: Int) -> Int {
      switchEnum.bridged.SwitchEnumInst_getCaseIndex(index)
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

final public class SelectEnumAddrInst : SingleValueInstruction {
}

final public class DynamicMethodBranchInst : TermInst {
}

final public class AwaitAsyncContinuationInst : TermInst, UnaryInstruction {
}

public struct CheckedCastInstOptions {
  var storage: UInt8 = 0
  
  var bridged: BridgedInstruction.CheckedCastInstOptions {
    .init(storage: storage)
  }
  
  var isolatedConformances: CastingIsolatedConformances {
    return (storage & 0x01) != 0 ? .prohibit : .allow
  }
}

public enum CastingIsolatedConformances {
  case allow
  case prohibit
}

final public class CheckedCastBranchInst : TermInst, UnaryInstruction {
  public var source: Value { operand.value }
  public var successBlock: BasicBlock { bridged.CheckedCastBranch_getSuccessBlock().block }
  public var failureBlock: BasicBlock { bridged.CheckedCastBranch_getFailureBlock().block }

  public func updateSourceFormalTypeFromOperandLoweredType() {
    bridged.CheckedCastBranch_updateSourceFormalTypeFromOperandLoweredType()
  }

  public var checkedCastOptions: CheckedCastInstOptions {
     .init(storage: bridged.CheckedCastBranch_getCheckedCastOptions().storage)
  }
}

final public class CheckedCastAddrBranchInst : TermInst {
  public var source: Value { operands[0].value }
  public var destination: Value { operands[1].value }

  public var sourceFormalType: CanonicalType {
    CanonicalType(bridged: bridged.CheckedCastAddrBranch_getSourceFormalType())
  }
  public var targetFormalType: CanonicalType {
    CanonicalType(bridged: bridged.CheckedCastAddrBranch_getTargetFormalType())
  }

  public var successBlock: BasicBlock { bridged.CheckedCastAddrBranch_getSuccessBlock().block }
  public var failureBlock: BasicBlock { bridged.CheckedCastAddrBranch_getFailureBlock().block }

  public enum CastConsumptionKind {
    /// The source value is always taken, regardless of whether the cast
    /// succeeds.  That is, if the cast fails, the source value is
    /// destroyed.
    case TakeAlways

    /// The source value is taken only on a successful cast; otherwise,
    /// it is left in place.
    case TakeOnSuccess

    /// The source value is always left in place, and the destination
    /// value is copied into on success.
    case CopyOnSuccess
  }

  public var consumptionKind: CastConsumptionKind {
    switch bridged.CheckedCastAddrBranch_getConsumptionKind() {
    case .TakeAlways:    return .TakeAlways
    case .TakeOnSuccess: return .TakeOnSuccess
    case .CopyOnSuccess: return .CopyOnSuccess
    default:
      fatalError("invalid cast consumption kind")
    }
  }

  public var checkedCastOptions: CheckedCastInstOptions {
     .init(storage: bridged.CheckedCastAddrBranch_getCheckedCastOptions().storage)
  }
}

final public class ThunkInst : Instruction {
}

final public class MergeIsolationRegionInst : Instruction {
}

final public class IgnoredUseInst : Instruction, UnaryInstruction {
}
