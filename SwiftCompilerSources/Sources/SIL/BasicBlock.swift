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

import AST
import SILBridging

@_semantics("arc.immortal")
final public class BasicBlock : CustomStringConvertible, HasShortDescription, Hashable {
  public var next: BasicBlock? { bridged.getNext().block }
  public var previous: BasicBlock? { bridged.getPrevious().block }

  public var parentFunction: Function { bridged.getFunction().function }

  public var description: String {
    return String(taking: bridged.getDebugDescription())
  }
  public var shortDescription: String { name }

  /// The index of the basic block in its function.
  /// This has O(n) complexity. Only use it for debugging
  public var index: Int {
    for (idx, block) in parentFunction.blocks.enumerated() {
      if block == self { return idx }
    }
    fatalError()
  }

  public var name: String { "bb\(index)" }

  public static func == (lhs: BasicBlock, rhs: BasicBlock) -> Bool { lhs === rhs }

  public func hash(into hasher: inout Hasher) {
    hasher.combine(ObjectIdentifier(self))
  }

  public var bridged: BridgedBasicBlock { BridgedBasicBlock(SwiftObject(self)) }

  //===----------------------------------------------------------------------===//
  //                                  Arguments
  //===----------------------------------------------------------------------===//

  public var arguments: ArgumentArray { ArgumentArray(block: self) }

  public func addArgument(type: Type, ownership: Ownership, _ context: some MutatingContext) -> Argument {
    context.notifyInstructionsChanged()
    return bridged.addBlockArgument(type.bridged, ownership._bridged).argument
  }

  public func addFunctionArgument(type: Type, _ context: some MutatingContext) -> FunctionArgument {
    context.notifyInstructionsChanged()
    return bridged.addFunctionArgument(type.bridged).argument as! FunctionArgument
  }

  public func insertFunctionArgument(atPosition: Int, type: Type, ownership: Ownership, decl: ValueDecl? = nil,
                              _ context: some MutatingContext) -> FunctionArgument
  {
    context.notifyInstructionsChanged()
    return bridged.insertFunctionArgument(atPosition, type.bridged, ownership._bridged,
                                          (decl as Decl?).bridged).argument as! FunctionArgument
  }

  public func insertPhiArgument(
    atPosition: Int, type: Type, ownership: Ownership, _ context: some MutatingContext
  ) -> Argument {
    context.notifyInstructionsChanged()
    return bridged.insertPhiArgument(atPosition, type.bridged, ownership._bridged).argument
  }

  public func eraseArgument(at index: Int, _ context: some MutatingContext) {
    context.notifyInstructionsChanged()
    bridged.eraseArgument(index)
  }

  public func eraseAllArguments(_ context: some MutatingContext) {
    // Arguments are stored in an array. We need to erase in reverse order to avoid quadratic complexity.
    for argIdx in (0 ..< arguments.count).reversed() {
      eraseArgument(at: argIdx, context)
    }
  }

  public func moveAllArguments(to otherBlock: BasicBlock, _ context: some MutatingContext) {
    bridged.moveArgumentsTo(otherBlock.bridged)
  }

  //===----------------------------------------------------------------------===//
  //                               Instructions
  //===----------------------------------------------------------------------===//

  public var instructions: InstructionList {
    InstructionList(first: bridged.getFirstInst().instruction)
  }

  public var terminator: TermInst {
    bridged.getLastInst().instruction as! TermInst
  }

  public func moveAllInstructions(toBeginOf otherBlock: BasicBlock, _ context: some MutatingContext) {
    context.notifyInstructionsChanged()
    context.notifyBranchesChanged()
    bridged.moveAllInstructionsToBegin(otherBlock.bridged)
  }

  public func moveAllInstructions(toEndOf otherBlock: BasicBlock, _ context: some MutatingContext) {
    context.notifyInstructionsChanged()
    context.notifyBranchesChanged()
    bridged.moveAllInstructionsToEnd(otherBlock.bridged)
  }

  //===----------------------------------------------------------------------===//
  //                        predecessors and successors
  //===----------------------------------------------------------------------===//

  public var successors: SuccessorArray { terminator.successors }

  public var predecessors: PredecessorList {
    PredecessorList(startAt: bridged.getFirstPred())
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

  public var singleSuccessor: BasicBlock? {
    successors.count == 1 ? successors[0] : nil
  }

  /// All function exiting blocks except for ones with an `unreachable` terminator,
  /// not immediately preceded by an apply of a no-return function.
  public var isReachableExitBlock: Bool {
    switch terminator {
      case let termInst where termInst.isFunctionExiting:
        return true
      case is UnreachableInst:
        if let instBeforeUnreachable = terminator.previous,
           let ai = instBeforeUnreachable as? ApplyInst,
           ai.isCalleeNoReturn && !ai.isCalleeTrapNoReturn
        {
          return true
        }

        return false
      default:
        return false
    }
  }
  
  public func isCriticalEdge(edgeIndex: Int) -> Bool {
    if terminator.successors.count <= 1 {
      return false
    } else {
      return !terminator.successors[edgeIndex].hasSinglePredecessor
    }
  }
}

/// The list of instructions in a BasicBlock.
///
/// It's allowed to delete the current, next or any other instructions while
/// iterating over the instruction list.
public struct InstructionList : CollectionLikeSequence, IteratorProtocol {
  private var currentInstruction: Instruction?

  public init(first: Instruction?) { currentInstruction = first }

  public mutating func next() -> Instruction? {
    if var inst = currentInstruction {
      while inst.isDeleted {
        guard let nextInst = inst.next else {
          return nil
        }
        inst = nextInst
      }
      currentInstruction = inst.next
      return inst
    }
    return nil
  }

  public var first: Instruction? { currentInstruction }

  public var last: Instruction? { reversed().first }

  public func reversed() -> ReverseInstructionList {
    if let inst = currentInstruction {
      let lastInst = inst.bridged.getLastInstOfParent().instruction
      return ReverseInstructionList(first: lastInst)
    }
    return ReverseInstructionList(first: nil)
  }
}

/// The list of instructions in a BasicBlock in reverse order.
///
/// It's allowed to delete the current, next or any other instructions while
/// iterating over the instruction list.
public struct ReverseInstructionList : CollectionLikeSequence, IteratorProtocol {
  private var currentInstruction: Instruction?

  public init(first: Instruction?) { currentInstruction = first }

  public mutating func next() -> Instruction? {
    if var inst = currentInstruction {
      while inst.isDeleted {
        guard let nextInst = inst.previous else {
          return nil
        }
        inst = nextInst
      }
      currentInstruction = inst.previous
      return inst
    }
    return nil
  }

  public var first: Instruction? { currentInstruction }
}

public struct ArgumentArray : RandomAccessCollection {
  fileprivate let block: BasicBlock

  public var startIndex: Int { return 0 }
  public var endIndex: Int { block.bridged.getNumArguments() }

  public subscript(_ index: Int) -> Argument {
    block.bridged.getArgument(index).argument
  }
}

public struct SuccessorArray : RandomAccessCollection, FormattedLikeArray {
  private let base: OptionalBridgedSuccessor
  public let count: Int

  init(base: OptionalBridgedSuccessor, count: Int) {
    self.base = base
    self.count = count
  }

  public var startIndex: Int { return 0 }
  public var endIndex: Int { return count }

  public subscript(_ index: Int) -> BasicBlock {
    assert(index >= startIndex && index < endIndex)
    return base.advancedBy(index).getTargetBlock().block
  }
}

public struct PredecessorList : CollectionLikeSequence, IteratorProtocol {
  private var currentSucc: OptionalBridgedSuccessor

  public init(startAt: OptionalBridgedSuccessor) { currentSucc = startAt }

  public mutating func next() -> BasicBlock? {
    if let succ = currentSucc.successor {
      currentSucc = succ.getNext()
      return succ.getContainingInst().instruction.parentBlock
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

extension OptionalBridgedSuccessor {
  var successor: BridgedSuccessor? {
    if let succ = succ {
      return BridgedSuccessor(succ: succ)
    }
    return nil
  }
}

//===--------------------------------------------------------------------===//
//                              Tests
//===--------------------------------------------------------------------===//

/// Most basic test of a BasicBlock and its contents
let basicBlockTest = Test("basic_block") {
  function, arguments, context in

  print("run SILPrinter on function: \(function.name)")

  for (bbIdx, block) in function.blocks.enumerated() {
    print("bb\(bbIdx):")

    print("  predecessors: \(block.predecessors)")
    print("  successors:   \(block.successors)")

    print("  arguments:")
    for arg in block.arguments {
      print("    arg: \(arg)")
      for use in arg.uses {
        print("      user: \(use.instruction)")
      }
      if let phi = Phi(arg) {
        for incoming in phi.incomingValues {
          print("      incoming: \(incoming)")
        }
      }
    }

    print("  instructions:")
    for inst in block.instructions {
      print("  \(inst)")
      for op in inst.operands {
        print("      op: \(op.value)")
      }
      for (resultIdx, result) in inst.results.enumerated() {
        for use in result.uses {
          print("      user of result \(resultIdx): \(use.instruction)")
        }
      }
    }
  }
}

/// Tests instruction iteration while modifying the instruction list.
///
/// This test iterates over the instruction list of the function's block and performs
/// modifications of the instruction list - mostly deleting instructions.
/// Modifications are triggered by `string_literal` instructions with known "commands".
/// E.g. if a
/// ```
///   %1 = string_literal utf8 "delete_strings"
/// ```
/// is encountered during the iteration, it triggers the deletion of all `string_literal`
/// instructions of the basic block (including the current one).
///
let instructionIterationTest = Test("instruction_iteration") {
  function, arguments, context in

  print("Test instruction iteration in \(function.name):")

  let reverse = function.name.string.hasSuffix("backward")

  for block in function.blocks {
    print("\(block.name):")
    let termLoc = block.terminator.location
    if reverse {
      for inst in block.instructions.reversed() {
        handle(instruction: inst, context)
      }
    } else {
      for inst in block.instructions {
        handle(instruction: inst, context)
      }
    }
    if block.instructions.isEmpty || !(block.instructions.reversed().first is TermInst) {
      let builder = Builder(atEndOf: block, location: termLoc, context)
      builder.createUnreachable()
    }
  }
  print("End function \(function.name):")
}

private func handle(instruction: Instruction, _ context: TestContext) {
  print(instruction)
  if let sl = instruction as? StringLiteralInst {
    switch sl.value {
      case "delete_strings":
        deleteAllInstructions(ofType: StringLiteralInst.self, in: instruction.parentBlock, context)
      case "delete_ints":
        deleteAllInstructions(ofType: IntegerLiteralInst.self, in: instruction.parentBlock, context)
      case "delete_branches":
        deleteAllInstructions(ofType: BranchInst.self, in: instruction.parentBlock, context)
      case "split_block":
        _ = context.splitBlock(before: instruction)
      case "print_uses":
        for use in sl.uses {
          print("use: \(use)")
        }
      case "delete_first_user":
        deleteUser(of: sl, at: 0, context)
      case "delete_second_user":
        deleteUser(of: sl, at: 1, context)
      default:
        break
    }
  }
}

private func deleteAllInstructions<InstType: Instruction>(ofType: InstType.Type,
                                                          in block: BasicBlock,
                                                          _ context: TestContext)
{
  for inst in block.instructions {
    if inst is InstType {
      context.erase(instruction: inst)
    }
  }
}

private func deleteUser(of value: Value, at deleteIndex: Int, _ context: TestContext) {
  for (idx, use) in value.uses.enumerated() {
    if idx == deleteIndex {
      context.erase(instruction: use.instruction)
    } else {
      print("use: \(use)")
    }
  }
}
