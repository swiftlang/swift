//===--- DebugReconstructionBlockSimplification.swift ---------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import SIL
import OptimizerBridging

/// Instructions which can be simplified within a debug reconstruction block
/// Some additional debug reconstruction specific simplifications, such as
/// folding undef operands, are needed.
protocol DebugReconstructionBlockSimplifiable : Instruction {
  func simplifyForDebugReconstructionBlock(_ context: SimplifyContext)
}

extension DebugReconstructionBlockSimplifiable where Self: Simplifiable {
  func simplifyForDebugReconstructionBlock(_ context: SimplifyContext) {
    simplify(context)
  }
}

extension DebugReconstructionBlockSimplifiable where Self: SingleValueInstruction {
  /// Folds this instruction to `undef` if any of its operands is `undef`, and returns
  /// whether it was folded. The instruction is erased when folded.
  ///
  /// Only valid for instructions whose result carries no information of its own beyond
  /// its operands. It must not be used where folding would drop something the debugger
  /// could still have shown: `enum $E, #E.some, undef` keeps a known case, and
  /// `builtin "and"(undef, 0)` is strictly `0`.
  @discardableResult
  func foldUndefOperands(_ context: SimplifyContext) -> Bool {
    guard operands.contains(where: { $0.value is Undef }) else {
      return false
    }
    replaceWithUndef(context)
    return true
  }
}

let debugReconstructionBlockSimplificationPass = FunctionPass(name: "debug-reconstruction-block-simplification") {
  (function: Function, context: FunctionPassContext) in
  simplifyDebugReconstructionBlocks(in: function, context, useSubpasses: true)
}

/// Simplifies the content of every debug reconstruction block of `function`, so that a block
/// which reconstructs nothing but `undef` returns `undef` literally, and an `undef` which the
/// reconstruction does not depend on leaves no trace. This is what makes it possible to tell
/// a variable which is optimized away from one which is only partially described.
///
/// Dead operands are dropped and a reconstruction block which does nothing is removed. Both
/// replace the debug value with a new instruction.
///
/// `useSubpasses` reports each step as a subpass. It must be false when this does not run as
/// a pass of its own.
func simplifyDebugReconstructionBlocks(in function: Function, _ context: FunctionPassContext,
                                       useSubpasses: Bool) {
  for inst in function.instructions {
    guard let debugValue = inst as? DebugValueInst,
          debugValue.debugReconstructionBlock != nil else {
      continue
    }
    // Use a subpass for each debug_value (operand simplification) and for each instruction in the reconstruction block.
    if useSubpasses, !context.continueWithNextSubpassRun(for: debugValue) {
      return
    }
    _ = simplifyDebugReconstructionBlock(of: debugValue, context, useSubpasses: useSubpasses)
  }
}

/// Simplifies the debug reconstruction block of `debugValue`, see
/// `simplifyDebugReconstructionBlocks(in:_:useSubpasses:)`.
/// Returns the debug value, which is a new instruction if it was replaced.
func simplifyDebugReconstructionBlock(of debugValue: DebugValueInst, _ context: FunctionPassContext,
                                      useSubpasses: Bool) -> DebugValueInst {
  guard let debugBB = debugValue.debugReconstructionBlock else {
    return debugValue
  }
  var worklist = InstructionWorklist(context)
  defer { worklist.deinitialize() }

  let simplifyCtxt = context.createSimplifyContext(preserveDebugInfo: false,
                                                   notifyInstructionChanged: {
    worklist.pushIfNotVisited($0)
  })

  // Canonicalize the operand list first.
  debugValue.mergeDuplicateOperands(context)
  debugValue.propagateUndefOperands(context)

  // Simplify the block's content.
  for debugInst in debugBB.instructions.reversed() {
    worklist.pushIfNotVisited(debugInst)
  }
  while let instruction = worklist.popAndForget() {
    if instruction.isDeleted {
      continue
    }
    if let simplifiable = instruction as? DebugReconstructionBlockSimplifiable {
      if useSubpasses, !context.continueWithNextSubpassRun(for: instruction) {
        return debugValue
      }
      simplifiable.simplifyForDebugReconstructionBlock(simplifyCtxt)
    }
  }

  // Cleanup dead instructions and operands.
  for instruction in debugBB.instructions.reversed() where instruction.isTriviallyDead {
    context.erase(instruction: instruction)
  }
  return debugValue.eraseDeadOperands(context)
                   .collapseTrivialReconstruction(context)
}

private extension DebugValueInst {
  /// Merges duplicate operands.
  /// Duplicates are left dead to be cleaned up by `eraseDeadOperands`.
  ///
  /// ```
  ///   debug_value (%0, %0), ..., transform { bb0(%a, %b): ... }
  ///   -> debug_value %0, ..., transform { bb0(%a): ... }  // %b -> %a
  /// ```
  func mergeDuplicateOperands(_ context: FunctionPassContext) {
    guard let debugBB = debugReconstructionBlock, operands.count > 1 else {
      return
    }
    // Replace each operand with its first occurrence.
    var firstArgument: [HashableValue: Argument] = [:]
    for (argument, operand) in zip(debugBB.arguments, operands) {
      if let original = firstArgument[operand.value.hashable] {
        argument.uses.replaceAll(with: original, context)
      } else {
        firstArgument[operand.value.hashable] = argument
      }
    }
  }

  /// Propagate an undef operand to the debug reconstruction block content.
  /// This allows more simplifications and folding to happen.
  func propagateUndefOperands(_ context: FunctionPassContext) {
    for index in operands.indices where operands[index].value is Undef {
      killOperand(index: index, context)
    }
  }

  /// Drops all dead operands, to shorten the operand list.
  /// Returns the debug value, which is a new instruction if an operand was dropped.
  func eraseDeadOperands(_ context: FunctionPassContext) -> DebugValueInst {
    guard let debugBB = debugReconstructionBlock else {
      return self
    }
    var liveOperands: [Value] = []
    for operand in operands {
      // The argument index is the amount of kept operands so far.
      let argumentIndex = liveOperands.count
      if debugBB.arguments[argumentIndex].uses.isEmpty {
        debugBB.eraseArgument(at: argumentIndex, context)
      } else {
        liveOperands.append(operand.value)
      }
    }
    if liveOperands.count == operands.count {
      return self
    }
    return replaceOperands(with: liveOperands, context)
  }

  /// Drops a reconstruction block which does nothing.
  /// Returns the debug value, which may be a new instruction if it was reallocated.
  ///
  /// ```
  ///   debug_value %0, ..., transform { bb0(%a): return %a }  ->  debug_value %0
  ///   debug_value (), ..., transform { bb0: return undef }   ->  debug_value undef
  /// ```
  func collapseTrivialReconstruction(_ context: FunctionPassContext) -> DebugValueInst {
    guard let debugBB = debugReconstructionBlock,
          let returnInst = debugBB.terminator as? ReturnInst else {
      return self
    }
    if operands.count == 1, returnInst.returnedValue == debugBB.arguments[0] {
      // Remove the no-op reconstruction block.
      clearDebugReconstructionBlock(context)
    } else if operands.isEmpty, let undef = returnInst.returnedValue as? Undef {
      // This is just undef, no need for a reconstruction block.
      clearDebugReconstructionBlock(context)
      return replaceOperands(with: [undef], context)
    }
    return self
  }
}

/// Lets the C++ side simplify debug reconstruction blocks outside of the simplification pass.
func registerDebugReconstructionSimplification() {
  BridgedOptimizerUtilities.registerDebugReconstructionSimplification(
    { (bridgedCtxt: BridgedContext, bridgedFunction: BridgedFunction) in
      let context = FunctionPassContext(_bridged: bridgedCtxt)
      simplifyDebugReconstructionBlocks(in: bridgedFunction.function, context, useSubpasses: false)
    },
    { (bridgedCtxt: BridgedContext, bridgedInst: BridgedInstruction) -> BridgedInstruction in
      let context = FunctionPassContext(_bridged: bridgedCtxt)
      let debugValue = bridgedInst.getAs(DebugValueInst.self)
      return simplifyDebugReconstructionBlock(of: debugValue, context, useSubpasses: false).bridged
    }
  )
}
