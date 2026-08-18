//===--- SimplificationPasses.swift ----------------------------------------==//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2023 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import SIL

//===--------------------------------------------------------------------===//
//                        Instruction protocols
//===--------------------------------------------------------------------===//

/// Instructions which can be simplified at all optimization levels
protocol Simplifiable : Instruction {
  func simplify(_ context: SimplifyContext)
}

/// Instructions which can be simplified at -Onone
protocol OnoneSimplifiable : Simplifiable {
}

/// Instructions which can only be simplified at the end of the -Onone pipeline
protocol LateOnoneSimplifiable : Instruction {
  func simplifyLate(_ context: SimplifyContext)
}

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

//===--------------------------------------------------------------------===//
//                        Simplification passes
//===--------------------------------------------------------------------===//

let ononeSimplificationPass = FunctionPass(name: "onone-simplification") {
  (function: Function, context: FunctionPassContext) in

  runSimplification(on: function, context, preserveDebugInfo: true) {
    if let i = $0 as? OnoneSimplifiable {
      i.simplify($1)
    }
  }
}

let simplificationPass = FunctionPass(name: "simplification") {
  (function: Function, context: FunctionPassContext) in

  runSimplification(on: function, context, preserveDebugInfo: false) {
    if let i = $0 as? Simplifiable {
      i.simplify($1)
    }
  }
}

let lateOnoneSimplificationPass = FunctionPass(name: "late-onone-simplification") {
  (function: Function, context: FunctionPassContext) in

  runSimplification(on: function, context, preserveDebugInfo: true) {
    if let i = $0 as? LateOnoneSimplifiable {
      i.simplifyLate($1)
    } else if let i = $0 as? OnoneSimplifiable {
      i.simplify($1)
    }
  }
}

let debugReconstructionBlockSimplificationPass = FunctionPass(name: "debug-reconstruction-block-simplification") {
  (function: Function, context: FunctionPassContext) in
  runDebugReconstructionBlockSimplification(on: function, context)
}

//===--------------------------------------------------------------------===//
//                         Pass implementation
//===--------------------------------------------------------------------===//

@discardableResult
func runSimplification(on function: Function, _ context: FunctionPassContext,
                       preserveDebugInfo: Bool,
                       _ simplify: (Instruction, SimplifyContext) -> ()) -> Bool {
  var worklist = InstructionWorklist(context)
  defer { worklist.deinitialize() }

  var changed = false
  let simplifyCtxt = context.createSimplifyContext(preserveDebugInfo: preserveDebugInfo,
                                                   notifyInstructionChanged: {
    worklist.pushIfNotVisited($0)
    changed = true
  })

  // Push in reverse order so that popping from the tail of the worklist visits instruction in forward order again.
  worklist.pushIfNotVisited(contentsOf: function.reversedInstructions)

  // Run multiple iterations because cleanupDeadCode can add new candidates to the worklist.
  repeat {

    // The core worklist-loop.
    while let instruction = worklist.popAndForget() {
      if instruction.isDeleted {
        continue
      }
      if !context.options.enableSimplification(for: instruction) {
        continue
      }
      if !context.continueWithNextSubpassRun(for: instruction) {
        return changed
      }
      simplify(instruction, simplifyCtxt)
    }

    cleanupDeadInstructions(in: function, preserveDebugInfo, context)
    cleanupDeadBlocks(in: function, pushNewCandidatesTo: &worklist, context)

  } while !worklist.isEmpty

  if context.needFixStackNesting {
    context.fixStackNesting(in: function)
  }

  if context.needBreakInfiniteLoops {
    breakInfiniteLoops(in: function, context)
  }
  if context.needCompleteLifetimes {
    completeLifetimes(in: function, context)
  }

  return changed
}

private func cleanupDeadInstructions(in function: Function,
                                     _ preserveDebugInfo: Bool,
                                     _ context: FunctionPassContext) {
  if preserveDebugInfo {
    context.removeTriviallyDeadInstructionsPreservingDebugInfo(in: function)
  } else {
    context.removeTriviallyDeadInstructionsIgnoringDebugUses(in: function)
  }
}

private func cleanupDeadBlocks(in function: Function,
                               pushNewCandidatesTo worklist: inout InstructionWorklist,
                               _ context: FunctionPassContext) {
  if context.removeDeadBlocks(in: function) {
    // After deleting dead blocks their (still alive) successor blocks may become eligible for block merging.
    // Therefore we re-run simplification for all branch instructions.
    for block in function.blocks.reversed() {
      if let bi = block.terminator as? BranchInst {
        worklist.pushIfNotVisited(bi)
      }
    }
  }
}

//===--------------------------------------------------------------------===//
//          Debug Reconstruction Block Simplification
//===--------------------------------------------------------------------===//

private func runDebugReconstructionBlockSimplification(on function: Function, _ context: FunctionPassContext) {
  var worklist = InstructionWorklist(context)
  defer { worklist.deinitialize() }

  let simplifyCtxt = context.createSimplifyContext(preserveDebugInfo: false,
                                                   notifyInstructionChanged: {
    worklist.pushIfNotVisited($0)
  })

  for inst in function.instructions {
    guard let debugValue = inst as? DebugValueInst,
          let debugBB = debugValue.debugReconstructionBlock else {
      continue
    }
    // Use a subpass for each debug_value (operand simplification) and for each instruction in the reconstruction block.
    if !context.continueWithNextSubpassRun(for: debugValue) {
      return
    }

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
        if !context.continueWithNextSubpassRun(for: instruction) {
          return
        }
        simplifiable.simplifyForDebugReconstructionBlock(simplifyCtxt)
      }
    }

    // Cleanup dead instructions and operands.
    for instruction in debugBB.instructions.reversed() where instruction.isTriviallyDead {
      context.erase(instruction: instruction)
    }
    debugValue.eraseDeadOperands(context)
    debugValue.collapseTrivialReconstruction(context)
  }
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
    for index in operands.indices.reversed() where operands[index].value is Undef {
      killOperand(index: index, context)
    }
  }

  /// Drops all dead operands, to shorten the operand list.
  func eraseDeadOperands(_ context: FunctionPassContext) {
    guard let debugBB = debugReconstructionBlock else {
      return
    }
    // Erase back to front, as erased operand will invalidate the next indices.
    for index in operands.indices.reversed() where debugBB.arguments[index].uses.isEmpty {
      eraseOperand(index: index, context)
    }
  }

  /// Drops a reconstruction block which does nothing.
  ///
  /// ```
  ///   debug_value %0, ..., transform { bb0(%a): return %a }  ->  debug_value %0
  ///   debug_value (), ..., transform { bb0: return undef }   ->  debug_value undef
  /// ```
  func collapseTrivialReconstruction(_ context: FunctionPassContext) {
    guard let debugBB = debugReconstructionBlock,
          let returnInst = debugBB.terminator as? ReturnInst,
          let debugVariable else {
      return
    }
    if operands.count == 1, returnInst.returnedValue == debugBB.arguments[0] {
      // Remove the no-op reconstruction block.
      clearDebugReconstructionBlock(context)
    } else if operands.isEmpty, let undef = returnInst.returnedValue as? Undef {
      // This is just undef, no need for a reconstruction block.
      // The operand list cannot grow back in place, so this needs a fresh instruction.
      let builder = Builder(replacing: self, context)
      builder.createDebugValue(value: undef, debugVariable: debugVariable)
      context.erase(instruction: self)
    }
  }
}
