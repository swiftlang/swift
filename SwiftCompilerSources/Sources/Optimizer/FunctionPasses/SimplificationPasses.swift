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

/// Instructions whose `simplify()` is safe to run inside debug reconstruction blocks.
protocol DebugReconstructionBlockSimplifiable : Simplifiable {}

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

  // Collect all debug reconstruction blocks and push their instructions onto the worklist.
  var debugBlocks: [BasicBlock] = []
  for block in function.blocks {
    for inst in block.instructions {
      guard let debugValue = inst as? DebugValueInst,
            let debugBB = debugValue.debugReconstructionBlock else {
        continue
      }
      debugBlocks.append(debugBB)
      for debugInst in debugBB.instructions.reversed() {
        worklist.pushIfNotVisited(debugInst)
      }
    }
  }

  // Core worklist loop.
  while let instruction = worklist.popAndForget() {
    if instruction.isDeleted { continue }
    if let simplifiable = instruction as? DebugReconstructionBlockSimplifiable {
      simplifiable.simplify(simplifyCtxt)
    }
  }

  // Clean up trivially dead instructions in all debug BBs.
  for debugBB in debugBlocks {
    for inst in debugBB.instructions.reversed() {
      if inst.isTriviallyDead {
        context.erase(instruction: inst)
      }
    }
  }
}
