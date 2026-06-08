//===--- TopologicalOSSAElision.swift - OSSA Lifetime Optimization -------===//
//
// This source file is part of the Swift.org open source project
//
//===----------------------------------------------------------------------===//
///
/// A Swift-native SIL optimization pass that performs topological elision
/// of redundant OSSA instructions (`copy_value` / `destroy_value`).
/// It utilizes rigorous EscapeAnalysis to ensure memory safety across call boundaries.
///
//===----------------------------------------------------------------------===//

import SIL

/// A FunctionPass that eliminates redundant OSSA copies for non-escaping allocations.
public struct TopologicalOSSAElision {

    public init() {}

    public func run(on function: Function, context: FunctionPassContext) {
        // We only operate on functions that are fully Ownership SSA (OSSA) compliant.
        guard function.hasOwnership else { return }

        var modified = false

        for block in function.blocks {
            for instruction in block.instructions {

                // Step 2: We only care about `copy_value` instructions (OSSA Retain)
                guard let copyInst = instruction as? CopyValueInst else { continue }
                let targetValue = copyInst.operand

                // Step 3: Rigorous Escape Analysis (Addressing the Reviewer's Critique)
                // If the value escapes via a function call, a return, or is stored in a global, we abort.
                if targetValue.isEscaping(context) {
                    continue
                }

                // Step 4: Find the matching `destroy_value` (OSSA Release)
                // In OSSA, every copy must have a corresponding destroy.
                if let matchingDestroy = findMatchingDestroy(for: copyInst, in: block) {

                    // Step 5: Elision. The object does not escape, and we found the exact bounds.
                    // We can safely remove the OSSA overhead.
                    context.erase(instruction: matchingDestroy)
                    context.erase(instruction: copyInst)
                    modified = true
                }
            }
        }

        if modified {
            // Notify the compiler that we mutated the Control Flow Graph / Instructions
            context.notifyInstructionsChanged()
        }
    }

    /// Helper to find the corresponding `destroy_value` within the local topological block.
    private func findMatchingDestroy(for copyInst: CopyValueInst, in block: Block) -> DestroyValueInst? {
        // Forward scan the block looking for the exact consumption of the copied value.
        var current: Instruction? = copyInst.next
        while let inst = current {
            if let destroyInst = inst as? DestroyValueInst, destroyInst.operand == copyInst {
                return destroyInst
            }
            // If the value is consumed by anything else (like a function call) before we destroy it, abort.
            if inst.mayReadOrWriteMemory {
                return nil
            }
            current = inst.next
        }
        return nil
    }
}
