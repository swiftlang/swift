//===--- RemoveSILGenLifetimes.swift -----------------------------------------==//
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

// Until SILGen gets out of the business of trying to manage lifetimes, we
// clean and remove instructions so that lifetimes can derived from solely from
// uses during LifetimeResolution.
let removeSILGenLifetimesPass = FunctionPass(name: "remove-silgen-lifetimes") {
  (function: Function, context: FunctionPassContext) in

  func processInst(_ inst: Instruction) {
    switch inst {
    case let marker as MarkUnresolvedNonCopyableValueInst:
      // Pure scaffolding for the existing move-checker: it classifies the access
      // kind on this marker. We derive ownership ourselves, so strip it.
      marker.replace(with: marker.operand.value, context)

    case let move as MoveValueInst where move.allowsDiagnostics:
      // We interpret move_value [allows_diagnostics] from SILGen to mean an explicit `consume` was written in source.

      // If we happen to have a `move_value` of a `load [copy]`, then convert the latter to a `[take]`.
      if let def = move.operand.value.definingInstruction,
         let load = def as? LoadInst,
         load.loadOwnership == .copy {

        load.set(ownership: .take, context)

        // Drop the move_value, if it won't mess up lexical lifetimes.
        if !move.isLexical {
          move.replace(with: load, context)
        }
      }

    // TODO: until LifetimeResolution handles copies of guaranteed values,
    //  limit deletion to copies of owned values.
    case let copy as CopyValueInst where copy.operand.value.ownership == .owned:
      let original = copy.operand.value
      // Construct a liverange of the copy's uses, and see if a destroy of the original falls within
      // its range. If so, that destroy needs to be removed too. For example, if before we have,
      //
      //   %original = ...
      //   %copy = copy_value %original
      //   destroy_value %original        <--- original's liverange stops here
      //   return %copy                   <--- copy's liverange is longer
      //
      // we want to erase the destroy that is limiting the original's liverange,
      // as we're going to replace the copy with the original.
      var copyRange = InstructionRange(begin: copy, ends: copy.users, context)
      defer { copyRange.deinitialize() }
      for destroy in original.uses.users(ofType: DestroyValueInst.self) {
        guard copyRange.contains(destroy) else { continue }
        context.erase(instruction: destroy)
      }
      copy.replace(with: original, context)

    default:
      return
    }
  }

  for block in function.blocks {
    for inst in block.instructions {
      processInst(inst)
    }
  }
}
