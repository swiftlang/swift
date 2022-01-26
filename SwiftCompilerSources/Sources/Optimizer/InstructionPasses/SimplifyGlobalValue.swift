//===--- SimplifyGlobalValue.swift - Simplify global_value instruction ----===//
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

import SIL

// Removes all reference counting instructions of a `global_value` instruction
// if it does not escape.
//
// Note that `simplifyStrongRetainPass` and `simplifyStrongReleasePass` can
// even remove "unbalanced" retains/releases of a `global_value`, but this
// requires a minimum deployment target.
let simplifyGlobalValuePass = InstructionPass<GlobalValueInst>(
  name: "simplify-global_value", {
  (globalValue: GlobalValueInst, context: PassContext) in

  var users = Stack<Instruction>(context)
  defer { users.deinitialize() }
  
  if checkUsers(of: globalValue, users: &users) {
    for inst in users {
      context.erase(instruction: inst)
    }
  }
})

/// Returns true if reference counting and debug_value users of a global_value
/// can be deleted.
private func checkUsers(of val: Value, users: inout Stack<Instruction>) -> Bool {
  for use in val.uses {
    let user = use.instruction
    if user is RefCountingInst || user is DebugValueInst {
      users.push(user)
      continue
    }
    if let upCast = user as? UpcastInst {
      if !checkUsers(of: upCast, users: &users) {
        return false
      }
      continue
    }
    // Projection instructions don't access the object header, so they don't
    // prevent deleting reference counting instructions.
    if user is RefElementAddrInst || user is RefTailAddrInst {
      continue
    }
    return false
  }
  return true
}
