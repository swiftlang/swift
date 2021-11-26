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

import SILBridging
import SIL

// Removes all reference counting instructions of a `global_value` instruction
// if it does not escape.
//
// Note that `simplifyStrongRetainPass` and `simplifyStrongReleasePass` can
// even remove "unbalanced" retains/releases of a `global_value`, but this
// requires a minimum deployment target.
let simplifyGlobalValuePass = InstructionPass(
  name: "simplify-global_value", {
    (inst: Instruction, context: PassContext) in
    
  let globalValue = getAsGlobalValueInst(inst)!

  var users = StackList<Instruction>(context)
  if checkUsers(of: getAsValue(globalValue)!, users: &users) {
    while let inst = users.pop() {
      context.erase(instruction: inst)
    }
  } else {
    users.removeAll()
  }
})
  
/// Returns true if reference counting and debug_value users of a global_value
/// can be deleted.
private func checkUsers(of val: Value, users: inout StackList<Instruction>) -> Bool {
  for use in val.uses {
    let user = use.instruction
    if user is RefCountingInst || isaDebugValueInst(user) {
      users.push(user)
      continue
    }
    if let upCast = getAsUpcastInst(user) {
      if !checkUsers(of: getAsValue(upCast)!, users: &users) {
        return false
      }
      continue
    }
    // Projection instructions don't access the object header, so they don't
    // prevent deleting reference counting instructions.
    if isaRefElementAddrInst(user) || isaRefTailAddrInst(user) {
      continue
    }
    return false
  }
  return true
}
