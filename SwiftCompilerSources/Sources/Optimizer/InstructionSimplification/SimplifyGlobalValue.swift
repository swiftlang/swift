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
extension GlobalValueInst : Simplifiable, SILCombineSimplifiable {
  func simplify(_ context: SimplifyContext) {
    var users = Stack<Instruction>(context)
    defer { users.deinitialize() }

    if checkUsers(of: self, users: &users) {
      for inst in users {
        context.erase(instruction: inst)
      }
    }
  }
}

/// Returns true if reference counting and debug_value users of a global_value
/// can be deleted.
private func checkUsers(of val: Value, users: inout Stack<Instruction>) -> Bool {
  for use in val.uses {
    let user = use.instruction
    switch user {
      case is RefCountingInst, is DebugValueInst, is FixLifetimeInst:
        users.push(user)
      case let upCast as UpcastInst:
        if !checkUsers(of: upCast, users: &users) {
          return false
        }
      case is RefElementAddrInst, is RefTailAddrInst:
        // Projection instructions don't access the object header, so they don't
        // prevent deleting reference counting instructions.
        break
      default:
        return false
    }
  }
  return true
}
