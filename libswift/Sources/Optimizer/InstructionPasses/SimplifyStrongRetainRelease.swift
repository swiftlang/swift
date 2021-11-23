//===--- SimplifyStrongRetainRelease.swift - strong_retain/release opt ----===//
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

let simplifyStrongRetainPass = InstructionPass(
  name: "simplify-strong_retain", {
  (inst: Instruction, context: PassContext) in
    
  let retain = getAsStrongRetainInst(inst)!

  if isNotReferenceCounted(value: retain.operand, context: context) {
    context.erase(instruction: getAsSILInstruction(retain))
    return
  }

  // Sometimes in the stdlib due to hand offs, we will see code like:
  //
  // strong_release %0
  // strong_retain %0
  //
  // with the matching strong_retain to the strong_release in a predecessor
  // basic block and the matching strong_release for the strong_retain in a
  // successor basic block.
  //
  // Due to the matching pairs being in different basic blocks, the ARC
  // Optimizer (which is currently local to one basic block does not handle
  // it). But that does not mean that we cannot eliminate this pair with a
  // peephole.
  if let prev = retain.previous {
    if let release = getAsStrongReleaseInst(prev) {
      if release.operand == retain.operand {
        context.erase(instruction: getAsSILInstruction(retain))
        context.erase(instruction: getAsSILInstruction(release))
        return
      }
    }
  }
})
  
let simplifyStrongReleasePass = InstructionPass(
  name: "simplify-strong_release", {
  (inst: Instruction, context: PassContext) in

  let release = getAsStrongReleaseInst(inst)!
  let op = release.operand
  if isNotReferenceCounted(value: op, context: context) {
    context.erase(instruction: getAsSILInstruction(release))
    return
  }

  // Release of a classbound existential converted from a class is just a
  // release of the class, squish the conversion.
    if let ier = getAsInitExistentialRefInst(op) {
    if getAsValue(ier)!.uses.isSingleUse {
      context.setOperand(of: getAsSILInstruction(release), at: 0, to: ier.operand)
      context.erase(instruction: getAsSILInstruction(ier))
      return
    }
  }
})
  
/// Returns true if \p value is something where reference counting instructions
/// don't have any effect.
private func isNotReferenceCounted(value: Value, context: PassContext) -> Bool {
  if let cfi = getAsConvertFunctionInst(value) {
    return isNotReferenceCounted(value: cfi.operand, context: context)
  } else if let uci = getAsUpcastInst(value) {
    return isNotReferenceCounted(value: uci.operand, context: context)
  } else if let urc = getAsUncheckedRefCastInst(value) {
    return isNotReferenceCounted(value: urc.operand, context: context)
  } else if let rptr = getAsRawPointerToRefInst(value) {
    // Like `global_value` but for the empty collection singletons from the
    // stdlib, e.g. the empty Array singleton.
    if context.isSwift51RuntimeAvailable {
      // The pattern generated for empty collection singletons is:
      //     %0 = global_addr @_swiftEmptyArrayStorage
      //     %1 = address_to_pointer %0
      //     %2 = raw_pointer_to_ref %1
      if let atp = getAsAddressToPointerInst(rptr.operand) {
        return isaGlobalAddrInst(atp.operand)
      }
    }
    return false
  } else if isaGlobalValueInst(value) {
    // Since Swift 5.1, statically allocated objects have "immortal" reference
    // counts. Therefore we can safely eliminate unbalaced retains and
    // releases, because they are no-ops on immortal objects.
    // Note that the `simplifyGlobalValuePass` pass is deleting balanced
    // retains/releases, which doesn't require a Swift 5.1 minimum deployment
    // targert.
    return context.isSwift51RuntimeAvailable
  } else if // Thin functions are not reference counted.
    isaThinToThickFunctionInst(value) ||
      // The same for meta types.
      isaObjCExistentialMetatypeToObjectInst(value) ||
      isaObjCMetatypeToObjectInst(value) ||
      // Retain and Release of tagged strings is a no-op.
      // The builtin code pattern to find tagged strings is:
      // builtin "stringObjectOr_Int64" (or to tag the string)
      // value_to_bridge_object (cast the UInt to bridge object)
      isaValueToBridgeObjectInst(value) {
    return true
  }
  return false
}
