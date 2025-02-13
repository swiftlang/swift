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

import SIL

extension StrongRetainInst : Simplifiable, SILCombineSimplifiable {
  func simplify(_ context: SimplifyContext) {
    if isNotReferenceCounted(value: instance) {
      context.erase(instruction: self)
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
    if let prev = previous {
      if let release = prev as? StrongReleaseInst {
        if release.instance == self.instance {
          context.erase(instruction: self)
          context.erase(instruction: release)
          return
        }
      }
    }
  }
}

extension StrongReleaseInst : Simplifiable, SILCombineSimplifiable {
  func simplify(_ context: SimplifyContext) {
    let op = instance
    if isNotReferenceCounted(value: op) {
      context.erase(instruction: self)
      return
    }

    // Release of a classbound existential converted from a class is just a
    // release of the class, squish the conversion.
    if let ier = op as? InitExistentialRefInst {
      if ier.uses.isSingleUse {
        setOperand(at: 0, to: ier.instance, context)
        context.erase(instruction: ier)
        return
      }
    }
  }
}

/// Returns true if \p value is something where reference counting instructions
/// don't have any effect.
private func isNotReferenceCounted(value: Value) -> Bool {
  if value.type.isMarkedAsImmortal {
    return true
  }
  switch value {
    case let cfi as ConvertFunctionInst:
      return isNotReferenceCounted(value: cfi.fromFunction)
    case let uci as UpcastInst:
      return isNotReferenceCounted(value: uci.fromInstance)
    case let urc as UncheckedRefCastInst:
      return isNotReferenceCounted(value: urc.fromInstance)
    case let gvi as GlobalValueInst:
      // Since Swift 5.1, statically allocated objects have "immortal" reference
      // counts. Therefore we can safely eliminate unbalanced retains and
      // releases, because they are no-ops on immortal objects.
      // Note that the `simplifyGlobalValuePass` pass is deleting balanced
      // retains/releases, which doesn't require a Swift 5.1 minimum deployment
      // target.
      return gvi.parentFunction.isSwift51RuntimeAvailable
    case let rptr as RawPointerToRefInst:
      // Like `global_value` but for the empty collection singletons from the
      // stdlib, e.g. the empty Array singleton.
      if rptr.parentFunction.isSwift51RuntimeAvailable {
        // The pattern generated for empty collection singletons is:
        //     %0 = global_addr @_swiftEmptyArrayStorage
        //     %1 = address_to_pointer %0
        //     %2 = raw_pointer_to_ref %1
        if let atp = rptr.pointer as? AddressToPointerInst {
          return atp.address is GlobalAddrInst
        }
      }
      return false
    case // Thin functions are not reference counted.
         is ThinToThickFunctionInst,
         // The same for meta types.
         is ObjCExistentialMetatypeToObjectInst,
         is ObjCMetatypeToObjectInst,
         // Retain and Release of tagged strings is a no-op.
         // The builtin code pattern to find tagged strings is:
         // builtin "stringObjectOr_Int64" (or to tag the string)
         // value_to_bridge_object (cast the UInt to bridge object)
         is ValueToBridgeObjectInst:
      return true
    default:
      return false
  }
}
