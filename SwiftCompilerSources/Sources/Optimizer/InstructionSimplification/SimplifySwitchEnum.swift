//===--- SimplifySwitchEnum.swift -----------------------------------------===//
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

// Removes an `enum` - `switch_enum` pair:
// ```
//     %1 = enum $E, #someCase, %payload
//     switch_enum %1, case #someCase: bb1, ...
//   bb1(%payloadArgument):
// ```
// ->
// ```
//   br bb1(%payload)
//   bb1(%payloadArgument):
// ```
//
// Other case blocks of the switch_enum become dead.
//
extension SwitchEnumInst : OnoneSimplifiable, SILCombineSimplifiable {
  func simplify(_ context: SimplifyContext) {
    if tryFoldWithEnum(context) {
      return
    }
    _ = forwardBorrowToOwned(context)
  }

  private func tryFoldWithEnum(_ context: SimplifyContext) -> Bool {
    guard let enumInst = enumOp as? EnumInst,
          let caseBlock = getUniqueSuccessor(forCaseIndex: enumInst.caseIndex) else
    {
      return false
    }

    let singleUse = context.preserveDebugInfo ? enumInst.uses.singleUse : enumInst.uses.ignoreDebugUses.singleUse
    let canEraseEnumInst = singleUse?.instruction == self

    if !canEraseEnumInst && parentFunction.hasOwnership && enumInst.ownership == .owned {
      // We cannot add more uses to the `enum` instruction without inserting a copy.
      return false
    }

    let builder = Builder(before: self, context)
    switch caseBlock.arguments.count {
    case 0:
      precondition(enumInst.payload == nil || !parentFunction.hasOwnership,
                   "missing payload argument in switch_enum case block")
      builder.createBranch(to: caseBlock)
      context.erase(instruction: self)
    case 1:
      builder.createBranch(to: caseBlock, arguments: [enumInst.payload!])
      context.erase(instruction: self)
      updateBorrowedFrom(for: [Phi(caseBlock.arguments[0])!], context)
    default:
      fatalError("case block of switch_enum cannot have more than 1 argument")
    }

    if canEraseEnumInst {
      context.erase(instruction: enumInst)
    }
    return true
  }

  /// Replaces a borrowed `switch_enum` operand with the owned enum value:
  /// ```
  ///     %1 = begin_borrow %0
  ///     switch_enum %1, case #someCase: bb1, case #otherCase: bb2
  ///   bb1(%payloadArgument : @guaranteed):
  ///     end_borrow %1
  ///     %2 = unchecked_enum_data %0, #someCase
  ///   bb2:
  ///     end_borrow %1
  ///     destroy_value %0
  /// ```
  /// ->
  /// ```
  ///     switch_enum %0, case #someCase: bb1, case #otherCase: bb2
  ///   bb1(%payloadArgument : @owned):
  ///   bb2:
  /// ```
  /// This requires that the enum's lifetime ends in _all_ case blocks, either with an
  /// `unchecked_enum_data` or with a `destroy_value`.
  private func forwardBorrowToOwned(_ context: SimplifyContext) -> Bool {
    guard let beginBorrow = enumOp as? BeginBorrowInst,
          beginBorrow.uses.ignoreDebugUses.ignore(user: self).hasOnlyUsers(ofType: EndBorrowInst.self),
          beginBorrow.borrowedValue.ownership == .owned
    else {
      return false
    }

    if context.preserveDebugInfo,
       beginBorrow.isLexical || beginBorrow.isFromVarDecl
    {
      return false
    }

    let ownedEnum = beginBorrow.borrowedValue
    let switchBlock = parentBlock
    var caseBlocksWithLifetimeEnds = BasicBlockSet(context)
    defer { caseBlocksWithLifetimeEnds.deinitialize() }

    for user in ownedEnum.users {
      let block = user.parentBlock
      guard block.singlePredecessor == switchBlock else {
        continue
      }
      switch user {
      case let enumData as UncheckedEnumDataInst:
        guard block.arguments.count == 1, enumData.type == block.arguments[0].type else {
          return false
        }
      case is DestroyValueInst:
        break
      default:
        return false
      }
      // The payload argument changes from `guaranteed` to `owned`. This is only possible if all
      // its existing uses accept an owned value, e.g. it doesn't have any borrowing uses.
      if let payloadArgument = ownedPayloadArgument(of: block),
         !payloadArgument.uses.allSatisfy({ $0.canAccept(ownership: .owned) })
      {
        return false
      }
      let inserted = caseBlocksWithLifetimeEnds.insert(block)
      assert(inserted, "multiple lifetime ends in a single case block")
    }

    guard successors.allSatisfy({ caseBlocksWithLifetimeEnds.contains($0) }) else {
      return false
    }

    for user in ownedEnum.users {
      let block = user.parentBlock
      guard block.singlePredecessor == switchBlock else {
        continue
      }
      let payloadArgument = ownedPayloadArgument(of: block)
      switch user {
      case let enumData as UncheckedEnumDataInst:
        enumData.replace(with: block.arguments[0], context)
      case let destroy as DestroyValueInst:
        if let payloadArgument {
          destroy.operand.set(to: payloadArgument, context)
        } else {
          // The case block doesn't forward the payload as an owned value, i.e. the `switch_enum`
          // itself consumes the enum and there is nothing left to destroy in the case block.
          context.erase(instruction: destroy)
        }
      default:
        fatalError("unexpected user of the owned enum")
      }
      if let payloadArgument {
        payloadArgument.set(ownership: .owned, context)
      }
    }
    operand.set(to: ownedEnum, context)
    setForwardingOwnership(to: .owned, context)
    context.erase(instructionIncludingAllUsers: beginBorrow)

    return true
  }
}

/// Returns the payload argument of `caseBlock` if its ownership needs to change from
/// `guaranteed` to `owned` when the `switch_enum` operand becomes owned.
private func ownedPayloadArgument(of caseBlock: BasicBlock) -> Argument? {
  if caseBlock.arguments.count == 1,
     !caseBlock.arguments[0].type.isTrivial(in: caseBlock.parentFunction)
  {
    return caseBlock.arguments[0]
  }
  return nil
}
