//===--- SimplifySwitchEnumAddr.swift -------------------------------------===//
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

extension SwitchEnumAddrInst : Simplifiable, SILCombineSimplifiable {
  /// Simplifies a `switch_enum_addr` instruction through two main optimizations:
  ///
  /// **1. Constant Folding**: When the address directly contains a known enum case from `inject_enum_addr`,
  /// the switch is eliminated and replaced with a direct branch.
  ///
  /// ```
  ///   %0 = alloc_stack $Optional<Int>
  ///   inject_enum_addr %0, #Optional.some!enumelt
  ///   switch_enum_addr %0, case #Optional.some!enumelt: bb1, case #Optional.none!enumelt: bb2
  ///
  /// bb1:
  ///   // some case
  /// bb2:
  ///   // none case
  /// ```
  /// ->
  /// ```
  ///   %0 = alloc_stack $Optional<Int>
  ///   inject_enum_addr %0, #Optional.some!enumelt
  ///   br bb1
  ///
  /// bb1:
  ///   // some case
  /// bb2: // becomes unreachable
  /// ```
  ///
  /// **2. Address-to-Value Conversion**: When the enum type is loadable, converts `switch_enum_addr`
  /// to `switch_enum` by loading the enum value, enabling better optimization opportunities.
  ///
  /// ```
  ///   switch_enum_addr %0, case #Optional.some!enumelt: bb1, case #Optional.none!enumelt: bb2
  ///
  /// bb1:
  ///   // some case
  /// bb2:
  ///   // none case
  /// ```
  /// ->
  /// ```
  ///   %1 = load_borrow %0
  ///   switch_enum %1, case #Optional.some!enumelt: bb1, case #Optional.none!enumelt: bb2
  ///
  /// bb1(%payload : $T):
  ///   end_borrow %1
  ///   // some case, payload available as argument
  /// bb2:
  ///   end_borrow %1
  ///   // none case
  /// ```
  func simplify(_ context: SimplifyContext) {
    if tryConstantFold(switchEnumAddr: self, context) {
      return
    }
    _ = tryReplaceWithSwitchEnum(switchEnumAddr: self, context)
  }
}

private func tryConstantFold(switchEnumAddr: SwitchEnumAddrInst, _ context: SimplifyContext) -> Bool {
  // Try to optimize switch_enum_addr preceded by inject_enum_addr
  guard let injectInst = getInjectEnumCase(to: switchEnumAddr.enumOp) else {
    return false
  }
  let succBlock = switchEnumAddr.getSuccessor(forCaseIndex: injectInst.caseIndex)
  Builder(before: switchEnumAddr, context).createBranch(to: succBlock)
  context.erase(instruction: switchEnumAddr)
  return true
}

private func tryReplaceWithSwitchEnum(switchEnumAddr: SwitchEnumAddrInst, _ context: SimplifyContext) -> Bool {
  let function = switchEnumAddr.parentFunction
  guard switchEnumAddr.enumOp.type.isLoadable(in: function),
        let enumCases = switchEnumAddr.enumOp.type.objectType.getEnumCases(in: function)
  else {
    return false
  }

  let builder = Builder(before: switchEnumAddr, context)
  let load = builder.emitLoadBorrow(fromAddress: switchEnumAddr.enumOp)
  builder.createSwitchEnum(enum: load, cases: Array(switchEnumAddr.cases),
                           defaultBlock: switchEnumAddr.getSuccessorForDefault())

  if function.hasOwnership {
    addPayloadArguments(toSuccessorsOf: switchEnumAddr, enumCases: enumCases, context)

    if load is LoadBorrowInst {
      for succ in switchEnumAddr.successors {
        Builder(atBeginOf: succ, context).createEndBorrow(of: load)
      }
    }
  }
  context.erase(instruction: switchEnumAddr)
  return true
}

private func addPayloadArguments(toSuccessorsOf switchEnumAddr: SwitchEnumAddrInst,
                                 enumCases: EnumCases,
                                 _ context: SimplifyContext)
{
  var caseSuccessors = Dictionary<Int, BasicBlock>()
  for (caseIndex, succ) in switchEnumAddr.cases {
    caseSuccessors[caseIndex] = succ
  }
  let uniqueDefaultEnumCaseIndex = switchEnumAddr.getUniqueCaseForDefault()

  for enumCase in enumCases {
    if let payloadType = enumCase.payload {
      if let succ = caseSuccessors[enumCase.index] {
        addPayloadArgument(to: succ, type: payloadType, context)
      } else if enumCase.index == uniqueDefaultEnumCaseIndex {
        // The "default" case covers a single enum case.
        addPayloadArgument(to: switchEnumAddr.getSuccessorForDefault()!, type: payloadType, context)
      }
    }
  }
  if uniqueDefaultEnumCaseIndex == nil, let defaultSucc = switchEnumAddr.getSuccessorForDefault() {
    // If the "default" case covers multiple cases we need to forward the original enum value.
    addPayloadArgument(to: defaultSucc, type: switchEnumAddr.enumOp.type.objectType, context)
  }
}

private func addPayloadArgument(to block: BasicBlock, type: Type, _ context: SimplifyContext) {
  let ownership: Ownership = type.isTrivial(in: block.parentFunction) ? .none : .guaranteed
  _ = block.addArgument(type: type, ownership: ownership, context)
}

/// Attempts to find an `inject_enum_addr` instruction that directly feeds into this switch.
/// Returns the `inject_enum_addr` instruction if found, otherwise nil.
private func getInjectEnumCase(to address: Value) -> InjectEnumAddrInst? {
  guard let allocStack = address as? AllocStackInst,
        let singleWrite = getSingleWrite(to: allocStack)
  else {
    return nil
  }
  switch singleWrite {
  case let injectEnum as InjectEnumAddrInst:
    return injectEnum
  case let copyAddr as CopyAddrInst:
    // Try to find the `inject_end_addr` in the source of the copy which copies to the `allocStack`.
    return getInjectEnumCase(to: copyAddr.source)
  default:
    fatalError("wrong single write instruction")
  }
}

/// Returns an `inject_enum_addr` or `copy_addr` which is the only instruction which writes to `alloc_stack`.
private func getSingleWrite(to allocStack: AllocStackInst) -> Instruction? {
  var singleWrite: Instruction? = nil

  for use in allocStack.uses {
    switch use.instruction {
    case is DestroyAddrInst,
         is DeallocStackInst,
         is SwitchEnumAddrInst,
         is InitEnumDataAddrInst,
         is DebugValueInst:
      break
    case let apply as FullApplySite:
      guard apply.convention(of: use) == .indirectInGuaranteed else {
        return nil
      }
    case let injectEnum as InjectEnumAddrInst:
      if singleWrite != nil {
        return nil
      }
      singleWrite = injectEnum
    case let copyAddr as CopyAddrInst:
      if copyAddr.destinationOperand == use {
        if singleWrite != nil {
          return nil
        }
        singleWrite = copyAddr
      }
    default:
      return nil
    }
  }
  return singleWrite
}
