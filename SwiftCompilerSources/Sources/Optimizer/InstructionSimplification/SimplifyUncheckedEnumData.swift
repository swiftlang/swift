//===--- SimplifyUncheckedEnumData.swift ----------------------------------===//
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

extension UncheckedEnumDataInst : OnoneSimplifiable, SILCombineSimplifiable {
  func simplify(_ context: SimplifyContext) {
    if let enumInst = self.enum as? EnumInst {
      if caseIndex == enumInst.caseIndex {
        context.tryReplaceRedundantInstructionPair(first: enumInst, second: self, with: enumInst.payload!)
      }
      return
    }
    replaceWithSwitchEnumPayloadArgument(context)
  }

  /// Replaces an `unchecked_enum_data` in a `switch_enum` case block with the block's payload
  /// argument, if it projects the same case of the same enum, e.g.
  ///
  ///   switch_enum %e, case #C: bb1
  /// bb1(%payload):
  ///   %d = unchecked_enum_data %e, #C
  /// ->
  ///   %payload
  ///
  private func replaceWithSwitchEnumPayloadArgument(_ context: SimplifyContext) {
    let block = parentBlock
    guard let pred = block.singlePredecessor,
          let switchEnum = pred.terminator as? SwitchEnumInst,
          switchEnum.enumOp == self.enum,
          switchEnum.getUniqueCase(forSuccessor: block) == caseIndex,
          block.arguments.count == 1
    else {
      return
    }
    self.replace(with: block.arguments[0], context)
  }
}
