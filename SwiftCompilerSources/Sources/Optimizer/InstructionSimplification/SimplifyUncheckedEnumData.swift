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

extension UncheckedEnumDataInst : OnoneSimplifyable, SILCombineSimplifyable {
  func simplify(_ context: SimplifyContext) {
    guard let enumInst = self.enum as? EnumInst else {
      return
    }
    if caseIndex != enumInst.caseIndex {
      return
    }
    context.tryReplaceRedundantInstructionPair(first: enumInst, second: self, with: enumInst.payload!)
  }
}
