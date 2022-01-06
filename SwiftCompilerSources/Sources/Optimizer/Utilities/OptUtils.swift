//===--- OptUtils.swift - Utilities for optimzations ----------------------===//
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

extension Value {
  var nonDebugUses: LazyFilterSequence<UseList> {
    uses.lazy.filter { !($0.instruction is DebugValueInst) }
  }
}
