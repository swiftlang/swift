//===--- AssumeSingleThreaded.swift - Assume single-threaded execution ----===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import SIL

let assumeSingleThreadedPass = FunctionPass(
  name: "sil-assume-single-threaded", { function, context in
    for block in function.blocks {
      for inst in block.instructions {
        guard let rcInst = inst as? RefCountingInst else { continue }

        context.setAtomicity(of: rcInst, isAtomic: false)
      }
    }
  }
)
