//===--- AssumeSingleThreaded.swift - Assume single-threaded execution --------------===//
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
//
// Assume that user code is single-thread.
//
// Convert all reference counting operations into non-atomic ones.
//
// To get read of most atomic reference counting operations, the standard
// library should be compiled in this mode as well 
//
// This pass affects only reference counting operations resulting from SIL
// instructions. It wouldn't affect places in the runtime C++ code which
// hard-code calls to retain/release. We could take advantage of the Instruments
// instrumentation stubs to redirect calls from the runtime if it was
// significant, or else just build a single-threaded variant of the runtime.
//
//===----------------------------------------------------------------------===//


import SIL
import SILBridging

let assumeSingleThreaded = FunctionPass(name: "sil-assume-single-threaded") { function, context in

  if !context.isAssumeSingleThreadedEnabled {
    return
  }

  for block in function.blocks {
    for instruction in block.instructions {
      if let rcInst = instruction as? RefCountingInst {
        rcInst.setNonAtomic()
      }
    }
  }
  context.invalidateAnalysis(instructionsChanged, at: function)
}
