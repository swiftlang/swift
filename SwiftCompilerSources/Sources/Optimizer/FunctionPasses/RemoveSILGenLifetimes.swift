//===--- RemoveSILGenLifetimes.swift -----------------------------------------==//
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

// Until SILGen gets out of the business of trying to manage lifetimes, we
// clean and remove instructions so that lifetimes can derived from solely from
// uses during LifetimeResolution.
let removeSILGenLifetimesPass = FunctionPass(name: "remove-silgen-lifetimes") {
  (function: Function, context: FunctionPassContext) in

  func processInst(_ inst: Instruction) {
    switch inst {
    // TODO: until LifetimeResolution handles copies of guaranteed values,
    //  limit deletion to copies of owned values.
    case let copy as CopyValueInst where copy.operand.value.ownership == .owned:
      copy.replace(with: copy.operand.value, context)

    default:
      return
    }
  }

  for block in function.blocks {
    for inst in block.instructions {
      processInst(inst)
    }
  }
}
