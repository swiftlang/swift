//===--- Verifier.swift ---------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2024 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import SIL
import OptimizerBridging

private protocol VerifyableInstruction : Instruction {
  func verify(_ context: FunctionPassContext)
}

private func require(_ condition: Bool, _ message: @autoclosure () -> String) {
  if !condition {
    fatalError(message())
  }
}

extension Function {
  func verify(_ context: FunctionPassContext) {
    for block in blocks {
      for inst in block.instructions {
        if let verifyableInst = inst as? VerifyableInstruction {
          verifyableInst.verify(context)
        }
      }
    }
  }
}

func registerVerifier() {
  BridgedUtilities.registerVerifier(
    { (bridgedCtxt: BridgedPassContext, bridgedFunction: BridgedFunction) in
      let context = FunctionPassContext(_bridged: bridgedCtxt)
      bridgedFunction.function.verify(context)
    }
  )
}
