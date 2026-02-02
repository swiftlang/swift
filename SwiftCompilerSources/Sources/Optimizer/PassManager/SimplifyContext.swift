//===--- SimplifyContext.swift --------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import SIL
import OptimizerBridging

struct SimplifyContext : MutatingContext {
  let _bridged: BridgedContext
  let notifyInstructionChanged: (Instruction) -> ()
  let preserveDebugInfo: Bool

  func constantFold(builtin: BuiltinInst) -> Value? {
    bridgedPassContext.constantFoldBuiltin(builtin.bridged).value
  }
}

// Those Type APIs get information from IRGenModule, so they are formally not part of SIL and
// therefore need to be defined here, in the Optimizer module.
extension Type {
  func getStaticSize(context: SimplifyContext) -> Int? {
    let v = context.bridgedPassContext.getStaticSize(self.bridged)
    return v == -1 ? nil : v
  }

  func getStaticAlignment(context: SimplifyContext) -> Int? {
    let v = context.bridgedPassContext.getStaticAlignment(self.bridged)
    return v == -1 ? nil : v
  }

  func getStaticStride(context: SimplifyContext) -> Int? {
    let v = context.bridgedPassContext.getStaticStride(self.bridged)
    return v == -1 ? nil : v
  }
}
