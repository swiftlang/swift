//===--- Passes.swift ---- instruction and function passes ----------------===//
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
import OptimizerBridging

struct FunctionPass {

  let name: String
  let runFunction: (Function, PassContext) -> ()

  public init(name: String,
              _ runFunction: @escaping (Function, PassContext) -> ()) {
    self.name = name
    self.runFunction = runFunction
  }

  func run(_ bridgedCtxt: BridgedFunctionPassCtxt) {
    let function = bridgedCtxt.function.function
    let context = PassContext(_bridged: bridgedCtxt.passContext)
    runFunction(function, context)
  }
}

struct InstructionPass<InstType: Instruction> {

  let name: String
  let runFunction: (InstType, PassContext) -> ()

  public init(name: String,
              _ runFunction: @escaping (InstType, PassContext) -> ()) {
    self.name = name
    self.runFunction = runFunction
  }

  func run(_ bridgedCtxt: BridgedInstructionPassCtxt) {
    let inst = bridgedCtxt.instruction.getAs(InstType.self)
    let context = PassContext(_bridged: bridgedCtxt.passContext)
    runFunction(inst, context)
  }
}

struct ModulePass {

  let name: String
  let runFunction: (ModulePassContext) -> ()

  public init(name: String,
              _ runFunction: @escaping (ModulePassContext) -> ()) {
    self.name = name
    self.runFunction = runFunction
  }

  func run(_ bridgedCtxt: BridgedPassContext) {
    let context = ModulePassContext(_bridged: bridgedCtxt)
    runFunction(context)
  }
}
