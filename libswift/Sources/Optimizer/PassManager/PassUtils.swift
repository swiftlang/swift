//===--- PassUtils.swift - Utilities for optimzation passes ---------------===//
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

import LibSwiftSIL
import OptimizerBridging

public typealias BridgedFunctionPassCtxt =
  OptimizerBridging.BridgedFunctionPassCtxt

struct FunctionPassContext {

  fileprivate let passContext: BridgedPassContext
  fileprivate let function: Function
  
  func erase(instruction: Instruction) {
    PassContext_eraseInstruction(passContext, instruction.bridged)
  }
  
  private func notifyChanges(_ kind: ChangeNotificationKind) {
    PassContext_notifyChanges(passContext, kind)
  }
}

struct FunctionPass {

  let name: String
  let runFunction: (Function, FunctionPassContext) -> ()

  public init(name: String,
              _ runFunction: @escaping (Function, FunctionPassContext) -> ()) {
    self.name = name
    self.runFunction = runFunction
  }

  func run(_ bridgedCtxt: BridgedFunctionPassCtxt) {
    let function = bridgedCtxt.function.function
    let context = FunctionPassContext(passContext: bridgedCtxt.passContext,
                                      function: function)
    runFunction(function, context)
  }
}

