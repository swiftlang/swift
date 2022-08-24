//===--- PassRegistration.swift - Register optimization passes -------------===//
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
import OptimizerBridging
import Parse

@_cdecl("initializeSwiftModules")
public func initializeSwiftModules() {
  registerSILClasses()
  registerSwiftPasses()
  initializeSwiftParseModules()
}

private func registerPass(
      _ pass: ModulePass,
      _ runFn: @escaping (@convention(c) (BridgedPassContext) -> ())) {
  pass.name.withStringRef { nameStr in
    SILPassManager_registerModulePass(nameStr, runFn)
  }
}

private func registerPass(
      _ pass: FunctionPass,
      _ runFn: @escaping (@convention(c) (BridgedFunctionPassCtxt) -> ())) {
  pass.name.withStringRef { nameStr in
    SILPassManager_registerFunctionPass(nameStr, runFn)
  }
}

private func registerPass<InstType: Instruction>(
      _ pass: InstructionPass<InstType>,
      _ runFn: @escaping (@convention(c) (BridgedInstructionPassCtxt) -> ())) {
  pass.name.withStringRef { nameStr in
    SILCombine_registerInstructionPass(nameStr, runFn)
  }
}

private func registerSwiftPasses() {
  // Function passes
  registerPass(mergeCondFailsPass, { mergeCondFailsPass.run($0) })
  registerPass(computeEffects, { computeEffects.run($0) })
  registerPass(objCBridgingOptimization, { objCBridgingOptimization.run($0) })
  registerPass(stackPromotion, { stackPromotion.run($0) })
  registerPass(assumeSingleThreadedPass, { assumeSingleThreadedPass.run($0) })
  registerPass(releaseDevirtualizerPass, { releaseDevirtualizerPass.run($0) })

  // Instruction passes
  registerPass(simplifyBeginCOWMutationPass, { simplifyBeginCOWMutationPass.run($0) })
  registerPass(simplifyGlobalValuePass, { simplifyGlobalValuePass.run($0) })
  registerPass(simplifyStrongRetainPass, { simplifyStrongRetainPass.run($0) })
  registerPass(simplifyStrongReleasePass, { simplifyStrongReleasePass.run($0) })

  // Test passes
  registerPass(functionUsesDumper, { functionUsesDumper.run($0) })
  registerPass(silPrinterPass, { silPrinterPass.run($0) })
  registerPass(escapeInfoDumper, { escapeInfoDumper.run($0) })
  registerPass(addressEscapeInfoDumper, { addressEscapeInfoDumper.run($0) })
  registerPass(accessDumper, { accessDumper.run($0) })
  registerPass(rangeDumper, { rangeDumper.run($0) })
  registerPass(runUnitTests, { runUnitTests.run($0) })
}
