//===----------- FunctionTest.swift ---------------------------------------===//
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
//
// Optimizer test infrastructure.
//
// For general documentation on how to write tests see `Test.swift` in the
// SIL module.
// To add an optimizer test, use `FunctionTest` instead of `Test` and register
// it in `registerOptimizerTests`.
//
//===----------------------------------------------------------------------===//

import SIL
import OptimizerBridging

/// Like `SIL.Test`,  but provides a `FunctionPass` to the invocation closure.
struct FunctionTest {
  let name: String
  let invocation: FunctionTestInvocation

  public init(_ name: String, invocation: @escaping FunctionTestInvocation) {
    self.name = name
    self.invocation = invocation
  }
}

/// The type of the closure passed to a FunctionTest.
typealias FunctionTestInvocation = @convention(thin) (Function, TestArguments, FunctionPassContext) -> ()

public func registerOptimizerTests() {
  SIL.registerTests()

  // Register each test.
  registerFunctionTests(
    addressOwnershipLiveRangeTest,
    argumentConventionsTest,
    borrowIntroducersTest,
    enclosingValuesTest,
    forwardingDefUseTest,
    forwardingUseDefTest,
    getPullbackClosureInfoTest,
    interiorLivenessTest,
    lifetimeDependenceRootTest,
    lifetimeDependenceScopeTest,
    lifetimeDependenceUseTest,
    linearLivenessTest,
    localVariableReachableUsesTest,
    localVariableReachingAssignmentsTest,
    rangeOverlapsPathTest,
    rewrittenCallerBodyTest,
    specializedFunctionSignatureAndBodyTest,
    variableIntroducerTest
  )

  // Finally register the thunk they all call through.
  registerFunctionTestThunk(functionTestThunk)
}

private func registerFunctionTests(_ tests: FunctionTest...) {
  tests.forEach { registerFunctionTest($0) }
}

private func registerFunctionTest(_ test: FunctionTest) {
  test.name._withBridgedStringRef { ref in
    registerFunctionTest(ref, castToOpaquePointer(fromInvocation: test.invocation))
  }
}

/// The function called by the swift::test::FunctionTest which invokes the
/// actual test function.
///
/// This function is necessary because tests need to be written in terms of
/// native Swift types (Function, TestArguments, FunctionPassContext)
/// rather than their bridged variants, but such a function isn't representable
/// in C++. This thunk unwraps the bridged types and invokes the real function.
private func functionTestThunk(
  _ erasedInvocation: UnsafeMutableRawPointer,
  _ function: BridgedFunction, 
  _ arguments: BridgedTestArguments, 
  _ bridgedContext: BridgedContext) {
  let invocation = castToInvocation(fromOpaquePointer: erasedInvocation)
  let context = FunctionPassContext(_bridged: bridgedContext)
  invocation(function.function, arguments.native, context)
}

/// Bitcast a thin test closure to void *.
///
/// Needed so that the closure can be represented in C++ for storage in the test
/// registry.
private func castToOpaquePointer(fromInvocation invocation: FunctionTestInvocation) -> UnsafeMutableRawPointer {
  return unsafeBitCast(invocation, to: UnsafeMutableRawPointer.self)
}

/// Bitcast a void * to a thin test closure.
///
/// Needed so that the closure stored in the C++ test registry can be invoked
/// via the functionTestThunk.
private func castToInvocation(fromOpaquePointer erasedInvocation: UnsafeMutableRawPointer) -> FunctionTestInvocation {
  return unsafeBitCast(erasedInvocation, to: FunctionTestInvocation.self)
}
