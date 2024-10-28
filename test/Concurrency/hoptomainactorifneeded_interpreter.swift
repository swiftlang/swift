// RUN: %empty-directory(%t)

// Build the non-preconcurrency library.
// RUN: %target-build-swift-dylib(%t/%target-library-name(PreconcurrencyUnchecked)) %S/Inputs/hoptomainactorifneeded_interpreter.swift -Xfrontend -disable-availability-checking -swift-version 5 -module-name PreconcurrencyUnchecked -emit-module
// RUN: %target-codesign %t/%target-library-name(PreconcurrencyUnchecked)

// RUN: %target-swiftc_driver -swift-version 6 -enable-experimental-feature GenerateForceToMainActorThunks -I %t %s -o %t/main -L %t -lPreconcurrencyUnchecked %target-rpath(%t)
// RUN: %target-codesign %t/main
// RUN: %target-run %t/main %t/%target-library-name(PreconcurrencyUnchecked)

// RUN: %target-swiftc_driver -swift-version 6 -I %t %s -o %t/main_crash -L %t -lPreconcurrencyUnchecked -DCRASH %target-rpath(%t)
// RUN: %target-codesign %t/main_crash
// RUN: %target-run %t/main_crash %t/%target-library-name(PreconcurrencyUnchecked)

// REQUIRES: asserts
// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: objc_interop

// UNSUPPORTED: back_deployment_runtime
// UNSUPPORTED: back_deploy_concurrency
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: freestanding

// READ THIS: This test validates that we properly crash in the closure if we do
// not have GenerateForceToMainActorThunks set, and the inverse (no crash) if we
// set the flag.

import PreconcurrencyUnchecked
import StdlibUnittest

actor Custom {
}

@globalActor
struct CustomActor {
  static var shared: Custom {
    return Custom()
  }
}

let tests = TestSuite("HopToMainActor End To End")

tests.test("Check if we crash (or not) depending on the compilation mode") { @CustomActor () async -> () in
  #if CRASH
  expectCrashLater()
  #endif
  useClosure { @MainActor in
  }
}

@CustomActor
func callUseClosureSync() {
  useClosure { @MainActor in
  }
}

tests.test("Check if we crash (or not) depending on the compilation mode 2") { @CustomActor () async -> () in
  #if CRASH
  expectCrashLater()
  #endif
  callUseClosureSync()
}

tests.test("No crash if run on main actor in both modes") { @MainActor () async -> () in
  useClosure { @MainActor in
  }
}

await runAllTestsAsync()
