// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend %t/generics.swift -module-name Generics -cxx-interoperability-mode=default -enable-experimental-feature GenerateBindingsForHashableRequirementsInCXX -typecheck -verify -emit-clang-header-path %t/generics.h
// RUN: %target-interop-build-clangxx -fno-exceptions -std=gnu++20 -c %t/isolated-hashable-execution.cpp -I %t -o %t/isolated-hashable-execution.o
// RUN: %target-interop-build-swift %t/generics.swift -o %t/isolated-hashable-execution -Xlinker %t/isolated-hashable-execution.o -module-name Generics -Xfrontend -entry-point-function-name -Xfrontend swiftMain -enable-experimental-feature GenerateBindingsForHashableRequirementsInCXX
// RUN: %target-codesign %t/isolated-hashable-execution
// RUN: not --crash %target-run %t/isolated-hashable-execution 2>&1 | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: concurrency_runtime
// REQUIRES: swift_feature_GenerateBindingsForHashableRequirementsInCXX
// UNSUPPORTED: back_deployment_runtime
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deploy_concurrency
// The test starts a thread.
// UNSUPPORTED: OS=wasip1

//--- generics.swift
@_expose(Cxx)
public struct IsolatedHashable: @MainActor Hashable {
    public let value: Int

    public init(_ value: Int) {
        self.value = value
    }
}

@_expose(Cxx)
public func genericAreEqual<T: Hashable>(_ a: T, _ b: T) -> Bool {
    return a == b
}

//--- isolated-hashable-execution.cpp
#include <cassert>
#include <cstdio>
#include <thread>
#include "generics.h"

int main() {
  auto value = Generics::IsolatedHashable::init(7);
  // The main thread runs on the main actor, so the conformance can be used.
  // The second call uses the cached conformance lookup.
  assert(Generics::genericAreEqual(value, value));
  assert(Generics::genericAreEqual(value, value));
  puts("OK on the main thread");
  fflush(stdout);

  // The execution context is checked on every use, so using the cached
  // conformance on another thread is a fatal error.
  std::thread([&] { (void)Generics::genericAreEqual(value, value); }).join();
  return 0;
}

// CHECK: OK on the main thread
// CHECK: Fatal error: Swift protocol conformance is unavailable in the current execution context
