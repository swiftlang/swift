// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend %t/generics.swift -module-name Generics -cxx-interoperability-mode=default -typecheck -verify -emit-clang-header-path %t/generics.h
// RUN: %target-interop-build-clangxx -fno-exceptions -std=gnu++20 -c %t/isolated-hashable-execution.cpp -I %t -o %t/isolated-hashable-execution.o
// RUN: %target-build-swift %t/generics.swift -o %t/isolated-hashable-execution -Xlinker %t/isolated-hashable-execution.o -module-name Generics -Xfrontend -entry-point-function-name -Xfrontend swiftMain
// RUN: %target-codesign %t/isolated-hashable-execution
// RUN: not --crash %target-run %t/isolated-hashable-execution 2>&1 | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: concurrency_runtime
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deploy_concurrency
// UNSUPPORTED: back_deployment_runtime

//--- generics.swift
@globalActor
private actor TestActor {
    static let shared = TestActor()
}

@_expose(Cxx)
public struct IsolatedHashable: @TestActor Hashable {
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
#include "generics.h"

int main() {
  auto value = Generics::IsolatedHashable::init(7);
  (void)Generics::genericAreEqual(value, value);
  return 0;
}

// CHECK: Fatal error: Swift protocol conformance is unavailable in the current execution context
