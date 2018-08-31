// RUN: %target-jit-run -O %s | %FileCheck %s
// REQUIRES: executable_test
// REQUIRES: swift_interpreter

// Check that when running the jit with -O we don't crash with an unresolved
// metadata symbol.

protocol Foo {
    func foo() -> Int
}

struct Dummy: Foo {
    func foo() -> Int { return 42 }
}

// CHECK: ok
print("ok")
