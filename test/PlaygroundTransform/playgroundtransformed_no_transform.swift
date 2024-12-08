// Tests the `@_PlaygroundTransformed` function annotation
// without any Playground transform enabled.
// The annotation shouldn't have any affect in this case.
//
// REQUIRES: executable_test
//
// RUN: %empty-directory(%t)
// RUN: cp %s %t/main.swift
//
// - Normal compilation, no transform
// RUN: %target-build-swift -o %t/main -I=%t %t/main.swift
// RUN: %target-codesign %t/main
// RUN: %target-run %t/main | %FileCheck %s

// Annotated function - silently ignored
@_PlaygroundTransformed
func f(x: Int, _ y: Float = 7.62) -> Int {
    return x + Int(y)
}
let foo = f(x: 42)
print("foo=\(foo)")

// CHECK:      foo=49
