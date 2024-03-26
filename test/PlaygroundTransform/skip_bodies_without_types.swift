// RUN: %empty-directory(%t)
// RUN: %target-build-swift -whole-module-optimization -module-name PlaygroundSupport -emit-module-path %t/PlaygroundSupport.swiftmodule -parse-as-library -c -o %t/PlaygroundSupport.o %S/Inputs/SilentPCMacroRuntime.swift %S/Inputs/PlaygroundsRuntime.swift
// RUN: %target-build-swift -emit-module -Xfrontend -experimental-skip-non-inlinable-function-bodies-without-types -Xfrontend -playground -I=%t %s
// REQUIRES: executable_test

import PlaygroundSupport

struct Foo {
    func Bar() {
        print("hello")
    }
}

// This test will cause the frontend to crash without the fix for skipping playground transformation of functions that have skipped type information. If it doesn't crash, it passes.
