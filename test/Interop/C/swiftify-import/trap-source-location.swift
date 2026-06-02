// REQUIRES: executable_test
// REQUIRES: swift_test_mode_optimize_none

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-build-swift %t/test.swift -plugin-path %swift-plugin-dir \
// RUN:   -import-bridging-header %t/bridging.h \
// RUN:   -Xfrontend -validate-tbd-against-ir=none -o %t/test -target %target-swift-6.2-abi-triple
// RUN: %target-codesign %t/test

// RUN: env SWIFT_BACKTRACE=enable=no %{python} %S/../../../Inputs/not.py "%target-run %t/test interop" 2>&1 | %FileCheck %s --check-prefix CLANG
// RUN: env SWIFT_BACKTRACE=enable=no %{python} %S/../../../Inputs/not.py "%target-run %t/test"         2>&1 | %FileCheck %s --check-prefix SWIFT

// NOTE: not.py is used above instead of "not --crash" because simctl's exit
// status doesn't reflect whether its child process crashed or not. So "not
// --crash %target-run ..." always fails when testing for the iOS Simulator.

// Trigger Optional unwrapping inside a macro by passing a default Span to a safe wrapper
// where the underlying function does not accept null pointers - the base pointer of the
// empty Span is null, and triggers a runtime exception. This is a regression test to make
// sure the source location of the trap is correct.

//--- test.swift
@_SwiftifyImport(.countedBy(pointer: .param(1), count: "len"), .nonescaping(pointer: .param(1)))
public func invokeMacroInSwiftModule(_ p: UnsafePointer<Int32>, _ len: Int32) {}

if CommandLine.arguments.dropFirst().first == "interop" {
  invokeMacroInClangModule(Span()) // CLANG: bridging.h:3: Fatal error: Unexpectedly found nil while unwrapping an Optional value
} else {
  invokeMacroInSwiftModule(Span()) // SWIFT: test.swift:2: Fatal error: Unexpectedly found nil while unwrapping an Optional value
}
//--- bridging.h
#include <ptrcheck.h>

static inline void invokeMacroInClangModule(
    const int* __counted_by(count) __attribute__((__noescape__)) arr,
    int count) {}
