// RUN: %empty-directory(%t)

// Make sure we can parse with and without skipping.
// RUN: %target-typecheck-verify-swift
// RUN: not %target-swift-frontend -emit-module -experimental-skip-non-inlinable-function-bodies -module-name Mod -emit-module-path %t/Mod.swiftmodule -diagnostic-style=llvm %s 2>&1 | %FileCheck %s

// https://github.com/swiftlang/swift/issues/74561
// Make sure we can parse this.
#sourceLocation(file: "A", line: 3)
public func foo(_ param: Int) {
#sourceLocation()
}

// FIXME: This should parse correctly.
#sourceLocation(file: "B", line: 3)
@inlinable
public func bar(_ param: Int) {
#sourceLocation()
}
// CHECK: B:6:1: error: parameterless closing #sourceLocation() directive without prior opening #sourceLocation(file:,line:) directive
