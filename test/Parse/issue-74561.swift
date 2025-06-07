// RUN: %empty-directory(%t)

// Make sure we can parse with and without skipping.
// RUN: %target-typecheck-verify-swift
// RUN: %target-swift-frontend -verify -emit-module -experimental-skip-non-inlinable-function-bodies -module-name Mod -emit-module-path %t/Mod.swiftmodule %s

// https://github.com/swiftlang/swift/issues/74561
// Make sure we can parse this.
#sourceLocation(file: "A", line: 3)
public func foo(_ param: Int) {
#sourceLocation()
}

#sourceLocation(file: "B", line: 3)
@inlinable
public func bar(_ param: Int) {
#sourceLocation()
}
