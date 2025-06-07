// RUN: %target-typecheck-verify-swift -analyze-requirement-machine

public func f<T: FixedWidthInteger>(_: T) {}
