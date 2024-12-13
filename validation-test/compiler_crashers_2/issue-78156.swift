// RUN: not --crash %target-swift-frontend -target %target-swift-5.9-abi-triple -typecheck %s

// https://github.com/apple/swift/issues/78156

func f<each T>(_: repeat [each T]) -> (repeat [each T], Bool) {}
let _ = f(f(f([4])))
