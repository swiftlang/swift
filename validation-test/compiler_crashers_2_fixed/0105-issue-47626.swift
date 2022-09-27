// RUN: not %target-swift-frontend %s -typecheck
// REQUIRES: asserts

// https://github.com/apple/swift/issues/47626

protocol P {}

func bar(p: P?) {
  foo(p is String)
}
    
func foo<T>(_: T, _: T) {}
func foo<T>(_: T?, _: T?) {}

