// RUN: not %target-swift-frontend %s -typecheck

// https://github.com/apple/swift/issues/47626

protocol P {}

func bar(p: P?) {
  foo(p is String)
}
    
func foo<T>(_: T, _: T) {}
func foo<T>(_: T?, _: T?) {}

