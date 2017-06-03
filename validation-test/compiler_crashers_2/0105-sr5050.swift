// RUN: not --crash %target-swift-frontend %s -typecheck

protocol P {}

func bar(p: P?) {
  foo(p is String)
}
    
func foo<T>(_: T, _: T) {}
func foo<T>(_: T?, _: T?) {}

