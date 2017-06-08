// RUN: not %target-swift-frontend -typecheck %s -import-objc-header %S/Inputs/typedef-with-generic-param.h 2>&1

// REQUIRES: OS=macosx

typealias Result<T> = (T?, Error?)
typealias Handler<T> = (Result<T>) -> Void

func foo<T>(_ handler: Handler<T>?) {}

let _ = Foo<AnyObject>(completionBlock: foo)
// expected-error@-1 {{'Foo<AnyObject>' cannot be constructed because it has no accessible initializers}}
