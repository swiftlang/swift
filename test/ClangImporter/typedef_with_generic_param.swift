// RUN: %target-swift-frontend -typecheck %s -import-objc-header %S/Inputs/typedef-with-generic-param.h 2>&1

// REQUIRES: OS=macosx

typealias Handler<T> = (T?, Error?) -> Void

func foo<T>(_ handler: Handler<T>?) {}

let _ = Foo<AnyObject>(completionBlock: foo)
