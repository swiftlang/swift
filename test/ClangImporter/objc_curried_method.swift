// RUN: %target-swift-frontend -swift-version 3 -typecheck %s -import-objc-header %S/Inputs/objc_curried_method.h
// RUN: %target-swift-frontend -swift-version 4 -typecheck %s -import-objc-header %S/Inputs/objc_curried_method.h

// REQUIRES: objc_interop

// rdar://problem/32588152

func apply(_: (Foo) -> () -> Void) {}

apply(Foo.someMethod)

