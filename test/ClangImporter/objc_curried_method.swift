// RUN: %target-swift-frontend -swift-version 3 -typecheck %s -enable-objc-interop -import-objc-header %S/Inputs/objc_curried_method.h
// RUN: %target-swift-frontend -swift-version 4 -typecheck %s -enable-objc-interop -import-objc-header %S/Inputs/objc_curried_method.h

// rdar://problem/32588152

func apply(_: (Foo) -> () -> Void) {}

apply(Foo.someMethod)

