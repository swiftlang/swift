// RUN: %target-swift-frontend -typecheck %s -enable-objc-interop -import-objc-header %S/Inputs/objc_curried_method.h

// rdar://problem/32588152

func apply(_: (Foo) -> () -> Void) {}

apply(Foo.someMethod)

