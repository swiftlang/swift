struct Foo {}
struct Bar {}

extension Foo {

extension Bar {
}

func someFunc() {}

// Closing brace for extension Foo intentionally left off, user is in the
// process of adding the extension

// This check is mostly to make sure we don't crash with an assertion -
// extensions are pushed and popped in an AST walk pre/post and they need to
// match up.
// RUN: %sourcekitd-test -req=cursor -pos=9:6 %s -- %s | %FileCheck %s
// CHECK: source.lang.swift.decl.function.method.instance
