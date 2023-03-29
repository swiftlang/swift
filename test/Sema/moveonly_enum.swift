// RUN: %target-typecheck-verify-swift

@_moveOnly
enum Foo {
    deinit {} // expected-error {{deinitializers are not yet supported on noncopyable enums}}
}

@_moveOnly
enum Foo2 {
}
