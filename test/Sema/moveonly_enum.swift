// RUN: %target-typecheck-verify-swift

enum Foo : ~Copyable {
    deinit {} // expected-error {{deinitializers are not yet supported on noncopyable enums}}
}

enum Foo2 : ~Copyable {
}
