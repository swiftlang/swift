// RUN: %target-typecheck-verify-swift -disable-availability-checking -enable-experimental-feature RuntimeDiscoverableAttrs

// REQUIRES: asserts

@runtimeMetadata
struct Flag<T> {
  init(_: T.Type, description: String = "") {}
  init(_: Any, description: String = "") {}
  init<Base>(_: KeyPath<Base, T>, description: String = "") {}
}

@runtimeMetadata
struct OnlyPropsTest<B, V> {
  init(_: KeyPath<B, V>) {}
}

@Flag("global") func gloabalFn() {}

@Flag
struct A { // Ok
  @Flag("v1") var v1: String = "" // Ok

  @Flag var comp: Int { // Ok
    get { 42 }
    @Flag set {} // expected-error {{@Flag can only be applied to non-generic types, methods, instance properties, and global functions}}
  }

  @Flag static var v2: String = "" // Ok
  // expected-error@-1 {{@Flag can only be applied to non-generic types, methods, instance properties, and global functions}}

  @Flag static func test1() -> Int { 42 } // Ok
  @Flag("test2") func test2() {} // Ok

  @Flag func genericFn<T>(_: T) {} // expected-error {{@Flag can only be applied to non-generic types, methods, instance properties, and global functions}}

  @OnlyPropsTest @Flag("x") var x: [Int]? = [] // Ok
}

struct Context<T> {
  @Flag struct B {} // expected-error {{@Flag can only be applied to non-generic types, methods, instance properties, and global functions}}

  @Flag let x: Int = 0 // expected-error {{@Flag can only be applied to non-generic types, methods, instance properties, and global functions}}
  @Flag subscript(v: Int) -> Bool { false }
  // expected-error@-1 {{@Flag can only be applied to non-generic types, methods, instance properties, and global functions}}

  @Flag func fnInGenericContext() {}
  // expected-error@-1 {{@Flag can only be applied to non-generic types, methods, instance properties, and global functions}}
}

do {
  @Flag let x: Int = 42 // expected-warning {{}}
  // expected-error@-1 {{@Flag can only be applied to non-generic types, methods, instance properties, and global functions}}

  @Flag func localFn() {}
  // expected-error@-1 {{@Flag can only be applied to non-generic types, methods, instance properties, and global functions}}
}
