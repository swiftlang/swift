// RUN: %target-typecheck-verify-swift -disable-availability-checking -enable-experimental-feature RuntimeDiscoverableAttrs

// REQUIRES: asserts

@runtimeMetadata
struct Flag<T> {
  init(attachedTo: T.Type, _ description: String = "") {}
  init<Args>(attachedTo: (Args) -> T, _ description: String = "") {}
  init<Base>(attachedTo: KeyPath<Base, T>, _ description: String = "") {}
}

@runtimeMetadata
struct OnlyPropsTest<B, V> {
  init(attachedTo: KeyPath<B, V>) {}
}

@Flag("global") func gloabalFn() {}

@Flag
struct A { // Ok
  @Flag("v1") var v1: String = "" // Ok

  @Flag var comp: Int { // Ok
    get { 42 }
    @Flag set {} // expected-error {{@Flag can only be applied to non-generic types, methods, instance properties, and global functions}}
  }

  @Flag static var v2: String = ""
  // expected-error@-1 {{@Flag can only be applied to non-generic types, methods, instance properties, and global functions}}

  @Flag static func test1() -> Int { 42 } // Ok
  @Flag("test2") func test2() {} // Ok

  @Flag func genericFn<T>(_: T) {} // expected-error {{@Flag can only be applied to non-generic types, methods, instance properties, and global functions}}

  @OnlyPropsTest @Flag("x") var x: [Int]? = [] // Ok

  class Inner {
    @OnlyPropsTest @Flag("test property") var test: [Int]? = nil // Ok
  }
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

@Flag @Flag func test() {} // expected-error {{duplicate runtime discoverable attribute}}

extension A.Inner {
  @Flag("B type") struct B { // Ok
    @Flag static func extInnerStaticTest() {} // Ok
    @Flag static func extInnerTest() {} // Ok

    @Flag let stored: Int = 42 // Ok
  }

  @Flag static func extStaticTest() {} // Ok
  @Flag static func extTest() {} // Ok

  @OnlyPropsTest @Flag("computed in extension") var extComputed: Int { // Ok
    get { 42 }
  }
}

@Flag func test(_: Int) {} // Ok
@Flag func test(_: String) {} // Ok

struct TestNoAmbiguity {
  @Flag static func testStatic() -> Int {} // Ok
  @Flag static func testStatic() {} // Ok

  @Flag func testInst(_: Int, _: String) {} // Ok
  @Flag func testInst(_: Int, _: Int) {} // Ok
}
