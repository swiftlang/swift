// RUN: %target-swift-emit-ir %s -DIGNORE_FAILS -enable-experimental-feature Embedded -wmo -o /dev/null
// RUN: %target-swift-emit-ir %s -enable-experimental-feature Embedded -wmo -verify

// REQUIRES: swift_feature_Embedded

struct MyStruct<Item> : ~Copyable {
    var member = "42"

    init() {}
    deinit {}
    mutating func foo() {}
}

var escape: (()->())?

#if !IGNORE_FAILS

public func test() {
    var s = MyStruct<Int>() // expected-error {{capturing generic non-copyable type with deinit in escaping closure not supported in embedded Swift}}
    s.foo()
    escape = {
        s.foo()
    }
}

//

struct Outer: ~Copyable {
  var inner: MyStruct<Int>
}

public func testNested() {
    var s = Outer(inner: MyStruct<Int>()) // expected-error {{capturing generic non-copyable type with deinit in escaping closure not supported in embedded Swift}}
    s.inner.foo()
    escape = {
        s.inner.foo()
    }
}

//

enum E: ~Copyable {
  case A(MyStruct<Int>)
  case B

  mutating func foo() {}
}

public func testEnum() {
    var s = E.A(MyStruct<Int>()) // expected-error {{capturing generic non-copyable type with deinit in escaping closure not supported in embedded Swift}}
    s.foo()
    escape = {
        s.foo()
    }
}

#endif

//

struct StructWithoutDeinit<Item> {
    var member = "42"

    init() {}
    mutating func foo() {}
}

public func testWithoutDeinit() {
    var s = StructWithoutDeinit<Int>()
    s.foo()
    escape = {
        s.foo()
    }
}

//

struct NonCopyableStructWithoutDeinit<Item>: ~Copyable {
    var member = "42"

    init() {}
    mutating func foo() {}
}

public func testNonCopyableWithoutDeinit() {
    var s = NonCopyableStructWithoutDeinit<Int>()
    s.foo()
    escape = {
        s.foo()
    }
}

//

struct NonGenericStruct : ~Copyable {
    var member = "42"

    init() {
    }

    deinit {
    }

    mutating func foo() {
    }
}

public func testNonGeneric() {
    var s = NonGenericStruct()
    s.foo()
    escape = {
        s.foo()
    }
}
