// RUN: %target-swift-emit-ir %s -enable-experimental-feature Embedded -wmo -verify -o /dev/null
// RUN: %target-swift-emit-ir %s -enable-experimental-feature Embedded -wmo -Osize -o /dev/null

// REQUIRES: swift_feature_Embedded

struct MyStruct<Item> : ~Copyable {
    var p: UnsafeMutablePointer<Int>

    init() { p = .allocate(capacity: 1) }
    // A non-trivial deinit, so that destroying the box really does reference it.
    deinit { p.deallocate() }
    mutating func foo() { p.pointee += 1 }
}

// Escaping into a class property, so that the box survives to IRGen rather than
// being promoted to the stack.
public final class Keeper {
    public var run: (() -> ())?
    public init() {}
}

public func test(_ k: Keeper) {
    var s = MyStruct<Int>()
    s.foo()
    k.run = {
        s.foo()
    }
}

//

struct Outer: ~Copyable {
  var inner: MyStruct<Int>
}

public func testNested(_ k: Keeper) {
    var s = Outer(inner: MyStruct<Int>())
    s.inner.foo()
    k.run = {
        s.inner.foo()
    }
}

//

enum E: ~Copyable {
  case A(MyStruct<Int>)
  case B

  mutating func foo() {}
}

public func testEnum(_ k: Keeper) {
    var s = E.A(MyStruct<Int>())
    s.foo()
    k.run = {
        s.foo()
    }
}

//

// A generic non-copyable type with a deinit that is *only* ever boxed: no
// non-generic non-copyable type has it as a field or payload, so the `alloc_box`
// is the only thing that makes IRGen emit metadata for it.
struct OnlyBoxed<Item>: ~Copyable {
    var p: UnsafeMutablePointer<Int>
    init() { p = .allocate(capacity: 1) }
    deinit { p.deallocate() }
    mutating func foo() { p.pointee += 1 }
}

public func testOnlyBoxed(_ k: Keeper) {
    var s = OnlyBoxed<Int>()
    s.foo()
    k.run = {
        s.foo()
    }
}

//

var escape: (()->())?

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
