// Optional components in Embedded Swift key paths: chaining (`?`), forcing
// (`!`), and the implicit wrap that makes a chained key path's value optional.
//
// The non-embedded walker reads the tag with `Builtin.getEnumTag` on a
// statically-typed value, which the embedded walker can't do — it has raw
// pointers and metadata pointers, and can't instantiate generics. Instead it
// calls the payload type's `getEnumTagSinglePayload` value witness, which is
// reachable from the metadata the key path buffer already carries: a component's
// recorded type is the type *after* applying it, so for chain/force that is
// exactly the `Wrapped` payload type the witness wants.

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -enable-experimental-feature Embedded -parse-as-library -wmo -O %t/ok.swift -module-name ok -c -o %t/ok-o.o
// RUN: %target-embedded-link %t/ok-o.o -o %t/ok-o.out %target-clang-resource-dir-opt -dead_strip
// RUN: %target-run %t/ok-o.out | %FileCheck %s

// RUN: %target-swift-frontend -enable-experimental-feature Embedded -parse-as-library -wmo -Onone %t/ok.swift -module-name ok -c -o %t/ok-onone.o
// RUN: %target-embedded-link %t/ok-onone.o -o %t/ok-onone.out %target-clang-resource-dir-opt -dead_strip
// RUN: %target-run %t/ok-onone.out | %FileCheck %s

// Force-unwrapping nil through a key path must trap, like `!` would.
// RUN: %target-swift-frontend -enable-experimental-feature Embedded -parse-as-library -wmo -O %t/trap.swift -module-name trap -c -o %t/trap.o
// RUN: %target-embedded-link %t/trap.o -o %t/trap.out %target-clang-resource-dir-opt -dead_strip
// RUN: %target-run not --crash %t/trap.out

// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// REQUIRES: OS=macosx
// REQUIRES: swift_feature_Embedded

//--- ok.swift

public struct Inner { public var x: Int32 = 0; public var y: Int32 = 0 }
public struct Outer { public var tag: Int32 = 0; public var opt: Inner? = nil }
public struct Nested { public var o: Outer = Outer() }
public final class Cls { public var opt: Inner? = nil }

// A class-typed payload exercises the extra-inhabitant (spare bit) tag
// representation, where `.none` is a reserved pointer value rather than a
// separate tag byte.
public final class Ref { public var v: Int32 = 0 }
public struct HasRef { public var r: Ref? = nil }

// The key paths come from `@inline(never)` functions and are applied behind one,
// so the optimizer can't fold the application into direct field accesses and the
// walker actually runs.
@inline(never) public func kpChain() -> KeyPath<Outer, Int32?> { \Outer.opt?.x }
@inline(never) public func kpForce() -> WritableKeyPath<Outer, Int32> { \Outer.opt!.x }
@inline(never) public func kpDeepChain() -> KeyPath<Nested, Int32?> { \Nested.o.opt?.y }
@inline(never) public func kpClassChain() -> KeyPath<Cls, Int32?> { \Cls.opt?.x }
@inline(never) public func kpRefChain() -> KeyPath<HasRef, Int32?> { \HasRef.r?.v }
@inline(never) public func kpWholeOptional() -> KeyPath<Outer, Inner?> { \Outer.opt }

@inline(never) public func read<R, V>(_ r: R, _ kp: KeyPath<R, V>) -> V {
  r[keyPath: kp]
}
@inline(never) public func write<R, V>(_ r: inout R, _ kp: WritableKeyPath<R, V>, _ v: V) {
  r[keyPath: kp] = v
}

@main
struct Main {
  static func main() {
    var o = Outer(tag: 1, opt: Inner(x: 42, y: 7))

    // Chaining through `.some` yields the wrapped value.
    print(read(o, kpChain()) == 42 ? "OK!" : "FAIL") // CHECK: OK!

    // Chaining through `.none` short-circuits the rest of the chain and yields nil.
    // A broken tag read would fall through and return the payload's zeroed bytes
    // (i.e. 0, not nil), so this genuinely exercises the witness call.
    print(read(Outer(tag: 1, opt: nil), kpChain()) == nil ? "OK!" : "FAIL") // CHECK-NEXT: OK!

    // Force-unwrapping reads and writes through the optional.
    print(read(o, kpForce()) == 42 ? "OK!" : "FAIL") // CHECK-NEXT: OK!
    write(&o, kpForce(), 99)
    print(o.opt!.x == 99 ? "OK!" : "FAIL") // CHECK-NEXT: OK!
    // The sibling field must be untouched — a writeback that copied the whole
    // payload back would clobber it.
    print(o.opt!.y == 7 ? "OK!" : "FAIL") // CHECK-NEXT: OK!

    // An optional component in the middle of a longer chain.
    let n = Nested(o: Outer(tag: 0, opt: Inner(x: 1, y: 55)))
    print(read(n, kpDeepChain()) == 55 ? "OK!" : "FAIL") // CHECK-NEXT: OK!
    print(read(Nested(o: Outer(tag: 0, opt: nil)), kpDeepChain()) == nil ? "OK!" : "FAIL") // CHECK-NEXT: OK!

    // Chaining after crossing a class boundary.
    let c = Cls()
    c.opt = Inner(x: 11, y: 0)
    print(read(c, kpClassChain()) == 11 ? "OK!" : "FAIL") // CHECK-NEXT: OK!
    let c2 = Cls()
    print(read(c2, kpClassChain()) == nil ? "OK!" : "FAIL") // CHECK-NEXT: OK!

    // A class-reference payload, where `.none` is a spare bit pattern rather than a
    // tag byte.
    let r = Ref(); r.v = 5
    print(read(HasRef(r: r), kpRefChain()) == 5 ? "OK!" : "FAIL") // CHECK-NEXT: OK!
    print(read(HasRef(r: nil), kpRefChain()) == nil ? "OK!" : "FAIL") // CHECK-NEXT: OK!

    // Reading a whole optional field (no optional component involved) still works.
    print(read(o, kpWholeOptional())!.x == 99 ? "OK!" : "FAIL") // CHECK-NEXT: OK!
    print(read(Outer(tag: 0, opt: nil), kpWholeOptional()) == nil ? "OK!" : "FAIL") // CHECK-NEXT: OK!
  }
}

//--- trap.swift

public struct Inner { public var x: Int32 = 0 }
public struct Outer { public var opt: Inner? = nil }

@inline(never) public func kpForce() -> WritableKeyPath<Outer, Int32> { \Outer.opt!.x }
@inline(never) public func read<R, V>(_ r: R, _ kp: KeyPath<R, V>) -> V {
  r[keyPath: kp]
}

@main
struct M {
  static func main() {
    _ = read(Outer(opt: nil), kpForce())
  }
}
