// Embedded key paths store a flat root-to-value byte offset in
// `AnyKeyPath._kvcKeyPathStringPtr` when every component is a struct member or
// tuple element, so projection is a single offset add instead of a walk of the
// component buffer. IRGen computes the offset at compile time, since embedded
// key paths are emitted as static objects — see
// `IRGenModule::emitStaticKeyPathInstance`.

// RUN: %empty-directory(%t)
// RUN: %target-swift-emit-ir %s -module-name kpoff -enable-experimental-feature Embedded -wmo -Onone -o - | %FileCheck -check-prefix=CHECK-IR %s
// RUN: %target-run-simple-swift(-O -enable-experimental-feature Embedded -wmo -runtime-compatibility-version none %target-embedded-posix-shim) | %FileCheck -check-prefix=CHECK-OUT %s
// RUN: %target-run-simple-swift(-Onone -enable-experimental-feature Embedded -wmo -runtime-compatibility-version none %target-embedded-posix-shim) | %FileCheck -check-prefix=CHECK-OUT %s

// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// REQUIRES: PTRSIZE=64
// REQUIRES: swift_feature_Embedded
// Embedded key paths and SIL opaque values don't currently mix: the
// combination trips `getSILArgumentConvention`. `keypaths-static.swift` and
// `keypaths-exec.swift` carry the same XFAIL.
// XFAIL: swift_test_mode_optimize_none_with_opaque_values

public struct Inner { public var a: Int32 = 0; public var b: Int32 = 0 }
public struct Mid { public var pad: Int64 = 0; public var inner = Inner() }
public struct Outer { public var tag: Int32 = 0; public var mid = Mid() }
public struct HasTuple { public var pad: Int32 = 0; public var t: (Int32, Int32) = (0, 0) }
public final class Cls { public var v: Int32 = 0; public var inner = Inner() }
public struct HasComputed { public var s: Int32 = 0; public var c: Int32 { s } }

// The offset field is the word right after the object header, and holds
// `-offset - 1` on 64-bit so it can never be mistaken for a real pointer. The
// globals are emitted in the declaration order of the `kp*` functions below.
//
// `\Outer.mid.inner.b`: `Outer.mid` is at 8 (Mid is Int64-aligned), `Mid.inner`
// at 8, `Inner.b` at 4 — a flat offset of 20.
// CHECK-IR: @keypath = private constant
// CHECK-IR-SAME: inttoptr (i64 -21 to ptr)
// `\Inner.b`: offset 4.
// CHECK-IR: @keypath.1 = private constant
// CHECK-IR-SAME: inttoptr (i64 -5 to ptr)
// `\HasTuple.t.1`: `t` at 4, element 1 at +4 — offset 8.
// CHECK-IR: @keypath.2 = private constant
// CHECK-IR-SAME: inttoptr (i64 -9 to ptr)
// `\Int32.self`: identity, a valid offset of 0 (encoded -1, not null).
// CHECK-IR: @keypath.3 = private constant
// CHECK-IR-SAME: inttoptr (i64 -1 to ptr)
//
// These two cannot use a flat offset — a class component dereferences and a
// computed one calls — so they store null and fall back to the buffer walk.
// CHECK-IR: @keypath.4 = private constant
// CHECK-IR-SAME: ptr null
// CHECK-IR: @keypath.5 = private constant
// CHECK-IR-SAME: ptr null

// The key paths are handed out from `@inline(never)` functions and applied
// behind another one, so the optimizer can't fold the application into a direct
// field access — which is what would otherwise bypass the offset entirely.
@inline(never) public func kpChain() -> WritableKeyPath<Outer, Int32> { \Outer.mid.inner.b }
@inline(never) public func kpSingle() -> WritableKeyPath<Inner, Int32> { \Inner.b }
@inline(never) public func kpTuple() -> WritableKeyPath<HasTuple, Int32> { \HasTuple.t.1 }
@inline(never) public func kpIdentity() -> WritableKeyPath<Int32, Int32> { \Int32.self }
@inline(never) public func kpThroughClass() -> ReferenceWritableKeyPath<Cls, Int32> { \Cls.inner.b }
@inline(never) public func kpComputed() -> KeyPath<HasComputed, Int32> { \HasComputed.c }

@inline(never) public func read<R, V>(_ r: R, _ kp: KeyPath<R, V>) -> V { r[keyPath: kp] }
@inline(never) public func write<R, V>(_ r: inout R, _ kp: WritableKeyPath<R, V>, _ v: V) {
  r[keyPath: kp] = v
}

var o = Outer()
o.mid.inner.b = 42
print(read(o, kpChain()) == 42 ? "OK!" : "FAIL") // CHECK-OUT: OK!
write(&o, kpChain(), 43)
print(o.mid.inner.b == 43 ? "OK!" : "FAIL") // CHECK-OUT: OK!

var i = Inner(a: 1, b: 2)
print(read(i, kpSingle()) == 2 ? "OK!" : "FAIL") // CHECK-OUT: OK!
write(&i, kpSingle(), 9)
print(i.b == 9 ? "OK!" : "FAIL") // CHECK-OUT: OK!

var ht = HasTuple()
ht.t.1 = 7
print(read(ht, kpTuple()) == 7 ? "OK!" : "FAIL") // CHECK-OUT: OK!
write(&ht, kpTuple(), 8)
print(ht.t.1 == 8 ? "OK!" : "FAIL") // CHECK-OUT: OK!

// Identity: a valid offset of 0, which must not be confused with "no offset".
var v: Int32 = 3
print(read(v, kpIdentity()) == 3 ? "OK!" : "FAIL") // CHECK-OUT: OK!
write(&v, kpIdentity(), 4)
print(v == 4 ? "OK!" : "FAIL") // CHECK-OUT: OK!

// Chains that must NOT take the offset path still work via the buffer walk.
let c = Cls()
c.inner.b = 5
print(read(c, kpThroughClass()) == 5 ? "OK!" : "FAIL") // CHECK-OUT: OK!
c[keyPath: kpThroughClass()] = 6
print(c.inner.b == 6 ? "OK!" : "FAIL") // CHECK-OUT: OK!

print(read(HasComputed(s: 11), kpComputed()) == 11 ? "OK!" : "FAIL") // CHECK-OUT: OK!
