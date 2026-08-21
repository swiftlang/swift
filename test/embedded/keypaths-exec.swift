// RUN: %target-run-simple-swift(   -enable-experimental-feature Embedded -wmo -Xfrontend -disable-access-control -runtime-compatibility-version none %target-embedded-posix-shim -enable-experimental-feature KeyPathWithMethodMembers) | %FileCheck %s
// RUN: %target-run-simple-swift(-O -enable-experimental-feature Embedded -wmo -Xfrontend -disable-access-control -runtime-compatibility-version none %target-embedded-posix-shim -enable-experimental-feature KeyPathWithMethodMembers) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// REQUIRES: swift_feature_Embedded
// REQUIRES: swift_feature_KeyPathWithMethodMembers
// XFAIL: swift_test_mode_optimize_none_with_opaque_values

// -----------------------------------------------------------------------------
// Generic wrappers
// -----------------------------------------------------------------------------

@inline(never)
public func getValueIn<Root, Value>(_ root: Root, keyPath: KeyPath<Root, Value>) -> Value {
  return root[keyPath: keyPath]
}

@inline(never)
public func setValueIn<Root, Value>(_ root: inout Root, _ value: Value,
                                    keyPath: WritableKeyPath<Root, Value>) {
  root[keyPath: keyPath] = value
}

@inline(never)
public func setValueIn<Root: AnyObject, Value>(_ root: Root, _ value: Value,
                                               keyPath: ReferenceWritableKeyPath<Root, Value>) {
  root[keyPath: keyPath] = value
}

// -----------------------------------------------------------------------------
// Stored properties: read and write through a mutating generic wrapper.
// -----------------------------------------------------------------------------

struct MyStruct {
  var x: Int8
  var y: Int16
  var z: Int32
}

var ms = MyStruct(x: 17, y: 25, z: 42)
print(getValueIn(ms, keyPath: \.x) == 17 ? "OK!" : "FAIL") // CHECK: OK!
print(getValueIn(ms, keyPath: \.y) == 25 ? "OK!" : "FAIL") // CHECK: OK!
print(getValueIn(ms, keyPath: \.z) == 42 ? "OK!" : "FAIL") // CHECK: OK!

setValueIn(&ms, Int8(-8),    keyPath: \.x)
setValueIn(&ms, Int16(1000), keyPath: \.y)
setValueIn(&ms, Int32(9999), keyPath: \.z)
print(ms.x == -8 && ms.y == 1000 && ms.z == 9999 ? "OK!" : "FAIL") // CHECK: OK!

// A `let` field: readable via KeyPath, cannot be written.
struct WithLet {
  let a: Int32 = 111
  var b: Int32 = 222
}
let wl = WithLet()
print(getValueIn(wl, keyPath: \.a) == 111 ? "OK!" : "FAIL") // CHECK: OK!
print(getValueIn(wl, keyPath: \.b) == 222 ? "OK!" : "FAIL") // CHECK: OK!

// -----------------------------------------------------------------------------
// Class ivars: reference-writable key path through generic wrapper.
// The generic setter takes the class instance (not `inout`) and mutates
// through the strong reference, so the same object is observed by other
// aliases.
// -----------------------------------------------------------------------------

final class Cell {
  var value: Int32 = 0
  var count: Int32 = 100
}

let cell = Cell()
setValueIn(cell, Int32(88),  keyPath: \Cell.value)
setValueIn(cell, Int32(200), keyPath: \Cell.count)
print(cell.value == 88 && cell.count == 200 ? "OK!" : "FAIL") // CHECK: OK!

let aliased = cell
print(getValueIn(aliased, keyPath: \Cell.value) == 88 ? "OK!" : "FAIL") // CHECK: OK!

// -----------------------------------------------------------------------------
// Identity key path: read + write on a bare value type.
// -----------------------------------------------------------------------------

var identity: Int32 = 5
print(getValueIn(identity, keyPath: \.self) == 5 ? "OK!" : "FAIL") // CHECK: OK!
setValueIn(&identity, Int32(9), keyPath: \.self)
print(identity == 9 ? "OK!" : "FAIL") // CHECK: OK!

// -----------------------------------------------------------------------------
// Tuple key paths: labeled + positional, read + write.
// -----------------------------------------------------------------------------

var pair: (Int32, Int64) = (11, 22)
print(getValueIn(pair, keyPath: \.0) == 11 ? "OK!" : "FAIL") // CHECK: OK!
print(getValueIn(pair, keyPath: \.1) == 22 ? "OK!" : "FAIL") // CHECK: OK!
setValueIn(&pair, Int32(-1), keyPath: \.0)
setValueIn(&pair, Int64(-2), keyPath: \.1)
print(pair.0 == -1 && pair.1 == -2 ? "OK!" : "FAIL") // CHECK: OK!

var labeled: (a: Int32, b: Int32) = (a: 3, b: 4)
setValueIn(&labeled, Int32(30), keyPath: \.a)
setValueIn(&labeled, Int32(40), keyPath: \.b)
print(labeled.a == 30 && labeled.b == 40 ? "OK!" : "FAIL") // CHECK: OK!

// -----------------------------------------------------------------------------
// Computed properties: get-only, mutating set, nonmutating set on struct.
// -----------------------------------------------------------------------------

// Get-only computed → `KeyPath<Root, Value>`.
struct GetOnly {
  var underlying: Int32 = 5
  var doubled: Int32 { underlying &* 2 }
}
var go = GetOnly(underlying: 21)
print(getValueIn(go, keyPath: \.doubled) == 42 ? "OK!" : "FAIL") // CHECK: OK!

// Settable + mutating computed → `WritableKeyPath<Root, Value>`.
struct MutatingSet {
  var storage: Int32 = 0
  var scaled: Int32 {
    get { storage &* 3 }
    set { storage = newValue / 3 }
  }
}
var mset = MutatingSet(storage: 0)
setValueIn(&mset, Int32(30), keyPath: \.scaled)
print(mset.storage == 10 ? "OK!" : "FAIL") // CHECK: OK!
print(getValueIn(mset, keyPath: \.scaled) == 30 ? "OK!" : "FAIL") // CHECK: OK!

// Settable + nonmutating on a struct → `ReferenceWritableKeyPath<Root, Value>`.
// The setter mutates external state; the struct value itself isn't touched.
// The `Root: AnyObject` generic wrapper doesn't apply here (struct root), so
// apply the key path directly to cover this shape.
var externalSideChannel: Int32 = 0
struct NonmutatingOnStruct {
  var canary: Int32 = 7
  var externallyStored: Int32 {
    get { externalSideChannel }
    nonmutating set { externalSideChannel = newValue }
  }
}
let nms = NonmutatingOnStruct()
let nmsKP: ReferenceWritableKeyPath<NonmutatingOnStruct, Int32> = \.externallyStored
nms[keyPath: nmsKP] = 1234
print(externalSideChannel == 1234 ? "OK!" : "FAIL") // CHECK: OK!
print(nms[keyPath: nmsKP] == 1234 ? "OK!" : "FAIL") // CHECK: OK!

// -----------------------------------------------------------------------------
// Writeback semantics: a read through a `WritableKeyPath` must NOT invoke
// the setter.  This exercises the `_read` vs `_modify` split in the
// embedded stdlib's key path subscript accessors.
// -----------------------------------------------------------------------------

var mutatingSetCount: Int32 = 0
var nonmutatingSetCount: Int32 = 0
var externalStorage: Int32 = 246

struct WritebackCounter {
  var canary: Int32 = 0

  var mutating: Int32 {
    get { externalStorage }
    set {
      mutatingSetCount &+= 1
      externalStorage = newValue
    }
  }

  var nonmutating: Int32 {
    get { externalStorage }
    nonmutating set {
      nonmutatingSetCount &+= 1
      externalStorage = newValue
    }
  }
}

do {
  var wc = WritebackCounter()
  wc = WritebackCounter()  // suppress "never mutated" note

  let wkp: WritableKeyPath<WritebackCounter, Int32> = \.mutating
  let rkp: ReferenceWritableKeyPath<WritebackCounter, Int32> = \.nonmutating

  mutatingSetCount = 0
  nonmutatingSetCount = 0

  _ = getValueIn(wc, keyPath: wkp)
  _ = getValueIn(wc, keyPath: rkp)
  _ = wc[keyPath: wkp]
  _ = wc[keyPath: rkp]

  print(mutatingSetCount == 0 && nonmutatingSetCount == 0 ? "OK!" : "FAIL") // CHECK: OK!

  // Now write through each and check the setter fires exactly once.
  setValueIn(&wc, Int32(300), keyPath: wkp)
  print(mutatingSetCount == 1 && externalStorage == 300 ? "OK!" : "FAIL") // CHECK: OK!

  // `rkp` on a struct root: the `setValueIn(_:AnyObject, ...)` overload can't
  // apply, so use the subscript directly.
  wc[keyPath: rkp] = 400
  print(nonmutatingSetCount == 1 && externalStorage == 400 ? "OK!" : "FAIL") // CHECK: OK!
}

// -----------------------------------------------------------------------------
// Generic-root specialization: a keypath created inside a generic function,
// then specialized at each call site.  Exercises the "no archetypes in the
// substitution map" static-instance guard.
// -----------------------------------------------------------------------------

struct Box<T> {
  var value: T
}

@inline(never)
func kpValue<T>(_: T.Type) -> WritableKeyPath<Box<T>, T> {
  return \Box<T>.value
}

do {
  var b = Box<Int>(value: 7)
  setValueIn(&b, 99, keyPath: kpValue(Int.self))
  print(b.value == 99 ? "OK!" : "FAIL") // CHECK: OK!

  var b2 = Box<Int32>(value: -1)
  setValueIn(&b2, Int32(1000), keyPath: kpValue(Int32.self))
  print(b2.value == 1000 ? "OK!" : "FAIL") // CHECK: OK!
}

// -----------------------------------------------------------------------------
// Method key path (`\Root.method`) — the projected value is an unapplied
// closure `(Root) -> Value`.  Read via the generic wrapper, then invoke it.
// -----------------------------------------------------------------------------

struct HasMethod {
  var base: Int32
  func doubled() -> Int32 { base &* 2 }
  func plus(_ x: Int32) -> Int32 { base &+ x }
}

do {
  let hm = HasMethod(base: 21)
  let fn0 = getValueIn(hm, keyPath: \HasMethod.doubled)
  print(fn0() == 42 ? "OK!" : "FAIL") // CHECK: OK!

  let fn1 = getValueIn(hm, keyPath: \HasMethod.plus)
  print(fn1(100) == 121 ? "OK!" : "FAIL") // CHECK: OK!
}

// -----------------------------------------------------------------------------
// Same-shape key paths used many times: the emitted immortal global is
// shared across calls, and each use projects independently.
// -----------------------------------------------------------------------------

do {
  let kp: WritableKeyPath<MyStruct, Int32> = \.z
  var a = MyStruct(x: 0, y: 0, z: 1)
  var b = MyStruct(x: 0, y: 0, z: 2)
  setValueIn(&a, Int32(10), keyPath: kp)
  setValueIn(&b, Int32(20), keyPath: kp)
  print(a.z == 10 && b.z == 20 ? "OK!" : "FAIL") // CHECK: OK!
  print(getValueIn(a, keyPath: kp) + getValueIn(b, keyPath: kp) == 30 ? "OK!" : "FAIL") // CHECK: OK!
}

// -----------------------------------------------------------------------------
// `UnsafePointer(to:)` / `UnsafeMutablePointer(to:)`: turn a `KeyPath` or
// `WritableKeyPath` into a raw byte offset via `_storedInlineOffset` and
// GEP through the pointer.  Returns `nil` for computed properties.
// -----------------------------------------------------------------------------

struct Sample {
  var head: Int8      // offset 0
  var mid:  Int32     // offset 4 (Int32 alignment)
  var tail: Int64     // offset 8
  var computed: Int32 { mid &* 10 }
}

do {
  var s = Sample(head: 1, mid: 2, tail: 3)

  // Read-only pointer through immutable base.
  withUnsafePointer(to: s) { p in
    let pHead = p.pointer(to: \Sample.head)!
    let pMid  = p.pointer(to: \Sample.mid)!
    let pTail = p.pointer(to: \Sample.tail)!
    print(pHead.pointee == 1 && pMid.pointee == 2 && pTail.pointee == 3
          ? "OK!" : "FAIL") // CHECK: OK!

    // Offsets are consistent with `MemoryLayout.offset(of:)`.
    let baseAddr = UInt(bitPattern: p)
    print(UInt(bitPattern: pMid)  &- baseAddr == 4 ? "OK!" : "FAIL") // CHECK: OK!
    print(UInt(bitPattern: pTail) &- baseAddr == 8 ? "OK!" : "FAIL") // CHECK: OK!
  }

  // Mutating pointer: write a new value through the pointer, observe it
  // in the original struct.
  withUnsafeMutablePointer(to: &s) { p in
    let pMid = p.pointer(to: \Sample.mid)!
    pMid.pointee = 555
  }
  print(s.mid == 555 ? "OK!" : "FAIL") // CHECK: OK!

  // Computed properties don't have a stored inline offset — the API
  // returns nil rather than trapping.
  withUnsafePointer(to: s) { p in
    let pComputed = p.pointer(to: \Sample.computed)
    print(pComputed == nil ? "OK!" : "FAIL") // CHECK: OK!
  }
}

// A `let` field still has a stored offset: readable via
// `UnsafePointer.pointer(to:)` because the API takes `KeyPath`, not
// `WritableKeyPath`.
do {
  let wl = WithLet()
  withUnsafePointer(to: wl) { p in
    let pa = p.pointer(to: \WithLet.a)!
    let pb = p.pointer(to: \WithLet.b)!
    print(pa.pointee == 111 && pb.pointee == 222 ? "OK!" : "FAIL") // CHECK: OK!
  }
}

// Tuples: the embedded static-instance emitter treats tuple elements the
// same as struct fields, so `pointer(to:)` should work here too.
do {
  var t: (Int32, Int64) = (100, 200)
  withUnsafeMutablePointer(to: &t) { p in
    let p0 = p.pointer(to: \.0)!
    let p1 = p.pointer(to: \.1)!
    print(p0.pointee == 100 && p1.pointee == 200 ? "OK!" : "FAIL") // CHECK: OK!
    p0.pointee = -1
    p1.pointee = -2
  }
  print(t.0 == -1 && t.1 == -2 ? "OK!" : "FAIL") // CHECK: OK!
}

// Generic-struct specialization: the same `pointer(to:)` on a `Box<Int>`
// specialization goes through the substitution-map-aware static-instance
// path.
do {
  var box = Box<Int>(value: 42)
  withUnsafeMutablePointer(to: &box) { p in
    let pv = p.pointer(to: kpValue(Int.self))!
    pv.pointee = 777
  }
  print(box.value == 777 ? "OK!" : "FAIL") // CHECK: OK!
}

// -----------------------------------------------------------------------------
// Multi-component chains of fixed-offset stored/tuple components.  These
// exercise the walker that iterates through more than one component in a
// single key path — supported in Embedded Swift when every step is a
// struct/tuple field or a class ivar (i.e. resolves to a fixed offset).
// -----------------------------------------------------------------------------

// Nested struct chain (2 components, pure struct/tuple, all `var`) →
// WritableKeyPath.
struct Inner {
  var a: Int32
  var b: Int32
}
struct Outer {
  var name: Int8
  var inner: Inner
}

do {
  var o = Outer(name: 1, inner: Inner(a: 10, b: 20))
  let kpA: WritableKeyPath<Outer, Int32> = \.inner.a
  let kpB: WritableKeyPath<Outer, Int32> = \.inner.b

  // Read.
  print(o[keyPath: kpA] == 10 && o[keyPath: kpB] == 20 ? "OK!" : "FAIL") // CHECK: OK!
  print(getValueIn(o, keyPath: kpA) == 10 ? "OK!" : "FAIL") // CHECK: OK!

  // Write.
  o[keyPath: kpA] = 111
  setValueIn(&o, Int32(222), keyPath: kpB)
  print(o.inner.a == 111 && o.inner.b == 222 ? "OK!" : "FAIL") // CHECK: OK!
}

// Longer chain: 3 stored struct fields.
struct L1 { var lower: L2 }
struct L2 { var lowest: L3 }
struct L3 { var value: Int32 }

do {
  var l = L1(lower: L2(lowest: L3(value: 100)))
  let kp: WritableKeyPath<L1, Int32> = \.lower.lowest.value
  print(l[keyPath: kp] == 100 ? "OK!" : "FAIL") // CHECK: OK!
  l[keyPath: kp] = 999
  print(l.lower.lowest.value == 999 ? "OK!" : "FAIL") // CHECK: OK!
}

// Chain with a `let` intermediate → read-only KeyPath (all `let` on any
// component demotes to KeyPath).
struct LetChainOuter {
  let inner: LetChainInner
}
struct LetChainInner {
  var v: Int32
}

do {
  let o = LetChainOuter(inner: LetChainInner(v: 42))
  let kp: KeyPath<LetChainOuter, Int32> = \.inner.v
  print(o[keyPath: kp] == 42 ? "OK!" : "FAIL") // CHECK: OK!
  print(getValueIn(o, keyPath: kp) == 42 ? "OK!" : "FAIL") // CHECK: OK!
}

// Chain crossing a class boundary → ReferenceWritableKeyPath.  Writing
// through the KP mutates the class instance, which is observed via an
// independent reference.
final class ClassBox {
  var value: Int32 = 0
}
struct HasClassField {
  var boxed: ClassBox
}

do {
  let box = ClassBox()
  var hcf = HasClassField(boxed: box)
  let kp: ReferenceWritableKeyPath<HasClassField, Int32> = \.boxed.value

  print(hcf[keyPath: kp] == 0 ? "OK!" : "FAIL") // CHECK: OK!
  hcf[keyPath: kp] = 55
  print(box.value == 55 ? "OK!" : "FAIL") // CHECK: OK!
  // The KP mutates the class, so an alias observes the write.
  print(hcf.boxed.value == 55 ? "OK!" : "FAIL") // CHECK: OK!
}

// Chain that starts at a class root and dives into a struct ivar:
// class → struct field → struct field.  Because the root is a class,
// this is ReferenceWritableKeyPath.
final class RootClass {
  var stuff = Inner(a: 1, b: 2)
}

do {
  let rc = RootClass()
  let kpA: ReferenceWritableKeyPath<RootClass, Int32> = \.stuff.a
  let kpB: ReferenceWritableKeyPath<RootClass, Int32> = \.stuff.b

  print(rc[keyPath: kpA] == 1 && rc[keyPath: kpB] == 2 ? "OK!" : "FAIL") // CHECK: OK!

  rc[keyPath: kpA] = 100
  rc[keyPath: kpB] = 200
  print(rc.stuff.a == 100 && rc.stuff.b == 200 ? "OK!" : "FAIL") // CHECK: OK!
}

// Chain into a tuple element (mixed struct + tuple), read + write.
struct HasTuple {
  var pair: (Int32, Int64)
}

do {
  var ht = HasTuple(pair: (7, 8))
  let kp0: WritableKeyPath<HasTuple, Int32> = \.pair.0
  let kp1: WritableKeyPath<HasTuple, Int64> = \.pair.1

  print(ht[keyPath: kp0] == 7 && ht[keyPath: kp1] == 8 ? "OK!" : "FAIL") // CHECK: OK!
  ht[keyPath: kp0] = 70
  ht[keyPath: kp1] = 80
  print(ht.pair.0 == 70 && ht.pair.1 == 80 ? "OK!" : "FAIL") // CHECK: OK!
}

// -----------------------------------------------------------------------------
// AnyKeyPath / PartialKeyPath type erasure.  Even though embedded doesn't
// have full existential type introspection, key path erasure works because
// each concrete KeyPath subclass overrides `_projectReadOnlyAsAny` — those
// overrides are found via the vtable of the immortal static-instance
// object, so `AnyKeyPath.subscript` / `PartialKeyPath.subscript` /
// `swift_getAtAnyKeyPath` / `swift_getAtPartialKeyPath` route through them
// without needing `_openExistential` on `Any.Type`.
//
// Covers: single-component, multi-component chain, class-boundary chain,
// wrong-typed root (nil result), PartialKeyPath, identity, Hashable use.
// -----------------------------------------------------------------------------

struct Erasable {
  var x: Int32
  var y: Int32
}
struct OtherRoot {
  var v: Int32
}
struct NestedErasable {
  var e: Erasable
}
final class ClsErasable {
  var count: Int32 = 100
}
struct HoldsClsErasable {
  var c: ClsErasable
}

do {
  // Single-component KP erased to `AnyKeyPath`.
  let kpX: KeyPath<Erasable, Int32> = \Erasable.x
  let anyX: AnyKeyPath = kpX
  let e = Erasable(x: 42, y: 99)
  print((e[keyPath: anyX] as? Int32) == 42 ? "OK!" : "FAIL") // CHECK: OK!

  // Multi-component chain erased.
  let kpNested: KeyPath<NestedErasable, Int32> = \NestedErasable.e.x
  let anyNested: AnyKeyPath = kpNested
  print((NestedErasable(e: e)[keyPath: anyNested] as? Int32) == 42
        ? "OK!" : "FAIL") // CHECK: OK!

  // Class-boundary chain erased.
  let hc = HoldsClsErasable(c: ClsErasable())
  hc.c.count = 77
  let kpCls: ReferenceWritableKeyPath<HoldsClsErasable, Int32> =
      \HoldsClsErasable.c.count
  let anyCls: AnyKeyPath = kpCls
  print((hc[keyPath: anyCls] as? Int32) == 77 ? "OK!" : "FAIL") // CHECK: OK!

  // Heterogeneous `[AnyKeyPath]` array — wrong-typed root yields nil.
  let kpV: KeyPath<OtherRoot, Int32> = \OtherRoot.v
  let paths: [AnyKeyPath] = [kpX, kpV]
  print((e[keyPath: paths[0]] as? Int32) == 42 ? "OK!" : "FAIL") // CHECK: OK!
  print(e[keyPath: paths[1]] == nil ? "OK!" : "FAIL") // CHECK: OK!

  // PartialKeyPath — root fixed, value erased to Any (not Any?).
  let partial: PartialKeyPath<Erasable> = kpX
  print((e[keyPath: partial] as? Int32) == 42 ? "OK!" : "FAIL") // CHECK: OK!

  // Identity via `AnyKeyPath`.
  var i: Int32 = 7
  let anyIdent: AnyKeyPath = \Int32.self
  print((i[keyPath: anyIdent] as? Int32) == 7 ? "OK!" : "FAIL") // CHECK: OK!
  _ = i  // suppress "never mutated" note

  // `AnyKeyPath == AnyKeyPath` — the static-instance emitter shares one
  // immortal global per pattern, so identical-shape paths compare equal
  // by object identity.  Different-typed paths compare unequal.
  print(anyX == kpX ? "OK!" : "FAIL") // CHECK: OK!
  print(anyX != paths[1] ? "OK!" : "FAIL") // CHECK: OK!

  // `AnyKeyPath`'s `Hashable` conformance is covered separately, in
  // `keypaths-hashable.swift`: using one as a `Dictionary` key pulls in
  // `_HashTable` and the hash seed, which need `ceil` and `arc4random_buf`.
  // This test's link recipe can't satisfy those on Linux, and restricting the
  // whole file would cost the rest of its coverage there.
}

// -----------------------------------------------------------------------------
// Multi-component chains that include a get-only computed (or method)
// intermediate.  The embedded runtime walker allocates a scratch buffer
// sized to the intermediate type (queried from the type-metadata pointer
// embedded in the KP buffer), invokes the getter thunk with the sret ABI
// via a C shim, walks the tail, and destroys the scratch.
// -----------------------------------------------------------------------------

struct MidComputedInner {
  var d: Int32
  var e: Int32
}
struct MidComputedOuter {
  var inner: MidComputedInner
  var innerGet: MidComputedInner { MidComputedInner(d: 100, e: 200) }
  var innerLive: MidComputedInner { inner }  // returns the actual storage
}

// stored -> computed (fresh copy) -> stored
do {
  let o = MidComputedOuter(inner: MidComputedInner(d: 10, e: 20))
  let kpD: KeyPath<MidComputedOuter, Int32> = \.innerGet.d
  let kpE: KeyPath<MidComputedOuter, Int32> = \.innerGet.e
  print(o[keyPath: kpD] == 100 ? "OK!" : "FAIL") // CHECK: OK!
  print(o[keyPath: kpE] == 200 ? "OK!" : "FAIL") // CHECK: OK!

  // Get-only computed that returns a copy of the underlying `inner`
  // storage — the walker reads through that copy.
  let kpLive: KeyPath<MidComputedOuter, Int32> = \.innerLive.d
  print(o[keyPath: kpLive] == 10 ? "OK!" : "FAIL") // CHECK: OK!
}

// stored -> computed (final)
do {
  struct C {
    var v: Int32 = 21
    var doubled: Int32 { v &* 2 }
  }
  struct H { var c: C = C() }
  let h = H()
  let kp: KeyPath<H, Int32> = \H.c.doubled
  print(h[keyPath: kp] == 42 ? "OK!" : "FAIL") // CHECK: OK!
}

// class root -> computed (final)
final class ClsRoot {
  var v: Int32 = 5
  var tripled: Int32 { v &* 3 }
}
do {
  let cr = ClsRoot()
  cr.v = 7
  let kp: KeyPath<ClsRoot, Int32> = \ClsRoot.tripled
  print(cr[keyPath: kp] == 21 ? "OK!" : "FAIL") // CHECK: OK!
}

// stored -> class -> computed (chain crossing a class boundary into a
// computed final step).
struct HoldsCls {
  var c: ClsRoot
}
do {
  let hc = HoldsCls(c: ClsRoot())
  hc.c.v = 9
  let kp: KeyPath<HoldsCls, Int32> = \HoldsCls.c.tripled
  print(hc[keyPath: kp] == 27 ? "OK!" : "FAIL") // CHECK: OK!
}

// Longer chain: stored -> stored -> computed -> stored (3 steps + computed).
struct Deep1 { var d2: Deep2 = Deep2() }
struct Deep2 { var mid: Deep3 { Deep3() }; var s: Int32 = 0 }
struct Deep3 { var v: Int32 = 77 }
do {
  let d = Deep1()
  let kp: KeyPath<Deep1, Int32> = \Deep1.d2.mid.v
  print(d[keyPath: kp] == 77 ? "OK!" : "FAIL") // CHECK: OK!
}

// Method component in a chain.
struct HasMethodInChain {
  var underlying: Int32 = 33
  func get() -> Int32 { underlying }
}
struct WrapsMethod {
  var m: HasMethodInChain = HasMethodInChain()
}
do {
  let w = WrapsMethod()
  // \.m.get produces a KeyPath<WrapsMethod, () -> Int32> — the last
  // component is a method.  Not directly what we're testing here.
  // Instead: stored -> computed (via a get-only property that returns a
  // fresh value).  Covered above.
  _ = w
}

// Non-trivial intermediate: ensure the walker destroys a class-holding
// scratch value.  A get-only computed returns a fresh Holds<Counter>,
// the walker reads through it to fetch the Counter's value, then must
// destroy the scratch (releasing Counter to zero refcount).
final class ChainCounter {
  static var live: Int32 = 0
  var value: Int32 = 42
  init() { ChainCounter.live &+= 1 }
  deinit { ChainCounter.live &-= 1 }
}
struct HoldsChainCounter {
  var c: ChainCounter
}
struct HasFresh {
  // Get-only computed returns a fresh Holds every time.  The walker
  // must destroy this intermediate after extracting `c.value`.
  var freshHolds: HoldsChainCounter { HoldsChainCounter(c: ChainCounter()) }
}
do {
  ChainCounter.live = 0
  let hf = HasFresh()
  let kp: KeyPath<HasFresh, Int32> = \HasFresh.freshHolds.c.value
  print(hf[keyPath: kp] == 42 ? "OK!" : "FAIL") // CHECK: OK!
  // The intermediate `HoldsChainCounter` was allocated inside the walker;
  // once the walker destroys it, ChainCounter's refcount drops.  The
  // walker's returned Int32 didn't retain anything.
  print(ChainCounter.live == 0 ? "OK!" : "FAIL") // CHECK: OK!
}

// Read-only through a settable-computed intermediate.  The pattern still
// contains the setter, but the walker only calls the getter.  Verifies
// the layout with a getter+setter slot is walked correctly.
struct WithSettableComputed {
  var storage: Int32 = 5
  var wrapped: SomeInner {
    get { SomeInner(v: storage) }
    set { storage = newValue.v }
  }
}
struct SomeInner {
  var v: Int32
}
do {
  let w = WithSettableComputed(storage: 88)
  let kp: KeyPath<WithSettableComputed, Int32> = \.wrapped.v
  print(w[keyPath: kp] == 88 ? "OK!" : "FAIL") // CHECK: OK!
}

// Erasing a computed-mid-chain KP to AnyKeyPath and applying via
// subscript-on-AnyKeyPath.  Exercises `_projectReadOnlyAsAny` through
// the vtable dispatch (KeyPath<Root,Value>._projectReadOnly(from:))
// which now handles computed intermediates.
do {
  let o = MidComputedOuter(inner: MidComputedInner(d: 1, e: 2))
  let kp: KeyPath<MidComputedOuter, Int32> = \.innerGet.d
  let any: AnyKeyPath = kp
  print((o[keyPath: any] as? Int32) == 100 ? "OK!" : "FAIL") // CHECK: OK!
}

// -----------------------------------------------------------------------------
// Writable computed intermediates: `WritableKeyPath` / `ReferenceWritableKeyPath`
// chains with settable computed components in the middle.  The embedded
// runtime installs an `_Embedded{Mutating,Nonmutating}WritebackBuffer` on
// the `keepAlive` chain for each such intermediate: the getter fills a
// heap scratch, the caller mutates through the leaf pointer, and each
// writeback's `deinit` fires the setter in LIFO order to propagate the
// mutation up.
// -----------------------------------------------------------------------------

struct WKInner {
  var v: Int32 = 0
}

struct WKOuterMut {
  var _b: WKInner = WKInner()
  var b: WKInner {
    get { _b }
    set { _b = newValue }
  }
}

// WK: stored -> mutating-computed -> stored
do {
  var o = WKOuterMut()
  o._b.v = 5
  let kp: WritableKeyPath<WKOuterMut, Int32> = \.b.v
  print(o[keyPath: kp] == 5 ? "OK!" : "FAIL") // CHECK: OK!
  o[keyPath: kp] = 42
  print(o._b.v == 42 ? "OK!" : "FAIL") // CHECK: OK!
}

// WK: nested mutating-computed intermediates (2 layers).
struct WKMid {
  var _o: WKOuterMut = WKOuterMut()
  var o: WKOuterMut {
    get { _o }
    set { _o = newValue }
  }
}
struct WKTop {
  var _m: WKMid = WKMid()
  var m: WKMid {
    get { _m }
    set { _m = newValue }
  }
}
do {
  var t = WKTop()
  t._m._o._b.v = 10
  let kp: WritableKeyPath<WKTop, Int32> = \.m.o.b.v
  print(t[keyPath: kp] == 10 ? "OK!" : "FAIL") // CHECK: OK!
  t[keyPath: kp] = 99
  print(t._m._o._b.v == 99 ? "OK!" : "FAIL") // CHECK: OK!
}

// WK: mutating computed at the leaf (writeback still fires).
do {
  var o = WKOuterMut()
  o._b.v = 3
  // The leaf `b` is settable — the KP is
  // `\WKOuterMut.b` (Value = WKInner).
  let kp: WritableKeyPath<WKOuterMut, WKInner> = \.b
  print(o[keyPath: kp].v == 3 ? "OK!" : "FAIL") // CHECK: OK!
  o[keyPath: kp] = WKInner(v: 77)
  print(o._b.v == 77 ? "OK!" : "FAIL") // CHECK: OK!
}

// RWK: single nonmutating-computed (class), read + write.
final class RWKCls {
  var _v: Int32 = 0
  var v: Int32 {
    get { _v }
    set { _v = newValue }
  }
}
do {
  let c = RWKCls()
  c._v = 5
  let kp: ReferenceWritableKeyPath<RWKCls, Int32> = \.v
  print(c[keyPath: kp] == 5 ? "OK!" : "FAIL") // CHECK: OK!
  c[keyPath: kp] = 42
  print(c._v == 42 ? "OK!" : "FAIL") // CHECK: OK!
}

// RWK: stored -> class -> nonmutating-computed.
struct HoldsRWK {
  var c: RWKCls = RWKCls()
}
do {
  var h = HoldsRWK()
  h.c._v = 5
  let kp: ReferenceWritableKeyPath<HoldsRWK, Int32> = \.c.v
  print(h[keyPath: kp] == 5 ? "OK!" : "FAIL") // CHECK: OK!
  h[keyPath: kp] = 42
  print(h.c._v == 42 ? "OK!" : "FAIL") // CHECK: OK!
  // Aliased reference sees the mutation (RWK semantics).
  let alias = h.c
  print(alias._v == 42 ? "OK!" : "FAIL") // CHECK: OK!
}

// RWK: stored -> mutating-computed -> stored -> class -> nonmutating-computed.
// Exercises a chain with BOTH kinds of writeback stacked LIFO.
struct RWKInner {
  var c: RWKCls = RWKCls()
}
struct RWKWrapper {
  var storage: RWKInner = RWKInner()
  var view: RWKInner {
    get { storage }
    set { storage = newValue }
  }
}
struct RWKOuter {
  var w: RWKWrapper = RWKWrapper()
}
do {
  var o = RWKOuter()
  o.w.storage.c._v = 100
  let kp: ReferenceWritableKeyPath<RWKOuter, Int32> = \.w.view.c.v
  print(o[keyPath: kp] == 100 ? "OK!" : "FAIL") // CHECK: OK!
  o[keyPath: kp] = 200
  print(o.w.storage.c._v == 200 ? "OK!" : "FAIL") // CHECK: OK!
}

// Non-trivial value in mutating writeback: the intermediate carries a
// class reference, so the writeback's `destroy` must release the class
// after firing the setter.
final class WBCounter {
  static var live: Int32 = 0
  var value: Int32 = 0
  init(_ v: Int32) { value = v; WBCounter.live &+= 1 }
  deinit { WBCounter.live &-= 1 }
}
struct HoldsWBCounter {
  var c: WBCounter = WBCounter(0)
  var v: Int32 { get { c.value } set { c.value = newValue } }
}
struct WKHoldsHolds {
  var _hh: HoldsWBCounter = HoldsWBCounter()
  var hh: HoldsWBCounter {
    get { _hh }
    set { _hh = newValue }
  }
}
do {
  WBCounter.live = 0
  var wh = WKHoldsHolds()
  // `WKHoldsHolds()` allocates one WBCounter inside its default
  // `_hh`.  Refcount = 1.
  print(WBCounter.live == 1 ? "OK!" : "FAIL") // CHECK: OK!

  let kp: WritableKeyPath<WKHoldsHolds, Int32> = \.hh.v
  print(wh[keyPath: kp] == 0 ? "OK!" : "FAIL") // CHECK: OK!
  wh[keyPath: kp] = 99
  print(wh._hh.c.value == 99 ? "OK!" : "FAIL") // CHECK: OK!
  // After the write:
  //   * walker copied `wh._hh` into scratch (retain → live=2)
  //   * caller mutated scratch's `c.value` via nested computed setter
  //   * mutating writeback deinit fires: setter copies scratch into
  //     `wh._hh` (that store releases the old `_hh.c` → live=1),
  //     then walker destroys scratch (releases scratch's `c` → live=0)
  //   * ...wait, but the original counter was actually the same object
  //     as scratch's, aliased through the retain.  The final store
  //     copies scratch back into `_hh`, so `wh._hh.c` again refers to
  //     the same underlying object.  Net: still 1 live.
  print(WBCounter.live == 1 ? "OK!" : "FAIL") // CHECK: OK!
  _ = wh  // keep wh alive so its _hh.c stays around
}
