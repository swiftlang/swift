// RUN: %target-run-simple-swift(   -enable-experimental-feature Embedded -wmo -Xfrontend -disable-access-control -runtime-compatibility-version none %target-embedded-posix-shim -enable-experimental-feature EmbeddedKeyPaths -enable-experimental-feature KeyPathWithMethodMembers) | %FileCheck %s
// RUN: %target-run-simple-swift(-O -enable-experimental-feature Embedded -wmo -Xfrontend -disable-access-control -runtime-compatibility-version none %target-embedded-posix-shim -enable-experimental-feature EmbeddedKeyPaths -enable-experimental-feature KeyPathWithMethodMembers) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// REQUIRES: swift_feature_Embedded
// REQUIRES: swift_feature_EmbeddedKeyPaths
// REQUIRES: swift_feature_KeyPathWithMethodMembers

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
