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
