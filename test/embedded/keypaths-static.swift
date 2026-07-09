// Verify that in Embedded Swift, `keypath` insts for qualifying single-
// component patterns are emitted as immortal constant globals rather than
// through the `swift_getKeyPath` runtime call.
//
// Covers the currently-supported cases (phases 1-3):
//   * struct stored property, `var` and `let`
//   * generic-struct stored property under specialization
//   * identity key path (0 components)
//   * tuple element
//   * computed properties on structs (get-only and settable mutating)
//   * class ivar (emitted as computed settable_property in SIL)

// RUN: %target-swift-emit-ir %s -enable-experimental-feature Embedded -enable-experimental-feature EmbeddedKeyPaths -enable-experimental-feature KeyPathWithMethodMembers -wmo -o - | %FileCheck -check-prefix=CHECK-IR %s
// RUN: %target-run-simple-swift(-O -enable-experimental-feature Embedded -enable-experimental-feature EmbeddedKeyPaths -enable-experimental-feature KeyPathWithMethodMembers -wmo -runtime-compatibility-version none %target-embedded-posix-shim) | %FileCheck -check-prefix=CHECK-OUT %s

// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// REQUIRES: swift_feature_Embedded
// REQUIRES: swift_feature_EmbeddedKeyPaths
// REQUIRES: swift_feature_KeyPathWithMethodMembers

public struct S {
  var a: Int32
  var b: Int32
}

// Static WritableKeyPath for a `var` struct field.
@inline(never)
public func kpS() -> WritableKeyPath<S, Int32> {
  return \S.b
}

// Static read-only KeyPath for a `let` struct field.
public struct SLet {
  let a: Int32
  let b: Int32
}

@inline(never)
public func kpSLet() -> KeyPath<SLet, Int32> {
  return \SLet.b
}

// A key path built inside a generic function, then specialized.  The
// `keypath` inst emitted by SILGen still carries a substitution map
// (mapping the generic parameter to the concrete type); the static-instance
// emitter has to consult the concrete SIL type of the instruction rather
// than the pattern's generic types.
public struct Box<T> {
  var value: T
}

@inline(never)
public func kpBoxGeneric<T>(_: T.Type) -> WritableKeyPath<Box<T>, T> {
  return \Box<T>.value
}

@inline(never)
public func kpBoxInt() -> WritableKeyPath<Box<Int>, Int> {
  return kpBoxGeneric(Int.self)
}

// Identity key path (0 components): the runtime always picks
// `WritableKeyPath` for the concrete class regardless of the declared type.
@inline(never)
public func kpIdentity() -> WritableKeyPath<Int, Int> {
  return \Int.self
}

// Tuple-element key paths — single `TupleElement` component encoded with the
// same `structTag` discriminator that the runtime uses.
public typealias Pair = (Int32, Int64)

@inline(never)
public func kpTuple0() -> WritableKeyPath<Pair, Int32> {
  return \.0
}

@inline(never)
public func kpTuple1() -> WritableKeyPath<Pair, Int64> {
  return \.1
}

// Class ivar via computed getter/setter — always emits as `settable_property`
// in SIL, so it exercises the computed-component path (phase 3), not the
// stored-property path.
public class Cell {
  public var value: Int32 = 0
}

@inline(never)
public func kpCell() -> ReferenceWritableKeyPath<Cell, Int32> {
  return \Cell.value
}

// Struct with explicit computed property — mutating setter → WritableKeyPath.
public struct Wrap {
  var storage: Int32 = 0
  public var doubled: Int32 {
    get { storage &* 2 }
    set { storage = newValue / 2 }
  }
}

@inline(never)
public func kpWrapDoubled() -> WritableKeyPath<Wrap, Int32> {
  return \Wrap.doubled
}

// Struct with get-only computed property → read-only KeyPath.
public struct Ro {
  public var v: Int32 { 7 }
}

@inline(never)
public func kpRoV() -> KeyPath<Ro, Int32> {
  return \Ro.v
}

// Instance-method key path — encoded as a get-only computed component
// whose "getter" returns the bound closure.  Same shape as a get-only
// computed property in memory, so no setter slot.
public struct HasMethod {
  public func doThing() -> Int { 42 }
}

@inline(never)
public func kpMethod() -> KeyPath<HasMethod, () -> Int> {
  return \HasMethod.doThing
}

// CHECK-IR-NOT: swift_getKeyPath
//
// FileCheck the concrete shape of the emitted immortal globals.  Each key
// path is emitted as a private constant whose isa points at the picked
// concrete `KeyPath` subclass's metadata, whose refcount is -1 (immortal),
// and whose tail buffer encodes the component list.
//
// Buffer-header word bits (`_SwiftKeyPathBufferHeader_*`):
//   0x8000_0000 TrivialFlag
//   0x4000_0000 HasReferencePrefixFlag
//   0x2000_0000 IsSingleComponentFlag
//   0x00FF_FFFF SizeMask (bytes of components, including per-component skew)
//
// Component-header word bits (`_SwiftKeyPathComponentHeader_*`):
//   discriminator (bits 24-30): 1=StructTag, 2=ComputedTag, 3=ClassTag
//   0x0080_0000 StoredMutableFlag / ComputedMutatingFlag
//   0x0040_0000 ComputedSettableFlag
//   payload (low 24 bits): stored inline offset, or computed id-resolution bits
//
// Values are shown as unsigned decimal to match FileCheck-friendly output.

//
// `\S.b` — var struct field at offset 4 → WritableKeyPath.
// Buffer header 0xA0000004 = -1610612732 (trivial + singleComponent + size=4)
// Comp   header 0x01800004 =    25165828 (structTag + mutable + offset=4)
//
// CHECK-IR-DAG: @keypath{{[.0-9]*}} = private constant { %swift.refcounted, i32, [4 x i8], i32 } { %swift.refcounted { ptr {{.*}}"$es15WritableKeyPathCy4main1SVs5Int32VGMf"{{.*}}, i64 -1 }, i32 -1610612732, [4 x i8] zeroinitializer, i32 25165828 }

//
// `\SLet.b` — let struct field at offset 4 → KeyPath (read-only).
// Buffer header 0xA0000004 = -1610612732
// Comp   header 0x01000004 =    16777220 (structTag + offset=4, no mutable bit)
//
// CHECK-IR-DAG: @keypath{{[.0-9]*}} = private constant { %swift.refcounted, i32, [4 x i8], i32 } { %swift.refcounted { ptr {{.*}}"$es7KeyPathCy4main4SLetVs5Int32VGMf"{{.*}}, i64 -1 }, i32 -1610612732, [4 x i8] zeroinitializer, i32 16777220 }

//
// `\Box<Int>.value` — specialization of generic `\Box<T>.value`
// (T = Int → offset 0 in the specialized layout).
// Comp header 0x01800000 = 25165824 (structTag + mutable + offset=0)
//
// CHECK-IR-DAG: @keypath{{[.0-9]*}} = private constant { %swift.refcounted, i32, [4 x i8], i32 } { %swift.refcounted { ptr {{.*}}"$es15WritableKeyPathCy4main3BoxVySiGSiGMf"{{.*}}, i64 -1 }, i32 -1610612732, [4 x i8] zeroinitializer, i32 25165824 }

//
// `\Int.self` — identity: no component, so no IsSingleComponent flag and
// size=0 in the buffer header.
// Buffer header 0x80000000 = -2147483648 (trivial only)
//
// CHECK-IR-DAG: @keypath{{[.0-9]*}} = private constant { %swift.refcounted, i32, [4 x i8] } { %swift.refcounted { ptr {{.*}}"$es15WritableKeyPathCyS2iGMf"{{.*}}, i64 -1 }, i32 -2147483648, [4 x i8] zeroinitializer }

//
// `\Pair.0` — tuple element at offset 0 → WritableKeyPath, structTag +
// mutable + offset=0 → 0x01800000.
//
// CHECK-IR-DAG: @keypath{{[.0-9]*}} = private constant { %swift.refcounted, i32, [4 x i8], i32 } { %swift.refcounted { ptr {{.*}}"$es15WritableKeyPathCys5Int32V_s5Int64VtADGMf"{{.*}}, i64 -1 }, i32 -1610612732, [4 x i8] zeroinitializer, i32 25165824 }

//
// `\Pair.1` — tuple element at offset 8 (Int32 then Int64 aligned) →
// WritableKeyPath, structTag + mutable + offset=8 → 0x01800008 = 25165832.
//
// CHECK-IR-DAG: @keypath{{[.0-9]*}} = private constant { %swift.refcounted, i32, [4 x i8], i32 } { %swift.refcounted { ptr {{.*}}"$es15WritableKeyPathCys5Int32V_s5Int64VtAFGMf"{{.*}}, i64 -1 }, i32 -1610612732, [4 x i8] zeroinitializer, i32 25165832 }

//
// `\Cell.value` — class settable computed → ReferenceWritableKeyPath.
// Buffer size = 4 (comp header) + 4 (skew) + 8 (id) + 8 (getter) + 8 (setter) = 32.
// Buffer header 0xA0000020 = -1610612704 (trivial + singleComponent + size=32).
// Comp   header 0x02400000 = 37748736 (computedTag + settable, non-mutating).
// The id field is the raw getter pointer; the getter and setter fields are
// (address-discriminated) ptr-auth-signed on arm64e — the layout still
// contains three ptr slots regardless.
//
// CHECK-IR-DAG: @keypath{{[.0-9]*}} = private constant { %swift.refcounted, i32, [4 x i8], i32, [4 x i8], ptr, ptr, ptr } { %swift.refcounted { ptr {{.*}}"$es24ReferenceWritableKeyPathCy4main4CellCs5Int32VGMf"{{.*}}, i64 -1 }, i32 -1610612704, [4 x i8] zeroinitializer, i32 37748736, [4 x i8] zeroinitializer, ptr @"$e4main4CellC5values5Int32VvpACTK", ptr @"$e4main4CellC5values5Int32VvpACTK", ptr @"$e4main4CellC5values5Int32VvpACTk" }

//
// `\Wrap.doubled` — struct settable mutating computed → WritableKeyPath.
// Comp header 0x02C00000 = 46137344 (computedTag + settable + mutating).
//
// CHECK-IR-DAG: @keypath{{[.0-9]*}} = private constant { %swift.refcounted, i32, [4 x i8], i32, [4 x i8], ptr, ptr, ptr } { %swift.refcounted { ptr {{.*}}"$es15WritableKeyPathCy4main4WrapVs5Int32VGMf"{{.*}}, i64 -1 }, i32 -1610612704, [4 x i8] zeroinitializer, i32 46137344, [4 x i8] zeroinitializer, ptr @"$e4main4WrapV7doubleds5Int32VvpACTK", ptr @"$e4main4WrapV7doubleds5Int32VvpACTK", ptr @"$e4main4WrapV7doubleds5Int32VvpACTk" }

//
// `\Ro.v` — get-only struct computed → KeyPath.
// Buffer size = 4 (comp header) + 4 (skew) + 8 (id) + 8 (getter) = 24.
// Buffer header 0xA0000018 = -1610612712 (trivial + singleComponent + size=24).
// Comp   header 0x02000000 = 33554432 (computedTag + getOnly).  Only two ptr
// slots (id, getter) — no setter.
//
// CHECK-IR-DAG: @keypath{{[.0-9]*}} = private constant { %swift.refcounted, i32, [4 x i8], i32, [4 x i8], ptr, ptr } { %swift.refcounted { ptr {{.*}}"$es7KeyPathCy4main2RoVs5Int32VGMf"{{.*}}, i64 -1 }, i32 -1610612712, [4 x i8] zeroinitializer, i32 33554432, [4 x i8] zeroinitializer, ptr @"$e4main2RoV1vs5Int32VvpACTK", ptr @"$e4main2RoV1vs5Int32VvpACTK" }

//
// `\HasMethod.doThing` — instance method → same shape as a get-only
// computed component (no setter), value type is `() -> Int`.
//
// CHECK-IR-DAG: @keypath{{[.0-9]*}} = private constant { %swift.refcounted, i32, [4 x i8], i32, [4 x i8], ptr, ptr } { %swift.refcounted { ptr {{.*}}"$es7KeyPathCy4main9HasMethodVSiycGMf"{{.*}}, i64 -1 }, i32 -1610612712, [4 x i8] zeroinitializer, i32 33554432, [4 x i8] zeroinitializer, ptr @"$e4main9HasMethodV7doThingSiyFACTkmu", ptr @"$e4main9HasMethodV7doThingSiyFACTkmu" }


// Runtime behavior: applying the keypath yields the right offset / value.
var s = S(a: 1, b: 2)
s[keyPath: kpS()] = 42
print(s.b == 42 ? "OK!" : "FAIL") // CHECK-OUT: OK!

let sl = SLet(a: 3, b: 4)
print(sl[keyPath: kpSLet()] == 4 ? "OK!" : "FAIL") // CHECK-OUT: OK!

var box = Box<Int>(value: 7)
box[keyPath: kpBoxInt()] = 99
print(box.value == 99 ? "OK!" : "FAIL") // CHECK-OUT: OK!

var identityValue = 123
identityValue[keyPath: kpIdentity()] = 456
print(identityValue == 456 ? "OK!" : "FAIL") // CHECK-OUT: OK!

var pair: Pair = (11, 22)
pair[keyPath: kpTuple0()] = 111
pair[keyPath: kpTuple1()] = 222
print(pair.0 == 111 && pair.1 == 222 ? "OK!" : "FAIL") // CHECK-OUT: OK!

let cell = Cell()
cell[keyPath: kpCell()] = 88
print(cell.value == 88 ? "OK!" : "FAIL") // CHECK-OUT: OK!

var wrap = Wrap(storage: 0)
wrap[keyPath: kpWrapDoubled()] = 20
print(wrap.storage == 10 && wrap[keyPath: kpWrapDoubled()] == 20 ? "OK!" : "FAIL") // CHECK-OUT: OK!

let ro = Ro()
print(ro[keyPath: kpRoV()] == 7 ? "OK!" : "FAIL") // CHECK-OUT: OK!

let hm = HasMethod()
let fn = hm[keyPath: kpMethod()]
print(fn() == 42 ? "OK!" : "FAIL") // CHECK-OUT: OK!
