// RUN: %target-run-simple-swift(-I %S/Inputs -cxx-interoperability-mode=default -enable-experimental-feature ForeignReferenceTypeSubclassing -target %target-swift-5.8-abi-triple -Xcc -fignore-exceptions)

// REQUIRES: executable_test
// REQUIRES: swift_feature_ForeignReferenceTypeSubclassing

// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

import StdlibUnittest
import InheritFRTSubclassing

var Suite = TestSuite("ForeignReferenceTypeSubclassing")

final class EmptySub: SubclassableShared {
  func tag() -> Int32 { return 99 }
}

final class FieldSub: SubclassableShared {
  let x: Int64
  let y: Int64
  init(x: Int64, y: Int64) {
    self.x = x
    self.y = y
    super.init()
  }
  func sum() -> Int64 { return x + y }
}

final class OneArgSub: SharedConstructed {
  let f: Int64
  init(f: Int64, a: Int32) {
    self.f = f
    super.init(a)
  }
}
final class TwoArgSub: SharedConstructed {
  let f: Int64
  init(f: Int64, a: Int32, b: CLong) {
    self.f = f
    super.init(a, b)
  }
}

// This one has no initializer of its own: it gets an implicit default
// initializer that chains to the base's no-argument constructor.
final class DefaultInitSub: SharedConstructed {
  let tag: Int64 = 7
}

Suite.test("empty subclass: static method dispatch and upcast") {
  let s = EmptySub()
  expectEqual(99, s.tag())

  let base: SubclassableShared = s
  base.set(3)
  expectEqual(3, s.get())
}

Suite.test("implicit default initializer") {
  let s = DefaultInitSub()
  expectEqual(7, s.tag)
  expectEqual(0, s.getA())
  expectEqual(0, s.getB())

  let base: SharedConstructed = s
  expectEqual(0, base.getA())
}

Suite.test("stored properties survive base construction") {
  let s = FieldSub(x: 42, y: 100)
  expectEqual(42, s.x)
  expectEqual(100, s.y)
  expectEqual(142, s.sum())

  s.set(7)
  expectEqual(7, s.get())
}

Suite.test("super.init forwards arguments to the C++ base constructor") {
  let one = OneArgSub(f: 7, a: 42)
  expectEqual(7, one.f)
  expectEqual(42, one.getA())
  expectEqual(0, one.getB())

  let two = TwoArgSub(f: 99, a: 11, b: 22)
  expectEqual(99, two.f)
  expectEqual(11, two.getA())
  expectEqual(22, two.getB())
}

final class ConstRefSub: ReferenceConstructed {
  let tag: Int64 = 1
  init(_ i: Int32) { super.init(i) }
}
final class InoutSub: ReferenceConstructed {
  let tag: Int64 = 1
  init(_ i: inout Int32) { super.init(&i, 0) }
}
final class ByValueSub: ReferenceConstructed {
  let tag: Int64 = 1
  init(_ arg: NonTrivialArg) { super.init(arg, CLong(0)) }
}
final class ConstRefNonTrivialSub: ReferenceConstructed {
  let tag: Int64 = 1
  init(_ arg: NonTrivialArg) { super.init(arg, CChar(0)) }
}
final class ReferenceTypeArgSub: ReferenceConstructed {
  let tag: Int64 = 1
  init(_ arg: RefcountedArg) { super.init(arg, CShort(0)) }
}
final class SelfFieldArgSub: ReferenceConstructed {
  let x: Int32
  init(x: Int32) {
    self.x = x
    super.init(self.x)
  }
}
final class SelfReferenceTypeArgSub: ReferenceConstructed {
  let arg: RefcountedArg
  init(_ arg: RefcountedArg) {
    self.arg = arg
    super.init(self.arg, CShort(0))
  }
}
final class PicksIntSub: OverloadedConstructed {
  let tag: Int64 = 1
  init() { super.init(Int32(1)) }
}
final class PicksDoubleSub: OverloadedConstructed {
  let tag: Int64 = 1
  init() { super.init(2.0) }
}

Suite.test("super.init passes reference parameters by reference") {
  expectEqual(42, ConstRefSub(42).getSeen())

  var i: Int32 = 42
  let s = InoutSub(&i)
  expectEqual(42, s.getSeen())
  // The C++ constructor wrote through the reference.
  expectEqual(100, i)
}

Suite.test("super.init copies and destroys non-trivial C++ arguments") {
  expectEqual(0, getLiveNonTrivialArgs())
  do {
    let arg = NonTrivialArg(7)
    expectEqual(7, ByValueSub(arg).getSeen())
    expectEqual(7, ConstRefNonTrivialSub(arg).getSeen())
    // Only `arg` itself is left alive.
    expectEqual(1, getLiveNonTrivialArgs())
  }
  expectEqual(0, getLiveNonTrivialArgs())
}

Suite.test("super.init does not leak foreign reference arguments") {
  let arg = RefcountedArg(9)
  expectEqual(1, arg.getRefcount())
  do {
    expectEqual(9, ReferenceTypeArgSub(arg).getSeen())
  }
  expectEqual(1, arg.getRefcount())
}

Suite.test("super.init arguments may read stored properties of self") {
  expectEqual(11, SelfFieldArgSub(x: 11).getSeen())

  let arg = RefcountedArg(5)
  let s = SelfReferenceTypeArgSub(arg)
  expectEqual(5, s.getSeen())
  // One reference from `arg`, one from `s.arg`.
  expectEqual(2, arg.getRefcount())
}

Suite.test("super.init constructs the base with the overload Swift picked") {
  expectEqual(1, PicksIntSub().getWhich())
  expectEqual(2, PicksDoubleSub().getWhich())
}

// A `let` property passed to a `const &` parameter is passed in place, as it
// would be to a direct call, so a reference the base keeps stays valid.
final class KeepsOwnFieldSub: KeepsReference {
  let x: Int32
  init(x: Int32) {
    self.x = x
    super.init(self.x)
  }
}

// A base with more than one v-table pointer can dispatch through each of them.
final class MultiplyInheritedSub: MultiplyInheritedShared {
  let z: Int64 = 3
}

Suite.test("multiple inheritance") {
  let s = MultiplyInheritedSub()
  expectEqual(7, callSecondary(s))
  expectEqual(3, s.z)
}

Suite.test("super.init passes let properties of self in place") {
  let s = KeepsOwnFieldSub(x: 42)
  expectTrue(s.refersIntoSelf())
  expectEqual(42, s.read())
}

// The constructor of the base returns the new object unretained. Constructing
// the base subobject retains it once, for the reference that `self` holds.
final class UnretainedSub: UnretainedConstructed {
  let x: Int64 = 5
}

Suite.test("unretained base constructor") {
  do {
    let s = UnretainedSub()
    expectEqual(5, s.x)
    expectEqual(1, s.getRefcount())
  }
  expectEqual(0, getLiveUnretainedConstructed())
}

protocol Tagged {
  func tag() -> Int64
}
extension UnretainedSub: Tagged {
  func tag() -> Int64 { return x }
}

func typeName<T>(_: T) -> String { return "\(T.self)" }

Suite.test("type metadata") {
  do {
    let s = UnretainedSub()
    let array = [s, s]
    expectEqual(2, array.count)
    expectEqual("UnretainedSub", typeName(s))
    expectEqual("UnretainedSub", "\(UnretainedSub.self)")
    expectTrue(type(of: s) == UnretainedSub.self)

    let any: Any = s
    expectTrue(any is UnretainedSub)
    expectEqual(5, (any as? UnretainedSub)?.x)

    let tagged: any Tagged = s
    expectEqual(5, tagged.tag())
  }
  expectEqual(0, getLiveUnretainedConstructed())
}

final class BitfieldLastSub: BitfieldLastShared {
  var x: Int8 = 0x7f
}

final class UnimportedFieldLastSub: UnimportedFieldLastShared {
  var x: Int8 = 0x7f
}

final class VTablePointerOnlySub: VTablePointerOnlyShared {
  var x: Int8 = 0x7f
}

Suite.test("stored properties after unimported storage of the base") {
  let b = BitfieldLastSub()
  b.x = -1
  expectEqual(0xabc, b.getFlags())
  expectEqual(-1, b.x)

  let u = UnimportedFieldLastSub()
  u.x = -1
  expectEqual(42, u.getInnerRefcount())
  expectEqual(-1, u.x)

  let v = VTablePointerOnlySub()
  v.x = -1
  expectEqual(42, v.tag())
  expectEqual(-1, v.x)
}

final class ClassAllocatedSub: ClassAllocatedShared {
  var x: Int64 = 1
}

Suite.test("allocated with class-specific allocation functions") {
  let before = getClassAllocationCounts()
  do {
    let s = ClassAllocatedSub()
    expectEqual(1, s.x)
  }
  let after = getClassAllocationCounts()
  expectEqual(before.newCount + 1, after.newCount)
  expectEqual(before.deleteCount + 1, after.deleteCount)
}

runAllTests()
