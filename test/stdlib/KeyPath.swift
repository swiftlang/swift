// RUN: %empty-directory(%t)
// RUN: %target-build-swift -swift-version 5 -g %s -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out
// REQUIRES: executable_test

import StdlibUnittest

var keyPath = TestSuite("key paths")

final class C<T> {
  var x: Int
  var y: LifetimeTracked?
  var z: T
  let immutable: String
  private(set) var secretlyMutable: String

  var computed: T {
    get {
      return z
    }
    set {
      z = newValue
    }
  }

  init(x: Int, y: LifetimeTracked?, z: T) {
    self.x = x
    self.y = y
    self.z = z
    self.immutable = "\(x) \(y) \(z)"
    self.secretlyMutable = immutable
  }
}

struct Point: Equatable {
  var x: Double
  var y: Double
  var trackLifetime = LifetimeTracked(123)
  let hypotenuse: Double
  private(set) var secretlyMutableHypotenuse: Double
  
  init(x: Double, y: Double) {
    self.x = x
    self.y = y
    hypotenuse = x*x + y*y
    secretlyMutableHypotenuse = x*x + y*y
  }
  
  static func ==(a: Point, b: Point) -> Bool {
    return a.x == b.x && a.y == b.y
  }
}

struct S<T: Equatable>: Equatable {
  var x: Int
  var y: LifetimeTracked?
  var z: T
  var p: Point
  var c: C<T>
  
  static func ==(a: S, b: S) -> Bool {
    return a.x == b.x
      && a.y === b.y
      && a.z == b.z
      && a.p == b.p
      && a.c === b.c
  }
}

final class ComputedA {
  var readOnly: ComputedB { fatalError() }
  var nonmutating: ComputedB {
    get { fatalError() }
    set { fatalError() }
  }
  var reabstracted: () -> () = {}
}

struct ComputedB {
  var readOnly: ComputedA { fatalError() }
  var mutating: ComputedA { 
    get { fatalError() }
    set { fatalError() }
  }
  var nonmutating: ComputedA {
    get { fatalError() }
    nonmutating set { fatalError() }
  }
  var reabstracted: () -> () = {}
}

keyPath.test("key path in-place instantiation") {
  for _ in 1...2 {
    let s_x = (\S<Int>.x as AnyKeyPath) as! WritableKeyPath<S<Int>, Int>
    let s_y = (\S<Int>.y as AnyKeyPath) as! WritableKeyPath<S<Int>, LifetimeTracked?>
    let s_z = (\S<Int>.z as AnyKeyPath) as! WritableKeyPath<S<Int>, Int>
    let s_p = (\S<Int>.p as AnyKeyPath) as! WritableKeyPath<S<Int>, Point>
    let s_p_x = (\S<Int>.p.x as AnyKeyPath) as! WritableKeyPath<S<Int>, Double>
    let s_p_y = (\S<Int>.p.y as AnyKeyPath) as! WritableKeyPath<S<Int>, Double>
    let s_c = (\S<Int>.c as AnyKeyPath) as! WritableKeyPath<S<Int>, C<Int>>
    let s_c_x = (\S<Int>.c.x as AnyKeyPath) as! ReferenceWritableKeyPath<S<Int>, Int>

    let c_x = (\C<Int>.x as AnyKeyPath) as! ReferenceWritableKeyPath<C<Int>, Int>
    let s_c_x_2 = s_c.appending(path: c_x)

    expectEqual(s_c_x, s_c_x_2)
    expectEqual(s_c_x_2, s_c_x)
    expectEqual(s_c_x.hashValue, s_c_x_2.hashValue)

    let point_x = (\Point.x as AnyKeyPath) as! WritableKeyPath<Point, Double>
    let point_y = (\Point.y as AnyKeyPath) as! WritableKeyPath<Point, Double>

    let s_p_x_2 = s_p.appending(path: point_x)
    let s_p_y_2 = s_p.appending(path: point_y)

    expectEqual(s_p_x, s_p_x_2)
    expectEqual(s_p_x_2, s_p_x)
    expectEqual(s_p_x_2.hashValue, s_p_x.hashValue)
    expectEqual(s_p_y, s_p_y_2)
    expectEqual(s_p_y_2, s_p_y)
    expectEqual(s_p_y_2.hashValue, s_p_y.hashValue)

    let ca_readOnly = (\ComputedA.readOnly as AnyKeyPath) as! KeyPath<ComputedA, ComputedB>
    let ca_nonmutating = (\ComputedA.nonmutating as AnyKeyPath) as! ReferenceWritableKeyPath<ComputedA, ComputedB>
    let ca_reabstracted = (\ComputedA.reabstracted as AnyKeyPath) as! ReferenceWritableKeyPath<ComputedA, () -> ()>

    let cb_readOnly = (\ComputedB.readOnly as AnyKeyPath) as! KeyPath<ComputedB, ComputedA>
    let cb_mutating = (\ComputedB.mutating as AnyKeyPath) as! WritableKeyPath<ComputedB, ComputedA>
    let cb_nonmutating = (\ComputedB.nonmutating as AnyKeyPath) as! ReferenceWritableKeyPath<ComputedB, ComputedA>
    let cb_reabstracted = (\ComputedB.reabstracted as AnyKeyPath) as! WritableKeyPath<ComputedB, () -> ()>
  
    let ca_readOnly_mutating = (\ComputedA.readOnly.mutating as AnyKeyPath) as! KeyPath<ComputedA, ComputedA>
    let cb_mutating_readOnly = (\ComputedB.mutating.readOnly as AnyKeyPath) as! KeyPath<ComputedB, ComputedB>
    let ca_readOnly_nonmutating = (\ComputedA.readOnly.nonmutating as AnyKeyPath) as! ReferenceWritableKeyPath<ComputedA, ComputedA>
    let cb_readOnly_reabstracted = (\ComputedB.readOnly.reabstracted as AnyKeyPath) as! ReferenceWritableKeyPath<ComputedB, () -> ()>

    let ca_readOnly_mutating2 = ca_readOnly.appending(path: cb_mutating)
    expectEqual(ca_readOnly_mutating, ca_readOnly_mutating2)
    expectEqual(ca_readOnly_mutating2, ca_readOnly_mutating)
    expectEqual(ca_readOnly_mutating.hashValue, ca_readOnly_mutating2.hashValue)

    let cb_mutating_readOnly2 = cb_mutating.appending(path: ca_readOnly)
    expectEqual(cb_mutating_readOnly, cb_mutating_readOnly2)
    expectEqual(cb_mutating_readOnly2, cb_mutating_readOnly)
    expectEqual(cb_mutating_readOnly.hashValue, cb_mutating_readOnly2.hashValue)

    let ca_readOnly_nonmutating2 = ca_readOnly.appending(path: cb_nonmutating)
    expectEqual(ca_readOnly_nonmutating, ca_readOnly_nonmutating2)
    expectEqual(ca_readOnly_nonmutating2, ca_readOnly_nonmutating)
    expectEqual(ca_readOnly_nonmutating.hashValue,
                ca_readOnly_nonmutating2.hashValue)

    let cb_readOnly_reabstracted2 = cb_readOnly.appending(path: ca_reabstracted)
    expectEqual(cb_readOnly_reabstracted,
                cb_readOnly_reabstracted2)
    expectEqual(cb_readOnly_reabstracted2,
                cb_readOnly_reabstracted)
    expectEqual(cb_readOnly_reabstracted2.hashValue,
                cb_readOnly_reabstracted.hashValue)
  }
}

keyPath.test("key path generic instantiation") {
  func testWithGenericParam<T: Equatable>(_: T.Type) -> ReferenceWritableKeyPath<S<T>, Int> {
    for i in 1...2 {
      let s_x = (\S<T>.x as AnyKeyPath) as! WritableKeyPath<S<T>, Int>
      let s_y = (\S<T>.y as AnyKeyPath) as! WritableKeyPath<S<T>, LifetimeTracked?>
      let s_z = (\S<T>.z as AnyKeyPath) as! WritableKeyPath<S<T>, T>
      let s_p = (\S<T>.p as AnyKeyPath) as! WritableKeyPath<S<T>, Point>
      let s_p_x = (\S<T>.p.x as AnyKeyPath) as! WritableKeyPath<S<T>, Double>
      let s_p_y = (\S<T>.p.y as AnyKeyPath) as! WritableKeyPath<S<T>, Double>
      let s_c = (\S<T>.c as AnyKeyPath) as! WritableKeyPath<S<T>, C<T>>
      let s_c_x = (\S<T>.c.x as AnyKeyPath) as! ReferenceWritableKeyPath<S<T>, Int>

      let c_x = (\C<T>.x as AnyKeyPath) as! ReferenceWritableKeyPath<C<T>, Int>
      let s_c_x_2 = s_c.appending(path: c_x)

      expectEqual(s_c_x, s_c_x_2)
      expectEqual(s_c_x_2, s_c_x)
      expectEqual(s_c_x.hashValue, s_c_x_2.hashValue)

      let point_x = (\Point.x as AnyKeyPath) as! WritableKeyPath<Point, Double>
      let point_y = (\Point.y as AnyKeyPath) as! WritableKeyPath<Point, Double>

      let s_p_x_2 = s_p.appending(path: point_x)
      let s_p_y_2 = s_p.appending(path: point_y)

      expectEqual(s_p_x, s_p_x_2)
      expectEqual(s_p_x_2, s_p_x)
      expectEqual(s_p_x_2.hashValue, s_p_x.hashValue)
      expectEqual(s_p_y, s_p_y_2)
      expectEqual(s_p_y_2, s_p_y)
      expectEqual(s_p_y_2.hashValue, s_p_y.hashValue)

      if i == 2 { return s_c_x }
    }
    fatalError()
  }
  let s_c_x_int = testWithGenericParam(Int.self)
  let s_c_x_int2 = \S<Int>.c.x
  expectEqual(s_c_x_int, s_c_x_int2)

  let s_c_x_string = testWithGenericParam(String.self)
  let s_c_x_string2 = \S<String>.c.x
  expectEqual(s_c_x_string, s_c_x_string2)

  let s_c_x_lt = testWithGenericParam(LifetimeTracked.self)
  let s_c_x_lt2 = \S<LifetimeTracked>.c.x
  expectEqual(s_c_x_lt, s_c_x_lt2)
}

protocol P {}

struct TestComputed: P {
  static var numNonmutatingSets = 0
  static var numMutatingSets = 0

  static func resetCounts() {
    numNonmutatingSets = 0
    numMutatingSets = 0
  }

  var canary = LifetimeTracked(0)

  var readonly: LifetimeTracked {
    return LifetimeTracked(1)
  }
  var nonmutating: LifetimeTracked {
    get {
      return LifetimeTracked(2)
    }
    nonmutating set { TestComputed.numNonmutatingSets += 1 }
  }
  var mutating: LifetimeTracked {
    get {
      return LifetimeTracked(3)
    }
    set {
      canary = newValue
    }
  }
}

extension P {
  var readonlyProtoExt: Self { return self }
  var mutatingProtoExt: Self {
    get { return self }
    set { self = newValue }
  }
}

keyPath.test("computed properties") {
  var test = TestComputed()

  do {
    let tc_readonly = \TestComputed.readonly
    expectTrue(test[keyPath: tc_readonly] !== test[keyPath: tc_readonly])
    expectEqual(test[keyPath: tc_readonly].value,
                test[keyPath: tc_readonly].value)
  }

  do {
    let tc_nonmutating = \TestComputed.nonmutating
    expectTrue(test[keyPath: tc_nonmutating] !== test[keyPath: tc_nonmutating])
    expectEqual(test[keyPath: tc_nonmutating].value,
                test[keyPath: tc_nonmutating].value)
    TestComputed.resetCounts()
    test[keyPath: tc_nonmutating] = LifetimeTracked(4)
    expectEqual(TestComputed.numNonmutatingSets, 1)
  }

  do {
    let tc_mutating = \TestComputed.mutating
    expectTrue(test[keyPath: tc_mutating] !== test[keyPath: tc_mutating])
    expectEqual(test[keyPath: tc_mutating].value,
                test[keyPath: tc_mutating].value)
    let newObject = LifetimeTracked(5)
    test[keyPath: tc_mutating] = newObject
    expectTrue(test.canary === newObject)
  }

  do {
    let tc_readonlyProtoExt = \TestComputed.readonlyProtoExt
    expectTrue(test.canary === test[keyPath: tc_readonlyProtoExt].canary)
  }

  do {
    let tc_mutatingProtoExt = \TestComputed.mutatingProtoExt
    expectTrue(test.canary === test[keyPath: tc_mutatingProtoExt].canary)
    let oldTest = test
    test[keyPath: tc_mutatingProtoExt] = TestComputed()
    expectTrue(oldTest.canary !== test.canary)
    expectTrue(test.canary === test[keyPath: tc_mutatingProtoExt].canary)
  }
}

class AB {
}
class ABC: AB, ABCProtocol {
  var a = LifetimeTracked(1)
  var b = LifetimeTracked(2)
  var c = LifetimeTracked(3)
}

protocol ABCProtocol {
  var a: LifetimeTracked { get }
  var b: LifetimeTracked { get set }
  var c: LifetimeTracked { get nonmutating set }
}

keyPath.test("dynamically-typed application") {
  let cPaths = [\ABC.a, \ABC.b, \ABC.c]

  let subject = ABC()

  do {
    let fields = cPaths.map { subject[keyPath: $0] }
    expectTrue(fields[0] as! AnyObject === subject.a)
    expectTrue(fields[1] as! AnyObject === subject.b)
    expectTrue(fields[2] as! AnyObject === subject.c)
  }

  let erasedSubject: AB = subject
  let erasedPaths: [AnyKeyPath] = cPaths
  let wrongSubject = AB()

  do {
    let fields = erasedPaths.map { erasedSubject[keyPath: $0] }
    expectTrue(fields[0]! as! AnyObject === subject.a)
    expectTrue(fields[1]! as! AnyObject === subject.b)
    expectTrue(fields[2]! as! AnyObject === subject.c)

    let wrongFields = erasedPaths.map { wrongSubject[keyPath: $0] }
    expectTrue(wrongFields[0] == nil)
    expectTrue(wrongFields[1] == nil)
    expectTrue(wrongFields[2] == nil)
  }

  var protoErasedSubject: ABCProtocol = subject
  let protoErasedPathA = \ABCProtocol.a
  let protoErasedPathB = \ABCProtocol.b
  let protoErasedPathC = \ABCProtocol.c

  do {
    expectTrue(protoErasedSubject.a ===
                  protoErasedSubject[keyPath: protoErasedPathA])

    let newB = LifetimeTracked(4)
    expectTrue(protoErasedSubject.b ===
                  protoErasedSubject[keyPath: protoErasedPathB])
    protoErasedSubject[keyPath: protoErasedPathB] = newB
    expectTrue(protoErasedSubject.b ===
                  protoErasedSubject[keyPath: protoErasedPathB])
    expectTrue(protoErasedSubject.b === newB)

    let newC = LifetimeTracked(5)
    expectTrue(protoErasedSubject.c ===
                  protoErasedSubject[keyPath: protoErasedPathC])
    protoErasedSubject[keyPath: protoErasedPathC] = newC
    expectTrue(protoErasedSubject.c ===
                  protoErasedSubject[keyPath: protoErasedPathC])
    expectTrue(protoErasedSubject.c === newC)
  }
}

struct TestOptional {
  var origin: Point?
  var questionableCanary: LifetimeTracked? = LifetimeTracked(123)

  init(origin: Point?) {
    self.origin = origin
  }
}

keyPath.test("optional force-unwrapping") {
  let origin_x = \TestOptional.origin!.x
  let canary = \TestOptional.questionableCanary!

  var value = TestOptional(origin: Point(x: 3, y: 4))

  expectEqual(value[keyPath: origin_x], 3)
  expectEqual(value.origin!.x, 3)

  value[keyPath: origin_x] = 5

  expectEqual(value[keyPath: origin_x], 5)
  expectEqual(value.origin!.x, 5)

  expectTrue(value[keyPath: canary] === value.questionableCanary)
  let newCanary = LifetimeTracked(456)
  value[keyPath: canary] = newCanary
  expectTrue(value[keyPath: canary] === newCanary)
  expectTrue(value.questionableCanary === newCanary)
}

keyPath.test("optional force-unwrapping trap") {
  let origin_x = \TestOptional.origin!.x
  var value = TestOptional(origin: nil)

  expectCrashLater()
  _ = value[keyPath: origin_x]
}

struct TestOptional2 {
  var optional: TestOptional?
}

keyPath.test("optional chaining") {
  let origin_x = \TestOptional.origin?.x
  let canary = \TestOptional.questionableCanary?.value
  
  let withPoint = TestOptional(origin: Point(x: 3, y: 4))
  expectEqual(withPoint[keyPath: origin_x]!, 3)
  expectEqual(withPoint[keyPath: canary]!, 123)

  let withoutPoint = TestOptional(origin: nil)
  expectNil(withoutPoint[keyPath: origin_x])

  let optional2: TestOptional2? = TestOptional2(optional: withPoint)
  let optional2_optional = \TestOptional2?.?.optional
  expectEqual(optional2[keyPath: optional2_optional]!.origin!.x, 3)
  expectEqual(optional2[keyPath: optional2_optional]!.origin!.y, 4)
}

func makeKeyPathInGenericContext<T>(of: T.Type)
    -> ReferenceWritableKeyPath<C<T>, T> {
  return \C<T>.computed
}

keyPath.test("computed generic key paths") {
  let path = makeKeyPathInGenericContext(of: LifetimeTracked.self)
  let z = LifetimeTracked(456)
  let c = C(x: 42, y: LifetimeTracked(123), z: z)

  expectTrue(c[keyPath: path] === z)

  let z2 = LifetimeTracked(789)
  c[keyPath: path] = z2
  expectTrue(c[keyPath: path] === z2)
  expectTrue(c.z === z2)

  let path2 = makeKeyPathInGenericContext(of: LifetimeTracked.self)

  expectEqual(path, path2)
  expectEqual(path.hashValue, path2.hashValue)

  let pathNonGeneric = \C<LifetimeTracked>.computed
  expectEqual(path, pathNonGeneric)
  expectEqual(path.hashValue, pathNonGeneric.hashValue)

  let valuePath = path.appending(path: \LifetimeTracked.value)

  expectEqual(c[keyPath: valuePath], 789)

  let valuePathNonGeneric = pathNonGeneric.appending(path: \LifetimeTracked.value)
  expectEqual(valuePath, valuePathNonGeneric)
  expectEqual(valuePath.hashValue, valuePathNonGeneric.hashValue)
}

var numberOfMutatingWritebacks = 0
var numberOfNonmutatingWritebacks = 0

struct NoisyWriteback {
  var canary = LifetimeTracked(246)

  var mutating: LifetimeTracked {
    get { return canary }
    set { numberOfMutatingWritebacks += 1 }
  }

  var nonmutating: LifetimeTracked {
    get { return canary }
    nonmutating set { numberOfNonmutatingWritebacks += 1 }
  }
}

keyPath.test("read-only accesses don't trigger writebacks") {
  var x = NoisyWriteback()
  x = NoisyWriteback() // suppress "never mutated" warnings

  let wkp = \NoisyWriteback.mutating
  let rkp = \NoisyWriteback.nonmutating

  numberOfMutatingWritebacks = 0
  numberOfNonmutatingWritebacks = 0
  _ = x[keyPath: wkp]
  _ = x[keyPath: rkp]

  expectEqual(x[keyPath: wkp].value, 246)
  expectEqual(x[keyPath: rkp].value, 246)

  expectEqual(numberOfMutatingWritebacks, 0)
  expectEqual(numberOfNonmutatingWritebacks, 0)

  let y = x
  _ = y[keyPath: wkp]
  _ = y[keyPath: rkp]

  expectEqual(y[keyPath: wkp].value, 246)
  expectEqual(y[keyPath: rkp].value, 246)

  expectEqual(numberOfMutatingWritebacks, 0)
  expectEqual(numberOfNonmutatingWritebacks, 0)
}

var nestedWritebackLog = 0

struct NoisyNestingWriteback {
  var value: Int

  var nested: NoisyNestingWriteback {
    get {
      return NoisyNestingWriteback(value: value + 1)
    }
    set {
      nestedWritebackLog = nestedWritebackLog << 8 | newValue.value
      value = newValue.value - 1
    }
  }
}

keyPath.test("writebacks nest properly") {
  var test = NoisyNestingWriteback(value: 0)
  nestedWritebackLog = 0
  test.nested.nested.nested.value = 0x38
  expectEqual(nestedWritebackLog, 0x383736)

  nestedWritebackLog = 0
  let kp = \NoisyNestingWriteback.nested.nested.nested
  test[keyPath: kp].value = 0x38
  expectEqual(nestedWritebackLog, 0x383736)
}

struct IUOWrapper {
  var wrapped: IUOWrapped!
}

struct IUOWrapped {
  var value: Int
}

keyPath.test("IUO and key paths") {
  var subject = IUOWrapper(wrapped: IUOWrapped(value: 1989))
  let kp1 = \IUOWrapper.wrapped.value

  expectEqual(subject[keyPath: kp1], 1989)
  subject[keyPath: kp1] = 1738
  expectEqual(subject[keyPath: kp1], 1738)
  expectEqual(subject.wrapped.value, 1738)

  let kp2 = \IUOWrapper.wrapped!.value

  expectEqual(kp1, kp2)
  expectEqual(kp1.hashValue, kp2.hashValue)
}

struct SubscriptResult<T: Hashable, U: Hashable> {
  var canary = LifetimeTracked(3333)
  var left: T
  var right: U

  init(left: T, right: U) {
    self.left = left
    self.right = right
  }

  subscript(left: T) -> Bool {
    return self.left == left
  }
  subscript(right: U) -> Bool {
    return self.right == right
  }
}

struct Subscripts<T: Hashable> {
  var canary = LifetimeTracked(4444)

  subscript<U: Hashable>(x: T, y: U) -> SubscriptResult<T, U> {
    return SubscriptResult(left: x, right: y)
  }

  subscript(x: Int, y: Int) -> Int {
    return x + y
  }
}

struct KeyA: Hashable {
  var canary = LifetimeTracked(1111)
  var value: String

  init(value: String) { self.value = value }

  static func ==(a: KeyA, b: KeyA) -> Bool { return a.value == b.value }
  func hash(into hasher: inout Hasher) {
    hasher.combine(value)
  }
}
struct KeyB: Hashable {
  var canary = LifetimeTracked(2222)

  var value: Int

  init(value: Int) { self.value = value }

  static func ==(a: KeyB, b: KeyB) -> Bool { return a.value == b.value }
  func hash(into hasher: inout Hasher) {
    hasher.combine(value)
  }
}

func fullGenericContext<T: Hashable, U: Hashable>(x: T, y: U) -> KeyPath<Subscripts<T>, SubscriptResult<T, U>> {
  return \Subscripts<T>.[x, y]
}

func halfGenericContext<U: Hashable>(x: KeyA, y: U) -> KeyPath<Subscripts<KeyA>, SubscriptResult<KeyA, U>> {
  return \Subscripts<KeyA>.[x, y]
}

func nonGenericContext(x: KeyA, y: KeyB) -> KeyPath<Subscripts<KeyA>, SubscriptResult<KeyA, KeyB>> {
  return \Subscripts<KeyA>.[x, y]
}

keyPath.test("subscripts") {
  let a = fullGenericContext(x: KeyA(value: "hey"), y: KeyB(value: 1738))
  let b = halfGenericContext(x: KeyA(value: "hey"), y: KeyB(value: 1738))
  let c = nonGenericContext(x: KeyA(value: "hey"), y: KeyB(value: 1738))

  expectEqual(a, b)
  expectEqual(a, c)
  expectEqual(b, a)
  expectEqual(b, c)
  expectEqual(c, a)
  expectEqual(c, b)
  expectEqual(a.hashValue, b.hashValue)
  expectEqual(a.hashValue, c.hashValue)
  expectEqual(b.hashValue, a.hashValue)
  expectEqual(b.hashValue, c.hashValue)
  expectEqual(c.hashValue, a.hashValue)
  expectEqual(c.hashValue, b.hashValue)

  let base = Subscripts<KeyA>()

  let kp2 = \SubscriptResult<KeyA, KeyB>.[KeyA(value: "hey")]

  for kp in [a, b, c] {
    let projected = base[keyPath: kp]
    expectEqual(projected.left.value, "hey")
    expectEqual(projected.right.value, 1738)

    expectEqual(projected[keyPath: kp2], true)

    let kp12 =
      \Subscripts<KeyA>.[KeyA(value: "hey"), KeyB(value: 1738)][KeyA(value: "hey")]

    let kp12a = kp.appending(path: kp2)

    expectEqual(kp12, kp12a)
    expectEqual(kp12a, kp12)
    expectEqual(kp12.hashValue, kp12a.hashValue)
  }

  let ints = \Subscripts<KeyA>.[17, 38]
  let ints2 = \Subscripts<KeyA>.[17, 38]
  let ints3 = \Subscripts<KeyA>.[38, 17]
  expectEqual(base[keyPath: ints], 17 + 38)

  expectEqual(ints, ints2)
  expectEqual(ints2, ints)
  expectNotEqual(ints, ints3)
  expectNotEqual(ints2, ints3)
  expectNotEqual(ints3, ints)
  expectNotEqual(ints3, ints2)

  expectEqual(ints.hashValue, ints2.hashValue)

  let ints_be = ints.appending(path: \Int.bigEndian)

  expectEqual(base[keyPath: ints_be], (17 + 38).bigEndian)
}

struct NonOffsetableProperties {
  // observers
  var x: Int { didSet {} }
  // reabstracted
  var y: () -> ()
  // computed
  var z: Int { return 0 }
}

func getIdentityKeyPathOfType<T>(_: T.Type) -> KeyPath<T, T> {
  return \.self
}

keyPath.test("offsets") {
  let SLayout = MemoryLayout<S<Int>>.self
  expectNotNil(SLayout.offset(of: \S<Int>.x))
  expectNotNil(SLayout.offset(of: \S<Int>.y))
  expectNotNil(SLayout.offset(of: \S<Int>.z))
  expectNotNil(SLayout.offset(of: \S<Int>.p))
  expectNotNil(SLayout.offset(of: \S<Int>.p.x))
  expectNotNil(SLayout.offset(of: \S<Int>.p.y))
  expectNotNil(SLayout.offset(of: \S<Int>.c))
  expectNil(SLayout.offset(of: \S<Int>.c.x))

  let NOPLayout = MemoryLayout<NonOffsetableProperties>.self
  expectNil(NOPLayout.offset(of: \NonOffsetableProperties.x))
  expectNil(NOPLayout.offset(of: \NonOffsetableProperties.y))
  expectNil(NOPLayout.offset(of: \NonOffsetableProperties.z))

  expectEqual(SLayout.offset(of: \.self), 0)
  expectEqual(SLayout.offset(of: getIdentityKeyPathOfType(S<Int>.self)), 0)
}

keyPath.test("identity key path") {
  var x = LifetimeTracked(1738)

  let id = \LifetimeTracked.self
  expectTrue(x === x[keyPath: id])

  let newX = LifetimeTracked(679)
  x[keyPath: id] = newX
  expectTrue(x === newX)

  let id2 = getIdentityKeyPathOfType(LifetimeTracked.self)
  expectEqual(id, id2)
  expectEqual(id.hashValue, id2.hashValue)
  expectNotNil(id2 as? WritableKeyPath)

  let id3 = id.appending(path: id2)
  expectEqual(id, id3)
  expectEqual(id.hashValue, id3.hashValue)
  expectNotNil(id3 as? WritableKeyPath)

  let valueKey = \LifetimeTracked.value
  let valueKey2 = id.appending(path: valueKey)
  let valueKey3 = (valueKey as KeyPath).appending(path: \Int.self)

  expectEqual(valueKey, valueKey2)
  expectEqual(valueKey.hashValue, valueKey2.hashValue)
  expectEqual(valueKey, valueKey3)
  expectEqual(valueKey.hashValue, valueKey3.hashValue)

  expectEqual(x[keyPath: valueKey2], 679)
  expectEqual(x[keyPath: valueKey3], 679)
}

keyPath.test("let-ness") {
  expectNil(\C<Int>.immutable as? ReferenceWritableKeyPath)
  expectNotNil(\C<Int>.secretlyMutable as? ReferenceWritableKeyPath)
  expectNil(\Point.hypotenuse as? WritableKeyPath)
  expectNotNil(\Point.secretlyMutableHypotenuse as? WritableKeyPath)
}

// SR-6096

protocol Protocol6096 {}
struct Value6096<ValueType> {}
extension Protocol6096 {
    var asString: String? {
        return self as? String
    }
}
extension Value6096 where ValueType: Protocol6096 {
    func doSomething() {
        _ = \ValueType.asString?.endIndex
    }
}
extension Int: Protocol6096 {}

keyPath.test("optional chaining component that needs generic instantiation") {
  Value6096<Int>().doSomething()
}

// Nested generics.
protocol HasAssoc {
  associatedtype A
}

struct Outer<T, U> {
  struct Middle<V, W, X> {
  }
}

extension Outer.Middle where V: HasAssoc, U == V.A, W == X {
  struct Inner<Y: Hashable> {
    func foo() ->  AnyKeyPath {
      return \[Y?: [U]].values
    }
  }
}

extension Double: HasAssoc {
  typealias A = Float
}

keyPath.test("nested generics") {
  let nested = Outer<Int, Float>.Middle<Double, String, String>.Inner<UInt>()
  let nestedKeyPath = nested.foo()
  typealias DictType = [UInt? : [Float]]
  expectTrue(nestedKeyPath is KeyPath<DictType, DictType.Values>)
}

runAllTests()

