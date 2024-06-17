// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import StdlibUnittest

defer { runAllTests() }

var ProtocolExtensionTestSuite = TestSuite("ProtocolExtensions")

// Extend a protocol with a property.
extension Sequence {
  var myCount: Int {
    var result = 0
    for _ in self {
      result += 1
    }
    return result
  }
}

// Extend a protocol with a function.
extension Collection {
  var myIndices: Range<Index> {
    return startIndex..<endIndex
  }

  func clone() -> Self {
    return self
  }
}

ProtocolExtensionTestSuite.test("Count") {
  expectEqual(4, ["a", "b", "c", "d"].myCount)
  expectEqual(4, ["a", "b", "c", "d"].clone().myCount)
}

extension Sequence {
  public func myEnumerated() -> EnumeratedSequence<Self> {
    return self.enumerated()
  }
}

ProtocolExtensionTestSuite.test("Enumerated") {
  var result: [(Int, String)] = []

  for (index, element) in ["a", "b", "c"].myEnumerated() {
    result.append((index, element))
  }

  expectTrue((0, "a") == result[0])
  expectTrue((1, "b") == result[1])
  expectTrue((2, "c") == result[2])
}

extension Sequence {
  public func myReduce<T>(
    _ initial: T, combine: (T, Self.Iterator.Element) -> T
  ) -> T {
    var result = initial
    for value in self {
      result = combine(result, value)
    }
    return result
  }
}

extension Sequence {
  public func myZip<S : Sequence>(_ s: S) -> Zip2Sequence<Self, S> {
    return zip(self, s)
  }
}

ProtocolExtensionTestSuite.test("Algorithms") {
  expectEqual(15, [1, 2, 3, 4, 5].myReduce(0, combine: { $0 + $1 }))

  var result: [(Int, String)] = []

  for (a, b) in [1, 2, 3].myZip(["a", "b", "c"]) {
    result.append((a, b))
  }

  expectTrue((1, "a") == result[0])
  expectTrue((2, "b") == result[1])
  expectTrue((3, "c") == result[2])
}

// Mutating algorithms.
extension MutableCollection
  where Self: RandomAccessCollection, Self.Iterator.Element : Comparable {

  public mutating func myPartition() -> Index {
    let first = self.first
    return self.partition(by: { $0 >= first! })
  }
}

extension RangeReplaceableCollection {
  public func myJoin<S : Sequence>(
    _ elements: S
  ) -> Self where S.Iterator.Element == Self {
    var result = Self()
    var iter = elements.makeIterator()
    if let first = iter.next() {
      result.append(contentsOf: first)
      while let next = iter.next() {
        result.append(contentsOf: self)
        result.append(contentsOf: next)
      }
    }
    return result
  }
}

ProtocolExtensionTestSuite.test("MutatingAlgorithms") {
  expectEqual("a,b,c", ",".myJoin(["a", "b", "c"]))
}

// Constrained extensions for specific types.
extension Collection where Self.Iterator.Element == String {
  var myCommaSeparatedList: String {
    if startIndex == endIndex { return "" }

    var result = ""
    var first = true
    for x in self {
      if first { first = false }
      else { result += ", " }
      result += x
    }
    return result
  }
}

ProtocolExtensionTestSuite.test("ConstrainedExtension") {
  expectEqual("x, y, z", ["x", "y", "z"].myCommaSeparatedList)
}

// Existentials
var runExistP1 = 0
var existP1_struct = 0
var existP1_class = 0

protocol ExistP1 {
  func existP1()
}

extension ExistP1 {
  func runExistP1() {
    main.runExistP1 += 1
    self.existP1()
  }
}

struct ExistP1_Struct : ExistP1 {
  func existP1() {
    existP1_struct += 1
  }
}

class ExistP1_Class : ExistP1 {
  func existP1() {
    existP1_class += 1
  }
}

ProtocolExtensionTestSuite.test("Existentials") {
  do {
    let existP1: ExistP1 = ExistP1_Struct()
    existP1.runExistP1()

    expectEqual(1, runExistP1)
    expectEqual(1, existP1_struct)
  }

  do {
    let existP1 = ExistP1_Class()
    existP1.runExistP1()

    expectEqual(2, runExistP1)
    expectEqual(1, existP1_class)
  }
}

protocol P {
  mutating func setValue(_ b: Bool)
  func getValue() -> Bool
}

extension P {
  var extValue: Bool {
    get { return getValue() }
    set(newValue) { setValue(newValue) }
  }
}

extension Bool : P {
  mutating func setValue(_ b: Bool) { self = b }
  func getValue() -> Bool { return self }
}

class C : P {
  var theValue: Bool = false
  func setValue(_ b: Bool) { theValue = b }
  func getValue() -> Bool { return theValue }
}

func toggle(_ value: inout Bool) {
  value = !value
}

ProtocolExtensionTestSuite.test("ExistentialToggle") {
  var p: P = true

  expectTrue(p.extValue)

  p.extValue = false
  expectFalse(p.extValue)

  toggle(&p.extValue)
  expectTrue(p.extValue)

  p = C()

  p.extValue = true
  expectTrue(p.extValue)

  p.extValue = false
  expectFalse(p.extValue)

  toggle(&p.extValue)
  expectTrue(p.extValue)
}

// Logical lvalues of existential type.
struct HasP {
  var _p: P
  var p: P {
    get { return _p }
    set { _p = newValue }
  }
}

ProtocolExtensionTestSuite.test("ExistentialLValue") {
  var hasP = HasP(_p: false)

  hasP.p.extValue = true
  expectTrue(hasP.p.extValue)

  toggle(&hasP.p.extValue)
  expectFalse(hasP.p.extValue)
}

var metatypes: [(Int, Any.Type)] = []

// rdar://problem/20739719
class Super: Init {
  required init(x: Int) {
    metatypes.append((x, type(of: self)))
  }
}

class Sub: Super {}

protocol Init { init(x: Int) }
extension Init { init() { self.init(x: 17) } }

ProtocolExtensionTestSuite.test("ClassInitializer") {
  _ = Super()

  _ = Sub()

  var sup: Super.Type = Super.self
  _ = sup.init()

  sup = Sub.self
  _ = sup.init()

  expectTrue(17 == metatypes[0].0)
  expectTrue(Super.self == metatypes[0].1)
  expectTrue(17 == metatypes[1].0)
  expectTrue(Sub.self == metatypes[1].1)
  expectTrue(17 == metatypes[2].0)
  expectTrue(Super.self == metatypes[2].1)
  expectTrue(17 == metatypes[3].0)
  expectTrue(Sub.self == metatypes[3].1)
}

// https://github.com/apple/swift/issues/43234

protocol SelfMetadataTest {
  associatedtype T = Int

  func staticTypeOfSelf() -> Any.Type
  func staticTypeOfSelfTakesT(_: T) -> Any.Type
  func staticTypeOfSelfCallsWitness() -> Any.Type
}

extension SelfMetadataTest {
  func staticTypeOfSelf() -> Any.Type {
    return Self.self
  }

  func staticTypeOfSelfTakesT(_: T) -> Any.Type {
    return Self.self
  }

  func staticTypeOfSelfNotAWitness() -> Any.Type {
    return Self.self
  }

  func staticTypeOfSelfCallsWitness() -> Any.Type {
    return staticTypeOfSelf()
  }
}

class SelfMetadataBase : SelfMetadataTest {}

class SelfMetadataDerived : SelfMetadataBase {}

class SelfMetadataGeneric<T> : SelfMetadataTest {}

func testSelfMetadata<T : SelfMetadataTest>(_ x: T, _ t: T.T) -> [Any.Type] {
  return [x.staticTypeOfSelf(),
          x.staticTypeOfSelfTakesT(t),
          x.staticTypeOfSelfNotAWitness(),
          x.staticTypeOfSelfCallsWitness()]
}

ProtocolExtensionTestSuite.test("WitnessSelf") {
  do {
    let result = testSelfMetadata(SelfMetadataBase(), 0)
    expectTrue(SelfMetadataBase.self == result[0])
    expectTrue(SelfMetadataBase.self == result[1])
    expectTrue(SelfMetadataBase.self == result[2])
    expectTrue(SelfMetadataBase.self == result[3])
  }

  do {
    let result = testSelfMetadata(SelfMetadataDerived() as SelfMetadataBase, 0)
    expectTrue(SelfMetadataBase.self == result[0])
    expectTrue(SelfMetadataBase.self == result[1])
    expectTrue(SelfMetadataBase.self == result[2])
    expectTrue(SelfMetadataBase.self == result[3])
  }

  // This is the interesting case -- make sure the static type of 'Self'
  // is correctly passed on from the call site to the extension method
  do {
    let result = testSelfMetadata(SelfMetadataDerived(), 0)
    expectTrue(SelfMetadataDerived.self == result[0])
    expectTrue(SelfMetadataBase.self == result[1])
    expectTrue(SelfMetadataDerived.self == result[2])
    expectTrue(SelfMetadataDerived.self == result[3])
  }

  // Make sure the calling convention works out if 'Self' is a generic
  // class too.
  do {
    let result = testSelfMetadata(SelfMetadataGeneric<Int>(), 0)
    expectTrue(SelfMetadataGeneric<Int>.self == result[0])
    expectTrue(SelfMetadataGeneric<Int>.self == result[1])
    expectTrue(SelfMetadataGeneric<Int>.self == result[2])
    expectTrue(SelfMetadataGeneric<Int>.self == result[3])
  }
}

@_marker protocol Addable {}
extension Addable {
    func increment(this x: Int) -> Int { return x + 100 }
}
extension String: Addable {}

ProtocolExtensionTestSuite.test("MarkerProtocolExtensions") {
    expectTrue("hello".increment(this: 11) == 111)
}

protocol DefaultArgumentsInExtension {
    func foo(a: Int, b: Int) -> (Int, Int)
}
extension DefaultArgumentsInExtension {
    func foo(a: Int, b: Int = 1) -> (Int, Int) {
        self.foo(a: a * 10, b: b * 10)
    }
}
struct DefaultArgumentsInExtensionImpl: DefaultArgumentsInExtension {
    func foo(a: Int, b: Int) -> (Int, Int) {
        (a * 2, b * 2)
    }
}

ProtocolExtensionTestSuite.test("DefaultArgumentsInExtension") {
    let instance = DefaultArgumentsInExtensionImpl()
    expectEqual((4, 6), instance.foo(a: 2, b: 3))
    expectEqual((4, 6), (instance as any DefaultArgumentsInExtension).foo(a: 2, b: 3))
    expectEqual((40, 20), instance.foo(a: 2))
    expectEqual((40, 20), (instance as any DefaultArgumentsInExtension).foo(a: 2))
}
