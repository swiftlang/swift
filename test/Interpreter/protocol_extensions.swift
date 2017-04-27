// RUN: %target-run-simple-swift | %FileCheck %s
// REQUIRES: executable_test

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

// CHECK: 4
print(["a", "b", "c", "d"].myCount)

// Extend a protocol with a function.
extension Collection {
  var myIndices: Range<Index> {
    return startIndex..<endIndex
  }

  func clone() -> Self {
    return self
  }
}

// CHECK: 4
print(["a", "b", "c", "d"].clone().myCount)

extension Sequence {
  public func myEnumerated() -> EnumeratedSequence<Self> {
    return self.enumerated()
  }
}

// CHECK: (0, a)
// CHECK-NEXT: (1, b)
// CHECK-NEXT: (2, c)
for (index, element) in ["a", "b", "c"].myEnumerated() {
  print("(\(index), \(element))")
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

// CHECK: 15
print([1, 2, 3, 4, 5].myReduce(0, combine: { $0 + $1 }))


extension Sequence {
  public func myZip<S : Sequence>(_ s: S) -> Zip2Sequence<Self, S> {
    return Zip2Sequence(_sequence1: self, _sequence2: s)
  }
}

// CHECK: (1, a)
// CHECK-NEXT: (2, b)
// CHECK-NEXT: (3, c)
for (a, b) in [1, 2, 3].myZip(["a", "b", "c"]) {
  print("(\(a), \(b))")
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

// CHECK: a,b,c
print(
  String(
    ",".characters.myJoin(["a".characters, "b".characters, "c".characters])
  )
)

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

// CHECK: x, y, z
print(["x", "y", "z"].myCommaSeparatedList)

// CHECK: {{[tuv], [tuv], [tuv]}}
print((["t", "u", "v"] as Set).myCommaSeparatedList)

// Existentials
protocol ExistP1 {
  func existP1()
}

extension ExistP1 {
  func runExistP1() {
    print("runExistP1")
    self.existP1()
  }
}

struct ExistP1_Struct : ExistP1 {
  func existP1() {
    print("  - ExistP1_Struct")
  }
}

class ExistP1_Class : ExistP1 {
  func existP1() {
    print("  - ExistP1_Class")
  }
}

// CHECK: runExistP1
// CHECK-NEXT: - ExistP1_Struct
var existP1: ExistP1 = ExistP1_Struct()
existP1.runExistP1()

// CHECK: runExistP1
// CHECK-NEXT: - ExistP1_Class
existP1 = ExistP1_Class()
existP1.runExistP1()

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

var p: P = true
// CHECK: Bool
print("Bool")

// CHECK: true
p.extValue = true
print(p.extValue)

// CHECK: false
p.extValue = false
print(p.extValue)

// CHECK: true
toggle(&p.extValue)
print(p.extValue)

// CHECK: C
print("C")
p = C()

// CHECK: true
p.extValue = true
print(p.extValue)

// CHECK: false
p.extValue = false
print(p.extValue)

// CHECK: true
toggle(&p.extValue)
print(p.extValue)

// Logical lvalues of existential type.
struct HasP {
  var _p: P
  var p: P {
    get { return _p }
    set { _p = newValue }
  }
}

var hasP = HasP(_p: false)

// CHECK: true
hasP.p.extValue = true
print(hasP.p.extValue)

// CHECK: false
toggle(&hasP.p.extValue)
print(hasP.p.extValue)

// rdar://problem/20739719
class Super: Init {
  required init(x: Int) { print("\(x) \(type(of: self))") }
}

class Sub: Super {}

protocol Init { init(x: Int) }
extension Init { init() { self.init(x: 17) } }

// CHECK: 17 Super
_ = Super()

// CHECK: 17 Sub
_ = Sub()

// CHECK: 17 Super
var sup: Super.Type = Super.self
_ = sup.init()

// CHECK: 17 Sub
sup = Sub.self
_ = sup.init()

// CHECK: DONE
print("DONE")
