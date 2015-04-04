// RUN: %target-run-simple-swift | FileCheck %s

// Extend a protocol with a property.
extension SequenceType {
  final var myCount: Int {
    var result = 0
    for x in self {
      ++result
    }
    return result
  }
}

// CHECK: 4
println(["a", "b", "c", "d"].myCount)

// Extend a protocol with a function.
extension CollectionType {
  final var myIndices: Range<Index> {
    return Range(start: startIndex, end: endIndex)
  }
}

extension CollectionType {
  final func indexMatching(fn: Generator.Element -> Bool) -> Index? {
    for i in myIndices {
      if fn(self[i]) { return i }
    }
    return nil
  }
}

// CHECK: 2
println(["a", "b", "c", "d"].indexMatching({$0 == "c"})!)

// Extend certain instances of a collection (those that have equatable
// element types) with another algorithm.
extension CollectionType where Self.Generator.Element : Equatable {
  final func myIndexOf(element: Generator.Element) -> Index? {
    for i in indices(self) {
      if self[i] == element { return i }
    }

    return nil
  }
}

// CHECK: 3
println(["a", "b", "c", "d", "e"].myIndexOf("d")!)

extension SequenceType {
  final public func myEnumerate() -> EnumerateSequence<Self> { 
    return EnumerateSequence(self)
  }
}

// CHECK: (0, a)
// CHECK-NEXT: (1, b)
// CHECK-NEXT: (2, c)
for (index, element) in ["a", "b", "c"].myEnumerate() {
  println("(\(index), \(element))")
}

extension SequenceType {
  final public func myReduce<T>(
    initial: T, @noescape combine: (T, Self.Generator.Element) -> T
  ) -> T { 
    var result = initial
    for value in self {
      result = combine(result, value)
    }
    return result
  }
}

// CHECK: 15
println([1, 2, 3, 4, 5].myReduce(0, combine: +))


extension SequenceType {
  final public func myZip<S : SequenceType>(s: S) -> Zip2<Self, S> {
    return Zip2(self, s)
  }
}

// CHECK: (1, a)
// CHECK-NEXT: (2, b)
// CHECK-NEXT: (3, c)
for (a, b) in [1, 2, 3].myZip(["a", "b", "c"]) {
  println("(\(a), \(b))")
}

// Mutating algorithms.
extension MutableCollectionType
  where Self.Index: RandomAccessIndexType, Self.Generator.Element : Comparable {

  public final mutating func myPartition(range: Range<Index>) -> Index {
    return Swift.partition(&self, range)
  }
}

// CHECK: 4 3 1 2 | 5 9 8 6 7 6
var evenOdd = [5, 3, 6, 2, 4, 9, 8, 1, 7, 6]
var evenOddSplit = evenOdd.myPartition(evenOdd.myIndices)
for i in evenOdd.myIndices {
  if i == evenOddSplit { print(" |") }
  if i > 0 { print(" ") }
  print(evenOdd[i])
}
println()

extension ExtensibleCollectionType {
  public final func myJoin<S : SequenceType where S.Generator.Element == Self>(
    elements: S
  ) -> Self {
    return Swift.join(self, elements)
  }
}

// CHECK: a,b,c
println(",".myJoin(["a", "b", "c"]))

// Constrained extensions for specific types.
extension CollectionType where Self.Generator.Element == String {
  final var myCommaSeparatedList: String {
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
println(["x", "y", "z"].myCommaSeparatedList)

// CHECK: {{[tuv], [tuv], [tuv]}}
println((["t", "u", "v"] as Set).myCommaSeparatedList)

// Existentials
protocol ExistP1 {
  func existP1()
}

extension ExistP1 {
  final func runExistP1() {
    println("runExistP1")
    self.existP1()
  }
}

struct ExistP1_Struct : ExistP1 {
  func existP1() {
    println("  - ExistP1_Struct")
  }
}

class ExistP1_Class : ExistP1 {
  func existP1() {
    println("  - ExistP1_Class")
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


// CHECK: DONE
println("DONE")
