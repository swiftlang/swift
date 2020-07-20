// RUN: %target-run-simple-swift | %FileCheck %s
// REQUIRES: executable_test

struct Wrapper<T> {
  let value: T
}

extension Wrapper: Equatable where T: Equatable {}
extension Wrapper: Hashable where T: Hashable {}

extension Wrapper: Comparable where T: Comparable {
  static func <(lhs: Wrapper, rhs: Wrapper) -> Bool {
    lhs.value < rhs.value
  }
}

class Foo {
  var age: Int

  init(age: Int) {
    self.age = age
  }
}

extension Foo: Equatable {
  static func ==(lhs: Foo, rhs: Foo) -> Bool {
    lhs.age == rhs.age
  }
}

extension Foo: Comparable {
  static func <(lhs: Foo, rhs: Foo) -> Bool {
    lhs.age < rhs.age
  }
}

extension Foo: Hashable {
  func hash(into hasher: inout Hasher) {
    hasher.combine(age)
  }
}

//===----------------------------------------------------------------------===//
// Tuple Equatable Conformance
//===----------------------------------------------------------------------===//

func equals<T: Equatable>(_ lhs: T, _ rhs: T) -> Bool {
  lhs == rhs
}

// CHECK: true
print(equals((), ()))

// CHECK: true
print(equals((128, 316), (128, 316)))

// CHECK: false
print(equals((128, 316), (316, 128)))

// CHECK: true
print(equals(((1, 2), 3), ((1, 2), 3)))

// CHECK: false
print(equals(((1, 2), 3), ((1, 2), 4)))

@available(iOS 13, macOS 10.15, tvOS 13, watchOS 6, *)
func opaqueTupleEquatableValue() -> some Equatable {
  (1, 2, 3, 4, 5)
}

if #available(iOS 13, macOS 10.15, tvOS 13, watchOS 6, *) {
  _ = opaqueTupleEquatableValue() == opaqueTupleEquatableValue()
}

// CHECK: true
print(Wrapper(value: ()) == Wrapper(value: ()))

// CHECK: true
print(Wrapper(value: (128, 316)) == Wrapper(value: (128, 316)))

// CHECK: false
print(Wrapper(value: (128, 316)) == Wrapper(value: (316, 128)))

func useEquatable<T: Equatable>(_ thing: T) -> Bool {
  equals((thing, thing), (thing, thing))
}

// CHECK: true
print(useEquatable(128))

// CHECK: true
print(equals((Foo(age: 128), false, 0), (Foo(age: 128), false, 0)))

// CHECK: false
print(equals((Foo(age: 128), false, 0), (Foo(age: 316), false, 0)))

//===----------------------------------------------------------------------===//
// Tuple Comparable Conformance
//===----------------------------------------------------------------------===//

func compareLT<T: Comparable>(_ lhs: T, _ rhs: T) -> Bool {
  lhs < rhs
}

func compareLTE<T: Comparable>(_ lhs: T, _ rhs: T) -> Bool {
  lhs <= rhs
}

func compareGTE<T: Comparable>(_ lhs: T, _ rhs: T) -> Bool {
  lhs >= rhs
}

func compareGT<T: Comparable>(_ lhs: T, _ rhs: T) -> Bool {
  lhs > rhs
}

// false
print(compareLT((), ()))
// true
print(compareLTE((), ()))
// true
print(compareGTE((), ()))
// false
print(compareGT((), ()))

// false
print(compareLT((1, 2), (1, 2)))
// true
print(compareLTE((1, 2), (1, 2)))
// true
print(compareGTE((1, 2), (1, 2)))
// false
print(compareGT((1, 2), (1, 2)))

// true
print(compareLT((1, 2), (2, 1)))
// true
print(compareLTE((1, 2), (2, 1)))
// false
print(compareGTE((1, 2), (2, 1)))
// false
print(compareGT((1, 2), (2, 1)))

// false
print(compareLT(((1, 2), 3), ((1, 2), 3)))
// true
print(compareLTE(((1, 2), 3), ((1, 2), 3)))
// true
print(compareGTE(((1, 2), 3), ((1, 2), 3)))
// false
print(compareGT(((1, 2), 3), ((1, 2), 3)))

// true
print(compareLT(((1, 2), 3), ((1, 2), 4)))
// true
print(compareLTE(((1, 2), 3), ((1, 2), 4)))
// false
print(compareGTE(((1, 2), 3), ((1, 2), 4)))
// false
print(compareGT(((1, 2), 3), ((1, 2), 4)))

@available(iOS 13, macOS 10.15, tvOS 13, watchOS 6, *)
func opaqueTupleComparableValue() -> some Comparable {
  (1, 2, 3, 4, 5)
}

if #available(iOS 13, macOS 10.15, tvOS 13, watchOS 6, *) {
  _ = opaqueTupleComparableValue() < opaqueTupleComparableValue()
  _ = opaqueTupleComparableValue() <= opaqueTupleComparableValue()
  _ = opaqueTupleComparableValue() >= opaqueTupleComparableValue()
  _ = opaqueTupleComparableValue() > opaqueTupleComparableValue()
}

// CHECK: false
print(Wrapper(value: ()) < Wrapper(value: ()))
// CHECK: true
print(Wrapper(value: ()) <= Wrapper(value: ()))
// CHECK: true
print(Wrapper(value: ()) >= Wrapper(value: ()))
// CHECK: false
print(Wrapper(value: ()) > Wrapper(value: ()))

// CHECK: false
print(Wrapper(value: (128, 316)) < Wrapper(value: (128, 316)))
// CHECK: true
print(Wrapper(value: (128, 316)) <= Wrapper(value: (128, 316)))
// CHECK: true
print(Wrapper(value: (128, 316)) >= Wrapper(value: (128, 316)))
// CHECK: false
print(Wrapper(value: (128, 316)) > Wrapper(value: (128, 316)))

// CHECK: true
print(Wrapper(value: (128, 316)) < Wrapper(value: (316, 128)))
// CHECK: true
print(Wrapper(value: (128, 316)) <= Wrapper(value: (316, 128)))
// CHECK: false
print(Wrapper(value: (128, 316)) >= Wrapper(value: (316, 128)))
// CHECK: false
print(Wrapper(value: (128, 316)) > Wrapper(value: (316, 128)))

func useComparable<T: Comparable>(_ thing: T) -> Bool {
  compareLT((thing, thing), (thing, thing))
}

// CHECK: false
print(useComparable(128))

// CHECK: false
print(compareLT((Foo(age: 128), 0), (Foo(age: 128), 0)))
// CHECK: true
print(compareLTE((Foo(age: 128), 0), (Foo(age: 128), 0)))
// CHECK: true
print(compareGTE((Foo(age: 128), 0), (Foo(age: 128), 0)))
// CHECK: false
print(compareGT((Foo(age: 128), 0), (Foo(age: 128), 0)))

// CHECK: true
print(compareLT((Foo(age: 128), 0), (Foo(age: 734), 0)))
// CHECK: true
print(compareLTE((Foo(age: 128), 0), (Foo(age: 734), 0)))
// CHECK: false
print(compareGTE((Foo(age: 128), 0), (Foo(age: 734), 0)))
// CHECK: false
print(compareGT((Foo(age: 128), 0), (Foo(age: 734), 0)))

//===----------------------------------------------------------------------===//
// Tuple Hashable Conformance
//===----------------------------------------------------------------------===//

var grid = [(x: Int, y: Int): Int]()

grid[(x: 0, y: 0)] = 0
// CHECK: 0
print(grid[(x: 0, y: 0)]!)
// CHECK: 0
print(grid[(0, 0)]!)

grid[(x: 1, y: 1)] = 1
// CHECK: 1
print(grid[(x: 1, y: 1)]!)
// CHECK: 1
print(grid[(1, 1)]!)

let tupleSet: Set = [(x: 0, y: 1), (x: 128, y: 32), (x: 10, y: 0)]

// CHECK: true
print(tupleSet.contains((x: 0, y: 1)))

// CHECK: true
print(tupleSet.contains((0, 1)))

func compareHashes<T: Hashable>(_ lhs: T, _ rhs: T) -> Bool {
  lhs.hashValue == rhs.hashValue
}

// CHECK: true
print(compareHashes((), ()))

// CHECK: true
print(compareHashes((1, 2), (1, 2)))

// CHECK: false
print(compareHashes((1, 2), (1, 3)))

// CHECK: false
print(compareHashes((1, 2), (2, 1)))

// CHECK: true
print(compareHashes(((1, 2), 3), ((1, 2), 3)))

// CHECK: false
print(compareHashes(((1, 2), 3), ((1, 2), 4)))

// CHECK: false
print(compareHashes(((1, 2), 3), ((3, 2), 1)))

@available(iOS 13, macOS 10.15, tvOS 13, watchOS 6, *)
func opaqueTupleHashableValue() -> some Hashable {
  (1, 2, 3, 4, 5)
}

if #available(iOS 13, macOS 10.15, tvOS 13, watchOS 6, *) {
  _ = opaqueTupleHashableValue().hashValue == opaqueTupleHashableValue().hashValue
}

// CHECK: true
print(compareHashes(Wrapper(value: ()), Wrapper(value: ())))

// CHECK: true
print(compareHashes(Wrapper(value: (1, 2)), Wrapper(value: (1, 2))))

// CHECK: false
print(compareHashes(Wrapper(value: (1, 2)), Wrapper(value: (1, 3))))

// CHECK: false
print(compareHashes(Wrapper(value: (1, 2)), Wrapper(value: (2, 1))))

func useHashable<T: Hashable>(_ thing: T) -> Bool {
  compareHashes((thing, thing), (thing, thing))
}

// CHECK: true
print(useHashable(128))

// CHECK: true
print(compareHashes((Foo(age: 128), 1), (Foo(age: 128), 1)))

// CHECK: false
print(compareHashes((Foo(age: 128), 1), (Foo(age: 0), 1)))
