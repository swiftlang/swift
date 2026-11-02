// RUN: %target-run-simple-swift | %FileCheck %s
// RUN: %target-run-simple-swift(-O) | %FileCheck %s
// RUN: %target-run-simple-swift(-Osize) | %FileCheck %s

// REQUIRES: executable_test

// https://github.com/swiftlang/swift/issues/93344

struct Empty {}

enum Unit: String {
  case only = "only"
}

@inline(never)
func insertVoid() -> Int {
  var array: [()] = []
  array.insert((), at: 0)
  return array.count
}

@inline(never)
func insertEmpty() -> Int {
  var array: ContiguousArray<Empty> = []
  for _ in 0 ..< 13 {
    array.insert(Empty(), at: array.endIndex)
  }
  return array.count
}

@inline(never)
func insertGeneric<T>(_ element: T) -> Int {
  var array: [T] = []
  array.insert(element, at: 0)
  return array.count
}

@inline(never)
func replaceWithOne() -> Int {
  var array: [Unit] = []
  array.replaceSubrange(0 ..< 0, with: CollectionOfOne(.only))
  array.insert(contentsOf: CollectionOfOne(.only), at: 0)
  return array.count
}

@inline(never)
func initializeFromOne() -> Int {
  let buffer: UnsafeMutableBufferPointer<Empty> = .allocate(capacity: 1)
  defer { buffer.deallocate() }
  return buffer.initialize(fromContentsOf: CollectionOfOne(Empty()))
}

// CHECK: 1
print(insertVoid())
// CHECK-NEXT: 13
print(insertEmpty())
// CHECK-NEXT: 1
print(insertGeneric(Empty()))
// CHECK-NEXT: 2
print(replaceWithOne())
// CHECK-NEXT: 1
print(initializeFromOne())
