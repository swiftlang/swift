// RUN: %empty-directory(%t)
// RUN: %target-build-swift -swift-version 5 -Xfrontend -enable-experimental-named-opaque-types %s -o %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: executable_test

@available(iOS 13, macOS 10.15, tvOS 13, watchOS 6, *)
dynamic func lazyMapCollection<C: Collection, T>(_ collection: C, body: @escaping (C.Element) -> T)
    -> <R: Collection where R.Element == T> R {
  return collection.lazy.map { body($0) }
}

@available(iOS 13, macOS 10.15, tvOS 13, watchOS 6, *)
dynamic func lazyMapCollection2<C: Collection, T>(_ collection: C, body: @escaping (C.Element) -> T)
    -> <R: Collection where R.Element == T> R {
  collection.lazy.map { body($0) }
}

@available(iOS 13, macOS 10.15, tvOS 13, watchOS 6, *)
dynamic func unzipCollection<C: Collection, T, U: Hashable>(_ collection: C)
-> <R1: Collection, R2: Collection where R1.Element == T, R2.Element == U> (R1, R2) where C.Element == (T, U) {
  return (Array(collection.map { $0.0 }), Set(collection.map { $0.1 }))
}

protocol P {
  associatedtype A
  func f() -> A
}

@available(iOS 13, macOS 10.15, tvOS 13, watchOS 6, *)
struct X<T, U: Hashable>: P {
  var data: [(T, U)]

  func f() -> <
      R1: Collection, R2: Collection where R1.Element == T, R2.Element == U
  > (R1, R2) {
    // FIXME: Use unzipCollection here
    return (Array(data.map { $0.0 }), Set(data.map { $0.1 }))
  }
}

func getP_A<T: P>(_: T.Type) -> Any.Type {
  return T.A.self
}

if #available(iOS 13, macOS 10.15, tvOS 13, watchOS 6, *) {
  // CHECK: {{[1.0, 2.0, 3.0]|too old}}
  let array = [1, 2, 3]
  let x = lazyMapCollection(array) { Double($0) }
  print(x)

  // CHECK: {{[0.5, 1.0, 1.5]|too old}}
  let x2 = lazyMapCollection2(array) { Double($0) / 2.0 }
  print(x2)

  let array2 = [(1, "Hello"), (2, "World"), (3, "!")]
  let x3 = unzipCollection(array2)

  // CHECK: {{[1, 2, 3]|too old}}
  print(x3.0)

  // CHECK: {{["Hello", "World", "!"]|too old}}
  print(x3.1)

  // CHECK: {{(Array<Int>, Set<String>)|too old}}
  let paType = getP_A(X<Int, String>.self)
  print(paType)
  typealias ExpectedPAType = ([Int], Set<String>)
  assert(paType == ExpectedPAType.self)
} else {
  print("too old")
  print("too old")
  print("too old")
  print("too old")
  print("too old")
}
