// RUN: %target-run-simple-swift | %FileCheck %s
// REQUIRES: executable_test

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
func opaqueEquatableValue() -> some Equatable {
  (1, 2, 3, 4, 5)
}

if #available(iOS 13, macOS 10.15, tvOS 13, watchOS 6, *) {
  // CHECK: true
  print(opaqueEquatableValue() == opaqueEquatableValue())
}

struct Wrapper<T> {
  let value: T
}

extension Wrapper: Equatable where T: Equatable {}

// CHECK: true
print(Wrapper(value: ()) == Wrapper(value: ()))

// CHECK: true
print(Wrapper(value: (128, 316)) == Wrapper(value: (128, 316)))

// CHECK: false
print(Wrapper(value: (128, 316)) == Wrapper(value: (316, 128)))

func use<T: Equatable>(_ thing: T) -> Bool {
  equals((thing, thing), (thing, thing))
}

// CHECK: true
print(use(128))

class Foo: Equatable {
  var age: Int

  init(age: Int) {
    self.age = age
  }

  static func == (lhs: Foo, rhs: Foo) -> Bool {
    lhs.age == rhs.age
  }
}

// CHECK: true
print(equals((Foo(age: 128), false, 0), (Foo(age: 128), false, 0)))

// CHECK: false
print(equals((Foo(age: 128), false, 0), (Foo(age: 316), false, 0)))
