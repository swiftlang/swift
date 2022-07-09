import RoundTrip

struct Concrete {}

extension Concrete {
  struct Nested {}
}

struct Generic<T> {}

protocol Proto {}

struct Foo : Proto {}

class Bar {}

extension Generic where T : Proto {
  struct Nested1 {}
}

extension Generic where T == Int {
  struct Nested2 {}
}

extension Generic where T: AnyObject {
  struct NestedViaAnyObject {}
}

public func test() {
  roundTripType(Concrete.Nested.self)
  roundTripType(Generic<Foo>.Nested1.self)
  roundTripType(Generic<Int>.Nested2.self)
  roundTripType(Generic<Bar>.NestedViaAnyObject.self)
}
