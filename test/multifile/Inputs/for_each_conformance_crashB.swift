protocol _P { }
protocol P : _P { }

protocol Q {
  associatedtype A: P

  func getArray() -> [RequiresP<A>]
}

struct RequiresP<T: P> { }

struct MyStruct: P { }
