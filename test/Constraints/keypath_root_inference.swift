// RUN: %target-typecheck-verify-swift -swift-version 5
// RUN: %target-typecheck-verify-swift -swift-version 6

// Test case for transitive key path root inference vs. keypath to function
// conversion
protocol Wrapper {
  associatedtype Value
}

struct _MapWrapper<T, U>: Wrapper {
  typealias Value = U
}

extension Wrapper {
  func map<T>(_: (Value) -> T) -> _MapWrapper<Self, T> {
    fatalError()
  }
}

extension Optional: Wrapper where Wrapped: Wrapper {
  typealias Value = Wrapped.Value
}

protocol Shape {}

struct Empty: Shape {}

struct Polygon {
  init(_: Int) {}

  var shape: any Shape {
    fatalError()
  }
}

func test_transitive_key_path_root_inference1(x: Int?) -> (any Shape)? {
  return x.map(Polygon.init).map(\.shape)
}

func test_transitive_key_path_root_inference2(x: Int?) -> any Shape {
  let shape = x.map(Polygon.init).map(\.shape) ?? Empty()
  return shape
}

// More root inference edge cases
func test_transitive_key_path_root_inference3() {
  @propertyWrapper
  class Wrapped<T> {
    var wrappedValue: T
    var projectedValue: Wrapped<T> { fatalError() }

    init(wrappedValue: T) { fatalError() }
  }

  struct Wrapper {
    @Wrapped var value: Int
  }

  struct Unwrapper<T> {
    func f1<U>(_: KeyPath<T, U>) {}
    func f2<U, V>(_: V) where V: KeyPath<T, U> {}
    func f3<U>(_: KeyPath<T, Wrapped<U>>) {}
    func f4<U, V>(_: V) where V: KeyPath<T, Wrapped<U>> {}
  }

  func id<T>(_: T) -> T { fatalError() }

  Unwrapper<Wrapper>().f1(\.value)
  Unwrapper<Wrapper>().f1(id(\.value))
  Unwrapper<Wrapper>().f1(id(id(\.value)))

  Unwrapper<Wrapper>().f2(\.value)
  Unwrapper<Wrapper>().f2(id(\.value))
  Unwrapper<Wrapper>().f2(id(id(\.value)))

  Unwrapper<Wrapper>().f3(\.$value)
  Unwrapper<Wrapper>().f3(id(\.$value))
  Unwrapper<Wrapper>().f3(id(id(\.$value)))

  Unwrapper<Wrapper>().f4(\.$value)
  Unwrapper<Wrapper>().f4(id(\.$value))
  Unwrapper<Wrapper>().f4(id(id(\.$value)))
}

func test_transitive_key_path_root_inference4() {
  class C {}

  class G<T, U> {}

  func fn<Input, Output, KeyPath: WritableKeyPath<Container, G<Input, Output>?>>(_: KeyPath, _: (Input) -> Output) {}

  struct Container {
    var value: G<C, Void>? = nil
  }

  func test(_ block: (C) -> Void) {
    fn(\.value, block)
  }
}