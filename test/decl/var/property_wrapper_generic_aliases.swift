// RUN: %target-typecheck-verify-swift

// https://github.com/apple/swift/issues/56522

@propertyWrapper
struct SomeLongFooName<Holder, T> {
  var wrappedValue: T
  
  init(wrappedValue: T) {
    self.wrappedValue = wrappedValue
  }
}


struct Bar {
  typealias Foo<T> = SomeLongFooName<Self, T>
  
  @Foo var baz: Bool = false
}
