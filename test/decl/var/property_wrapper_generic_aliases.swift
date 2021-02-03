// RUN: %target-typecheck-verify-swift

// https://bugs.swift.org/browse/SR-14143

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
