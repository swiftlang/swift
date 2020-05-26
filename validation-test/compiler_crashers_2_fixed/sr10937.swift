// RUN: %target-swift-frontend -emit-sil %s

@propertyWrapper
struct Foo<T> {
  init(wrappedValue: T) {
    self.wrappedValue = wrappedValue
  }

  var wrappedValue: T
}

@propertyWrapper
struct Bar<T> {
  init(wrappedValue: T) {
    self.wrappedValue = wrappedValue
  }

  var wrappedValue: T
}

struct Container {
  @Foo @Foo var x: Int = 0
  @Foo @Foo @Bar @Bar var y: Int = 1
  @Foo @Bar @Foo @Foo var z: Int = 2
}

_ = Container()
