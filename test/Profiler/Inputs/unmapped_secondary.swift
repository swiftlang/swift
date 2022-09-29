// A secondary file for unmapped.swift that can contain profiled code.

@propertyWrapper
struct Wrapper<T> {
  var wrappedValue: T
}

struct Projected<T> {
  var value: T
}

@propertyWrapper
struct WrapperWithProjectedValue<T> {
  var wrappedValue: T
  init(projectedValue: Projected<T>) {
    self.wrappedValue = projectedValue.value
  }
  var projectedValue: Projected<T> {
    Projected(value: wrappedValue)
  }
}

