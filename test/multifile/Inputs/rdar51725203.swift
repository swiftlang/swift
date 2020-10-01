@propertyWrapper
struct Wrapper<Value> {
  var wrappedValue: Value

  var projectedValue: Wrapper<Value> {
    get { self }
    set { self = newValue }
  }

  init(wrappedValue initialValue: Value) {
    wrappedValue = initialValue
  }
}

struct StructModel {
  @Wrapper var foo: Int
  @Wrapper var bar: Int // expected-note{{'_bar' declared here}}
}

class ClassModel {
  @Wrapper var foo = 17
  @Wrapper var bar = 17 // expected-note{{'_bar' declared here}}
}
