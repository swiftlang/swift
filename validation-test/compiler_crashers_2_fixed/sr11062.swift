// RUN: %target-swift-frontend -emit-ir %s

struct MyText<V> {
  let label: String
  let value: V
}

extension MyText where V == Void {
  init(_ label: String, defaulted: Int = 17) {
    self.label = label
    self.value = ()
  }
}

struct ImagePulse {
  @Inspectable(control: { MyText("duration") })
  var test: Double = 0
}

@propertyWrapper
final class Inspectable<Value> {
  var wrappedValue: Value
  
  init<V>(wrappedValue initialValue: Value, control: @escaping () -> V) {
    self.wrappedValue = initialValue
  }
}
