// RUN: %target-swift-frontend -c %s

@propertyWrapper
class OneOf<Value: Equatable> {
  var wrappedValue: Value {
    get { value }
    set { storeIfAllowed(newValue) }
  }
  
  private var value: Value
  
  private let allowedValues: [Value]
  
  init(wrappedValue value: Value, _ allowedValues: Value...) {
    precondition(allowedValues.contains(value))
    self.value = value
    self.allowedValues = allowedValues
  }
  
  private func storeIfAllowed(_ value: Value) {
    guard allowedValues.contains(value) else {
      return
    }
    
    self.value = value
  }
}

struct Test {
  @OneOf(4, 8, 15, 16, 23, 42) private var numbers: Int = 4
}
func test() {
  _ = Test()
}
