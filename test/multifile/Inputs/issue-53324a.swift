@propertyWrapper
class Wrapper<T> {
  private var _value: T
  
  var wrappedValue: T {
    get { _value }
    set { _value = newValue }
  }
  
  init(defaultValue: T) {
    self._value = defaultValue
  }
}
