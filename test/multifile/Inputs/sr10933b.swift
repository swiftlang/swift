@propertyWrapper
final class IntWrapper: Wrapper<Int> {
  override var wrappedValue: Int {
    get { super.wrappedValue }
    set { super.wrappedValue = newValue }
  }
}

