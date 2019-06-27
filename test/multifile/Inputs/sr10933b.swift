@propertyWrapper
final class IntWrapper: Wrapper<Int> {
  override var value: Int {
    get { super.value }
    set { super.value = newValue }
  }
}

