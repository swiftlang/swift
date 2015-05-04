// Do not put any classes in this file. It's part of the test that no classes
// get serialized here.

enum TheEnum {
  case A, B, C(MyClass)
}

enum EquatableEnum {
  case A
}


@inline(never)
func useEquatable<T: Equatable>(_: T) {}

public func hasLocal() {
  enum LocalEnum : Int {
    case A, B
  }

  struct Wrapper {
    enum LocalEnum : Int {
      case A, B
    }
  }

  // Make sure we can handle the == of local enums.
  useEquatable(LocalEnum.A)
  useEquatable(Wrapper.LocalEnum.A)
}
