// Do not put any protocols in this file. It's part of the test that no
// protocols get serialized here.

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

class Base {
  class var conflict: Int { return 0 }
  var conflict: Int { return 1 }
}
