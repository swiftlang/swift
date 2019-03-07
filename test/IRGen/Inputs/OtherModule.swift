import resilient_struct

public struct First {}
public struct Second {
    public let resilientData: Size
}

private enum PrivateEnum {
  case first(First?)
  case second(Second?)
}

public struct Foo {
    private var _property = PrivateEnum.first(nil)
}

internal enum InternalEnum {
  case first(First?)
  case second(Second?)
}

public struct Bar {
    private var _property = InternalEnum.first(nil)
}
