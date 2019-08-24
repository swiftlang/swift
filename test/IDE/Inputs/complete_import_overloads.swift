
public struct HasFooGeneric<T> {
  public var foo: Int = 0
}

extension HasFooGeneric {
  public var bar: Int { return 0 }
}

public class HasFooNonGeneric {
  public var foo: Int = 0
}

extension HasFooNonGeneric {
  public var bar: Int { return 0 }
}
