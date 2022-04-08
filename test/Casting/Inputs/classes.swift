
public protocol P : AnyObject { }

public class Base {
  public init() {}
}

public class Nonfinal : Base, P {
  public override init() {}
}

final public class Final : Base, P {
  public override init() {}
}

open class OpenBase {
  public init() {}
}
