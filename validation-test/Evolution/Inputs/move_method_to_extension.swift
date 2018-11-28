public struct Rain {
  public init() {}

#if BEFORE
  public func doIt() {}
#endif
}

public class Snow {
  public init() {}

#if BEFORE
  public final func doIt() {}
#endif
}

#if AFTER
extension Rain {
  public func doIt() {}
}

extension Snow {
  public func doIt() {}
}
#endif
