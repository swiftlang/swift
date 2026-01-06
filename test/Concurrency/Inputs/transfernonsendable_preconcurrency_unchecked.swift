
public class NonSendableKlass {
  public init() {}
}

public class ExplicitlyNonSendableKlass {
  public init() {}
}

@available(*, unavailable)
extension ExplicitlyNonSendableKlass: Sendable {}
