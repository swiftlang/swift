public class Base {
  public init() {}
}

public protocol BaseProto {
  func method()
}

extension Base : BaseProto {
  public func method() {}
}

// Make sure we can serialize witness substitutions where replacement types
// involve generic parameters.
public class GenericWitness<T> : BaseProto {
  public func method() {}
}
