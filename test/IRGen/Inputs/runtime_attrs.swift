@runtimeMetadata
public struct Ignore<T> {
  public init(attachedTo: T) {}
}

@runtimeMetadata
public struct TestAmbiguity {
  public init<Args, Result>(attachedTo: (Args) -> Result) {}
}

@Ignore
public protocol Ignored {
}
