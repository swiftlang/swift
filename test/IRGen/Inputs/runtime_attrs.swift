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

@runtimeMetadata
public enum EnumFlag<B, V> {
case type(B.Type)
case method((B) -> V)
case property(KeyPath<B, V>)
case function(() -> V)
}

extension EnumFlag {
  public init(attachedTo: KeyPath<B, V>) { self = .property(attachedTo) }
  public init(attachedTo: @escaping (B) -> V) { self = .method(attachedTo) }
}

extension EnumFlag where V == Void {
  public init(attachedTo: B.Type) { self = .type(attachedTo) }
}

extension EnumFlag where B == Void {
  public init(attachedTo: @escaping () -> V) { self = .function(attachedTo) }
}
