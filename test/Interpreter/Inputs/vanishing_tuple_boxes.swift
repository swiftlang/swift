// Compiled with library evolution, so its layout is opaque to clients. A
// client that stores a `Box<T>` must instantiate the box's metadata at
// runtime, which forces the enclosing type's metadata completion function to
// bind its metadata pack before the tuple field is laid out.
public struct Box<Value> {
  public var value: Value
  public init(_ value: Value) { self.value = value }
}
