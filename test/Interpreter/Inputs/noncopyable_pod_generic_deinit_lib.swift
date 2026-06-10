public struct SomePOD {
  public var x: Int = 0
  public init() {}
}

// ~Copyable type which is POD according to its fields as long as T is also POD
public struct NCWrapper<T: ~Copyable>: ~Copyable {
  var inner: T
  public init(_ t: consuming T) { inner = t }
  deinit {
    print("Container.deinit fired")
  }
}
