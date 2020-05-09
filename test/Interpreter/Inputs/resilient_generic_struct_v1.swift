public struct ResilientGenericStruct<T> {
  public init(value: T) {
    size = MemoryLayout<T>.size
  }
  public var size: Int
}
