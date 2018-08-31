public struct ResilientGenericStruct<T> {
  public init(value: T) {
    size = MemoryLayout<T>.size
    storage = value
  }
  public var size: Int
  private var storage: T
}
