import pre_specialized_module

@frozen
public struct SomeOtherData {
  public init() {}
}

@_specialize(exported: true, target: publicPrespecialized2(_:), availability: macOS 50, *; where T == SomeOtherData)
public func pre_specialize_publicPrespecialized<T>(_ t: T) {
}
