public protocol Sized {
  static var size: Int { get }
}

@backDeployed(before: macOS 99)
@frozen public struct Wide {
  public var x: Int
  public init(x: Int) { self.x = x }
}

extension Wide: Sized {
  @_transparent
  public static var size: Int { 16 }
}
