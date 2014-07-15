public let x: Int = 0
internal let y: Int = 0
private let z: Int = 0

#if DEFINE_VAR_FOR_SCOPED_IMPORT
internal let zz: Int = 0
#endif

public struct Foo {
  public static func x() {}
  internal static func y() {}
  private static func z() {}
}

public class Base {
  public internal(set) var value = 0
  internal func method() {}
}

