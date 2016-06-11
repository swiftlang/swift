public let x: Int = 0
internal let y: Int = 0
fileprivate let z: Int = 0

#if DEFINE_VAR_FOR_SCOPED_IMPORT
internal let zz: Int = 0
#endif

public struct Foo {
  internal init() {}

  public static func x() {}
  internal static func y() {}
  fileprivate static func z() {}
}

public class Base {
  internal func method() {}
  public internal(set) var value = 0
}

public class HiddenMethod {
  internal func method() {}
}

public class HiddenType {
  typealias TheType = Int
}

public struct OriginallyEmpty {}


public struct StructWithPrivateSetter {
  public private(set) var x = 0
  public init() {}
}
