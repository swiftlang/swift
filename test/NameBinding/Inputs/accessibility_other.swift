import has_accessibility

public let a = 0
internal let b = 0
private let c = 0

extension Foo {
  public static func a() {}
  internal static func b() {}
  private static func c() {}
}

struct PrivateInit {
  private init() {}
}
