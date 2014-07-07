@public let x: Int = 0
@internal let y: Int = 0
@private let z: Int = 0

@public struct Foo {
  @public static func x() {}
  @internal static func y() {}
  @private static func z() {}
}
