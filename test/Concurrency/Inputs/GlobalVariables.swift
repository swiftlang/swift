public struct Globals {
  public static let integerConstant = 0
  public static var integerMutable = 0

  public static let nonisolatedUnsafeIntegerConstant = 0
  public static nonisolated(unsafe) var nonisolatedUnsafeIntegerMutable = 0

  @MainActor public static var actorInteger = 0

  public init() {}
}

public var mutableGlobal: String = "can't touch this"
public var globalInt = 17
