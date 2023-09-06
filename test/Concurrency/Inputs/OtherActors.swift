public class SomeClass { }
public final class SomeSendableClass: Sendable { }

public actor OtherModuleActor {
  public let a: Int = 1
  public nonisolated let b: Int = 2
  public let c: SomeClass = SomeClass()
  public let d: SomeSendableClass = SomeSendableClass()
}

@MainActor
public class OtherModuleGAIT {
  public static let A: Int = 1
}
