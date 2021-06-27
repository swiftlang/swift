public class SomeClass { }

public actor OtherModuleActor {
  public let a: Int = 1
  public nonisolated let b: Int = 2
  public let c: SomeClass = SomeClass()
}
