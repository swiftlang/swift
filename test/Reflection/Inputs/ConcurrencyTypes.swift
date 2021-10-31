@available(SwiftStdlib 5.1, *)
public actor SomeActor {
}

@available(SwiftStdlib 5.1, *)
public struct UsesConcurrency {
  public var mainActorFunction: @MainActor () -> Void
  public var actorIsolatedFunction: (isolated SomeActor) -> Void
}
