@available(SwiftStdlib 5.5, *)
public actor SomeActor {
}

@available(SwiftStdlib 5.5, *)
public struct UsesConcurrency {
  public var mainActorFunction: @MainActor () -> Void
  public var actorIsolatedFunction: (isolated SomeActor) -> Void
}
