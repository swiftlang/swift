@available(SwiftStdlib 5.1, *)
@globalActor public final class MeowActor {
  public static let shared = _Impl()

  public actor _Impl {
    nonisolated
    public func enqueue(_ job: UnownedJob) {
      // DOES NOTHING! :)
    }
  }
}

@available(SwiftStdlib 5.1, *)
@globalActor public struct HissActor {
  public static let shared = _Impl()

  public actor _Impl {
    nonisolated
    public func enqueue(_ job: UnownedJob) {
      // DOES NOTHING! :)
    }
  }
}
