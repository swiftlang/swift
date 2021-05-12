@available(SwiftStdlib 5.5, *)
@globalActor public final class MeowActor {
  public static let shared = _Impl()
  
  public actor _Impl {
    @actorIndependent
    public func enqueue(_ job: UnownedJob) {
      // DOES NOTHING! :)
    }
  }
}
