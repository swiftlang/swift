@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
@globalActor public final class MeowActor {
  public static let shared = _Impl()
  
  public actor _Impl {
    @actorIndependent
    public func enqueue(_ job: UnownedJob) {
      // DOES NOTHING! :)
    }
  }
}
