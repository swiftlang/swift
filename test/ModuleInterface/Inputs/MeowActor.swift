@globalActor public final class MeowActor {
  public static let shared = _Impl()
  
  public actor class _Impl {
    @actorIndependent
    public func enqueue(partialTask: PartialAsyncTask) {
      // DOES NOTHING! :)
    }
  }
}
