@globalActor public final class MeowActor {
  public static let shared = _Impl()
  
  public actor _Impl {
    @actorIndependent
    public func enqueue(partialTask: PartialAsyncTask) {
      // DOES NOTHING! :)
    }
  }
}
