class ClassWithAsyncAndHandler {
  @completionHandlerAsync("foo(_:)", completionHandlerIndex: 1)
  func foo(_ operation: String, completionHandler handler: @escaping (Int) -> Void) {}
  func foo(_ operation: String) async -> Int { 0 }
}
