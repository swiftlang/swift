class ClassWithAsyncAndHandler {
  @available(*, renamed: "foo(_:)")
  func foo(_ operation: String, completionHandler handler: @escaping (Int) -> Void) {}
  func foo(_ operation: String) async -> Int { 0 }
}
