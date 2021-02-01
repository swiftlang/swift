public protocol Awaitable {
  associatedtype Result
  func waitForNothing() async
  func wait() async -> Result
  func wait(orThrow: Bool) async throws
}
