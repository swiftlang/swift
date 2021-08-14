public protocol Awaitable {
  associatedtype Result
  func waitForNothing() async
  func waitForInt() async -> Int
  func wait() async -> Result
  func wait(orThrow: Bool) async throws
}
