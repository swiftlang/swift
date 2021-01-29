public protocol Awaitable {
  associatedtype Result
  func wait() async -> Result
}
