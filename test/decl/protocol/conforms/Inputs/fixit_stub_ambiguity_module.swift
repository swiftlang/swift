public struct Notification {}

public protocol AmbiguousFuncProtocol {
  func application(received: Notification)
}

public protocol AmbiguousVarProtocol {
  var notification: Notification? { get }
}
