public struct Notification {}

public protocol AmbiguousFuncProtocol {
  func application(recieved: Notification)
}

public protocol AmbiguousVarProtocol {
  var notification: Notification? { get }
}
