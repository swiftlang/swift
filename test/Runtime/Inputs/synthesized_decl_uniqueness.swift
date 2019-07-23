import CoreLocation
import Foundation

public func getCLError() -> Any.Type {
  return CLError.self
}

public func getCLErrorCode() -> Any.Type {
  return CLError.Code.self
}

public func getNotificationNameSet() -> Any.Type {
  return Set<NSNotification.Name>.self
}
