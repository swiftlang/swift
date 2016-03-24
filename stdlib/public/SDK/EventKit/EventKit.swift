@_exported import EventKit
import Foundation

extension EKErrorCode : _BridgedNSError {
  public static var _nsErrorDomain: String { return EKErrorDomain }
}
