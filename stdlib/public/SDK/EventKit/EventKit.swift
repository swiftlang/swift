import Foundation

extension EKErrorCode : _BridgedNSError {
  public static var _NSErrorDomain: String { return EKErrorDomain }
}
