@_exported import HomeKit
import Foundation

@available(iOS 8.0, *)
extension HMErrorCode : _BridgedNSError {
  public static var _nsErrorDomain: String {
    return HMErrorDomain
  }
}
