@exported import HomeKit
import Foundation

// FIXME: HMErrorDomain has @available(iOS 8.0), but NMErrorCode does not.
extension HMErrorCode : _BridgedNSError {
  public static var _NSErrorDomain: String {
    guard #available(iOS 8.0, *) else { return "" }
    return HMErrorDomain
  }
}
