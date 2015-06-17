import Foundation

@available(iOS 8.0, *)
extension HMErrorCode : _BridgedNSError {
  public static var _NSErrorDomain: String {
    return HMErrorDomain
  }
}
