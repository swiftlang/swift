@_exported import LocalAuthentication
import Foundation

@available(OSX 10.10, iOS 8.0, *)
extension LAError : _BridgedNSError {
  public static var _nsErrorDomain: String {
    if #available(iOS 8.3, *) {
      return LAErrorDomain
    }
    return "com.apple.LocalAuthentication"
  }
}
