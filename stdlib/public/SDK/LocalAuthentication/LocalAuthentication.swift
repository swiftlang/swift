@_exported import LocalAuthentication
import Foundation

// rdar://27144738
//@available(OSX 10.10, iOS 8.0, *)
@available(OSX 10.9, iOS 8.0, *)
extension LAError : _BridgedNSError {
  public static var _nsErrorDomain: String {
    if #available(iOS 8.3, *) {
      return LAErrorDomain
    }
    return "com.apple.LocalAuthentication"
  }
}
