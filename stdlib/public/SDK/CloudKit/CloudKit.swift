import Foundation

@available(OSX, introduced=10.10) @available(iOS, introduced=8.0)
extension CKErrorCode : _BridgedNSError {
  public static var _NSErrorDomain: String { return CKErrorDomain }
}
