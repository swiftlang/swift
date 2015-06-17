@exported import WebKit
import Foundation

@available(OSX, introduced=10.10) @available(iOS, introduced=8.0)
extension WKErrorCode : _BridgedNSError {
  public static var _NSErrorDomain: String { return WKErrorDomain }
}
