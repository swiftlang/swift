@exported import MultipeerConnectivity
import Foundation

@available(OSX, introduced=10.10) @available(iOS, introduced=7.0)
extension MCErrorCode : _BridgedNSError {
  public static var _NSErrorDomain: String { return MCErrorDomain }
}
