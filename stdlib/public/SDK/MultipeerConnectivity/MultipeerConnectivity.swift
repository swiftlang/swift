@_exported import MultipeerConnectivity
import Foundation

// rdar://27144738
//@available(OSX, introduced: 10.10)
@available(OSX, introduced: 10.9)
@available(iOS, introduced: 7.0)
extension MCErrorCode : _BridgedNSError {
  public static var _nsErrorDomain: String { return MCErrorDomain }
}
