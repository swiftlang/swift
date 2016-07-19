@_exported import WatchConnectivity
import Foundation

@available(iOS, introduced: 9.0)
extension WCErrorCode : _BridgedNSError {
  public static var _nsErrorDomain: String { return WCErrorDomain }
}
