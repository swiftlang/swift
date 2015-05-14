@exported import WatchKit
import Foundation

extension WatchKitErrorCode : _BridgedNSError {
  public static var _NSErrorDomain: String { return WatchKitErrorDomain }
}
