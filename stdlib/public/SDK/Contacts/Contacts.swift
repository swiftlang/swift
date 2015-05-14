@exported import Contacts
import Foundation

@available(OSX, introduced=10.11) @available(iOS, introduced=9.0)
extension CNErrorCode : _BridgedNSError {
  public static var _NSErrorDomain: String { return CNErrorDomain }
}
