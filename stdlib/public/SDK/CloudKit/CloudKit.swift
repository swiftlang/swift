@_exported import CloudKit
import Foundation

// rdar://27144738
//@available(OSX, introduced: 10.10)
@available(OSX, introduced: 10.9)
@available(iOS, introduced: 8.0)
extension CKErrorCode : _BridgedNSError {
  public static var _nsErrorDomain: String { return CKErrorDomain }
}
