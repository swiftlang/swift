@_exported import PassKit
import Foundation

@available(iOS, introduced: 6.0)
extension PKPassKitErrorCode : _BridgedNSError {
  public static var _nsErrorDomain: String { return PKPassKitErrorDomain }
}
