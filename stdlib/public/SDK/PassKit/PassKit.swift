import Foundation

@available(iOS, introduced=6.0)
extension PKPassKitErrorCode : _BridgedNSError {
  public static var _NSErrorDomain: String { return PKPassKitErrorDomain }
}
