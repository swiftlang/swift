import Foundation

extension CBError : _BridgedNSError {
  public static var _NSErrorDomain: String { return CBErrorDomain }
}

extension CBATTError : _BridgedNSError {
  public static var _NSErrorDomain: String { return CBATTErrorDomain }
}
