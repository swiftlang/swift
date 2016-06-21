@_exported import CoreBluetooth
import Foundation

extension CBError : _BridgedNSError {
  public static var _nsErrorDomain: String { return CBErrorDomain }
}

extension CBATTError : _BridgedNSError {
  public static var _nsErrorDomain: String { return CBATTErrorDomain }
}
