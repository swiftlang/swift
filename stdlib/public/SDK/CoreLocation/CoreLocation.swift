import Foundation

extension CLError : _BridgedNSError {
  public static var _NSErrorDomain: String { return kCLErrorDomain }
}
