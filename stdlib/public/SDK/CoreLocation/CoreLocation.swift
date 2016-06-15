@_exported import CoreLocation
import Foundation

extension CLError : _BridgedNSError {
  public static var _nsErrorDomain: String { return kCLErrorDomain }
}
