@_exported import GameKit
import Foundation

extension GKErrorCode : _BridgedNSError {
  public static var _nsErrorDomain: String { return GKErrorDomain }
}
