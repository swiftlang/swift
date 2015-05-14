@exported import GameKit
import Foundation

extension GKErrorCode : _BridgedNSError {
  public static var _NSErrorDomain: String { return GKErrorDomain }
}
