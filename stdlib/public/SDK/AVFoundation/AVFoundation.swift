@exported import AVFoundation // Clang module
import Foundation

extension AVError : _BridgedNSError {
  public static var _NSErrorDomain: String { return AVFoundationErrorDomain }
}

