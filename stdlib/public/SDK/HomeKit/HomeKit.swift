@_exported import HomeKit
import Foundation

@available(iOS 8.0, watchOS 2.0, tvOS 10.0, *)
extension HMErrorCode : _BridgedNSError {
  public static var _nsErrorDomain: String {
    return HMErrorDomain
  }
}

@available(iOS 8.0, watchOS 2.0, tvOS 10.0, *)
public let HMCharacteristicPropertySupportsEventNotification = Notification.Name.HMCharacteristicPropertySupportsEvent.rawValue
