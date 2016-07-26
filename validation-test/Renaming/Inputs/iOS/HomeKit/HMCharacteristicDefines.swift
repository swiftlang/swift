
@available(iOS 8.0, *)
enum HMCharacteristicValueDoorState : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case open
  case closed
  case opening
  case closing
  case stopped
}
@available(iOS 8.0, *)
enum HMCharacteristicValueHeatingCooling : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case off
  case heat
  case cool
  case auto
}
@available(iOS 8.0, *)
enum HMCharacteristicValueRotationDirection : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case clockwise
  case counterClockwise
}
@available(iOS 8.0, *)
enum HMCharacteristicValueTemperatureUnit : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case celsius
  case fahrenheit
}
@available(iOS 8.0, *)
enum HMCharacteristicValueLockMechanismState : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case unsecured
  case secured
  case jammed
  case unknown
}
@available(iOS 8.0, *)
enum HMCharacteristicValueLockMechanismLastKnownAction : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case securedUsingPhysicalMovementInterior
  case unsecuredUsingPhysicalMovementInterior
  case securedUsingPhysicalMovementExterior
  case unsecuredUsingPhysicalMovementExterior
  case securedWithKeypad
  case unsecuredWithKeypad
  case securedRemotely
  case unsecuredRemotely
  case securedWithAutomaticSecureTimeout
  case securedUsingPhysicalMovement
  case unsecuredUsingPhysicalMovement
}
@available(iOS 9.0, *)
enum HMCharacteristicValueAirParticulateSize : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case size2_5
  case size10
}
@available(iOS 9.0, *)
enum HMCharacteristicValueAirQuality : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case unknown
  case excellent
  case good
  case fair
  case inferior
  case poor
}
@available(iOS 9.0, *)
enum HMCharacteristicValuePositionState : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case closing
  case opening
  case stopped
}
@available(iOS 9.0, *)
enum HMCharacteristicValueCurrentSecuritySystemState : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case stayArm
  case awayArm
  case nightArm
  case disarmed
  case triggered
}
@available(iOS 9.0, *)
enum HMCharacteristicValueTargetSecuritySystemState : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case stayArm
  case awayArm
  case nightArm
  case disarm
}
