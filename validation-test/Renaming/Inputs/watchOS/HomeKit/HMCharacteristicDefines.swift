
@available(watchOS 20000, *)
enum HMCharacteristicValueDoorState : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case open
  case closed
  case opening
  case closing
  case stopped
}
@available(watchOS 20000, *)
enum HMCharacteristicValueHeatingCooling : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case off
  case heat
  case cool
  case auto
}
@available(watchOS 20000, *)
enum HMCharacteristicValueRotationDirection : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case clockwise
  case counterClockwise
}
@available(watchOS 20000, *)
enum HMCharacteristicValueTemperatureUnit : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case celsius
  case fahrenheit
}
@available(watchOS 20000, *)
enum HMCharacteristicValueLockMechanismState : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case unsecured
  case secured
  case jammed
  case unknown
}
@available(watchOS 20000, *)
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
@available(watchOS 20000, *)
enum HMCharacteristicValueAirParticulateSize : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case size2_5
  case size10
}
@available(watchOS 20000, *)
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
@available(watchOS 20000, *)
enum HMCharacteristicValuePositionState : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case closing
  case opening
  case stopped
}
@available(watchOS 20000, *)
enum HMCharacteristicValueCurrentSecuritySystemState : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case stayArm
  case awayArm
  case nightArm
  case disarmed
  case triggered
}
@available(watchOS 20000, *)
enum HMCharacteristicValueTargetSecuritySystemState : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case stayArm
  case awayArm
  case nightArm
  case disarm
}
