
@available(watchOS 2.0, *)
let HKMetadataKeyDeviceSerialNumber: String
@available(watchOS 2.0, *)
let HKMetadataKeyBodyTemperatureSensorLocation: String
@available(watchOS 2.0, *)
enum HKBodyTemperatureSensorLocation : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case other
  case armpit
  case body
  case ear
  case finger
  case gastroIntestinal
  case mouth
  case rectum
  case toe
  case earDrum
  case temporalArtery
  case forehead
}
@available(watchOS 2.0, *)
let HKMetadataKeyHeartRateSensorLocation: String
@available(watchOS 2.0, *)
enum HKHeartRateSensorLocation : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case other
  case chest
  case wrist
  case finger
  case hand
  case earLobe
  case foot
}
@available(watchOS 2.0, *)
let HKMetadataKeyFoodType: String
@available(watchOS 2.0, *)
let HKMetadataKeyUDIDeviceIdentifier: String
@available(watchOS 2.0, *)
let HKMetadataKeyUDIProductionIdentifier: String
@available(watchOS 2.0, *)
let HKMetadataKeyDigitalSignature: String
@available(watchOS 2.0, *)
let HKMetadataKeyExternalUUID: String
@available(watchOS 2.0, *)
let HKMetadataKeyTimeZone: String
@available(watchOS 2.0, *)
let HKMetadataKeyDeviceName: String
@available(watchOS 2.0, *)
let HKMetadataKeyDeviceManufacturerName: String
@available(watchOS 2.0, *)
let HKMetadataKeyWasTakenInLab: String
@available(watchOS 2.0, *)
let HKMetadataKeyReferenceRangeLowerLimit: String
@available(watchOS 2.0, *)
let HKMetadataKeyReferenceRangeUpperLimit: String
@available(watchOS 2.0, *)
let HKMetadataKeyWasUserEntered: String
@available(watchOS 2.0, *)
let HKMetadataKeyWorkoutBrandName: String
@available(watchOS 2.0, *)
let HKMetadataKeyGroupFitness: String
@available(watchOS 2.0, *)
let HKMetadataKeyIndoorWorkout: String
@available(watchOS 2.0, *)
let HKMetadataKeyCoachedWorkout: String
@available(watchOS 2.0, *)
let HKMetadataKeySexualActivityProtectionUsed: String
@available(watchOS 2.0, *)
let HKMetadataKeyMenstrualCycleStart: String
