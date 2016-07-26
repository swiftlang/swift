
@available(iOS 8.0, *)
let HKMetadataKeyDeviceSerialNumber: String
@available(iOS 8.0, *)
let HKMetadataKeyBodyTemperatureSensorLocation: String
@available(iOS 8.0, *)
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
@available(iOS 8.0, *)
let HKMetadataKeyHeartRateSensorLocation: String
@available(iOS 8.0, *)
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
@available(iOS 8.0, *)
let HKMetadataKeyFoodType: String
@available(iOS 8.0, *)
let HKMetadataKeyUDIDeviceIdentifier: String
@available(iOS 8.0, *)
let HKMetadataKeyUDIProductionIdentifier: String
@available(iOS 8.0, *)
let HKMetadataKeyDigitalSignature: String
@available(iOS 8.0, *)
let HKMetadataKeyExternalUUID: String
@available(iOS 8.0, *)
let HKMetadataKeyTimeZone: String
@available(iOS 8.0, *)
let HKMetadataKeyDeviceName: String
@available(iOS 8.0, *)
let HKMetadataKeyDeviceManufacturerName: String
@available(iOS 8.0, *)
let HKMetadataKeyWasTakenInLab: String
@available(iOS 8.0, *)
let HKMetadataKeyReferenceRangeLowerLimit: String
@available(iOS 8.0, *)
let HKMetadataKeyReferenceRangeUpperLimit: String
@available(iOS 8.0, *)
let HKMetadataKeyWasUserEntered: String
@available(iOS 8.0, *)
let HKMetadataKeyWorkoutBrandName: String
@available(iOS 8.0, *)
let HKMetadataKeyGroupFitness: String
@available(iOS 8.0, *)
let HKMetadataKeyIndoorWorkout: String
@available(iOS 8.0, *)
let HKMetadataKeyCoachedWorkout: String
@available(iOS 9.0, *)
let HKMetadataKeySexualActivityProtectionUsed: String
@available(iOS 9.0, *)
let HKMetadataKeyMenstrualCycleStart: String
