
@available(watchOS 2.0, *)
let HKErrorDomain: String
@available(watchOS 2.0, *)
enum HKErrorCode : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case noError
  case errorHealthDataUnavailable
  case errorHealthDataRestricted
  case errorInvalidArgument
  case errorAuthorizationDenied
  case errorAuthorizationNotDetermined
  case errorDatabaseInaccessible
  case errorUserCanceled
  @available(watchOS 2.0, *)
  case errorAnotherWorkoutSessionStarted
  @available(watchOS 2.0, *)
  case errorUserExitedWorkoutSession
}
@available(watchOS 2.0, *)
enum HKUpdateFrequency : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case immediate
  case hourly
  case daily
  case weekly
}
@available(watchOS 2.0, *)
enum HKAuthorizationStatus : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case notDetermined
  case sharingDenied
  case sharingAuthorized
}
enum HKBiologicalSex : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case notSet
  @available(watchOS 2.0, *)
  case female
  @available(watchOS 2.0, *)
  case male
  @available(watchOS 2.0, *)
  case other
}
@available(watchOS 2.0, *)
enum HKBloodType : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case notSet
  case aPositive
  case aNegative
  case bPositive
  case bNegative
  case abPositive
  case abNegative
  case oPositive
  case oNegative
}
@available(watchOS 2.0, *)
enum HKCategoryValueSleepAnalysis : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case inBed
  case asleep
}
@available(watchOS 2.0, *)
enum HKCategoryValueAppleStandHour : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case stood
  case idle
}
@available(watchOS 2.0, *)
enum HKFitzpatrickSkinType : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case notSet
  case I
  case II
  case III
  case IV
  case V
  case VI
}
@available(watchOS 2.0, *)
enum HKCategoryValueCervicalMucusQuality : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case dry
  case sticky
  case creamy
  case watery
  case eggWhite
}
@available(watchOS 2.0, *)
enum HKCategoryValueOvulationTestResult : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case negative
  case positive
  case indeterminate
}
@available(watchOS 2.0, *)
enum HKCategoryValueMenstrualFlow : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case unspecified
  case light
  case medium
  case heavy
}
@available(watchOS 2.0, *)
enum HKCategoryValue : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case notApplicable
}
