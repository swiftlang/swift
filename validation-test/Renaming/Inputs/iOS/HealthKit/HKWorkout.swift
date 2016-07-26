
@available(iOS 8.0, *)
enum HKWorkoutActivityType : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case americanFootball
  case archery
  case australianFootball
  case badminton
  case baseball
  case basketball
  case bowling
  case boxing
  case climbing
  case cricket
  case crossTraining
  case curling
  case cycling
  case dance
  case danceInspiredTraining
  case elliptical
  case equestrianSports
  case fencing
  case fishing
  case functionalStrengthTraining
  case golf
  case gymnastics
  case handball
  case hiking
  case hockey
  case hunting
  case lacrosse
  case martialArts
  case mindAndBody
  case mixedMetabolicCardioTraining
  case paddleSports
  case play
  case preparationAndRecovery
  case racquetball
  case rowing
  case rugby
  case running
  case sailing
  case skatingSports
  case snowSports
  case soccer
  case softball
  case squash
  case stairClimbing
  case surfingSports
  case swimming
  case tableTennis
  case tennis
  case trackAndField
  case traditionalStrengthTraining
  case volleyball
  case walking
  case waterFitness
  case waterPolo
  case waterSports
  case wrestling
  case yoga
  case other
}
@available(iOS 8.0, *)
enum HKWorkoutEventType : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case pause
  case resume
}
@available(iOS 8.0, *)
class HKWorkoutEvent : NSObject, NSSecureCoding {
  var type: HKWorkoutEventType { get }
  @NSCopying var date: NSDate { get }
  convenience init(type type: HKWorkoutEventType, date date: NSDate)
}
@available(iOS 8.0, *)
class HKWorkout : HKSample {
  var workoutActivityType: HKWorkoutActivityType { get }
  var workoutEvents: [HKWorkoutEvent]? { get }
  var duration: NSTimeInterval { get }
  var totalEnergyBurned: HKQuantity? { get }
  var totalDistance: HKQuantity? { get }
  convenience init(activityType workoutActivityType: HKWorkoutActivityType, start startDate: NSDate, end endDate: NSDate)
  convenience init(activityType workoutActivityType: HKWorkoutActivityType, start startDate: NSDate, end endDate: NSDate, workoutEvents workoutEvents: [HKWorkoutEvent]?, totalEnergyBurned totalEnergyBurned: HKQuantity?, totalDistance totalDistance: HKQuantity?, metadata metadata: [String : AnyObject]?)
  @available(iOS 9.0, *)
  convenience init(activityType workoutActivityType: HKWorkoutActivityType, start startDate: NSDate, end endDate: NSDate, workoutEvents workoutEvents: [HKWorkoutEvent]?, totalEnergyBurned totalEnergyBurned: HKQuantity?, totalDistance totalDistance: HKQuantity?, device device: HKDevice?, metadata metadata: [String : AnyObject]?)
  convenience init(activityType workoutActivityType: HKWorkoutActivityType, start startDate: NSDate, end endDate: NSDate, duration duration: NSTimeInterval, totalEnergyBurned totalEnergyBurned: HKQuantity?, totalDistance totalDistance: HKQuantity?, metadata metadata: [String : AnyObject]?)
  @available(iOS 9.0, *)
  convenience init(activityType workoutActivityType: HKWorkoutActivityType, start startDate: NSDate, end endDate: NSDate, duration duration: NSTimeInterval, totalEnergyBurned totalEnergyBurned: HKQuantity?, totalDistance totalDistance: HKQuantity?, device device: HKDevice?, metadata metadata: [String : AnyObject]?)
}
@available(iOS 8.0, *)
let HKPredicateKeyPathWorkoutDuration: String
@available(iOS 8.0, *)
let HKPredicateKeyPathWorkoutTotalDistance: String
@available(iOS 8.0, *)
let HKPredicateKeyPathWorkoutTotalEnergyBurned: String
@available(iOS 8.0, *)
let HKPredicateKeyPathWorkoutType: String
@available(iOS 8.0, *)
let HKWorkoutSortIdentifierDuration: String
@available(iOS 8.0, *)
let HKWorkoutSortIdentifierTotalDistance: String
@available(iOS 8.0, *)
let HKWorkoutSortIdentifierTotalEnergyBurned: String
