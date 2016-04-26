
@available(watchOS 2.0, *)
enum HKWorkoutSessionState : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case notStarted
  case running
  case ended
}
@available(watchOS 2.0, *)
enum HKWorkoutSessionLocationType : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case unknown
  case indoor
  case outdoor
}
@available(watchOS 2.0, *)
class HKWorkoutSession : NSObject, NSSecureCoding {
  var activityType: HKWorkoutActivityType { get }
  var locationType: HKWorkoutSessionLocationType { get }
  weak var delegate: @sil_weak HKWorkoutSessionDelegate?
  var state: HKWorkoutSessionState { get }
  var startDate: NSDate? { get }
  var endDate: NSDate? { get }
  init(activityType activityType: HKWorkoutActivityType, locationType locationType: HKWorkoutSessionLocationType)
}
@available(watchOS 2.0, *)
protocol HKWorkoutSessionDelegate : NSObjectProtocol {
  func workoutSession(_ workoutSession: HKWorkoutSession, didChangeTo toState: HKWorkoutSessionState, from fromState: HKWorkoutSessionState, date date: NSDate)
  func workoutSession(_ workoutSession: HKWorkoutSession, didFailWithError error: NSError)
}
