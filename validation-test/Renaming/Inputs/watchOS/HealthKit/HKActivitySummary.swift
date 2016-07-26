
@available(watchOS 2.2, *)
class HKActivitySummary : NSObject, NSSecureCoding, NSCopying {
  @discardableResult
  func dateComponents(for calendar: NSCalendar) -> NSDateComponents
  var activeEnergyBurned: HKQuantity
  var appleExerciseTime: HKQuantity
  var appleStandHours: HKQuantity
  var activeEnergyBurnedGoal: HKQuantity
  var appleExerciseTimeGoal: HKQuantity
  var appleStandHoursGoal: HKQuantity
}
@available(watchOS 2.2, *)
let HKPredicateKeyPathDateComponents: String
