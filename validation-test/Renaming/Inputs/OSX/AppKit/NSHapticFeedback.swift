
@available(OSX 10.11, *)
enum NSHapticFeedbackPattern : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case generic
  case alignment
  case levelChange
}
@available(OSX 10.11, *)
enum NSHapticFeedbackPerformanceTime : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case `default`
  case now
  case drawCompleted
}
protocol NSHapticFeedbackPerformer : NSObjectProtocol {
  @available(OSX 10.11, *)
  func perform(_ pattern: NSHapticFeedbackPattern, performanceTime performanceTime: NSHapticFeedbackPerformanceTime)
}
@available(OSX 10.11, *)
class NSHapticFeedbackManager : NSObject {
  @discardableResult
  class func defaultPerformer() -> NSHapticFeedbackPerformer
}
