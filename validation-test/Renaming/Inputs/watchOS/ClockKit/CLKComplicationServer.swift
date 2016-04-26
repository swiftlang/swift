
let CLKComplicationServerActiveComplicationsDidChangeNotification: String
class CLKComplicationServer : NSObject {
  @discardableResult
  class func sharedInstance() -> Self
  var activeComplications: [CLKComplication]? { get }
  var earliestTimeTravelDate: NSDate { get }
  var latestTimeTravelDate: NSDate { get }
  func reloadTimeline(for complication: CLKComplication)
  func extendTimeline(for complication: CLKComplication)
}
