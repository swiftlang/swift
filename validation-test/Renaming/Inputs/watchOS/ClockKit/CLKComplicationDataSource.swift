
protocol CLKComplicationDataSource : NSObjectProtocol {
  func getSupportedTimeTravelDirections(for complication: CLKComplication, withHandler handler: (CLKComplicationTimeTravelDirections) -> Void)
  optional func getTimelineStartDate(for complication: CLKComplication, withHandler handler: (NSDate?) -> Void)
  optional func getTimelineEndDate(for complication: CLKComplication, withHandler handler: (NSDate?) -> Void)
  optional func getPrivacyBehavior(for complication: CLKComplication, withHandler handler: (CLKComplicationPrivacyBehavior) -> Void)
  optional func getTimelineAnimationBehavior(for complication: CLKComplication, withHandler handler: (CLKComplicationTimelineAnimationBehavior) -> Void)
  func getCurrentTimelineEntry(for complication: CLKComplication, withHandler handler: (CLKComplicationTimelineEntry?) -> Void)
  optional func getTimelineEntries(for complication: CLKComplication, before date: NSDate, limit limit: Int, withHandler handler: ([CLKComplicationTimelineEntry]?) -> Void)
  optional func getTimelineEntries(for complication: CLKComplication, after date: NSDate, limit limit: Int, withHandler handler: ([CLKComplicationTimelineEntry]?) -> Void)
  optional func getNextRequestedUpdateDate(handler handler: (NSDate?) -> Void)
  optional func requestedUpdateDidBegin()
  optional func requestedUpdateBudgetExhausted()
  func getPlaceholderTemplate(for complication: CLKComplication, withHandler handler: (CLKComplicationTemplate?) -> Void)
}
