
class CLKComplicationTimelineEntry : NSObject {
  convenience init(date date: NSDate, complicationTemplate complicationTemplate: CLKComplicationTemplate)
  convenience init(date date: NSDate, complicationTemplate complicationTemplate: CLKComplicationTemplate, timelineAnimationGroup timelineAnimationGroup: String?)
  var date: NSDate
  @NSCopying var complicationTemplate: CLKComplicationTemplate
  var timelineAnimationGroup: String?
}
