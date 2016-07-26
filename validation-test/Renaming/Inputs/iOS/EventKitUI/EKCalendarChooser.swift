
enum EKCalendarChooserSelectionStyle : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case single
  case multiple
}
enum EKCalendarChooserDisplayStyle : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case allCalendars
  case writableCalendarsOnly
}
@available(iOS 5.0, *)
class EKCalendarChooser : UIViewController {
  init(selectionStyle selectionStyle: EKCalendarChooserSelectionStyle, displayStyle displayStyle: EKCalendarChooserDisplayStyle, eventStore eventStore: EKEventStore)
  init(selectionStyle style: EKCalendarChooserSelectionStyle, displayStyle displayStyle: EKCalendarChooserDisplayStyle, entityType entityType: EKEntityType, eventStore eventStore: EKEventStore)
  var selectionStyle: EKCalendarChooserSelectionStyle { get }
  weak var delegate: @sil_weak EKCalendarChooserDelegate?
  var showsDoneButton: Bool
  var showsCancelButton: Bool
  var selectedCalendars: Set<EKCalendar>
}
protocol EKCalendarChooserDelegate : NSObjectProtocol {
  @available(iOS 5.0, *)
  optional func calendarChooserSelectionDidChange(_ calendarChooser: EKCalendarChooser)
  @available(iOS 5.0, *)
  optional func calendarChooserDidFinish(_ calendarChooser: EKCalendarChooser)
  @available(iOS 5.0, *)
  optional func calendarChooserDidCancel(_ calendarChooser: EKCalendarChooser)
}
