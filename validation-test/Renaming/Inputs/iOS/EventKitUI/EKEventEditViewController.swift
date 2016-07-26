
enum EKEventEditViewAction : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case canceled
  case saved
  case deleted
  static var cancelled: EKEventEditViewAction { get }
}
@available(iOS 4.0, *)
class EKEventEditViewController : UINavigationController {
  weak var editViewDelegate: @sil_weak EKEventEditViewDelegate?
  var eventStore: EKEventStore
  var event: EKEvent?
  func cancelEditing()
}
protocol EKEventEditViewDelegate : NSObjectProtocol {
  @available(iOS 4.0, *)
  func eventEditViewController(_ controller: EKEventEditViewController, didCompleteWith action: EKEventEditViewAction)
  @available(iOS 4.0, *)
  @discardableResult
  optional func eventEditViewControllerDefaultCalendar(forNewEvents controller: EKEventEditViewController) -> EKCalendar
}
