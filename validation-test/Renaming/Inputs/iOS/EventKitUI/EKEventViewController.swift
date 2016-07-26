
enum EKEventViewAction : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case done
  case responded
  case deleted
}
@available(iOS 4.0, *)
class EKEventViewController : UIViewController {
  @available(iOS 4.2, *)
  weak var delegate: @sil_weak EKEventViewDelegate?
  var event: EKEvent
  var allowsEditing: Bool
  var allowsCalendarPreview: Bool
}
protocol EKEventViewDelegate : NSObjectProtocol {
  @available(iOS 4.2, *)
  func eventViewController(_ controller: EKEventViewController, didCompleteWith action: EKEventViewAction)
}
