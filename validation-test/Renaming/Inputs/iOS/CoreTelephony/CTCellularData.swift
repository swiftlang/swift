
enum CTCellularDataRestrictedState : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case restrictedStateUnknown
  case restricted
  case notRestricted
}
typealias CellularDataRestrictionDidUpdateNotifier = (CTCellularDataRestrictedState) -> Void
@available(iOS 9.0, *)
class CTCellularData : NSObject {
  @available(iOS 9.0, *)
  var cellularDataRestrictionDidUpdateNotifier: CellularDataRestrictionDidUpdateNotifier?
  @available(iOS 9.0, *)
  var restrictedState: CTCellularDataRestrictedState { get }
}
