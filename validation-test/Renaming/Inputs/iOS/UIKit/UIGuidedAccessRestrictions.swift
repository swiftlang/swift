
enum UIGuidedAccessRestrictionState : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case allow
  case deny
}
@available(iOS 7.0, *)
protocol UIGuidedAccessRestrictionDelegate : NSObjectProtocol {
  @discardableResult
  func guidedAccessRestrictionIdentifiers() -> [String]?
  func guidedAccessRestriction(withIdentifier restrictionIdentifier: String, didChange newRestrictionState: UIGuidedAccessRestrictionState)
  @discardableResult
  func textForGuidedAccessRestriction(withIdentifier restrictionIdentifier: String) -> String?
  @discardableResult
  optional func detailTextForGuidedAccessRestriction(withIdentifier restrictionIdentifier: String) -> String?
}
@available(iOS 7.0, *)
@discardableResult
func UIGuidedAccessRestrictionStateForIdentifier(_ restrictionIdentifier: String) -> UIGuidedAccessRestrictionState
