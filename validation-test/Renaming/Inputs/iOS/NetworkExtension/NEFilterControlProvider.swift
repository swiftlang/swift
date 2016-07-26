
@available(iOS 9.0, *)
class NEFilterControlProvider : NEFilterProvider {
  @available(iOS 9.0, *)
  var remediationMap: [String : [String : NSObject]]?
  @available(iOS 9.0, *)
  var urlAppendStringMap: [String : String]?
  @available(iOS 9.0, *)
  func handleRemediation(for flow: NEFilterFlow, completionHandler completionHandler: (NEFilterControlVerdict) -> Void)
  @available(iOS 9.0, *)
  func handleNewFlow(_ flow: NEFilterFlow, completionHandler completionHandler: (NEFilterControlVerdict) -> Void)
  @available(iOS 9.0, *)
  func notifyRulesChanged()
}
