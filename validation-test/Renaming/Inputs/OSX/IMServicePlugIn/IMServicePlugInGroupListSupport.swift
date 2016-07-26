
protocol IMServicePlugInGroupListSupport {
  func requestGroupList()
}
protocol IMServicePlugInGroupListEditingSupport : IMServicePlugInGroupListSupport {
  func addGroups(_ groupNames: [AnyObject]!)
  func removeGroups(_ groupNames: [AnyObject]!)
  func renameGroup(_ oldGroupName: String!, toGroup newGroupName: String!)
  func addHandles(_ handles: [AnyObject]!, toGroup groupName: String!)
  func removeHandles(_ handles: [AnyObject]!, fromGroup groupName: String!)
}
protocol IMServicePlugInGroupListOrderingSupport : IMServicePlugInGroupListSupport {
  func reorderGroups(_ groupNames: [AnyObject]!)
  func reorderHandles(_ handles: [AnyObject]!, inGroup groupName: String!)
}
protocol IMServicePlugInGroupListAuthorizationSupport : IMServicePlugInGroupListSupport {
  func sendAuthorizationRequest(toHandle handle: String!)
  func acceptAuthorizationRequest(fromHandle handle: String!)
  func declineAuthorizationRequest(fromHandle handle: String!)
}
protocol IMServicePlugInGroupListHandlePictureSupport : NSObjectProtocol {
  func requestPicture(forHandle handle: String!, withIdentifier identifier: String!)
}
protocol IMServiceApplicationGroupListSupport : IMServiceApplication {
  func plug(inDidUpdateGroupList groups: [AnyObject]!, error error: NSError!)
}
protocol IMServiceApplicationGroupListAuthorizationSupport : IMServiceApplicationGroupListSupport {
  func plugInDidReceiveAuthorizationRequest(fromHandle handle: String!)
}
