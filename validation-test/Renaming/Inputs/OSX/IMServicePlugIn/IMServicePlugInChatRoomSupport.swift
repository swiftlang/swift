
protocol IMServicePlugInChatRoomSupport {
  func joinChatRoom(_ roomName: String!)
  func leaveChatRoom(_ roomName: String!)
  func inviteHandles(_ handles: [AnyObject]!, toChatRoom roomName: String!, with message: IMServicePlugInMessage!)
  func send(_ message: IMServicePlugInMessage!, toChatRoom roomName: String!)
  func declineChatRoomInvitation(_ roomName: String!)
}
protocol IMServiceApplicationChatRoomSupport : IMServiceApplication {
  func plug(inDidReceiveInvitation invitation: IMServicePlugInMessage!, forChatRoom roomName: String!, fromHandle handle: String!)
  func plug(inDidReceive message: IMServicePlugInMessage!, forChatRoom roomName: String!, fromHandle handle: String!)
  func plug(inDidReceiveNotice notice: String!, forChatRoom roomName: String!)
  func plug(inDidSend message: IMServicePlugInMessage!, toChatRoom roomName: String!, error error: NSError!)
  func plug(inDidJoinChatRoom roomName: String!)
  func plug(inDidLeaveChatRoom roomName: String!, error error: NSError!)
  func handles(_ handles: [AnyObject]!, didJoinChatRoom roomName: String!)
  func handles(_ handles: [AnyObject]!, didLeaveChatRoom roomName: String!)
}
