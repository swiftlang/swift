
protocol IMServicePlugInInstantMessagingSupport {
  func userDidStartTyping(toHandle handle: String!)
  func userDidStopTyping(toHandle handle: String!)
  func send(_ message: IMServicePlugInMessage!, toHandle handle: String!)
}
protocol IMServiceApplicationInstantMessagingSupport {
  func handleDidStartTyping(_ handle: String!)
  func handleDidStopTyping(_ handle: String!)
  func plug(inDidReceive message: IMServicePlugInMessage!, fromHandle handle: String!)
  func plug(inDidSend message: IMServicePlugInMessage!, toHandle handle: String!, error error: NSError!)
}
