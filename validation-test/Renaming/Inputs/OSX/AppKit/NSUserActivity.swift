
var NS_USER_ACTIVITY_SUPPORTED: Int32 { get }
extension NSResponder {
  @available(OSX 10.10, *)
  var userActivity: NSUserActivity?
  @available(OSX 10.10, *)
  func updateUserActivityState(_ userActivity: NSUserActivity)
  @available(OSX 10.10, *)
  func restoreUserActivityState(_ userActivity: NSUserActivity)
}
extension NSDocument {
  @available(OSX 10.10, *)
  var userActivity: NSUserActivity?
  @available(OSX 10.10, *)
  func updateUserActivityState(_ activity: NSUserActivity)
  @available(OSX 10.10, *)
  func restoreUserActivityState(_ activity: NSUserActivity)
}
@available(OSX 10.10, *)
let NSUserActivityDocumentURLKey: String
