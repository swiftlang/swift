
protocol IMServicePlugIn : NSObjectProtocol {
  init!(serviceApplication serviceApplication: IMServiceApplication!)
  @available(OSX 10.0, *)
  func updateAccountSettings(_ accountSettings: [NSObject : AnyObject]!)
  func login()
  func logout()
}
protocol IMServiceApplication : NSObjectProtocol {
  func plugInDidLogIn()
  func plugInDidLogOutWithError(_ error: NSError!, reconnect reconnect: Bool)
  func plugInDidFailToAuthenticate()
  @available(OSX 10.0, *)
  func plug(inDidUpdateProperties changes: [NSObject : AnyObject]!, ofHandle handle: String!)
}
