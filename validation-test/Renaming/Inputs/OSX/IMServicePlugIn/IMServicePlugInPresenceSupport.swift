
protocol IMServicePlugInPresenceSupport {
  @available(OSX 10.0, *)
  func updateSessionProperties(_ properties: [NSObject : AnyObject]!)
}
