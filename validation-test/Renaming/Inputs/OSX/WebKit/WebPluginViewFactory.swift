
let WebPlugInBaseURLKey: String
let WebPlugInAttributesKey: String
let WebPlugInContainerKey: String
let WebPlugInContainingElementKey: String
@available(OSX 10.6, *)
let WebPlugInShouldLoadMainResourceKey: String
protocol WebPlugInViewFactory : NSObjectProtocol {
  @available(OSX 10.0, *)
  @discardableResult
  static func plugInView(withArguments arguments: [NSObject : AnyObject]!) -> NSView!
}
