
class IMServicePlugInMessage : NSObject, NSCoding, NSCopying {
  @discardableResult
  class func servicePlugInMessage(withContent content: NSAttributedString!) -> AnyObject!
  init!(content content: NSAttributedString!)
  @discardableResult
  class func servicePlugInMessage(withContent content: NSAttributedString!, date date: NSDate!) -> AnyObject!
  init!(content content: NSAttributedString!, date date: NSDate!)
  var guid: String! { get }
  @NSCopying var content: NSAttributedString!
  @NSCopying var date: NSDate!
}
