
@available(OSX 10.11, *)
enum NSTableViewRowActionStyle : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case regular
  case destructive
}
@available(OSX 10.11, *)
class NSTableViewRowAction : NSObject, NSCopying {
  convenience init(style style: NSTableViewRowActionStyle, title title: String, handler handler: (NSTableViewRowAction, Int) -> Void)
  var style: NSTableViewRowActionStyle { get }
  var title: String
  @NSCopying var backgroundColor: NSColor!
}
