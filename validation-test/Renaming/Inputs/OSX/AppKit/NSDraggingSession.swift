
@available(OSX 10.7, *)
class NSDraggingSession : NSObject {
  var draggingFormation: NSDraggingFormation
  var animatesToStartingPositionsOnCancelOrFail: Bool
  var draggingLeaderIndex: Int
  var draggingPasteboard: NSPasteboard { get }
  var draggingSequenceNumber: Int { get }
  var draggingLocation: NSPoint { get }
  func enumerateDraggingItems(_ enumOpts: NSDraggingItemEnumerationOptions = [], for view: NSView?, classes classArray: [AnyClass], searchOptions searchOptions: [String : AnyObject] = [:], using block: (NSDraggingItem, Int, UnsafeMutablePointer<ObjCBool>) -> Void)
}
