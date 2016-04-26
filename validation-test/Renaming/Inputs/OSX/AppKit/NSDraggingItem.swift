
@available(OSX 10.7, *)
let NSDraggingImageComponentIconKey: String
@available(OSX 10.7, *)
let NSDraggingImageComponentLabelKey: String
@available(OSX 10.7, *)
class NSDraggingImageComponent : NSObject {
  init(key key: String)
  var key: String
  var contents: AnyObject?
  var frame: NSRect
}
@available(OSX 10.7, *)
class NSDraggingItem : NSObject {
  init(pasteboardWriter pasteboardWriter: NSPasteboardWriting)
  var item: AnyObject { get }
  var draggingFrame: NSRect
  var imageComponentsProvider: (() -> [NSDraggingImageComponent])?
  func setDraggingFrame(_ frame: NSRect, contents contents: AnyObject)
  var imageComponents: [NSDraggingImageComponent]? { get }
}
