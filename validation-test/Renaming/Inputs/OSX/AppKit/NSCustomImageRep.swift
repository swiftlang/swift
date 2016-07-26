
class NSCustomImageRep : NSImageRep {
  @available(OSX 10.8, *)
  init(size size: NSSize, flipped drawingHandlerShouldBeCalledWithFlippedContext: Bool, drawingHandler drawingHandler: (NSRect) -> Bool)
  @available(OSX 10.8, *)
  var drawingHandler: ((NSRect) -> Bool)? { get }
  init(draw aMethod: Selector, delegate anObject: AnyObject)
  var drawSelector: Selector? { get }
  unowned(unsafe) var delegate: @sil_unmanaged AnyObject? { get }
}
