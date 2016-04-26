
class NSRulerMarker : NSObject, NSCopying, NSCoding {
  init(rulerView ruler: NSRulerView, markerLocation location: CGFloat, image image: NSImage, imageOrigin imageOrigin: NSPoint)
  unowned(unsafe) var ruler: @sil_unmanaged NSRulerView { get }
  var markerLocation: CGFloat
  var image: NSImage
  var imageOrigin: NSPoint
  var isMovable: Bool
  var isRemovable: Bool
  var isDragging: Bool { get }
  var representedObject: NSCopying?
  var imageRectInRuler: NSRect { get }
  var thicknessRequiredInRuler: CGFloat { get }
  func draw(_ rect: NSRect)
  @discardableResult
  func trackMouse(_ mouseDownEvent: NSEvent, adding isAdding: Bool) -> Bool
}
struct __rFlags {
  var movable: UInt32
  var removable: UInt32
  var dragging: UInt32
  var pinned: UInt32
  var _reserved: UInt32
  init()
  init(movable movable: UInt32, removable removable: UInt32, dragging dragging: UInt32, pinned pinned: UInt32, _reserved _reserved: UInt32)
}
