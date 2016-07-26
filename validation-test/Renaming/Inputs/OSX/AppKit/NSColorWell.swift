
class NSColorWell : NSControl {
  func deactivate()
  func activate(_ exclusive: Bool)
  var isActive: Bool { get }
  func draw(inside insideRect: NSRect)
  var isBordered: Bool
  func takeColorFrom(_ sender: AnyObject?)
  @NSCopying var color: NSColor
}
struct __cwFlags {
  var isActive: UInt32
  var isBordered: UInt32
  var cantDraw: UInt32
  var isNotContinuous: UInt32
  var refusesFR: UInt32
  var reservedColorWell: UInt32
  init()
  init(isActive isActive: UInt32, isBordered isBordered: UInt32, cantDraw cantDraw: UInt32, isNotContinuous isNotContinuous: UInt32, refusesFR refusesFR: UInt32, reservedColorWell reservedColorWell: UInt32)
}
