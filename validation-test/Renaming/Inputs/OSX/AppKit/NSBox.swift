
enum NSTitlePosition : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case noTitle
  case aboveTop
  case atTop
  case belowTop
  case aboveBottom
  case atBottom
  case belowBottom
}
enum NSBoxType : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case primary
  case secondary
  case separator
  case oldStyle
  @available(OSX 10.5, *)
  case custom
}
class NSBox : NSView {
  var borderType: NSBorderType
  var titlePosition: NSTitlePosition
  var boxType: NSBoxType
  var title: String
  var titleFont: NSFont
  var borderRect: NSRect { get }
  var titleRect: NSRect { get }
  var titleCell: AnyObject { get }
  func sizeToFit()
  var contentViewMargins: NSSize
  func setFrameFromContentFrame(_ contentFrame: NSRect)
  unowned(unsafe) var contentView: @sil_unmanaged NSView?
  @available(OSX 10.5, *)
  var isTransparent: Bool
  @available(OSX 10.5, *)
  var borderWidth: CGFloat
  @available(OSX 10.5, *)
  var cornerRadius: CGFloat
  @available(OSX 10.5, *)
  @NSCopying var borderColor: NSColor
  @available(OSX 10.5, *)
  @NSCopying var fillColor: NSColor
}
struct __bFlags {
  var borderType: NSBorderType
  var titlePosition: NSTitlePosition
  var backgroundTransparent: UInt32
  var orientation: UInt32
  var needsTile: UInt32
  var transparent: UInt32
  var colorAltInterpretation: UInt32
  var boxType: UInt32
  var useSuperAddSubview: UInt32
  var _RESERVED: UInt32
  init()
  init(borderType borderType: NSBorderType, titlePosition titlePosition: NSTitlePosition, backgroundTransparent backgroundTransparent: UInt32, orientation orientation: UInt32, needsTile needsTile: UInt32, transparent transparent: UInt32, colorAltInterpretation colorAltInterpretation: UInt32, boxType boxType: UInt32, useSuperAddSubview useSuperAddSubview: UInt32, _RESERVED _RESERVED: UInt32)
}
extension NSBox {
}
