
enum NSColorPanelMode : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  @available(OSX 10.5, *)
  case NSNoModeColorPanel
  case NSGrayModeColorPanel
  case NSRGBModeColorPanel
  case NSCMYKModeColorPanel
  case NSHSBModeColorPanel
  case NSCustomPaletteModeColorPanel
  case NSColorListModeColorPanel
  case NSWheelModeColorPanel
  case NSCrayonModeColorPanel
}
struct NSColorPanelOptions : OptionSet {
  init(rawValue rawValue: UInt)
  let rawValue: UInt
  static var grayModeMask: NSColorPanelOptions { get }
  static var rgbModeMask: NSColorPanelOptions { get }
  static var cmykModeMask: NSColorPanelOptions { get }
  static var hsbModeMask: NSColorPanelOptions { get }
  static var customPaletteModeMask: NSColorPanelOptions { get }
  static var colorListModeMask: NSColorPanelOptions { get }
  static var wheelModeMask: NSColorPanelOptions { get }
  static var crayonModeMask: NSColorPanelOptions { get }
  static var allModesMask: NSColorPanelOptions { get }
}
class NSColorPanel : NSPanel {
  @discardableResult
  class func shared() -> NSColorPanel
  @discardableResult
  class func sharedColorPanelExists() -> Bool
  @discardableResult
  class func drag(_ color: NSColor, with theEvent: NSEvent, from sourceView: NSView) -> Bool
  class func setPickerMask(_ mask: NSColorPanelOptions)
  class func setPickerMode(_ mode: NSColorPanelMode)
  var accessoryView: NSView?
  var isContinuous: Bool
  var showsAlpha: Bool
  var mode: NSColorPanelMode
  @NSCopying var color: NSColor
  var alpha: CGFloat { get }
  func setAction(_ aSelector: Selector?)
  func setTarget(_ anObject: AnyObject?)
  func attach(_ colorList: NSColorList)
  func detach(_ colorList: NSColorList)
}
extension NSApplication {
  func orderFrontColorPanel(_ sender: AnyObject?)
}
extension NSObject {
  class func changeColor(_ sender: AnyObject?)
  func changeColor(_ sender: AnyObject?)
}
let NSColorPanelColorDidChangeNotification: String
