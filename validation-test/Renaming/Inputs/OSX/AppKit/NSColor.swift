
var NSAppKitVersionNumberWithPatternColorLeakFix: Double { get }
class NSColor : NSObject, NSCopying, NSSecureCoding, NSPasteboardReading, NSPasteboardWriting {
  /*not inherited*/ init(calibratedWhite white: CGFloat, alpha alpha: CGFloat)
  /*not inherited*/ init(calibratedHue hue: CGFloat, saturation saturation: CGFloat, brightness brightness: CGFloat, alpha alpha: CGFloat)
  /*not inherited*/ init(calibratedRed red: CGFloat, green green: CGFloat, blue blue: CGFloat, alpha alpha: CGFloat)
  /*not inherited*/ init(deviceWhite white: CGFloat, alpha alpha: CGFloat)
  /*not inherited*/ init(deviceHue hue: CGFloat, saturation saturation: CGFloat, brightness brightness: CGFloat, alpha alpha: CGFloat)
  /*not inherited*/ init(deviceRed red: CGFloat, green green: CGFloat, blue blue: CGFloat, alpha alpha: CGFloat)
  /*not inherited*/ init(deviceCyan cyan: CGFloat, magenta magenta: CGFloat, yellow yellow: CGFloat, black black: CGFloat, alpha alpha: CGFloat)
  /*not inherited*/ init?(catalogName listName: String, colorName colorName: String)
  /*not inherited*/ init(colorSpace space: NSColorSpace, components components: UnsafePointer<CGFloat>, count numberOfComponents: Int)
  @available(OSX 10.7, *)
  /*not inherited*/ init(genericGamma22White white: CGFloat, alpha alpha: CGFloat)
  @available(OSX 10.7, *)
  /*not inherited*/ init(srgbRed red: CGFloat, green green: CGFloat, blue blue: CGFloat, alpha alpha: CGFloat)
  @available(OSX 10.9, *)
  /*not inherited*/ init(white white: CGFloat, alpha alpha: CGFloat)
  @available(OSX 10.9, *)
  /*not inherited*/ init(red red: CGFloat, green green: CGFloat, blue blue: CGFloat, alpha alpha: CGFloat)
  @available(OSX 10.9, *)
  /*not inherited*/ init(hue hue: CGFloat, saturation saturation: CGFloat, brightness brightness: CGFloat, alpha alpha: CGFloat)
  @discardableResult
  class func black() -> NSColor
  @discardableResult
  class func darkGray() -> NSColor
  @discardableResult
  class func lightGray() -> NSColor
  @discardableResult
  class func white() -> NSColor
  @discardableResult
  class func gray() -> NSColor
  @discardableResult
  class func red() -> NSColor
  @discardableResult
  class func green() -> NSColor
  @discardableResult
  class func blue() -> NSColor
  @discardableResult
  class func cyan() -> NSColor
  @discardableResult
  class func yellow() -> NSColor
  @discardableResult
  class func magenta() -> NSColor
  @discardableResult
  class func orange() -> NSColor
  @discardableResult
  class func purple() -> NSColor
  @discardableResult
  class func brown() -> NSColor
  @discardableResult
  class func clear() -> NSColor
  @discardableResult
  class func controlShadow() -> NSColor
  @discardableResult
  class func controlDarkShadow() -> NSColor
  @discardableResult
  class func control() -> NSColor
  @discardableResult
  class func controlHighlight() -> NSColor
  @discardableResult
  class func controlLightHighlight() -> NSColor
  @discardableResult
  class func controlText() -> NSColor
  @discardableResult
  class func controlBackground() -> NSColor
  @discardableResult
  class func selectedControl() -> NSColor
  @discardableResult
  class func secondarySelectedControl() -> NSColor
  @discardableResult
  class func selectedControlText() -> NSColor
  @discardableResult
  class func disabledControlText() -> NSColor
  @discardableResult
  class func text() -> NSColor
  @discardableResult
  class func textBackground() -> NSColor
  @discardableResult
  class func selectedText() -> NSColor
  @discardableResult
  class func selectedTextBackground() -> NSColor
  @discardableResult
  class func grid() -> NSColor
  @discardableResult
  class func keyboardFocusIndicator() -> NSColor
  @discardableResult
  class func windowBackground() -> NSColor
  @available(OSX 10.8, *)
  @discardableResult
  class func underPageBackground() -> NSColor
  @available(OSX 10.10, *)
  @discardableResult
  class func label() -> NSColor
  @available(OSX 10.10, *)
  @discardableResult
  class func secondaryLabel() -> NSColor
  @available(OSX 10.10, *)
  @discardableResult
  class func tertiaryLabel() -> NSColor
  @available(OSX 10.10, *)
  @discardableResult
  class func quaternaryLabel() -> NSColor
  @discardableResult
  class func scrollBar() -> NSColor
  @discardableResult
  class func knob() -> NSColor
  @discardableResult
  class func selectedKnob() -> NSColor
  @discardableResult
  class func windowFrame() -> NSColor
  @discardableResult
  class func windowFrameText() -> NSColor
  @discardableResult
  class func selectedMenuItem() -> NSColor
  @discardableResult
  class func selectedMenuItemText() -> NSColor
  @discardableResult
  class func highlight() -> NSColor
  @discardableResult
  class func shadow() -> NSColor
  @discardableResult
  class func header() -> NSColor
  @discardableResult
  class func headerText() -> NSColor
  @discardableResult
  class func alternateSelectedControl() -> NSColor
  @discardableResult
  class func alternateSelectedControlText() -> NSColor
  @discardableResult
  class func controlAlternatingRowBackgroundColors() -> [NSColor]
  @discardableResult
  func highlight(withLevel val: CGFloat) -> NSColor?
  @discardableResult
  func shadow(withLevel val: CGFloat) -> NSColor?
  /*not inherited*/ init(for controlTint: NSControlTint)
  @discardableResult
  class func currentControlTint() -> NSControlTint
  func set()
  func setFill()
  func setStroke()
  var colorSpaceName: String { get }
  @discardableResult
  func usingColorSpaceName(_ colorSpace: String) -> NSColor?
  @discardableResult
  func usingColorSpaceName(_ colorSpace: String?, device deviceDescription: [String : AnyObject]?) -> NSColor?
  @discardableResult
  func usingColorSpace(_ space: NSColorSpace) -> NSColor?
  @discardableResult
  func blendedColor(withFraction fraction: CGFloat, of color: NSColor) -> NSColor?
  @discardableResult
  func withAlphaComponent(_ alpha: CGFloat) -> NSColor
  var catalogNameComponent: String { get }
  var colorNameComponent: String { get }
  var localizedCatalogNameComponent: String { get }
  var localizedColorNameComponent: String { get }
  var redComponent: CGFloat { get }
  var greenComponent: CGFloat { get }
  var blueComponent: CGFloat { get }
  func getRed(_ red: UnsafeMutablePointer<CGFloat>?, green green: UnsafeMutablePointer<CGFloat>?, blue blue: UnsafeMutablePointer<CGFloat>?, alpha alpha: UnsafeMutablePointer<CGFloat>?)
  var hueComponent: CGFloat { get }
  var saturationComponent: CGFloat { get }
  var brightnessComponent: CGFloat { get }
  func getHue(_ hue: UnsafeMutablePointer<CGFloat>?, saturation saturation: UnsafeMutablePointer<CGFloat>?, brightness brightness: UnsafeMutablePointer<CGFloat>?, alpha alpha: UnsafeMutablePointer<CGFloat>?)
  var whiteComponent: CGFloat { get }
  func getWhite(_ white: UnsafeMutablePointer<CGFloat>?, alpha alpha: UnsafeMutablePointer<CGFloat>?)
  var cyanComponent: CGFloat { get }
  var magentaComponent: CGFloat { get }
  var yellowComponent: CGFloat { get }
  var blackComponent: CGFloat { get }
  func getCyan(_ cyan: UnsafeMutablePointer<CGFloat>?, magenta magenta: UnsafeMutablePointer<CGFloat>?, yellow yellow: UnsafeMutablePointer<CGFloat>?, black black: UnsafeMutablePointer<CGFloat>?, alpha alpha: UnsafeMutablePointer<CGFloat>?)
  var colorSpace: NSColorSpace { get }
  var numberOfComponents: Int { get }
  func getComponents(_ components: UnsafeMutablePointer<CGFloat>)
  var alphaComponent: CGFloat { get }
  /*not inherited*/ init?(from pasteBoard: NSPasteboard)
  func write(to pasteBoard: NSPasteboard)
  /*not inherited*/ init(patternImage image: NSImage)
  var patternImage: NSImage { get }
  func drawSwatch(in rect: NSRect)
  @available(OSX 10.8, *)
  /*not inherited*/ init?(cgColor cgColor: CGColor)
  @available(OSX 10.8, *)
  var cgColor: CGColor { get }
  class func setIgnoresAlpha(_ flag: Bool)
  @discardableResult
  class func ignoresAlpha() -> Bool
}

extension NSColor : _ColorLiteralConvertible {
}
extension NSColor {
  /*not inherited*/ init(ciColor color: CIColor)
}
extension CIColor {
  convenience init?(color color: NSColor)
}
extension NSCoder {
}
let NSSystemColorsDidChangeNotification: String
