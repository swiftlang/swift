
class CGColor {
}
extension CGColor {
  @available(OSX 10.3, *)
  /*not inherited*/ init?(withColorSpace space: CGColorSpace?, components components: UnsafePointer<CGFloat>?)
  @available(OSX 10.5, *)
  init(genericGray gray: CGFloat, alpha alpha: CGFloat)
  @available(OSX 10.5, *)
  init(genericRGBRed red: CGFloat, green green: CGFloat, blue blue: CGFloat, alpha alpha: CGFloat)
  @available(OSX 10.5, *)
  init(genericCMYKCyan cyan: CGFloat, magenta magenta: CGFloat, yellow yellow: CGFloat, black black: CGFloat, alpha alpha: CGFloat)
  @available(OSX 10.5, *)
  @discardableResult
  class func constantColorForName(_ colorName: CFString?) -> CGColor?
  @available(OSX 10.3, *)
  init?(withPatternSpace space: CGColorSpace?, pattern pattern: CGPattern?, components components: UnsafePointer<CGFloat>?)
  @available(OSX 10.3, *)
  init?(copy color: CGColor?)
  @available(OSX 10.3, *)
  init?(copyWithAlphaColor color: CGColor?, alpha alpha: CGFloat)
  @available(OSX 10.11, *)
  init?(copyByMatchingTo _: CGColorSpace?, intent intent: CGColorRenderingIntent, color color: CGColor?, options options: CFDictionary?)
  @available(OSX 10.3, *)
  @discardableResult
  func equalTo(_ color2: CGColor?) -> Bool
  @available(OSX 10.3, *)
  var numberOfComponents: Int { get }
  @available(OSX 10.3, *)
  var components: UnsafePointer<CGFloat>? { get }
  @available(OSX 10.3, *)
  var alpha: CGFloat { get }
  @available(OSX 10.3, *)
  var colorSpace: CGColorSpace? { get }
  @available(OSX 10.3, *)
  var pattern: CGPattern? { get }
  @available(OSX 10.3, *)
  class var typeID: CFTypeID { get }
  @available(OSX 10.5, *)
  class let white: CFString
  @available(OSX 10.5, *)
  class let black: CFString
  @available(OSX 10.5, *)
  class let clear: CFString
}
