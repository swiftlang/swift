
class CGContext {
}
enum CGPathDrawingMode : Int32 {
  init?(rawValue rawValue: Int32)
  var rawValue: Int32 { get }
  case fill
  case eoFill
  case stroke
  case fillStroke
  case eoFillStroke
}
enum CGTextDrawingMode : Int32 {
  init?(rawValue rawValue: Int32)
  var rawValue: Int32 { get }
  case fill
  case stroke
  case fillStroke
  case invisible
  case fillClip
  case strokeClip
  case fillStrokeClip
  case clip
}
@available(watchOS, introduced: 2.0, deprecated: 2.0)
enum CGTextEncoding : Int32 {
  init?(rawValue rawValue: Int32)
  var rawValue: Int32 { get }
  case encodingFontSpecific
  case encodingMacRoman
}
enum CGInterpolationQuality : Int32 {
  init?(rawValue rawValue: Int32)
  var rawValue: Int32 { get }
  case `default`
  case none
  case low
  case medium
  case high
}
enum CGBlendMode : Int32 {
  init?(rawValue rawValue: Int32)
  var rawValue: Int32 { get }
  case normal
  case multiply
  case screen
  case overlay
  case darken
  case lighten
  case colorDodge
  case colorBurn
  case softLight
  case hardLight
  case difference
  case exclusion
  case hue
  case saturation
  case color
  case luminosity
  case clear
  case copy
  case sourceIn
  case sourceOut
  case sourceAtop
  case destinationOver
  case destinationIn
  case destinationOut
  case destinationAtop
  case xor
  case plusDarker
  case plusLighter
}
extension CGContext {
  @available(watchOS 2.0, *)
  class var typeID: CFTypeID { get }
  @available(watchOS 2.0, *)
  func saveGState()
  @available(watchOS 2.0, *)
  func restoreGState()
  @available(watchOS 2.0, *)
  func scaleBy(x sx: CGFloat, y sy: CGFloat)
  @available(watchOS 2.0, *)
  func translateBy(x tx: CGFloat, y ty: CGFloat)
  @available(watchOS 2.0, *)
  func rotate(byAngle angle: CGFloat)
  @available(watchOS 2.0, *)
  func concatCTM(_ transform: CGAffineTransform)
  @available(watchOS 2.0, *)
  var ctm: CGAffineTransform { get }
  @available(watchOS 2.0, *)
  func setLineWidth(_ width: CGFloat)
  @available(watchOS 2.0, *)
  func setLineCap(_ cap: CGLineCap)
  @available(watchOS 2.0, *)
  func setLineJoin(_ join: CGLineJoin)
  @available(watchOS 2.0, *)
  func setMiterLimit(_ limit: CGFloat)
  @available(watchOS 2.0, *)
  func setLineDash(withPhase phase: CGFloat, lengths lengths: UnsafePointer<CGFloat>?, count count: Int)
  @available(watchOS 2.0, *)
  func setFlatness(_ flatness: CGFloat)
  @available(watchOS 2.0, *)
  func setAlpha(_ alpha: CGFloat)
  @available(watchOS 2.0, *)
  func setBlendMode(_ mode: CGBlendMode)
  @available(watchOS 2.0, *)
  func beginPath()
  @available(watchOS 2.0, *)
  func moveTo(x x: CGFloat, y y: CGFloat)
  @available(watchOS 2.0, *)
  func addLineTo(x x: CGFloat, y y: CGFloat)
  @available(watchOS 2.0, *)
  func addCurve(cp1x cp1x: CGFloat, cp1y cp1y: CGFloat, cp2x cp2x: CGFloat, cp2y cp2y: CGFloat, endingAtX x: CGFloat, y y: CGFloat)
  @available(watchOS 2.0, *)
  func addQuadCurve(cpx cpx: CGFloat, cpy cpy: CGFloat, endingAtX x: CGFloat, y y: CGFloat)
  @available(watchOS 2.0, *)
  func closePath()
  @available(watchOS 2.0, *)
  func addRect(_ rect: CGRect)
  @available(watchOS 2.0, *)
  func addRects(_ rects: UnsafePointer<CGRect>?, count count: Int)
  @available(watchOS 2.0, *)
  func addLines(between points: UnsafePointer<CGPoint>?, count count: Int)
  @available(watchOS 2.0, *)
  func addEllipseIn(_ rect: CGRect)
  @available(watchOS 2.0, *)
  func addArc(centeredAtX x: CGFloat, y y: CGFloat, radius radius: CGFloat, startAngle startAngle: CGFloat, endAngle endAngle: CGFloat, clockwise clockwise: Int32)
  @available(watchOS 2.0, *)
  func addArc(x1 x1: CGFloat, y1 y1: CGFloat, x2 x2: CGFloat, y2 y2: CGFloat, radius radius: CGFloat)
  @available(watchOS 2.0, *)
  func addPath(_ path: CGPath?)
  @available(watchOS 2.0, *)
  func replacePathWithStrokedPath()
  @available(watchOS 2.0, *)
  var isPathEmpty: Bool { get }
  @available(watchOS 2.0, *)
  var currentPointOfPath: CGPoint { get }
  @available(watchOS 2.0, *)
  var boundingBoxOfPath: CGRect { get }
  @available(watchOS 2.0, *)
  @discardableResult
  func copyPath() -> CGPath?
  @available(watchOS 2.0, *)
  @discardableResult
  func pathContains(_ point: CGPoint, mode mode: CGPathDrawingMode) -> Bool
  @available(watchOS 2.0, *)
  func drawPath(using mode: CGPathDrawingMode)
  @available(watchOS 2.0, *)
  func fillPath()
  @available(watchOS 2.0, *)
  func eoFillPath()
  @available(watchOS 2.0, *)
  func strokePath()
  @available(watchOS 2.0, *)
  func fill(_ rect: CGRect)
  @available(watchOS 2.0, *)
  func fill(_ rects: UnsafePointer<CGRect>?, count count: Int)
  @available(watchOS 2.0, *)
  func stroke(_ rect: CGRect)
  @available(watchOS 2.0, *)
  func stroke(_ rect: CGRect, width width: CGFloat)
  @available(watchOS 2.0, *)
  func clear(_ rect: CGRect)
  @available(watchOS 2.0, *)
  func fillEllipse(in rect: CGRect)
  @available(watchOS 2.0, *)
  func strokeEllipse(in rect: CGRect)
  @available(watchOS 2.0, *)
  func strokeLineSegments(between points: UnsafePointer<CGPoint>?, count count: Int)
  @available(watchOS 2.0, *)
  func clip()
  @available(watchOS 2.0, *)
  func eoClip()
  @available(watchOS 2.0, *)
  func clipToMask(_ rect: CGRect, mask mask: CGImage?)
  @available(watchOS 2.0, *)
  var boundingBoxOfClipPath: CGRect { get }
  @available(watchOS 2.0, *)
  func clip(to rect: CGRect)
  @available(watchOS 2.0, *)
  func clip(to rects: UnsafePointer<CGRect>, count count: Int)
  @available(watchOS 2.0, *)
  func setFillColor(_ color: CGColor?)
  @available(watchOS 2.0, *)
  func setStrokeColor(_ color: CGColor?)
  @available(watchOS 2.0, *)
  func setFillColorSpace(_ space: CGColorSpace?)
  @available(watchOS 2.0, *)
  func setStrokeColorSpace(_ space: CGColorSpace?)
  @available(watchOS 2.0, *)
  func setFillColor(withComponents components: UnsafePointer<CGFloat>?)
  @available(watchOS 2.0, *)
  func setStrokeColor(withComponents components: UnsafePointer<CGFloat>?)
  @available(watchOS 2.0, *)
  func setFillPattern(_ pattern: CGPattern?, colorComponents components: UnsafePointer<CGFloat>?)
  @available(watchOS 2.0, *)
  func setStrokePattern(_ pattern: CGPattern?, colorComponents components: UnsafePointer<CGFloat>?)
  @available(watchOS 2.0, *)
  func setPatternPhase(_ phase: CGSize)
  @available(watchOS 2.0, *)
  func setFillColor(withGray gray: CGFloat, alpha alpha: CGFloat)
  @available(watchOS 2.0, *)
  func setStrokeColor(withGray gray: CGFloat, alpha alpha: CGFloat)
  @available(watchOS 2.0, *)
  func setFillColor(withRed red: CGFloat, green green: CGFloat, blue blue: CGFloat, alpha alpha: CGFloat)
  @available(watchOS 2.0, *)
  func setStrokeColor(withRed red: CGFloat, green green: CGFloat, blue blue: CGFloat, alpha alpha: CGFloat)
  @available(watchOS 2.0, *)
  func setFillColor(withCyan cyan: CGFloat, magenta magenta: CGFloat, yellow yellow: CGFloat, black black: CGFloat, alpha alpha: CGFloat)
  @available(watchOS 2.0, *)
  func setStrokeColor(withCyan cyan: CGFloat, magenta magenta: CGFloat, yellow yellow: CGFloat, black black: CGFloat, alpha alpha: CGFloat)
  @available(watchOS 2.0, *)
  func setRenderingIntent(_ intent: CGColorRenderingIntent)
  @available(watchOS 2.0, *)
  func draw(in rect: CGRect, image image: CGImage?)
  @available(watchOS 2.0, *)
  func draw(in rect: CGRect, byTiling image: CGImage?)
  @available(watchOS 2.0, *)
  var interpolationQuality: CGInterpolationQuality
  @available(watchOS 2.0, *)
  func setShadow(withOffset offset: CGSize, blur blur: CGFloat, color color: CGColor?)
  @available(watchOS 2.0, *)
  func setShadow(withOffset offset: CGSize, blur blur: CGFloat)
  @available(watchOS 2.0, *)
  func drawLinearGradient(_ gradient: CGGradient?, start startPoint: CGPoint, end endPoint: CGPoint, options options: CGGradientDrawingOptions)
  @available(watchOS 2.0, *)
  func drawRadialGradient(_ gradient: CGGradient?, startCenter startCenter: CGPoint, startRadius startRadius: CGFloat, endCenter endCenter: CGPoint, endRadius endRadius: CGFloat, options options: CGGradientDrawingOptions)
  @available(watchOS 2.0, *)
  func drawShading(_ shading: CGShading?)
  @available(watchOS 2.0, *)
  func setCharacterSpacing(_ spacing: CGFloat)
  @available(watchOS 2.0, *)
  func setTextPosition(x x: CGFloat, y y: CGFloat)
  @available(watchOS 2.0, *)
  var textPosition: CGPoint { get }
  var textMatrix: CGAffineTransform
  @available(watchOS 2.0, *)
  func setTextDrawingMode(_ mode: CGTextDrawingMode)
  @available(watchOS 2.0, *)
  func setFont(_ font: CGFont?)
  @available(watchOS 2.0, *)
  func setFontSize(_ size: CGFloat)
  @available(watchOS 2.0, *)
  func showGlyphs(_ glyphs: UnsafePointer<CGGlyph>?, atPositions Lpositions: UnsafePointer<CGPoint>?, count count: Int)
  @available(watchOS 2.0, *)
  func drawPDFPage(_ page: CGPDFPage?)
  @available(watchOS 2.0, *)
  func beginPage(withMediaBox mediaBox: UnsafePointer<CGRect>?)
  @available(watchOS 2.0, *)
  func endPage()
  @available(watchOS 2.0, *)
  func flush()
  @available(watchOS 2.0, *)
  func synchronize()
  @available(watchOS 2.0, *)
  func setShouldAntialias(_ shouldAntialias: Bool)
  @available(watchOS 2.0, *)
  func setAllowsAntialiasing(_ allowsAntialiasing: Bool)
  @available(watchOS 2.0, *)
  func setShouldSmoothFonts(_ shouldSmoothFonts: Bool)
  @available(watchOS 2.0, *)
  func setAllowsFontSmoothing(_ allowsFontSmoothing: Bool)
  @available(watchOS 2.0, *)
  func setShouldSubpixelPositionFonts(_ shouldSubpixelPositionFonts: Bool)
  @available(watchOS 2.0, *)
  func setAllowsFontSubpixelPositioning(_ allowsFontSubpixelPositioning: Bool)
  @available(watchOS 2.0, *)
  func setShouldSubpixelQuantizeFonts(_ shouldSubpixelQuantizeFonts: Bool)
  @available(watchOS 2.0, *)
  func setAllowsFontSubpixelQuantization(_ allowsFontSubpixelQuantization: Bool)
  @available(watchOS 2.0, *)
  func beginTransparencyLayer(withAuxiliaryInfo auxiliaryInfo: CFDictionary?)
  @available(watchOS 2.0, *)
  func beginTransparencyLayer(in rect: CGRect, auxiliaryInfo auxInfo: CFDictionary?)
  @available(watchOS 2.0, *)
  func endTransparencyLayer()
  @available(watchOS 2.0, *)
  var userSpaceToDeviceSpaceTransform: CGAffineTransform { get }
  @available(watchOS 2.0, *)
  @discardableResult
  func convertPointToDeviceSpace(_ point: CGPoint) -> CGPoint
  @available(watchOS 2.0, *)
  @discardableResult
  func convertPointToUserSpace(_ point: CGPoint) -> CGPoint
  @available(watchOS 2.0, *)
  @discardableResult
  func convertSizeToDeviceSpace(_ size: CGSize) -> CGSize
  @available(watchOS 2.0, *)
  @discardableResult
  func convertSizeToUserSpace(_ size: CGSize) -> CGSize
  @available(watchOS 2.0, *)
  @discardableResult
  func convertRectToDeviceSpace(_ rect: CGRect) -> CGRect
  @available(watchOS 2.0, *)
  @discardableResult
  func convertRectToUserSpace(_ rect: CGRect) -> CGRect
  @available(watchOS, introduced: 2.0, deprecated: 2.0)
  func selectFont(name name: UnsafePointer<Int8>?, size size: CGFloat, textEncoding textEncoding: CGTextEncoding)
  @available(watchOS, introduced: 2.0, deprecated: 2.0)
  func showText(string string: UnsafePointer<Int8>?, length length: Int)
  @available(watchOS, introduced: 2.0, deprecated: 2.0)
  func showTextAtPoint(x x: CGFloat, y y: CGFloat, string string: UnsafePointer<Int8>?, length length: Int)
  @available(watchOS, introduced: 2.0, deprecated: 2.0)
  func showGlyphs(g g: UnsafePointer<CGGlyph>?, count count: Int)
  @available(watchOS, introduced: 2.0, deprecated: 2.0)
  func showGlyphsAtPoint(x x: CGFloat, y y: CGFloat, glyphs glyphs: UnsafePointer<CGGlyph>?, count count: Int)
  @available(watchOS, introduced: 2.0, deprecated: 2.0)
  func showGlyphsWithAdvances(glyphs glyphs: UnsafePointer<CGGlyph>?, advances advances: UnsafePointer<CGSize>?, count count: Int)
}
