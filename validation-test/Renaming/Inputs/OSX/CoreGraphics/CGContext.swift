
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
  @available(OSX 10.2, *)
  class var typeID: CFTypeID { get }
  @available(OSX 10.0, *)
  func saveGState()
  @available(OSX 10.0, *)
  func restoreGState()
  @available(OSX 10.0, *)
  func scaleBy(x sx: CGFloat, y sy: CGFloat)
  @available(OSX 10.0, *)
  func translateBy(x tx: CGFloat, y ty: CGFloat)
  @available(OSX 10.0, *)
  func rotate(byAngle angle: CGFloat)
  @available(OSX 10.0, *)
  func concatCTM(_ transform: CGAffineTransform)
  @available(OSX 10.0, *)
  var ctm: CGAffineTransform { get }
  @available(OSX 10.0, *)
  func setLineWidth(_ width: CGFloat)
  @available(OSX 10.0, *)
  func setLineCap(_ cap: CGLineCap)
  @available(OSX 10.0, *)
  func setLineJoin(_ join: CGLineJoin)
  @available(OSX 10.0, *)
  func setMiterLimit(_ limit: CGFloat)
  @available(OSX 10.0, *)
  func setLineDash(withPhase phase: CGFloat, lengths lengths: UnsafePointer<CGFloat>?, count count: Int)
  @available(OSX 10.0, *)
  func setFlatness(_ flatness: CGFloat)
  @available(OSX 10.0, *)
  func setAlpha(_ alpha: CGFloat)
  @available(OSX 10.4, *)
  func setBlendMode(_ mode: CGBlendMode)
  @available(OSX 10.0, *)
  func beginPath()
  @available(OSX 10.0, *)
  func moveTo(x x: CGFloat, y y: CGFloat)
  @available(OSX 10.0, *)
  func addLineTo(x x: CGFloat, y y: CGFloat)
  @available(OSX 10.0, *)
  func addCurve(cp1x cp1x: CGFloat, cp1y cp1y: CGFloat, cp2x cp2x: CGFloat, cp2y cp2y: CGFloat, endingAtX x: CGFloat, y y: CGFloat)
  @available(OSX 10.0, *)
  func addQuadCurve(cpx cpx: CGFloat, cpy cpy: CGFloat, endingAtX x: CGFloat, y y: CGFloat)
  @available(OSX 10.0, *)
  func closePath()
  @available(OSX 10.0, *)
  func addRect(_ rect: CGRect)
  @available(OSX 10.0, *)
  func addRects(_ rects: UnsafePointer<CGRect>?, count count: Int)
  @available(OSX 10.0, *)
  func addLines(between points: UnsafePointer<CGPoint>?, count count: Int)
  @available(OSX 10.4, *)
  func addEllipseIn(_ rect: CGRect)
  @available(OSX 10.0, *)
  func addArc(centeredAtX x: CGFloat, y y: CGFloat, radius radius: CGFloat, startAngle startAngle: CGFloat, endAngle endAngle: CGFloat, clockwise clockwise: Int32)
  @available(OSX 10.0, *)
  func addArc(x1 x1: CGFloat, y1 y1: CGFloat, x2 x2: CGFloat, y2 y2: CGFloat, radius radius: CGFloat)
  @available(OSX 10.2, *)
  func addPath(_ path: CGPath?)
  @available(OSX 10.4, *)
  func replacePathWithStrokedPath()
  @available(OSX 10.0, *)
  var isPathEmpty: Bool { get }
  @available(OSX 10.0, *)
  var currentPointOfPath: CGPoint { get }
  @available(OSX 10.0, *)
  var boundingBoxOfPath: CGRect { get }
  @available(OSX 10.2, *)
  @discardableResult
  func copyPath() -> CGPath?
  @available(OSX 10.4, *)
  @discardableResult
  func pathContains(_ point: CGPoint, mode mode: CGPathDrawingMode) -> Bool
  @available(OSX 10.0, *)
  func drawPath(using mode: CGPathDrawingMode)
  @available(OSX 10.0, *)
  func fillPath()
  @available(OSX 10.0, *)
  func eoFillPath()
  @available(OSX 10.0, *)
  func strokePath()
  @available(OSX 10.0, *)
  func fill(_ rect: CGRect)
  @available(OSX 10.0, *)
  func fill(_ rects: UnsafePointer<CGRect>?, count count: Int)
  @available(OSX 10.0, *)
  func stroke(_ rect: CGRect)
  @available(OSX 10.0, *)
  func stroke(_ rect: CGRect, width width: CGFloat)
  @available(OSX 10.0, *)
  func clear(_ rect: CGRect)
  @available(OSX 10.4, *)
  func fillEllipse(in rect: CGRect)
  @available(OSX 10.4, *)
  func strokeEllipse(in rect: CGRect)
  @available(OSX 10.4, *)
  func strokeLineSegments(between points: UnsafePointer<CGPoint>?, count count: Int)
  @available(OSX 10.0, *)
  func clip()
  @available(OSX 10.0, *)
  func eoClip()
  @available(OSX 10.4, *)
  func clipToMask(_ rect: CGRect, mask mask: CGImage?)
  @available(OSX 10.3, *)
  var boundingBoxOfClipPath: CGRect { get }
  @available(OSX 10.0, *)
  func clip(to rect: CGRect)
  @available(OSX 10.0, *)
  func clip(to rects: UnsafePointer<CGRect>, count count: Int)
  @available(OSX 10.3, *)
  func setFillColor(_ color: CGColor?)
  @available(OSX 10.3, *)
  func setStrokeColor(_ color: CGColor?)
  @available(OSX 10.0, *)
  func setFillColorSpace(_ space: CGColorSpace?)
  @available(OSX 10.0, *)
  func setStrokeColorSpace(_ space: CGColorSpace?)
  @available(OSX 10.0, *)
  func setFillColor(withComponents components: UnsafePointer<CGFloat>?)
  @available(OSX 10.0, *)
  func setStrokeColor(withComponents components: UnsafePointer<CGFloat>?)
  @available(OSX 10.0, *)
  func setFillPattern(_ pattern: CGPattern?, colorComponents components: UnsafePointer<CGFloat>?)
  @available(OSX 10.0, *)
  func setStrokePattern(_ pattern: CGPattern?, colorComponents components: UnsafePointer<CGFloat>?)
  @available(OSX 10.0, *)
  func setPatternPhase(_ phase: CGSize)
  @available(OSX 10.0, *)
  func setFillColor(withGray gray: CGFloat, alpha alpha: CGFloat)
  @available(OSX 10.0, *)
  func setStrokeColor(withGray gray: CGFloat, alpha alpha: CGFloat)
  @available(OSX 10.0, *)
  func setFillColor(withRed red: CGFloat, green green: CGFloat, blue blue: CGFloat, alpha alpha: CGFloat)
  @available(OSX 10.0, *)
  func setStrokeColor(withRed red: CGFloat, green green: CGFloat, blue blue: CGFloat, alpha alpha: CGFloat)
  @available(OSX 10.0, *)
  func setFillColor(withCyan cyan: CGFloat, magenta magenta: CGFloat, yellow yellow: CGFloat, black black: CGFloat, alpha alpha: CGFloat)
  @available(OSX 10.0, *)
  func setStrokeColor(withCyan cyan: CGFloat, magenta magenta: CGFloat, yellow yellow: CGFloat, black black: CGFloat, alpha alpha: CGFloat)
  @available(OSX 10.0, *)
  func setRenderingIntent(_ intent: CGColorRenderingIntent)
  @available(OSX 10.0, *)
  func draw(in rect: CGRect, image image: CGImage?)
  @available(OSX 10.5, *)
  func draw(in rect: CGRect, byTiling image: CGImage?)
  @available(OSX 10.0, *)
  var interpolationQuality: CGInterpolationQuality
  @available(OSX 10.3, *)
  func setShadow(withOffset offset: CGSize, blur blur: CGFloat, color color: CGColor?)
  @available(OSX 10.3, *)
  func setShadow(withOffset offset: CGSize, blur blur: CGFloat)
  @available(OSX 10.5, *)
  func drawLinearGradient(_ gradient: CGGradient?, start startPoint: CGPoint, end endPoint: CGPoint, options options: CGGradientDrawingOptions)
  @available(OSX 10.5, *)
  func drawRadialGradient(_ gradient: CGGradient?, startCenter startCenter: CGPoint, startRadius startRadius: CGFloat, endCenter endCenter: CGPoint, endRadius endRadius: CGFloat, options options: CGGradientDrawingOptions)
  @available(OSX 10.2, *)
  func drawShading(_ shading: CGShading?)
  @available(OSX 10.0, *)
  func setCharacterSpacing(_ spacing: CGFloat)
  @available(OSX 10.0, *)
  func setTextPosition(x x: CGFloat, y y: CGFloat)
  @available(OSX 10.0, *)
  var textPosition: CGPoint { get }
  var textMatrix: CGAffineTransform
  @available(OSX 10.0, *)
  func setTextDrawingMode(_ mode: CGTextDrawingMode)
  @available(OSX 10.0, *)
  func setFont(_ font: CGFont?)
  @available(OSX 10.0, *)
  func setFontSize(_ size: CGFloat)
  @available(OSX 10.5, *)
  func showGlyphs(_ glyphs: UnsafePointer<CGGlyph>?, atPositions Lpositions: UnsafePointer<CGPoint>?, count count: Int)
  @available(OSX 10.3, *)
  func drawPDFPage(_ page: CGPDFPage?)
  @available(OSX 10.0, *)
  func beginPage(withMediaBox mediaBox: UnsafePointer<CGRect>?)
  @available(OSX 10.0, *)
  func endPage()
  @available(OSX 10.0, *)
  func flush()
  @available(OSX 10.0, *)
  func synchronize()
  @available(OSX 10.0, *)
  func setShouldAntialias(_ shouldAntialias: Bool)
  @available(OSX 10.4, *)
  func setAllowsAntialiasing(_ allowsAntialiasing: Bool)
  @available(OSX 10.2, *)
  func setShouldSmoothFonts(_ shouldSmoothFonts: Bool)
  @available(OSX 10.2, *)
  func setAllowsFontSmoothing(_ allowsFontSmoothing: Bool)
  @available(OSX 10.5, *)
  func setShouldSubpixelPositionFonts(_ shouldSubpixelPositionFonts: Bool)
  @available(OSX 10.5, *)
  func setAllowsFontSubpixelPositioning(_ allowsFontSubpixelPositioning: Bool)
  @available(OSX 10.5, *)
  func setShouldSubpixelQuantizeFonts(_ shouldSubpixelQuantizeFonts: Bool)
  @available(OSX 10.5, *)
  func setAllowsFontSubpixelQuantization(_ allowsFontSubpixelQuantization: Bool)
  @available(OSX 10.3, *)
  func beginTransparencyLayer(withAuxiliaryInfo auxiliaryInfo: CFDictionary?)
  @available(OSX 10.5, *)
  func beginTransparencyLayer(in rect: CGRect, auxiliaryInfo auxInfo: CFDictionary?)
  @available(OSX 10.3, *)
  func endTransparencyLayer()
  @available(OSX 10.4, *)
  var userSpaceToDeviceSpaceTransform: CGAffineTransform { get }
  @available(OSX 10.4, *)
  @discardableResult
  func convertPointToDeviceSpace(_ point: CGPoint) -> CGPoint
  @available(OSX 10.4, *)
  @discardableResult
  func convertPointToUserSpace(_ point: CGPoint) -> CGPoint
  @available(OSX 10.4, *)
  @discardableResult
  func convertSizeToDeviceSpace(_ size: CGSize) -> CGSize
  @available(OSX 10.4, *)
  @discardableResult
  func convertSizeToUserSpace(_ size: CGSize) -> CGSize
  @available(OSX 10.4, *)
  @discardableResult
  func convertRectToDeviceSpace(_ rect: CGRect) -> CGRect
  @available(OSX 10.4, *)
  @discardableResult
  func convertRectToUserSpace(_ rect: CGRect) -> CGRect
}
