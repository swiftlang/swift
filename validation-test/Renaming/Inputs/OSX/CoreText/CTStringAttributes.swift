
@available(OSX 10.5, *)
let kCTFontAttributeName: CFString
@available(OSX 10.5, *)
let kCTForegroundColorFromContextAttributeName: CFString
@available(OSX 10.5, *)
let kCTKernAttributeName: CFString
@available(OSX 10.5, *)
let kCTLigatureAttributeName: CFString
@available(OSX 10.5, *)
let kCTForegroundColorAttributeName: CFString
@available(OSX 10.5, *)
let kCTParagraphStyleAttributeName: CFString
@available(OSX 10.6, *)
let kCTStrokeWidthAttributeName: CFString
@available(OSX 10.6, *)
let kCTStrokeColorAttributeName: CFString
@available(OSX 10.5, *)
let kCTUnderlineStyleAttributeName: CFString
@available(OSX 10.5, *)
let kCTSuperscriptAttributeName: CFString
@available(OSX 10.5, *)
let kCTUnderlineColorAttributeName: CFString
@available(OSX 10.5, *)
let kCTVerticalFormsAttributeName: CFString
@available(OSX 10.5, *)
let kCTGlyphInfoAttributeName: CFString
@available(OSX, introduced: 10.5, deprecated: 10.11)
let kCTCharacterShapeAttributeName: CFString
@available(OSX 10.9, *)
let kCTLanguageAttributeName: CFString
@available(OSX 10.5, *)
let kCTRunDelegateAttributeName: CFString
struct CTUnderlineStyle : OptionSet {
  init(rawValue rawValue: Int32)
  let rawValue: Int32
  static var single: CTUnderlineStyle { get }
  static var thick: CTUnderlineStyle { get }
  static var double: CTUnderlineStyle { get }
}
struct CTUnderlineStyleModifiers : OptionSet {
  init(rawValue rawValue: Int32)
  let rawValue: Int32
  static var patternDot: CTUnderlineStyleModifiers { get }
  static var patternDash: CTUnderlineStyleModifiers { get }
  static var patternDashDot: CTUnderlineStyleModifiers { get }
  static var patternDashDotDot: CTUnderlineStyleModifiers { get }
}
@available(OSX 10.8, *)
let kCTBaselineClassAttributeName: CFString
@available(OSX 10.8, *)
let kCTBaselineInfoAttributeName: CFString
@available(OSX 10.8, *)
let kCTBaselineReferenceInfoAttributeName: CFString
@available(OSX 10.8, *)
let kCTWritingDirectionAttributeName: CFString
var kCTWritingDirectionEmbedding: Int { get }
var kCTWritingDirectionOverride: Int { get }
@available(OSX 10.10, *)
let kCTRubyAnnotationAttributeName: CFString
