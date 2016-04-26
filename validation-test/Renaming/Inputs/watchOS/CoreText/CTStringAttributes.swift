
@available(watchOS 2.0, *)
let kCTFontAttributeName: CFString
@available(watchOS 2.0, *)
let kCTForegroundColorFromContextAttributeName: CFString
@available(watchOS 2.0, *)
let kCTKernAttributeName: CFString
@available(watchOS 2.0, *)
let kCTLigatureAttributeName: CFString
@available(watchOS 2.0, *)
let kCTForegroundColorAttributeName: CFString
@available(watchOS 2.0, *)
let kCTParagraphStyleAttributeName: CFString
@available(watchOS 2.0, *)
let kCTStrokeWidthAttributeName: CFString
@available(watchOS 2.0, *)
let kCTStrokeColorAttributeName: CFString
@available(watchOS 2.0, *)
let kCTUnderlineStyleAttributeName: CFString
@available(watchOS 2.0, *)
let kCTSuperscriptAttributeName: CFString
@available(watchOS 2.0, *)
let kCTUnderlineColorAttributeName: CFString
@available(watchOS 2.0, *)
let kCTVerticalFormsAttributeName: CFString
@available(watchOS 2.0, *)
let kCTGlyphInfoAttributeName: CFString
@available(watchOS, introduced: 2.0, deprecated: 2.0)
let kCTCharacterShapeAttributeName: CFString
@available(watchOS 2.0, *)
let kCTLanguageAttributeName: CFString
@available(watchOS 2.0, *)
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
@available(watchOS 2.0, *)
let kCTBaselineClassAttributeName: CFString
@available(watchOS 2.0, *)
let kCTBaselineInfoAttributeName: CFString
@available(watchOS 2.0, *)
let kCTBaselineReferenceInfoAttributeName: CFString
@available(watchOS 2.0, *)
let kCTWritingDirectionAttributeName: CFString
var kCTWritingDirectionEmbedding: Int { get }
var kCTWritingDirectionOverride: Int { get }
@available(watchOS 2.0, *)
let kCTRubyAnnotationAttributeName: CFString
