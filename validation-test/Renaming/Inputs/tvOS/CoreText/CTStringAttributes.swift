
@available(tvOS 3.2, *)
let kCTFontAttributeName: CFString
@available(tvOS 3.2, *)
let kCTForegroundColorFromContextAttributeName: CFString
@available(tvOS 3.2, *)
let kCTKernAttributeName: CFString
@available(tvOS 3.2, *)
let kCTLigatureAttributeName: CFString
@available(tvOS 3.2, *)
let kCTForegroundColorAttributeName: CFString
@available(tvOS 3.2, *)
let kCTParagraphStyleAttributeName: CFString
@available(tvOS 3.2, *)
let kCTStrokeWidthAttributeName: CFString
@available(tvOS 3.2, *)
let kCTStrokeColorAttributeName: CFString
@available(tvOS 3.2, *)
let kCTUnderlineStyleAttributeName: CFString
@available(tvOS 3.2, *)
let kCTSuperscriptAttributeName: CFString
@available(tvOS 3.2, *)
let kCTUnderlineColorAttributeName: CFString
@available(tvOS 4.3, *)
let kCTVerticalFormsAttributeName: CFString
@available(tvOS 3.2, *)
let kCTGlyphInfoAttributeName: CFString
@available(tvOS, introduced: 3.2, deprecated: 9.0)
let kCTCharacterShapeAttributeName: CFString
@available(tvOS 7.0, *)
let kCTLanguageAttributeName: CFString
@available(tvOS 3.2, *)
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
@available(tvOS 6.0, *)
let kCTBaselineClassAttributeName: CFString
@available(tvOS 6.0, *)
let kCTBaselineInfoAttributeName: CFString
@available(tvOS 6.0, *)
let kCTBaselineReferenceInfoAttributeName: CFString
@available(tvOS 6.0, *)
let kCTWritingDirectionAttributeName: CFString
var kCTWritingDirectionEmbedding: Int { get }
var kCTWritingDirectionOverride: Int { get }
@available(tvOS 8.0, *)
let kCTRubyAnnotationAttributeName: CFString
