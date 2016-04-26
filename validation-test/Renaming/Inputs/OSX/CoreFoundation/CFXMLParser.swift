
class CFXMLParser {
}
struct CFXMLParserOptions : OptionSet {
  init(rawValue rawValue: CFOptionFlags)
  let rawValue: CFOptionFlags
  static var validateDocument: CFXMLParserOptions { get }
  static var skipMetaData: CFXMLParserOptions { get }
  static var replacePhysicalEntities: CFXMLParserOptions { get }
  static var skipWhitespace: CFXMLParserOptions { get }
  static var resolveExternalEntities: CFXMLParserOptions { get }
  static var addImpliedAttributes: CFXMLParserOptions { get }
  static var allOptions: CFXMLParserOptions { get }
}
struct CFXMLParserStatusCode : OptionSet {
  init(rawValue rawValue: CFIndex)
  let rawValue: CFIndex
  static var statusParseNotBegun: CFXMLParserStatusCode { get }
  static var statusParseInProgress: CFXMLParserStatusCode { get }
  static var errorUnexpectedEOF: CFXMLParserStatusCode { get }
  static var errorUnknownEncoding: CFXMLParserStatusCode { get }
  static var errorEncodingConversionFailure: CFXMLParserStatusCode { get }
  static var errorMalformedProcessingInstruction: CFXMLParserStatusCode { get }
  static var errorMalformedDTD: CFXMLParserStatusCode { get }
  static var errorMalformedName: CFXMLParserStatusCode { get }
  static var errorMalformedCDSect: CFXMLParserStatusCode { get }
  static var errorMalformedCloseTag: CFXMLParserStatusCode { get }
  static var errorMalformedStartTag: CFXMLParserStatusCode { get }
  static var errorMalformedDocument: CFXMLParserStatusCode { get }
  static var errorElementlessDocument: CFXMLParserStatusCode { get }
  static var errorMalformedComment: CFXMLParserStatusCode { get }
  static var errorMalformedCharacterReference: CFXMLParserStatusCode { get }
  static var errorMalformedParsedCharacterData: CFXMLParserStatusCode { get }
  static var errorNoData: CFXMLParserStatusCode { get }
}
typealias CFXMLParserCreateXMLStructureCallBack = @convention(c) (CFXMLParser!, CFXMLNode!, UnsafeMutablePointer<Void>!) -> UnsafeMutablePointer<Void>!
typealias CFXMLParserAddChildCallBack = @convention(c) (CFXMLParser!, UnsafeMutablePointer<Void>!, UnsafeMutablePointer<Void>!, UnsafeMutablePointer<Void>!) -> Void
typealias CFXMLParserEndXMLStructureCallBack = @convention(c) (CFXMLParser!, UnsafeMutablePointer<Void>!, UnsafeMutablePointer<Void>!) -> Void
typealias CFXMLParserResolveExternalEntityCallBack = @convention(c) (CFXMLParser!, UnsafeMutablePointer<CFXMLExternalID>!, UnsafeMutablePointer<Void>!) -> Unmanaged<CFData>!
typealias CFXMLParserHandleErrorCallBack = @convention(c) (CFXMLParser!, CFXMLParserStatusCode, UnsafeMutablePointer<Void>!) -> DarwinBoolean
struct CFXMLParserCallBacks {
  var version: CFIndex
  var createXMLStructure: CFXMLParserCreateXMLStructureCallBack!
  var addChild: CFXMLParserAddChildCallBack!
  var endXMLStructure: CFXMLParserEndXMLStructureCallBack!
  var resolveExternalEntity: CFXMLParserResolveExternalEntityCallBack!
  var handleError: CFXMLParserHandleErrorCallBack!
  init()
  init(version version: CFIndex, createXMLStructure createXMLStructure: CFXMLParserCreateXMLStructureCallBack!, addChild addChild: CFXMLParserAddChildCallBack!, endXMLStructure endXMLStructure: CFXMLParserEndXMLStructureCallBack!, resolveExternalEntity resolveExternalEntity: CFXMLParserResolveExternalEntityCallBack!, handleError handleError: CFXMLParserHandleErrorCallBack!)
}
typealias CFXMLParserRetainCallBack = @convention(c) (UnsafePointer<Void>!) -> UnsafePointer<Void>!
typealias CFXMLParserReleaseCallBack = @convention(c) (UnsafePointer<Void>!) -> Void
typealias CFXMLParserCopyDescriptionCallBack = @convention(c) (UnsafePointer<Void>!) -> Unmanaged<CFString>!
struct CFXMLParserContext {
  var version: CFIndex
  var info: UnsafeMutablePointer<Void>!
  var retain: CFXMLParserRetainCallBack!
  var release: CFXMLParserReleaseCallBack!
  var copyDescription: CFXMLParserCopyDescriptionCallBack!
  init()
  init(version version: CFIndex, info info: UnsafeMutablePointer<Void>!, retain retain: CFXMLParserRetainCallBack!, release release: CFXMLParserReleaseCallBack!, copyDescription copyDescription: CFXMLParserCopyDescriptionCallBack!)
}
@discardableResult
func CFXMLCreateStringByEscapingEntities(_ allocator: CFAllocator!, _ string: CFString!, _ entitiesDictionary: CFDictionary!) -> CFString!
@discardableResult
func CFXMLCreateStringByUnescapingEntities(_ allocator: CFAllocator!, _ string: CFString!, _ entitiesDictionary: CFDictionary!) -> CFString!
let kCFXMLTreeErrorDescription: CFString!
let kCFXMLTreeErrorLineNumber: CFString!
let kCFXMLTreeErrorLocation: CFString!
let kCFXMLTreeErrorStatusCode: CFString!
