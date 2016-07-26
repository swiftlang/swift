
@available(tvOS 8.0, *)
enum NSXMLParserExternalEntityResolvingPolicy : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case resolveExternalEntitiesNever
  case resolveExternalEntitiesNoNetwork
  case resolveExternalEntitiesSameOriginOnly
  case resolveExternalEntitiesAlways
}
class NSXMLParser : NSObject {
  convenience init?(contentsOf url: NSURL)
  init(data data: NSData)
  @available(tvOS 5.0, *)
  convenience init(stream stream: NSInputStream)
  unowned(unsafe) var delegate: @sil_unmanaged NSXMLParserDelegate?
  var shouldProcessNamespaces: Bool
  var shouldReportNamespacePrefixes: Bool
  @available(tvOS 8.0, *)
  var externalEntityResolvingPolicy: NSXMLParserExternalEntityResolvingPolicy
  @available(tvOS 8.0, *)
  var allowedExternalEntityURLs: Set<NSURL>?
  @discardableResult
  func parse() -> Bool
  func abortParsing()
  @NSCopying var parserError: NSError? { get }
  var shouldResolveExternalEntities: Bool
}
extension NSXMLParser {
  var publicID: String? { get }
  var systemID: String? { get }
  var lineNumber: Int { get }
  var columnNumber: Int { get }
}
protocol NSXMLParserDelegate : NSObjectProtocol {
  optional func parserDidStartDocument(_ parser: NSXMLParser)
  optional func parserDidEndDocument(_ parser: NSXMLParser)
  optional func parser(_ parser: NSXMLParser, foundNotationDeclarationWithName name: String, publicID publicID: String?, systemID systemID: String?)
  optional func parser(_ parser: NSXMLParser, foundUnparsedEntityDeclarationWithName name: String, publicID publicID: String?, systemID systemID: String?, notationName notationName: String?)
  optional func parser(_ parser: NSXMLParser, foundAttributeDeclarationWithName attributeName: String, forElement elementName: String, type type: String?, defaultValue defaultValue: String?)
  optional func parser(_ parser: NSXMLParser, foundElementDeclarationWithName elementName: String, model model: String)
  optional func parser(_ parser: NSXMLParser, foundInternalEntityDeclarationWithName name: String, value value: String?)
  optional func parser(_ parser: NSXMLParser, foundExternalEntityDeclarationWithName name: String, publicID publicID: String?, systemID systemID: String?)
  optional func parser(_ parser: NSXMLParser, didStartElement elementName: String, namespaceURI namespaceURI: String?, qualifiedName qName: String?, attributes attributeDict: [String : String] = [:])
  optional func parser(_ parser: NSXMLParser, didEndElement elementName: String, namespaceURI namespaceURI: String?, qualifiedName qName: String?)
  optional func parser(_ parser: NSXMLParser, didStartMappingPrefix prefix: String, toURI namespaceURI: String)
  optional func parser(_ parser: NSXMLParser, didEndMappingPrefix prefix: String)
  optional func parser(_ parser: NSXMLParser, foundCharacters string: String)
  optional func parser(_ parser: NSXMLParser, foundIgnorableWhitespace whitespaceString: String)
  optional func parser(_ parser: NSXMLParser, foundProcessingInstructionWithTarget target: String, data data: String?)
  optional func parser(_ parser: NSXMLParser, foundComment comment: String)
  optional func parser(_ parser: NSXMLParser, foundCDATA CDATABlock: NSData)
  @discardableResult
  optional func parser(_ parser: NSXMLParser, resolveExternalEntityName name: String, systemID systemID: String?) -> NSData?
  optional func parser(_ parser: NSXMLParser, parseErrorOccurred parseError: NSError)
  optional func parser(_ parser: NSXMLParser, validationErrorOccurred validationError: NSError)
}
@available(tvOS 2.0, *)
let NSXMLParserErrorDomain: String
enum NSXMLParserError : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case internalError
  case outOfMemoryError
  case documentStartError
  case emptyDocumentError
  case prematureDocumentEndError
  case invalidHexCharacterRefError
  case invalidDecimalCharacterRefError
  case invalidCharacterRefError
  case invalidCharacterError
  case characterRefAtEOFError
  case characterRefInPrologError
  case characterRefInEpilogError
  case characterRefInDTDError
  case entityRefAtEOFError
  case entityRefInPrologError
  case entityRefInEpilogError
  case entityRefInDTDError
  case parsedEntityRefAtEOFError
  case parsedEntityRefInPrologError
  case parsedEntityRefInEpilogError
  case parsedEntityRefInInternalSubsetError
  case entityReferenceWithoutNameError
  case entityReferenceMissingSemiError
  case parsedEntityRefNoNameError
  case parsedEntityRefMissingSemiError
  case undeclaredEntityError
  case unparsedEntityError
  case entityIsExternalError
  case entityIsParameterError
  case unknownEncodingError
  case encodingNotSupportedError
  case stringNotStartedError
  case stringNotClosedError
  case namespaceDeclarationError
  case entityNotStartedError
  case entityNotFinishedError
  case lessThanSymbolInAttributeError
  case attributeNotStartedError
  case attributeNotFinishedError
  case attributeHasNoValueError
  case attributeRedefinedError
  case literalNotStartedError
  case literalNotFinishedError
  case commentNotFinishedError
  case processingInstructionNotStartedError
  case processingInstructionNotFinishedError
  case notationNotStartedError
  case notationNotFinishedError
  case attributeListNotStartedError
  case attributeListNotFinishedError
  case mixedContentDeclNotStartedError
  case mixedContentDeclNotFinishedError
  case elementContentDeclNotStartedError
  case elementContentDeclNotFinishedError
  case xmlDeclNotStartedError
  case xmlDeclNotFinishedError
  case conditionalSectionNotStartedError
  case conditionalSectionNotFinishedError
  case externalSubsetNotFinishedError
  case doctypeDeclNotFinishedError
  case misplacedCDATAEndStringError
  case cdataNotFinishedError
  case misplacedXMLDeclarationError
  case spaceRequiredError
  case separatorRequiredError
  case nmtokenRequiredError
  case nameRequiredError
  case pcdataRequiredError
  case uriRequiredError
  case publicIdentifierRequiredError
  case ltRequiredError
  case gtRequiredError
  case ltSlashRequiredError
  case equalExpectedError
  case tagNameMismatchError
  case unfinishedTagError
  case standaloneValueError
  case invalidEncodingNameError
  case commentContainsDoubleHyphenError
  case invalidEncodingError
  case externalStandaloneEntityError
  case invalidConditionalSectionError
  case entityValueRequiredError
  case notWellBalancedError
  case extraContentError
  case invalidCharacterInEntityError
  case parsedEntityRefInInternalError
  case entityRefLoopError
  case entityBoundaryError
  case invalidURIError
  case uriFragmentError
  case nodtdError
  case delegateAbortedParseError
}
