
enum NSXMLDocumentContentKind : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case xmlKind
  case xhtmlKind
  case htmlKind
  case textKind
}
class NSXMLDocument : NSXMLNode {
  convenience init(xmlString string: String, options mask: Int) throws
  convenience init(contentsOf url: NSURL, options mask: Int) throws
  init(data data: NSData, options mask: Int) throws
  init(rootElement element: NSXMLElement?)
  @discardableResult
  class func replacementClass(for cls: AnyClass) -> AnyClass
  var characterEncoding: String?
  var version: String?
  var isStandalone: Bool
  var documentContentKind: NSXMLDocumentContentKind
  var mimeType: String?
  @NSCopying var dtd: NSXMLDTD?
  func setRootElement(_ root: NSXMLElement)
  @discardableResult
  func rootElement() -> NSXMLElement?
  func insertChild(_ child: NSXMLNode, at index: Int)
  func insertChildren(_ children: [NSXMLNode], at index: Int)
  func removeChild(at index: Int)
  func setChildren(_ children: [NSXMLNode]?)
  func addChild(_ child: NSXMLNode)
  func replaceChild(at index: Int, with node: NSXMLNode)
  @NSCopying var xmlData: NSData { get }
  @discardableResult
  func xmlData(withOptions options: Int) -> NSData
  @discardableResult
  func object(byApplyingXSLT xslt: NSData, arguments arguments: [String : String]?) throws -> AnyObject
  @discardableResult
  func object(byApplyingXSLTString xslt: String, arguments arguments: [String : String]?) throws -> AnyObject
  @discardableResult
  func objectByApplyingXSLT(at xsltURL: NSURL, arguments argument: [String : String]?) throws -> AnyObject
  func validate() throws
}
