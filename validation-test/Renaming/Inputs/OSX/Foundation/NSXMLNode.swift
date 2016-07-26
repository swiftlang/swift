
enum NSXMLNodeKind : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case invalidKind
  case documentKind
  case elementKind
  case attributeKind
  case namespaceKind
  case processingInstructionKind
  case commentKind
  case textKind
  case DTDKind
  case entityDeclarationKind
  case attributeDeclarationKind
  case elementDeclarationKind
  case notationDeclarationKind
}
class NSXMLNode : NSObject, NSCopying {
  convenience init(kind kind: NSXMLNodeKind)
  init(kind kind: NSXMLNodeKind, options options: Int)
  @discardableResult
  class func document() -> AnyObject
  @discardableResult
  class func document(withRootElement element: NSXMLElement) -> AnyObject
  @discardableResult
  class func element(withName name: String) -> AnyObject
  @discardableResult
  class func element(withName name: String, uri URI: String) -> AnyObject
  @discardableResult
  class func element(withName name: String, stringValue string: String) -> AnyObject
  @discardableResult
  class func element(withName name: String, children children: [NSXMLNode]?, attributes attributes: [NSXMLNode]?) -> AnyObject
  @discardableResult
  class func attribute(withName name: String, stringValue stringValue: String) -> AnyObject
  @discardableResult
  class func attribute(withName name: String, uri URI: String, stringValue stringValue: String) -> AnyObject
  @discardableResult
  class func namespace(withName name: String, stringValue stringValue: String) -> AnyObject
  @discardableResult
  class func processingInstruction(withName name: String, stringValue stringValue: String) -> AnyObject
  @discardableResult
  class func comment(withStringValue stringValue: String) -> AnyObject
  @discardableResult
  class func text(withStringValue stringValue: String) -> AnyObject
  @discardableResult
  class func dtdNode(withXMLString string: String) -> AnyObject?
  var kind: NSXMLNodeKind { get }
  var name: String?
  var objectValue: AnyObject?
  var stringValue: String?
  func setStringValue(_ string: String, resolvingEntities resolve: Bool)
  var index: Int { get }
  var level: Int { get }
  var rootDocument: NSXMLDocument? { get }
  @NSCopying var parent: NSXMLNode? { get }
  var childCount: Int { get }
  var children: [NSXMLNode]? { get }
  @discardableResult
  func child(at index: Int) -> NSXMLNode?
  @NSCopying var previousSibling: NSXMLNode? { get }
  @NSCopying var nextSibling: NSXMLNode? { get }
  @NSCopying var previous: NSXMLNode? { get }
  @NSCopying var next: NSXMLNode? { get }
  func detach()
  var xPath: String? { get }
  var localName: String? { get }
  var prefix: String? { get }
  var uri: String?
  @discardableResult
  class func localName(forName name: String) -> String
  @discardableResult
  class func prefix(forName name: String) -> String?
  @discardableResult
  class func predefinedNamespace(forPrefix name: String) -> NSXMLNode?
  var xmlString: String { get }
  @discardableResult
  func xmlString(withOptions options: Int) -> String
  @discardableResult
  func canonicalXMLStringPreservingComments(_ comments: Bool) -> String
  @discardableResult
  func nodes(forXPath xpath: String) throws -> [NSXMLNode]
  @discardableResult
  func objects(forXQuery xquery: String, constants constants: [String : AnyObject]?) throws -> [AnyObject]
  @discardableResult
  func objects(forXQuery xquery: String) throws -> [AnyObject]
}
