
class NSXMLDTD : NSXMLNode {
  convenience init(contentsOf url: NSURL, options mask: Int) throws
  init(data data: NSData, options mask: Int) throws
  var publicID: String?
  var systemID: String?
  func insertChild(_ child: NSXMLNode, at index: Int)
  func insertChildren(_ children: [NSXMLNode], at index: Int)
  func removeChild(at index: Int)
  func setChildren(_ children: [NSXMLNode]?)
  func addChild(_ child: NSXMLNode)
  func replaceChild(at index: Int, with node: NSXMLNode)
  @discardableResult
  func entityDeclaration(forName name: String) -> NSXMLDTDNode?
  @discardableResult
  func notationDeclaration(forName name: String) -> NSXMLDTDNode?
  @discardableResult
  func elementDeclaration(forName name: String) -> NSXMLDTDNode?
  @discardableResult
  func attributeDeclaration(forName name: String, elementName elementName: String) -> NSXMLDTDNode?
  @discardableResult
  class func predefinedEntityDeclaration(forName name: String) -> NSXMLDTDNode?
}
