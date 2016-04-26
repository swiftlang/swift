
@available(OSX 10.5, *)
protocol DOMXPathNSResolver : NSObjectProtocol {
  @discardableResult
  func lookupNamespaceURI(_ prefix: String!) -> String!
}
