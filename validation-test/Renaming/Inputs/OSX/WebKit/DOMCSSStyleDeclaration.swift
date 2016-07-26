
@available(OSX 10.4, *)
class DOMCSSStyleDeclaration : DOMObject {
  var cssText: String!
  var length: UInt32 { get }
  var parentRule: DOMCSSRule! { get }
  @discardableResult
  func getPropertyValue(_ propertyName: String!) -> String!
  @discardableResult
  func getPropertyCSSValue(_ propertyName: String!) -> DOMCSSValue!
  @discardableResult
  func removeProperty(_ propertyName: String!) -> String!
  @discardableResult
  func getPropertyPriority(_ propertyName: String!) -> String!
  @available(OSX 10.5, *)
  func setProperty(_ propertyName: String!, value value: String!, priority priority: String!)
  @discardableResult
  func item(_ index: UInt32) -> String!
  @available(OSX 10.5, *)
  @discardableResult
  func isPropertyImplicit(_ propertyName: String!) -> Bool
}
extension DOMCSSStyleDeclaration {
}
