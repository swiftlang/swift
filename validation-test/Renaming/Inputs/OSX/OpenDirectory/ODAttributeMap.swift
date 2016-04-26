
@available(OSX 10.9, *)
class ODAttributeMap : NSObject {
  @available(OSX 10.9, *)
  var customQueryFunction: String!
  @available(OSX 10.9, *)
  var customTranslationFunction: String!
  @available(OSX 10.9, *)
  var customAttributes: [AnyObject]!
  @available(OSX 10.9, *)
  var value: String!
  convenience init!(value value: String!)
  convenience init!(staticValue staticValue: String!)
  @available(OSX 10.9, *)
  func setStaticValue(_ staticValue: String!)
  @available(OSX 10.9, *)
  func setVariableSubstitution(_ variableSubstitution: String!)
}
