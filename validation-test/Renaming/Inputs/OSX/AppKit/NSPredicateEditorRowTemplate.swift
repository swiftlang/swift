
@available(OSX 10.5, *)
class NSPredicateEditorRowTemplate : NSObject, NSCoding, NSCopying {
  @discardableResult
  func match(for predicate: NSPredicate) -> Double
  var templateViews: [NSView] { get }
  func setPredicate(_ predicate: NSPredicate)
  @discardableResult
  func predicate(withSubpredicates subpredicates: [NSPredicate]?) -> NSPredicate
  @discardableResult
  func displayableSubpredicates(of predicate: NSPredicate) -> [NSPredicate]?
  init(leftExpressions leftExpressions: [NSExpression], rightExpressions rightExpressions: [NSExpression], modifier modifier: NSComparisonPredicateModifier, operators operators: [NSNumber], options options: Int)
  init(leftExpressions leftExpressions: [NSExpression], rightExpressionAttributeType attributeType: NSAttributeType, modifier modifier: NSComparisonPredicateModifier, operators operators: [NSNumber], options options: Int)
  init(compoundTypes compoundTypes: [NSNumber])
  var leftExpressions: [NSExpression]? { get }
  var rightExpressions: [NSExpression]? { get }
  var rightExpressionAttributeType: NSAttributeType { get }
  var modifier: NSComparisonPredicateModifier { get }
  var operators: [NSNumber]? { get }
  var options: Int { get }
  var compoundTypes: [NSNumber]? { get }
  @discardableResult
  class func templates(withAttributeKeyPaths keyPaths: [String], in entityDescription: NSEntityDescription) -> [NSPredicateEditorRowTemplate]
}
