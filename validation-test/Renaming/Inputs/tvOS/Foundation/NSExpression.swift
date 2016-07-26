
enum NSExpressionType : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case constantValueExpressionType
  case evaluatedObjectExpressionType
  case variableExpressionType
  case keyPathExpressionType
  case functionExpressionType
  @available(tvOS 3.0, *)
  case unionSetExpressionType
  @available(tvOS 3.0, *)
  case intersectSetExpressionType
  @available(tvOS 3.0, *)
  case minusSetExpressionType
  @available(tvOS 3.0, *)
  case subqueryExpressionType
  @available(tvOS 3.0, *)
  case aggregateExpressionType
  @available(tvOS 7.0, *)
  case anyKeyExpressionType
  case blockExpressionType
  @available(tvOS 9.0, *)
  case conditionalExpressionType
}
@available(tvOS 3.0, *)
class NSExpression : NSObject, NSSecureCoding, NSCopying {
  @available(tvOS 4.0, *)
  /*not inherited*/ init(format expressionFormat: String, argumentArray arguments: [AnyObject])
  @available(tvOS 4.0, *)
  /*not inherited*/ init(format expressionFormat: String, arguments argList: CVaListPointer)
  /*not inherited*/ init(forConstantValue obj: AnyObject?)
  @discardableResult
  class func expressionForEvaluatedObject() -> NSExpression
  /*not inherited*/ init(forVariable string: String)
  /*not inherited*/ init(forKeyPath keyPath: String)
  /*not inherited*/ init(forFunction name: String, arguments parameters: [AnyObject])
  @available(tvOS 3.0, *)
  /*not inherited*/ init(forAggregate subexpressions: [AnyObject])
  @available(tvOS 3.0, *)
  /*not inherited*/ init(forUnionSet left: NSExpression, with right: NSExpression)
  @available(tvOS 3.0, *)
  /*not inherited*/ init(forIntersectSet left: NSExpression, with right: NSExpression)
  @available(tvOS 3.0, *)
  /*not inherited*/ init(forMinusSet left: NSExpression, with right: NSExpression)
  @available(tvOS 3.0, *)
  /*not inherited*/ init(forSubquery expression: NSExpression, usingIteratorVariable variable: String, predicate predicate: AnyObject)
  @available(tvOS 3.0, *)
  /*not inherited*/ init(forFunction target: NSExpression, selectorName name: String, arguments parameters: [AnyObject]?)
  @available(tvOS 7.0, *)
  @discardableResult
  class func expressionForAnyKey() -> NSExpression
  @available(tvOS 4.0, *)
  /*not inherited*/ init(block block: (AnyObject?, [AnyObject], NSMutableDictionary?) -> AnyObject, arguments arguments: [NSExpression]?)
  @available(tvOS 9.0, *)
  /*not inherited*/ init(forConditional predicate: NSPredicate, trueExpression trueExpression: NSExpression, falseExpression falseExpression: NSExpression)
  init(expressionType type: NSExpressionType)
  var expressionType: NSExpressionType { get }
  var constantValue: AnyObject { get }
  var keyPath: String { get }
  var function: String { get }
  var variable: String { get }
  @NSCopying var operand: NSExpression { get }
  var arguments: [NSExpression]? { get }
  @available(tvOS 3.0, *)
  var collection: AnyObject { get }
  @available(tvOS 3.0, *)
  @NSCopying var predicate: NSPredicate { get }
  @available(tvOS 3.0, *)
  @NSCopying var left: NSExpression { get }
  @available(tvOS 3.0, *)
  @NSCopying var right: NSExpression { get }
  @available(tvOS 9.0, *)
  @NSCopying var trueExpression: NSExpression { get }
  @available(tvOS 9.0, *)
  @NSCopying var falseExpression: NSExpression { get }
  @available(tvOS 4.0, *)
  var expressionBlock: (AnyObject?, [AnyObject], NSMutableDictionary?) -> AnyObject { get }
  @discardableResult
  func expressionValue(with object: AnyObject?, context context: NSMutableDictionary?) -> AnyObject
  @available(tvOS 7.0, *)
  func allowEvaluation()
}

extension NSExpression {
  convenience init(format expressionFormat: String, _ args: CVarArg...)
}
struct _expressionFlags {
  var _evaluationBlocked: UInt32
  var _reservedExpressionFlags: UInt32
  init()
  init(_evaluationBlocked _evaluationBlocked: UInt32, _reservedExpressionFlags _reservedExpressionFlags: UInt32)
}
