
enum NSExpressionType : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case constantValueExpressionType
  case evaluatedObjectExpressionType
  case variableExpressionType
  case keyPathExpressionType
  case functionExpressionType
  @available(watchOS 2.0, *)
  case unionSetExpressionType
  @available(watchOS 2.0, *)
  case intersectSetExpressionType
  @available(watchOS 2.0, *)
  case minusSetExpressionType
  @available(watchOS 2.0, *)
  case subqueryExpressionType
  @available(watchOS 2.0, *)
  case aggregateExpressionType
  @available(watchOS 2.0, *)
  case anyKeyExpressionType
  case blockExpressionType
  @available(watchOS 2.0, *)
  case conditionalExpressionType
}
@available(watchOS 2.0, *)
class NSExpression : NSObject, NSSecureCoding, NSCopying {
  @available(watchOS 2.0, *)
  /*not inherited*/ init(format expressionFormat: String, argumentArray arguments: [AnyObject])
  @available(watchOS 2.0, *)
  /*not inherited*/ init(format expressionFormat: String, arguments argList: CVaListPointer)
  /*not inherited*/ init(forConstantValue obj: AnyObject?)
  @discardableResult
  class func expressionForEvaluatedObject() -> NSExpression
  /*not inherited*/ init(forVariable string: String)
  /*not inherited*/ init(forKeyPath keyPath: String)
  /*not inherited*/ init(forFunction name: String, arguments parameters: [AnyObject])
  @available(watchOS 2.0, *)
  /*not inherited*/ init(forAggregate subexpressions: [AnyObject])
  @available(watchOS 2.0, *)
  /*not inherited*/ init(forUnionSet left: NSExpression, with right: NSExpression)
  @available(watchOS 2.0, *)
  /*not inherited*/ init(forIntersectSet left: NSExpression, with right: NSExpression)
  @available(watchOS 2.0, *)
  /*not inherited*/ init(forMinusSet left: NSExpression, with right: NSExpression)
  @available(watchOS 2.0, *)
  /*not inherited*/ init(forSubquery expression: NSExpression, usingIteratorVariable variable: String, predicate predicate: AnyObject)
  @available(watchOS 2.0, *)
  /*not inherited*/ init(forFunction target: NSExpression, selectorName name: String, arguments parameters: [AnyObject]?)
  @available(watchOS 2.0, *)
  @discardableResult
  class func expressionForAnyKey() -> NSExpression
  @available(watchOS 2.0, *)
  /*not inherited*/ init(block block: (AnyObject?, [AnyObject], NSMutableDictionary?) -> AnyObject, arguments arguments: [NSExpression]?)
  @available(watchOS 2.0, *)
  /*not inherited*/ init(forConditional predicate: NSPredicate, trueExpression trueExpression: NSExpression, falseExpression falseExpression: NSExpression)
  init(expressionType type: NSExpressionType)
  var expressionType: NSExpressionType { get }
  var constantValue: AnyObject { get }
  var keyPath: String { get }
  var function: String { get }
  var variable: String { get }
  @NSCopying var operand: NSExpression { get }
  var arguments: [NSExpression]? { get }
  @available(watchOS 2.0, *)
  var collection: AnyObject { get }
  @available(watchOS 2.0, *)
  @NSCopying var predicate: NSPredicate { get }
  @available(watchOS 2.0, *)
  @NSCopying var left: NSExpression { get }
  @available(watchOS 2.0, *)
  @NSCopying var right: NSExpression { get }
  @available(watchOS 2.0, *)
  @NSCopying var trueExpression: NSExpression { get }
  @available(watchOS 2.0, *)
  @NSCopying var falseExpression: NSExpression { get }
  @available(watchOS 2.0, *)
  var expressionBlock: (AnyObject?, [AnyObject], NSMutableDictionary?) -> AnyObject { get }
  @discardableResult
  func expressionValue(with object: AnyObject?, context context: NSMutableDictionary?) -> AnyObject
  @available(watchOS 2.0, *)
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
