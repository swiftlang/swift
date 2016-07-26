
enum NSExpressionType : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case constantValueExpressionType
  case evaluatedObjectExpressionType
  case variableExpressionType
  case keyPathExpressionType
  case functionExpressionType
  @available(OSX 10.5, *)
  case unionSetExpressionType
  @available(OSX 10.5, *)
  case intersectSetExpressionType
  @available(OSX 10.5, *)
  case minusSetExpressionType
  @available(OSX 10.5, *)
  case subqueryExpressionType
  @available(OSX 10.5, *)
  case aggregateExpressionType
  @available(OSX 10.9, *)
  case anyKeyExpressionType
  case blockExpressionType
  @available(OSX 10.11, *)
  case conditionalExpressionType
}
@available(OSX 10.4, *)
class NSExpression : NSObject, NSSecureCoding, NSCopying {
  @available(OSX 10.6, *)
  /*not inherited*/ init(format expressionFormat: String, argumentArray arguments: [AnyObject])
  @available(OSX 10.6, *)
  /*not inherited*/ init(format expressionFormat: String, arguments argList: CVaListPointer)
  /*not inherited*/ init(forConstantValue obj: AnyObject?)
  @discardableResult
  class func expressionForEvaluatedObject() -> NSExpression
  /*not inherited*/ init(forVariable string: String)
  /*not inherited*/ init(forKeyPath keyPath: String)
  /*not inherited*/ init(forFunction name: String, arguments parameters: [AnyObject])
  @available(OSX 10.5, *)
  /*not inherited*/ init(forAggregate subexpressions: [AnyObject])
  @available(OSX 10.5, *)
  /*not inherited*/ init(forUnionSet left: NSExpression, with right: NSExpression)
  @available(OSX 10.5, *)
  /*not inherited*/ init(forIntersectSet left: NSExpression, with right: NSExpression)
  @available(OSX 10.5, *)
  /*not inherited*/ init(forMinusSet left: NSExpression, with right: NSExpression)
  @available(OSX 10.5, *)
  /*not inherited*/ init(forSubquery expression: NSExpression, usingIteratorVariable variable: String, predicate predicate: AnyObject)
  @available(OSX 10.5, *)
  /*not inherited*/ init(forFunction target: NSExpression, selectorName name: String, arguments parameters: [AnyObject]?)
  @available(OSX 10.9, *)
  @discardableResult
  class func expressionForAnyKey() -> NSExpression
  @available(OSX 10.6, *)
  /*not inherited*/ init(block block: (AnyObject?, [AnyObject], NSMutableDictionary?) -> AnyObject, arguments arguments: [NSExpression]?)
  @available(OSX 10.11, *)
  /*not inherited*/ init(forConditional predicate: NSPredicate, trueExpression trueExpression: NSExpression, falseExpression falseExpression: NSExpression)
  init(expressionType type: NSExpressionType)
  var expressionType: NSExpressionType { get }
  var constantValue: AnyObject { get }
  var keyPath: String { get }
  var function: String { get }
  var variable: String { get }
  @NSCopying var operand: NSExpression { get }
  var arguments: [NSExpression]? { get }
  @available(OSX 10.5, *)
  var collection: AnyObject { get }
  @available(OSX 10.5, *)
  @NSCopying var predicate: NSPredicate { get }
  @available(OSX 10.5, *)
  @NSCopying var left: NSExpression { get }
  @available(OSX 10.5, *)
  @NSCopying var right: NSExpression { get }
  @available(OSX 10.11, *)
  @NSCopying var trueExpression: NSExpression { get }
  @available(OSX 10.11, *)
  @NSCopying var falseExpression: NSExpression { get }
  @available(OSX 10.6, *)
  var expressionBlock: (AnyObject?, [AnyObject], NSMutableDictionary?) -> AnyObject { get }
  @discardableResult
  func expressionValue(with object: AnyObject?, context context: NSMutableDictionary?) -> AnyObject
  @available(OSX 10.9, *)
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
