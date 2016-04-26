
enum NSCompoundPredicateType : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case notPredicateType
  case andPredicateType
  case orPredicateType
}
@available(iOS 3.0, *)
class NSCompoundPredicate : NSPredicate {
  init(type type: NSCompoundPredicateType, subpredicates subpredicates: [NSPredicate])
  var compoundPredicateType: NSCompoundPredicateType { get }
  var subpredicates: [AnyObject] { get }
  /*not inherited*/ init(andPredicateWithSubpredicates subpredicates: [NSPredicate])
  /*not inherited*/ init(orPredicateWithSubpredicates subpredicates: [NSPredicate])
  /*not inherited*/ init(notPredicateWithSubpredicate predicate: NSPredicate)
}
