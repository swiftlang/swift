
enum NSDeleteRule : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case noActionDeleteRule
  case nullifyDeleteRule
  case cascadeDeleteRule
  case denyDeleteRule
}
@available(OSX 10.4, *)
class NSRelationshipDescription : NSPropertyDescription {
  unowned(unsafe) var destinationEntity: @sil_unmanaged NSEntityDescription?
  unowned(unsafe) var inverseRelationship: @sil_unmanaged NSRelationshipDescription?
  var maxCount: Int
  var minCount: Int
  var deleteRule: NSDeleteRule
  var isToMany: Bool { get }
  @available(OSX 10.7, *)
  var isOrdered: Bool
}
