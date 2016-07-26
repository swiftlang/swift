
@available(watchOS 20000, *)
class HMActionSet : NSObject {
  var name: String { get }
  var actions: Set<HMAction> { get }
  var isExecuting: Bool { get }
  @available(watchOS 2.0, *)
  var actionSetType: String { get }
  @available(watchOS 2.0, *)
  @NSCopying var uniqueIdentifier: NSUUID { get }
}
@available(watchOS 20000, *)
let HMActionSetTypeWakeUp: String
@available(watchOS 20000, *)
let HMActionSetTypeSleep: String
@available(watchOS 20000, *)
let HMActionSetTypeHomeDeparture: String
@available(watchOS 20000, *)
let HMActionSetTypeHomeArrival: String
@available(watchOS 20000, *)
let HMActionSetTypeUserDefined: String
