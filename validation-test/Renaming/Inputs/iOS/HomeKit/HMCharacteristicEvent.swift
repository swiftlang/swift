
@available(iOS 9.0, *)
class HMCharacteristicEvent<TriggerValueType : NSCopying> : HMEvent {
  init(characteristic characteristic: HMCharacteristic, triggerValue triggerValue: TriggerValueType?)
  var characteristic: HMCharacteristic { get }
  @NSCopying var triggerValue: TriggerValueType? { get }
  func updateTriggerValue(_ triggerValue: TriggerValueType?, completionHandler completion: (NSError?) -> Void)
}
