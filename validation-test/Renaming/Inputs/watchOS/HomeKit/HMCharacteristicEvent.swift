
@available(watchOS 20000, *)
class HMCharacteristicEvent<TriggerValueType : NSCopying> : HMEvent {
  var characteristic: HMCharacteristic { get }
  @NSCopying var triggerValue: TriggerValueType? { get }
}
