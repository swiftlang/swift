
@available(watchOS 20000, *)
class HMCharacteristicWriteAction<TargetValueType : NSCopying> : HMAction {
  var characteristic: HMCharacteristic { get }
  @NSCopying var targetValue: TargetValueType { get }
}
