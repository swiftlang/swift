
@available(iOS 8.0, *)
class HMCharacteristicWriteAction<TargetValueType : NSCopying> : HMAction {
  init(characteristic characteristic: HMCharacteristic, targetValue targetValue: TargetValueType)
  var characteristic: HMCharacteristic { get }
  @NSCopying var targetValue: TargetValueType { get }
  func updateTargetValue(_ targetValue: TargetValueType, completionHandler completion: (NSError?) -> Void)
}
