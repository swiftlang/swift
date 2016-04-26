
extension NSValue {
  @available(iOS 4.0, *)
  /*not inherited*/ init(cmTime time: CMTime)
  @available(iOS 4.0, *)
  var cmTimeValue: CMTime { get }
  @available(iOS 4.0, *)
  /*not inherited*/ init(cmTimeRange timeRange: CMTimeRange)
  @available(iOS 4.0, *)
  var cmTimeRangeValue: CMTimeRange { get }
  @available(iOS 4.0, *)
  /*not inherited*/ init(cmTimeMapping timeMapping: CMTimeMapping)
  @available(iOS 4.0, *)
  var cmTimeMappingValue: CMTimeMapping { get }
}
extension NSCoder {
  @available(iOS 4.0, *)
  func encode(_ time: CMTime, forKey key: String)
  @available(iOS 4.0, *)
  @discardableResult
  func decodeCMTime(forKey key: String) -> CMTime
  @available(iOS 4.0, *)
  func encode(_ timeRange: CMTimeRange, forKey key: String)
  @available(iOS 4.0, *)
  @discardableResult
  func decodeCMTimeRange(forKey key: String) -> CMTimeRange
  @available(iOS 4.0, *)
  func encode(_ timeMapping: CMTimeMapping, forKey key: String)
  @available(iOS 4.0, *)
  @discardableResult
  func decodeCMTimeMapping(forKey key: String) -> CMTimeMapping
}
