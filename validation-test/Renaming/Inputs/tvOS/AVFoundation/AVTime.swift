
extension NSValue {
  @available(tvOS 4.0, *)
  /*not inherited*/ init(cmTime time: CMTime)
  @available(tvOS 4.0, *)
  var cmTimeValue: CMTime { get }
  @available(tvOS 4.0, *)
  /*not inherited*/ init(cmTimeRange timeRange: CMTimeRange)
  @available(tvOS 4.0, *)
  var cmTimeRangeValue: CMTimeRange { get }
  @available(tvOS 4.0, *)
  /*not inherited*/ init(cmTimeMapping timeMapping: CMTimeMapping)
  @available(tvOS 4.0, *)
  var cmTimeMappingValue: CMTimeMapping { get }
}
extension NSCoder {
  @available(tvOS 4.0, *)
  func encode(_ time: CMTime, forKey key: String)
  @available(tvOS 4.0, *)
  @discardableResult
  func decodeCMTime(forKey key: String) -> CMTime
  @available(tvOS 4.0, *)
  func encode(_ timeRange: CMTimeRange, forKey key: String)
  @available(tvOS 4.0, *)
  @discardableResult
  func decodeCMTimeRange(forKey key: String) -> CMTimeRange
  @available(tvOS 4.0, *)
  func encode(_ timeMapping: CMTimeMapping, forKey key: String)
  @available(tvOS 4.0, *)
  @discardableResult
  func decodeCMTimeMapping(forKey key: String) -> CMTimeMapping
}
