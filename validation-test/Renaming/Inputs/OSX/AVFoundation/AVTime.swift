
extension NSValue {
  @available(OSX 10.7, *)
  /*not inherited*/ init(cmTime time: CMTime)
  @available(OSX 10.7, *)
  var cmTimeValue: CMTime { get }
  @available(OSX 10.7, *)
  /*not inherited*/ init(cmTimeRange timeRange: CMTimeRange)
  @available(OSX 10.7, *)
  var cmTimeRangeValue: CMTimeRange { get }
  @available(OSX 10.7, *)
  /*not inherited*/ init(cmTimeMapping timeMapping: CMTimeMapping)
  @available(OSX 10.7, *)
  var cmTimeMappingValue: CMTimeMapping { get }
}
extension NSCoder {
  @available(OSX 10.7, *)
  func encode(_ time: CMTime, forKey key: String)
  @available(OSX 10.7, *)
  @discardableResult
  func decodeCMTime(forKey key: String) -> CMTime
  @available(OSX 10.7, *)
  func encode(_ timeRange: CMTimeRange, forKey key: String)
  @available(OSX 10.7, *)
  @discardableResult
  func decodeCMTimeRange(forKey key: String) -> CMTimeRange
  @available(OSX 10.7, *)
  func encode(_ timeMapping: CMTimeMapping, forKey key: String)
  @available(OSX 10.7, *)
  @discardableResult
  func decodeCMTimeMapping(forKey key: String) -> CMTimeMapping
}
