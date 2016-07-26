
extension MPMediaItem {
  @available(tvOS 4.2, *)
  @discardableResult
  class func persistentIDProperty(forGroupingType groupingType: MPMediaGrouping) -> String
  @available(tvOS 4.2, *)
  @discardableResult
  class func titleProperty(forGroupingType groupingType: MPMediaGrouping) -> String
}
