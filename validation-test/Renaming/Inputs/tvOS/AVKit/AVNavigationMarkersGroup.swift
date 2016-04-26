
@available(tvOS 9.0, *)
class AVNavigationMarkersGroup : NSObject {
  init(title title: String?, timedNavigationMarkers navigationMarkers: [AVTimedMetadataGroup])
  init(title title: String?, dateRangeNavigationMarkers navigationMarkers: [AVDateRangeMetadataGroup])
  var title: String? { get }
  var timedNavigationMarkers: [AVTimedMetadataGroup]? { get }
  var dateRangeNavigationMarkers: [AVDateRangeMetadataGroup]? { get }
}
