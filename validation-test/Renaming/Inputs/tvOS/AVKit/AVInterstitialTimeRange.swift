
@available(tvOS 9.0, *)
class AVInterstitialTimeRange : NSObject, NSCopying, NSSecureCoding {
  init(timeRange timeRange: CMTimeRange)
  var timeRange: CMTimeRange { get }
}
