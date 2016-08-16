@_exported import CoreLocation
import Foundation

#if os(iOS)
extension CLError {
  /// In a regionMonitoringResponseDelayed error, the region that the
  /// location services can more effectively monitor.
  public var alternateRegion: CLRegion? {
    return userInfo[kCLErrorUserInfoAlternateRegionKey] as? CLRegion
  }
}
#endif
