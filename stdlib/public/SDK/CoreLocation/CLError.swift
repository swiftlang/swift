//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

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
