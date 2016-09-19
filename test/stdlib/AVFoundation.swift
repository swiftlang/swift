// RUN: %target-run-simple-swift
// REQUIRES: executable_test
// REQUIRES: objc_interop
// CoreMedia is not present on watchOS.
// UNSUPPORTED: OS=watchos

import CoreMedia
import AVFoundation
import StdlibUnittest
import StdlibUnittestFoundationExtras

var coreMedia = TestSuite("CoreMedia")

func equalCMTimeMappings(_ x: CMTimeMapping, _ y: CMTimeMapping) -> Bool {
  var xx = x, yy = y
  return memcmp(&xx, &yy, MemoryLayout<CMTimeMapping>.size) == 0
}

coreMedia.test("NSValue bridging") {
  let time1 = CMTimeMake(181, 60)
  expectBridgeToNSValue(time1,
                        nsValueInitializer: { NSValue(time: $0) },
                        nsValueGetter: { $0.timeValue },
                        equal: (==))
  let time2 = CMTimeMake(242, 60)
  let timeRange1 = CMTimeRangeFromTimeToTime(time1, time2)

  expectBridgeToNSValue(timeRange1,
                        nsValueInitializer: { NSValue(timeRange: $0) },
                        nsValueGetter: { $0.timeRangeValue },
                        equal: (==))

  let time3 = CMTimeMake(303, 60)
  let timeRange2 = CMTimeRangeFromTimeToTime(time2, time3)
  let timeMapping = CMTimeMapping(source: timeRange1, target: timeRange2)
  expectBridgeToNSValue(timeMapping,
                        nsValueInitializer: { NSValue(timeMapping: $0) },
                        nsValueGetter: { $0.timeMappingValue },
                        equal: equalCMTimeMappings)
}

runAllTests()
