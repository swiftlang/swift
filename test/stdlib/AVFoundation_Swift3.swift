// RUN: %empty-directory(%t)
// RUN: %target-build-swift -swift-version 3 %s -o %t/a.out
// RUN: %target-run %t/a.out
// REQUIRES: objc_interop
// REQUIRES: executable_test
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


var AVFoundationTests = TestSuite("AVFoundation_Swift3")

let boxedPixelFormat = NSNumber(value: kCVPixelFormatType_420YpCbCr8BiPlanarFullRange)

#if os(macOS) || os(iOS)

if #available(iOS 5, *) {
  AVFoundationTests.test("AVCaptureVideoDataOutput.availableVideoCVPixelFormatTypes") {
    func f(v: AVCaptureVideoDataOutput) -> Bool {
      return v.availableVideoCVPixelFormatTypes.contains(where: { e in
        if let e = e as? NSNumber, e == boxedPixelFormat {
          return true
        }
        else {
          return false
        }
      })
    }
  }
}

#endif

#if os(iOS)

if #available(iOS 7, *) {
  AVFoundationTests.test("AVMetadataMachineReadableCodeObject.corners") {
    func f(m: AVMetadataMachineReadableCodeObject) -> [Any]! {
      return m.corners
    }
  }
}

if #available(iOS 10, *) {
  AVFoundationTests.test("AVCaptureDeviceFormat.supportedColorSpaces") {
    func f(df: AVCaptureDeviceFormat) -> Bool {
      return df.supportedColorSpaces.contains(NSNumber(value: AVCaptureColorSpace.sRGB.rawValue))
    }
  }

  AVFoundationTests.test("AVCapturePhotoOutput.supportedFlashModes") {
    func f(p: AVCapturePhotoOutput) -> Bool {
      return p.supportedFlashModes.contains(NSNumber(value: AVCaptureFlashMode.off.rawValue))
    }
  }

  AVFoundationTests.test("AVCapturePhotoOutput.availablePhotoPixelFormatTypes") {
    func f(p: AVCapturePhotoOutput) -> Bool {
      return p.availablePhotoPixelFormatTypes.contains(boxedPixelFormat)
    }
  }

  AVFoundationTests.test("AVCapturePhotoOutput.availableRawPhotoPixelFormatTypes") {
    func f(p: AVCapturePhotoOutput) -> Bool {
      return p.availableRawPhotoPixelFormatTypes.contains(boxedPixelFormat)
    }
  }

  AVFoundationTests.test("AVCapturePhotoSettings.availablePreviewPhotoPixelFormatTypes") {
    func f(p: AVCapturePhotoSettings) -> Bool {
      return p.availablePreviewPhotoPixelFormatTypes.contains(boxedPixelFormat)
    }
  }
}

if #available(iOS 11, *) {
  AVFoundationTests.test("AVCaptureSynchronizedDataCollection/iteration") {
    func f(c: AVCaptureSynchronizedDataCollection) {
      for element in c {
        var element = element
        expectType(AVCaptureSynchronizedData.self, &element)
      }
    }
  }
}

#endif

runAllTests()
