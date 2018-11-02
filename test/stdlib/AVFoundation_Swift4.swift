// RUN: %empty-directory(%t)
// RUN: %target-build-swift -swift-version 4 %s -o %t/a.out
// RUN: %target-run %t/a.out
// REQUIRES: objc_interop
// REQUIRES: executable_test
// CoreMedia is not present on watchOS.
// UNSUPPORTED: OS=watchos

import AVFoundation
import StdlibUnittest

var AVFoundationTests = TestSuite("AVFoundation_Swift4")

let pixelFormat = kCVPixelFormatType_420YpCbCr8BiPlanarFullRange

#if os(macOS) || os(iOS)

if #available(iOS 5, *) {
  AVFoundationTests.test("AVCaptureVideoDataOutput.availableVideoPixelFormatTypes") {
    func f(v: AVCaptureVideoDataOutput) -> Bool {
      return v.availableVideoPixelFormatTypes.contains(pixelFormat)
    }
  }
}

#endif

#if os(iOS)

if #available(iOS 7, *) {
  AVFoundationTests.test("AVMetadataMachineReadableCodeObject.corners") {
    func f(m: AVMetadataMachineReadableCodeObject) -> Bool {
      if let c = m.corners.first {
        return c.x == 0
      }
      return false
    }
  }
}

if #available(iOS 10, *) {
  AVFoundationTests.test("AVCaptureDevice.Format.supportedColorSpaces") {
    func f(df: AVCaptureDevice.Format) -> Bool {
      return df.supportedColorSpaces.contains(.sRGB)
    }
  }

  AVFoundationTests.test("AVCapturePhotoOutput.supportedFlashModes") {
    func f(p: AVCapturePhotoOutput) -> Bool {
      return p.supportedFlashModes.contains(.off)
    }
  }

  AVFoundationTests.test("AVCapturePhotoOutput.availablePhotoPixelFormatTypes") {
    func f(p: AVCapturePhotoOutput) -> Bool {
      return p.availablePhotoPixelFormatTypes.contains(pixelFormat)
    }
  }

  AVFoundationTests.test("AVCapturePhotoOutput.availableRawPhotoPixelFormatTypes") {
    func f(p: AVCapturePhotoOutput) -> Bool {
      return p.availableRawPhotoPixelFormatTypes.contains(pixelFormat)
    }
  }

  AVFoundationTests.test("AVCapturePhotoSettings.availablePreviewPhotoPixelFormatTypes") {
    func f(p: AVCapturePhotoSettings) -> Bool {
      return p.availablePreviewPhotoPixelFormatTypes.contains(pixelFormat)
    }
  }
}

if #available(iOS 11, *) {
  AVFoundationTests.test("AVCaptureSynchronizedDataCollection.makeIterator()") {
    func f(c: AVCaptureSynchronizedDataCollection) {
      for _ in c {}
    }
  }
}

#endif

runAllTests()
