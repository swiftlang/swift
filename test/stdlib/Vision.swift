// RUN: %target-run-simple-swift
// REQUIRES: executable_test
// REQUIRES: objc_interop
// UNSUPPORTED: OS=watchos

import StdlibUnittest

import Vision


var testsuite = TestSuite("Vision")

if #available(OSX 10.13, iOS 11.0, tvOS 11.0, *) {
  testsuite.test("normalizedPoints overlay") {
    // NOTE: This function is here only to validate syntactical correctness of the overlay.
    func overlaySyntaxTest(region: VNFaceLandmarkRegion2D) -> [CGPoint] {
      return region.normalizedPoints
    }
  }

  testsuite.test("pointsInImage() overlay") {
    // NOTE: This function is here only to validate syntactical correctness of the overlay.
    func overlaySyntaxTest(region: VNFaceLandmarkRegion2D) -> [CGPoint] {
      return region.pointsInImage(imageSize: CGSize(width: 1024, height: 768))
    }
  }
}

runAllTests()
