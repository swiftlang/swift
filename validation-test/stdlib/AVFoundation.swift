// RUN: %target-run-simple-swift
// REQUIRES: executable_test
//
// REQUIRES: OS=ios

import StdlibUnittest


import AVFoundation

var AVFoundationTests = TestSuite("AVFoundation")

if #available(iOS 11, *) {
  AVFoundationTests.test("AVCaptureSynchronizedDataCollection.makeIterator()") {
    func f(c: AVCaptureSynchronizedDataCollection) {
      for _ in c {}
    }
  }
}

runAllTests()