// RUN: %target-run-simple-swift
// REQUIRES: executable_test

// REQUIRES: objc_interop

import os
import Foundation
import StdlibUnittest

defer { runAllTests() }

var osAPI = TestSuite("osAPI")

if #available(OSX 10.12, iOS 10.0, watchOS 3.0, tvOS 10.0, *) {
  osAPI.test("log") {
    os_log("test: %d", 42)
    os_log("test2: %@", "test")
  }

  /* FIXME: currently disabled: rdar://problem/38354907
  osAPI.test("logData") {
    let data = "hello logging world".data(using: .utf8)!

    data.withUnsafeBytes { (bytes: UnsafePointer<CChar>) in
      os_log("%.3P", OpaquePointer(bytes))
      os_log("%.10P", OpaquePointer(bytes))
      os_log("%.*P", OpaquePointer(bytes))
    }
  }
  */
  osAPI.test("newLog") {
    let newLog = OSLog(subsystem: "com.apple.Swift", category: "Test")
    os_log("test", log: newLog)
  }
}
