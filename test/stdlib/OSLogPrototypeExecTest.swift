// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -swift-version 5 -DPTR_SIZE_%target-ptrsize -o %t/OSLogPrototypeExecTest
// RUN: %target-run %t/OSLogPrototypeExecTest
// REQUIRES: executable_test
// REQUIRES: OS=macosx || OS=ios || OS=tvos || OS=watchos

// Run-time tests for testing the new OS log APIs that accept string
// interpolations. The new APIs are still prototypes and must be used only in
// tests.

import OSLogPrototype
import StdlibUnittest

defer { runAllTests() }

var OSLogTestSuite = TestSuite("OSLogTest")

if #available(OSX 10.12, iOS 10.0, watchOS 3.0, tvOS 10.0, *) {

  // Following tests check whether valid log calls execute without
  // compile-time and run-time errors.

  func logMessages(_ h: Logger) {
    // Test logging of simple messages.
    h.log("A message with no data")

    // Test logging at specific levels.
    h.log(level: .debug, "Minimum integer value: \(Int.min, format: .hex)")
    h.log(level: .info, "Maximum integer value: \(Int.max, format: .hex)")

    let privateID = 0x79abcdef
    h.log(
      level: .error,
      "Private Identifier: \(privateID, format: .hex, privacy: .private)")
    let addr = 0x7afebabe
    h.log(
      level: .fault,
      "Invalid address: 0x\(addr, format: .hex, privacy: .public)")

    // Test logging with multiple arguments.
    let filePermissions = 0o777
    let pid = 122225
    h.log(
      level: .error,
      """
      Access prevented: process \(pid) initiated by \
      user: \(privateID, privacy: .private) attempted resetting \
      permissions to \(filePermissions, format: .octal)
      """)
  }

  OSLogTestSuite.test("log with default logger") {
    let h = Logger()
    logMessages(h)
  }

  OSLogTestSuite.test("log with custom logger") {
    let h =
      Logger(subsystem: "com.swift.test", category: "OSLogAPIPrototypeTest")
    logMessages(h)
  }

  OSLogTestSuite.test("escaping of percents") {
    let h = Logger()
    h.log("a = c % d")
    h.log("Process failed after 99% completion")
    h.log("Double percents: %%")
  }
}
