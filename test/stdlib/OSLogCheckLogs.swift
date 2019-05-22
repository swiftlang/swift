// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -swift-version 5 -o %t/OSLogCheckLogs
// RUN: %target-run %t/OSLogCheckLogs
// REQUIRES: executable_test
// REQUIRES: OS=macosx

// Run-time tests that check that the messages logged with the existing os_log
// APIs and the new prototype APIs (based on string interpolation) are correctly
// readable from the log archive. These tests log some messages and execute the
// `log show` command (by forking a process) to read back the logs and display
// them.

// If these tests fail, it means either there are errors in the overlay
// implementation or certain guarantees that os_log relies on, such as that the
// format string passed to the C ABIs point to a string literal in the __cstring
// section of compiled binary, is broken by the compiler.

import OSLogPrototype
import Foundation
import StdlibUnittest

internal var OSLogTestSuite = TestSuite("CheckLogs")

defer {
  runAllTests()
}

if #available(macOS 10.13, *) {

  /// Tests for the existing os_log APIs.
  OSLogTestSuite.test("log messages and check logs") {
    // Record the time before logging. This is used to filter the logs.
    let startTime = Int64(Date().timeIntervalSince1970)

    let subsystem = "com.apple.oslog.test"
    let logObject = OSLog(subsystem: subsystem, category: "LogCheck")

    os_log("test: %{public}d", log: logObject, 42)
      // CHECK: test: 42
    os_log("test string: %{public}s", log: logObject, "simple string")
      // CHECK: test string: simple string
    os_log(
      "test NS object: %{public}@",
      log: logObject,
      NSOrderedSet(array: [0, 1, 2]))
      // CHECK: test NS object:
      // CHECK: 0
      // CHECK: 1
      // CHECK: 2
    os_log("test NS bridgeable: %{public}@", log: logObject, ["1" : "a"])
      // CHECK: test NS bridgeable:
      // CHECK: 1 = a;

    // Test format specifiers of pointers.
    let data = "hello logging world".data(using: .utf8)!
    data.withUnsafeBytes { bufPtr in
      let basePtr = bufPtr.baseAddress!
      os_log("%{public}.3P", log: logObject, OpaquePointer(basePtr))
        // CHECK: '68 65 6C'
      os_log("%{public}.10P", log: logObject, OpaquePointer(basePtr))
        // CHECK: '68 65 6C 6C 6F 20 6C 6F 67 67'
      os_log("%{public}.*P", log: logObject, data.count, OpaquePointer(basePtr))
        // CHECK: '68 65 6C 6C 6F 20 6C 6F 67 67 69 6E 67 20 77 6F 72 6C 64'
    }

    // Test os_log specific format specifiers.
    os_log(
      "Boolean value %{public,bool}d denotes %{public,BOOL}d",
       log: logObject, 0, 0)
      // CHECK: Boolean value false denotes NO
    os_log("Darwin errorno: %{public,darwin.errno}d", log: logObject, 32)
      // CHECK: Darwin errorno: [32: Broken pipe]
    os_log("File permissions: %{public,darwin.mode}d", log: logObject, 0o754)
      // CHECK: File permissions: -rwxr-xr--
    os_log("Time stamp: %{public,time_t}d", log: logObject, Int32.max)
      // CHECK: Time stamp: 2038-01-18 19:14:07-0800
    os_log("IP address: %{public,network:in_addr}d", log: logObject, 0x0100007f)
      // CHECK: IP address: 127.0.0.1

    var loopbackAddr = in6addr_loopback
    withUnsafePointer(to: &loopbackAddr) { ptr in
      os_log(
        "IP v6 loopback address: %{public,network:in6_addr}.16P",
        log: logObject,
        ptr)
    } // CHECK: IP v6 loopback address: ::1

    dumpLogs(subsystem, startTime)
  }

  /// Tests for the new os log APIs.
  /*OSLogTestSuite.test("log messages using the new APIs and check logs") {
    // Record the time before logging. This is used to filter the logs.
    let startTime = Int64(Date().timeIntervalSince1970)

    // Create a logger with a specific subsystem and category.
    let subsystem = "com.apple.oslog.prototype"
    let h = Logger(subsystem: subsystem, category: "LogCheck")

    // FIXME: We cannot log small strings (<= 15 bytes) with the new APIs yet,
    // e.g. h.log("small") will not work yet.

    // Perform logging. Note that debug and info levels may be disabled in the
    // host machines and the log messages may not show up in the logs.
    // Therefore, use default level or above here.
    h.log("A message with no data")
      // CHECK: A message with no data
    h.log("Minimum integer value: \(Int(Int64.min), format: .hex)")
      // CHECK: Minimum integer value: 8000000000000000
    h.log("Maximum integer value: \(Int(Int64.max), format: .hex)")
      // CHECK: Maximum integer value: 7fffffffffffffff

    // Note that the private value may or may not be visible in the logs.
    // depending on the configuration. Therefore, don't rely on this.
    let privateID = 0x79abcdef
    h.log(
      level: .error,
      "Private Identifier: \(privateID, format: .hex, privacy: .private)")
      // CHECK: Private Identifier

    let addr = 0x7afebabe
    h.log(
      level: .fault,
      "Invalid address: 0x\(addr, format: .hex, privacy: .public)")
      // CHECK: Invalid address: 0x7afebabe

    let filePermissions = 0o777
    let pid = 122225
    h.log(
      level: .error,
      """
      Access prevented: process \(pid) initiated by \
      user: \(privateID, privacy: .public) attempted resetting \
      permissions to \(filePermissions, format: .octal)
      """)
      // CHECK: Access prevented: process 122225 initiated by user: 2041302511 attempted resetting permissions to 777

    h.log("Double percents: %%")
      // CHECK: Double percents: %%

    // Test the limit on the number of arguments to the log messages.
    h.log(
      level: .error,
      """
      \(1) \(1) \(1) \(1) \(1) \(1) \(1) \(1) \(1) \(1) \(1) \(1) \(1) \(1) \
      \(1) \(1) \(1) \(1) \(1) \(1) \(1) \(1) \(1) \(1) \(1) \(1) \(1) \(1) \
      \(1) \(1) \(1) \(1) \(1) \(1) \(1) \(1) \(1) \(1) \(1) \(1) \(1) \(1) \
      \(1) \(1) \(1) \(1) \(1) \(48) \(49)
      """)
      // CHECK: 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 48{{$}}

    // Test escape characters.
    h.log("\"Imagination is more important than knowledge\" - Einstein")
      // CHECK: "Imagination is more important than knowledge" - Einstein
    h.log("Imagination is more important than knowledge \n - Einstein")
      // CHECK: Imagination is more important than knowledge
      // CHECK_NEXT: - Einstein
    h.log("Imagination is more important than knowledge - \\Einstein")
      // CHECK: Imagination is more important than knowledge - \Einstein
    h.log("sparkling heart: \u{1F496}")
      // CHECK: sparkling heart: 💖
    h.log(#"'\b' is an escape character in C but not in Swift"#)
      // CHECK: '\b' is an escape character in C but not in Swift
    h.log(#"This is not escaped \n"#)
      // CHECK: This is not escaped \n
    h.log(##"The interpolated value is \##(10)"##)
      // CHECK: The interpolated value is 10

    dumpLogs(subsystem, startTime)
  }*/

  /// Print the messages logged by the current process to the given `subsystem`
  /// that have a timestamp greater than `startTime`. This function creates a
  /// new process and invokes the `log show` command with necessary filters.
  func dumpLogs(_ subsystem: String, _ startTime : Int64) {
    let currentPID = ProcessInfo.processInfo.processIdentifier
    let args =
      ["show", "--predicate",
       "(subsystem == '\(subsystem)') AND " +
       "(processIdentifier == \(currentPID))",
       "--start", "@\(startTime)"]

    // Set up the log read process.
    let logReadProcess = Process()
    logReadProcess.executableURL = URL(fileURLWithPath: "/usr/bin/log")
    logReadProcess.arguments = args

    // Set up standard out to a pipe and later print the output. Do not
    // rely on the fact that child will inherit stdout of the parent process.
    let pipe = Pipe()
    logReadProcess.standardOutput = pipe
    logReadProcess.terminationHandler = { (p: Process) in
      let reason = p.terminationReason != .exit ? "uncaught signal" : "normal"
      print("Termination reason: \(reason)")
    }

    // Set up the log process and run. Fail if the process cannot be executed.
    do {
      try logReadProcess.run()
      logReadProcess.waitUntilExit()

      let data = pipe.fileHandleForReading.readDataToEndOfFile()
      if let output = String(data: data, encoding: .utf8) {
        expectTrue(output.contains("test: 42"))
        expectTrue(output.split(separator: "\n").count > 3)
      } else {
        expectTrue(false, "Data not convertible to string")
      }
    } catch let error {
      expectUnreachableCatch(error)
    }
  }
}
