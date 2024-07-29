// Check that we can access localizedDescription, which crashes in the runtime
// if Foundation is loaded after the runtime is already initialized on Darwin.

// REQUIRES: executable_test
// REQUIRES: objc_interop
// REQUIRES: OS=macosx

// RUN: %target-jit-run %s
// RUN: DYLD_INSERT_LIBRARIES=/usr/lib/libobjc.dylib %target-jit-run %s

import Foundation

print(CommandLine.arguments)
print("Insert Libraries: \(ProcessInfo.processInfo.environment["DYLD_INSERT_LIBRARIES"] ?? "<nil>")")

enum SomeError: LocalizedError {
  case fail
}

let err = SomeError.fail
let path = (#file as NSString).lastPathComponent
let desc = err.localizedDescription
