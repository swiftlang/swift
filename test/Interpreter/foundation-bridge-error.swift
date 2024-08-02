// Check that we can access localizedDescription, which crashes in the runtime
// if Foundation is loaded after the runtime is already initialized on Darwin.

// REQUIRES: executable_test
// REQUIRES: objc_interop
// REQUIRES: OS=macosx

// FIXME: There's a separate bridging error with the just-built stdlib on CI
// nodes.
// REQUIRES: use_os_stdlib

// RUN: %target-jit-run %s
// RUN: DYLD_INSERT_LIBRARIES=/System/Library/Frameworks/Foundation.framework/Foundation %target-jit-run %s

import Foundation

print("Insert Libraries: \(ProcessInfo.processInfo.environment["DYLD_INSERT_LIBRARIES"] ?? "<nil>")")

enum SomeError: LocalizedError {
 case fail
}

let err = SomeError.fail
let path = (#file as NSString).lastPathComponent
let desc = err.localizedDescription