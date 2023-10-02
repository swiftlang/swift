// RUN: %target-swift-frontend -emit-ir %s -enable-experimental-feature Embedded -enable-builtin-module

// REQUIRES: swift_in_compiler
// REQUIRES: optimized_stdlib
// REQUIRES: VENDOR=apple
// REQUIRES: OS=macosx

import Builtin

public func test() async {
    _ = Builtin.createAsyncTask(0) { () async throws -> Int in
      return 42
    }
}
