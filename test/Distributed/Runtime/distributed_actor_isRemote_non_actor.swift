// RUN: %empty-directory(%t)
// RUN: %target-build-swift -module-name main -target %target-swift-5.7-abi-triple -j2 -parse-as-library %s -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: distributed
// REQUIRES: objc_interop

// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

import Distributed
import Foundation

final class ZeroFirstByte {
  var first: UInt8 = 0
}

final class NonZeroFirstByte {
  var first: UInt8 = 0xFF
}

/// No storage at all, so there is nothing to read at object+16
final class Empty {}

/// A plain actor is never a *distributed* actor, so it is never remote
actor PlainActor {
  var field: Int = 0xFF
}

@main
struct Main {
  static func main() {
    // The answer must not depend on the contents of the first stored property
    print("isRemote(ZeroFirstByte) = \(__isRemoteActor(ZeroFirstByte()))")
    // CHECK: isRemote(ZeroFirstByte) = false
    print("isRemote(NonZeroFirstByte) = \(__isRemoteActor(NonZeroFirstByte()))")
    // CHECK: isRemote(NonZeroFirstByte) = false

    print("isRemote(Empty) = \(__isRemoteActor(Empty()))")
    // CHECK: isRemote(Empty) = false

    print("isRemote(PlainActor) = \(__isRemoteActor(PlainActor()))")
    // CHECK: isRemote(PlainActor) = false

    // A pure ObjC object's isa also reports 'MetadataKind::Class', but it has
    // no class descriptor to read
    print("isRemote(NSObject) = \(__isRemoteActor(NSObject()))")
    // CHECK: isRemote(NSObject) = false
    print("isRemote(NSMutableArray) = \(__isRemoteActor(NSMutableArray()))")
    // CHECK: isRemote(NSMutableArray) = false

    // A tagged pointer has no metadata field at all, so it must be rejected
    // before the metadata is even loaded
    print("isRemote(NSString) = \(__isRemoteActor(NSString(string: "hi")))")
    // CHECK: isRemote(NSString) = false

    print("isLocal(NSObject) = \(__isLocalActor(NSObject()))")
    // CHECK: isLocal(NSObject) = true

    print("done")
    // CHECK: done
  }
}
