// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeCodableForDistributedTests.swiftmodule -module-name FakeCodableForDistributedTests -disable-availability-checking %S/../Inputs/FakeCodableForDistributedTests.swift
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems -disable-availability-checking %S/../Inputs/FakeDistributedActorSystems.swift
// RUN: %target-build-swift -module-name main -enable-experimental-feature Extern -Xfrontend -enable-experimental-distributed -Xfrontend -disable-availability-checking -j2 -parse-as-library -I %t %s %S/../Inputs/FakeCodableForDistributedTests.swift %S/../Inputs/FakeDistributedActorSystems.swift -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s --dump-input=always

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: distributed
// REQUIRES: objc_interop

// rdar://76038845
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

import Foundation
import Distributed
import FakeDistributedActorSystems
import FakeCodableForDistributedTests

typealias DefaultDistributedActorSystem = FakeRoundtripActorSystem

@objc
class SentinelNSError:
  NSObject,
//  NSError,
  Codable, @unchecked Sendable {
  let str: String

  init(_ str: String) {
    self.str = str
    super.init()
    print("\(str).init: \(Unmanaged.passUnretained(self).toOpaque())")
  }

  required init(from: any Decoder) { fatalError("Not implemented") }
  func encode (to: any Decoder) { fatalError("Not implemented") }

  deinit {
    print("\(str).deinit: \(Unmanaged.passUnretained(self).toOpaque())")
  }
}

class Sentinel: Codable {
  let str: String

  init(_ str: String) {
    self.str = str
    print("\(str).init: \(Unmanaged.passUnretained(self).toOpaque())")
  }

  required init(coder: NSCoder) { fatalError() }
  required init(from: any Decoder) { fatalError("Not implemented") }
  func encode (to: any Decoder) { fatalError("Not implemented") }

  deinit {
    print("\(str).deinit: \(Unmanaged.passUnretained(self).toOpaque())")
  }
}

 distributed actor TestActor {
   distributed func returnNSSentinel() -> SentinelNSError? {
     return .init("returnNSSentinel")
   }

   distributed func returnSentinel() -> Sentinel? {
     return .init("returnSentinel")
   }
 }

@main
struct Main {

  static func main() async throws {
    let system = DefaultDistributedActorSystem()

    let instance = TestActor(actorSystem: system)
    let resolved = try TestActor.resolve(id: instance.id, using: system)

    var s0 = try await instance.returnSentinel()
    // CHECK: returnSentinel.init: [[P0:0x[0-9]+]]
    print("s0 retain count = \(_getRetainCount(s0!))")
    // CHECK: s0 retain count = 2
    // CHECK: returnSentinel.deinit: [[P0]]
    s0 = nil

//    var s1 = try await resolved.returnNSSentinel()
//    // CHECK: returnNSSentinel.init: [[P1:0x[0-9]+]]
//    print("retain count = \(_getRetainCount(s1!))")
//    // CHECK: s1 retain count = 2
//    // CHECK: returnNSSentinel.deinit: [[P1]]
//    s1 = nil
//
    var s2 = try await resolved.returnSentinel()
    // CHECK: nserror.init: [[P2:0x[0-9]+]]
    print("s2 retain count = \(_getRetainCount(s2!))")
    // CHECK: s2 retain count = 2
    // CHECK: nserror.deinit: [[P2]]
    s2 = nil


    print("DONE.")
  }
}

@_alwaysEmitIntoClient
public func _getRetainCount(_ object: AnyObject) -> UInt {
  let count = _withHeapObject(of: object) { _swift_retainCount($0) }
  return UInt(bitPattern: count)
}

@_extern(c, "swift_retainCount") @usableFromInline
internal func _swift_retainCount(_: UnsafeMutableRawPointer) -> Int

@_alwaysEmitIntoClient @_transparent
internal func _withHeapObject<R>(
  of object: AnyObject,
  _ body: (UnsafeMutableRawPointer) -> R
) -> R {
  defer { _fixLifetime(object) }
  let unmanaged = Unmanaged.passUnretained(object)
  return body(unmanaged.toOpaque())
}
