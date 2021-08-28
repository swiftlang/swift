// RUN: %target-run-simple-swift( -Xfrontend -disable-availability-checking -Xfrontend -enable-experimental-distributed -parse-as-library)

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: distributed

// rdar://76038845
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

import StdlibUnittest
import _Distributed

@available(SwiftStdlib 5.5, *)
struct ActorAddress: ActorIdentity, CustomStringConvertible {
  let id: String
  var description: Swift.String {
    "ActorAddress(id: \(id))"
  }
}

@main struct Main {
  static func main() async {
    if #available(SwiftStdlib 5.5, *) {

      let ActorIdentityTests = TestSuite("ActorIdentity")

      ActorIdentityTests.test("equality") {
        let a = ActorAddress(id: "a")
        let b = ActorAddress(id: "b")

        let anyA = AnyActorIdentity(a)
        let anyB = AnyActorIdentity(b)

        expectEqual(a, a)
        expectEqual(anyA, anyA)

        expectNotEqual(a, b)
        expectNotEqual(anyA, anyB)
      }

      ActorIdentityTests.test("hash") {
        let a = ActorAddress(id: "a")
        let b = ActorAddress(id: "b")

        let anyA = AnyActorIdentity(a)
        let anyB = AnyActorIdentity(b)

        expectEqual(a.hashValue, a.hashValue)
        expectEqual(anyA.hashValue, anyA.hashValue)

        expectNotEqual(a.hashValue, b.hashValue)
        expectNotEqual(anyA.hashValue, anyB.hashValue)
      }
    }

    await runAllTestsAsync()
  }
}